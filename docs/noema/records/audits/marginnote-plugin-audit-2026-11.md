# Noema / MarginNote 安全与能力审计

审计日期：2026-11-05。范围为本机 Noema 的 MarginNote 插件、host mailbox/HTTP、CLI/MCP、打包流程及
marginnote-agent skill。采用源码审查、隔离回归测试、在线只读鉴权和已有局部图文回执；未对用户笔记做
破坏性测试，未修改 MarginNote 应用本体或直接写数据库。本文是一次性记录，当前契约由
[接口设计](../../design/marginnote-agent-bridge.md) 维护。

后续进展见[官方通道与局部复查补充验收](marginnote-official-channel-2026-11.md)：正式源码已到 1.0.6，
官方临时面板只读闭环已打通；下文保留首次审计时的版本与测试结果，不代表最新部署状态。

## 结论与部署状态

当前适用于受信本机 Agent 操作 Mac 的高权限原生通道。监听和鉴权有明确边界，但它不是沙箱，
没有逐学习集、只读角色或逐 API 的权限隔离。没有证据可以宣称已经实现 iPad 的同一插件安全接入、
官方同 URL 前缀/同认证执行闭环，或批注随笔迹自动移动。

| 层 | 本次状态 |
|---|---|
| Skill | 已更新，图像特征/真实笔迹/批注卡统一坐标与依赖；59 项测试通过 |
| Noema 源码 | 已修复下表问题；插件版本升至 1.0.5，ID 仍为 `noema.extension.agent`，显示名 Noema |
| 本机安装包 | `site-lisp/noema/build/marginnote/noema.mnaddon`，1.0.5，0600，打包源码与工作树一致；含凭据，未上传 |
| MN 在线插件 | 查询仍为 1.0.3；没有把新包安装到 MN，不将模拟测试称为新插件真机验收 |
| Noema host | 当前 PID 64125 未重启；常驻 mailbox/HTTP 修复需 host 重载后生效。CLI 每次新进程使用新源码 |
| 本机构建 | `make build install` 成功；其 install 安装 Noema 构建产物，不是 MN 插件安装器 |

## 发现与修复

以下“已修复”指源码和新构建包，部署边界见上表。风险以受信本机执行模型评估；没有宣称发现互联网可达的
免认证远程执行漏洞。

| 项 | 影响与证据 | 处理 |
|---|---|---|
| 凭据包初始权限窗口，中 | 原打包先创建 ZIP 写入令牌，再 chmod；旧目标若为符号链接还会被跟随。由代码路径确认，隔离回归检查目标替换与文件保留 | [build-marginnote-addon.mjs](../../../../site-lisp/noema/scripts/build-marginnote-addon.mjs) 使用 0700 临时目录、写前 0600 文件、原子 rename；未触碰符号链接指向文件 |
| 目的地址与重定向，中，配置误用风险 | 客户端原先直接信任描述符 URL，fetch 默认跟随重定向；这不等于外部攻击者已能修改私有描述符 | [client.mjs](../../../../site-lisp/noema/plugins/marginnote/client.mjs) 限制回环、路径、凭据格式并禁止重定向；原生包增加回环配置校验。隔离 HTTP 302 测试确认第二请求未发出 |
| 超时后窗口卡死，中，可复现 | 将时钟推进 16 分钟，旧版本丢失仍在执行的 request；晚到结果后 running 仍占用窗口 | [bridge.mjs](../../../../site-lisp/noema/server/Features/MarginNote/bridge.mjs) 保留 running 身份；测试确认晚到结果释放窗口并可执行下一任务 |
| 中文请求分块损坏，中，数据完整性 | Buffer 分块各自转字符串，若汉字跨块会产生替代字符 | [api.mjs](../../../../site-lisp/noema/server/Features/MarginNote/api.mjs) 按字节预算收集 Buffer，完整拼接后 UTF-8 解码；回归刻意在“眼”字第 1 字节后分块 |
| 返回大小与错误无界，中，可靠性 | 原生值按字符数限 1 MiB，中文值可能在 HTTP 层超 2 MiB 后持续重投；抛出的错误没有字段长度上限 | [main.js](../../../../site-lisp/noema/plugins/marginnote/main.js) 按 NSData UTF-8 字节计数，错误消息/堆栈限长。40 万汉字返回被拒绝，10 万汉字通过；旧在线运行时只读验证“眼睛”为 6 字节 |
| 请求 ID 原型键碰撞，低 | 普通对象缓存让 `constructor`、`__proto__`、`toString` 与继承属性冲突 | 同上使用 `Object.create(null)`；三种 ID 均执行一次，重复响应不再执行 |
| 畸形 URL 未处理拒绝，低 | 路由构造 URL 原先在 try 外，畸形 request-target 会抛出 | HTTP handler 返回 400；隔离测试覆盖 `//[`，没有向在线旧 host 发送会触发故障的样本 |

## 已有安全边界与剩余限制

在线只读 `/sessions` 验证：无令牌 401、错误令牌 401、正确令牌 200、带外站 Origin 401、
`Origin: null` 401。host 描述符、令牌文件、安装包均为 0600；报告没有记录令牌内容。
服务只监听回环，发布 server 模式不创建原生 bridge，比较令牌使用等长 `timingSafeEqual`。
MCP 执行工具标记 destructive/non-idempotent/open-world，查询工具标记只读。

这些是访问入口保护，不是原生代码沙箱。持有令牌者能执行全部宿主 JSB API，包含文件、网络、资料库和
UI 操作；官方 API 的 confirm 规则不会自动阻止插件写入。原生端和 Agent 端共享同一令牌，sessionId
用于调度和结果归属，并非另一个独立身份认证因素。笔记/图片/OCR 内的指令不能提升为执行授权。

其他限制：

- token 及 package 明文落盘，0600 不防同一 OS 用户、已受信 Agent 或系统管理员；没有自动撤销/轮换 UI。
  未擅自更换在用 token，也未将包公开分发。
- Node 客户端拒绝重定向；原生 NSURLConnection 仍受平台重定向行为影响，本次未证明它具备同样控制。
- 原生任务与 Promise 不能强制中断；断开只停止轮询和后续回执，不撤回已写数据，也不能阻止已运行代码继续副作用。
- 请求去重在内存中且有容量/保留期；host 重启后不保证 exactly-once。长任务超时必须查询原请求和目标，
  批注使用稳定 plan/source 元数据查重，不能直接重建。
- 256 KiB 代码、2 MiB body、1 MiB JSON 值、队列/会话上限仅限制传输与调度，不限制原生代码 CPU、内存或访问范围。
- 私有 KVC 坐标字段与 UIView 路径有版本依赖。避免曾闪退的 `notebook.mindmapView` getter；只用静态确认且
  测试画布验过的字段，升级后重新校准。语义关系不由坐标变换自动证明。

## 能力验收矩阵

| 能力 | 证据与边界 |
|---|---|
| 按区域、增量阅读 | 官方轻量库存后只取选定局部。最终同时复查两个已有区域，6 次请求、0 下载、0 解码、0 渲染，均可复用 |
| 图片上笔迹一体阅读 | 鳄鱼单图、普通背景笔迹、叠合图共同支持两个红圈覆盖双眼；照片四角独立校正，没有按圈的位置反调底图 |
| 原生图片像素 | 官方该图片依旧白图。旧回执升级时仅重取这一个 26,593 字节图片，4 个笔迹块复用；参考图对齐不等于官方图片接口已修复 |
| 图像特征进入批注规划 | 新 `--image-view` 核验场景、图片实例、源摘要和上下文图；特征框映回 `official-writing-stored`，保留参考图来源。当前眼睛窗口放不下 700×552 卡，正确返回无空位；未为审计增建卡 |
| 文字旁独立批注 | 前次 Research 实卡已有内容、实际 frame、关系元数据与源未变化的回执，横向误差约 0.03，纵向 0；卡直接属于实际草稿根，没有写进定位卡 |
| 自由卡坐标 | Mac 5.0.1 顶层独立卡/子脑图直属卡。`S=C-size/2-offset+20000`，mindpos 直接写 S；普通树分支自动排版不同 |
| 自动跟随移动 | 尚未实现；保存源特征、实例、位置和摘要仅支持复核，不能宣称已经绑定 |
| 官方插件通道 | 401 鉴权路径已确认，panel/eval 仍缺执行闭环；正式包仍是独立本机旧传输 |
| iPad 插件 | 没有完成执行/坐标/iPad 同步验收；本机 loopback 包不是双端方案。iPad 官方 Bridge 的已验证功能独立记录 |

## 验证记录

- Noema 针对性：3 文件、24 项通过，覆盖 UTF-8 分块、字节上限、错误限长、在途 TTL、请求原型键、
  鉴权、畸形 URL、重定向以及包目标符号链接。
- Skill：59 项通过；校验器通过；真实眼睛样例的离线规划为 0 网络、0 下载、0 解码、0 OCR。
  完整源检查后图片对照页 `reused:true`、`changedViews:[]`。
- Kernel：`go test -tags fts5 ./...` 通过。`make build install` 通过。
- 最终全量前端：3583 通过、2 失败、16 跳过。`image-layout` 为已有 LaTeX 期望差异；
  `drag-select-cost` 为此前也出现过的性能阈值失败。首轮 `latex-export` 的单项超时，单独复跑及最终全量均通过。
  没有修改这些无关实现或测试。
- Emacs research：234 通过、1 失败（`agent-shell-requests-resolve-in-their-own-session`）；
  Jupyter：214 通过、1 失败（`+noema-related-notes-uses-existing-agent-context`）。与此前报告一致。
- 静态核对 `web-host.mjs`：缺省或 `NOEMA_HOST_MODE=desktop` 均归入 emacs；package/Makefile 没有 Electron 构建或安装入口。全量中的 Jupyter cell service 测试覆盖 `.noema` 在 kernel 启动前拒绝。

本机详细回执位于 `var/noema/marginnote/security-audit/`，包括测试日志、版本/权限检查、局部增量结果、
`eye-plan.json` 和未写入 MN 的预览。实际文字批注回执保存在相邻 `annotation-live/`。

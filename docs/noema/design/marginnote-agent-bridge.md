# MarginNote agent 接口

## 范围

MarginNote 插件是 Codex 等 agent 操作原生应用的接口。它在应用自身的
JavaScriptCore/JSB 环境执行代码，使用宿主已经提供的全部 API；文件、学习集、
笔记、脑图、阅读器及撤销仍由 MarginNote 管理。Noema host 只传递请求与结果。

## 模型

每个连接中的原生窗口有一个独立会话。插件主动向现有 Noema host 长轮询任务，
在主队列执行 JavaScript，再提交结果。agent 可通过 stdio MCP 或命令行调用。
代码是支持 `return`、`await` 的函数体；`args` 是 JSON 参数，`bridge.state`
保留该窗口会话中的对象。原生全局对象全部可用，便利变量为 `app`、`db`、
`window`、`study`、`reader`、`notebook`。返回值须能序列化为 JSON。

## 契约

- **MN-C1：** 不为原生 API 设置操作白名单，不逐方法另建代理。能使用的范围由
  当前 MarginNote 插件宿主决定；未暴露的私有接口不会因此新增。
- **MN-C2：** Noema host 不执行收到的代码。代码只在指定的原生窗口执行，支持
  Promise，返回 JSON 结果或带堆栈的错误；控制台输出随结果返回。
- **MN-C3：** 每个窗口串行领取任务；任务有唯一 ID，不因网络重试再次执行。
  未领取任务超时后撤销；已领取任务超时只报告结果未知，不重试、不宣称已撤销。
- **MN-C4：** 窗口关闭、插件卸载和 iOS 应用后台会逻辑撤销轮询及定时器；
  Mac 切换前台应用仍可调用。无取消句柄的网络请求有 25 秒上限，过期回调被忽略。重新连接使用
  新会话 ID，不复用断开窗口的任务。host 关闭时结束所有等待。
- **MN-C5：** 通道由现有 Emacs host 进程拥有，固定的 `127.0.0.1:3212` 仅提供
  `/v1/marginnote/*`，避免主 host 随机端口变化后重新安装插件。独立 bearer token，拒绝浏览器
  跨域请求；发布服务器不提供执行通道。连接配置和安装包只保存在本机。
- **MN-C6：** API 查询提供上游声明及运行时探查；声明是参考快照，实际支持以
  当前安装版本为准。数据库修改由调用者使用原生 UndoManager 与刷新接口。
- **MN-C7：** `marginnote-digitize` pack 只链接 OCR 主 skill、PDF、
  `noema-authoring-syntax` 和 `noema-tikz-figures`。语法由当前权威文档
  `docs/noema/design/syntax.md` 提供，skill 不复制语法规则。普通菜单任务
  在发送时直接载入该文档全文；其他入口由语法 skill 要求先读取同一文档。
  Skill Manager 管理其成员、前导语和作用域；不复制正文。
  默认按任务选用，不全局启用。`@@pack(marginnote-digitize)` 经现有 Run 路径解析。
- **MN-C8：** `noema-marginnote` 是 Emacs 操作菜单，提供原生笔记本/文档的
  OCR、继续资产目录中的 OCR、显式合并 Markdown、查询进度和管理 pack。
  操作读取当前作用域的有效 pack、skill 引用及当前语法文档，经 `noema-context-send-prompt`
  发送给已有 agent 会话；忙时沿用会话队列，前缀参数只起草。没有会话时可以
  从菜单启动现有 agent 入口。它不新建 Project、另建模型客户端或自行运行 OCR。
- **MN-C9：** 默认只对用户指定的原生范围做智能 OCR 并写入指定目标，保留
  笔记内容、顺序、结构与未完成推导，允许简单纠正明显笔误。正文不加入识别
  过程、符号辨认理由或未经请求的论文解释。截图与 TikZ 承载实际图示，主稿
  使用官方指定页压平 PDF；数据库和笔画只辅助增量、定位与区分背景。整理、扩写、内容合并须由用户明确要求；接续
  OCR 或存在旧稿不构成内容合并授权。
- **MN-C10：** `noema-sync-marginnote`（`noema-marginnote-sync`）是正式入口，
  与 LaTeX 导出一样拥有自己的 agent 会话：读入目标 Markdown（可空）、写入方式
  和 prompt，解析当前作用域的有效 `marginnote-digitize` pack，以 `marginnote`
  来源启动会话，初始化完成后把 pack 引用、语法文档路径、目标、写入方式和
  prompt 作为一轮发送，会话在独立 frame 中显示，`noema-marginnote-sync-open`
  可重新打开。手写转 Markdown、转 Wiki 页并挂 wikilink 等用途都由 prompt
  经 skill 激活，Emacs 不按用途分支。写入方式是 `new`（目标已存在则拒绝）、
  `update`（只换对应原稿范围）、`append`、`replace`、`merge`；后两者即为用户
  对覆盖或内容合并的明确授权。
- **MN-C11：** 同步会话钉住 `noema-marginnote-sync-agent` / `-model` /
  `-effort`（默认 Codex、`gpt-6.1-terra`、`xhigh`），经
  `noema-agent-acp-pinned-config`：普通会话记住的模型不进入它，它里面的模型
  选择也不写回普通会话的记忆。钉住的模型不可用时任务只留作草稿，不在别的
  模型上运行。
- **MN-C12：** 同步任务的中间产物（官方导出 PDF、页面渲染、裁图、OCR 候选、
  分页草稿、`workflow.json` 与回执）放在 `noema-marginnote-work-root`
  （`var/noema/marginnote/`）下按原生来源身份分的子目录，重跑时复用，不进
  vault。正文显示的图片按 Noema 笔记文件约定放在目标旁的
  `images/<笔记文件名>/`，其他被链接文件放 `attachments/<笔记文件名>/`
  （[syntax.md](syntax.md) “A note's own files”），不另建目录。会话工作目录
  就是该根目录。
- **MN-C13：** 原生 PKDrawing 笔迹的公式与拉丁文字疑难，skill 可用
  `render_drawing.swift --strokes` 导出区域笔画坐标，经 `mathpix_ocr.py
  --strokes` 调 Mathpix `/v3/strokes` 取候选；与图像接口一样按需调用、缓存于
  工作目录、不自动重试，中文仍由 agent 看图转写。

## 实现位置

- `plugins/marginnote/`：原生插件、API 声明索引、agent CLI 与 stdio MCP。
- `server/Features/MarginNote/`：任务中转、会话和 HTTP 控制器。
- `scripts/build-marginnote-addon.mjs`：将本机连接配置打入 `.mnaddon`。
- `tests/marginnote-*.test.ts`：原生宿主、传输与 agent 接口契约。
- `lisp/noema-marginnote.el`：任务菜单（复用能力解析和已有上下文发送）与
  `noema-marginnote-sync` 自有会话入口。
- `lisp/noema-agent-acp.el`：`noema-agent-acp-pinned-config`，LaTeX 导出与同步共用。
- Emacs 的 `etc/noema/capabilities.json`：`marginnote-digitize` pack 定义；
  主 skill、引用和脚本仍在 README 技能库。
- `tests/elisp/noema-marginnote-tests.el`：作用域、原生身份、任务引用、续接与同步会话
  （MN-C10–C12）；`noema-agent-acp-tests.el` 覆盖模型双向隔离。

## 已知差距

- **MN-G1：** 本机 1.0.3 的真实查询、二进制分块读取与结果回传已验证；iPad
  连接及取材仍未验证。
- **MN-G2：** 公开 API 声明未包含通用按页压平 PDF 函数，不能把笔记本备份
  当作 PDF 导出。macOS 4.4.6（44602）的官方 `ExportDocPDF` 命令及后续选页、
  PDF 格式选择、临时文件取回已验证；skill 的 `official_export.mjs` 按原生页号
  回读校验范围，保存官方 PDF 与摘要回执，并收起保存窗口。该界面适配限定于
  已核对版本；其他版本与 iPad 须重新核对，导出后仍需视觉检查图层完整性。

## 安装与调用

在 Noema 源码目录运行 `node scripts/build-marginnote-addon.mjs`，将
`build/marginnote/noema-agent.mnaddon` 交给 MarginNote 原生安装器并启用。
构建器读取活跃 host 的私有描述符，把本机连接配置打入权限为 0600 的安装包；
安装包不得提交或公开。插件默认连接，工具栏 N 按钮控制连接。

MCP 入口为 `plugins/marginnote/mcp.mjs`，四个工具是 `marginnote_status`、
`marginnote_api`、`marginnote_execute` 和 `marginnote_result`。Noema 的全局
能力配置已声明 `noema-marginnote`。其他 MCP 客户端使用 Node 启动该入口。
本地 agent 也可调用 `node plugins/marginnote/cli.mjs status`，或
`node plugins/marginnote/cli.mjs exec /path/to/code.js`。代码是函数体，例如：

```javascript
return db.allNotebooks().map(n => ({ id: n.topicId, title: n.title }));
```

`marginnote_api` 搜索完整上游声明快照；`bridge.globals()` 与
`bridge.inspect(object)` 探查实际运行时。声明索引通过构建脚本
`--refresh-api /path/to/marginnoteapp-Addon` 更新，来源与 revision 在索引中。
返回值上限 1 MiB，大文件通过原生文件路径或分块读取。超时的已领取代码
不能强制中断 JavaScriptCore，返回结果未知；通过原 requestId 查询晚到结果。
原生回调的空错误参数可能是 truthy 的 `NSNull` 包装对象，不能只用 JS 真值
判断错误。同步代码直接返回；原生回调可用 `bridge.defer()` 后调用
`bridge.done(value)` 或 `bridge.fail(error)`，也支持 Promise / `await`。

业务 skill 位于
`~/Documents/Noema/public/README/Skills/skills/marginnote-digitize/`，原生笔记本和
文档为起点，按页/区域增量 OCR 并保留已有 Markdown 编辑。合并另一份手写稿
或已有文稿是显式操作，不随 OCR 自动执行。流程脚本维护只读取材、变化页、
OCR 候选、写入前的版本校验与进度；三方内容合并仅在用户要求时使用。
多模态解析由当前 agent 完成，不依赖模型 API。agent 可直接解析有权限读取
的本地数据库和数据包；脚本查询一致快照，原库不被写入。

MarginNote 4 的文档手写可能属于 `options.topicLinkMap[docMd5]` 指向的关联
topic；父学习集的空查询不代表没有手写。业务 skill 的 `read_native_drawing.js`
查询该关联 topic 和实际 JSB 方法名，将 PKDrawing 媒体分块取回；
`render_drawing.swift` 在已核对的空白纸条件下保留全部笔迹范围、方向和颜色，
生成 PDF 证据。原始媒体的 hash 变化只提示重新比较，不自动触发重新识别；
渲染像素相同仍复用已核对结果。成功写入的逐页回执独立保存，后续计划不抹掉它。

## 待定设计

无。

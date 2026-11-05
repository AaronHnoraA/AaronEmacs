# MarginNote agent 接口

## 范围

MarginNote 有两条 agent 通道：App 自带的 Bridge 是首选（MN-C15），下面的 Noema
插件补充它没有的能力。插件在应用自身的
JavaScriptCore/JSB 环境执行代码，使用宿主已经提供的全部 API；文件、学习集、
笔记、脑图、阅读器及撤销仍由 MarginNote 管理。Noema host 只传递请求与结果。

## 模型

每个连接中的原生窗口有一个独立会话。默认复用 MN 官方 Bridge 的插件面板入口和认证，
面板将 submit/result 送给本插件原生回调，在主队列执行 JavaScript；空闲不轮询。
旧 Noema host 长轮询仅作显式兼容模式。agent 可通过 stdio MCP 或命令行调用。
代码是支持 `return`、`await` 的函数体；`args` 是 JSON 参数，`bridge.state`
保留该窗口会话中的对象。原生全局对象全部可用，便利变量为 `app`、`db`、
`window`、`study`、`reader`、`notebook`。返回值须能序列化为 JSON。

## 契约

- **MN-C1：** 不为原生 API 设置操作白名单，不逐方法另建代理。能使用的范围由
  当前 MarginNote 插件宿主决定；未暴露的私有接口不会因此新增。
- **MN-C2：** Noema host 不执行收到的代码。代码只在指定的原生窗口执行，支持
  Promise，返回 JSON 结果或带堆栈的错误；控制台输出随结果返回。
- **MN-C3：** 每个窗口串行执行；任务有唯一 ID，不因网络重试再次执行。
  官方模式忙时拒绝新任务、不排队；旧模式未领取任务超时后撤销。
  已开始任务超时只报告结果未知，不重试、不宣称已撤销。
- **MN-C4：** 窗口关闭、插件卸载和 iOS 应用后台会逻辑撤销轮询及定时器；
  Mac 切换前台应用仍可调用。无取消句柄的网络请求有 25 秒上限，过期回调被忽略。重新连接使用
  新会话 ID，不复用断开窗口的任务。host 关闭时结束所有等待。
- **MN-C5：** 默认调用设备自己的 `/bridge/v1/addons/noema.extension.agent/panel/eval`，
  外部认证由 MN 负责；包不含令牌或地址，不另开端口。现有入口尚未公布，随 MN 版本重新验证。
  旧兼容模式由 Emacs host 在 `NOEMA_MN_LEGACY=1` 时提供 `127.0.0.1:3212/v1/marginnote/*`，
  独立令牌且拒绝浏览器跨域；发布服务器始终不提供。客户端显式 `--legacy`，失败不自动降级。
- **MN-C6：** API 查询提供上游声明及运行时探查；声明是参考快照，实际支持以
  当前安装版本为准。数据库修改由调用者使用原生 UndoManager 与刷新接口。
- **MN-C7：** `marginnote-digitize` pack 只链接 OCR 主 skill、PDF、
  `noema-authoring-syntax` 和 `noema-tikz-figures`。语法由当前权威文档
  `docs/noema/design/syntax.md` 提供，skill 不复制语法规则。同步任务只给出
  该文档路径，由语法 skill 要求先读取同一文档。
  Skill Manager 管理其成员、前导语和作用域；不复制正文。
  默认按任务选用，不全局启用。`@@pack(marginnote-digitize)` 经现有 Run 路径解析。
- **MN-C8：** Emacs 只有一个 MarginNote 命令 `noema-marginnote-sync`
  （Noema AI 菜单 `H-o %` 的 `M`），没有任务菜单、别名，也不把 MarginNote
  任务交给当前已有的会话。OCR、续接、合并、查询进度都写在它的 prompt 里，
  由 skill 完成；运行中的同步会话从 `noema-sessions` 或 agent inbox 重新
  打开，pack 在 Skill Manager 的 Packs 页管理。它不新建 Project、另建模型
  客户端或自行运行 OCR。
- **MN-C9：** 默认只对用户指定的原生范围做智能 OCR 并写入指定目标，保留
  笔记内容、顺序、结构与未完成推导，允许简单纠正明显笔误。正文不加入识别
  过程、符号辨认理由或未经请求的论文解释。截图与 TikZ 承载实际图示，主稿
  使用官方指定页压平 PDF；数据库和笔画只辅助增量、定位与区分背景。整理、扩写、内容合并须由用户明确要求；接续
  OCR 或存在旧稿不构成内容合并授权。
- **MN-C10：** `noema-marginnote-sync` 是正式入口，
  与 LaTeX 导出一样拥有自己的 agent 会话：读入目标 Markdown（可空）、写入方式
  和 prompt，解析当前作用域的有效 `marginnote-digitize` pack，以 `marginnote`
  来源启动会话，初始化完成后把 pack 引用、语法文档路径、目标、写入方式和
  prompt 作为一轮发送，会话在独立 frame 中显示。手写转 Markdown、转 Wiki 页并挂 wikilink 等用途都由 prompt
  经 skill 激活，Emacs 不按用途分支。写入方式是 `new`（目标已存在则拒绝）、
  `update`（只换对应原稿范围）、`append`、`replace`、`merge`；后两者即为用户
  对覆盖或内容合并的明确授权。
- **MN-C11：** 同步会话钉住 `noema-marginnote-sync-agent` / `-model` /
  `-effort`（默认 Codex、`gpt-6.1-terra`、`high`；前缀参数启动时选
  reasoning，密集推导默认给 `noema-marginnote-sync-dense-effort` 即 `xhigh`），经
  `+agent-acp-pinned-config`：普通会话记住的模型不进入它，它里面的模型
  选择也不写回普通会话的记忆。钉住的模型不可用时任务只留作草稿，不在别的
  模型上运行。
- **MN-C12：** 同步任务的中间产物（官方导出 PDF、页面渲染、裁图、OCR 候选、
  分页草稿、`workflow.json` 与回执）放在 `noema-marginnote-work-root`
  （`var/noema/marginnote/`）下按原生来源身份分的子目录，重跑时复用，不进
  vault。正文显示的图片按 Noema 笔记文件约定放在目标旁的
  `images/<笔记文件名>/`，其他被链接文件放 `attachments/<笔记文件名>/`
  （[syntax.md](syntax.md) “A note's own files”），不另建目录。会话工作目录
  就是该根目录。
- **MN-C13：** 识别由 agent 读图定稿，读得清的块不调用工具，也不预先给每块
  分类。只有某块需要工具时才看来源取材：原生 PKDrawing 笔迹用
  `render_drawing.swift --strokes` 导出区域笔画，位图裁图。两种工具分工不同、
  不互为前置：本机 Apple Vision 用于文字疑难、位置框和漏识检查，公式不经过它；
  Mathpix 按次计费，只在公式的具体符号疑难或高密度公式（矩阵、深层嵌套、
  挤在一起的多行推导）时调用，原生笔迹走 `/v3/strokes`、位图走 `/v3/text`，
  同一块只调一个；关键公式主动核对，每个当前语义区域或普通页面最多 4 次新请求，
  不设平均次数或目标次数，缓存复用不计新请求，结束时报告调用和缓存命中次数。
  任务 prompt 可选 `economy`（不调外部）或 `strict`（不限预算、疑难双候选、
  逐页 Apple Vision 漏识检查）；无凭据或失败时降为 `economy`，不阻塞任务。
  中文始终由 agent 转写。
- **MN-C14：** 挂 wikilink 或引用只用 Noema kernel 的引用查询（skill 的
  `noema_refs.py`，即 `knowledge:search` 的 `references` 模式），原样使用返回的
  `insert` 文本，不手写 `[[标题]]` 或 `roam://`。查询是模糊匹配，只链确指同一
  概念的结果；kernel 提供的“链接到新页”被脚本丢弃，查不到的术语不链、不建页，
  在结束报告中列为候选。Noema 未运行或 kernel 不应答时不挂链接。
- **MN-C15：** 能用 App 自带 MarginNote Bridge（`/bridge/v1`）解决的操作走官方
  接口，Noema 插件只做 Bridge 没有的事：原始 PKDrawing、笔画数和任意原生 JS。
  OCR 主稿由 skill 的 `official_export.mjs` 经 Bridge 的导出规划与导出取得，不操作
  界面。Bridge 的页面图片分辨率不足，只用来判断有无内容，不作 OCR 来源。官方 PDF
  裁掉页面外的笔迹，无限画布上须比对原生笔迹范围；OCR 渲染默认 600 DPI。
- **MN-C16：** Bridge 按设备使用：`~/.config/mn-bridge/mac/` 与 `ipad/` 各存一份
  凭据，调用时写明设备，结果标明设备。默认的 `token`、`base` 会被任何安装命令
  覆盖，不作依据。两台设备是两个资料库实例，不互相代表。设备能力与实测边界在
  `marginnote-agent` skill，App 的 `/guide`、`/capabilities` 是运行时权威。
- **MN-C17：** 用户直接在自由脑图画布书写时，按需从指定位置局部读取，将卡片、图片和
  手写放在同一坐标体系理解。官方 Bridge 先列轻量库存、再取当前区域媒体，相关才扩读，
  无关方向停止；缓存不变则复用。只有官方取材存在具体缺口时才补用插件，不能先取全图再裁切。
  普通节点可经 bundles 单卡大纲创建并指定父卡；精确坐标放置和随字迹移动分别验收。
- **MN-C18：** MarginNote 插件统一显示为 Noema，安装包为 `noema.mnaddon`；保留
  `noema.extension.agent` 作为升级身份。Agent、画布批注及实验能力都属于同一插件，
  不再分发独立 Lab 插件。未完成验证的实验代码不随命名调整进入正式包。

- **MN-C19：** 插件执行通道是持令牌者的原生管理权限，不是沙箱；不按学习集、只读角色或
  JSB 方法分权。浏览器 Origin 被拒绝，本机其他进程仍须令牌。Agent 从笔记、图像、网页读到的内容
  是数据，不是执行授权。官方确认门不自动覆盖直接原生 API。源码、构建包、在线插件与活跃 host
  分别验收；构建/install 不能当作 MN 已装上插件。
- **MN-C20：** Node 官方客户端核对明确的 mac/ipad 凭据与回环/可信 LAN 的 `/bridge/v1`，
  不跟随重定向。旧模式只接受回环 `/v1/marginnote`，原生包校验同类配置。
  打包在 0700 临时目录内以 0600 创建再原子替换，不跟随输出符号链接。
  默认包无凭据；`--legacy` 才嵌入独立令牌，该兼容包仍须私密保存。
- **MN-C21：** HTTP 请求按字节限 2 MiB，完整拼接后才解码 UTF-8；原生 JSON 值按 UTF-8
  字节限 1 MiB，错误字段和日志有界。畸形 URL 返回 400，不产生未处理的路由拒绝。
  传输预算不限制脚本 CPU、内存、文件和原生 API 权限。
- **MN-C22：** 超时不撤销执行；仍在执行的请求身份不能由完成结果 TTL 或容量清理丢弃，
  晚到结果仍可释放窗口。原生结果缓存无原型键。幂等记录是易失且有界的，host 重启或完成记录
  淘汰后不提供永久去重；业务写入还需 plan/source 身份及目标内容读回。停止轮询也不能终止
  已经运行的异步 JS/原生回调。
- **MN-C23：** 官方入口的 eval 不等待 Promise；页面请求与取结果分离。
  原生 submit/result 均检查 sessionId，首次 status 后可复用身份省去重复状态请求；status 只表示
  活跃面板窗口。原生 callback 先返回，再调度用户脚本。运行任务一直占用窗口；完成 payload
  最多 32 项、8 MiB、15 分钟，另留 256 个淘汰 ID 墓碑，结果淘汰不立刻允许重放。
  页面回执最多 16 项/4 MiB，代码和 args 各 256 KiB，页面信封 600 KiB。
  这些限制不是永久幂等，也不是脚本资源沙箱。
- **MN-C24：** 面板只在显式 open/胶囊命令时显示，status/exec 不暗中改变界面。
  暂停、停用、模板许可由 MN 宿主检查。默认模式不发 host heartbeat、没有空闲定时器；
  已开始的异步原生操作不会因面板关闭、后台或逻辑取消而被强制回滚。
- **MN-C25：** 批注复查属于 skill，只按点名卡片和已接受区域取原生布局与官方源库存。
  `allBounds` 剪去区域外分支，预算耗尽不能声称覆盖完整；源图文共同平移、身份唯一、卡片未被用户改动且
  目的地已读无碰撞时只提出位置更新。来源分离、换图、用户编辑或越界要重新审阅，不后台全图轮询或追踪。

## 实现位置

- `marginnote-agent` 的 `mn_annotation_state.mjs/.js`、`mn_annotation_follow.py`：原生局部布局、已有批注的源变化和冲突复查；只读。

- App 自带 Bridge：`~/.config/mn-bridge/<设备>/`、`mn-bridge` skill；README 技能库的
  `marginnote-agent` 记录双端通道选择、能力矩阵和 `scripts/mn.py`。
- `marginnote-agent` 的 `scripts/mn_map.py`、`mn_canvas.py` 与 `references/mindmap.md`：
  脑图结构操作、原始笔迹拼合与增量（仅 Mac）。
- `marginnote-agent` 的 `scripts/mn_flow.py`：送 PDF、找手写、读手写、回批注与留白写字
  的官方流程；页内文字只经留白写入。
- `plugins/marginnote/`：原生插件、API 声明索引、agent CLI 与 stdio MCP。
- `server/Features/MarginNote/`：任务中转、会话和 HTTP 控制器。
- `scripts/build-marginnote-addon.mjs`：将本机连接配置打入 `.mnaddon`。
- `tests/marginnote-*.test.ts`：原生宿主、传输与 agent 接口契约。
- `lisp/noema-marginnote.el`：`noema-marginnote-sync` 自有会话入口及其 pack
  解析。
- `lisp/tools/agent/acp.el`：`+agent-acp-pinned-config`，LaTeX 导出与同步共用。
- Emacs 的 `etc/noema/capabilities.json`：`marginnote-digitize` pack 定义；
  主 skill、引用和脚本仍在 README 技能库。
- `tests/elisp/noema-marginnote-tests.el`：pack 解析、作用域与同步会话
  （MN-C8、MN-C10–C12）；`+agent-acp-tests.el` 覆盖模型双向隔离。

## 已知差距

- **MN-G6：** skill 的局部布局与源移动复查已在真实批注跑通，移动建议与冲突规则有隔离测试。
  建议尚未接通通用的原生位置/关系写入与读回，也没有后台移动监听；大脑图只做预算保护，尚无真实规模基准。

- **MN-G5：** 官方 panel/eval → 本插件 onMessage → 原生返回已在现有 Noema 身份的临时面板实测。
  1.0.6 正式包、设备客户端、异步回执与去重已实现并通过针对性测试；原生安装器替换确认尚待完成。
  仍须核对实际加载源码、完整 exec、关闭重开，以及实际设备的暂停/模板行为。旧 host 尚未重启，
  不因新默认关闭旧监听就声称当前进程已无监听。

- **MN-G4：** 官方认证插件入口已在 Mac 上验证鉴权和临时面板原生回调，正式包与 iPad 尚待验收。
  经旧 Noema 通道的 Mac 坐标原型已在测试卡验证实际布局、落盘及模型 Undo；支持独立卡及
  子脑图直属卡；又在真实研究画布完成手写旁独立批注，官方源复查、原生 frame 回读、正文及关系元数据回读均通过。
  CLI 根画布存储坐标可直接写自由卡的 mindpos；frame 转换须减 size/2、减 offset、加 20000，不能重复加偏移。
  普通父子分支仍受自动排版控制，随字迹移动未实现。当前在线旧包仍使用
  MN-C5 的兼容通道。历史实验插件的已安装副本尚未确认卸载。
- **MN-G1：** Noema 插件 1.0.3 在 macOS 5.0.1 上的查询与手写读取已验证；iPad
  连接及取材仍未验证。插件通道固定在 `127.0.0.1`，iPad 只能走 Bridge。
- **MN-G2：** 5.0.1 上 Bridge 的读写路由、官方导出和页内留白写字已在 macOS 和 iPad
  验证，含带手写的插入留白；贴图等其他图层仍需每次视觉核对。插件命令执行、批注稿
  编辑器与 `/writing/*` 写入路由未验证。
- **MN-G3：** Noema 笔记导 PDF、批注后合并回 Markdown 的流程只有设计，手写到 Markdown
  位置的映射未实现；`workflow.py` 的增量快照未接新导出。步骤见 `marginnote-agent` 的
  `references/workflows.md`。

## 安装与调用

安全边界见[首次审计](../records/audits/marginnote-plugin-audit-2026-11.md)，
官方通道的实测、迁移与部署限制见[后续复核](../records/audits/marginnote-official-channel-2026-11.md)。
当前源码 1.0.6；安装和 host 重新加载以运行时版本、PID 与回执为准，不能由本节版本推断。

在 Noema 源码目录运行 `node scripts/build-marginnote-addon.mjs`，将
`build/marginnote/noema.mnaddon` 交给 MarginNote 原生安装器并启用。
默认包只有源码、面板、manifest 和图标，不含凭据。工具栏 N 或 CLI `open` 显示官方通道面板。
明确需要兼容旧通道才加 `--legacy` 构建，并在 host 启动前设置 `NOEMA_MN_LEGACY=1`；
该包读取活跃 host 私有描述符，内含本机令牌，不得提交或公开。

MCP 入口为 `plugins/marginnote/mcp.mjs`，工具是 `marginnote_open`、`marginnote_status`、
`marginnote_api`、`marginnote_execute` 和 `marginnote_result`。Noema 的全局
能力配置已声明 `noema-marginnote`，默认关闭；需要的项目在 `noema-mcp-manager` 中启用。其他 MCP 客户端使用 Node 启动该入口。
本地 agent 可调用 `node plugins/marginnote/cli.mjs --device mac open`、`status`、
`exec /path/to/code.js [args.json] [sessionId]`。官方凭据来自 `~/.config/mn-bridge/<device>/`；
客户端默认 mac，也支持显式 ipad。代码是函数体，例如：

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

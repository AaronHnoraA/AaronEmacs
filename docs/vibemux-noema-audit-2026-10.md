# 两个 Vibemux 与 Noema 的源码对照（2026-10-02）

审计固定在 [yoko19191/vibemux `efcf704`](https://github.com/yoko19191/vibemux/tree/efcf704daef899b2a8448e66396b9e7176350b88) 和 [UgOrange/vibemux `fc1b51a`](https://github.com/UgOrange/vibemux/tree/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f)。两者同名，但不是同一套实现：前者是 Tauri/Rust/Svelte 终端复用器，后者是 Go/Bubble Tea 的 agent CLI TUI。核查的是会话创建、输出路由、后台驻留、恢复、提醒、权限、配置和键盘导航的实际路径；图标、字体、纯样式与介绍性文字不作为行为依据。

Noema 对照边界：[`noema-agent-acp.el`](../site-lisp/noema/lisp/noema-agent-acp.el) 负责 ACP 会话，[`noema-agent-render.el`](../site-lisp/noema/lisp/noema-agent-render.el) 负责隐藏渲染，[`noema-sessions.el`](../site-lisp/noema/lisp/noema-sessions.el) 与 [kernel `session_names.go`](../site-lisp/noema/kernel/noema/research/session_names.go) 负责持久身份和注意力，[`noema-agent-inbox.el`](../site-lisp/noema/lisp/noema-agent-inbox.el) 负责跨项目总览，[`noema-agent-worker.el`](../site-lisp/noema/lisp/noema-agent-worker.el) 负责 Run、权限和通知；终端入口是 [`init-vterm-popup.el`](../lisp/init-vterm-popup.el)。Noema 的 Agent 窗口是 Emacs 内真正的 agent-shell buffer，不是终端转录副本。

## 功能与取舍

| 能力 | 两个 Vibemux 的实际实现 | Noema 当前状态与决定 |
|---|---|---|
| 多会话与项目切换 | yoko 使用同一 workspace 的 hot/warm 列表；UgOrange 的 runtime `sessions` 以 project ID 为键，同一项目同时只保留一个 PTY。见 [Rust manager](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/session_manager.rs#L50-L55)、[Go engine](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/runtime/engine.go#L39-L76)。 | Noema 已有同一项目多个命名 Session、真正的 agent-shell tab、跨项目 Inbox、abtop、并行 Run 上限；不引入第二个 PTY 会话注册表。 |
| 后台驻留与低成本呈现 | yoko 的 Warm 保留 PTY，卸载 xterm，输出进入 ring buffer；UgOrange 保留 PTY 和 50 KiB 历史缓存。见 [Rust 路由](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/session_manager.rs#L525-L567)、[Go session](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/runtime/session.go#L52-L66)。 | Noema `visible` 渲染策略让隐藏的 ACP 进程继续工作，回到窗口时刷出已缓存输出；自动停机还检查忙碌、排队 prompt、可见性和来源。终端快照语义不适用于结构化 ACP 消息。 |
| 提醒与未读 | yoko 生成输入/完成通知，并能跳到最近未读项；UgOrange 从去 ANSI 的 PTY 文本、OSC 9/777 和 bell 猜测事件。见 [yoko App](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/App.svelte#L169-L328)、[UgOrange watcher](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/output_watcher.go#L20-L132)。 | Noema 从 latest Run 与 `read_at` 派生 `permission`、`input`、`failed`、`unread`，不会因为打开或读过而消去失败。本轮吸收了跨项目 `!` 轮转待处理 Session 与 `u` 标记已读，走原有 session API。 |
| Agent 配置与权限 | UgOrange Profile 含命令、环境变量、自动批准等级；实际输出 watcher 在 `vibe/yolo` 两档对匹配文字发送 `y\n`。见 [Profile](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/model/profile.go#L9-L49)、[watcher](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/output_watcher.go#L66-L75)。 | Noema 已按 workspace/target 隔离进程环境，并用结构化 ACP 权限请求与项目策略判断。保留此边界，不从屏幕文字自动批准。若将来需要多个账号，应在现有 Agent 配置与 Remote 环境 capsule 层加显式 profile。 |
| 键盘控制 | yoko 的 prefix 导航和全局搜索是终端壳层；UgOrange 有 Control/Terminal 模式与网格焦点。 | Noema 已有 Emacs 命令、Agent tab 键和 Inbox；补上跨项目待处理导航即可，无需在 Emacs 中再建一套模态终端控制。 |
| 恢复与历史 | yoko 通过 xterm 快照/ring replay；UgOrange 进程内 PTY 缓存不会恢复已关闭会话。 | Noema 用 ACP `session/list`、`session/load`/`resume` 与原生 session ID 恢复对话，Run/Session 名持久登记。继续由 agent 后端保存转录。 |

## 继续逐路径核对

| 路径 | 上游代码确认 | Noema 对照和处理 |
|---|---|---|
| yoko 新建会话 | [`NewSessionPanel`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/lib/NewSessionPanel.svelte#L63-L198) 读取保存的 Profile 和运行能力，优先选上次使用的 Profile，调用 `session_create` 后记住选择。 | Noema Agent 由 ACP 配置、项目身份和 Remote 目标启动；不把 shell/SSH/WSL Profile 加进研究 Session。若做终端 Profile，应落在已有终端和 Remote 层。 |
| yoko 热/温状态与重放 | [`App`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/App.svelte#L412-L493) 处理 output、exit、park、replay、attention 等事件；[事件桥](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/events.rs#L63-L87) 每 12 ms 合批，连续的同会话输出合并。 | Noema 的 ACP 渲染已有隐藏缓冲与显示刷新；Inbox 吸收的是事件驱动且合并刷新这个交互原则，不搬 PTY 字节重放。 |
| yoko 命令面板 | [`SessionSearch`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/lib/SessionSearch.svelte#L85-L112) 按名称、cwd、终端标题筛会话，另带 AI thread 搜索和 `#` 聊天。 | Noema 的 Emacs buffer、Session 列表、Inbox、现有 AI 入口各有归属；未见需要复制的第二套聊天或终端索引。 |
| yoko 通知跳转 | [`App`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/App.svelte#L182-L328) 维护前端最多 80 条通知，按 session ID 跳转；输入/完成判断含 OSC 文本关键词。 | Noema 的 Session 注意力来自 kernel 的 Run、权限、输入和 `read_at`；已加入跨项目 `!` 导航及 `u` 已读操作。 |
| UgOrange 启动和状态 | [`Update`](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/update.go#L102-L204) 将 PTY output、start、stop 变成 tab 和项目列表状态；[engine](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/runtime/engine.go#L50-L77) 按项目 ID 复用会话。 | Inbox 现在监听 Noema 已有 ACP 变化、worker 结束和新增的待决请求计数事件；窗口隐藏时不查询，重新显示后补查。 |
| UgOrange Profile 更新 | [`Update`](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/update.go#L99-L134) 编辑 Profile 后会关闭并重启所有使用它的运行中会话。 | Noema 的活动 Run 不因设置编辑而隐式中断；不吸收这种重启语义。 |
| UgOrange 持久化 | [`JSONStore`](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/store/json_store.go#L71-L92) 把项目和 Profile 整体序列化后直接写 `data.json`。 | Noema 的 Run、Session 与权限由 kernel SQLite 和事务维护；不用该 JSON Store 替换持久层。 |

这里的“逐路径”指逐段追踪与会话行为有关的源文件和调用路径；没有把 CSS、字体、图标或重复的纯展示组件当作功能证据。上游桌面/TUI 的交互边界与 Noema 不同，代码问题除编译和测试结果外均标注为静态推断。

## 源码校验发现

1. **yoko 的 Warm 快照恢复有丢输出路径。** `parkSessionById` 先保存屏幕，后台 `route_output` 只缓存、不发 Warm 输出；`recall_session` 有快照时只发快照，只有无快照才回放 ring buffer。因此停车后至唤回前产生的输出在常见快照路径不会重放。见 [park](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/App.svelte#L808-L820)、[route](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/session_manager.rs#L533-L565)、[recall](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/session_manager.rs#L440-L501)。这是从代码路径得出的推断，尚未做桌面运行复现。
2. **yoko 的 Cold/Archive 描述尚未形成可访问历史。** 进程退出时 Rust manager 移除会话，前端也从 `sessions` 移除；完成通知的跳转要求该 session 仍在列表中。因此退出后的完成通知可能留下，但不能用它打开旧终端。见 [manager](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/session_manager.rs#L333-L343)、[App 退出和跳转](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/App.svelte#L304-L317)。不以 README 的 Cold 承诺作为 Noema 需求。
3. **yoko 的 replay `max_lines` 实际限制的是读取 chunk 数，且单个大 chunk 可以超过 `max_bytes`。** [`OutputRingBuffer::push`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/ring_buffer.rs#L40-L56) 用 `entries.len()` 而非换行数，字节上限在仅剩一个 entry 时停止淘汰。Noema 不复制这一缓存实现。
4. **UgOrange 的提示词状态由文本启发式猜测。** `reError` 的 `\berror:\b` 对通常的 `Error: message` 在冒号后空格处没有词边界；读循环在 256 个 chunk 队列满时丢最老消息，且历史缓存只有约 50 KiB。默认 Profile 是 `AutoApproveVibe`，实际匹配到命令确认行就写入 `y\n`。见 [watcher](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/output_watcher.go#L20-L75)、[session](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/runtime/session.go#L50-L130)、[Profile 默认值](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/model/profile.go#L30-L63)。因此通知和自动批准均不接入 Noema 的研究 Run。
5. **UgOrange 的设计说明与当前驱动有差距。** README 提多驱动和每项目多个会话，但 engine 总用 native driver、会话表按 project ID 复用。见 [engine](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/runtime/engine.go#L50-L77)。Noema 的对照只认实现，不按宣传项补功能。
6. **UgOrange 的标准 Go 验证目前不通过。** `go test ./...` 在 [`internal/ui/update.go:768`](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/update.go#L768) 被 vet 指出 `int` 转 `string` 得到单个字符而非十进制数字。这里的值来自 0–7 的修饰键编码，代码可能正是要生成单个数字字符；这是检查失败，不能据此断言运行时功能错误。`go test -vet=off ./...` 可编译，但各包均显示 `[no test files]`。
7. **yoko 的 AI API key 随普通配置写入 TOML。** [`AiConfig`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/config.rs#L136-L151) 包含 `api_key`，[`save_config`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src-tauri/src/config.rs#L393-L408) 将完整 `UserConfig` 序列化，并用 `std::fs::write` 写临时文件后改名；代码里没有显式限制文件权限。在常见 umask 下，新建文件可能允许其他本机用户读取。这是静态风险判断，未在目标系统检查实际权限。Noema 不吸收此存储方式或内建聊天配置。
8. **UgOrange 的项目/Profile 写入没有原子替换。** [`JSONStore.save`](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/store/json_store.go#L86-L93) 对同一个 `data.json` 直接 `os.WriteFile`；创建、编辑和删除操作都会调用它。进程或机器在写入期间中断可能留下部分 JSON，下一次加载会报错。Noema 的持久 Session/Run 继续使用现有 SQLite 事务。

## 本轮落地与验证

- [`noema-agent-inbox.el`](../site-lisp/noema/lisp/noema-agent-inbox.el)：`!` 跳到下一个有权限、输入、失败或未读信息的持久 Session；跨项目循环，跳过无注意力的行与错误行。`u` 对所选项目和 Session 调用已有的 `session:name:read`，成功后刷新总览。失败标记仍由 kernel 投影，读取不清除。
- [`noema-agent-worker.el`](../site-lisp/noema/lisp/noema-agent-worker.el) 与 Inbox：待决请求数量变化发出带 worker 的 hook；Inbox 的可见 ACP 事件只重绘，Run/请求事件在 400 ms 内合并为一次 Project 查询。隐藏期间不查询，重新显示后补查；查询仍用原有 Session API，保留过期响应隔离。
- [`noema-agent-inbox-tests.el`](../site-lisp/noema/test/elisp/noema-agent-inbox-tests.el)：覆盖跨项目循环、同名 Session 身份、事件合并、隐藏时不查询以及重新显示时补查。定向 ERT 7/7 通过。
- 上游 yoko `cargo test --lib` 为 22/22。Noema 本轮定向 ERT 7/7、`make research-test`、`make jupyter-test`、kernel `go test -tags fts5 ./...` 均通过。完整 `make test` 在与根配置测试并发的首轮有 3 个编辑器性能/布局测试失败；这 3 项单独复测通过，随后独立重跑完整 `make test` 为 2713 通过、16 跳过。
- `make build` 首次被同时进行的研究记忆改动挡在 TypeScript 检查：测试引用了新文件 `server/lib/research-memory.mjs`，当时尚无对应 `.d.mts`，报 TS7016。该声明随后由那组改动补齐；本轮未修改它，并在当前工作区重跑 `make build`、`make install`，均通过。构建仍报告原有字体路径和大 chunk 警告。
- 验证期间共享工作区又引入粘贴接口改动，曾使旧的 clipboard 断言与新参数不符，且 5 MB 拖选性能测试偶有耗时阈值失败。对应测试更新后，两组定向测试 15/15 通过；按当时最新树独立重跑完整 `make test` 为 2749 通过、16 跳过。相关粘贴实现和测试未由本轮修改。
- 未引入 Vibemux 源码或依赖。两份克隆用于审计，位于 `/tmp`；Noema 的宿主、Remote 和 ACP 边界保持原样。

## 后续吸收：定向更新

两个上游都把状态事件绑定到具体会话：[yoko 的 `sessionUpdated`](https://github.com/yoko19191/vibemux/blob/efcf704daef899b2a8448e66396b9e7176350b88/apps/desktop/src/App.svelte#L482-L493) 先取单会话快照，失败才回退到整个 workspace；[UgOrange 的 `SessionOutputMsg`](https://github.com/UgOrange/vibemux/blob/fc1b51a6c2c6b37b7dd6416914a138b680d7c65f/internal/ui/update.go#L159-L185) 更新指定 project 的终端和 tab。Noema 上轮虽合并事件，但任意一个 worker 事件会重查所有已知 Project；有 N 个 Project 时一次本地审批变化就会发 N 个 `session:names` 请求。

现在 [`noema-agent-inbox.el`](../site-lisp/noema/lisp/noema-agent-inbox.el) 根据 worker 的规范 Project root，只查询变化的 Project；同一合并窗口内重复事件只查一次，其他 Project 的行保留。`u` 标记已读成功后也只刷新所属 Project。全量 `g`、初次打开和隐藏后重新显示仍同步所有已知 Project；新 worker Project 会加入已知根。每个 Project 的请求另有序号，慢的旧回包不能覆盖更新的会话列表。该优化复用现有 Session API，没有改 host 协议，也没有按终端文本猜测状态。

本次追加验证：定向 ERT 9/9（含所属 Project 查询、重复事件合并、旧回包隔离），`make research-test`、`make jupyter-test`、kernel `go test -tags fts5 ./...`、`make build`、`make install` 均通过。完整 `make test` 为 2765 通过、16 跳过、1 项 5 MB 拖选性能阈值失败（p95 17.62 ms，阈值 16 ms）；该项单独复测 3/3 通过，且不涉及本次 Elisp 路径。

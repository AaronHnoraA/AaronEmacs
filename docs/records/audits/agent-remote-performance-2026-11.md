# ACP 与 Remote 的放置和性能检查

2026-11-03，Emacs 31，客户端 macOS，SSH 目标 Aaron-PC（Linux x86_64）。
本轮负责 ACP、agent-shell 与活动显示；Remote 连接、tramp-rpc 版本和 backend
适配由另一个会话处理。检查以当前工作树和真实进程的 route metadata 为准。

## 对照 VS Code 的公开契约

[Remote extensions](https://code.visualstudio.com/api/advanced-topics/remote-extensions)
把工作区扩展放在工作区所在机器，UI 扩展留在客户端；VS Code API 负责跨越这条
边界，绕开它直接使用主机 API 的扩展须自己处理放置。其剪贴板和 Webview API
固定在客户端，不能把远端 localhost 当作客户端。

这里对应的是 framework 的 placement、`remote-make-process`、逻辑文件身份和
`remote-channel`。ACP 的聊天和审批 UI 留在 Emacs，agent 进程在 workspace target；
本机也是一个 target。不能因 `file-remote-p` 对 local 的兼容答案不同而绕开执行契约。

[VS Code 文件监听 API](https://code.visualstudio.com/api/references/vscode-api#workspace.createFileSystemWatcher)
用事件和可释放的 watcher 表达工作区文件变化。Emacs 已有 framework watch 与
按 backend 代价触发的回合结束检查；本轮不再给 agent 面板增加文件轮询。
这只是 API 行为与责任边界的对照，没有测量 VS Code 客户端，也不宣称整体速度相当。

## 发现和修改

| 检查项 | 原来的行为 | 修改后的行为与证据 |
|---|---|---|
| local / SSH 进程入口 | ACP 按 `file-remote-p` 设置 `make-process :file-handler`；`/fs:local:` 回答 nil，本机进程没有 framework route 与调度 metadata | 在 ACP client 自己的 context buffer 内，把它唯一的一次 spawn 交给 `remote-make-process`；native、TRAMP、RPC 都有 `agent-shell` adapter、target placement 与 interactive class |
| 启动回调归属 | ACP 的启动依赖调用点的当前 buffer | 明确进入 client context buffer；其他 target 的 timer 不改变 cwd、环境或进程 owner |
| stdout 无 buffer | ACP 把输出交给 filter，进程没有正文 buffer；本机 agent 的原生 cwd 本身不声明 target owner | 仅在进程创建成功后给 context buffer 保留 `remote-buffer-target-id`；本机进程也可由框架识别归属 |
| 显式环境 | file handler 与 process plan 之间容易只留下 workspace capsule | ACP 的 `environment-variables` 显式传为 invocation overrides，保留首条重复变量优先、值中的等号和 unset；真实 fixture 检查 marker 与 cwd |
| CLI 查找 | agent-shell 在一次启动中构造三个 client，Claude/Codex 的底层 CLI 也随之重复查找 | 正向结果只在这次同步启动内复用；key 包含 buffer、cwd、环境、PATH 和程序名；缺失、环境变化、另一个 buffer 或下一次启动都重新查找 |
| 剪贴板/截图 | UI utility 是客户端进程，但远端工作区的输出参数是 `/fs:`，本机工具无法使用；其 cwd/PATH 也可能来自远端 buffer | 在客户端环境和原生目录内采集；client-accessible 目录直接写入，其他 target 经统一 `copy-file` 保存附件，临时文件在成功和失败时均清理 |
| Activity 单会话事件 | 每次变更重新列出全部会话并构造完整 snapshot | 只读取变更 buffer 的执行事实与显示字段；没有显示变化就保留原 activity 对象与起始时间，不重新发布 |
| Activity 定期核对 | 全量完整 snapshot 用于发现死亡 buffer 和 Run 取得会话 | 只核对最小忙碌事实，不查路径、磁盘或网络，也不构造 session snapshot；保留原有核对周期 |
| session 标题 | 重叠刷新各发一条 `session/list`，旧回包可把新会话标题改回旧值 | 按物理 session 合并在途读；新回合最多补一次。state、client、进程、session ID、cwd 任一变化即丢弃旧结果，也不覆盖查询期间已经更新的标题 |
| project 池查询 | 先构造所有项目的快照，再筛选一个项目 | 先按 target、规范根、管理类型选成员；其他 target 连 placement 查询也不做。只为候选成员构造完整快照，隐私筛选仍使用完整成员事实 |
| project 身份和回收 | 本机绝对根先在远端 buffer 中 expand，可能借用远端身份；暖回收仅为分组构造完整快照 | 先走 Emacs 路径来源边界，根发现与 pool key 均保持显式本机路径；回收直接使用规范 pool key，不读 prompt、usage 或请求明细 |
| project tab 与插件隐私 API | 排除其他项目后，每次 tab 刷新仍构造本项目所有完整快照；批量关闭也只为取得 buffer 而构造快照 | buffer 与 snapshot 查询共用候选筛选，tab/批量关闭直接取 buffer。新增只读 buffer 隐私谓词，Noema 仅据 origin/root 过滤；旧 snapshot 谓词保持完整参数，池与管理器统一应用两类过滤 |
| 精确 session API 与项目分页 | 已有 buffer/ID 的请求先读所有快照；列表也在全量快照后分页，status 再重复读一次自己 | 精确定位物理 buffer、校验 ID/隐私/项目边界；只读 status 复用已校验快照。列表先筛 buffer、隐私和页码，再构造返回项。无 root 的显式 target 也在隐私前筛选；变更操作之后仍读新状态 |

ACP 没有公开 process factory。兼容包装只拦截已锁定上游的 client 确切进程名、
同一个 context buffer、首次创建；backend 内部 spawn 和其他 buffer 的回调立即使用
原始 primitive。它不安装全局 `make-process` advice，不修改 package-vc 的包源码。
真实 stdio 测试检验这一私有 seam；上游升级后必须重跑这些测试。

## 测量

同一独立 batch Emacs 中，构造 20 个忙碌会话，对一个会话发送 1000 次 Activity
状态更新。修改前函数取自 `8a990e2a:lisp/tools/agent/activity.el`，修改后取工作树；
两者均先预热并 GC，再测量同一组会话。

| 实现 | 总时间 | 完整 snapshot 次数 | 重复发布 |
|---|---:|---:|---:|
| 修改前 | 1.278839 s | 20,000 | 0 |
| 修改后 | 0.009942 s | 0 | 0 |

这是单会话事件的 CPU/分配成本，约降低 129 倍；没有网络延迟或模型响应的含义。
CLI 测试以计数替身证明同一次启动的三次请求变成一次实际查找；它不是 SSH
往返耗时测量。两个独立启动、环境变化、不同 buffer 和缺失后恢复均另有断言。

按 project 测量：10 个项目各有 3 个 session，连续查询一个三成员项目 100 次，
修改前函数取自 HEAD 的 `+agent-pool-sessions`，修改后取工作树。同一 batch Emacs
预热和 GC 后：

| 实现 | 总时间 | 完整 snapshot 次数 |
|---|---:|---:|
| 修改前 | 0.256942 s | 3000 |
| 修改后 | 0.158721 s | 300 |

继续检查本项目 tab 后，用相同脚本分别启动独立的完整 batch Emacs：10 个项目各
3 个 session，每个 session 的最近 prompt 含 16 KiB 正文，加载真实 Noema 隐私
过滤器。预热和 GC 后，查询一个三成员项目的 tab 100 次：

| 实现 | 总时间 | 完整 snapshot 次数 |
|---|---:|---:|
| 修改前（已先排除其他项目） | 0.154941 s | 300 |
| buffer 查询与轻量隐私过滤 | 0.119909 s | 0 |

这是客户端查询的单次采样，耗时下降约 23%，不代表网络或模型速度。
没有增加成员缓存；新增、关闭、迁移项目或变更私有来源会在下一次查询立即反映。
自定义旧隐私谓词可以依赖 prompt、usage 等完整字段，因此配置这类谓词时仍为
候选成员构造快照，不能宣称任意第三方插件下都是零快照。

继续以同样的 30 个 session、16 KiB prompt 与真实 Noema 隐私过滤器测试 API。
修改前后分别启动独立的完整 batch Emacs，预热、GC 后各调用 100 次：

| 查询 | 修改前时间 / snapshot | 修改后时间 / snapshot |
|---|---:|---:|
| 精确 buffer/ID 的 status | 0.493339 s / 3100 | 0.017003 s / 100 |
| 单项目 list、limit=1 | 0.640532 s / 3000 | 0.147492 s / 100 |

这仍是客户端单次采样。列表需要读取候选身份与隐私来计算 total，未新增成员缓存；
旧 snapshot 隐私插件仍按兼容契约取得完整候选事实。测试也暴露并修正了 API
从远端 buffer 推断普通本机 root 和候选 buffer 目录的 target 时借用当前 target 的
问题；精确查询、列表及候选身份均先按 Emacs 路径来源规范化，再推断 target。

标题的最小复现中，10 次重叠调用原生刷新函数会发 10 条请求，切换 session 后
旧回包会覆盖新标题。修复后同一回合只发 1 条；有新回合时补 1 条，而非按事件数量
继续叠加。真实 fixture 延迟 100 ms 返回列表，并从目标侧收到的 JSON-RPC 日志确认
10 次刷新对应 1 条 `session/list`。这不省略不发送标题事件的 provider 所需的回退查询。

## 真实协议与验证

`test/fixtures/agent-flow-acp.mjs` 是确定性的真实 stdio ACP 服务，不调用外部模型，
不读取登录凭据。其 `runtime` prompt 返回进程真实 cwd、hostname、Node 可执行路径和
测试 marker。文件和事件日志只写随机临时目录，退出后清理。

```sh
make agent-remote-test
AGENT_REMOTE_TARGET=local AGENT_REMOTE_BACKEND=native make agent-remote-live
AGENT_REMOTE_TARGET=aaron-pc AGENT_REMOTE_BACKEND=tramp make agent-remote-live
AGENT_REMOTE_TARGET=aaron-pc AGENT_REMOTE_BACKEND=tramp-rpc make agent-remote-live
```

native 与两种真实 SSH route 均已通过。测试从实际 process 上读取 backend、adapter、
target 和 class，不能用“首选配置里写了 RPC”代替命中证明。同一份生命周期测试覆盖
初始化、cwd/显式环境、队列取消后挂起与显式恢复、agent 进程异常退出即时变为 dead、
文件改动路径回到 target、保存会话恢复。native 还检查 initialize、session/new 与
prompt 各只有一次；factory 重入、环境覆盖次序和活动更新有独立回归。
剪贴板采集使用替身 UI handler，真实启动本机 utility 并读写附件，再通过真实
SSH 文件 API 发布与读取目标图片字节；没有读取用户的真实剪贴板或交互式截屏。

后续按 session / project 增加的同一份真机用例，在 native、Aaron-PC TRAMP、RPC
上验证：同项目两个 ACP client 独立但复用同一 backend attachment；关闭一个不
杀另一个，也不影响另一项目。按项目停止工作只取消该池的回合，另一项目继续忙碌。
peer 的标题更新不污染 main，即使两个 client 使用相同原生 session ID。
后续用同一 stdio fixture 在 local 和临时 macOS SSH 的 TRAMP / RPC 各自实际运行：
两个项目保持 pending prompt，杀掉其中一个项目的 forward 只重建其资源；原有
ACP client、进程、session ID、backend attachment 与 target epoch 均保留。另一个
项目继续收到新的 `session/list` 响应，随后按项目停止也仅取消对应池。
这覆盖转发资源故障的跨项目隔离；真实 transport 失效后的 provider 会话恢复仍待验收。
复现可用 `TERMINAL_REMOTE_AGENT=1 python3 -B test/terminal-remote-loopback.py`，
测试后清理私有 sshd/key/config、目标 fixture 与 helper；无 provider 或模型调用。
ACP `fs/read_text_file` 读取编辑器未保存内容及指定行范围，磁盘仍保留旧内容；
`fs/write_text_file` 更新同一个 buffer 并保存到目标；缺失文件在各 target 都返回
协议的 `-32002`，没有偷读客户端同路径文件。

首轮 ACP、API、原生 stdio、队列、渲染、runtime 与 popup 回归为 211 项全部通过。
按 session / project 扩展到项目池、管理器、Activity 与 Pi 作用域后，336 项中
335 项通过，1 项原生终端 GUI 用例按条件跳过；另有下述 2 项既有 TTY 键位失败
在 HEAD 上复现后单列，没有把它们混入通过数。
两条旧测试仍把 RET 当作发送键，已按现有 C56 改为 S-RET；产品键位没有改变。
退出测试等待消费端收到 sentinel 事件后再检查中断事实，不把 OS 已报告退出、
Emacs 尚未派发事件的中间状态当作终态。`check-parens` 与限定到本轮文件的
`git diff --check` 通过。Activity 与 agent 配置的严格字节编译通过；该配置文件既有的
docstring 宽度告警被排除，其余告警仍按错误处理，产物只写临时目录并清理。
ACP 的完整字节编译另修正了两处 `declare-function` 的 CL keyword 形参声明；
该声明不能按 `cl-defun` 解析 `&key`，改为符合实际调用方式的 `&rest`。这不改变
运行时调用。ACP 编译排除既有 docstring/obsolete 告警，其余仍按错误处理。
API 精确查询与分页扩展后，专项 31 项通过；API、项目显示、Pi 管理域、生命周期与
窗口回归共 148 项，146 通过、2 项 GUI 条件跳过、0 意外结果。覆盖旧 ID 拒绝操作
新物理会话、普通同名 buffer 拒绝、私有会话不可查询、远端调用方保留本机 root、
显式 target 的跨项目列表、空页 total 与旧完整 snapshot 隐私谓词。
ACP/API 严格字节编译仍只排除既有 docstring/obsolete 告警，临时产物已清理。
`var/agent-api-scope-state-check` 的同步启动比较为 0 条差异；最终完整 Remote
回归的 16 组 ERT 共 646 项，643 通过、3 项条件跳过、0 意外结果。
项目 tab 的新回归验证真实成员增删和迁移、私有来源变化、旧谓词收到完整 prompt/
usage/进程字段、Noema 新谓词对 buffer 与管理器 snapshot 一致；既有测试继续覆盖
Pi 管理域和退役会话的队列/忙碌可达性。三种真实 route 均额外断言 tab 不读完整
snapshot、关闭 peer 后立即只剩 main。独立 `var/agent-project-tab-state-check`
启动快照比较为 0 条差异。
ACP、管理器与 Noema adapter 在加载其可选业务依赖后完成严格字节编译，仅排除
既有 docstring/obsolete 告警；管理器补全已有 float frame accessor 的函数声明。
这不等于 Noema adapter 在未加载 worker 时也能无告警独立编译。

全启动 state-check 同时跨过另一个会话的 tramp-rpc 0.13.1 → 0.16.0 迁移，产生
17 条旧状态、67 条新状态差异，涉及 TRAMP/RPC advice、feature、hook 和新增变量；
它不是本轮 ACP 的零差异证明。运行时的新 adapter、process class、owner 和临时
factory 行为由上述真实进程测试验证。

session / project 检查另取 `var/agent-title-state-check` 的独立启动快照；这次比较
只有另一会话新加的目录可访问性优化（1 条旧 hook、2 条新 hook/advice），未出现
ACP 启动状态差异。Activity 的两个 TTY 页面测试有既有 Evil 键位失败（`j`/`x`
被 normal state 抢占）；在独立进程加载 HEAD 的原始 ACP、Activity 与测试文件后，
同样两项失败，未算成本轮通过。批处理里的原生终端 GUI 用例按其条件跳过。

## 显式断开与协议回调

进一步检查 retained session 时，用真实 ACP fixture 复现：Remote 先设置 disconnect
标志并运行 buffer hook 后，两个 prompt 仍在原生自动发送队列，挂起数实际为 0。
原有显式断开测试只验证 synthetic sentinel 不发失败提醒，未覆盖这段资源关闭前
的窗口，不能据此认为整条退出链已经适配。

现在每个物理 session 在 buffer disconnect hook 里挂起 prompt，作废标题查询回调、
停止计时器、native idle/heartbeat，并退出 warm retention 的调度候选。旧 client
的 request guard 连同 initialize 都拒绝发送；明确重连创建新 state 后才可请求。
队列恢复与 structured prompt 入口也拒绝在已断开的 buffer 发送。snapshot 即时
显示 disconnected，活动回合为 interrupted，保留原生 ID、草稿、挂起区及未知结果。
进程的 sentinel 仍释放 client-ended 观察者，最后再取消可能被迟到回包重新安排的
idle/heartbeat；明确断开不报告为 provider 失败。

同一检查还补齐反向路径：timer 交付的入站请求只在仍存在、未断开且持有同一
state 的 buffer 执行，旧 buffer 已销毁时不落到任意当前 buffer；异步审批回调也
核对 disconnect 标志，不发送旧许可。真实 fixture 验证晚到审批没有写出 allow
响应，关闭后不再次调用 settle-pending broker。普通会话中的正常许可、拒绝、
队列、错误、恢复和 editor FS 操作仍通过。

`test/agent-lifecycle-remote-live.el` 追加完整 Remote target 断开：保持 pending
prompt 和待发队列，通过关闭否决保留会话，再明确原生重连。native、临时 macOS
SSH 的 TRAMP 和 RPC 都恢复同一原生 session ID，保留挂起项；目标侧 JSON-RPC
日志中 session/prompt 数量没有增加，证明重连未重放这些内容。以上为确定性 ACP
fixture，没有调用 provider 或模型；真实长会话与物理断网仍未验收。

最新消费者 ACP/API/生命周期/Eshell 合并回归为 133 项全部通过。额外的严格编译
不再排除文档宽度告警；Eshell、queue、Agent config、ACP 四个模块现已纳入
`make remote-byte-check` 的完整初始化 lane。整个 Remote gate 与启动状态差异
详见终端检查记录；后者保留了并行 backend 的三个 listing/completion advice 差异，
没有覆盖或重写基线。

## 仍需继续检查

- Aaron-PC 只有测试所需的 Node，未用真实 Claude/Codex provider 做远端长会话。
  fixture 的异常进程退出不等于半开 SSH 或物理网络中断，G13 保持开放。
- agent-shell 每轮结束保留一次 `session/list` 标题回退（重叠读已合并）；只有明确
  provider 的完整标题事件契约后，才可省去不同回合之间的独立查询。分页搜索仍
  沿用上游的首屏行为，不能以每轮遍历全部历史来换取标题。
- project 的成员筛选与生命周期隔离已经验证；继续检查多个真实 provider 在同一
  项目中的环境/工具探测和长期资源占用，不能把共享 backend attachment 当作完整
  的 project 资源共享验收。
- 继续核对远端文件上下文、图片/资源、工具调用与编辑器 API 的客户端/target 边界，
  以及 Noema host 只在客户端时的 G14；本轮通过不代表全部插件适配完成。

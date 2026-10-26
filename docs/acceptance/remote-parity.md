# Remote 开发能力验收矩阵

目标不是复制 VS Code 的内部实现，而是让 Emacs 在 `/fs` 语义下达到同等级的用户
结果：本地 UI、远端 workspace 计算、稳定文件身份、可恢复连接，以及本地与远端
边界清晰的扩展 API。

这也是仓库级开发门槛：Remote 框架是核心基础设施，本机作为 target `local` 参与
同一套 API 和验收。任何可能涉及 filesystem、project/workspace、process、LSP、
watch、service 或 channel 的能力，都不能先做一套 local consumer，再把 remote
支持留给未来补丁。

对照基线：

- [VS Code Remote Development](https://code.visualstudio.com/docs/remote/remote-overview)
- [Remote Development using SSH](https://code.visualstudio.com/docs/remote/ssh)
- [Supporting Remote Development](https://code.visualstudio.com/api/advanced-topics/remote-extensions)
- [Developing inside a Container](https://code.visualstudio.com/docs/devcontainers/containers)
- [Remote Tunnels](https://code.visualstudio.com/docs/remote/tunnels)

## 1. “完整”如何判定

每项能力分四级：

| 等级 | 含义 |
|---|---|
| API | 有稳定公共契约，消费者不需要解析 TRAMP 字符串 |
| local | `local` target 走同一抽象并通过真实运行测试 |
| remote | SSH/WSL/container target 有真实端到端测试 |
| resilient | 有超时、取消、重连、清理、诊断和故障注入测试 |

只有达到 `resilient` 才算完成。单独存在 struct、配置字段或 UI 按钮不算完成。

### 1.1 本地/远程同构门槛

每个共享能力还必须同时满足：

- consumer 对 `local` 和其他 target 调用同一公共函数、使用同一对象模型和清理
  路径；测试可以换 target fixture，但不能复制两套实现；
- consumer 不读取 `"local"`、`file-remote-p`、TRAMP method 或 backend ID 来决定
  placement、PATH、功能开关或降级；
- 普通本地 buffer 在框架外继续使用原生路径；进入 project/workspace/LSP 边界后，
  target `local` 与其他 target 都使用稳定 `/fs:` identity；
- backend capability 缺失要明确失败或由 route 选择另一 backend，不能悄悄落到
  client filesystem、进程或 localhost；
- 新增框架 API 同时有 native/local contract test、remote E2E 和生命周期清理测试。

LSP 额外要求 root、URI、server process、executable/environment、watcher、helper
service 与 channel 来自同一 owning workspace target。异步 callback 在别的 buffer
执行也不能改变 target；没有 watcher/helper/channel 的断线恢复测试，就不能达到
`resilient`。

## 2. 当前矩阵

| 能力 | 当前 | 完成标准 |
|---|---|---|
| 稳定 workspace/file 身份 | remote | backend/pipeline 切换不改变 buffer、project、LSP URI |
| 文件读写、目录、metadata | remote | 原生 API 全量契约、跨文件操作和错误语义一致；CSE 连续保存后的命名 ACL、inode、属主和权限已实测保持；经版本验证的原位写入跳过重复 ACL/SELinux 往返，小文件覆写中位耗时约 71→48 ms；属性可用性探测按连接换代缓存 |
| target HOME 与符号链接 | remote | Aaron-PC 上 SSH 与 tramp-rpc 已验证 `~/`、`~user/`、相对/绝对/断链、truename 与跨目录链接；仍需 WSL/container 回归 |
| 同步/异步 process | remote | cwd/env/executable 全由 backend execution 契约投影；`round-trip` 后端上 `remote-exec` 的 stderr 与 `remote-executable-find` 的 PATH 搜索各只用一次往返，QWD-VPS-Wiki 实测热态 `remote-exec` 约 3.86 s→0.23 s |
| PTY terminal | remote | CSE 与 Aaron-PC 真机上的 VTerm 已验证独立 `ssh-pty` 选路、workspace 跟踪、目标目录和输入输出；已有工作区中首个 PTY 不再建立 TRAMP 文件会话。RPC 传输或 PTY 进程故障后保留 disconnected buffer，工作区重连再手动重启终端已通过。frontend 断线契约（`:disconnect-function`、先判定后交给 frontend sentinel、重启失败保留旧终端、杀 buffer 不算断线）有故障注入单元测试；真实 Ghostel 已在 `local` 与 SSH target（QWD-VPS-Wiki，`ssh-pty`，往返约 250 ms）上验证断线保留与原名重启（`REMOTE_GHOSTEL_E2E_FAULT=transport`）。终端不为询问 shell 或加载 Ghostel 建立文件会话，该 target 上冷态打开约 15.2 s→0.12 s；仍需物理断网、WSL/container 回归 |
| 有序 transport pipeline | local | stage 准备、复用、健康、失败回滚和逆序释放 |
| SSH jump/multi-hop | local | 真机 ProxyJump、多 backend 共用 pipeline |
| WSL/container hop | API | `/ssh:host|docker:container:`、WSL 与 Podman 真机回归 |
| workspace lifecycle | local | open/reconnect/close 管理服务、任务、terminal、forward |
| workspace-side service | local | 探测、可信部署、版本协商、启动、健康、停止 |
| 环境与 remote settings | remote | user → target → workspace → tool → invocation 分层 |
| 文件 watch | remote | 真实 SSH inotify 事件、路径重写、workspace owner，以及主动重连后的原句柄与新事件已验证；批处理 Emacs 的 RPC 传输进程退出、回复被丢弃及受控 TCP 流量黑洞后，新监听事件均已验证；往返约 250 ms 的 QWD-VPS-Wiki 上，shell TRAMP 与 tramp-rpc 两条路由的递归 watch 事件送达与重连恢复均已通过：就绪等待按连接时限放宽，恢复与旧 watcher 死亡触发的 resync 互斥（此前两者在高延迟下竞态，重连后 watch 失败）；补长期运行与物理网络故障回归 |
| LSP/IntelliSense | remote | root/URI/server/cwd/env/watch/helper/channel 同属一个 workspace target；local/remote 同路由；受管 SSH 文件夹空闲时预热客户端 LSP 模块，Python 服务端初始化后异步预热首次补全，预热期间自动补全延后且结束后按原光标位置恢复；远程增量变更按顺序短时合批，请求前强制发送，CSE 模拟按键中位耗时约 1.25→0.83 ms；CSE 现有文件 Company 首次补全实测 959→48 ms；远端补全请求超过 1 秒时启用本地代码词补全，同时异步探活；持续无回复时按窗口限流重建 LSP 资源，CSE 故障注入已验证停旧服、新服预热及补全恢复；local 与 SSH 已验证主动重连后两个源码 buffer、诊断、补全及监听事件；RPC 传输进程退出、持续无回复及受控 SSH TCP 黑洞后均通过双 buffer 自动恢复；补 GUI 输入延迟、长期运行与更多语言服务端回归 |
| 搜索与 SCM | remote | Aaron-PC 无系统 rg 时可在可信目标的用户缓存部署固定版本 rg，真实 Consult 异步候选含未跟踪文件；不支持的目标回退 grep。Magit status、worktree 访问、stage/unstage 已有 SSH 真机回归，未显式打开 workspace 时访问源码也复用 `/fs:` buffer，本地 target 复用原生 buffer；仍缺其他目标平台与 commit/push 等真机回归 |
| tasks/tests | remote | `remote-task-run` 与命名 profile 在 local/CSE SSH 走同一 workspace process route；Compilation 输出与绝对错误路径回到 `/fs:`，任务可并发且随 buffer/workspace 关闭取消。CSE 的 tramp-rpc 和标准 TRAMP 真机均验证目标进程、普通子进程退出、失败码和错误跳转；RPC 传输故障注入已验证任务结果标为未知、workspace 自动恢复。仍需长时任务、真实物理断网和更多目标平台回归 |
| debug | API | Python 文件和模块调试的 Dape adapter 与集成终端已在 local、CSE 通过同一 `/fs:` target 的真实启动、入口暂停、堆栈路径映射、继续与输出验证；`python-file`/`python-module` 改用目标侧 stdio 连接，避免 `/fs:` 的本机端口误连；CSE 的 `python-attach` 通过目标侧 DAP 字节桥接实测连接已有 debugpy 环回监听、入口暂停、堆栈映射、继续和输出。其他语言及断线恢复仍需验证 |
| TCP/TLS client | local | SSH forward 上继续使用 Emacs 原生 network stream |
| port forwarding | remote | SSH `-L` 转发经面板创建、命名、复制、关闭，并可调整本地端口；占用端口的失败操作保留原转发。独立 SSH 连接避免 ControlMaster 重建竞态；Aaron-PC 真机已验证换端口后的 SSH banner、重连后端口与名称保留；补突发断线故障注入和访问策略 |
| remote listener/reverse forward | remote | native 回环与 SSH `-R` 真机动态端口已验证；补访问策略和断线恢复故障注入 |
| Noema Jupyter cells | remote | kernel/cwd/env、`.cell`、五通道与 widget asset 跟随 owning Target；`hb` 心跳覆盖三种 connector 的死亡检测，liveness 探测区分确认死亡与 `unknown`，Emacs 退出时回收 broker kernel；关闭或重启 kernel 即从 workspace 取下其 runtime 与五通道资源，重连不再为已死的 kernel 重开转发，重复关闭不再向旧进程组发信号。broker 生命周期已在真实 SSH + tramp-rpc 会话（本机回环 sshd）上走通：启动、心跳端口经转发可达、重连后 generation 递增且 kernel 存活、关闭后进程/资源/转发/连接文件全部回收。Linux SSH 目标验证了 broker 待关闭记录跨 Emacs 会话保存、真实 SIGKILL 后的远端进程回收，以及完整 ipykernel 的启动、执行、补全、五通道恢复后保留变量状态和关闭；Board 异步 kernelspec 发现也通过。中途不可达阶段仍仅有模拟状态，实际执行中途断线的故障注入未完成，因此未到 resilient |
| ACP agent（agent-shell） | remote | 所有 agent-shell 入口共用一条 placement：cwd 规范到 `/fs:`，进程经 `make-process` 句柄在 target 启动，可执行文件按 target PATH 查找，ACP 路径双向映射；local 与 CSE SSH 以 stdio ACP stub 实测 initialize/session/new、目标主机、cwd、显式环境变量与路径回映射；尚缺真实 agent 长会话、断线后会话恢复回归 |
| 批量传输 | remote | `remote-transfer` 的 rsync 任务（复制、同步、镜像、演练）在 `local` 上以真实进程验证异步启动、进度、失败状态、重跑和取消；无共享文件系统的 backend 经 `:client-shell` 的上传与下载以替身远程 shell 跑通真实 rsync 协议。真实 SSH（本机回环 sshd，密钥登录的临时机器）上已验证带空格文件名的上传、下载，以及同一 target 经 TRAMP 的目录列表；askpass 环境经真实 ssh 应答成功、错误口令干净失败。target 断开时任务被取消有故障注入测试。未验证：真实密码登录的服务器、Linux 远端主机（`REMOTE_TRANSFER_E2E_TARGET`）、传输中途断网，因此未到 resilient |
| tunnel | 模型 | 外部 Tailscale/FRP endpoint 可用；尚无托管 tunnel service |
| Dev Container lifecycle | 未实现 | 读取 devcontainer、build/create/start/attach/rebuild |
| 工具/“扩展”部署 | API | service manifest、版本锁、离线包、更新与回滚 |
| 认证与 workspace trust | API | provisioning trust gate；面板显示 SSH 认证、主机密钥、主机名和网络失败状态，以及首次建连的传输、登录、后端检查阶段；`L` 查看按目标保留的连接事件，`D` 查看异步 SSH 诊断，`T` 打开交互式登录终端；认证输入仍由 TRAMP 处理，尚无统一的认证提示 UI |
| 失败工况（不可达、半开、长时间断线） | remote | 处理矩阵见 [remote-framework.md](../design/remote-framework.md) 的“失败工况的处理矩阵”。Linux 容器 target 上暂停容器（主机不回包）实测：不可达时一条命令的等待约 117 s→一次时限，保持期内后续操作立即失败；断线超过重连预算后由异步探测等待，主机恢复后 workspace 与 watch 自动回到 open；重启主机后恢复。真实拔网线与跨网络切换仍未验证 |
| 自动重连/恢复 | remote | SSH 传输失败注入、真实 RPC 传输进程退出、客户端丢弃 RPC 回复，以及仅作用于测试连接的 TCP 中继丢包后，workspace、LSP 两个源码 buffer 和 watch 自动恢复；真实 tramp-rpc 会话上冻结服务端进程树形成的半开连接也已验证（命令在 RPC 读超时后失败、workspace 断开、下一次文件操作经新连接恢复）；受管 SSH 主连接和转发统一启用 OpenSSH 15 秒 × 3 次保活探测，首次自动重连默认缩短建连等待、后续重试保留原上限，目标可覆盖配置；补物理网络故障、认证过期与长期运行回归；terminal 明确手动恢复 |
| Remote Explorer/Doctor | remote | 面板可打开配置、当前和最近文件夹，按目标补全目录，显示连接阶段与端口，重连 workspace、管理转发；模式栏显示当前 target；`a` 可逐项加入 SSH 主机，`A` 可粘贴常见 SSH 连接命令，自定义文件通过 `-F` 贯通连接、进程和转发；`L`、`D`、`T` 提供连接事件、按目标诊断与登录终端 |

### SSH v1 已落地的基线

本轮把范围收敛在 native + SSH，而不是同时宣称 WSL/container/devcontainer 完成：

- `remote-pipeline`、`remote-session` 已是真实类型，旧 link/connection 仅为兼容 API；
- 配置 schema 为 v2，仍可读取 v1；同一 pipeline 的兼容 backend 定义会合并，
  transport/config 冲突会直接报错；
- file handler 使用可扩展的 Emacs 31/32 操作契约；未知写操作不重试；
- channel 保持原生 process/forward 返回值，同时附加统一生命周期描述；
- client/target 混合进程有显式边界：本地 UI proxy 由
  `remote-make-client-process` 启动，target stdio peer 由
  `remote-local-bridge-command` 接入，不依赖宽泛的原生 API advice；
- workspace 在 transport failure 后按 1/2/4 秒恢复；目前自动登记 environment、
  service、workspace-owned forward、watch 与 LSP resource，terminal 必须显式重启；
- `remote-doctor` 已能逐层检查并在 `Aaron-Pi` 上完成 Linux probe；
- `make remote-e2e` 已在真实 SSH target 上验证临时文件往返、远端 cwd、session
  复用和动态 SSH `-R` listener 数据往返。
- `make lsp-remote-live-smoke` 已从普通 `/ssh:` buffer 在真实 SSH target 上验证
  clangd、Pyright/JDTLS 的目标端启动与版本缓存、诊断、补全、typed Imenu、核心
  navigation/edit capabilities，以及 workspace-owned inotify 事件。
- WSL 上的 Lean 实链验证了本地 Node/HTTP proxy、远端 direnv/Nix capsule、
  远端 `lake serve`/Lean worker、Eglot 文档 URI 与本地 Infoview status 端点。

WSL/container/devcontainer、Dape/tasks 编排、dynamic SOCKS forward 和 managed
tunnel 保持为后续范围，不用 capability symbol 代替真机完成。reverse forward 与
remote listener 已有 API、native 数据面回环、SSH 命令/lifecycle 测试和真实
SSH target 动态端口回归，因此达到 remote；完成断线恢复故障注入前仍不算
resilient。

### 图形界面下的延迟基线

2026-10-12，QWD-VPS（往返约 184 ms，shell TRAMP 后端，clangd），独立的图形界面
Emacs 实例，完整初始化：

| 项目 | 结果 |
|---|---|
| 按键到重绘（40 次） | 中位 17 ms，p95 22 ms；输入在本地完成，不等待网络 |
| 补全请求往返（12 次） | 中位 250 ms，约一次网络往返加服务端 18 ms |
| hover 请求往返（12 次） | 中位 183 ms，即一次网络往返 |
| 启动后首次打开远程文件 | 约 14.5 s / 54 次往返（主机事实已持久化；此前约 20 s / 69 次） |
| 同一会话内再打开系统头文件 | 约 2.6~4.7 s |
| 冷态打开首个远程终端 | 窗口约 30 ms 出现，首个输出约 7 s（SSH 建连与登录 shell），之后的终端约 0.7 s |

首次打开文件的 54 次往返里约 24 次是 TRAMP 每条新连接固有的 shell 初始化与 PATH
计算；要再明显缩短，需要该主机走 tramp-rpc（标记为受信任）。

同日在同一主机上的真机回归：

- `make lsp-remote-gui-smoke`（`REMOTE_LSP_E2E_TARGET=QWD-VPS`，C）通过，输入探针中位
  0.94 ms，重绘 3.8 ms。
- 目标端没有 `inotifywait`：单文件 watch 经 Python agent 收到追加写与原子替换，
  不递归子目录；Emacs 退出后目标端 0 个 watcher 进程。
- 显式断开后重新注册的 watch：重连空窗内的写入由 `READY` 的变化时间补发为一次
  `changed`，此前这次写入不会有任何事件。
- 树 watch 丢失后恢复：空窗内新建和修改的文件各补发一次 `changed`，恢复后的实时
  事件正常，结束后目标端 0 个 watcher 进程。
- Noema 网关：目标端经 reverse forward 访问到网关健康检查，两次重连后仍是同一个
  forward、同一端口，停止后为 0。

### 仍按 target 名判断的位置

不变量要求 consumer 不按 `"local"` 选择行为。下面是 `lisp/remote/` 之外仍出现
`"local"` 的位置及其性质，改动前先看这张表：

| 位置 | 性质 |
|---|---|
| `tools/jupyter/config.el`、`tools/jupyter/management.el` 中的 `(remote-get-target "local")`、默认 owner、显示文字 | 身份与默认值，不是行为分支 |
| `tools/agent/config.el`、`core/aaron-base.el` 的报错与分类文字 | 显示用 |
| `noema/bridge/config.el` 的原始主机路径默认归属、`noema/jupyter/server.el` 的默认 owner | 客户端边界上的默认身份 |
| `term/shell/config.el` 的终端 buffer 命名 | 仅影响名称 |
| `noema/jupyter/server.el`：本机 server 不建转发 | 明文规定的策略（非本机 target 必须经 channel） |
| `tools/lsp/config.el`：非本机 target 的 watch 归 workspace 所有、shutdown 超时放宽 | 由测试固定的 target 身份契约；换成放置查询会改变契约 |
| `tools/jupyter/config.el` 的路由说明文字（“Direct from this machine”） | 显示用 |

已改为放置查询（`remote-client-file-name`）的：系统打开器（`core/aaron-open.el`、
`os/macos/config.el`）、Noema 给本机 web host 的路径（`+noema--host-file`）、kernelspec
REPL 的可达性判断、客户端 kernelspec 配置读取、Ghostel 的启动流程。Jupyter 管理
的 `+jupyter-management-local-target-p` 也是同一个放置查询。LSP 增量变更
合批改问 `remote-process-write-cost`（backend 声明的 `:process-write-cost`：
`direct` 或 `queued`），不再看 target 名。

## 3. Emacs 与 VS Code 的对应关系

微软的 [Remote Development FAQ](https://code.visualstudio.com/docs/remote/faq)
明确说明 Remote 扩展及组件目前并不开源，VS Code Server 也不授权给其他客户端单独
使用。因此这里对照公开的架构与用户行为，用独立实现和官方 VS Code 客户端做性能
基准；不把 Remote-SSH 扩展或 VS Code Server 的代码复制、翻译进 Emacs。

| VS Code 概念 | 本框架 |
|---|---|
| URI / remote filesystem provider | `/fs:TARGET:/path` file-name handler |
| Remote authority | target |
| SSH / WSL / container / tunnel composition | transport pipeline |
| VS Code Server connection | backend session |
| Remote Extension Host | workspace-side service 集合 |
| Remote window | remote workspace |
| Integrated terminal | routed PTY terminal |
| Forwarded Ports | remote channel / forward |
| Remote settings | target/workspace environment and settings layers |
| Workspace extension | service/tool registered for workspace placement |
| UI extension | 保持在本地 Emacs 的普通 package |

Emacs package 默认继续在本地运行。需要 workspace 文件、目标 OS 或工具链的计算，
通过 file/process/channel API 远端执行；只有高频、强状态或协议型功能才部署为
remote service。这保留了 Emacs 生态兼容性，也避免要求所有 package 改写成远端
插件。

[VS Code Remote SSH 官方文档](https://code.visualstudio.com/docs/remote/ssh)
明确把工作区扩展和命令放在目标侧 VS Code Server，UI 扩展留在客户端。本框架已经
通过持久的 tramp-rpc 会话和目标侧 LSP、watch、搜索进程承担这些热路径；目前
CSE 暖态 `/fs:` 打开源码相对直接 `/rpc:` 的配对中位数为 1.028 倍，本地
`/fs:local:` 相对原生为 1.037 倍。因此暂时没有测量证据支持再加入一层通用 Node
文件代理；这只是针对当前路径的设计判断，不是对 VS Code 整体速度的结论。
如果后续实测显示大量跨进程小 RPC 或某项服务需要长期状态，优先在已有
workspace service 契约下部署目标侧专用进程，并保持 `/fs:` 身份与重连生命周期。

## 4. 实施顺序

### P0：基础不变量

- `/fs` 身份与 native compatibility；
- target/pipeline/backend/session；
- process execution、环境隔离、错误分类；
- 进入 workspace/project/LSP 等框架边界的本地路径经过 `local` target；普通本地
  `find-file` 保持原生路径以维持第三方兼容。

### P1：可用远端工作区

- workspace open/reconnect/close；
- routed PTY terminal；
- SSH port forward 与 TCP/TLS stream；
- Eglot、watch、Magit、search 端到端验证；
- Remote Doctor 显示所有生命周期对象。

### P2：远端服务

- service manifest、版本和 capability negotiation；
- 可信安装、离线部署、升级回滚；
- 远端 watch/index/search/debug adapter；
- workspace 资源在重连后的恢复策略。

### P3：环境类型

- WSL；
- SSH host 内 container；
- Docker/Podman/Kubernetes attach；
- devcontainer lifecycle 与配置；
- Tailscale、FRP 和受管理 tunnel。

### P4：韧性与兼容

- 高延迟、断网、进程崩溃、半开连接和认证过期故障注入；
- 大目录、海量 watch、长时间 terminal/LSP/debug；
- 第三方 package 原生 API 兼容矩阵；
- 每项能力达到 `resilient`。

## 5. 禁止用假完成替代

- 不能因为某个 capability symbol 已注册就声称能力可用。
- 不支持的 channel 必须报错，不能落回客户端 localhost。
- pipeline stage 必须实际参与投影或生命周期，不能只保存在 JSON。
- service provisioning 必须有 trust gate、版本与清理。
- local 测试不能替代 SSH/WSL/container 真机测试。
- consumer 不能承担 backend 的 cwd、PATH、executable、连接或转发逻辑。
- consumer 不能用 `file-remote-p`、target/backend 字符串维护 local/remote 两套
  server、toolchain、watcher 或 feature policy。
- LSP URI 不能从 callback 当时的 current buffer 推断 target，必须绑定 owning
  server/workspace root。
- 没有登记到 workspace owner 的 watch/service/channel 不能宣称自动重连。

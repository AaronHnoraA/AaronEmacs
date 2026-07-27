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
| 文件读写、目录、metadata | remote | 原生 API 全量契约、跨文件操作和错误语义一致 |
| target HOME 与符号链接 | local | SSH/WSL/container 上验证 `~user`、相对/绝对/断链、truename 与跨目录链接 |
| 同步/异步 process | remote | cwd/env/executable 全由 backend execution 契约投影 |
| PTY terminal | local | SSH、WSL、container 真机回归；断线和 buffer teardown |
| 有序 transport pipeline | local | stage 准备、复用、健康、失败回滚和逆序释放 |
| SSH jump/multi-hop | local | 真机 ProxyJump、多 backend 共用 pipeline |
| WSL/container hop | API | `/ssh:host|docker:container:`、WSL 与 Podman 真机回归 |
| workspace lifecycle | local | open/reconnect/close 管理服务、任务、terminal、forward |
| workspace-side service | local | 探测、可信部署、版本协商、启动、健康、停止 |
| 环境与 remote settings | remote | user → target → workspace → tool → invocation 分层 |
| 文件 watch | API | 长期监听、断线重订阅、事件路径重写和去重 |
| LSP/IntelliSense | remote | root/URI/server/cwd/env/watch/helper/channel 同属一个 workspace target；local/remote 同路由，断线可恢复 |
| 搜索与 SCM | API | rg/git/Magit 在 target 执行，无本地路径泄漏 |
| tasks/tests | API | task registry、并发、取消、后台任务和结果模型 |
| debug | API | Dape adapter 在 target 启动；launch/attach/forward 可组合 |
| TCP/TLS client | local | SSH forward 上继续使用 Emacs 原生 network stream |
| port forwarding | local | SSH `-L`、jump host、关闭清理；真机验证和 UI |
| remote listener/reverse forward | remote | native 回环与 SSH `-R` 真机动态端口已验证；补访问策略和断线恢复故障注入 |
| tunnel | 模型 | 外部 Tailscale/FRP endpoint 可用；尚无托管 tunnel service |
| Dev Container lifecycle | 未实现 | 读取 devcontainer、build/create/start/attach/rebuild |
| 工具/“扩展”部署 | API | service manifest、版本锁、离线包、更新与回滚 |
| 认证与 workspace trust | API | provisioning trust gate；补 host key/auth 状态与交互 UI |
| 自动重连/恢复 | local | session 与自动资源故障注入；terminal 明确手动恢复 |
| Remote Explorer/Doctor | remote | 统一结构化状态与 SSH target probe；Explorer UI 继续扩展 |

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
  service 与 workspace-owned forward。watch/LSP consumer 尚未全部接入 resource
  owner，terminal 必须显式重启；
- `remote-doctor` 已能逐层检查并在 `Aaron-Pi` 上完成 Linux probe；
- `make remote-e2e` 已在真实 SSH target 上验证临时文件往返、远端 cwd、session
  复用和动态 SSH `-R` listener 数据往返。
- WSL 上的 Lean 实链验证了本地 Node/HTTP proxy、远端 direnv/Nix capsule、
  远端 `lake serve`/Lean worker、Eglot 文档 URI 与本地 Infoview status 端点。

WSL/container/devcontainer、Dape/tasks 编排、dynamic SOCKS forward 和 managed
tunnel 保持为后续范围，不用 capability symbol 代替真机完成。reverse forward 与
remote listener 已有 API、native 数据面回环、SSH 命令/lifecycle 测试和真实
SSH target 动态端口回归，因此达到 remote；完成断线恢复故障注入前仍不算
resilient。

## 3. Emacs 与 VS Code 的对应关系

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

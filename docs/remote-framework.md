# Remote 框架

这是一层对 Emacs 原生文件、进程与连接 API 的扩展。它保留 Emacs 的
buffer/file-name 哲学，以 `/fs:TARGET:/path` 表示稳定的逻辑文件身份；只有到达
操作系统或传输后端边界时，才投影为本地路径、TRAMP 路径或 RPC 路径。

公共入口是：

```elisp
(require 'remote-framework)
```

`init-remote.el` 只负责配置集成、UI 和启用 `remote-mode`。

## 1. 不变量

框架实现与后续扩展必须保持以下约束：

1. 进入 remote framework 的 `buffer-file-name`、`default-directory`、workspace
   root 和 LSP 文档身份使用 `/fs:`，不保存 `/ssh:`、`/rpc:` 等临时传输形式。
   普通本地 `find-file` 仍保留原生路径；只有 workspace/project/LSP 等框架边界
   才把本地路径规范化为 `/fs:local:`。
2. `fs://TARGET/path` 是对外 URI；`/fs:TARGET:/path` 是 Emacs 内部 file name。
3. `local` 是普通 target 的特例。框架内的 `/fs:local:/tmp/a` 在 native backend
   边界投影为 `/tmp/a`；框架外已有的原生本地 buffer 不被强制改名。
4. 文件操作继续调用 Emacs 原生 API。file-name handler 只处理 `/fs:` 上下文，
   不全局替换 `find-file`、`write-region`、`process-file` 等函数。
5. socket、端口转发等没有 file-name 参数的 API 使用显式 `remote-*` 入口；
   远端能力缺失时必须报错，绝不静默在客户端机器执行。
6. target-native path、逻辑 `/fs:` path 与 backend physical path 是三种不同值，
   不允许跨层混用。

逻辑语法的注册与 handler 启用彼此独立。因此只加载库即可解析和比较 `/fs:`
身份；只有 `remote-mode` 才安装实际文件拦截。

## 2. 对象模型

新 API 使用六个对象。`remote-pipeline` 与 `remote-session` 是真实结构和 registry；
旧的 `link` / `connection` / `link-plugin` 名称只保留为 v1 兼容入口。

| 对象 | 责任 | 示例 |
|---|---|---|
| target | 稳定的逻辑机器身份 | `local`、`aaron-wsl2` |
| pipeline | 到达 target 的有序传输链 | Tailscale → SSH → FRP |
| backend | 把一次操作适配给 Emacs 实现 | `native`、`tramp`、`tramp-rpc` |
| route | adapter + capability 的一次选择结果 | direnv 通过某 pipeline 的 RPC |
| session | target/pipeline/backend 的复用连接 | 一条已打开的 TRAMP/RPC 会话 |
| channel | stream、listener 或 port forward | TCP client、LSP channel、forward |

adapter 表示调用者及其偏好，例如 `emacs-file`、`process`、`exec`、`environment`、
`language-server` 和 `network`。`eglot` / `lsp-mode` ID 保留为兼容入口，但两种
客户端的实际启动都绑定到 `language-server` adapter。

```text
logical request
      |
   adapter + capability
      |
    target
      |
  pipeline [stage 1 -> stage 2 -> ...]
      |
    backend
      |
 pooled session
      |
 file / process / channel
```

pipeline 描述“如何到达”，backend 描述“Emacs 如何执行”。SSH、FRP、Tailscale、
jump host 和端口映射属于 pipeline stage；TRAMP 与 tramp-rpc 属于 backend。
两者不能再混成一个协议字符串。

## 3. 逻辑文件与原生 API

```elisp
(remote-canonicalize-file-name "/tmp/a")
;; => "/fs:local:/tmp/a"

(remote-file-name-to-uri "/fs:aaron-wsl2:/home/hc/a.el")
;; => "fs://aaron-wsl2/home/hc/a.el"

(remote-uri-to-file-name "fs://aaron-wsl2/home/hc/a.el")
(remote-file-name-target "/fs:aaron-wsl2:/home/hc/a.el")
(remote-file-local-name "/fs:aaron-wsl2:/home/hc/a.el")
(remote-expand-file-name "~/src" nil "aaron-wsl2")
(remote-file-equal-p left right)
```

`remote-file-local-name` 返回 target-native path，不表示该路径能由客户端 OS
直接访问。`remote-make-file-name` 只接受已经绝对化的 target-native path；
`~/src` 必须先经 `remote-expand-file-name`，由所选 backend 在 target 上解析
HOME。配置里的 workspace path 也走这条边界并缓存绝对结果，不能使用客户端
HOME 猜测远端身份。

Emacs 在咨询 file-name handler 之前就把裸 `~/` 当作客户端绝对路径，因此框架
不全局 advice `expand-file-name`。配置入口和需要表达 target HOME 的调用者使用
`remote-expand-file-name`；普通第三方 package 的原生 `expand-file-name` 行为
保持不变。`abbreviate-file-name` 也参与 `buffer-file-name` 的确定，因此 `/fs:`
handler 保持规范的绝对逻辑身份，不生成不合法的 `/fs:local:~/...`，也不把本地
buffer 身份意外降级成裸路径。确实只用于 UI 展示时，可显式缩写
`remote-file-local-name` 的结果。

符号链接保持 Emacs 原生区分：

- `file-symlink-p` 返回链接中原样保存的 target string，不改写为 `/fs:`；
- `make-symbolic-link` 保留相对 target；同 target 的逻辑绝对 target 只转换成
  target-native path，跨 target 的逻辑链接明确报错；
- `file-truename` 追踪链接后返回稳定的 `/fs:` 逻辑身份；
- lexical expansion 不追踪链接，`remote-file-equal-p` 也只比较逻辑拼写；
  需要 inode/链接等价性时继续使用原生 `file-equal-p`。

物理投影是 backend API：

```elisp
(remote-project-file-name logical-file route)
```

普通插件仍应使用 `file-exists-p`、`insert-file-contents`、`write-region`、
`directory-files`、`file-notify-add-watch`、`make-process` 与
`start-file-process`。`remote-fs` 在 `/fs:` 上下文内路由，再把结果中的物理路径
重新包装为逻辑路径；buffer 的 visited-file 状态不因 backend 切换而改变。

官方 `make-process` / `start-file-process` 的 cwd 是调用时的逻辑
`default-directory`，不是 workspace root。workspace 只提供资源与环境作用域，
不能改变原生进程 API 的目录语义。自定义调用可以用
`remote-make-process` 的 `:remote-directory` 显式表达同一边界。

文件 handler 的契约不是封闭常量。扩展可以登记路径参数、能力、返回值映射和
重试安全性：

```elisp
(remote-register-file-operation
 'example-operation
 :capability 'file-read
 :path-arguments '(0)
 :result-kind 'path
 :retry-safe t)
```

内建表覆盖 Emacs 31/32 的主要 file/directory/process/watch 操作。未知操作会写入
route log，并保守地按不可重试的 `file-write` 只执行一次。跨 target 的
`copy-file` / `copy-directory` 被允许；rename、硬链接和符号链接明确拒绝。

## 4. Pipeline 与 backend

注册有序传输链：

```elisp
(remote-register-pipeline
 "lab" "via-edge" '("tramp-rpc" "tramp")
 :stages
 '((:id "overlay" :transport "tailscale")
   (:id "gateway" :transport "ssh"
    :config (:host "edge"))
   (:id "tunnel" :transport "frp"))
 :config '(:host "lab")
 :priority 100)
```

主要 API：

```elisp
(remote-get-pipeline "via-edge" "lab")
(remote-pipelines-for-target "lab")
(remote-pipeline-stages pipeline)
(remote-pipeline-resolve pipeline adapter capability context)
(remote-route-pipeline route)
```

backend 负责这些边界：

- 逻辑文件名到 physical file name 的投影；
- target-native `~` / `~user` path 到绝对 localname 的展开；
- session 的 connect、liveness 与 disconnect；
- 命令、工作目录、环境和 executable 形式的执行准备；
- 可选的 network process、network stream 与 port forward；
- backend/transport/operation 错误分类。

```elisp
(remote-register-backend
 "example"
 :capabilities '(file-read process-sync)
 :project project-function
 :expand-localname expand-target-path-function
 :prepare prepare-execution-function
 :connect connect-function
 :live live-function
 :disconnect disconnect-function
 :program-form 'absolute)

(remote-backend-prepare-execution
 route context '("tool" "--flag") environment)
```

`remote-backend-execution` 同时保存 logical directory 和 physical directory。
`tramp-rpc` 声明 `program-form = absolute`；裸命令查找与目标 PATH 解析应在这一
backend 契约下完成，而不是由 direnv、Eglot 等消费者各自猜测。

同步、异步和官方 `make-process` 边界都消费这个 execution record。`/fs:` handler
转交到 `/rpc:` 或 `/ssh:` 后会重新允许物理 backend 的 TRAMP handler 接管；
tramp-rpc 的本地 relay 则固定在客户端临时目录运行，不能继承 target 的 cwd。
因此 Eglot、compile 和普通第三方插件看到标准 Emacs API，远端进程最终只收到
target-native cwd，例如 `/home/hc/project/`。

`remote-register-backend` 会把 backend 映射到旧 `remote-link-plugin` 兼容
registry，所以旧调用者可以渐进迁移；pipeline 与 session 本身已经不再是别名。

## 5. Session 生命周期

session 按 `(target pipeline backend)` 缓存，不按 adapter 或 capability 重复建连。
TRAMP/tramp-rpc 仍拥有底层 process；框架拥有身份、复用、健康状态和失效策略。
首次 backend 建连有框架 deadline；SSH pipeline 还会把 `ConnectTimeout` 与
`ConnectionAttempts` 注入该 route 的官方 TRAMP `login-args` 和 tramp-rpc raw
SSH args，避免一个离线 target 长时间阻塞界面。pipeline config 可以用
`:connect-timeout`、`:connection-attempts` 和 `:ssh-options` 覆盖默认值。

```elisp
(remote-session-acquire route context)
(remote-session-warm context adapter capability constraints)
(remote-session-invalidate route)
(remote-session-invalidate-pipeline pipeline-id)
(remote-session-list)
(remote-session-clear)
```

错误被分为三类：

- `backend`：当前 backend 不兼容，可在同一 pipeline 尝试另一个 backend；
- `transport`：pipeline 已失效，可换另一条 pipeline；
- `operation`：权限、退出码、参数等业务错误，不自动换路重试。

## 6. 进程、环境与网络 channel

```elisp
(remote-process-file "git" nil t nil "status" "--short")
(remote-make-process
 :name "worker"
 :command '("worker" "--stdio")
 :remote-context context)
(remote-executable-find "lake" context)

(remote-exec "uname"
             :args '("-a")
             :context context
             :adapter "exec"
             :check t)
```

`remote-exec` 返回 status、stdout、stderr、route、context 和 command。环境是按
`target@workspace` 隔离的 capsule；pipeline/backend 切换不会创建另一份环境。
direnv、Nix、语言工具链等通过 maintainer 或派生 layer 修改环境，不直接全局
修改 `process-environment` 和 `exec-path`。

语言服务器默认是 target placement。Eglot 和 lsp-mode 在启动前等待同一份
workspace 环境，随后通过官方 `make-process` / `start-file-process` 边界路由；
clangd、pylsp、typescript-language-server、rust-analyzer、texlab、bash-language-
server、Lean 的 Node/Lake 等都从 target PATH 或 workspace toolchain 查找。
客户端 UI helper 只有显式标记为 client placement 时才允许在本机运行。
Lean Infoview 的 proxy 端口文件按 Eglot 实例隔离；远端 HTTP 端口经
`remote-port-forward` 暴露给本地 xwidget，不能用项目级共享端口文件覆盖活动实例。

tramp-rpc backend 还包含当前 `msgpack.el` 的 large-map 兼容修饰：旧 encoder 在
环境 map 超过 15 项时会把二进制长度误传给 `unibyte-string`。direnv/Nix 环境很
容易超过该阈值，因此兼容逻辑由 backend 集中维护，消费者不截断环境。

Emacs 的 `make-network-process` 与 `open-network-stream` 没有 file-name handler
入口，因此使用显式 API：

```elisp
(remote-make-network-process
 :name "client"
 :host "127.0.0.1"
 :service 9000
 :remote-context context)

(remote-open-network-stream
 "client" buffer "127.0.0.1" 9000
 :remote-context context)

(remote-port-forward
 '(:host "127.0.0.1" :port 9000)
 :context context
 :local-endpoint '(:host "127.0.0.1" :port 0))

(remote-channel-of native-process-or-forward)
(remote-close-channel channel)
```

网络 API 继续返回 Emacs 原生 process 或既有 forward 对象，第三方软件无需认识
新的包装类型；框架把统一的 `remote-channel` 描述附在返回值上。native backend
实现 network client/server。TRAMP 与 tramp-rpc 可以通过所选
SSH pipeline 建立本地 forward，再实现 target 侧 network client/stream 与显式
`remote-port-forward`。远端 listener/reverse-forward 仍未实现，会明确失败；
绝不会错误地把 target 的 `127.0.0.1` 当成客户端的 loopback。

## 7. Workspace、service 与 terminal

workspace 是高于单次 buffer 的资源所有者，稳定身份来自
`target + workspace root`。它复用 route、environment、service、terminal 和
channel，并在关闭时按生命周期释放资源：

```elisp
(remote-workspace-open "/fs:aaron-wsl2:/home/hc/project/")
(remote-workspace-route workspace "language-server" 'lsp)
(remote-workspace-refresh-environment workspace)
(remote-workspace-ensure-service workspace "indexer")
(remote-workspace-register-recoverable-resource
 workspace 'watch watch
 :close close-function
 :recover recover-function)
(remote-workspace-reconnect workspace)
(remote-workspace-close workspace)

(remote-terminal-open workspace)
(remote-terminal-command workspace "default")
(remote-terminal-adopt workspace frontend-buffer
                       :metadata '(:frontend vterm))
(remote-terminal-restart disconnected-terminal)
```

service 是可选的 target-side tool 生命周期契约，支持 probe、trust-gated
provision、start/live/stop；它不是强制常驻的 VS Code Server。Eglot、direnv
等普通消费者仍优先直接使用 process/environment API。

`remote-terminal-open` 提供内建 comint frontend；`remote-terminal-adopt` 让
vterm 等 native frontend 保留自己的 module、filter、sentinel 与 UI，同时把
process/buffer teardown 登记到 workspace。配置层的 popup vterm 已走这条边界：
在任意 `/fs:TARGET:/path` buffer 中按 `C-c e`，会打开或复用同一 workspace 的
terminal，且不同 target/workspace 的 popup 池不会串线。本地也是
`/fs:local:` 的同一流程。

冷启动远端 vterm 只执行可缓存的 host facts 探测，用它解析远端账户真正的登录
shell（例如 bash 或 zsh）；它不会同步等待完整的 Nix/direnv capsule。shell
探测失败时按目标上的 `zsh` → `bash` → `sh` 顺序选择，最后才使用
`/bin/sh`。routed vterm 会截断自身的 TRAMP shell 二次探测，防止正确结果又被
覆盖。已有 capsule 会直接复用。本地 capsule 在 spawn 时传给进程，并在 vterm
mode 完成初始化后投影回 terminal buffer，避免在 vterm 临时绑定
`process-environment` 时制造 buffer-local 警告。
transport 断线时不会重放 shell 历史；vterm 保留为 disconnected buffer，显式
执行 `remote-terminal-restart` 会按原目录和 frontend 新建一个 vterm。

transport failure 会把相关 workspace 标记为 disconnected，并按 1、2、4 秒进行
自动恢复。environment、service、forward、watch 和 LSP 等登记了 recovery
function 的资源会在 session 恢复后重建。PTY shell 不安全重放，因此 terminal
只标记为 disconnected，并要求显式 `remote-terminal-restart`。

`M-x remote-doctor` 从 target → pipeline stage → backend → route → session →
workspace/resource 输出诊断；加前缀参数会实际连接并运行 `uname -s`。

## 8. 配置兼容

配置继续接受 `links`、`plugin`、`plugins`；新配置可以使用
`pipelines`、`backend`、`backends` 和 `stages`：

```json
{
  "version": 2,
  "targets": [
    {
      "id": "lab",
      "trusted": true,
      "workspaces": [{"id": "main", "path": "/home/me/project"}],
      "pipelines": [
        {
          "id": "via-edge",
          "backends": ["tramp-rpc", "tramp"],
          "priority": 100,
          "stages": [
            {"id": "overlay", "transport": "tailscale"},
            {"id": "gateway", "transport": "ssh"}
          ],
          "config": {"host": "lab"}
        }
      ]
    }
  ]
}
```

## 9. 模块边界

```text
remote-framework.el       public library entry
├── remote-core.el        target, adapter, capability, route
├── remote-pipeline.el    ordered reachability pipelines
├── remote-transport.el   pipeline stage executor/runtime
├── remote-backend.el
│   └── backend/          native, TRAMP, tramp-rpc implementations
├── remote-connection.el  real session pool + v1 compatibility API
├── remote-session.el     public session lifecycle facade
├── remote-fs.el          /fs identity and scoped file handler
├── remote-process.el     routed sync/async process APIs
├── remote-channel.el     socket, stream and forward boundary
├── remote-environment.el environment capsules and maintainers
├── remote-path.el        target-native host probing
├── remote-workspace.el   workspace identity and resource ownership
├── remote-service.el     optional target-side service lifecycle
├── remote-terminal.el    routed PTY terminal lifecycle
└── remote-doctor.el      structured diagnostics and optional probe
```

`remote-config.el` 与 `remote-board.el` 是可选集成层。direnv、Eglot、Lean、
Aaronnote 等消费者留在框架外，只调用公共 API。

## 10. 测试与支持范围

```sh
make remote-test
make remote-e2e
REMOTE_E2E_TARGET=Aaron-Pi make remote-e2e
```

SSH E2E 是显式 opt-in，自动选择 SSH config 导入的 `Aaron-*` target，也可以通过
`REMOTE_E2E_TARGET` 指定。它只在本地和 target 的 `/tmp` 创建随机目录，并在结束
时清理；覆盖文件复制/读取/枚举、target cwd 进程和 session 复用。

当前 v1 稳定目标是 native + SSH：逻辑文件、同步/异步进程、PTY、环境、SSH
forward、workspace/service 生命周期和原生开发工具兼容。WSL/container/
devcontainer、Dape/tasks 编排、reverse/dynamic forward、remote listener 与
托管 tunnel 不在这个版本承诺范围内。

## 11. 当前完成度

| 范围 | 状态 |
|---|---|
| `/fs` / `fs://` 稳定身份、local 特殊 target | 已建立 |
| 原生文件 API 的 `/fs:` scoped handler | 已建立可扩展操作契约与未知操作保守策略 |
| target/pipeline/backend/route 分层 | 已建立；pipeline 为真实类型，旧 link API 保留兼容 |
| session 池、健康与失效 | 已建立 |
| backend 执行准备契约 | 已建立；sync/async/官方进程边界均已接入 |
| native socket client/server | 已建立 |
| TRAMP/RPC network client 与 SSH port forward | 已建立；remote listener 尚缺 |
| pipeline stage 的实际逐段建连 | executor/runtime 已建立；内建 overlay/hop 主要负责 endpoint 变换 |
| workspace/service/channel/terminal 生命周期 | 已建立；自动资源恢复，terminal 手动重启 |
| Remote Doctor | 已建立结构化报告与可选 target probe |
| SSH 真机回归 | `make remote-e2e`，只使用随机临时目录 |
| WSL2 direnv + C clangd + Python pylsp | 已真实验证走远程环境与 tramp-rpc |
| Eglot/lsp-mode 统一 target placement | 已建立 `language-server` adapter |
| Lean Node proxy + Lake | 已接入可信 target 部署、远端 Node/Lake 与本地端口转发；待目标恢复在线后完成真机回归 |
| watch、multi-hop、断线恢复等长期回归 | 尚需继续补齐 |

当前已经可用于文件、环境、进程、LSP、terminal 和部分 channel 工作流。下一阶段
优先完善 managed FRP/tunnel stage、reverse-forward/remote listener、watch
一致性和断线重连；消费者继续只做环境或工具逻辑，不承担物理路径、spawn 形式和
连接生命周期。

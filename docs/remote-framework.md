# Remote 框架

这是一层对 Emacs 原生文件、进程与连接 API 的扩展。它保留 Emacs 的
buffer/file-name 哲学，以 `/fs:TARGET:/path` 表示稳定的逻辑文件身份；只有到达
操作系统或传输后端边界时，才投影为本地路径、TRAMP 路径或 RPC 路径。

公共入口是：

```elisp
(require 'remote-framework)
```

构建和测试命令通过 `remote-task-run` 在 owning workspace 的 target 上运行：

```elisp
(remote-task-run '("make" "test") :workspace workspace :display t)
(remote-task-register "test" '("make" "test"))
(remote-task-run-profile "test" :workspace workspace :display t)
```

在 Remote 面板的文件夹行按 `!` 可以输入一次性 shell 命令；源码 buffer 中也可
运行 `M-x remote-task-run-command`。输出是 Compilation buffer，`next-error`
会把目标机的绝对路径映射为同一 `/fs:` target 的源码。`M-x remote-task-cancel`
取消运行中的任务；关闭输出 buffer 或 workspace 也会取消。每次运行有独立结果
和退出码。目标需有 POSIX `sh`：它先上报 PID，再以原始 argv `exec` 目标命令，
不会重新解析 argv 中的 shell 元字符；交互式命令另由 `sh -c` 解释。
目标支持 `setsid -w` 时，每个任务有独立进程组；取消会向整组发终止信号，
因此普通子进程也会退出。其他目标回退为只终止顶层 PID；自行脱离进程组的
后台进程仍须由任务自行清理。随后清理本地 process；若启动后立刻取消，
编辑器立即返回，最多异步等待五秒取得 PID。任务不会在断线后自动重跑。
目标侧信号也异步等待确认，最多五秒；若传输不可用或确认失败，输出窗口明确显示
`Cancellation unconfirmed: target may still run`，不能将 relay 关闭当作目标任务退出。
若传输在任务运行中失效，Compilation 会显示 `interrupted, remote result unknown`，
不会把 relay 的退出码 0 误作目标命令成功；workspace 恢复后，在任务输出窗口按
`g` 即可用相同命令、工作目录和环境重新运行。运行中的任务须先取消，避免重复构建。
真实 SSH 验收命令是
`REMOTE_TASK_E2E_TARGET=host make remote-task-live-smoke`；RPC 断线故障注入是
`REMOTE_TASK_E2E_TARGET=host make remote-task-disconnect-smoke`。

`init-remote.el` 只负责配置集成、UI 和启用 `remote-mode`。

外部 helper 与 Emacs 的统一控制面是 `remote-gateway`。它复用本框架的 channel、
workspace ownership 与 reverse forwarding，使本地和 TRAMP/SSH target 使用同一套
JSON-RPC API；协议和注册方式见 [Emacs 通讯网关](emacs-gateway.md)。

## 0. 仓库级核心地位

Remote 是整个开发环境的基础能力层，不是“打开 SSH 文件时才用”的可选功能。
filesystem、project/workspace、process、executable/tool placement、environment、
watch、service、terminal、stream、socket 和 forwarding 的新设计，只要现在或以后
可能需要跨机器，就必须基于本框架。所属模块保留业务逻辑，但不得再建一套独立的
remote implementation。

本机统一建模为 target `local`。这意味着 local 与 remote 使用相同的 consumer
API、route 选择、资源所有权和验收标准；差异只在 pipeline/backend/capability 或
显式 client boundary。consumer 不得用 `"local"`、`file-remote-p`、TRAMP method
或 backend ID 决定产品行为。若公共 API 不能表达需求，应扩展框架并同时实现 native
与 remote backend，而不是在 consumer 内绕过。

这一核心地位不改变 Emacs 的 buffer/filesystem 哲学：普通本地 buffer 继续使用
原生路径和原生 API；只有进入 framework-owned project/workspace/LSP 边界时才规范
为 `/fs:local:`。框架通过 scoped file-name handler 和窄适配层扩展原生能力，尽量
享受 Emacs 与第三方 package 的后续改进。

## 1. 不变量

框架实现与后续扩展必须保持以下约束：

1. 框架拥有的 workspace root 与资源身份使用 `/fs:`。经 Remote 面板打开的文件，
   其 `buffer-file-name` 和 `default-directory` 保留 `/fs:`；直接经 `/ssh:` 或
   `/rpc:` 打开的文件保留 TRAMP buffer 身份，但 target、workspace、process、
   LSP 所有权解析到同一个逻辑 `/fs:` workspace。普通本地 `find-file` 保留原生
   路径；workspace/project/LSP 边界将其 target context 规范为 `/fs:local:`。
2. `fs://TARGET/path` 是对外 URI；`/fs:TARGET:/path` 是 Emacs 内部 file name。
3. `local` 是普通 target 的特例。框架内的 `/fs:local:/tmp/a` 在 native backend
   边界投影为 `/tmp/a`；框架外已有的原生本地 buffer 不被强制改名。
4. 文件操作继续调用 Emacs 原生 API。file-name handler 只处理 `/fs:` 上下文，
   不全局替换 `find-file`、`write-region`、`process-file` 等函数。
5. socket、端口转发等没有 file-name 参数的 API 使用显式 `remote-*` 入口；
   远端能力缺失时必须报错，绝不静默在客户端机器执行。
6. target-native path、逻辑 `/fs:` path 与 backend physical path 是三种不同值，
   不允许跨层混用。
7. consumer 不按 local/remote、TRAMP method 或 backend ID 分叉；物理差异由
   pipeline/backend/capability 投影。
8. connection、session、service、watch、channel 与恢复由 framework owner 管理；
   consumer 只登记资源意图和业务回调。
9. LSP 的 root、URI、server、environment、watcher、helper 与 channel 必须绑定
   同一个 owning workspace target，不能从偶然的当前 buffer 猜测。

逻辑语法的注册与 handler 启用彼此独立。因此只加载库即可解析和比较 `/fs:`
身份；只有 `remote-mode` 才安装实际文件拦截。

## 2. 对象模型

### 按工作区管理的异步来源 IO

`(require 'remote-source)` 提供 `remote-source-open`、`remote-source-request`
和 `remote-source-close`。来源属于一个已打开的 Remote workspace；helper 通过
`remote-make-process` 和 `source-files` adapter 在该 workspace target 上启动。
目标需提供 Node 26.5（使用内置 `path.matchesGlob`）；不安装额外 npm 包。

请求协议支持 `list`、`read`、`canonical`、`write`，文件名相对来源根目录。
`read` 返回内容和 SHA-256 版本，`write` 必须携带 `expectedRevision`，
新建使用 null。原地更新在相邻临时文件写入后核对版本并原子替换；这不提供
对任意外部写入程序的跨进程锁。拒绝符号链接和根目录之外的路径。
`extensions`、`exclude`、`hidden` 在遍历及监听入口过滤；单文件最多 16 MiB。

目录通知使用 `fs.watch`，没有轮询；请求超时 timer 由请求拥有并在结束时取消。
关闭来源或 workspace 释放进程和监听，拒绝未完成请求。workspace 恢复重建
helper 并忽略旧进程回执；来源请求自身不重新激活已退出的 workspace。
网关消费者还应在 `remote-gateway-client-disconnected-hook` 中释放客户端租约。

`make remote-source-test` 验证真实本机进程、版本冲突、排除、资源释放和恢复，
以及非本机 context 到进程边界的路由契约。Noema 的
`node site-lisp/noema/scripts/check-routed-agenda.mjs` 进一步验证真实 WebSocket
网关、目标 helper、Go 原生计算和 Emacs Agenda 操作。当前这些检查不等于真实
SSH target 的端到端验收。

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
(remote-client-file-name "/fs:local:/tmp/a.el")
;; => "/tmp/a.el"
(remote-client-file-name "/fs:aaron-wsl2:/home/hc/a.el")
;; => nil，除非所选 backend 明确声明客户端可直接访问
(remote-expand-file-name "~/src" nil "aaron-wsl2")
(remote-file-equal-p left right)
```

`remote-file-local-name` 返回 target-native path，不表示该路径能由客户端 OS
直接访问。需要给 client-side tool 传文件时使用纯 placement 查询
`remote-client-file-name`，不要用 target ID 判断 local/remote。
`remote-make-file-name` 只接受已经绝对化的 target-native path；
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

`~` 在 Emacs 里始终表示客户端 HOME，远端 buffer 也一样。为此
`remote-environment-apply` 把 target capsule 投影进 buffer 时保留
`remote-environment-client-owned-variables`（目前是 `HOME`）的客户端值：Emacs
展开 `~` 和 `getenv` 读的是 buffer 的 `process-environment`，投影 target HOME
会把 `~/x` 变成本机上不存在的 `/home/…/x`（伪本地路径）。capsule 本身仍保存
target HOME，路由进程显式应用 capsule，因此远端进程的 HOME 不变。

minibuffer 里的 `/fs:` 名字遵循 Emacs `substitute-in-file-name` 的重启规则：
`/fs:box:/a/~/x` → `~/x`（回到客户端）；`/fs:box:/a//fs:other:/x`、
`//fs:local:/…`、`//ssh:host:/…` → 该名字本身（切换机器）；
`/fs:box:/a//etc` → `/fs:box:/etc`（同 TRAMP 约定，仍在当前 target）。
因此 `file-name-shadow-mode` 与 vertico 的 tidy 能正常隐藏被重启的前缀。

符号链接保持 Emacs 原生区分：

- `file-symlink-p` 返回链接中原样保存的 target string，不改写为 `/fs:`；
- `make-symbolic-link` 保留相对 target；同 target 的逻辑绝对 target 只转换成
  target-native path，跨 target 的逻辑链接明确报错；
- `file-truename` 追踪链接后返回稳定的 `/fs:` 逻辑身份；
- lexical expansion 不追踪链接，`remote-file-equal-p` 也只比较逻辑拼写；
  需要 inode/链接等价性时继续使用原生 `file-equal-p`。

buffer 身份属于 Emacs，不属于 backend：`get-file-buffer` 先在逻辑名字空间里找
访问该 `/fs:` 名的 buffer，找不到再交给 backend 用它的物理写法回答（本机原生
buffer 借此成为 `/fs:local:` 名的别名）。只转发给 backend 会让访问 `/fs:` 名的
buffer 对 `get-file-buffer` 不可见；Treemacs 的 imenu 索引器据此把用户正在编辑的
buffer 当成临时访问并 kill 掉。

物理投影是 backend API：

```elisp
(remote-project-file-name logical-file route)
```

普通插件仍应使用 `file-exists-p`、`insert-file-contents`、`write-region`、
`directory-files`、`file-notify-add-watch`、`make-process` 与
`start-file-process`。`remote-fs` 在 `/fs:` 上下文内路由，再把结果中的物理路径
重新包装为逻辑路径；buffer 的 visited-file 状态不因 backend 切换而改变。

原生消费者的兼容修饰保持在最窄边界。例如 Emacs 32 的 Git backend 在状态输出
超过 `vc-dir-process-output-limit` 时假定调用者是 VC-Dir；Diff-HL 的 Dired
临时 buffer 不满足这个假设。配置只在 `diff-hl-dired-status-files` 调用期间取消
该限制，使 Diff-HL 得到完整状态，同时不改变全局 VC 设置、Dired 的 `/fs:`
身份或其他 Git/VC 调用。

官方 `make-process` / `start-file-process` 的 cwd 是调用时的逻辑
`default-directory`，不是 workspace root。workspace 只提供资源与环境作用域，
不能改变原生进程 API 的目录语义。自定义调用可以用
`remote-make-process` 的 `:remote-directory` 显式表达同一边界。
`remote-process-file` 也投影原生位置参数中的 `INFILE` 与 stderr 文件名；
stdout 的 buffer/string 目的地仍严格保留 Emacs 的 buffer 语义。

文件 handler 的契约不是封闭常量。扩展可以登记路径参数、能力、返回值映射和
重试安全性：

```elisp
(remote-register-file-operation
 'example-operation
 :capability 'file-read
 :path-arguments '(0)
 :result-kind 'path
 :filesystem-effects 'none
 :retry-safe t)
```

内建表按公开 file handler 契约覆盖 file/directory/process/watch 操作，而不按
Emacs 版本号分叉。未知操作会写入 route log，并保守地按不可重试的 `file-write`
只执行一次；若陌生返回值里仍含 backend 物理路径，则抛出
`remote-operation-contract-error`，不会把 `/ssh:` 或 `/rpc:` 身份泄漏给 consumer。
跨 target 的
`copy-file` / `copy-directory` 被允许；rename、硬链接和符号链接明确拒绝。

TRAMP 的 foreign handler 在不同上游修订中可能收到 filename，也可能收到已经解析
的 connection vector。`remote-compat-tramp-vector` 统一这两种入口；`/fs:` handler
同时承接 vector 形式的 home/uid/gid/groups 查询，并在调用物理 backend 前把逻辑
vector 投影成对应的 `/ssh:` 或 `/rpc:` vector。`file-local-name` 也作为正式 file
operation 实现，不再依赖全局 advice。`remote-file-operation-coverage-report` 会把当前
运行时 `tramp-file-name-handler` 的公开 operation surface 与本框架注册表对比；新增
上游操作必须先有明确 placement、capability、重试和结果契约，不能混进未知写操作。

`filesystem-effects` 取 `none`、`metadata`、`content` 或 `unknown`。只有调用者明确
声明 `none` 时，process/file 边界才局部关闭 TRAMP 的保守文件属性 cache flush；
未知进程和扩展操作继续刷新，不能用性能优化掩盖内容变化。

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
- 逻辑文件是否能由 client-side tool 直接访问的 placement 查询；
- target-native `~` / `~user` path 到绝对 localname 的展开；
- session 的 connect、liveness 与 disconnect；
- 命令、工作目录、环境和 executable 形式的执行准备；
- async `make-process` plan 与 client-local stdio bridge；
- client → target 大文件的 backend-native 批量传输；
- 可选的 network process、network stream 与 port forward；
- backend/transport/operation 错误分类。

```elisp
(remote-register-backend
 "example"
 :capabilities '(file-read process-sync)
 :project project-function
 :client-file-name optional-client-path-function
 :expand-localname expand-target-path-function
 :prepare prepare-execution-function
 :prepare-process prepare-async-process-plan-function
 :stdio-bridge client-stdio-bridge-function
 :probe protocol-capability-function
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

同步、异步和官方 `make-process` 边界都消费这个 execution record。异步 placement
由 `remote-backend-process-plan` 表达，公共 process 层不检查 `"local"`、
`"tramp"` 或物理 TRAMP method。`remote-local-bridge-command` 同样只分派
backend 的 `:stdio-bridge`，因此 WSL/container 等新 backend 不需要修改公共 API。
`/fs:` handler
转交到 `/rpc:` 或 `/ssh:` 后会重新允许物理 backend 的 TRAMP handler 接管；
tramp-rpc 的本地 relay 则固定在客户端临时目录运行，不能继承 target 的 cwd。
因此 Eglot、compile 和普通第三方插件看到标准 Emacs API，远端进程最终只收到
target-native cwd，例如 `/home/hc/project/`。

`remote-register-backend` 会把 backend 映射到旧 `remote-link-plugin` 兼容
registry，所以旧调用者可以渐进迁移；只有这个桥接生成的 plugin 才参与 backend
probe，同名旧 plugin 不会意外继承新协议。probe 在连接打开后协商实现版本、协议、
watcher 与 capabilities；`incompatible` 和 `unsupported` 是结构化 condition，失败只
冷却当前 backend，仍可在同一 pipeline 上回退到标准 TRAMP。

capability 不是标签，而是可检查的承接接口。`remote-capability-contract-list` 描述
file/process/channel/composite 边界及其 callback 要求；backend 注册时若声明
`metadata` 却没有 path projection、声明 listener 却没有 network callback，或声明
LSP 却缺少 process/environment 基础能力，会立即得到
`remote-operation-contract-error`。adapter 同样不能登记未知能力。连接后的动态 probe
只能缩减静态 capabilities，不能凭空增加实现未声明的能力。

## 5. 加速与升级防线

`remote-compat.el` 是 Emacs/TRAMP 演进的唯一兼容边界。框架优先使用公开的 foreign
handler、external operation 和 file-notify API；旧版本所需的窄 fallback 只存在于
这个模块，其他模块不读取 TRAMP 的 operation 定位变量，也不直接删 file-notify
内部 descriptor。

防御按函数形状和行为做，不按 Emacs/TRAMP 版本号做。external operation 必须是与
原函数公开 arglist 完全一致的命名函数；例如 `dir-locals--all-files` 新增可选参数后，
旧 tramp-hlo 不承接该形状，框架只对这次调用回落到标准 TRAMP，而不是关闭整个加速
能力。tramp-rpc 的少数私有 seam 仅存在于 backend adapter，安装 advice 前验证
arity；形状改变时相应优化自动停用，文件、进程和 channel 主路径仍可继续工作。

tramp-rpc 的文件元数据按“每个用户操作的往返次数”设预算，对标 VS Code Remote 的
stat + read / stat + write。`remote-backend-tramp-rpc-metadata.el` 在
`find-file-noselect` 与 `basic-save-buffer` 外包一个文件操作作用域：作用域内第一次
元数据未命中，用一个 `batch` 同时取该文件的 stat、lstat、truename 以及父目录的
stat、lstat，并写入 tramp-rpc 自己的缓存；TRAMP 为数据安全强制 fresh 的 stat
（`remote-file-name-inhibit-cache` 为 t）在同一作用域、该文件未被写入之前复用这份
结果；visit 的 modtime 取自读之前的 stat，因此并发写入最多造成一次误报的
“changed on disk”，不会掩盖更新的磁盘内容。写入会使作用域遗忘该文件，写后的
modtime stat 仍是真实往返。服务端并发执行 batch 条目，所以 read/write 从不并入
batch。`locate-dominating-file` 结果在 tramp-rpc 元数据 TTL 内复用，只有可能增删
marker 的失效（同名 marker、目录/子树/连接级 flush）才清空。实测 Aaron-PC
（RTT≈5ms）：热打开 6→2 次往返（约 155→90ms），保存 8→3 次往返。该适配只在已验证
release 与私有 seam arity 完整时安装，`remote-backend-tramp-rpc-metadata-report`
给出计数器。

程序化 `write-region`（`with-temp-file`、Noema 原子保存）在没有外层作用域时自建
作用域：`write-region` 先问 truename，这次未命中也触发同一个 batch；batch 发现文件
不存在时直接写入 truename 缓存（没有文件就没有符号链接可追，答案就是名字本身），
省掉一次必然失败、还会打印 “File is missing” 的 `file.truename`。原地改写已存在的
普通文件不改变父目录自身的 mode 与 mtime，所以失效后保留父目录的 stat（目录列表
照常失效）；已验证的原地写入也不再 chown，属主本就未变，新文件保留服务端默认属主
与组，与本机 Emacs 写入一致。实测程序化写入 5→2 次往返。

`lock-file` 在第一次修改时检查文件是否被外部修改。buffer 的 auto-revert watch
有效且自上次精确 stat 证明未变之后没有收到事件时，`remote-fs-notification-proves-unchanged-p`
直接回答“未变”，保存后第一次按键不再同步等一次往返；保存时仍然精确 stat，
正在路上的事件会在那里被拦下。

`remote-fs` 为一次文件操作投影 `default-directory` 时，只在它与被操作文件同属一个
target 时才投影；远端 Dired/notebook buffer 里访问本机文件（或反过来）不再报
“Native backend cannot access target”，也不会给进程编造另一台机器上的工作目录。
`remote-compat` 让 TRAMP 的 `tramp-signal-hook-function` 不记录 `file-missing`：
清理、探测类调用在 `ignore-errors` 下得到“文件不存在”是正常答案，不应在关闭
notebook、kernel 或连接时刷满 *Messages*；未处理的错误照常由命令循环报告。

`remote-accelerator.el` 提供按 operation + route 选择的可选 provider。目前接入 GNU
ELPA `tramp-hlo` 的三个高层操作，但不调用它的全局 `tramp-hlo-setup`：

- 只加速 `/fs:` 投影到 shell TRAMP 的 route，RPC 继续使用自己的实现；
- 每个 target/operation 探测所需命令；探测失败立即走原生 TRAMP；
- `locate-dominating-stop-dir-regexp` 非空时不用上游快路径；
- dir-locals 额外验证 GNU `stat -c` 和 `realpath` 的路径保持性，避免 macOS
  `/var` → `/private/var` 改写；
- provider 抛出 `remote-backend-unsupported` 时，在同一路由内回落到普通实现。

升级验收入口：

```sh
make remote-contract-test  # operation/provider/probe/file-notify 契约
make remote-conformance-test # /fs:local 与原生 API 的语义 oracle
make remote-byte-check     # warnings-as-errors，产物仅写临时目录
make remote-check          # 全套 Remote 与 consumer 回归
```

`remote-doctor` 同时报告当前 Emacs/TRAMP API、加速器可用性、已协商 backend
contract，以及 tramp-rpc 是否处于干净的精确 release tag；非 release checkout
不禁用 RPC，而是保持 upstream 的 source-keyed build 策略，避免 client/server
二进制错配。

代价要说清楚：source-keyed 策略只接受本机构建的 server，因此 aarch64-darwin
客户端无法为 x86_64-linux target 产出二进制，tramp-rpc route 每次都失败并回退
TRAMP。这类"本机永远造不出该 target 的 server"被 backend 归类为
`incompatible` 而不是可重试失败，否则每 30 秒 cooldown 过后都会重开一次
bootstrap 连接并重试一次注定失败的构建，远端每个操作都要付这份钱。修复方式是
把 checkout 放回 `package-lock.el` 记录的 `:last-release`（见
[maintenance.md](maintenance.md) 的排查条目），恢复后 release 二进制对每种
架构都能直接下载部署。

当前实现持续对照的上游设计来源如下：

- [GNU TRAMP](https://www.gnu.org/software/tramp/)：以运行时 operation inventory、
  foreign handler 与 external operation 公共契约作为 Emacs 文件能力的真值；
- [tramp-rpc](https://github.com/jdtsmith/tramp-rpc)：借鉴 filename/vector 归一化、
  client/server 同版本部署和 cache invalidation 测试，不复制它已经覆盖的架构映射；
- [Distant](https://distant.dev/)：借鉴 server/protocol/capability 分离及 typed
  unsupported 思路，不引入另一套协议或 vendored server；
- GNU ELPA `compat` 与 `tramp-hlo`：前者只承担窄 API 兼容，后者只作为可回退的
  route-scoped 高层加速器。

sshfs/rclone 可以解决部分 mount/file transfer，但不能承接 Emacs process、PTY、
watch、LSP stdio 和 network/forward 接口，因此不作为 primary backend，也不据此
把统一 capability 模型拆回文件专用分支。

## 6. Session 生命周期

session 按 `(target pipeline backend)` 缓存，不按 adapter 或 capability 重复建连。
TRAMP/tramp-rpc 仍拥有底层 process；框架拥有身份、复用、健康状态和失效策略。
pipeline runtime 引用采用 take-before-release 所有权；即使 transport connect
让出事件循环期间发生配置重载或取消，也只能由 opener/invalidation 其中一方释放。
首次 backend 建连有框架 deadline；SSH pipeline 还会把 `ConnectTimeout` 与
`ConnectionAttempts` 注入该 route 的官方 TRAMP `login-args` 和 tramp-rpc raw
SSH args，避免一个离线 target 长时间阻塞界面。pipeline config 可以用
`:connect-timeout`、`:connection-attempts` 和 `:ssh-options` 覆盖默认值。

每次路由操作在复用池中的连接前都会校验 pipeline，而校验一条 OpenSSH master
就是在本机 fork 一次 `ssh -O check`。那一次 fork 约 7ms，比它守护的那次远端
操作贵一个数量级，而一次 `find-file` 会走 60~190 次路由操作。因此一个刚刚答过
的 master 在 `remote-transport-ssh-control-check-interval`（默认 2 秒）内直接复用
上次答案；连接建立时清理陈旧 socket 会强制重问。master 在这个窗口内掉线时，操作
失败一次，框架把它归类为 transport failure 并重连——这与 master 在操作中途死亡
走的是同一条恢复路径。实测 `file-exists-p` 在远端从 8.0ms 降到 0.53ms，一次远端
`find-file` 从 0.67s 降到 0.19s。

每次连接尝试还获得单调递增的 generation。backend protocol contract、高层加速
probe 都按 generation 缓存；session 关闭时统一失效该 target 的 HOME/path expansion、
PATH facts 和 environment capsule。重连因此不会复用上一条连接观察到的 server
version、watcher、realpath 或 shell 环境。

PATH facts 由 `remote-path--probe-script` 在 `sh -lc` 中探测，其中 PATH 取自
target 用户的 POSIX 登录 shell（`$SHELL` 为 bash/zsh/ksh/dash 时执行
`$SHELL -lc`，启动输出用 marker 丢弃），失败或非 POSIX shell 时回落到 `sh -l`
的 PATH。`local` 与远端走同一脚本：macOS 上 Homebrew 等 PATH 通常只写在
`~/.zprofile`，由 launchd 启动的 GUI Emacs 不会继承它，只读 `sh -l` 会找不到
`/opt/homebrew/bin/jupyter` 这类工具。

timer 驱动的 PATH、environment 与 workspace reconnect 统一经过
`remote-background-submit`。相同 logical key 的任务只运行一个；若 TRAMP 正忙则有界
退避并加入 jitter；函数执行期间 target epoch 变化时，结果不会写进 cache，而是从
新 epoch 重做。PATH/environment 的 cache commit 被刻意放在 generation 检查之后，
workspace 关闭和 framework reset 会取消仍在等待的任务。

```elisp
(remote-session-acquire route context)
(remote-session-warm context adapter capability constraints)
(remote-session-invalidate route)
(remote-session-invalidate-pipeline pipeline-id)
(remote-session-list)
(remote-session-clear)
```

### 显式断开与 TRAMP 清理

断开分两种语义，框架必须区分：

- **transport failure**（网络掉线、ssh 被杀）：workspace 标记 disconnected 并按
  1、2、4 秒自动恢复，资源随之重建。
- **显式断开**：`remote-board` 的 disconnect、`M-x tramp-cleanup-connection` 与
  `tramp-cleanup-all-connections`。三者都走 `remote-workspace-disconnect-target`：
  先关闭该 target 的 workspace（取消挂起的重连任务，LSP/service/watch 在连接仍在时
  优雅关闭），关闭该 target 的文件、Dired、任务、终端及通讯 buffer，最后失效全部
  session、关闭底层传输。其它 target 的 buffer 不受影响。

未保存的文件或可编辑 buffer、以及 kill query 拒绝关闭的 buffer 会保留，并在
清理结果中列出名称，不自动保存或丢弃内容。保留的 buffer 关闭 auto-revert，设置
`remote-buffer-disconnected-p`；排队的 LSP startup/idle callback 必须尊重此标志。
direnv 取消该 buffer 的刷新和 export waiter，已排队的 retry/completion 在发现 `.envrc`
之前检查标志；否则保留的未保存文件会通过 `locate-dominating-file` 重新建立连接。
Copilot 取消该 buffer 的延迟启动并关闭其 mode，不停止服务其它 buffer 的共享客户端。
用户显式执行 `my/language-server-ensure`、`my/lsp-mode-ensure` 或
`direnv-update-environment` 可恢复该 buffer 的自动启动。
返回值包含 `:workspaces`、`:sessions`、`:buffers` 计数和 `:kept-buffers` 名称列表。

`remote-buffer-target` 只根据文件/Dired 的逻辑身份、routed process 的 owner，以及
显式远端目录判断归属，不访问文件。`remote-make-process` 在 stdout/stderr buffer
上保留 `remote-buffer-target-id`，因此进程退出后仍能回收使用本机临时目录的输出
buffer；普通 scratch buffer 的本机目录不意味着它属于需要关闭的 workspace。

TRAMP 清理命令是 TRAMP 兼容边界上的用户入口，由 `remote-backend-tramp.el` 以
around advice 桥接：按 session 保留的物理 handle、workspace route 或已配置的
pipeline 匹配 target，因此 session 已清空后也能再次关闭先前保留的文件 buffer。
清理期间绑定 `remote-backend-tramp-explicit-cleanup`。
`delete-process` 会同步触发 sentinel，tramp-rpc 的 transport-death 观察者看到该
标志后不再上报 failure，否则清理会在几秒后被自动重连撤销。
内部 backend disconnect（失效 session、切换 backend、transport recovery）同样绑定
该标志，但跳过整个 target 的用户级清理，恢复过程不会因此关闭编辑 buffer。
TRAMP 的 keep-debug/keep-password/keep-processes 内部清理调用也保持 session 语义。
目标清理最后运行 `remote-target-disconnect-hook`，backend 根据 pipeline 的物理投影
回收未进入 session pool 的旧 TRAMP 连接；buffer 消费者通过
`remote-buffer-disconnect-hook` 停止诊断和补全探测，资源仍由 workspace 正常关闭。

`file-remote-p` 的 CONNECTED 参数对 `/fs:` 只读内存中的 session 池
（`remote-connection-target-open-p`，不做 `ssh -O check`）：target 没有 open
session 时返回 nil。auto-revert、VC、recentf 等后台调用方用这个查询决定是否碰
远端；用户再次访问文件时才按需重开。保留的未保存 buffer 另有上述 startup 标志，
不能仅依靠 CONNECTED 查询防止排队的主动启动回调。

tramp-rpc 0.13.1 在客户端还留下两类状态，由 `remote-backend-tramp-rpc.el` 回收：
`tramp-buffer-name` 旁边的 `NAME stderr` buffer 及其 relay 进程（TRAMP 通用清理
不认识它，旧 relay 存活还会让重连得到 `NAME stderr<1>`，这些编号残留也会回收）；
单目标清理也扫描已脱离 connection 表的该主机 RPC 通讯 buffer，全局清理扫描全部。
另有每个 file-notify
descriptor 由 `make-pipe-process` 隐式创建的 `tramp-rpc` buffer。transport 意外
死亡时只释放 relay，stderr buffer 保留供诊断，直到清理或下一代连接复用它。

离线回归在 `make remote-check`。完整初始化的 LSP + Dired + watch + 长任务清理
测试在 `test/remote-cleanup-live-tests.el`：运行
`REMOTE_E2E_TARGET=host make remote-cleanup-live-smoke`，通过正常 init 加载测试。
测试只在新建的 `/tmp` 目录写入
Python stdio peer，不要求安装 clangd；先断言 LSP 已初始化，再验证三个清理入口、
未保存编辑、延迟回调及 8 秒后的连接状态。

错误被分为三类：

- `backend`：当前 backend 不兼容，可在同一 pipeline 尝试另一个 backend；
- `transport`：pipeline 已失效，可换另一条 pipeline；
- `operation`：权限、退出码、参数等业务错误，不自动换路重试。

## 7. 进程、环境与网络 channel

```elisp
(remote-process-file "git" nil t nil "status" "--short")
(remote-make-process
 :name "worker"
 :command '("worker" "--stdio")
 :remote-context context)
(remote-make-client-process
 :name "ui-proxy"
 :command
 (append '("/usr/local/bin/node" "/client/proxy.mjs" "--")
         (remote-local-bridge-command
          "language-server"
          :args '("--stdio")
          :context context
          :directory workspace-root)))
(remote-executable-find "lake" context)
(remote-copy-file-to-target
 "/client/cache/server.tar.gz"
 "/fs:lab:/home/me/.cache/server.tar.gz"
 :context context :adapter "language-server" :overwrite t)
(remote-service-provision-directory
 "language-server" "/client/cache/server/"
 "/fs:lab:/home/me/.cache/server/1.2.3/"
 :context context :adapter "language-server"
 :ready-file "bin/server" :ready-kind 'executable)

(remote-exec "uname"
             :args '("-a")
             :context context
             :adapter "exec"
             :check t)
```

`remote-exec` 返回 status、stdout、stderr、route、context 和 command。环境是按
`target@workspace` 隔离的 capsule；pipeline/backend 切换不会创建另一份环境。
direnv、Nix、语言工具链等通过 maintainer 或派生 layer 修改环境，不直接全局
修改 `process-environment` 和 `exec-path`。provider 返回的整份 `PATH` 是
`replace` layer，会盖掉更低层（host path、target）；只知道增量的 provider 返回
`:path-layers ((remove …) (prepend …) (append …))`。direnv 就是这样：它输出的
PATH 基于它自己启动时的环境（本机即 Emacs 全局 PATH），所以只取 `DIRENV_DIFF`
记录的前后差异叠在 target PATH 上，`.envrc` 不会抹掉 host-path 探测到的目录。

语言服务器默认是 target placement。当前唯一客户端 lsp-mode 在启动前等待同一份
workspace 环境，随后通过官方 `make-process` / `start-file-process` 边界路由；
clangd、pylsp、typescript-language-server、rust-analyzer、texlab 和 bash-language-
server/workspace 所拥有的 logical root 也是 URI 反投影的唯一 target 来源；异步
callback 即使发生在其他 buffer，也不能读取环境中的 `default-directory` 来改写
文档身份。语言模块不能按 `file-remote-p` 选择另一套 contact、PATH 或 feature
降级；当前残留的此类分支都是迁移债务。

`remote-copy-file-to-target` 是 provisioning 的批量传输边界。native backend 使用
本地复制，TRAMP/tramp-rpc SSH backend 使用 SCP；consumer 只提交客户端绝对源文件、
逻辑目标文件和 context，不能自行解析 host/method。普通编辑器文件读写仍走 `/fs:`
handler，这个 API 只用于 JDTLS/Pyright 等版本化工具包。

`remote-service-provision-directory` 在这个传输边界之上统一目录型工具的供给流程：
trusted-target 门禁、客户端打包、版本缓存、目标暂存解压、ready probe、不完整缓存修复、
原子发布和失败清理都属于 Remote。语言层只提供源目录、版本化安装目录、ready 文件，
以及可选的轻量 `prepare`（例如生成 Pyright launcher 或给 JDTLS launcher 加执行位）。
可选的 `validate` 在已有缓存、暂存目录和发布后检查版本或内容摘要；即使 ready 文件
仍可执行但内容损坏，也会先验证新暂存目录再替换该版本目录。语言层
不再各自维护上传/安装脚本。

逻辑 `file-notify-add-watch` 返回稳定 Remote descriptor。递归 watch，以及
`:file-watch-cost` 不是 `push` 的路由上目标端有 `inotifywait` 时，watch 进程由
`remote-make-process` 启动并把 target-native 事件路径重写回 `/fs:`；否则使用
backend 的公开 file-notify。descriptor 的 valid/remove、断线 resync、事件
去重和 workspace resource 清理保持同一生命周期。

client placement 必须显式进入 `remote-make-client-process`。该 API 即使从远端
buffer 调用，也会恢复应用 direnv 之前的客户端环境、使用本机 cwd，并禁止 `/fs:`
handler 接管。

`remote-client-process-environment` / `remote-client-exec-path` /
`remote-client-executable-find` 是同一条边界上的查询入口，backend 用它们解析
自己必须在本机执行的辅助程序（SSH、stdio bridge、协议 proxy）。三条规则由框架
保证，consumer 和 backend 不再各自防御：

- adapter 可以合法地把 `executable-find` 重定向到目标（`init-lsp.el` 就这样让
  上游 `lsp-clients-*` 的裸 `executable-find` 变成 target-correct）。
  `remote-client-executable-find` 在查询期间清空 `remote-current-adapter-id`，
  因此它永远回答本机；否则 backend 会拿到一个目标端路径再在本机 `vfork`，
  症状是 `Doing vfork No such file or directory`。
- 路由执行期间投影 target 环境，会连 `process-environment` / `exec-path` 的
  default value 一起改写。`remote--call-with-process-route` 与
  `remote-fs--call-routed` 先用 `remote-with-client-environment` 固定投影前的
  客户端值，client boundary 因此在整个 backend 调用链里保持有效。固定值通过
  `remote-client-process-environment` / `remote-client-exec-path` 取得，而不是
  直接复制进入时的绑定：consumer 可能在进入框架前就已绑定了 target 投影。
  `python-shell-with-environment` 在 `run-python` 外层这样做，旧实现因此把目标
  的 `HOME=/home/…` 固定成“客户端”值，tramp-rpc 的 SSH ControlPath 落到不存在的
  目录，远端 REPL 一启动即退出。`remote-client-process-environment` 的最后兜底也
  是 `default-toplevel-value`：没有 buffer-local 绑定时，consumer 的 `let` 会连
  `default-value` 一起改写，只有顶层值仍然代表本机。
- 第三方 consumer 也会在框架之外重绑 `exec-path`。Citre 的远端可执行查找就在
  `find-file-hook` 里把整条 `exec-path` 换成 target 目录，而 backend probe 正好
  在那层下面运行。`remote-client-exec-path` 因此丢弃属于别的文件系统的目录，并
  在什么都不剩时回答上一次可用的客户端路径——空搜索路径从来不是事实。

`exec-path` 这个 handler operation 先由 backend 回答，再把 workspace
environment capsule 的 PATH（target PATH 加 direnv 等 provider）叠在最前面，
与 `remote-executable-find`、路由进程看到的是同一份列表；backend 独有的项
（TRAMP 附加的 `default-directory`、本机的 `exec-directory`）保留在后面。只靠
TRAMP 的登录 PATH 时，第三方 `(executable-find cmd t)`（agent-shell、acp.el、
lsp-mode 自带 client）会漏掉项目环境提供的全部工具。
它回答的是 **target-native localname**，与 TRAMP 自己的 handler 一致；调用方（`executable-find` 的 REMOTE 分支、Citre 的
远端查找等）自己补远端前缀。把结果投影成 `/fs:` 名字会让它们拿到
`/fs:TARGET:/fs:TARGET:/bin`，其下每次探测都是一次注定失败的往返。

一次文件操作的代价本身也是可查询的契约。backend 用 `:describe` 声明
`:file-operation-cost`（`batched` 或 `round-trip`），consumer 用
`(remote-file-operation-cost FILE)` 提问：

```elisp
(remote-file-operation-cost "/fs:aaron-wsl2:/home/hc/src/main.rs")
;; => batched
```

需要"每个文件一次子进程"的功能（VC、Magit、per-file 探测）用它决定开关，而不是
测 `file-remote-p`、TRAMP method 或 backend ID。`native` 与 `tramp-rpc` 声明
`batched`，`tramp` 声明 `round-trip`；`local` target 因此天然走同一条判断。

文件通知的代价和时间戳精度同样是 backend 声明的契约：`:file-watch-cost` 为
`push`（事件经 backend 已拥有的通道推送：本机 kqueue/inotify、tramp-rpc 服务端
inotify 流）、`process`（每个 watch 一个目标进程，shell TRAMP 的 `inotifywait`）或
`none`；`:mtime-compare` 为 `exact` 或 `window`。consumer 用
`(remote-file-watch-cost FILE)` 决定是否给每个 buffer 挂通知，例如
`init-doom-extra.el` 的 auto-revert 只在默认排除规则会跳过该 buffer、而路由为
`push` 时才让它跟随外部修改（VS Code 的 watcher 模型：无轮询，远端
`git checkout` 后已打开的 buffer 约 0.2s 内刷新）。逻辑单文件 watch 在 `push`
路由上直接注册到 backend，不再先做一次 `inotifywait` 查找、也不为每个 buffer 起
目标进程；递归 watch 仍走 inotifywait/Python。

`exact` backend 的 `verify-visited-file-modtime` 精确比较（tramp-rpc 的 mtime 是
服务端同一来源的整数秒），`window` backend 保留 TRAMP 的 2 秒容差。容差会把保存后
两秒内的外部写入（格式化工具、目标端 checkout）误当成自己的写入而静默忽略；
直接以 `/rpc:` 访问的 buffer 由 tramp-rpc 适配层的同一规则覆盖
（`remote-backend-tramp-rpc-exact-mtime`）。

`vc-registered` 由 `/fs:` 句柄按逻辑名回答，不投影到物理名。VC 把结果缓存在它
拿到的那个名字下，投影后 `vc-backend` 会对 buffer 自己的名字永远回答 nil，分支
和 diff 指示也就不会出现。backend 无需投影：VC 后端本身用普通文件操作和
`process-file` 访问 target。
需要把协议 peer 留在 target 时，使用
`remote-local-bridge-command` 生成本机可执行的 stdio bridge argv；它把 target
cwd、workspace 环境和 pipeline 封装在所选 backend 边界内。`local` target
同样调用 native backend 的 bridge，不存在 consumer 侧的 local/remote 分支。

Lean 使用这一 hybrid 模式：Node/HTTP Infoview proxy 固定运行在 Emacs 客户端，
`lake serve` / `lean --server` 运行在 target 并继承远端 direnv/Nix capsule。
Eglot 得到显式 process factory，因此不会因为 project root 是远端路径而用
`sh -c "stty raw; …"` 再包装本地 Node。proxy 的端口文件按 Eglot 实例隔离并保存在
客户端；xwidget 直接访问本机 loopback，不需要远端 Node、远端部署或 port forward。
在远端 buffer 内校验 proxy PID 时也强制使用客户端 process namespace。

ACP agent（agent-shell 的所有入口：Noema Run、popup、`C-c A a`、裸
`M-x agent-shell`）是 target-placement consumer，且不自带任何 remote 分支。
`init-ai-ide.el` 把 `agent-shell-cwd` 规范成 `/fs:TARGET:/path`，再经
`remote-client-file-name` 取本机可直接访问的写法：`local` 得到原生目录，远端 target
保持逻辑名。acp.el 以 `:file-handler` 调用官方 `make-process`，远端时进入 `/fs:` 句柄
和 `remote-make-process`，agent 可执行文件、cwd、环境都由 target 投影；agent-shell 的
`executable-find … t` 同样按 target PATH 解析，所以“target 上有对应二进制”就是唯一
前提。ACP 协议里的路径经 `agent-shell-path-resolver-function` 双向映射：Emacs 名
→ target-native localname，agent 发来的 native 路径 → 会话 target 的 Emacs 名；
`agent-shell--on-request` 被包在会话 buffer 内执行，映射因此不依赖 timer 触发时的
当前 buffer。本机固定的 OpenCode 二进制只在 agent 运行于客户端时替换默认命令。
agent 的环境与同 workspace 的源码 buffer 相同（host path、target 环境、direnv
等 provider）：由本机原生启动的 agent，在创建 client 前用
`remote-environment-ensure` 把 capsule 投影进它的 buffer，因此也使用项目的
direnv/Nix PATH，而不是 Emacs 全局 PATH；路由到 target 的 agent 则由进程路由与
`exec-path` handler 取得 capsule，它的 buffer 保留本机 HOME——agent-shell 的缓存
与历史在这个 buffer 里展开 `~`，投影 target 的 HOME 会指向本机不存在的
`/home/…`。判断依据是 `remote-client-file-name` 这一放置查询，而非 target ID。找不到可执行文件时报错会写明查找的
target。workspace 不可达时直接报错，不会被 shell-maker 静默改到本机 `~/`。

Copilot 是纯 client-placement consumer：Remote buffer 的文档内容仍由
`copilot.el` 同步给 language server，但 binary、PATH、环境、进程 namespace
全部来自 Emacs 客户端。target 不安装 Copilot，也不会继承 target 的 Node/PATH。

tramp-rpc backend 还包含当前 `msgpack.el` 的 large-map 兼容修饰：旧 encoder 在
环境 map 超过 15 项时会把二进制长度误传给 `unibyte-string`。direnv/Nix 环境很
容易超过该阈值，因此兼容逻辑由 backend 集中维护，消费者不截断环境。

`remote-environment-apply` 把 capsule 投影进 buffer 后，`process-environment` 与
`exec-path` 在该 buffer 中是 buffer-local。`let` 绑定的是当前 buffer 的值，切到
`with-temp-buffer` 等新 buffer 后绑定就不可见，子进程会退回登录 PATH（曾导致
direnv 项目的 `.conda/bin/python3` 被 `/usr/bin/python3` 取代，pyright 无法解析
项目依赖）。在切换 buffer 之前取出这两个值，在新 buffer 内重新绑定；
`remote-exec` 与 `remote--rpc-executable-find` 都遵循这一写法。

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

(remote-open-network-stream
 "tls-client" buffer "service.internal" 443
 :type 'tls :return-list t
 :remote-context context)

(remote-port-forward
 '(:host "127.0.0.1" :port 9000)
 :context context
 :local-endpoint '(:host "127.0.0.1" :port 0))

(remote-reverse-port-forward
 '(:host "127.0.0.1" :port 3000)
 :context context
 :remote-endpoint '(:host "127.0.0.1" :port 0))

(remote-channel-of native-process-or-forward)
(remote-channel-live-p native-process-or-forward)
(remote-channel-endpoint native-process-or-forward 'remote)
(remote-channel-list)
(remote-channel-recover channel)
(remote-channel-group-open
 '((shell . (:host "127.0.0.1" :port 9001))
   (iopub . (:host "127.0.0.1" :port 9002)))
 :context context :workspace workspace :key 'protocol-session)
(remote-channel-group-endpoints group 'local)
(remote-channel-group-recover group)
(remote-channel-group-close group)
(remote-close-channel channel)
(remote-channel-clear)
```

网络 API 继续返回 Emacs 原生 process 或既有 forward 对象，第三方软件无需认识
新的包装类型；框架把统一的 `remote-channel` 描述附在返回值上。native backend
完整保留 `open-network-stream :return-list t` 的 `(PROCESS . PROPERTIES)` 形式，
channel 描述附在其中的 PROCESS 上。SSH relay 只在最低层 socket connect 时替换
host/port；TLS/STARTTLS 协议层继续看到原始 target host/service，因此 SNI、
证书验证和 auth-source 不会误用 `127.0.0.1`。没有内建 GnuTLS 时 routed TLS
明确报 unsupported，避免外部 TLS helper 绕过 relay。
native backend
实现 network client/server 和双向 TCP proxy。TRAMP 与 tramp-rpc 可以通过所选
SSH pipeline 建立 `-L`/`-R` forward，再实现 target 侧 network client/stream、
远端 listener 与显式双向 port forward。远端 listener 仍返回原生 Emacs server
process；其物理 socket 是本机 relay，但 `process-contact` 的 host/service 与
`remote-channel-endpoint` 暴露 target listener 身份，避免把 relay 端口泄漏给
原生消费者。动态 `-R` 端口从 OpenSSH 确认信息中取得；建立超时、失败诊断和关闭
清理均由 channel/backend 边界负责。转发使用独立 SSH 连接，避免 workspace 的
ControlMaster 重建时误关新监听；`-L` 的就绪状态从 OpenSSH 输出读取，正常路径
不向目标服务建立探针连接。诊断输出有 32 KiB 上限。native proxy 在 outbound peer 配对完成前
缓存已经到达的数据，避免连接刚建立时静默丢失首包。
Remote 面板通过 `p` 建立 target 到本地回环的 `-L` 转发，并登记到 workspace
恢复资源；端口行的 `RET`/`w` 复制动态本地地址，`k` 关闭且移除恢复资源。
重连先关闭 workspace 拥有的 channel，再重建 session，避免关闭哨兵把预期退出
误报为新的传输故障。恢复时复用首次分配的本地端口；端口被其他进程占用时资源
会报告失败。Aaron-PC SSH E2E 验证建立、强制重连、原端口恢复及目标 SSH banner。

多端口协议使用 `remote-channel-group-*`，不让 consumer 循环创建和恢复
forward。成员有稳定名称并共享 context/workspace；建立中任一成员失败会回滚
全部成员。workspace 恢复优先复用客户端 listener，无法复用时生成新 endpoint
generation，并通过恢复 callback 通知协议 owner 更新连接信息。

## 8. Workspace、service 与 terminal

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
provision、目录型工具的统一版本化安装、start/live/stop；它不是强制常驻的 VS Code Server。Eglot、direnv
等普通消费者仍优先直接使用 process/environment API。
target-scoped service 在强制恢复时原位替换 handle 并保留 instance identity 与
引用计数；多个 workspace 不会各自留下一个已停止的旧 instance。

`remote-terminal-open` 提供内建 comint frontend；`remote-terminal-adopt` 让
vterm 等 native frontend 保留自己的 module、filter、sentinel 与 UI，同时把
process/buffer teardown 登记到 workspace。配置层的 popup vterm 已走这条边界：
在任意 `/fs:TARGET:/path` buffer 中按 `C-c e`，会打开或复用同一 workspace 的
terminal，且不同 target/workspace 的 popup 池不会串线。本地也是
`/fs:local:` 的同一流程。
PTY 按 `pty` capability 单独选路：SSH 配置优先 `ssh-pty`，直接复用 pipeline
拥有的 OpenSSH ControlMaster，不建立 TRAMP 文件会话；文件与 LSP 按各自
配置继续选路。选中路线失败时会尝试同一 target 的其他可用 backend。
workspace task 仍优先复用所属 workspace 的进程路线。可运行
`REMOTE_TERMINAL_E2E_TARGET=host make remote-terminal-live-smoke` 验证真实 SSH
终端的 cwd、环境和双向输入输出；设置
`REMOTE_TERMINAL_E2E_BACKEND=tramp-rpc` 可单独验证 RPC 路线。
`REMOTE_TERMINAL_E2E_STANDALONE=1` 验证没有预先打开 workspace 时的终端。
配合 `REMOTE_TERMINAL_E2E_FAULT=rpc` 会仅终止测试 Emacs 的 RPC 传输，验证
disconnected 状态、工作区重连和手动重启后的终端输入输出；
`REMOTE_TERMINAL_E2E_FAULT=pty` 则测试 PTY 进程异常退出。
`REMOTE_VTERM_E2E_TARGET=host make remote-vterm-live-smoke` 会用真实 VTerm
frontend 验证所选路线、workspace 跟踪与目标目录中的 shell 输入输出。

冷启动远端 vterm 只执行可缓存的 host facts 探测，用它解析远端账户真正的登录
shell（例如 bash 或 zsh）；它不会同步等待完整的 Nix/direnv capsule。shell
探测失败时按目标上的 `zsh` → `bash` → `sh` 顺序选择，最后才使用
`/bin/sh`。routed vterm 会截断自身的 TRAMP shell 二次探测，防止正确结果又被
覆盖。已有 capsule 会直接复用。本地 capsule 在 spawn 时传给进程，并在 vterm
mode 完成初始化后投影回 terminal buffer，避免在 vterm 临时绑定
`process-environment` 时制造 buffer-local 警告。
交互式 Emacs 会在启动或打开 workspace 后的空闲时段，用客户端环境预加载
VTerm 包；首次打开终端不用再同步支付 VTerm 包加载时间。可用
`REMOTE_VTERM_E2E_TARGET=host REMOTE_VTERM_E2E_PREOPEN=1
REMOTE_VTERM_E2E_PRELOAD=1 make remote-vterm-live-smoke` 分开测量预热后终端
启动及首条命令响应。
工作区关闭后，连接池会暂存会话以供快速重开；Emacs 退出时会显式关闭连接池及
由它拥有的 SSH ControlMaster，不依赖 OpenSSH 的持久期自然到期。
transport 断线时不会重放 shell 历史；vterm 保留为 disconnected buffer，显式
执行 `remote-terminal-restart` 会按原目录和 frontend 新建一个 vterm。

transport failure 会把相关 workspace 标记为 disconnected，并按 1、2、4 秒进行
自动恢复。任何显式登记了 recovery function 的资源都会在 session 恢复后重建；
手动 `remote-workspace-reconnect-async` 复用同一个合并调度器，命令立即返回，
重试间隔不阻塞 Emacs；单次 TRAMP 建连本身仍可能等待。重连主动关闭旧 session
后立即确认自己推进的 target epoch，避免把成功误判为过期；建连或资源恢复期间
如果又发生外部失效，本次尝试会重试。workspace 在等待中被关闭或替换时，晚到的
结果不会重新打开它。
框架目前自动登记 service、workspace-owned forward、逻辑 watch 与 lsp-mode
resource。Noema 的远程 Markdown watch 也使用这一边界：文件仍以 `/fs:` 标识，
watch 随 workspace 恢复，并在 Noema 停止时显式释放。资源已登记不等于完成
长期断线故障注入；该项仍需 SSH 真机验证。
逻辑 watcher 还会对窄时间窗口内完全相同的 backend event 去重，并维护单调
sequence。物理 watcher 意外发出 `stopped` 时，框架合并 resync 请求：先调用
metadata 中可选的 `:resync` 内容扫描函数，再重建物理 descriptor。显式关闭通过
callback generation 隔离，不会被误判成丢事件而重新打开。
PTY shell 不安全重放，因此 terminal 只标记为 disconnected，并要求显式
`remote-terminal-restart`。

`M-x remote-doctor` 从 target → pipeline stage → backend → route → session →
workspace/resource 输出诊断；加前缀参数会实际连接并运行 `uname -s`。
`M-x remote-board` 中的 `D` 会针对当前 target 异步运行 OpenSSH 诊断，并在
`*Remote SSH TARGET*` 保留详细输出；检查使用 `BatchMode=yes` 和短连接超时，
不会弹出密码提示或启动 workspace。连接错误会在面板 State 列区分认证、主机密钥、
主机名与网络问题，悬停可看最近错误；`T` 打开使用同一 SSH 配置与跳板参数的
交互式登录终端，供输入密码、密钥口令或一次性验证码。终端会话是独立的客户端
SSH 进程，登录后仍需正常打开文件夹或重连 workspace。面板绘制和状态刷新只读
本地缓存，不发起 SSH 探测。
首次建连时，面板 State 列会依次显示传输打开、SSH 登录与后端检查阶段；按
`L` 可查看当前 target 最近的连接阶段和失败原因。进度事件只在创建连接时发布，
暖态文件查询不增加进度观察开销。旧连接的迟到事件不能覆盖新连接的状态；
`C-g` 取消会释放已打开的传输阶段并清除进行中状态。认证输入仍由 TRAMP 处理。
从面板打开文件夹会先验证目录，再建立以该路径为上下文的受管理 workspace，
使面板的打开状态、重连和端口等资源归属一致；若随后 Dired 打开失败，新建的
workspace 会被关闭。`c` 关闭选中文件夹的 workspace 及其资源，`C` 关闭当前
target 的所有 workspace 和连接。两者都保留已访问的 Emacs buffer；下次文件
操作可以按原有路由重新连接。关闭与断开命令已通过本机状态测试和真实 SSH
文件夹生命周期测试。面板在打开文件夹期间显示 `opening folder` / `opening`，
成功、报错或取消后清除；刷新只读取本机缓存。

## 9. 配置兼容

配置继续接受 `links`、`plugin`、`plugins`；新配置可以使用
`pipelines`、`backend`、`backends` 和 `stages`。

ssh-config 导入出来的 target 没有显式对象承载路由偏好，因此 pipeline 条目可以
用 `preferences` 为它匹配到的主机声明 target 级偏好，写法与它已经用来提升
`trusted` 的方式一致；键是 capability 名或 `default`：

```json
{
  "id": "ssh",
  "backends": ["tramp-rpc", "tramp"],
  "include": ["Aaron-*"],
  "trusted": true,
  "preferences": { "default": ["tramp-rpc", "tramp"] }
}
```

target 偏好优先于 adapter 偏好，所以这一条会让这些主机的普通文件操作也走
tramp-rpc，而不只是进程、环境和 LSP。

在 `M-x remote-board` 中按 `a` 可新增 SSH 主机：选择已导入的 SSH 配置文件，
输入别名、主机名以及可选的用户、端口和密钥路径。新建文件权限为 `0600`，
随后重载目标列表。新增时不发起网络连接；写入前会检查别名是否被导入过滤
规则接受并有启用的 pipeline。可再按 `o` 打开目标，或按 `f` 选择文件夹。
按 `A` 可以粘贴 `ssh -i ~/.ssh/key -p 2222 user@host` 一类的连接命令，再
选择别名。支持 `-i`、`-p`、`-l`、`-J`、`-F` 和 `-o Name=Value`；远端命令
及不支持的选项会直接拒绝，不运行粘贴内容。若本机有 OpenSSH，写入前还会用
隔离的临时配置执行 `ssh -G` 语法检查；它不会读取现有配置的 `Match` 规则。
`-F` 必须指向已导入的 SSH
配置文件。新增 Host 会原子地写在现有规则之前，使它的显式连接参数优先于
已有的 `Host *` 或 `Include`；写入后恢复 `Host *` 作用域，避免改变旧文件开头
全局选项对其他主机的效果。已有配置文件的权限和符号链接保持有效。
非默认 SSH 配置文件会记录在对应 pipeline 上，客户端连接以独立参数
`-F FILE` 传给 TRAMP、tramp-rpc、直连进程、SCP、转发及控制连接；默认
`~/.ssh/config` 保持 OpenSSH 原生解析。SSH 客户端进程使用本机环境，目标环境
通过远端命令显式传递，避免目标 `HOME` 影响本机 `Include` 路径展开。TRAMP
连接缓存也会收到该 pipeline 的登录参数；这些参数标记为临时属性，不写入
TRAMP 的持久缓存，重连时仍保持选定的配置文件。
相对路径形式的 SSH 导入文件以 `etc/remote.json` 所在目录为基准解析，不依赖
当前 buffer 的目录。

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

配置重载先在隔离 registry 中完成解析、合并和冲突校验，再一次性提交。失败的
JSON、schema 或 pipeline 注册不会清空当前 target/session；成功提交只关闭发生
变化或被移除 pipeline 的 session/runtime。

## 10. 模块边界

```text
remote-framework.el       public library entry
├── remote-compat.el      public upstream API boundary and capability report
├── remote-core.el        target, adapter, capability, route
├── remote-pipeline.el    ordered reachability pipelines
├── remote-transport.el   pipeline stage executor/runtime
├── remote-backend.el
│   └── backend/          native, TRAMP, tramp-rpc implementations
├── remote-connection.el  real session pool + v1 compatibility API
├── remote-session.el     public session lifecycle facade
├── remote-fs.el          /fs identity and scoped file handler
├── remote-accelerator.el route-scoped optional high-level providers
├── remote-background.el timer reentrancy, retry and epoch-safe commits
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
Noema 等消费者留在框架外，只调用公共 API。

## 11. 测试与支持范围

```sh
make remote-test
make remote-conformance-test
make remote-e2e
REMOTE_E2E_TARGET=Aaron-Pi make remote-e2e
emacs --batch --init-directory=. -q -l early-init.el -l init.el \
  -l test/init-git-core-tests.el -f ert-run-tests-batch-and-exit
```

SSH E2E 是显式 opt-in，自动选择 SSH config 导入的 `Aaron-*` target，也可以通过
`REMOTE_E2E_TARGET` 指定。它只在本地和 target 的 `/tmp` 创建随机目录，并在结束
时清理；覆盖文件复制/读取/枚举、target cwd 进程、session 复用，以及动态 SSH
`-R` listener 从 target 到原生 Emacs server process 的数据往返，以及关闭旧
session 后真实 SSH workspace 能在后台重新连接并恢复命令执行。

当前稳定目标是 native + SSH：逻辑文件、同步/异步进程、PTY、环境、SSH
双向 forward、workspace/service 生命周期和原生开发工具兼容。WSL/container/
devcontainer、Dape/tasks 编排、动态 SOCKS forward 与托管 tunnel 不在这个版本
承诺范围内。

## 12. 当前完成度

| 范围 | 状态 |
|---|---|
| `/fs` / `fs://` 稳定身份、local 特殊 target | 已建立 |
| 原生文件 API 的 `/fs:` scoped handler | 已建立可扩展操作契约与未知操作保守策略 |
| target/pipeline/backend/route 分层 | 已建立；pipeline 为真实类型，旧 link API 保留兼容 |
| session 池、健康与失效 | 已建立 |
| backend 执行准备契约 | 已建立；sync/async/process plan/stdio bridge 均由 backend 分派 |
| native socket client/server | 已建立 |
| TRAMP/RPC network client、remote listener 与 SSH 双向 port forward | 已通过 native 回环、命令测试、SSH `-R` 真机数据往返，以及 `-L` 真机强制重连后原端口数据往返；突发断线与 `-R` 恢复故障注入待补 |
| pipeline stage 的实际逐段建连 | executor/runtime 已建立；内建 overlay/hop 主要负责 endpoint 变换 |
| workspace/service/channel/terminal 生命周期 | 基础已建立；service/forward、逻辑 watch 与 lsp-mode resource 已登记恢复，仍缺长期真机故障注入；terminal 手动重启 |
| Remote Doctor | 已建立结构化报告与可选 target probe |
| SSH 真机回归 | `make remote-e2e`，只使用随机临时目录 |
| WSL2 direnv + C clangd + Python pylsp | 已真实验证走远程环境与 tramp-rpc |
| Eglot/lsp-mode 统一 target placement | 已建立 `language-server` adapter |
| Lean Node proxy + Lake | client Node proxy 与 target Lake 通过 backend stdio bridge 组合；待目标恢复在线后完成真机回归 |
| watch、multi-hop、断线恢复等长期回归 | 尚需继续补齐 |

当前已经可用于文件、环境、进程、LSP、terminal 和部分 channel 工作流。下一阶段
优先完善 managed FRP/tunnel stage、SSH `-R` 长期真机回归、watch 一致性和断线
重连，并迁移语言模块中遗留的 local/remote contact、PATH 与 watcher 分支；
消费者继续只做环境或工具逻辑，不承担物理路径、spawn 形式和连接生命周期。

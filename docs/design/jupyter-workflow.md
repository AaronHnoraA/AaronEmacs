# Jupyter Kernel Workflow

完整对齐范围、实测证据和未完成项见 [Jupyter parity audit](../records/audits/jupyter-parity-audit-2026-09.md)。

> Scope: this document covers ordinary `.ipynb` files and Markdown `@@cell`
> sidecars. D-023 `.noema` files are AI prompt/workflow documents: work blocks
> run through an ACP agent and save replies as outputs, without any Jupyter
> kernel or programming-language code block.

# Architecture

Noema's Node Jupyter service is the sole live-state and protocol authority.
The ownership boundary follows the Jupyter Server model:

- a `Kernel` is a global running resource identified by an opaque `kernelId`;
- a `Session` is one notebook path explicitly attached to zero or one kernel;
- a `Task` belongs to a session/cell and is reported by the global manager;
- Noema owns the kernel registry, Jupyter protocol/ZMQ traffic, execution,
  completion, outputs, variables, widgets, and lifecycle operations;
- Emacs projects ipynb as editable source and presents controls that call the
  Noema API. This notebook path has no second protocol owner. Standalone
  Board REPLs still use emacs-jupyter and are listed as separate clients;
- for a Remote target, Emacs may place the process and forward five opaque
  ports, but Noema remains the endpoint that speaks the Jupyter protocol.

Noema note sidecars are standard nbformat 4.5 files under `.cell/`, named
`NOTE.LANGUAGE.SESSION.ipynb`. Ordinary ipynb files use the same path without
requiring Noema note metadata. Emacs visits both as editable percent-style
source projections; the file on disk remains valid ipynb JSON. Language and
session come from the path, configured kernelspec lives in notebook metadata,
stable identity/source/output use standard `cell.id`, `source`,
`execution_count` and `outputs` fields; Noema-only state stays under
`cell.metadata.noema`. The projection codec is built into Emacs and does not
invoke Jupytext. A live `kernelId` is never persisted. A note marker therefore has only
`@@cell(language, session) [cell-id]` authority.

这里的 `LANGUAGE` 是 notebook 语言，不是 kernelspec 名。例如 SageMath 是
Python-family kernel，因此使用 `NOTE.python.SESSION.ipynb` 和
`@@cell(python, session)`；`sagemath` 只写在 `metadata.kernelspec` 与 Noema
session 状态。旧的 `*.sage.*.ipynb` 在首次打开时迁移到 `*.python.*.ipynb`。

Visiting an `*.ipynb` path that does not exist yet, or one that some other
tool left empty, seeds a valid nbformat 4.5 notebook with one empty code cell
and writes it before the source projection opens. An empty file is not JSON,
so without that step neither the projection nor a kernel could read it. The
kernelspec recorded there is `my/noema-jupyter-notebook-new-kernelspec` (a
registered setting, Python 3 by default); its `language` entry also selects the
major mode the new notebook opens in. A new `.noema` work document keeps its
own initialization and still asks for its Noema project first.

Opening any `*.ipynb` automatically composes the language major mode (for
example `sh-mode`) with the `NCell` minor mode and Noema Jupyter header. Old
Noema-owned notebooks with missing or duplicate cell ids are upgraded in place
to unique nbformat 4.5 ids. An ordinary external notebook is less invasive:
missing ids and its existing `nbformat_minor` stay unchanged on disk. Emacs
and Noema use a transient in-memory id only for UI addressing. If its metadata
does not identify a language, the source projection defaults to Python.
Closing that Emacs source buffer does not stop the Noema kernel.

Noema's web-host only serves native notebooks inside its notes root
(`my/noema--notes-root`) or inside a validated Noema project, i.e. an
ancestor directory whose `noema.toml` has a `[project]` table (a Wiki
repository manifest alone does not count). Every Jupyter request from the
source buffer carries `projectRoot` when such a project exists. For a native
notebook outside both (for example `~/Desktop/a.ipynb`), the first explicit
command (kernel selection, Run, Restart, …) calls `noema-project-ensure`,
which offers the notebook's directory and its ancestors (proposing a code
repository's `project.el` root, but never a vault's) and writes nothing unless
you accept. Declining aborts that command. Opening the file, session refresh,
save notifications, and keystroke completion never ask. Logical `/fs:` paths
are not subject to this native-root check.

The singleton Noema Jupyter workspace reuses one page for every notebook via
tabs and provides the JupyterLab-like
layout: Server/Running Kernels/Kernel Specs on the left, output Cells in the
center, Cell Inspector/Variables/Sessions on the right, and Tasks at the
bottom. The Emacs header exposes kernel selection, Run, Run All, Interrupt,
Restart, Outputs, Variables, and Manage as Noema API/UI operations. Its
`Editor/LSP:` control switches only the Emacs major mode (including a Text/LSP-off
choice), then restores the ipynb projection and NCell UI; it never changes the
notebook language, kernelspec, or Noema session. The same command is bound to
`C-c i l`. The distinct `Kernel:` control (`C-c i K`) uses exactly the catalog
shown by the Web workspace.

Kernel selection in Emacs and the Web workspace consumes the same flattened
Noema catalog (local/Remote kernelspecs, attach targets, and configured server
targets). Emacs does not hide entries merely because their declared language
differs from the current editor mode. Connecting to a running private kernel
is offered only for the notebook that already owns it.

## Kernel language and LSP placement

The selected kernelspec is the source of truth for the notebook language;
`metadata.language_info` is treated as a cache and is refreshed on selection.
Python-family names such as `sage` are normalized to `python`, while shell,
Julia, R, JavaScript, and Lean names select their corresponding source mode.
Kernel selection changes execution and kernel completion. LSP follows the
notebook's visiting file, its Remote target, direnv environment and Python
toolchain; switching kernels does not restart or relocate the analyzer.
Opening `/fs:TARGET:/path/file.ipynb` puts source files and LSP on that target.
A local notebook using an SSH kernel retains local LSP. For project development,
open the remote project itself. A `remote_ikernel` SSH profile remains available
for a portable REPL or short debugging session.

Noema broadcasts one authoritative session snapshot after either UI changes a
kernel. Emacs also reads that snapshot by `scriptFile` instead of querying a
possibly stale local kernel name. Current-cell selection is mirrored in both
directions between the source projection and the singleton Web workspace; it
does not reload the document, focus another window, or start a kernel.

Noema uses ordinary kernelspecs. Its Node launcher starts local kernels. For
a Remote target, the Emacs Remote broker starts the process over the target
transport and forwards five opaque ports back to the Noema Node service. A
remote Jupyter Server is not required. Connecting to an existing server is a
separate opt-in path; see "Remote Jupyter servers" below.

A kernel therefore comes from exactly one of three configured sources selected
for a session:

| Kernel name                   | Source                                      |
|-------------------------------|---------------------------------------------|
| `python3`, `sagemath`, … | a kernelspec, launched by the Remote broker |
| `attach:<connection-file>`                  | an explicitly attached existing kernel      |
| `server:<id>:<kernelspec>`                  | a new kernel on a remote Jupyter server     |
| `server:<id>:kernel:<kernel-id>`                  | a kernel already running on that server     |

Everything downstream — execution, completion, ipywidgets, the browser bridge
— is identical across the three. Only lifecycle differs: the broker signals a
process it owns, an attached kernel is never killed, and a server kernel is
started, interrupted, restarted, and shut down over REST.

# SSH 项目入口

项目开发推荐 `M-x my/jupyter-ssh-open-project`，或在项目菜单 `project-menu`
选择 `Jupyter SSH project`。`Jupyter Board` 顶部和 `SSH Notebook Projects`
区域也有 `Open SSH Project`；Board 原有的 Remote Kernels / Open REPL
入口继续保留。已配置的 SSH 项目还会显示独立行，可直接按 `Open Project`
进入项目，或按 `Files` 打开远端 Dired。
现有 `COMP9444` 快捷入口指向 `aaron-pc` 的
`/home/aaron/Desktop/UNSW/COMP9444/`；其他项目可选 `Choose SSH folder…`。
配置入口是 `my/jupyter-ssh-projects`，每项只有 `:name`、已有 Remote `:target`
和该机器的绝对 `:root`。

打开后用普通 Dired / `find-file` 进入 `.ipynb`，按 `C-c i K` 选目标机上的
`python3` 等 kernelspec，再运行单元。编辑和保存通过同一个 `/fs:TARGET:`
文件系统；项目 shell、源码、Git、任务和 lsp-mode 也走这个 Remote workspace。
嵌套目录中的 notebook 会继承已打开的项目根目录，优先发现根目录的 `.conda`
或 `.venv`。内核由 Remote broker 在目标机直接启动，Noema 负责 ZMQ 协议；
这里不需要 Jupyter Server，也不需要安装 `remote_ikernel`。

在远端 notebook 中可用 `C-c i p o` 重新进入 SSH 项目。Board 的
`Quick Add SSH` / `Remote Kernels` 仍管理便携 `remote_ikernel` profile；
`Target` 决定 Board 查看哪台机器上的 profile，notebook 的 `C-c i K`
则列出其文件所在 target 的 kernelspec。Server URL 和活动 session 继续在
Board 里管理。

`COMP9444` 的项目根目录已有 `.envrc`，把 `PATH` 和 `CONDA_PREFIX`
指向 `.conda`，并隔离 Python user site；`.gitignore` 排除了本地环境
和 notebook 缓存。远端 `direnv exec .` 已验证能选中
`/home/aaron/Desktop/UNSW/COMP9444/.conda/bin/python`。Jupyter 的
`python3` kernelspec argv 也指向该解释器。PyTorch 的 Python 导入名是
`torch`；notebook 应写 `import torch`，写 `import pytorch` 会产生
`ModuleNotFoundError`，即使内核环境正确。可用 `C-c i p ?` 检查
notebook 对应的目标、项目根目录和解释器。上一次执行失败时，页头标为
`last run error`，与内核启动失败区分开。

`aaron-pc` 的交互式 Bash 在 `~/.bashrc` 末尾加载 `direnv hook bash`；
Emacs Remote target 的 Shell 也设为 `/bin/bash`。在终端 `cd` 进出
`COMP9444` 会自动加载或卸载项目环境。非交互脚本若需要相同环境，使用
`direnv exec . command`。

真机回归：

```sh
JUPYTER_SSH_TARGET=aaron-pc \
JUPYTER_SSH_ROOT=/home/aaron/Desktop/UNSW/COMP9444 \
make jupyter-ssh-project-live-smoke
```

它创建临时 notebook 和源码，检查远端
Dired、创建、列目录、重命名、删除、notebook 编辑与保存、项目 `.conda` 发现、
Noema 选核与运行后输出写回，以及 Pyright 对 notebook 的实际分析。测试使用隔离的
Noema host/state 和随机端口，结束时清理测试文件、LSP workspace、内核及 host。

# Noema 与 Remote workspace

Noema 的 Node Jupyter service 是唯一的协议与管理 authority：kernel registry、
ZMQ 消息、执行、completion、output 和 widget channel 都由 Noema 负责。Emacs 不
加载 `jupyter-client`，也不建立 kernel ZMQ client；它只在需要 Remote target 时
帮助放置进程和转发五个端口。打开 `/fs:TARGET:/path/note.md` 时，broker 在同一
Target/workspace 中发现 kernelspec、以 workspace root 为 cwd 启动进程，再把连接
信息交回 Noema。远端 note 同目录只保留同一份 `.cell/*.ipynb`，不再拆分 script
和 output mirror。

传输断开只释放 channel；重连后先探测目标 kernel。进程仍存活时重建 forwarding
并保留内存状态；进程已经死亡时下一次执行重建 kernel，并以 `stateLost` 报告
状态丢失。不会自动安装依赖或回退到客户端 kernel；使用 Remote Doctor 的 probe
检查目标 `python3`、`jupyter` 和 kernelspec。

探测有三种结果，不是两种。启动时记录实际主机名；随后在目标端同一条命令里先核对
主机，再执行 `kill -0` 或发送信号。进程组确实不存在才是确认死亡；连接断开或
登录别名重连到另一台节点都返回 `unknown`。`unknown` 不标 `stateLost`、不向可能
重用旧 PID 的节点发信号，并保留本机关闭记录。`noema.jupyter.status` 此时
报错，而不是返回 `alive: false` 让 Noema 丢弃内存状态。Board 会显示记录中的
实际主机，方便从固定节点恢复和清理。

kernel 是否在服务 Jupyter 消息由 Noema 侧的 `hb` 心跳观察。五端口里的 `hb` 一直
被 broker 转发；local kernel 连续丢失回声可直接判死。Remote broker/attach 路径则
不同：心跳还穿过 channel，丢回声只能证明当前 transport generation 不可用，不能
证明目标进程死亡。Noema 此时先问 broker：只有 target 明确回答 `alive: false` 才
dispose 并丢弃状态；探测失败记为 `disconnected` 并继续在稳定的 client endpoint 上
重试；进程仍在且 channel generation 已变化时重绑五个 socket。一次 SSH/network
抖动因此不会被误当成 kernel death。外部 attached kernel 没有归 broker 所有的 PID，
所以其 liveness 明确报告为 `unknown`；connection file 消失只作为诊断信息，不能单凭
这一点判死，最终仍由 `hb` 判断它是否还能服务消息。

Noema 输出页的页头读取同一内核注册表的只读状态：`disconnected` 显示连接丢失、
内存状态未知；`dead` 才显示状态已丢失。页面可见时每 5 秒只查询内核状态，
恢复或内核身份变化才重读输出快照。远端文件暂时不可读时，页面保留上一次
成功加载的输出并提示刷新失败；真正换核会清空旧的已执行源码版本记录，
避免把失效的输出标作当前结果。完整 Noema GUI 断线交互仍需真机验收。

执行本身没有 wall-clock 超时（`NOEMA_JUPYTER_EXEC_TIMEOUT_MS` 默认 0），
这是有意的：它是**空闲**超时，会被消息重置，而 `time.sleep(3600)` 这类静默长任务
不发消息，给它非零默认值会杀掉合法计算。存活性归心跳管，不归超时管。

对应地，Emacs 侧发起执行的 gateway 请求也不能用普通请求的截止时间：Noema 要等
run-all 里每个 cell 都跑完才应答，用 10 秒的默认值会把一次正常运行报成失败。
执行与 kernel 控制走 `my/noema-jupyter-cell-execute-timeout`（默认 24 小时），
它是"一个 notebook 最长允许算多久"的上界，不是健康检查。

补全（capf）走的是同步桥，所以它包在 `while-no-input` 里：你继续打字就立刻放弃
这次请求，不会为了等一个忙碌的 kernel 冻住编辑器。kernel 连续答不上来时，
`my/noema-jupyter-cell-introspect-backoff`（默认 20 秒）内不再隐式打扰它；
它一旦重新应答就立即清除。**显式** `C-c C-i` 不受 backoff 影响。

LSP 由文件所在的 Remote workspace 和目录环境决定，不再查询 kernelspec 的
Python runtime。不同目录下的 `.envrc` 可同时对应不同的 buffer 和 LSP workspace。

`attach:` 的 connection file 现在在**任意 target 上**都能发现：runtime 目录取自
该 target 自己的 `jupyter --runtime-dir`，再经 `/fs` handler 扫描。选择后也由
Emacs broker 在同一 target 读取并验证完整的 `hb/control/shell/stdin/iopub` 五端口，
以一个原子 channel group 转发给 Node；Node 不会把 target connection-file path 当作
本地文件打开。group 恢复保留五个 client listener，并递增 generation；释放 attach
只关闭路由，绝不向外部 kernel 发送 shutdown/restart。此前非 local target 只在 Board
里能看见文件，Noema 主路径仍用 Node `fs` 读取，实际无法 attach。若外部 owner 原地
重启 kernel 并改写同名 connection file，broker 会重新验证 HMAC key 与完整五端口，
保持 client listener 不变而替换 target tuple；通配 bind IP 也会改写为 target loopback，
绝不会把 `0.0.0.0` 当成 Node 侧连接地址。

kernel 启动握手默认允许 60 秒（`NOEMA_JUPYTER_KERNEL_LAUNCH_TIMEOUT_MS`），因为
SSH 首连或 scheduler 排队通常超过本地进程原先的 15 秒上限；readiness 仍必须收到
`kernel_info_reply`，仅仅 ZMQ socket 打开不算成功。

`attach:` 和 `server:` 都没有可探测的 target 侧解释器，因此对 LSP 是**预期**
回退（不刷 minibuffer，但 Doctor 里可见）。此前 `server:` 没被处理，会报
"kernelspec \`server:id:python3' was not found on target \`local'"。

`attach:` 与 `remote_ikernel` 继续用于显式 existing-kernel/Jupyter Board 工作流，
attached kernel 不归 Noema broker 杀死或自动重启。desktop host mode 已退役；
它不是另一套可用的 Jupyter UI。

所有 `*.ipynb` 在 Emacs 中打开时都会自动进入语言 major mode 与 `NCell` minor
mode，并从 notebook metadata 读取当前 kernelspec；普通 notebook 没有
`source_file` 时以自身作为独立 document。Emacs 顶栏只是
Noema manager 的 UI client；Run、Restart、Manage 等操作
全部调用 Noema API。runtime provider 在 owning Target 上执行 kernelspec 的轻量 probe，读取
`sys.executable`、`sys.prefix` 和 `sys.path`，再让 lsp-mode 的 Python analyzer 自动
跟随 kernel；server binary 本身仍来自该 Target/workspace 的 tooling environment。
同一 runtime 复用保温中的 lsp-mode workspace，不同 kernel runtime 在同一真实逻辑根
下使用独立的 client-side workspace identity；最后一个 cell buffer 关闭后默认保温
10 分钟。切换 kernel 会先发送 `textDocument/didClose` 并完整 detach 旧 workspace，
不会只关掉 managed-mode 后把旧 buffer/workspace 状态残留下来。

UI 使用通用的上下两层规则：所有 LSP buffer 的 breadcrumb 都作为上层 tab line
左侧的展示型 underlay；仓库原有 buffer tabs 按整行宽度居中，和 breadcrumb 相交时
由 tab 覆盖自身列，长 breadcrumb 会在 tab group 右侧继续显示。真实 tab 保留
切换/关闭行为；breadcrumb 的 mouse-1 则只打开
Treemacs、定位当前 Imenu node 并 pulse。Noema 的 kernel / Run / output 顶栏仍留在
紧邻正文的 header line，因此 Jupyter 控件不会被 LSP 接管。

普通 Python/ipykernel kernelspec 可自动推导。项目自带 Python/Sage wrapper 在
`metadata.aaron.runtime.probe_argv` 声明 probe 入口。带 Aaron metadata 的 SSH
`remote_ikernel` 会从 `kernel_cmd` 提取解释器并通过 Remote target 探测；旧版或
非 SSH 的不透明 launcher、`attach:` 仍回退到项目默认 toolchain。这类预期回退不再
反复写入 minibuffer，具体原因仍可在 Language Server Hub / Doctor 查看。笔记项目的
`:noema-jupyter` 默认 kernel 仍可设置，但不要再用项目级 `:toolchain` 把 Python
固定到 Sage。

当前配置不启用 Jupyter 包可选的 `/jpy:` 和 `/jpys:` TRAMP 文件名处理器。该
处理器在 Emacs 31 的 TRAMP foreign-predicate 调用约定下与当前 Jupyter 包版本
不兼容；Jupyter 的普通执行、`remote_ikernel`、Jupyter Board 和 Noema broker
不依赖它。

The vendored baseline is upstream tag `0.4.6` at commit
`1c437edb9df017141a6357a90de751bb200bd9fb`. Local changes are documented in
`site-lisp/remote_ikernel/UPSTREAM.md`.

# Install and verify

Install the editable source into the configured Anaconda environment:

``` bash
make remote-ikernel-install
bin/install-remote-ikernel --check
```

The check must report version `0.4.6+aaron.1` and a module path below this
repository's `site-lisp/remote_ikernel` directory. Emacs startup only checks
the tool; it never mutates the Python environment automatically.

vendored `remote_ikernel` 的五端口 tunnel 使用单一 OpenSSH session：`-N -T`、
`ExitOnForwardFailure=yes`、TCP keepalive 与 `ProxyJump`。因此任一 listener bind 失败
会让 launcher 失败，半开连接会变成可恢复的进程退出，也不再靠远端 `sleep 600`
每十分钟人为断线。

# Remote Jupyter servers (HTTP/HTTPS)

用于连接**已经存在**的 Jupyter server：集群 login node 上的 lab、JupyterHub、
或 kernel gateway。这条路径不启动任何进程，全部通过 REST + WebSocket。

## 配置

服务器清单是注册过的 config 项 `my/noema-jupyter-servers`（用 config board 或
`etc/config-store.el` 设置，不要 `setq`）。每项是一个 plist：

``` commonlisp
(:id "hpc"                          ; kernel 名里用的稳定标识
 :name "Cluster Lab"                ; 显示名，默认取 :id
 :url "http://127.0.0.1:8888/"      ; server URL
 :kind server                       ; server（默认）或 gateway
 :auth token                        ; token（默认）/ password / hub / none
 :user "aaron"                      ; JupyterHub 用户
 :target "cluster"                  ; 拥有它的 Remote target，默认 "local"
 :insecure nil                      ; 接受无法验证的 TLS 证书
 :server-name nil)                  ; :url 是 forward 时的真实 TLS/Host 名
```

注意密钥不写在这里：`:auth token` 和 `:auth password` 通过
`auth-source-search` 按 `:url` 的 host（以及 `:user`，如果有）查找，凭据像其他
credential 一样留在 authinfo/GPG 里。`jupyter lab` 启动时打印的
`http://localhost:8888/lab?token`…= 可以整条粘进 Board 的 `Add Server URL`；
保存前会提取 token，仅保留在本次 Emacs 会话中，持久配置只保存无密钥的 base URL。
重启 Emacs 后使用 auth-source，或通过 `Edit` 重新输入凭据。旧配置里已有的 token
URL 仍能连接，但不会在公开 catalog / Board 中显示密钥。

## Remote-first 路由

`:target` 不是 `local` 时，Emacs 先在该 Target 的 workspace 里开一个
`remote-port-forward`，再把改写成 `127.0.0.1:<本地端口>` 的 URL 交给 Noema，同时
带上真实主机名（否则 TLS 证书校验和 virtual-host 路由会对着 127.0.0.1 失败）。
forward 作为 recoverable resource 注册在 workspace 上，传输断开后与 kernel 的
channel group 一样被修复。一个 server forward 只有一个 keyed resource owner（不会
同时被 generic forward 和 Jupyter resource 各恢复一次），恢复时复用原 client
listener，所以 Noema 已建立的 REST/WebSocket base URL 不变。若该端口确实无法复用，
下一次 provider resolve 会得到新 URL；Node 的 server registry 会重新解析并重建
connection，而不是永久缓存第一次收到的本地端口。

每个 kernel record 还保留自己实际绑定的 provider signature，不能被 catalog/contents
提前刷新 registry cache 掩盖。WebSocket 重绑或握手失败只记 `disconnected` 并继续连接
同一个 server kernel id；只有 server 明确返回该 kernel 不存在才判死。配置清单成功
刷新时，已经删除的 server provider 会立即注销，其 Remote forward 不会等进程退出才清理。

Target 无法提供 channel 时会报错，不会退回到客户端直连。这条规则是有意的：
远端 server 常常绑在 loopback 上，直接在本机连同一个端口要么失败得莫名其妙，
要么连到本机上另一个恰好在监听的 server。

## Host 边界

当前一方交互入口是 Emacs；Node/Go 是 headless service。旧的 desktop 模式说明
不再适用。Jupyter URL 配置表示计算服务连接，不会自动把本地项目迁移到该服务器。

## Kernel ownership

`server:<id>:<kernelspec>` 由 Noema 通过 Session API 新建，生命周期结束时会关闭
对应 session；`:kind gateway` 则通过 Kernel API 新建并关闭裸 kernel。相反，从
running 列表采用的 `server:<id>:kernel:<kernel-id>` 不归 Noema 所有：关闭文档或
Noema 只断开客户端，绝不会顺带 shutdown 这个可能仍被其他 session 使用的 kernel。
interrupt/restart 只有在用户显式执行对应操作时才发给 adopted kernel。

## Contents API

`:kind server` 的服务器现在通过 Remote 的 `jupyter-contents` 文件后端挂载。
在 Jupyter Board 的服务器条目点击 `Browse Files`，或运行
`M-x my/noema-jupyter-server-browse`，即可在 Dired 浏览服务器目录、打开
`.ipynb`、编辑并保存。文件与执行输出都留在服务器，不要求本地 Noema project。
也可以用 `(my/noema-jupyter-server-file-name "lab" "folder/demo.ipynb")`
取得文件名，再交给 `find-file`。

这个命名空间使用 `/fs:jupyter.<server-id 的 UTF-8 十六进制>:/路径`，路径相对于
Contents 根目录。notebook 默认在所属服务器启动所选 kernelspec，子目录传给
Jupyter session，因此工作目录与 notebook 所在目录一致。服务器 URL 的
`/user/alice/` 或其他代理前缀仍是 HTTP base path，不是 OS 路径。

目录、读写、复制、重命名和删除经 `noema:api:jupyter-cell:server-file`
处理。文件 API 不提供 shell、Git 或 LSP 进程，所以使用标准 Dired；补全和
检查仍可以经 notebook 的 kernel 协议完成。需要远端 shell/LSP 时，应使用
已有的 `/fs:TARGET:/真实路径` 工作区。`:kind gateway` 不提供 Contents 挂载。

保存交给服务器的 ContentsManager；不承诺通用的 POSIX 原子替换、文件锁、
chmod、符号链接或跨客户端原子冲突检测。隐藏文件是否可访问也由服务器决定。
协议依据见 [Jupyter Server REST API](https://jupyter-server.readthedocs.io/en/latest/developers/rest-api.html)
和 [ContentsManager 文档](https://jupyter-server.readthedocs.io/en/latest/developers/contents.html)。

# 协议覆盖

除执行外，以下 Jupyter 消息也已实现，local / attach / server 三条路径通用：

- `complete_request` / `inspect_request` / `is_complete_request` /
  `history_request` / `comm_info_request`。全部带超时：kernel 串行处理 shell
  channel，长任务运行时的补全请求必须放弃而不是卡住编辑器。
- `input_request` / `input_reply`。cell 里的 `input()` 和 `getpass()` 可用；
  取消会回 EOF（U+0004），cell 以 EOFError 结束，而不是让 kernel 一直阻塞。
  没有任何 UI 能应答时（例如无 publish 通道）不声明 `allow_stdin`，
  `input()` 直接抛 StdinNotImplementedError。
- `shutdown_request`：自己拥有的 kernel 在 SIGTERM 之前先请求优雅退出。
- `kernel_info_reply` 的 `language_info` / `banner` / `help_links` 会保留，
  经 `kernel-status` 暴露。
- `execute_reply` 的 payload：`page`（`?obj` 分页器）转为 cell 输出，
  `set_next_input`（`%load`）随结果返回，`ask_exit`（`exit`）关闭 kernel。

## Emacs 里的补全与文档

所有 ipynb 源码投影（`my/noema-jupyter-cell-mode`）上挂了
`my/noema-jupyter-cell-capf`，直接问 kernel。它看得见静态分析看不见的东西：
前面 cell 定义的名字、DataFrame 列名、IPython magic、Sage 注入的 builtin。
它声明 `:exclusive no`，并固定排在 `lsp-completion-at-point` 前面；所以优先保留
live kernel 的手感，而 kernel 空闲、不可达或没有结果时仍会透传给 lsp-mode。
`C-c C-i` 或 `S-TAB` 是 `my/noema-jupyter-cell-inspect`（Jupyter 的
`inspect_request`，前缀参数取源码，相当于 `??obj`）。

打字不会顺带启动 kernel：没有正在运行的 kernel 时这些请求直接返回
`supported: false`。

Emacs 的 snippet 入口也能调结构动作，但这些不是 Yasnippet 模板。在 ipynb
buffer 中输入 `jcode`、`jmd` 或 `jraw` 后按 `C-c y y`，会通过 Noema API 在下方创建
code/Markdown/raw cell；新 `cell.id` 只由 Noema 生成。`jrun`、`jrunnext`、`jall`、
`jsplit`、`jmerge`、`jclear`、`jout` 等同理。`C-c y j` 可直接选择完整动作表。
若直接调用 Yasnippet 而绕过这组结构动作，`jcode`、`jmd`、`jraw` 和 `jcell`
模板也会生成合法 id，因此新建 cell 不会退化成裸 `# %%`。
共享 notebook snippet 表会跟随投影语言使用 `#`、`//`、`--` 或 `;` 注释；
离开 NCell 后该表停用。Python cell magic 模板包括 `jtime`、`jtimeit`、`jcap`、
`jbash`、`jhtml`、`jlatex`、`jjs`、`jsql`、`jwrite`；SQL 需要 kernel 安装并加载
相应 IPython extension。这些 magic 不是 \`.noema\` Agent Work block。
对于旧会话或手工输入留下的裸 `# %%`，Noema-owned notebook 在保存/运行前会
先把它提升为带稳定 id 的真实 cell，避免 marker 被吞进上一 cell 的 source 后再报
`Unknown Jupyter cell`。普通外部 ipynb 仍保留其无 id 形式。

## 实时输出

cell 运行期间 iopub 以补丁流（`set` / `append` / `clear` / `status`）经
已有的 SSE 通道推给前端，服务端按 80ms 合并。执行响应仍然是权威值，前端以它
为准对账，所以丢帧只影响流畅度，不影响正确性。

# Jupyter Board

用 `M-x jupyter-board` 或 Hyper dispatch `j` 打开 `Jupyter Projects`。
主页以 `SSH Notebook Projects` 为主入口，进入远端完整项目并直接编辑文件；
已配置项目提供 `Open Project` / `Files` 快捷操作，按 `J` 可直接选择项目。
`Remote Kernels · Quick REPL` 保留便携 `remote_ikernel` 的 Quick Add SSH、
Open REPL、编辑和清理入口。`Show technical` 展开本地或目标机
kernelspec、connection file、工具路径与诊断。Notebook 的 kernelspec
选择仍用 `C-c i K`。

Main keys:

| Key | Action |
|----|----|
| `J` | Open an SSH Jupyter project |
| `T` | Choose the target whose kernelspecs are shown |
| `a` / `A` | Add a portable `remote_ikernel` profile |
| `e` / `E` | Edit a portable profile |
| `d` / `C` | Delete one profile / clean temporary profiles |
| `RET` | Show the normalized fields and raw `kernel.json` |
| `r` | Open/connect the profile or session REPL |
| `o` | Open the kernelspec resource directory |
| `g` | Refresh |
| `s` | Explicitly clean abandoned connection files on the selected target, then refresh |
| `v` | Toggle technical resources |
| `?` | Open the usage guide |
| `M` | Open the full Jupyter dispatch |

`g` 只发现并展示资源，不删除文件。旧连接文件的清理单独由 `s` 发起；
本机还会每 15 分钟定时维护一次。清理脚本会再次检查进程、监听端口和文件身份，
不会终止 kernel。发现请求还在运行时，再次打开 Board 不会重复发起请求；`g`
可以主动重试。明确断开所选 Remote target 后，Board 清空旧快照并提示手动按 `g`；
断开前发出的异步结果不会重新填回。

活动 session 提供 interrupt/restart/shutdown。便携 profile 的 `SAVED` 对应
Core group，`TEMP` 可批量清理；旧 `rik_*` kernelspec 仍按 Core 处理。

broker kernel 启动并取得 PID 后，立即在本机
`var/noema/jupyter/pending-shutdown/` 保存进程身份；正常关闭后删除记录。
同一 Emacs 会话中活动 runtime 遮住这条记录。若 Emacs 被强制结束，或退出时
target 不可达，重新打开 Jupyter Board 并选择该 target，会显示
`SHUTDOWN-PENDING` 行；该行只提供 Retry shutdown。
重试先确认进程组仍存在，再检查该 PID 的命令行仍包含本 runtime 独有的连接文件名；
只有匹配才发送 TERM。目标不可达、命令不匹配或探测失败时，保留记录供下次重试。
同一 Emacs 会话里 `shutdown-pending` 的再次关闭也执行相同的 PID 核对。
TERM 发出后还会在有界时间内核对进程组确已退出；仍存活或无法确认时不删除
运行时和本机记录。记录写入失败则在发信号前停止。远端 PID 命令行核对和
连接文件清理都在目标端同一条命令里重新核对原物理主机，避免动态登录名
中途转到另一节点。持续忽略 TERM 的内核保留待关闭状态，供 Board 再次重试；
目前不会未经再次确认就自动发送 KILL。
Board 刷新只读本机记录，不会为查看状态连接远端。
损坏的记录在 Board 的 Saved Broker Record Problems 区块中提示，不弹出额外窗口，
也不会被当成可关闭的进程。

Linux SSH target 的跨 Emacs 会话回收可显式运行
`JUPYTER_PENDING_LIVE_TARGET`\<SSH target\> make jupyter-pending-remote-live-smoke=。
探针在远端临时目录启动一个带专属连接文件名的睡眠进程：第一会话写入关闭记录，
第二会话模拟 target 不可达并确认记录保留，第三会话通过真实 Remote 路由核对
进程身份后关闭。探针退出时删除自己的远端进程、文件和本机记录。第二阶段是
不可达状态模拟，不等同于实际拔网线；此探针也不执行真正的 ipykernel。

`JUPYTER_PENDING_LIVE_TARGET`\<SSH target\> make jupyter-abrupt-exit-live-smoke=
另用正式 broker 启动路径生成记录，再对测试 Emacs 发送真实 SIGKILL；新 Emacs
会话从本机记录列出该 kernel，经 Remote 路由核对 PID 后关闭远端睡眠进程。
即使探针失败，也会按专属临时目录和进程命令行清理测试资源。它验证强制退出
后的回收入口，不等同于完整 ipykernel 执行测试。

完整 Linux broker 路径可运行
`JUPYTER_LINUX_TARGET`\<SSH target\> make jupyter-linux-kernel-live-smoke=。
它在目标机临时目录启动真实 `ipykernel`，通过五个转发端口执行代码和补全，
重建 channel 后确认变量状态保留，再关闭内核并检查本机记录与远端连接文件。
探针使用生产设置 `remote-client-ssh-program`：macOS 默认 `/usr/bin/ssh`，
也可用 `JUPYTER_LINUX_SSH` 仅覆盖本次探针。测试固定物理节点时可用
`JUPYTER_LINUX_CONFIG`/path/to/remote.json= 指向一次性的 Remote 配置，
不改日常 target。这个目标上的 Homebrew SSH 曾在
转发握手时失败。
目标只装了 `jupyter` 分发器而没有 `jupyter-kernelspec` 子命令时，目录查询改用
目标机已安装的 `jupyter_client` / `jupyter_core` Python API；Board 的异步发现
也使用同一命令。执行中途故障另用
`JUPYTER_LINUX_TARGET`\<SSH target\> make jupyter-linux-disconnect-live-smoke=：
内核报告 busy 后只切断探针自己的五条 SSH 转发，等待 Remote 自动恢复，并核对
原内核若存活则状态仍在，若目标确认死亡则记录保留到关闭确认、新内核为空白状态。
在 CSE 目标上实测为后者；前端请求 12 秒内返回不确定结果，原进程死亡的原因
尚未确认。`login.cse.unsw.edu.au` 是动态入口；它在另一次故障注入后可能转到
另一台节点，不能用新节点的 PID 查询确认旧内核死亡。针对多端口内核，SSH 转发
现共用受管 ControlPath；仍应以固定物理节点 target 做重复断线验收。此探针覆盖
broker 和原始客户端，不代替完整 Noema 页面交互验收。

# Persistent group metadata

New remote kernels store their management data inside namespaced kernelspec
metadata:

``` json
{
  "metadata": {
    "aaron": {
      "remote_kernel": {
        "group": "temporary",
        "config": {}
      }
    }
  }
}
```

CLI/Advanced Add profiles default to Temporary; the guided Quick Add defaults to
SAVED/Core for semester-long use. Change a group from the board or CLI:

``` bash
remote_ikernel manage --set-group rik_ssh_example_python core
```

# Maintenance

Run the vendored Python tests before changing the launcher:

``` bash
/opt/homebrew/anaconda3/bin/python -m pytest -q site-lisp/remote_ikernel
```

The SSH implementation uses `StrictHostKeyChecking=accept-new`: first-use keys
are recorded, while changed keys fail instead of being silently accepted.
CLI output from Board operations is retained in `*Jupyter Board Log*`.

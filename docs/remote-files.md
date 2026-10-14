# 临时机器与远程文件管理

两件事，都建在 [Remote 框架](remote-framework.md) 上：

- **临时机器**：手动输入地址和密码/密钥就能用的 target，记在 `var/` 里；
- **文件管理器**：本机文件夹和 target 文件夹并排，用 rsync 上传、下载、同步，
  全部异步并显示进度。只用普通的用户态命令行工具，不挂载文件系统，不需要
  任何系统扩展或系统设置改动。

## 1. 临时机器

SSH config 里没有、也不值得写进去的机器：新开的 VPS、同学的服务器、一台只用
几天的实验机。

| 入口 | 作用 |
|---|---|
| 任何选 target 的提示里选 `临时机器`（`SPC o r`、`M-x my/remote-connectboard`、`SPC o f` 等） | 输入一台机器，记住并直接用它继续 |
| Remote 面板 `m`，或 `M-x remote-machines-add` | 记住一台机器 |
| Remote 面板 `M`，`SPC o M`，或 `M-x remote-machines` | 管理列表 |

添加时依次输入：

1. 地址，写法 `[user@]host[:port]`，例如 `root@203.0.113.7:2222`；IPv6 写
   `user@[2001:db8::1]:22`。没写的用户名和端口会再问；
2. 登录方式：`password`、`key file`，或 `agent or SSH defaults`（用 ssh-agent
   和 `~/.ssh/config` 的默认行为）；
3. 名字。target ID 由名字生成，之后不变。

之后它就是一个普通 target：出现在 Remote 面板（Trust 列为 `temporary`）、
`SPC o r` 的补全、文件管理器的 target 列表里，路由、workspace、终端、断开和
自动重连都和 SSH config 导入的主机一样。它默认不受信任，不会被部署工具包。

管理列表 `*Remote Machines*` 的按键：

| 键 | 作用 |
|---|---|
| `RET` / `o` | 打开它的默认文件夹 |
| `f` | 打开文件管理器 |
| `a` | 添加 |
| `e` | 修改地址、登录方式或名字（改动会关闭用旧设置建立的连接） |
| `p` | 只忘记密码，保留机器 |
| `k` / `D` | 忘记机器：先断开，再删除记录和密码 |
| `g` | 刷新 |

### 存在哪里

都在 `var/remote/`，不进 Git：

| 文件 | 内容 |
|---|---|
| `machines.json` | 机器列表，**含明文密码**，权限 `0600` |
| `machines.authinfo` | 由上面生成的 netrc，TRAMP 通过 auth-source 读它；权限 `0600` |
| `askpass.sh` | 给 rsync 用的 askpass 小脚本，本身不含密码 |

密码是明文存的，只靠文件权限保护；这是“在 `var` 里记住”换来的代价。介意的
机器请用密钥登录，或者添加后按 `p` 忘记密码（之后每次连接 TRAMP 会问你，而
传输工具因为没有终端可问，会直接登录失败）。

两个细节：

- 临时机器的主机密钥第一次见到时自动接受（`StrictHostKeyChecking=accept-new`），
  密钥变了仍然拒绝；
- 密码同时含单引号和双引号、并且含空格时，netrc 写不出来：传输照常，打开文件
  时 TRAMP 会问一次密码。

临时机器的 ID 和 `etc/remote.json` 或 SSH config 的 target 撞名时，配置优先，
列表的 Note 列会标 `shadowed by a target`。

## 2. 文件管理器

### 打开

| 入口 | 行为 |
|---|---|
| target 的 Dired 里 `C-c C-t` | 管理当前文件夹 |
| `SPC o f` 或 `M-x remote-files` | 在 target 的文件夹里同上；在别处先问管理哪个 target |
| Remote 面板 `F` | 管理光标所在 target 或文件夹行 |
| 临时机器列表 `f` | 管理那台机器 |

打开后当前 frame 变成三块：左边本机文件夹，右边 target 文件夹，下面是传输
队列。默认只有文件，没有终端；需要时在菜单里按 `e` 才会开一个，每个会话
最多一个。两个 pane 都是普通 Dired，照常进目录、标记、改名、删除。本机 pane 起始
目录是上次在这个 target 上用过的目录，否则是 `remote-files-local-directory`
（默认 `~/`）。

会话打开期间，`C-c C-t`、`SPC o f` 显示命令菜单：

| 键 | 作用 |
|---|---|
| `c` | 把当前 pane 标记的文件（没有标记就是光标所在项）复制到另一个 pane 的文件夹。在本机 pane 是上传，在 target pane 是下载 |
| `s` | 同步：让另一边的文件夹跟上这边，不删除任何东西 |
| `S` | 镜像：让另一边和这边完全一致，**另一边多出来的会被删除** |
| `d` | 只比较两边差异，不改动 |
| `o` | 跳到另一个 pane |
| `l` / `r` | 换本机 / target 文件夹 |
| `t` | 重新显示传输队列 |
| `e` | 在上方打开一个 target 上的终端（Ghostel），位置是 target pane 当前的文件夹；已有终端时让它 `cd` 过去 |
| `.` | 让 target pane 跳到终端当前所在的文件夹 |
| `q` | 退出并释放资源 |

`s` 和 `S` 不会直接动手：先做一次 rsync 演练，把将要发生的每一项改动列在一个
buffer 里（`*deleting` 开头的是要删除的），按 `C-c C-c` 才执行，按 `q` 放弃。

### 进度与队列

传输都是后台进程，Emacs 不等它们：

- 模式栏在有传输时显示 `Xfer:2 45%`（个数和平均进度）；
- 传输队列 `*Remote Transfers*`（Remote 面板 `t`，或
  `M-x remote-transfer-list`）每半秒刷新，列出状态、进度条、速率、剩余时间。

队列里的按键：`RET` / `l` 看 rsync 的输出，`k` 取消，`r` 重跑，`c` 清掉已
结束的，`g` 刷新。取消或失败后留下的半截文件带 `--partial`，重跑会接着传。

传输完成后，目标文件夹对应的 Dired 自动刷新。

### 退出

菜单里按 `q`，或直接关掉这个 frame。会话只收回它自己起的东西：

1. 还在跑的传输：问一次是否取消；不取消就不退出；
2. 关闭会话开的终端、会话建的 Dired 和传输队列窗口，恢复原来的窗口布局；
3. 关闭会话打开的 workspace；
4. 如果连接是这次会话建立的，而且这个 target 上没有别的 buffer 或 workspace，
   就断开它。打开会话之前就连着的 target、或期间从 pane 里打开了文件的
   target，保持连接。

此外，在 Remote 面板上显式断开一个 target（`C`）会取消它的传输；退出 Emacs
时还在跑的传输随之结束。

### 依赖

都是本机的命令行工具，不是 Emacs 包：

| 工具 | 用途 | 说明 |
|---|---|---|
| `rsync` | 所有传输 | 需要 3.1 以上才有整体进度；macOS 自带的 openrsync 只能显示单个文件的进度。Homebrew 的 `rsync` 即可。**target 上也要有 `rsync`**，版本不限（macOS target 自带的 openrsync 也可以）；本机 rsync 低于 3.2.4 时会改用 `-s` 保护带空格的远程文件名，那时 target 必须是正牌 rsync |
| `ssh` | 传输的连接 | 密码登录靠 `SSH_ASKPASS_REQUIRE`，需要 OpenSSH 8.4 以上 |

### 限制

- 两个不同 target 之间不能直接传（rsync 不支持两端都是远程），经本机文件夹
  中转；
- 传输用自己的 SSH 连接，不走编辑会话的 ControlMaster，所以大文件不会
  卡住编辑，但也意味着每次传输各登录一次；
- 需要输入密钥口令而又没有 ssh-agent 的密钥，传输工具没有终端可问，会登录
  失败。先 `ssh-add`；
- 不提供 sshfs 挂载。在 macOS 上它依赖 macFUSE，需要降低启动安全策略或启用
  系统扩展，而上传、下载、同步和浏览都不需要它。

## 3. 实现

| 模块 | 责任 |
|---|---|
| `lisp/remote/remote-machines.el` | 临时机器的存储、管理列表，以及作为 target 来源的注册 |
| `lisp/remote/remote-transfer.el` | rsync 任务、进度解析、传输队列 |
| `lisp/remote/remote-files.el` | 双 pane 会话和命令菜单 |

框架为此增加的两个契约见 [remote-framework.md](remote-framework.md) 的
“target 来源”和“客户端批量工具”两节。测试：`make remote-transfer-test`；真机
回归 `REMOTE_TRANSFER_E2E_TARGET=<target> make remote-transfer-test`。

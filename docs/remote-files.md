# 临时机器与远程文件传输

两件事，都建在 [Remote 框架](remote-framework.md) 上：

- **临时机器**：手动输入地址和密码/密钥就能用的 target，记在 `var/` 里；
- **文件传输**：Dired / Dirvish 里的一个菜单，用 rsync 上传、下载、同步，
  全部异步，任务进 Emacs 的后台任务管理（皮卡丘）。只用普通的用户态命令行工具，不挂载文件系统，不需要
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
`SPC o r` 的补全、传输菜单的 target 列表里，路由、workspace、终端、断开和
自动重连都和 SSH config 导入的主机一样。它默认不受信任，不会被部署工具包。

管理列表 `*Remote Machines*` 的按键：

| 键 | 作用 |
|---|---|
| `RET` / `o` | 打开它的默认文件夹 |
| `f` | 打开它的文件夹并弹出传输菜单 |
| `a` | 添加 |
| `e` | 修改地址、登录方式、密码或名字；地址、登录方式或密码变化会关闭旧连接，下次操作用新设置重连 |
| `p` | 只忘记密码，保留机器 |
| `k` / `D` | 忘记机器：保存删除后断开旧连接 |
| `g` | 从磁盘重载其他 Emacs 会话保存的机器，并刷新列表 |
| `q` | 返回打开列表前的窗口 |

列表在每个 frame 中各自保留排序和阅读位置；同一 frame 再打开会复用该视图。
新增、编辑或删除机器会同步刷新所有可见的机器列表及 Remote 面板，关闭 frame
只释放该 frame 的列表，不会删除机器记录或断开连接。
保存机器或派生的凭据文件、重新登记 target 失败时，会恢复原来的内存记录和
两个私有文件；删除机器只在文件保存成功后才开始断开旧连接。若断开已执行而
后续重载失败，机器记录仍会恢复，但旧连接需要在下次操作时重新建立。
Remote 主配置不可读取时，添加会直接报错并回收写入的机器记录。
多个 Emacs 会话共用这份列表时，保存会持有 Emacs 原生文件锁并检查磁盘版本。
若其他会话已修改，当前操作会报冲突且不会覆盖对方的记录；在管理列表按 `g`
（或执行 `M-x remote-machines-reload-from-disk`）读入最新记录，再重试操作。

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

## 2. 文件传输（Dired / Dirvish 里的菜单）

没有单独的界面：它是 Dired / Dirvish 的一个扩展。在任何文件夹里按一个键弹出菜单，
对面那台机器的文件或文件夹在一个**弹出的 Dirvish 窗口**里选，选完窗口自动关掉。

### 入口

| 入口 | 行为 |
|---|---|
| Dirvish 里 `M`（或 `C-c C-t`） | 弹出当前文件夹的传输菜单 |
| Dirvish 里 `y` 选中文件，到另一台机器的文件夹里 `p p` | 跨机器的粘贴按来源 target 分别排传输任务，不弹菜单；同一目标但客户端不可直接读取时，在目标端排复制任务 |
| `M-x remote-files` | 同上；不在 Dired 里时先问 target、打开它的文件夹再弹菜单 |
| `SPC o f` | 同 `M-x remote-files`（Dired 里没有 `SPC` leader，用 `M`） |
| Remote 面板 `F`、临时机器列表 `f` | 打开那台机器的文件夹并弹菜单 |

经 `SPC o r` / Remote 面板打开 target 文件夹时，echo 区会提示这个键。

“对面”由当前文件夹在哪决定，两个方向是同一套操作：

- 在 **target 的文件夹**里：对面是本机，弹出窗口显示本机文件夹（上次用过的，
  否则 `remote-files-local-directory`，默认 `~/`）；
- 在**本机文件夹**里：先问对面是哪个 target（列表里可选 `临时机器` 现场输入），
  弹出窗口显示它的文件夹。

### 菜单

“这里 / 这个文件夹”默认取光标所在层级：目录行取该目录，文件行取其父目录；
展开的子目录和弹出窗口中的目录选择也遵循这条规则。

| 键 | `M-x` 指令 | 作用 |
|---|---|---|
| `c` | `remote-files-send` | 把这里标记的文件（没有标记就是光标所在项）送到对面：在弹出窗口里走到目标文件夹后确认 |
| `g` | `remote-files-fetch` | 从对面取文件到这里：在弹出窗口里标记文件后确认 |
| `s` | `remote-files-push` | 同步：让对面的某个文件夹跟上这个文件夹 |
| `r` | `remote-files-pull` | 同步：让这个文件夹跟上对面的某个文件夹 |
| `=` | `remote-files-compare` | 只比较两个文件夹的差异 |
| `-d` | | 开关：同步时删除目标端多出来的文件（镜像） |
| `t` | `remote-transfer-list` | 传输队列 |
| `e` | `remote-files-terminal` | 在这个文件夹开一个终端（在文件夹所在的机器上） |
| `x` | `remote-files-disconnect` | 断开这个文件夹的 target |

所以（Dirvish 里标记文件用 `SPC`，见 [daily-usage.md](daily-usage.md) 的 Dirvish 键位）：

- **下载**：在 target 文件夹里标记文件 → `M` `c` → 弹出本机文件夹，走到要放的
  位置 → `C-c C-c`；
- **上传**（从远程这边发起）：在 target 文件夹里 `M` `g` → 弹出本机文件夹，
  标记要传的文件 → `C-c C-c`；
- **上传**（从本机这边发起）：在本机文件夹里标记文件 → `M` `c` → 选 target →
  弹出它的文件夹，走到目标位置 → `C-c C-c`。

### 弹出窗口

弹出的是一个普通 Dirvish，照常进目录、标记。它的状态栏会一直写着正在选什么、
按什么确认：

| 键 | `M-x` 指令 | 作用 |
|---|---|---|
| `C-c C-c` | `remote-files-pick-confirm` | 确认：取标记的文件，或当前所在的文件夹 |
| `C-c C-k` | `remote-files-pick-cancel` | 取消 |
| `M` | | 弹一个只有确认 / 取消的小菜单 |

确认并成功排队后，或取消时，窗口关闭，为这次选择新开的 Dired buffer 一并杀掉，光标回到原来的
文件夹。若工具或路由不可用、任务没能启动，弹窗与标记留在原处，可修正后再确认。

`s` / `r` 同步不会直接动手：先做一次 rsync 演练，把将要发生的每一项改动列在一个
buffer 里（`*deleting` 开头的是要删除的），按 `C-c C-c` 才执行，按 `q` 放弃。
正式任务未能启动时，预览和应用动作保留，可重试。

### 任务在哪看

传输都是后台进程，Emacs 不等它们。它们进的是 Emacs 自己的后台任务管理，也就是
模式栏那只皮卡丘：

- 有传输时皮卡丘开始跑，点它（或 `M-x activity-board`）打开 `*Activity*`，
  传输列在 task 一组，名字前带百分比。`RET` 看传输队列保存的日志；`x` / `i` / `X`
  都通过队列取消，避免目标端文件操作被误记为失败；
- Dirvish 状态栏在该 target 有传输时显示 `Transfers:2 45%`；
- 更细的队列是 `*Remote Transfers*`（菜单 `t`、`*Activity*` 底部的
  `Transfers` 按钮、Remote 面板 `t`、`M-x remote-transfer-list`）：rsync 显示进度、
  速率和剩余时间；目标端文件操作显示运行状态，`RET` / `l` 看输出，`k` 取消，`r` 重跑，
  `c` 清掉已结束的。rsync 取消或失败后留下的半截文件带 `--partial`，重跑会接着传；
  目标端文件操作重跑前重新检查来源与目标。队列和日志的 `q` 回到打开前的页面；
  自动裁剪历史时不会关闭正在阅读的日志，手动 `c` 清理可关闭它并恢复原窗口。
  每个 frame 有自己的队列表格与排序、阅读位置；任务及每项日志仍全局共用。
  关闭 frame 只释放它的队列视图，不取消任务。队列和日志在 Evil 下也使用这些
  Emacs 风格操作键；打开队列或日志留在当前 frame。

传输完成后，目标文件夹对应的 Dired 自动刷新。
如果目标的 SSH 传输意外退出，队列会结束正在等待的目标端任务，释放本地进程，
并在日志写明远端命令结果未知。重新连接后先检查目标目录，再按队列的 `r` 重试；
重试前会重新检查来源与目标是否冲突。

### 连接和资源

没有需要“退出”的会话：

- 上传、下载和同步的 rsync 任务使用自己的 SSH 进程，任务结束后释放；同目标的
  复制、移动和链接复用该 target 的 TRAMP 连接，目标命令结束后释放任务进程；
- 弹出窗口自己收拾它开的 buffer；
- 浏览 target 文件夹用的连接属于那个 target，和平时一样：菜单 `x` 或 Remote
  面板 `C` 断开，同时取消它还在跑的传输、关闭它的 buffer；
- 退出 Emacs 时还在跑的传输随之结束（会先问一次）。

### 依赖

都是本机的用户态命令行工具，不是 Emacs 包，也不需要改动系统：

| 工具 | 用途 | 说明 |
|---|---|---|
| `rsync` | 所有传输 | 需要 3.1 以上才有整体进度；macOS 自带的 openrsync 只能显示单个文件的进度。Homebrew 的 `rsync` 即可。**target 上也要有 `rsync`**，版本不限（macOS target 自带的 openrsync 也可以）；本机 rsync 低于 3.2.4 时会改用 `-s` 保护带空格的远程文件名，那时 target 必须是正牌 rsync |
| `ssh` | 传输的连接 | 密码登录靠 `SSH_ASKPASS_REQUIRE`，需要 OpenSSH 8.4 以上 |

### 限制

- 两个不同 target 之间不能直接传（rsync 不支持两端都是远程），经本机文件夹
  中转；
- 传输用自己的 SSH 连接，不走编辑会话的 ControlMaster，所以大文件不会卡住
  编辑，但也意味着每次传输各登录一次；
- 需要输入密钥口令而又没有 ssh-agent 的密钥，传输工具没有终端可问，会登录
  失败。先 `ssh-add`；
- 不提供 sshfs 挂载。在 macOS 上它依赖 macFUSE，需要降低启动安全策略或启用
  系统扩展，而上传、下载、同步和浏览都不需要它。

## 3. 实现

| 模块 | 责任 |
|---|---|
| `lisp/remote/remote-machines.el` | 临时机器的存储、管理列表，以及作为 target 来源的注册 |
| `lisp/remote/remote-transfer.el` | rsync 任务、目标端文件操作、进度解析、共同的任务队列 |
| `lisp/remote/remote-files.el` | Dired 里的传输菜单和弹出式选择 |
| `lisp/init-activity.el` | 把传输任务认领为 task（标签、取消、查看输出） |
| `lisp/init-windows.el` | Dirvish 状态栏段 `remote-files` |
| `lisp/init-dirvish-keys.el` | Dirvish 的 `M` 键，以及 `y` / `p p` 按路径可见性选择原生操作或传输队列 |

框架为此增加的两个契约见 [remote-framework.md](remote-framework.md) 的
“target 来源”和“客户端批量工具”两节。测试：`make remote-transfer-test`；真机
回归 `REMOTE_TRANSFER_E2E_TARGET=<target> make remote-transfer-test`。

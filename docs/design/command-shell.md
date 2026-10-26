# 命令 shell（eshell）

## 范围

本文规定“需要拉起一个命令 shell”时用什么、怎么用：程序化地在某个目录里跑命令并拿到
结果（自动化），以及手动打开一个 shell 输入命令（交互）。两者共用同一套 eshell
buffer 和同一份实现，位于 `lisp/term/shell/eshell.el`。

不在范围内：

- **构建、项目运行和测试**：`+task/run`、`+task/build`、`+project/run`、`SPC c c`
  走 `compile` 和 `+output`。它们需要错误跳转、Activity 任务和重跑，所以不改用 eshell。
- **全屏终端程序**（TUI、ssh 交互会话）：用 Portal。
- **comint shell**：`M-x shell`、`M-x shell-toggle` 保留，但不是默认入口。

## 模型

- **上下文 buffer**：每个 target 上下文一个 eshell buffer，名字是
  `*eshell*<上下文>`（`+eshell-context-buffer-name`），由 `M-x eshell` 和本文的 API
  共用。
  - 目录决定 target：本机目录在本机跑；`/fs:TARGET:` 目录经文件名处理器在该 target
    上跑。调用方不按 target 分支。
  - 复用一个 buffer 时，先把它切到调用给的目录。
- **命令队列**：一个 buffer 同时只跑一条命令。buffer 忙时（前台命令在跑，或上一条
  API 命令还没结束），新命令排进该 buffer 的队列，按顺序执行。
  - 用户在提示符上已经输入、还没发送的文字会先存起来，命令结束后再放回，不会被覆盖。
- **结果**：一条 API 命令结束时，回调收到一个 plist：
  - 正常结束：`:status`（退出码）、`:output`（这条命令输出的文字）、`:command`、
    `:directory`、`:buffer`。
  - 被取消或 buffer 被关掉：`:status` 为 nil，`:reason` 为 `cancelled` 或 `killed`。
- **输出规整**：很多命令行工具用 ANSI 序列重画同一行（`ESC[nG` 回到列首、`ESC[2K`
  清行），eshell 不认这些序列，会把每一帧叠成一行新输出。这里把列复位改成 `\r`、去掉
  其余光标和清屏序列（颜色序列保留），让 eshell 就地覆盖。落在一段输出末尾的 `\r`
  先缓存，等下一段输出到来再拼上。

## API

| 函数 | 作用 |
| --- | --- |
| `(+eshell-buffer &optional DIRECTORY)` | 返回 DIRECTORY 所在上下文的 eshell buffer；没有就创建，不显示 |
| `(+eshell-run COMMAND &rest KEYS)` | 在该 buffer 里跑 COMMAND，立即返回 buffer。KEYS 有 `:directory`（默认 `default-directory`）、`:callback`（结束时以结果 plist 调用）、`:display`（非 nil 时用 `+eshell/toggle` 的方式显示） |
| `(+eshell-run-sync COMMAND &rest KEYS)` | 同上，但等命令结束再返回结果 plist。KEYS 有 `:directory`、`:timeout`（秒，默认取 `+eshell-sync-timeout`）。超时会中断命令，并抛出 `+eshell-timeout` |
| `(+eshell-busy-p &optional BUFFER)` | buffer 正在跑命令，或队列里还有命令时为真 |
| `(+eshell-interrupt &optional BUFFER)` | 中断正在跑的命令，队列保留 |
| `(+eshell-cancel &optional BUFFER)` | 清空队列（各回调收到 `cancelled`），再中断正在跑的命令 |

交互入口：

- `SPC o s` / `M-x command-shell`（`+eshell/toggle`）在底部弹出当前目录所在上下文
  的 eshell，行为与原来的 `shell-toggle` 相同：
  - 已显示但未选中时，选中它；
  - 选中时，关掉它的窗口，buffer 保留。
- `M-x command-shell-run`（`+eshell/run-command`）读一条命令，在项目根（没有项目
  时用当前目录）的 eshell 里排队执行并显示。

在 shell 里：连续重复的命令只记一次历史；以空格开头的命令不进历史（同 shell 的
`ignorespace`，用于带密钥的命令）；`emacs FILE` / `e FILE` 与 `vi FILE` 一样在本
Emacs 里打开文件，不会在 shell 里再起一个 Emacs。

## 契约

- **C1 上下文**：`+eshell-buffer` 对同一上下文返回同一个 buffer，并把它切到调用给的
  目录；本机目录和 `/fs:local:` 目录是两种写法，结果一致。测试：
  `command-shell-c1-buffer-is-per-context`、`command-shell-c1-fs-local-parity`。
- **C2 结果**：`+eshell-run-sync` 返回命令的退出码和输出，外部程序和 eshell 内建命令
  都适用。测试：`command-shell-c2-sync-returns-status-and-output`。
- **C3 队列**：buffer 忙时，后来的命令排队按顺序跑，各自得到自己的结果。测试：
  `command-shell-c3-commands-queue-in-order`。
- **C4 不覆盖用户输入**：提示符上未发送的输入在 API 命令结束后放回原处。测试：
  `command-shell-c4-pending-input-is-restored`。
- **C5 取消与关闭**：`+eshell-cancel` 和关闭 buffer 都会让所有未完成的回调收到结果，
  并取消本模块的计时器。测试：`command-shell-c5-cancel-and-kill-resolve-callbacks`。
- **C6 超时有界**：`+eshell-run-sync` 不会超过 `:timeout`；超时后中断命令并报错。
  测试：`command-shell-c6-sync-timeout-interrupts`。
- **C7 输出规整**：`ESC[nG` 重画的进度行就地覆盖，颜色保留。测试：
  `command-shell-c7-redraw-sequences-overwrite`。

## 已知差距

- **G1 远程验证**：远程 target 上的 C1–C6 还没有在 Aaron-PC 上做真机验证。按
  `docs/acceptance/remote-parity.md` 的要求，这一项完成前，远程只算“经文件名处理器
  可用”，不算达到对等。

## 实现位置

| 部分 | 位置 |
| --- | --- |
| API、队列、输出规整、交互入口 | `lisp/term/shell/eshell.el` |
| eshell 的基础设置、上下文命名、按上下文复用 | `lisp/term/shell/config.el` |
| 键 `SPC o s` | `lisp/editor/evil/config.el` |
| 终端起始目录（`/fs:` target 的 HOME 由其 backend 解析） | `aaron-terminal-home-directory`，`lisp/core/aaron-funcs.el` |

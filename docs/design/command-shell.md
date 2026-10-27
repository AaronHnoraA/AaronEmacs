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
  - 自动化任务可显式传 `:name`；同一 target 下每个任务得到
    `*eshell:TARGET/NAME*` buffer 和独立队列。名字只用作稳定的任务标签，不从
    命令文本推断。未命名命令仍进入默认上下文 buffer。
  - 目录决定 target：本机目录在本机跑；`/fs:TARGET:` 目录经文件名处理器在该 target
    上跑。调用方不按 target 分支。
  - 新建或复用空闲 buffer 时，把它切到调用给的目录并重画提示符；忙时保留正在执行
    的目录，新命令轮到执行时才切目录。
- **命令队列**：一个 buffer 同时只跑一条命令。buffer 忙时（前台命令在跑，或上一条
  API 命令还没结束），新命令排进该 buffer 的队列，按顺序执行。
  - 用户在提示符上已经输入、还没发送的文字会先存起来，命令结束后再放回，不会被覆盖。
  - `+eshell-run` 只排队，返回后由计时器调度命令；默认不显示或选择窗口。等待队列
    最多容纳 `+eshell-max-queued-commands` 条，满时调用直接报错。
  - 每条 API 命令从实际开始执行起计时，默认 `+eshell-command-timeout` 秒，可用
    `:timeout` 覆盖。到时停止前台进程、以 `:reason timeout` 结束该命令，后续队列
    在 shell 恢复提示符后继续。
- **结果**：一条 API 命令结束时，回调收到一个 plist：
  - 正常结束：`:status`（退出码）、`:output`（这条命令输出的文字）、`:command`、
    `:name`（有任务名时）、`:directory`、`:buffer`。
  - 超时、被取消或 buffer 被关掉：`:status` 为 nil，`:reason` 分别为 `timeout`、
    `cancelled` 或 `killed`。超时和取消会带已产生的部分输出。
  - 启动命令时出错：`:status` 为 nil、`:reason` 为 `error`，`:error` 带错误消息；
    队列仍继续调度。
  - 单条 API 命令最多保留 `+eshell-max-output-chars` 个输出字符；超出时保留末尾，
    结果带 `:truncated t`。普通 Eshell 的历史缓冲区最多保留 5000 行。
  - 回调出错会报告消息，但不阻断剩余命令。杀掉 buffer 会取消其调度和命令计时器。
- **显示**：自动化默认只保留 buffer，不显示。GUI 下 `SPC o s`、`M-x eshell` 和
  显式的 `:display t` 把同一 buffer 显示在可接受键盘焦点的 Aaron UI child frame，
  不压缩现有分屏。Ibuffer、`switch-to-buffer`、`display-buffer` 等通用入口选中
  Eshell buffer 时也转到该浮窗；再次呼出时隐藏 child frame，buffer 和任务保持
  运行。新建 Eshell 先初始化 buffer，不经过原生 `pop-to-buffer`；若旧会话仍占据
  普通窗口，呼出浮窗时撤下该 buffer 并保留分屏布局。TTY 下使用底部普通窗口。
  在 Eshell 浮窗内按 `Cmd-W` 只隐藏浮窗，保留会话、输入和运行中的任务；
  再从会话选择器或 Ibuffer 打开即可继续。输入 `exit` 或点红色按钮会结束
  Eshell buffer；即使它是 Perspective 的最后一个 buffer，也允许结束。
  普通窗口中的 `Cmd-W` 仍按窗格规则处理。
  Eshell 浮窗禁用 Company-box 的嵌套 child frame，补全仍使用 Company 的缓冲区内
  候选框，避免补全窗口在浮窗里报错。
- **输出规整**：很多命令行工具用 ANSI 序列重画同一行（`ESC[nG` 回到列首、`ESC[2K`
  清行），eshell 不认这些序列，会把每一帧叠成一行新输出。这里把列复位改成 `\r`、去掉
  其余光标和清屏序列（颜色序列保留），让 eshell 就地覆盖。落在一段输出末尾的 `\r`
  先缓存，等下一段输出到来再拼上。

## API

| 函数 | 作用 |
| --- | --- |
| `(+eshell-buffer &optional DIRECTORY TASK-NAME)` | 返回 DIRECTORY 所在上下文、可选任务名的 eshell buffer；没有就创建，不显示。忙时不改目录 |
| `(+eshell-run COMMAND &rest KEYS)` | 异步排队，立即返回 buffer。KEYS 有 `:directory`（默认 `default-directory`）、`:name`（任务 buffer 标签）、`:callback`（结束时以结果 plist 调用）、`:timeout`（执行时间上限）、`:display`（非 nil 时显示并选中）。默认不显示 |
| `(+eshell-build-root SOURCE-DIRECTORY MARKERS)` | 在项目内寻找最近的构建文件目录；无匹配时回到源文件目录 |
| `(+eshell-run-build BUILD &rest KEYS)` | 在按项目目录稳定命名的 Eshell 中排队构建。`:run` 指定成功后运行的命令；另支持 `:directory`、`:name`、`:timeout`、`:display`、`:callback`。显式构建默认显示浮窗 |
| `(+eshell-run-sync COMMAND &rest KEYS)` | 仅供批处理调用；交互 Emacs 中报错，避免阻塞窗口。KEYS 有 `:directory`、`:name`、`:timeout`（秒，默认取 `+eshell-sync-timeout`）；超时抛出 `+eshell-timeout` |
| `(+eshell-busy-p &optional BUFFER)` | buffer 正在跑命令，或队列里还有命令时为真 |
| `(+eshell-interrupt &optional BUFFER)` | 中断正在跑的命令，队列保留 |
| `(+eshell-cancel &optional BUFFER)` | 清空队列（各回调收到 `cancelled`），再中断正在跑的命令 |

交互入口：

- `SPC o s` / `M-x command-shell`（`+eshell/toggle`）搜索已有 Eshell 会话或
  新建一个独立会话；在当前浮窗调用时还能选“隐藏当前”。`Cmd-T` 的 Portal 终端
  选择器保持独立。GUI 中选择的 Eshell 以浮窗显示，TTY 使用底部窗口。
- `M-x +eshell/toggle-current` 快速显示或隐藏当前 target 的默认会话。
- `M-x +eshell/new` 新建自动编号的会话；前缀参数可输入会话名。
- `M-x command-shell-run`（`+eshell/run-command`）读一条命令，在项目根（没有项目
  时用当前目录）的 eshell 里排队执行并显示。

在 shell 里：连续重复的命令只记一次历史；以空格开头的命令不进历史（同 shell 的
`ignorespace`，用于带密钥的命令）；`emacs FILE` / `e FILE` 与 `vi FILE` 一样在本
Emacs 里打开文件，不会在 shell 里再起一个 Emacs。

## 语言构建示例

C、C++ 和 Go 源文件里的 `C-c C-c` 保存当前文件后启动异步构建，复用该构建目录的
`*eshell:TARGET/build-项目名-摘要*`。构建会话浮动显示，重复按键在同一会话排队。
`C-u C-c C-c` 会在构建成功后再运行；也可分别设置
`+cpp-eshell-run-after-build` / `+go-eshell-run-after-build` 使普通按键运行。

| 语言 | 有项目构建文件 | 没有项目构建文件 |
| --- | --- | --- |
| C/C++ | 最近的 Makefile 目录执行 `make` | 在源文件目录用 `cc` / `c++` 编译当前文件，`-o 文件名.out` |
| Go | 最近的 Makefile 执行 `make`；否则 `go.mod` 目录执行 `go build ./...` | 在源文件目录执行 `go build -o 文件名.out 当前文件` |

单文件构建请求运行时执行 `./文件名.out`；Go module 默认用 `go run .`。
这个语言示例目前只接本地文件；Eshell 自动化 API 本身仍支持已配置的远端 target。
Makefile 项目的运行目标无法从构建命令可靠推断，需配置
`+cpp-eshell-run-command` / `+go-eshell-run-command`；构建命令可分别用
`+cpp-eshell-build-command` / `+go-eshell-build-command` 覆盖。命令设置位于各自的
`lang/cpp/config.el` 和 `lang/go/config.el`，可在受信任的 `.dir-locals.el` 中按
项目覆盖。任务通过现有 `+eshell-run` 取得退出码、超时与输出上限；需要交互式
输入或终端画面的程序仍用 Portal。

此示例参考 [VS Code Makefile Tools 的构建目标与运行目标设置](https://github.com/microsoft/vscode-makefile-tools/blob/main/README.md)
以及 [Code Runner 的语言命令、保存与工作目录设置](https://github.com/formulahendry/vscode-code-runner#configuration)。
当前只实现直接构建/运行入口，没有复制其配置发现、调试或编辑器集成。

## 契约

- **C1 上下文**：`+eshell-buffer` 对同一上下文返回同一个 buffer；空闲时切到调用给
  的目录，首个提示符也显示该目录；忙时不改变目录。本机目录和 `/fs:local:` 目录是
  两种写法，结果一致。测试：`command-shell-c1-buffer-is-per-context`、
  `command-shell-c1-first-prompt-shows-target-directory`、`command-shell-c1-fs-local-parity`。
- **C2 结果**：`+eshell-run-sync` 返回命令的退出码和输出，外部程序和 eshell 内建命令
  都适用。测试：`command-shell-c2-sync-returns-status-and-output`。
- **C3 队列**：`+eshell-run` 返回时命令尚未开始；buffer 忙时，后来的命令排队按顺
  序跑，且不提前改变当前目录，各自得到自己的结果。测试：
  `command-shell-c3-async-call-returns-before-dispatch`、
  `command-shell-c3-commands-queue-in-order`、
  `command-shell-c3-queued-directory-waits-for-dispatch`。
- **C4 不覆盖用户输入**：提示符上未发送的输入在 API 命令结束后放回原处。测试：
  `command-shell-c4-pending-input-is-restored`。
- **C5 取消与关闭**：`+eshell-cancel` 和关闭 buffer 都会让所有未完成的回调收到结果，
  并取消本模块的计时器；回调或命令启动抛错不影响后续队列。测试：
  `command-shell-c5-cancel-and-kill-resolve-callbacks`、
  `command-shell-c5-callback-error-does-not-stall-queue`、
  `command-shell-c5-start-error-resolves-and-continues`。
- **C6 超时有界**：异步命令超过 `:timeout` 后回调收到 `timeout`，后续队列继续；
  批处理的 `+eshell-run-sync` 超时报错，只取消自己的命令；交互 Emacs 中拒绝同步
  调用。测试：
  `command-shell-c6-async-timeout-resolves-and-continues`、
  `command-shell-c6-sync-timeout-interrupts`、`command-shell-c6-sync-is-batch-only`、
  `command-shell-c6-sync-timeout-keeps-other-commands`。
- **C7 输出规整**：`ESC[nG` 重画的进度行就地覆盖，颜色保留。测试：
  `command-shell-c7-redraw-sequences-overwrite`。
- **C8 资源上限**：等待队列和单条输出均有上限，超出的输出标记 `:truncated`。
  测试：`command-shell-c8-output-and-queue-are-bounded`。
- **C9 显示不占分屏**：GUI 的直接 `M-x eshell` 和显式显示入口都使用可聚焦的
  Aaron UI child frame；标题栏可拖动，红黄绿按钮分别关闭、隐藏、切换大小。隐藏后保留
  buffer 和运行中的命令，关闭结束 buffer。Ibuffer 与普通 buffer 打开入口也走
  浮窗。TTY 回退到底部窗口。测试：
  `command-shell-c9-direct-eshell-uses-display-surface`（显示入口）；GUI smoke 核对
  child frame、分屏几何不变、隐藏和焦点返回。

## 已知差距

- **G1 远程验证**：远程 target 上的 C1–C6 还没有在 Aaron-PC 上做真机验证。按
  `docs/acceptance/remote-parity.md` 的要求，这一项完成前，远程只算“经文件名处理器
  可用”，不算达到对等。
- **G2 进程内边界**：Eshell 的内建命令和输出处理在 Emacs 进程中运行；纯 Elisp
  死循环会阻止计时器运行，任意恶意命令或 Emacs 原生崩溃也无法由本 API 隔离。
  需要强隔离的任务必须交给独立进程和 Remote 任务接口。
- **G3 GUI 输入验收**：macOS GUI smoke 已核对 child frame、分屏几何不变、隐藏
  和焦点返回；实际键盘事件、父 frame 销毁及其他 GUI 环境尚未手动验收。若该环境的
  child frame 焦点不可靠，需要回退或改用独立 frame。

## 实现位置

| 部分 | 位置 |
| --- | --- |
| API、队列、输出规整、交互入口 | `lisp/term/shell/eshell.el` |
| eshell 的基础设置、上下文命名、按上下文复用 | `lisp/term/shell/config.el` |
| 键 `SPC o s` | `lisp/editor/evil/config.el` |
| Eshell 起始目录 | `lisp/term/shell/config.el`：直接从调用目录启动，首个提示符即显示该目录 |

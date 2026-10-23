# 窗口、buffer 与关闭

本文定义“关闭”和“回退”：一个窗口失去它显示的 buffer 时发生什么，各类 buffer
被关闭时经过哪些状态。它覆盖普通 buffer、Portal 终端、Noema 页面与其他 xwidget、
popup 和 frame。键位清单在 [daily-usage.md](../guide/daily-usage.md)，这里只定义行为。

> 状态：**现状已记录，目标状态机待设计**（见“待定设计”）。实现应以“契约”为准；
> “已知差距”列出的是契约尚未成立或尚未定义的地方。

## 范围

- 管：窗口关闭、buffer 关闭（kill / bury / quit）、frame 关闭、关闭后窗口显示什么。
- 不管：buffer 内容的保存与撤销；Remote 断开时 buffer 的处理（见
  [remote-framework.md](remote-framework.md)）；Emacs 作为后台应用时的隐藏（见
  `AGENT.md` “Background application”）。

## 模型

### buffer 的种类

| 种类 | 例子 | 能否同时显示在多个窗口 | 识别方式（现状） |
| --- | --- | --- | --- |
| 普通 | 文件、Dired、帮助、输出 | 能 | 默认 |
| 单实例 surface | Portal 终端、Portal Web、Noema 页面、xwidget 网页 | **不能**：原生视图只能在一个窗口里；在另一个窗口显示会把它从旧窗口移走 | 各模块各自判断：`portal-view--target`、`portal-terminal-mode`、`xwidget-webkit-mode` |
| popup | popper 弹窗、Portal popup 终端 | 由 popup 管理器决定 | `popper`、`my/portal-popup-instance-p` |
| 占位 | `*scratch*`、Dashboard、主目录 Dired | 能 | 名字或 mode |

“单实例 surface” 目前没有统一的定义或谓词，见 G3。

### 关闭动作（现状）

| 动作 | 普通 buffer | Portal 终端 | Noema 页面 / xwidget / Portal Web | EWW |
| --- | --- | --- | --- | --- |
| `Cmd-W`（`M-w`） | `delete-window` | 原生键表里是 `delete-window` | `my/browser-close-current`：kill buffer 并删除窗口 | `delete-window` |
| `H-q` | `my/kill-buffer-dwim`：文件 buffer kill；只读或 `*…*` buffer bury，若窗口有 quit-restore 记录则 `quit-window` | 同左 | 同左 | 同左 |
| `H-w` | `my/delete-frame-dwim`：多于一个 frame 时删 frame，否则 bury | 同左 | 同左 | 同左 |
| `H-0` | `delete-window` | 同左 | 同左 | 同左 |
| `SPC b z` | `my/window-bury-current-buffer` | 同左 | 同左 | 同左 |

由此可见同一个 `Cmd-W` 在不同种类上含义不同：普通 buffer 和终端只关窗口，页面连
buffer 一起结束。

### 回退：窗口失去 buffer 后显示什么（现状）

触发点有三个：`quit-window`（advice `my/window-quit-return-a`）、kill buffer
（`kill-buffer-hook` 中的 `my/window-kill-buffer-h`，对 `portal-terminal-mode`
不生效）、bury（`my/window-bury-current-buffer`）。`delete-window` 不回退，窗口
直接消失。

1. **选候选**（`my/window--return-buffer`）：按该窗口的 `window-prev-buffers`
   从新到旧，取第一个满足下列条件的 buffer：活着、不是正在关闭的那个、通过 frame
   的 `buffer-predicate`、不被跳过规则排除、不是 `*scratch*`、不是隐藏 buffer。
2. **跳过规则**（`switch-to-prev-buffer-skip` =
   `my/window-skip-dashboard-and-xwidget-p`，Emacs 原生的回退也用它）：
   - Portal 视图（有 `portal-view--target`）已在别的窗口显示：总是跳过；
   - 在 bury / kill / quit 时还跳过：已在别的窗口显示的任何 buffer、Dashboard、
     **任何** xwidget buffer（不论是否已显示）。
3. **修复**（`my/window--repair-after-close`）：若窗口最终显示的是重复可见的
   buffer、`*scratch*`、Dashboard 或主目录 Dired，则依次改为：第 1 步的候选；
   删掉这个多余窗口；它是最后一个窗口时打开项目目录（在主目录时打开 Dashboard）。

### frame

- 关掉唯一可见的 frame 会隐藏 Emacs，而不是退出（`init-background.el`）。
- 为一个资源打开的 frame 拥有该资源：关 frame 结束资源，隐藏 frame 保留资源
  （Portal 终端：`my/portal--delete-frame-terminal`）。

## 契约

现在已经成立、改动时必须保持的规则。编号稳定，测试与修复引用编号。

- **C1** 回退不会选中正在关闭的 buffer，也不会选中 `*scratch*` 或隐藏 buffer。
- **C2** 回退不会把一个已在别的窗口显示的 Portal 视图放进这个窗口。
- **C3** bury / kill / quit 之后，窗口不会停在一个已在别处显示的 buffer、Dashboard
  或 xwidget 上；没有合格候选时，多余窗口被删除，最后一个窗口打开项目目录。
- **C4** 关掉唯一可见的 frame 只隐藏 Emacs。
- **C5** 关闭为某资源打开的 frame 会结束该资源；隐藏它不会。（已提交的实现是
  `my/portal--delete-frame-terminal`；另一会话未提交的改动删除了它，合并时须确认 C5
  由什么保证，或把它改成差距。）

测试：`test/init-windows-tests.el`、`test/init-frame-buffers-tests.el`。契约与
测试的对应关系尚未逐条标注（G4）。

## 实现位置

| 部分 | 位置 |
| --- | --- |
| 跳过规则、回退候选、修复、bury、`H-q` | `lisp/init-windows.el` “Buffer management helpers” |
| 全局 `Cmd-W`、`H-0` | `lisp/init-windows.el`（`M-w`）、`lisp/init-macos.el` |
| `H-w` | `lisp/init-base.el` `my/delete-frame-dwim` |
| 页面关闭 | `lisp/init-browser.el` `my/browser-close-current` |
| Portal 终端的关闭、移动与 frame 所有权 | `lisp/init-portal.el`、`lisp/init-portal-popup.el` |
| 后台 frame | `lisp/init-background.el` |

## 已知差距

契约尚未成立或尚未定义的地方。修好一条，就在同一提交里删除它并补上对应契约。

- **G1 关闭没有统一的状态机。** 终端和 Noema 编辑页用 `Cmd-W` 或其他方式退出时，
  只是删窗口（终端）或直接结束 buffer（页面），不像普通 buffer 那样经过
  “显示 → 隐藏/埋藏 → 结束”的过程。同一个动作在不同种类上含义不同（见“关闭动作”）。
- **G2 回退会落到已打开的单实例 buffer 上。** 窗口回退时会选中“只能在一个窗口
  显示、且已在别处打开”的 buffer（Portal 终端、xwidget 页面）。C2 只覆盖带
  `portal-view--target` 的视图；xwidget 只在 bury/kill/quit 时跳过；不经过本文
  三个触发点的原生回退路径没有统一处理。
- **G3 “单实例 surface” 没有统一定义。** 每个模块用自己的 mode 或变量判断，跳过
  规则只能逐个列举。
- **G4 契约与测试未对应。** 现有测试没有标注它们保证的是哪条契约。

另一个会话正在修改 `init-windows.el` 与 `init-portal.el` 中的这部分逻辑（未提交），
合并后应重新核对本节。

## 待定设计

需要人来决定，写定后移入“模型”和“契约”：

1. 状态机的状态与转移：例如 *显示中 → 未显示（仍存活，可切回）→ 结束*；每个关闭
   动作（`Cmd-W`、`H-q`、`H-w`、`H-0`、关 frame）在每种 buffer 上走哪条转移。
2. 单实例 surface 的统一谓词：由谁声明（buffer-local 变量、mode 属性或注册表），
   跳过规则与 `display-buffer` 如何使用它。
3. 终端被“关窗口”后是继续运行（变成标签可切回）还是结束；Noema 页面被关闭时是否
   保留为可切回的状态。
4. 回退候选全被排除时的统一结果（当前：删窗口 / 项目目录 / Dashboard）。

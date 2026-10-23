# 窗口、buffer 与关闭

本文定义“关闭”和“回退”：一个窗口失去它显示的 buffer 时发生什么，各类 buffer
被关闭时经过哪些状态。它覆盖普通 buffer、Portal 终端、Noema 页面与其他 xwidget、
popup 和 frame。键位清单在 [daily-usage.md](../guide/daily-usage.md)，这里只定义行为。

> 状态：**现状已记录；目标状态机有提案，待确认**（见“待定设计”）。实现应以“契约”为准；
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

需要人来决定，写定后移入“模型”和“契约”。每个问题下是一份**提案（草案，未确认）**；
确认、修改或否决后删掉“提案”字样，把结论移入正文。

### Q1 状态机

提案：每个 buffer 只有三个状态，所有种类共用：

```text
           显示                 结束
  显示中 ───────→ 未显示 ───────→ 已结束
    ↑     ←───────   │
    └──── 切回 ──────┘
```

- **显示中**：至少在一个窗口里（单实例 surface 恰好一个）。
- **未显示**：仍存活、可切回（buffer 列表、终端标签、`window-prev-buffers`），
  不占窗口。终端进程继续运行；Noema 页面保留 xwidget，渲染由现有的按客户端活动
  快照停掉（不可见页面不活动）。
- **已结束**：kill；资源释放。只能由“结束”类动作进入。

动作分成三个动词，按动词而不是按 buffer 种类决定转移：

| 动词 | 键 | buffer 的转移 | 窗口的结果 |
| --- | --- | --- | --- |
| 关视图 | `Cmd-W` | 显示中 → 未显示 | 按回退规则显示上一个合格 buffer；没有则删掉多余窗口；最后一个窗口打开项目目录 / Dashboard |
| 结束 | `H-q` | → 已结束（终端有运行中的前台进程时先确认；页面先保存草稿） | 同上 |
| 关窗口 | `H-0` | 若这是它唯一的窗口：显示中 → 未显示 | 窗口消失，不回退 |
| 关 frame | `H-w` | frame 中各 buffer：显示中 → 未显示；为某资源打开的 frame（C5）中的该资源 → 已结束 | frame 消失；唯一可见 frame 只隐藏（C4） |

与现状的差别：`Cmd-W` 在页面上不再结束 buffer（`my/browser-close-current` 改为
“关视图”），在普通 buffer 和终端上不再直接删窗口，而是先回退。

拟定契约：

- **C6** 同一个动词在所有种类的 buffer 上做同一个状态转移。
- **C7** 只有“结束”类动作（`H-q`、显式 kill、资源 frame 的关闭、Remote 断开）把
  buffer 带入已结束；“关视图”“关窗口”从不结束 buffer。

### Q2 单实例 surface 的统一谓词

提案：一个 buffer-local 变量 `my/buffer-single-instance`，由各自的 mode 在启用时
设为 t（`portal-terminal-mode`、`portal-web-mode`、`xwidget-webkit-mode`，因此 Noema
页面也包括在内）；判断只经 `my/buffer-single-instance-p`。跳过规则、回退、
`display-buffer` 都只用这个谓词，不再列举 mode 或变量。

拟定契约：

- **C8** 任何回退路径（关视图、结束、bury、quit，以及 Emacs 原生的
  `switch-to-prev-buffer`）都不会选中一个已在别的窗口显示的单实例 buffer。
  未显示的单实例 buffer 可以被选中：那是把它切回来。
- **C9** 在一个窗口显示已在别处显示的单实例 buffer，会把它从旧窗口移来（旧窗口
  按回退规则补位），而不是在两处同时显示或静默失败。

### Q3 终端与 Noema 页面被关视图后

提案：都进入“未显示”。终端继续运行，出现在终端标签与终端选择器里；页面保留编辑
状态与撤销历史，可以从 buffer 列表或再次打开同一文件切回。为限制内存，未显示的
Noema 页面超过 `my/noema-detached-page-limit`（拟加入 config 注册表）时，最久未显示
的页面先保存草稿再结束。

### Q4 回退候选全被排除时

提案：保持现状并写成契约：

- **C10** 回退没有合格候选时：多余窗口被删除；最后一个窗口打开当前项目目录，项目
  是主目录或不存在时打开 Dashboard。

### 提案落实后关闭的差距

Q1 关闭 G1；Q2 关闭 G2、G3；实现时为 C1–C10 各补一个以编号命名的测试，关闭 G4。

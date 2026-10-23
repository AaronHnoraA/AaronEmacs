# 窗口、buffer 与关闭

本文定义“关闭”和“回退”：各类 buffer 被关闭时经过哪些状态，一个窗口失去它显示的
buffer 时显示什么。它覆盖普通 buffer、Portal 终端、Noema 页面与其他 xwidget、popup
和 frame。键位清单在 [daily-usage.md](../guide/daily-usage.md)，这里只定义行为。

## 范围

- 管：关视图、结束 buffer、关窗口、关 frame，以及之后窗口显示什么；原生视图
  （Portal 终端、Portal Web 上的 Noema 页面）与 Emacs 之间谁拿键盘。
- 不管：buffer 内容的保存与撤销；Remote 断开时 buffer 的处理（见
  [remote-framework.md](remote-framework.md)）；Emacs 作为后台应用时的隐藏（见
  `AGENT.md` “Background application”）。

## 模型

### 三个状态

所有种类的 buffer 共用一个状态机：

```text
           关视图 / 关窗口 / 关 frame        结束
  显示中 ─────────────────────────→ 未显示 ─────────→ 已结束
    ↑          ←─── 切回 ───            │
    └─────────────── 结束 ──────────────┴──────────→ 已结束
```

- **显示中**：至少在一个窗口里；单实例 buffer 恰好在一个窗口里。
- **未显示**：仍存活、可切回（buffer 列表、终端标签与选择器、`window-prev-buffers`），
  不占窗口。终端程序继续运行；Noema 页面保留编辑状态，渲染由按客户端活动快照停掉。
- **已结束**：buffer 被 kill，资源释放。只有“结束”动作进入这里。

### buffer 的种类

| 种类 | 例子 | 能否同时在多个窗口 | 判定 |
| --- | --- | --- | --- |
| 普通 | 文件、Dired、帮助、输出 | 能 | 默认 |
| 单实例 | Portal 终端、Portal Web、Excalidraw、Noema 页面、xwidget 网页 | 不能：原生视图只能在一个窗口里，显示到别处会把它移走 | `my/buffer-single-instance-p` |
| popup | popper 弹窗、Portal popup 终端 | 由 popup 管理器决定 | `popper`、`my/portal-popup-instance-p` |
| 占位 | `*scratch*`、Dashboard、主目录 Dired | 能 | 名字或 mode |

`my/buffer-single-instance-p` 是唯一的判定：mode 在 `my/single-instance-modes`
中、buffer 带有 Portal 视图（`portal-view--target`），或 buffer 自己把
`my/buffer-single-instance` 设为 t。窗口规则只问它，不再列举 mode 或包变量。

### 关闭动作

按动词，而不是按 buffer 种类，决定状态转移：

| 动词 | 键 / 命令 | buffer | 窗口 |
| --- | --- | --- | --- |
| 关视图 | `Cmd-W`（`M-w`）`my/close-view`；`SPC b z`；最后一个 frame 上的 `H-w` | 显示中 → 未显示 | 回退（见下）；popup 由原生 quit 精确返回来源 |
| 结束 | `H-q` `my/kill-buffer-dwim`；`C-c w d`（浏览后端） | → 已结束；终端程序运行中先确认（Portal 的 kill 询问）。只读或 `*…*` 的普通 buffer 只关视图 | 回退 |
| 关窗口 | `H-0` `delete-window` | 它的最后一个窗口消失时：显示中 → 未显示 | 窗口消失，不回退 |
| 关 frame | `H-w` `my/delete-frame-dwim`（多于一个 frame 时） | frame 中各 buffer：显示中 → 未显示；终端 frame 也保留终端 | frame 消失；唯一可见 frame 只隐藏 |

每种 buffer 上，`Cmd-W` 都只关视图：终端、页面、EWW、Portal Web 和普通 buffer 一样。

### 回退：窗口失去 buffer 后显示什么

触发点有三个：`quit-window`（advice `my/window-quit-return-a`）、kill
（`kill-buffer-hook` 中的 `my/window-kill-buffer-h`；Portal 终端由 Portal 侧自行
收尾）、bury（`my/window-bury-current-buffer`，关视图走这里）。关窗口不回退。

1. **选候选**（`my/window--return-buffer`）：按该窗口的 `window-prev-buffers`
   从新到旧，取第一个活着、不是正在关闭的、通过 frame 的 `buffer-predicate`、
   不被跳过规则排除、不是 `*scratch*`、不是隐藏 buffer 的 buffer。
2. **跳过规则**（`switch-to-prev-buffer-skip` =
   `my/window-skip-dashboard-and-xwidget-p`，Emacs 原生回退也用它）：
   - 已在别的窗口显示的单实例 buffer：任何路径都跳过；
   - 关视图 / 结束 / quit 时还跳过：已在别的窗口显示的任何 buffer、Dashboard、
     xwidget 页面（即使未显示：Markdown 重定向结束临时 buffer 时，窗口不能落回旧页面；
     页面只由显式切换带回）。
3. **修复**（`my/window--repair-after-close`）：窗口最终显示的是重复可见的 buffer、
   `*scratch*`、Dashboard 或主目录 Dired 时，改为第 1 步的候选；没有候选时删掉这个
   多余窗口；最后一个窗口打开项目目录（主目录或无项目时打开 Dashboard）。

### 原生视图的键盘归属

原生视图的 buffer 有两种键盘状态：视图持有（mode line `[app]`）与 Emacs 持有
（`[emacs]`）。视图里按下它不用的组合键会交给 Emacs 执行一条命令（handoff）。
Emacs 持有分两种：handoff 期间的暂时持有，命令结束后视图取回键盘；以及明确持有
（`Cmd-.` 退出视图，或 handoff 的命令打开了 minibuffer，如 `M-x`），之后的命令都在
Emacs 里，直到回车 / 点击进入视图，或者移到别的窗口或 buffer 再回来。

### 未显示的 Noema 页面

未显示的页面最多保留 `my/noema-detached-page-limit` 个（config 注册表，默认 8，nil
不限）；超过时最久未显示的页面被结束。页面编辑时自动保存（约 650 ms），结束不丢
文字；共享的 app buffer 不计入。检查在 Noema 活动快照的去抖转换里进行
（`my/noema--trim-detached-pages`）。

## 契约

编号稳定；测试名以编号开头或在此列出。

- **C1** 回退不会选中正在关闭的 buffer、`*scratch*` 或隐藏 buffer。
  测试：`my/window-repair-candidate-respects-native-frame-predicate`、
  `my/window-repair-candidate-skips-xwidget-history`。
- **C2** 已并入 C8。
- **C3** 关视图 / 结束 / quit 之后，窗口不会停在已在别处显示的 buffer、Dashboard
  或 xwidget 页面上。测试：`my/window-fallback-skips-xwidget-after-kill`、
  `my/window-fallback-skips-dashboard-on-quit`、`…-on-kill`。
- **C4** 关掉唯一可见的 frame 只隐藏 Emacs。测试：缺（G6）。
- **C5** 关 frame 不结束其中的 buffer，终端 frame 也一样。测试：
  `my/portal-deleting-frame-keeps-terminal-session`。
- **C6** 同一个动词在所有种类的 buffer 上做同一个状态转移。测试：
  `c6-close-view-is-one-transition-for-every-kind`、
  `c6-close-view-closes-a-spare-window-without-history`。
- **C7** 只有结束动作把 buffer 带入已结束；关视图、关窗口、关 frame 从不结束 buffer。
  测试：`c7-only-ending-actions-end-buffers`、
  `c7-ending-a-special-buffer-only-closes-its-view`。
- **C8** 任何回退路径都不会选中已在别的窗口显示的单实例 buffer；未显示的单实例
  buffer 可以被选中（xwidget 页面除外，见 C3）。测试：
  `c8-fallback-never-takes-a-single-instance-buffer-shown-elsewhere`、
  `c8-an-undisplayed-terminal-may-come-back`、
  `c8-a-buffer-can-declare-itself-single-instance`、
  `my/window-fallback-never-duplicates-visible-portal-view`。
- **C9** 在一个窗口显示已在别处显示的单实例 buffer，会把它从旧窗口移来，旧窗口
  按回退规则补位。测试（Portal）：`my/portal-moving-buffer-vacates-its-old-window`、
  `my/portal-native-sync-vacates-duplicate-before-placement`。
- **C10** 回退没有合格候选时：多余窗口被删除；最后一个窗口打开当前项目目录，主目录
  或无项目时打开 Dashboard。测试：`c10-last-window-without-candidates-stays-open`、
  `my/window-close-last-page-opens-project-directory`、
  `my/window-quit-home-fallback-opens-dashboard`。
- **C11** 未显示的 Noema 页面不超过 `my/noema-detached-page-limit` 个，超出时先结束
  最久未显示的。测试：`q3-detached-pages-beyond-the-limit-are-ended`。

- **C12** 视图交给 Emacs 的命令若读 minibuffer（`M-x`、提示输入），结束后 Emacs
  明确持有键盘；不读 minibuffer 的快捷键结束后视图取回键盘。测试：
  `c12-a-handed-off-prompt-leaves-emacs-the-keyboard`。
- **C13** Emacs 明确持有键盘时，离开 minibuffer、frame 重新激活或视图布局变化都不把
  键盘还给视图；只有进入视图（`RET`、点击、显式聚焦请求）或窗口 / buffer 真正变化后
  再选回它才会。测试：`c13-emacs-keeps-the-keyboard-until-the-pane-changes`。

测试文件：`test/init-window-lifecycle-tests.el`、`test/init-windows-tests.el`，
均由 `make ui-test` 运行。

## 实现位置

| 部分 | 位置 |
| --- | --- |
| 单实例判定、跳过规则、回退候选、修复 | `lisp/init-windows.el` “Buffer management helpers” |
| 关视图 `my/close-view`、结束 `my/kill-buffer-dwim`、全局 `M-w` | `lisp/init-windows.el` |
| `H-w` | `lisp/init-base.el` `my/delete-frame-dwim` |
| 浏览后端的 `M-w` 与 `C-c w d` | `lisp/init-browser.el` |
| 终端原生键表的 `s-w`、终端移动与 frame 关闭 | `lisp/init-portal.el`、`lisp/init-portal-popup.el` |
| 原生视图键盘归属 | `lisp/init-portal.el` `my/portal--focus-selected-window` |
| 未显示页面上限 | `lisp/roam/init-noema.el` |
| 后台 frame | `lisp/init-background.el` |

## 已知差距

- **G5** C9 只对 Portal 终端有测试；Noema 页面（每个 frame 有自己的编辑页）与
  Portal Web 是否满足 C9 尚未核对。
- **G6** C4 没有测试。

## 待定设计

无。

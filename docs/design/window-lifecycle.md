# 窗口、buffer 与关闭

本文定义“关闭”和“回退”：各类 buffer 被关闭时经过哪些状态，一个窗口失去它显示的
buffer 时显示什么。它覆盖普通 buffer、Portal 终端、Noema 页面与其他 xwidget、popup
和 frame。键位清单在 [daily-usage.md](../guide/daily-usage.md)，这里只定义行为。

## 范围

- 管：关视图、结束 buffer、关窗口、关 frame，以及之后窗口显示什么；切换
  Perspective 工作区时选区的生命周期；原生视图（Portal 终端、Portal Web 上的
  Noema 页面）与 Emacs 之间谁拿键盘。
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

Ibuffer 保留全局 buffer 总览。日常 `C-x b`、`SPC b b` 的 Consult 候选也跨越
所有 frame 和 Perspective，但使用黑名单排除不适合让用户进入或日常进入的
buffer：目前包括 `Fundamental`、minibuffer、JSONRPC 事件和 Emacs Lisp 编译日志，
以及名称以空格开头的临时内部 buffer；同时保留 Consult 默认排除的补全/日志类
候选。隐藏的 Eshell 会话、`*Messages*`、
代码和文档 buffer 仍可搜索；选择 Eshell 时由其浮窗承载。新增无需日常访问的
内部 buffer 时，应加入这份黑名单。需要按 Perspective 操作 buffer 时，使用
该工作区的专用入口。

### buffer 的种类

| 种类 | 例子 | 能否同时在多个窗口 | 判定 |
| --- | --- | --- | --- |
| 普通 | 文件、Dired、帮助、输出 | 能 | 默认 |
| 单实例 | Portal 终端、Portal Web、Excalidraw、Noema 页面、xwidget 网页 | 不能：原生视图只能在一个窗口里，显示到别处会把它移走 | `+buffer-single-instance-p` |
| popup | popper 弹窗、Portal popup 终端、Eshell 与统一监控浮窗 | 由 popup 管理器决定 | `popper`、`+portal-popup-instance-p`、posframe child frame |
| 占位 | `*scratch*`、Dashboard、主目录 Dired | 能 | 名字或 mode |

`+buffer-single-instance-p` 是唯一的判定：mode 在 `+single-instance-modes`
中、buffer 带有 Portal 视图（`portal-view--target`），或 buffer 自己把
`+buffer-single-instance` 设为 t。窗口规则只问它，不再列举 mode 或包变量。

GUI 中的 Eshell 和统一监控板面通过可聚焦的 Aaron UI child frame 显示；
Dashboard `[monitor]` 与皮卡丘打开同一个监控 buffer，分别选中性能总览和后台任务。
标题栏可拖动、切换大小、隐藏或关闭。隐藏 Eshell 时保留运行中的命令和队列；
隐藏监控时保留页签与筛选状态，并停止性能采样及后台任务页的自动刷新。
关闭会结束对应 buffer。
Ibuffer 与普通 buffer 打开入口选中 Eshell 时也走浮窗。TTY 使用普通窗口。

### 关闭动作

按动词，而不是按 buffer 种类，决定状态转移：

| 动词 | 键 / 命令 | buffer | 窗口 |
| --- | --- | --- | --- |
| 关视图 | `SPC b z`；最后一个 frame 上的 `H-w`；唯一主窗格上的 `Cmd-W`（`+windows/close-view`） | 当前窗口撤下 buffer；若无别的窗口显示它则变为未显示 | 回退（见下）；popup 由原生 quit 精确返回来源 |
| 结束 | `H-q` `+windows/kill-buffer-dwim`；`C-c w d`（浏览后端） | → 已结束；终端程序运行中先确认（Portal 的 kill 询问）。只读或 `*…*` 的普通 buffer 只关视图 | 回退 |
| 关窗口 | `Cmd-W`（`M-w`）`+windows/close-window`；`H-0` `delete-window` | 它的最后一个窗口消失时：显示中 → 未显示 | `Cmd-W` 删选中的侧窗格或多余主窗格；唯一主窗格时关视图，frame 保留 |
| 关 frame | `H-w` `aaron/delete-frame-dwim`（多于一个 frame 时） | frame 中各 buffer 若无别的窗口显示则变为未显示；终端 buffer 保留 | 多于一个 frame 时 frame 消失；只有一个 frame 时只关当前视图 |

`Cmd-W` 在普通 buffer、终端、Noema 页面和浏览视图上执行相同的窗格动作；不调用
`kill-buffer`。唯一主窗格时（即使 Treemacs 等 side window 仍在）用既有回退规则，
popup 按原生 `quit-window` 返回来源。Eshell 的 Aaron UI child frame 是例外：
在该浮窗内按 `Cmd-W` 只隐藏 child frame，保留 buffer、输入与任务；在普通窗口
显示的 Eshell 仍走窗格动作。
`Cmd-Shift-W`（`M-W`）继续调用 Perspective 的 buffer 选择器，选定后才结束 buffer；
它不参与 `Cmd-W` 的关闭动作。Noema 未显示页面的数量清理另见下文。

### Command-W 两键的分支

1. 普通 macOS Emacs 把物理 Command 映射为 Meta；Portal 原生视图把 `s-w` / `s-W`
   分别交给 Emacs 的关窗格命令和 Perspective 选择器；Noema xwidget 编辑区把
   `M-w` / `M-W` 转交给 Emacs，页面输入框中的 `Cmd-W` / `Cmd-Shift-W` 也交给 Emacs。
   Shift 只决定两个不同命令，不改变关闭深度。
2. `Cmd-W` 若正处于 minibuffer，先退出提示；否则，选中的侧窗格或多余主窗格执行
   `delete-window`，唯一主窗格执行 `+windows/close-view`。side window 不算可替代
   主窗格；后一分支保留 frame，按同窗格历史回退；popup 用 `quit-window` 返回来源。没有合格历史候选时显示
   项目目录，主目录或无项目时显示 Dashboard。两个分支都不 kill buffer。
3. `Cmd-Shift-W` 打开当前 Perspective 的 buffer 选择器；取消不改布局，选择
   buffer 后调用 `kill-buffer`。若选中正在显示的 buffer，原生 kill 后按回退规则
   修复窗口；选中别处的 buffer 时当前窗格不变。`C-x k` 使用同一个选择器。

### 回退：窗口失去 buffer 后显示什么

触发点有三个：`quit-window`（advice `+window-quit-return-a`）、kill
（`kill-buffer-hook` 中的 `+window-kill-buffer-h`；Portal 终端由 Portal 侧自行
收尾）、bury（`+windows/window-bury-current-buffer`，关视图走这里）。关窗口不回退。

1. **选候选**（`+window--return-buffer`）：按该窗口的 `window-prev-buffers`
   从新到旧，取第一个活着、不是正在关闭的、通过 frame 的 `buffer-predicate`、
   不被跳过规则排除、不是 `*scratch*`、不是隐藏 buffer 的 buffer。
2. **跳过规则**（`switch-to-prev-buffer-skip` =
   `+window-skip-dashboard-and-xwidget-p`，Emacs 原生回退也用它）：
   - 已在别的窗口显示的单实例 buffer：任何路径都跳过；
   - 关视图 / 结束 / quit 时还跳过：已在别的窗口显示的任何 buffer、Dashboard、
     xwidget 页面（即使未显示：Markdown 重定向结束临时 buffer 时，窗口不能落回旧页面；
     页面只由显式切换带回）。
3. **修复**（`+window--repair-after-close`）：窗口最终显示的是重复可见的 buffer、
   `*scratch*`、Dashboard 或主目录 Dired 时，改为第 1 步的候选；没有候选时删掉这个
   多余窗口；最后一个主窗口打开项目目录（主目录或无项目时打开 Dashboard）。

### 原生视图的键盘归属

原生视图的 buffer 有两种键盘状态：视图持有（mode line `[app]`）与 Emacs 持有
（`[emacs]`）。视图里按下它不用的组合键会交给 Emacs 执行一条命令（handoff）。
Portal Web（含 Noema Markdown）和 Portal 终端的 mode line 只用这个标记说明
键盘归属；即使 buffer 内的 Evil 状态仍是 `normal`，也不显示 Evil 状态色块。
Emacs 持有分两种：handoff 期间的暂时持有，命令结束后视图取回键盘；以及明确持有
（`Cmd-.` 退出视图，或 handoff 的命令打开了 minibuffer，如 `M-x`），之后的命令都在
Emacs 里，直到回车 / 点击进入视图，或者移到别的窗口或 buffer 再回来。

### 工作区切换时的选区

切换 Perspective 工作区前，当前 Emacs buffer 退出 Evil Visual 状态并取消活动
选区。切回原工作区不恢复旧选区；buffer 内容与 mark 位置不变。内部临时切换
（`persp-switch` 的 `norecord` 路径）不触发这项清理。

### 共享 popup 顶栏

Portal 终端与 agent-shell 使用同一行 popup 标签，放在 Emacs 的 tab line。
终端 popup 不另占 header line；agent-shell 的原生会话 header line 保留。
移出 popup 时恢复 buffer 进入前的 tab line 和 header line。

### 未显示的 Noema 页面

未显示的页面最多保留 `+noema-detached-page-limit` 个（config 注册表，默认 8，nil
不限）；超过时最久未显示的页面被结束。页面编辑时自动保存（约 650 ms），结束不丢
文字；共享的 app buffer 不计入。检查在 Noema 活动快照的去抖转换里进行
（`+noema--trim-detached-pages`）。

## 契约

编号稳定；测试名以编号开头或在此列出。

- **C1** 回退不会选中正在关闭的 buffer、`*scratch*` 或隐藏 buffer。
  测试：`+window-repair-candidate-respects-native-frame-predicate`、
  `+window-repair-candidate-skips-xwidget-history`。
- **C2** 已并入 C8。
- **C3** 关视图 / 结束 / quit 之后，窗口不会停在已在别处显示的 buffer、Dashboard
  或 xwidget 页面上。测试：`+window-fallback-skips-xwidget-after-kill`、
  `+window-fallback-skips-dashboard-on-quit`、`…-on-kill`。
- **C4** 关掉唯一可见的 frame 只隐藏 Emacs。测试：缺（G6）。
- **C5** 关 frame 不结束其中的 buffer，终端 frame 也一样。测试：
  `+portal-deleting-frame-keeps-terminal-session`。
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
  `+window-fallback-never-duplicates-visible-portal-view`。
- **C9** 在一个窗口显示已在别处显示的单实例 buffer，会把它从旧窗口移来，旧窗口
  按回退规则补位。测试（Portal）：`+portal-moving-buffer-vacates-its-old-window`、
  `+portal-native-sync-vacates-duplicate-before-placement`。
- **C10** 回退没有合格候选时：多余窗口被删除；最后一个窗口打开当前项目目录，主目录
  或无项目时打开 Dashboard。测试：`c10-last-window-without-candidates-stays-open`、
  `c10-sole-main-view-beside-side-window-has-no-history`、
  `+window-close-last-page-opens-project-directory`、
  `+window-quit-home-fallback-opens-dashboard`。
- **C11** 未显示的 Noema 页面不超过 `+noema-detached-page-limit` 个，超出时先结束
  最久未显示的。测试：`q3-detached-pages-beyond-the-limit-are-ended`。

- **C12** 视图交给 Emacs 的命令若读 minibuffer（`M-x`、提示输入），结束后 Emacs
  明确持有键盘；不读 minibuffer 的快捷键结束后视图取回键盘。测试：
  `c12-a-handed-off-prompt-leaves-emacs-the-keyboard`。
- **C13** Emacs 明确持有键盘时，离开 minibuffer、frame 重新激活或视图布局变化都不把
  键盘还给视图；只有进入视图（`RET`、点击、显式聚焦请求）或窗口 / buffer 真正变化后
  再选回它才会。测试：`c13-emacs-keeps-the-keyboard-until-the-pane-changes`。
- **C14** `Cmd-W` 在侧窗格或有多个主窗格时删当前窗格，唯一主窗格时只关视图并保留 frame；
  side window 不会让唯一主窗格变成可删除窗格；
  当前 buffer 在两条路径中都保持存活。`Cmd-Shift-W` 保持 Perspective 的交互式
  buffer 结束动作。测试：`c14-cmd-w-closes-pane-and-keeps-buffer`、
  `c14-cmd-w-keeps-frame-for-sole-pane`、
  `c14-cmd-w-closes-sole-main-view-beside-side-window`、
  `c14-cmd-w-dismisses-minibuffer-before-changing-layout`、
  `+window-close-select-key-keeps-picker-and-window-history`，以及 Noema 的
  `xwidget-key-guard.test.ts` 中页面输入框转交两键的测试。
- **C15** 共享 popup 标签只占一个 tab line；Portal 终端没有附加 header line，
  agent-shell 保留自身 header line；移出 popup 恢复原 UI。测试：
  `popup-terminal-and-agent-use-one-shared-tab-row`。
- **C16** 切换 Perspective 工作区时，离开的 Emacs buffer 不保留活动选区或
  Evil Visual 状态。测试：`c16-workspace-switch-clears-outgoing-visual-selection`、
  `c16-workspace-switch-clears-outgoing-emacs-selection`。
- **C17** Helpful 的源码按钮在发起帮助的窗口打开源码，Help 窗口保持显示；
  原窗口已不存在或 Helpful 没有独立窗口时，在当前窗口打开，不新建窗格。
  测试：`c17-helpful-source-opens-in-origin-window`、
  `c17-helpful-source-falls-back-when-origin-is-gone`。
- **C18** wdired 和 wgrep 成功应用修改后退出结果视图，buffer 不结束；
  操作失败、仍有未应用的 wgrep 修改，或完成命令并非从当前结果窗口发起时，
  不关闭其他窗口。测试：`c18-wdired-finish-quits-result-view`、
  `c18-wgrep-finish-quits-only-after-complete-apply`。

窗口测试文件：`test/init-window-lifecycle-tests.el`、`test/init-windows-tests.el`，
由 `make ui-test` 运行。popup 顶栏测试在 `test/popup-agent-tests.el`；Noema 页面
转交测试在 `site-lisp/noema/tests/xwidget-key-guard.test.ts`；工作区选区测试在
`test/init-workspaces-tests.el`。

## 实现位置

| 部分 | 位置 |
| --- | --- |
| 单实例判定、跳过规则、回退候选、修复 | `lisp/ui/windows/config.el` “Buffer management helpers” |
| 关视图 `+windows/close-view`、关窗格 `+windows/close-window`、结束 `+windows/kill-buffer-dwim`、全局 `M-w` | `lisp/ui/windows/config.el` |
| `H-w` | `lisp/core/aaron-base.el` `aaron/delete-frame-dwim` |
| 浏览后端的 `M-w` 与 `C-c w d` | `lisp/app/browser/config.el` |
| 终端原生键表的 `s-w`、终端移动、popup 顶栏与 frame 关闭 | `lisp/term/portal/config.el`、`lisp/term/portal/popup.el` |
| 原生视图键盘归属 | `lisp/term/portal/config.el` `+portal--focus-selected-window` |
| 未显示页面上限 | `lisp/noema/bridge/config.el` |
| 后台 frame | `lisp/config/background/config.el` |
| 工作区切换前的选区清理 | `lisp/ui/workspaces/config.el` |
| Helpful 源码跳转 | `lisp/ui/theme/config.el` |
| wdired / wgrep 完成后的视图 | `lisp/emacs/dired/config.el`、`lisp/tools/menus/config.el` |

## 已知差距

- **G5** C9 只对 Portal 终端有测试；Noema 页面（每个 frame 有自己的编辑页）与
  Portal Web 是否满足 C9 尚未核对。
- **G6** C4 没有测试。

## 待定设计

无。

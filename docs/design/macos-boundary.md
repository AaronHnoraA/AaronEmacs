# macOS 边界

本文定义 Emacs 与 macOS 之间的边界：系统把什么交给 Emacs，Emacs 把什么交给系统，
这两个方向各由谁负责。所有这类交互都穿过同一层 `lisp/os/macos/system.el`，
其他模块只声明意图，不直接碰系统。

## 范围

- 管：Finder 与 Dock 打开文件、拖放、系统请求退出；打开、在 Finder 中显示、
  Quick Look、文件进出剪贴板；通知与 Dock 角标；顶层 frame 的拖动抓手。
- 不管：键位与修饰键、帧外观、性能调节（`os/macos/config.el`）；输入法
  （`os/macos/input.el`）；隐藏、Dock 策略与后台应用（`config/background/config.el`，
  见 `AGENT.md` “Background application”）；Noema 页面和 Portal Web 内部的拖放与
  粘贴，它们属于 WebKit，由 Noema 负责。

## 模型

边界有两个方向，每个方向只有一个入口。

```text
 macOS                      os/macos/system.el                 Emacs
 ─────                      ──────────────────                 ─────
 打开文件 ─────────────→ 呈现 ───────────────→ ns-find-file
 退出请求 ─────────────→ 呈现 ───────────────→ save-buffers-kill-emacs
 文件拖放 ─────────────→ 落点 → 动作 → 执行
 URL 拖放 ─────────────→ aaron-open 路由
 点击通知 ─────────────→ 呈现 ───────────────→ 通知的动作
 剪贴板的文件 ─────────→ Dirvish 粘贴
 系统打开 ←──────────── +macos/open-target
 Finder  ←──────────── +macos-reveal
 Quick Look ←────────── +macos/quick-look
 剪贴板  ←──────────── +macos/copy-file-to-clipboard
 通知    ←──────────── aaron-notify（→ +macos-notify）
```

**呈现**：系统交来的东西用户必须看得见。边界本身不知道后台的存在，只运行
`+macos-present-hook`；隐藏 Emacs 的模块把“重新显示”挂在这里。

### 拖放状态机

一次文件拖放走三步，全部在 `os/macos/drop.el`：

```text
落点 ──→ 动作 ──→ 执行
```

**落点**决定含义，文件本身不决定：

| 落点 | 默认动作 | 负责方 |
| --- | --- | --- |
| 正在读文件名的 minibuffer（`C-x C-f`） | 文件夹就进入它，文件就填入它的路径，回车由用户按 | 边界 |
| 其他 minibuffer | 在光标处插入转义路径 | 边界 |
| Eshell、shell、term、Portal 终端 | 在提示符处插入转义路径，不执行 | `set-file-drop-handler!` |
| Org | 原生询问：附件、链接或打开 | buffer 局部的 `dnd-protocol-alist` |
| 普通 buffer、Dired | 打开：文件走打开路由，文件夹进 Dired | 边界 |
| 侧边窗口、专用窗口 | 同上，但在主窗口打开 | 边界 |

**动作**默认不询问。下列情况弹出单键菜单：按住 Option 拖入；落点在其他机器上；
一次超过 `+macos-file-drop-many` 个文件；或 `+macos-file-drop-ask` 设为 `always`。
菜单只列在这里说得通的动作：

| 键 | 动作 | 何时出现 |
| --- | --- | --- |
| `o` | 打开 | 总是 |
| `c` | 复制到此目录 | 总是，跨机器时走 `remote-transfer` |
| `m`、`l` | 移动、建符号链接 | 只在同一台机器上 |
| `i` | 在光标处插入路径 | buffer 可写 |
| `r` | 插入相对路径 | buffer 可写、同一台机器、单个文件 |
| `q` | 取消 | 总是 |

**执行**时，打开总以客户端目录运行，因为拖入的是本机路径，而裸绝对路径在远程
buffer 里会解析到对方机器。复制、移动和符号链接交给 Dirvish 的粘贴管线
（`+macos-drop-receive-function`），覆盖确认和跨机器传输只有这一份实现。

终端里按住 Control 拖入时 macOS 只提供链接操作，Emacs 把路径当文本插入。

### 通知

`aaron-notify` 是唯一入口，不依赖平台；`notify-send` 是旧接口，转发到它。

```text
aaron-notify ──→ 历史（总是）
             ├─ Emacs 有键盘 ──→ 回显区
             └─ 否则 ─────────→ 系统通知 ──→ Dock 角标 +1
                                    └ 点击 → 呈现 → :action，没有就打开历史
```

系统一侧优先用通知中心，启动时申请授权。授权后通知带 Emacs 图标，按来源分组，
点击能回到 Emacs。未授权时退回 `osascript`，通知照常出现，但图标是脚本图标，
点击也没有效果。

### 窗口抓手

指针一靠近顶层 frame 的顶边（内外两侧都算），frame 上方立刻弹出一个抓手，没有延迟
也没有动画，拖它移动窗口。拖动优先交给窗口服务器，不经过 Emacs 主线程；窗口没跟上
时才由抓手自己移动，`macos-window-last-drag` 报告上次走的是哪条路。抓手是独立的子窗口，不占 frame 内容。frame 上方没有空间时，抓手浮在顶边之上，同样不挤压内容。

## 契约

- **C1 单一边界。** Finder 打开、退出请求、文件与 URL 拖放、系统打开、Finder 显示、
  文件进剪贴板和通知都只在 `os/macos/system.el` 实现。其他模块调用这里的入口，
  或通过 `set-file-drop-handler!` 声明落点行为，不自己调 `open`、`osascript` 或
  改 `dnd-protocol-alist` 的全局值。
- **C2 先呈现再处理。** Finder 打开和系统退出在处理之前运行
  `+macos-present-hook`。后台的 Emacs 因此不会把文件打开在隐藏的 frame 里，
  也不会在看不见的地方询问未保存的 buffer。
- **C3 依赖方向。** `os/` 不依赖上层模块：边界只提供钩子和声明接口，
  `config/background` 与终端模块反过来挂到边界上。
- **C4 终端插入路径。** 落到终端类落点的文件变成一行空格分隔的转义路径，末尾留一个
  空格，绝不含换行，绝不替用户执行，也不替换终端窗口。
- **C5 路径属于本机。** Eshell 或 shell 在其他 target 上时，本机路径对它没有意义：
  询问后把文件复制到它的目录并插入文件名，拒绝时什么也不插入。
- **C6 URL 走路由。** 拖入的 URL 由 `aaron-open` 按用户配置的路由打开，绝不打开全局的
  `url-handler-mode`。
- **C7 外发动作只在本机。** 外发动作以客户端目录运行，不受当前 buffer 的 target
  影响；需要文件时只接受本机可直接访问的路径，其他 target 上的文件被拒绝。
- **C8 不依赖额外安装。** 回退路径只用系统自带工具：通知走 `osascript`，不依赖
  `terminal-notifier`。`notify-send` 保持可用。
- **C9 拖放默认是打开。** 普通 buffer 和 Dired 打开文件，文件夹进 Dired；绝不静默
  复制或移动文件。侧边窗口和专用窗口保留原内容。
- **C10 询问只列可行动作。** 何时询问和列出什么见“拖放状态机”。复制、移动、符号链接
  只走 `+macos-drop-receive-function`。
- **C11 minibuffer 优先。** 正在读 minibuffer 时拖入的内容都交给它，不打开文件，
  键盘留在 minibuffer。
- **C12 通知落在用户所在之处。** 每条通知都进入有界历史；Emacs 有键盘时只进回显区，
  否则发系统通知并增加未读。相同 `:id` 替换旧的一条，Emacs 拿回键盘后未读清零。
  批处理的 Emacs 没有用户，绝不发系统通知。
- **C13 点击回到 Emacs。** 点击先呈现，再运行 `:action`，没有动作时打开历史。
- **C14 抓手不占内容。** 抓手是独立窗口，绝不改变 frame 布局；全屏时不出现；只在
  frame 顶边确实露在外面时出现，顶边上的 child frame 算 frame 的一部分。
- **C15 替换原生模块是原子的。** `bin/emacs-app build` 写到旁边再改名，因为运行中的
  Emacs 映射着旧文件，原地覆盖可能让它崩溃。

## 实现位置

| 内容 | 位置 |
| --- | --- |
| 呈现钩子、外发动作、通知后端、剪贴板 | `lisp/os/macos/system.el` |
| 拖放状态机 | `lisp/os/macos/drop.el` |
| `set-file-drop-handler!` | `lisp/os/macos/autoload/drop.el` |
| 通用通知 `aaron-notify`、历史 `aaron/notifications` | `lisp/core/aaron-notify.el` |
| 通知中心、Dock 角标、文件剪贴板 | `tools/macos-background/system.m` |
| 窗口抓手 | `tools/macos-background/window.m` |
| 后台时重新显示 | `config/background/config.el` 的 `+background--present-h` |
| 落点声明 | `term/shell/eshell.el`、`term/shell/config.el`、`term/portal/config.el` |
| 接收文件 | `emacs/dirvish/config.el` 的 `+dirvish-receive-files` |
| 用户菜单 | `lisp/os/macos-tools/config.el` |
| 契约测试 | `test/macos-boundary-tests.el`（`make macos-test`） |

新增一种落点时调用 `set-file-drop-handler!`，处理器在目标 buffer 中被调用，参数是
本机绝对路径的列表。需要提示用户时调用 `aaron-notify`，不要直接调系统工具。

Noema 页面内部的拖放和粘贴不经过这里：文件一律存为附件并插入链接，任何位置的文件
拖放都不会让页面导航走。见 [current-architecture.md](../noema/design/current-architecture.md)。

## 已知差距

- **G1 真实系统事件未实测。** 拖放分派、通知路由和剪贴板都有测试，但测试不产生真实
  的 Finder 拖放。Portal 终端、Option 判定、Dirvish 里拖出文件、通知授权弹窗和窗口
  抓手要在重启后的 Emacs 里手动验证一次。
- **G2 Noema 拿不到拖入文件的路径。** WebKit 只给页面文件内容，所以库内文件也会复制
  一份，文件夹被跳过。要做到 Obsidian 的 Option 拖入（只链接不复制），需要改 Portal
  的原生层。

## 待定设计

- 窗口抓手是否要支持双击缩放。

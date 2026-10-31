# macOS 边界

本文定义 Emacs 与 macOS 之间的边界：系统把什么交给 Emacs，Emacs 把什么交给系统，
这两个方向各由谁负责。所有这类交互都穿过同一层 `lisp/os/macos/system.el`，
其他模块只声明意图，不直接碰系统。

## 范围

- 管：Finder 与 Dock 打开文件、拖放、系统请求退出；打开、在 Finder 中显示、复制
  文件到剪贴板、通知。
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
 文件拖放 ─────────────→ 按落点分派 ─────────→ 模式声明的处理器，否则打开
 URL 拖放 ─────────────→ aaron-open 路由
 系统打开 ←──────────── +macos/open-target
 Finder  ←──────────── +macos-reveal
 剪贴板  ←──────────── +macos/copy-file-to-clipboard
 通知    ←──────────── notify-send
```

**呈现**：系统交来的东西用户必须看得见。边界本身不知道后台的存在，只运行
`+macos-present-hook`；隐藏 Emacs 的模块把“重新显示”挂在这里。

**落点**：同一个拖放手势的含义由落点决定，不由文件决定：

| 落点 | 含义 | 负责方 |
| --- | --- | --- |
| 普通 buffer | 在落点窗口打开，多个文件时停在最后一个 | 边界的默认行为 |
| Eshell、shell、term、Portal 终端 | 在提示符处插入转义后的路径，不执行 | `set-file-drop-handler!` 声明 |
| Dired、Org | 各自原生处理：Dired 进入该目录，Org 询问 | buffer 局部的 `dnd-protocol-alist` |
| 拖入的 URL | 走打开路由 `aaron-open` | 边界 |

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
- **C5 路径属于本机。** 拖入的路径是本机路径。Eshell 或 shell 在其他 target 上时，
  边界拒绝插入并说明原因，不把一个对方机器上不存在的路径放进命令行。
- **C6 URL 走路由。** 拖入的 URL 由 `aaron-open` 按用户配置的路由打开，绝不打开全局的
  `url-handler-mode`。
- **C7 外发动作只在本机。** 外发动作以客户端目录运行，不受当前 buffer 的 target
  影响；需要文件时只接受本机可直接访问的路径，其他 target 上的文件被拒绝。
- **C8 不依赖额外安装。** 外发动作只用系统自带工具。通知走 `osascript`，
  不依赖 `terminal-notifier`。

## 实现位置

| 内容 | 位置 |
| --- | --- |
| 呈现钩子、拖放分派、外发动作 | `lisp/os/macos/system.el` |
| `set-file-drop-handler!` | `lisp/os/macos/autoload/drop.el` |
| 后台时重新显示 | `config/background/config.el` 的 `+background--present-h` |
| 落点声明 | `term/shell/eshell.el`、`term/shell/config.el`、`term/portal/config.el` |
| 用户菜单 | `lisp/os/macos-tools/config.el` |
| 契约测试 | `test/macos-boundary-tests.el`（`make macos-test`） |

新增一种落点时调用 `set-file-drop-handler!`，处理器在目标 buffer 中被调用，参数是
本机绝对路径的列表。

## 已知差距

- **G1 Portal 终端的真实拖放未实测。** Portal 原生层没有注册拖放类型，预期事件落到
  Emacs 视图再分派给 `+portal-send-text`。分派逻辑有测试，从 Finder 的真实拖放还没验证。
- **G2 不能从 Emacs 拖出文件。** `dired-mouse-drag-files` 未开启，需要先确认它与
  Dirvish 的鼠标绑定是否冲突。
- **G3 Finder 复制的文件不能粘贴进 Dired。** `yank-media` 也没有绑定键位。
- **G4 Noema 的拖放语义与本文不同。** 全部为文本的文件会被 CodeMirror 把全文插入笔记，
  编辑区之外没有 document 级拦截，粘贴优先图片。这些属于 Noema，需要在其设计文档中定义。

## 待定设计

- 拖放时的修饰键（例如按住某键时只插入链接或路径）是否要定义语义。

# Neomacs 兼容层

本仓库同时支持 GNU Emacs 31 和 Neomacs（Rust 重写，跟随 GNU Emacs 31.1 的
Lisp 层）。两者的差异全部收敛在 `lisp/init-neomacs.el` 这一个边界里，其它模块
不做构建分支判断。在 GNU Emacs 下整个模块是空操作。

判定谓词是 `my/neomacs-p`（`(fboundp 'neomacs-core-backend)`）。

## 为什么需要这一层

Neomacs 的 Lisp 层与 GNU Emacs 31.1 对齐，但底层运行时不是。对本配置真正有
影响的是下面三条，都会直接破坏手感：

### 1. 物理修饰键

GNU Emacs 通过 `mac-option-modifier` / `mac-command-modifier` 决定物理键的语
义，`lisp/init-macos.el` 把它们设为 `hyper` / `meta`。

Neomacs 没有这两个变量。它的输入桥
（`crates/neomacs-display-runtime/src/render_thread/window_events.rs`）把
winit 的 `alt_key()`（Option）硬编码为 Meta、`super_key()`（Command）硬编码为
Super，而且事件协议只有 shift/ctrl/meta/super 四个位，**没有 Hyper 位**。

后果是 `init-macos.el` 的 54 个 `H-` 绑定、`init-dev.el` 的 `H-<tab>` 折叠键、
`init-mouse.el` 预留给鼠标软件的 `H-C-M-` 层全部打不出来，同时 `M-c` / `M-v`
剪贴板也失效。

兼容层的做法是在 `key-translation-map` 上做**同时重命名**：

| Neomacs 实际送出 | 重命名为 | 物理按键 |
| --- | --- | --- |
| `M-x` | `H-x` | Option |
| `s-x` | `M-x` | Command |
| `C-M-s-a` | `C-H-M-a` | Ctrl+Option+Command |

重命名是一次性的，不会级联（`s-c` 只变成 `M-c`，不会再被翻译成 `H-c`）。
`ESC f` 这类 ESC 前缀序列不受影响，仍然是 `M-f`，与 GNU Emacs 一致。

三个实现细节值得记住：

- 翻译表以 `key-translation-map` 的**父 keymap** 形式挂载，用户自己的条目优先，
  卸载只需要一次 `set-keymap-parent`。
- 表是手工构造的 alist，不用 `define-key`。`define-key` 会把 meta **字符**改写
  成 `ESC` 前缀序列，而 `read-key-sequence` 的翻译步骤按事件原样查表，改写过的
  条目永远命中不了，`M-<char>` 会整体失效。
- 每个条目绑定的是函数而不是常量，只在 `display-graphic-p` 为真时翻译。TTY 上
  没有 Command 键，在那里重命名 Meta 只会把 Meta 弄丢。

代价是每次按键多约 37µs 的查表开销（1638 个条目的线性扫描），相对 16ms 一帧
可以忽略。

### 2. Fringe frame 参数

`lisp/init-ui.el` 调用 `(set-fringe-mode '(nil . nil))`，语义是「用默认宽度」。
GNU Emacs 的 `frame-parameter` 仍然返回解析后的宽度 8；Neomacs 直接返回 nil，
而 `window-fringes` 报告的是真实宽度。

于是 `diff-hl-define-bitmaps` 里的 `(min (frame-parameter nil 'left-fringe) 16)`
变成 `(min nil 16)`，**每次打开一个受版本控制的文件都会掉进 debugger**。

`my/neomacs-repair-fringe-parameters` 把 `window-fringes` 的真实值写回 frame
参数。因为 `init-ui.el` 在本模块之后加载并会再次清空这些参数，修复挂在
`after-make-frame-functions`、`window-setup-hook` 和 `emacs-startup-hook` 上各
跑一次。

### 3. 启动时的第一个 frame

`early-init.el` 有一个「先隐藏第一个 GUI frame，等 Dashboard 就绪再显示」的开关
`my/gui-hide-initial-frame-during-startup`。

GNU Emacs 在 early-init 阶段 `display-graphic-p` 为 nil、`window-system` 为 nil，
所以**这段代码在 GNU 上从来没有生效过**。Neomacs 在 early-init 阶段已经有活的
图形 frame（`window-system` 为 `neo`，`frame-list` 已有两个 frame），这时通过
`initial-frame-alist` 设 `(visibility . nil)` 会把真实窗口取消映射，而
`emacs-startup-hook` 里的显示又和渲染线程竞态——表现就是启动后窗口长时间没有
反应，要按 quit 之类的操作才恢复。

因此 `early-init.el` 增加了「frame 还不存在时才预隐藏」的条件，行为与 GNU 侧
完全一致；`my/neomacs-reveal-frames` 作为兜底，在 `window-setup-hook`、
`emacs-startup-hook` 和一个 1 秒的一次性 timer 上重新显示任何仍不可见的图形
frame。

## Elsa 并发

Neomacs 目前没有 native compilation（`(native-comp-available-p)` 为 nil）。
Elsa 的 `elsa-analyse-file-parallel` 按 `(num-processors)` 无上限地 fork worker，
每个 worker 都是一个从头加载 Elsa 的全新 Emacs。在这台机器上就是 12 个纯解释
执行的 Neomacs 同时启动，打开任何 `.el` 文件都会把机器压满。

`my/elisp-elsa-worker-limit`（`lisp/lang/init-elisp.el`）默认为 nil，含义是
自动：有 native compilation 时完全不干预，没有时在注入给 LSP server 的
`--eval` 里给 `num-processors` 加一条 override，上限
`my/elisp-elsa-worker-fallback-limit`（4）。GNU Emacs 侧的命令行完全不变。

## 可调项

两个都注册在 `config` registry 里，可以用 `M-x config-board` 修改：

| 设置 | 默认 | 含义 |
| --- | --- | --- |
| `my/neomacs-modifier-remap` | nil（自动） | 修饰键重命名表；`none` 关闭；也可给出自定义 alist |
| `my/elisp-elsa-worker-limit` | nil（自动） | Elsa worker 上限；整数强制 |

`M-x +mac-swap-option-and-command` 在 Neomacs 下切换重命名表而不是
`mac-*-modifier`，交换 Option / Command 的角色。

## 状态：2026-09-13 暂停迁移

在 0.0.18 上评估后决定**不迁移**，Neomacs 已从本机删除，GNU Emacs 仍是唯一
主力。本模块保留下来并且在 GNU Emacs 下完全空操作，等上游成熟后可以直接复用。

停下来的直接原因是下面第一条：xwidget 在 Retina 上只渲染一半，而它无法在配置
层修复。

## 仍然存在的上游差异

这些不是配置能修的，将来重新评估时先验证这几条：

- **xwidget 在 HiDPI 上只有一半尺寸（阻塞项）。** 2x Retina 上每个 xwidget —
  内置浏览器、Noema 面板、Lean infoview — 都只占 window 的左上四分之一，右侧和
  下方被裁掉。原因在
  `crates/neomacs-webview/src/platform/macos/view.rs` 的 placement 路径：注释写着
  "Frame geometry is expressed in root-surface device pixels"，于是把
  `content_rect` / `visible_rect` 除以 `device_scale`；但 Emacs 侧的 frame 和
  window 几何是逻辑点（`frame-native-width` 3008 对应 "UI looks like 3008x1692"，
  而同一个 build 的 `display-monitor-attributes-list` 又报物理的 6016x3384），
  于是又被除了一次。

  实测确认无法从 Lisp 修复：给 `xwidget-resize` 和 `xwidget-insert` 加 advice 把
  尺寸预乘 `scale-factor`（请求从 1784x1014 变成 3568x2028）之后，渲染结果完全
  没有变化。placement 矩形来自渲染管线内部的 scene 几何，不是 Lisp 请求的尺寸。
- `xwidget-webkit-set-cookie-storage-file` 不存在。Neomacs 照搬了 GNU 的
  `xwidget.el`，其中 `xwidget-webkit--create-new-session-buffer` 在
  `xwidget-webkit-cookie-file` 非空时无条件调用它，所以每次新建 webkit session 都
  会 void-function 报错。本模块用一个 no-op 顶替（只在缺失时定义），代价是 cookie
  不会写入配置的文件，与 Brave 共享登录态失效。
- 没有 native compilation，纯解释/字节码执行。
- `window-system` 是 `neo`，不是 `ns`；`(featurep 'ns)` 为 nil。`ns-*` frame
  参数（`ns-appearance` 等）被当作未知参数保留，但没有效果。
- 输入桥会丢弃文件拖放事件，应用清单也没有声明文档类型，Finder 双击打开还不
  可靠。
- xwidget 的键盘焦点、JavaScript 回调、多窗口支持仍不完整。
- macOS 平台在上游仍标注为 experimental。

## 验证

```sh
# Neomacs 当前未安装；重新装上后
NEO=/Applications/neomacs.app/Contents/MacOS/neomacs
make EMACS=$NEO health-startup doctor

# GNU 侧不应有任何变化
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --debug-init \
  -l ./early-init.el -l ./init.el \
  --eval '(princ (format "neomacs-p=%S\n" (bound-and-true-p my/neomacs-p)))'
```

在图形实例里检查手感是否还原：

```elisp
;; 应当返回 find-file，而不是 forward-word
(let ((unread-command-events (listify-key-sequence (kbd "M-f"))))
  (key-binding (read-key-sequence-vector nil) t))
```

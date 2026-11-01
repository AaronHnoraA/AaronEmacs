# Neomacs 兼容层

## 范围

GNU Emacs 31 仍是默认与主力；Neomacs 0.0.20 是可选的启动试用。
本轮验收完整配置、启动回调和基本修饰键，不包含 Portal、Noema 页面或其它原生
界面的功能迁移。已有兼容层位于 `lisp/os/neomacs/config.el`；本轮新增修复集中在
`lisp/os/neomacs/trial.el`，只由独立入口 `bin/neomacs-trial` 启用。

## 模型

判定谓词是 `+neomacs-p`（`(fboundp 'neomacs-core-backend)`）。普通启动不加载 `os/neomacs/trial`。只有独立入口设置
`NEOMACS_CONFIG_TRIAL=1` 且运行在 Neomacs 时，才加载试验 patch；GNU 不安装这些
advice、翻译表或启动 hook。

### 修饰键

试验 patch 在有 `neomacs-set-modifier-policy` 的构建上使用 NS 修饰键设置、`mac-*-modifier`
别名与 Hyper，直接沿用 `os/macos` 的物理 Option→Hyper、Command→Meta。
0.0.20 使用这条路径；再安装 Meta→Hyper 翻译会让 Cmd-X 误开 `H-x` 的 Telescope。

较旧构建没有这套原生策略，才在 `key-translation-map` 上同时重命名：

| 输入 | 翻译 | 物理键 |
| --- | --- | --- |
| Meta | Hyper | Option |
| Super | Meta | Command |

翻译表挂在原 map 的父级，保留用户条目；条目手工构造，避免 `define-key` 把 Meta
字符改成 ESC 序列。翻译函数只作用于 GUI；TTY 与 ESC 前缀不变。
`+neomacs-modifier-remap` 的 nil 表示自动，`none` 关闭，也接受显式 alist。
`+mac-swap-option-and-command` 在原生策略下交换正常的 modifier 变量，旧构建才
交换翻译表。

### 字体与 fringe

0.0.20 的 `clear-font-cache` 会删除底层自定义 face 定义，但 Lisp 的 `facep` 仍
返回存在；随后 `set-face-attribute` 报 Invalid face，启动字体回调因此中断。
兼容层将这个破坏性清理替换为空操作，字体属性继续正常更新。它不为缺失 face
伪造定义，也不隐藏字体回调中的其它错误。

Neomacs 对默认 fringe 的 frame 参数返回 nil，`diff-hl` 等需要数值。
`+neomacs-repair-fringe-parameters` 用 `window-fringes` 的实际值补齐，并在
创建 frame、启动和窗口设置完成后重查，避免主题再次清空参数。

### 启动与运行状态

Neomacs 在 early-init 已有 GUI frame；`early-init.el` 只在 frame 尚不存在时
预隐藏，避免取消映射真实窗口。显帧兜底尊重后台模块主动隐藏的 frame，启动 timer
会在执行和退出时取消。

试验 GUI 默认 server 名为 `neomacs-trial`；显式命名的实例不被覆盖，批量进程不改
server 名称。`bin/emacs-app`、`bin/emacsopen`、Raycast 与登录项保持原来的 GNU
路径，不参与试验。`bin/neomacs-trial --app /完整路径/neomacs.app` 只启动指定应用，
不安装、不切换默认应用，也不修改登录项。未指定 `--app` 时先找 `/Applications`，
再找原下载目录；`NEOMACS_APP` 也可指定 bundle。

saveplace 里 PDF 的 `(pdf-page . 15)` 是页码，不是文件名。试验 patch
在原生 saveplace 规范化前暂存该元数据，规范化后恢复；文件路径、普通文本位置与
Dired 条目仍交给 saveplace。这个问题也能在 GNU 复现，本轮按试验范围仅在独立
Neomacs 入口启用修复，不改普通启动的 saveplace 行为。

### Elsa 并发

Neomacs 没有 native compilation。`+elisp-elsa-worker-limit` 的 nil 表示自动：
原生编译构建不干预，其它构建给 Elsa worker 数设置
`+elisp-elsa-worker-fallback-limit`（4）的上限。该设置与修饰键设置均在 Config
registry 中，可由 `M-x config-board` 修改。

## 契约

- **C1** 原生修饰键策略与兼容翻译互斥；Cmd-X 是 `execute-extended-command`，
  Option-X 保持 `H-x` 的 Telescope。测试：
  `neomacs-c1-native-modifiers-avoid-double-translation`。
- **C2** 刷新字体后已有自定义 face 仍可设置属性，启动字体回调不报 Invalid face。
  测试：`neomacs-c2-font-cache-refresh-preserves-custom-faces`。
- **C3** 启动显帧不能撤销后台模块的隐藏意图。测试：
  `neomacs-c3-reveal-respects-background-frames`。
- **C4** 试验只从 `bin/neomacs-trial` 进入；普通 GNU 启动不加载 patch，现有外部
  入口与登录项不变。
- **C5** 从磁盘读取 PDF 页码不把整数传给文件名规范化；文本位置与 Dired 条目保持
  原语义。测试：`neomacs-c5-save-place-loads-numeric-page-metadata`。
- **C6** 完整配置必须能从源码加载，不能靠旧编译缓存掩盖语法错误；启动模块失败和
  配置回调错误都算启动失败。当前声明为 117 个直接加载模块与 3 个延迟模块。

## 实现位置

- `lisp/os/neomacs/config.el`：已有构建探测、旧修饰键兼容、fringe、显帧与 cookie；
  增加显式试验开关。
- `lisp/os/neomacs/trial.el`：本轮独立 patch，持有原生修饰键、字体缓存、显帧与
  PDF 保存位置修复。
- `lisp/lang/elisp/config.el`：Elsa worker 上限。
- `bin/neomacs-trial`：唯一试验入口。
- `test/neomacs-compat-tests.el`：试验 patch 的回归验证。

## 已知差距

- **G1** 当前 Portal 原生模块要求 GNU Cocoa `ns` 后端，Neomacs 的 `neo` 后端被
  `portal-view--unsupported` 拒绝；原生主线程与窗口挂载也未适配。本轮不改 Portal。
  可使用现有 Eshell（`SPC o s`）。
- **G2** Noema、Lean infoview 和其它内嵌网页未完成本轮功能验收。较旧构建在 Retina
  上出现过 xwidget 裁剪、焦点、JavaScript 回调及多窗口差异，启动通过不代表这些
  能力可用。
- **G3** 缺少 `xwidget-webkit-set-cookie-storage-file` 时保留一次提示的 no-op 替身；
  cookie 文件持久化不能因此视为可用。
- **G4** 后台 Dock 策略、抓手、Finder 打开、拖放、输入法和长期运行均未完成 Neomacs
  图形验收；试验入口不代表这些原生交互已迁移。

## 待定设计

暂无。继续使用 GNU Emacs；扩大 Neomacs 的功能验收范围需另行安排。

## 验证

```sh
bin/neomacs-trial --app /完整路径/neomacs.app

# 专项回归，所有新 patch 都由试验入口启用
bin/neomacs-trial --batch -q -l early-init.el -l init.el \
  -l test/neomacs-compat-tests.el -f ert-run-tests-batch-and-exit

# 默认入口仍然连接 GNU Emacs
bin/emacs-app status
make state-check
```

# Daily Usage

这份文档只讲高频操作，不讲配置原理。

## 1. 你每天最常用的入口

- macOS GUI 下：
  `Command = Meta (M-)`，`Option = Hyper (H-)`
- `SPC`
  Evil leader，总入口。
- `SPC SPC`
  `telescope` 统一搜索面板
- `M-x`
  原生 `M-x` + `amx` 历史排序
- `M-x telescope`
  `telescope` 统一搜索面板
- `C-x C-f`
  `find-file` + `vertico-directory`
- `C-x b`
  `consult-buffer`
- `C-s`
  `consult-line`
- `C-c p`
  Projectile 前缀
- `C-c p g`
  `consult-ripgrep`
- `C-x g`
  Magit
- `M-\``
  `vterm-toggle`
- `C-c e`
  切换当前 popup `vterm`
- `C-c E`
  切换到下一个 popup `vterm`，`C-u C-c E` 新建一个
- `C-c M-e`
  切换当前 popup `vterm` 的固定状态
- `C-\``
  `popper-toggle`
- `F1` / `F2` / `F3` / `F4`
  `help-command` / `telescope` / 项目工作台 / 项目 `ripgrep`
- `F5` / `F6` / `F7` / `F8`
  运行 profile / 测试菜单 / 调试 profile / `olivetti-mode`
- `F9` / `F10` / `F12`
  `org-agenda` / popup `vterm` / Claude Code 菜单

### macOS Option `H-`

- `H-,` / `H-x`
  Hyper 管理菜单 / 只显示 `my/` 交互命令的独立 M-x
- `H-f` / `H-F` / `H-b` / `H-B`
  打开文件 / 其他窗口打开文件 / 切 buffer / 其他窗口切 buffer
- `H-r` / `H-s` / `H-g` / `H-t`
  最近文件 / 当前 buffer 搜索 / 项目 ripgrep / `telescope`
- `H-p` / `H-P` / `H-R` / `H-T`
  项目工作台 / workspace 菜单 / run profile 菜单 / test 菜单
- `H-m` / `H-a` / `H-l` / `H-y`
  `magit-status` / `org-agenda` / Claude Code 菜单 / 粘贴剪贴板图片到 Org
- `H-h` / `H-H` / `H-z` / `H-Z`
  help / health 菜单 / zoxide 跳目录 / 当前文件目录
- `H-e` / `H-E` / `H-d` / `H-D`
  code menu / compile menu / diagnostics menu / debug profile 菜单
- `H-i` / `H-u` / `H-j` / `H-n` / `H-N`
  `show-imenu` / language server 菜单 / `dape` / 最近测试 / output 菜单
- `H-\`` / `H-q` / `H-Q` / `H-w`
  `popper-toggle` / 关闭当前 buffer / 退出 Emacs / 关当前 frame
- `H-0` / `H-1` / `H-2` / `H-3`
  关当前窗口 / 单窗口切换 / 上下分屏 / 左右分屏
- `H-o` / `H-O` / `H-k` / `H-K`
  Org 快捷前缀 / 上方开新行 / 向下复制当前行或区域 / 向上复制当前行或区域
- `H-<up>` / `H-<down>` / `H--` / `H-=`
  上移 / 下移当前行或区域 / 收缩选择 / 扩大选择
- `H-;` / `H-'` / `H-[` / `H-]` / `H-/`
  注释切换 / 多光标按行 / 上一个相同项 / 下一个相同项 / 全选相同项
- `H-X` / `H-c` / `H-v`
  剪切 / 复制 / 粘贴

`H-x` 使用独立的 `my/hyper-command-history`，不会混进普通 `M-x`
的 `extended-command-history`；普通 `M-x` 也不会因为 `H-x` 的使用而改变排序。
`H-x` 会隐藏 mode、refresh、quit、board 行动作、manager 内部按钮命令、
setup/ensure 和低层维护命令。普通 `M-x` 仍走默认 `amx` 行为。

`H-,` 内部按键按用途分组：board 入口用 `c/g/L/j/a/h`，菜单入口用
`p/w/r/k/t/o`，代码和运维入口用 `./e/d/D/u/x`，其他工具用
`m/s/n/J/R/P`。

## 2. Leader 键分组

### 文件 `SPC f`

- `SPC f f`
  打开文件
- `SPC f F`
  其他窗口打开文件
- `SPC f r`
  最近文件
- `SPC f o`
  `find-sibling-file`
- `SPC f C`
  复制当前文件
- `SPC f R`
  重命名当前文件
- `SPC f D`
  删除当前文件

### Buffer / Bookmark `SPC b`

- `SPC b b`
  切 buffer
- `SPC b .`
  打开 bookmark 管理菜单
- `C-x r .`
  打开 bookmark 管理菜单
- `C-x r j`
  跳转 bookmark（带 preview）
- `C-x r l`
  切换当前行书签
- `C-x r n` / `C-x r p`
  下一个 / 上一个行书签
- `SPC b c`
  clone indirect buffer
- `SPC b x`
  `scratch-buffer`
- `SPC b z`
  bury buffer
- `SPC b j`
  跳转 bookmark（带 preview）
- `SPC b J`
  在其他窗口跳转 bookmark
- `SPC b m`
  设置 bookmark
- `SPC b r`
  重命名 bookmark
- `SPC b l`
  打开 bookmark 列表；`RET` 跳转，`D` 删除，当前项目条目优先
- `SPC b t`
  切换当前行书签
- `SPC b n` / `SPC b p`
  下一个 / 上一个行书签
- `SPC b L`
  直接设置当前行书签

### 编辑 `SPC e`

- `SPC e d`
  向下复制当前行/区域
- `SPC e D`
  向上复制当前行/区域
- `SPC e o`
  在下方开新行
- `SPC e O`
  在上方开新行
- `SPC e j`
  下移当前行/区域
- `SPC e k`
  上移当前行/区域
- `SPC e 1`
  单窗口 / 恢复窗口布局切换

### Git `SPC g`

- `SPC g .`
  Git Hub；把状态、当前文件 diff / log / blame / stage、merge conflict 收到一个 transient 菜单里
- `SPC g g`
  `magit-status`
- `SPC g w`
  打开 Git 工作台，列表看当前仓库文件状态；`RET` 打开文件，`d` diff，`l` log，`B` blame，`s` / `u` stage / unstage
- `SPC g t`
  打开 `gittree` 可视化；当前窗口显示带颜色的 `git log --graph --decorate --oneline --all`
- `SPC g d`
  当前文件直接对比任意 Git revision 和现在的 buffer，当前窗口打开 unified diff
- `SPC g =`
  当前文件直接对比 `HEAD` 和现在的 buffer
- `SPC g b`
  当前文件对比当前 branch 基线；优先取 upstream merge-base，没有 upstream 时退回仓库 root commit
- `SPC g l`
  当前文件历史
- `SPC g B`
  blame 切换
- `SPC g S` / `SPC g U`
  stage / unstage 当前文件
- `SPC g [` / `SPC g ]`
  上一个 / 下一个 hunk
- `SPC g r`
  回滚当前 hunk
- `SPC g s`
  stage 当前 hunk
- `SPC g h`
  查看当前 hunk
- merge conflict 文件内
  `o` ours，`t` theirs，`b` both，`B` base，`n` / `p` 或 `[c` / `]c` 跳冲突，`e` 进 `ediff`，`q` 打开冲突菜单
- `gittree` buffer 内
  `RET` / `o` 或鼠标点 commit 查看当前 commit，`n` / `p` 上下跳 commit，`y` 复制 hash，`g` 刷新，`q` 退出回原 buffer

macOS GUI 下也可以直接用 `Option(H-)` 拉平这组编辑操作：

- `H-O`
  上方开新行
- `H-k` / `H-K`
  向下 / 向上复制当前行或区域
- `H-<up>` / `H-<down>`
  上移 / 下移当前行或区域
- `H--` / `H-=`
  收缩 / 扩大选择
- `H-;`
  注释或取消注释当前行/区域
- `H-'` / `H-[` / `H-]` / `H-/`
  多光标按行 / 上一个相同项 / 下一个相同项 / 全选相同项

### Help `SPC h`

- `SPC h f`
  `helpful-callable`
- `SPC h c`
  `helpful-command`
- `SPC h v`
  `helpful-variable`
- `SPC h k`
  `helpful-key`
- `SPC h w`
  打开 `*Warnings*` 日志
- `SPC h d`
  `devdocs-lookup`
- `SPC h t`
  `tldr`

### Code `SPC c`

- `SPC c ?`
  diagnostics hub
  统一入口：当前 / 项目 picker、buffer / project panel、error / warning 过滤都在这里
- `SPC c !`
  当前 buffer diagnostics picker
- `SPC c a`
  code actions
- `SPC c .`
  code menu；`b` build，`B` rerun build，自动识别常见的 `make` / `cmake` / `ninja`
- `SPC c f`
  format buffer
- `SPC c r`
  rename
- `SPC c o`
  organize imports
- `SPC c R`
  restart language server
- `SPC c L`
  语言服务器菜单
  可以进 Hub / Doctor / 调参 / log / session / config
- `SPC c i`
  `show-imenu`
  左侧 smart-toggle `treemacs`，并跟随当前文件和光标所在 symbol
- `SPC c I`
  `eldoc-box-help-at-point`
- `SPC c j`
  `dape`
- `SPC c t`
  测试菜单
- `SPC c n`
  当前附近测试
- `SPC c N`
  当前文件测试
- `SPC c p`
  当前项目测试
- `SPC c T`
  重跑上次测试
- `SPC c c`
  `compile`
- `SPC c C`
  `recompile`
- `SPC c D`
  当前 buffer diagnostics panel
- `SPC c P`
  当前项目 diagnostics panel
- `SPC c x`
  `quickrun`

### Open `SPC o`

- `SPC o d`
  `dirvish-dwim`
- `SPC o D`
  `dirvish-fd`
- `SPC o q`
  `clutch-query-console`
- `SPC o e`
  `vterm-toggle`
- `SPC o E`
  切换到下一个 popup `vterm`
- `SPC o F`
  切换当前 popup `vterm` 的固定状态
- `SPC o t`
  `vterm-toggle`
- `SPC o v`
  直接打开新 `vterm`
- `M-x my/project-popup-vterm-app`
  在当前项目根目录的新 popup `vterm` 里运行 `lazygit` / `btop` / `yazi` / `tmux`
- `SPC o V`
  命名 `vterm`
- `SPC o S`
  `my/vterm-ssh`
- `SPC o s`
  `shell-toggle`
- `SPC o w`
  `my/open-eww-url`
- `SPC o x`
  `my/open-xwidget-url`
- `SPC o a`
  `my/appine-open-url`
- `SPC o W`
  统一搜索入口，可选搜索引擎和浏览后端
- `SPC o B`
  在 `eww` / `xwidget` / `appine` / macOS `open` 之间切换当前页面

### Browser `C-c w`

- `C-c w w`
  统一 `browse-url` 入口，默认弹选择菜单，默认项是 `xwidget`
- `C-c w e` / `C-c w x` / `C-c w a`
  直接用 `eww` / `xwidget-webkit` / `appine` 打开 URL；`eww` 和 `xwidget-webkit`
  会在独立浏览 buffer 里打开，连续多开也不会顶掉已显示的浏览 buffer
- `C-c w E` / `C-c w X` / `C-c w A` / `C-c w O`
  把当前页面快速切到 `eww` / `xwidget-webkit` / `appine` / macOS `open`
- `C-c w s`
  交互选择目标后端；可选 `xwidget` / `appine` / `eww` / `system`
- `C-c w f` / `C-c w g`
  用 `appine` 打开文件 / 打开光标下 URL
- `C-c w h` / `C-c w l` / `C-c w r`
  `appine` 后退 / 前进 / 刷新
- `C-c w [` / `C-c w ]` / `C-c w 0`
  `appine` 上一标签 / 下一标签 / 关闭当前标签
- `C-c w d`
  关闭当前浏览后端；在 `eww` / `xwidget-webkit` buffer 中也可以按 `M-w`，
  会同时 kill browser buffer 并删除对应窗口
- `C-c w ?` / `C-c w k`
  打开 Appine board / 清理全部 Appine view

当前策略是手动分流：默认打开方式统一在 `lisp/init-open.el` 的
`my/open-routes` 里维护，route DSL helper 来自 vendored `general.el` 的
`general-route-*`。`browse-url` 会先让你选后端，默认项是 `xwidget`；
`appine` 保留为原生嵌入/文件查看入口，`eww` 适合阅读，`system` 用系统
应用处理文件或链接。

### Appine `SPC a p`

- `SPC a p a`
  打开 URL 到 `appine`
- `SPC a p f`
  用 `appine` 打开文件
- `SPC a p p`
  用 `appine` 打开光标下 URL
- `SPC a p h` / `SPC a p l` / `SPC a p r`
  后退 / 前进 / 刷新
- `SPC a p [` / `SPC a p ]` / `SPC a p c`
  上一标签 / 下一标签 / 关闭当前标签
- `SPC a p k`
  `my/appine-kill-all`
- `SPC a p R`
  `my/appine-restart`
- `SPC a p s`
  切换当前页面到 `eww` / `xwidget` / `appine` / macOS `open`
- `SPC a p S`
  统一搜索入口

关闭 Appine 的最后一个标签时会自动清掉 `*Appine Window*` host buffer。
Appine board 里的文件、目录、URL 和 tab registry 都带 `[open]` / `mac open`
入口，用 macOS `open` 交给系统应用处理。

### Tab `SPC t`

- `SPC t n`
  新 tab
- `SPC t t`
  切 tab
- `SPC t r`
  重命名 tab
- `SPC t [`
  上一个 centaur tab
- `SPC t ]`
  下一个 centaur tab

### Project `SPC p`

- `SPC p .`
  打开项目工作台
- `SPC p p`
  切项目
- `SPC p o`
  打开项目工作台式入口
- `SPC p f`
  当前项目找文件
- `SPC p s`
  当前项目全文搜索
- `SPC p d`
  打开项目根目录
- `SPC p m`
  打开当前项目 Magit
- `SPC p v`
  打开当前项目 vterm
- `SPC p a`
  手动添加项目
- `SPC p D`
  批量扫描目录下的项目
- `SPC p x`
  彻底移除一个项目及其相关状态（包含 Projectile、`project.el`、Treemacs、perspective、项目 buffer/vterm）
- `SPC p l`
  查看当前项目 project-local overrides（来自 `my/project-local-overrides` 全局配置）
- `SPC p L`
  快捷打开 `.dir-locals.el`（同 `SPC p e e`）

### Dir-locals / 项目环境 `SPC p e`

- `SPC p e e`
  编辑当前项目 `.dir-locals.el`
- `SPC p e c`
  从模板创建 `.dir-locals.el`
- `SPC p e m`
  将模板合并进现有 `.dir-locals.el`
- `SPC p e r`
  重载 dir-locals 并刷新 direnv 环境（PATH 等）
- `SPC p e s`
  将所有非 `eval` 变量静默（加入 `safe-local-variable-values`）
- `SPC p e d`
  查看哪些 dir-locals 条目对当前 buffer 生效

可用模板：`python-venv`、`python-uv`、`python-conda`、`cc-cmake`、`cc-meson`、`nix-flake`、`nix-gcc`、`nix-clang`、`nix-shell`、`sagemath`、`node`、`lsp-workspace`、`emacs-lisp`、`indent-2`、`indent-4`、`direnv`。详见 [settings-cookbook.md § 16](settings-cookbook.md)。

## 3. 搜索与跳转

- `SPC SPC`
  打开 `telescope`
- `SPC SPC f`
  当前项目找文件
- `SPC SPC b`
  统一切换 buffer
- `SPC SPC g`
  当前项目 ripgrep
- `SPC SPC I`
  当前 workspace / 项目 symbols；输入时实时刷新候选并 preview
- `SPC SPC i`
  当前 buffer symbols；输入时实时 preview 到候选 symbol
- `SPC SPC m`
  bookmark picker；没有书签时打开 bookmark 列表
- `C-s`
  当前 buffer 搜索
- `C-x C-r`
  最近文件
- `SPC s p`
  `consult-ripgrep`
- `SPC s s`
  `consult-line`
- `SPC s i`
  `imenu`
- `C-c p .`
  非 Evil 下打开项目工作台
- `C-;`
  `avy-goto-char`
- `C-:`
  `avy-goto-char-2`
- `C-'`
  `avy-goto-word-1`

## 4. 结构导航

- `SPC n a`
  跳到当前函数开头
- `SPC n e`
  跳到当前函数结尾
- `SPC n [`
  上一个函数
- `SPC n ]`
  下一个函数
- `SPC n u`
  跳到外层结构
- `[f`
  上一个函数
- `]f`
  下一个函数

## 5. 折叠与结构选择

### 折叠

- `za`
  切换当前折叠
- `zo`
  打开当前折叠
- `zc`
  关闭当前折叠
- `zR`
  展开当前 buffer 的所有折叠
- `zM`
  折叠当前 buffer 的所有折叠
- `SPC z a`
  同 `za`
- `SPC z o`
  同 `zo`
- `SPC z c`
  同 `zc`
- `SPC z R`
  同 `zR`
- `SPC z M`
  同 `zM`

### 结构选择 `SPC v`

- `SPC v v`
  逐级扩选
- `SPC v V`
  缩回上一步
- `SPC v f`
  选整个函数 / method / class
- `SPC v F`
  选函数 / class 的 body
- `SPC v s`
  选当前语句
- `SPC v e`
  选当前表达式
- `SPC v b`
  选当前代码块
- `SPC v B`
  选当前代码块内部
- `SPC v p`
  选下一层外层结构

## 6. 多光标和 snippet

### 多光标

- `C-S-c C-S-c`
  对选中多行建立多光标
- `C->`
  选中下一个相同项
- `C-<`
  选中上一个相同项
- `C-c C-<`
  全选相同项
- Evil visual 下：
  - `g n`
  - `g p`
  - `g a`

### Snippet

- `C-c y y`
  展开 snippet
- `C-c y i`
  插入 snippet
- `C-c y n`
  新建 snippet
- `C-c y v`
  打开 snippet 文件

## 5. Dired / Dirvish

- `C-c o d`
  打开 Dirvish
- `C-c o f`
  `dirvish-fd`
- 在 Dired 里：
  - `H`
    显示/隐藏 dotfiles
  - `C-c C-e`
    进入 `wdired`

## 6. 窗口和弹出层

- `M-o`
  `ace-window`
- `M-\``
  `vterm-toggle`
- `H-\`` / `C-\``
  `popper-toggle`
- `C-M-\``
  改变 popup 类型

## 7. 有冲突时优先记住什么

- `M-w` 现在是正常复制，不再关窗口
- 关 frame 用 `H-w`
- 普通 warning 现在只写入 `*Warnings*`，不再自动弹窗抢操作；需要时用 `SPC h w`
- `C-c y` 现在是 snippet 前缀，不再直接展开
- `C-c n` 是 org-roam 前缀，不再给 centaur-tabs

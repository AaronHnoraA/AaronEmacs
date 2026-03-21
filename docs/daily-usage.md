# Daily Usage

这份文档只讲高频操作，不讲配置原理。

## 1. 你每天最常用的入口

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
- `SPC c x`
  `quickrun`

### Open `SPC o`

- `SPC o d`
  `dirvish-dwim`
- `SPC o D`
  `dirvish-fd`
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
  在当前项目根目录的新 popup `vterm` 里运行 `lazygit` / `btop` / `codex`
- `SPC o V`
  命名 `vterm`
- `SPC o S`
  `my/vterm-ssh`
- `SPC o s`
  `shell-toggle`
- `SPC o w`
  `eww-browse-url`
- `SPC o x`
  `xwidget-webkit-browse-url`

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
  当前 workspace / 项目 symbols；输入关键词后走 `xref-find-apropos` + `consult-xref`，并带 preview
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
- `C-\``
  `popper-toggle`
- `M-\``
  `popper-cycle`
- `C-M-\``
  改变 popper 类型

## 7. 有冲突时优先记住什么

- `M-w` 现在是正常复制，不再关窗口
- 关 frame 用 `H-w`
- 普通 warning 现在只写入 `*Warnings*`，不再自动弹窗抢操作；需要时用 `SPC h w`
- `C-c y` 现在是 snippet 前缀，不再直接展开
- `C-c n` 是 org-roam 前缀，不再给 centaur-tabs

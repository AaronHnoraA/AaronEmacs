# Project Guide

这套配置现在把项目管理做成了一个单独工作流，不再只是“有 Projectile，也有 Treemacs，但你自己拼”。
Treemacs 不再直接挂在项目工作台里，而是交给 `show-imenu` 和手动 `C-c t` 使用。

## 1. 现在的项目工作流是什么

- `Projectile`
  管理已知项目、项目文件、项目搜索、项目缓存
- `Perspective`
  按项目切工作区
- `Transient`
  提供项目工作台菜单
- `show-imenu`
  左侧 smart-toggle `Treemacs`，同时承担文件树和符号导航

这几层现在已经接起来了。

## 2. 最常用入口

- `SPC p .`
  打开项目工作台
- `C-c p .`
  非 Evil 场景下打开项目工作台
- `SPC p p`
  切项目
- `SPC p o`
  打开项目工作台式入口
  这会切 perspective，并执行 Projectile 的项目切换动作
- `SPC p s`
  当前项目 `ripgrep`
  如果加前缀参数 `C-u`，会先让你选项目
- `SPC p f`
  当前项目找文件
- `SPC p d`
  打开项目根目录
- `SPC p m`
  打开当前项目 Magit
- `SPC p v`
  打开当前项目专属 vterm
- `SPC c i`
  打开 `show-imenu`
  左侧 Treemacs smart toggle，并跟随当前文件和光标所在符号

## 3. 项目工作台菜单

`SPC p .` / `C-c p .` 会打开 `my/project-dispatch`，分成四组：

- Switch
  切项目、开项目工作台、找文件、最近文件、项目 buffer
- Search
  项目内 ripgrep、跨已知项目找文件、项目 Magit
- Open / Shell
  项目根目录、项目 vterm
- Manage
  手动加项目、批量发现项目、彻底移除项目、杀当前项目 buffer

## 4. 如何加项目

### 手动加一个

- `SPC p a`
- 或 `M-x my/project-add-known-project`

这适合零散仓库。

如果某个项目之前被 `SPC p x` 移除过，再次用 `SPC p a` 添加，也会把它重新恢复到项目工作流里。

### 批量扫描一个目录

- `SPC p D`
- 或 `M-x my/project-discover-projects-in-directory`

你给一个目录，再给一个扫描深度，例如：

- `~/code` 深度 `2`
- `~/work` 深度 `3`

适合把一整片代码目录扫进 Projectile。

### 彻底移除一个项目

- `SPC p x`
- 或 `M-x my/project-remove-known-project`

这不再只是把项目从 `Projectile known-projects` 里删掉，而是会一起清：

- Projectile 已知项目和缓存
- Emacs 内置 `project.el` 的已知项目记录
- Treemacs workspace 里的项目项
- 对应 perspective
- 项目相关 buffer / 专属 vterm

`SPC p x` 会把项目加入忽略列表，所以它不会再被当前项目工作流默认选中；重新启用它的方式就是 `SPC p a`。

## 5. 如何配置常用项目搜索根

如果你有固定项目根目录，去改：

- [lisp/init-project.el](../lisp/init-project.el)

变量是：

- `my/project-search-paths`

示例：

```elisp
(setq my/project-search-paths
      '(("~/code" . 2)
        ("~/work" . 3)
        "~/dotfiles"))
```

含义：

- `("~/code" . 2)`
  在 `~/code` 下向下找 2 层项目
- `"~/dotfiles"`
  直接把这个目录本身当搜索根

改完重启 Emacs，或者手动执行 `M-x projectile-discover-projects-in-search-path`。

## 6. Treemacs / Show-imenu 现在怎么用

日常更推荐用 `show-imenu`：

- `M-x show-imenu`
- 或 `SPC c i`

行为是：

- 左侧打开或关闭 `Treemacs`
- 如果当前 buffer 在项目内，就用项目根
- 如果当前 buffer 不在项目内，就用当前目录
- 保持焦点在当前编辑窗口
- 自动跟随当前文件和光标所在 symbol/tag

手动 Treemacs 仍然保留：

- `C-c t`
  打开左侧 Treemacs
- `M-0`
  焦点跳到 Treemacs

它依然保留 filewatch、follow mode、project-follow mode，并且从树里打开文件默认进入最近使用的编辑窗口。

## 7. Perspective 现在怎么配合

`SPC p p` / `SPC p o` 切项目时，会先切到以项目名命名的 perspective。

这意味着：

- 不同项目的 buffer 更容易隔离
- 项目切换更像 IDE workspace
- Treemacs 在支持时也会跟着 perspective scope 走

## 8. 推荐用法

如果你是单仓库专注开发：

1. `SPC p o`
2. 选项目
3. `SPC c i`
4. 直接进文件、搜索、开终端、开 Magit

如果你经常在多个仓库之间跳：

1. 先用 `SPC p D` 把常用目录扫进已知项目
2. 用 `SPC p p` 快速切项目
3. 用 `SPC p .` 调项目工作台做具体动作
4. 需要看符号和目录时用 `SPC c i`

## 9. 如果项目切换不对

优先检查：

1. 该目录是不是真的项目根
2. Projectile 是否已经认识这个项目
3. `my/project-search-paths` 是否配置正确
4. 当前 buffer 是否真的处在你想要的项目里

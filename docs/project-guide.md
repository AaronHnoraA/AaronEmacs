# Project Guide

这套配置现在把项目管理做成了一个单独工作流，不再只是“有 Projectile，也有 Treemacs，但你自己拼”。
Treemacs 不再直接挂在项目工作台里，而是交给 `show-imenu` 和手动 `C-c t` 使用。

## 1. 现在的项目工作流是什么

- `Projectile`
  管理本工作台的已知项目、项目文件、项目搜索、项目缓存
- Emacs `project.el`
  保留自己的原生项目识别与记忆；项目工作台显式添加项目时也会请求登记，遵循其排除规则
- `Perspective`
  按项目切工作区
- `Transient`
  提供项目工作台菜单
- `show-imenu`
  左侧 smart-toggle `Treemacs`，同时承担文件树和符号导航

这几层现在已经接起来了。

原生 `project.el` 仍可独立记住项目，不必先经 Projectile 添加。配置使用
Emacs 31 的 `project-list-exclude` 排除明确隐藏的根；只有显式添加到本工作台的项目
才自动进入 Projectile 列表。需要反向导入时可设置
`my/project-import-project-el-entries`。

### 项目身份与 target

项目/workspace 一旦进入开发框架，就以 `/fs:TARGET:/root` 作为稳定身份；本机项目的
target 是 `local`，远程项目只是另一个 target。项目搜索、终端、LSP、环境、watch、
SCM 与后续 debug/task 集成都应从这个同一 identity 派生，不能各自用
`file-remote-p` 再判断一次。本地普通 buffer 在框架外仍保留原生文件名，因此不会
牺牲 Emacs package 兼容性。

**已知项目列表里的远程项目**：`my/project-normalize-root` 让每个根只有一种写法——
本机能直接访问的是原生路径，其余一律是 `/fs:TARGET:/root/`；`/ssh:` 或 `/fs:local:`
写法在入口处就归一，不会产生重复条目。`project-add-to-workbench`（输入
`/fs:aaron-pc:/…` 或 `/ssh:…` 均可）和 `my/jupyter-ssh-open-project` 先打开该 root
的 Remote workspace 再登记。列出项目（切换、dashboard）时远程根不探测、不连线：
主机暂时不可达也照常列出，打开时才连接并报告目录缺失。
远端登记失败时，只关闭本次登记新开的 workspace；已由其他功能打开的 workspace
继续保留。

最近文件同理：`recentf` 记录逻辑名（远程是 `/fs:`，本机是原生路径），只丢弃映射不到
target 的物理 TRAMP 写法；从不探测已记录的远程文件，dashboard 的 Recent Files
因此也包含远程文件。
项目菜单的“recent file”直接从原生 `recentf-list` 筛当前项目，保留 Projectile 补全；
列出候选时不对远端项目根调用 `file-truename`。同名相邻目录不会混入列表。

**项目根探测的代价**：Projectile 的 marker 列表有一百多项，其中八项是通配符
（`?*.sln`、`?*.xcodeproj` 等）。上游用一次 `directory-files` 回答所有普通
marker，但每个通配符仍要自己展开一次，也就是每层目录多出八次列目录。往上走四层
就是 32 次额外往返，在 target 上这是打开一个文件最大的单项开销。
`my/project--directory-marker` 用同一份列表回答通配符 marker，带路径分隔符的
marker 仍然直接探测。这个改写对本地同样有效，因此不按 target 分支。

同类问题还有 Citre：`citre-auto-enable-citre-mode` 会在每个 `prog-mode` buffer
里找 ctags/readtags/global，target 上没装就是每次打开文件约 30 次往返，而答案在
连接存续期间不会变。`my/citre-executable-find-a` 只缓存 target 侧的查找；装好工具
后用 `M-x my/citre-forget-executables` 清掉。

## 2. 最常用入口

- `SPC p .`
  打开项目工作台
- `C-c p .`
  非 Evil 场景下打开项目工作台
- `SPC p p`
  切项目
- `SPC p o`
  与 `SPC p p` 相同的旧快捷键；`M-x project-workbench` 也是同一条进入路径
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
  打开当前项目专属 portal
- `SPC c i`
  打开 `show-imenu`
  左侧 Treemacs smart toggle，并跟随当前文件和光标所在符号
  （Noema Markdown 页面也一样：大纲跟随页面的笔记，点击标题由
  `my/treemacs-tag-visit-functions` 让页面跳过去；Treemacs 读取文件建索引时绑定
  `my/programmatic-file-visit`，不会触发 Markdown→Noema 跳转）

## 3. 项目工作台菜单

`SPC p .` / `C-c p .` 会打开 `project-menu`，分成四组：

- Switch
  切项目、找文件、最近文件、项目 buffer
- Search
  项目内 ripgrep、跨已知项目找文件、项目 Magit
- Open / Shell
  项目根目录、项目 portal
- Manage
  退出当前项目、手动加项目、批量发现项目、彻底移除项目、关闭当前项目的文件 buffer

### 项目进入与退出

项目切换、打开工作台、找文件、最近文件、切换项目 buffer、打开根目录、Magit
或项目终端，在操作成功后发送统一的项目激活事件。身份使用 `/fs:TARGET:/root/`；
相同项目不会重复激活。后台查询、搜索的临时上下文和已知项目列表不会触发激活。

Perspective 记住明确进入过的项目，用户切换回来时恢复该项目；切到没有项目关联的
Perspective 时退出当前项目。包内部临时切换 Perspective 不触发 Agenda 扫描。
关闭对应 Remote workspace 也释放当前项目的消费者。

`M-x my/project-leave`，或项目菜单的 `l`，退出当前项目的后台消费者并解除当前
Perspective 的项目关联，保留文件缓冲区。Noema Agenda 随之释放项目 scope 的
监听和缓存，知识库继续常驻；再次明确进入项目即可恢复。

### 运行、任务与构建

`F5`、`H-R`、`SPC r r` 和 `M-x project-run-menu` 打开运行 profile 菜单；
`M-x project-run` 直接选择 profile，`M-x project-rerun` 重跑本 frame 最近一次
成功启动的运行命令。`SPC r k` / `M-x task-menu` 提供任务与构建，直接入口是
`task-run`、`task-build` 及各自的 `*-rerun`。

两个菜单共用 `package.json` 脚本与 Makefile、justfile 目标识别；只有脚本或目标
确实存在时才列出。运行 profile 取 `dev`、`run`、`serve`、`start`、`preview`
等常用目标，并可用 `my/project-run-profile-alist` 配置。相同名称按项目本地
`:run` 覆盖、手工 profile、Node、just、Make、语言默认命令依次选择；任务菜单
在混合仓库里给重名项加 `cargo:`、`go:`、`node:` 等前缀，让每个命令都能选到。
两边都从打开菜单的源 buffer 取项目上下文，使用项目环境与按 frame 管理的
Compilation 输出。已有 owning Remote workspace 时，运行、构建、测试和 Bazel
任务在它的 target 上执行；从 `/fs:` 项目启动时会按需打开 workspace。关闭该
workspace 或输出 buffer 会取消仍在运行的任务，输出窗口的 `K` 可取消、`R` 可按
原目录和环境重跑。启动失败只回收这次新开的 workspace，不覆盖上一次可重跑命令。
普通本地目录没有 owning workspace 时沿用 Emacs 原生 `compile`。
`SPC r O` / `M-x output-switch` 只在当前 frame 打开最近的输出；关闭最新输出后会
找回这个 frame 仍在的上一份输出。`SPC r o` / `M-x output-menu` 汇总重跑、取消
与错误跳转。关闭 frame 不会替 workspace 取消运行中的任务，输出 buffer 会释放对已
删除 frame 的引用；显式关闭 workspace 或输出 buffer 才会取消其任务。
临时输入一条任意编译命令仍用 Emacs 原生 `C-x p c` / `M-x project-compile`；
运行与任务菜单只负责已命名、可发现的命令。

## 4. 如何加项目

### 手动加一个

- `SPC p a`
- 或 `M-x project-add-to-workbench`

这适合零散仓库。
如果 Projectile 的其他忽略规则排除了该目录，登记会说明原因并保留原来的忽略状态。
项目忽略列表损坏时需先修复并重新加载，再登记项目。

如果某个项目之前从项目菜单（`SPC p .` 后按 `x`）移除过，再次用 `SPC p a`
添加，也会把它重新恢复到项目工作流里。

### 批量扫描一个目录

- `SPC p D`
- 或 `M-x project-discover-in-directory`

你给一个目录，再给一个扫描深度，例如：

- `~/code` 深度 `2`
- `~/work` 深度 `3`

适合把一整片代码目录扫进 Projectile。
批量发现会逐个登记新项目；某项失败时撤掉它由扫描新加入的 Projectile 记录，
继续处理其他项目，并在结束时列出失败项及未能完成的清理。
扫描或登记时按 `C-g`，会撤掉这次尚未完成登记的新条目；此前已登记成功的项目保留。

### 彻底移除一个项目

- `SPC p .` 后按 `x`
- 或 `M-x project-remove-from-workbench`

这不再只是把项目从 `Projectile known-projects` 里删掉，而是会一起清：

- Projectile 已知项目和缓存
- Emacs 内置 `project.el` 的已知项目记录
- Treemacs workspace 里的项目项
- 对应 perspective
- 项目相关 buffer / 专属 portal

项目菜单里的 `x` 会把项目加入忽略列表，所以它不会再被当前项目工作流默认选中；
重新启用它的方式就是 `SPC p a`。`SPC p x` 当前用于项目 scratch buffer。

未保存内容若否决 `kill-buffer`，该 buffer 会保留，完成消息会给出关闭与保留数量。
若 Projectile、原生 `project.el`、Treemacs 等某一步清理失败，其余步骤仍会执行，
最后报告未完成部分；再次运行移除命令可从已忽略项目中选同一根重试。

如果忽略列表文件 `var/project/ignored-roots.el` 损坏，启动会在健康面板显示错误，
并暂停自动清理已知项目，防止丢失隐藏规则。修复该文件后运行
`M-x project-reload-ignored-roots`（项目菜单按 `R` 也可进入），重新加载并恢复清理。
修复前不会覆盖损坏文件。

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
- 文件下的 Outline 不继承深层目录缩进，而从固定浅层开始
- `OUTLINE · 文件名` 行明确当前符号树归属；类、结构体、接口、方法、字段和变量
  使用不同的语义图标，并按类型后源码位置稳定排序

点击顶部 tab line 左侧的 LSP/breadcrumb 文字会执行 ensure-open 版本：它不会关闭
已经显示的 Treemacs，而是把 Treemacs 光标落到当前 Imenu node、切换焦点并闪烁该行。
breadcrumb 不继承 buffer tab 的切换、关闭、菜单或滚轮行为。

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

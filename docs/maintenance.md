# Maintenance

这份文档讲长期维护，不讲日常使用。

## 1. 依赖管理

### 更新锁文件

执行：

```sh
make install
```

或者：

```sh
emacs --debug-init -q -l ./bootstrap.el
```

### 什么时候该更新锁文件

- 你新增了 package
- 你删除了 package
- 你把 VC 包 recipe 改了
- 你在另一台机器上完成了一次安装整理，想同步回来

## 2. Elisp 编译

文件：

- [lisp/init-compile.el](../lisp/init-compile.el)

常用命令：

- `M-x my/byte-recompile-lisp-dir`
- `M-x my/native-compile-lisp-dir`
- `M-x my/native-comp-open-log`

leader 入口：

- `SPC c e`
  byte-compile
- `SPC c E`
  native-compile

## 3. 状态目录

集中在 [var/](../var/)：

- backup
- auto-save
- lockfiles
- tramp
- company
- copilot
- projectile
- transient
- dirvish
- treemacs
- org

这意味着：

- 项目目录更干净
- 状态问题可以定向删除
- 出故障时比“散落在仓库里”更容易排查

## 4. 常见清理

### 清 transient 历史

删：

- [var/transient](../var/transient)

### 清 projectile 历史

删：

- [var/projectile](../var/projectile)

### 清 dirvish / treemacs 状态

删：

- [var/dirvish](../var/dirvish)
- [var/treemacs](../var/treemacs)

### 清 backup / auto-save

删：

- [var/backup](../var/backup)
- [var/auto-save](../var/auto-save)

### 清 TRAMP 状态

删：

- [var/tramp](../var/tramp)

## 5. 升级 package 的基本原则

- 普通包：优先 `use-package :ensure t`
- VC 包：优先 `my/package-ensure-vc`
- 不要到处手写零散的 `package-vc-install`
- 包职责尽量放回对应模块，不要堆到 `init.el`

## 6. 当前需要记住的结构边界

- [lisp/init-base.el](../lisp/init-base.el)
  基础行为、字体、系统交互、状态目录
- [lisp/init-ui.el](../lisp/init-ui.el)
  主题、dashboard、help、window behavior
- [lisp/init-minibuffer.el](../lisp/init-minibuffer.el)
  vertico/orderless/consult/embark
- [lisp/init-search.el](../lisp/init-search.el)
  ivy/counsel/swiper
- [lisp/init-lsp.el](../lisp/init-lsp.el)
  company/eglot/flymake/dape
- [lisp/init-org.el](../lisp/init-org.el)
  Org 全家桶
- [lisp/init-shell.el](../lisp/init-shell.el)
  shell/eshell/vterm/ssh

## 7. 常见故障排查

### Emacs 能启动，但某功能没反应

先看：

1. 这个命令是不是懒加载的
2. 依赖包是否已安装
3. 外部工具是否在 PATH

### warning 老是冒出来打断操作

现在普通 `warning` 默认只记录到 `*Warnings*`，不会自动弹窗。

需要手动查看时：

1. `SPC h w`
2. 或者 `M-x my/show-warnings-buffer`

### LSP 挂了

看：

1. 语言服务器是否存在
2. `M-x eglot` 是否能手动起
3. 当前 major mode 是否真的被纳入 `eglot`

### Org LaTeX 预览挂了

看：

1. `xelatex`
2. `dvisvgm`
3. [tools/org-xdvisvgm-hires](../tools/org-xdvisvgm-hires)

### `my/vterm-ssh` 没读到主机

看：

1. `~/.ssh/config` 是否可读
2. Host 行是不是通配符
3. 是否真的写成了独立 Host 条目

### Dired 排版奇怪

看：

1. `gls` 是否存在
2. macOS 上 coreutils 是否装好

## 8. 推荐维护节奏

每隔一段时间做一次：

1. 更新依赖
2. 跑一次 batch 启动检查
3. 清理不用的键位和旧包
4. 同步更新 [docs/](.)

如果你继续大改配置，记得同时更新 [docs/README.md](README.md) 里的索引。

# Xenodium 配置取舍与工作台

这次参考的是 `xenodium/dotsies` 的 Emacs 配置（分析基线：
`440ec4cd5f4faf5ed5e71b9973c550fb4627d292`）。采用的是其中成熟、边界清楚的
交互思路；没有直接合并它的模块体系，也没有引入 Ivy、Company、Eglot 等与当前
配置重复或冲突的基础设施。

## 已采用的功能

### 编辑工作台：`SPC e .`

- 标识符命名风格循环转换。
- 两阶段交换任意两个不重叠区域；第一次选择会保留高亮，`T` 可取消并释放 marker。
- region 优先的 narrow/widen；Org 源码块、引用块、Org subtree、LaTeX environment 和普通 defun
  会按当前上下文处理。
- 对 region 或整 buffer 计算 hash 并复制结果。
- 比较 kill ring 中最近两项、两个可见 buffer 或当前文件的 VC 版本；
  两窗口比较读取当前 buffer 内容，临时 yank buffer 由各自 Ediff 会话结束时清理。
  Ediff 关闭后按 frame 恢复原窗口布局；同一 frame 的多场比较结束后只恢复一次。
- 以 Markdown、GitHub、Org、HTML、Slack、reStructuredText 或 AsciiDoc 格式复制
  region；没有 region 时处理整个 buffer。
- Emacs Lisp buffer 中启用 Eros 求值反馈，并提供 Relint 检查。
- Elisp buffer 的 Flymake 字节编译检查用当前会话的 `load-path`，本配置的模块文件
  能查到 `require` 之后的真实警告（以前停在第一个 `require` 报“Cannot open load file”）。
- 光标在变量上时，eldoc 在文档旁显示它的当前值（Emacs 自带的
  `elisp-eldoc-var-docstring-with-value`）。
- `M-x elisp-search-load-path` 用 ripgrep 搜索 `load-path` 上的所有目录（本配置的模块
  和已安装的包），以光标处的符号为初始输入。

`SPC n c` 跳到最近一次可见文本编辑的位置，`SPC n n` 或 `M-x narrow-or-widen` 缩小/展开当前范围。Org 源码块和引用块在原 buffer 内缩小到块，其他位置缩小到 subtree。
最近修改位置只保存在内存中；mode 关闭或 Emacs 退出时会释放 marker。

### 文件与 Dired 工作台：`SPC f .`

- 复制 region 覆盖的 Dired 行、marked files、光标下文件或当前 buffer 文件路径。
- 一次复制多个路径时用换行连接，也可只复制去重后的父目录。
- 复制相对当前项目根的路径（`file-copy-project-paths`，菜单 `r`）；项目根是
  `/fs:` 名字、buffer 是本机路径时也按同一写法比较。
- 在本地或远程 target 上执行 `du -sk`，显示 Dired 选择项的总大小。
- 从选择项建立一个独立的临时 Dired 列表，原目录视图保持完整；在临时列表按原生
  `q` 返回原目录并释放临时 buffer。
- `M-x file-copy-paths`、`file-copy-parent-paths`、`dired-selected-size` 和
  `dired-selection` 可直接搜索到这些动作。图片裁剪统一由编辑工作台的原生
  `image-crop` 提供，文件菜单不再重复列出。
- 新文件缺少父目录时默认询问是否创建；可把
  `+files-create-parent-directories` 改成 `always` 或 `never`。
- 以 `fundamental-mode` 打开的文件（没有扩展名、内容也认不出）保存后会重新判断
  major mode，例如刚写上 `#!/bin/sh` 的脚本保存后进入 shell 模式；只对当前窗口
  显示的 buffer 生效，后台保存的 buffer 不会被换模式。
- Proced buffer 自动刷新；`SPC o p` 打开 Proced。

路径和外部进程都沿用现有 Remote contract。远程文件会转换为 target-native 参数，
`du`、`gh`、Bazel 和 Buildifier 也在当前 logical target 上解析和执行。

### GitHub：`SPC g .`

Git 工作台新增 GitHub 一栏：

- `i`：从当前仓库选择并打开 issue 或 PR。
- `I`：插入 `#编号`。
- `a`：先按作者筛选，再打开 topic。
- `@`：通过 GitHub GraphQL 选择 mentionable user 并插入 `@login`。
- `P`：预览 PR patch。应用前要求工作树干净，先运行 `git apply --check`，再明确确认
  是否使用 `--3way` 应用。
- `R`：刷新 issue/PR 缓存。

topic 缓存放在 `var/github/`，默认有效 300 秒。功能依赖 `gh`，首次使用前需要在相应
target 上完成 `gh auth login`。

### Bazel：`SPC r z`

- `M-x bazel-target-menu` 打开面向当前 logical target 的菜单；直接命令为
  `bazel-target-build`、`bazel-target-test`、`bazel-target-run`、
  `bazel-target-insert`、`bazel-target-jump`、`bazel-target-refresh`，输出目录用
  `bazel-output-bin` / `bazel-output-base`。Bazel 包原生的 `bazel-build`、
  `bazel-test`、`bazel-run` 及 mode 键位仍可直接使用。
- 自动识别 `MODULE.bazel`、`WORKSPACE.bazel` 或 `WORKSPACE` 根目录。
- 通过 `bazelisk`（回退到 `bazel`）异步执行 `query //...`，补全 build、test、run、
  插入 label 和跳转 target definition。
- 打开 `bazel-bin` 或 `output_base`。
- target cache 放在 `var/bazel/`，默认有效 300 秒；保存 BUILD、MODULE、WORKSPACE 或
  `.bzl` 文件会使对应 cache 失效。
- 构建命令进入原有 task/build 重跑链路，并应用项目环境和 direnv。
- 异步查询后，选择目标、插入 label、跳转文件及打开输出目录都回到发起操作的
  frame；原 buffer 或 frame 已关闭时不打开别处的页面。查询失败、空结果会反馈；
  插入命令释放位置 marker，缓存写入失败不阻断已取得的目标列表。
- 只有当前 target 能找到 `buildifier` 时，Bazel buffer 才启用保存时格式化。

同一入口也放在 `SPC r k` 的任务工作台中。

### 日历、Org 与 macOS

- `SPC a y` 打开全年日历；`<`/`>` 或 `[`/`]` 前后移动一年，`g` 回到今年。
- Org localleader `l`：在 link 上编辑；region + 剪贴板 URL 时用 region 作为描述；只有
  URL 时由 `org-cliplink` 获取网页标题。
- `SPC a m` 打开 macOS 工具：系统取色器会把颜色复制成 `#RRGGBB`，prefix 调用时也
  插入当前位置；另保留当前配置已有的 macOS open-at-point。

## 加载策略

功能按现有模块边界放置：GitHub 在 Git 模块，Bazel 在语言/任务模块，Org、Dired、
编辑和 macOS 工具各自独立。仅交互时需要的包通过 `:commands`、autoload 或
`with-eval-after-load` 延迟加载；全局 last-change 记录和缺失父目录 hook 需要持续生效，
因此随主配置启用。

## 明确未采用

- Downloads 目录监视、通知、收件箱和历史记录。
- Xenodium 配置中的 Ivy/Counsel、Company、Eglot、Projectile 等旧基础层。
- 与个人目录、邮件、RSS、博客发布、财务查询和特定 Xcode 工作流绑定的功能。
- 已由当前 Remote、Consult、Vertico、LSP、Noema、Dirvish 或任务系统覆盖的重复实现。

这样保留了可复用的交互，但没有把另一套配置的全局状态、包栈和个人路径带进来。


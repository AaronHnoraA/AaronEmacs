# Aaron Elegant UI

## 范围

本文定义 Emacs 内自建界面的默认构建方式、主题来源和浮窗边界。原生
Customize 仍负责编辑 `config` 设置，Transient 仍负责命令菜单；Noema 的
CSS 与编辑器组件由 Noema 自己维护。

## 模型

Aaron UI 是 Emacs 与 Noema 共用的设计系统。深色 Kanagawa Wave 是 Emacs
的默认基调；`lisp/core/aaron-ui/aaron-ui.el` 提供语义角色、间距和形状 token。
Noema 继续由自己的 `pages/style.css` 独占现有配色和 Markdown 宽度，
共享层不会覆盖这些变量或布局规则。

在 Emacs 中用 `M-x aaron-ui-load-theme` 选择 Wave、Dragon 或浅色 Lotus。
启动时使用 `aaron-ui-default-variant`；三种主题的界面语义色都跟随各自的
底色。标签栏、mode line、Org 标题和补全列表使用同一套主题色，减少
与正文竞争注意力的高饱和色块。Noema 的已提交 CSS token 仍导出 Wave。

新的 Emacs 自建交互界面默认用 ETAF 的 View、ref 和 Runtime 布局与更新，
用 ETAF UI 的公开 Component 构建按钮、输入、列表、状态和浮层。
`aaron-ui-etaf` 把 Aaron UI 语义色映射到 ETAF Theme；业务界面不直接依赖
ETAF UI 的内部 `lisp/` 文件或私有函数。现有简单只读报告可继续使用
`aaron-ui-board`，原生 Customize、Transient 和专用原生视图保持各自入口。

`aaron-ui-float` 承载普通 Emacs buffer 或 ETAF View，不占用原有窗口布局。
调用者用 `:kill-on-close` 决定普通 buffer 是否在关闭时结束；隐藏始终保留
buffer 和会话。ETAF View 的 Runtime 由浮窗承载层挂载和释放。标题栏统一提供
拖动、红黄绿的关闭／隐藏／大小操作，并可增加页面自己的动作。整页 Activity 和
Performance Watch 保留当前展示方式。图形 frame 使用子 frame；TTY 入口
退回普通 buffer 或原生命令。

## 契约

- **C1** Aaron UI 是 Emacs 自建界面的颜色来源。ETAF UI 组件从
  `aaron-ui-etaf-theme` 接受同一组语义色；切换 Wave、Dragon、Lotus 后，
  已打开的 ETAF View 读到新 Theme。
- **C2** 新交互界面优先复用 ETAF UI 的公开组件和 ETAF 的生命周期；
  不为相同按钮、输入或浮层再造本地状态模型。`aaron-ui-board` 服务已有或
  简单的只读报告。
- **C3** `aaron-ui-float-show` 展示任何普通 buffer；
  `aaron-ui-float-show-view` 挂载 ETAF View。两者共用标题栏、焦点、隐藏和关闭。
  业务状态由调用方持有；关闭 ETAF View 时释放 Runtime。隐藏保留 buffer；
  需持续运行的应用由调用方显式管理 session。
- **C4** Config 管理直接使用原生 Customize；注册表、校验、应用与存储由
  Config 后端负责，不另建设置界面。
- **C5** ETAF、ETAF UI 和 posframe 在启动时只登记包与命令；首次打开相关
  页面或浮窗时才加载实现。
- **C6** Markdown 文档浮窗在打开时读取并解析本地文档，使用 ETAF UI 的
  Markdown 渲染器、目录和滚动容器；链接跳到本页标题或打开对应文档。
  `config-manual-panel` 展示仓库现有的 Config 设计文档，TTY 打开原文件。
- **C7** 启动 Dashboard 的 `[config]` 仍调用原生 `config-board`，设置的
  控件、校验和保存只由 Customize 与 Config 后端负责。
- **C8** `[docs]` 使用 ETAF 文档浮窗打开 `documentation-center.md`，再从
  该页进入 Emacs、Noema 和 Config 的现有 Markdown 文档；官方 Emacs 手册
  仍用 Info。相同入口在 `config-dispatch` 的 `h`。Dashboard 导航按钮在窗口
  显示后按实际字形宽度居中，不触发滚动；导航区域的结束 marker 不随随后
  插入的内容移动。Recent Files、Projects 与 Agenda 各自按最长可见条目
  测量内容块宽度，把整个块居中，块内条目共用左边界；各标题分别居中。
  长路径使用 Dashboard 自带的中间截短展示。
- **C9** 文档浮窗的“Ask · 询问配置”直接调用已有的 Noema agent-shell → ACP
  后端；只用 minibuffer 输入问题，并在回显区显示回复。后台会话不打开窗口，
  用仓库真实路径作为工作目录，单次提问只显示最终答案，回答后释放。
  默认 Codex `gpt-6-luna` 模型由 Config 注册项
  `+config-ask-model` 修改，沿用现有 Codex 登录态。
- **C10** 文档浮窗标题栏右侧不另放翻页文字；红色关闭、黄色隐藏，绿色左键
  上一页、右键下一页。`C-c C-=` 切换大小。同一 ETAF View 中切换文档，
  保留用户拖动后的位置。

## 实现位置

- `lisp/core/aaron-ui/aaron-ui.el`：Aaron 色板与主题选择。
- `lisp/core/aaron-ui/aaron-ui-etaf.el`：ETAF Theme 适配和开发预览。
- `lisp/core/aaron-ui/aaron-ui-float.el`：普通 buffer 与 ETAF View 的浮窗承载。
- `lisp/core/aaron-ui/aaron-ui-docs.el`：通用 Markdown 文档浮窗与个人配置文档入口。
- `lisp/ui/theme/config.el`：包声明、延迟加载入口与 Dashboard `[config]` 动作。
- `lisp/core/config-tools.el`：原生 Config 菜单和文档入口。
- `lisp/tools/agent/config.el`：询问配置的隐藏 ACP 和 minibuffer 入口。

## 已知差距

现有 Activity 和 Performance Watch 仍有各自的手写基础控件；后续改动这些
页面时可逐项换用 ETAF UI 组件，但需保留原有焦点与键盘动作。

## 待定设计

暂无。

## 语义角色

- `role-critical`：错误和不可逆操作
- `role-popout`：当前焦点和最需要注意的操作
- `role-strong`：正文主信息
- `role-salient`：链接、项目和二级强调
- `role-faded`：元数据与辅助信息
- `role-subtle`：边界、分隔和最低层级信息

Emacs 组件可通过 `aaron-ui-token`、`aaron-ui-color` 使用这些角色。Noema
导入角色 token 供新组件显式选用，但 Elegant adapter 只应用间距、圆角和交互
形态，不改变既有色值。

## Noema token 同步

Noema 使用仓库内已提交的纯 CSS 导出物，因此桌面 App 和发布站点可以独立于
Emacs 构建和运行。

```sh
make ui-tokens
make audit-ui-tokens
make ui-test
```

修改 Aaron UI token 后先重新导出，再运行审计。`make health` 已包含 token
测试与漂移检查。

## 窗口与文件界面

- 无原生顶栏的 GUI frame 不额外预留内部边距。窗口分隔线视觉上保持 1px，
  原生拖拽命中区为 7px，并沿用当前主题自己的背景与分隔色。
- Doom modeline 保留在底部；`C-x w d` 切换窗口 dedicated 状态，
  `C-u C-x w d` 设置强 dedicated，状态栏分别显示 `d` / `D`。
- Dired/Dirvish 和 Ibuffer 使用 Material SVG 文件图标。
- Treemacs 保留远程、Perspective、Projectile、Magit、LSP 行为和现有配色，
  Elegant 层只调整行距。
- TTY 或没有 SVG 支持时自动退回无 SVG 图标界面。

# Aaronnote 当前架构

本结构在 Overleaf Source Editor 的分层原则上改造，但 Aaronnote 仍以 Markdown
为唯一真相源；不包含 LaTeX Visual Editor、OT、编译或 Overleaf UI。

## 编辑器

```text
src/cm6/
  commands/                 Markdown 编辑命令
  extensions/index.ts       唯一 feature composition root
  extensions/visual/        Visual/Source mode 与 visual features
    widgets/                数学、proof/org-env、表格、图片、Jupyter 等
  languages/markdown/       Lezer Markdown 配置与 Aaronnote 扩展
  utils/tree-operations/    增量 change/viewport 查询
  utils/                    effect 与 projection 基础设施
  editor-cm6.ts             稳定 Editor facade 与 standalone/embedded shell
```

`editor-cm6.ts` 只决定宿主策略（history、editable、DOM event、callback）；语言、
feature 顺序和 visual mode 由 composition root 决定。Visual mode 使用
`StateField + StateEffect + Compartment`，不再依赖闭包状态。
Pointer drag 使用独立 StateField；拖选期间复用 decoration，结束后一次刷新。

原有性能路径保持：viewport delta、8ms/16KB 解析预算、CJK line cache、近变更修补、
byte-budget caches、MeasuredWidget、async epoch 以及 worker/observer teardown。

## 浏览器应用

```text
aaronnote/
  features/zoom/controller.ts   缩放状态、手势 listener、timer 生命周期
  features/writing-stats/       文档/章节缓存、idle 调度与大文档延迟
  main.ts                       兼容装配入口；其他 feature 按相同边界逐步拆分
```

Controller 显式返回 `destroy()`；`window.aaronnoteApi`、宿主事件和 xwidget wire
protocol 保持不变。

## Node host

```text
server/
  Features/*/api.mjs            feature controller / channel registration
  Features/Session/manager.mjs  可独立测试的 session 领域逻辑
  infrastructure/api-router.mjs transport-neutral router 与冲突检查
  lib/runtime.mjs               旧 public facade 与尚待迁移的领域实现
web-host.mjs                    HTTP/SSE、静态资源、router composition
```

HTTP handler 不再直接拥有 Jupyter、Assets、Session、Tasks、Filesystem、Prose、
Emacs channel 表。Session manager 通过注入合法路径和原子写策略与 runtime 解耦。

## Emacs

```text
lisp/roam/init-aaronnote.el
  进程、buffer/session、公开命令和 UI 装配
lisp/roam/aaronnote/emacs/aaronnote-xwidget-keys.el
  md/xwidget 输入、焦点、Undo/Redo、Shift-Tab 与 windmove bridge
```

按键桥迁移时保留了原命令名和调用协议；浏览器端 `xwidget-key-guard.ts` 无语义变化。

## 兼容与性能门禁

- `.md`、`.markdown` 与 README 仍由 Aaronnote 处理；`.tex` 不由 Aaronnote 接管。
- Markdown source offset、Editor facade、API channel、SSE command 和 Emacs 公开命令不变。
- Aaronnote 主题、proof/custom block class 和样式不变。
- CM6 roundtrip/command/editor API、5MB 大文档、Node feature、xwidget ERT 必须通过。

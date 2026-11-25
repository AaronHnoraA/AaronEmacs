# ETAF Markdown 源码对照与迁移设计

Noema 的主笔记界面是 Emacs 承载的 WebView。CodeMirror 同时提供原位编辑和视觉排版，Markdown 文件持有内容和身份。向 `etaf-md` 学习每类组件的界面、使用手感、解析和更新方式，并为它支持的每种语法和组件提供 Noema 对应能力。语法优先复用 CommonMark/GFM、Noema 的 `#+begin …` 环境、`@@` 命令和现有属性。字体和通用 `org-env` 外观采用当前 Noema 设计。Emacs 中的 ETAF 页面使用 Aaron 主题与 ETAF 组件。

本文的实现状态以每行的「Noema 对应入口」为准。「扩展」表示已有可复用的入口，「新增」表示提议中的能力。「待核」表示需要用同一输入跑两边再确认。现行语法由 [Noema 语法](../../noema/design/syntax.md) 定义。

## 源码和本机验证

参考 `etaf-md` 0.3.0，固定在 `b4157e5c88a5df32a0bb5d74a77aa805ba62d825`。加载该版本的 Elisp 后枚举 `etaf-md-node-definitions`，有 59 个注册项。其中 53 个有文档可写的 `:name`。`cell`、`code-block`、`list`、`list-item`、`table-block`、`table-row` 是内部节点。在有名字的容器之外，另有 `col`、`stat`、`tab` 等父组件声明的子节点。完整能力清单覆盖三者。 [源码](https://github.com/emacs-taf/etaf-md/tree/b4157e5c88a5df32a0bb5d74a77aa805ba62d825) · [扩展文档](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/docs/extensions.md)

在 macOS 的 Emacs 31.1 上，用临时目录加载 ETAF 0.3.8、ETAF UI 0.3.6、Ebox 3.1.5、ECSS 0.1.2。运行了 `etaf-md` 的 5 项阅读、双栏、跟随和资料权限测试，全部通过。原生模块用 CoreText 编译成功，另两项原生测试通过，覆盖 Mermaid、块公式和临时语法错误时保留旧图。同一组新版依赖下，Aaron 配置的 10 项现有 ETAF 集成测试通过。以上均是批处理测试。实际界面的版心、鼠标可达性和窄窗呈现单独验收。官网 tar 包与源码头均声明 ETAF 0.3.8、ETAF UI 0.3.6，README 里的依赖下限低于软件包元数据。 [包入口](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/etaf-md.el) · [ETAF 验收用例](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/tests/etaf-md-tests.el) · [Aaron 集成测试](../../../test/aaron-ui-etaf-tests.el)

## 可迁移的做法

| 层面 | 在 ETAF Markdown 中的实现 | Noema / Aaron 的迁移点与验收 |
| --- | --- | --- |
| 阅读布局 | `etaf-md--view-document` 把内容限制在可居中的版心内，顶层、正文和脚注各自排版；左右 gutter 给边框和长表留界限 | Aaron 文档浮窗使用有上限的正文栏，按当前视口收缩；目录、正文和动作区之间靠对齐、间距和细线区分。Noema 已有正文宽度，比较组件在列内、窄窗和长内容中的溢出与对齐。 |
| 视觉层级 | `stats` 以数值、标签和趋势构成视线顺序；`table` 用表头、数值对齐与淡分隔线；`timeline` 用色点连接事件；卡片的 `accent` 与 surface 各有职责 | 用现有主题色做主次、统一组件内边界；学术表沿用三线表。指标数值可用位置、对齐和颜色建立层级。现有字族、字号、字重、行高和通用 `org-env` 样式固定。 |
| 操作反馈 | `etaf-md--hover-fill` 和各组件视图区分可操作面、常驻选中态和 hover；表头排序、复制、折叠、返回源码各有明确落点 | 只有可操作控件获得 hover 手势线索，键盘焦点有独立可见状态；按键和鼠标做同一件事。非导航操作保持视口，布局变化时保留焦点和组件交互状态。 |
| 解析 | `etaf-md--parse` 预先识别容器的边界、类型和属性，把容器正文及普通文档交给 `etaf-ui-markdown-parse`。类型注册提供属性校验、子项约束和回退 | 循现有 `#+begin/end`、围栏和行内语法入口，共享组件内容模型供编辑器和 HTML 使用。用样例锁定同名嵌套、代码围栏中的伪边界、不闭合、非法属性和标题 ID。需要跨端消费的语义使用 `shared/` 契约。 |
| 增量更新 | `after-change` 记录旧区间和新区间；合并后用 `etaf-md--splice-parse` 解析变化片段，并复用两侧块。块位置容器持有稳定 slot 和独立的状态 ref | 对照 Noema 的 CM6 `ChangeSet`、范围映射、构件注册和 `orgEnvBlocksField`。当前普通修改可走映射，边界变更无法映射时可能扫描全文。以固定样例验证全量解析与增量结果等价，同时验证未受影响的 widget DOM、展开状态、焦点和滚动。引用定义等全局依赖须纳入失效规则。 |
| 异步排版 | Mermaid/MathJax 的任务键包含类型、源码、主题和后端，成功结果缓存，编辑过程中保留上一张有效图 | Noema 的 Mermaid 实时预览已有保留上一张图和失败反馈。接下来验证快速连续修改时旧任务无法覆盖新结果，检查跨主题、缩放和离屏重挂后的表现。保留现有 Mermaid 和 KaTeX 渲染入口。 |
| 资料与执行 | `etaf-md-assets.el` 在渲染函数之外读取文件、版本或符号信息，并让可执行动作经显式操作与确认进入后端 | Noema 组件消费现有引用、资产、版本和工作区能力，渲染函数只读模型。动态执行沿用已授权的工作区执行入口，发布或导出只消费可发布的快照和源内容。 |

[上游架构与基准](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/docs/architecture.md) · [Noema 编辑器构件](../../noema/design/rendered-constructs.md) · [Noema 滚动归属](../../noema/design/scroll-ownership.md) · [Aaron UI](../../design/elegant-ui.md)

## ETAF 应用与游戏的视觉参考

查看了官网的 Markdown、网易云音乐、微信读书、Desktop，以及四个游戏的界面图。游戏提供明确的主次和状态表达实例，文档与管理界面按各自的信息密度借用。 [ETAF 官网](https://etaf.top/#games)

| 参考 | 图中可观察到的处理 | 适用位置 |
| --- | --- | --- |
| Markdown 阅读视图 | 源和阅读面可对照，长单元格在表内换行，信息按列对齐 | 窄窗让数据留在块内，精确保持从渲染到源码的落点。 |
| 网易云音乐 | 当前播放项、主动作和进度共用一种强调色，歌曲、时长、操作各成一列。`etaf-ncm` 的行变体在紧凑模式优先保留名称 | Aaron 管理页让当前项与键盘焦点各有反馈；表格按信息重要性改变窄窗排布。 [视图源码](https://github.com/emacs-taf/etaf-ncm/blob/0b1eadd04979b4169247645f81048c2b5ffd00ba/lisp/etaf-ncm-views.el) |
| 微信读书 | 小进度条、百分比和时长放在相邻列。`etaf-weread--row-meter` 让未读项保持空白 | 进度组件对齐标签、轨道和数值；表格按字段区分缺失值和有效的零。 [视图源码](https://github.com/emacs-taf/etaf-weread/blob/fce49ea69cdb05c3214325296074e24785426167/lisp/etaf-weread-views.el) |
| Desktop | 聚焦的窗口与任务栏项目有可见状态，窄小播放器仍留主要操作 | 借鉴 Aaron 浮窗的焦点归还、收缩和显示状态。 [运行时源码](https://github.com/emacs-taf/etaf-desktop/blob/008988c26ae592e6fd2a3a0067b1aa1fdb9e265e/lisp/etaf-desktop-runtime.el) |
| Verdant 纸牌 | 弱标签成组排列，数字与操作提示贴近所解释的对象，规则线将两个区域分开 | 指标组、进度和键位说明。 |
| Chu He 象棋 | 棋盘线的明度低于前景，中央带留出视觉停顿 | 表格分隔线、时间轴轨道、目录边界退到辅助层。 |
| Iron Tide 坦克 | 可视网格和实体的边界清楚，地图依材质和符号区分 | 研究有密集离散状态的图表如何标识变化。截图本身不提供局部重绘算法证据。 |
| Salvage Station | 地图、检查器、选项卡分别在明确的面板内，数值在行右侧对齐。源码把选择、地图格与标签状态分别交给组件 | Aaron 的检查器和日志面板，以及 Noema 的轻量标签切换与数值组件。 [视图源码](https://github.com/emacs-taf/etaf-salvage/blob/42612524c3a8e842520b4d03e010833ab7e05e5a/lisp/etaf-salvage-app.el) |

### Gemini 审查

通过 `agy` ACP 用 Gemini 4 Neon (High) 审阅。第一轮提交了项目负责人的原话、WebView 和 Markdown 的角色、保留字体与 `org-env` 等约束及八张公开参考图。收到的设计建议包括窄窗目录转抽屉、弱化结构线、排序区分视图与源、按查找结果打开被隐藏的页签。第一轮在输出末尾超时，已收到的内容用于设计讨论。

第二轮使用同一合成文档的四张实际 Emacs 浮窗截图，Gemini 正常完成回复，结论为通过。宽窗居中、目录细线、窄窗完整正文和抽屉在截图中可见。程序测试另外覆盖跨宽度的布局切换、键盘激活抽屉、跳到标题和焦点归还。Gemini 对抽屉高度、层间对比和当前章节标记的建议为后续可选改进。截图与原始答复存放在本机 `.agent/views/etaf-md/aaron-docs/`。

## 53 个容器的对应入口

| 上游可写名称 | Noema 对应入口 | 组件独立验收点 |
| --- | --- | --- |
| `note`, `tip`, `important`, `warning`, `caution`, `info`, `danger` | 已实现 `[!type]` 默认标题及折叠，原生环境可选择折叠正文 | 标题、语义色、嵌套状态、惰性正文和完整导出。 |
| `columns` | 已扩展原有 `#+begin columns` 和 `#+col`，支持等宽减栏和混合轨宽 | 权重和混合宽度、最小宽度后堆叠、窄窗顺序、导出。 |
| `center`, `justify`, `left`, `right` | 已实现四种普通 Markdown 对齐环境，沿用 Noema 的环境边界 | 段落和列表的对齐、行内及嵌套内容、原位编辑。 |
| `box`, `card`, `panel` | 已实现独立的 `box`、`card`、`panel` 正文，复用已有环境标题 | 面与语义色、状态、主链接和独立正文链接、窄窗尺寸约束。 |
| `steps`、`step` | 已实现 `#+begin steps [title]`，普通有序列表保留起始编号，任务标记提供状态 | 当前项、已完成项和等待项均可读，保留正文和普通列表回退。 |
| `timeline`、`event` | 已实现 `#+begin timeline [title]`，普通列表承载日期、标题、状态与正文 | 独立日期状态行、竖向和横向布局，窄窗纵向按原序阅读。 |
| `stats`、`stat` | 已实现 `#+begin stats [title]`，普通 GFM 表格的一行对应一张指标卡 | 名称、值、趋势、说明、迷你柱图、语义强调；窄窗自动收列。 |
| `progress` | 已实现 `#+begin progress [title]`，正文为带标签的普通 Markdown 无序列表 | 多项、百分比及 `完成量/总量`，数值可读，窄窗无溢出。 |
| `chart` | 已实现 `#+begin chart [title] {type=…}`，用 GFM 表格承载类别、单位及数值 | 条形、堆叠、热图、折线，保留原始数据和文本可访问性。 |
| `table` | 已实现 `#+begin table [title]`，复用 GFM 表格和现有 Markdown 源码编辑 | 可选排序、复制、行列和单元格强调。窄窗局部横移，发布保留原表顺序。 |
| `field`, `form` | 用参数卡和两种列表布局呈现说明、配置和值，匹配当前 ETAF 的文档语义 | 参数类型和状态、默认值、约束、窄窗键值布局、链接和静态导出。 |
| `file-tree`, `code-tree` | 已实现 Markdown 列表目录和围栏代码树，复用文件链接和语法高亮 | 目录折叠、键盘移动、相对文件、选中预览、长行换行、复制及完整导出。 |
| `project-tree` | 已实现工作区目录和有界展开，复用文件树的键盘交互和主题 | 根范围、目录和文件识别、刷新保留选择、静态回退。 |
| `chat` | 已实现用 Markdown 标题分组的消息、作者归属和时间行 | 长代码、时间与作者、键盘链接、完整复制。 |
| `tabs`, `code-tabs`, `tab-panel`, `swiper` | 已实现以同层 Markdown 标题分组的标签、代码和轮播，纯代码组可从围栏推断页名 | 初始页、键盘换页、窄窗导航、编辑别处保留选择。只挂载活动页，按源顺序导出所有页。 |
| `details`, `accordion` | 已实现原生单块折叠和同层 Markdown 标题分组，复用选页的源码与状态模型 | 单开、多开、默认展开、鼠标键盘、闭合卸载正文、打印保留全部内容。 |
| `window`, `demo` | 已实现限高阅读窗与可展开源码的 Markdown 演示 | 独立焦点、窗口局部滚动、逐字符复制、窄窗换行和完整导出。 |
| `diff` | 已实现只读比较，用两份普通 Markdown 围栏，共享字词比较的有界算法 | 并排/统一切换、窄窗自动堆叠、配对行、完整复制和对应行源码。 |
| `pros-cons` | 已实现双栏比较环境，支持自定义栏头、普通 Markdown 和跨栏引用 | 语义各自可读，窄窗按源顺序堆叠，结构有误时保留可读内容。 |
| `keymap`, `breadcrumb` | 已实现键帽速查和文档路径，使用列表、语义化标签和现有链接导航 | 当前项、键位分组、紧凑时的换行、键盘打开链接。 |
| `log`, `statusbar` | 已支持 Markdown 中的级别、时间、模式、快捷键和状态文字 | 与正文共用字体，宽窄窗重排，静态导出保留原文与配置。 |
| `home-box`, `image-card`, `link-card` | 已实现首页卡、图片卡和链接卡，复用文档框体、附件解析与宿主链接入口 | 主动作可键盘到达，卡片保持独立次链接、自然图片比例和普通 Markdown 回退。 |
| `qrcode` | 已实现离线二维码，可用 `text` 属性或普通文本围栏，保留原始值 | 深浅主题下保持黑白编码、安全区与方形比例，容量不足仍显示原文。 |
| `npm-badge`, `npm-to` | 已实现静态包元数据和围栏命令转换，复用标签页 | 保留包名与版本、窄窗工具选项、活动命令复制、复杂脚本原文和静态导出。 |
| `include`, `code-ref`, `symbol` | 已实现本地摘录和 Emacs 符号卡，支持源码定位、帮助、命令签名和键位。行内符号也可以写作者提供的释义和链接 | 转到源行或原生帮助、切换笔记时清除旧结果、发布使用作者回退。 |
| `git-log`, `git-diff` | 已实现 Git 与 Jujutsu 的按路径历史和统一补丁，支持键盘复制对象 ID 和补丁 | 已删除文件、仓库边界、刷新和异常回退、窄窗元数据。 |
| `run` | 已接入原生编译命令输入和项目输出面板，卡片显示状态和有界输出 | 运行由按键触发，Emacs 审核后执行；退出、取消、停止可追踪，导出保留作者命令。 |

父组件记录所有子节点数据：`col`、`step`、`event`、`stat`、`tab`、`collapse`、`before`、`after`、`pro`、`con`、`slide`、`file`。对应的 Noema 源优先使用已有列分隔、标题、列表、表格、围栏或必要的环境嵌套。`list`、`list-item`、`table-block`、`table-row`、`cell`、`code-block` 已由 Markdown 解析节点表达，验证列表、表格和围栏传给组件时保留原来的子内容。

## 内联、源语法和通用交互

| 能力 | 已有基础与需验证的补齐点 |
| --- | --- |
| CommonMark/GFM 与可选方言 | 两边都识别常见段落、引用链接、列表、标题、行内强调、围栏、任务和表格。用相同的行尾、边界、嵌套、转义样例确认具体差异。参考上游的 CommonMark/GFM/功能开关，先明确 Noema 阅读和导出的兼容情形，再决定是否需要文档级开关。 |
| YAML、Wiki、图像与视频 | 复用 Noema metadata、Wiki 符号解析、附件和预览，核对 YAML 中作者、日期、状态等字段，媒体海报、连续浏览、播放/暂停、失效回退及有界缓存。 |
| 脚注、任务框、标题导航 | 已有脚注、TOC 和源码任务框。验证脚注往返、书签/标题跳转、原位写回与撤销、键盘导航。 |
| 高亮、剧透、词汇表、注释 | 已有高亮和 Noema 批注。新增剧透、术语卡、带 Markdown 和链接的注解，以及只在源稿里显示的说明。 |
| `kbd`、徽章、语义色、符号 | 已实现 Noema 行内命令，键帽沿用 `<kbd>`，徽章和色片使用主题语义。符号卡用作者写的释义或链接。 |
| 告警、折叠和代码 info string | 已实现可选行号和起始号、连续尾行折叠，以及差异、焦点、警告、错误、词级标记、空白和缩进显示。保留代码标题、`{行号}` 与原稿复制。 |
| 块公式、Mermaid、媒体查询 | 复用 `\[…\]` 与 KaTeX、既有 Mermaid 和灯箱。参考上游缩放、按主题取色、占位和缓冲策略。以 Noema 语法和现有数学宏顺序验证视觉效果。 |
| 阅读与源交互 | Noema 保留原位编辑及源码入口。两侧各自验证可调版心、目录定位、全文搜索、焦点返回、鼠标和键盘导航。可选阅读视图的跟随应以源锚点验证，并保证拖动选区或历史返回时无意外滚动。 |
| 扩展定义 | 上游允许宿主注册新节点、属性、子项和视图。迁移为 Noema 的有类型组件定义与固定声明式样式角色。先让内置组件共用契约，再验证项目定义发现、确定性回退、发布及禁用后的可读性。 |

[使用手册](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/docs/manual.md) · [上游展示文档](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/examples/widgets-showcase.markdown)

## 交付与验收顺序

1. Aaron 文档浮窗。用现有配置手册验证版心、目录和动作；Wave、Dragon、Lotus 均加载，宽窗静止居中，窄窗仍能导航。验证文档历史、标题跳转和主题切换。
2. Noema 组件表现与交互的样板。选截图中能直接对照的进度、指标、排序表与 Diff；输入要能以普通 Markdown 阅读。用 Day/Night、桌面/窄窗、鼠标/键盘和保存前后回读验收。
3. 普通语法、静态组件与多面板交互。共享边界、属性和子项模型，逐批通过文件读回、撤销、HTML/发布和 PDF 回退；完整清单每一项都要有用例。
4. 引用、文件、版本与显式运行。为动态组件建立真实工作区 fixture，覆盖授权目标、错误、过期结果和发布快照。保留现有后端的状态及操作边界。
5. 增量与真实 WebKit。针对普通段落编辑、顶端插入、块分割、边界破坏、代码围栏、定义变更和大文档，采集冷启动、单次更新、p50/p95、长帧、DOM 复用及滚动偏移。每个优化先用同源输入检查等价性，再与自己的基线做比较。ETAF 标注的帧预算与 Noema 的 WebKit 延迟分别记录，两边使用不同渲染层。

每批都检查既有 `org-env` 与字体，用现有常用笔记和同一套混合中文、英文、数学、代码、表格样例做前后对照。组件颜色与可用状态对照本主题。将本地 GUI 或 WebKit 检查与结构、源码和行为测试的结果一并记录。

### 首个 Noema 组件的验证

`progress` 复用 `org-env` 的标题、侧线和结束标记，编辑器只将正文列表替换为图形进度和文本值。宽版按标签、轨道、数值三列对齐，窄版用两行保全标签与数值。第二轮 Gemini 验收通过。WebKit 27.2 中用 1060 px 的深浅主题和 425 px 的深色主题验证无组件横向溢出，值与正文的字族、字号、字重和行高相同。Go 预处理加 Pandoc 的 LaTeX 输出保留了标题和列表。截图与审阅在本机 `.agent/views/etaf-md/noema-progress/`。 [当前语法](../../noema/design/syntax.md#progress-rows)

实测中，在笔记前插入文字后，CodeMirror 可保留进度组件的 DOM。点击原来取了旧起点，修复后使用该 DOM 在当前文档中的位置。行为测试覆盖插入后点开源文，以及光标在进度正文、环境标题、结束标记之间切换。另用真实 WebKit 的滚动检查验证点击、离开与惯性滚动期间的稳定性。

在同一台 Mac 的 WebKit 27.2 中，用同一个进度块加合成段落做一次定向测量。编辑（30 次固定宽度替换）的同步 `dispatch` 观测没有计入后续布局与绘制，时间分辨率约 1 ms。9 KB、89 KB、449 KB 的测试文档中，挂载分别为 33、43、68 ms；进度数据编辑的中位数均为 1 ms，p95 分别为 2、4、3 ms。结果提供下批组件的参考，覆盖范围仅为这三个输入和这次运行。

### 指标卡验证

`stats` 从 GFM 表格读取指标和值，支持趋势、补充说明、等间隔迷你柱图和语义色。显示层与进度块共享正文组件生命周期，编辑时仍使用原有表格。WebKit 27.2 实测宽屏双主题三列、425 px 深色单列，组件无横向溢出，检查了点击回源码和正文排版。Gemini 将高饱和趋势色列为第一轮阻断点，改用语义色并减弱图形色后复审通过。Go 预处理加 Pandoc 的 LaTeX 输出保留了标题和源表的值与趋势。样例、截图和审阅在本机 `.agent/views/etaf-md/noema-stats/`。 [当前语法](../../noema/design/syntax.md#metric-cards)

### 图表验证

`chart` 复用 GFM 的解析结果和 `org-env` 的正文渲染生命周期。条形图共用连续零线，构成图提供原值和占比，热图区分空值、零、正数和负数，折线的缺失值断开并保留数据表。宽屏深浅主题及 425 px 的深色窗口验证了字体、字号、字重、行高和原地编辑。Gemini 复审通过了浅色图例、窄窗热图、连续零线和更轻的数据表。Node 与 Go 的 LaTeX 测试共用同一份预处理样例，输出的表格和彩色 WebView 图形读取同一份源数据。 [当前语法](../../noema/design/syntax.md#data-charts)

长文实测暴露了一个高度补偿的时序问题。CodeMirror 在 `requestMeasure` 的写入回调结束后补偿高度映射差量，Noema 此前在回调中也写了滚动位置，焦点处于编辑器时会叠加两次。修正后，Noema 在同帧微任务中按文档锚点校正。浏览器探针覆盖外部光标位置、焦点有无、退出编辑、惯性滚动和紧随点击的滚动；图表、指标卡、进度块三组均通过。样例、截图、审阅和实测记录存于本机 `.agent/views/etaf-md/noema-chart/`。

### 时间线与步骤验证

`timeline` 以普通列表承载内容，日期与状态在同一行，事件标题独立一行。宽视口横排卡片，425 px 窄窗沿轴线按原顺序向下阅读。`steps` 保留 Markdown 有序列表的起始号，使用任务标记标出已完成、当前和等待三种状态。转义过的方括号按文字显示。Gemini 复审通过了宽窄布局、卡片层级及既有排版。实测还检查了滚动、源码回读、行内链接与数学，以及保留编号的 LaTeX 回退。样例与截图存于本机 `.agent/views/etaf-md/noema-sequences/`。 [当前语法](../../noema/design/syntax.md#timelines-and-steps)

### 可排序表格验证

`table` 采用三线表，支持行、列和单元格的语义高亮，激活的列有底色、细线与方向指示。排序保持相同键值的原始顺序，剪贴板保留所见行序、原始对齐、行内标记和引用式链接定义。点击重排后的单元格使用它在原表中的位置。WebKit 的 1060 px 深浅主题与 425 px 深色验证了键盘排序和复制、窄窗横移及原地编辑。Gemini 最后复查通过了窄窗的工具条，验证样例与截图存于本机 `.agent/views/etaf-md/noema-tables/`。 [当前语法](../../noema/design/syntax.md#sortable-document-tables)

### 文档框体和对齐验证

`box`、`card` 与 `panel` 沿用 `org-env` 的标题、侧轨和结束记号，分别提供阅读框、明确的主链接和可见状态。四种对齐环境承载普通 Markdown。复审后默认对齐正文栏，面板状态在宽窗与正文并排，窄窗顺序换行。验证了 WebKit 宽窄与深浅主题、完整窄窗面板、键盘激活卡片链接、原地源码切换和滚动稳定性。引用在 Node 与 Go 的 LaTeX 回退中保留标题、描述、目的地和状态，引号中的逗号和分号有共用回归用例。样例与 Gemini 复审记录存于本机 `.agent/views/etaf-md/noema-frames/`。 [当前语法](../../noema/design/syntax.md#document-frames-and-alignment)

### 日志、导航和双栏比较验证

`log` 在宽窗对齐时间、级别和正文，窄窗将正文放到下一行。`keymap` 共用键帽与说明的对齐线，`breadcrumb` 可用键盘打开链接，`statusbar` 显示由作者提供的模式、键位和说明。`pros-cons` 在窄窗按源顺序展开两栏，允许引用定义跨栏使用。Gemini 阅读实际 WebKit 截图后指出窄窗键位说明拆字，复查时确认了修正后的对齐和换行。通过了 1060 px 深浅主题和 425 px 深色的字体、溢出、原地编辑和滚动检查。Node 与 Go 共用 LaTeX 样例，覆盖有效比较、格式错误的比较和状态文字。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-reading/`。 [当前语法](../../noema/design/syntax.md#reading-and-navigation-blocks)

### 二维码验证

ETAF 使用 `qrencode` 与磁盘 PNG 缓存。Noema 用固定版本的 `uqr` 在本地生成带四格安全区的 SVG，沿用 WebView 渲染缓存。宽窗图文并排，窄窗保留扫描区域和全文。macOS Vision 从 1060 px 深浅主题和 425 px 深色 WebKit 截图各解出了完整的 URL 与多行中文，实测图像为正方形、正文沿用原排版、选入源码或离开组件后文档与滚动稳定。Gemini 通过对齐、留白与文字层级审查。回归测试涵盖注释标记和布局属性的字面值、块引用、围栏缩进、超长回退与导出。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-qrcode/`。 [当前语法](../../noema/design/syntax.md#qr-codes) · [UnJS uqr](https://github.com/unjs/uqr) · [ETAF 二维码实现](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/lisp/etaf-md-qrcode.el)

### 首页卡、链接卡和图片卡验证

三种卡片沿用 Noema 的标题、侧轨和正文排版，主链接用带细框和箭头的动作入口。WebKit 的 1060 px 深浅主题与 425 px 深色实测包含键盘激活、卡片表面点击、相对次链接、嵌套主链接、原地源码和无横向溢出。作者态的正文可原地打开源码，发布页的内外卡片各自命中主链接。图片使用现有附件路径解析，宽窄窗都以完整原比例展示。Go 与 Node 共用导出样例，Markdown 写法保留图片，`src` 写法保留路径和替代文本。Gemini 复审通过主次动作、图片完整性和宽窄对齐。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-cover-cards/`。 [当前语法](../../noema/design/syntax.md#home-link-and-image-cards)

### 标签页与轮播验证

四种分段视图由 Markdown 标题和围栏定义。只在活动页挂正文，实际 WebKit 证实切到图片页前资源请求为零。下一页、上一页和键盘移动保留文档视口，窄窗长标签只滚动标签条。页面选择跟随环境源标记的位置，离屏后在文首插字、Source 往返、折叠及分栏转原位编辑都保留状态。同标题同正文的两个组各自保留原有选择。WebKit 实测确认页内链接、任务框、原地源码、深浅字体和布局，Gemini 通过四种视图的当前项、窄窗方向按钮和代码区边界审查。HTML 与共用 Node/Go LaTeX 样例按原序保留全部内容。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-tabs/`。 [当前语法](../../noema/design/syntax.md#tabbed-documents-and-examples)

### 单块折叠与折叠组验证

`details` 与 `accordion` 复用原生披露控件、Markdown 标题和按源位置保存的阅读状态。关闭正文时卸载其节点。重新打开混合嵌套的内容后，各组标签页和折叠沿用各自的选择。WebKit 宽窄实测核对键盘、鼠标、单开与多开、源码打开、原字体和已发布页面的折叠组互斥。PDF 和浏览器打印显示完整正文，Go 与 Node 的 LaTeX 回退保留各节原文。Gemini 通过箭头、源码控件、展开层级与窄窗换行审查。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-disclosures/`。 [当前语法](../../noema/design/syntax.md#details-and-accordions)

### 只读 Diff 验证

`diff` 对两份 Markdown 围栏做有界的行对齐，配对的变更行显示词级差异。窄窗统一显示同一份对齐结果，保留双列行号和删增符号。独立 WebKit 复核在普通正文、标签页、折叠和分栏中逐行打开正确源码，复制内容逐字符一致。Go 与 Node 的 LaTeX 样例覆盖 raw 格式标签和引用中的 Diff，实际 Pandoc 把两边保留为字面代码。比较超限时保留两段带标签的文本。Gemini 复审通过清除代码框后的行级颜色、词级标记和窄窗字行布局。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-diff/`。 [当前语法](../../noema/design/syntax.md#read-only-comparisons)

### 文件树与代码树验证

`file-tree` 用普通 Markdown 列表定义目录。相对路径交给现有 Emacs 链接入口，显式链接保留原目标。`code-tree` 只高亮选中的文件，宽窗并排显示目录与代码，窄窗沿文档顺序排列。代码预览支持长行换行、局部滚动、原生选区与逐字符复制。目录展开和选中文件按名称及路径记忆，在同块插入文件后仍指向原目标。独立 WebKit 复核覆盖长树键盘导航、中文路径、折叠打印与分栏中的预览。Go 和 Node 共用 LaTeX 样例，实际 Pandoc 验证列表中的 raw 格式示例保留为字面代码。样例、截图和 Gemini 审阅存于本机 `.agent/views/etaf-md/noema-trees/`。 [当前语法](../../noema/design/syntax.md#file-and-code-trees)

### 参数卡与字段列表验证

`field` 参数卡显示签名、状态、正文和约束。字段列表对齐名称和值，`form` 使用 ETAF 文档中的名称在上和值在下的读取布局。两种列表共用 Markdown 解析与链接处理，遵循转义，保留链接标题和代码里的冒号。宽窄 WebKit 实测覆盖正文排版、键盘打开类型链接与原地源码，Gemini 复审通过窄窗状态标签。实际 Pandoc 保留参数名、类型、默认值、约束和字段列表。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-fields/`。 [当前语法](../../noema/design/syntax.md#parameter-cards-and-field-lists)

### 阅读窗与演示块验证

`window` 以既有环境标题标明内容，高度由作者设置，达到上限后显示局部滚动条。`demo` 将效果留在上方，把源码展开与复制放在底栏的独立焦点中。源码区按需挂载，引用和列表中的演示保留原始 Markdown。发布页面显示已批准的演示正文，PDF 与实际 Pandoc 检查覆盖完整阅读窗内容。独立 WebKit 复核验证了分栏内的点击、拖动局部滚动条、引用与列表的精确复制，Gemini 复审通过宽窄布局和控件。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-demos/`。 [当前语法](../../noema/design/syntax.md#document-windows-and-demonstrations)

### 对话验证

`chat` 用同层标题分组，时间居中，作者的答复沿右侧排列。正文沿用 Markdown 和既有字体，窄窗代码跟随消息宽度换行。复制读取原稿，发布页面显示已批准的内容。WebKit 测试覆盖左右归属、长代码、键盘链接、整段复制和原地源码，Gemini 复审通过宽窄布局。实际 Pandoc 检查确认常规对话里的时间、作者及围栏字面代码都保留。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-chat/`。 [当前语法](../../noema/design/syntax.md#conversations)

### 包徽章和命令转换验证

`npm-badge` 显示作者提供的版本和下载文字。`npm-to` 共用一份 shell 分词结果，识别单行命令和引号外的 `&&`。各管理器使用其官方 CLI 写法，保留包范围、版本与引号内参数。复杂脚本保留原文。编辑态只挂载当前工具的面板，发布把配置过的工具一起列出，实际 Pandoc 导出保留原始 npm 命令。WebKit 样例覆盖 425 px 五个标签、键盘切换、原样复制和编辑后的选择记忆，独立复核还覆盖 361 px 双层标签页。Gemini 复审通过宽窄布局。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-packages/`。 [当前语法](../../noema/design/syntax.md#package-metadata-and-commands) · [pnpm CLI](https://pnpm.io/pnpm-cli) · [Yarn CLI](https://yarnpkg.com/cli) · [Bun 安装](https://bun.com/docs/pm/cli/install) · [Deno CLI](https://docs.deno.com/runtime/reference/cli/)

### 提示块验证

Markdown 引用从首行读取语义、默认标题和 `+`、`-` 折叠状态。原生提示保留 Noema 环境标题、侧线和结束符，标题旁的箭头折叠正文。编辑器与发布共用 MarkdownIt 的块边界。列表和标签页里的折叠按原稿位置记忆，正文在展开时挂载，宽窄 WebKit 的实测中收起时图像请求为零。PDF 和实际 Pandoc 保留完整正文，原地源码编辑与滚动复核另有测试。Gemini 用窄屏展开和宽屏收起的实机图确认了调整后的控件位置和排版。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-callouts/`。 [当前语法](../../noema/design/syntax.md#callouts)

### 代码围栏验证

行号、行与词级标记、可选空白符和缩进线沿用代码卡片字号。在 425 px 的 WebKit 截图中，长行的续行保持在源码列。折叠尾部保留首行和源码选择，复制包含注释的完整代码。独立复核覆盖列表、引用、相邻围栏、源头缩进编辑和 5,000 行样例，长块收起时样例仅挂载前三个代码行。发布 HTML 保留可展开尾部，PDF 包含全部行。实际 Pandoc 验证完整代码正文与指定起始行号输出。Gemini 复审通过续行位置、空白符和焦点状态。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-fences/`。 [当前语法](../../noema/design/syntax.md#code-fence-presentation)

### 行内阅读验证

答案原位展开，术语、注解和符号卡可以用键盘开启和关闭。窄窗附注在母句之后留出空间，并对齐正文左边，关闭后恢复间距。WebKit 测试覆盖 425 px 深色与 1060 px 深浅色、Standalone、链接、数学和源码往返。独立复核另覆盖标签页、分栏和原生折叠内的点击。Node、Go、Pandoc 和 HTML 验证展开释义与省略源稿说明的处理，含原生标题、未闭合的附加属性与代码字面内容。Gemini 复审通过附注边界、极窄视口的标点间距和键帽、徽章、色片。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-inline/`。 [当前语法](../../noema/design/syntax.md#inline-reading-controls)

### 工作区摘录验证

`include` 和 `code-ref` 按行、标记、Markdown 章节或定义读源码。服务端用 CommonMark 块定位章节，TypeScript 和 JavaScript 用 Lezer 定位完整定义。WebKit 的真实临时项目验证 CRLF 复制、源行跳转参数、点击刷新、滚到视野后加载，以及错误时恢复作者回退。独立复核覆盖标签页未激活时零请求、同窗切换文件、外链越界拒绝和 Unicode 符号名。导出使用作者回退。Gemini 核对了宽窄截图的路径、行范围、操作区和完整错误图。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-excerpts/`。 [当前语法](../../noema/design/syntax.md#workspace-source-excerpts)

### 工作区目录验证

`project-tree` 从已保存笔记的工作区读取有限深度的目录。它复用文件树的分支和键盘移动，刷新后按同一路径恢复文件夹状态。WebKit 以真实临时项目验证键盘开文件、长文件名、无横向溢出、刷新和源码往返。共享 Node 与 Go 导出验证只保留作者回退。Gemini 复审通过文件与目录图标、窄窗续行和操作区。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-project-tree/`。 [当前语法](../../noema/design/syntax.md#workspace-directory-tree)

### 仓库历史和补丁验证

使用真实 Git 和 Jujutsu 0.45.1 临时仓库验证历史、修订范围、已删除文件以及带方括号、引号或空格的路径。只读操作前后，Git 索引字节和 Jujutsu 操作日志相同。独立复核发现了子目录笔记引用已删除根目录文件，以及空标记错误匹配外层仓库的问题，修正后均纳入回归样例。WebKit 覆盖分栏中的活动页惰性请求、刷新错误回退、键盘复制完整对象 ID 与补丁。发布 HTML、PDF 和 Pandoc 使用作者内容和引用参数。Gemini 第一轮指出窄窗的作者名断词，作者和标题移到整行后复审通过。补丁截图包含真实增删行。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-repository/`。 [当前语法](../../noema/design/syntax.md#repository-history-and-patches)

### Emacs 符号验证

符号卡显示实际 Emacs 会话的命令签名、第一段帮助、键位和 Lisp 文件名，点击可进原生帮助或定义。独立复核覆盖函数、变量、面孔、尚未加载的符号、分栏内的惰性加载及切换笔记时取消旧显示。函数和变量的动态文档属性只留给原生帮助处理，自动查阅只读取现有文档字符串和引用。实际 HTML、PDF 和 Pandoc 保留作者文本和符号名。Gemini 确认 UTF-8 引号、窄窗的完整签名边框及按钮间距，保留原环境和字体设置。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-symbols/`。 [当前语法](../../noema/design/syntax.md#emacs-symbol-cards)

### 文档运行验证

`run` 在用户点击后调用 Emacs 自带的编译命令输入，然后通过项目输出机制执行。原生测试以合成指令验证取消输入与执行审阅后修改的命令。WebKit 经实际 Emacs 网关验证成功、失败退出码、停止及打开原生输出，且原稿保持完整。卡片显示近期输出，状态请求暂时失败时继续重连。独立复核验证了切换标签页后跟踪同一次启动。Pandoc 和 HTML 保留作者命令及说明。Gemini 复审通过完整的窄窗命令换行、状态层次、输出区域以及原生环境和字体。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-run/`。 [当前语法](../../noema/design/syntax.md#commands-in-documents)

### 分栏验证

ETAF-MD 0.3.0 把布局、等宽卡片和分栏统一为 `columns`，按可用字宽计算每行的列数。Noema 在原有分隔符上加入份数、字宽和百分比轨道，并以 WebKit 实际测得的栏宽计算换行。分栏改变时按源顺序调整原有节点。1060、720 和 425 px 的 WebKit 样例分别测得三列、两列和一列，混合轨宽在放不下时堆叠且没有横向溢出。独立复核还验证了旧百分比布局、标签页状态和文末源码往返的字节保真。Standalone 使用同一布局函数，Node 和 Go 的 LaTeX 变换以约 80 字的标称行宽计算印刷比例，Pandoc 经 XeLaTeX 实际生成了包含全部栏的 PDF。Gemini 复审通过随栏宽延伸的原生标题线、同行等高竖线和窄窗堆叠分隔。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-columns/`。 [当前语法](../../noema/design/syntax.md#responsive-columns) · [ETAF 分栏源码](https://github.com/emacs-taf/etaf-md/blob/b4157e5c88a5df32a0bb5d74a77aa805ba62d825/lisp/etaf-md-nodes.el#L191)

### 脚注和引用式链接验证

正文、卡片、分栏和标签页共享 Markdown 定义，脚注按定义出现顺序编号。编辑器与独立阅读页各验证了重复引用的精确回跳。独立复核覆盖隐藏标记、嵌套标签页、编辑定义后的回跳、列表边界失效以及 Unicode 锚点。围栏保留字面代码，HTML 评论使用独立锚点。实际 Pandoc 输出保留跨块脚注、缩进续行和紧邻的引用式链接。Pandoc 对导出的评论和重名定义仍采用自身的全篇解析规则。Gemini 用 1060 px 深色编辑器和 425 px 浅色独立阅读页复审通过了脚注层次、末尾回链和两侧留白。样例、截图和审阅存于本机 `.agent/views/etaf-md/noema-references/`。 [当前语法](../../noema/design/syntax.md#document-references)

### 任务框验证

任务框保留原勾选样式，可用鼠标、Space 或 Enter 逐项改变 Markdown 标记。控件在状态转换后保留焦点，每次操作可以单独撤销。发布 HTML 在紧凑和宽松列表中都显示完成状态。真实 WebKit 验证了两次键盘操作、单次撤销、源码变化和焦点复用，Gemini 在 1060 px 和 425 px 的实机图中确认了焦点环、纵向对齐和完成态层次。样例与审阅存于本机 `.agent/views/etaf-md/noema-tasks/`。 [当前语法](../../noema/design/syntax.md#task-lists)

### 媒体验证

图片与 Mermaid 图共用放大对话框。真实 WebKit 验证了键盘开启、关闭后归还焦点、生成图的清晰渲染以及逐字节保留 Markdown。独立 HTML 使用同一查看器打开已内嵌的离线图片。实际 H.264 样例以浏览器原生控件载入，初始暂停，播放与暂停都成功。行内图库的左右切换沿用已有交互。Gemini 通过了宽屏图片与 425 px 图表的比例、关闭按钮和外缘留白。样例与审阅存于本机 `.agent/views/etaf-md/noema-media/`。 [当前语法](../../noema/design/syntax.md#media-previews)

### 独立阅读页验证

工具条提供目录与主题切换，带答案的笔记另有全篇遮罩开关。宽窗的目录靠右定位，窄窗目录在正文前展开，标题仍完整可读。WebKit 实测了焦点进入目录、跳转后聚焦标题、Escape 归还焦点、Shift+S 批量显隐和明暗切换，425 px 无横向溢出。Gemini 通过了宽窗深色与窄窗浅色的布局、焦点和原生环境外观。样例与审阅存于本机 `.agent/views/etaf-md/noema-standalone-reader/`。 [读取方法](../../noema/design/syntax.md#standalone-reading-controls)

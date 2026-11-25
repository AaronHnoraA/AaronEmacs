# Noema Markdown 细节：Pele 与 AutumnMark 学习笔记

后续的 [ETAF Markdown 源码对照](etaf-md.md) 覆盖界面、交互、解析、增量渲染和组件能力迁移。

状态：源码学习结论，供用户决定取舍；不表示下述改动已经实现。现行行为以
[syntax.md](../../noema/design/syntax.md) 与
[rendered-constructs.md](../../noema/design/rendered-constructs.md) 为准。
按“只学能改进 Noema 的部分”的原则：逐项判断它能否改进 Noema，
不能的就不搬。

## 来源（固定版本，克隆在被忽略的 `upstream/`）

| 项目 | 版本 | 学习范围 |
| --- | --- | --- |
| [obsidianmd/pele](https://github.com/obsidianmd/pele) | `3a2f075`（1.13.0，2026-10-30） | `src/mount.ts`、`src/zoom.ts`、`src/svg/theme.ts`、`src/text/math.ts`、`src/types.ts`、`AGENTS.md` |
| [Autumn-one/autumnmark](https://github.com/Autumn-one/autumnmark) | `860b080`（develop，2026-11-21） | `README.md`、`docs/examples/ridge-notes.md`、网站语法文档 |

AutumnMark 公开的 `develop` 是 MarkText（muya + Vue）加文档；README 介绍的四项扩展
（选项卡、折叠、代码块标题与行高亮、Base64 内嵌图片）在仓库源码中搜不到实现，
只能学它的语法与渐进回退原则，没有代码可读。

两个仓库都只读了源码：没有在本机安装依赖或运行它们的测试与基准，下文的性能数字
引自 Pele 官方说明，未经本地测量。

## 1. Mermaid：现状的细节问题

读 `src/diagram-render.ts` 与 `src/cm6/extensions/visual/widgets/fenced-code.ts`
得到的断点（阅读所得，尚无复现测试）：

1. **两次异步跳转，文字闪烁。** 组件先显示 `Loading diagram renderer...`，动态
   import `diagram-render.ts`；再等一次 idle 回调，显示 `Rendering diagram...`，
   import `mermaid`（约 1.6 MB gzip），最后 `await mermaid.render`。已有
   `minHeight` 占位防高度跳动，但文字仍会闪一下；虚拟化重新挂载时，命中缓存也要
   走 import。
2. **配色不跟主题。** 普通图固定 `theme: "default"`，注释说“图的颜色是内容”；
   深色主题下是一张白底卡片。Aaron mindmap 把一组颜色写死在 frontmatter 中
   （`aaronMindmapThemeSource`）。
3. **不适应窗宽。** SVG 以 `max-width:100%` 等比缩小：窄窗格里横向流程图的字变得
   很小，只能打开 lightbox 放大。
4. **出错即清空。** 渲染失败时 `div.textContent = err`，上一张好图被替换。实时
   预览（`cm-mermaid-block-preview`）边打字边重绘，句子写到一半就闪成错误文字，
   高度随之跳动。
5. **导出依赖页面。** `renderDiagramSVG` 的注释写明 Mermaid 需要真实浏览器排版，
   LaTeX 导出只能请求已打开的页面代画；还要专门切到 `htmlLabels:false`，因为
   `rsvg-convert` 会丢弃 `foreignObject`。
6. **每次 `mermaid.initialize` 修改全局配置**，交错的渲染之间共享这份全局状态。

## 2. Pele 能解决什么

Pele 是 Mermaid 12.1.0 语法的另一套实现，自带布局。对应上面各条：

| 问题 | Pele 的做法 | 出处 |
| --- | --- | --- |
| 1 异步闪烁 | `render()` 同步、不访问 DOM，同一输入与测量器输出相同 SVG；官方称每图 0.05–0.3 ms，约 230 kB gzip。缓存命中后即可同步挂载 | `AGENTS.md`、README |
| 2 主题 | 只用 CSS 变量：`--pele-bg/fg/muted/line/surface/surface-alt/border/accent/font/radius`，图表 `--pele-series-1..8`；换主题无需重绘 | `src/svg/theme.ts` `ROOT_STYLE` |
| 3 窗宽 | `mount()` 读容器宽度：横向图放不下时重新布局为纵向（`autoDirection`，默认 640px 以下），而非缩小文字；窄宽能改变布局的图类型会按 `maxWidth` 重排 | `src/mount.ts` `fit`、`src/types.ts` |
| 4 出错 | 解析失败保留上一张图，降低透明度，错误单行压在图底边、不占高度 | `src/mount.ts` `mark`、`OVER_STYLE` |
| 5 导出 | 渲染不需要 DOM，只需要文字测量器（`RenderOptions.measurer`），理论上服务器端可以直接出 SVG，不再请求页面 | `src/types.ts` |
| 6 全局状态 | 每次调用传入 `RenderOptions`，无全局 initialize | `src/core.ts` |
| 数学 | `math?: MathResolver` 把 `$$…$$` 交给宿主排版，返回 SVG 或 HTML 及尺寸——可接 Noema 的 KaTeX 与宏层 | `src/text/math.ts` |
| 安全 | 转义文本，过滤链接与样式（`RE_UNSAFE`），限制输入与输出大小；`mount` 的 `sanitize` 钩子可接现有 `sanitizeDiagramSvg` | `src/svg/theme.ts`、`src/mount.ts` |
| 缩放 | 内联缩放只给“被缩小才放得下”的图（`zoom: 'auto'`），放大只改变 viewBox，不改变周围布局 | `src/zoom.ts` `frame` |

**风险与代价**

- 布局与 Mermaid 不同：同一源码画出来不一样，已有笔记的图会变样。
- 不支持 JS click 回调与 ZenUML 等插件。
- Aaron mindmap（`marmind`/`markmind`）目前生成 Mermaid `mindmap` 源再交给
  Mermaid；换 Pele 后颜色改由 CSS 变量提供，`aaronMindmapThemeSource` 可删除。
  但 `{…}` 尺寸与位置属性及数学节点要逐条核对。
- 服务器端导出要有与页面一致的文字测量器；没有的话仍应由页面代画，不能凭空宣称
  导出一致。
- 依赖版本须走 `package.json` 锁定与 Noema 现有构建流程。

**结论建议**：Pele 正对第 1–4 条，值得替换 Mermaid，但要先在 Noema 自己的图例集上
做像素外的对照（类型覆盖、数学标签、mindmap、导出），再决定是否保留 Mermaid 作
回退。按 AGENT.md 的流程，先在 `rendered-constructs.md`（或新开 diagram 设计文档）
登记缺口与契约。

## 3. AutumnMark 可学的部分

| 项目 | AutumnMark | Noema 现状 | 判断 |
| --- | --- | --- | --- |
| 代码块标题 | 围栏首行 `title="ridge.py"`，显示在代码上方 | 只有语言徽章、复制、整块折叠（`fenced-code.ts`） | 值得加：写教程、论文附录有用；info string 中的属性在其他编辑器里被忽略，渐进回退成立 |
| 行高亮 | `{2-3}` | 无 | 值得加，同一套 info string 解析 |
| 预览高度 | `max-lines=4`，滚动或展开读完，复制得到全文 | 只有整块折叠为一行占位 | 可选：与现有折叠合并为“折到 N 行”，避免两套机制 |
| 内容选项卡 | `<!-- tabs -->` + `#### \| 标签` | 无 | 暂不建议：与 `#+begin layout` 和 Noema 的块环境语法重叠，需另行设计 |
| 折叠块 | `<!-- fold -->` 包住标题和正文 | 已有 `org-env-fold`（`render-html.ts`） | 不搬，已有 |
| Base64 内嵌图片 | 偏好设置切换，`data:` 地址 | 图片走附件目录 | 不搬：Noema 是 vault + Git，内嵌 Base64 会让 diff 和搜索索引膨胀 |
| 表格拖拽行高列宽 | v0.24.34：拖单元格边，双击恢复自动 | 表格已有编辑块与尺寸手柄 | 待用户确认是否有缺口 |

最值得学的是**原则**：每项扩展都写在 Markdown 已有结构上（标题、注释、info
string），换编辑器打开仍可读。这与 Noema 现有 `syntax.md` 的方向一致，新增代码块
属性时应保持。

## 4. 待用户决定

1. 是否以 Pele 替换 Mermaid（全部，或先替换实时编辑、导出暂留 Mermaid）。
2. 代码块 `title=` / `{行范围}` / `max-lines=` 是否加入 `syntax.md`。
3. “很多细节有问题”具体指哪些：上面只覆盖了图表与代码块，读码看不到的视觉问题
   （间距、字号、光标进出渲染块等）需要用户指出或给截图，再逐条登记成缺口。

## 5. 已落地（2026-11-22）

| 问题 | 结论与改动 | 提交 |
| --- | --- | --- |
| 补全弹窗上下选择抖动 | 当前项 `::after` 自动排到第三行使行高变化；弹窗按当前高度判断上下方，在窗口底部附近打字时来回翻。固定 `::after` 的位置；上下方按最大高度判断，且一旦选定就保持 | noema `920d903` |
| Dirvish 单击即打开、右键无菜单 | Dired 的 `follow-link` 把快速的 `mouse-1` 变成 `mouse-2`。单击只选中，双击打开，右键菜单包含剪切 | emacs `2f8e5aab` |
| Wiki 数据库“坏” | 文件完好；转写草稿复制了页面 id（QMM 有 6 个文件），https 链接被记为失效的 wiki 链接。诊断列出全部文件；外部链接不再算引用；转写脚本拒绝 vault 内的工作目录 | noema `68fddab`、README `ce717e3` |
| Agent 改动迫使人停笔 | 页面把另一写入者的保存变基到未保存输入之上，在原处应用（C74） | noema `48275ae` |
| Insert 中 Esc | 有选区时进入 Visual，再按一次 Esc 才取消 | noema `4d4614a` |
| Mermaid | 深色主题用 mermaid 的 dark 配色；实时预览保留上一张图并单行显示错误 | noema `cf51376` |
| TikZ | 光标外的源码修改也会触发渲染；颜色名和 `rgb()` 也做深色增亮；只读页面里没有 id 的块也能渲染 | noema `e51ad88`、`2ef094a` |
| 刷新按钮 | 清空公式、图表、TikZ、高亮缓存，重载宏、索引、补全和 snippet，并重画所有渲染块 | noema `bfa7fed` |
| 代码块 | info 字符串支持 `title="…"`、`{2-3}`；修复语言识别 | noema `b433512` |
| 服务器阅读器 | 加载和点击都不再露出标题、强调等源码 | noema `2ef094a` |

## 6. 待办与需要决定的事

1. **服务器阅读器的引用 `[?]`**：已放行 `noema:api:bibliography:document`（noema `d228f00`，用户授权）。
   块引用、embed 查询、attribute view 仍未放行，需要时另行授权。
2. **Pele 替换 Mermaid**：用户决定不做。
3. **移入 Go kernel 的候选**（按收益排序）：Wiki 索引构建（`server/lib/wiki-workspace.mjs`
   的扫描、解析、关系计算与写入 `wiki.db`；kernel 已经读这个库做搜索）、Agenda 索引、参考文献解析。
   原则：kernel 拥有派生数据，Node 只做传输。
4. **Agent 代码归位**：`site-lisp/noema/lisp/` 中约 2.1 万行 Elisp 有一半属于通用 agent 层
   （`noema-agent-worker`、`noema-sessions`、`noema-pi-router`、`noema-interaction-*`），
   按 AGENT.md 应在 `lisp/tools/agent/`；Research 只保留自己的 Run/DAG 适配。需逐文件列出调用方后
   分批迁移，每批跑 `make state-check`。

   归类结论（核对 `docs/design/agent-management.md` “实现位置”与依赖后）：只有 `noema-interaction-*`
   不依赖 Noema host，已迁到 `lisp/tools/agent/interaction*.el`、改名 `+agent-interaction-*`
   （emacs 与 noema 两侧提交）。`noema-sessions`、`noema-agent-worker`、`takeover`、`promote`
   依赖 Research 的会话记录与干预 API，设计文档本来就定为 Research 所有，留在 Noema。
   `noema-pi-*`、`noema-agent-inbox`、`noema-hy-memory` 依赖项目与 host，暂留。
   迁移中发现并修正：Noema 服务端到不存在的 `var/noema-interaction/magent/sessions` 找 Magent 历史，
   Emacs 实际写在 `var/noema/magent/sessions/`。

## 7. 外观对照：AutumnMark（muya）与 Noema 默认主题

读 `upstream/autumnmark/packages/muya/src/assets/styles/{index,blockSyntax}.css` 与 Noema
`src/styles/themes/theme-typora.css`、`src/styles/widgets.css` 得出的数值对照（2026-11-23，静态阅读，
未截图比对）。**更正（2026-11-24）**：编辑器默认启用视觉排版（`noema-visual-typography`），正文实际是
`typography.css` 的 23.2px / 1.5，`theme-typora.css` 里 20px / 1.46 的规则在编辑器中不生效；下表“Noema
现状”一列的正文与标题数值因此不准，实际结论与落地见第 8 节。Pele 只画图表，不涉及正文排版，不在此列。

| 项 | AutumnMark | Noema 现状 | 建议（待用户圈选） |
| --- | --- | --- | --- |
| 正文 | 16px 无衬线，行高 1.6 | 20px 衬线/楷体，行高 1.46 | 中文段落行高偏紧；可只把含 CJK 的正文行升到 1.6，西文保持 1.46 |
| 段间距 | 段落上下 0.5em | 由行高决定，无额外段距（CM6 行不能有外边距） | 不改：CM6 的行高必须精确，段距只能靠行内 padding |
| 标题阶梯 | h1 1.875 / h2 1.5 / h3 1.375em，粗体，行高 1.4，上下 1rem | h1 1.56em 小型大写居中加饰线，h2 1.16、h3 1.06em | h2、h3 与正文差距太小，层级不明显；可拉到 h2 1.3、h3 1.15em，保留现有字体与饰线 |
| 引用块 | 2px 细竖线，内缩 15px，文字 30px 内边距，文字为 50% 墨色 | 3px 竖线，0.8em 内边距，次要色 | 可改为 2px 细线、略增内边距，更轻 |
| 表格 | 单元格 6px 13px，细线全框，首行粗体 | 学术三线风格（上下 2px，表头 1.5px，内部细线） | 不改：三线表更合 Noema 的论文语境 |
| 代码块 | 90% 字号，复制按钮，可换行 | 0.875em，卡片化（标题条、细边、圆角），Fold/Copy | 已对齐（noema c957d57、本轮收紧结束行） |
| 折叠 | 标题左侧折叠按钮 | 标题折叠箭头 | 已有 |

结论：可学的主要是**标题层级拉开**、**CJK 正文行高**、**更轻的引用线**三项；表格和段距保持 Noema
现有做法。三项都只改 `theme-typora.css` 的数值，不动结构，等用户圈选后再改并截图验收。

## 8. 渲染性能、手感、排版与 wiki.db（2026-11-24）

### 导出渲染性能

用 vault 里最大的真实笔记 `GraphTensor.md`（113 KB、115 个公式）做 CPU profile：渲染 2.7 s 中有
2.1 s 在 `cleanEditorHTML`。三处改动（noema `3bdb21a`）：

| 热点 | 改法 |
| --- | --- |
| `protectMathML` 每个 `<math>` 一次 DOMPurify（每次重建文档） | 全部公式一次清洗，私用区字符分隔，每段仍包在自己的 `<div>` 里 |
| 四次全树 `querySelectorAll`（编辑器部件、图表源码、`[hidden]`、剥属性） | 一次树遍历，用 `classList` 和父节点判断，删掉的子树直接跳过 |
| `renderMarkdownHTML` 新建的临时根节点被深拷贝 | 调用方拥有的根原地处理 |

只把这次改动应用到 HEAD 上，`GraphTensor.md` 与 5 MB fixture 的 250 KB、500 KB 前缀输出逐字节一致。
下表是 happy-dom（Node 脚本）里的数字；**在 WebKit 里复测后（`bench-render-webkit.mjs`）单遍 JS 遍历反而更慢，
已改回原生选择器（noema `ce13f6f`），见下文“WebKit 实测”。**

| 输入 | 耗时 | 内存峰值 |
| --- | --- | --- |
| GraphTensor.md | 3.5 s → 2.5 s | — |
| 250 KB 前缀 | 3.3 s → 2.4 s | 2182 → 1544 MB |
| 500 KB 前缀 | 7.7 s → 4.9 s | 4101 → 2925 MB |

测量用的是 `scripts/render-html.mjs`：它在 Node 里用 happy-dom 跑同一套 `render-html.ts` 与
`cleanEditorHTML`，能在命令行精确复现耗时。这个脚本本身没有调用方（编辑器里没有“导出 HTML”，LaTeX 导出走
Pandoc，`make publish` 发布手写站点、不渲染笔记）；同一段代码的真实使用者是页面里的组件、幻灯片、链接预览与
Jupyter 输出页，它们在 WebKit 里运行，也因此受益。超过约 800 KB 的笔记会让这个脚本在 happy-dom 下耗尽
4 GB 堆（happy-dom 为 KaTeX 的每个 span 建很重的节点），只影响这个无人调用的脚本；按顶层块分段渲染在 250 KB
前缀上与整篇逐字节一致，可行，但为一个无人调用的入口加代码不值得，没有加入。

### WebKit 实测（渲染器真正运行的环境）

`bench-render-webkit.mjs` 用 Vite 在 Playwright 驱动的 WebKit 里加载某个源码树的 `render-html.ts`，测
GraphTensor.md 整篇（带选项，不走缓存）与 200 个段落（组件大小）的首次、再次渲染，各跑三次取典型值：

| 版本 | 整篇 | 200 段首次 | 200 段再次 |
| --- | --- | --- | --- |
| 改动前（`3bdb21a^`） | 618 ms | 372 ms | 351 ms |
| 只有单遍 JS 遍历的清理（`3bdb21a`） | 600 ms | 421 ms | 372 ms |
| 现在：原生选择器 + 批量公式清洗 + 原地处理 + 渲染缓存 | 579 ms | 365 ms | 0 ms |

happy-dom 里慢的是选择器引擎与 DOMPurify，WebKit 里两者是原生实现，JS 遍历在小片段上输给原生
`querySelectorAll`；以 WebKit 为准改回。Node 脚本因此为 3.1 s（原 3.5 s，单遍遍历时 2.5 s）。

### 手感：组件渲染缓存

表格单元格、环境块、折叠块、评论在每次重建时重新渲染周围没变的 Markdown。无选项的
`renderMarkdownHTML` / `renderMarkdownInlineHTML` 现在进有界 LRU（8 MB HTML，超过 2 万字符的整篇不进），
key 含源文本、宏版本和数学缓存代数（刷新按钮与宏变化都会重新渲染），借自 NocoDB 表格对单元格
Markdown 的缓存（noema `30fbb0c`）。happy-dom 下 50 个带公式的单元格冷渲染 103 ms、命中 0.1 ms。

### 排版

视觉排版里正文从 20px 放大到 23.2px 时，标题留在旧的 20px 体系：h3（24px）几乎等于正文，h4–h6
（21/19/18px）比正文还小。改动（noema `9b8684a`）：

| 项 | 改前 | 改后 |
| --- | --- | --- |
| 正文行高 | 1.5（34.8px） | 1.6（37.1px），统一值，不做“仅中文行”：那样在英文行里打出第一个汉字会让整行跳高 |
| h2 / h3 / h4 / h5 / h6 | 29 / 24 / 21 / 19 / 18px | 正文的 1.35 / 1.15 / 1.05 / 1.0 / 0.95 倍（31.3 / 26.7 / 24.4 / 23.2 / 22.0px）；h1 35px 不变 |
| 引用块 | 3px 竖线、0.8em 缩进 | 2px 细线、1em 缩进（导出与实时一致） |

发布页（`theme-typora.css`）的 h2–h4 取同样比例。引用块的默认规则在 `widgets.css`：主题样式先于打包
样式加载、选择器同样具体，只改主题时实际页面仍是 3px / 0.8em，所以默认规则本身也改为细线（noema `538ac64`）。

验证：

- **真实页面**：`make build-web`（`.nvmrc` 的 Node 26.5.0）后，由真实 Noema host 提供的编辑页上读计算样式：
  正文 23.2px / 37.12px，h2 31.32px，h3 26.68px，h4 24.36px，引用 2px 线、23.2px 缩进；页面的
  `__noemaRendererBuild` 与新构建一致。
- **WebKit 滚动检查**（`npm run check:scroll`，`NOEMA_PLAYWRIGHT_MODULE` 指向离线安装的 playwright 1.65.1，
  浏览器用本机缓存的 WebKit 2370）：

  | 版本 | 违规 |
  | --- | --- |
  | `5cf3b6c`：本轮之前最后一个一致的提交（HEAD 页面因缺符号无法加载） | 12 |
  | 当前工作区，去掉本次排版提交 | 21 |
  | 当前工作区，带上本次排版提交 | 20 |

  排版没有引入违规。

  **已修复（noema `560e1e1`）**：多数违规是“点击正文后光标没落在被点的字上”。`repro-kp-click.mjs` 记录
  每笔改选区的事务后定位到根因：指针手势期间 `selection.ts` 暂停了 CM6 写 DOM 选区并设 `user-select: none`，
  WebKit 把原生选区停在内容元素开头；CM6 观察器在点击期间的一次 flush 里把它读回，光标被改回 0 或旧光标处
  （三层坐标映射本身都正确；最优换行重排不是原因，冻结它的尝试已撤回）。手势期间读也暂停后，复现脚本两组
  场景各 5/5 正确（之前 0/5 与 3/5），滚动检查从 20 处降到 6 处：任务框、分隔线、代码块、多栏、行内评论的
  “点击正文离开”全部通过。剩下的：左右环绕表格（本轮之前就有，浮动布局离开时高度变化），图片与行内评论各
  一类位移（本轮之前没有，去掉排版提交时也存在，随工作区里另一任务未提交的改动出现）。设计文档
  `scroll-ownership.md` 已记录。

  剩下 6 处的归因（逐项临时撤掉本轮改动后只跑 `ONLY="image,inline comment"`，各两次）：撤掉排版、
  撤掉渲染缓存（让 `cachedRender` 每次直接渲染）、换回本轮之前的 `export-html.ts`，图片“点击正文离开”与
  行内评论“未聚焦点击”的失败都不变；点击修复之前它们也已存在。`5cf3b6c` 上没有这两类，所以来自其后另一
  任务的改动（已提交的部分或工作区里未提交的部分；HEAD 本身因缺符号无法运行，不能再细分）。环绕表格两处见
  `scroll-ownership.md` 的已知差距。

**部署说明**：`dist/` 是从工作区构建的，不是从 HEAD：HEAD 本身过不了 `tsc`，已提交的
`src/cm6/editor-cm6.ts` 引用了只存在于另一任务未提交的 `commands/index.ts`、`paste-context.ts` 中的符号
（`handleTablePipeInput`、`tableGridPasteTransactionSpec` 等），只有工作区是一致的。Go kernel 从 HEAD 构建
（`eddefe9`），不含其它任务未提交的 Go 改动。

### wiki.db 结构

真实库 325 页 32 MB。读表结构、各表体量（`dbstat`）、增量写入与搜索的执行计划后：

| 发现 | 处理 |
| --- | --- |
| `page_bigrams(page_key)` 二级索引是整表副本（4.7 MB）；kernel 只走 `(bigram, page_key)` 主键；Node 服务器阅读器按页删除有无索引 19 vs 21 ms，建库插入 70 vs 12 ms | 删除（noema `652562b`）。新建库 32 → 27 MB，各表内容一致 |
| 搜索的孤立页 / `link:` 过滤对每页做 `links.source_key` 关联子查询，该列无索引，每页扫全部链接 | 加 `links(source_key)`（noema `eddefe9`）：10 ms → 0.5 ms |
| 增量写入删除 FTS 行时按全部列定位，FTS 只能全表扫 | 实测单页 1–12 ms，当前规模可忽略，不改 |
| 两张 FTS 表各存一份正文（3.8 MB × 2） | 不改：外部内容表需要 FTS5 的 `'delete'` 命令与对齐的 rowid，增量写入按 rowid 通用删除，改动面大 |
| 引用选择器读全部正文解析标题 | 已按索引代数缓存（`catalog.anchorsOf`），不改 |

两项索引改动都不升 schema 版本：现有库在下一次全量重建（每周自愈等）时得到新结构，不会被迫重建。

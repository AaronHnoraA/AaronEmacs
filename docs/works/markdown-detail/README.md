# Noema Markdown 细节：Pele 与 AutumnMark 学习笔记

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

1. **服务器阅读器的引用 `[?]`**：阅读模式 API 白名单（`server/lib/server-policy.mjs`）缺少
   `noema:api:bibliography:document`；块引用、embed 查询、attribute view 同样缺失。这些处理函数都把
   解析限制在笔记根目录内。放宽白名单需要用户同意。
2. **Pele 替换 Mermaid**：沙箱连不上 npm 源；需要用户本机执行 `npm install pele@1.13.0 --save-exact`，
   之后在 Noema 自己的图例集上对照。
3. **移入 Go kernel 的候选**（按收益排序）：Wiki 索引构建（`server/lib/wiki-workspace.mjs`
   的扫描、解析、关系计算与写入 `wiki.db`；kernel 已经读这个库做搜索）、Agenda 索引、参考文献解析。
   原则：kernel 拥有派生数据，Node 只做传输。
4. **Agent 代码归位**：`site-lisp/noema/lisp/` 中约 2.1 万行 Elisp 有一半属于通用 agent 层
   （`noema-agent-worker`、`noema-sessions`、`noema-pi-router`、`noema-interaction-*`），
   按 AGENT.md 应在 `lisp/tools/agent/`；Research 只保留自己的 Run/DAG 适配。需逐文件列出调用方后
   分批迁移，每批跑 `make state-check`。

   初步归类（按文件首行说明，尚未逐一核对调用方）：

   | 去 `lisp/tools/agent/` | 留在 Noema（Research／项目） | 待核对 |
   | --- | --- | --- |
   | `noema-sessions`（1023）、`noema-agent-takeover`（192）、`noema-agent-promote`（169）、`noema-agent-text`（43）、`noema-interaction-*`（约 4300，CLI 适配、gptel/Magent 引擎） | `noema-agent-acp`（Research 关联适配，AGENT.md 已指定）、`noema-orchestration`、`noema-research*`、`noema-exploration`、`noema-findings` | `noema-agent-worker`（2190，文档执行 worker）、`noema-pi-router`/`noema-pi-sessions`（Pi 是 Emacs 全局管家，但会话记录在 Noema）、`noema-agent-inbox`、`noema-hy-memory`、`noema-agent-entry` |

   迁移改名要同步所有调用方（含 `etc/config-store.el`、Raycast、Noema 服务端），不留兼容别名；
   每批先在 `docs/design/agent-management.md` 写明归属，再按 `make state-baseline` → `make state-check` 验证。


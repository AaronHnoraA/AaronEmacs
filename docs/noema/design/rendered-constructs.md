# 渲染组件（Rendered Constructs）统一架构

编辑器里所有"画成别的样子的 Markdown 源码"——公式、图片、脚注、块引用、任务框、表格、
图表、org 环境块、`@@` 命令、HTML 块——都通过同一层：
`src/cm6/extensions/visual/constructs/`。本文是这层的规范，开发者与 agent 新增或修改
任何渲染组件前都先读它。规则由 `tests/architecture-constructs.test.ts` 强制执行。

## 为什么只有一层

每个渲染组件都要回答同样的三个问题：

1. **何时显示源码**：光标/选区在哪里时，组件让位给可编辑的源码？
2. **画在哪里**：Vim、方向键、选区需要知道哪些源码范围被画成了一个整体。
3. **模式**：Vim 的 Normal/Visual 与 Insert 下行为是否不同？

过去每个组件各自回答，结果是 7 种互相矛盾的显示规则、3 套 Vim 的私有扫描（屏幕外的
组件 Vim 看不见）、模式存在 DOM 上导致组件慢一拍更新。`j`/`k` 在公式附近卡住、表格与
正文手感不同，都源于此。现在三个问题各有唯一答案，组件只做选择。

源码始终是唯一文档；组件只是源码的视图（见 `CLAUDE.md` Invariants）。

## 词汇

| 概念 | 含义 | 所在 |
|---|---|---|
| **construct** | 被 widget 替换绘制的一段源码 | — |
| **object** 角色 | 读者看到的"东西"：公式、图片、引用、图表、emoji。Vim 把它当一个字符跨过；Normal/Visual 下保持渲染；删除时整体删除 | `reveal.ts` |
| **structure** 角色 | 读者编辑的文字与语法：表格、块标题/分隔、HR、TOC、`@@` 命令、行内标记。任何模式下光标进入都显示源码 | `reveal.ts` |
| **reveal policy** | 选区何时打开源码，见下表 | `reveal.ts` |
| **pinning** | Vim 在 Normal/Visual 时让 object 保持渲染。是编辑器状态，不是 DOM 属性 | `mode.ts` |
| **literal ranges** | 代码与公式（含跨行的行内公式）内部不识别任何组件（`[^1]`、`[[链接]]`、`@@命令` 写在公式里就是文字）。所有家族用同一个答案：`literalRangesIn` / `mathRangesIn` / `literalRangesOnLine`；只有公式自身（`math.ts`）与只放块级组件的 `block-extras` 直接用底层扫描 | `literal-ranges.ts` |
| **registry** | 唯一的"哪里画着什么"。所有在源码上绘制 widget 的组件都在此登记；从状态计算的来源屏幕外也正确，视口插件经 `drawnConstructSource` 直接读其绘制结果 | `registry.ts` |
| **embedded surface** | 组件内部用自己的编辑器编辑文字的元素（公式 MathLive、图表灯箱） | `src/cm6/embedded-surface.ts` |
| **visual row** | 读者看到的折行行；仍被绘制的块整体算一行。Vim 的 `j`/`k` 与编辑器的 ↑/↓ 共用 | `src/cm6/screen-rows.ts` |

### Reveal policy

| policy | 光标（空选区） | 范围选区 | 用于 |
|---|---|---|---|
| `edge` | 在内部或贴着两端 | 一端落在内部 | 脚注、块引用、块 ID、任务框、表格 |
| `interior` | 严格在内部 | 一端落在内部 | 公式、行内标记 |
| `caret-edge` | 在内部或贴着两端 | 从不 | 图片、`@@` 命令、块结构、HTML 块 |
| `caret-interior` | 严格在内部 | 从不 | 图表、front matter |
| `never` | 从不（由自己的开关打开） | 从不 | 代码折叠 |

一个范围选区**跨过**组件时永远保持渲染：全选或拖选不会把途经的所有 widget 打开成源码、
也不会随文档规模变慢。组件可以有比自身更大的 reveal region（任务框：整行）。

## 组件一览

| 组件 | 模块 | 角色 | policy | 进入方式 |
|---|---|---|---|---|
| 脚注、块引用、块 ID | `widgets/footnotes.ts`、`block-ref.ts`、`block-anchor.ts` | object | `edge` | `inlineConstruct` |
| 任务框 | `widgets/task-list.ts` | object | `edge`（整行） | `inlineConstruct` |
| `@@lean4`、`@@note-code` | `widgets/lean-placeholder.ts`、`note-code.ts` | structure | `edge` | `inlineConstruct` |
| 图片 / html 嵌入 | `widgets/image.ts` | object | `caret-edge` | 自有插件 + `constructSources` |
| 公式 | `widgets/math.ts` | object | `interior`，由公式编辑会话打开 | 自有字段 + `constructSources`（`revealed` 覆盖） |
| 代码折叠、图表 | `widgets/fenced-code.ts` | object | `never` / `caret-interior` | 自有字段 + `constructSources` |
| 表格 | `live-preview.ts` | structure（块） | `edge`（文档开头的 0 偏移不算） | 自有字段 + `constructSources` |
| HTML 块、行内标记、emoji | `live-preview.ts` | structure | `caret-edge` / `interior` | `selectionOpensSource` / `rangeRevealsSource` |
| org 环境、TOC、HR、语义标题、front matter | `widgets/block-extras.ts` | structure（TikZ 为 object） | `caret-edge`（front matter `caret-interior`） | `selectionOpensSource` |
| 进度与指标环境 | `widgets/block-extras.ts` + `render-html.ts` | structure | `caret-edge` | 正文显示与 HTML 共用渲染；标题、侧线、结束标记沿用 org-env。指标环境展示卡片时接管表格范围，进入表格后恢复表格编辑逻辑 |
| `ediff` 待审 patch | `widgets/ediff.ts`，由 `block-extras.ts` 放置 | structure | `caret-interior` | `selectionOpensSource`，进入源码可改 patch |
| `@@` 行内命令 | `widgets/inline-commands.ts` | structure | `caret-edge` | `selectionOpensSource` |
| wiki 链接标题 | `roam-link-status.ts` | structure | `edge`（任一选区） | `anySelectionRevealsSource` |

| 断行与间距 | `optimal-linebreak.ts` | structure | 由排版计算决定 | `drawnConstructSource` |

没有例外：凡是绘制在源码上的 widget 都是组件，都在登记表中。

**登记方式**：`inlineConstruct` 的家族自动登记；有自有状态的家族用
`constructSources.of({ kind, at })` 从状态回答；绘制本身已体现显示规则的家族（`block-extras`、
`inline-commands`、HTML 块、emoji/行内 HTML、wiki 标题、断行间距）用
`drawnConstructSource(kind, role, decorations)` 直接读取自己的装饰，于是登记表与屏幕按构造一致。

## 新增一个渲染组件

先判断两件事：它是 **object** 还是 **structure**？选区何时应该打开它（选上表的一个
policy；没有合适的就扩展 `reveal.ts` 并在本文登记，而不是在组件里手写比较）。

**行内组件**（一行内、从该行文本即可匹配）——用工厂，只写匹配与绘制：

```ts
export const myExtension = inlineConstruct({
  kind: "my-thing",
  reveal: "edge",
  role: "object",            // 默认 object；@@ 命令等语法用 "structure"
  maxPerViewport: 2_000,
  *match(line, state) {
    if (!line.text.includes("<<")) return;       // 廉价预检
    for (const m of line.text.matchAll(MY_RE)) {
      const from = line.from + m.index!;
      yield { from, to: from + m[0].length, widget: () => new MyWidget(m[1]!) };
    }
  },
});
```

工厂负责：跳过代码与公式、reveal 与 pinning、打字合并期间只映射、拖选中的视口刷新与松开后的选区结算、
可视模式切换时的缓存、在 registry 中登记。把扩展加入
`extensions/visual/index.ts` 的 `createVisualMarkdownExtensions()`。

**块级或有自己状态的组件**（跨行、需要增量索引、有编辑会话）：

1. 用自己的 `StateField`/插件维护范围索引与装饰（保持现有的增量与视口预算）。
2. 每个"是否显示源码"的判断都调用 `selectionOpensSource(state, from, to, policy, role)`；
   重建判断用的 key 与绘制必须调用**同一个**判断函数（参考 `tableOpened`、
   `diagramOpened`）。
3. object 组件在 `update` 中对 `transactionChangesPinning(tr)` 做出反应（模式切换）。
4. 用 `constructSources.of({ kind, at(state, pos) })` 登记，`at` 从状态返回包含 `pos`
   的组件；若源码由会话而不是选区打开，提供 `revealed(view, construct)`。
5. 增加高度的 widget 继承 `MeasuredWidget`（见 `CLAUDE.md` Widget Rules）。

**不要**：比较 `sel.empty && sel.from ...`；读 `dataset.vimMode`（用
`renderedConstructsPinned(state)`）；在 `pages/vim-lite.ts` 的 `publishMode` 之外设置它；
在 `src/cm6/embedded-surface.ts` 之外拼写 `data-noema-vim`；在视口家族里自己调用
`isCoalescedVisualTyping` 或持久缓存 helper（交给 `constructViewPlugin`）；给装饰加私有
标记让 Vim 识别；在 Vim 里扫描 Markdown 找组件。守卫测试会拒绝这些写法。

**视口**：点击组件不移动视图（导航除外）。组件内聚焦用 `{ preventScroll: true }`，
显示位置用事务的 `scrollIntoView`，不用 `element.scrollIntoView` 或 `scrollTop`
（[scroll-ownership.md](scroll-ownership.md)）。新组件在 `scripts/check-scroll-invariance.mjs`
的样例里加一段，标明点击意图（`opens`、`edits`、`navigates`），再跑 `npm run check:scroll`。

`ediff` 复用已有 org-env 范围索引与块级装饰；每个环境直接放在要修改的
原文行处。审阅条继承正文的字号与主题，默认 Preview 经共享 Markdown renderer
比较 Current / Proposed 两个局部片段，窄面板自动上下排列；它不是整篇重排预览。
Diff 切换到带独立旧／新行号的源码差异。切换只改变视图，不写 Markdown。
源码差异四行以内全部显示；更长的块预览新旧各一行（单侧改动前两行），
Details 展开其余改动和上下文，也解除排版预览的高度限制。
接受、拒绝、留言与编辑源码在底部直接可见。已保存留言直接显示；有留言或拒绝
时，Send feedback 向宿主报告精确提案快照，由宿主复用保存与发送选区路径，
不在 widget 内另建 Agent 客户端。接受只把该块替换成新行，拒绝和留言只写回
该 environment 的元数据，每次操作都是一个可撤销事务。
光标进入块内显示原始局部 diff；块外保持卡片。
HTML、LaTeX 和 Wiki 索引把待审块还原为旧行。Tools 的汇总弹窗按本文档中的
位置列出所有提案，并提供跳转和同样的审阅动作。

## 左右与词移动：光标从不被隐藏

插入模式的 ←/→ 与 Vim 的 `h`/`l`/`w`/`b`/`e` 规则不同，但都由本层决定：插入模式下光标碰到组件
时按其 reveal policy 打开（脚注、引用贴边即打开；公式到达边界后进入编辑）；Vim 的 Normal/Visual
模式下 object 保持渲染，作为一个字符跨过。两者共享一条不变量：**光标从不停在仍被绘制的组件内部**
——要么组件为它打开，要么光标落在组件边缘。守卫按屏幕上实际绘制的 widget 判断（而不是按
registry 的看法），用覆盖全部家族的示例笔记扫过 ←/→/↑/↓ 与 `l h w b e j k`。

## 删除：object 整体删除

object 是一个东西，不会被删掉半个。插入模式下，在 object 之后按 Backspace（之前按 Delete）先选中
整个 object，再按一次才删除——图片、公式（行内与块）、图表、emoji、脚注、块引用、块锚点、任务框
一律如此；Vim 的 `x`/`X` 本来就把 object 当一个字符整体删除。从前只有图片这样做，其他 object
被逐字删掉源码，往往还在绘制状态下就坏成了文字（`\(x^2`、`smile:`）。判断来自 registry 的
`objectAtEdge(view, pos, side)`：无论 object 正在绘制还是因光标贴边而打开，都算。structure
（表格、`@@` 命令、块标题、行内标记）是文字，照常逐字删除。emoji 由行内标记家族绘制，但它是一个
字形，因此登记为 object（`drawnConstructSource` 的 role 可以按 widget 决定）。

## 从组件写文档：一个入口

组件自己的控件写回文档——勾选任务框、改代码块语言、接受/保留 AI 建议、布局菜单、表格工具与
就地编辑写回、离开文末表格时补的空行——一律经 `dispatchConstructEdit(view, spec, kind?)`
（`constructs/edit.ts`）：每个动作是独立的一步撤销，标记为 `construct[.kind]`。从前这些写入有的
不加标记（两次快速勾选被 CodeMirror 历史合成一步，撤销一次看不出变化；紧接着打字的勾选和打字
合成一步），有的标成 `input`（被当成连续打字，重绘被推迟）。公式编辑会话（`math.ts`）自行管理
历史分组，是唯一的具名例外。

## 块组件旁的输入

WebKit 可能把打字或输入法组字的文字放进块组件（`#+begin proof` 的标签、标题、图）旁边的错误 DOM
节点：把组件后一行压平进前一行，CM6 读成“用文字替换换行”。`src/cm6/input-line-break-guard.ts`
规定：光标为空时，浏览器输入的不含换行的文字永不删除换行——文字落到光标处（浏览器放错位置时）或
保留的换行之后（光标在被替换范围内时）；纯删除（行首退格）与跨行选区替换不受影响。停放的编辑器被
按键唤醒时经 `view.focus()` 同步写入 DOM 光标（`focus-quiescence.ts`），否则输入法会在
CodeMirror 10 ms 后的聚焦处理之前，从 WebKit 默认的行首开始组字。测试：
`tests/input-line-break-guard.test.ts`、`tests/focus-quiescence.test.ts`“waking a parked editor”。

输入法直接写页面 DOM，WebKit 有时写进组件旁的错误节点，CM6 自己的记录却认为该行是新的，于是显示出重复或缺失的字
（文档本身正确），且 CM6 复用行 DOM、刷新也不修。`src/cm6/composition-repair.ts` 规定：显示的文字永远等于文档——
每次组字结束（以及 Refresh）后，可见行中属于文档文字（组件之外）的每个文本节点与 CM6 映射到的文档位置比对，
不符的行用“以相同文字替换”重绘，文档与选区不变；同一行修一次后仍不符只报告一次，不循环。测试：
`tests/composition-repair.test.ts`。

## 何时重建：一个生命周期

由 ViewPlugin 绘制的家族（行内组件工厂、图片、`@@` 命令、wiki 标题、代码块）都用
`constructs/view-plugin.ts` 的 `constructViewPlugin`：输入法组字时的选区移动不重建；打字合并时
只映射（家族可映射自己的状态，或拒绝）；鼠标拖选时纯选区变化到松开后结算，边缘滚动和解析刷新照常绘制新视口，并用按下时的选区决定源码显示；文档、视口、刷新变化时重建；
选区变化只在"打开了哪些组件"（`revealKey`）改变或 Vim 固定状态切换时重建；插件重建之间经持久
缓存保留。家族只提供 `build`，以及可选的 `revealKey`、`extra`（自有状态）、`mapDuringTyping`、
`invalidates`、`beforeUpdate`、`setup`、`provide`。

三个模块保留自己的生命周期，各有理由：`live-preview`（视口增量扫描与逐行 CJK 缓存）、`math`
（公式编辑会话）、以及只做测量的 `block-extras` 轨道与 `optimal-linebreak`。

## 增量重绘：一个修补器

块级家族在整篇文档上保存装饰，每个事务只重绘变化的部分，全部经由
`constructs/decoration-patch.ts`：

- `openedKey` / `spansFromKey`：选区打开了哪些组件；键不变即无事可做。
- `rebuildForRevealChange(current, oldKey, newKey, build)`：只重绘打开或关闭的组件。
- `rebuildDecorations(current, ranges, build)`：编辑、视口刷新涉及的范围。
- `replaceDecorations(current, ranges, add)`：用别处建好的装饰替换范围。

家族只提供"如何构建一个范围"。

## 组件内部的 Markdown

widget 内部显示的 Markdown（表格单元格、todo 标签、旁注卡片、org 环境正文、嵌入查询）一律经由
`src/render-html.ts` 渲染：块级用 `renderMarkdownHTML`，行内用 `renderInlineMarkdownHTML`（去掉
外层段落，`<img>` 走共享的图片与资源解析）。同一段 Markdown 在哪个组件里都长得一样；
不要引入别的 Markdown 库或另写包装。

## 嵌入式编辑面

组件内部若用自己的编辑器编辑文字（公式 MathLive、图表灯箱、meta 摘要编辑器），在根元素上
调用 `markEmbeddedSurface(el)`。之后 Vim 不解释其中的
按键、编辑器不在指针事件上抢回焦点、滚动稳定器与 Emacs 按键转发都放行。需要区分
"嵌入面"与"文档"的代码一律使用 `EMBEDDED_SURFACE_SELECTOR` /
`isInEmbeddedSurface`，不要手写属性选择器。

**先问能否不要嵌入面。** 能编辑为文档文字的内容就编辑为文档文字：嵌入面意味着另一套
光标、按键、补全、撤销与输入法处理。表格单元格及格内公式都由主 CM6 编辑。

## 表格：统一渲染模型与同一编辑面

表格与正文共用同一套 CM6 编辑引擎、撤销历史、Vim、输入法、补全、公式与样式体系；
**绝不在单元格内嵌套第二套编辑器**，也不在预览态与编辑态之间维护两套割裂的模型或样式。

- **单一模型与坐标**：GFM 表格识别（含无外侧管道符与短分隔行）、单元格源码范围、
  按内容加权的列宽比例（`computeTableColumnPercentages`）、结构变更与多格粘贴由
  `table-source-cells.ts` 与 `table-model.ts` 统一提供，预览与编辑共用。
- **零跳变统一渲染**：
  - 未编辑时，`TableWidget`（继承 `MeasuredWidget`）以 `<colgroup>` 渲染精确比例的 `<table>`，
    阅读态隐藏顶部浮层工具栏（悬停或聚焦浮现，不占位、不移动视口），边缘 `+` 条置于表框外侧，
    点击行/列拖拽柄打开右键同款上下文菜单，拖动重排行/列时显示插入指示线。
  - 进入单元格后，`TableSourceToolbarWidget`、`tableSourceGridDecos` 与 `TableSourceEdgeWidget`
    复用同一套 CSS 变量（边框、内边距、表头排版、列宽比例）与同一套工具栏/边缘添加控件：
    非激活行按相同列宽比例用 CSS Grid 渲染 `TableSourceCellWidget`；**激活行以原生 `display: block`
    承载当前单元格**（左右 `padding` 精确限定在激活列内，同行其余非激活格绝对定位到对应列并同步行高），
    因此当前格内的 `<br>`、多个行内公式、emoji、脚注、链接都能以原生 CM6 流式排版，不再因行内
    widget 拆分 mark 而坍塌为裸源码。
  - 当前格的 mark 仍包住其右侧隐藏的 `|`（确保 WebKit 格尾光标与 IME 候选窗定位稳定），
    打字与组字期间与正文共用 visual typing burst 和 `viewport-refresh.ts`。
  - 点击预览格或非激活格时，经由 `pointer-position.ts`（`renderedTextOffsetAtPoint`）映射回单元格源码的
    精确字符位置；`Cmd/Ctrl-Click` 链接直接打开链接；点击格内公式走共享的 `rendered-formula-click.ts`。
  - 预览态与编辑态共用 `table-context-menu.ts` 提供的右键菜单（插删行列、复制行列、移动行列、对齐、排序、
    清空格、格式化源码、复制 CSV、删除整表）。
  - 布局属性行 `{...}` 与分隔行 `| --- |` 平时收起，仅在光标进入该行时显示源码。

| 按键 | 行为 |
|---|---|
| Tab / Shift-Tab | 下/上一个单元格并选中其文字（不重写已有行空格；最后一格之后追加一行） |
| Enter | 下一行同一列（不重写已有行空格）；在空的末行上离开表格；单行 `\| a \| b \|` 末尾回车展开为表格 |
| ↑ / ↓ | 折行或含 `<br>` 的单元格内先按 `screen-rows.ts` 视觉行上下移动；到首/末视觉行后跨到相邻行同一列并保持水平偏移；越过首/末行离开表格（文末自动新开一行） |
| ← / → | 在单元格文字边缘进入相邻单元格 |
| Backspace / Delete | 在单元格开头/末尾移到上/下一格边缘，从不删除 `\|` |
| Home / End / Mod-← / Mod-→ / Mod-Backspace | 优先限定在当前单元格文字范围内 |
| Mod-A | 三段式扩选：当前单元格文字 → 整张表格 → 整篇文档 |
| Shift-Enter | 单元格内换行 `<br>` |
| Mod-Enter | 在下方插入一行，光标进入其第一格 |
| 键入 `\|` | 在已有表格的单元格内自动转义为 `\\\|`，防止意外劈裂列 |
| 粘贴表格/TSV | 在单元格内粘贴 GFM 子表或制表符矩阵时按目标格展开覆盖（必要时自动扩充行列） |

渲染状态下：从一格拖到另一格是框选；Shift-点击选中单元格，方向键在选中的格子间移动，Enter
进入主编辑器中的对应单元格。工具栏、右键菜单或边缘条插入行列后，光标进入新单元格。

## 图元定位、环绕与自适应尺寸（Figure Layout）

所有视觉图元——图片（含视频、draw.io、HTML 嵌入）、TikZ、Mermaid/Mindmap 图表与表格——共用同一套源码属性（`shared/layout-attrs.mjs`：`{align, wrap, width, height}`）与唯一的交互控制器（`src/cm6/figure-layout-menu.ts`）：

- **自适应百分比优先**：交互式宽度缩放（拖拽与方向键 ±5%）一律以正文栏宽为基准换算并写回相对百分比 `{width=62%}`（拖拽过程带实时百分比徽章并轻吸附 `25/33/50/67/75/100%`），确保窄分屏、移动端阅读器、发布 HTML 与 LaTeX 导出（`\textwidth`）全部自适应；已有笔记中的历史 `px` 属性继续兼容解析，且外层始终受 `max-width: 100%` 约束不溢出。
- **单一容器测量（禁止双重百分比）**：外层 figure 在带 `noema-layout-has-width` 时负责按栏宽解析一次 `width`，内部媒体（`img/video/svg/iframe`）铺满 `width: 100%`，避免 `fit-content` 内百分比失效或 `wrap` 浮动下宽度被双重压缩/锁死在 45%。窄视口（`≤ 560px`）下 `wrap` 浮动自动退化为居中块级堆叠。
- **统一浮动锚点**：所有块级环绕图元（TikZ、Mermaid/Mindmap、预览表格）一律使用 `MeasuredWidget.floatedBlock = true`（零高 `.cm-float-anchor`），保证 CM6 高度图记为 `0` 且拖拽手柄始终锚定在浮动本体底部；编辑态表格行保持对齐块流而不逐行浮动。
- **按能力挂载控件**：高度拖拽条仅挂载于固定视口类图元（HTML `iframe` 嵌入、Mermaid/Mindmap 图表），不挂载于等比图画（图片、TikZ）或流式文本表格；表格将图元排版收拢进唯一顶栏的 `◫` 浮层与右键菜单，不再叠加第二条悬浮条，编辑态宽度手柄位于底部边缘条并实时同步所有表格行。

## 多栏排版（`#+begin columns` + `#+col`）

多栏排版由 `shared/columns-layout.mjs` 统一定义语法解析、属性序列化、CSS Grid 轨道/分隔线位置与 LaTeX 导出（Go 端在 `kernel/noema/latex/preprocess.go` 保持字节一致镜像）：

- **表达完整 Markdown 分栏**：默认模式以顶层 `#+col [{align=...}]` 分隔各栏（自动跳过围栏代码、展示公式、HTML 注释与嵌套 `#+begin ... #+end` 环境），每栏可自由包含标题、多段落、列表、公式、代码块、图片、表格乃至嵌套 org 环境块；报纸式流式文本分栏使用 `{mode=flow cols=2..4}`。
- **单一编辑面所见即所得（语法与手感分离）**：
  - 预览态下 `ColumnsWidget`（继承 `MeasuredWidget`，根节点零纵向 `margin`）使用 CSS Grid 渲染各栏，通过共享 `renderMarkdownHTML`（携带 `assetResolver` 与 `noteFile`）及 `enhanceRenderedMarkdown` 渲染每栏内容；栏间分隔线使用 `::before` + `columnDividerLeftCss` 精确落在 CSS Grid 缝隙正中，拖拽缩放时显示实时百分比徽章并提交严格相加为 100 的整数 `widths=60-40`。
  - **就地并排编辑（类似表格单一编辑面）**：直接点击任意一栏后，**不折叠为竖排、也不暴露 `#+begin columns` / `#+col` / `#+end columns` 源码**——激活栏以原生 CM6 `.cm-line`（通过 `--cm-col-left`/`--cm-col-right` 与 CSS 兄弟选择器对齐到对应栏宽）直接编辑，无嵌套编辑器；其余兄弟栏由 `ColumnsActiveTopWidget` 中的 `.cm-columns-sibling-stage` 实时渲染在原位，并由 `ColumnsStageSyncPlugin` 同步最大栏高与底部占位（`ColumnsActiveBottomWidget`），点击任意兄弟栏即可原地切换焦点；编辑中仍可直接使用右上角工具栏与栏间拖拽柄；`←`/`→` 在栏首/栏尾平滑跨栏或离开，`↑`/`↓` 在首/末行离开分栏，栏边界上的 `Backspace`/`Delete` 受保护不会误删指令行。
  - **按需展开源码**：仅当用户手动点击工具栏 `Source`（或光标直接落到 `#+begin`/`#+col`/`#+end` 指令行）时，经 `columnsRawSourceField` 切入源码/横幅视图（`ColumnsOpenBannerWidget` / `ColumnsMarkerBannerWidget` / `ColumnsCloseBannerWidget`，并附 `Visual` / `Done` 快捷返回）。
- **全链路导出一致**：HTML/发布阅读器与编辑器共用 `.noema-columns` 样式契约；LaTeX/PDF 导出将显式多栏转为紧邻无空行的 `minipage`（继承栏宽比例、垂直对齐与单栏对齐），流式多栏转为 `multicols` 并按需自动注入 `\RequirePackage{multicol}`。

## 上下移动：一个行模型

Vim 的 `j`/`k` 与编辑器的 ↑/↓（原生按键与 xwidget 宿主都经由 `runEditorMovement`）走同一套
视觉行（`src/cm6/screen-rows.ts`：`visualRowAt`、`nextVisualRow`、`prevVisualRow`、
`moveByVisualRow`）：

- 一行是读者看到的折行行；**仍被绘制的块**（公式、图表、元数据封面、未打开的表格）整体算一行。
- ↑/↓ 越过 object 块时落在其后（越过显示公式即进入公式）；进入 structure 块（表格）时落在近端，
  它随即打开为文字。Vim 落在块行上时落在块的开头，从不落进隐藏源码。
- 行内 object 不能容纳光标：落点在其开头；若它恰好开启一行，落在其后（CodeMirror 把它之前的
  位置画在上一行末尾）。
- 无布局时（脱离文档的编辑器、测试）行退化为源码行，目标列用字符列。
- CodeMirror 的 `moveVertically` 不再使用。

## Vim 与屏幕行

- Vim 的行是屏幕折行行（`src/cm6/screen-rows.ts`）。判断一个字符画在哪一行时，用它
  自身的结束边缘 `coordsAtPos(next, -1)`：CodeMirror 把行首 widget 前的光标位置
  报告在上一行末尾。行首是公式或隐藏标记（`**`）时，这个位置归属它真正显示的行。
- Vim 通过 `renderedConstructAt(view, pos)` 知道哪里画着什么，只把 **object** 当作一个字符
  （在公式上按 `i`/`a` 进入其 TeX 源码）；**structure**（表格、块标题、`@@` 命令）在光标
  落上时打开，Vim 按文字移动。
- 模式通过 `publishMode` 一处发布：写 DOM 属性（供主题/宿主）并在状态中设置 pinning。

## 守卫与测试

| 测试 | 保证 |
|---|---|
| `tests/architecture-constructs.test.ts` | 视口家族共用一个生命周期；字面范围只有一个答案；光标从不被隐藏；组件写文档只经 `dispatchConstructEdit`；静态规则：只有一个行模型、一个装饰修补器，表格文字及公式使用主编辑器；运行时 registry 与绘制双向一致 |
| `tests/rendered-constructs.test.ts` | policy 语义、registry、pinning、代码/公式内不识别、Vim 逐行穿过表格 |
| `tests/cm6/table-keyboard.test.ts`、`table-interaction.test.ts` | 鼠标与键盘进入同一主编辑面、可见网格、单元格按键模型与撤销 |
| `tests/cm6/table-cell-assist.test.ts` | 格内公式进入正文相同的源码编辑路径 |
| `tests/vim-row-geometry.test.ts` | 折行处以组件开头时的屏幕行 |
| `tests/rendered-constructs.test.ts`（vertical motion） | ↑/↓ 进入表格的首/末行 |

新组件要在 `tests/architecture-constructs.test.ts` 的样例笔记里加一段、把 `kind` 加进它检查的列表，
证明 registry 与绘制一致。改动后依次运行：

```sh
npx tsc --noEmit -p .
npx vp test --run tests/architecture-constructs.test.ts tests/rendered-constructs.test.ts tests/vim-row-geometry.test.ts
npm test
```

守卫没有允许列表。失败时修代码；规则本身需要改变时，先改本文，再改守卫。

## 已知差距与路线

- 表格编辑中的网格由源码行装饰绘制；当前格使用主编辑器，未编辑格显示渲染结果。
- **行内 Markdown 标记**（`**`、`` ` ``、链接括号）使用统一 policy，但不进 registry：它们是
  structure，被打开时就是普通文本。

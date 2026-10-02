# Marker / MarkText / files.md → Noema Markdown 编辑器审计与升级

日期：2026-10-02。三个上游固定在以下提交，审计以源码为准，不以 README 宣称代替实现：

| 项目 | 提交 | 架构 | 审计范围 |
| --- | --- | --- | --- |
| [`tk04/Marker`](https://github.com/tk04/Marker/tree/b878afcb2c8895702cacce2613f9081d3682ddc7) | `b878afc`（2024-06-16） | Tauri + React + Tiptap(ProseMirror) 富文本，保存时 HTML→Markdown | `src/`、`src-tauri/src/` 74 个文件、4,935 行 |
| [`marktext/marktext`](https://github.com/marktext/marktext/tree/34b59d0abe505fea5a801cffb3719bb7835f2ede) | `34b59d0`（2026-10-02） | Electron + Vue；`packages/muya`（`@muyajs/core` v2）contenteditable 块树 + ot-json1 历史 | muya 引擎 206 个文件 51,073 行、桌面端 240 个文件 39,500 行；测试/旧版 `muyajs`/语言包/站点 796 个文件按目录核对 |
| [`zakirullin/files.md`](https://github.com/zakirullin/files.md/tree/9e948ba6071e320c41c866f92817cf96f9bb7ba6) | `9e948ba`（2026-09-02） | 浏览器 PWA + CodeMirror 5 + HyperMD“隐藏标记”实时预览；Go 同步/机器人服务 | `web/`、`server/`、`cmd/`、`tests/` 114 个文件、54,388 行（含 CM5/Mermaid/KaTeX 打包，只列不审） |

逐文件完整行号范围与判断见[行号清单](markdown-editors-line-audit-2026-10.md)。本报告把涉及写作语义、渲染、交互、页面逻辑和数据可靠性的判断定位到具体代码行；每一项合并都先在 Noema 中用探针测试复现问题，再实现并加回归测试。

筛选标准沿用 [MarkWright 审计](markwright-noema-audit-2026-09.md)：Noema 是 Emacs 内、以 Markdown 源码为唯一权威的 CM6 实时预览编辑器（[`site-lisp/noema/CLAUDE.md`](../site-lisp/noema/CLAUDE.md)）。上游的成熟**行为**按源码模型重述后合并；块树、富文本往返、Electron/PWA 外壳和独立产品功能不进入 Noema。

## 架构差异决定了能借鉴什么

| 维度 | Marker | MarkText | files.md | Noema |
| --- | --- | --- | --- | --- |
| 文档权威 | ProseMirror 文档；保存时 `turndown(getHTML())` 整篇回写，**有损** | muya 块状态树（JSON），与 Markdown 双向序列化 | CM5 文本即源码 | CM6 文本即源码；Lezer 语法树 + 装饰 |
| 预览方式 | 富文本 | 块级 WYSIWYG，光标所在 token 展开源码 | 隐藏标记 token；仅光标展开 | 行内标记/部件；光标展开源码 |
| 历史 | ProseMirror history | ot-json1，空白/增删切换断组 | CM5 history | CM6 history（本次加入断组规则） |
| 保存 | 200ms 去抖整篇写，失败 `alert` | 自动保存 + 文件监视 + 换行/编码保真 | FS Access 写；脏标记竞态处理 | 增量 ChangeSet + 版本 CAS（本次补换行保真与退避重试） |

所以：MarkText 的段落/粘贴/格式**语义**和 files.md 的“只由光标展开”**交互规则**可以按源码重述；Marker 的价值主要在交互外观（气泡菜单激活态）和它的反例（有损往返保存）。

## 一、功能：已合并

| 问题（先用探针在 Noema 复现） | 参考实现 | Noema 实现与测试 |
| --- | --- | --- |
| `Cmd-B/I/Shift-X` 与选区工具栏只会**包裹**不会**切换**：在 `**hello**` 内再按粗体得到 `****hello****`；选区两端有空格时得到不渲染的 `** hello **`；多光标只处理主选区 | MarkText [`format.ts:1681-1779`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/format.ts#L1681-L1779)（同类 token 内移除、部分重叠合并、[空白移出 `:1735-1742`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/format.ts#L1735-L1742)）；HyperMD [`keymap.js:515-572`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/keymap.js#L515-L572)；files.md [`editor.js:408-457`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/editor.js#L408-L457) | [`inline-format.ts`](../site-lisp/noema/src/cm6/inline-format.ts)：粗/斜/删除线/行内代码/高亮/上下标切换；光标在 span 内即移除；部分重叠合并；跨行逐行包裹并跳过列表/引用/标题前缀（同 MarkText 不格式化 `# `）；行内代码自动加长反引号；代码块内不插入；一次撤销。新增 `clear-format`（`Mod-\`，选区工具栏 `Tx`）。测试 [`format-toggle.test.ts`](../site-lisp/noema/tests/cm6/format-toggle.test.ts) |
| 标题/引用/列表命令只改当前行且不能撤销格式 | MarkText [`muya.ts:1213-1329`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/muya.ts#L1213-L1329)（再次点击同类型即还原为段落；跨块包裹） | [`block-format.ts`](../site-lisp/noema/src/cm6/block-format.ts)：作用于选区覆盖的所有行；同级标题再按还原；引用整体加/减一级；有序/无序/任务互转、保留缩进与勾选状态；光标留在原文字上 |
| 缺少标题升降级 | MarkText [`muya.ts:1535-1547`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/muya.ts#L1535-L1547) | `heading-promote` / `heading-demote` 命令（正文→`######`、`#` 不再升；`######`→正文），进入快速插入。`Cmd-=`/`Cmd--` 已属缩放，`Option` 和弦属于 Emacs，故不加新快捷键 |
| 勾选任务回车续行仍是 `- [x]`；`* [ ]`、`1. [ ]` 续行丢失勾选框；空的 `* [ ]` 回车不退出列表 | HyperMD [`keymap.js:46-63`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/keymap.js#L46-L63)（`replace("x", " ")`）；MarkText [`paragraphContent:458-579`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/content/paragraphContent/index.ts#L458-L579)（新项 `checked: false`） | [`commands/index.ts`](../site-lisp/noema/src/cm6/commands/index.ts) `nextListMarker`：任何项目符号与有序项都可带任务框，新项一律未勾选；空任务项按任意标记退出；Vim `o/O` 共用同一前缀函数 |
| 输入 ```` ```python ```` 回车后其下全部变成代码，必须手动补闭合围栏 | MarkText Enter 转换代码块；files.md [`editor.js:179-195`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/editor.js#L179-L195)（第三个反引号补闭合） | `closeFencedCodeOnEnter`：只在语法树判定为**未闭合的开围栏**行尾回车时插入闭合围栏，已闭合或作为闭合围栏的行不受影响；保留缩进与 `~~~` |
| 单独一行 `\| a \| b \|` 回车只加一行，没有分隔行，结果不是表格；Tab 还会把它当表格重排 | HyperMD [`keymap.js:96-115`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/keymap.js#L96-L115)；MarkText `_enterConvert` table；Marker [`useEditor.ts:68-90`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/hooks/useEditor.ts#L68-L90) | `createTableFromHeaderRow`：两列以上的单行管道行回车补分隔行与空行，光标进第一格；没有分隔行的管道文本不再被 Tab/Enter 当表格 |
| 代码块里 `{\|}` 回车中间行不缩进；Tab 缩进整行而不是在光标处插入 | MarkText [`codeBlockContent:19-30, 277-416`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/content/codeBlockContent/index.ts#L277-L416) | [`code-block-input.ts`](../site-lisp/noema/src/cm6/code-block-input.ts)：括号对回车展开并缩进；Tab 在光标处补到下一缩进位；选区 Tab/Shift-Tab 按块自身单位移动；`Mod-Enter` 跳出代码块（与跳出 org 环境同键）。缩进单位取块内实际缩进（制表符或最小空格数），否则 4 空格 |
| **中文强调不渲染**：`**（注）**说明`、`前面**加粗。**后面`、`中文**“加粗”**中文` 在编辑器（Lezer）和 HTML（markdown-it）都显示为星号；新的粗体切换也会产生这种标记 | MarkText [`cjkEmStrong.ts:1-93`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/utils/marked/extensions/cjkEmStrong.ts#L1-L93)（把 CJK 当作标点参与 flanking） | [`cjk-emphasis.ts`](../site-lisp/noema/src/cjk-emphasis.ts)：编辑器与 HTML 同一规则。上游“只把 CJK 当标点”并非严格增量（`a**中文**` 会失去开标记），Noema 取 **CommonMark 规则 ∪ CJK 放宽规则**，保证拉丁文本解析不变；Lezer 通过公开 `getDelimiterAt` 取得内部分隔符类型、只在放宽改变结果时接管；markdown-it 只在本实例子类化 `scanDelims`。覆盖 `*`、`_`、`~~` 与中日韩文字。测试 [`cjk-emphasis.test.ts`](../site-lisp/noema/tests/cjk-emphasis.test.ts) |
| 拖入图片/PDF 无反应（CM6 默认把文件当文本读，二进制被丢弃） | MarkText [`dragDropImage.ts:48-153`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/editor/dragDropImage.ts#L48-L153) | `dropAttachmentFiles`：非文本文件走与粘贴相同的资产存储，在落点插入 `![](...)`/链接；纯文本文件仍交给 CM6 |
| 查找只能区分大小写的字面量；没有全词、正则开关和替换 UI（`find.ts` 已有替换函数但面板没用） | MarkText [`utils/search.ts:102-147`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/utils/search.ts#L102-L147) | [`find.ts`](../site-lisp/noema/aaronnote/find.ts)：默认**智能大小写**（查询含大写才区分，与 Emacs isearch 一致）、全词（Unicode 字母数字边界）、正则（Unicode 模式失败时回退）；面板 `Aa / W / .*` 开关与替换行（单个替换、全部替换为一次撤销，`Alt-Enter` 全部替换）|

## 二、可靠性：已合并

| 问题 | 参考 | Noema 修复 |
| --- | --- | --- |
| **CRLF 笔记永远保存失败**：CM6 内部统一 `\n`，增量 ChangeSet 偏移按 LF 计算，Node 与 Go 两条持久化路径都按原始 CRLF 字节核对长度，于是每次保存都报“change-set source length mismatch”，编辑无法落盘；整篇保存则把文件全部改成 LF | MarkText [`main/filesystem/markdown.ts:66-159`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/desktop/src/main/filesystem/markdown.ts#L66-L159)（读取时识别 LF/CRLF/混合，写回原换行） | [`editor-save-changes.ts`](../site-lisp/noema/aaronnote/editor-save-changes.ts) `sourceLineEnding` / `sourceWithLineEnding`：含 `\r` 的笔记不走增量（CM6 偏移无法修补），整篇保存与元数据更新按原换行写回；混合换行取多数。修复在客户端边界，Node/Go 的“偏移即 UTF-16 单元”契约不变 |
| 写入抛错（Emacs 重启 Node 宿主、传输中断）后不再重试，作者停止输入时笔记会一直处于未保存 | files.md 定时同步；files.md [`files.js:1492-1518`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/files.js#L1492-L1518)（失败后恢复脏状态） | [`save-drain.ts`](../site-lisp/noema/aaronnote/save-drain.ts) 增加可选 `retryDelayMs`：抛错的写入按 1s→2s→…→30s 退避重试，成功即复位；冲突与拒绝仍是结果而非失败，不重试；切换笔记时取消；只读与远程笔记不自动重试；状态栏显示“retrying in Ns” |
| 粘贴不看落点：HTML 粘进代码块被转成 Markdown；多行文本粘进表格行把表格拆开；`[t](url)` 粘进 `](\|)` 变成嵌套链接；DOM 粘贴只替换主选区 | MarkText [`clipboard/paste.ts:444-533`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/clipboard/paste.ts#L444-L533)；files.md [`editor.js:262-275`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/editor.js#L262-L275)（URL 粘到选区成链接）；Marker [`link-text.ts`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/components/Editor/extensions/link-text.ts#L1-L119) | [`paste-context.ts`](../site-lisp/noema/src/cm6/paste-context.ts)：代码内粘贴剪贴板纯文本；行内代码折行为空格；表格行 `\n→<br>`、转义 `\|`；链接目标只取 URL；选中文字粘贴 URL 成为链接；多光标按 CM6 规则逐行分发；事务标 `input.paste`。粘贴管线把剪贴板纯文本一路传入（`EditorPasteSource`） |
| 粘贴 Excel/Numbers/网页表格（无 `<th>`）保留为原始 `<table>` HTML；单元格内段落/换行拆行；`\|` 未转义；`colspan` 导致列数不齐 | MarkText [`utils/paste.ts:13-153`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/utils/paste.ts#L13-L153) | [`paste-html.ts`](../site-lisp/noema/src/paste-html.ts) `normalizePastedTables`：首行提升为表头、展开 `colspan`、补齐短行；单元格规则把块级换行折成 `<br>` 并转义管道 |

## 三、性能与页面逻辑：已合并

1. **范围选择不再把渲染还原成源码**。Noema 原先让任何与选区相交的行内 span 显示标记，`Cmd-A` 或拖选一段会把视口内每个 `**`、反引号和链接 URL 都展开并整段重排。files.md 的补丁只让光标展开（[`hide-token.js:276-289`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/hide-token.js#L276-L289)），MarkText 以 anchor/focus 判断。Noema 采用后者：光标展开所在 span，范围只展开**两端所在的** span（[`live-preview.ts`](../site-lisp/noema/src/cm6/live-preview.ts) `selectionIntersectsSpan`）。
2. **部件的“更新键”与“构建器”曾经不一致**。图片、行内命令、HTML 块、Mermaid 的 `active*Key` 都写明“范围选择保持渲染”并对范围返回空键，但对应构建器仍按相交判断展开。结果是范围选择期间一旦因编辑、滚动或尺寸变化重建，跨过的部件全部变回源码。现在四者都改为只由光标展开，与键函数一致；`@@note-code` 与 Lean 占位（每次选择都重建）使用共享的端点规则 `selectionRevealsSource`（[`selection.ts`](../site-lisp/noema/src/cm6/extensions/visual/selection.ts)），参考 files.md [`fold.js:165-173`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/fold.js#L165-L173)。显示公式本来就只由显式 `revealFormulaSource` 展开，不变。回归测试 [`live-preview-range-reveal.test.ts`](../site-lisp/noema/tests/live-preview-range-reveal.test.ts) 在旧代码上失败、新代码通过。
3. **撤销粒度**。CM6 只按 500ms 时间合并，连续打完一句话一次撤销全部，打错再删也和前文合并。MarkText [`history/index.ts:82-101`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/history/index.ts#L82-L101) 在输入空白和“插入/删除切换”处断组；[`history-grouping.ts`](../site-lisp/noema/src/cm6/history-grouping.ts) 通过 CM6 `joinToEvent` 实现同样规则，每笔事务 O(1)。Vim `u` 走同一历史：插入会话原先按时间被切碎，现在按词切分（真正的“整段插入一步撤销”需要 vim-lite 显式隔离，列为后续）。
4. 新命令的开销都限定在选区：行内格式只在选区范围迭代语法树、高亮只扫描选区所在行；粘贴上下文只解析落点所在行；代码块缩进单位最多扫描 200 行。

## 四、人机交互：已合并

- 选区工具栏按 Marker [`Menu.tsx:19-96`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/components/Editor/Menu.tsx#L19-L96) 的 `isActive` 显示已生效格式（`aria-pressed`，`activeInlineFormats`），新增 `Tx` 清除格式；`Mod-\` 与 Typora 一致（MarkText 用 `Shift-Cmd-R`，见 [`inlineFormatToolbar/config.ts:17-75`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/ui/inlineFormatToolbar/config.ts#L17-L75)）。
- 查找面板默认智能大小写；替换行默认隐藏，`⇄` 打开；`Enter` 替换当前并跳下一个。
- 标题/列表切换保留光标在原文字上（MarkText `_withPreservedOffset`），不再跳到行尾。

### 第二轮：按按键、落点和可见反馈复查

第一轮的功能测试偏重“命令执行后文本是什么”。第二轮把**光标落点、下一次按键、跨容器边界、内容是否仍可见**也纳入断言。以下是具体实现判断；对照仍是上表锁定的三个提交。

| 场景与原有问题 | 三项目的实现对照 | Noema 的边界与反馈 |
| --- | --- | --- |
| 标题位于引用或列表内时，标题命令把外层 `>` / `- ` 当正文；标题内容起点 Enter 降成段落，Backspace 留下 `#Title` | MarkText [`muya.ts:1213-1329`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/muya.ts#L1213-L1329) 在所属块内变换；[`atxHeadingContent:33-75`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/content/atxHeadingContent/index.ts#L33-L75) 的 Enter 保留标题、Backspace 退成段落。Marker 的 ProseMirror 节点命令不能直接用于源码。 | `block-format.ts` 先解析引用/列表容器，只改其中 `#`；`insertLineBeforeHeading` 在标题前插入空行，引用中的空行保留 `>`；`deleteHeadingMarkerBackward` 一次删完整标记。测试在 `format-toggle.test.ts`。 |
| 列表中间转换类型时只改选中行，列表被拆开；任务项退格一次删掉任务框和列表两层 | MarkText [`_convertListType`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/muya.ts#L1641-L1685) 作用于整组同级项；files.md 的 HyperMD 保留文本模型。 | `siblingListItems` 沿同一引用深度、缩进和标记族查找同级项，跨过嵌套项及宽松列表空行；任务项 Backspace 先删 `[ ] `，下次才退出列表。成员判断用集合，避免大选区逐项线性查找形成二次开销。 |
| 行尾 Delete 把下一标题/任务/引用的隐藏标记并成可见文字，如 `para- item` | MarkText [`Format.deleteHandler`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/format.ts#L1526-L1575) 合并块的文本内容。 | `deleteForwardJoinBlock` 只剥下一行的块前缀；代码围栏、表格行、公式边界保持原块结构。普通段落仍走 CM6 的逐字素删除。 |
| 表格 Tab/Enter 到有内容的格子后光标只停在开头，必须手动全选才能覆盖 | files.md [`tableEnterCell`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/table.js#L414-L445) 选中目标格非空内容，空格落在内边距；Marker 的表格节点也以格子为导航单位。 | `cellTarget` 计算去掉内边距后的范围，Tab/Enter 选中非空格内容；空格给出插入点。下次输入直接覆盖旧值，方向键/点击仍能定位单个字符。 |
| 在文档最后的表格/围栏/公式块按 ↓ 后输入，会接到末行源码上；最初修复只凭末行正则，误把单独的 `\| a \| b \|` 当表格，也会抢走长行内的正常向下移动 | MarkText [`Content.arrowHandler`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/content.ts#L478-L555) 在视觉底部且无后继块时创建尾段落；files.md 保持源码行导航。 | `openLineAfterTrailingBlock` 只在**文档真正末尾的空选区**触发；表格看 Lezer `Table` 节点，围栏要求成对闭合，`\]` 要有对应的公式范围。普通管道文本和未闭合开围栏不劫持方向键，重复按 ↓ 不会不断追加空行。 |
| 编辑独占一行的图片源码时图片消失，页面高度收缩；粘贴截图显示 `image-1.png` 伪说明 | files.md [`hide-token.js:276-289`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/hide-token.js#L276-L289) 只因光标揭露标记；MarkText 图片保留可见预览并以空 alt 插入。 | 图片独占一行时源码出现、图片留在下方；行内图片只出现源码，避免破坏句内排版。粘贴或拖入图片写 `![](...)`，附件仍用文件名作链接文字。图片 `alt` 与可见说明目前同源；为截图提供可访问的描述而不显示伪说明，需要单独设计图片说明输入。 |
| 选区工具栏点击粗体后消失；代码块里点击格式按钮无反馈；跨链接边界加粗会把链接源码截断 | Marker [`Menu.tsx:19-96`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/components/Editor/Menu.tsx#L19-L96) 用 `isActive` 显示状态；MarkText 持续显示格式栏、按 token 边界改格式。 | 保持文字选中时工具栏继续显示并刷新 `aria-pressed`；代码块里禁用无效按钮；选区端点落在链接、图片、行内代码或公式里时先扩到完整节点。格式切换按区间循环求首尾位置，避免数万节点时 `Math.min(...spans)` 参数溢出。 |
| 查找面板打开后编辑文档，计数与跳转还用旧偏移；单个替换 `a→aa` 到文末时又绕回新插入的 `aa` | MarkText 搜索在内容变化后重算；Marker 的查找 UI 由编辑器状态管理。 | 文档变化标记匹配过期、180 ms 停顿后重算且不移动光标；按导航或替换前立即重算。替换只走到本轮文末，随后停在 `–/N`，明确按导航才开始新一轮，避免连续 Enter 不断扩写同一替换结果。 |
| 网页 HTML 中 Google Docs 的正常字重外壳产生孤立 `**`；已有 `<strong>` 再带粗体 CSS 会产生 `****Bold****`；`<ol><li value>` 粘贴后序号错误 | MarkText [`normalizePastedHTML`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/utils/paste.ts) 清理外来 HTML；Marker 使用 turndown，但它的整篇 HTML 回写不适合 Noema。 | `normalizePastedInlines` 去掉正常字重外壳、把确有样式的 span 变语义标签，并识别同类祖先/子标签，避免重复包裹；列表规则保留 `start`、`value` 与倒序编号，一次缓存序号，长列表转换不再每项重新遍历前项。测试在 `paste-html.test.ts`。 |

性能复核：5 MB 合成笔记中跨公式拖选 60 步，单独运行的第 95 百分位为 **8.46 ms**（源码模式对照 1.08 ms）；该测试在整套并行运行时曾偶发超帧，单独复跑通过。对长列表和大量格式 span 的复杂度修正是静态边界检查加功能测试，未把合成基准当成真实 Emacs/WebKit 帧时间。最终完整测试与构建状态见下方“验证”。

## 五、已核对、Noema 已有或更好

| 上游 | Noema |
| --- | --- |
| Marker [`Editor.tsx:36-55`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/components/Editor/Editor.tsx#L36-L55) 200ms 去抖整篇 `htmlToMarkdown` 写回、失败 `alert` | 增量 ChangeSet + 版本 CAS + 串行 SaveDrain；源码无往返损失 |
| files.md [`files.js:1193-1221`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/files.js#L1193-L1221) 同文件重载用前后缀最小 diff | `minimalDocumentChange` + 视口稳定器 + 选区映射已有 |
| files.md [`table.js:251-529`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/table.js#L251-L529) 悬浮 ± 行列、Enter 同列下一行；Marker `TableView.tsx` 悬浮 + | 表格 widget：工具栏、边缘 +、拖拽排序、8×8 尺寸网格、Enter 同列（已有） |
| MarkText 代码语言选择器、复制、Mermaid/数学预览、脚注工具、链接工具 | 语言徽章、复制按钮、折叠、Mermaid/数学 widget、脚注 widget、链接预览（已有） |
| MarkText 文件监视器用时间窗忽略自身写入（[`watcher.ts:405-445`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/desktop/src/main/filesystem/watcher.ts#L405-L445)） | `watch.mjs` + 内容摘要版本，比时间窗准确 |
| CM5 拖选自动滚动（files.md `autoscroll.js`） | CM6 原生 |

## 六、明确不合并

| 上游 | 理由 |
| --- | --- |
| files.md [`server/sync/merge.go:16-82`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/server/sync/merge.go#L16-L82) 无公共祖先的行级 LCS 并集合并 | 一侧删除的行会被另一侧复活；Noema 保留版本 CAS 冲突并交给 Emacs 处理 |
| MarkText [`content.ts:113-167`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/content.ts#L113-L167) `* _ ~ \`` 等 Markdown 语法自动配对 | 源码编辑下与输入 `* ` 列表、TeX 下标 `_` 冲突；选区包裹已有 |
| MarkText `copyAsRich`（`text/html` 富文本复制） | Emacs xwidget 宿主剪贴板只能传纯文本；且 HTML 回贴会损失 `[[wiki]]`、`@@` 命令等 Noema 语法 |
| files.md 图片点击灯箱（[`fold-image.js:102-142`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/fold-image.js#L102-L142)） | Cmd 点击已交给 Emacs/系统打开附件；不另建页面级模态 |
| files.md 首行强制 `# ` 标题与改名联动 | 产品约束，不适用于 Noema 的 meta/文件名模型 |
| Marker 整篇 HTML 往返、MarkText 块状态序列化 | Noema 源码即文档，引入任何往返都会制造损失 |
| MarkText 桌面端多标签、偏好、菜单、拼写、导出、PWA/聊天/Telegram（files.md） | 宿主是 Emacs；无关产品外壳 |

## 七、后续

- **崩溃草稿**：MarkText [`editorBufferStore`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/desktop/src/main/editorBufferStore/index.ts) 把未保存标签落盘。Noema 当前依赖 Emacs 缓冲所有权、pagehide keepalive 与新增的退避重试；若要覆盖“宿主长时间不可用且页面被关闭”，需要 IndexedDB 草稿（笔记可超 localStorage 上限）与按版本恢复的交互，单独设计。
- **Vim 插入会话撤销**：让 vim-lite 在进入/离开插入模式时隔离历史，使 `u` 撤销整段插入。

## 验证

### 第三轮：链接边界与大文档操作

这一轮同时核对四个上游。MarkWright 的 [`useEditorActions.ts:8-29,61`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useEditorActions.ts#L8-L29) 将选区直接包成链接，短文本够用；Marker 的 [`Popover/Link.tsx`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/components/Editor/Popover/Link.tsx) 以已有链接范围执行设置/取消；MarkText 的 [`format.ts`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/format.ts) 先处理当前 token；files.md 的 [`editor.js:262-275`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/editor.js#L262-L275) 在选中文字时把粘贴 URL 变为链接。Noema 保持源码模型，沿用后面三个项目的语义：已有链接可改址或取消；选区切入链接、图片、行内代码、公式时先补齐元素边界；新链接中的旧链接被摊成文字，避免生成嵌套链接；代码块中不执行链接/格式命令，工具栏相应按钮禁用。实现见 [`commands/index.ts`](../site-lisp/noema/src/cm6/commands/index.ts)、[`inline-format.ts`](../site-lisp/noema/src/cm6/inline-format.ts)，回归在 [`format-toggle.test.ts`](../site-lisp/noema/tests/cm6/format-toggle.test.ts)。

5 MB 合成笔记暴露了另一条路径：Lezer 初始语法树只覆盖前约 3 KB，光标在末尾时请求从文首解析到光标，格式工具栏三个连续查询合计约 **1.25 秒**，而且拿到的仍可能是不完整的树。现在行内命令使用与编辑器相同语法配置解析所需行，围栏位置按不可变文档缓存；探针首次状态检查约 **20 ms**，其后格式状态约 **1 ms**。末尾链接命令约 **115 ms**，其中普通 CM6 编辑提交约 **60–75 ms**；这些是 happy-dom 中的诊断计时，不能外推为 WebKit 帧时间。5 MB 末尾代码块、粗体和跨链接操作有独立的 [`large-document-format.test.ts`](../site-lisp/noema/tests/cm6/large-document-format.test.ts)，并用行为断言避免把机器负载写成脆弱阈值。

本轮 Noema 提交为 `243f9ec`。串行全量测试为 **278 个通过文件、2831 项通过、16 项跳过**；`tsc --noEmit` 通过。只含本轮暂存改动的独立快照 `make build` 成功（渲染器与 Go 内核），其渲染器已安装到 Emacs 使用的 `dist/aaronnote`，构建标识为 `1790931170929-28ce5334-bbf4-4bef-a149-1eba2b093d3d`。

进一步拆分 5 MB 文档的编辑提交：最小 CM6 约 3 ms，分别加入公式或目录索引约 5 ms，完整 Noema 约 83 ms。剖析后修正三处：普通正文的单行输入先排除与有序列表无关的解析；视口预览只合并与视口相交的公式范围；在最后一个公式之后编辑普通文字且没有新分隔符时直接复用公式索引。相同提交探针降到约 **50 ms**；通过宿主与原生共用的 `runEditorTextInput` 测得约 **48 ms**。剩余耗时主要见于 CM6 的语法解析预算和装饰比较，仍是 happy-dom 诊断数据。对照四项目时，沿用 files.md 的当前行/可见渲染范围更新及 MarkText 的块内操作思路；MarkWright 的 120 ms 全量双栏预览节流、Marker 的 ProseMirror 文档更新各有自身模型，Noema 以已有输入合并策略承接其行为。

这三处追加优化提交为 Noema `08ca3b2`。类型检查与串行全量测试通过：**278 个通过文件、2835 项通过、16 项跳过**；列表结构插入、引用内数字修正、尾部编辑复用公式索引及新增公式都有回归断言。独立快照 `make build` 成功，渲染器已安装，构建标识更新为 `1790932450001-365cfccf-925b-4503-86c8-db74e1e62223`。

第四轮回查发现局部解析加速的语义回归：Markdown 的粗体、行内代码和链接可以跨**同一段落**的软换行，逐行解析却把第二行视作普通文本。在 `**first\nsecond**` 第二行取消粗体会插入四个星号，在 `` `first\nsecond` `` 内按粗体会插入字面星号，在 `[first\nsecond](url)` 内按链接会嵌套新链接。MarkText 的 [`format.ts:1681-1715`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/format.ts#L1681-L1715) 从当前块的完整 token 范围取得格式；files.md 的 [`markdown.js:466-477`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/markdown.js#L466-L477) 在续行保留代码段状态。Marker 的链接命令扩展到整个链接 mark；MarkWright 的简单源码包裹没有这个识别层。Noema 现在优先复用 CM6 已解析的文档树，未解析到的大文档位置只解析当前块和段落续行，按不可变文档缓存局部树。链接命令可取消或改址跨行链接，也允许为同一段落的软换行创建链接；跨空行仍拒绝。局部树从代码围栏内部起步时会缺少开围栏，因此格式状态额外用代码块边界排除字面标记。引用和列表续行、5 MB 文档尾部都有回归测试。实现见 [`languages/markdown/index.ts`](../site-lisp/noema/src/cm6/languages/markdown/index.ts)、[`inline-format.ts`](../site-lisp/noema/src/cm6/inline-format.ts) 和 [`commands/index.ts`](../site-lisp/noema/src/cm6/commands/index.ts)。类型检查和串行全量测试通过：**278 个通过文件、2841 项通过、16 项跳过**。5 MB 文档尾部的单次诊断测量：首次跨行格式状态查询约 **27 ms**，缓存后约 **0.1 ms**；取消粗体和链接各约 **74–81 ms**（含 CM6 编辑提交），均为 happy-dom 计时，不代表实际 WebKit 帧预算。Noema 提交 `9bb4a75`；独立快照 `make build` 成功，渲染器已安装到 `dist/aaronnote`，构建标识 `1790933782161-67e51505-a97e-49c0-be55-563cf72bda31`。

### 第五轮：跨块格式的可逆性与表格编辑成本

这一轮同时检查“操作一次”“再按一次取消”“下一步继续编辑”三种状态。MarkText 的 [`muya.ts:401-535`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/muya.ts#L401-L535) 通过 `_formatAcrossBlocks` / `_formatLeafInRange` 处理各个可格式化叶块，保留选区端点并避开标题标记；Marker 用节点和 mark 状态表达格式；files.md 的 [`keymap.js:515-580`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/keymap.js#L515-L580) 结合 token 状态包装选区；MarkWright 的简单字符串包装适用于单次操作，但没有完整的已有格式识别。Noema 本轮采用 MarkText 的叶块处理语义，并继续直接修改 Markdown 源码。

- **多行格式可以取消**：两个列表项同时加粗后，再按粗体会移除两项的标记；工具栏能识别整段已经加粗。同一段落的软换行只使用一对粗体、斜体、删除线或代码标记，避免逐行包裹留下不能正确切换的组合。
- **混合选区保留结构**：选区穿过正文、代码块、表格和分隔线时，只格式化段落、标题文字和表格单元格，跳过代码内容及结构标记。已有粗体与普通文字混选时先统一加粗，再按一次统一取消。反向选区保留方向。这里验证的是上述块类型，没有把结果推广到所有自定义环境。
- **标题链接只覆盖标题文字**：Lezer 的标题节点名带级别，原有节点识别遗漏这些名字；现在 ATX 与 Setext 标题都能建立链接，开头及结尾的 `#`、Setext 下划线留在链接外。
- **大文档尾部使用完整上下文**：局部解析表格行时带上相邻表格行，使表头和分隔行参与语法判定；选区从代码围栏内部跨到正文时，先补齐围栏上下文，清除格式不会误删代码里的字面星号。5 MB 尾部场景已有回归断言。
- **表格编辑复用单元格**：同尺寸表格的文字或对齐变化更新已有单元格；增删行列时重建，保证新单元格获得输入和导航处理器。默认 MarkdownIt 实例只配置一次，各次渲染保持独立的 token/env；显式渲染选项仍使用独立实例，引用、脚注和 HTML 选项隔离有测试。
- **后续操作定位当前表格**：在表格前插入文字后，再增行或提交单元格内容，旧闭包偏移可能覆盖正文。写入现在从当前 DOM 与文档表格索引重新取得范围。编辑或 Escape 恢复预览后，行列拖拽柄继续保留。

性能诊断使用 100 行和 1,000 行的合成表格，把格式变换准备与编辑器提交分别计时。1,000 行整表格式操作的提交从约 **5,416 ms** 降至 **555 ms**，取消格式的提交从约 **5,123 ms** 降至 **468 ms**；100 行提交从约 **445 ms** 降至 **72 ms**。优化后，1,000 行表格中单格格式操作约 **33 ms**，直接编辑单元格后的提交约 **39 ms**。主要减少了每格重新配置 MarkdownIt 和每次提交重建整表的成本。以上是 happy-dom 单次诊断数据，**不是实际 Emacs/WebKit 的帧时间**；整表大范围格式操作仍有可见成本，不能据此声称所有大表格操作都已流畅。

行为回归位于 `format-toggle.test.ts`、`large-document-format.test.ts`、`roundtrip.test.ts` 和 `render-html.test.ts`。工作区串行全量测试通过：**278 个通过文件、2,856 项通过、16 项跳过**；只含本轮暂存改动的独立快照也通过完整测试：**278 个通过文件、2,847 项通过、16 项跳过**。两者数量差来自其他会话的未提交测试。快照首次测试因 Vite 不允许读取外部链接的依赖目录、`resources/snippets` 相对链接在临时目录失效而失败；补充快照的依赖访问路径并指向规范 snippets 目录后，全量重跑通过，没有调整源码或测试阈值。

Noema 提交为 **`9676140`**，仅包含本轮 9 个文件。独立快照的 `make build`（含类型检查及 Go 构建）、临时目标路径的安装规则检查、`go test -tags fts5 ./...` 均通过；AaronEmacs 的 `make research-test` 424 项、`make jupyter-test` 268 项也通过。安装时发现当前渲染器已经包含另一会话部署的 LaTeX 任务干预按钮和 TikZ 主题修正，因此另建渲染器快照，保留那 7 个前端文件的现有修改，再合入本轮已验证文件。该组合构建通过，源码哈希及安装前构建标识均核对，产物逐文件哈希与安装目录一致。渲染器已安装到 `dist/aaronnote`，构建标识 **`1790936910247-7b0be450-f00c-4464-87ac-90f85a8ecb31`**；其他会话的修改未纳入本轮代码提交。没有把这些 DOM 测试当作真实 Emacs/WebKit 交互验收。

### 第六轮：富预览滚动与原生表格输入

用户指出富含公式、图片、图表的 Markdown 在预览中滚动“躁”，而源码模式舒服。本轮用 **Playwright 的无界面 WebKit** 加载 Noema 实际编辑器和 CSS，对同一份 100 节合成笔记执行向下、反向与源码模式滚动。它能验证 WebKit 排版和 wheel 事件，但不是 Emacs WKWebView 的物理滚轮或触控板手感验收。

滚动路径修正：

- **图表上方继续滚动文档**：行内图表原来截获所有 wheel，且 CSS 的 `overscroll-behavior: contain` 仍会阻断 WebKit 的滚动传递。现在普通滚轮穿过图表，拖拽平移、修饰键缩放、捏合与全屏内平移保留。
- **监听实际滚动容器**：Emacs 页面滚动的是外层 host，原监听器却只在内部 `.cm-scroller`。捕获监听现在覆盖这两个目标，公式内部的横向滚动不触发整页策略。
- **连续滚动仍补上公式**：原有队列会等到最后一次滚动后 120 ms，持续手势可让可见公式一直留白。现在按可见性排序，每帧最多挂载两个公式，并在已用约 4 ms 后让出下一帧；单个公式不能被中途打断，因此这不是硬帧时限。占位高度不再写入真实尺寸缓存。
- **标题箭头不改变行高**：WebKit 中，负边距的行内折叠箭头与 CM6 的 widget buffer、`break-spaces` 组合会把标题撑成两行。箭头移出视口后多出来的一行又消失。箭头现在绝对定位在页边，不参加正文换行。
- **稳定正文尺寸采样**：CM6 从短的纯文本行采样默认字体和行高。离屏标题失去临时标记后可能被当成正文；尚未获得语法高亮的代码行也有同样问题。采样变化会清空整篇高度图，令已离屏的图片等重新按源码高度估算。标题、引用和代码行现在保留结构标记，让采样继续取普通正文，未改 CM6 私有状态或依赖源码。
- **图表加载保留空间**：即使命中 SVG 缓存，重新挂载仍跨过一次动态导入。等待期间保留已测高度，成功和失败后都释放占位；绕排图表的零高锚点不增加高度。

对照依据：files.md [`fold.js:193-209`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/fold.js#L193-L209) 明确处理可见内容延后折叠造成的闪烁，以及折叠和滚动恢复互相干扰；Noema 采用“可见内容及时出现”的原则，具体队列和高度采样修复来自 Noema/CM6 的复现。MarkText 的 [`diagramPreview.ts`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/extra/diagram/diagramPreview.ts) 也有异步加载占位，但不能直接解决 CM6 虚拟视口高度图的问题。Marker 的 ProseMirror 节点与 MarkWright 的源码/预览结构不同，没有把它们的实现当作本轮滚动修复的直接来源。

可重复检查在 [`scripts/check-rich-scroll.mjs`](../site-lisp/noema/scripts/check-rich-scroll.mjs)。在本机一次运行中，向下/反向滚动时持续可见行的**文档坐标修正**最大分别为 **0 / 0.36 px**，正文采样行高固定为 18.1875 px；源码模式为 0 px。这里的坐标修正不是屏幕跳动幅度，因为 CM6 可能同时补偿 scrollTop。三种模式采样帧间隔的第 95 百分位分别为 **37 / 51 / 38 ms**，最大 **56 / 67 / 42 ms**；无界面滚轮调度与探针自身有成本，因此只记录计时，回归门槛检查几何、滚动传递和源码不变。预览两个方向各捕获到一帧可见公式占位，持续手势不会让队列饥饿；仍不能声称富预览帧时间已经与源码模式相同。

表格输入同时修复：组合输入期间 Enter、Tab、Escape 留给输入法，兼顾 `isComposing`、composition 生命周期和 WebKit 的 keyCode 229；Shift-Enter 在选区插入 `<br>`，支持原生输入框撤销；两个快速提交的单元格各有独立撤销步骤；退出单元格输入后，预览中的 Cmd/Ctrl-Z 与重做恢复工作。MarkText [`content.ts`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/base/content.ts) 在 composition 中不处理导航，其 [`tableCell/index.ts`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/block/content/tableCell/index.ts) 的 Shift-Enter 插入 `<br/>`；files.md `tableEnterCell`、Marker 的表格节点及 MarkWright 的 CM6 undo/redo 用于核对导航和历史边界。Noema 的“每格提交单独撤销”是本产品选择。WebKit 已验证 Shift-Enter 后原生 Cmd-Z 能恢复原单元格，组合输入验证使用注入事件，未替代系统输入法候选窗测试。

本轮提交为 **`d621e43`**。与该提交树一致的独立快照通过 `make test`：**280 个通过文件、2,870 项通过、16 项跳过**；`make build`、临时目标安装规则、Go 全量测试均通过。AaronEmacs 的 research 回归 424 项、Jupyter 回归 268 项通过。渲染器已安装到 `dist/aaronnote`，242 个产物文件的哈希与构建快照一致；构建标识 **`1790939658211-7dfe87dc-4fa0-4377-9603-994fe4624b9b`**。另一会话的 5 MB 测试笔记修改未纳入提交或构建。

#### 追加：Company 补全与公式预览按实际位置共存

用户截图指出，即使两窗没有覆盖，补全也会关掉公式预览。复查发现 `showSnippetPopup` 直接调用 `hideMathPreview`，`updateMathPreview` 又以补全可见为条件提前退出，形成无条件互斥。现在两处都移除这条条件，保留视觉公式编辑器本身的重复预览保护。

[`preview-placement.ts`](../site-lisp/noema/aaronnote/preview-placement.ts) 只在实际矩形重叠时寻找视口内附近的位置，同时避开正在编辑的源码行带；补全菜单的位置和键盘焦点不变。若窗口小到没有空位，预览只设为不可见，继续保留公式会话；菜单关闭或移开后立即恢复。异步公式渲染改变尺寸、菜单重新定位、预览报错回退都走同一套避让。

6 个几何用例覆盖宽屏不重叠、上下/左右避让、无空间恢复及异步尺寸变化。另有 [`scripts/check-math-completion.mjs`](../site-lisp/noema/scripts/check-math-completion.mjs)：在无界面 WebKit 中加载实际 `main.ts`，仅为测试附加私有函数入口并提供空宿主桥，不打开或保存用户笔记。验证补全先出现与预览先出现两种顺序、真实 DOM 矩形互不覆盖、400×220 小视口的暂时隐藏和关闭菜单后的会话恢复。这是实际渲染器逻辑的浏览器检查，仍不等同于 Emacs 现场输入法测试。

追加提交为 **`9d9b2ac`**。与提交树一致的独立快照 `make test` 通过：**281 个通过文件、2,876 项通过、16 项跳过**；类型检查、`make build` 和临时安装规则通过。真实页面函数的 WebKit 检查通过。最终渲染器已安装，242 个产物文件哈希与快照一致，构建标识 **`1790940680571-27fe0b2a-df07-43a6-bbd6-696c53622dc9`**。它包含前述滚动修复。

### 第七轮：图片与嵌入页面的持续性

复查图片生命周期，先用回归测试复现四个问题：图片前输入普通文字，120 ms 合并输入结束后会新建 `<img>` 或 `<iframe>`；合并输入期间控件仍持有旧源码位置，点击对齐会失效；相同图片的不同宽度、标题共用一个精确高度缓存；取消拖拽缩放只恢复 width，没有恢复 max-width。

现在图片内容、尺寸和解析后的资源地址不变时，只更新源码位置，保留已有媒体 DOM。合并输入期间同步映射可见图片的位置元数据，图片控件、源码点击和附件打开均能读到新位置；若编辑本身碰到图片源码，则立即重建该视口的图片装饰。尺寸改变和资源地址改变仍会刷新。高度缓存区分解析后的资源地址、完整 caption 和 layout，组估计也区分宽高；取消拖拽恢复原来的宽度约束。

| 对照项目 | 本轮核对的实现与采用边界 |
|---|---|
| MarkText | [`loadImageAsync.ts`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/inlineRenderer/renderer/loadImageAsync.ts) 按资源保存加载状态和天然尺寸，[`image.ts`](https://github.com/marktext/marktext/blob/34b59d0abe505fea5a801cffb3719bb7835f2ede/packages/muya/src/inlineRenderer/renderer/image.ts) 在渲染时另行应用当前尺寸。资源相同与渲染高度相同是两回事；Noema 修复后者的缓存键，未照搬其资源加载器。 |
| Marker | [`Image.tsx`](https://github.com/tk04/Marker/blob/b878afcb2c8895702cacce2613f9081d3682ddc7/src/components/Editor/NodeViews/Image/Image.tsx) 以图片节点属性驱动 React NodeView，资源解析 effect 依赖 src。Noema 用 CM6 的 `updateDOM` 保留内容未变的媒体元素。 |
| files.md | [`fold-image.js`](https://github.com/zakirullin/files.md/blob/9e948ba6071e320c41c866f92817cf96f9bb7ba6/web/lib/fold-image.js) 通过 CM5 marker 持有媒体，load/error 调用 `marker.changed()`；核对了“媒体加载只通知测量”的职责，未引入其灯箱。 |
| MarkWright | [`useImageArchive.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useImageArchive.ts) 负责归档并插入相对路径，未提供 CM6 富预览的图片生命周期方案；本轮不改附件归档语义。 |

[`image-interaction.test.ts`](../site-lisp/noema/tests/cm6/image-interaction.test.ts) 的 7 项回归覆盖上述问题，以及更改图片源码、切换笔记基址。[`check-image-stability.mjs`](../site-lisp/noema/scripts/check-image-stability.mjs) 在无界面 WebKit 中验证：嵌入页输入未保存内容后，在它上方连续打字并等待每次装饰重建，图片和 iframe 元素保持同一实例，输入内容保留，两者的网络请求均保持 1 次；输入后立即点击图片对齐，也只修改当前图片的属性。这是实际浏览器生命周期检查，未将其等同于物理滚轮手感测试。

本轮提交 **`70c5f3a`**。同一暂存树的独立快照通过 `make test`（**282 个通过文件、2,883 项通过、16 项跳过**）、`make build` 和临时安装规则；Go 全量、AaronEmacs research 424 项、Jupyter 268 项通过。完整字体下的 WebKit 滚动检查保持通过：持续可见行的文档坐标修正为向下 0、反向 0.36 px，正文行高采样保持不变；帧间隔第 95 百分位为 38 / 46 ms，源码模式 36 ms，仅作本机观测。图片生命周期脚本也在该快照复验通过。渲染器 242 个产物文件已安装并核对哈希，构建标识 **`1790941862252-e91df193-8946-4155-873b-d668cd7ebf8e`**；另一会话的 5 MB 笔记修改未进入提交或构建。

### 第一、二轮验证记录

新增/更新测试：`tests/cm6/format-toggle.test.ts`、`tests/cm6/paste-context.test.ts`、`tests/cm6/code-block-input.test.ts`、`tests/cm6/history-grouping.test.ts`、`tests/cjk-emphasis.test.ts`、`tests/live-preview-range-reveal.test.ts`、`tests/editor-line-endings.test.ts`、`tests/save-drain.test.ts`、`tests/find.test.ts`、`tests/system-clipboard.test.ts`。Noema 全量 `npm test`：277 个测试文件、2,788 个测试全部通过；`tsc --noEmit` 无新增错误。

第二轮提交为 Noema `e472254`，只含编辑器相关 18 个文件；与 LaTeX 导出、research-memory 等并行会话的未提交改动分开。锁定 Node 26.5.0 / npm 11.17.0 后，串行全量测试 **277 个文件、2,826 项通过、16 项跳过**；同一提交树的独立快照 `make build` 成功（渲染器 + Go 内核），并已将该快照的渲染器产物安装到 Emacs 指向的 `dist/aaronnote`。并行全量运行中各有一次独立的性能计时断言超阈值（链接解析比值 3.014 对 3.0；5,143 标题输入第 95 百分位 9.81 ms 对当次动态阈值 6.86 ms）；两项单独复跑分别为 1.96 倍和 3.95 ms，串行全量亦通过。未调整测试阈值，也未把这些并行负载波动记为功能通过的证据。

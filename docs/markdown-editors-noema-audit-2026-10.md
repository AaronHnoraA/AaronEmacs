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

新增/更新测试：`tests/cm6/format-toggle.test.ts`、`tests/cm6/paste-context.test.ts`、`tests/cm6/code-block-input.test.ts`、`tests/cm6/history-grouping.test.ts`、`tests/cjk-emphasis.test.ts`、`tests/live-preview-range-reveal.test.ts`、`tests/editor-line-endings.test.ts`、`tests/save-drain.test.ts`、`tests/find.test.ts`、`tests/system-clipboard.test.ts`。Noema 全量 `npm test`：277 个测试文件、2,788 个测试全部通过；`tsc --noEmit` 无新增错误。

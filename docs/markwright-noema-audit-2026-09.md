# MarkWright → Noema Markdown 编辑器审计

日期：2026-09-28。上游固定在 [`okazaki112/MarkWright@18a0b28`](https://github.com/okazaki112/MarkWright/tree/18a0b28e4b409814db80289952ba37f8b7932a31)。审计以这个提交的源码为准，不以 README 的功能宣称代替实现。检查范围为 `src/`、`src-tauri/src/` 和 `tests/` 的 **114 个源码/测试文件、17,069 行**。逐文件完整行号及取舍理由列在[行号清单](markwright-line-audit-2026-09.md)；下文把涉及写作、保存、渲染、导出和数据可靠性的判断定位到具体代码行。锁文件、图标和生成资源不属于功能审计。

筛选标准：Noema 是 Emacs 内的 Markdown 知识与研究写作界面。只有能改善现有写作、预览或数据可靠性的功能才合并；独立桌面应用的窗口、工作区和打卡功能不进入 Noema。

| MarkWright 源码与实际行为 | Noema 对应实现 | 判决 |
| --- | --- | --- |
| [`useReadability.ts:7-23`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useReadability.ts#L7-L23) 显示阅读时长，按中文 300 字/分钟、英文 200 词/分钟估算 | [`writing-stats.ts`](../site-lisp/noema/aaronnote/writing-stats.ts) 已有更广的 CJK 计数与 `readingMinutes`，但编辑 HUD 只显示字数 | **不合并**（用户不需要）：`writing-stats.ts` 仍保留 `readingMinutes` 计算，HUD 只显示字数 |
| [`useEditor.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useEditor.ts#L1-L175)、[`useEditorActions.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useEditorActions.ts#L1-L78) 的 CM6、格式化、搜索、多选区 | [`editor-cm6.ts`](../site-lisp/noema/src/cm6/editor-cm6.ts)、[`commands/index.ts`](../site-lisp/noema/src/cm6/commands/index.ts) 已实现，且要遵守 Emacs/xwidget 键盘所有权 | **已有**，不复制较浅的编辑命令 |
| [`useListRenumber.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useListRenumber.ts#L1-L175) 的同事务列表重编号 | [`ordered-list-renumber.ts`](../site-lisp/noema/src/cm6/ordered-list-renumber.ts) 已有同事务实现 | **已有且更好**：Noema 基于语法树并限制解析窗口，跳过撤销/重做；上游每次击键把全文拆成行数组。上游保留中间项手改序号（`1. 5. 6.`），但 CommonMark 仍渲染为 1-2-3，Noema 让源码与渲染一致，不改 |
| [`useSlashMenu.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useSlashMenu.ts#L1-L310) 的静态 Markdown 片段菜单 | [`hint-core.ts`](../site-lisp/noema/src/hint-core.ts) 与 Noema 快速插入菜单已有扩展、排序、隐藏及中文触发支持 | **部分合并**：[`:80-85`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useSlashMenu.ts#L80-L85) 在中日韩文字后直接输入 `/` 也触发（`、` 仍需边界）；[`:23-45`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useSlashMenu.ts#L23-L45) 的拼音首字母别名、四至六级标题和分割线。上游 `\n---\n` 紧贴正文时会变成 setext 二级标题，Noema 的分割线命令总在上方保留空行 |
| [`useMarkdownRenderer.ts:14-129`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L14-L129) 的表格、任务、锚点、图片、Wiki 链接和标签 | [`render-html.ts`](../site-lisp/noema/src/render-html.ts)、CM6 live preview、知识索引已覆盖，还支持脚注、callout、数学、引用等 | **已有**；见下方全局替换风险 |
| [`useRenderer.ts:119-225`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useRenderer.ts#L119-L225) 延迟渲染 Mermaid、KaTeX、Chart.js 和 Kanban | [`diagram-render.ts`](../site-lisp/noema/src/diagram-render.ts) 有按需 Mermaid 与有界缓存；Noema 有数学小部件、Jupyter 图像/输出、Agenda | Mermaid/数学 **已有**。Chart.js JSON 配置和可编辑 Kanban 不适合当前研究文档的证据与任务模型，不合并 |
| [`useOutline.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useOutline.ts#L1-L122) 的标题正则扫描、200 ms 更新 | [`toc-index.ts`](../site-lisp/noema/src/cm6/toc-index.ts) 与页面大纲已有结构化标题索引 | **已有**，不另建并行索引 |
| [`stores/links.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/links.ts#L1-L278) 的全工作区双链、反链和标签云 | Noema Wiki/Graph、backlinks、tag 索引和未链接提及已经承担这些功能 | **已有**，不用每次刷新独立并发扫完整工作区 |
| [`useImageArchive.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useImageArchive.ts#L1-L95) 的图片粘贴到 `assets/` | [`paste.ts`](../site-lisp/noema/src/paste.ts) 已处理图片、文件、HTML、宿主剪贴板和资产存储 | **已有**，保留 Noema 的异步光标定位与宿主路径 |
| [`useScrollSync.ts:17-66`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useScrollSync.ts#L17-L66) 的双栏滚动同步 | Noema 主界面是单栏原位实时预览，Emacs 管理窗口 | **不需要**。该同步只按滚动总高度比例换算，公式/图表高度不一致时也不能准确对应段落 |
| [`useExport.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExport.ts#L1-L158)、[`useExportDocx.ts:28-163`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExportDocx.ts#L28-L163) 的 HTML/PDF/DOCX/富文本复制 | Noema 有 [`self-contained-html.ts`](../site-lisp/noema/src/self-contained-html.ts)、发布 HTML 与 LaTeX/Pandoc 研究稿导出；宿主剪贴板传输当前只写纯文本 | HTML **已有**。上游 DOCX 逐行匹配标题、列表和少量行内标记；数学、图片、表格、脚注、引文无法保真。其 PDF/HTML 从基础 `renderMarkdown` 直接生成，扩展图表不会经过预览增强。浏览器的 `ClipboardItem` 也不能直接代替 Emacs xwidget 的宿主剪贴板协议；这三段实现均不直接合并 |
| [`useReadability.ts:27-64`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useReadability.ts#L27-L64) 的 Flesch 和中文“可读性”分数 | Noema 已有可定位问题的 prose diagnostics/LanguageTool | **不合并**。上游音节以元音组估计，中文分数主要由句长构造；研究论文、公式、引用混排时分数没有足够解释力 |
| [`usePomodoro.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/usePomodoro.ts#L1-L172)、`useWhiteNoise.ts`、统计热力图、日记模板、Tauri 标签页 | Emacs 窗口/项目、Noema WorkNode/Agenda 与现有片段模板 | **不需要**，不新增另一套工作状态或娱乐性界面 |
| [`src-tauri/src/lib.rs:311-440`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src-tauri/src/lib.rs#L311-L440) 的加密 Vault | Noema 的 Markdown 文件与 Git/Wiki 索引工作流 | **不合并**。见下方密钥派生和并发保存问题 |

## 与 Noema 相关的源码行段判读

下面按实际读写路径拆开，而不是以功能名称或 README 文案代替实现。其余桌面壳、主题、休闲功能及测试文件仍逐文件列在[完整行号清单](markwright-line-audit-2026-09.md)。

| 上游行段 | 实际行为与合并判断 |
| --- | --- |
| [`src/composables/useReadability.ts:7-23`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useReadability.ts#L7-L23) | 阅读时长按 CJK/拉丁词速估算；用户不需要，不合并。 |
| [`src/composables/useReadability.ts:27-65`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useReadability.ts#L27-L65) | Flesch/中文句长评分只有粗略启发式；不进入研究写作诊断。 |
| [`src/composables/useAutoSave.ts:16-40`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useAutoSave.ts#L16-L40) | 活动标签监听与延时写入无稳定标签/版本快照；拒绝迁入保存链。 |
| [`src/composables/useFileSystem.ts:25-68`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useFileSystem.ts#L25-L68) | 保存/另存为在异步完成后对当前活动标签执行 markSaved；拒绝迁入。 |
| [`src/composables/useFileSystem.ts:72-145`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useFileSystem.ts#L72-L145) | 打开、丢弃、回退及路径打开；已有 Emacs/Noema 文件边界，且同路径打开可覆盖脏标签。 |
| [`src/stores/document.ts:63-114`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/document.ts#L63-L114) | 标签状态和字数计算；Noema 已有原生 buffer 与更广的 CJK 计数；markSaved 无目标 ID。 |
| [`src/stores/document.ts:123-132`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/document.ts#L123-L132) | 重开已有路径覆盖标签内容；拒绝。 |
| [`src/components/EditorPane.vue:28-82`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/EditorPane.vue#L28-L82) | 每标签保存滚动/选区，但重建 EditorState 导致撤销历史消失；不能移植此状态模型。 |
| [`src/components/EditorPane.vue:114-169`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/EditorPane.vue#L114-L169) | 外部内容、主题/字体变动的同步；Noema 的宿主/CM6 边界已有，主题重建状态不可取。 |
| [`src/components/EditorPane.vue:176-240`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/EditorPane.vue#L176-L240) | 插入/注释/列表编辑命令；多数已有，注释只加开头标记。 |
| [`src/components/TabBar.vue:14-28`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/TabBar.vue#L14-L28) | 切换/关闭标签；保存失败仍关标签；Noema 保留 Emacs 缓冲区。 |
| [`src/components/QuickOpenPanel.vue:22-53`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/QuickOpenPanel.vue#L22-L53) | 文件树扁平化和打开；Emacs 已有检索，直接明文读文件绕过 Vault。 |
| [`src/components/GlobalSearchPanel.vue:22-102`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/GlobalSearchPanel.vue#L22-L102) | 文档搜索替换；Noema 已有，单次替换实际是全替换。 |
| [`src/composables/useMarkdownRenderer.ts:14-78`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L14-L78) | markdown-it 基础扩展、标题锚点与链接属性；Noema 已有更完整渲染。 |
| [`src/composables/useMarkdownRenderer.ts:83-108`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L83-L108) | Wiki 源码预替换和文本标签后处理；可能改写代码样例，不迁入。 |
| [`src/composables/useMarkdownRenderer.ts:110-131`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L110-L131) | 本地图占位和远程图属性；Noema 资产路径/粘贴已有。 |
| [`src/composables/useRenderer.ts:20-118`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useRenderer.ts#L20-L118) | 视口延迟渲染、Mermaid 缓存与释放；Noema Mermaid 已按需处理。 |
| [`src/composables/useRenderer.ts:122-225`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useRenderer.ts#L122-L225) | Mermaid/Kanban/Chart/KaTeX 增强；数学与 Mermaid 已有，另两者不属于当前任务/证据模型。 |
| [`src/components/PreviewPane.vue:27-60`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/PreviewPane.vue#L27-L60) | 全量预览按 120ms 节流；Noema 使用原位预览，无双栏需求。 |
| [`src/components/PreviewPane.vue:143-213`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/PreviewPane.vue#L143-L213) | 本地图资产协议、回退和缓存；Noema 已有资产机制。 |
| [`src/components/PreviewPane.vue:253-273`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/PreviewPane.vue#L253-L273) | Wiki 链点击打开走 Vault；与其他打开入口不一致，Noema 不引入这套分叉。 |
| [`src/composables/useImageArchive.ts:30-95`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useImageArchive.ts#L30-L95) | 图片落 assets 并异步插入；Noema 已有带定位的粘贴流程，见风险 12。 |
| [`src/composables/useExport.ts:21-74`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExport.ts#L21-L74) | HTML/PDF/富文本复制；Noema 已有自包含 HTML，浏览器剪贴板不能直接跨 xwidget 宿主。 |
| [`src/composables/useExport.ts:78-158`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExport.ts#L78-L158) | 打印/导出 CSS 和基础 Markdown HTML；图表/数学增强不经过此导出路径。 |
| [`src/composables/useExportDocx.ts:28-163`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExportDocx.ts#L28-L163) | 逐行/少量内联标记转换；数学、引用、表格、图片不保真。 |
| [`src/composables/useExportPng.ts:125-162`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExportPng.ts#L125-L162) | PNG 调用与写盘；当前写作流程不需，测试契约陈旧。 |
| [`src/composables/useScrollSync.ts:17-66`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useScrollSync.ts#L17-L66) | 双栏滚动比率映射；原位预览不需要。 |
| [`src/composables/useListRenumber.ts:1-175`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useListRenumber.ts#L1-L175) | 有序列表重排；Noema 已有同事务插件。 |
| [`src/composables/useOutline.ts:1-122`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useOutline.ts#L1-L122) | 标题扫描与节流；Noema 已有 TOC 语法索引。 |
| [`src/composables/useSlashMenu.ts:1-310`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useSlashMenu.ts#L1-L310) | 静态斜杠菜单；Noema 已有可扩展快速插入。 |
| [`src/stores/links.ts:1-278`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/links.ts#L1-L278) | Wiki、反链、标签扫描；Noema Wiki/Graph 索引已有。 |
| [`src/composables/useVault.ts:96-109`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useVault.ts#L96-L109) | 全局 verifier 与密码更新；不满足逐工作区边界。 |
| [`src-tauri/src/lib.rs:311-443`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src-tauri/src/lib.rs#L311-L443) | AES-GCM 文件/文字命令与原子写；口令 KDF 和临时文件并发问题使其不可迁入。 |
| [`src/App.vue:196-236`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/App.vue#L196-L236) | 窗口退出流程；保存失败仍销毁窗口，不用于 Noema。 |

## 图片、表格、渲染与排版

| 上游行段 | Noema 对应 | 判决 |
| --- | --- | --- |
| [`preview.css:207-238`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/preview.css#L207-L238) 图片 `max-width:100%` 居中、加载失败斜纹占位 | [`widgets/image.ts`](../site-lisp/noema/src/cm6/extensions/visual/widgets/image.ts)：`{width height align wrap}` 属性可往返源码、拖拽缩放柄、alt 作 figcaption、`cm-image-broken`、测量高度估计 | **Noema 更好**，不合并 |
| [`useMarkdownRenderer.ts:106-127`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L106-L127) 本地图 `data-local-src` 异步填充，刻意不用 `loading=lazy`（WebView2 延迟 load 事件） | Noema 用 `AaronnoteResolveAssetUrl` + `loading=lazy`/`decoding=async`，由 load 事件触发重新测量 | WebView2 问题不适用于 WebKit xwidget；**不合并** |
| [`preview.css:181-198`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/preview.css#L181-L198) 表格边框、斑马纹，`th, td { text-align:left }` | [`table-model.ts`](../site-lisp/noema/src/cm6/table-model.ts) 与表格行列/对齐/移动/格式化命令 | **Noema 更好**；上游只有样式 |
| [`useMarkdownRenderer.ts:14-59`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L14-L59) markdown-it + hljs + anchor；`typographer: true` | CM6 原位实时预览 + `render-html.ts` 发布渲染 | 上游 `typographer` 会把引号/`--` 改成印刷字符，对含代码/公式的研究稿有害；**不合并** |
| [`preview.css:139-147`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/preview.css#L139-L147) 引用块 `font-style: italic` | Noema 正文排版 [`typography.css`](../site-lisp/noema/src/styles/typography.css) 有 kerning、`text-spacing-trim` | 中文无真斜体，会被合成倾斜；**不合并**。上游无任何中西文间距、标点挤压或断行规则，排版算法方面 Noema 更完整 |
| [`preview.css:251-262`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/preview.css#L251-L262) `<kbd>` 键帽样式 | 原先：lezer 把 `<kbd>` 与 `</kbd>` 当作两个独立 `HTMLTag`，中间文字从不在真实元素内，`<kbd>`/`<sub>`/`<sup>`/`<mark>` 在实时预览里都只显示为普通文字 | **合并并修复**：[`live-preview.ts`](../site-lisp/noema/src/cm6/live-preview.ts) `addPairedHtmlTokens` 把同一行的成对标签配对并标记内容（`kbd sub sup mark u ins small`），`<mark>` 复用 `--aaron-highlight` |
| [`useExport.ts:137-141`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExport.ts#L137-L141) 打印分页：标题后不断页，代码/引用/表格不拆 | [`typography.css`](../site-lisp/noema/src/styles/typography.css) 的 `@media print` 原先只调字号 | **合并**：限定在 `.published-note-page #content`，标题 `break-after: avoid`；代码、引用、图、公式不拆；表格改为**行**不拆（整表不拆会把长表推到下一页留空白） |
| [`useRenderer.ts:1-118`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useRenderer.ts#L1-L118) Mermaid 视口内渲染、`securityLevel:'strict'`、40 项 FIFO 缓存 | [`diagram-render.ts`](../site-lisp/noema/src/diagram-render.ts) 已有按需加载、有界缓存，发布页 `strict` | **已有** |

## 文件组织、CSS 结构、主题与生命周期

| 维度 | MarkWright | Noema | 结论 |
| --- | --- | --- | --- |
| 源码组织 | `composables/`（每个功能 100-300 行）+ `stores/`（Pinia）+ `components/`；主题数据拆到 [`data/themes.ts`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/data/themes.ts) | `aaronnote/main.ts` **12,596 行**，页面功能大多内联；`aaronnote/features/` 只拆出 writing-stats、zoom 两个 controller；CM6 层（`src/cm6/`）已按扩展分文件 | **借鉴方向**：新页面功能按 `features/<name>/controller.ts` 的 `create…Controller → { destroy }` 形态落地，逐步从 `main.ts` 迁出；不在本次做整体搬迁 |
| 生命周期 | ViewPlugin `destroy` 移除 DOM/监听；[`useRenderer.ts` `dispose()`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useRenderer.ts#L99-L115) 统一释放 observer/Chart/Kanban | `main.ts` 138 处 `addEventListener`、3 处 remove；复核后 window/document 级监听都在模块顶层一次性注册，按需监听走返回取消函数的 `subscribe()`，**未发现泄漏** | 现状可接受；缺的是“功能 = 可销毁单元”的组织，而非泄漏 |
| CSS 分层 | `tokens → base → editor → preview → content-themes`，`preview.css` 仅 18 个十六进制色（多为 hljs） | 令牌更丰富：生成的语义角色令牌 `aaron-ui-tokens.css`，主题定义标题 1-6、数学、callout、定理环境色；但 `widgets.css`（4,906 行）有 **151** 处、`style.css`（5,308 行）有 **107** 处不在 `var()` 内的裸色值，主题无法覆盖 | **后续重构建议**：把裸色值迁到现有 `--aaron-*` 令牌；新增样式一律用令牌（本次 `<mark>` 已照做）。`!important`：Noema 92 处 vs 上游 9 处 |
| 主题 | 数据驱动的六个正交维度：配色、正文风格、装饰、表面纹理、密度、动效；字体独立设置 | `themes.json` 清单 + 每主题一个 CSS，glob 自动加载；字体随主题 | Noema 的 CSS 文件式主题更易扩展；上游的“密度/装饰”正交维度可作为以后的设计参考，不迁入 |

## 关键源码风险，不能照搬

1. [`useAutoSave.ts:17-36`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useAutoSave.ts#L17-L36) 监听活动标签的内容，却在延时回调内重新读取 `doc.activeTab`。编辑 A 后切到 B，定时器可能改为保存 B；异步写盘成功后又以当前 `tab.content` 标记已保存，期间的新输入也可能被误标记。不能移植到 Noema 的文件保存边界。
2. [`useMarkdownRenderer.ts:83-93`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L83-L93) 在 Markdown 解析前对整篇源码执行 `[[...]]` 正则替换，代码围栏/行内代码里的样例也会被改写；标签又在渲染后的文本 HTML 上替换。Noema 应继续用语法节点和索引，不采用此预处理。
3. [`App.vue:72-83`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/App.vue#L72-L83) 的 Kanban 改动用 `content.replace(original, next)` 回写第一个匹配字符串。重复代码块、同时编辑或异步重渲染后无法确定块身份；Noema 的任务写入必须走现有 scoped owner 和版本约束。
4. [`lib.rs:311-319`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src-tauri/src/lib.rs#L311-L319) 直接以 SHA-256(密码 + 固定盐) 当 AES 密钥，没有针对口令猜测的慢 KDF 或逐 Vault 随机盐；[`lib.rs:416-421`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src-tauri/src/lib.rs#L416-L421) 的临时文件名固定为 `path.mwtmp`，并发写入会互相覆盖。不能因 README 标称 AES-256-GCM 就视为可迁入的加密层。
5. [`document.ts:103-132`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/document.ts#L103-L132) 的 `markSaved` 操作当前活动标签；`loadFromPath` 重开同一路径时无条件用磁盘文本覆盖已有标签，即使它有未保存修改。[`useFileSystem.ts:25-60`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useFileSystem.ts#L25-L60) 在 `await` 前捕获标签，写盘后却调用无标签参数的 `markSaved`：切标签或继续编辑期间，可能把别的标签或尚未写盘的新版内容标为已保存。
6. [`TabBar.vue:18-28`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/TabBar.vue#L18-L28) 在用户选择“保存”后不检查 `fs.save()` 的布尔结果，保存失败或取消另存为也会直接关闭脏标签。Noema 不应采用这一关闭流程。
7. [`QuickOpenPanel.vue:45-53`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/QuickOpenPanel.vue#L45-L53)、[`FilesTab.vue:37-44`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/sidebar/FilesTab.vue#L37-L44) 直接调用 `readTextFile`，没有走 `vault.readDocument`。这些入口无法按 Vault 流程自动解密，还会触发第 5 项的覆盖问题。
8. [`EditorPane.vue:28-82`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/EditorPane.vue#L28-L82) 仅保存滚动和选区；切标签时为新文档创建全新的 `EditorState` 并调用 `view.setState`。注释所称“撤销历史按 Tab 保留”与实现不符。主题切换调用 `rebuild()` 也会重建状态。[`EditorPane.vue:204-221`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/EditorPane.vue#L204-L221) 的“切换注释”只插入 `<!--`，不插入结束标记。
9. [`App.vue:196-236`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/App.vue#L196-L236) 一开始阻止窗口关闭，却在 `finally` 中无条件销毁窗口；用户选“保存”时，未命名草稿、加密未解锁或写盘失败都没有阻止退出。测试没有覆盖这条会丢稿的路径。
10. [`GlobalSearchPanel.vue:22-45`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/GlobalSearchPanel.vue#L22-L45) 只搜活动文档，每次命中都从文首切片数行；大量命中时累计开销较高。[`GlobalSearchPanel.vue:74-102`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/GlobalSearchPanel.vue#L74-L102) 的“替换”和“全部替换”都执行全局替换，前者并非单次替换。
11. [`useVault.ts:96-109`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useVault.ts#L96-L109) 把所谓工作区加密的校验值写进应用全局设置，设置新口令时也没有重加密既有文件。与 Noema 的文件/项目边界不符，不能直接借用其模型。
12. [`useImageArchive.ts:62-90`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useImageArchive.ts#L62-L90) 以秒级时间戳和原名命名资产；同一秒重复粘贴同名图片会落在同一路径。写盘后通过全局事件在**当时**的活动编辑器与光标插入 Markdown，异步期间切标签或移动光标会插错位置。Noema 已有自己的资产与异步定位流程。

## 测试证据与覆盖缺口

- 上游在本机 `npm ci --ignore-scripts` 后，`npm run build` 通过。初次 `npm test -- --run` 得到 169 通过、6 失败（175 个），其中 4 个失败伴随 Node 26.7 的 `localStorage` 不可用警告。用项目内已有的 Node 26.5、为测试进程提供全新 `--localstorage-file` 后重跑，结果为 **173 通过、2 失败**；只剩 `exportPng` 测试仍假定旧的 `htmlToPng` 返回契约：成功分支 mock 返回字符串，失败分支返回 `null`，实现却解构 `{ dataUrl, reason }`。见 [`exportPng.test.ts:51-70`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/exportPng.test.ts#L51-L70) 与 [`useExportPng.ts:135-143`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExportPng.ts#L135-L143)。
- 32 个测试文件覆盖解析器、store 和部分文件操作；没有针对自动保存期间切标签/继续输入、保存失败后关闭、加密文件快速打开、每标签撤销历史、窗口关闭时保存失败的测试。上述问题由实际调用路径确认，不依赖测试失败的推断。

## 已落地与验证

- 斜杠菜单：[`hint-core.ts`](../site-lisp/noema/src/hint-core.ts) 允许中日韩文字后直接触发 `/`；[`main.ts`](../site-lisp/noema/aaronnote/main.ts) 的 `QUICK_INSERT_ALIASES` 补拼音首字母；[`editor-api.ts`](../site-lisp/noema/src/editor-api.ts) 新增四至六级标题与 `insert-horizontal-rule`。
- 顺带修复：快速插入注册表原先在筛选**之前**截断为 18 项，拼音/别名筛选无法命中排在后面的条目（如 `org-env-note`）；现在由弹窗在筛选后截断。
- 覆盖：[`hint-core.test.ts`](../site-lisp/noema/tests/hint-core.test.ts)（中日韩后触发、`、` 不误触）、[`cm6/commands.test.ts`](../site-lisp/noema/tests/cm6/commands.test.ts)（分割线四种位置、所有条目可达）。
- 成对行内 HTML：[`live-preview-paired-html.test.ts`](../site-lisp/noema/tests/live-preview-paired-html.test.ts) 覆盖 kbd/sub/sup/mark、未闭合与错配、代码内不标记。
- Noema 完整测试 `make test`（Node 26.5）：255 个测试文件通过、7 个跳过；2569 项通过、16 项跳过；`tsc --noEmit`、`make install` 通过。`make build`、`make install`、内核 `go test -tags fts5 ./...`、AaronEmacs `make research-test` 与 `make jupyter-test` 均通过。`git diff --check` 通过。构建仍提示已有的字体路径与大 chunk 警告。

本次吸收斜杠菜单的中文细节、成对行内 HTML 渲染与打印分页；首轮合并的阅读时长按用户要求撤回。未把 MarkWright 的桌面壳、统计数据库或粗粒度 Markdown 处理并入 Noema。

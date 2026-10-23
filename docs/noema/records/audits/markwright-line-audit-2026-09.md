# MarkWright 固定提交的逐文件行号清单

上游提交：`18a0b28e4b409814db80289952ba37f8b7932a31`。本清单覆盖 `src/`、`src-tauri/src/` 和 `tests/` 下 114 个可执行源码/测试文件，共 17,069 行。链接的 `L1-LN` 给出每个文件的完整行号范围；取舍理由见主报告。排除锁文件、图标、生成物、项目配置和说明文档。

## 实现文件（82 个）

| 完整行号范围 | 行数 | 对 Noema 的判断 |
| --- | ---: | --- |
| [`src/App.vue:1-349`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/App.vue#L1-L349) | 349 | 入口、生命周期、字数跟踪、关闭窗口、Kanban 回写；相关，见风险 3/9 |
| [`src/components/CommandPalette.vue:1-201`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/CommandPalette.vue#L1-L201) | 201 | 命令面板 UI；Noema/Emacs 已有命令入口 |
| [`src/components/EditorPane.vue:1-316`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/EditorPane.vue#L1-L316) | 316 | CM6 状态、文件切换、编辑命令；相关，见风险 8 |
| [`src/components/ExportMenu.vue:1-156`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/ExportMenu.vue#L1-L156) | 156 | 导出菜单；Noema 的导出入口已有 |
| [`src/components/FocusOverlay.vue:1-271`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/FocusOverlay.vue#L1-L271) | 271 | 专注模式和番茄钟；不需要 |
| [`src/components/FsNodeView.tsx:1-71`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/FsNodeView.tsx#L1-L71) | 71 | 递归文件树节点；Emacs/Noema 已有导航 |
| [`src/components/GlobalSearchPanel.vue:1-194`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/GlobalSearchPanel.vue#L1-L194) | 194 | 当前文档搜索替换；Noema 已有，见风险 10 |
| [`src/components/JumpLinePanel.vue:1-86`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/JumpLinePanel.vue#L1-L86) | 86 | 按行跳转；Emacs/CM6 已有 |
| [`src/components/LinksPanel.vue:1-183`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/LinksPanel.vue#L1-L183) | 183 | 反链/标签 UI；Noema Wiki/Graph 已有，加密读取路径不一致 |
| [`src/components/MenuBar.vue:1-189`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/MenuBar.vue#L1-L189) | 189 | Tauri 菜单壳；不需要 |
| [`src/components/OutlinePanel.vue:1-209`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/OutlinePanel.vue#L1-L209) | 209 | 标题目录 UI；Noema 大纲已有 |
| [`src/components/PreviewPane.vue:1-337`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/PreviewPane.vue#L1-L337) | 337 | 双栏 HTML 预览与图片路径解析；Noema 原位预览/资产已有 |
| [`src/components/QuickOpenPanel.vue:1-154`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/QuickOpenPanel.vue#L1-L154) | 154 | 工作区快速打开；Emacs/Noema 已有，见风险 7 |
| [`src/components/ReadabilityPanel.vue:1-155`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/ReadabilityPanel.vue#L1-L155) | 155 | 可读性评分和阅读时长 UI；均不合并（阅读时长用户不需要） |
| [`src/components/SettingsPanel.vue:1-119`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/SettingsPanel.vue#L1-L119) | 119 | 设置容器；Emacs/Noema 配置已有 |
| [`src/components/ShortcutHint.vue:1-115`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/ShortcutHint.vue#L1-L115) | 115 | 快捷键提示；Emacs 帮助/键绑定已有 |
| [`src/components/Sidebar.vue:1-203`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/Sidebar.vue#L1-L203) | 203 | 桌面侧栏组合；不需要 |
| [`src/components/StatsDashboard.vue:1-95`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/StatsDashboard.vue#L1-L95) | 95 | 写作习惯仪表盘；不需要 |
| [`src/components/StatusBar.vue:1-163`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/StatusBar.vue#L1-L163) | 163 | 字数、行列、保存状态；Noema HUD 已有；阅读时长用户不需要 |
| [`src/components/TabBar.vue:1-304`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/TabBar.vue#L1-L304) | 304 | 标签切换/关闭；Emacs 缓冲区已有，见风险 6 |
| [`src/components/TemplatePanel.vue:1-144`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/TemplatePanel.vue#L1-L144) | 144 | 日记模板 UI；Noema 片段/模板已有 |
| [`src/components/TitleBar.vue:1-120`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/TitleBar.vue#L1-L120) | 120 | Tauri 窗口标题栏；不需要 |
| [`src/components/ToastHost.vue:1-95`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/ToastHost.vue#L1-L95) | 95 | 通知 UI；Noema/Emacs 已有 |
| [`src/components/ToolBar.vue:1-116`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/ToolBar.vue#L1-L116) | 116 | Markdown 格式按钮；Noema 编辑命令已有 |
| [`src/components/VaultDialog.vue:1-385`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/VaultDialog.vue#L1-L385) | 385 | Vault 操作 UI；不合并，见风险 11 |
| [`src/components/VaultUnlockDialog.vue:1-175`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/VaultUnlockDialog.vue#L1-L175) | 175 | Vault 解锁 UI；不合并 |
| [`src/components/VirtualList.vue:1-112`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/VirtualList.vue#L1-L112) | 112 | 通用虚拟列表；Noema 无需此独立组件 |
| [`src/components/WhiteNoisePanel.vue:1-141`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/WhiteNoisePanel.vue#L1-L141) | 141 | 环境音 UI；不需要 |
| [`src/components/settings/AboutTab.vue:1-52`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/settings/AboutTab.vue#L1-L52) | 52 | 产品信息；不需要 |
| [`src/components/settings/EditorTab.vue:1-94`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/settings/EditorTab.vue#L1-L94) | 94 | 编辑器偏好；Noema 现有设置已有 |
| [`src/components/settings/FontTab.vue:1-130`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/settings/FontTab.vue#L1-L130) | 130 | 字体偏好；Emacs/Noema 已有 |
| [`src/components/settings/ThemeTab.vue:1-130`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/settings/ThemeTab.vue#L1-L130) | 130 | 配色偏好；Emacs/Noema 已有 |
| [`src/components/sidebar/DashboardGoals.vue:1-100`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/sidebar/DashboardGoals.vue#L1-L100) | 100 | 每日目标；不需要 |
| [`src/components/sidebar/DashboardHeatmap.vue:1-127`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/sidebar/DashboardHeatmap.vue#L1-L127) | 127 | 写作热力图；不需要 |
| [`src/components/sidebar/DashboardOverview.vue:1-149`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/sidebar/DashboardOverview.vue#L1-L149) | 149 | 写作统计概览；不需要 |
| [`src/components/sidebar/FilesTab.vue:1-286`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/components/sidebar/FilesTab.vue#L1-L286) | 286 | 文件树 CRUD；Noema/Emacs 已有，明文读取绕过 Vault |
| [`src/composables/useAutoSave.ts:1-40`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useAutoSave.ts#L1-L40) | 40 | 自动保存；相关但不能移植，见风险 1 |
| [`src/composables/useCommands.ts:1-196`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useCommands.ts#L1-L196) | 196 | 命令目录/全局键位；Noema/Emacs 已有 |
| [`src/composables/useEditor.ts:1-175`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useEditor.ts#L1-L175) | 175 | CM6 扩展和格式化；Noema 已有 |
| [`src/composables/useEditorActions.ts:1-78`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useEditorActions.ts#L1-L78) | 78 | 工具栏格式化；Noema 已有 |
| [`src/composables/useExport.ts:1-158`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExport.ts#L1-L158) | 158 | HTML/PDF/富文本复制；HTML 已有，跨宿主剪贴板需另行设计 |
| [`src/composables/useExportDocx.ts:1-225`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExportDocx.ts#L1-L225) | 225 | 简化 DOCX 转换；研究文档不保真，不合并 |
| [`src/composables/useExportPng.ts:1-162`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useExportPng.ts#L1-L162) | 162 | PNG 出图；当前工作流不需要，见测试失效 |
| [`src/composables/useFileSystem.ts:1-165`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useFileSystem.ts#L1-L165) | 165 | 打开/保存/另存；相关但不能移植，见风险 2 |
| [`src/composables/useHtmlToPng.ts:1-207`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useHtmlToPng.ts#L1-L207) | 207 | foreignObject/Canvas 光栅化；当前工作流不需要 |
| [`src/composables/useImageArchive.ts:1-95`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useImageArchive.ts#L1-L95) | 95 | 图片归档；Noema paste/资产已有 |
| [`src/composables/useKanban.ts:1-317`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useKanban.ts#L1-L317) | 317 | 可编辑看板；Noema Agenda 模型不同，不合并 |
| [`src/composables/useKanbanStyle.ts:1-98`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useKanbanStyle.ts#L1-L98) | 98 | 看板样式；不需要 |
| [`src/composables/useListRenumber.ts:1-175`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useListRenumber.ts#L1-L175) | 175 | 有序列表重编号；Noema 语法树实现更优，保留渲染一致的编号策略 |
| [`src/composables/useLocalImage.ts:1-58`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useLocalImage.ts#L1-L58) | 58 | 本地图候选路径；Noema 资产系统已有 |
| [`src/composables/useMarkdownRenderer.ts:1-131`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useMarkdownRenderer.ts#L1-L131) | 131 | Markdown 扩展渲染；Noema 已有，见风险 4 |
| [`src/composables/useOutline.ts:1-122`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useOutline.ts#L1-L122) | 122 | 正则标题索引；Noema 语法树索引已有 |
| [`src/composables/usePomodoro.ts:1-172`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/usePomodoro.ts#L1-L172) | 172 | 番茄钟；不需要 |
| [`src/composables/useReadability.ts:1-65`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useReadability.ts#L1-L65) | 65 | 阅读时长+可读性分数；均不合并 |
| [`src/composables/useRenderer.ts:1-225`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useRenderer.ts#L1-L225) | 225 | Mermaid/KaTeX/Chart/Kanban；前二已有，后二不需要 |
| [`src/composables/useScrollSync.ts:1-66`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useScrollSync.ts#L1-L66) | 66 | 双栏滚动同步；Noema 原位预览不需要 |
| [`src/composables/useSlashMenu.ts:1-310`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useSlashMenu.ts#L1-L310) | 310 | 斜杠菜单；已合并中日韩后触发、拼音首字母、h4-h6 与分割线（修正 setext 风险） |
| [`src/composables/useVault.ts:1-282`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useVault.ts#L1-L282) | 282 | 加密读写入口；不合并，见风险 11 |
| [`src/composables/useWhiteNoise.ts:1-212`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useWhiteNoise.ts#L1-L212) | 212 | 环境音；不需要 |
| [`src/composables/useWorkspaceActions.ts:1-71`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/composables/useWorkspaceActions.ts#L1-L71) | 71 | 桌面工作区切换；Emacs/Noema 已有 |
| [`src/data/themes.ts:1-762`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/data/themes.ts#L1-L762) | 762 | 内置主题数据；不需要复制 |
| [`src/main.ts:1-16`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/main.ts#L1-L16) | 16 | Vue/Pinia 入口；宿主不同 |
| [`src/stores/busy.ts:1-36`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/busy.ts#L1-L36) | 36 | 全局忙碌状态；Noema/Emacs 已有 |
| [`src/stores/commands.ts:1-87`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/commands.ts#L1-L87) | 87 | 命令面板状态和过滤；Noema/Emacs 已有 |
| [`src/stores/document.ts:1-251`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/document.ts#L1-L251) | 251 | 标签/脏状态；相关，见风险 5 |
| [`src/stores/font.ts:1-147`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/font.ts#L1-L147) | 147 | 字体持久化；Emacs/Noema 已有 |
| [`src/stores/links.ts:1-278`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/links.ts#L1-L278) | 278 | 反链/标签工作区扫描；Noema Wiki/Graph 索引已有 |
| [`src/stores/session.ts:1-128`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/session.ts#L1-L128) | 128 | 桌面会话 UI 恢复；宿主不同 |
| [`src/stores/stats.ts:1-194`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/stats.ts#L1-L194) | 194 | SQLite 写作增量/目标；不需要独立统计数据库 |
| [`src/stores/templates.ts:1-153`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/templates.ts#L1-L153) | 153 | 日记模板数据；Noema 模板已有 |
| [`src/stores/theme.ts:1-118`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/theme.ts#L1-L118) | 118 | 主题持久化；Emacs/Noema 已有 |
| [`src/stores/toast.ts:1-56`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/toast.ts#L1-L56) | 56 | 通知队列；Noema/Emacs 已有 |
| [`src/stores/ui.ts:1-140`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/ui.ts#L1-L140) | 140 | 分栏/侧栏/自动保存设置；宿主不同 |
| [`src/stores/workspace.ts:1-248`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/stores/workspace.ts#L1-L248) | 248 | Tauri 文件树；Emacs/Noema 工作区已有 |
| [`src/styles/base.css:1-114`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/base.css#L1-L114) | 114 | 桌面应用全局样式；不需要 |
| [`src/styles/content-themes.css:1-693`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/content-themes.css#L1-L693) | 693 | 预览主题样式；Noema 自有主题，不复制 |
| [`src/styles/editor.css:1-65`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/editor.css#L1-L65) | 65 | 桌面 CM6 样式；Noema 自有编辑样式 |
| [`src/styles/preview.css:1-397`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/preview.css#L1-L397) | 397 | 双栏预览样式；Noema 原位预览不需要 |
| [`src/styles/tokens.css:1-292`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/styles/tokens.css#L1-L292) | 292 | 主题 token；Emacs/Noema 已有 |
| [`src/types/shim.d.ts:1-16`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src/types/shim.d.ts#L1-L16) | 16 | Vue/资源类型声明；宿主不同 |
| [`src-tauri/src/lib.rs:1-870`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src-tauri/src/lib.rs#L1-L870) | 870 | Tauri/SQLite/加密/IO 后端；宿主不同，见风险 4/11 |
| [`src-tauri/src/main.rs:1-6`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/src-tauri/src/main.rs#L1-L6) | 6 | Tauri 启动入口；不需要 |

## 测试文件（32 个）

| 完整行号范围 | 行数 | 覆盖主题 |
| --- | ---: | --- |
| [`tests/helpers/withSetup.ts:1-19`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/helpers/withSetup.ts#L1-L19) | 19 | 测试环境/辅助函数；详见主报告的覆盖缺口 |
| [`tests/setup.ts:1-67`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/setup.ts#L1-L67) | 67 | 测试环境/辅助函数；详见主报告的覆盖缺口 |
| [`tests/unit/exportDocx.test.ts:1-63`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/exportDocx.test.ts#L1-L63) | 63 | exportDocx；详见主报告的覆盖缺口 |
| [`tests/unit/exportPng.test.ts:1-70`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/exportPng.test.ts#L1-L70) | 70 | exportPng；详见主报告的覆盖缺口 |
| [`tests/unit/kanban.test.ts:1-45`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/kanban.test.ts#L1-L45) | 45 | kanban；详见主报告的覆盖缺口 |
| [`tests/unit/markdown.test.ts:1-54`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/markdown.test.ts#L1-L54) | 54 | markdown；详见主报告的覆盖缺口 |
| [`tests/unit/outline.test.ts:1-45`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/outline.test.ts#L1-L45) | 45 | outline；详见主报告的覆盖缺口 |
| [`tests/unit/pomodoro.test.ts:1-117`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/pomodoro.test.ts#L1-L117) | 117 | pomodoro；详见主报告的覆盖缺口 |
| [`tests/unit/readability.test.ts:1-48`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/readability.test.ts#L1-L48) | 48 | readability；详见主报告的覆盖缺口 |
| [`tests/unit/store.busy.test.ts:1-34`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.busy.test.ts#L1-L34) | 34 | store/busy；详见主报告的覆盖缺口 |
| [`tests/unit/store.commands.test.ts:1-48`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.commands.test.ts#L1-L48) | 48 | store/commands；详见主报告的覆盖缺口 |
| [`tests/unit/store.document.test.ts:1-144`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.document.test.ts#L1-L144) | 144 | store/document；详见主报告的覆盖缺口 |
| [`tests/unit/store.links.test.ts:1-65`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.links.test.ts#L1-L65) | 65 | store/links；详见主报告的覆盖缺口 |
| [`tests/unit/store.session.test.ts:1-65`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.session.test.ts#L1-L65) | 65 | store/session；详见主报告的覆盖缺口 |
| [`tests/unit/store.stats.test.ts:1-61`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.stats.test.ts#L1-L61) | 61 | store/stats；详见主报告的覆盖缺口 |
| [`tests/unit/store.templates.test.ts:1-38`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.templates.test.ts#L1-L38) | 38 | store/templates；详见主报告的覆盖缺口 |
| [`tests/unit/store.theme.test.ts:1-35`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.theme.test.ts#L1-L35) | 35 | store/theme；详见主报告的覆盖缺口 |
| [`tests/unit/store.toast.test.ts:1-50`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.toast.test.ts#L1-L50) | 50 | store/toast；详见主报告的覆盖缺口 |
| [`tests/unit/store.ui.test.ts:1-56`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.ui.test.ts#L1-L56) | 56 | store/ui；详见主报告的覆盖缺口 |
| [`tests/unit/store.workspace.test.ts:1-112`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/store.workspace.test.ts#L1-L112) | 112 | store/workspace；详见主报告的覆盖缺口 |
| [`tests/unit/useCommands.test.ts:1-53`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useCommands.test.ts#L1-L53) | 53 | use Commands；详见主报告的覆盖缺口 |
| [`tests/unit/useFileSystem.test.ts:1-44`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useFileSystem.test.ts#L1-L44) | 44 | use FileSystem；详见主报告的覆盖缺口 |
| [`tests/unit/useImageArchive.test.ts:1-80`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useImageArchive.test.ts#L1-L80) | 80 | use ImageArchive；详见主报告的覆盖缺口 |
| [`tests/unit/useImageRender.test.ts:1-34`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useImageRender.test.ts#L1-L34) | 34 | use ImageRender；详见主报告的覆盖缺口 |
| [`tests/unit/useListRenumber.plugin.test.ts:1-56`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useListRenumber.plugin.test.ts#L1-L56) | 56 | use ListRenumber.plugin；详见主报告的覆盖缺口 |
| [`tests/unit/useListRenumber.test.ts:1-83`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useListRenumber.test.ts#L1-L83) | 83 | use ListRenumber；详见主报告的覆盖缺口 |
| [`tests/unit/useLocalImage.test.ts:1-78`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useLocalImage.test.ts#L1-L78) | 78 | use LocalImage；详见主报告的覆盖缺口 |
| [`tests/unit/useRenderer.test.ts:1-11`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useRenderer.test.ts#L1-L11) | 11 | use Renderer；详见主报告的覆盖缺口 |
| [`tests/unit/useSlashMenu.test.ts:1-112`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useSlashMenu.test.ts#L1-L112) | 112 | use SlashMenu；详见主报告的覆盖缺口 |
| [`tests/unit/useVault.test.ts:1-49`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useVault.test.ts#L1-L49) | 49 | use Vault；详见主报告的覆盖缺口 |
| [`tests/unit/useWhiteNoise.test.ts:1-27`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/useWhiteNoise.test.ts#L1-L27) | 27 | use WhiteNoise；详见主报告的覆盖缺口 |
| [`tests/unit/vault.secure.test.ts:1-108`](https://github.com/okazaki112/MarkWright/blob/18a0b28e4b409814db80289952ba37f8b7932a31/tests/unit/vault.secure.test.ts#L1-L108) | 108 | vault.secure；详见主报告的覆盖缺口 |

所有行号取自固定提交的工作树 `wc -l`；代码结论由主报告中的具体行号和调用路径支持。这里的完整范围用于逐文件核对，不表示每一行都有独立功能或独立合并价值。

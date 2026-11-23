# NocoDB 逐文件清点

由同目录的 `nocodb_files.py` 生成（`python3 -I nocodb_files.py upstream OUT.tsv nocodb-files.md`），覆盖 `packages/` 下全部 ts/vue/js 源文件（测试、类型声明、配置与构建产物除外）。每个文件都被读入并检查：行数、导出名、头部注释、是否为社区版空壳（小文件且方法体都是 `..._args`/`return null`/`() => {}` 一类）。人工结论见 [inventory.md](inventory.md) 与 [README](README.md)。

## 按包

| 包 | 文件 | 行数 | 空壳 |
| --- | ---: | ---: | ---: |
| `nc-gui` | 1757 | 357021 | 17 |
| `nocodb` | 1418 | 326490 | 31 |
| `nocodb-sdk` | 275 | 87413 | 0 |
| `noco-integrations` | 41 | 7240 | 0 |
| `nocodb-sdk-v2` | 7 | 3677 | 0 |
| `nc-secret-mgr` | 8 | 561 | 0 |
| `nc-integration-scaffolder` | 1 | 424 | 0 |
| `nc-lib-gui` | 1 | 14 | 0 |
| 合计 | 3508 | 782840 | 48 |

## 按目录（前三级）

| 目录 | 文件 | 行数 | 空壳 |
| --- | ---: | ---: | ---: |
| `nc-gui` | 3 | 236 | 0 |
| `nc-gui/assets` | 7 | 623 | 0 |
| `nc-gui/components/account` | 36 | 5099 | 0 |
| `nc-gui/components/actions` | 1 | 4 | 0 |
| `nc-gui/components/admin` | 13 | 875 | 0 |
| `nc-gui/components/agent` | 1 | 4 | 0 |
| `nc-gui/components/ai` | 10 | 1546 | 0 |
| `nc-gui/components/api-client` | 2 | 270 | 0 |
| `nc-gui/components/auth` | 19 | 2531 | 0 |
| `nc-gui/components/bookmarks` | 3 | 12 | 0 |
| `nc-gui/components/cell` | 108 | 23347 | 0 |
| `nc-gui/components/chat` | 2 | 8 | 0 |
| `nc-gui/components/cmd-footer` | 1 | 58 | 0 |
| `nc-gui/components/cmd-j` | 1 | 249 | 0 |
| `nc-gui/components/cmd-k` | 5 | 1993 | 0 |
| `nc-gui/components/cmd-l` | 1 | 297 | 0 |
| `nc-gui/components/connect-ai` | 2 | 10 | 0 |
| `nc-gui/components/dashboard` | 84 | 15901 | 0 |
| `nc-gui/components/dlg` | 66 | 13953 | 0 |
| `nc-gui/components/erd` | 8 | 1152 | 0 |
| `nc-gui/components/extensions` | 9 | 1723 | 0 |
| `nc-gui/components/feed` | 12 | 935 | 0 |
| `nc-gui/components/general` | 58 | 5557 | 0 |
| `nc-gui/components/monaco` | 6 | 1909 | 0 |
| `nc-gui/components/n` | 3 | 907 | 0 |
| `nc-gui/components/nc` | 115 | 22247 | 0 |
| `nc-gui/components/notification` | 15 | 406 | 0 |
| `nc-gui/components/payment` | 13 | 159 | 0 |
| `nc-gui/components/permissions` | 2 | 47 | 0 |
| `nc-gui/components/pierre` | 3 | 596 | 0 |
| `nc-gui/components/project` | 24 | 2407 | 0 |
| `nc-gui/components/rls` | 1 | 19 | 1 |
| `nc-gui/components/roles` | 3 | 625 | 0 |
| `nc-gui/components/share` | 6 | 471 | 0 |
| `nc-gui/components/shared-view` | 10 | 447 | 0 |
| `nc-gui/components/shell` | 8 | 749 | 0 |
| `nc-gui/components/skill` | 6 | 24 | 0 |
| `nc-gui/components/smartsheet` | 360 | 111484 | 0 |
| `nc-gui/components/tabs` | 2 | 517 | 0 |
| `nc-gui/components/template` | 2 | 1763 | 0 |
| `nc-gui/components/tour` | 3 | 283 | 0 |
| `nc-gui/components/virtual-cell` | 22 | 7369 | 0 |
| `nc-gui/components/webhook` | 12 | 4359 | 0 |
| `nc-gui/components/workspace` | 61 | 10909 | 0 |
| `nc-gui/composables` | 166 | 40062 | 14 |
| `nc-gui/composables/useApi` | 3 | 527 | 0 |
| `nc-gui/composables/useCommandPalette` | 12 | 3314 | 0 |
| `nc-gui/composables/useDialog` | 1 | 150 | 0 |
| `nc-gui/composables/useExpandedFormDetached` | 1 | 58 | 0 |
| `nc-gui/composables/useFormBuilder` | 1 | 536 | 0 |
| `nc-gui/composables/useGlobal` | 5 | 894 | 0 |
| `nc-gui/composables/useInjectionState` | 1 | 41 | 0 |
| `nc-gui/composables/useMenuCloseOnEsc` | 1 | 29 | 0 |
| `nc-gui/composables/useMultiSelect` | 3 | 2145 | 0 |
| `nc-gui/composables/usePlugin` | 2 | 432 | 0 |
| `nc-gui/composables/useRoles` | 1 | 271 | 0 |
| `nc-gui/composables/useSelectedCellKeyupListener` | 1 | 70 | 0 |
| `nc-gui/composables/useSidebar` | 1 | 81 | 0 |
| `nc-gui/composables/viewRowColor` | 2 | 72 | 0 |
| `nc-gui/context` | 1 | 612 | 0 |
| `nc-gui/error` | 5 | 10 | 0 |
| `nc-gui/extensions/data-exporter` | 1 | 798 | 0 |
| `nc-gui/extensions/json-exporter` | 1 | 114 | 0 |
| `nc-gui/helpers` | 11 | 982 | 0 |
| `nc-gui/helpers/parsers` | 7 | 1521 | 0 |
| `nc-gui/helpers/tiptap` | 10 | 726 | 0 |
| `nc-gui/helpers/tiptap-markdown` | 61 | 4459 | 0 |
| `nc-gui/layouts` | 8 | 802 | 0 |
| `nc-gui/lib` | 12 | 4661 | 0 |
| `nc-gui/middleware` | 4 | 520 | 0 |
| `nc-gui/modules` | 4 | 493 | 0 |
| `nc-gui/pages` | 6 | 836 | 0 |
| `nc-gui/pages/account` | 7 | 357 | 0 |
| `nc-gui/pages/admin` | 1 | 337 | 0 |
| `nc-gui/pages/error` | 1 | 11 | 0 |
| `nc-gui/pages/index` | 30 | 2450 | 0 |
| `nc-gui/pages/invite` | 1 | 377 | 0 |
| `nc-gui/pages/leaving` | 1 | 11 | 0 |
| `nc-gui/pages/oauth` | 1 | 11 | 0 |
| `nc-gui/pages/playground` | 81 | 13503 | 0 |
| `nc-gui/pages/projects` | 3 | 327 | 0 |
| `nc-gui/pages/reset` | 1 | 136 | 0 |
| `nc-gui/pages/signup` | 1 | 248 | 0 |
| `nc-gui/plugins` | 25 | 1265 | 0 |
| `nc-gui/public/js` | 1 | 2 | 0 |
| `nc-gui/store` | 29 | 5672 | 2 |
| `nc-gui/store/ui` | 1 | 128 | 0 |
| `nc-gui/tours` | 2 | 228 | 0 |
| `nc-gui/tours/defs` | 1 | 122 | 0 |
| `nc-gui/utils` | 106 | 22188 | 0 |
| `nc-gui/utils/search` | 2 | 141 | 0 |
| `nc-gui/workers` | 1 | 211 | 0 |
| `nc-integration-scaffolder/src` | 1 | 424 | 0 |
| `nc-lib-gui/lib` | 1 | 14 | 0 |
| `nc-secret-mgr` | 1 | 83 | 0 |
| `nc-secret-mgr/src` | 1 | 73 | 0 |
| `nc-secret-mgr/src/core` | 5 | 381 | 0 |
| `nc-secret-mgr/src/nocodb` | 1 | 24 | 0 |
| `noco-integrations` | 1 | 227 | 0 |
| `noco-integrations/core/src` | 39 | 6626 | 0 |
| `noco-integrations/scripts` | 1 | 387 | 0 |
| `nocodb` | 1 | 105 | 0 |
| `nocodb-sdk-v2/script` | 1 | 16 | 0 |
| `nocodb-sdk-v2/src` | 1 | 2 | 0 |
| `nocodb-sdk-v2/src/lib` | 1 | 3543 | 0 |
| `nocodb-sdk-v2/src/sdk` | 4 | 116 | 0 |
| `nocodb-sdk/build-script` | 2 | 79 | 0 |
| `nocodb-sdk/src` | 1 | 2 | 0 |
| `nocodb-sdk/src/lib` | 272 | 87332 | 0 |
| `nocodb/build-utils` | 3 | 554 | 0 |
| `nocodb/src` | 9 | 1072 | 0 |
| `nocodb/src/cache` | 6 | 1657 | 0 |
| `nocodb/src/command-registry` | 4 | 957 | 0 |
| `nocodb/src/constants` | 2 | 115 | 0 |
| `nocodb/src/controllers` | 80 | 11283 | 0 |
| `nocodb/src/db` | 207 | 82676 | 0 |
| `nocodb/src/dbQueryClient` | 20 | 3795 | 0 |
| `nocodb/src/decorators` | 6 | 365 | 1 |
| `nocodb/src/filters` | 2 | 473 | 0 |
| `nocodb/src/gateways` | 2 | 135 | 0 |
| `nocodb/src/guards` | 7 | 214 | 0 |
| `nocodb/src/helpers` | 180 | 27757 | 0 |
| `nocodb/src/integrations` | 2 | 81 | 0 |
| `nocodb/src/interceptors` | 2 | 234 | 0 |
| `nocodb/src/interface` | 7 | 1375 | 0 |
| `nocodb/src/mcp` | 21 | 3839 | 1 |
| `nocodb/src/meta` | 269 | 22374 | 0 |
| `nocodb/src/middlewares` | 8 | 2135 | 0 |
| `nocodb/src/models` | 97 | 30715 | 28 |
| `nocodb/src/modules` | 114 | 35409 | 0 |
| `nocodb/src/plugins` | 65 | 4542 | 0 |
| `nocodb/src/providers` | 2 | 217 | 0 |
| `nocodb/src/public` | 6 | 17983 | 1 |
| `nocodb/src/redis` | 1 | 129 | 0 |
| `nocodb/src/run` | 9 | 735 | 0 |
| `nocodb/src/schema` | 1 | 25 | 0 |
| `nocodb/src/services` | 172 | 60187 | 0 |
| `nocodb/src/socket` | 2 | 31 | 0 |
| `nocodb/src/strategies` | 9 | 862 | 0 |
| `nocodb/src/types` | 18 | 590 | 0 |
| `nocodb/src/utils` | 69 | 11380 | 0 |
| `nocodb/src/version-upgrader` | 15 | 2489 | 0 |

## 社区版空壳（全部）

| 文件 | 行数 | 头注释 |
| --- | ---: | --- |
| `nc-gui/components/rls/PolicyList.vue` | 19 | CE stub — Record-Level Security is EE-only. Mirrors the EE component's props |
| `nc-gui/composables/useActionPane.ts` | 31 |  |
| `nc-gui/composables/useAppShareHub.ts` | 23 | CE stub: apps are EE-only, so the share modal never opens over one. */ |
| `nc-gui/composables/useBaseTrash.ts` | 54 | Per-row restore — used by data composables (undo, "restore selected"). |
| `nc-gui/composables/useCellCollabSession.ts` | 64 | False when any prerequisite is missing — the editor then behaves exactly as before. */ |
| `nc-gui/composables/useChatPanel.ts` | 47 |  |
| `nc-gui/composables/useCommentAttachments.ts` | 41 | CE stub — attaching files to comments is an EE/paid feature. In CE the UI is |
| `nc-gui/composables/useGanttViewStore.ts` | 92 | TEMPORARY CLONE — duplicated from Timeline/useTimelineViewStore on 2026-04-24. |
| `nc-gui/composables/useInterfaceShareHub.ts` | 22 | CE stub: interfaces are EE-only, so the share modal never opens over one. */ |
| `nc-gui/composables/useOrgUserInvitePicker.ts` | 19 | CE stub — org users are an EE concept. Returns an empty list so the invite |
| `nc-gui/composables/useRecordTemplate.ts` | 58 |  |
| `nc-gui/composables/useServerConfig.ts` | 16 |  |
| `nc-gui/composables/useSharedDoc.ts` | 33 | CE stub for useSharedDoc. The real implementation lives in |
| `nc-gui/composables/useSmartText.ts` | 79 | CE stub for SmartText panel composable. |
| `nc-gui/composables/useUndoRedo.ts` | 31 | Signature mirrors the EE impl; CE has no undo, so the options are inert. |
| `nc-gui/store/audits.ts` | 93 |  |
| `nc-gui/store/chat.ts` | 97 |  |
| `nocodb/src/decorators/trace-command.decorator.ts` | 80 | CE no-op stub. EE overrides with the real implementation. |
| `nocodb/src/mcp/oauth-scope-guard.ts` | 34 | CE has no scope rows, so a grant is bounded only by `granted_resources`. |
| `nocodb/src/models/Agent.ts` | 32 | CE stub. Agents are an EE feature; `src/ee/models/Agent.ts` is the real |
| `nocodb/src/models/App.ts` | 28 |  |
| `nocodb/src/models/AppVersion.ts` | 18 |  |
| `nocodb/src/models/AutomationSection.ts` | 34 |  |
| `nocodb/src/models/BaseSection.ts` | 34 |  |
| `nocodb/src/models/Bookmark.ts` | 34 |  |
| `nocodb/src/models/BookmarkGroup.ts` | 28 |  |
| `nocodb/src/models/ChatMessage.ts` | 47 |  |
| `nocodb/src/models/ChatSession.ts` | 26 |  |
| `nocodb/src/models/Dashboard.ts` | 36 |  |
| `nocodb/src/models/DateDependency.ts` | 38 |  |
| `nocodb/src/models/Document.ts` | 116 | Base-level sidebar section (EE). Root docs only — a child's effective |
| `nocodb/src/models/GanttView.ts` | 31 |  |
| `nocodb/src/models/GanttViewColumn.ts` | 42 |  |
| `nocodb/src/models/Interface.ts` | 28 |  |
| `nocodb/src/models/InterfacePage.ts` | 28 |  |
| `nocodb/src/models/ListView.ts` | 22 |  |
| `nocodb/src/models/ListViewColumn.ts` | 28 |  |
| `nocodb/src/models/ListViewLevel.ts` | 34 |  |
| `nocodb/src/models/Script.ts` | 55 |  |
| `nocodb/src/models/TimelineRange.ts` | 35 |  |
| `nocodb/src/models/TimelineView.ts` | 32 |  |
| `nocodb/src/models/TimelineViewColumn.ts` | 41 |  |
| `nocodb/src/models/ViewSection.ts` | 34 |  |
| `nocodb/src/models/Widget.ts` | 28 |  |
| `nocodb/src/models/Workflow.ts` | 40 |  |
| `nocodb/src/models/Workspace.ts` | 89 |  |
| `nocodb/src/models/WorkspaceUser.ts` | 197 |  |
| `nocodb/src/public/js/swagger-ui-bundle.js` | 2 |  |

## 与 Noema 相关主题的文件（AI、Markdown、工作流、MCP、Agent/Chat）

| 文件 | 行数 | 主题 | 空壳 | 导出 | 头注释 |
| --- | ---: | --- | :---: | --- | --- |
| `nc-gui/components/account/Mcp/Create.vue` | 4 | mcp |  |  |  |
| `nc-gui/components/account/Mcp/LegacyOptInNote.vue` | 4 | mcp |  |  |  |
| `nc-gui/components/account/Mcp/index.vue` | 506 | mcp |  |  | The account MCP surface. With `lockedBaseId` (base settings → MCP Server) |
| `nc-gui/components/agent/PersonalOversight.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/ai/Erd/Flow.vue` | 151 | ai |  |  | Center elements without changing zoom level |
| `nc-gui/components/ai/Erd/TableNode.vue` | 157 | ai |  |  |  |
| `nc-gui/components/ai/Erd/utils.ts` | 307 | ai |  | AiBaseSchema,AiERDConfig,AiNodeData,AiEdgeData,useErdElements |  |
| `nc-gui/components/ai/Erd/view.vue` | 47 | ai |  |  |  |
| `nc-gui/components/ai/IntegrationNotFound.vue` | 39 | ai |  |  |  |
| `nc-gui/components/ai/PromptWithFields.vue` | 348 | ai |  |  | Use \n to show placeholder as preline |
| `nc-gui/components/ai/Settings.vue` | 231 | ai |  |  | const vRandomness = useVModel(props, 'randomness', emits) |
| `nc-gui/components/ai/ToggleButton.vue` | 70 | ai |  |  |  |
| `nc-gui/components/ai/WizardCard.vue` | 100 | ai |  |  |  |
| `nc-gui/components/ai/WizardTabs.vue` | 96 | ai |  |  | null to hide tooltip |
| `nc-gui/components/auth/preview/Workflow.vue` | 190 | workflow |  |  | Decorative, aria-hidden copy of the marketing site's live workflow editor (landing-page components/nc/workflow.tsx): |
| `nc-gui/components/cell/AI.vue` | 245 | ai |  |  |  |
| `nc-gui/components/cell/RichText.vue` | 1054 | markdown |  |  | Ephemeral co-editing identity for this cell. All three must be set for a |
| `nc-gui/components/cell/RichText/ImageMenu.vue` | 17 | markdown |  |  |  |
| `nc-gui/components/cell/RichText/ImageNode.vue` | 120 | markdown |  |  | Width while dragging; written to the node once on release, so one drag is one undo step |
| `nc-gui/components/cell/RichText/ImageOptions.vue` | 21 | markdown |  |  |  |
| `nc-gui/components/cell/RichText/LinkOrImageOptions.vue` | 413 | markdown |  |  | Image options state |
| `nc-gui/components/cell/RichText/SelectedBubbleMenu.vue` | 891 | markdown |  |  | ── Color picker (text color + background color) — doc editor / embedMode only ── |
| `nc-gui/components/cell/RichText/SelectedBubbleMenuPopup.vue` | 104 | markdown |  |  | Debounce show menu to prevent flickering |
| `nc-gui/components/chat/BuildPlan.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/chat/Panel.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/connect-ai/Host.vue` | 2 | ai |  |  |  |
| `nc-gui/components/connect-ai/Pane.vue` | 8 | ai |  |  |  |
| `nc-gui/components/dashboard/Sidebar/ChatSupport.vue` | 34 | agent/chat |  |  | if chatwoot is not initialized, show a message |
| `nc-gui/components/dashboard/settings/base/MCP/Code.vue` | 131 | mcp |  |  | Define Monaco Editor as an async component |
| `nc-gui/components/dashboard/settings/base/MCP/Modal.vue` | 127 | mcp |  |  | A connection whose authority is its scopes names no base, so it is named after itself. */ |
| `nc-gui/components/dashboard/settings/base/MCP/Setup.vue` | 257 | mcp |  |  | Name the server after the connection's workspace and base rather than the open base. */ |
| `nc-gui/components/dashboard/settings/base/MCP/index.vue` | 298 | mcp |  |  | Base settings -> MCP Server. Takes the base explicitly, the way the API-token |
| `nc-gui/components/dlg/MCP/Delete.vue` | 56 | mcp |  |  | The account surface goes through the org-scoped route: a scoped |
| `nc-gui/components/nc/Icon/Workflow.vue` | 15 | workflow |  |  |  |
| `nc-gui/components/nc/form-builder/input/WorkflowCodeInput.vue` | 1099 | workflow |  |  | Real step title when `nodeTitle` is a display label. */ |
| `nc-gui/components/nc/form-builder/input/WorkflowInput.vue` | 2233 | workflow |  |  |  |
| `nc-gui/components/nc/form-builder/input/WorkflowInputAi.vue` | 273 | workflow |  |  | "Write with AI" popover for the email body, opened from the toolbar and the selection bubble. |
| `nc-gui/components/nc/form-builder/input/WorkflowInputAiEmptyState.vue` | 278 | workflow |  |  | Empty-body state for the email editor: a card offering "Write with AI" or "Start blank", |
| `nc-gui/components/nc/form-builder/input/WorkflowInputTools.vue` | 454 | workflow |  | WorkflowInputTool | An empty body shows its own AI prompt; the button focuses that one instead of opening a second. */ |
| `nc-gui/components/nc/form-builder/input/WorkflowTransformMenu.vue` | 274 | workflow |  |  | What the chip points at, e.g. "Name". */ |
| `nc-gui/components/project/Action/CreateEmptyWorkflow.vue` | 4 | workflow |  |  |  |
| `nc-gui/components/project/WorkflowsList.vue` | 4 | workflow |  |  |  |
| `nc-gui/components/smartsheet/column/ButtonOptions/Workflow.vue` | 4 | workflow |  |  |  |
| `nc-gui/components/smartsheet/expanded-form/RichTextOptions.vue` | 241 | markdown |  |  |  |
| `nc-gui/components/smartsheet/grid/canvas/loaders/markdownLoader.ts` | 87 | markdown |  | markdownTextCache,MarkdownParserOptions,MarkdownLoader | Sync render API |
| `nc-gui/components/smartsheet/grid/canvas/utils/markdownUtils.ts` | 416 | markdown |  | Marker,tokenizeLine,Block,parseMarkdown,getFontForToken | Optional handler to process complex markers (like links) |
| `nc-gui/components/smartsheet/topbar/Agent/Action.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/smartsheet/topbar/Agent/Info.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/smartsheet/topbar/Agent/ListDropdown.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/smartsheet/topbar/Agents.vue` | 4 | agent/chat |  |  |  |
| `nc-gui/components/smartsheet/topbar/Workflow/Action.vue` | 4 | workflow |  |  |  |
| `nc-gui/components/smartsheet/topbar/Workflow/Info.vue` | 4 | workflow |  |  |  |
| `nc-gui/components/smartsheet/topbar/Workflow/ListDropdown.vue` | 4 | workflow |  |  |  |
| `nc-gui/components/smartsheet/topbar/Workflow/State.vue` | 4 | workflow |  |  |  |
| `nc-gui/composables/useRichTextMarkdownMode.ts` | 122 | markdown |  | RichTextEditorMode,MarkdownEditState,markdownWordsMissingFrom,canEditMarkdown,useRichTextMarkdownMode | A co-editing session was started for this cell. */ |
| `nc-gui/composables/useWorkflowEmailAi.ts` | 41 | workflow |  | WorkflowEmailAiVariable,WorkflowEmailAiSuggestion,WorkflowEmailAiOptions,useWorkflowEmailAi | Show failures as a toast (default). Callers with an inline error slot pass `false`. */ |
| `nc-gui/composables/useWorkflowEmailAiSuggestions.ts` | 48 | workflow |  | useWorkflowEmailAiSuggestions | Suggestion chips for the AI email prompt: static ideas immediately, replaced by |
| `nc-gui/composables/useWorkflowEmailImages.ts` | 79 | workflow |  | useWorkflowEmailImages | Signed links outlive this by far (2h default); refreshing early avoids a broken image mid-edit. |
| `nc-gui/helpers/tiptap-markdown/Markdown.ts` | 80 | markdown |  | Markdown |  |
| `nc-gui/helpers/tiptap-markdown/extensions/defaultMarkdownExtensions.ts` | 52 | markdown |  |  | Nodes |
| `nc-gui/helpers/tiptap-markdown/extensions/emailImage.ts` | 336 | markdown |  | EMAIL_IMAGE_TYPES,EMAIL_IMAGE_EXTENSIONS,isEmailImageFile,EMAIL_IMAGE_MAX_SIZE,EMAIL_IMAGE_MAX_COUNT,EMAIL_IMAGE_MAX_TOTAL_SIZE | Types the send path turns into Content-ID parts; SVG is excluded there as a script-capable document. */ |
| `nc-gui/helpers/tiptap-markdown/extensions/emoji/EmojiSuggestionList.vue` | 91 | markdown |  |  | ":" or ":D" then Enter is someone finishing a sentence, so Enter sends until the intent is clear. |
| `nc-gui/helpers/tiptap-markdown/extensions/emoji/index.ts` | 97 | markdown |  | EmojiSuggestionOptions,EmojiSuggestionItem,EmojiSuggestion | For telemetry: the host wires its `$e` here, since this module has no Nuxt context. */ |
| `nc-gui/helpers/tiptap-markdown/extensions/index.ts` | 5 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/bold.ts` | 19 | markdown |  | Bold | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/code.ts` | 17 | markdown |  | Code | handled by markdown-it |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/fontFamily.ts` | 57 | markdown |  | EMAIL_FONTS,FontFamily | Web-safe stacks only: an email can't ship fonts, so anything else falls back unpredictably. |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/fontSize.ts` | 51 | markdown |  | EMAIL_FONT_SIZES,FontSize | "Normal" is no mark at all, so the email inherits its declared base size. |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/highlight.ts` | 71 | markdown |  | HIGHLIGHT_COLORS,Highlight | Email-safe highlight. Renders a <span> rather than <mark>: Outlook's Word engine does not |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/html.ts` | 41 | markdown |  | HTMLMark | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/index.ts` | 8 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/italic.ts` | 16 | markdown |  | Italic | handled by markdown-it |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/link.ts` | 238 | markdown |  | Link |  |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/strike.ts` | 46 | markdown |  | inputSingleTildeRegex,pasteSingleTildeRegex,Strike | Matches a strike to a ~strike~ on input. |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/textColor.ts` | 67 | markdown |  | TEXT_COLORS,TextColor | Email-safe text colour: serialises to an inline style, which is what mail clients honour. |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/textStyle.ts` | 19 | markdown |  | EmailTextStyle | TipTap's TextStyle parses *any* styled span; the colour and highlight marks also render |
| `nc-gui/helpers/tiptap-markdown/extensions/marks/underline.ts` | 22 | markdown |  | Underline | addStorage() { |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/blockquote.ts` | 20 | markdown |  | Blockquote | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/bullet-list.ts` | 37 | markdown |  | BulletList | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/code-block.ts` | 36 | markdown |  | CodeBlock | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/hard-break.ts` | 41 | markdown |  | HardBreak | handled by markdown-it |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/heading.ts` | 20 | markdown |  | Heading | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/horizontal-rule.ts` | 20 | markdown |  | HorizontalRule | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/html.ts` | 52 | markdown |  | HTMLNode | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/image.ts` | 120 | markdown |  | IMAGE_INPUT_REGEX,IMAGE_PASTE_REGEX,imagePixelWidth,imageToHTML,Image | Matches ONLY completed image syntax while typing |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/index.ts` | 17 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/list-item.ts` | 20 | markdown |  | ListItem | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/entity/EntityMentionChip.vue` | 43 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/entity/EntityMentionList.vue` | 175 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/entity/index.ts` | 78 | markdown |  | EntityMentionKind,EntityMentionItem,EntityMention | Every entity a host may allow mentioning. Hosts opt into a subset. */ |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/expression/WorkflowVariablePicker.vue` | 565 | markdown,workflow |  |  | An Iterate step whose loop body holds the step being edited. */ |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/expression/index.ts` | 105 | markdown |  | WorkflowExpression |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/field/FieldMentionList.vue` | 139 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/field/index.ts` | 100 | markdown |  | parseFieldMention,FieldMention | Todo: Update user mention to field mention |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/index.ts` | 12 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/user/UserMentionList.vue` | 140 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/mention/user/index.ts` | 140 | markdown |  | parseUserMention,UserMention | A mention can carry neither name nor email (e.g. emails withheld in a shared base). |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/ordered-list.ts` | 39 | markdown |  | OrderedList | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/paragraph.ts` | 66 | markdown |  | Paragraph | Remove all stored marks for the next typed text |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/table.ts` | 92 | markdown |  | Table | A cell holds one line: its paragraphs are joined with <br>, which loads back as line breaks |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/task-item.ts` | 325 | markdown |  | TaskItemOptions,inputRegex,PmJsonNode,TaskItem | Id of the user to credit when this editor toggles an item. Returning null |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/task-list.ts` | 46 | markdown |  | TaskList | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/nodes/text.ts` | 23 | markdown |  | Text | TODO: Extend from tiptap extension |
| `nc-gui/helpers/tiptap-markdown/extensions/textAlign.ts` | 49 | markdown |  | EmailTextAlign,TextAlign | Left is the default everywhere; emitting it would only add noise to the email. |
| `nc-gui/helpers/tiptap-markdown/extensions/tiptap/clipboard.ts` | 49 | markdown |  | MarkdownClipboard | Remove outer quotes & unescape |
| `nc-gui/helpers/tiptap-markdown/extensions/tiptap/index.ts` | 3 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/extensions/tiptap/tight-lists.ts` | 54 | markdown |  | MarkdownTightListsOptions,MarkdownTightListsOptionsStorage,MarkdownTightLists |  |
| `nc-gui/helpers/tiptap-markdown/index.ts` | 2 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/parse/MarkdownParser.ts` | 147 | markdown |  | MarkdownParser | DOMPurify's default URI allowlist plus `nc-file:`, the inert reference workflow email bodies store |
| `nc-gui/helpers/tiptap-markdown/parse/index.ts` | 2 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/serialize/MarkdownSerializer.ts` | 68 | markdown |  | MarkdownSerializer |  |
| `nc-gui/helpers/tiptap-markdown/serialize/index.ts` | 3 | markdown |  |  |  |
| `nc-gui/helpers/tiptap-markdown/serialize/state.ts` | 87 | markdown |  | MarkdownSerializerState | Override default MarkdownSerializerState to: |
| `nc-gui/helpers/tiptap-markdown/util/dom.ts` | 39 | markdown |  | elementFromString,escapeHTML,extractElement,unwrapElement | add a wrapper to preserve leading and trailing whitespace |
| `nc-gui/helpers/tiptap-markdown/util/extensions.ts` | 21 | markdown |  | getMarkdownSpec |  |
| `nc-gui/helpers/tiptap-markdown/util/imgTag.ts` | 26 | markdown |  | imgTagsToMarkdown | Stops at the next '<' so an unclosed tag can't make the scan quadratic; stored attributes escape '<' |
| `nc-gui/helpers/tiptap-markdown/util/markdown.ts` | 59 | markdown |  | shiftDelim,trimInline |  |
| `nc-gui/helpers/tiptap-markdown/util/outsideCode.ts` | 40 | markdown |  | rewriteOutsideCode | Opening code fence, also inside blockquotes and on a list item's marker line ("- ```"). |
| `nc-gui/helpers/tiptap-markdown/util/prosemirror.ts` | 6 | markdown |  | childNodes |  |
| `nc-gui/helpers/tiptap/functionality/index.ts` | 4 | markdown |  |  |  |
| `nc-gui/helpers/tiptap/functionality/markdown/index.ts` | 5 | markdown |  |  |  |
| `nc-gui/helpers/tiptap/functionality/markdown/markdown.ts` | 334 | markdown |  | NcMarkdownExtension,NcMarkdownParserConstructorType,sanitizeRichTextHtml,NcMarkdownParser | Editor-controlled HTML: nothing that loads, submits, or can be styled into an overlay by app CSS. |
| `nc-gui/helpers/tiptap/functionality/markdown/md-image-as-text-ext.ts` | 19 | markdown |  |  | Override the image rule |
| `nc-gui/helpers/tiptap/functionality/markdown/md-link-rule-setup.ts` | 54 | markdown |  |  | Check if the href contains the special separator |
| `nc-gui/helpers/tiptap/functionality/markdown/md-strike-ext.ts` | 73 | markdown |  |  | don't allow unescaped spaces/newlines inside |
| `nc-gui/helpers/tiptap/functionality/resizableImage.ts` | 11 | markdown |  | ResizableImage | The Rich Text cell's image: drag a corner handle to set its width. */ |
| `nc-gui/helpers/tiptap/functionality/suggestion.ts` | 85 | markdown |  | suggestionHandledKeyEvents | Keydown events the open list consumed. ProseMirror prevents default on every |
| `nc-gui/helpers/tiptap/functionality/table.ts` | 139 | markdown |  | richTextTableExtensions | Rich Text cells store GFM pipe tables: a header row, text-only cells, no merged cells. |
| `nc-gui/helpers/tiptap/index.ts` | 2 | markdown |  |  |  |
| `nc-gui/helpers/workflowEmailPreview.ts` | 131 | workflow |  | sanitizeEmailPreviewHtml | Mirrors of the send path's allowlists (EMAIL_HTML_SANITIZE_CONFIG / EMAIL_ALLOWED_CSS_PROPS in |
| `nc-gui/helpers/workflowExpressionHtml.ts` | 52 | workflow |  | parseInertHtml,stashImages,restoreImages,expressionSpansToTokens | Parse without executing: editor / AI HTML is untrusted, and a detached div still fires |
| `nc-gui/store/agent.ts` | 59 | agent/chat |  | useAgentStore | State |
| `nc-gui/store/chat.ts` | 97 | agent/chat | ✓ | useChatStore |  |
| `nc-gui/store/workflow.ts` | 51 | workflow |  | useWorkflowStore | State |
| `nc-gui/utils/agentPickerUtils.ts` | 7 | agent/chat |  | selectableAgents | Pickers and member lists offer agents that act as an identity; a personal agent acts as its owner. */ |
| `nc-gui/utils/chatwootUtils.ts` | 11 | agent/chat |  | syncChatwootColorScheme | Chatwoot SDK method that the @productdevbook/chatwoot typings omit |
| `nc-gui/utils/workflowExpressionUtils.ts` | 126 | workflow |  |  | Matches one `{{ }}` token; braces can't nest inside, same as the engine. */ |
| `nc-gui/utils/workflowGraphUtils.ts` | 328 | workflow |  | filterOutPlusNodes,getNodeOutputPorts,hasOutputPorts,getNodeOutgoingEdges,edgeSourcePort,getPortOutgoingEdges | Filter out plus nodes and their associated edges |
| `nc-gui/utils/workflowUtils.ts` | 727 | workflow |  |  | Filter nodes and edges based on edit permission |
| `noco-integrations/core/src/ai/index.ts` | 3 | ai |  |  |  |
| `noco-integrations/core/src/ai/types.ts` | 838 | ai |  | ModelCapability,AiUseCase,ModelInfo,AiSdkUsageLike,AiSdkNestedUsageLike,AiUsageBuckets | AI SDK DevTools — opt-in via NC_AI_DEVTOOLS=true. The watch:run* scripts in |
| `noco-integrations/core/src/workflow-node/index.ts` | 2 | workflow |  |  |  |
| `noco-integrations/core/src/workflow-node/types.ts` | 408 | workflow |  | WorkflowNodeLog,WorkflowNodeRunContext,WorkflowActivationContext,WorkflowActivationState,PollingContext,PollingResult | Config paths (dot-notation, e.g. `config.body`) whose stored template was rich text, so |
| `nocodb-sdk/src/lib/agent/channel.ts` | 95 | agent/chat |  | AgentChannelRespondMode,AgentChannelSettings,AgentChannelType,AgentChannelCreateReqType,AgentChannelUpdateReqType,AGENT_CHANNEL_TYPES | Channels — the surfaces an agent can be reached on. |
| `nocodb-sdk/src/lib/agent/files.ts` | 94 | agent/chat |  | AgentFileType,AGENT_MAX_FILES,AGENT_MAX_FILE_SIZE,AGENT_FILE_ID_PATTERN,AGENT_FILE_EXTENSIONS,AGENT_FILE_ACCEPT | A file the agent reads on every run. `title` is the name its instructions |
| `nocodb-sdk/src/lib/agent/index.ts` | 309 | agent/chat |  | AgentMode,AgentScope,AgentRoleCap,AgentConfigType,AgentPrMode,AgentCodeConfig | What an agent runs on, fixed at creation. A coding agent gets repositories |
| `nocodb-sdk/src/lib/agent/setup.ts` | 330 | agent/chat |  | AgentSetupSource,AGENT_SETUP_DAYS,AgentSetupDay,AgentSetupWhen,AgentSetupShare,AgentSetupDraft | A general agent's guided setup: the draft it builds in its own chat before it |
| `nocodb-sdk/src/lib/agentSection.ts` | 33 | agent/chat |  | AgentSectionType,AgentSectionListType,AgentSectionCreateReqType,AgentSectionUpdateReqType | Agent sections — collapsible folders in the Agents sidebar that group a |
| `nocodb-sdk/src/lib/ai/index.ts` | 108 | ai |  | SerializedAiTableType,SerializedAiViewType,PredictNextFieldsType,PredictNextFormulasType,substituteColumnIdWithAliasInPrompt |  |
| `nocodb-sdk/src/lib/app/agent.ts` | 238 | agent/chat |  | AppAgentMode,AppAgentConfig,APP_AGENT_MAX_STARTERS,APP_AGENT_MAX_INSTRUCTIONS,APP_AGENT_MAX_TITLE,APP_AGENT_MAX_STARTER | The in-app AI assistant — the chat widget a published app serves to its own end |
| `nocodb-sdk/src/lib/chat/index.ts` | 909 | agent/chat |  | ChatMessageRole,ChatEventAction,CHAT_AGENT_APP_BUILDER,ChatToolProgress,FactoryBootStage,ChatToolCallStatus | Thinking, streamed like TOKEN. Persisted as a reasoning part, not rendered. */ |
| `nocodb-sdk/src/lib/workflow/constants.ts` | 109 | workflow |  | WORKFLOW_EXECUTION_STATUSES,TRIGGER_MAX_CHAIN_DEPTH,WorkflowSkipReason,GeneralNodeID,TriggerActivationType,TriggerTestMode | How many trigger hops a chain of writes may take before the next firing is |
| `nocodb-sdk/src/lib/workflow/expressionCatalog.ts` | 217 | workflow |  | WorkflowExpressionMember,WORKFLOW_EXPRESSION_METHODS,WORKFLOW_EXPRESSION_BUILTINS,getWorkflowExpressionMethods | What the workflow expression engine lets you call, for editor autocomplete. |
| `nocodb-sdk/src/lib/workflow/index.ts` | 6 | workflow |  |  |  |
| `nocodb-sdk/src/lib/workflow/interface.ts` | 439 | workflow |  |  | Expression reference (e.g., "fields.Title" for use in {{ $("Node").fields.Title }}) |
| `nocodb-sdk/src/lib/workflow/node.ts` | 109 | workflow |  | WorkflowNodeCategory,WorkflowNodeCategoryType,WorkflowPortDirection,WorkflowNodePort,WorkflowNodeDefinition,WorkflowNodeRetryPolicy | Workflow Categories |
| `nocodb-sdk/src/lib/workflow/transforms.ts` | 408 | workflow |  | WorkflowTransformCategory,WorkflowTransformArgType,WorkflowTransformArg,WorkflowTransformStep,WorkflowValueKind,WorkflowExpressionTransform | Transforms a user can stack on a workflow variable without writing an expression. |
| `nocodb/src/mcp/attachment-read.ts` | 47 | mcp |  | resolveReadableAttachmentPath | Storage key `readAttachment` may read for `file`, or `null` when the caller's |
| `nocodb/src/mcp/data-schemas.ts` | 48 | mcp |  | sortSchema,McpSort,serializeSort,fieldsSchema,viewIdSchema,recordIdSchema | v3 documents `sort` as a JSON string. Passing the array through instead reaches |
| `nocodb/src/mcp/descriptions.ts` | 145 | mcp |  | whereDescription,whereDescriptionRef,aggregationDescription |  |
| `nocodb/src/mcp/mcp-error.ts` | 46 | mcp |  | McpErrorResult,mcpErrorStatus,mcpErrorResult,mcpErrorMessage,serializeMcpError | A type alias, not an interface: it must fit the SDK's indexed CallToolResult. |
| `nocodb/src/mcp/mcp-http.ts` | 56 | mcp |  | refuseNonPostMcp,isMcpEndpoint,toJsonRpcError,JSON_RPC_PARSE_ERROR | The MCP transport is stateless, so there is nothing to push on a GET's SSE |
| `nocodb/src/mcp/mcp.controller.ts` | 99 | mcp |  | McpController | On the method, not the class: class guards are inherited, and EE overrides this route with an MCP limit of its own. |
| `nocodb/src/mcp/mcp.service.ts` | 1489 | mcp |  | McpService |  |
| `nocodb/src/mcp/oauth-scope-guard.ts` | 34 | mcp | ✓ | OAuthResourceToken,OAuthResourceContext,checkOAuthResourceAccess | CE has no scope rows, so a grant is bounded only by `granted_resources`. |
| `nocodb/src/mcp/tool-guards.ts` | 71 | mcp |  | assertTableVisible,resolveToolTableId | MCP tools receive `tableId` in the JSON-RPC body, so they never pass through |
| `nocodb/src/mcp/tools/call-scope.ts` | 76 | mcp |  | scopeAuditFieldsPerCall,withCallScope,callScopedRegistrar | Every `req` field the duplicate service stamps mid-call. `ncSourceId` is the |
| `nocodb/src/mcp/tools/field-defaults.ts` | 46 | mcp |  | typedFieldDefault,withTypedFieldDefaults | `cdf` is a text meta column, so the v3 builder hands back Checkbox and |
| `nocodb/src/mcp/tools/filter-compile.ts` | 512 | mcp |  | COMPUTED_TYPES,filterInputSchema,filterInputDescription,FilterColumn,matchFieldRef,resolveFieldRef | The operator vocabulary the string DSL documents, minus its internal aliases |
| `nocodb/src/mcp/tools/filter-input.ts` | 164 | mcp |  | compileFilterInput,resolveWhere,normalizeWhere,countWhereConditions | `compileFilterTree` against the columns of one table. */ |
| `nocodb/src/mcp/tools/filter-verify.ts` | 46 | mcp |  | FilterInputLeaf,FilterInputNode,resolveViewFilter | Resolve every `field_id` (id or title) to a field id. FiltersV3Service |
| `nocodb/src/mcp/tools/read-echo.ts` | 141 | mcp |  | ReadEcho,AUDIT_KEYS,readEchoShape,takeReadEcho,assertImmutableEcho | Keys a write's paired read returns that the write cannot set. |
| `nocodb/src/mcp/tools/record-fields.ts` | 199 | mcp |  | isUserVisibleField,checkRecordFields,resolveReturnFields,pickReturnFields | System links (the hidden has-many side of a many-to-one, the `_nc_m2m_*` |
| `nocodb/src/mcp/tools/strict-schema.ts` | 65 | mcp |  | isZodSchema,toStrictInputSchema,strictRegistrar | Build a tool's `inputSchema` so unknown keys are rejected rather than |
| `nocodb/src/mcp/tools/system-fields.ts` | 39 | mcp |  | includeSystemSchema,systemAuditFields | The v3 table read drops system columns. MCP callers asking for them get the |
| `nocodb/src/mcp/tools/tool-helpers.ts` | 379 | mcp |  | McpToolUser,McpRoleFlags,McpToolRegisterCtx,ALL_ROLE_FLAGS,getRoleFlags,REJECT_DEPRECATED_LINKS_MESSAGE | `User.getWithRoles` returns it and RLS role-subject matching reads it off |
| `nocodb/src/mcp/tools/tool-scope.ts` | 111 | mcp |  | McpAclScope,McpToolAcl,McpScopeTarget,McpToolTarget,McpToolScope,baseIdInput | The ACL scope an operation is declared under, as in `@Acl(op, { scope })`. */ |
| `nocodb/src/mcp/tools/tool-units.ts` | 26 | mcp |  | atLeastOne,unitsForBulk,unitsForRows | A tool call that ran costs at least one API call, the way any REST request |
| `nocodb/src/meta/chat-messages.service.ts` | 17 | agent/chat |  | ChatMessagesService |  |
| `nocodb/src/meta/migrations/chat-messages/nc_001_init.ts` | 27 | agent/chat |  |  |  |
| `nocodb/src/meta/migrations/chat-messages/nc_002_base_id.ts` | 25 | agent/chat |  |  | Drop all messages — existing ones lack base_id. |
| `nocodb/src/meta/migrations/chat-messages/nc_003_created_files.ts` | 21 | agent/chat |  |  | Stores files the AI generated for a message (e.g. a CSV/chart written by the |
| `nocodb/src/meta/migrations/chat-messages/nc_004_ui_context_record.ts` | 22 | agent/chat |  |  | Stores the record a user message was scoped to ("Ask AI about this record" / |
| `nocodb/src/meta/migrations/chat-messages/nc_005_agents.ts` | 100 | agent/chat |  |  | Agent messages, and the web artifacts a chat or agent turn publishes. |
| `nocodb/src/meta/migrations/chat-messages/nc_006_agent_app.ts` | 24 | agent/chat |  |  | App-builder turns now persist as normal chat messages. `agent` attributes the |
| `nocodb/src/meta/migrations/chat-messages/nc_007_agent_messages.ts` | 81 | agent/chat |  |  | Agent messages become chat messages: same parts, same files, one table. The |
| `nocodb/src/meta/migrations/v0/nc_004_workflows.ts` | 93 | workflow |  |  | Indexes for fast lookups |
| `nocodb/src/meta/migrations/v0/nc_007_workflow_draft.ts` | 19 | workflow |  |  |  |
| `nocodb/src/meta/migrations/v0/nc_011_merge_workflows_scripts.ts` | 221 | workflow |  |  | Type discriminator: 'script' or 'workflow' |
| `nocodb/src/meta/migrations/v0/nc_012_workflow_delay.ts` | 25 | workflow |  |  |  |
| `nocodb/src/meta/migrations/v0/nc_202603170001_workflow_draft_reminder.ts` | 22 | workflow |  |  | Seed existing drafts so we don't spam reminders for pre-existing workflows on first deploy |
| `nocodb/src/meta/migrations/v0/nc_202610081400_col_button_workflow_meta.ts` | 26 | workflow |  |  |  |
| `nocodb/src/models/Agent.ts` | 32 | agent/chat | ✓ | Agent | CE stub. Agents are an EE feature; `src/ee/models/Agent.ts` is the real |
| `nocodb/src/models/ChatMessage.ts` | 47 | agent/chat | ✓ | ChatMessage |  |
| `nocodb/src/models/ChatSession.ts` | 26 | agent/chat | ✓ | ChatSession |  |
| `nocodb/src/models/Workflow.ts` | 40 | workflow | ✓ | Workflow |  |
| `nocodb/src/services/mcp.service.ts` | 156 | mcp |  | McpTokenService | Set required fields |
| `nocodb/src/utils/richTextHelper.ts` | 85 | markdown |  | extractMentionsFromProseMirror,extractFileReferenceIds,extractMentions | Extract user IDs from mention nodes in ProseMirror JSON content. |

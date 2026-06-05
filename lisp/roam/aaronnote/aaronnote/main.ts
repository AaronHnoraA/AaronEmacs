import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import { createEditor, type Editor, type EditorCommand, type QuickInsertItem } from "../src/lib.ts";
import { CoalescedTimer } from "../src/coalesced-timer.ts";
import { Epoch } from "../src/async-epoch.ts";
import { matchChord, historyChordKind, type ShortcutCtx } from "../src/keymap/shortcut-router.ts";
import type { EditorView } from "@codemirror/view";
import { setFindHighlightRanges } from "../src/cm6/find-highlight.ts";
import { markdownHrefAt } from "../src/cm6/editor-cm6.ts";
import { setKnownRoamRefs } from "../src/cm6/roam-link-status.ts";
import { proseDiagnosticsAt, setProseDiagnostics, type ProseDiagnostic } from "../src/cm6/prose-diagnostics.ts";
import { equationTagsFromText, getEquationTagHits } from "../src/equation-tags.ts";
import { INLINE_MATH_RE, isLikelyInlineMath } from "../src/inline-math.ts";
import { getBlockMathRanges, rangeAtPosition, rangeOverlapsAny } from "../src/cm6/math-ranges.ts";
import { formatMathRenderError, renderMathLazy } from "../src/math-render.ts";
import { noteCssHrefFromMarkdown } from "../src/render-html.ts";
import { safeHref } from "../src/url-safety.ts";
import { visualMarkdownAttachmentP } from "../src/visual-attachments.ts";
import { createUnusedAssetsManager } from "./asset-cleanup.ts";
import {
  collectFindMatches,
  collectFindMatchesInRanges,
  createFindPattern,
  replacementText as findReplacementText,
  replaceAllFindMatches as replaceAllFindText,
  type FindMatch,
} from "./find.ts";
import { createFloatingTocPanel, inlineTagAnchorsFromText, markdownHeadingsFromText } from "./floating-toc.ts";
import { setBookContext, type BookEditorContext, type BookEditorTocItem } from "../src/cm6/widgets/block-extras.ts";
import { createLinkPreviewController, type LinkPreviewTarget } from "./link-preview.ts";
import { createLocalGraphPanel } from "./local-graph.ts";
import { clampCommandIndex, filterCommands, type AaronnoteCommand } from "./command-palette.ts";
import {
  canonicalRoamNoteId,
  escapeMarkdownLinkText,
  inlineTagFromHash,
  markdownRoamIdLink,
  resolveRoamNoteSearch,
  roamHrefForNote,
  roamNoteSearchValue,
} from "./roam-idlink.ts";
import { resolveNoteReference as resolveSharedNoteReference } from "../shared/note-refs.mjs";
import { matchingSnippetsForPrefix, SnippetSession, snippetDetail, snippetLabel } from "./snippets.ts";
import type { CursorPosition, DirectorySummary, FileSummary, Inbound, NoteSummary, RecentNote, SnippetSummary, TemplateSummary, UploadedAsset } from "./types.ts";
import { api } from "./api-client.ts";
import { createVimLite, type VimLiteMode } from "./vim-lite.ts";
import { createVimCursor, updateVimCursor } from "./vim-cursor.ts";
import { collectBrowserSpellWords, maskAaronnoteProse } from "../shared/prose-mask.mjs";

declare global {
  interface Window {
    SITE_DATA?: { meta?: Record<string, unknown>; notes?: NoteSummary[]; books?: unknown[] };
    KNOWLEDGE_DATA?: {
      notes: Array<NoteSummary & { key: string }>;
      tags: Array<{ name: string; count: number; notes: string[] }>;
      groups: Array<{ key: string; label: string; items: NoteSummary[] }>;
    };
    initKnowledgeGraph?: (options?: Record<string, unknown>) => { destroy?: () => void; setVisibleKeys?: (keys: string[]) => void } | null;
    buildKnowledgeData?: () => void;
    __GRAPH_NO_AUTO_INIT__?: boolean;
    AaronnoteCurrentFile?: () => string;
    AaronnoteResolveAssetUrl?: (src: string) => string;
    AaronnoteDesktop?: {
      chooseNotePath?: (options?: { suggestedPath?: string; title?: string; mode?: "file" | "directory" | "openFile" }) => Promise<string>;
      trashNote?: (file: string) => Promise<{ ok?: boolean; file?: string; message?: string }>;
      exportPdf?: (options?: { file?: string; name?: string }) => Promise<{ ok?: boolean; canceled?: boolean; file?: string; message?: string }>;
      ready?: () => void;
      onOpenFile?: (handler: (file: string) => void) => () => void;
    };
  }
}

window.__GRAPH_NO_AUTO_INIT__ = true;

// Stubs for removed panels (lean, jupyter, agenda, filesystem, git, graph).
// These replaced the deleted panel modules; the HTML chrome still references
// them via DOM queries, but the functionality is now in Emacs.
type _PanelStub = { visible: boolean; show: () => void; hide: () => void; destroy: () => void };
type _JupyterTarget = { kind?: string; file?: string; kernel?: string };
type LeanLocation = { file?: string; line?: number; col?: number };
function createLeanPanel(_opts: { root: HTMLElement }): _PanelStub {
  return { visible: false, show() {}, hide() {}, destroy() {} };
}
function createJupyterPanel(_opts: { root: HTMLElement }): _PanelStub & { setTarget: (_t: _JupyterTarget) => void } {
  return { visible: false, show() {}, hide() {}, setTarget() {}, destroy() {} };
}
function createAgendaManager(_opts: unknown): { refresh: () => void; destroy: () => void } {
  return { refresh() {}, destroy() {} };
}
function createFilesystemBrowser(_opts: unknown): { focus: () => boolean; focusRecent: () => boolean; destroy: () => void } {
  return { focus: () => false, focusRecent: () => false, destroy() {} };
}
function createGraphPanel(_opts: unknown): _PanelStub {
  return { visible: false, show() {}, hide() {}, destroy() {} };
}
type JupyterTarget = _JupyterTarget;
function activeLeanController(_file: string) { return null; }
function clearLeanSnippetCache() {}
function getLeanController(_file: string) { return null; }
function setLeanLocationsPicker(_picker: unknown) {}
function setLeanNotePath(_view: unknown, _file: string) {}
type LeanEditAction = { kind: string };
type LeanLspAction = { kind: string };
function canonicalLeanSelector(s: string) { return s; }
function formatLeanPlaceholder(_sel: string, _tag: string) { return ""; }
function parseLeanPlaceholderLine(_line: string) { return null; }
function scanMarkdownLeanPlaceholdersShared(_md: string) { return []; }
const leanSpliceField: unknown = null;

const params = new URLSearchParams(window.location.search);
const hostAssetResolver = window.AaronnoteResolveAssetUrl;

function installNativeFontFace(): void {
  if (!window.aaronnoteApi) return;
  const nativeFontUrl = hostAssetResolver?.("aaronnote-asset://font/FZLiuGongQuanKaiShuJF.ttf")
    ?? "aaronnote-asset://font/FZLiuGongQuanKaiShuJF.ttf";
  const style = document.createElement("style");
  style.dataset.aaronnoteNativeFont = "true";
  style.textContent = `
@font-face {
  font-family: "Aaron LiuGongQuan";
  src:
    url("${nativeFontUrl}") format("truetype"),
    local("FZLIUGQKSJF--GBK1-0"),
    local("FZLiuGongQuanKaiShuJF"),
    local("方正柳公权楷书 简繁");
  font-style: normal;
  font-weight: 400;
  font-display: block;
  unicode-range:
    U+2E80-2EFF,
    U+3000-303F,
    U+31C0-31EF,
    U+3400-4DBF,
    U+4E00-9FFF,
    U+F900-FAFF,
    U+FF00-FFEF;
}
`;
  document.head.appendChild(style);
}

installNativeFontFace();

const root = document.querySelector<HTMLDivElement>("#app")!;
root.innerHTML = `
  <main class="aaronnote-shell">
    <header class="aaronnote-toolbar">
      <div class="aaronnote-title">
        <strong>Aaronnote</strong>
        <span data-file-label>No file</span>
      </div>
      <div class="aaronnote-actions">
        <button type="button" data-action="notes">Notes</button>
        <button type="button" data-action="relation">Relation</button>
        <button type="button" data-action="agenda">Agenda</button>
        <button type="button" data-action="focus-mode">Focus</button>
        <button type="button" data-action="source">Source</button>
        <button type="button" data-action="force-save" class="aaronnote-danger-action" hidden>Overwrite</button>
        <button type="button" data-action="editor" hidden>Editor</button>
      </div>
      <span class="aaronnote-vim-mode" data-vim-mode>INSERT</span>
      <span class="aaronnote-status" data-status>Connecting</span>
    </header>
    <div class="aaronnote-draft-banner" data-draft-banner hidden>
      <span data-draft-message></span>
      <div>
        <button type="button" data-action="draft-recover">Recover</button>
        <button type="button" data-action="draft-discard">Discard</button>
      </div>
    </div>
    <section class="aaronnote-body">
      <section class="aaronnote-editor" id="editor"></section>
      <section class="aaronnote-notes" data-notes-page hidden>
        <div class="aaronnote-notes-inner">
          <header class="aaronnote-notes-head">
            <h1>Notes</h1>
            <button type="button" data-action="editor-inline">Back</button>
          </header>
          <div class="aaronnote-notes-tabs" role="tablist" aria-label="Roam tools">
            <button type="button" data-notes-tab="recent">Recent</button>
            <button type="button" data-notes-tab="agenda">Agenda</button>
            <button type="button" data-notes-tab="filesystem" class="is-active">Filesystem</button>
            <button type="button" data-notes-tab="graph">Roam graph</button>
            <button type="button" data-notes-tab="git">Git</button>
            <button type="button" data-notes-tab="lean">Lean</button>
            <button type="button" data-notes-tab="management">Roam management</button>
          </div>
          <div data-notes-panel="recent" hidden>
            <div data-recent-list class="aaronnote-note-list"></div>
          </div>
          <div data-notes-panel="agenda" hidden>
            <div class="aaronnote-agenda-toolbar">
              <input data-agenda-filter type="search" placeholder="Filter active todos" />
              <select data-agenda-sort aria-label="Sort todos">
                <option value="status">Status</option>
                <option value="ddl">DDL</option>
                <option value="file">File</option>
                <option value="time">Time</option>
              </select>
              <select data-agenda-group aria-label="Group todos">
                <option value="status">Group: Status</option>
                <option value="ddl">Group: DDL</option>
                <option value="file">Group: File</option>
                <option value="time">Group: Updated</option>
              </select>
              <label><input data-agenda-done type="checkbox" /> Done</label>
              <button type="button" data-action="agenda-refresh">Refresh</button>
              <span data-agenda-count></span>
            </div>
            <div data-agenda-list class="aaronnote-agenda-list"></div>
          </div>
          <div data-notes-panel="filesystem">
            <div class="aaronnote-files-toolbar">
              <input data-note-filter type="search" placeholder="Search notes: tag:, title:, path:, ref:, backlink:" />
              <div class="aaronnote-files-actions">
                <button type="button" data-action="notes-collapse-all">Parent</button>
                <button type="button" data-action="notes-expand-all">Current</button>
                <button type="button" data-action="notes-show-all" aria-pressed="false">显示所有</button>
                <button type="button" data-action="ensure-roam-id">生成/复制 Roam ID</button>
                <span data-note-count></span>
              </div>
            </div>
            <div data-note-list class="aaronnote-note-list aaronnote-files-list"></div>
          </div>
          <div data-notes-panel="graph" data-graph-page hidden>
            <div class="aaronnote-graph-toolbar">
              <input data-graph-filter type="search" placeholder="Filter graph" hidden />
              <span data-graph-stats></span>
            </div>
            <div class="aaronnote-graph-grid">
              <div id="graph-container" class="aaronnote-graph-canvas graph-container" data-graph-canvas></div>
              <aside class="aaronnote-graph-focus graph-focus empty" data-graph-focus></aside>
            </div>
          </div>
          <div data-notes-panel="git" hidden>
            <section class="aaronnote-git" data-git-root>
              <div class="aaronnote-git-top">
                <section class="aaronnote-git-status-card">
                  <span>Branch</span>
                  <strong data-git-branch>No branch</strong>
                  <small data-git-summary>Not loaded</small>
                </section>
                <section class="aaronnote-git-status-card">
                  <span>Remote</span>
                  <strong data-git-remote>No remote</strong>
                  <small data-git-counts>0 files</small>
                </section>
                <section class="aaronnote-git-commit-box">
                  <input data-git-message type="text" placeholder="Commit message" />
                  <button type="button" data-action="git-commit">Commit all</button>
                </section>
              </div>
              <div class="aaronnote-git-actions">
                <button type="button" data-action="git-refresh">Refresh</button>
                <button type="button" data-action="git-pull">Pull</button>
                <button type="button" data-action="git-push">Push</button>
                <button type="button" data-action="git-sync">Sync roamdb</button>
                <button type="button" data-action="git-open-file" disabled>Open file</button>
                <button type="button" data-action="git-restore-file" disabled>Restore latest</button>
              </div>
              <div class="aaronnote-git-grid">
                <section class="aaronnote-git-list-panel">
                  <header>Working tree</header>
                  <div class="aaronnote-git-list" data-git-changes></div>
                </section>
                <section class="aaronnote-git-list-panel">
                  <header>History</header>
                  <div class="aaronnote-git-list" data-git-history></div>
                </section>
                <section class="aaronnote-git-diff-panel">
                  <header>
                    <strong data-git-diff-title>Diff</strong>
                    <span data-git-diff-meta>No target selected</span>
                  </header>
                  <pre class="aaronnote-git-diff" data-git-diff></pre>
                </section>
              </div>
            </section>
          </div>
          <div data-notes-panel="lean" hidden>
            <section class="aaronnote-lean-project" data-lean-project-root>
              <div class="aaronnote-lean-project-top">
                <section>
                  <span>Project</span>
                  <strong data-lean-project-path>Not loaded</strong>
                  <small data-lean-project-toolchain></small>
                </section>
                <section>
                  <span>Lean</span>
                  <strong data-lean-project-lean-version>Not loaded</strong>
                  <small data-lean-project-lake-version></small>
                </section>
                <section>
                  <span>Packages</span>
                  <strong data-lean-project-package-count>0</strong>
                  <small data-lean-project-status>Idle</small>
                </section>
              </div>
              <div class="aaronnote-lean-project-actions">
                <button type="button" data-lean-project-command="info">Info</button>
                <button type="button" data-lean-project-command="update">Update</button>
                <button type="button" data-lean-project-command="cache">Cache</button>
                <button type="button" data-lean-project-command="build">Build</button>
                <button type="button" data-lean-project-command="clean">Clean</button>
                <button type="button" data-lean-project-refresh>Refresh</button>
              </div>
              <div class="aaronnote-lean-project-packages" data-lean-project-packages></div>
              <pre class="aaronnote-lean-project-output" data-lean-project-output>Open this tab to load Lean project info.</pre>
            </section>
          </div>
          <div data-notes-panel="management" hidden>
            <div class="aaronnote-management-grid">
              <button type="button" data-action="sync">Sync roamdb</button>
              <button type="button" data-action="rename-roam-tag">Rename tag</button>
              <button type="button" data-action="delete-roam-tag">Delete tag</button>
              <button type="button" data-action="tag-overlap-report">Tag overlap</button>
              <button type="button" data-action="rewrite-path-refs">Rewrite path refs</button>
              <button type="button" data-action="scan-unused-assets">Scan unused assets</button>
              <button type="button" data-action="trash-unused-assets" disabled>Move selected to Trash</button>
            </div>
            <div class="aaronnote-management-status">
              <strong data-management-count>0</strong>
              <span>nodes indexed from the current root</span>
            </div>
            <section class="aaronnote-unused-assets" data-unused-assets-section hidden>
              <header>
                <strong data-unused-assets-count>0 unused assets</strong>
                <label><input data-unused-assets-select-all type="checkbox" /> Select all</label>
              </header>
              <div data-unused-assets-list class="aaronnote-unused-assets-list"></div>
            </section>
            <section class="aaronnote-roam-tools" data-roam-tools-section hidden>
              <header>
                <strong data-roam-tools-title>Roam tools</strong>
              </header>
              <div data-roam-tools-list class="aaronnote-roam-tools-list"></div>
            </section>
          </div>
        </div>
      </section>
    </section>
    <aside class="aaronnote-tool-panel tool-panel--hidden" data-tool-panel hidden>
      <div class="aaronnote-tool-tabs" role="tablist" aria-label="Lean and Jupyter panel">
        <button type="button" data-tool-tab="lean" role="tab" aria-selected="false">Lean</button>
        <button type="button" data-tool-tab="jupyter" role="tab" aria-selected="false">Jupyter</button>
      </div>
      <section class="aaronnote-tool-pane" data-tool-pane="lean" role="tabpanel">
        <div class="aaronnote-lean-panel" data-lean-panel hidden></div>
      </section>
      <section class="aaronnote-tool-pane" data-tool-pane="jupyter" role="tabpanel" hidden>
        <div class="aaronnote-jupyter-panel" data-jupyter-panel hidden></div>
      </section>
      <div class="aaronnote-tool-panel-resizer" data-tool-panel-resizer role="separator" aria-orientation="vertical" title="Resize panel"></div>
    </aside>
    <button type="button" class="aaronnote-lean-trigger" data-lean-trigger hidden title="Toggle Lean Infoview">⊢</button>
    <div class="aaronnote-panel-switcher" data-panel-switcher hidden>
      <button type="button" class="aaronnote-jupyter-trigger" data-jupyter-trigger hidden title="Toggle Jupyter preview">&lt;/&gt;</button>
    </div>
    <aside class="aaronnote-floating-toc is-collapsed" data-floating-toc>
      <button type="button" data-toc-toggle aria-expanded="false" title="Toggle page outline">Page</button>
      <nav data-toc-list aria-label="Page outline"></nav>
    </aside>
    <aside class="aaronnote-book-toc is-collapsed" data-book-toc hidden>
      <nav data-book-toc-list aria-label="Book contents"></nav>
    </aside>
    <button type="button" class="aaronnote-book-trigger" data-book-toc-toggle aria-expanded="false" hidden title="Toggle book contents">Book</button>
    <aside class="aaronnote-local-graph is-collapsed" data-local-graph hidden>
      <button type="button" data-local-graph-toggle aria-expanded="false">Graph</button>
      <section class="aaronnote-local-graph-panel" aria-label="Local graph">
        <header>
          <strong>Local graph</strong>
          <span data-local-graph-status></span>
        </header>
        <div class="aaronnote-local-graph-controls">
          <label class="aaronnote-local-graph-depth">
            <span>Depth</span>
            <input data-local-graph-depth type="range" min="1" max="2" step="1" value="1" />
            <b data-local-graph-depth-label>1</b>
          </label>
          <label><input data-local-graph-refs type="checkbox" checked /> Refs</label>
          <label><input data-local-graph-backlinks type="checkbox" checked /> Backlinks</label>
          <label><input data-local-graph-tags type="checkbox" checked /> Tags</label>
        </div>
        <div class="aaronnote-local-graph-canvas" data-local-graph-canvas></div>
      </section>
    </aside>
  </main>
`;

const host = document.querySelector<HTMLElement>("#editor")!;
const statusEl = document.querySelector<HTMLElement>("[data-status]")!;
const vimModeEl = document.querySelector<HTMLElement>("[data-vim-mode]")!;
const fileLabel = document.querySelector<HTMLElement>("[data-file-label]")!;
const noteList = document.querySelector<HTMLElement>("[data-note-list]")!;
const recentList = document.querySelector<HTMLElement>("[data-recent-list]")!;
const noteFilter = document.querySelector<HTMLInputElement>("[data-note-filter]")!;
const noteCount = document.querySelector<HTMLElement>("[data-note-count]")!;
const notesPage = document.querySelector<HTMLElement>("[data-notes-page]")!;
const graphPage = document.querySelector<HTMLElement>("[data-graph-page]")!;
const gitRoot = document.querySelector<HTMLElement>("[data-git-root]")!;
const leanProjectRoot = document.querySelector<HTMLElement>("[data-lean-project-root]")!;
const leanProjectPath = document.querySelector<HTMLElement>("[data-lean-project-path]")!;
const leanProjectToolchain = document.querySelector<HTMLElement>("[data-lean-project-toolchain]")!;
const leanProjectLeanVersion = document.querySelector<HTMLElement>("[data-lean-project-lean-version]")!;
const leanProjectLakeVersion = document.querySelector<HTMLElement>("[data-lean-project-lake-version]")!;
const leanProjectPackageCount = document.querySelector<HTMLElement>("[data-lean-project-package-count]")!;
const leanProjectStatus = document.querySelector<HTMLElement>("[data-lean-project-status]")!;
const leanProjectPackages = document.querySelector<HTMLElement>("[data-lean-project-packages]")!;
const leanProjectOutput = document.querySelector<HTMLElement>("[data-lean-project-output]")!;
const leanProjectRefresh = document.querySelector<HTMLButtonElement>("[data-lean-project-refresh]")!;
const syncButton = document.querySelector<HTMLButtonElement>("[data-action='sync']")!;
const renameRoamTagButton = document.querySelector<HTMLButtonElement>("[data-action='rename-roam-tag']")!;
const deleteRoamTagButton = document.querySelector<HTMLButtonElement>("[data-action='delete-roam-tag']")!;
const tagOverlapReportButton = document.querySelector<HTMLButtonElement>("[data-action='tag-overlap-report']")!;
const rewritePathRefsButton = document.querySelector<HTMLButtonElement>("[data-action='rewrite-path-refs']")!;
const ensureRoamIdButton = document.querySelector<HTMLButtonElement>("[data-action='ensure-roam-id']")!;
const notesCollapseAllButton = document.querySelector<HTMLButtonElement>("[data-action='notes-collapse-all']")!;
const notesExpandAllButton = document.querySelector<HTMLButtonElement>("[data-action='notes-expand-all']")!;
const notesShowAllButton = document.querySelector<HTMLButtonElement>("[data-action='notes-show-all']")!;
const scanUnusedAssetsButton = document.querySelector<HTMLButtonElement>("[data-action='scan-unused-assets']")!;
const trashUnusedAssetsButton = document.querySelector<HTMLButtonElement>("[data-action='trash-unused-assets']")!;
const unusedAssetsSection = document.querySelector<HTMLElement>("[data-unused-assets-section]")!;
const unusedAssetsCount = document.querySelector<HTMLElement>("[data-unused-assets-count]")!;
const unusedAssetsSelectAll = document.querySelector<HTMLInputElement>("[data-unused-assets-select-all]")!;
const unusedAssetsList = document.querySelector<HTMLElement>("[data-unused-assets-list]")!;
const roamToolsSection = document.querySelector<HTMLElement>("[data-roam-tools-section]")!;
const roamToolsTitle = document.querySelector<HTMLElement>("[data-roam-tools-title]")!;
const roamToolsList = document.querySelector<HTMLElement>("[data-roam-tools-list]")!;
const managementCount = document.querySelector<HTMLElement>("[data-management-count]")!;
const graphFilter = document.querySelector<HTMLInputElement>("[data-graph-filter]")!;
const graphCanvas = document.querySelector<HTMLElement>("[data-graph-canvas]")!;
const graphFocus = document.querySelector<HTMLElement>("[data-graph-focus]")!;
const graphStats = document.querySelector<HTMLElement>("[data-graph-stats]")!;
const notesButton = document.querySelector<HTMLButtonElement>("[data-action='notes']")!;
const relationButton = document.querySelector<HTMLButtonElement>("[data-action='relation']")!;
const agendaButton = document.querySelector<HTMLButtonElement>("[data-action='agenda']")!;
const sourceButton = document.querySelector<HTMLButtonElement>("[data-action='source']")!;
const forceSaveButton = document.querySelector<HTMLButtonElement>("[data-action='force-save']")!;
const editorButton = document.querySelector<HTMLButtonElement>("[data-action='editor']")!;
const editorInlineButton = document.querySelector<HTMLButtonElement>("[data-action='editor-inline']")!;
const focusModeButton = document.querySelector<HTMLButtonElement>("[data-action='focus-mode']")!;
const agendaFilter = document.querySelector<HTMLInputElement>("[data-agenda-filter]")!;
const agendaSort = document.querySelector<HTMLSelectElement>("[data-agenda-sort]")!;
const agendaGroup = document.querySelector<HTMLSelectElement>("[data-agenda-group]")!;
const agendaDone = document.querySelector<HTMLInputElement>("[data-agenda-done]")!;
const agendaRefresh = document.querySelector<HTMLButtonElement>("[data-action='agenda-refresh']")!;
const agendaCount = document.querySelector<HTMLElement>("[data-agenda-count]")!;
const agendaList = document.querySelector<HTMLElement>("[data-agenda-list]")!;
const toolPanelRoot = document.querySelector<HTMLElement>("[data-tool-panel]")!;
const leanToolPane = document.querySelector<HTMLElement>("[data-tool-pane='lean']")!;
const jupyterToolPane = document.querySelector<HTMLElement>("[data-tool-pane='jupyter']")!;
const leanToolTab = document.querySelector<HTMLButtonElement>("[data-tool-tab='lean']")!;
const jupyterToolTab = document.querySelector<HTMLButtonElement>("[data-tool-tab='jupyter']")!;
const toolPanelResizer = document.querySelector<HTMLElement>("[data-tool-panel-resizer]")!;
const leanPanelRoot = document.querySelector<HTMLElement>("[data-lean-panel]")!;
const jupyterPanelRoot = document.querySelector<HTMLElement>("[data-jupyter-panel]")!;
const panelSwitcher = document.querySelector<HTMLElement>("[data-panel-switcher]")!;
const leanTriggerBtn = document.querySelector<HTMLButtonElement>("[data-lean-trigger]")!;
const jupyterTriggerBtn = document.querySelector<HTMLButtonElement>("[data-jupyter-trigger]")!;
const toc = document.querySelector<HTMLElement>("[data-floating-toc]")!;
const tocList = document.querySelector<HTMLElement>("[data-toc-list]")!;
const tocToggle = document.querySelector<HTMLButtonElement>("[data-toc-toggle]")!;
const bookToc = document.querySelector<HTMLElement>("[data-book-toc]")!;
const bookTocList = document.querySelector<HTMLElement>("[data-book-toc-list]")!;
const bookTocToggle = document.querySelector<HTMLButtonElement>("[data-book-toc-toggle]")!;
const localGraph = document.querySelector<HTMLElement>("[data-local-graph]")!;
const localGraphToggle = document.querySelector<HTMLButtonElement>("[data-local-graph-toggle]")!;
const localGraphDepth = document.querySelector<HTMLInputElement>("[data-local-graph-depth]")!;
const localGraphDepthLabel = document.querySelector<HTMLElement>("[data-local-graph-depth-label]")!;
const localGraphRefs = document.querySelector<HTMLInputElement>("[data-local-graph-refs]")!;
const localGraphBacklinks = document.querySelector<HTMLInputElement>("[data-local-graph-backlinks]")!;
const localGraphTags = document.querySelector<HTMLInputElement>("[data-local-graph-tags]")!;
const localGraphCanvas = document.querySelector<HTMLElement>("[data-local-graph-canvas]")!;
const localGraphStatus = document.querySelector<HTMLElement>("[data-local-graph-status]")!;
const draftBanner = document.querySelector<HTMLElement>("[data-draft-banner]")!;
const draftMessage = document.querySelector<HTMLElement>("[data-draft-message]")!;
const draftRecoverButton = document.querySelector<HTMLButtonElement>("[data-action='draft-recover']")!;
const draftDiscardButton = document.querySelector<HTMLButtonElement>("[data-action='draft-discard']")!;

function notesTabButtonElements(): HTMLButtonElement[] {
  return Array.from(document.querySelectorAll<HTMLButtonElement>("[data-notes-tab]"));
}

function notesPanelElements(): HTMLElement[] {
  return Array.from(document.querySelectorAll<HTMLElement>("[data-notes-panel]"));
}

function notesToolVisible(tab: string): boolean {
  return !notesPage.hidden && notesPanelElements().some((panel) => panel.dataset.notesPanel === tab && !panel.hidden);
}

function activeNotesTool(): string {
  return notesPanelElements().find((panel) => !panel.hidden)?.dataset.notesPanel || "";
}

function graphToolVisible(): boolean {
  return notesToolVisible("graph");
}

function standaloneHiddenNotesTool(tab: string): boolean {
  return currentStandalone && ["graph", "git", "lean", "management", "roamlookup"].includes(tab);
}

for (const button of [
  notesButton,
  relationButton,
  agendaButton,
  sourceButton,
  forceSaveButton,
  editorButton,
  editorInlineButton,
  focusModeButton,
  notesShowAllButton,
]) {
  const label = button.textContent?.trim() || button.dataset.action || "Action";
  button.title = label;
  button.setAttribute("aria-label", label);
}

const snippetPopup = document.createElement("div");
snippetPopup.className = "aaronnote-snippet-popup";
snippetPopup.hidden = true;
snippetPopup.setAttribute("role", "listbox");
document.body.appendChild(snippetPopup);

const mathPreview = document.createElement("div");
mathPreview.className = "aaronnote-math-preview";
mathPreview.hidden = true;
document.body.appendChild(mathPreview);

const selectionTool = document.createElement("div");
selectionTool.className = "aaronnote-selection-tool";
selectionTool.innerHTML = `
  <button type="button" data-selection-command="bold" title="Bold">B</button>
  <button type="button" data-selection-command="italic" title="Italic">I</button>
  <button type="button" data-selection-command="highlight" title="Highlight">==</button>
  <button type="button" data-selection-command="strike" title="Strikethrough">~~</button>
  <button type="button" data-selection-command="code" title="Inline code">&lt;&gt;</button>
  <button type="button" data-selection-command="link" title="Link">@</button>
  <span aria-hidden="true"></span>
  <button type="button" data-selection-command="copy" title="Copy">Copy</button>
  <button type="button" data-selection-command="more" title="More actions">...</button>
  <div class="aaronnote-selection-more" data-selection-more hidden>
    <button type="button" data-selection-command="find">Find selected text</button>
    <button type="button" data-selection-command="insert-roam-idlink">Insert roam idlink...</button>
  </div>
`;
selectionTool.hidden = true;
document.body.appendChild(selectionTool);
const selectionMore = selectionTool.querySelector<HTMLElement>("[data-selection-more]")!;
const selectionRoamIdlink = selectionTool.querySelector<HTMLButtonElement>("[data-selection-command='insert-roam-idlink']")!;

const findTool = document.createElement("div");
findTool.className = "aaronnote-find-tool";
findTool.innerHTML = `
  <input data-find-query type="search" placeholder="Find" />
  <input data-find-replace type="text" placeholder="Replace" />
  <select data-find-scope title="Find scope">
    <option value="all">All</option>
    <option value="note">Note</option>
    <option value="code">Code</option>
  </select>
  <label><input data-find-regex type="checkbox" /> Regex</label>
  <span data-find-count></span>
  <button type="button" data-find-action="prev">Prev</button>
  <button type="button" data-find-action="next">Next</button>
  <button type="button" data-find-action="replace">Replace</button>
  <button type="button" data-find-action="all">All</button>
  <button type="button" data-find-action="close">Close</button>
`;
findTool.hidden = true;
document.body.appendChild(findTool);
const findQuery = findTool.querySelector<HTMLInputElement>("[data-find-query]")!;
const findReplace = findTool.querySelector<HTMLInputElement>("[data-find-replace]")!;
const findScope = findTool.querySelector<HTMLSelectElement>("[data-find-scope]")!;
const findRegex = findTool.querySelector<HTMLInputElement>("[data-find-regex]")!;
const findCount = findTool.querySelector<HTMLElement>("[data-find-count]")!;

const quickInsertPopup = document.createElement("div");
quickInsertPopup.className = "aaronnote-quick-popup";
quickInsertPopup.hidden = true;
quickInsertPopup.setAttribute("role", "listbox");
document.body.appendChild(quickInsertPopup);

const jumpOverlay = document.createElement("div");
jumpOverlay.className = "aaronnote-jump-overlay";
jumpOverlay.hidden = true;
document.body.appendChild(jumpOverlay);

const modal = document.createElement("div");
modal.className = "aaronnote-modal";
modal.hidden = true;
document.body.appendChild(modal);

const commandPalette = document.createElement("div");
commandPalette.className = "aaronnote-command-palette";
commandPalette.hidden = true;
commandPalette.innerHTML = `
  <div class="aaronnote-command-scrim" data-command-close></div>
  <section class="aaronnote-command-panel" role="dialog" aria-modal="true" aria-label="Command palette">
    <input data-command-query type="search" placeholder="Run command" autocomplete="off" spellcheck="false" />
    <div data-command-list class="aaronnote-command-list" role="listbox"></div>
  </section>
`;
document.body.appendChild(commandPalette);
const commandQuery = commandPalette.querySelector<HTMLInputElement>("[data-command-query]")!;
const commandList = commandPalette.querySelector<HTMLElement>("[data-command-list]")!;

const leanLocationsPicker = document.createElement("div");
leanLocationsPicker.className = "aaronnote-command-palette aaronnote-lean-locations-picker";
leanLocationsPicker.hidden = true;
leanLocationsPicker.innerHTML = `
  <div class="aaronnote-command-scrim" data-lean-locations-close></div>
  <section class="aaronnote-command-panel" role="dialog" aria-modal="true" aria-label="Lean locations">
    <input data-lean-locations-query type="search" placeholder="Filter locations" autocomplete="off" spellcheck="false" />
    <div data-lean-locations-list class="aaronnote-command-list" role="listbox"></div>
  </section>
`;
document.body.appendChild(leanLocationsPicker);
const leanLocationsQuery = leanLocationsPicker.querySelector<HTMLInputElement>("[data-lean-locations-query]")!;
const leanLocationsList = leanLocationsPicker.querySelector<HTMLElement>("[data-lean-locations-list]")!;
let leanLocationsItems: LeanLocation[] = [];
let leanLocationsOnPick: ((location: LeanLocation) => void) | null = null;
let leanLocationsIndex = 0;

const jumpStackPanel = document.createElement("div");
jumpStackPanel.className = "aaronnote-jump-stack-panel";
jumpStackPanel.hidden = true;
jumpStackPanel.innerHTML = `
  <header class="aaronnote-jump-stack-head">
    <strong>Jump Stack</strong>
    <span data-jump-stack-count>0</span>
  </header>
  <div class="aaronnote-jump-stack-list" data-jump-stack-list></div>
`;
document.body.appendChild(jumpStackPanel);
const jumpStackCount = jumpStackPanel.querySelector<HTMLElement>("[data-jump-stack-count]")!;
const jumpStackList = jumpStackPanel.querySelector<HTMLElement>("[data-jump-stack-list]")!;

const relationPanel = document.createElement("div");
relationPanel.className = "aaronnote-relation-panel";
relationPanel.hidden = true;
relationPanel.innerHTML = `
  <header class="aaronnote-relation-head">
    <strong>Relation</strong>
    <div class="aaronnote-relation-actions">
      <button type="button" data-relation-refresh>Refresh</button>
      <button type="button" data-relation-close>Close</button>
    </div>
  </header>
  <div class="aaronnote-relation-body" data-relation-body></div>
`;
document.body.appendChild(relationPanel);
const relationBody = relationPanel.querySelector<HTMLElement>("[data-relation-body]")!;
const relationRefresh = relationPanel.querySelector<HTMLButtonElement>("[data-relation-refresh]")!;
const relationClose = relationPanel.querySelector<HTMLButtonElement>("[data-relation-close]")!;

const linkPreview = createLinkPreviewController({
  resolveTarget: previewTargetFromHref,
  openNoteContent: api.notes.open,
  openNote,
  openExternalUrl,
  isSafeHref: safeHref,
  noteTitle: relationNoteTitle,
  resolveAssetUrl,
  beforeShow: closeRelationPanel,
  setStatus,
});

const vimCursor = createVimCursor();

let currentFile = "";
window.AaronnoteCurrentFile = () => currentFile;
let leanNotesRoot = "";
let currentMode: "markdown" | "source" = "markdown";
const LARGE_RENDERED_OPEN_BYTES = 1_000_000;
let currentStandalone = false;
const noteCssDebounce = new CoalescedTimer(120);
const saveDebounce = new CoalescedTimer(900);
const draftSaveDebounce = new CoalescedTimer(700);
let cancelDraftIdle: (() => void) | null = null;
let draftSavePending = false;
let notes: NoteSummary[] = [];
let directories: DirectorySummary[] = [];
let files: FileSummary[] = [];
let snippets: SnippetSummary[] = [];
let templates: TemplateSummary[] = [];
let showAllFilesystemEntries = false;
let pendingTodoFocus: { file: string; source: string; index?: number } | null = null;
let assistFrame = 0;
const assistTimer = new CoalescedTimer(35);
let vimMode: VimLiteMode = "insert";
let snippetPopupItems: SnippetSummary[] = [];
let snippetPopupIndex = 0;
let snippetDeleteBefore = 0;
let snippetSuppressedPrefix = "";
let snippetRenderKey = "";
let snippetSuggestionsEnabled = true;
let snippetMouseSuppressed = false;
let quickInsertItems: QuickInsertItem[] = [];
let quickInsertIndex = 0;
let quickInsertDeleteBefore = 0;
let quickInsertRenderKey = "";
let quickInsertSuppressedPrefix = "";
let quickInsertMode: "slash" | "block" = "slash";
let blockMenuPinned = false;
let jumpMode: JumpModeState | null = null;
let commandPaletteIndex = 0;
let commandPaletteRenderKey = "";
let relationRenderKey = "";
const relationEpoch = new Epoch();
let snippetSession: SnippetSession;
let mathPreviewKey = "";
let mathPreviewPendingErrorKey = "";
let mathPreviewUpdateRequested = false;
let snippetScanRequested = false;
let tocUpdateRequested = false;
let selectionToolUpdateRequested = false;
let vimCursorUpdateRequested = false;
const MATH_PREVIEW_ERROR_IDLE_MS = 650;
const MATH_PREVIEW_ERROR_MAX_LENGTH = 180;
const mathPreviewErrorTimer = new CoalescedTimer(MATH_PREVIEW_ERROR_IDLE_MS);

function clearMathPreviewErrorTimer(): void {
  mathPreviewErrorTimer.cancel();
  mathPreviewPendingErrorKey = "";
}

function hideMathPreview(resetKey = true): void {
  clearMathPreviewErrorTimer();
  mathPreview.hidden = true;
  mathPreview.classList.remove("is-error", "is-overflowing");
  if (resetKey) mathPreviewKey = "";
}
type MarkdownFindMatch = FindMatch & { source: "note" };
type LeanFindMatch = FindMatch & { source: "code"; tag: string; view: EditorView; host: HTMLElement };
type AaronFindMatch = MarkdownFindMatch | LeanFindMatch;
let findMatches: AaronFindMatch[] = [];
let findIndex = -1;
const findRefreshTimer = new CoalescedTimer(80);
const findFullScanTimer = new CoalescedTimer(0);
const saveEpoch = new Epoch();
const proseEpoch = new Epoch();
let editRevision = 0;
let savedRevision = 0;
let currentFileMtimeMs = 0;
let currentFileSize = 0;
let saveConflictActive = false;
const notesRefreshDebounce = new CoalescedTimer(1400);
let cancelNotesIdle: (() => void) | null = null;
let notesRefreshPending = false;
let applyingRemoteContent = false;
const saveClientId = (() => {
  try {
    return window.crypto?.randomUUID?.() ?? `${Date.now()}-${Math.random().toString(16).slice(2)}`;
  } catch {
    return `${Date.now()}-${Math.random().toString(16).slice(2)}`;
  }
})();
let pathSuggestions: string[] = [];
let pendingEquationTag = params.get("eqTag") || "";
let pendingInlineTag = params.get("tag") || "";
let pendingDomTarget = params.get("dom") || "";
let pendingOpenAtTop = false;
let activeNoteKind = "";
let noteKindCleanup: (() => void) | null = null;
const noteKindEpoch = new Epoch();
const recentLocalSaveTimer = new CoalescedTimer(650);
let jumpStack: CursorPosition[] = [];
const jumpStackLimit = 24;

const recentStorageKey = "aaronnote.recent";
const writingModeStorageKey = "aaronnote.writingMode";
const snippetSuggestionsStorageKey = "aaronnote.snippetSuggestions.enabled";
const draftStoragePrefix = "aaronnote.draft.";
type StoredDraft = { file: string; content: string; revision: number; updatedAt: number };
type OpenNoteOptions = { newWindow?: boolean; equationTag?: string; inlineTag?: string; domTarget?: string; recordJump?: boolean; scrollTop?: boolean };
type BookTocItem = NonNullable<NoteSummary["bookToc"]>[number];
type BookTocNode = { item: BookTocItem | BookEditorTocItem; key: string; level: number; children: BookTocNode[] };
type DomTargetEntry = {
  label: string;
  slug: string;
  path: string[];
  labelPath: string[];
  level?: number;
  pos?: number;
  to?: number;
  notePath?: string;
};
type JumpTarget = {
  pos: number;
  label: string;
  rect: { left: number; top: number; bottom: number };
};
type JumpModeState =
  | { phase: "target" }
  | { phase: "label"; query: string; typed: string; targets: JumpTarget[] };
let recentNotes = loadRecentNotes();
let writingMode = loadWritingMode();
let pendingDraft: StoredDraft | null = null;
snippetSuggestionsEnabled = loadSnippetSuggestionsEnabled();

const cursorStorageKey = "aaronnote.cursorPositions";
const cursorStorageIndexKey = "aaronnote.cursorPositions.index";
const cursorStorageEntryPrefix = "aaronnote.cursor.";
let cursorPositions = loadCursorPositions();
const cursorSaveTimer = new CoalescedTimer(500);
const cursorLocalSaveTimer = new CoalescedTimer(700);
let lastCursorSaveKey = "";
let editorCursorBeforePanel: CursorPosition | null = null;

type NoteKindContext = {
  kind: string;
  file: string;
  note?: NoteSummary;
  content: string;
  editor: unknown;
  host: HTMLElement;
  root: HTMLElement;
};

type NoteKindModule = {
  default?: (context: NoteKindContext) => void | (() => void);
  setup?: (context: NoteKindContext) => void | (() => void);
  teardown?: (context: NoteKindContext) => void;
};

const demoSnippets: SnippetSummary[] = [
  {
    key: ";",
    name: "Inline math",
    mode: "markdown-mode",
    group: "Aaronnote local",
    body: "$${1:x}$ $0",
  },
  {
    key: "eq",
    name: "Display equation",
    mode: "markdown-mode",
    group: "Aaronnote local",
    body: "$$\n${1:E = mc^2}\n$$\n$0",
  },
  {
    key: "proof",
    name: "Proof block",
    mode: "markdown-mode",
    group: "Aaronnote local",
    body: "#+begin proof\n${1:Proof.}\n#+end proof\n$0",
  },
  {
    key: "thm",
    name: "Theorem block",
    mode: "markdown-mode",
    group: "Aaronnote local",
    body: "#+begin theorem ${1:name}\n${2:Statement.}\n#+end theorem\n$0",
  },
  {
    key: "frac",
    name: "Fraction",
    mode: "tex-mode",
    group: "Aaronnote local",
    body: "\\frac{${1:a}}{${2:b}}$0",
  },
  {
    key: "o+",
    name: "Direct sum",
    mode: "tex-mode",
    group: "Aaronnote local",
    body: "\\oplus",
  },
  {
    key: "ox",
    name: "Tensor product",
    mode: "tex-mode",
    group: "Aaronnote local",
    body: "\\otimes",
  },
];

function scratchStatus(): string {
  return "Scratch";
}

function handleEditorDocumentChange(dirty: boolean): void {
  snippetMouseSuppressed = false;
  scheduleAssistUpdate({ snippets: true, mathPreview: true, toc: true });
  scheduleNoteCssUpdate();
  if (!findTool.hidden) scheduleFindRefresh();
  if (dirty) markDirty();
}

const editor = createEditor(host, {
  initialContent: "",
  onChange: () => {
    handleEditorDocumentChange(!applyingRemoteContent);
  },
});
(window as Window & typeof globalThis & { __aaronoteEditor?: unknown }).__aaronoteEditor = editor;
snippetSession = new SnippetSession(editor);
// Contenteditable widgets (org-env content area) dispatch this to trigger snippet popup
host.addEventListener("aaronnote-assist-update", () => scheduleAssistUpdate({ snippets: true }));
applyWritingMode();

const vim = createVimLite(editor, host, {
  onUndo: () => editor.undo(),
  onRedo: () => editor.redo(),
  onModeChange(mode) {
    vimMode = mode;
    root.dataset.vimMode = mode;
    vimModeEl.textContent = mode === "visual-line" ? "VISUAL LINE" : mode.toUpperCase();
    hideJumpOverlay();
    if (mode !== "insert") {
      hideSnippetPopup();
      hideMathPreview();
      setStatus(mode === "visual-line" ? "VISUAL LINE" : mode.toUpperCase());
      scheduleAssistUpdate();
    } else {
      setStatus(currentFile ? "INSERT" : scratchStatus());
      scheduleAssistUpdate();
    }
  },
});

const filesystemBrowser = createFilesystemBrowser({
  noteList,
  recentList,
  noteFilter,
  noteCount,
  managementCount,
  getNotes: () => notes,
  getDirectories: () => directories,
  getFiles: () => files,
  getRecentNotes: () => recentNotes,
  getCurrentFile: () => currentFile,
  getShowAllFiles: () => showAllFilesystemEntries,
  toggleShowAllFiles: () => {
    showAllFilesystemEntries = !showAllFilesystemEntries;
    syncShowAllButton();
    renderNotes();
    focusFilesystemRangerSoon();
  },
  openNote,
  deleteNote: (note) => void deleteNoteFromBrowser(note),
  deleteFile: (file) => trashFileFromBrowser(file),
  createNode: (dir, behavior) => void createNode(dir, behavior),
  createFolder: (dir) => createFolderFromBrowser(dir),
  renameNote: (note) => renameNoteFromBrowser(note),
  renameFile: (file) => renameFileFromBrowser(file),
  renameDirectory: (dir) => renameDirectoryFromBrowser(dir),
  moveNote: (note) => moveNoteFromBrowser(note),
  moveFile: (file) => moveFileFromBrowser(file),
  moveDirectory: (dir) => moveDirectoryFromBrowser(dir),
  duplicateNote: (note) => duplicateNoteFromBrowser(note),
  duplicateFile: (file) => duplicateFileFromBrowser(file),
  trashDirectory: (dir) => trashDirectoryFromBrowser(dir),
  revealPath: api.shell.available() ? (path) => revealPathFromBrowser(path) : undefined,
  openDirectory: api.shell.available() ? (path) => openDirectoryFromBrowser(path) : undefined,
  openDirectoryInKitty: api.shell.available() ? (path) => openDirectoryInKittyFromBrowser(path) : undefined,
});

function focusFilesystemRangerSoon(attempts = 8): void {
  if (!notesToolVisible("filesystem")) return;
  const run = (remaining: number) => {
    window.requestAnimationFrame(() => {
      if (filesystemBrowser.focus() || remaining <= 1) return;
      run(remaining - 1);
    });
  };
  run(attempts);
}

function focusRecentListSoon(attempts = 8): void {
  if (!notesToolVisible("recent")) return;
  const run = (remaining: number) => {
    window.requestAnimationFrame(() => {
      if (filesystemBrowser.focusRecent() || remaining <= 1) return;
      run(remaining - 1);
    });
  };
  run(attempts);
}

const agendaManager = createAgendaManager({
  filter: agendaFilter,
  sort: agendaSort,
  group: agendaGroup,
  done: agendaDone,
  count: agendaCount,
  list: agendaList,
  isVisible: () => notesToolVisible("agenda"),
  getNotes: () => notes,
  getCurrentFile: () => currentFile,
  getAgendaScopeFile: () => currentStandalone ? currentFile : "",
  setStatus,
  setPendingTodoFocus: (focus) => {
    pendingTodoFocus = focus;
  },
  showEditorPage,
  jumpToTodoSource,
  openNote,
});

const unusedAssetsManager = createUnusedAssetsManager({
  section: unusedAssetsSection,
  count: unusedAssetsCount,
  list: unusedAssetsList,
  selectAll: unusedAssetsSelectAll,
  scanButton: scanUnusedAssetsButton,
  trashButton: trashUnusedAssetsButton,
  setStatus,
  openFormModal,
});

const floatingTocPanel = createFloatingTocPanel({
  toc,
  toggleButton: tocToggle,
  list: tocList,
  editor,
  getNotes: () => notes,
  getCurrentFile: () => currentFile,
  resolveNoteRef,
  openNote,
  openTag: openTagFilter,
});

const localGraphPanel = createLocalGraphPanel({
  root: localGraph,
  toggleButton: localGraphToggle,
  depthInput: localGraphDepth,
  depthLabel: localGraphDepthLabel,
  refsInput: localGraphRefs,
  backlinksInput: localGraphBacklinks,
  tagsInput: localGraphTags,
  canvas: localGraphCanvas,
  status: localGraphStatus,
  getNotes: () => notes,
  getCurrentNote: currentNote,
  getMarkdown: () => editor.getMarkdown(),
  resolveNoteRef,
  openNote,
  openTag: openTagFilter,
});

const leanPanel = createLeanPanel({
  root: leanPanelRoot,
  getEditor: () => editor,
  jumpToNoteOffset: (offset) => {
    editor.view.dispatch({ selection: { anchor: offset }, scrollIntoView: true });
  },
  onVisibilityChange: syncPanelSwitcher,
});

const jupyterPanel = createJupyterPanel({
  root: jupyterPanelRoot,
  setStatus,
  onVisibilityChange: syncPanelSwitcher,
});

const TOOL_PANEL_WIDTH_KEY = "aaronnote.toolPanel.width";
const TOOL_PANEL_MIN_WIDTH = 320;
const TOOL_PANEL_MAX_WIDTH = 920;
let toolPanelWidth = (() => {
  const stored = Number(window.localStorage.getItem(TOOL_PANEL_WIDTH_KEY));
  return Number.isFinite(stored) && stored > 0 ? stored : 560;
})();

function clampToolPanelWidth(width: number): number {
  const maxByViewport = Math.max(TOOL_PANEL_MIN_WIDTH, Math.min(TOOL_PANEL_MAX_WIDTH, window.innerWidth - 220));
  return Math.max(TOOL_PANEL_MIN_WIDTH, Math.min(maxByViewport, Math.round(width)));
}

function applyToolPanelWidth(width = toolPanelWidth): void {
  toolPanelWidth = clampToolPanelWidth(width);
  const value = `${toolPanelWidth}px`;
  toolPanelRoot.style.setProperty("--lean-panel-width", value);
  document.body.style.setProperty("--lean-panel-width", value);
  window.localStorage.setItem(TOOL_PANEL_WIDTH_KEY, String(toolPanelWidth));
}

applyToolPanelWidth();

function syncPanelSwitcher(): void {
  const jupyterAvailable = api.jupyter.available();
  const panelVisible = leanPanel.visible || jupyterPanel.visible;
  toolPanelRoot.hidden = !panelVisible;
  toolPanelRoot.classList.toggle("tool-panel--hidden", !panelVisible);
  leanToolPane.hidden = !leanPanel.visible;
  jupyterToolPane.hidden = !jupyterPanel.visible;
  leanToolTab.hidden = leanTriggerBtn.hidden;
  jupyterToolTab.hidden = !jupyterAvailable && !jupyterPanel.visible;
  leanToolTab.classList.toggle("is-active", leanPanel.visible);
  jupyterToolTab.classList.toggle("is-active", jupyterPanel.visible);
  leanToolTab.setAttribute("aria-selected", leanPanel.visible ? "true" : "false");
  jupyterToolTab.setAttribute("aria-selected", jupyterPanel.visible ? "true" : "false");
  if (panelVisible) applyToolPanelWidth();
  jupyterTriggerBtn.hidden = true;
  panelSwitcher.hidden = true;
  leanTriggerBtn.classList.toggle("is-active", leanPanel.visible);
  leanTriggerBtn.setAttribute("aria-pressed", leanPanel.visible ? "true" : "false");
  jupyterTriggerBtn.classList.toggle("is-active", jupyterPanel.visible);
  jupyterTriggerBtn.setAttribute("aria-pressed", jupyterPanel.visible ? "true" : "false");
}

function showLeanPanelTab(): void {
  if (leanTriggerBtn.hidden) return;
  if (!leanPanel.visible) {
    jupyterPanel.hide();
    leanPanel.show();
  }
  syncPanelSwitcher();
}

function showJupyterPanelTab(): void {
  if (!api.jupyter.available()) {
    setStatus("Jupyter preview unavailable");
    return;
  }
  if (!jupyterPanel.visible) {
    leanPanel.hide();
    jupyterPanel.show();
  }
  syncPanelSwitcher();
}

function toggleLeanPanel(): void {
  if (leanPanel.visible) {
    leanPanel.hide();
  } else {
    jupyterPanel.hide();
    leanPanel.show();
  }
  syncPanelSwitcher();
}

function toggleJupyterPanel(): void {
  if (!api.jupyter.available()) {
    setStatus("Jupyter preview unavailable");
    return;
  }
  if (jupyterPanel.visible) {
    jupyterPanel.hide();
  } else {
    leanPanel.hide();
    jupyterPanel.show();
  }
  syncPanelSwitcher();
}

leanTriggerBtn.addEventListener("click", toggleLeanPanel);
jupyterTriggerBtn.addEventListener("click", toggleJupyterPanel);
leanToolTab.addEventListener("click", showLeanPanelTab);
jupyterToolTab.addEventListener("click", showJupyterPanelTab);
toolPanelResizer.addEventListener("mousedown", (event) => {
  event.preventDefault();
  toolPanelRoot.classList.add("tool-panel--resizing");
  const onMove = (moveEvent: MouseEvent) => applyToolPanelWidth(moveEvent.clientX);
  const onUp = (): void => {
    toolPanelRoot.classList.remove("tool-panel--resizing");
    document.removeEventListener("mousemove", onMove);
    document.removeEventListener("mouseup", onUp);
  };
  document.addEventListener("mousemove", onMove);
  document.addEventListener("mouseup", onUp, { once: true });
});
window.addEventListener("resize", () => {
  if (!toolPanelRoot.hidden) applyToolPanelWidth();
});
syncPanelSwitcher();

let activeLeanRegionForCommand: { notePath: string; tag: string; selector: string; leanPath: string } | null = null;

window.addEventListener("aaronnote:lean-region-infoview", (event) => {
  const detail = (event as CustomEvent<{ notePath?: string; tag?: string; selector?: string; leanPath?: string }>).detail;
  const notePath = String(detail?.notePath ?? "");
  const tag = String(detail?.tag ?? "").trim();
  const selector = canonicalLeanSelector(String(detail?.selector ?? ""));
  const leanPath = String(detail?.leanPath ?? "");
  activeLeanRegionForCommand = notePath && tag ? { notePath, tag, selector, leanPath } : null;
});

window.addEventListener("aaronnote:lean-region-active", (event) => {
  const detail = (event as CustomEvent<{ notePath?: string; tag?: string; selector?: string; leanPath?: string }>).detail;
  const notePath = String(detail?.notePath ?? "");
  const tag = String(detail?.tag ?? "").trim();
  const selector = canonicalLeanSelector(String(detail?.selector ?? ""));
  const leanPath = String(detail?.leanPath ?? "");
  activeLeanRegionForCommand = notePath && tag ? { notePath, tag, selector, leanPath } : null;
});

async function restartLeanServerForCurrentNote(): Promise<void> {
  if (!api.lean.available()) {
    setStatus("Lean unavailable");
    return;
  }
  const splice = editor.view.state.field(leanSpliceField, false);
  const region = scanMarkdownLeanPlaceholders(editor.getMarkdown())[0] ?? null;
  const regionTag = region?.tag ?? "";
  if (!currentFile || !leanNotesRoot || (!splice && !regionTag)) {
    setStatus("No Lean document active");
    return;
  }
  setStatus("Lean restarting");
  try {
    await api.lean.request("stop");
    const result = regionTag
      ? await api.lean.openRegionFile({ notePath: currentFile, tag: regionTag, selector: region?.selector ?? "" })
      : await api.lean.openNote({
        notePath: currentFile,
        notesRoot: leanNotesRoot,
        leanText: splice!.leanText,
        leanPath: splice!.leanPath,
      });
    const response = result as { ok?: boolean; message?: string } | null;
    if (response?.ok === false) throw new Error(response.message || "Lean restart failed");
    setStatus("Lean restarted");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Lean restart failed");
  }
}

function generatedLeanTag(): string {
  return `lean-${Date.now().toString(36)}`;
}

type LeanPlaceholderRef = {
  tag: string;
  selector: string;
  from: number;
  to: number;
  lineFrom: number;
  lineTo: number;
};

function scanMarkdownLeanPlaceholders(markdown: string): LeanPlaceholderRef[] {
  return scanMarkdownLeanPlaceholdersShared(markdown).map((placeholder) => ({
    tag: placeholder.tag,
    selector: placeholder.selector,
    from: placeholder.from,
    to: placeholder.to,
    lineFrom: placeholder.lineFrom,
    lineTo: placeholder.lineTo,
  }));
}

function leanPlaceholderContextAt(markdown: string, pos: number, selector = ""): { beforeTag: string; afterTag: string } {
  const cursor = Math.max(0, Math.min(markdown.length, pos));
  const targetSelector = canonicalLeanSelector(selector);
  let beforeTag = "";
  let afterTag = "";
  for (const placeholder of scanMarkdownLeanPlaceholders(markdown)) {
    if (placeholder.selector !== targetSelector) continue;
    if (placeholder.lineFrom < cursor && placeholder.lineTo <= cursor) {
      beforeTag = placeholder.tag;
      continue;
    }
    afterTag = placeholder.tag;
    break;
  }
  return { beforeTag, afterTag };
}

function leanPlaceholderByTag(markdown: string, tag: string, selector = ""): LeanPlaceholderRef | null {
  const cleanTag = tag.trim();
  const cleanSelector = canonicalLeanSelector(selector);
  if (!cleanTag) return null;
  return scanMarkdownLeanPlaceholders(markdown).find((placeholder) => placeholder.tag === cleanTag && placeholder.selector === cleanSelector) ?? null;
}

function currentLeanPlaceholder(markdown: string, pos: number): LeanPlaceholderRef | null {
  const cursor = Math.max(0, Math.min(markdown.length, pos));
  const placeholders = scanMarkdownLeanPlaceholders(markdown);
  for (const placeholder of placeholders) {
    if (cursor >= placeholder.lineFrom && cursor <= placeholder.lineTo) return placeholder;
  }
  return null;
}

function editorLeanPlaceholderAt(pos: number): LeanPlaceholderRef | null {
  const doc = editor.view.state.doc;
  const line = doc.lineAt(Math.max(0, Math.min(doc.length, pos)));
  const parsed = parseLeanPlaceholderLine(line.text);
  if (!parsed) return null;
  return {
    tag: parsed.tag,
    selector: canonicalLeanSelector(parsed.selector),
    from: line.from + parsed.commandFrom,
    to: line.from + parsed.commandTo,
    lineFrom: line.from,
    lineTo: line.to,
  };
}

function removeLeanPlaceholderLine(markdown: string, placeholder: LeanPlaceholderRef): void {
  let from = placeholder.lineFrom;
  let to = placeholder.lineTo;
  if (to < markdown.length && markdown[to] === "\n") {
    to += 1;
  } else if (from > 0 && markdown[from - 1] === "\n") {
    from -= 1;
  }
  editor.replaceMarkdownRange(from, to, "", "start");
}

type LeanTargetInfo = {
  label: string;
  selector: string;
  tags: string[];
  targetKind?: string;
  leanPath?: string;
};

function leanTargetLabel(selector: string, targetKind = ""): string {
  if (!selector) return "Default mirror";
  if (selector === "newfile") return "New mirror";
  if (targetKind === "link") return `Lean file ${selector}`;
  if (targetKind === "extra-mirror" || /^newfile:\d+$/.test(selector)) return `Mirror ${selector.replace(/^newfile:/, "")}`;
  return selector;
}

async function leanContextTargets(): Promise<LeanTargetInfo[]> {
  const out = new Map<string, LeanTargetInfo>();
  const mergeTarget = (selectorValue: string, targetKind = "", tags: string[] = [], leanPath = ""): void => {
    const selector = canonicalLeanSelector(selectorValue);
    const current = out.get(selector);
    const mergedTags = [...new Set([
      ...(current?.tags ?? []),
      ...tags.map((tag) => String(tag || "").trim()).filter(Boolean),
    ])];
    out.set(selector, {
      label: leanTargetLabel(selector, targetKind || current?.targetKind || ""),
      selector,
      tags: mergedTags,
      targetKind: targetKind || current?.targetKind,
      leanPath: leanPath || current?.leanPath,
    });
  };
  mergeTarget("", "default-mirror");
  mergeTarget("newfile", "extra-mirror");
  for (const block of currentNote()?.leanBlocks ?? []) {
    const selector = canonicalLeanSelector(String(block.selector ?? ""));
    if (!selector) continue;
    mergeTarget(selector, String(block.targetKind ?? ""), [], String(block.leanPath ?? ""));
  }
  if (api.lean.available() && currentFile && leanNotesRoot && !currentStandalone) {
    try {
      const response = await api.lean.request("targets", { notePath: currentFile }) as {
        targets?: Array<{ selector?: string; label?: string; targetKind?: string; tags?: string[]; leanPath?: string }>;
      };
      for (const target of response.targets ?? []) {
        const selector = canonicalLeanSelector(String(target.selector ?? ""));
        mergeTarget(selector, String(target.targetKind ?? ""), Array.isArray(target.tags) ? target.tags : [], String(target.leanPath ?? ""));
      }
    } catch (err) {
      console.warn("[lean] targets query failed", err);
    }
  }
  return [...out.values()];
}

function leanContextMenuOptions(pos: number | null): { leanBlock?: { tag: string; selector: string } } {
  const placeholder = pos == null ? null : editorLeanPlaceholderAt(pos);
  return {
    ...(placeholder ? { leanBlock: { tag: placeholder.tag, selector: placeholder.selector } } : {}),
  };
}

function nextLeanNewfileSelector(markdown: string): string {
  let next = 1;
  for (const placeholder of scanMarkdownLeanPlaceholders(markdown)) {
    const match = /^newfile:(\d+)$/.exec(placeholder.selector);
    if (match) next = Math.max(next, Number(match[1]) + 1);
  }
  return `newfile:${next}`;
}

function slashPath(value: string): string {
  return String(value || "").replace(/\\/g, "/").replace(/\/+/g, "/");
}

function pathDirname(value: string): string {
  const path = slashPath(value);
  const index = path.lastIndexOf("/");
  return index >= 0 ? path.slice(0, index) : "";
}

function pathRelative(fromDir: string, toPath: string): string {
  const from = slashPath(fromDir).split("/").filter(Boolean);
  const to = slashPath(toPath).split("/").filter(Boolean);
  while (from.length && to.length && from[0] === to[0]) {
    from.shift();
    to.shift();
  }
  return [...from.map(() => ".."), ...to].join("/") || ".";
}

function leanPathForCurrentFile(): string {
  const file = slashPath(currentFile);
  const root = slashPath(leanNotesRoot || "");
  const leanRoot = `${root}/.lean`;
  if (file.toLowerCase().endsWith(".lean") && (file === leanRoot || file.startsWith(`${leanRoot}/`))) return file;
  const rel = file.startsWith(`${root}/`) ? file.slice(root.length + 1) : file.split("/").pop() || "note.md";
  const leanRel = rel.toLowerCase().endsWith(".md") ? `${rel.slice(0, -3)}.lean` : `${rel}.lean`;
  return `${leanRoot}/${leanRel}`;
}

function leanSelectorFromPickedFile(picked: string): string {
  const root = slashPath(leanNotesRoot || "");
  const leanRoot = `${root}/.lean`;
  const raw = slashPath(picked);
  const absolute = raw.startsWith("/") ? raw : `${root}/${raw}`;
  if (!(absolute === leanRoot || absolute.startsWith(`${leanRoot}/`))) {
    throw new Error("Lean file must be inside .lean");
  }
  return pathRelative(pathDirname(leanPathForCurrentFile()), absolute);
}

function leanTagSuggestionsForSelector(selector: string, targets: readonly LeanTargetInfo[] = []): string[] {
  const clean = canonicalLeanSelector(selector);
  const target = targets.find((item) => item.selector === clean);
  if (target) return [...new Set(target.tags)];
  if (clean && !/^newfile(?::\d+)?$/.test(clean)) return [];
  return scanMarkdownLeanPlaceholders(editor.getMarkdown())
    .filter((placeholder) => placeholder.selector === clean)
    .map((placeholder) => placeholder.tag);
}

function leanMirrorNumberSuggestions(markdown: string): string[] {
  const out = new Set<string>();
  for (const placeholder of scanMarkdownLeanPlaceholders(markdown)) {
    const match = /^newfile:(\d+)$/.exec(placeholder.selector);
    if (match) out.add(match[1]);
  }
  out.add(nextLeanNewfileSelector(markdown).replace(/^newfile:/, ""));
  return [...out].sort((a, b) => Number(a) - Number(b));
}

type LeanBlockModalInitial = {
  fileMode: "default" | "mirror" | "link";
  number: string;
  file: string;
  tag: string;
  numbers: string[];
  targets: LeanTargetInfo[];
};

function openLeanBlockModal(initial: LeanBlockModalInitial): Promise<Record<string, string> | null> {
  return new Promise((resolve) => {
    modal.innerHTML = "";
    const panel = document.createElement("form");
    panel.className = "aaronnote-modal-panel aaronnote-lean-block-modal";
    const heading = document.createElement("h2");
    heading.textContent = "Insert Lean Block";
    panel.appendChild(heading);

    const listId = (id: string): string => `aaronnote-lean-block-${id}`;
    const datalist = (id: string, values: string[]): HTMLDataListElement => {
      const list = document.createElement("datalist");
      list.id = listId(id);
      for (const value of [...new Set(values.filter(Boolean))]) {
        const option = document.createElement("option");
        option.value = value;
        list.appendChild(option);
      }
      panel.appendChild(list);
      return list;
    };
    datalist("number", initial.numbers);
    const tagList = datalist("tag", leanTagSuggestionsForSelector(initial.fileMode === "mirror" ? `newfile:${initial.number}` : initial.fileMode === "link" ? initial.file : "", initial.targets));

    const labelWrap = (text: string): HTMLLabelElement => {
      const label = document.createElement("label");
      label.textContent = text;
      return label;
    };
    const select = (name: string, value: string, options: Array<{ label: string; value: string }>): HTMLSelectElement => {
      const input = document.createElement("select");
      input.name = name;
      for (const item of options) {
        const option = document.createElement("option");
        option.value = item.value;
        option.textContent = item.label;
        input.appendChild(option);
      }
      input.value = value;
      return input;
    };
    const input = (name: string, value: string, list = ""): HTMLInputElement => {
      const control = document.createElement("input");
      control.type = "text";
      control.name = name;
      control.value = value;
      if (list) control.setAttribute("list", list);
      return control;
    };
    const segmented = (
      name: string,
      value: string,
      options: Array<{ label: string; value: string }>,
    ): { wrap: HTMLDivElement; value: () => string; setValue: (next: string) => void; onChange: (handler: () => void) => void } => {
      let current = value;
      const handlers = new Set<() => void>();
      const wrap = document.createElement("div");
      wrap.className = "aaronnote-modal-segmented";
      wrap.setAttribute("role", "group");
      wrap.dataset.name = name;
      const sync = (): void => {
        for (const button of wrap.querySelectorAll<HTMLButtonElement>("button")) {
          const active = button.value === current;
          button.classList.toggle("is-active", active);
          button.setAttribute("aria-pressed", active ? "true" : "false");
        }
      };
      for (const optionSpec of options) {
        const button = document.createElement("button");
        button.type = "button";
        button.value = optionSpec.value;
        button.textContent = optionSpec.label;
        button.addEventListener("click", () => {
          if (current === optionSpec.value) return;
          current = optionSpec.value;
          sync();
          for (const handler of handlers) handler();
        });
        wrap.appendChild(button);
      }
      sync();
      return {
        wrap,
        value: () => current,
        setValue: (next) => {
          current = next;
          sync();
          for (const handler of handlers) handler();
        },
        onChange: (handler) => { handlers.add(handler); },
      };
    };

    const targetLabel = labelWrap("Target");
    const targetMode = segmented("fileMode", initial.fileMode, [
      { label: "Default mirror", value: "default" },
      { label: "Mirror number", value: "mirror" },
      { label: "Lean file", value: "link" },
    ]);
    targetLabel.appendChild(targetMode.wrap);

    const tagModeLabel = labelWrap("Tag source");
    const tagMode = segmented("tagMode", "new", [
      { label: "New tag", value: "new" },
      { label: "Existing tag", value: "existing" },
    ]);
    tagModeLabel.appendChild(tagMode.wrap);

    const numberLabel = labelWrap("Mirror number");
    const number = input("number", initial.number, listId("number"));
    const numberChoices = document.createElement("div");
    numberChoices.className = "aaronnote-modal-choice-grid";
    for (const value of initial.numbers.slice(0, 18)) {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = value;
      button.addEventListener("click", () => {
        number.value = value;
        updateTags();
      });
      numberChoices.appendChild(button);
    }
    numberLabel.append(number, numberChoices);

    const fileLabel = labelWrap("Lean file");
    const fileRow = document.createElement("div");
    fileRow.className = "aaronnote-modal-path-row";
    const file = input("file", initial.file);
    const choose = document.createElement("button");
    choose.type = "button";
    choose.textContent = "Choose";
    choose.addEventListener("click", async () => {
      try {
        const picked = await window.AaronnoteDesktop?.chooseNotePath?.({
          suggestedPath: ".lean",
          title: "Choose Lean file",
          mode: "openFile",
        });
        if (picked) {
          file.value = leanSelectorFromPickedFile(picked);
          targetMode.setValue("link");
          updateTags();
          file.focus();
        }
      } catch (err) {
        setStatus(err instanceof Error ? err.message : "Lean file choose failed");
      }
    });
    fileRow.append(file, choose);
    fileLabel.append(fileRow);

    const tagLabel = labelWrap("Tag");
    const tag = input("tag", initial.tag || generatedLeanTag(), listId("tag"));
    const existingTag = select("existingTag", "", []);
    const tagChoices = document.createElement("div");
    tagChoices.className = "aaronnote-modal-choice-grid";
    tagLabel.append(tag, existingTag, tagChoices);

    const error = document.createElement("div");
    error.className = "aaronnote-modal-field-error";
    error.hidden = true;

    const hint = document.createElement("div");
    hint.className = "aaronnote-modal-field-hint";
    hint.hidden = true;

    const actions = document.createElement("div");
    actions.className = "aaronnote-modal-actions";
    const cancel = document.createElement("button");
    cancel.type = "button";
    cancel.textContent = "Cancel";
    const submit = document.createElement("button");
    submit.type = "submit";
    submit.textContent = "Insert";
    actions.append(cancel, submit);
    panel.append(targetLabel, numberLabel, fileLabel, tagModeLabel, tagLabel, error, hint, actions);

    const selector = (): string => targetMode.value() === "default" ? "" : targetMode.value() === "mirror" ? `newfile:${number.value.trim()}` : file.value.trim();
    const currentTagChoices = (): string[] => leanTagSuggestionsForSelector(selector(), initial.targets);
    const renderTagChoices = (): void => {
      const choices = currentTagChoices();
      tagList.replaceChildren();
      existingTag.replaceChildren();
      tagChoices.replaceChildren();
      for (const value of choices) {
        const option = document.createElement("option");
        option.value = value;
        tagList.appendChild(option);
      }
      for (const value of choices) {
        const option = document.createElement("option");
        option.value = value;
        option.textContent = value;
        existingTag.appendChild(option);
      }
      if (choices.length > 0 && !choices.includes(existingTag.value)) existingTag.value = choices[0] ?? "";
      for (const value of choices.slice(0, 24)) {
        const button = document.createElement("button");
        button.type = "button";
        button.textContent = value;
        button.addEventListener("click", () => {
          existingTag.value = value;
          tagMode.setValue("existing");
          update();
        });
        tagChoices.appendChild(button);
      }
    };
    function update(): void {
      const mode = targetMode.value();
      const source = tagMode.value();
      const choices = currentTagChoices();
      numberLabel.style.display = mode === "mirror" ? "grid" : "none";
      fileLabel.style.display = mode === "link" ? "grid" : "none";
      tag.style.display = source === "new" ? "" : "none";
      existingTag.style.display = source === "existing" ? "" : "none";
      existingTag.disabled = source === "existing" && choices.length === 0;
      tagChoices.style.display = source === "existing" ? "flex" : "none";
      for (const button of numberChoices.querySelectorAll("button")) button.classList.toggle("is-active", button.textContent === number.value.trim());
      for (const button of tagChoices.querySelectorAll("button")) button.classList.toggle("is-active", button.textContent === existingTag.value.trim());
      let message = "";
      if (mode === "mirror" && !/^[1-9]\d*$/.test(number.value.trim())) message = "Use a positive mirror number.";
      if (mode === "link" && !file.value.trim()) message = "Choose a Lean file.";
      if (source === "new" && !tag.value.trim()) message = "Tag is required.";
      if (source === "existing" && choices.length === 0) message = "No tags in the selected Lean file.";
      if (source === "existing" && choices.length > 0 && !choices.includes(existingTag.value.trim())) message = "Choose an existing tag from the selected Lean file.";
      error.textContent = message;
      error.hidden = !message;
      submit.disabled = Boolean(message);
      let hintMessage = "";
      if (!message) {
        if (mode === "mirror" && /^[1-9]\d*$/.test(number.value.trim())) {
          const mirrorSel = `newfile:${number.value.trim()}`;
          const known = leanTagSuggestionsForSelector(mirrorSel, initial.targets);
          hintMessage = known.length === 0 ? "New mirror file will be created" : `Mirror exists with ${known.length} tag${known.length === 1 ? "" : "s"}`;
        }
        if (source === "new" && tag.value.trim() && choices.includes(tag.value.trim())) {
          hintMessage = "Tag already exists — will write to the same region";
        }
      }
      hint.textContent = hintMessage;
      hint.hidden = !hintMessage;
    }
    const updateTags = (): void => {
      renderTagChoices();
      const choices = currentTagChoices();
      if (tagMode.value() === "existing" && choices.length > 0 && !choices.includes(existingTag.value.trim())) existingTag.value = choices[0] ?? "";
      update();
    };
    targetMode.onChange(updateTags);
    number.addEventListener("input", updateTags);
    file.addEventListener("input", updateTags);
    tagMode.onChange(() => {
      if (tagMode.value() === "new" && !tag.value.trim()) tag.value = generatedLeanTag();
      if (tagMode.value() === "existing") {
        const choices = currentTagChoices();
        if (choices.length > 0 && !choices.includes(existingTag.value.trim())) existingTag.value = choices[0] ?? "";
      }
      update();
      (tagMode.value() === "new" ? tag : existingTag).focus();
    });
    tag.addEventListener("input", update);
    existingTag.addEventListener("change", update);
    const close = (value: Record<string, string> | null): void => {
      modal.hidden = true;
      modal.innerHTML = "";
      resolve(value);
    };
    cancel.addEventListener("click", () => close(null));
    panel.addEventListener("submit", (event) => {
      event.preventDefault();
      update();
      if (submit.disabled) return;
      close({
        fileMode: targetMode.value(),
        number: number.value.trim(),
        file: file.value.trim(),
        tag: tagMode.value() === "new" ? tag.value.trim() : existingTag.value.trim(),
        tagMode: tagMode.value(),
      });
    });
    modal.addEventListener("mousedown", (event) => {
      if (event.target === modal) close(null);
    }, { once: true });
    modal.appendChild(panel);
    modal.hidden = false;
    renderTagChoices();
    update();
    targetMode.wrap.querySelector<HTMLButtonElement>("button.is-active")?.focus();
  });
}

async function insertLeanBlock(options: { selector?: string; tag?: string } = {}): Promise<void> {
  if (!api.lean.available()) {
    setStatus("Lean unavailable");
    return;
  }
  if (!currentFile || !leanNotesRoot || currentStandalone) {
    setStatus("Lean blocks require a roam markdown note");
    return;
  }
  const tag = String(options.tag ?? "").trim() || generatedLeanTag();
  const markdown = editor.getMarkdown();
  const selector = canonicalLeanSelector(options.selector === "newfile"
    ? nextLeanNewfileSelector(markdown)
    : String(options.selector ?? ""));
  setStatus("Creating Lean block");
  try {
    const context = leanPlaceholderContextAt(markdown, editor.getMarkdownSelection().from, selector);
    const result = await api.lean.ensureRegion({ notePath: currentFile, tag, selector, ...context });
    const response = result as { ok?: boolean; tag?: string; message?: string } | null;
    if (response?.ok === false) throw new Error(response.message || "Lean region create failed");
    const finalTag = response?.tag || tag;
    editor.insertText(formatLeanPlaceholder(selector, finalTag));
    scheduleAssistUpdate();
    setStatus(`Lean block ${finalTag}`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Lean block create failed");
  }
}

async function cleanCurrentLeanBlock(options: { tag?: string; selector?: string } = {}): Promise<void> {
  if (!api.lean.available()) {
    setStatus("Lean unavailable");
    return;
  }
  if (!currentFile || !leanNotesRoot || currentStandalone) {
    setStatus("Lean cleanup requires a roam markdown note");
    return;
  }
  const markdown = editor.getMarkdown();
  const requestedTag = String(options.tag ?? "").trim();
  const requestedSelector = canonicalLeanSelector(String(options.selector ?? ""));
  const activeTag = requestedTag || (activeLeanRegionForCommand?.notePath === currentFile ? activeLeanRegionForCommand.tag : "");
  const activeSelector = requestedTag ? requestedSelector : (activeLeanRegionForCommand?.notePath === currentFile ? activeLeanRegionForCommand.selector : "");
  const placeholder = leanPlaceholderByTag(markdown, activeTag, activeSelector)
    ?? currentLeanPlaceholder(markdown, editor.getMarkdownSelection().from);
  if (!placeholder) {
    setStatus("No Lean tag at cursor");
    return;
  }
  setStatus("Cleaning Lean block");
  try {
    const result = await api.lean.deleteRegion({ notePath: currentFile, tag: placeholder.tag, selector: placeholder.selector });
    const response = result as { ok?: boolean; message?: string } | null;
    if (response?.ok === false) throw new Error(response.message || "Lean region cleanup failed");
    removeLeanPlaceholderLine(markdown, placeholder);
    if (activeLeanRegionForCommand?.tag === placeholder.tag && activeLeanRegionForCommand.selector === placeholder.selector) activeLeanRegionForCommand = null;
    scheduleAssistUpdate();
    setStatus(`Lean block ${placeholder.tag} cleaned`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Lean block cleanup failed");
  }
}

async function openLeanBlockManager(): Promise<void> {
  if (!currentFile || !leanNotesRoot || currentStandalone) {
    setStatus("Lean blocks require a roam markdown note");
    return;
  }
  const markdown = editor.getMarkdown();
  const cursor = editor.getMarkdownSelection().from;
  const activeRegion = activeLeanRegionForCommand?.notePath === currentFile ? activeLeanRegionForCommand : null;
  const placeholder = currentLeanPlaceholder(markdown, cursor)
    ?? (activeRegion
      ? leanPlaceholderByTag(markdown, activeRegion.tag, activeRegion.selector)
      : null);
  const targets = await leanContextTargets();
  const currentSelector = placeholder?.selector ?? activeRegion?.selector ?? "";
  const currentNewfile = /^newfile:(\d+)$/.exec(currentSelector);
  const values = await openLeanBlockModal({
    fileMode: currentNewfile ? "mirror" : currentSelector ? "link" : "default",
    number: currentNewfile?.[1] ?? nextLeanNewfileSelector(markdown).replace(/^newfile:/, ""),
    file: currentSelector && !currentNewfile ? currentSelector : "",
    tag: generatedLeanTag(),
    numbers: leanMirrorNumberSuggestions(markdown),
    targets,
  });
  if (!values) { editor.focus(); return; }
  const selector = values.fileMode === "default"
    ? ""
    : values.fileMode === "mirror"
      ? `newfile:${values.number}`
      : values.file;
  await insertLeanBlock({ selector, tag: values.tag });
}

// Fetch notesRoot from lean status once on startup
if (api.lean.available()) {
  api.lean.onStatus((raw) => {
    const data = raw as { message?: string; busy?: boolean };
    const message = String(data.message || "");
    if (message) setStatus(`Lean: ${message}`);
  });
  void api.lean.status().then((s) => {
    const status = s as { notesRoot?: string } | null;
    if (status?.notesRoot) leanNotesRoot = status.notesRoot;
  }).catch((err) => console.warn("[lean] status query failed", err));
}

const graphPanel = createGraphPanel({
  page: graphPage,
  filter: graphFilter,
  stats: graphStats,
  canvas: graphCanvas,
  focusPanel: graphFocus,
  getNotes: () => notes,
  openNote,
});

type LazyGitPanel = { refresh: () => void; deactivate: () => void };
let gitPanel: LazyGitPanel | null = null;
let gitPanelLoading: Promise<LazyGitPanel> | null = null;
const gitPanelEpoch = new Epoch();

function deactivateGitPanel(): void {
  gitPanelEpoch.cancel();
  gitPanel?.deactivate();
}

function activateGitPanel(): void {
  const run = gitPanelEpoch.begin();
  if (gitPanel) {
    gitPanel.refresh();
    return;
  }
  if (!gitPanelLoading) {
    setStatus("Loading git panel");
    gitPanelLoading = import("./git-panel.ts")
      .then(({ createGitPanel }) => {
        const panel = createGitPanel({
          root: gitRoot,
          getCurrentFile: () => currentFile,
          openNote: (file) => openNote({ file }),
          setStatus,
          syncRoamDb,
          beforeRefresh: flushCurrentSaveForGit,
        });
        gitPanel = panel;
        return panel;
      })
      .finally(() => {
        gitPanelLoading = null;
      });
  }
  void gitPanelLoading
    .then((panel) => {
      if (run.current && notesToolVisible("git")) panel.refresh();
    })
    .catch((err) => setStatus(err instanceof Error ? err.message : "Git panel failed"));
}

host.addEventListener("aaronnote:insert-files", (event) => {
  const evt = event as CustomEvent<{ files?: File[]; pos?: number; mode?: "image-src" | "markdown" }>;
  const files = Array.isArray(evt.detail?.files) ? evt.detail.files : [];
  if (files.length === 0) return;
  event.preventDefault();
  void insertFiles(files, { pos: evt.detail?.pos, mode: evt.detail?.mode });
});

document.addEventListener("paste", (event) => {
  const active = document.activeElement;
  if (!active || !host.contains(active)) return;
  const files = filesFromClipboard(event);
  if (files.length === 0) return;
  event.preventDefault();
  void insertFiles(files);
});

host.addEventListener("dragover", (event) => {
  if (event.dataTransfer?.files?.length) event.preventDefault();
});

host.addEventListener("drop", (event) => {
  const files = Array.from(event.dataTransfer?.files ?? []);
  if (files.length === 0) return;
  event.preventDefault();
  void insertFiles(files);
});

host.addEventListener("focusout", () => {
  window.setTimeout(() => {
    if (document.activeElement && host.contains(document.activeElement)) return;
    hideMathPreview();
  }, 0);
});

function setStatus(text: string): void {
  statusEl.textContent = text;
  statusEl.title = text;
  root.dataset.saveState = text.toLowerCase().replace(/[^a-z]+/g, "-").replace(/^-|-$/g, "") || "idle";
  syncReliabilityActions();
}

function savedStatusText(time = Date.now()): string {
  return `Saved ${new Date(time).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}`;
}

function draftStorageKey(file = currentFile): string {
  return `${draftStoragePrefix}${file || "scratch"}`;
}

// Run heavy work during browser idle time, falling back to a 0ms timeout where
// requestIdleCallback is unavailable. Returns a cancel handle for either path.
function runWhenIdle(fn: () => void, timeout: number): () => void {
  if (typeof window.requestIdleCallback === "function") {
    const id = window.requestIdleCallback(fn, { timeout });
    return () => window.cancelIdleCallback?.(id);
  }
  const id = window.setTimeout(fn, 0);
  return () => window.clearTimeout(id);
}

function cancelScheduledDraftRemember(): void {
  draftSaveDebounce.cancel();
  cancelDraftIdle?.();
  cancelDraftIdle = null;
  draftSavePending = false;
}

function rememberDraft(content = editor.getMarkdown()): void {
  if (!currentFile) return;
  cancelScheduledDraftRemember();
  try {
    window.localStorage.setItem(draftStorageKey(), JSON.stringify({
      file: currentFile,
      content,
      revision: editRevision,
      updatedAt: Date.now(),
    }));
  } catch {
    // Draft autosave is a local convenience; ignore storage quota/availability failures.
  }
}

function scheduleDraftRemember(delay = 700): void {
  if (!currentFile) return;
  draftSavePending = true;
  cancelDraftIdle?.();
  cancelDraftIdle = null;
  draftSaveDebounce.schedule(() => {
    cancelDraftIdle = runWhenIdle(() => {
      cancelDraftIdle = null;
      if (!draftSavePending) return;
      rememberDraft();
    }, 1200);
  }, undefined, delay);
}

function flushDraftRemember(): void {
  if (!draftSavePending) return;
  rememberDraft();
}

function clearDraft(file = currentFile): void {
  if (!file) return;
  if (file === currentFile) cancelScheduledDraftRemember();
  try {
    window.localStorage.removeItem(draftStorageKey(file));
  } catch {
    // Best-effort cleanup; a failed removal only leaves a stale local draft.
  }
}

function readDraft(file = currentFile): StoredDraft | null {
  if (!file) return null;
  try {
    const raw = window.localStorage.getItem(draftStorageKey(file));
    if (!raw) return null;
    const parsed = JSON.parse(raw) as Partial<StoredDraft>;
    if (parsed.file !== file || typeof parsed.content !== "string") return null;
    return {
      file,
      content: parsed.content,
      revision: Number(parsed.revision) || 0,
      updatedAt: Number(parsed.updatedAt) || 0,
    };
  } catch {
    return null;
  }
}

function syncReliabilityActions(): void {
  forceSaveButton.hidden = !(currentFile && saveConflictActive);
  forceSaveButton.disabled = !currentFile || !saveConflictActive;
  draftBanner.hidden = pendingDraft == null;
}

function showDraftRecovery(draft: StoredDraft): void {
  pendingDraft = draft;
  const time = draft.updatedAt
    ? new Date(draft.updatedAt).toLocaleString([], { dateStyle: "short", timeStyle: "short" })
    : "unknown time";
  draftMessage.textContent = `Unsaved local draft for ${fileNameFromPath(draft.file)} from ${time}`;
  syncReliabilityActions();
}

function hideDraftRecovery(): void {
  pendingDraft = null;
  syncReliabilityActions();
}

function offerDraftRecovery(file: string, diskContent: string): void {
  const draft = readDraft(file);
  if (!draft) {
    hideDraftRecovery();
    return;
  }
  if (draft.content === diskContent) {
    clearDraft(file);
    hideDraftRecovery();
    return;
  }
  showDraftRecovery(draft);
}

function recoverPendingDraft(): void {
  const draft = pendingDraft;
  if (!draft || draft.file !== currentFile) return;
  hideDraftRecovery();
  applyingRemoteContent = true;
  try {
    editor.setMarkdown(draft.content);
  } finally {
    applyingRemoteContent = false;
  }
  markDirty();
  setStatus("Dirty draft recovered");
  scheduleAssistUpdate({ snippets: true, mathPreview: true, toc: true });
  editor.focus();
}

function discardPendingDraft(): void {
  const draft = pendingDraft;
  if (!draft) return;
  clearDraft(draft.file);
  hideDraftRecovery();
  setStatus("Local draft discarded");
  editor.focus();
}

function markDirty(): void {
  saveConflictActive = false;
  editRevision++;
  cancelScheduledNotesRefresh();
  scheduleDraftRemember();
  if (!currentFile) {
    setStatus(scratchStatus());
    return;
  }
  setStatus("Dirty");
  saveDebounce.schedule(() => save());
}

function stringArrayEqual(a: readonly string[] = [], b: readonly string[] = []): boolean {
  if (a.length !== b.length) return false;
  const left = [...a].sort();
  const right = [...b].sort();
  return left.every((value, index) => value === right[index]);
}

function noteNeedsRelationshipRefresh(previous: NoteSummary | undefined, next: NoteSummary): boolean {
  if (!previous) return true;
  return previous.id !== next.id
    || previous.title !== next.title
    || previous.path !== next.path
    || previous.source !== next.source
    || !stringArrayEqual(previous.aliases ?? [], next.aliases ?? [])
    || !stringArrayEqual(previous.refs ?? [], next.refs ?? []);
}

function mergeBookDerivedFields(previous: NoteSummary, next: NoteSummary): NoteSummary {
  if (!previous.bookRole || previous.bookRole !== next.bookRole) return next;
  const merged = { ...next };
  const keepString = (key: "bookCoverId" | "bookCoverPath" | "bookParentPath") => {
    if (!merged[key] && previous[key]) merged[key] = previous[key];
  };
  const keepArray = <K extends "bookIncludedPaths" | "bookToc" | "bookDomTargets" | "bookDiagnostics">(key: K) => {
    if ((merged[key] == null || merged[key]?.length === 0) && previous[key]?.length) {
      merged[key] = previous[key] as NoteSummary[K];
    }
  };
  keepString("bookCoverId");
  keepString("bookCoverPath");
  keepString("bookParentPath");
  keepArray("bookIncludedPaths");
  keepArray("bookToc");
  keepArray("bookDomTargets");
  keepArray("bookDiagnostics");
  return merged;
}

function upsertCurrentNoteSummary(note: NoteSummary, options: { preserveBacklinks?: boolean } = {}): void {
  if (!note.file) return;
  const index = notes.findIndex((item) => item.file === note.file);
  if (index >= 0) {
    const previous = notes[index]!;
    const merged = { ...previous, ...mergeBookDerivedFields(previous, note) };
    if (options.preserveBacklinks && (note.backlinks == null || note.backlinks.length === 0)) {
      merged.backlinks = previous.backlinks;
    }
    notes = [...notes.slice(0, index), merged, ...notes.slice(index + 1)];
  }
  else notes = [...notes, note];
  renderNotes();
  if (graphToolVisible()) renderGraph();
  updateFloatingToc();
  syncLocalGraphAvailability();
  if (!relationPanel.hidden) renderRelationPanel(true);
}

function knownRefsForNotes(items: readonly NoteSummary[]): string[] {
  return [...new Set(items.flatMap((note) => [
    note.id,
    note.key,
    note.title,
    note.path,
    note.link,
    note.source,
    note.file,
    note.file?.split(/[\\/]/).pop(),
    ...(note.aliases ?? []),
  ].filter((value): value is string => Boolean(value))))];
}

function syncEditorRoamLinkStatus(): void {
  editor.view.dispatch({ effects: setKnownRoamRefs.of(knownRefsForNotes(notes)) });
}

function cancelScheduledNotesRefresh(): void {
  notesRefreshDebounce.cancel();
  cancelNotesIdle?.();
  cancelNotesIdle = null;
}

function scheduleNotesRefresh(delay = 1400): void {
  notesRefreshPending = true;
  cancelScheduledNotesRefresh();
  notesRefreshDebounce.schedule(() => {
    cancelNotesIdle = runWhenIdle(() => {
      cancelNotesIdle = null;
      void refreshNotesIndex();
    }, 3000);
  }, undefined, delay);
}

async function refreshNotesIndex(force = false): Promise<void> {
  try {
    const msg = await api.notes.list(force);
    if (!Array.isArray(msg.notes)) throw new Error("Notes refresh failed");
    notesRefreshPending = false;
    applyIndexPayload(msg);
    renderNotes();
    focusFilesystemRangerSoon();
    if (notesToolVisible("agenda")) void loadAgendaTodos(true);
    if (graphToolVisible()) renderGraph();
    updateFloatingToc();
    syncLocalGraphAvailability();
    if (!relationPanel.hidden) renderRelationPanel(true);
  } catch (err) {
    console.warn("[notes] post-refresh UI update failed", err);
  }
}

function applyIndexPayload(msg: { notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[]; templates?: TemplateSummary[] }): void {
  if (Array.isArray(msg.notes)) {
    notes = msg.notes;
    syncLocalGraphAvailability();
    relationRenderKey = "";
  }
  if (Array.isArray(msg.directories)) directories = msg.directories;
  if (Array.isArray(msg.files)) files = msg.files;
  if (Array.isArray(msg.templates)) templates = msg.templates;
}

function findPattern(): RegExp | null {
  const result = createFindPattern(findQuery.value, findRegex.checked);
  if (result.error) findCount.textContent = result.error;
  return result.pattern;
}

function leanFindTargets(): Array<{ host: HTMLElement; view: EditorView; tag: string }> {
  return Array.from(document.querySelectorAll<HTMLElement>(".cm-lean-placeholder-widget"))
    .map((host) => {
      const view = (host as HTMLElement & { __leanChild?: EditorView }).__leanChild;
      return view ? { host, view, tag: host.dataset.leanTag || "" } : null;
    })
    .filter((item): item is { host: HTMLElement; view: EditorView; tag: string } => Boolean(item));
}

function findScopeValue(): "all" | "note" | "code" {
  return findScope.value === "note" || findScope.value === "code" ? findScope.value : "all";
}

function collectLeanFindMatches(pattern: RegExp | null): LeanFindMatch[] {
  if (!pattern) return [];
  const matches: LeanFindMatch[] = [];
  for (const target of leanFindTargets()) {
    const text = target.view.state.doc.toString();
    for (const match of collectFindMatches(text, pattern)) {
      matches.push({ ...match, source: "code", tag: target.tag, view: target.view, host: target.host });
    }
  }
  return matches;
}

function collectScopedFindMatches(
  markdown: string,
  pattern: RegExp | null,
  options: { viewportFirst?: boolean } = {},
): AaronFindMatch[] {
  const scope = findScopeValue();
  const noteMatches = scope === "code"
    ? []
    : (options.viewportFirst
        ? collectFindMatchesInRanges(markdown, pattern, editor.view.visibleRanges)
        : collectFindMatches(markdown, pattern))
      .map((match): MarkdownFindMatch => ({ ...match, source: "note" }));
  const codeMatches = scope === "note" ? [] : collectLeanFindMatches(pattern);
  return [...noteMatches, ...codeMatches].sort((a, b) => {
    if (a.source !== b.source) return a.source === "note" ? -1 : 1;
    if (a.source === "code" && b.source === "code" && a.tag !== b.tag) return a.tag.localeCompare(b.tag);
    return a.from - b.from || a.to - b.to;
  });
}

function applyFindDecorations(matches: readonly AaronFindMatch[], currentIndex = -1): void {
  editor.view.dispatch({
    effects: setFindHighlightRanges.of(matches.filter((match) => match.source === "note").map((match, index) => ({
      from: match.from,
      to: match.to,
      current: matches.indexOf(match) === currentIndex,
    }))),
  });
  const rangesByView = new Map<EditorView, Array<{ from: number; to: number; current?: boolean }>>();
  for (const target of leanFindTargets()) rangesByView.set(target.view, []);
  matches.forEach((match, index) => {
    if (match.source !== "code") return;
    const ranges = rangesByView.get(match.view) ?? [];
    ranges.push({ from: match.from, to: match.to, current: index === currentIndex });
    rangesByView.set(match.view, ranges);
  });
  for (const [view, ranges] of rangesByView) {
    view.dispatch({ effects: setFindHighlightRanges.of(ranges) });
  }
}

function refreshFindMatches(options: { viewportFirst?: boolean } = {}): void {
  findRefreshTimer.cancel();
  findFullScanTimer.cancel();
  findMatches = [];
  findIndex = -1;
  const pattern = findPattern();
  if (!pattern) {
    if (!findCount.textContent || !findQuery.value) findCount.textContent = "";
    applyFindDecorations([]);
    return;
  }
  const markdown = editor.getMarkdown();
  if (options.viewportFirst && findScopeValue() === "note") {
    const query = findQuery.value;
    const regex = findRegex.checked;
    const viewportMatches = collectScopedFindMatches(markdown, pattern, { viewportFirst: true });
    findMatches = viewportMatches;
    findCount.textContent = viewportMatches.length
      ? `Viewport ${viewportMatches.length}...`
      : "Scanning...";
    applyFindDecorations(viewportMatches);
    findFullScanTimer.schedule(() => {
      if (findTool.hidden) return;
      if (findQuery.value !== query || findRegex.checked !== regex) return;
      const fullPattern = findPattern();
      findMatches = collectScopedFindMatches(editor.getMarkdown(), fullPattern);
      findCount.textContent = findMatches.length ? `0 / ${findMatches.length}` : "No matches";
      applyFindDecorations(findMatches);
      if (findMatches.length) selectFindMatch(0);
    });
    return;
  }
  findMatches = collectScopedFindMatches(markdown, pattern);
  findCount.textContent = findMatches.length ? `0 / ${findMatches.length}` : "No matches";
  applyFindDecorations(findMatches);
}

function scheduleFindRefresh(): void {
  findRefreshTimer.schedule(() => {
    refreshFindMatches({ viewportFirst: true });
  });
}

function selectFindMatch(index: number): void {
  if (findMatches.length === 0) {
    refreshFindMatches();
    if (findMatches.length === 0) return;
  }
  findIndex = (index + findMatches.length) % findMatches.length;
  const match = findMatches[findIndex]!;
  if (match.source === "note") {
    editor.setMarkdownSelection(match.from, match.to);
  } else {
    match.host.scrollIntoView({ block: "center", inline: "nearest" });
    match.view.dispatch({
      selection: { anchor: match.from, head: match.to },
      scrollIntoView: true,
    });
    match.view.focus();
  }
  findCount.textContent = `${findIndex + 1} / ${findMatches.length}`;
  applyFindDecorations(findMatches, findIndex);
}

function findNext(delta = 1): void {
  if (findMatches.length === 0) refreshFindMatches();
  selectFindMatch(findIndex + delta);
}

function findSeedFromEditorSelection(): string {
  const selection = editor.getSelection();
  const from = Math.min(selection.from, selection.to);
  const to = Math.max(selection.from, selection.to);
  if (from >= to) return "";
  const text = editor.textBetween(from, to).trim();
  if (!text || text.length > 120 || /[\r\n]/.test(text)) return "";
  return text;
}

function openFindTool(): void {
  if (!notesPage.hidden) {
    showNotesTool("filesystem");
    noteFilter.focus();
    noteFilter.select();
    return;
  }
  hideEditorOverlays({ keepFind: true });
  const seed = findSeedFromEditorSelection();
  if (seed) findQuery.value = seed;
  findTool.hidden = false;
  findQuery.focus();
  findQuery.select();
  refreshFindMatches();
}

function closeFindTool(refocusEditor = true): void {
  findTool.hidden = true;
  findRefreshTimer.cancel();
  findFullScanTimer.cancel();
  findMatches = [];
  findIndex = -1;
  findCount.textContent = "";
  applyFindDecorations([]);
  if (refocusEditor) editor.focus();
}

function replaceCurrentFindMatch(): void {
  if (findMatches.length === 0) refreshFindMatches();
  if (findMatches.length === 0) return;
  const match = findMatches[Math.max(0, findIndex)] ?? findMatches[0]!;
  const replacement = findReplacementText(match.match, findReplace.value, findRegex.checked);
  if (match.source === "note") {
    editor.replaceMarkdownRange(match.from, match.to, replacement, "end");
  } else {
    match.view.dispatch({
      changes: { from: match.from, to: match.to, insert: replacement },
      selection: { anchor: match.from + replacement.length },
      scrollIntoView: true,
    });
  }
  refreshFindMatches();
  selectFindMatch(Math.min(findIndex, findMatches.length - 1));
}

function replaceAllFindMatches(): void {
  const pattern = findPattern();
  if (!pattern) return;
  const markdown = editor.getMarkdown();
  const scope = findScopeValue();
  const matches = collectScopedFindMatches(markdown, pattern);
  if (matches.length === 0) return;
  const codeByView = new Map<EditorView, LeanFindMatch[]>();
  for (const match of matches) {
    if (match.source !== "code") continue;
    const list = codeByView.get(match.view) ?? [];
    list.push(match);
    codeByView.set(match.view, list);
  }
  for (const [view, codeMatches] of codeByView) {
    view.dispatch({
      changes: codeMatches
        .slice()
        .sort((a, b) => a.from - b.from)
        .map((match) => ({
          from: match.from,
          to: match.to,
          insert: findReplacementText(match.match, findReplace.value, findRegex.checked),
        })),
    });
  }
  if (scope !== "code") {
    const next = replaceAllFindText(markdown, pattern, findReplace.value, findRegex.checked);
    if (next !== markdown) editor.setMarkdown(next);
  }
  refreshFindMatches();
  scheduleAssistUpdate({ snippets: true });
}

function decodeNoteRef(ref: string): string {
  let decoded = ref;
  try {
    decoded = decodeURIComponent(ref);
  } catch {
    decoded = ref;
  }
  return decoded.replace(/\\([\\`*_[\](){}#+.!<>-])/g, "$1");
}

function cleanHref(href: string): string {
  const raw = String(href || "").trim();
  if (raw.startsWith("<") && raw.endsWith(">")) return raw.slice(1, -1).trim();
  return raw;
}

function hrefProtocol(href: string): string | null {
  return cleanHref(href).match(/^([A-Za-z][\w+.-]*):/)?.[1]?.toLowerCase() ?? null;
}

function stripJupyterSelectorPath(path: string): string {
  const match = String(path || "").match(/^(.+?\.ipynb)@/i);
  if (match) return match[1] || "";
  return path;
}

function jupyterAtSelectorFromHref(href: string): string {
  const rawPath = cleanHref(href).split(/[?#]/, 1)[0] ?? "";
  let decoded = "";
  if (/^file:\/\//i.test(rawPath)) {
    try {
      decoded = decodeNoteRef(new URL(rawPath).pathname);
    } catch {
      decoded = decodeNoteRef(rawPath.replace(/^file:\/\//i, ""));
    }
  } else if (/^file:/i.test(rawPath)) {
    decoded = decodeNoteRef(rawPath.replace(/^file:/i, ""));
  } else {
    decoded = decodeNoteRef(rawPath);
  }
  const match = decoded.match(/^(.+?\.ipynb)@(.+)$/i);
  if (!match) return "";
  return domTargetPathSegments(match[2] || "")[0] || "";
}

function jupyterHashSelectorFromHref(href: string): string {
  const hash = hrefHash(href);
  if (!hash) return "";
  return domTargetPathSegments(decodeNoteRef(hash).trim())[0] || "";
}

function jupyterTocSelectorFromHref(href: string): string {
  return jupyterAtSelectorFromHref(href) || jupyterHashSelectorFromHref(href);
}

function hrefPath(href: string): string {
  const raw = cleanHref(href);
  if (/^file:\/\//i.test(raw)) {
    try {
      return stripJupyterSelectorPath(decodeNoteRef(new URL(raw).pathname));
    } catch {
      return stripJupyterSelectorPath(decodeNoteRef(raw.replace(/^file:\/\//i, "")));
    }
  }
  if (/^file:/i.test(raw)) return stripJupyterSelectorPath(decodeNoteRef(raw.replace(/^file:/i, "").split(/[?#]/, 1)[0] ?? ""));
  return stripJupyterSelectorPath(decodeNoteRef(raw.split(/[?#]/, 1)[0] ?? ""));
}

function jupyterHrefP(href: string): boolean {
  const protocol = hrefProtocol(href);
  if (protocol && protocol !== "file") return false;
  return /\.ipynb$/i.test(hrefPath(href));
}

function canonicalJupyterPath(path: string): string {
  const clean = normalizeNotePath(path);
  if (!clean || clean.startsWith("/") || !currentFile) return clean;
  return joinNotePath(dirnamePath(currentFile), clean);
}

function jupyterTargetFromHref(href: string): JupyterTarget | null {
  if (!jupyterHrefP(href)) return null;
  const tocTarget = jupyterTocSelectorFromHref(href);
  const path = canonicalJupyterPath(hrefPath(href));
  return {
    href,
    path,
    base: currentFile,
    selector: tocTarget,
    selectorKind: tocTarget ? "toc" : "",
  };
}

type MarkdownJupyterLinkAtCursor = {
  insertAt: number;
  hasAtSelector: boolean;
};

function markdownLinkTargetBounds(rawTarget: string): { href: string; start: number; end: number } | null {
  const leading = rawTarget.match(/^\s*/)?.[0].length ?? 0;
  if (rawTarget[leading] === "<") {
    for (let index = leading + 1; index < rawTarget.length; index++) {
      if (rawTarget[index] === ">" && !markdownEscapedAt(rawTarget, index)) {
        const href = rawTarget.slice(leading + 1, index).trim();
        return href ? { href, start: leading + 1, end: index } : null;
      }
    }
  }
  const titleMatch = rawTarget.match(/\s+(?:"[^"]*"|'[^']*')\s*$/);
  const beforeTitleEnd = titleMatch?.index ?? rawTarget.length;
  const beforeTitle = rawTarget.slice(0, beforeTitleEnd);
  const start = leading;
  const end = beforeTitle.replace(/\s+$/, "").length;
  if (start >= end) return null;
  return { href: rawTarget.slice(start, end), start, end };
}

function ipynbEndInHref(href: string): number {
  const re = /\.ipynb/gi;
  let end = -1;
  let match: RegExpExecArray | null;
  while ((match = re.exec(href)) !== null) end = match.index + match[0].length;
  return end;
}

function markdownEscapedAt(text: string, index: number): boolean {
  let slashCount = 0;
  for (let pos = index - 1; pos >= 0 && text[pos] === "\\"; pos--) slashCount++;
  return slashCount % 2 === 1;
}

function markdownLinkLabelEnd(line: string, open: number): number {
  let depth = 1;
  for (let pos = open + 1; pos < line.length; pos++) {
    const ch = line[pos];
    if ((ch === "[" || ch === "]") && markdownEscapedAt(line, pos)) continue;
    if (ch === "[") {
      depth++;
      continue;
    }
    if (ch === "]") {
      depth--;
      if (depth === 0) return pos;
    }
  }
  return -1;
}

function markdownLinkTargetEnd(line: string, open: number): number {
  let depth = 0;
  let quote = "";
  for (let pos = open + 1; pos < line.length; pos++) {
    const ch = line[pos];
    if (markdownEscapedAt(line, pos)) continue;
    if (quote) {
      if (ch === quote) quote = "";
      continue;
    }
    if ((ch === "\"" || ch === "'") && /\s/.test(line[pos - 1] ?? "")) {
      quote = ch;
      continue;
    }
    if (ch === "(") {
      depth++;
      continue;
    }
    if (ch === ")") {
      if (depth === 0) return pos;
      depth--;
    }
  }
  return -1;
}

function markdownInlineLinkAtLinePosition(line: string, localPos: number): { rawTarget: string; rawTargetFrom: number } | null {
  for (let pos = 0; pos < line.length; pos++) {
    if (line[pos] !== "[" || markdownEscapedAt(line, pos)) continue;
    if (pos > 0 && line[pos - 1] === "!" && !markdownEscapedAt(line, pos - 1)) continue;
    const labelEnd = markdownLinkLabelEnd(line, pos);
    if (labelEnd < 0 || line[labelEnd + 1] !== "(") continue;
    const targetOpen = labelEnd + 1;
    const targetEnd = markdownLinkTargetEnd(line, targetOpen);
    if (targetEnd < 0) continue;
    if (localPos >= pos && localPos < targetEnd + 1) {
      return {
        rawTarget: line.slice(targetOpen + 1, targetEnd),
        rawTargetFrom: targetOpen + 1,
      };
    }
    pos = targetEnd;
  }
  return null;
}

function markdownJupyterLinkAtCursor(): MarkdownJupyterLinkAtCursor | null {
  const selection = editor.getMarkdownSelection();
  const pos = Math.max(0, Math.min(selection.from, editor.view.state.doc.length));
  const line = editor.view.state.doc.lineAt(pos);
  const localPos = pos - line.from;
  const link = markdownInlineLinkAtLinePosition(line.text, localPos);
  if (!link) return null;
  const bounds = markdownLinkTargetBounds(link.rawTarget);
  if (!bounds || !jupyterHrefP(bounds.href)) return null;
  const ipynbEnd = ipynbEndInHref(bounds.href);
  if (ipynbEnd < 0) return null;
  return {
    insertAt: line.from + link.rawTargetFrom + bounds.start + ipynbEnd,
    hasAtSelector: Boolean(jupyterAtSelectorFromHref(bounds.href)),
  };
}

function randomJupyterTocId(): string {
  try {
    const bytes = new Uint8Array(6);
    window.crypto.getRandomValues(bytes);
    return `jpy-${Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join("")}`;
  } catch {
    return `jpy-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
  }
}

function handleJupyterTocTagCommand(): "miss" | "noop" | "inserted" {
  const link = markdownJupyterLinkAtCursor();
  if (!link) return "miss";
  if (link.hasAtSelector) return "noop";
  const id = randomJupyterTocId();
  editor.view.dispatch({
    changes: { from: link.insertAt, insert: `@${id}` },
  });
  void copyText(`########## ${id}`)
    .then(() => setStatus(`Jupyter TOC tag copied: ${id}`))
    .catch(() => setStatus("Copy failed"));
  scheduleCursorPositionSave();
  return "inserted";
}

function attachmentHrefP(href: string): boolean {
  const raw = cleanHref(href);
  if (!raw || raw.startsWith("#")) return false;
  const protocol = hrefProtocol(raw);
  if (protocol && protocol !== "file") return false;
  const path = hrefPath(raw).trim();
  return Boolean(path) && !/\.(?:md|markdown|typ)$/i.test(path);
}

function markdownAttachmentHrefNear(markdown: string, pos: number): string {
  const clamped = Math.max(0, Math.min(pos, markdown.length));
  const lineFrom = markdown.lastIndexOf("\n", Math.max(0, clamped - 1)) + 1;
  const nextNewline = markdown.indexOf("\n", clamped);
  const lineTo = nextNewline < 0 ? markdown.length : nextNewline;
  const line = markdown.slice(lineFrom, lineTo);
  const localPos = clamped - lineFrom;
  const re = /!?\[[^\]\n]*\]\(([^)\n]+)\)/g;
  let match: RegExpExecArray | null;
  while ((match = re.exec(line)) !== null) {
    if (localPos < match.index || localPos > match.index + match[0].length) continue;
    const href = (match[1] || "")
      .replace(/\s+"[^"]*"\s*$/, "")
      .replace(/\s+'[^']*'\s*$/, "")
      .trim();
    return attachmentHrefP(href) ? href : "";
  }
  return "";
}

function markdownAttachmentHrefFromSelection(markdown: string, from: number, to: number): string {
  if (from === to) return "";
  const selected = markdown.slice(Math.max(0, from), Math.min(markdown.length, to));
  const match = selected.match(/!?\[[^\]\n]*\]\(([^)\n]+)\)/);
  const href = (match?.[1] || "")
    .replace(/\s+"[^"]*"\s*$/, "")
    .replace(/\s+'[^']*'\s*$/, "")
    .trim();
  return attachmentHrefP(href) ? href : "";
}

function attachmentHrefFromContextMenu(event: MouseEvent): string {
  const anchor = (event.target as Element | null)?.closest<HTMLAnchorElement>("a[href]");
  if (anchor && host.contains(anchor)) {
    const href = anchor.getAttribute("href") || anchor.href;
    if (attachmentHrefP(href)) return href;
  }
  try {
    const markdown = editor.getMarkdown();
    const selection = editor.getMarkdownSelection();
    const selectedHref = markdownAttachmentHrefFromSelection(markdown, selection.from, selection.to);
    if (selectedHref) return selectedHref;
    const pos = editor.view.posAtCoords({ x: event.clientX, y: event.clientY });
    if (pos == null) return "";
    return markdownAttachmentHrefNear(markdown, pos);
  } catch {
    return "";
  }
}

function hrefHash(href: string): string {
  const raw = cleanHref(href);
  const index = raw.indexOf("#");
  if (index < 0) return "";
  return decodeNoteRef((raw.slice(index + 1).split(/[?&]/, 1)[0] ?? "").trim());
}

function equationTagFromHref(href: string): string | null {
  const hash = hrefHash(href);
  if (!hash) return null;
  if (/^eq-/i.test(hash)) return decodeNoteRef(hash.slice(3)).trim() || null;
  return null;
}

function inlineTagFromHref(href: string): string {
  const hash = hrefHash(href);
  if (!hash || /^eq-/i.test(hash)) return "";
  const legacy = inlineTagFromHash(hash);
  return legacy || normalizeInlineTag(decodeNoteRef(hash));
}

function currentNoteTarget(ref: string): boolean {
  const clean = decodeNoteRef(String(ref || "")).trim();
  return clean === "" || clean === "." || clean === "./";
}

function splitRoamLikeHref(href: string): { ref: string; hash: string; dom: string } | null {
  const raw = cleanHref(href);
  if (!raw || /^[A-Za-z][\w+.-]*:/i.test(raw) && !/^roam:\/\//i.test(raw)) return null;
  let body = raw.replace(/^roam:\/\//i, "");
  body = body.split(/[?&]/, 1)[0] ?? body;
  let hash = "";
  const hashIndex = body.indexOf("#");
  if (hashIndex >= 0) {
    hash = decodeNoteRef(body.slice(hashIndex + 1));
    body = body.slice(0, hashIndex);
  }
  let dom = "";
  const fileDomMatch = body.match(/^(.+?\.(?:md|markdown|typ))@(.+)$/i);
  if (fileDomMatch) {
    body = fileDomMatch[1] || "";
    dom = domTargetPathSegments(fileDomMatch[2] || "").join("@");
  } else {
    const atIndex = body.indexOf("@");
    if (atIndex >= 0) {
      dom = domTargetPathSegments(body.slice(atIndex + 1)).join("@");
      body = body.slice(0, atIndex);
    }
  }
  const ref = decodeNoteRef(body.replace(/^\/+/, "").replace(/[.,;:]+$/, "")).trim();
  if (!ref && !hash && !dom) return null;
  return { ref, hash: hash.trim(), dom: normalizeDomTargetPath(dom) };
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function noteRefFromRoamHref(href: string): string | null {
  if (!/^roam:\/\//i.test(cleanHref(href))) return null;
  const ref = splitRoamLikeHref(href)?.ref ?? "";
  return ref || null;
}

function canonicalNoteRef(ref: string): string {
  const roamRef = noteRefFromRoamHref(ref);
  return normalizeNotePath(decodeNoteRef(roamRef ?? ref).trim().replace(/^\.\/+/, ""));
}

function normalizeNotePath(path: string): string {
  const normalized = path.replace(/\\/g, "/");
  const absolute = normalized.startsWith("/");
  const parts: string[] = [];
  for (const part of normalized.split("/")) {
    if (!part || part === ".") continue;
    if (part === "..") {
      if (parts.length > 0 && parts[parts.length - 1] !== "..") {
        parts.pop();
      } else if (!absolute) {
        parts.push(part);
      }
      continue;
    }
    parts.push(part);
  }
  return `${absolute ? "/" : ""}${parts.join("/")}`;
}

function dirnamePath(path: string): string {
  const normalized = normalizeNotePath(path);
  const index = normalized.lastIndexOf("/");
  if (index < 0) return "";
  if (index === 0) return "/";
  return normalized.slice(0, index);
}

function joinNotePath(baseDir: string, path: string): string {
  if (!baseDir || path.startsWith("/")) return normalizeNotePath(path);
  return normalizeNotePath(`${baseDir}/${path}`);
}

function markdownNoteHref(href: string): boolean {
  const protocol = hrefProtocol(href);
  if (protocol && protocol !== "file") return false;
  return /\.(?:md|markdown|typ)$/i.test(hrefPath(href));
}

function internalNoteCandidates(href: string): string[] {
  const path = hrefPath(href);
  const candidates = new Set<string>();
  const add = (value: string) => {
    const normalized = normalizeNotePath(value);
    if (normalized) candidates.add(normalized);
  };
  add(path);
  add(path.replace(/^\.\/+/, ""));
  if (!path.startsWith("/") && currentFile) add(joinNotePath(dirnamePath(currentFile), path));
  const currentNote = notes.find((note) => note.file === currentFile);
  if (!path.startsWith("/") && currentNote?.path) add(joinNotePath(dirnamePath(currentNote.path), path));
  return [...candidates];
}

function resolveNoteRef(ref: string): NoteSummary | undefined {
  if (currentNoteTarget(ref)) return currentNote() ?? (currentFile ? { file: currentFile, path: currentFile, title: fileNameFromPath(currentFile), standalone: currentStandalone } : undefined);
  const note = resolveSharedNoteReference(notes, ref) as NoteSummary | undefined;
  return externalBookNote(note);
}

function externalBookNote(note: NoteSummary | undefined): NoteSummary | undefined {
  if (!note || note.bookRole !== "included" || !note.bookCoverId) return note;
  return notes.find((item) =>
    item.bookRole === "cover"
    && (item.id === note.bookCoverId || item.key === note.bookCoverId || canonicalRoamNoteId(item) === note.bookCoverId))
    || note;
}

function bookPathKey(value: unknown): string {
  let path = normalizeNotePath(String(value || "").replace(/\\/g, "/").replace(/^\.\/+/, ""));
  const roamIndex = path.indexOf("/roam/");
  if (roamIndex >= 0) path = path.slice(roamIndex + "/roam/".length);
  return path.replace(/^roam\//, "");
}

function noteMatchesBookPath(note: NoteSummary, path: unknown): boolean {
  const key = bookPathKey(path);
  if (!key) return false;
  return [note.path, note.file, note.source, note.link]
    .map(bookPathKey)
    .some((value) => value === key);
}

function bookCoverForNote(note: NoteSummary | undefined): NoteSummary | undefined {
  const external = externalBookNote(note);
  return external?.bookRole === "cover" ? external : undefined;
}

function currentBookContext(): BookEditorContext | null {
  const note = currentNote();
  const cover = bookCoverForNote(note);
  if (!cover || (cover.bookToc ?? []).length === 0) return null;
  return {
    role: note?.bookRole || cover.bookRole || "",
    title: cover.title || cover.id || "Book",
    coverPath: cover.path || cover.file || "",
    currentPath: note?.path || note?.file || currentFile,
    includedCount: cover.bookIncludedPaths?.length || 0,
    toc: cover.bookToc as BookEditorTocItem[],
  };
}

function bookContextKey(context: BookEditorContext | null): string {
  if (!context) return "";
  return [
    context.role || "",
    context.title || "",
    context.coverPath || "",
    context.currentPath || "",
    String(context.includedCount || 0),
    ...(context.toc || []).map((item) => [
      item.level || 1,
      item.text || "",
      item.slug || "",
      item.path || "",
      item.id || "",
    ].join("\t")),
  ].join("\n");
}

let syncedBookContextKey = "";

function syncEditorBookContext(context: BookEditorContext | null): void {
  const key = bookContextKey(context);
  if (key === syncedBookContextKey) return;
  syncedBookContextKey = key;
  setBookContext(editor.view, context);
}

function resolveBookTocItemNote(item: BookTocItem | BookEditorTocItem): NoteSummary | undefined {
  const itemPath = item.path || "";
  if (itemPath) {
    const match = notes.find((note) => noteMatchesBookPath(note, itemPath));
    if (match?.file) return match;
  }
  const cover = bookCoverForNote(currentNote());
  return cover?.file ? cover : undefined;
}

function bookTocDomTargetEntries(cover: NoteSummary): DomTargetEntry[] {
  const tocItems = (cover.bookToc ?? []) as Array<BookTocItem | BookEditorTocItem>;
  const stack: Array<{ level: number; path: string[]; labelPath: string[] }> = [];
  const out: DomTargetEntry[] = [];
  for (const item of tocItems) {
    const label = normalizeDomTarget(item.text || item.slug || "");
    const slug = slugDomTarget(item.slug || label);
    if (!label || !slug) continue;
    const level = Math.max(1, Number(item.level || 1));
    while (stack.length > 0 && stack[stack.length - 1]!.level >= level) stack.pop();
    const parent = stack[stack.length - 1];
    const path = [...(parent?.path ?? []), slug];
    const labelPath = [...(parent?.labelPath ?? []), label];
    stack.push({ level, path, labelPath });
    out.push({
      label,
      slug,
      path,
      labelPath,
      level,
      notePath: item.path || "",
    });
  }
  return out;
}

function resolveBookDomTarget(note: NoteSummary | undefined, rawTarget: string): { note?: NoteSummary; domTarget?: string } | null {
  const cover = bookCoverForNote(note);
  if (!cover || !rawTarget) return null;
  const hit = findDomTargetEntry(bookTocDomTargetEntries(cover), rawTarget);
  if (!hit) return null;
  const item = (cover.bookToc ?? []).find((tocItem) =>
    bookPathKey(tocItem.path) === bookPathKey(hit.notePath)
    && (tocItem.slug === hit.slug || slugDomTarget(tocItem.text || "") === hit.slug));
  const target = item ? resolveBookTocItemNote(item) : undefined;
  return {
    note: target?.file ? target : cover,
    domTarget: hit.path.join("@") || hit.slug,
  };
}

function activeBookHeadingSlug(): string {
  const pos = editor.getMarkdownSelection().from;
  let slug = "";
  for (const heading of markdownHeadingsFromText(editor.view.state.doc)) {
    if (heading.pos > pos) break;
    slug = heading.slug || slugDomTarget(heading.text);
  }
  return slug;
}

function openBookTocItem(item: BookTocItem | BookEditorTocItem, options: { newWindow?: boolean } = {}): void {
  const target = resolveBookTocItemNote(item);
  if (!target?.file) {
    setStatus("Book target not found");
    return;
  }
  openNote(target, {
    domTarget: item.slug || item.text || "",
    newWindow: options.newWindow,
    recordJump: true,
  });
}

function openBookIncludeRef(ref: string): void {
  const target = resolvePhysicalInternalNoteHref(ref) || resolveNoteRef(ref);
  if (!target?.file) {
    setStatus(`Include target not found: ${ref}`);
    return;
  }
  openNote(target, { recordJump: true, scrollTop: true });
}

function resolveRoamLikeNoteTarget(href: string): { note?: NoteSummary; equationTag?: string; inlineTag?: string; domTarget?: string } | null {
  const target = splitRoamLikeHref(href);
  if (!target) return null;
  const note = resolveNoteRef(target.ref);
  if (!note) {
    if (markdownNoteHref(href)) return null;
    return { equationTag: equationTagFromHref(href) || "", inlineTag: inlineTagFromHref(href), domTarget: target.dom };
  }
  const equationTag = /^eq-/i.test(target.hash) ? decodeNoteRef(target.hash.slice(3)).trim() : "";
  const inlineTag = equationTag ? "" : normalizeInlineTag(target.hash);
  if (target.dom) {
    const bookTarget = resolveBookDomTarget(note, target.dom);
    if (bookTarget?.note) return { note: bookTarget.note, equationTag, inlineTag, domTarget: bookTarget.domTarget || target.dom };
  }
  return { note, equationTag, inlineTag, domTarget: target.dom };
}

async function openJupyterPreviewTarget(target: JupyterTarget, options: { restart?: boolean } = {}): Promise<void> {
  if (!api.jupyter.available()) {
    setStatus("Jupyter preview unavailable");
    return;
  }
  leanPanel.hide();
  await jupyterPanel.open(target, { restart: options.restart });
  syncPanelSwitcher();
}

async function openJupyterPreviewFromHref(href: string, options: { restart?: boolean } = {}): Promise<void> {
  const target = jupyterTargetFromHref(href);
  if (!target) {
    setStatus("No Jupyter notebook link");
    return;
  }
  await openJupyterPreviewTarget(target, options);
}

function resolveInternalNoteHref(href: string): NoteSummary | undefined {
  if (!markdownNoteHref(href)) return undefined;
  for (const candidate of internalNoteCandidates(href)) {
    const note = resolveNoteRef(candidate);
    if (note) return note;
  }
  return undefined;
}

function resolvePhysicalInternalNoteHref(href: string): NoteSummary | undefined {
  if (!markdownNoteHref(href)) return undefined;
  for (const candidate of internalNoteCandidates(href)) {
    const note = notes.find((item) => noteMatchesBookPath(item, candidate));
    if (note?.file) return note;
  }
  return undefined;
}

function standaloneNoteFromMarkdownHref(href: string): NoteSummary | undefined {
  if (!markdownNoteHref(href)) return undefined;
  const file = internalNoteCandidates(href).find((candidate) => candidate.startsWith("/"));
  if (!file) return undefined;
  return {
    file,
    path: file,
    title: fileNameFromPath(file),
    standalone: true,
  };
}

function noteWindowUrl(note: NoteSummary, equationTag = "", inlineTag = "", domTarget = ""): string {
  const url = new URL(window.location.href);
  url.searchParams.set("file", note.file || "");
  if (equationTag) url.searchParams.set("eqTag", equationTag);
  else url.searchParams.delete("eqTag");
  if (inlineTag) url.searchParams.set("tag", inlineTag);
  else url.searchParams.delete("tag");
  if (domTarget) url.searchParams.set("dom", domTarget);
  else url.searchParams.delete("dom");
  return url.toString();
}

function syncCurrentFileUrl(): void {
  const url = new URL(window.location.href);
  if (currentFile) url.searchParams.set("file", currentFile);
  else url.searchParams.delete("file");
  window.history.replaceState(null, "", url.toString());
}

function openExternalUrl(href: string, options: OpenNoteOptions = {}): void {
  if (!safeHref(href)) {
    setStatus("Blocked unsafe link");
    return;
  }
  if (jupyterHrefP(href)) {
    void openJupyterPreviewFromHref(href);
    return;
  }
  const roamLike = resolveRoamLikeNoteTarget(href);
  if (roamLike) {
    if (!roamLike.note) {
      setStatus(`Roam note not found: ${splitRoamLikeHref(href)?.ref || href}`);
      return;
    }
    openNote(roamLike.note, {
      ...options,
      equationTag: roamLike.equationTag || options.equationTag,
      inlineTag: roamLike.inlineTag || options.inlineTag,
      domTarget: roamLike.domTarget || options.domTarget,
      recordJump: true,
    });
    return;
  }
  const equationTag = options.equationTag || equationTagFromHref(href) || "";
  const inlineTag = options.inlineTag || inlineTagFromHref(href);
  if (equationTag && String(href || "").trim().startsWith("#")) {
    pushJumpPoint();
    if (!jumpToEquationTag(equationTag)) setStatus(`Equation tag not found: ${equationTag}`);
    return;
  }
  if (inlineTag && String(href || "").trim().startsWith("#")) {
    pushJumpPoint();
    if (!jumpToInlineTag(inlineTag)) setStatus(`Inline tag not found: ${inlineTag}`);
    return;
  }
  const roamRef = noteRefFromRoamHref(href);
  if (roamRef != null) {
    const note = resolveNoteRef(roamRef);
    if (note) openNote(note, { ...options, equationTag, inlineTag });
    else setStatus(`Roam note not found: ${roamRef}`);
    return;
  }
  if (markdownNoteHref(href)) {
    const note = resolveInternalNoteHref(href) || standaloneNoteFromMarkdownHref(href);
    if (note) openNote(note, { ...options, equationTag, inlineTag, recordJump: true });
    else setStatus(`Note not found: ${hrefPath(href)}`);
    return;
  }
  if (options.newWindow) {
    window.open(href, "_blank", "noopener,noreferrer");
    return;
  }
  window.location.href = href;
}

function resolveAssetUrl(src: string, baseFile = currentFile): string {
  const raw = String(src || "");
  if (!raw) return raw;
  if (/^(?:data:|https?:|blob:)/i.test(raw) || raw.startsWith("#")) return raw;
  if (/^(?:aaronnote-asset|file):/i.test(raw)) return hostAssetResolver?.(raw) ?? raw;
  if (/^[A-Za-z][\w+.-]*:/i.test(raw)) return raw;
  if (raw.startsWith("/") && !raw.startsWith("/Users/")) return raw;
  const file = raw.startsWith("file://")
    ? new URL(raw).pathname
    : raw.startsWith("file:")
      ? raw.slice(5)
      : raw;
  const url = new URL("aaronnote-asset://media/");
  url.searchParams.set("file", file);
  if (baseFile) url.searchParams.set("base", baseFile);
  return hostAssetResolver?.(url.toString()) ?? url.toString();
}

window.AaronnoteResolveAssetUrl = resolveAssetUrl;

function setNoteCssHref(href: string): void {
  const existing = document.querySelector<HTMLLinkElement>("link[data-aaronnote-note-css]");
  if (!href) {
    existing?.remove();
    return;
  }
  if (existing?.getAttribute("href") === href) return;
  const link = existing ?? document.createElement("link");
  link.rel = "stylesheet";
  link.href = href;
  link.dataset.aaronnoteNoteCss = "true";
  if (!existing) document.head.appendChild(link);
  else document.head.appendChild(link);
}

function updateNoteCss(markdown = editor.getMarkdown()): void {
  setNoteCssHref(noteCssHrefFromMarkdown(markdown));
}

function scheduleNoteCssUpdate(): void {
  noteCssDebounce.schedule(() => updateNoteCss());
}

function fileNameFromPath(path: string): string {
  return path.split(/[\\/]/).filter(Boolean).at(-1) || "attachment";
}

function nativeFilePath(file: File): string {
  return String((file as File & { path?: string }).path || "");
}

function fileToBase64(file: File): Promise<string> {
  return file.arrayBuffer().then((buffer) => {
    const bytes = new Uint8Array(buffer);
    const chunkSize = 0x8000;
    let binary = "";
    for (let i = 0; i < bytes.length; i += chunkSize) {
      binary += String.fromCharCode(...bytes.slice(i, i + chunkSize));
    }
    return window.btoa(binary);
  });
}

async function uploadAsset(file: File): Promise<UploadedAsset> {
  if (!currentFile) throw new Error("Save or open a note before attaching files");
  const path = nativeFilePath(file);
  if (path && window.aaronnoteApi?.assets?.storeFromPath) {
    const msg = await api.assets.storeFromPath({
      file: currentFile,
      path,
      name: file.name || fileNameFromPath(path),
      type: file.type || "",
    });
    if (!msg.ok || !msg.markdownPath) throw new Error(msg.message || "Asset upload failed");
    return msg;
  }
  const msg = await api.assets.upload({
    file: currentFile,
    name: file.name || "clipboard-image.png",
    type: file.type || "",
    data: await fileToBase64(file),
  });
  if (!msg.ok || !msg.markdownPath) throw new Error(msg.message || "Asset upload failed");
  return msg;
}

function markdownForAsset(asset: UploadedAsset): string {
  const path = asset.markdownPath || asset.file || "";
  const name = asset.name || fileNameFromPath(path);
  return (asset.isImage || visualMarkdownAttachmentP(path || name, asset.type))
    ? `![${name}](${path})`
    : `[${name}](${path})`;
}

async function insertFiles(files: File[], options: { pos?: number; mode?: "image-src" | "markdown" } = {}): Promise<void> {
  if (files.length === 0) return;
  setStatus(`Attaching ${files.length} file${files.length === 1 ? "" : "s"}`);
  try {
    const uploaded = await Promise.all(files.map(uploadAsset));
    if (typeof options.pos === "number" && options.mode === "image-src") {
      editor.replaceRange(options.pos, options.pos, uploaded[0]?.markdownPath || "", "end");
    } else {
      const markdown = uploaded.map(markdownForAsset).join("\n");
      const ctx = editor.cursorContext(200);
      const prefix = ctx.before && !ctx.before.endsWith("\n") ? "\n\n" : "";
      const suffix = ctx.after && !ctx.after.startsWith("\n") ? "\n\n" : "\n";
      editor.insertText(`${prefix}${markdown}${suffix}`);
    }
    void loadPathSuggestions();
    setStatus(`Attached ${uploaded.length} file${uploaded.length === 1 ? "" : "s"}`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Attach failed");
  }
}

function filesFromClipboard(event: ClipboardEvent): File[] {
  const files = Array.from(event.clipboardData?.files ?? []);
  if (files.length > 0) return files;
  return Array.from(event.clipboardData?.items ?? [])
    .filter((item) => item.kind === "file")
    .map((item) => item.getAsFile())
    .filter((file): file is File => Boolean(file));
}

function save(): void {
  if (!currentFile) {
    setStatus(scratchStatus());
    return;
  }
  void saveStandalone();
}

async function flushCurrentSaveForGit(): Promise<void> {
  saveDebounce.cancel();
  if (!currentFile || editRevision === savedRevision || saveConflictActive) return;
  await saveStandalone();
}

function draftContentForRevision(revision: number, snapshot: string): string {
  return revision === editRevision ? snapshot : editor.getMarkdown();
}

function syncSourceUi(): void {
  currentMode = editor.isSourceMode() ? "source" : "markdown";
  host.classList.toggle("is-source-mode", currentMode === "source");
  root.dataset.viewMode = currentMode;
  sourceButton.textContent = currentMode === "source" ? "Preview" : "Source";
  sourceButton.setAttribute("aria-pressed", currentMode === "source" ? "true" : "false");
  sourceButton.classList.toggle("is-active", currentMode === "source");
}

async function saveStandalone(): Promise<boolean> {
  const run = saveEpoch.begin();
  const revision = editRevision;
  const file = currentFile;
  if (!file) {
    setStatus(scratchStatus());
    return false;
  }
  const content = await editor.getMarkdownAsync();
  if (!run.current || file !== currentFile) return false;
  const mode = editor.isSourceMode() ? "source" : "markdown";
  setStatus("Saving");
  let saved = false;
  try {
    const msg = await api.notes.save({
      file,
      content,
      mode,
      clientId: saveClientId,
      seq: run.id,
      baseMtimeMs: currentFileMtimeMs,
      refresh: "deferred",
    });
    saved = msg.ok === true;
    if (!run.current || file !== currentFile) return false;
    if (msg.conflict) {
      saveConflictActive = true;
      currentFileMtimeMs = Number(msg.mtimeMs) || currentFileMtimeMs;
      currentFileSize = Number(msg.size) || currentFileSize;
      rememberDraft(draftContentForRevision(revision, content));
      setStatus(msg.message || "Save conflict");
      return false;
    }
    if (saved && typeof msg.mtimeMs === "number") currentFileMtimeMs = msg.mtimeMs;
    if (saved && typeof msg.size === "number") currentFileSize = msg.size;
    if (saved && revision === editRevision) {
      savedRevision = revision;
      clearDraft(file);
      setStatus(savedStatusText());
    } else if (saved) {
      setStatus("Dirty");
    } else {
      rememberDraft(draftContentForRevision(revision, content));
      setStatus(msg.message || "Save failed");
    }
    if (Array.isArray(msg.notes)) {
      applyIndexPayload(msg);
      renderNotes();
      if (notesToolVisible("agenda")) void loadAgendaTodos(true);
      if (graphToolVisible()) renderGraph();
      updateFloatingToc();
    }
    if (msg.note) {
      const previous = notes.find((item) => item.file === msg.note?.file);
      const refreshRelationships = noteNeedsRelationshipRefresh(previous, msg.note);
      upsertCurrentNoteSummary(msg.note, { preserveBacklinks: msg.notesRefresh === "deferred" });
      if (refreshRelationships || notesRefreshPending) scheduleNotesRefresh();
    }
    void applyNoteKindAssets(msg.kind ?? currentNote()?.kind ?? noteKindFromMarkdown(content));
  } catch (err) {
    if (err instanceof DOMException && err.name === "AbortError") return false;
    if (!run.current || file !== currentFile) return false;
    rememberDraft(draftContentForRevision(revision, content));
    setStatus(err instanceof Error ? err.message : "Save failed");
  }
  return saved;
}

async function forceSaveStandalone(): Promise<void> {
  if (!currentFile) return;
  const run = saveEpoch.begin();
  const revision = editRevision;
  const file = currentFile;
  const content = editor.getMarkdown();
  setStatus("Saving");
  try {
    const msg = await api.notes.save({
      file,
      content,
      mode: editor.isSourceMode() ? "source" : "markdown",
      clientId: saveClientId,
      seq: run.id,
      force: true,
      refresh: "deferred",
    });
    if (msg.ok !== true) throw new Error(msg.message || "Force save failed");
    if (file !== currentFile || !run.current) return;
    saveConflictActive = false;
    currentFileMtimeMs = Number(msg.mtimeMs) || currentFileMtimeMs;
    currentFileSize = Number(msg.size) || currentFileSize;
    if (revision === editRevision) {
      savedRevision = revision;
      clearDraft(file);
      setStatus(savedStatusText());
    } else {
      setStatus("Dirty");
    }
    if (msg.note) {
      upsertCurrentNoteSummary(msg.note);
      scheduleNotesRefresh();
    }
  } catch (err) {
    rememberDraft(draftContentForRevision(revision, content));
    setStatus(err instanceof Error ? err.message : "Force save failed");
  }
}

function pdfExportName(): string {
  const rawName = (currentFile || "Aaronnote.md").split(/[\\/]/).pop() || "Aaronnote.md";
  return `${rawName.replace(/\.[^.]+$/, "") || "Aaronnote"}.pdf`;
}

async function exportPdf(): Promise<void> {
  setStatus("Exporting PDF");
  const desktopExport = window.AaronnoteDesktop?.exportPdf;
  if (desktopExport) {
    try {
      if (!currentFile) {
        setStatus("Save the note before exporting PDF");
        return;
      }
      if (!await saveStandalone()) throw new Error("Save failed");
      const msg = await desktopExport({
        file: currentFile || "Aaronnote.md",
        name: pdfExportName(),
      });
      if (msg?.canceled) {
        setStatus("Export canceled");
        return;
      }
      if (!msg?.ok) throw new Error(msg?.message || "PDF export failed");
      setStatus(msg.message || `Exported ${msg.file || pdfExportName()}`);
    } catch (err) {
      setStatus(err instanceof Error ? err.message : "PDF export failed");
    }
    return;
  }

  try {
    const res = await api.notes.exportPdf({
      file: currentFile || "Aaronnote.md",
      content: editor.getMarkdown(),
    });
    if (!res.ok) {
      const msg = await res.json().catch(() => null) as { message?: string } | null;
      throw new Error(msg?.message || "PDF export failed");
    }
    const blob = await res.blob();
    const href = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = href;
    link.download = pdfExportName();
    document.body.appendChild(link);
    link.click();
    link.remove();
    URL.revokeObjectURL(href);
    setStatus(`Exported PDF ${link.download}`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "PDF export failed");
  }
}

async function syncRoamDb(): Promise<void> {
  setStatus("Syncing");
  try {
    const msg = await api.notes.roamSync();
    if (!Array.isArray(msg.notes)) throw new Error(msg.message || "Sync failed");
    applyIndexPayload(msg);
    renderNotes();
    if (notesToolVisible("agenda")) void loadAgendaTodos(true);
    if (graphToolVisible()) renderGraph();
    updateFloatingToc();
    setStatus(`Synced ${roamNotes().length} roam nodes`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Sync failed");
  }
}

async function syncRoamDbFull(): Promise<void> {
  setStatus("Full rebuild…");
  try {
    const msg = await api.notes.roamSyncFull();
    if (!Array.isArray(msg.notes)) throw new Error(msg.message || "Full sync failed");
    applyIndexPayload(msg);
    renderNotes();
    if (notesToolVisible("agenda")) void loadAgendaTodos(true);
    if (graphToolVisible()) renderGraph();
    updateFloatingToc();
    setStatus(`Rebuilt ${roamNotes().length} roam nodes`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Full sync failed");
  }
}

function normalizeProseDiagnostic(diag: unknown): ProseDiagnostic | null {
  const item = diag as Partial<ProseDiagnostic> | null;
  const source = item?.source;
  const from = Number(item?.from);
  const to = Number(item?.to);
  if (source !== "vale" && source !== "cspell" && source !== "browser") return null;
  if (!Number.isFinite(from) || !Number.isFinite(to) || from < 0 || to <= from) return null;
  if (to > editor.view.state.doc.length) return null;
  return {
    source,
    from,
    to,
    severity: item.severity === "error" || item.severity === "warning" || item.severity === "info" ? item.severity : "warning",
    message: String(item.message || "Prose issue"),
    rule: item.rule ? String(item.rule) : undefined,
    word: item.word ? String(item.word) : undefined,
    suggestions: Array.isArray(item.suggestions) ? item.suggestions.map((value) => String(value)).slice(0, 8) : [],
  };
}

type ProseCheckRange = { from: number; to: number };

const PROSE_FULL_DOCUMENT_LIMIT = 180_000;
const PROSE_VISIBLE_PADDING = 24_000;
const PROSE_SELECTION_PADDING = 1_200;
const PROSE_BROWSER_WORD_LIMIT = 1_200;
const PROSE_BROWSER_BATCH_SIZE = 140;
const PROSE_BROWSER_DIAGNOSTIC_LIMIT = 240;
const PROSE_DIAGNOSTIC_LIMIT = 520;

function expandProseRange(from: number, to: number, padding: number): ProseCheckRange {
  const doc = editor.view.state.doc;
  const docLength = doc.length;
  const start = Math.max(0, Math.min(docLength, Math.min(from, to) - padding));
  const end = Math.max(0, Math.min(docLength, Math.max(from, to) + padding));
  return {
    from: doc.lineAt(start).from,
    to: doc.lineAt(end).to,
  };
}

function mergeProseRanges(ranges: ProseCheckRange[]): ProseCheckRange[] {
  const sorted = ranges
    .filter((range) => Number.isFinite(range.from) && Number.isFinite(range.to) && range.to > range.from)
    .sort((a, b) => a.from - b.from || a.to - b.to);
  const merged: ProseCheckRange[] = [];
  for (const range of sorted) {
    const previous = merged[merged.length - 1];
    if (previous && range.from <= previous.to + 1) {
      previous.to = Math.max(previous.to, range.to);
    } else {
      merged.push({ ...range });
    }
  }
  return merged;
}

function proseCheckScope(markdown: string): { ranges: ProseCheckRange[]; label: string } {
  if (markdown.length <= PROSE_FULL_DOCUMENT_LIMIT) return { ranges: [], label: "" };
  const selected = editor.view.state.selection.ranges
    .filter((range) => !range.empty)
    .map((range) => expandProseRange(range.from, range.to, PROSE_SELECTION_PADDING));
  if (selected.length > 0) return { ranges: mergeProseRanges(selected), label: "selection" };
  const visible = editor.view.visibleRanges.length > 0
    ? editor.view.visibleRanges
    : [{ from: editor.view.state.selection.main.from, to: editor.view.state.selection.main.to }];
  return {
    ranges: mergeProseRanges(visible.map((range) => expandProseRange(range.from, range.to, PROSE_VISIBLE_PADDING))),
    label: "visible area",
  };
}

function proseScopeSegments(markdown: string, ranges: ProseCheckRange[]): Array<{ from: number; to: number; text: string }> {
  return ranges.map((range) => {
    const from = Math.max(0, Math.min(markdown.length, range.from));
    const to = Math.max(from, Math.min(markdown.length, range.to));
    return { from, to, text: markdown.slice(from, to) };
  }).filter((segment) => segment.to > segment.from);
}

function proseCheckPayload(file: string, markdown: string, ranges: ProseCheckRange[]): { file: string; content: string; ranges?: ProseCheckRange[]; segments?: Array<{ from: number; to: number; text: string }>; totalChars?: number } {
  const segments = proseScopeSegments(markdown, ranges);
  if (segments.length > 0) return { file, content: "", segments, totalChars: markdown.length };
  return { file, content: markdown, ranges };
}

function yieldForProseCheck(): Promise<void> {
  const idle = window.requestIdleCallback as ((callback: () => void, options?: { timeout: number }) => number) | undefined;
  if (idle) return new Promise((resolve) => idle(() => resolve(), { timeout: 80 }));
  return new Promise((resolve) => window.setTimeout(resolve, 0));
}

function browserSpellEntries(masked: string, ranges: ProseCheckRange[]): Array<{ word: string; ranges: ProseCheckRange[] }> {
  if (ranges.length === 0) {
    return collectBrowserSpellWords(masked, PROSE_BROWSER_WORD_LIMIT) as Array<{ word: string; ranges: ProseCheckRange[] }>;
  }
  const byWord = new Map<string, { word: string; ranges: ProseCheckRange[] }>();
  for (const range of ranges) {
    if (byWord.size >= PROSE_BROWSER_WORD_LIMIT) break;
    const from = Math.max(0, Math.min(masked.length, range.from));
    const to = Math.max(from, Math.min(masked.length, range.to));
    const entries = collectBrowserSpellWords(masked.slice(from, to), PROSE_BROWSER_WORD_LIMIT - byWord.size) as Array<{ word: string; ranges: ProseCheckRange[] }>;
    for (const entry of entries) {
      const existing = byWord.get(entry.word) ?? { word: entry.word, ranges: [] };
      existing.ranges.push(...entry.ranges.map((item) => ({ from: item.from + from, to: item.to + from })));
      byWord.set(entry.word, existing);
      if (byWord.size >= PROSE_BROWSER_WORD_LIMIT) break;
    }
  }
  return [...byWord.values()];
}

async function browserProseDiagnostics(markdown: string, ranges: ProseCheckRange[], run: { current: boolean }): Promise<ProseDiagnostic[]> {
  await yieldForProseCheck();
  if (!run.current) return [];
  const segments = proseScopeSegments(markdown, ranges);
  const diagnostics: ProseDiagnostic[] = [];
  const entries = segments.length > 0
    ? segments.flatMap((segment) => {
      const masked = maskAaronnoteProse(segment.text);
      return browserSpellEntries(masked, []).map((entry) => ({
        word: entry.word,
        ranges: entry.ranges.map((range) => ({ from: range.from + segment.from, to: range.to + segment.from })),
      }));
    }).slice(0, PROSE_BROWSER_WORD_LIMIT)
    : browserSpellEntries(maskAaronnoteProse(markdown), ranges);
  if (entries.length === 0) return [];
  for (let i = 0; i < entries.length; i += PROSE_BROWSER_BATCH_SIZE) {
    if (!run.current) return [];
    const batch = entries.slice(i, i + PROSE_BROWSER_BATCH_SIZE);
    const results = api.proseCheck.browserSpellcheck(batch.map((entry) => entry.word));
    const byWord = new Map(results.map((result) => [String(result.word || ""), result]));
    for (const entry of batch) {
      const result = byWord.get(entry.word);
      if (!result?.misspelled) continue;
      for (const range of entry.ranges) {
        diagnostics.push({
          source: "browser",
          from: range.from,
          to: range.to,
          severity: "warning",
          message: `Possible misspelling: ${entry.word}`,
          word: entry.word,
          suggestions: Array.isArray(result.suggestions) ? result.suggestions.map(String).slice(0, 8) : [],
        });
        if (diagnostics.length >= PROSE_BROWSER_DIAGNOSTIC_LIMIT) return diagnostics;
      }
    }
    await yieldForProseCheck();
  }
  return diagnostics;
}

function proseToolWarnings(tools: Array<{ source?: string; ok?: boolean; message?: string; optional?: boolean }> = []): string {
  const failed = tools
    .filter((tool) => tool.ok === false && !tool.optional)
    .map((tool) => `${tool.source || "tool"}: ${tool.message || "unavailable"}`);
  return failed.length ? ` (${failed.join("; ")})` : "";
}

async function checkProse(): Promise<void> {
  const run = proseEpoch.begin();
  const markdown = editor.getMarkdown();
  const file = currentFile || "Scratch.md";
  const scope = proseCheckScope(markdown);
  setStatus(scope.label ? `Checking prose in ${scope.label}` : "Checking prose");

  const diagnostics: ProseDiagnostic[] = [];
  let warnings = "";
  let externalDone = false;
  let browserDone = false;

  const applyProseResults = (): void => {
    if (!run.current) return;
    diagnostics.sort((a, b) => a.from - b.from || a.to - b.to || a.source.localeCompare(b.source));
    const shown = diagnostics.slice(0, PROSE_DIAGNOSTIC_LIMIT);
    setProseDiagnostics(editor.view, shown);
    const scopeText = scope.label ? ` in ${scope.label}` : "";
    const limitText = diagnostics.length > shown.length ? `, showing first ${shown.length}` : "";
    const pendingText = externalDone && !browserDone ? ", browser spellcheck finishing" : "";
    setStatus(`Prose check: ${shown.length} issue${shown.length === 1 ? "" : "s"}${scopeText}${limitText}${pendingText}${warnings}`);
  };

  const externalTask = api.proseCheck.run(proseCheckPayload(file, markdown, scope.ranges))
    .then((result) => {
      if (!run.current) return;
      diagnostics.push(...(result.diagnostics ?? []).map(normalizeProseDiagnostic).filter((item): item is ProseDiagnostic => !!item));
      warnings = proseToolWarnings(result.tools);
    })
    .catch((err) => {
      if (!run.current) return;
      warnings = ` (${err instanceof Error ? err.message : "external checks failed"})`;
    })
    .finally(() => {
      externalDone = true;
      applyProseResults();
    });

  const browserTask = browserProseDiagnostics(markdown, scope.ranges, run)
    .then((result) => {
      if (!run.current) return;
      diagnostics.push(...result);
    })
    .catch((err) => console.warn("[prose] browser check failed", err))
    .finally(() => {
      browserDone = true;
      applyProseResults();
    });

  await Promise.allSettled([externalTask, browserTask]);
}

function applyProseFixFromCommand(detail: { from?: unknown; to?: unknown; replacement?: unknown }): void {
  const from = Number(detail.from);
  const to = Number(detail.to);
  if (!Number.isFinite(from) || !Number.isFinite(to) || from < 0 || to <= from || to > editor.view.state.doc.length) {
    setStatus("Suggestion range is no longer valid");
    return;
  }
  const replacement = String(detail.replacement ?? "");
  editor.view.dispatch({ changes: { from, to, insert: replacement } });
  setStatus(replacement ? `Applied suggestion: ${replacement}` : "Removed flagged text");
}

async function restoreCurrentFileVersion(): Promise<void> {
  const file = currentFile;
  if (!file) { setStatus("No file open"); return; }
  try {
    const histMsg = await api.roamTools.fileHistory(file);
    const entries = histMsg.entries ?? [];
    if (entries.length === 0) { setStatus("No commit history for this file"); return; }
    const result = await openFormModal("Restore from commit", [
      {
        id: "sha",
        label: "Select version to restore",
        type: "select",
        value: entries[0].sha,
        options: entries.map((e) => ({
          label: `${e.date.slice(0, 16).replace("T", " ")}  ${e.subject}`,
          value: e.sha,
        })),
      },
    ], "Restore");
    if (!result?.sha) return;
    setStatus("Restoring…");
    const msg = await api.roamTools.restoreFileVersion({ file, sha: result.sha });
    if (msg.restoredFile) {
      applyIndexPayload(msg);
      renderNotes();
      const openMsg = await api.notes.open(file);
      applyOpen(openMsg as Extract<Inbound, { type: "open" }>);
      setStatus("Restored");
    }
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Restore failed");
  }
}

async function roamGitLog(): Promise<void> {
  setStatus("Loading git log…");
  try {
    const msg = await api.roamTools.repoHistory(30);
    const entries = msg.entries ?? [];
    showRoamToolRows(
      "Roam Git Log",
      entries.map((e) => ({
        title: e.subject,
        detail: `${e.date.slice(0, 16).replace("T", " ")}  ${e.sha.slice(0, 8)}`,
        kind: "commit",
      })),
    );
    setStatus(entries.length ? `${entries.length} commits` : "No commits");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Git log failed");
  }
}

async function roamGitStatus(): Promise<void> {
  setStatus("Checking git status…");
  try {
    const s = await api.roamTools.repoStatus();
    const parts: string[] = [];
    if (s.branch) parts.push(`Branch: ${s.branch}`);
    if ((s.ahead ?? 0) > 0) parts.push(`↑${s.ahead} ahead`);
    if ((s.behind ?? 0) > 0) parts.push(`↓${s.behind} behind`);
    if (s.uncommitted) parts.push("uncommitted changes");
    if (!s.hasRemote) parts.push("no remote");
    showRoamToolRows("Roam Git Status", parts.map((p) => ({ title: p })));
    setStatus(parts.join("  ·  ") || "Up to date");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Git status failed");
  }
}

async function roamCommitNow(): Promise<void> {
  try {
    const result = await openFormModal("Commit Roam", [
      { id: "message", label: "Commit message", type: "text", value: "" },
    ], "Commit");
    if (!result) return;
    setStatus("Committing…");
    await api.roamTools.commit(result.message ?? "");
    setStatus("Committed");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Commit failed");
  }
}

async function roamPush(): Promise<void> {
  setStatus("Pushing…");
  try {
    await api.roamTools.push();
    setStatus("Pushed to remote");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Push failed");
  }
}

function applyRoamToolIndexPayload(msg: { notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[] }): void {
  applyIndexPayload(msg);
  renderNotes();
  if (notesToolVisible("agenda")) void loadAgendaTodos(true);
  if (graphToolVisible()) renderGraph();
  updateFloatingToc();
}

function showRoamToolRows(title: string, rows: Array<{ title: string; detail?: string; kind?: string }>): void {
  roamToolsTitle.textContent = title;
  const fragment = document.createDocumentFragment();
  if (rows.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "No issues";
    fragment.appendChild(empty);
  }
  for (const row of rows) {
    const item = document.createElement("div");
    item.className = "aaronnote-roam-tool-item";
    const kind = document.createElement("span");
    kind.className = "aaronnote-roam-tool-kind";
    kind.textContent = row.kind || "ROAM";
    const body = document.createElement("div");
    body.className = "aaronnote-roam-tool-body";
    const titleEl = document.createElement("strong");
    titleEl.textContent = row.title;
    body.appendChild(titleEl);
    if (row.detail) {
      const detail = document.createElement("span");
      detail.textContent = row.detail;
      body.appendChild(detail);
    }
    item.append(kind, body);
    fragment.appendChild(item);
  }
  roamToolsList.replaceChildren(fragment);
  roamToolsSection.hidden = false;
}

function changedRows(changed: unknown): Array<{ title: string; detail?: string; kind?: string }> {
  const items = Array.isArray(changed) ? changed : [];
  return items.slice(0, 80).map((item) => {
    const value = item as { title?: string; path?: string; file?: string; count?: number; tags?: string[] };
    return {
      title: value.title || value.path || value.file || "Untitled",
      detail: [
        value.path || value.file || "",
        typeof value.count === "number" ? `${value.count} refs` : "",
        Array.isArray(value.tags) ? value.tags.join(", ") : "",
      ].filter(Boolean).join(" · "),
      kind: typeof value.count === "number" ? "REF" : "TAG",
    };
  });
}

async function renameRoamTagTool(): Promise<void> {
  const result = await openFormModal("Rename roam tag", [
    { id: "from", label: "Current tag", type: "tags", value: "", suggestions: tagSuggestions(), refreshSuggestions: refreshRoamTagSuggestions },
    { id: "to", label: "New tag", value: "" },
    { id: "confirm", label: "Type RENAME to update all roam notes", value: "" },
  ], "Rename");
  if (!result || result.confirm !== "RENAME") return;
  setStatus("Renaming roam tag");
  try {
    const msg = await api.roamTools.renameTag({ from: parseTagPrompt(result.from)[0] || result.from, to: result.to });
    applyRoamToolIndexPayload(msg);
    showRoamToolRows(`Renamed ${msg.changedCount ?? 0} notes`, changedRows(msg.changed));
    setStatus(`Renamed tag in ${msg.changedCount ?? 0} notes`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Roam tag rename failed");
  }
}

async function deleteRoamTagTool(): Promise<void> {
  const result = await openFormModal("Delete roam tag", [
    { id: "tag", label: "Tag", type: "tags", value: "", suggestions: tagSuggestions(), refreshSuggestions: refreshRoamTagSuggestions },
    { id: "confirm", label: "Type DELETE to remove it from all roam notes", value: "" },
  ], "Delete");
  if (!result || result.confirm !== "DELETE") return;
  setStatus("Deleting roam tag");
  try {
    const msg = await api.roamTools.deleteTag({ tag: parseTagPrompt(result.tag)[0] || result.tag });
    applyRoamToolIndexPayload(msg);
    showRoamToolRows(`Deleted tag from ${msg.changedCount ?? 0} notes`, changedRows(msg.changed));
    setStatus(`Deleted tag from ${msg.changedCount ?? 0} notes`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Roam tag delete failed");
  }
}

async function tagOverlapReportTool(): Promise<void> {
  setStatus("Scanning tag overlap");
  try {
    const report = await api.roamTools.tagOverlap();
    const duplicateRows = (Array.isArray(report.duplicateCase) ? report.duplicateCase : []).map((item) => {
      const value = item as { variants?: string[] };
      return {
        title: `Case variants: ${(value.variants || []).join(" / ")}`,
        detail: "Use Rename tag to normalize these",
        kind: "CASE",
      };
    });
    const overlapRows = (Array.isArray(report.overlaps) ? report.overlaps : []).map((item) => {
      const value = item as { a?: string; b?: string; aCount?: number; bCount?: number; sharedCount?: number; containment?: number };
      return {
        title: `${value.a || ""} overlaps ${value.b || ""}`,
        detail: `${value.sharedCount ?? 0} shared · ${value.aCount ?? 0}/${value.bCount ?? 0} notes · ${Math.round((value.containment ?? 0) * 100)}% containment`,
        kind: "TAG",
      };
    });
    showRoamToolRows(`Tag overlap (${report.tagCount ?? 0} tags)`, [...duplicateRows, ...overlapRows]);
    setStatus("Tag overlap scanned");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Tag overlap scan failed");
  }
}

async function rewritePathRefsTool(): Promise<void> {
  const result = await openFormModal("Rewrite path references", [
    { id: "oldPath", label: "Old target path", type: "path", value: "", suggestions: notePathSuggestions() },
    { id: "newPath", label: "New target path", type: "path", value: "", suggestions: notePathSuggestions() },
    { id: "confirm", label: "Type UPDATE to rewrite Markdown path links", value: "" },
  ], "Update");
  if (!result || result.confirm !== "UPDATE") return;
  setStatus("Rewriting path references");
  try {
    const msg = await api.roamTools.rewritePathRefs({ oldPath: result.oldPath, newPath: result.newPath });
    applyRoamToolIndexPayload(msg);
    showRoamToolRows(`Rewrote ${msg.referenceCount ?? 0} references`, changedRows(msg.changed));
    setStatus(`Rewrote ${msg.referenceCount ?? 0} references`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Path reference rewrite failed");
  }
}

function clientSlug(value: string): string {
  const slug = value
    .normalize("NFKD")
    .replace(/[^\w\s-]/g, "")
    .trim()
    .replace(/\s+/g, "-")
    .replace(/-+/g, "-")
    .toLowerCase();
  return slug || "untitled";
}

function parseTagPrompt(value: string | null): string[] {
  const byKey = new Map<string, string>();
  for (const tag of String(value || "").split(/[, ]+/)) {
    const clean = tag.trim().replace(/^#/, "");
    if (!clean) continue;
    const key = clean.toLowerCase();
    const previous = byKey.get(key);
    if (!previous || clean === key) byKey.set(key, clean);
  }
  return [...byKey.values()];
}

type ModalField = {
  id: string;
  label: string;
  value?: string;
  type?: "text" | "select" | "path" | "tags";
  pathMode?: "file" | "directory";
  browseTitle?: string;
  options?: Array<{ label: string; value: string }>;
  suggestions?: string[];
  refreshSuggestions?: () => Promise<string[]>;
  validate?: (value: string, values: Record<string, string>) => string;
};

function tagSuggestions(): string[] {
  const tags = new Map<string, string>();
  for (const note of notes) {
    if (!note.roam) continue;
    for (const tag of note.tags ?? []) {
      const clean = String(tag).trim().replace(/^#/, "");
      if (!clean) continue;
      const key = clean.toLowerCase();
      const previous = tags.get(key);
      if (!previous || clean === key) tags.set(key, clean);
    }
  }
  return [...tags.values()].sort((a, b) => a.localeCompare(b));
}

async function refreshRoamTagSuggestions(): Promise<string[]> {
  setStatus("Refreshing roam tags");
  const msg = await api.notes.roamSync(true);
  if (!Array.isArray(msg.notes)) throw new Error(msg.message || "Tag refresh failed");
  applyIndexPayload(msg);
  renderNotes();
  if (notesToolVisible("agenda")) void loadAgendaTodos(true);
  if (graphToolVisible()) renderGraph();
  updateFloatingToc();
  setStatus(`Synced ${roamNotes().length} roam nodes`);
  return tagSuggestions();
}

function roamNoteSuggestions(): string[] {
  return notes
    .filter((note) => note.roam && canonicalRoamNoteId(note))
    .map(roamNoteSearchValue)
    .sort((a, b) => a.localeCompare(b));
}

function selectedMarkdownText(): string {
  const selection = editor.getMarkdownSelection();
  if (selection.from === selection.to) return "";
  return editor.textBetween(selection.from, selection.to).trim();
}

async function insertRoamIdLink(): Promise<void> {
  if (currentStandalone) {
    setStatus("Roam idlinks are unavailable for standalone Markdown files");
    return;
  }
  const selection = editor.getMarkdownSelection();
  const selected = selectedMarkdownText();
  const result = await openFormModal("Insert roam idlink", [
    { id: "note", label: "Roam note", value: "", suggestions: roamNoteSuggestions() },
    { id: "label", label: "Link text", value: selected },
  ], "Insert");
  if (!result) return;
  const target = resolveRoamNoteSearch(notes, result.note);
  if (!target) {
    setStatus("Roam note not found");
    return;
  }
  const label = result.label || selected || target.title || canonicalRoamNoteId(target);
  const markdown = markdownRoamIdLink(target, label);
  if (!markdown) {
    setStatus("Roam note has no id");
    return;
  }
  editor.replaceMarkdownRange(selection.from, selection.to, markdown, "end");
  setStatus("Roam idlink inserted");
  scheduleAssistUpdate({ snippets: true, toc: true });
  scheduleCursorPositionSave();
}

async function openRoamNode(): Promise<void> {
  if (currentStandalone) {
    setStatus("Roam nodes are unavailable for standalone Markdown files");
    return;
  }
  const result = await openFormModal("Open roam node", [
    { id: "note", label: "Roam note", value: "", suggestions: roamNoteSuggestions() },
  ], "Open");
  if (!result) return;
  const target = resolveRoamNoteSearch(notes, result.note);
  if (!target) {
    setStatus("Roam note not found");
    return;
  }
  openNote(target);
}

function localDateSlug(date = new Date()): string {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}-${month}-${day}`;
}

async function openTodayDaily(): Promise<void> {
  if (currentStandalone) {
    setStatus("Daily notes are unavailable for standalone Markdown files");
    return;
  }
  const date = localDateSlug();
  const relPath = `daily/${date}.md`;
  const existing = notes.find((note) =>
    note.path === relPath
    || note.link === `daily/${date}.html`
    || note.file?.replace(/\\/g, "/").endsWith(`/roam/${relPath}`)
    || note.title === date);
  if (existing) {
    openNote(existing);
    return;
  }
  setStatus("Creating daily note");
  try {
    const msg = await api.notes.createNode({
      nodeType: "roam",
      title: date,
      path: relPath,
      tags: ["daily"],
    });
    applyOpen(msg);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Daily note create failed");
  }
}

function setTagButtonState(button: HTMLButtonElement, active: boolean): void {
  button.classList.toggle("is-active", active);
  button.setAttribute("aria-pressed", active ? "true" : "false");
}

function syncTagPickerState(input: HTMLInputElement, picker: HTMLElement): void {
  const existing = new Set(parseTagPrompt(input.value).map((tag) => tag.toLowerCase()));
  picker.querySelectorAll<HTMLButtonElement>("[data-modal-tag]").forEach((button) => {
    setTagButtonState(button, existing.has((button.dataset.modalTag || "").toLowerCase()));
  });
}

function toggleTagValue(input: HTMLInputElement, tag: string): void {
  const existing = parseTagPrompt(input.value);
  const index = existing.findIndex((item) => item.toLowerCase() === tag.toLowerCase());
  if (index >= 0) existing.splice(index, 1);
  else existing.push(tag);
  input.value = existing.join(", ");
  input.dispatchEvent(new Event("input", { bubbles: true }));
  input.focus();
}

function renderTagPickerButtons(input: HTMLInputElement, picker: HTMLElement, suggestions: string[]): void {
  const fragment = document.createDocumentFragment();
  for (const tag of suggestions.slice(0, 80)) {
    const button = document.createElement("button");
    button.type = "button";
    button.dataset.modalTag = tag;
    button.textContent = tag;
    button.addEventListener("click", () => toggleTagValue(input, tag));
    fragment.appendChild(button);
  }
  picker.replaceChildren(fragment);
  syncTagPickerState(input, picker);
}

function openFormModal(title: string, fields: ModalField[], submitLabel = "OK"): Promise<Record<string, string> | null> {
  return new Promise((resolve) => {
    modal.innerHTML = "";
    const panel = document.createElement("form");
    panel.className = "aaronnote-modal-panel";
    if (fields.some((field) => field.type === "tags")) panel.classList.add("has-tags");
    const heading = document.createElement("h2");
    heading.textContent = title;
    panel.appendChild(heading);
    const controls = new Map<string, HTMLInputElement | HTMLSelectElement>();
    const errors = new Map<string, HTMLElement>();
    let submit: HTMLButtonElement | null = null;
    const formValues = (): Record<string, string> => {
      const values: Record<string, string> = {};
      controls.forEach((control, id) => {
        values[id] = control.value.trim();
      });
      return values;
    };
    const updateValidation = (): boolean => {
      const values = formValues();
      let ok = true;
      for (const field of fields) {
        const control = controls.get(field.id);
        const error = errors.get(field.id);
        if (!control || !error || !field.validate) continue;
        const message = field.validate(control.value.trim(), values);
        error.textContent = message;
        error.hidden = !message;
        control.classList.toggle("has-error", Boolean(message));
        if (message) ok = false;
      }
      if (submit) submit.disabled = !ok;
      return ok;
    };
    for (const field of fields) {
      const label = document.createElement("label");
      label.textContent = field.label;
      const input = field.type === "select" ? document.createElement("select") : document.createElement("input");
      if (input instanceof HTMLInputElement) input.type = "text";
      input.name = field.id;
      if (input instanceof HTMLSelectElement) {
        for (const optionSpec of field.options ?? []) {
          const option = document.createElement("option");
          option.value = optionSpec.value;
          option.textContent = optionSpec.label;
          input.appendChild(option);
        }
      }
      input.value = field.value ?? "";
      if (input instanceof HTMLInputElement && field.suggestions?.length) {
        const listId = `aaronnote-modal-list-${field.id}`;
        const list = document.createElement("datalist");
        list.id = listId;
        for (const value of field.suggestions) {
          const option = document.createElement("option");
          option.value = value;
          list.appendChild(option);
        }
        input.setAttribute("list", listId);
        panel.appendChild(list);
      }
      if (field.type === "path" && input instanceof HTMLInputElement) {
        const row = document.createElement("div");
        row.className = "aaronnote-modal-path-row";
        const browse = document.createElement("button");
        browse.type = "button";
        browse.textContent = "Choose";
        browse.addEventListener("click", async () => {
          const directoryMode = field.pathMode === "directory";
          const picked = await window.AaronnoteDesktop?.chooseNotePath?.({
            suggestedPath: input.value || (directoryMode ? "." : "untitled.md"),
            title: field.browseTitle || (directoryMode ? "Choose folder" : "Choose note path"),
            mode: directoryMode ? "directory" : "file",
          });
          if (picked) input.value = picked;
          input.focus();
        });
        row.append(input, browse);
        label.appendChild(row);
      } else if (field.type === "tags" && input instanceof HTMLInputElement) {
        label.appendChild(input);
        if (field.suggestions?.length || field.refreshSuggestions) {
          const tools = document.createElement("div");
          tools.className = "aaronnote-modal-tag-tools";
          const picker = document.createElement("div");
          picker.className = "aaronnote-modal-tag-picker";
          if (field.refreshSuggestions) {
            const refresh = document.createElement("button");
            refresh.type = "button";
            refresh.textContent = "Refresh";
            refresh.addEventListener("click", async () => {
              refresh.disabled = true;
              refresh.textContent = "Refreshing";
              try {
                const suggestions = (await field.refreshSuggestions?.()) ?? [];
                renderTagPickerButtons(input, picker, suggestions);
              } catch (err) {
                setStatus(err instanceof Error ? err.message : "Tag refresh failed");
              } finally {
                refresh.disabled = false;
                refresh.textContent = "Refresh";
                input.focus();
              }
            });
            tools.appendChild(refresh);
          }
          renderTagPickerButtons(input, picker, field.suggestions ?? []);
          input.addEventListener("input", () => syncTagPickerState(input, picker));
          label.appendChild(tools);
          label.appendChild(picker);
        }
      } else {
        label.appendChild(input);
      }
      if (field.validate) {
        const error = document.createElement("div");
        error.className = "aaronnote-modal-field-error";
        error.hidden = true;
        label.appendChild(error);
        errors.set(field.id, error);
        input.addEventListener("input", updateValidation);
      }
      panel.appendChild(label);
      controls.set(field.id, input);
    }
    const actions = document.createElement("div");
    actions.className = "aaronnote-modal-actions";
    const cancel = document.createElement("button");
    cancel.type = "button";
    cancel.textContent = "Cancel";
    submit = document.createElement("button");
    submit.type = "submit";
    submit.textContent = submitLabel;
    actions.append(cancel, submit);
    panel.appendChild(actions);
    const close = (value: Record<string, string> | null) => {
      modal.hidden = true;
      modal.innerHTML = "";
      resolve(value);
    };
    cancel.addEventListener("click", () => close(null));
    modal.addEventListener("mousedown", (event) => {
      if (event.target === modal) close(null);
    }, { once: true });
    panel.addEventListener("submit", (event) => {
      event.preventDefault();
      if (!updateValidation()) return;
      const out: Record<string, string> = {};
      controls.forEach((control, id) => {
        out[id] = control.value.trim();
      });
      close(out);
    });
    modal.appendChild(panel);
    modal.hidden = false;
    updateValidation();
    controls.values().next().value?.focus();
  });
}

function notePathSuggestions(): string[] {
  const dirs = new Set<string>([""]);
  for (const dir of directories) {
    const path = normalizeNotePath(dir.path || "");
    if (path && path !== "Root") dirs.add(`${path}/`);
  }
  for (const note of notes) {
    const path = normalizeNotePath(note.path || note.file || "");
    const parts = path.split("/").filter(Boolean);
    parts.pop();
    if (parts.length) dirs.add(`${parts.join("/")}/`);
  }
  return [...dirs].sort();
}

async function loadPathSuggestions(): Promise<void> {
  if (!currentFile) {
    pathSuggestions = [];
    return;
  }
  try {
    const msg = await api.notes.pathSuggestions(currentFile);
    if (Array.isArray(msg.paths)) pathSuggestions = msg.paths;
  } catch {
    pathSuggestions = [];
  }
}

async function reloadTemplates(force = false): Promise<void> {
  try {
    const msg = await api.notes.templates(force);
    if (Array.isArray(msg.templates)) templates = msg.templates;
  } catch (err) {
    console.warn("[notes] template reload failed", err);
  }
}

function templateOptions(kind = ""): Array<{ label: string; value: string }> {
  const base = [{ label: "None", value: "" }];
  const activeKind = normalizeNotePath(kind || "").replace(/\//g, "");
  const available = templates.length > 0 ? templates : fallbackTemplates;
  const items = available
    .filter((template) => !template.kind || !activeKind || template.kind === activeKind)
    .map((template) => ({
      label: `${template.name || template.key || "Template"}${template.kind ? ` (${template.kind})` : ""}`,
      value: template.key || "",
    }))
    .filter((item) => item.value);
  return [...base, ...items];
}

function noteDraftPath(title: string, baseDir = ""): string {
  const name = `${clientSlug(title || "Untitled") || "untitled"}.md`;
  const dir = normalizeNotePath(baseDir);
  return dir ? `${dir}/${name}` : name;
}

type NewNodeDraft = {
  nodeType: "roam" | "regular";
  title: string;
  path: string;
  tags: string[];
  kind?: string;
  templateKey?: string;
};
type CreateNodeBehavior = { stayInFilesystem?: boolean };

const fallbackTemplates: TemplateSummary[] = [
  { key: "basic", name: "Basic Markdown note", mode: "markdown-mode" },
  { key: "roam", name: "Roam note", mode: "markdown-mode" },
  { key: "daily", name: "Daily note", mode: "markdown-mode" },
  { key: "weekly-review", name: "Weekly review", mode: "markdown-mode" },
  { key: "meeting", name: "Meeting notes", mode: "markdown-mode" },
  { key: "project", name: "Project brief", mode: "markdown-mode" },
  { key: "reading", name: "Reading notes", mode: "markdown-mode" },
  { key: "zettel", name: "Zettel", mode: "markdown-mode" },
  { key: "task-plan", name: "Task plan", mode: "markdown-mode" },
  { key: "decision", name: "Decision record", mode: "markdown-mode" },
];

async function promptNewNode(baseDir = ""): Promise<NewNodeDraft | null> {
  await reloadTemplates(true);
  const first = await openFormModal("New note", [
    { id: "nodeType", label: "Type", type: "select", value: "roam", options: [
      { label: "Roam", value: "roam" },
      { label: "Regular", value: "regular" },
    ] },
    { id: "title", label: "Title", value: "Untitled" },
    { id: "path", label: "Save path", type: "path", value: noteDraftPath("Untitled", baseDir), suggestions: notePathSuggestions() },
    { id: "kind", label: "Kind", value: "note" },
    { id: "templateKey", label: "Template", type: "select", value: "roam", options: templateOptions() },
    { id: "tags", label: "Tags", type: "tags", value: "", suggestions: tagSuggestions(), refreshSuggestions: refreshRoamTagSuggestions },
  ], "Create");
  if (!first) return null;
  const title = first.title || "Untitled";
  const nodeType = first.nodeType === "regular" ? "regular" : "roam";
  return {
    nodeType,
    title,
    path: first.path || noteDraftPath(title, baseDir),
    tags: parseTagPrompt(first.tags),
    kind: first.kind || (nodeType === "roam" ? "note" : "default"),
    templateKey: first.templateKey || "",
  };
}

async function promptTypedNewNode(nodeType: "roam" | "regular", baseDir = ""): Promise<NewNodeDraft | null> {
  await reloadTemplates(true);
  const defaultTemplate = nodeType === "roam" ? "roam" : "basic";
  const fields: ModalField[] = [
    { id: "title", label: "Title", value: "Untitled" },
    { id: "path", label: "Save path", type: "path", value: noteDraftPath("Untitled", baseDir), suggestions: notePathSuggestions() },
    { id: "kind", label: "Kind", value: nodeType === "roam" ? "note" : "default" },
    { id: "templateKey", label: "Template", type: "select", value: defaultTemplate, options: templateOptions(nodeType === "roam" ? "note" : "") },
  ];
  if (nodeType === "roam") {
    fields.push({ id: "tags", label: "Tags", type: "tags", value: "", suggestions: tagSuggestions(), refreshSuggestions: refreshRoamTagSuggestions });
  }
  const result = await openFormModal(nodeType === "roam" ? "New roam note" : "New Markdown note", fields, "Create");
  if (!result) return null;
  const title = result.title || "Untitled";
  return {
    nodeType,
    title,
    path: result.path || noteDraftPath(title, baseDir),
    tags: nodeType === "roam" ? parseTagPrompt(result.tags) : [],
    kind: result.kind || (nodeType === "roam" ? "note" : "default"),
    templateKey: result.templateKey || "",
  };
}

async function createNodeFromDraft(draft: NewNodeDraft | null, behavior: CreateNodeBehavior = {}): Promise<void> {
  if (!draft) return;
  setStatus("Creating node");
  try {
    const msg = await api.notes.createNode(draft as Record<string, unknown>);
    applyOpen(msg, { preserveFocus: behavior.stayInFilesystem === true });
    if (behavior.stayInFilesystem) {
      showNotesPage("filesystem");
      focusFilesystemRangerSoon();
    } else {
      showEditorPage();
    }
    setStatus(draft.nodeType === "roam" ? "Roam node created" : "Markdown file created");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Create node failed");
  }
}

async function createNode(baseDir = "", behavior: CreateNodeBehavior = {}): Promise<void> {
  if (currentStandalone) {
    await createMarkdownNote(baseDir, behavior);
    return;
  }
  await createNodeFromDraft(await promptNewNode(baseDir), behavior);
}

async function createRoamNode(baseDir = "", behavior: CreateNodeBehavior = {}): Promise<void> {
  await createNodeFromDraft(await promptTypedNewNode("roam", baseDir), behavior);
}

async function createMarkdownNote(baseDir = "", behavior: CreateNodeBehavior = {}): Promise<void> {
  await createNodeFromDraft(await promptTypedNewNode("regular", baseDir), behavior);
}

async function deleteCurrentNote(): Promise<void> {
  if (!currentFile) {
    setStatus("No current note");
    return;
  }
  if (currentStandalone) {
    setStatus("Standalone Markdown files are not managed as roam notes");
    return;
  }
  const confirmed = await openFormModal("Delete note", [
    { id: "confirm", label: `Type TRASH to move ${currentFile} to the system Trash`, value: "" },
  ], "Move to Trash");
  if (confirmed?.confirm !== "TRASH") return;
  setStatus("Moving note to Trash");
  try {
    const fileToDelete = currentFile;
    if (window.AaronnoteDesktop?.trashNote) {
      const desktopResult = await window.AaronnoteDesktop.trashNote(fileToDelete);
      if (!desktopResult?.ok) throw new Error(desktopResult?.message || "Move to Trash failed");
      const msg = await api.notes.roamSync(true);
      if (!Array.isArray(msg.notes)) throw new Error(msg.message || "Refresh failed");
      applyIndexPayload(msg);
    } else {
      const msg = await api.notes.deleteNote(fileToDelete);
      if (!msg.ok) throw new Error(msg.message || "Move to Trash failed");
      applyIndexPayload(msg);
    }
    cursorPositions.delete(fileToDelete);
    saveCursorPositionsLocalNow();
    currentFile = "";
    fileLabel.textContent = "Scratch";
    editor.setMarkdown("", { history: "reset" });
    renderNotes();
    focusFilesystemRangerSoon();
    if (graphToolVisible()) renderGraph();
    updateFloatingToc();
    setStatus("Moved note to Trash");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move to Trash failed");
  }
}

async function deleteNoteFromBrowser(note: NoteSummary): Promise<void> {
  if (!note.file) {
    setStatus("No selected note");
    return;
  }
  const label = note.path || note.file;
  const confirmed = await openFormModal("Delete note", [
    { id: "confirm", label: `Type TRASH to move ${label} to the system Trash`, value: "" },
  ], "Move to Trash");
  if (confirmed?.confirm !== "TRASH") return;
  setStatus("Moving note to Trash");
  try {
    const fileToDelete = note.file;
    if (window.AaronnoteDesktop?.trashNote) {
      const desktopResult = await window.AaronnoteDesktop.trashNote(fileToDelete);
      if (!desktopResult?.ok) throw new Error(desktopResult?.message || "Move to Trash failed");
      const msg = note.standalone ? await api.notes.list(true) : await api.notes.roamSync(true);
      if (!Array.isArray(msg.notes)) throw new Error((msg as { message?: string }).message || "Refresh failed");
      applyIndexPayload(msg);
    } else {
      const msg = await api.notes.deleteNote(fileToDelete);
      if (!msg.ok) throw new Error(msg.message || "Move to Trash failed");
      applyIndexPayload(msg);
    }
    cursorPositions.delete(fileToDelete);
    saveCursorPositionsLocalNow();
    if (fileToDelete === currentFile) {
      currentFile = "";
      fileLabel.textContent = "Scratch";
      editor.setMarkdown("", { history: "reset" });
    }
    renderNotes();
    focusFilesystemRangerSoon();
    if (graphToolVisible()) renderGraph();
    updateFloatingToc();
    setStatus("Moved note to Trash");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move to Trash failed");
  }
}

async function createFolderFromBrowser(baseDir: string): Promise<string | null> {
  const initial = baseDir ? `${normalizeNotePath(baseDir)}/` : "";
  const result = await openFormModal("New folder", [
    { id: "path", label: "Folder path", type: "path", pathMode: "directory", browseTitle: "Choose folder", value: initial, suggestions: notePathSuggestions() },
  ], "Create");
  if (!result) return null;
  const folder = normalizeNotePath(result.path || "");
  if (!folder) return null;
  setStatus("Creating folder");
  try {
    const msg = await api.notes.createFolder(folder);
    if (!msg.ok) throw new Error(msg.message || "Create folder failed");
    applyIndexPayload(msg);
    renderNotes();
    focusFilesystemRangerSoon();
    setStatus(`Folder created: ${msg.path || folder}`);
    return msg.path || folder;
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Create folder failed");
    return null;
  }
}

async function postFilesystemAction(
  action: (body: Record<string, unknown>) => Promise<Record<string, unknown>>,
  body: Record<string, unknown>,
): Promise<Record<string, unknown>> {
  const msg = await action(body);
  applyIndexPayload(msg as { notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[] });
  renderNotes();
  focusFilesystemRangerSoon();
  if (graphToolVisible()) renderGraph();
  updateFloatingToc();
  return msg;
}

async function revealPathFromBrowser(path: string): Promise<void> {
  const target = String(path || "").trim();
  if (!target) return;
  setStatus("Revealing file");
  try {
    await api.shell.showInFolder(target);
    setStatus("Revealed");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Reveal failed");
  }
}

async function openDirectoryFromBrowser(path: string): Promise<void> {
  const target = String(path || "").trim();
  if (!target) return;
  setStatus("Opening folder");
  try {
    await api.shell.openDirectory(target, currentFile);
    setStatus("Folder opened");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Open folder failed");
  }
}

async function openDirectoryInKittyFromBrowser(path: string): Promise<void> {
  const target = String(path || "").trim();
  if (!target) return;
  setStatus("Opening Kitty");
  try {
    await api.shell.openDirectoryInKitty(target, currentFile);
    setStatus("Kitty opened");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Open Kitty failed");
  }
}

function noteFileName(note: NoteSummary): string {
  const path = normalizeNotePath(note.path || note.file || "");
  return path.split("/").filter(Boolean).at(-1) || "note.md";
}

function directoryLeaf(path: string): string {
  return normalizeNotePath(path).split("/").filter(Boolean).at(-1) || "folder";
}

function directoryParent(path: string): string {
  return dirnamePath(normalizeNotePath(path));
}

function filePathForClient(file: FileSummary): string {
  return normalizeNotePath(file.path || file.file || "");
}

function fileDisplayName(file: FileSummary): string {
  return filePathForClient(file).split("/").filter(Boolean).at(-1) || "file";
}

function fileDirectoryForClient(file: FileSummary): string {
  return dirnamePath(filePathForClient(file));
}

function suggestedDuplicatePath(path: string, fallbackExt = ""): string {
  const currentPath = normalizeNotePath(path);
  const extIndex = currentPath.lastIndexOf(".");
  if (extIndex >= 0) return `${currentPath.slice(0, extIndex)} copy${currentPath.slice(extIndex)}`;
  return `${currentPath} copy${fallbackExt}`;
}

async function renameNoteFromBrowser(note: NoteSummary): Promise<void> {
  if (!note.file) return;
  const result = await openFormModal("Rename note", [
    { id: "name", label: "File name", value: noteFileName(note) },
  ], "Rename");
  if (!result?.name) return;
  setStatus("Renaming note");
  try {
    const msg = await postFilesystemAction(api.fs.rename, { path: note.path || note.file, name: result.name });
    if (note.file === currentFile && typeof msg?.file === "string") {
      currentFile = msg.file;
      fileLabel.textContent = currentFile;
      syncCurrentFileUrl();
    }
    setStatus("Note renamed");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Rename failed");
  }
}

async function renameFileFromBrowser(file: FileSummary): Promise<void> {
  const path = filePathForClient(file);
  if (!path) return;
  const result = await openFormModal("Rename file", [
    { id: "name", label: "File name", value: fileDisplayName(file) },
  ], "Rename");
  if (!result?.name) return;
  setStatus("Renaming file");
  try {
    await postFilesystemAction(api.fs.rename, { path, name: result.name });
    setStatus("File renamed");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Rename failed");
  }
}

async function renameDirectoryFromBrowser(dir: string): Promise<void> {
  const result = await openFormModal("Rename folder", [
    { id: "name", label: "Folder name", value: directoryLeaf(dir) },
  ], "Rename");
  if (!result?.name) return;
  setStatus("Renaming folder");
  try {
    await postFilesystemAction(api.fs.rename, { path: dir, name: result.name });
    setStatus("Folder renamed");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Rename failed");
  }
}

async function moveNoteFromBrowser(note: NoteSummary): Promise<void> {
  if (!note.file) return;
  const result = await openFormModal("Move note", [
    { id: "directory", label: "Target folder", type: "path", pathMode: "directory", browseTitle: "Choose target folder", value: filesystemGroupForClient(note), suggestions: notePathSuggestions() },
  ], "Move");
  if (!result) return;
  setStatus("Moving note");
  try {
    const msg = await postFilesystemAction(api.fs.move, { path: note.path || note.file, directory: result.directory || "." });
    await refreshNotesIndex(true);
    if (note.file === currentFile && typeof msg?.file === "string") {
      currentFile = msg.file;
      fileLabel.textContent = currentFile;
      syncCurrentFileUrl();
    }
    setStatus("Note moved");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move failed");
  }
}

async function moveFileFromBrowser(file: FileSummary): Promise<void> {
  const path = filePathForClient(file);
  if (!path) return;
  const result = await openFormModal("Move file", [
    { id: "directory", label: "Target folder", type: "path", pathMode: "directory", browseTitle: "Choose target folder", value: fileDirectoryForClient(file), suggestions: notePathSuggestions() },
  ], "Move");
  if (!result) return;
  setStatus("Moving file");
  try {
    await postFilesystemAction(api.fs.move, { path, directory: result.directory || "." });
    await refreshNotesIndex(true);
    setStatus("File moved");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move failed");
  }
}

async function moveDirectoryFromBrowser(dir: string): Promise<void> {
  const result = await openFormModal("Move folder", [
    { id: "directory", label: "Target folder", type: "path", pathMode: "directory", browseTitle: "Choose target folder", value: directoryParent(dir), suggestions: notePathSuggestions() },
  ], "Move");
  if (!result) return;
  setStatus("Moving folder");
  try {
    await postFilesystemAction(api.fs.move, { path: dir, directory: result.directory || "." });
    await refreshNotesIndex(true);
    setStatus("Folder moved");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move failed");
  }
}

async function duplicateNoteFromBrowser(note: NoteSummary): Promise<void> {
  if (!note.file) return;
  const currentPath = normalizeNotePath(note.path || note.file || "");
  const suggested = suggestedDuplicatePath(currentPath, ".md");
  const result = await openFormModal("Duplicate note", [
    { id: "target", label: "New path", type: "path", value: suggested, suggestions: notePathSuggestions() },
  ], "Duplicate");
  if (!result) return;
  setStatus("Duplicating note");
  try {
    await postFilesystemAction(api.fs.duplicate, { path: note.path || note.file, target: result.target || suggested });
    setStatus("Note duplicated");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Duplicate failed");
  }
}

async function duplicateFileFromBrowser(file: FileSummary): Promise<void> {
  const path = filePathForClient(file);
  if (!path) return;
  const suggested = suggestedDuplicatePath(path);
  const result = await openFormModal("Duplicate file", [
    { id: "target", label: "New path", type: "path", value: suggested, suggestions: notePathSuggestions() },
  ], "Duplicate");
  if (!result) return;
  setStatus("Duplicating file");
  try {
    await postFilesystemAction(api.fs.duplicate, { path, target: result.target || suggested });
    setStatus("File duplicated");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Duplicate failed");
  }
}

async function trashFileFromBrowser(file: FileSummary): Promise<void> {
  const path = filePathForClient(file);
  if (!path) return;
  const confirmed = await openFormModal("Move file to Trash", [
    { id: "confirm", label: `Type TRASH to move ${path} to the system Trash`, value: "" },
  ], "Move to Trash");
  if (confirmed?.confirm !== "TRASH") return;
  setStatus("Moving file to Trash");
  try {
    await postFilesystemAction(api.fs.trash, { path, confirm: "TRASH" });
    await refreshNotesIndex(true);
    focusFilesystemRangerSoon();
    setStatus("File moved to Trash");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move to Trash failed");
  }
}

async function trashDirectoryFromBrowser(dir: string): Promise<void> {
  const confirmed = await openFormModal("Move folder to Trash", [
    { id: "confirm", label: `Type TRASH to move ${dir} to the system Trash`, value: "" },
  ], "Move to Trash");
  if (confirmed?.confirm !== "TRASH") return;
  setStatus("Moving folder to Trash");
  try {
    await postFilesystemAction(api.fs.trash, { path: dir, confirm: "TRASH" });
    await refreshNotesIndex(true);
    focusFilesystemRangerSoon();
    setStatus("Folder moved to Trash");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Move to Trash failed");
  }
}

function filesystemGroupForClient(note: NoteSummary): string {
  return note.groupKey || dirnamePath(note.path || note.file || "");
}

async function updateNoteMeta(
  action: (body: Record<string, unknown>) => Promise<Record<string, unknown>>,
  body: Record<string, unknown>,
  success: string,
): Promise<void> {
  if (!currentFile) {
    setStatus("No current note");
    return;
  }
  if (currentStandalone) {
    setStatus("Roam metadata is unavailable for standalone Markdown files");
    return;
  }
  setStatus("Updating note");
  try {
    const msg = await action({
      file: currentFile,
      content: editor.getMarkdown(),
      ...body,
    }) as Extract<Inbound, { type: "open" }> & { message?: string };
    applyOpen(msg);
    setStatus(success);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Update failed");
  }
}

async function ensureRoamId(): Promise<void> {
  if (!currentFile) {
    setStatus("No current note");
    return;
  }
  if (currentStandalone) {
    setStatus("Roam ID is unavailable for standalone Markdown files");
    return;
  }
  const existing = currentNote();
  if (existing?.roam && existing.id) {
    await copyText(existing.id);
    setStatus("Roam ID copied");
    return;
  }
  setStatus("Generating Roam ID");
  try {
    const msg = await api.notes.metaAdd({
      file: currentFile,
      content: editor.getMarkdown(),
      title: existing?.title || fileLabel.textContent || "Untitled",
      tags: existing?.tags || [],
    });
    applyOpen(msg);
    const generated = currentNote();
    const id = generated?.roam ? generated.id || "" : "";
    if (!id) throw new Error("Roam ID was not generated");
    await copyText(id);
    setStatus("Roam ID generated and copied");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Generate Roam ID failed");
  }
}

async function quickAddMeta(): Promise<void> {
  const result = await openFormModal("Quick add meta", [
    { id: "title", label: "Title", value: fileLabel.textContent || "Untitled" },
    { id: "tags", label: "Tags", type: "tags", value: "", suggestions: tagSuggestions(), refreshSuggestions: refreshRoamTagSuggestions },
  ], "Register");
  if (!result) return;
  await updateNoteMeta(api.meta.add, { title: result.title, tags: parseTagPrompt(result.tags) }, "Meta registered");
}

async function unregisterMeta(): Promise<void> {
  const confirmed = await openFormModal("Unregister meta", [
    { id: "confirm", label: "Type REMOVE to delete roam meta from current note", value: "" },
  ], "Remove");
  if (confirmed?.confirm !== "REMOVE") return;
  await updateNoteMeta(api.meta.remove, {}, "Meta unregistered");
}

async function hideCurrentRoam(): Promise<void> {
  await updateNoteMeta(api.meta.hideRoam, {}, "roam: off set");
}

async function activateCurrentRoam(): Promise<void> {
  await updateNoteMeta(api.meta.activateRoam, {}, "roam: off cleared");
}

async function addTag(): Promise<void> {
  const result = await openFormModal("Add tag", [
    { id: "tags", label: "Tags", type: "tags", value: "", suggestions: tagSuggestions(), refreshSuggestions: refreshRoamTagSuggestions },
  ], "Add");
  if (!result) return;
  const tags = parseTagPrompt(result.tags);
  if (tags.length === 0) return;
  await updateNoteMeta(api.meta.tag, { tags }, "Tag added");
}

type MathTagTarget = {
  tex: string;
  replace: (nextTex: string, tag: string) => void;
};

function findDisplayMathRangeInMarkdown(markdown: string, pos: number): { bodyFrom: number; bodyTo: number; tex: string } | null {
  const fence = /^[ \t]*\$\$[ \t]*$/gm;
  let open: RegExpExecArray | null;
  while ((open = fence.exec(markdown))) {
    const openStart = open.index;
    const openEnd = openStart + open[0].length;
    const bodyFrom = markdown[openEnd] === "\n" ? openEnd + 1 : openEnd;
    fence.lastIndex = bodyFrom;
    const close = fence.exec(markdown);
    if (!close) return null;
    const closeStart = close.index;
    const closeEnd = closeStart + close[0].length;
    const bodyTo = markdown[closeStart - 1] === "\n" ? closeStart - 1 : closeStart;
    if (pos >= openStart && pos <= closeEnd) {
      return { bodyFrom, bodyTo, tex: markdown.slice(bodyFrom, bodyTo) };
    }
    fence.lastIndex = closeEnd;
  }
  return null;
}

function activeDisplayMathTarget(): MathTagTarget | null {
  const selection = editor.getSelection();
  const markdown = editor.getMarkdown();
  const range = findDisplayMathRangeInMarkdown(markdown, selection.from);
  if (!range) return null;
  return {
    tex: range.tex,
    replace(nextTex, tag) {
      editor.replaceRange(range.bodyFrom, range.bodyTo, nextTex, "end");
      selectLatexTag(range.bodyFrom, nextTex, tag);
    },
  };
}

function selectLatexTag(base: number, tex: string, tag: string): void {
  const range = findLatexTagRange(tex, tag);
  if (!range) return;
  editor.setSelection(base + range.from, base + range.to);
  editor.revealCursor();
}

function currentNote(): NoteSummary | undefined {
  const note = notes.find((item) => item.file === currentFile);
  if (note) return note;
  if (!currentFile) return undefined;
  return {
    file: currentFile,
    path: currentFile,
    title: fileNameFromPath(currentFile),
    kind: noteKindFromMarkdown(editor.getMarkdown()),
    standalone: currentStandalone,
  };
}

function notePathUnderRoam(value: unknown): boolean {
  let path = String(value || "").replace(/\\/g, "/").replace(/^\.\//, "");
  const roamIndex = path.indexOf("/roam/");
  if (roamIndex >= 0) path = path.slice(roamIndex + 1);
  return path === "roam" || path.startsWith("roam/");
}

function currentNoteSupportsLocalGraph(): boolean {
  const note = currentNote();
  return Boolean(note && (
    notePathUnderRoam(note.path)
    || notePathUnderRoam(note.file)
    || notePathUnderRoam(note.groupKey)
  ));
}

function syncLocalGraphAvailability(): void {
  const enabled = !host.hidden && currentNoteSupportsLocalGraph();
  localGraph.hidden = !enabled;
  if (!enabled) {
    localGraphPanel.collapse();
    return;
  }
  localGraphPanel.invalidate();
}

function parseMetaScalar(value: string): string {
  const trimmed = value.trim();
  if ((trimmed.startsWith('"') && trimmed.endsWith('"')) || (trimmed.startsWith("'") && trimmed.endsWith("'"))) {
    return trimmed.slice(1, -1);
  }
  return trimmed.replace(/\\_/g, "_");
}

function firstMetaValue(raw: string, keys: string[]): string {
  const wanted = new Set(keys.map((key) => key.toLowerCase()));
  for (const line of raw.split(/\r?\n/)) {
    const pair = line.match(/^\s*([A-Za-z0-9_-]+)\s*:\s*(.*?)\s*$/);
    if (!pair || !wanted.has(pair[1].toLowerCase())) continue;
    return parseMetaScalar(pair[2]);
  }
  return "";
}

function noteKindFromMarkdown(markdown: string): string {
  const text = String(markdown || "");
  const org = text.match(/^\s*#\+begin\s+meta\s*\r?\n([\s\S]*?)\r?\n\s*#\+end\s+meta\s*$/im);
  const yaml = text.match(/^\s*---\s*\r?\n([\s\S]*?)\r?\n---\s*(?:\r?\n|$)/);
  return normalizeNoteKind(firstMetaValue(org?.[1] ?? yaml?.[1] ?? "", ["kind", "kinds"]));
}

function normalizeNoteKind(value: unknown): string {
  const kind = String(Array.isArray(value) ? value[0] : value || "").trim().replace(/\\_/g, "_").toLowerCase();
  if (!kind || kind === "default" || kind === "note") return "default";
  return /^[a-z0-9_-]+$/.test(kind) ? kind : "default";
}

function activeKindName(value: unknown): string {
  const kind = normalizeNoteKind(value);
  return kind === "default" ? "" : kind;
}

function noteKindContext(kind: string): NoteKindContext {
  return {
    kind,
    file: currentFile,
    note: currentNote(),
    content: editor.getMarkdown(),
    editor,
    host,
    root,
  };
}

function setKindDataset(kind: string): void {
  const value = kind || "default";
  root.dataset.noteKind = value;
  host.dataset.noteKind = value;
  document.body.dataset.noteKind = value;
}

function prepareNoteKindRender(kindValue: unknown): void {
  const kind = activeKindName(kindValue);
  if (activeNoteKind && activeNoteKind !== kind) clearNoteKindAssets();
  setKindDataset(kind);
}

function clearNoteKindAssets(): void {
  const context = activeNoteKind ? noteKindContext(activeNoteKind) : null;
  try {
    noteKindCleanup?.();
  } catch (err) {
    console.warn("Aaronnote kind cleanup failed", err);
  }
  if (context) {
    window.dispatchEvent(new CustomEvent("aaronnote:kind-leave", { detail: context }));
  }
  noteKindCleanup = null;
  activeNoteKind = "";
  document.querySelectorAll<HTMLLinkElement>("link[data-aaronnote-kind-asset]").forEach((link) => link.remove());
}

function dispatchNoteKindReady(kind: string): void {
  window.dispatchEvent(new CustomEvent("aaronnote:kind-ready", { detail: noteKindContext(kind) }));
}

async function applyNoteKindAssets(kindValue: unknown): Promise<void> {
  const kind = activeKindName(kindValue);
  const run = noteKindEpoch.begin();
  setKindDataset(kind);
  if (!kind) {
    clearNoteKindAssets();
    return;
  }
  if (activeNoteKind === kind) {
    dispatchNoteKindReady(kind);
    return;
  }

  clearNoteKindAssets();
  activeNoteKind = kind;
  const kindRoot = window.aaronnoteApi
    ? hostAssetResolver?.(`aaronnote-asset://kinds/${encodeURIComponent(kind)}`) ?? `aaronnote-asset://kinds/${encodeURIComponent(kind)}`
    : `/kinds/${encodeURIComponent(kind)}`;

  const css = document.createElement("link");
  css.rel = "stylesheet";
  css.href = `${kindRoot}/index.css`;
  css.dataset.aaronnoteKindAsset = "style";
  css.dataset.kind = kind;
  document.head.appendChild(css);

  try {
    const mod = await import(/* @vite-ignore */ `${kindRoot}/index.js`) as NoteKindModule;
    if (!run.current || activeNoteKind !== kind) {
      const staleContext = noteKindContext(kind);
      if (typeof mod.teardown === "function") mod.teardown(staleContext);
      return;
    }
    const context = noteKindContext(kind);
    const setup = typeof mod.default === "function" ? mod.default : typeof mod.setup === "function" ? mod.setup : null;
    const cleanup = setup?.(context);
    noteKindCleanup = typeof cleanup === "function"
      ? cleanup
      : typeof mod.teardown === "function"
        ? () => mod.teardown?.(context)
        : null;
    dispatchNoteKindReady(kind);
  } catch (err) {
    if (run.current && activeNoteKind === kind) {
      console.warn(`Aaronnote kind assets unavailable for ${kind}`, err);
    }
  }
}

function findLatexTagRange(tex: string, tag: string): { from: number; to: number } | null {
  if (!tag) return null;
  const exact = `\\tag{${tag}}`;
  const exactIndex = tex.indexOf(exact);
  if (exactIndex >= 0) {
    const from = exactIndex + "\\tag{".length;
    return { from, to: from + tag.length };
  }
  const pattern = new RegExp(`\\\\tag\\s*\\{\\s*${escapeRegExp(tag)}\\s*\\}`, "g");
  const match = pattern.exec(tex);
  if (!match) return null;
  const matched = match[0] ?? "";
  const tagIndex = matched.indexOf(tag);
  if (tagIndex < 0) return null;
  const from = match.index + tagIndex;
  return { from, to: from + tag.length };
}

function jumpToEquationTag(rawTag: string): boolean {
  const tag = normalizeEquationTag(rawTag);
  if (!tag) return false;

  if (editor.isSourceMode()) {
    const range = findLatexTagRange(editor.getMarkdown(), tag);
    if (!range) return false;
    editor.setSelection(range.from, range.to);
    editor.revealCursor();
    setStatus(`Equation tag ${tag}`);
    scheduleAssistUpdate();
    return true;
  }

  const hit = getEquationTagHits(editor.view.state).find((item) => item.tag === tag) ?? null;
  if (!hit) return false;
  editor.setSelection(hit.from, hit.to);
  editor.revealCursor();
  setStatus(`Equation tag ${tag}`);
  scheduleAssistUpdate();
  return true;
}

function jumpToInlineTag(rawTag: string): boolean {
  const tag = normalizeInlineTag(rawTag);
  if (!tag) return false;
  const anchor = inlineTagAnchorsFromText(editor.getMarkdown())
    .find((item) => normalizeInlineTag(item.tag).toLowerCase() === tag.toLowerCase());
  if (!anchor) return false;
  editor.setSelection(anchor.pos, anchor.to);
  editor.revealCursor();
  setStatus(`Inline anchor ${tag}`);
  scheduleAssistUpdate({ toc: true });
  return true;
}

function jumpToTodoSource(source: string, preferredIndex?: number): boolean {
  const target = String(source || "");
  if (!target) return false;
  const contentOffset = Math.max(0, target.indexOf("[") + 1);
  if (editor.isSourceMode()) {
    const markdown = editor.getMarkdown();
    const index = typeof preferredIndex === "number" && markdown.slice(preferredIndex, preferredIndex + target.length) === target
      ? preferredIndex
      : markdown.indexOf(target);
    if (index < 0) return false;
    editor.setSelection(index + contentOffset, index + contentOffset + Math.min(1, Math.max(0, target.length - contentOffset - 1)));
    editor.revealCursor();
    return true;
  }

  let hit: { from: number; to: number } | null = null;
  const doc = editor.view.state.doc as {
    descendants?: (callback: (node: { isTextblock?: boolean; textContent: string }, pos: number) => boolean | void) => void;
  };
  if (typeof doc.descendants !== "function") {
    const markdown = editor.getMarkdown();
    const index = typeof preferredIndex === "number" && markdown.slice(preferredIndex, preferredIndex + target.length) === target
      ? preferredIndex
      : markdown.indexOf(target);
    if (index < 0) return false;
    const from = index + contentOffset;
    hit = {
      from,
      to: Math.min(markdown.length, from + Math.min(1, Math.max(0, target.length - contentOffset - 1))),
    };
  } else {
    doc.descendants((node, pos) => {
      if (hit || !node.isTextblock) return !hit;
      const text = node.textContent;
      const index = text.indexOf(target);
      if (index < 0) return true;
      const from = pos + 1 + index + contentOffset;
      hit = {
        from,
        to: Math.min(pos + 1 + text.length, from + Math.min(1, Math.max(0, target.length - contentOffset - 1))),
      };
      return false;
    });
  }
  if (!hit) return false;
  editor.setSelection(hit.from, hit.to);
  editor.revealCursor();
  scheduleAssistUpdate();
  return true;
}

function anchorTagOccurrences(content = editor.getMarkdown()): string[] {
  const tags: string[] = [];
  for (const tag of equationTagsFromText(content)) {
    const clean = normalizeInlineTag(tag);
    if (clean) tags.push(clean);
  }
  for (const anchor of inlineTagAnchorsFromText(content)) {
    const clean = normalizeInlineTag(anchor.tag);
    if (clean) tags.push(clean);
  }
  return tags;
}

function allAnchorTagSuggestions(content = editor.getMarkdown()): string[] {
  const tags = new Set<string>();
  for (const tag of anchorTagOccurrences(content)) tags.add(tag);
  return [...tags].sort((a, b) => a.localeCompare(b));
}

function existingLatexTag(tex: string): string {
  return tex.match(/\\tag\s*\{([^{}\n]+)\}/)?.[1]?.trim() || "";
}

function normalizeEquationTag(value: string): string {
  return String(value || "")
    .replace(/[\r\n{}]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function normalizeAnchorTag(value: string): string {
  return normalizeInlineTag(normalizeEquationTag(value));
}

function anchorTagDuplicateMessage(value: string, currentTag = ""): string {
  const tag = normalizeAnchorTag(value);
  if (!tag) return "";
  const current = normalizeAnchorTag(currentTag);
  const occurrences = anchorTagOccurrences()
    .filter((item) => item.toLowerCase() === tag.toLowerCase()).length;
  const duplicateCount = occurrences - (current && current.toLowerCase() === tag.toLowerCase() ? 1 : 0);
  return duplicateCount > 0
    ? `Duplicate anchor "${tag}" in this note. Use a unique tag for precise jumps.`
    : "";
}

function tagSlugSegment(value: string): string {
  return String(value || "")
    .normalize("NFKC")
    .trim()
    .replace(/[^\p{L}\p{N}]+/gu, "-")
    .replace(/^-+|-+$/g, "")
    .replace(/-+/g, "-")
    .toLowerCase();
}

function activeHeadingPath(): string[] {
  const pos = editor.getMarkdownSelection().from;
  const stack: string[] = [];
  for (const heading of markdownHeadingsFromText(editor.view.state.doc)) {
    if (heading.pos > pos) break;
    stack[heading.level - 1] = heading.text;
    stack.length = heading.level;
  }
  return stack;
}

function nextAnchorTagSuggestion(kind: "equation" | "inline"): string {
  const headingParts = activeHeadingPath().map(tagSlugSegment).filter(Boolean);
  const fallback = tagSlugSegment(currentNote()?.title || fileNameFromPath(currentFile || "note")) || "anchor";
  const core = headingParts.slice(-3).join(".") || fallback;
  const base = kind === "equation" ? `eq:${core}` : core;
  const used = new Set(allAnchorTagSuggestions().map((tag) => tag.toLowerCase()));
  if (!used.has(base.toLowerCase())) return base;
  for (let i = 2; i < 1000; i++) {
    const candidate = `${base}.${i}`;
    if (!used.has(candidate.toLowerCase())) return candidate;
  }
  return `${base}.${Date.now()}`;
}

function upsertLatexTag(tex: string, tag: string): string {
  const clean = tex
    .replace(/\s*\\tag\s*\{[^{}\n]*\}/g, "")
    .replace(/\s+$/g, "");
  const separator = clean.includes("\n") ? "\n" : " ";
  return `${clean}${separator}\\tag{${tag}}`;
}

function equationHash(tag: string): string {
  return `eq-${encodeURIComponent(tag)}`;
}

function encodeMarkdownHrefPathPart(part: string): string {
  return encodeURIComponent(part).replace(/[!'()*]/g, (char) =>
    `%${char.charCodeAt(0).toString(16).toUpperCase()}`);
}

function encodeMarkdownHrefPath(path: string): string {
  return decodeNoteRef(path)
    .replace(/\\/g, "/")
    .split("/")
    .map((part) => encodeMarkdownHrefPathPart(part))
    .join("/");
}

function noteAnchorHref(note: NoteSummary | undefined, hash: string): string {
  const externalNote = externalBookNote(note);
  const targetHref = !currentStandalone && externalNote?.roam ? roamHrefForNote(externalNote, hash) : "";
  if (targetHref) return targetHref;
  const targetPath = externalNote?.path || externalNote?.link || currentFile || externalNote?.source || fileNameFromPath(currentFile || "note.md");
  return `${encodeMarkdownHrefPath(targetPath)}#${hash}`;
}

function noteDomHref(note: NoteSummary | undefined, domTarget: string): string {
  const externalNote = externalBookNote(note);
  const clean = normalizeDomTargetPath(domTarget);
  const encoded = encodeDomTargetPath(clean);
  if (!encoded) return noteAnchorHref(externalNote, "");
  const targetHref = !currentStandalone && externalNote?.roam ? roamHrefForNote(externalNote).replace(/#.*$/, "") : "";
  if (targetHref) return `${targetHref}@${encoded}`;
  const targetPath = externalNote?.path || externalNote?.link || currentFile || externalNote?.source || fileNameFromPath(currentFile || "note.md");
  return `${encodeMarkdownHrefPath(targetPath)}@${encoded}`;
}

function equationReferenceMarkdown(tag: string): string {
  const note = currentNote();
  return `[${escapeMarkdownLinkText(tag)}](${noteAnchorHref(note, equationHash(tag))})`;
}

function inlineTagReferenceMarkdown(tag: string): string {
  const note = currentNote();
  return `[${escapeMarkdownLinkText(`#${tag}`)}](${noteAnchorHref(note, encodeURIComponent(normalizeInlineTag(tag)))})`;
}

function domReferenceMarkdown(domTarget: string): string {
  const note = currentNote();
  const clean = normalizeDomTargetPath(domTarget);
  return `[${escapeMarkdownLinkText(`@${clean}`)}](${noteDomHref(note, clean)})`;
}

async function copyText(text: string): Promise<void> {
  try {
    await navigator.clipboard.writeText(text);
  } catch {
    const fallback = document.createElement("textarea");
    fallback.value = text;
    fallback.style.position = "fixed";
    fallback.style.left = "-9999px";
    document.body.appendChild(fallback);
    fallback.select();
    document.execCommand("copy");
    fallback.remove();
  }
}

function nextEquationTagSuggestion(): string {
  return nextAnchorTagSuggestion("equation");
}

async function copyEquationRef(tag: string): Promise<boolean> {
  const clean = normalizeEquationTag(tag);
  if (!clean) return false;
  await copyText(equationReferenceMarkdown(clean));
  setStatus(`Equation ref copied: ${clean}`);
  return true;
}

async function tagActiveEquation(): Promise<boolean> {
  const target = activeDisplayMathTarget();
  if (!target) return false;
  const current = existingLatexTag(target.tex);
  if (current) return copyEquationRef(current);
  const result = await openFormModal("Equation tag", [
    {
      id: "tag",
      label: "LaTeX tag",
      value: current || nextEquationTagSuggestion(),
      suggestions: allAnchorTagSuggestions(),
      validate: (value) => anchorTagDuplicateMessage(value, current),
    },
  ], "Tag & Copy Ref");
  if (!result) return true;
  const tag = normalizeEquationTag(result.tag);
  if (!tag) return true;
  const nextTex = upsertLatexTag(target.tex, tag);
  target.replace(nextTex, tag);
  const ref = equationReferenceMarkdown(tag);
  await copyText(ref);
  setStatus(`Equation tag ${tag}; ref copied`);
  scheduleAssistUpdate();
  return true;
}

async function openTagManager(): Promise<void> {
  if (!currentFile) {
    setStatus("Open a note before managing tags");
    return;
  }
  if (currentStandalone) {
    setStatus("Roam tag manager is unavailable for standalone Markdown files");
    return;
  }
  const note = currentNote();
  const result = await openFormModal("Note tags", [
    {
      id: "tags",
      label: "Note tags",
      type: "tags",
      value: (note?.tags ?? []).join(", "),
      suggestions: tagSuggestions(),
      refreshSuggestions: refreshRoamTagSuggestions,
    },
  ], "Update Tags");
  if (!result) return;
  await updateNoteMeta(api.meta.add, {
    title: note?.title || fileLabel.textContent || "Untitled",
    tags: parseTagPrompt(result.tags),
    kind: note?.kind || "default",
  }, "Tags updated");
}

function normalizeInlineTag(value: string): string {
  return String(value || "")
    .replace(/[\r\n\[\]]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function normalizeDomTarget(value: string): string {
  return decodeNoteRef(String(value || ""))
    .replace(/^@/, "")
    .replace(/[\r\n\[\]]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function domTargetPathSegments(value: string): string[] {
  return String(value || "")
    .trim()
    .replace(/^@+/, "")
    .split("@")
    .map((segment) => normalizeDomTarget(segment))
    .filter(Boolean);
}

function slugDomTarget(value: string): string {
  return normalizeDomTarget(value)
    .toLowerCase()
    .replace(/[`*_~()[\]{}#+.!<>:;,'"“”‘’@]/g, " ")
    .trim()
    .replace(/\s+/g, "-");
}

function normalizeDomTargetPath(value: string): string {
  return domTargetPathSegments(value)
    .map(slugDomTarget)
    .filter(Boolean)
    .join("@");
}

function encodeDomTargetPath(value: string): string {
  return domTargetPathSegments(value)
    .map(slugDomTarget)
    .filter(Boolean)
    .map(encodeURIComponent)
    .join("@");
}

function domTargetPathLabel(path: readonly string[]): string {
  return path.filter(Boolean).join(" / ");
}

function targetSegmentMatches(actual: string, wanted: string): boolean {
  const actualNorm = normalizeDomTarget(actual).toLowerCase();
  const wantedNorm = normalizeDomTarget(wanted).toLowerCase();
  if (actualNorm && actualNorm === wantedNorm) return true;
  const actualSlug = slugDomTarget(actual);
  const wantedSlug = slugDomTarget(wanted);
  return Boolean(actualSlug && wantedSlug && actualSlug === wantedSlug);
}

function targetPathMatches(actualPath: readonly string[], wantedPath: readonly string[], allowSuffix = true): boolean {
  if (wantedPath.length === 0 || actualPath.length === 0) return false;
  const pathMatchesAt = (offset: number) => wantedPath.every((segment, index) =>
    targetSegmentMatches(actualPath[offset + index] || "", segment));
  if (actualPath.length === wantedPath.length && pathMatchesAt(0)) return true;
  if (!allowSuffix || actualPath.length < wantedPath.length) return false;
  return pathMatchesAt(actualPath.length - wantedPath.length);
}

function findDomTargetEntry(entries: readonly DomTargetEntry[], rawTarget: string): DomTargetEntry | undefined {
  const targetPath = domTargetPathSegments(rawTarget);
  if (targetPath.length === 0) return undefined;
  if (targetPath.length > 1) {
    return entries.find((entry) => targetPathMatches(entry.path, targetPath, false))
      ?? entries.find((entry) => targetPathMatches(entry.path, targetPath, true));
  }
  const target = targetPath[0] || "";
  const targetSlug = slugDomTarget(target);
  const targetNorm = normalizeDomTarget(target).toLowerCase();
  return entries.find((entry) => {
    const label = normalizeDomTarget(entry.label).toLowerCase();
    return label === targetNorm || entry.slug === targetSlug || entry.slug === targetNorm;
  });
}

function currentDomTargets(): DomTargetEntry[] {
  const doc = editor.view.state.doc;
  const stack: string[] = [];
  const labelStack: string[] = [];
  const headings = markdownHeadingsFromText(doc).map((heading) => {
    const level = Math.max(1, Number(heading.level || 1));
    const label = normalizeDomTarget(heading.text);
    const slug = heading.slug || slugDomTarget(label);
    stack.length = Math.min(stack.length, level - 1);
    labelStack.length = Math.min(labelStack.length, level - 1);
    stack.push(slug);
    labelStack.push(label);
    return {
      label,
      slug,
      path: [...stack],
      labelPath: [...labelStack],
      level,
      pos: heading.pos,
      to: heading.to ?? heading.pos + heading.text.length,
    };
  });
  const note = currentNote();
  const title = note?.title || "";
  if (!title) return headings;
  const titleLabel = normalizeDomTarget(title);
  const titleSlug = slugDomTarget(titleLabel);
  return [{
    label: titleLabel,
    slug: titleSlug,
    path: [titleSlug],
    labelPath: [titleLabel],
    level: 1,
    pos: 0,
    to: Math.min(editor.getMarkdown().length, title.length),
  }, ...headings];
}

function domTargetAtCursor(): string {
  const selection = editor.getMarkdownSelection();
  const pos = selection.from;
  const targets = currentDomTargets();
  const target = targets.find((item) => pos >= (item.pos ?? 0) && pos <= (item.to ?? item.pos ?? 0))
    ?? targets.find((item) => selection.from < (item.to ?? item.pos ?? 0) && selection.to > (item.pos ?? 0));
  return target ? target.path.join("@") : "";
}

function jumpToDomTarget(rawTarget: string): boolean {
  const target = normalizeDomTargetPath(rawTarget);
  if (!target) return false;
  const hit = findDomTargetEntry(currentDomTargets(), target);
  if (!hit) return false;
  editor.setSelection(hit.pos ?? 0, hit.to ?? hit.pos ?? 0);
  editor.revealCursor();
  setStatus(`DOM target ${target}`);
  scheduleAssistUpdate({ toc: true });
  return true;
}

function inlineTagMarkdown(tag: string): string {
  return `@@tag[${tag}]`;
}

function inlineTagAtCursor(): string {
  const selection = editor.getMarkdownSelection();
  const from = Math.min(selection.from, selection.to);
  const to = Math.max(selection.from, selection.to);
  return inlineTagAnchorsFromText(editor.getMarkdown())
    .find((anchor) => {
      if (from === to) return from >= anchor.pos && from <= anchor.to;
      return from < anchor.to && to > anchor.pos;
    })?.tag ?? "";
}

async function copyInlineTagRef(tag: string): Promise<boolean> {
  const clean = normalizeInlineTag(tag);
  if (!clean) return false;
  await copyText(inlineTagReferenceMarkdown(clean));
  setStatus(`Inline anchor ref copied: ${clean}`);
  return true;
}

async function copyDomRef(domTarget: string): Promise<boolean> {
  const clean = normalizeDomTargetPath(domTarget);
  if (!clean) return false;
  await copyText(domReferenceMarkdown(clean));
  setStatus(`DOM ref copied: ${clean}`);
  return true;
}

async function insertInlineTag(): Promise<void> {
  const result = await openFormModal("Inline anchor", [
    {
      id: "tag",
      label: "Anchor tag",
      value: nextAnchorTagSuggestion("inline"),
      suggestions: allAnchorTagSuggestions(),
      validate: (value) => anchorTagDuplicateMessage(value),
    },
  ], "Tag & Copy Ref");
  if (!result) return;
  const tag = normalizeInlineTag(result.tag);
  if (!tag) return;
  const markdown = editor.getMarkdown();
  const selection = editor.getMarkdownSelection();
  const insertAt = selection.to;
  const before = markdown[insertAt - 1] ?? "";
  const after = markdown[insertAt] ?? "";
  const prefix = before && !/\s/.test(before) ? " " : "";
  const suffix = after && !/\s/.test(after) ? " " : "";
  editor.replaceMarkdownRange(insertAt, insertAt, `${prefix}${inlineTagMarkdown(tag)}${suffix}`, "end");
  const ref = inlineTagReferenceMarkdown(tag);
  await copyText(ref);
  setStatus(`Inline anchor ${tag}; ref copied`);
  scheduleAssistUpdate({ snippets: true, toc: true });
  scheduleCursorPositionSave();
}

async function handleTagCommand(): Promise<void> {
  if (await tagActiveEquation()) return;
  if (await copyInlineTagRef(inlineTagAtCursor())) return;
  if (await copyDomRef(domTargetAtCursor())) return;
  await insertInlineTag();
}

function toggleSourceMode(): void {
  saveCursorPositionNow({ force: true });
  editor.toggleSource();
  vim.setMode("insert");
  syncSourceUi();
  setStatus(currentMode === "source" ? "Source mode" : "Ready");
  scheduleAssistUpdate();
  scheduleCursorPositionSave(80);
}

function cleanupTransientUi(): void {
  hideSnippetPopup();
  hideMathPreview();
  selectionTool.hidden = true;
  assistTimer.cancel();
  window.cancelAnimationFrame(assistFrame);
}

function disposeGraph(): void {
  graphPanel.dispose();
}

function setTopNavState(view: "editor" | "notes" | "agenda"): void {
  notesButton.classList.toggle("is-active", view === "notes");
  agendaButton.classList.toggle("is-active", view === "agenda");
  editorButton.classList.toggle("is-active", view === "editor");
  if (view !== "editor") relationButton.classList.remove("is-active");
  for (const [button, active] of [
    [notesButton, view === "notes"],
    [relationButton, !relationPanel.hidden],
    [agendaButton, view === "agenda"],
    [editorButton, view === "editor"],
  ] as Array<[HTMLButtonElement, boolean]>) {
    button.setAttribute("aria-pressed", active ? "true" : "false");
  }
}

function showNotesPage(tab = "filesystem"): void {
  const targetTab = standaloneHiddenNotesTool(tab) ? "filesystem" : tab;
  editorCursorBeforePanel = currentCursorPosition();
  saveCursorPositionNow({ force: true });
  cleanupTransientUi();
  closeRelationPanel();
  if (targetTab === "filesystem" || targetTab === "recent") {
    leanPanel.hide();
    jupyterPanel.hide();
  }
  linkPreview.hide();
  disposeGraph();
  host.hidden = true;
  notesPage.hidden = false;
  toc.hidden = true;
  bookToc.hidden = true;
  bookTocToggle.hidden = true;
  document.body.classList.remove("book-toc-open");
  localGraph.hidden = true;
  localGraphPanel.collapse();
  notesButton.hidden = true;
  relationButton.hidden = true;
  agendaButton.hidden = true;
  sourceButton.hidden = true;
  editorButton.hidden = false;
  setTopNavState(targetTab === "agenda" ? "agenda" : "notes");
  void refreshNotesIndex();
  showNotesTool(targetTab);
  if (targetTab === "filesystem") focusFilesystemRangerSoon();
  else if (targetTab === "recent") focusRecentListSoon();
}

function openTagFilter(tag: string): void {
  const clean = String(tag || "").trim().replace(/^#/, "");
  if (!clean) return;
  showNotesPage("graph");
  graphFilter.value = `tag:${clean}`;
  renderGraph();
  setStatus(`#${clean}`);
}

function openFilesystemPage(): void {
  showNotesPage("filesystem");
}

function showNotesTool(tab: string): void {
  if (standaloneHiddenNotesTool(tab)) {
    tab = "filesystem";
  }
  if (tab !== "graph") disposeGraph();
  if (tab !== "git") deactivateGitPanel();
  notesTabButtonElements().forEach((button) => {
    button.classList.toggle("is-active", button.dataset.notesTab === tab);
  });
  notesPanelElements().forEach((panel) => {
    panel.hidden = panel.dataset.notesPanel !== tab;
  });
  if (tab === "graph") {
    renderGraph();
    graphFilter.focus();
  } else if (tab === "agenda") {
    void loadAgendaTodos();
    agendaFilter.focus();
  } else if (tab === "recent") {
    renderRecentNotes();
    focusRecentListSoon();
  } else if (tab === "git") {
    activateGitPanel();
  } else if (tab === "lean") {
    void refreshLeanProjectInfo();
  } else if (tab === "filesystem") {
    expandFilesystemGroups();
    focusFilesystemRangerSoon();
  }
}

function showEditorPage(): void {
  disposeGraph();
  deactivateGitPanel();
  notesPage.hidden = true;
  host.hidden = false;
  toc.hidden = false;
  updateBookToc();
  notesButton.hidden = false;
  relationButton.hidden = false;
  agendaButton.hidden = false;
  sourceButton.hidden = false;
  syncSourceUi();
  editorButton.hidden = true;
  syncEditorRoamLinkStatus();
  updateJumpStackUi();
  syncLocalGraphAvailability();
  setTopNavState("editor");
  editor.focus();
  const restore = editorCursorBeforePanel;
  editorCursorBeforePanel = null;
  if (restore?.file === currentFile) {
    const max = editor.view.state.doc.length;
    const from = Math.max(0, Math.min(restore.from, max));
    const to = Math.max(0, Math.min(restore.to, max));
    editor.setSelection(from, to);
    host.scrollTop = restore.scrollY;
  }
  scheduleAssistUpdate();
}

function updateFloatingToc(): void {
  if (host.hidden) {
    syncEditorBookContext(null);
    bookToc.hidden = true;
    return;
  }
  if (!toc.hidden) floatingTocPanel.update();
  updateBookToc();
  leanPanel.refresh();
}

let bookTocRenderKey = "";
const expandedBookTocKeys = new Set<string>();

function bookTocNodeKey(item: BookTocItem | BookEditorTocItem, index: number): string {
  return [
    item.path || "",
    item.slug || "",
    item.text || "",
    String(index),
  ].join("\t");
}

function buildBookTocTree(items: Array<BookTocItem | BookEditorTocItem>): BookTocNode[] {
  const roots: BookTocNode[] = [];
  const stack: BookTocNode[] = [];
  items.forEach((item, index) => {
    const level = Math.max(1, Number(item.level || 1));
    const node: BookTocNode = { item, key: bookTocNodeKey(item, index), level, children: [] };
    while (stack.length > 0 && stack[stack.length - 1].level >= level) stack.pop();
    const parent = stack[stack.length - 1];
    if (parent) parent.children.push(node);
    else roots.push(node);
    stack.push(node);
  });
  return roots;
}

function renderBookTocNode(
  frag: DocumentFragment,
  node: BookTocNode,
  context: BookEditorContext,
  currentPath: string,
  activeSlug: string,
): void {
  const item = node.item;
  const row = document.createElement("div");
  row.className = "aaronnote-book-toc-row";
  row.style.setProperty("--book-depth", String(Math.max(0, node.level - 1)));
  const expanded = expandedBookTocKeys.has(node.key);
  const hasChildren = node.children.length > 0;

  const toggle = document.createElement("button");
  toggle.type = "button";
  toggle.className = hasChildren ? "aaronnote-book-toc-branch" : "aaronnote-book-toc-spacer";
  toggle.setAttribute("aria-label", expanded ? "Collapse section" : "Expand section");
  toggle.setAttribute("aria-expanded", expanded ? "true" : "false");
  toggle.textContent = hasChildren ? (expanded ? "▾" : "▸") : "";
  toggle.disabled = !hasChildren;
  toggle.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    if (!hasChildren) return;
    if (expandedBookTocKeys.has(node.key)) expandedBookTocKeys.delete(node.key);
    else expandedBookTocKeys.add(node.key);
    bookTocRenderKey = "";
    renderBookTocPanel(context);
  });
  row.appendChild(toggle);

  const button = document.createElement("button");
  button.type = "button";
  button.className = "aaronnote-book-toc-item";
  button.dataset.path = item.path || "";
  button.dataset.slug = item.slug || "";
  button.textContent = item.text || item.path || "Untitled";
  button.title = [item.text || "", item.path || ""].filter(Boolean).join(" · ");
  const sameFile = bookPathKey(item.path) === currentPath;
  if (sameFile) button.classList.add("is-current-file");
  if (sameFile && activeSlug && (item.slug === activeSlug || slugDomTarget(item.text || "") === activeSlug)) {
    button.classList.add("is-active");
    button.setAttribute("aria-current", "location");
  }
  button.addEventListener("click", (event) => {
    event.preventDefault();
    openBookTocItem(item, { newWindow: event.altKey || event.metaKey });
  });
  button.addEventListener("auxclick", (event) => {
    if (event.button !== 1) return;
    event.preventDefault();
    openBookTocItem(item, { newWindow: true });
  });
  row.appendChild(button);
  frag.appendChild(row);

  if (hasChildren && expanded) {
    for (const child of node.children) renderBookTocNode(frag, child, context, currentPath, activeSlug);
  }
}

function renderBookTocPanel(context: BookEditorContext | null): void {
  syncEditorBookContext(context);
  const items = context?.toc || [];
  const visible = Boolean(context && items.length > 0 && !host.hidden);
  bookToc.hidden = !visible;
  bookTocToggle.hidden = !visible;
  document.body.classList.toggle("book-toc-open", visible && !bookToc.classList.contains("is-collapsed"));
  if (!visible) {
    bookTocRenderKey = "";
    bookTocList.replaceChildren();
    return;
  }

  const currentPath = bookPathKey(context?.currentPath || "");
  const activeSlug = activeBookHeadingSlug();
  const key = `${bookContextKey(context)}\n${activeSlug}\n${bookToc.classList.contains("is-collapsed")}`;
  if (key === bookTocRenderKey) return;
  bookTocRenderKey = key;

  bookTocToggle.textContent = "Book";
  bookTocToggle.title = `${context?.title || "Book"} · ${items.length} headings`;
  const frag = document.createDocumentFragment();
  const status = document.createElement("div");
  status.className = "aaronnote-book-toc-status";
  status.textContent = [
    context?.title || "Book",
    `${items.length} headings`,
    context?.includedCount ? `${context.includedCount} files` : "",
  ].filter(Boolean).join(" · ");
  frag.appendChild(status);

  for (const node of buildBookTocTree(items)) renderBookTocNode(frag, node, context, currentPath, activeSlug);
  bookTocList.replaceChildren(frag);
}

function updateBookToc(): void {
  renderBookTocPanel(currentBookContext());
}

function openNote(note: NoteSummary, options: OpenNoteOptions = {}): void {
  if (!note.file) return;
  const equationTag = normalizeEquationTag(options.equationTag || "");
  const inlineTag = normalizeInlineTag(options.inlineTag || "");
  const domTarget = normalizeDomTargetPath(options.domTarget || "");
  saveCursorPositionNow({ force: true });
  if (options.recordJump) pushJumpPoint();
  else setJumpStack([]);
  touchRecentNote(note.file);
  if (options.newWindow) {
    window.open(noteWindowUrl(note, equationTag, inlineTag, domTarget), "_blank", "noopener,noreferrer");
    setStatus("Opening note window");
    return;
  }
  if (equationTag && note.file === currentFile) {
    showEditorPage();
    if (!jumpToEquationTag(equationTag)) setStatus(`Equation tag not found: ${equationTag}`);
    return;
  }
  if (inlineTag && note.file === currentFile) {
    showEditorPage();
    if (!jumpToInlineTag(inlineTag)) setStatus(`Inline anchor not found: ${inlineTag}`);
    return;
  }
  if (domTarget && note.file === currentFile) {
    showEditorPage();
    if (!jumpToDomTarget(domTarget)) setStatus(`DOM target not found: ${domTarget}`);
    return;
  }
  pendingEquationTag = equationTag;
  pendingInlineTag = inlineTag;
  pendingDomTarget = domTarget;
  pendingOpenAtTop = options.scrollTop === true;
  void openStandaloneFile(note.file);
  showEditorPage();
}

function insertSnippet(snippet: SnippetSummary, deleteBefore = 0): void {
  if (!snippetSession.insert(snippet, deleteBefore)) return;
  setStatus(`Inserted ${snippet.key || snippet.name || "snippet"}`);
  scheduleAssistUpdate({ snippets: true });
}

function jumpSnippetTabstop(): boolean {
  const moved = snippetSession.next();
  if (moved) setStatus("Snippet field");
  return moved;
}

function jumpSnippetTabstopBack(): boolean {
  const moved = snippetSession.previous();
  if (moved) setStatus("Snippet field");
  return moved;
}

const forwardDelimiterChars = ")]}>】］」〕｝〗』";
const backwardDelimiterChars = "([{<【［「〔｛〖『";

function moveMarkdownDelimiter(dir: 1 | -1): boolean {
  const markdown = editor.getMarkdown();
  const selection = editor.getMarkdownSelection();
  if (dir > 0) {
    const start = Math.max(selection.to, 0);
    for (let i = start; i < markdown.length; i++) {
      if (!forwardDelimiterChars.includes(markdown[i] ?? "")) continue;
      editor.setMarkdownSelection(i + 1);
      editor.revealCursor();
      return true;
    }
    return false;
  }
  const start = Math.min(selection.from - 1, markdown.length - 1);
  for (let i = start; i >= 0; i--) {
    if (!backwardDelimiterChars.includes(markdown[i] ?? "")) continue;
    editor.setMarkdownSelection(i);
    editor.revealCursor();
    return true;
  }
  return false;
}

function readStringSet(storageKey: string): Set<string> {
  try {
    const raw = JSON.parse(window.localStorage.getItem(storageKey) || "[]");
    return new Set(Array.isArray(raw) ? raw.map(String) : []);
  } catch {
    return new Set();
  }
}

function saveStringSet(storageKey: string, values: Set<string>): void {
  window.localStorage.setItem(storageKey, JSON.stringify([...values].sort()));
}

function currentSnippetKind(): string {
  return activeKindName(root.dataset.noteKind || document.body.dataset.noteKind || currentNote()?.kind || "");
}

function matchingSnippets(prefix: string, mode: string): SnippetSummary[] {
  return matchingSnippetsForPrefix(snippets, prefix, { kind: currentSnippetKind(), mode, limit: 10 });
}

function pathCompletionPrefix(before: string): string {
  const match = before.match(/(?:^|[\s([{"'=])((?:\.{1,2}\/)[^\s\])}"'`<>]*)$/);
  return match?.[1] ?? "";
}

function roamCompletionPrefix(before: string): string | null {
  const match = before.match(/(?:^|[\s([{"'=])roam:\/\/([^\s\])}"'`<>]*)$/i);
  return match ? match[1] ?? "" : null;
}

function wikilinkCompletionPrefix(before: string): string | null {
  const match = before.match(/(?:^|[\s([{"'=])\[\[([^\]\n]*)$/);
  return match ? match[1] ?? "" : null;
}

function completionPreviewText(snippet: SnippetSummary): string {
  if (snippet.group === "path") return "";
  if (snippet.group === "wikilink") return snippet.source || "";
  if (snippet.group === "roam") return snippet.source || "";
  if (snippet.group === "tag") return snippet.source || snippet.body || "";
  if (snippet.group === "dom") return snippet.source || snippet.body || "";
  return "";
}

function completionDetail(snippet: SnippetSummary): string {
  if (snippet.group === "path") return snippet.source || "";
  if (snippet.group === "wikilink") return snippet.source ? `[[${snippet.source}]]` : "wikilink";
  if (snippet.group === "roam") return snippet.body ? `roam -> ${snippet.body}` : "roam";
  if (snippet.group === "tag") return snippet.source ? `inline tag in ${snippet.source}` : "inline tag";
  if (snippet.group === "dom") return snippet.source ? `DOM target in ${snippet.source}` : "DOM target";
  return snippetDetail(snippet);
}

function displayPathCompletion(path: string, prefix: string): string {
  if (prefix.startsWith("./") && !path.startsWith("./") && !path.startsWith("../") && !path.startsWith("/")) return `./${path}`;
  return path;
}

function pathCompletionMatches(path: string, prefix: string): boolean {
  const query = prefix.toLowerCase();
  const display = displayPathCompletion(path, prefix).toLowerCase();
  if (display.startsWith(query)) return true;
  if (query.startsWith("./")) return path.toLowerCase().startsWith(query.slice(2));
  return false;
}

function isPureTraversalPath(path: string): boolean {
  const parts = path
    .replace(/\\/g, "/")
    .split("/")
    .map((part) => part.trim())
    .filter(Boolean);
  return parts.length > 0 && parts.every((part) => part === "." || part === "..");
}

function pathCompletionRank(path: string, prefix: string): number {
  const display = displayPathCompletion(path, prefix);
  const sameDir = display.startsWith("./") ? 0 : 100;
  const parentPenalty = (display.match(/\.\.\//g) ?? []).length * 25;
  const dirPenalty = display.split("/").length;
  const directoryBoost = display.endsWith("/") ? -2 : 0;
  const exactPrefixBoost = display.toLowerCase().startsWith(prefix.toLowerCase()) ? -4 : 0;
  return sameDir + parentPenalty + dirPenalty + directoryBoost + exactPrefixBoost;
}

function noteFromCompletionRef(ref: string): NoteSummary | undefined {
  return resolveInternalNoteHref(ref) || resolveNoteRef(ref);
}

function tagCompletionContext(before: string): { note: NoteSummary; tagPrefix: string } | null {
  const roamMatch = before.match(/(?:^|[\s([{"'=])roam:\/\/([^\s\])}"'`<>#]*)#([^\s\])}"'`<>]*)$/i);
  if (roamMatch) {
    const note = noteFromCompletionRef(roamMatch[1] ?? "");
    if (note) return { note, tagPrefix: roamMatch[2] ?? "" };
  }
  const pathMatch = before.match(/(?:^|[\s([{"'=])((?:\.{1,2}\/|\.|[^\s\])}"'`<>#@]+)[^\s\])}"'`<>#@]*)#([^\s\])}"'`<>]*)$/);
  if (pathMatch) {
    const note = noteFromCompletionRef(pathMatch[1] ?? "");
    if (note) return { note, tagPrefix: pathMatch[2] ?? "" };
  }
  return null;
}

function domCompletionParts(rawHref: string): { ref: string; parentSegments: string[]; domPrefix: string } | null {
  const clean = cleanHref(rawHref);
  if (!clean || clean.includes("#")) return null;
  const roamTarget = splitRoamLikeHref(clean);
  if (roamTarget?.dom) {
    const endsAtSeparator = /@$/.test(clean);
    const segments = domTargetPathSegments(roamTarget.dom);
    return {
      ref: roamTarget.ref,
      parentSegments: endsAtSeparator ? segments : segments.slice(0, -1),
      domPrefix: endsAtSeparator ? "" : segments[segments.length - 1] || "",
    };
  }
  const fileDomMatch = clean.match(/^(.+?\.(?:md|markdown|typ))@(.+)$/i);
  const plainDomMatch = fileDomMatch ? null : clean.match(/^(.+?)@([^@]*)$/);
  const match = fileDomMatch || plainDomMatch;
  if (!match) return null;
  const endsAtSeparator = /@$/.test(clean);
  const segments = domTargetPathSegments(match[2] || "");
  return {
    ref: match[1] || "",
    parentSegments: endsAtSeparator ? segments : segments.slice(0, -1),
    domPrefix: endsAtSeparator ? "" : segments[segments.length - 1] || "",
  };
}

function domCompletionContext(before: string): { note: NoteSummary; domPrefix: string; parentSegments: string[] } | null {
  const match = before.match(/(?:^|[\s([{"'=])((?:roam:\/\/|\.{1,2}\/|\.|[^\s()[\]{}"'`<>#]+)[^\s()[\]{}"'`<>#]*)$/i);
  const parts = match ? domCompletionParts(match[1] ?? "") : null;
  if (!parts) return null;
  const note = noteFromCompletionRef(parts.ref);
  if (!note) return null;
  return { note, domPrefix: parts.domPrefix, parentSegments: parts.parentSegments };
}

function noteInlineTagsForCompletion(note: NoteSummary): string[] {
  return [...new Set((note.inlineTags ?? [])
    .map((tag) => String(tag || "").trim().replace(/^#/, ""))
    .filter(Boolean))]
    .sort((a, b) => a.localeCompare(b));
}

function matchingTagCompletions(note: NoteSummary, prefix: string): SnippetSummary[] {
  note = externalBookNote(note) || note;
  const query = prefix.toLowerCase().replace(/^tag-/, "");
  return noteInlineTagsForCompletion(note)
    .filter((tag) => tag.toLowerCase().includes(query))
    .slice(0, 12)
    .map((tag) => ({
      key: tag,
      name: `#${tag}`,
      mode: "markdown-mode",
      group: "tag",
      body: encodeURIComponent(tag),
      source: note.path || note.file || canonicalRoamNoteId(note),
    }));
}

function domTargetsForCompletion(note: NoteSummary): DomTargetEntry[] {
  const externalNote = externalBookNote(note) || note;
  if (externalNote.file === currentFile) {
    const seen = new Set<string>();
    return currentDomTargets().filter((target) => {
      const key = target.path.join("@");
      if (!target.label || !target.slug || !key || seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }
  const bookTargets = bookTocDomTargetEntries(externalNote);
  if (bookTargets.length > 0) return bookTargets;
  const indexedTargets = (externalNote.domTargets ?? [])
    .map((target) => {
      const label = normalizeDomTarget(target.label || target.slug || "");
      const slug = slugDomTarget(target.slug || label);
      const path = (Array.isArray(target.path) && target.path.length > 0 ? target.path : [slug])
        .map((segment) => slugDomTarget(segment))
        .filter(Boolean);
      const labelPath = (Array.isArray(target.labelPath) && target.labelPath.length > 0 ? target.labelPath : [label])
        .map(normalizeDomTarget)
        .filter(Boolean);
      return {
        label,
        slug,
        path,
        labelPath,
        level: Math.max(1, Number(target.level || 1)),
        notePath: target.notePath || externalNote.path || "",
      };
    })
    .filter((target) => target.label && target.slug && target.path.length > 0);
  if (indexedTargets.length > 0) return indexedTargets;
  const legacyBookTargets = externalNote.bookDomTargets ?? [];
  if (legacyBookTargets.length > 0) {
    const seen = new Set<string>();
    return legacyBookTargets
      .map((target) => ({
        label: normalizeDomTarget(target.label || target.slug || ""),
        slug: slugDomTarget(target.slug || target.label || ""),
        path: [slugDomTarget(target.slug || target.label || "")],
        labelPath: [normalizeDomTarget(target.label || target.slug || "")],
        level: target.level || 1,
        notePath: target.path || "",
      }))
      .filter((target) => {
        if (!target.label || !target.slug || seen.has(target.slug)) return false;
        seen.add(target.slug);
        return true;
      });
  }
  const rawTargets = [externalNote.title || externalNote.path || externalNote.file || canonicalRoamNoteId(externalNote)].filter(Boolean);
  const seen = new Set<string>();
  const targets: Array<{ label: string; slug: string }> = [];
  for (const label of rawTargets) {
    const clean = normalizeDomTarget(label);
    const slug = slugDomTarget(clean);
    if (!clean || !slug || seen.has(slug)) continue;
    seen.add(slug);
    targets.push({ label: clean, slug });
  }
  return targets.map((target) => ({
    ...target,
    path: [target.slug],
    labelPath: [target.label],
    level: 1,
  }));
}

function immediateDomCompletionTargets(entries: readonly DomTargetEntry[], parentSegments: readonly string[]): DomTargetEntry[] {
  const parentPath = parentSegments.map(slugDomTarget).filter(Boolean);
  const parentLength = parentPath.length;
  return entries.filter((entry) => {
    if (entry.path.length !== parentLength + 1) return false;
    if (parentLength === 0) return true;
    return targetPathMatches(entry.path.slice(0, parentLength), parentPath, false);
  });
}

// Every heading nested under `parentSegments` at any depth (not just immediate
// children). Used for by-name matching so a leaf like `@plan` or `@sad` resolves
// regardless of how deep it sits in the outline.
function descendantDomCompletionTargets(entries: readonly DomTargetEntry[], parentSegments: readonly string[]): DomTargetEntry[] {
  const parentPath = parentSegments.map(slugDomTarget).filter(Boolean);
  const parentLength = parentPath.length;
  return entries.filter((entry) => {
    if (entry.path.length <= parentLength) return false;
    if (parentLength === 0) return true;
    return targetPathMatches(entry.path.slice(0, parentLength), parentPath, false);
  });
}

function matchingDomCompletions(note: NoteSummary, prefix: string, parentSegments: readonly string[] = []): SnippetSummary[] {
  note = externalBookNote(note) || note;
  const query = normalizeDomTarget(prefix).toLowerCase();
  const entries = domTargetsForCompletion(note);
  // Empty prefix → drill one level (show the current parent's direct children).
  // Typed name → precise match by leaf name anywhere below the current parent.
  const candidates = query
    ? descendantDomCompletionTargets(entries, parentSegments)
        .filter((target) => target.slug.includes(query) || target.label.toLowerCase().includes(query))
    : immediateDomCompletionTargets(entries, parentSegments);
  return candidates
    .slice(0, 12)
    .map((target) => ({
      key: target.slug,
      name: `@${target.slug}`,
      mode: "markdown-mode",
      group: "dom",
      body: encodeURIComponent(target.slug),
      source: domTargetPathLabel(target.labelPath) || note.path || note.file || canonicalRoamNoteId(note) || target.label,
    }));
}

function matchingRoamCompletions(prefix: string): SnippetSummary[] {
  const query = prefix.toLowerCase();
  return notes
    .filter((note) => note.roam && canonicalRoamNoteId(note))
    .map((note) => {
      const id = canonicalRoamNoteId(note);
      const haystack = [
        id,
        note.title,
        note.path,
        note.link,
        note.source,
        ...(note.aliases ?? []),
        ...(note.tags ?? []),
      ].join(" ").toLowerCase();
      const score = id.toLowerCase().startsWith(query)
        ? 0
        : haystack.includes(query)
          ? 1
          : Number.POSITIVE_INFINITY;
      return { note, id, score };
    })
    .filter((item) => Number.isFinite(item.score))
    .sort((a, b) => a.score - b.score || String(a.note.title || a.id).localeCompare(String(b.note.title || b.id)))
    .slice(0, 12)
    .map(({ note, id }) => {
      const filename = fileNameFromPath(note.path || note.file || note.title || id);
      return {
        key: filename,
        name: note.title && note.title !== filename ? note.title : id,
        mode: "markdown-mode",
        group: "roam",
        body: encodeURIComponent(id),
        source: `${filename} <-> ${id}`,
      };
    });
}

function matchingWikilinkCompletions(prefix: string): SnippetSummary[] {
  const query = prefix.toLowerCase();
  return notes
    .filter((note) => note.roam && (note.title || note.path || note.id || note.key))
    .map((note) => {
      const title = note.title || fileNameFromPath(note.path || note.file || note.id || note.key || "");
      const haystack = [
        title,
        note.id,
        note.key,
        note.path,
        note.link,
        note.source,
        ...(note.aliases ?? []),
        ...(note.tags ?? []),
      ].join(" ").toLowerCase();
      const score = title.toLowerCase().startsWith(query)
        ? 0
        : haystack.includes(query)
          ? 1
          : Number.POSITIVE_INFINITY;
      return { note, title, score };
    })
    .filter((item) => Number.isFinite(item.score) && item.title)
    .sort((a, b) => a.score - b.score || a.title.localeCompare(b.title))
    .slice(0, 12)
    .map(({ note, title }) => ({
      key: title,
      name: title,
      mode: "markdown-mode",
      group: "wikilink",
      body: `${title}]]`,
      source: note.path || note.file || canonicalRoamNoteId(note),
    }));
}

function matchingPathCompletions(prefix: string): SnippetSummary[] {
  if (!prefix) return [];
  return pathSuggestions
    .filter((path) => pathCompletionMatches(path, prefix))
    .filter((path) => !isPureTraversalPath(displayPathCompletion(path, prefix)))
    .sort((a, b) => {
      const rank = pathCompletionRank(a, prefix) - pathCompletionRank(b, prefix);
      return rank || displayPathCompletion(a, prefix).localeCompare(displayPathCompletion(b, prefix));
    })
    .slice(0, 8)
    .map((path) => {
      const displayPath = displayPathCompletion(path, prefix);
      const note = externalBookNote(resolveInternalNoteHref(displayPath));
      const roamId = note?.roam ? canonicalRoamNoteId(note) : "";
      return {
        key: displayPath,
        name: displayPath,
        mode: "markdown-mode",
        group: "path",
        body: roamId ? roamHrefForNote(note) : displayPath,
        source: note?.title && note.title !== displayPath ? note.title : "",
      };
    });
}

function hideSnippetPopup(): void {
  snippetPopup.hidden = true;
  snippetPopupItems = [];
  snippetRenderKey = "";
}

function hideQuickInsertPopup(): void {
  quickInsertPopup.hidden = true;
  quickInsertItems = [];
  quickInsertRenderKey = "";
  quickInsertMode = "slash";
  blockMenuPinned = false;
}

function hideEditorOverlays(options: { keepFind?: boolean; keepCommandPalette?: boolean } = {}): void {
  hideSnippetPopup();
  hideQuickInsertPopup();
  hideJumpOverlay();
  hideMathPreview();
  selectionTool.hidden = true;
  linkPreview.hide();
  if (!options.keepFind) closeFindTool(false);
  if (!options.keepCommandPalette) closeCommandPalette(false);
  closeLeanLocationsPicker();
}

function placeFloating(el: HTMLElement, rect: { left: number; top: number; bottom: number } | null, width = 320): void {
  if (!rect) {
    el.hidden = true;
    return;
  }
  const margin = 8;
  const left = Math.min(Math.max(margin, rect.left), Math.max(margin, window.innerWidth - width - margin));
  let top = rect.bottom + 8;
  if (top + 240 > window.innerHeight) top = Math.max(margin, rect.top - 220);
  el.style.left = `${left}px`;
  el.style.top = `${top}px`;
  el.style.width = `${Math.min(width, window.innerWidth - margin * 2)}px`;
}

function relationNoteKey(note: NoteSummary | undefined): string {
  return note?.key || note?.id || note?.path || note?.file || note?.title || "";
}

function relationNoteTitle(note: NoteSummary | undefined): string {
  return note?.title || note?.id || note?.path || note?.file || "Untitled";
}

function uniqueRelationNotes(items: NoteSummary[]): NoteSummary[] {
  const seen = new Set<string>();
  const out: NoteSummary[] = [];
  for (const item of items) {
    const key = relationNoteKey(item);
    if (!key || seen.has(key)) continue;
    seen.add(key);
    out.push(item);
  }
  return out;
}

function relationMarkdownRefs(markdown: string): string[] {
  const text = String(markdown || "");
  const refs: string[] = [];
  let match: RegExpExecArray | null;
  const wiki = /\[\[([^\]\n]+)\]\]/g;
  while ((match = wiki.exec(text)) !== null) {
    const ref = String(match[1] || "").split("|", 1)[0]!.split("#", 1)[0]!.trim();
    if (ref) refs.push(ref);
  }
  const roam = /roam:\/\/([^\s)\]'"<>]+)/gi;
  while ((match = roam.exec(text)) !== null) {
    const ref = decodeNoteRef(String(match[1] || "").split(/[?#@]/, 1)[0] || "").trim();
    if (ref) refs.push(ref);
  }
  const markdownLink = /\[[^\]\n]*\]\(([^)\s]+(?:\.md|\.markdown|\.typ)(?:#[^)]+)?)\)/gi;
  while ((match = markdownLink.exec(text)) !== null) {
    const href = String(match[1] || "").split("#", 1)[0]!.trim();
    const note = resolveInternalNoteHref(href) || standaloneNoteFromMarkdownHref(href);
    if (note) refs.push(relationNoteKey(note));
    else if (href) refs.push(href);
  }
  return [...new Set(refs.map((ref) => ref.trim()).filter(Boolean))];
}

function relationResolveRefs(refs: readonly string[]): NoteSummary[] {
  return uniqueRelationNotes(refs
    .map((ref) => resolveNoteRef(ref))
    .filter((note): note is NoteSummary => Boolean(note?.file)));
}

function relationOutgoingNotes(note: NoteSummary): NoteSummary[] {
  const liveRefs = note.file === currentFile ? relationMarkdownRefs(editor.getMarkdown()) : [];
  return relationResolveRefs([...(note.refs ?? []), ...liveRefs])
    .filter((item) => relationNoteKey(item) !== relationNoteKey(note));
}

function relationBacklinkNotes(note: NoteSummary): NoteSummary[] {
  const direct = relationResolveRefs(note.backlinks ?? []);
  const noteKey = relationNoteKey(note);
  const scanned = notes.filter((candidate) => {
    if (relationNoteKey(candidate) === noteKey) return false;
    return relationResolveRefs(candidate.refs ?? []).some((target) => relationNoteKey(target) === noteKey);
  });
  return uniqueRelationNotes([...direct, ...scanned]);
}

function relationTags(note: NoteSummary | undefined): string[] {
  return [...new Set([...(note?.tags ?? []), ...(note?.inlineTags ?? [])]
    .map((tag) => String(tag || "").trim().replace(/^#/, ""))
    .filter(Boolean))]
    .sort((a, b) => a.localeCompare(b));
}

function relationSearchText(note: NoteSummary): string {
  return [
    note.title,
    note.path,
    note.summary,
    note.source,
    ...(note.aliases ?? []),
    ...((note as NoteSummary & { searchText?: string; content?: string }).searchText ? [(note as NoteSummary & { searchText?: string }).searchText] : []),
    ...((note as NoteSummary & { content?: string }).content ? [(note as NoteSummary & { content?: string }).content] : []),
  ].join("\n").slice(0, 180_000);
}

function relationMentionLabels(note: NoteSummary): string[] {
  const labels = [note.title, ...(note.aliases ?? [])]
    .map((label) => String(label || "").trim())
    .filter((label) => {
      if (!label) return false;
      if (/^[A-Za-z0-9 _-]+$/.test(label) && label.replace(/[^A-Za-z0-9]/g, "").length < 4) return false;
      if (!/[A-Za-z0-9]/.test(label) && label.length < 2) return false;
      return !["note", "notes", "math", "read", "book", "work", "daily"].includes(label.toLowerCase());
    });
  return [...new Set(labels)];
}

function textContainsRelationLabel(text: string, label: string): boolean {
  if (!text || !label) return false;
  if (/^[A-Za-z0-9 _-]+$/.test(label)) {
    return new RegExp(`(^|[^A-Za-z0-9_])${escapeRegExp(label)}([^A-Za-z0-9_]|$)`, "i").test(text);
  }
  return text.includes(label);
}

function relationUnlinkedMentions(note: NoteSummary, linkedKeys: Set<string>): Array<{ note: NoteSummary; label: string }> {
  const labels = relationMentionLabels(note);
  if (labels.length === 0) return [];
  const currentKey = relationNoteKey(note);
  const out: Array<{ note: NoteSummary; label: string }> = [];
  for (const candidate of notes) {
    const candidateKey = relationNoteKey(candidate);
    if (!candidateKey || candidateKey === currentKey || linkedKeys.has(candidateKey)) continue;
    const text = relationSearchText(candidate);
    const label = labels.find((item) => textContainsRelationLabel(text, item));
    if (!label) continue;
    out.push({ note: candidate, label });
    if (out.length >= 80) break;
  }
  return out;
}

function relationOpenNoteButton(note: NoteSummary, detail = ""): HTMLButtonElement {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "aaronnote-relation-note";
  const title = document.createElement("strong");
  title.textContent = relationNoteTitle(note);
  const meta = document.createElement("span");
  meta.textContent = detail || note.path || note.file || "";
  button.append(title, meta);
  button.addEventListener("click", (event) => openNote(note, { newWindow: event.metaKey || event.altKey, recordJump: true }));
  button.addEventListener("auxclick", (event) => {
    if (event.button !== 1) return;
    event.preventDefault();
    openNote(note, { newWindow: true, recordJump: true });
  });
  return button;
}

function appendRelationSection(parent: DocumentFragment | HTMLElement, title: string, count: number, fill: (section: HTMLElement) => void): void {
  const section = document.createElement("section");
  section.className = "aaronnote-relation-section";
  const head = document.createElement("header");
  const strong = document.createElement("strong");
  strong.textContent = title;
  const badge = document.createElement("span");
  badge.textContent = String(count);
  head.append(strong, badge);
  const list = document.createElement("div");
  list.className = "aaronnote-relation-list";
  section.append(head, list);
  fill(list);
  if (count === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-relation-empty";
    empty.textContent = "None";
    list.appendChild(empty);
  }
  parent.appendChild(section);
}

function renderRelationPanel(force = false): void {
  if (relationPanel.hidden) return;
  const note = currentNote();
  if (!note) {
    relationBody.textContent = "No current note";
    return;
  }
  const run = relationEpoch.begin();
  const key = [
    relationNoteKey(note),
    editor.getMarkdown().length,
    (note.refs ?? []).join(","),
    (note.backlinks ?? []).join(","),
    relationTags(note).join(","),
    notes.length,
  ].join("\n");
  if (!force && key === relationRenderKey) return;
  relationRenderKey = key;

  const outgoing = relationOutgoingNotes(note);
  const backlinks = relationBacklinkNotes(note);
  const tags = relationTags(note);
  const linkedKeys = new Set([...outgoing, ...backlinks].map(relationNoteKey));
  const unlinked = relationUnlinkedMentions(note, linkedKeys);
  if (!run.current) return;

  const frag = document.createDocumentFragment();
  const current = document.createElement("section");
  current.className = "aaronnote-relation-current";
  const currentTitle = document.createElement("strong");
  currentTitle.textContent = relationNoteTitle(note);
  const currentMeta = document.createElement("span");
  currentMeta.textContent = [
    note.kind && note.kind !== "default" ? note.kind : "",
    note.path || note.file || "",
  ].filter(Boolean).join(" · ");
  current.append(currentTitle, currentMeta);
  frag.appendChild(current);

  appendRelationSection(frag, "Outgoing", outgoing.length, (section) => {
    for (const item of outgoing) section.appendChild(relationOpenNoteButton(item, item.path || item.file || ""));
  });
  appendRelationSection(frag, "Backlinks", backlinks.length, (section) => {
    for (const item of backlinks) section.appendChild(relationOpenNoteButton(item, item.path || item.file || ""));
  });
  appendRelationSection(frag, "Unlinked", unlinked.length, (section) => {
    for (const item of unlinked) section.appendChild(relationOpenNoteButton(item.note, `mentions "${item.label}"`));
  });
  appendRelationSection(frag, "Tags", tags.length, (section) => {
    section.classList.add("aaronnote-relation-tags");
    for (const tag of tags) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-relation-tag";
      button.textContent = `#${tag}`;
      button.addEventListener("click", () => openTagFilter(tag));
      section.appendChild(button);
    }
  });
  relationBody.replaceChildren(frag);
}

function positionRelationPanel(): void {
  const width = Math.min(860, window.innerWidth - 24);
  relationPanel.style.width = `${Math.max(320, width)}px`;
}

function openRelationPanel(): void {
  if (!currentFile) {
    setStatus("No current note");
    return;
  }
  hideEditorOverlays();
  linkPreview.hide();
  relationPanel.hidden = false;
  relationButton.classList.add("is-active");
  relationButton.setAttribute("aria-pressed", "true");
  positionRelationPanel();
  renderRelationPanel(true);
}

function closeRelationPanel(): void {
  relationPanel.hidden = true;
  relationButton.classList.remove("is-active");
  relationButton.setAttribute("aria-pressed", "false");
}

function toggleRelationPanel(): void {
  if (relationPanel.hidden) openRelationPanel();
  else closeRelationPanel();
}

function primaryPointerModifier(event: MouseEvent): boolean {
  if (event.metaKey && !event.ctrlKey) return true;
  return !/Mac/.test(navigator.platform) && event.ctrlKey && !event.metaKey;
}

function previewTargetFromHref(href: string): LinkPreviewTarget {
  const roamLike = resolveRoamLikeNoteTarget(href);
  if (roamLike?.note) return { href, ...roamLike };
  if (markdownNoteHref(href)) {
    const note = resolveInternalNoteHref(href) || standaloneNoteFromMarkdownHref(href);
    if (note) return {
      href,
      note,
      equationTag: equationTagFromHref(href) || "",
      inlineTag: inlineTagFromHref(href),
    };
  }
  return { href, external: true };
}

function commandPaletteCommands(): AaronnoteCommand[] {
  const base: AaronnoteCommand[] = [
    { id: "new-markdown-note", title: "New Markdown note", group: "File", keywords: ["regular", "md"], run: () => void createMarkdownNote() },
    { id: "save", title: "Save note", group: "File", keywords: ["write"], enabled: () => !!currentFile, run: () => save() },
    { id: "force-save", title: "Force save over disk changes", group: "File", keywords: ["conflict", "overwrite"], enabled: () => !!currentFile && saveConflictActive, run: () => void forceSaveStandalone() },
    { id: "export-pdf", title: "Export PDF", group: "File", keywords: ["print"], enabled: () => !!currentFile, run: () => void exportPdf() },
    { id: "delete-node", title: "Delete current note", group: "File", keywords: ["trash", "roam"], enabled: () => !!currentFile, run: () => void deleteCurrentNote() },

    { id: "source", title: editor.isSourceMode() ? "Switch to preview" : "Switch to source", group: "Editor", keywords: ["markdown", "raw"], run: toggleSourceMode },
    { id: "focus", title: writingMode.focusMode ? "Disable focus mode" : "Enable focus mode", group: "Editor", keywords: ["writing"], run: toggleFocusMode },
    { id: "find", title: "Find and replace", group: "Editor", keywords: ["search"], run: openFindTool },
    { id: "check-prose", title: "Check spelling and prose", group: "Editor", keywords: ["vale", "cspell", "spellcheck"], run: () => void checkProse() },
    { id: "block-menu", title: "Open block menu", group: "Editor", keywords: ["slash", "insert"], run: openBlockMenu },
    { id: "lean-block-manager", title: "Lean block manager", group: "Editor", keywords: ["lean4", "proof", "mirror", "file"], enabled: () => !!currentFile && !currentStandalone, run: () => void openLeanBlockManager() },
    { id: "insert-lean-block", title: "Insert Lean block", group: "Editor", keywords: ["lean4", "proof"], enabled: () => !!currentFile && !currentStandalone, run: () => void insertLeanBlock() },
    { id: "clean-lean-block", title: "Clean current Lean block", group: "Editor", keywords: ["lean4", "delete", "tag"], enabled: () => !!currentFile && !currentStandalone, run: () => void cleanCurrentLeanBlock() },
    { id: "toggle-lean-panel", title: "Toggle Lean panel", group: "Editor", keywords: ["lean4", "infoview", "lsp"], enabled: () => !leanTriggerBtn.hidden, run: toggleLeanPanel },
    { id: "toggle-jupyter-preview", title: "Toggle Jupyter preview", group: "Editor", keywords: ["ipynb", "notebook", "jupyter"], enabled: () => api.jupyter.available(), run: toggleJupyterPanel },
    { id: "stop-jupyter", title: "Stop Jupyter preview", group: "Editor", keywords: ["ipynb", "notebook", "jupyter"], enabled: () => api.jupyter.available(), run: () => void jupyterPanel.stop() },

    { id: "lean-goto-definition", title: "Lean: Go to Definition", group: "Lean", keywords: ["lsp", "lean4", "gd"], enabled: () => !!activeLeanController(), run: () => void activeLeanController()?.runLspAction("definition") },
    { id: "lean-goto-declaration", title: "Lean: Go to Declaration", group: "Lean", keywords: ["lsp", "lean4", "gD"], enabled: () => !!activeLeanController(), run: () => void activeLeanController()?.runLspAction("declaration") },
    { id: "lean-goto-type-definition", title: "Lean: Go to Type Definition", group: "Lean", keywords: ["lsp", "lean4", "gy"], enabled: () => !!activeLeanController(), run: () => void activeLeanController()?.runLspAction("typeDefinition") },
    { id: "lean-goto-implementation", title: "Lean: Go to Implementation", group: "Lean", keywords: ["lsp", "lean4", "gi"], enabled: () => !!activeLeanController(), run: () => void activeLeanController()?.runLspAction("implementation") },
    { id: "lean-find-references", title: "Lean: Find References", group: "Lean", keywords: ["lsp", "lean4", "gr"], enabled: () => !!activeLeanController(), run: () => void activeLeanController()?.runLspAction("references") },
    { id: "lean-show-hover", title: "Lean: Show Hover", group: "Lean", keywords: ["lsp", "lean4", "K", "docs"], enabled: () => !!activeLeanController(), run: () => void activeLeanController()?.runLspAction("hover") },
    { id: "lean-open-neovide", title: "Lean: Open in Neovide", group: "Lean", keywords: ["external", "editor", "nvim", "neovim"], enabled: () => !!activeLeanController() && api.externalEditor.available(), run: async () => { await activeLeanController()?.openExternal(); } },
    { id: "lean-toggle-line-comment", title: "Lean: Toggle Line Comment", group: "Lean", keywords: ["edit", "lean4", "--"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("toggleLineComment") },
    { id: "lean-toggle-block-comment", title: "Lean: Toggle Block Comment", group: "Lean", keywords: ["edit", "lean4", "/-"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("toggleBlockComment") },
    { id: "lean-duplicate-down", title: "Lean: Duplicate Line Down", group: "Lean", keywords: ["edit", "lean4", "copy"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("duplicateDown") },
    { id: "lean-duplicate-up", title: "Lean: Duplicate Line Up", group: "Lean", keywords: ["edit", "lean4", "copy"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("duplicateUp") },
    { id: "lean-move-down", title: "Lean: Move Lines Down", group: "Lean", keywords: ["edit", "lean4"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("moveDown") },
    { id: "lean-move-up", title: "Lean: Move Lines Up", group: "Lean", keywords: ["edit", "lean4"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("moveUp") },
    { id: "lean-join-lines", title: "Lean: Join Lines", group: "Lean", keywords: ["edit", "lean4", "J"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("joinLines") },
    { id: "lean-delete-trailing-whitespace", title: "Lean: Delete Trailing Whitespace", group: "Lean", keywords: ["edit", "lean4", "trim"], enabled: () => !!activeLeanController(), run: () => activeLeanController()?.runEditAction("deleteTrailingWhitespace") },

    { id: "notes", title: "Open notes", group: "Navigation", keywords: ["filesystem"], run: () => showNotesPage("filesystem") },
    { id: "relation", title: "Open relation", group: "Navigation", keywords: ["backlinks", "refs", "links"], enabled: () => !!currentFile, run: toggleRelationPanel },
    { id: "local-graph", title: "Toggle local graph", group: "Navigation", keywords: ["obsidian", "depth", "backlinks"], enabled: currentNoteSupportsLocalGraph, run: localGraphPanel.toggle },
    { id: "agenda", title: "Open agenda", group: "Navigation", keywords: ["todo"], run: () => showNotesPage("agenda") },
    { id: "jump-stack", title: "Open jump stack", group: "Navigation", keywords: ["roam", "back"], run: toggleJumpStackPanel },
    { id: "jump-back", title: "Jump back", group: "Navigation", keywords: ["roam", "back"], enabled: () => jumpStack.length > 0, run: jumpBack },

    { id: "new-roam-node", title: "New roam note", group: "Roam", keywords: ["note"], enabled: () => !currentStandalone, run: () => void createRoamNode() },
    { id: "new-node", title: "New note", group: "Roam", keywords: ["regular", "markdown"], enabled: () => !currentStandalone, run: () => void createNode() },
    { id: "open-today-daily", title: "Open today's daily note", group: "Roam", keywords: ["daily", "journal", "today"], enabled: () => !currentStandalone, run: () => void openTodayDaily() },
    { id: "open-roam-node", title: "Open roam node", group: "Roam", keywords: ["idlink", "switch"], enabled: () => !currentStandalone, run: () => void openRoamNode() },
    { id: "graph", title: "Open roam graph", group: "Roam", keywords: ["network"], enabled: () => !currentStandalone, run: () => showNotesPage("graph") },
    { id: "git", title: "Open git control", group: "Roam", keywords: ["version", "commit", "diff"], enabled: () => !currentStandalone, run: () => showNotesPage("git") },
    { id: "lean-project", title: "Open Lean project", group: "Roam", keywords: ["lake", "mathlib", "cache"], enabled: () => !currentStandalone, run: () => showNotesPage("lean") },
    { id: "sync", title: "Sync roamdb", group: "Roam", keywords: ["index"], enabled: () => !currentStandalone, run: () => void syncRoamDb() },
    { id: "ensure-roam-id", title: "Generate or copy Roam ID", group: "Roam", keywords: ["id", "clipboard"], enabled: () => !currentStandalone && !!currentFile, run: () => void ensureRoamId() },
    { id: "insert-roam-idlink", title: "Insert roam idlink", group: "Roam", keywords: ["link", "reference"], enabled: () => !currentStandalone, run: () => void insertRoamIdLink() },
    { id: "add-meta", title: "Add meta", group: "Roam", keywords: ["kind"], enabled: () => !!currentFile, run: () => void quickAddMeta() },
    { id: "hide-roam", title: "Set roam: off for current note", group: "Roam", keywords: ["exclude", "disable", "off"], enabled: () => !currentStandalone && !!currentFile, run: () => void hideCurrentRoam() },
    { id: "activate-roam", title: "Clear roam: off for current note", group: "Roam", keywords: ["include", "enable", "on"], enabled: () => !currentStandalone && !!currentFile, run: () => void activateCurrentRoam() },
    { id: "add-tag", title: "Add tag", group: "Roam", keywords: ["tags"], enabled: () => !!currentFile, run: () => void addTag() },
    { id: "manage-note-tags", title: "Manage note tags", group: "Roam", keywords: ["tags"], enabled: () => !!currentFile, run: () => void openTagManager() },
    { id: "insert-inline-tag", title: "Insert inline tag", group: "Roam", keywords: ["tags"], enabled: () => !!currentFile, run: () => void insertInlineTag() },
    { id: "tag-context", title: "Tag context", group: "Roam", keywords: ["latex", "label", "tags"], enabled: () => !!currentFile, run: () => void handleTagCommand() },

    { id: "reload-snippets", title: "Reload snippets", group: "Snippets", keywords: ["yasnippet"], run: () => void reloadSnippets() },
    {
      id: "toggle-snippets",
      title: snippetSuggestionsEnabled ? "Disable snippet suggestions" : "Enable snippet suggestions",
      group: "Snippets",
      keywords: ["autocomplete"],
      run: () => setSnippetSuggestionsEnabled(!snippetSuggestionsEnabled),
    },
    { id: "reset-snippets", title: "Reset snippet suppression", group: "Snippets", keywords: ["autocomplete"], run: clearSnippetSuggestionState },

  ];

  return base;
}

function filteredCommandPaletteCommands(): AaronnoteCommand[] {
  return filterCommands(commandPaletteCommands(), commandQuery.value, 16);
}

function renderCommandPalette(): void {
  const commands = filteredCommandPaletteCommands();
  commandPaletteIndex = clampCommandIndex(commandPaletteIndex, commands.length);
  const key = `${commandQuery.value}\n${commandPaletteIndex}\n${commands.map((command) => command.id).join("\n")}`;
  if (commandPaletteRenderKey === key) return;
  commandPaletteRenderKey = key;
  commandList.innerHTML = "";
  if (commands.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-command-empty";
    empty.textContent = "No commands";
    commandList.append(empty);
    return;
  }
  commands.forEach((command, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.id = `aaronnote-command-option-${index}`;
    button.className = index === commandPaletteIndex ? "aaronnote-command-option is-active" : "aaronnote-command-option";
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", index === commandPaletteIndex ? "true" : "false");
    const title = document.createElement("span");
    title.className = "aaronnote-command-title";
    title.textContent = command.title;
    const group = document.createElement("span");
    group.className = "aaronnote-command-group";
    group.textContent = command.group;
    button.append(title, group);
    button.addEventListener("mousedown", (event) => {
      event.preventDefault();
      commandPaletteIndex = index;
      chooseCommandPaletteItem();
    });
    button.addEventListener("mouseenter", () => {
      if (commandPaletteIndex === index) return;
      commandPaletteIndex = index;
      commandPaletteRenderKey = "";
      renderCommandPalette();
    });
    commandList.append(button);
  });
  commandList.setAttribute("aria-activedescendant", `aaronnote-command-option-${commandPaletteIndex}`);
}

function openCommandPalette(): void {
  hideEditorOverlays({ keepCommandPalette: true });
  commandPalette.hidden = false;
  commandPaletteIndex = 0;
  commandPaletteRenderKey = "";
  commandQuery.value = "";
  renderCommandPalette();
  commandQuery.focus();
  commandQuery.select();
}

function closeCommandPalette(refocusEditor = true): void {
  if (commandPalette.hidden) return;
  commandPalette.hidden = true;
  commandPaletteRenderKey = "";
  if (refocusEditor) editor.focus();
}

function chooseCommandPaletteItem(): void {
  const command = filteredCommandPaletteCommands()[commandPaletteIndex];
  if (!command) return;
  closeCommandPalette(false);
  const result = command.run();
  if (result instanceof Promise) void result.catch((err) => {
    setStatus(err instanceof Error ? err.message : `${command.title} failed`);
  });
  else setStatus(command.title);
}

function moveCommandPaletteIndex(index: number): void {
  const commands = filteredCommandPaletteCommands();
  commandPaletteIndex = commands.length ? ((index % commands.length) + commands.length) % commands.length : 0;
  commandPaletteRenderKey = "";
  renderCommandPalette();
  commandList.querySelector(".aaronnote-command-option.is-active")?.scrollIntoView({ block: "nearest" });
}

function handleCommandPaletteKey(event: KeyboardEvent): boolean {
  if (commandPalette.hidden) return false;
  if (event.key === "Escape") {
    event.preventDefault();
    closeCommandPalette();
    return true;
  }
  const commands = filteredCommandPaletteCommands();
  if (event.key === "ArrowDown") {
    event.preventDefault();
    moveCommandPaletteIndex(commandPaletteIndex + 1);
    return true;
  }
  if (event.key === "ArrowUp") {
    event.preventDefault();
    moveCommandPaletteIndex(commandPaletteIndex - 1);
    return true;
  }
  if (event.key === "PageDown" || event.key === "PageUp") {
    event.preventDefault();
    moveCommandPaletteIndex(commandPaletteIndex + (event.key === "PageDown" ? 6 : -6));
    return true;
  }
  if (event.key === "Home" || event.key === "End") {
    event.preventDefault();
    moveCommandPaletteIndex(event.key === "Home" ? 0 : commands.length - 1);
    return true;
  }
  if (event.key === "Enter" || (event.key === "Tab" && !event.shiftKey)) {
    event.preventDefault();
    chooseCommandPaletteItem();
    return true;
  }
  return false;
}

function leanLocationLabel(file: string): string {
  const parts = String(file).split("/").filter(Boolean);
  return parts.slice(-2).join("/") || file;
}

function filteredLeanLocations(): LeanLocation[] {
  const query = leanLocationsQuery.value.trim().toLowerCase();
  if (!query) return leanLocationsItems;
  return leanLocationsItems.filter((loc) => `${loc.file} ${loc.summary}`.toLowerCase().includes(query));
}

function renderLeanLocationsPicker(): void {
  const items = filteredLeanLocations();
  leanLocationsIndex = items.length ? Math.max(0, Math.min(leanLocationsIndex, items.length - 1)) : 0;
  leanLocationsList.innerHTML = "";
  if (items.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-command-empty";
    empty.textContent = "No locations";
    leanLocationsList.append(empty);
    return;
  }
  items.forEach((loc, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.className = index === leanLocationsIndex ? "aaronnote-command-option is-active" : "aaronnote-command-option";
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", index === leanLocationsIndex ? "true" : "false");
    const title = document.createElement("span");
    title.className = "aaronnote-command-title";
    title.textContent = loc.summary || leanLocationLabel(loc.file);
    const group = document.createElement("span");
    group.className = "aaronnote-command-group";
    group.textContent = `${leanLocationLabel(loc.file)}:${loc.range.start.line + 1}:${loc.range.start.character + 1}`;
    button.append(title, group);
    button.addEventListener("mousedown", (event) => {
      event.preventDefault();
      leanLocationsIndex = index;
      chooseLeanLocation();
    });
    button.addEventListener("mouseenter", () => {
      if (leanLocationsIndex === index) return;
      leanLocationsIndex = index;
      renderLeanLocationsPicker();
    });
    leanLocationsList.append(button);
  });
}

function openLeanLocationsPicker(locations: LeanLocation[], onPick: (location: LeanLocation) => void): void {
  leanLocationsItems = locations;
  leanLocationsOnPick = onPick;
  leanLocationsIndex = 0;
  leanLocationsQuery.value = "";
  leanLocationsPicker.hidden = false;
  renderLeanLocationsPicker();
  leanLocationsQuery.focus();
}

function closeLeanLocationsPicker(): void {
  if (leanLocationsPicker.hidden) return;
  leanLocationsPicker.hidden = true;
  leanLocationsItems = [];
  leanLocationsOnPick = null;
}

function chooseLeanLocation(): void {
  const loc = filteredLeanLocations()[leanLocationsIndex];
  const onPick = leanLocationsOnPick;
  closeLeanLocationsPicker();
  if (loc && onPick) onPick(loc);
}

function handleLeanLocationsPickerKey(event: KeyboardEvent): boolean {
  if (leanLocationsPicker.hidden) return false;
  if (event.key === "Escape") {
    event.preventDefault();
    closeLeanLocationsPicker();
    editor.focus();
    return true;
  }
  const items = filteredLeanLocations();
  if (event.key === "ArrowDown") {
    event.preventDefault();
    leanLocationsIndex = items.length ? (leanLocationsIndex + 1) % items.length : 0;
    renderLeanLocationsPicker();
    leanLocationsList.querySelector(".aaronnote-command-option.is-active")?.scrollIntoView({ block: "nearest" });
    return true;
  }
  if (event.key === "ArrowUp") {
    event.preventDefault();
    leanLocationsIndex = items.length ? (leanLocationsIndex + items.length - 1) % items.length : 0;
    renderLeanLocationsPicker();
    leanLocationsList.querySelector(".aaronnote-command-option.is-active")?.scrollIntoView({ block: "nearest" });
    return true;
  }
  if (event.key === "PageDown" || event.key === "PageUp") {
    event.preventDefault();
    const delta = event.key === "PageDown" ? 6 : -6;
    leanLocationsIndex = items.length ? ((leanLocationsIndex + delta) % items.length + items.length) % items.length : 0;
    renderLeanLocationsPicker();
    leanLocationsList.querySelector(".aaronnote-command-option.is-active")?.scrollIntoView({ block: "nearest" });
    return true;
  }
  if (event.key === "Home" || event.key === "End") {
    event.preventDefault();
    leanLocationsIndex = event.key === "Home" ? 0 : Math.max(0, items.length - 1);
    renderLeanLocationsPicker();
    leanLocationsList.querySelector(".aaronnote-command-option.is-active")?.scrollIntoView({ block: "nearest" });
    return true;
  }
  if (event.key === "Enter") {
    event.preventDefault();
    chooseLeanLocation();
    return true;
  }
  return false;
}

leanLocationsQuery.addEventListener("input", () => {
  leanLocationsIndex = 0;
  renderLeanLocationsPicker();
});
leanLocationsPicker.querySelector("[data-lean-locations-close]")?.addEventListener("mousedown", (event) => {
  event.preventDefault();
  closeLeanLocationsPicker();
  editor.focus();
});
setLeanLocationsPicker(openLeanLocationsPicker);

function handleLeanEditorMenuAction(detail: Record<string, unknown>): void {
  const editorId = String(detail?.editorId ?? "");
  const controller = getLeanController(editorId) ?? activeLeanController();
  if (!controller) return;
  const kind = String(detail?.kind ?? "");
  const action = String(detail?.action ?? "");
  const line = Number(detail?.line ?? 0);
  const character = Number(detail?.character ?? 0);
  if (kind === "lsp") void controller.runLspAction(action as LeanLspAction, { line, character });
  else if (kind === "edit") controller.runEditAction(action as LeanEditAction);
  else if (kind === "external" && action === "openNeovide") void controller.openExternal({ line, character });
}

function quickInsertPrefix(before: string): { query: string; deleteBefore: number } | null {
  const line = before.slice(before.lastIndexOf("\n") + 1);
  const match = line.match(/^[ \t]*\/([A-Za-z0-9_-]{0,32})$/);
  if (!match) return null;
  const query = match[1] ?? "";
  if (query === quickInsertSuppressedPrefix) return null;
  return { query, deleteBefore: query.length + 1 };
}

function editorHasNativeSelection(): boolean {
  if (editor.isSourceMode()) return false;
  const selection = window.getSelection();
  if (!selection || selection.isCollapsed || selection.rangeCount === 0) return false;
  const anchor = selection.anchorNode;
  const focus = selection.focusNode;
  return !!anchor && !!focus && host.contains(anchor) && host.contains(focus);
}

function renderQuickInsertPopup(query: string, rect: { left: number; top: number; bottom: number } | null): void {
  const nextKey = `${quickInsertMode}\n${query}\n${quickInsertIndex}\n${quickInsertItems.map((item) => `${item.id}:${item.label}`).join("\n")}`;
  if (!quickInsertPopup.hidden && quickInsertRenderKey === nextKey) {
    placeFloating(quickInsertPopup, rect, 360);
    quickInsertPopup.querySelector(".aaronnote-quick-option.is-active")?.scrollIntoView({ block: "nearest" });
    return;
  }
  quickInsertRenderKey = nextKey;
  quickInsertPopup.innerHTML = "";
  quickInsertItems.forEach((item, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.id = `aaronnote-quick-option-${index}`;
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", index === quickInsertIndex ? "true" : "false");
    button.className = index === quickInsertIndex
      ? "aaronnote-quick-option is-active"
      : "aaronnote-quick-option";
    const icon = document.createElement("span");
    icon.className = "aaronnote-quick-option-icon";
    icon.textContent = item.label.slice(0, 1).toUpperCase();
    const label = document.createElement("span");
    label.className = "aaronnote-quick-option-label";
    label.textContent = item.label;
    const detail = document.createElement("span");
    detail.className = "aaronnote-quick-option-detail";
    detail.textContent = item.detail ?? item.command ?? "";
    button.append(icon, label, detail);
    button.addEventListener("mousedown", (event) => {
      event.preventDefault();
      quickInsertIndex = index;
      chooseQuickInsertItem();
    });
    button.addEventListener("mouseenter", () => {
      if (quickInsertIndex === index) return;
      quickInsertIndex = index;
      quickInsertRenderKey = "";
      renderQuickInsertPopup(quickInsertPopup.dataset.query ?? query, editor.cursorRect());
    });
    quickInsertPopup.appendChild(button);
  });
  quickInsertPopup.dataset.query = query;
  quickInsertPopup.setAttribute("role", "listbox");
  quickInsertPopup.setAttribute("aria-activedescendant", `aaronnote-quick-option-${quickInsertIndex}`);
  quickInsertPopup.hidden = false;
  placeFloating(quickInsertPopup, rect, 360);
  quickInsertPopup.querySelector(".aaronnote-quick-option.is-active")?.scrollIntoView({ block: "nearest" });
}

function updateQuickInsertPopup(ctx: ReturnType<typeof editor.cursorContext>): boolean {
  if (quickInsertMode === "block" && blockMenuPinned) return true;
  const active = document.activeElement;
  if (!active || !host.contains(active)) {
    hideQuickInsertPopup();
    return false;
  }
  const prefix = quickInsertPrefix(ctx.before);
  if (!prefix) {
    hideQuickInsertPopup();
    return false;
  }
  const items = editor.getQuickInsertItems(prefix.query);
  if (items.length === 0) {
    hideQuickInsertPopup();
    return false;
  }
  quickInsertMode = "slash";
  quickInsertDeleteBefore = prefix.deleteBefore;
  quickInsertIndex = Math.min(quickInsertIndex, items.length - 1);
  quickInsertItems = items;
  renderQuickInsertPopup(prefix.query, ctx.rect);
  return true;
}

function chooseQuickInsertItem(): void {
  const item = quickInsertItems[quickInsertIndex];
  if (!item) return;
  const deleteBefore = quickInsertDeleteBefore;
  hideQuickInsertPopup();
  quickInsertSuppressedPrefix = "";
  if (deleteBefore > 0) editor.insertText("", deleteBefore);
  if (editor.runQuickInsert(item)) {
    setStatus(item.label);
    scheduleAssistUpdate({ snippets: true });
    scheduleCursorPositionSave();
  }
}

function handleQuickInsertKey(event: KeyboardEvent): boolean {
  if (quickInsertPopup.hidden) return false;
  if (event.isComposing) return false;
  const active = document.activeElement;
  const target = event.target as Node | null;
  if ((!active || !host.contains(active)) && (!target || !host.contains(target))) {
    hideQuickInsertPopup();
    return false;
  }
  if (quickInsertItems.length === 0) {
    hideQuickInsertPopup();
    return false;
  }
  if (event.key === "ArrowDown") {
    event.preventDefault();
    quickInsertIndex = (quickInsertIndex + 1) % quickInsertItems.length;
    renderQuickInsertPopup(quickInsertPopup.dataset.query ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "ArrowUp") {
    event.preventDefault();
    quickInsertIndex = (quickInsertIndex + quickInsertItems.length - 1) % quickInsertItems.length;
    renderQuickInsertPopup(quickInsertPopup.dataset.query ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "PageDown" || event.key === "PageUp") {
    event.preventDefault();
    const delta = event.key === "PageDown" ? 6 : -6;
    quickInsertIndex = ((quickInsertIndex + delta) % quickInsertItems.length + quickInsertItems.length) % quickInsertItems.length;
    renderQuickInsertPopup(quickInsertPopup.dataset.query ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "Home" || event.key === "End") {
    event.preventDefault();
    quickInsertIndex = event.key === "Home" ? 0 : quickInsertItems.length - 1;
    renderQuickInsertPopup(quickInsertPopup.dataset.query ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "Enter" || (event.key === "Tab" && !event.shiftKey)) {
    event.preventDefault();
    chooseQuickInsertItem();
    return true;
  }
  if (event.key === "Escape") {
    event.preventDefault();
    if (quickInsertMode === "slash") quickInsertSuppressedPrefix = quickInsertPopup.dataset.query ?? "";
    hideQuickInsertPopup();
    return true;
  }
  return false;
}

function openBlockMenu(): void {
  const ctx = editor.getBlockContext();
  if (ctx.sourceMode || !ctx.rect) return;
  const items = editor.getQuickInsertItems("");
  if (items.length === 0) return;
  quickInsertMode = "block";
  blockMenuPinned = true;
  quickInsertDeleteBefore = 0;
  quickInsertIndex = 0;
  quickInsertItems = items;
  hideSnippetPopup();
  selectionTool.hidden = true;
  editor.focus();
  renderQuickInsertPopup(ctx.type, ctx.rect);
}

const jumpLabelAlphabet = "asdfghjklqwertyuiopzxcvbnm";

function jumpLabels(count: number): string[] {
  if (count <= jumpLabelAlphabet.length) return [...jumpLabelAlphabet.slice(0, count)];
  const labels: string[] = [];
  for (const first of jumpLabelAlphabet) {
    for (const second of jumpLabelAlphabet) {
      labels.push(`${first}${second}`);
      if (labels.length >= count) return labels;
    }
  }
  return labels;
}

function jumpInputChar(key: string): string {
  return key.length === 1 && !/\s/.test(key) ? key : "";
}

function jumpLabelChar(key: string): string {
  const ch = key.length === 1 ? key.toLowerCase() : "";
  return jumpLabelAlphabet.includes(ch) ? ch : "";
}

function hideJumpOverlay(): void {
  jumpMode = null;
  jumpOverlay.hidden = true;
  jumpOverlay.innerHTML = "";
}

function jumpRectInViewport(rect: { left: number; top: number; bottom: number }): boolean {
  return rect.bottom >= 44
    && rect.top <= window.innerHeight
    && rect.left >= 0
    && rect.left <= window.innerWidth;
}

function addJumpTarget(
  targets: Omit<JumpTarget, "label">[],
  seen: Set<string>,
  pos: number,
  rect: { left: number; top: number; bottom: number } | null,
): void {
  if (!rect || !jumpRectInViewport(rect)) return;
  const key = `${pos}:${Math.round(rect.left)}:${Math.round(rect.top)}`;
  if (seen.has(key)) return;
  seen.add(key);
  targets.push({ pos, rect: { left: rect.left, top: rect.top, bottom: rect.bottom } });
}

function sourceJumpTargets(queryLower: string, limit: number): Omit<JumpTarget, "label">[] {
  const view = editor.view;
  const targets: Omit<JumpTarget, "label">[] = [];
  const seen = new Set<string>();
  for (const range of view.visibleRanges) {
    const text = view.state.doc.sliceString(range.from, range.to);
    for (let index = 0; index < text.length; index++) {
      if (text[index]?.toLowerCase() !== queryLower) continue;
      const pos = range.from + index;
      let coords: { left: number; top: number; bottom: number } | null = null;
      try {
        coords = view.coordsAtPos(pos);
      } catch {
        coords = null;
      }
      addJumpTarget(targets, seen, pos, coords);
      if (targets.length >= limit) return targets;
    }
  }
  return targets;
}

function domTextJumpTargets(queryLower: string, limit: number): Omit<JumpTarget, "label">[] {
  const view = editor.view;
  const targets: Omit<JumpTarget, "label">[] = [];
  const seen = new Set<string>();
  const doc = view.contentDOM.ownerDocument;
  const walker = doc.createTreeWalker(view.contentDOM, NodeFilter.SHOW_TEXT, {
    acceptNode(node) {
      const text = node.textContent ?? "";
      if (!text.toLowerCase().includes(queryLower)) return NodeFilter.FILTER_REJECT;
      const parent = node.parentElement;
      if (!parent) return NodeFilter.FILTER_REJECT;
      if (parent.closest("[aria-hidden='true'], script, style")) return NodeFilter.FILTER_REJECT;
      return NodeFilter.FILTER_ACCEPT;
    },
  });

  for (let node = walker.nextNode(); node; node = walker.nextNode()) {
    const textNode = node as Text;
    const text = textNode.data;
    const lower = text.toLowerCase();
    for (let index = lower.indexOf(queryLower); index >= 0; index = lower.indexOf(queryLower, index + 1)) {
      const range = doc.createRange();
      range.setStart(textNode, index);
      range.setEnd(textNode, index + 1);
      const rect = range.getClientRects()[0] ?? null;
      let pos: number | null = null;
      try {
        pos = view.posAtDOM(textNode, index);
      } catch {
        pos = null;
      }
      addJumpTarget(targets, seen, pos ?? view.state.selection.main.from, rect);
      range.detach();
      if (targets.length >= limit) return targets;
    }
  }
  return targets;
}

function visibleJumpTargets(query: string): JumpTarget[] {
  const labels = jumpLabels(180);
  const queryLower = query.toLowerCase();
  const targets = domTextJumpTargets(queryLower, labels.length);
  if (targets.length === 0) targets.push(...sourceJumpTargets(queryLower, labels.length));
  return targets.map((target, index) => ({ ...target, label: labels[index] ?? "" })).filter((target) => target.label);
}

function renderJumpOverlay(): void {
  if (!jumpMode || jumpMode.phase !== "label") {
    jumpOverlay.hidden = true;
    return;
  }
  const typed = jumpMode.typed;
  jumpOverlay.innerHTML = "";
  for (const target of jumpMode.targets) {
    const marker = document.createElement("span");
    marker.className = target.label.startsWith(typed)
      ? "aaronnote-jump-label"
      : "aaronnote-jump-label is-muted";
    marker.textContent = target.label;
    marker.style.left = `${target.rect.left}px`;
    marker.style.top = `${target.rect.top}px`;
    jumpOverlay.appendChild(marker);
  }
  jumpOverlay.hidden = false;
}

function finishJump(target: JumpTarget): void {
  editor.setSelection(target.pos);
  editor.focus();
  editor.revealCursor();
  hideJumpOverlay();
  setStatus("Jumped");
  scheduleCursorPositionSave();
  scheduleAssistUpdate({ cursor: true });
}

function startJumpMode(): void {
  if (!editorOwnsActiveSurface()) return;
  hideEditorOverlays();
  editor.focus();
  jumpMode = { phase: "target" };
  setStatus("Jump: type target char");
}

function handleJumpModeKey(event: KeyboardEvent): boolean {
  if (!jumpMode) return false;
  if (event.metaKey || event.ctrlKey || event.altKey) {
    hideJumpOverlay();
    return false;
  }
  event.preventDefault();
  event.stopPropagation();
  event.stopImmediatePropagation();
  if (event.key === "Escape") {
    hideJumpOverlay();
    setStatus(vimMode === "normal" ? "NORMAL" : vimMode.toUpperCase());
    return true;
  }
  if (jumpMode.phase === "target") {
    const query = jumpInputChar(event.key);
    if (!query) return true;
    const targets = visibleJumpTargets(query);
    if (targets.length === 0) {
      hideJumpOverlay();
      setStatus(`Jump target not visible: ${query}`);
      return true;
    }
    jumpMode = { phase: "label", query, typed: "", targets };
    renderJumpOverlay();
    setStatus(`Jump ${query}: ${targets.length} targets`);
    return true;
  }
  if (event.key === "Backspace") {
    jumpMode = { ...jumpMode, typed: jumpMode.typed.slice(0, -1) };
    renderJumpOverlay();
    return true;
  }
  const labelChar = jumpLabelChar(event.key);
  if (!labelChar) return true;
  const typed = jumpMode.typed + labelChar;
  const exact = jumpMode.targets.find((target) => target.label === typed);
  if (exact) {
    finishJump(exact);
    return true;
  }
  if (jumpMode.targets.some((target) => target.label.startsWith(typed))) {
    jumpMode = { ...jumpMode, typed };
    renderJumpOverlay();
    return true;
  }
  setStatus(`No jump label: ${typed}`);
  return true;
}

function editorOwnsActiveSurface(): boolean {
  const active = document.activeElement;
  if (!active || !host.contains(active)) return false;
  const editable = active.closest<HTMLElement>("input, textarea, select, [contenteditable='true']");
  return !editable || editable.classList.contains("cm-content");
}

function placeFloatingAbove(el: HTMLElement, rect: { left: number; top: number; bottom: number } | null, width = 320, bottomRect?: { bottom: number } | null): void {
  if (!rect) {
    el.hidden = true;
    return;
  }
  const margin = 8;
  const resolvedWidth = Math.min(width, window.innerWidth - margin * 2);
  const left = Math.min(Math.max(margin, rect.left), Math.max(margin, window.innerWidth - resolvedWidth - margin));
  const previewHeight = Math.min(el.offsetHeight || 180, window.innerHeight - margin * 2);
  let top = rect.top - previewHeight - 8;
  if (top < margin) top = (bottomRect ?? rect).bottom + 8;
  if (top + previewHeight > window.innerHeight - margin) {
    top = Math.max(margin, window.innerHeight - previewHeight - margin);
  }
  el.style.left = `${left}px`;
  el.style.top = `${top}px`;
  el.style.width = `${resolvedWidth}px`;
}

function renderSnippetPopup(prefix: string, rect: { left: number; top: number; bottom: number } | null): void {
  const nextKey = `${prefix}\n${snippetPopupIndex}\n${snippetPopupItems.map((snippet) => `${snippet.mode}:${snippet.key}:${snippet.name}`).join("\n")}`;
  if (!snippetPopup.hidden && snippetRenderKey === nextKey) {
    placeFloating(snippetPopup, rect);
    snippetPopup.querySelector(".aaronnote-snippet-option.is-active")?.scrollIntoView({ block: "nearest" });
    return;
  }
  snippetRenderKey = nextKey;
  snippetPopup.innerHTML = "";
  snippetPopupItems.forEach((snippet, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.id = `aaronnote-snippet-option-${index}`;
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", index === snippetPopupIndex ? "true" : "false");
    button.className = index === snippetPopupIndex
      ? "aaronnote-snippet-option is-active"
      : "aaronnote-snippet-option";
    const number = document.createElement("span");
    number.className = "aaronnote-snippet-option-number";
    number.textContent = index < 9 ? String(index + 1) : index === 9 ? "0" : "";
    const key = document.createElement("span");
    key.className = "aaronnote-snippet-option-key";
    key.textContent = snippetLabel(snippet);
    const detail = document.createElement("span");
    detail.className = "aaronnote-snippet-option-detail";
    detail.textContent = completionDetail(snippet);
    button.append(number, key, detail);
    const previewText = completionPreviewText(snippet);
    if (previewText) {
      const preview = document.createElement("span");
      preview.className = "aaronnote-snippet-option-preview";
      preview.textContent = previewText;
      button.appendChild(preview);
    }
    button.addEventListener("mousedown", (event) => {
      event.preventDefault();
      snippetPopupIndex = index;
      chooseSnippetPopupItem();
    });
    button.addEventListener("mouseenter", () => {
      if (snippetPopupIndex === index) return;
      snippetPopupIndex = index;
      snippetRenderKey = "";
      renderSnippetPopup(snippetPopup.dataset.prefix ?? prefix, editor.cursorRect());
    });
    snippetPopup.appendChild(button);
  });
  snippetPopup.dataset.prefix = prefix;
  snippetPopup.setAttribute("role", "listbox");
  snippetPopup.setAttribute("aria-activedescendant", `aaronnote-snippet-option-${snippetPopupIndex}`);
  snippetPopup.hidden = false;
  placeFloating(snippetPopup, rect);
  snippetPopup.querySelector(".aaronnote-snippet-option.is-active")?.scrollIntoView({ block: "nearest" });
}

function snippetContextMode(ctx: ReturnType<typeof editor.cursorContext>): string {
  if (mathAtCursor(ctx)) return "tex-mode";
  return "markdown-mode";
}

function updateSnippetPopup(ctx: ReturnType<typeof editor.cursorContext>): void {
  if (!snippetSuggestionsEnabled || snippetMouseSuppressed) {
    hideSnippetPopup();
    return;
  }
  const active = document.activeElement;
  if (!active || !host.contains(active)) {
    hideSnippetPopup();
    return;
  }
  const domContext = domCompletionContext(ctx.before);
  if (domContext) {
    const matches = matchingDomCompletions(domContext.note, domContext.domPrefix, domContext.parentSegments);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = domContext.domPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(`@${domContext.domPrefix}`, ctx.rect);
    return;
  }
  const tagContext = tagCompletionContext(ctx.before);
  if (tagContext) {
    const matches = matchingTagCompletions(tagContext.note, tagContext.tagPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = tagContext.tagPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(`#${tagContext.tagPrefix}`, ctx.rect);
    return;
  }
  const wikilinkPrefix = wikilinkCompletionPrefix(ctx.before);
  if (wikilinkPrefix != null) {
    const matches = matchingWikilinkCompletions(wikilinkPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = wikilinkPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(`[[${wikilinkPrefix}`, ctx.rect);
    return;
  }
  const roamPrefix = roamCompletionPrefix(ctx.before);
  if (roamPrefix != null) {
    const matches = matchingRoamCompletions(roamPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = roamPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(`roam://${roamPrefix}`, ctx.rect);
    return;
  }
  const pathPrefix = pathCompletionPrefix(ctx.before);
  if (pathPrefix) {
    const matches = matchingPathCompletions(pathPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = pathPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(pathPrefix, ctx.rect);
    return;
  }
  const match = ctx.before.match(/([A-Za-z0-9_:/;.+\\-]{1,40})$/);
  const prefix = match?.[1] ?? "";
  if (!prefix || prefix === snippetSuppressedPrefix) {
    hideSnippetPopup();
    return;
  }
  const mode = snippetContextMode(ctx);
  const matches = matchingSnippets(prefix, mode);
  if (matches.length === 0) {
    hideSnippetPopup();
    return;
  }
  snippetDeleteBefore = prefix.length;
  snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
  snippetPopupItems = matches;
  renderSnippetPopup(prefix, ctx.rect);
}

function chooseSnippetPopupItem(): void {
  const snippet = snippetPopupItems[snippetPopupIndex];
  if (!snippet) return;
  const deleteBefore = snippetDeleteBefore;
  hideSnippetPopup();
  snippetSuppressedPrefix = "";
  insertSnippet(snippet, deleteBefore);
}

function handleSnippetPopupKey(event: KeyboardEvent): boolean {
  if (snippetPopup.hidden) return false;
  if (event.isComposing) return false;
  const active = document.activeElement;
  const target = event.target as Node | null;
  if ((!active || !host.contains(active)) && (!target || !host.contains(target))) {
    hideSnippetPopup();
    return false;
  }
  if (snippetPopupItems.length === 0) {
    hideSnippetPopup();
    return false;
  }
  if (event.metaKey && !event.ctrlKey && !event.altKey && !event.shiftKey && /^\d$/.test(event.key)) {
    const index = event.key === "0" ? 9 : Number(event.key) - 1;
    if (index >= 0 && index < snippetPopupItems.length) {
      event.preventDefault();
      snippetPopupIndex = index;
      chooseSnippetPopupItem();
      return true;
    }
  }
  if (event.key === "ArrowDown") {
    event.preventDefault();
    snippetPopupIndex = (snippetPopupIndex + 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "ArrowUp") {
    event.preventDefault();
    snippetPopupIndex = (snippetPopupIndex + snippetPopupItems.length - 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "PageDown" || event.key === "PageUp") {
    event.preventDefault();
    const delta = event.key === "PageDown" ? 6 : -6;
    snippetPopupIndex = ((snippetPopupIndex + delta) % snippetPopupItems.length + snippetPopupItems.length) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "Home" || event.key === "End") {
    event.preventDefault();
    snippetPopupIndex = event.key === "Home" ? 0 : snippetPopupItems.length - 1;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (event.key === "Enter" || (event.key === "Tab" && !event.shiftKey)) {
    event.preventDefault();
    chooseSnippetPopupItem();
    return true;
  }
  if (event.key === "Escape") {
    event.preventDefault();
    snippetSuppressedPrefix = snippetPopup.dataset.prefix ?? "";
    hideSnippetPopup();
    return true;
  }
  return false;
}

function mathAtCursor(ctx: ReturnType<typeof editor.cursorContext>): { tex: string; display: boolean; rect: { left: number; top: number; bottom: number } | null; rectEnd?: { left: number; top: number; bottom: number } | null } | null {
  const state = editor.view.state;
  const cursor = state.selection.main.from;
  const contextStart = Math.max(0, cursor - ctx.before.length);
  const rectAtSourceOffset = (offset: number) => ctx.rectAtOffset(offset - contextStart);
  const blockRanges = getBlockMathRanges(state);

  const displayMath = rangeAtPosition(cursor, blockRanges);
  if (displayMath && cursor > displayMath.from && cursor < displayMath.to) {
    return {
      tex: displayMath.tex,
      display: true,
      rect: rectAtSourceOffset(displayMath.from),
      rectEnd: rectAtSourceOffset(displayMath.to),
    };
  }

  const sourceLine = state.doc.lineAt(cursor);
  const start = sourceLine.from;
  const line = sourceLine.text;
  INLINE_MATH_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = INLINE_MATH_RE.exec(line)) !== null) {
    const from = start + match.index;
    const to = from + match[0].length;
    const tex = match[1]!;
    if (cursor <= from || cursor >= to) continue;
    if (rangeOverlapsAny(from, to, blockRanges)) continue;
    if (!isLikelyInlineMath(tex)) continue;
    return { tex, display: false, rect: rectAtSourceOffset(from) };
  }
  return null;
}

function mathPreviewKeyFor(math: { tex: string; display: boolean }): string {
  return `${math.display ? "display" : "inline"}\n${math.tex.trim()}`;
}

function mathPreviewPreferredWidth(display: boolean): number {
  const margin = 8;
  const maxWidth = Math.max(220, window.innerWidth - margin * 2);
  const fallback = display ? 640 : 320;
  // Measure the formula's intrinsic width by letting the popup shrink-wrap first.
  // In display mode `.katex-display` and its inner `.katex` are centered blocks that
  // stretch to the container width, so measuring them directly feeds the previous
  // (already-padded) popup width back in and the box creeps wider on every cursor
  // move. `max-content` collapses the container to its content so the measurement is
  // stable; the read happens synchronously before restore, so there is no repaint.
  const prevWidth = mathPreview.style.width;
  mathPreview.style.width = "max-content";
  const child = mathPreview.querySelector<HTMLElement>(".katex-display, .katex, math, mjx-container");
  const natural = child
    ? Math.max(child.scrollWidth, child.getBoundingClientRect().width)
    : Math.max(mathPreview.scrollWidth, fallback);
  mathPreview.style.width = prevWidth;
  if (!Number.isFinite(natural) || natural <= 0) return Math.min(fallback, maxWidth);
  const padding = display ? 40 : 28;
  const minimum = display ? 420 : 280;
  return Math.min(maxWidth, Math.max(Math.min(fallback, maxWidth), minimum, Math.ceil(natural + padding)));
}

function updateMathPreviewOverflow(): void {
  if (mathPreview.hidden || mathPreview.classList.contains("is-error")) return;
  const overflowX = mathPreview.scrollWidth > mathPreview.clientWidth + 2;
  const overflowY = mathPreview.scrollHeight > mathPreview.clientHeight + 2;
  mathPreview.classList.toggle("is-overflowing", overflowX || overflowY);
}

function placeMathPreview(
  anchorRect: { left: number; top: number; bottom: number } | null,
  display: boolean,
  bottomRect?: { bottom: number } | null,
): void {
  mathPreview.classList.remove("is-overflowing");
  placeFloatingAbove(mathPreview, anchorRect, mathPreviewPreferredWidth(display), bottomRect);
  updateMathPreviewOverflow();
}

function scheduleMathPreviewError(
  nextKey: string,
  error: string,
  display: boolean,
): void {
  clearMathPreviewErrorTimer();
  mathPreviewPendingErrorKey = nextKey;
  const message = `Math error: ${formatMathRenderError(error, MATH_PREVIEW_ERROR_MAX_LENGTH)}`;
  mathPreviewErrorTimer.schedule(() => {
    if (mathPreviewPendingErrorKey !== nextKey || mathPreviewKey !== nextKey) return;
    if (vimMode !== "insert" || !editorSurfaceVisible()) return;
    const ctx = editor.cursorContext(display ? 640 : 320);
    const math = mathAtCursor(ctx);
    if (!math || mathPreviewKeyFor(math) !== nextKey) return;
    const anchorRect = math.rect ?? ctx.rect;
    const bottomRect = math.display ? (math.rectEnd ?? anchorRect) : undefined;
    mathPreview.innerHTML = "";
    mathPreview.textContent = message;
    mathPreview.classList.add("is-error");
    mathPreview.classList.toggle("is-display", math.display);
    mathPreview.hidden = false;
    placeFloatingAbove(mathPreview, anchorRect, math.display ? 640 : 320, bottomRect);
  });
}

function updateMathPreview(ctx: ReturnType<typeof editor.cursorContext>, allowNewPreview: boolean): void {
  const math = mathAtCursor(ctx);
  if (!math || math.tex.trim().length === 0) {
    if (!mathPreview.hidden || mathPreviewKey) hideMathPreview();
    return;
  }
  const nextKey = mathPreviewKeyFor(math);
  const anchorRect = math.rect ?? ctx.rect;
  const bottomRect = math.display ? (math.rectEnd ?? anchorRect) : undefined;
  if (mathPreview.hidden && !allowNewPreview) return;
  if (mathPreviewKey === nextKey && !mathPreview.hidden) {
    placeMathPreview(anchorRect, math.display, bottomRect);
    return;
  }
  if (mathPreviewKey !== nextKey && !allowNewPreview) return;
  if (mathPreviewKey !== nextKey) {
    clearMathPreviewErrorTimer();
    mathPreviewKey = nextKey;
    mathPreview.innerHTML = "";
    mathPreview.classList.remove("is-error");
    mathPreview.classList.toggle("is-display", math.display);
    let renderFailed = false;
    renderMathLazy(math.tex.trim(), mathPreview, {
      displayMode: math.display,
      throwOnError: false,
      strict: "ignore",
    }, (error) => {
      renderFailed = true;
      scheduleMathPreviewError(nextKey, error, math.display);
    });
    if (renderFailed) {
      mathPreview.hidden = true;
      return;
    }
  }
  if (mathPreviewPendingErrorKey === nextKey && mathPreview.hidden) return;
  clearMathPreviewErrorTimer();
  mathPreview.classList.remove("is-error");
  mathPreview.hidden = false;
  placeMathPreview(anchorRect, math.display, bottomRect);
  window.requestAnimationFrame(() => {
    if (mathPreviewKey === nextKey && !mathPreview.hidden) {
      placeMathPreview(anchorRect, math.display, bottomRect);
    }
  });
}

function activeEditorSelection(): { text: string; rect: DOMRect } | null {
  if (editor.isSourceMode()) return null;
  const selection = window.getSelection();
  if (!selection || selection.isCollapsed || selection.rangeCount === 0) return null;
  const anchor = selection.anchorNode;
  const focus = selection.focusNode;
  if (!anchor || !focus || !host.contains(anchor) || !host.contains(focus)) return null;
  const logical = editor.getSelection();
  const from = Math.min(logical.from, logical.to);
  const to = Math.max(logical.from, logical.to);
  const text = from < to ? editor.textBetween(from, to) : selection.toString();
  if (!text.trim()) return null;
  const rect = selection.getRangeAt(0).getBoundingClientRect();
  if (rect.width === 0 && rect.height === 0) return null;
  return { text, rect };
}

function updateSelectionTool(active = activeEditorSelection()): void {
  if (!active) {
    selectionTool.hidden = true;
    selectionMore.hidden = true;
    return;
  }
  selectionRoamIdlink.hidden = currentStandalone;
  const margin = 8;
  const width = Math.min(520, Math.max(360, selectionTool.offsetWidth || 440));
  const left = Math.min(
    Math.max(margin, active.rect.left + active.rect.width / 2 - width / 2),
    Math.max(margin, window.innerWidth - width - margin),
  );
  const top = Math.max(margin, active.rect.top - 42);
  selectionTool.style.left = `${left}px`;
  selectionTool.style.top = `${top}px`;
  selectionTool.hidden = false;
}

async function copyActiveSelection(): Promise<void> {
  const active = activeEditorSelection();
  if (!active) return;
  let copied = false;
  try {
    await navigator.clipboard.writeText(active.text);
    copied = true;
  } catch {
    const fallback = document.createElement("textarea");
    fallback.value = active.text;
    fallback.style.position = "fixed";
    fallback.style.left = "-9999px";
    document.body.appendChild(fallback);
    fallback.select();
    copied = document.execCommand("copy");
    fallback.remove();
  }
  setStatus(copied ? "Selection copied" : "Copy failed");
  selectionTool.hidden = true;
}

async function copyMarkdownSelection(): Promise<void> {
  const selection = editor.getMarkdownSelection();
  if (selection.from === selection.to) return;
  await copyText(editor.textBetween(selection.from, selection.to));
  setStatus("Markdown copied");
  selectionTool.hidden = true;
}

function runSelectionCommand(command: string): void {
  if (command === "copy") {
    void copyActiveSelection();
    return;
  }
  if (command === "more") {
    selectionMore.hidden = !selectionMore.hidden;
    return;
  }
  if (command === "find") {
    selectionMore.hidden = true;
    openFindTool();
    return;
  }
  if (command === "insert-roam-idlink") {
    selectionMore.hidden = true;
    void insertRoamIdLink();
    return;
  }
  if (!["bold", "italic", "highlight", "strike", "code", "link"].includes(command)) return;
  runEditorCommand(command as EditorCommand);
  selectionTool.hidden = true;
  selectionMore.hidden = true;
}

type AssistUpdateOptions = {
  snippets?: boolean;
  mathPreview?: boolean;
  toc?: boolean;
  selectionTool?: boolean;
  cursor?: boolean;
};

function scheduleAssistUpdate(options: AssistUpdateOptions = {}): void {
  if (host.hidden) {
    snippetScanRequested = false;
    mathPreviewUpdateRequested = false;
    tocUpdateRequested = false;
    selectionToolUpdateRequested = false;
    vimCursorUpdateRequested = false;
    assistTimer.cancel();
    window.cancelAnimationFrame(assistFrame);
    return;
  }
  const explicit = Object.keys(options).length > 0;
  snippetScanRequested = snippetScanRequested || options.snippets === true;
  mathPreviewUpdateRequested = mathPreviewUpdateRequested || options.mathPreview === true;
  tocUpdateRequested = tocUpdateRequested || options.toc === true;
  selectionToolUpdateRequested = selectionToolUpdateRequested || (explicit ? options.selectionTool === true : true);
  vimCursorUpdateRequested = vimCursorUpdateRequested || options.cursor !== false;
  assistTimer.schedule(() => {
    window.cancelAnimationFrame(assistFrame);
    assistFrame = window.requestAnimationFrame(() => {
      const shouldScanSnippets = snippetScanRequested;
      const shouldUpdateMathPreview = mathPreviewUpdateRequested;
      const shouldUpdateToc = tocUpdateRequested;
      const shouldUpdateSelectionTool = selectionToolUpdateRequested;
      const shouldUpdateVimCursor = vimCursorUpdateRequested;
      snippetScanRequested = false;
      mathPreviewUpdateRequested = false;
      tocUpdateRequested = false;
      selectionToolUpdateRequested = false;
      vimCursorUpdateRequested = false;
      const quickInsertVisible = !quickInsertPopup.hidden;
      const snippetVisible = !snippetPopup.hidden;
      const mathPreviewVisible = !mathPreview.hidden;
      const needsCursorContext = vimMode === "insert" && (
        shouldScanSnippets
        || shouldUpdateMathPreview
        || quickInsertVisible
        || snippetVisible
        || mathPreviewVisible
      );
      const needsWideContext = snippetVisible;
      const ctx = needsCursorContext ? editor.cursorContext(needsWideContext ? 640 : 320) : null;
      if (shouldUpdateVimCursor || ctx) updateVimCursor(vimCursor, editor, vimMode, ctx?.rect);
      if (vimMode !== "insert") {
        hideSnippetPopup();
        hideQuickInsertPopup();
        hideMathPreview();
        selectionTool.hidden = true;
        return;
      }
      if (ctx) {
        const quickOpen = updateQuickInsertPopup(ctx);
        if (quickOpen) {
          hideSnippetPopup();
        }
        else if (shouldScanSnippets || !snippetPopup.hidden) updateSnippetPopup(ctx);
        updateMathPreview(ctx, shouldUpdateMathPreview);
      }
      if (shouldUpdateToc) updateFloatingToc();
      const activeSelection = shouldUpdateSelectionTool ? activeEditorSelection() : null;
      if (shouldUpdateSelectionTool) updateSelectionTool(activeSelection);
    });
  });
}

function updateVimCursorNow(): void {
  updateVimCursor(vimCursor, editor, vimMode);
}

function renderSnippets(): void {
  scheduleAssistUpdate({ snippets: true });
}

async function reloadSnippets(): Promise<void> {
  setStatus("Reloading snippets");
  try {
    const msg = await api.notes.snippets();
    if (!Array.isArray(msg.snippets)) throw new Error(msg.message || "Snippet reload failed");
    clearLeanSnippetCache();
    snippets = msg.snippets.length > 0 ? msg.snippets : demoSnippets;
    hideSnippetPopup();
    renderSnippets();
    setStatus(`Reloaded ${snippets.length} snippets`);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Snippet reload failed");
  }
}

function normalizeRecentNotes(entries: unknown): RecentNote[] {
  if (!Array.isArray(entries)) return [];
  const byFile = new Map<string, RecentNote>();
  for (const item of entries) {
    if (!item || typeof item !== "object") continue;
    const entry = item as Partial<RecentNote>;
    if (typeof entry.file !== "string" || !entry.file) continue;
    if (typeof entry.openedAt !== "number" || !Number.isFinite(entry.openedAt)) continue;
    const current = byFile.get(entry.file);
    if (!current || entry.openedAt > current.openedAt) {
      byFile.set(entry.file, { file: entry.file, openedAt: entry.openedAt });
    }
  }
  return [...byFile.values()].sort((a, b) => b.openedAt - a.openedAt).slice(0, 24);
}

function loadRecentNotes(): RecentNote[] {
  try {
    const raw = window.localStorage.getItem(recentStorageKey);
    const parsed = raw ? JSON.parse(raw) as unknown : [];
    return normalizeRecentNotes(parsed);
  } catch {
    return [];
  }
}

function saveRecentNotesLocalNow(): void {
  recentLocalSaveTimer.cancel();
  try {
    window.localStorage.setItem(recentStorageKey, JSON.stringify(recentNotes.slice(0, 24)));
  } catch {
    // Recent notes are a local convenience; ignore storage failures.
  }
}

function scheduleRecentNotesLocalSave(delay = 650): void {
  recentLocalSaveTimer.schedule(saveRecentNotesLocalNow, undefined, delay);
}

function loadWritingMode(): { focusMode: boolean; typewriterMode: boolean } {
  try {
    const raw = window.localStorage.getItem(writingModeStorageKey);
    const parsed = raw ? JSON.parse(raw) as { focusMode?: unknown; typewriterMode?: unknown } : {};
    return {
      focusMode: parsed.focusMode === true,
      typewriterMode: false,
    };
  } catch {
    return { focusMode: false, typewriterMode: false };
  }
}

function saveWritingMode(): void {
  try {
    window.localStorage.setItem(writingModeStorageKey, JSON.stringify(writingMode));
  } catch {
    // Writing mode is a local preference; ignore storage failures.
  }
}

function loadSnippetSuggestionsEnabled(): boolean {
  try {
    return window.localStorage.getItem(snippetSuggestionsStorageKey) !== "false";
  } catch {
    return true;
  }
}

function saveSnippetSuggestionsEnabled(): void {
  try {
    window.localStorage.setItem(snippetSuggestionsStorageKey, snippetSuggestionsEnabled ? "true" : "false");
  } catch {
    // Snippet suggestions are a local preference; ignore storage failures.
  }
}

function setSnippetSuggestionsEnabled(enabled: boolean): void {
  snippetSuggestionsEnabled = enabled;
  snippetSuppressedPrefix = "";
  saveSnippetSuggestionsEnabled();
  if (!enabled) {
    hideSnippetPopup();
    setStatus("Snippet suggestions disabled");
    return;
  }
  setStatus("Snippet suggestions enabled");
  scheduleAssistUpdate({ snippets: true });
}

function clearSnippetSuggestionState(): void {
  snippetSuppressedPrefix = "";
  snippetMouseSuppressed = false;
  hideSnippetPopup();
  setStatus("Snippet suggestions reset");
  if (snippetSuggestionsEnabled) scheduleAssistUpdate({ snippets: true });
}

function applyWritingMode(): void {
  editor.setWritingMode(writingMode);
  host.classList.toggle("is-focus-mode", writingMode.focusMode);
  root.dataset.focusMode = writingMode.focusMode ? "true" : "false";
  focusModeButton.setAttribute("aria-pressed", writingMode.focusMode ? "true" : "false");
  focusModeButton.classList.toggle("is-active", writingMode.focusMode);
  saveWritingMode();
}

function toggleFocusMode(): void {
  writingMode = { ...writingMode, focusMode: !writingMode.focusMode, typewriterMode: false };
  applyWritingMode();
  setStatus(writingMode.focusMode ? "Focus mode" : "Focus off");
}

function mergeRecentNotes(entries: unknown): void {
  const incoming = normalizeRecentNotes(entries);
  if (incoming.length === 0) return;
  recentNotes = normalizeRecentNotes([...incoming, ...recentNotes]);
  scheduleRecentNotesLocalSave();
  renderRecentNotes();
}

async function loadServerRecentNotes(): Promise<void> {
  try {
    const msg = await api.session.getRecent();
    mergeRecentNotes(msg.recent ?? []);
  } catch {
    // Standalone persistence is best effort; localStorage remains as fallback.
  }
}

async function persistRecentNote(file: string, openedAt: number): Promise<void> {
  try {
    await api.session.touchRecent(file, openedAt);
  } catch {
    // Ignore persistence failures; the UI already updated locally.
  }
}

function touchRecentNote(file: string): void {
  if (!file) return;
  const openedAt = Date.now();
  recentNotes = [
    { file, openedAt },
    ...recentNotes.filter((item) => item.file !== file),
  ].slice(0, 24);
  scheduleRecentNotesLocalSave();
  renderRecentNotes();
  void persistRecentNote(file, openedAt);
}

function normalizeCursorPositions(entries: unknown): Map<string, CursorPosition> {
  const byFile = new Map<string, CursorPosition>();
  if (!Array.isArray(entries)) return byFile;
  for (const item of entries) {
    if (!item || typeof item !== "object") continue;
    const entry = item as Partial<CursorPosition>;
    if (typeof entry.file !== "string" || !entry.file) continue;
    const mode = entry.mode === "source" ? "source" : "markdown";
    const from = typeof entry.from === "number" && Number.isFinite(entry.from) ? Math.max(0, entry.from) : 0;
    const to = typeof entry.to === "number" && Number.isFinite(entry.to) ? Math.max(0, entry.to) : from;
    const scrollY = typeof entry.scrollY === "number" && Number.isFinite(entry.scrollY) ? Math.max(0, entry.scrollY) : 0;
    const updatedAt = typeof entry.updatedAt === "number" && Number.isFinite(entry.updatedAt) ? entry.updatedAt : 0;
    const current = byFile.get(entry.file);
    if (!current || updatedAt > current.updatedAt) {
      byFile.set(entry.file, { file: entry.file, mode, from, to, scrollY, updatedAt });
    }
  }
  return new Map([...byFile.entries()].sort((a, b) => b[1].updatedAt - a[1].updatedAt).slice(0, 240));
}

function cursorPositionStorageKey(file: string): string {
  return `${cursorStorageEntryPrefix}${encodeURIComponent(file)}`;
}

function loadCursorPositions(): Map<string, CursorPosition> {
  try {
    const indexed: CursorPosition[] = [];
    const rawIndex = window.localStorage.getItem(cursorStorageIndexKey);
    const parsedIndex = rawIndex ? JSON.parse(rawIndex) as unknown : [];
    if (Array.isArray(parsedIndex)) {
      for (const file of parsedIndex) {
        if (typeof file !== "string" || !file) continue;
        const rawEntry = window.localStorage.getItem(cursorPositionStorageKey(file));
        if (!rawEntry) continue;
        indexed.push(JSON.parse(rawEntry) as CursorPosition);
      }
    }
    const raw = window.localStorage.getItem(cursorStorageKey);
    const parsed = raw ? JSON.parse(raw) as unknown : [];
    return normalizeCursorPositions([...indexed, ...(Array.isArray(parsed) ? parsed : [])]);
  } catch {
    return new Map();
  }
}

function saveCursorPositionsLocalNow(): void {
  cursorLocalSaveTimer.cancel();
  try {
    const positions = [...cursorPositions.values()].slice(0, 240);
    const files = positions.map((position) => position.file);
    const previousRawIndex = window.localStorage.getItem(cursorStorageIndexKey);
    const previousFiles = previousRawIndex ? JSON.parse(previousRawIndex) as unknown : [];
    window.localStorage.setItem(cursorStorageIndexKey, JSON.stringify(files));
    for (const position of positions) {
      window.localStorage.setItem(cursorPositionStorageKey(position.file), JSON.stringify(position));
    }
    if (Array.isArray(previousFiles)) {
      const keep = new Set(files);
      for (const file of previousFiles) {
        if (typeof file === "string" && !keep.has(file)) {
          window.localStorage.removeItem(cursorPositionStorageKey(file));
        }
      }
    }
    window.localStorage.removeItem(cursorStorageKey);
  } catch {
    // Cursor restore is a local convenience; ignore storage failures.
  }
}

function scheduleCursorPositionsLocalSave(delay = 700): void {
  cursorLocalSaveTimer.schedule(saveCursorPositionsLocalNow, undefined, delay);
}

function mergeCursorPositions(entries: unknown): void {
  const incoming = normalizeCursorPositions(entries);
  if (incoming.size === 0) return;
  cursorPositions = normalizeCursorPositions([...incoming.values(), ...cursorPositions.values()]);
  saveCursorPositionsLocalNow();
}

async function loadServerCursorPositions(): Promise<void> {
  try {
    const msg = await api.session.getPositions();
    mergeCursorPositions(msg.positions ?? []);
  } catch {
    // localStorage remains as fallback.
  }
}

function currentCursorPosition(): CursorPosition | null {
  if (!currentFile) return null;
  const selection = editor.getSelection();
  return {
    file: currentFile,
    mode: editor.isSourceMode() ? "source" : "markdown",
    from: Math.max(0, selection.from),
    to: Math.max(0, selection.to),
    scrollY: Math.max(0, host.scrollTop || window.scrollY),
    updatedAt: Date.now(),
  };
}

function jumpPointLabel(point: CursorPosition): string {
  const note = notes.find((item) => item.file === point.file);
  return note?.title || fileNameFromPath(point.file) || "Jump";
}

function restoreJumpPoint(point: CursorPosition): void {
  pendingEquationTag = "";
  pendingInlineTag = "";
  pendingDomTarget = "";
  pendingTodoFocus = null;
  if (point.file !== currentFile) {
    cursorPositions.set(point.file, point);
    void openStandaloneFile(point.file);
    showEditorPage();
    return;
  }
  showEditorPage();
  editor.setSelection(point.from, point.to);
  editor.revealCursor();
  setStatus("Jumped back");
  scheduleAssistUpdate({ cursor: true, toc: true });
}

function renderJumpStackPanel(): void {
  jumpStackList.innerHTML = "";
  jumpStackCount.textContent = String(jumpStack.length);
  const points = [...jumpStack].map((point, index) => ({ point, index })).reverse();
  if (points.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-jump-stack-empty";
    empty.textContent = "Jump stack empty";
    jumpStackList.append(empty);
    return;
  }
  for (const { point, index } of points) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "aaronnote-jump-stack-item";
    button.dataset.jumpIndex = String(index);
    const title = document.createElement("span");
    title.className = "aaronnote-jump-stack-title";
    title.textContent = jumpPointLabel(point);
    const detail = document.createElement("span");
    detail.className = "aaronnote-jump-stack-detail";
    detail.textContent = `${fileNameFromPath(point.file)}:${point.from}`;
    button.append(title, detail);
    button.addEventListener("mousedown", (event) => event.preventDefault());
    button.addEventListener("click", () => {
      const target = jumpStack[index];
      if (!target) return;
      setJumpStack(jumpStack.slice(0, index));
      jumpStackPanel.hidden = true;
      restoreJumpPoint(target);
    });
    jumpStackList.append(button);
  }
}

function positionJumpStackPanel(): void {
  const width = Math.min(360, Math.max(280, window.innerWidth - 24));
  const left = Math.max(12, Math.round((window.innerWidth - width) / 2));
  jumpStackPanel.style.left = `${left}px`;
  jumpStackPanel.style.top = "54px";
  jumpStackPanel.style.width = `${width}px`;
}

function updateJumpStackUi(): void {
  if (!jumpStackPanel.hidden) {
    renderJumpStackPanel();
    positionJumpStackPanel();
  }
}

function setJumpStack(next: CursorPosition[]): void {
  jumpStack = next.slice(-jumpStackLimit);
  updateJumpStackUi();
}

function toggleJumpStackPanel(): void {
  jumpStackPanel.hidden = !jumpStackPanel.hidden;
  if (!jumpStackPanel.hidden) {
    renderJumpStackPanel();
    positionJumpStackPanel();
  }
}

function pushJumpPoint(): void {
  const point = currentCursorPosition();
  if (!point) return;
  const key = `${point.file}:${point.from}:${point.to}:${point.mode}`;
  const last = jumpStack[jumpStack.length - 1];
  const lastKey = last ? `${last.file}:${last.from}:${last.to}:${last.mode}` : "";
  if (key === lastKey) return;
  setJumpStack([...jumpStack, point]);
}

function jumpBack(): void {
  const point = jumpStack[jumpStack.length - 1];
  if (!point) {
    setStatus("Jump stack empty");
    return;
  }
  setJumpStack(jumpStack.slice(0, -1));
  jumpStackPanel.hidden = true;
  restoreJumpPoint(point);
}

function persistCursorPosition(position: CursorPosition, keepalive = false): void {
  api.session.savePosition(position, keepalive);
}

function saveCursorPositionNow(options: { keepalive?: boolean; force?: boolean } = {}): void {
  cursorSaveTimer.cancel();
  const position = currentCursorPosition();
  if (!position) return;
  const key = `${position.file}:${position.mode}:${position.from}:${position.to}:${Math.round(position.scrollY)}`;
  if (!options.force && key === lastCursorSaveKey) return;
  lastCursorSaveKey = key;
  cursorPositions.set(position.file, position);
  cursorPositions = normalizeCursorPositions([...cursorPositions.values()]);
  if (options.force || options.keepalive) saveCursorPositionsLocalNow();
  else scheduleCursorPositionsLocalSave();
  persistCursorPosition(position, options.keepalive === true);
}

function scheduleCursorPositionSave(delay = 500): void {
  if (!currentFile || !editorSurfaceVisible()) return;
  cursorSaveTimer.schedule(() => saveCursorPositionNow(), undefined, delay);
}

function restoreCursorPosition(file: string): boolean {
  const position = cursorPositions.get(file);
  if (!position) return false;
  const max = editor.isSourceMode()
    ? editor.getMarkdown().length
    : editor.view.state.doc.length;
  const from = Math.max(0, Math.min(position.from, max));
  const to = Math.max(0, Math.min(position.to, max));
  window.requestAnimationFrame(() => {
    editor.setSelection(from, to);
    host.scrollTop = position.scrollY;
    window.scrollTo({ top: position.scrollY, behavior: "instant" as ScrollBehavior });
    scheduleAssistUpdate();
  });
  return true;
}

function flushSaveKeepalive(): void {
  if (!currentFile) return;
  if (saveConflictActive) {
    rememberDraft();
    return;
  }
  if (editRevision === savedRevision) return;
  const run = saveEpoch.begin();
  api.notes.saveKeepalive({
    file: currentFile,
    content: editor.getMarkdown(),
    mode: editor.isSourceMode() ? "source" : "markdown",
    clientId: saveClientId,
    seq: run.id,
    baseMtimeMs: currentFileMtimeMs,
    refresh: "deferred",
  });
}

function flushState(options: { keepalive?: boolean } = {}): void {
  saveCursorPositionNow({ keepalive: options.keepalive === true, force: true });
  saveCursorPositionsLocalNow();
  saveRecentNotesLocalNow();
  flushDraftRemember();
  saveDebounce.cancel();
  flushSaveKeepalive();
}

function renderRecentNotes(): void {
  if (!notesToolVisible("recent")) return;
  filesystemBrowser.renderRecent();
}

function roamNotes(): NoteSummary[] {
  return notes.filter((note) => note.roam);
}

function collapseFilesystemGroups(): void {
  filesystemBrowser.collapseAll();
}

function expandFilesystemGroups(): void {
  filesystemBrowser.expandAll();
}

function renderNotes(): void {
  if (!host.hidden) syncEditorRoamLinkStatus();
  if (notesPage.hidden) return;
  const activeTool = activeNotesTool();
  if (activeTool === "filesystem") filesystemBrowser.render();
  else if (activeTool === "recent") filesystemBrowser.renderRecent();
}

type LeanProjectInfo = {
  ok?: boolean;
  message?: string;
  notesRoot?: string;
  projectRoot?: string;
  toolchain?: string;
  lakefile?: string;
  hasMakefile?: boolean;
  leanVersion?: string;
  lakeVersion?: string;
  packages?: Array<{ name?: string; inputRev?: string; rev?: string }>;
  cache?: { state?: string; message?: string };
};

function setLeanProjectBusy(busy: boolean): void {
  for (const button of leanProjectRoot.querySelectorAll<HTMLButtonElement>("[data-lean-project-command], [data-lean-project-refresh]")) {
    button.disabled = busy;
  }
}

function renderLeanProjectInfo(raw: LeanProjectInfo): void {
  if (raw.ok === false) {
    leanProjectStatus.textContent = raw.message || "Lean project unavailable";
    leanProjectOutput.textContent = raw.message || "Lean project unavailable";
    return;
  }
  leanProjectPath.textContent = raw.projectRoot || "No project root";
  leanProjectToolchain.textContent = raw.toolchain ? `toolchain ${raw.toolchain}` : "toolchain missing";
  leanProjectLeanVersion.textContent = raw.leanVersion || "Lean unavailable";
  leanProjectLakeVersion.textContent = raw.lakeVersion || "Lake unavailable";
  const packages = raw.packages ?? [];
  leanProjectPackageCount.textContent = String(packages.length);
  leanProjectStatus.textContent = raw.cache?.message || (raw.hasMakefile ? "Makefile ready" : "Makefile missing");
  leanProjectPackages.replaceChildren(...packages.slice(0, 18).map((pkg) => {
    const row = document.createElement("div");
    row.className = "aaronnote-lean-project-package";
    const name = document.createElement("strong");
    name.textContent = pkg.name || "package";
    const rev = document.createElement("span");
    rev.textContent = [pkg.inputRev, pkg.rev].filter(Boolean).join(" · ");
    row.append(name, rev);
    return row;
  }));
  if (packages.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-lean-project-empty";
    empty.textContent = "No Lake packages in manifest.";
    leanProjectPackages.replaceChildren(empty);
  }
}

async function refreshLeanProjectInfo(): Promise<void> {
  if (!notesToolVisible("lean")) return;
  if (!api.lean.available()) {
    leanProjectOutput.textContent = "Lean IPC unavailable.";
    return;
  }
  leanProjectStatus.textContent = "Loading...";
  try {
    const raw = await api.lean.request("project-info") as LeanProjectInfo;
    renderLeanProjectInfo(raw);
    leanProjectOutput.textContent = raw.ok === false
      ? (raw.message || "Lean project unavailable")
      : "Use the buttons above to run make targets from the notes root.";
  } catch (err) {
    leanProjectOutput.textContent = err instanceof Error ? err.message : "Lean project info failed";
  }
}

async function runLeanProjectCommand(target: string): Promise<void> {
  if (!api.lean.available()) {
    leanProjectOutput.textContent = "Lean IPC unavailable.";
    return;
  }
  setLeanProjectBusy(true);
  leanProjectStatus.textContent = `Running make ${target}...`;
  leanProjectOutput.textContent = `$ make ${target}\n`;
  try {
    const raw = await api.lean.request("project-command", { target }) as { ok?: boolean; message?: string; output?: string };
    const output = `$ make ${target}\n\n${raw.output || raw.message || ""}`;
    leanProjectStatus.textContent = raw.message || (raw.ok === false ? "Command failed" : "Command finished");
    await refreshLeanProjectInfo();
    leanProjectOutput.textContent = output;
  } catch (err) {
    const message = err instanceof Error ? err.message : "Lean command failed";
    leanProjectStatus.textContent = message;
    leanProjectOutput.textContent += `\n${message}`;
  } finally {
    setLeanProjectBusy(false);
  }
}

function syncShowAllButton(): void {
  notesShowAllButton.classList.toggle("is-active", showAllFilesystemEntries);
  notesShowAllButton.setAttribute("aria-pressed", showAllFilesystemEntries ? "true" : "false");
  notesShowAllButton.textContent = showAllFilesystemEntries ? "隐藏附件" : "显示所有";
}

async function loadAgendaTodos(force = false): Promise<void> {
  if (!notesToolVisible("agenda")) return;
  await agendaManager.load(force);
}

function scheduleRenderAgenda(): void {
  if (!notesToolVisible("agenda")) return;
  agendaManager.scheduleRender();
}

function renderAgenda(): void {
  if (!notesToolVisible("agenda")) return;
  agendaManager.render();
}

function renderGraph(): void {
  if (!graphToolVisible()) return;
  graphPanel.render();
}

function scheduleRenderGraph(delay = 120): void {
  if (!graphToolVisible()) return;
  graphPanel.scheduleRender(delay);
}

function scheduleRenderNotes(): void {
  if (!notesToolVisible("filesystem")) return;
  filesystemBrowser.scheduleRender();
}

async function openStandaloneFile(file: string): Promise<void> {
  if (/\.lean$/i.test(file)) {
    setStatus("Lean files are edited manually");
    return;
  }
  setStatus("Opening");
  try {
    const msg = await api.notes.open(file);
    applyOpen(msg);
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Open failed");
  }
}

function applyOpen(msg: Extract<Inbound, { type: "open" }>, options: { preserveFocus?: boolean } = {}): void {
  saveCursorPositionNow({ force: true });
  flushDraftRemember();
  saveDebounce.cancel();
  noteCssDebounce.cancel();
  saveEpoch.cancel();
  saveConflictActive = false;
  hideDraftRecovery();
  currentFile = msg.file || "";
  currentStandalone = msg.standalone === true;
  currentFileMtimeMs = Number(msg.mtimeMs) || 0;
  currentFileSize = Number(msg.size) || 0;
  editRevision = 0;
  savedRevision = 0;
  root.dataset.standalone = currentStandalone ? "true" : "false";
  const storedPosition = currentFile ? cursorPositions.get(currentFile) : undefined;
  const largeOpen = (msg.content?.length ?? 0) >= LARGE_RENDERED_OPEN_BYTES;
  currentMode = storedPosition?.mode ?? (msg.mode === "source" || largeOpen ? "source" : "markdown");
  fileLabel.textContent = currentFile || "Scratch";
  syncCurrentFileUrl();
  notesButton.hidden = false;
  relationButton.hidden = false;
  agendaButton.hidden = false;
  updateJumpStackUi();
  if (standaloneHiddenNotesTool(activeNotesTool())) showNotesTool("filesystem");
  touchRecentNote(currentFile);

  if (Array.isArray(msg.notes)) {
    applyIndexPayload(msg);
    renderNotes();
    if (notesToolVisible("agenda")) void loadAgendaTodos(true);
    if (graphToolVisible()) renderGraph();
  }
  if (Array.isArray(msg.snippets)) {
    snippets = msg.snippets.length > 0 ? msg.snippets : demoSnippets;
    renderSnippets();
  }
  if (Array.isArray(msg.templates)) templates = msg.templates;
  if (!Array.isArray(msg.notes) && notes.length === 0) void refreshNotesIndex();
  if (!Array.isArray(msg.snippets) && snippets.length === 0) void reloadSnippets();

  if (currentMode === "source" && !editor.isSourceMode()) editor.toggleSource();
  if (currentMode === "markdown" && editor.isSourceMode()) editor.toggleSource();
  syncSourceUi();
  if (leanNotesRoot && currentFile && !currentStandalone) {
    setLeanNotePath(editor.view, currentFile, leanNotesRoot);
  } else {
    setLeanNotePath(editor.view, "", "");
  }
  const kindValue = msg.kind ?? currentNote()?.kind ?? noteKindFromMarkdown(msg.content ?? "");
  prepareNoteKindRender(kindValue);
  updateNoteCss(msg.content ?? "");
  applyingRemoteContent = true;
  try {
    editor.setMarkdown(msg.content ?? "", { history: "reset" });
  } finally {
    applyingRemoteContent = false;
  }
  offerDraftRecovery(currentFile, msg.content ?? "");
  void applyNoteKindAssets(kindValue).finally(() => updateNoteCss());
  const equationTag = normalizeEquationTag(pendingEquationTag);
  pendingEquationTag = "";
  const inlineTag = normalizeInlineTag(pendingInlineTag);
  pendingInlineTag = "";
  const domTarget = normalizeDomTargetPath(pendingDomTarget);
  pendingDomTarget = "";
  const openAtTop = pendingOpenAtTop;
  pendingOpenAtTop = false;
  const todoFocus = pendingTodoFocus && pendingTodoFocus.file === currentFile ? pendingTodoFocus : null;
  if (todoFocus) pendingTodoFocus = null;
  const jumped = !options.preserveFocus && equationTag ? jumpToEquationTag(equationTag) : false;
  const inlineJumped = !options.preserveFocus && !jumped && inlineTag ? jumpToInlineTag(inlineTag) : false;
  const domJumped = !options.preserveFocus && !jumped && !inlineJumped && domTarget ? jumpToDomTarget(domTarget) : false;
  const todoJumped = !options.preserveFocus && !jumped && !inlineJumped && !domJumped && todoFocus ? jumpToTodoSource(todoFocus.source, todoFocus.index) : false;
  const templateSelection = msg.selection && Number.isFinite(Number(msg.selection.from))
    ? { from: Math.max(0, Number(msg.selection.from)), to: Math.max(0, Number(msg.selection.to ?? msg.selection.from)) }
    : null;
  const selectedTemplate = !options.preserveFocus && !jumped && !inlineJumped && !domJumped && !todoJumped && templateSelection
    ? (editor.setMarkdownSelection(templateSelection.from, templateSelection.to), true)
    : false;
  if (openAtTop && !jumped && !inlineJumped && !domJumped && !todoJumped && !selectedTemplate) {
    editor.setMarkdownSelection(0, 0);
    host.scrollTop = 0;
  }
  const restored = !options.preserveFocus && !openAtTop && !jumped && !inlineJumped && !domJumped && !todoJumped && !selectedTemplate && currentFile ? restoreCursorPosition(currentFile) : false;
  if (!options.preserveFocus && !jumped && !inlineJumped && !domJumped && !todoJumped && !selectedTemplate && !restored) editor.focus();
  vim.setMode("insert");
  if (equationTag) {
    setStatus(jumped ? `Equation tag ${equationTag}` : `Equation tag not found: ${equationTag}`);
  } else if (inlineTag) {
    setStatus(inlineJumped ? `Inline anchor ${inlineTag}` : `Inline anchor not found: ${inlineTag}`);
  } else if (domTarget) {
    setStatus(domJumped ? `DOM target ${domTarget}` : `DOM target not found: ${domTarget}`);
  } else if (todoFocus) {
    setStatus(todoJumped ? "Todo focused" : "Todo source not found");
  } else {
    setStatus(currentMode === "source" ? "Source mode" : "Ready");
  }
  updateFloatingToc();
  syncLocalGraphAvailability();
  if (leanNotesRoot && currentFile && !currentStandalone) {
    setLeanNotePath(editor.view, currentFile, leanNotesRoot);
    leanPanel.setNote(currentFile, leanNotesRoot);
    const hasLean4 = scanMarkdownLeanPlaceholders(editor.getMarkdown()).length > 0;
    leanTriggerBtn.hidden = !hasLean4;
    if (!hasLean4) {
      leanPanel.hide();
      leanPanelRoot.classList.add("lean-panel--gone");
    } else {
      leanPanelRoot.classList.remove("lean-panel--gone");
    }
  } else {
    setLeanNotePath(editor.view, "", "");
    leanPanel.setNote("", "");
    leanPanel.hide();
    leanPanelRoot.classList.add("lean-panel--gone");
    leanTriggerBtn.hidden = true;
  }
  syncPanelSwitcher();
  if (!relationPanel.hidden) renderRelationPanel(true);
  scheduleAssistUpdate();
  void loadPathSuggestions();
}

async function bootstrapStandalone(): Promise<void> {
  try {
    const rawRequestedFile = params.get("file") ?? undefined;
    const requestedFile = rawRequestedFile && !/\.lean$/i.test(rawRequestedFile) ? rawRequestedFile : undefined;
    const msg = await api.notes.bootstrap(requestedFile);
    applyOpen(msg);
    if (rawRequestedFile && !requestedFile) setStatus("Lean files are edited manually");
  } catch (err) {
    setStatus(err instanceof Error ? err.message : "Bootstrap failed");
  }
}

window.AaronnoteDesktop?.onOpenFile?.((file) => {
  if (!file) return;
  void openStandaloneFile(file);
});
window.AaronnoteDesktop?.ready?.();

function editorSurfaceVisible(): boolean {
  return !host.hidden;
}

function editorOwnsEventTarget(event: Event): boolean {
  const target = event.target as Node | null;
  return editorSurfaceVisible() && !!target && host.contains(target);
}

function editorOwnsKeyTarget(event: KeyboardEvent): boolean {
  if (!editorOwnsEventTarget(event)) return false;
  const target = event.target instanceof Element ? event.target : event.target instanceof Text ? event.target.parentElement : null;
  const editable = target?.closest<HTMLElement>("input, textarea, select, [contenteditable='true']");
  return !editable || editable.classList.contains("cm-content");
}

function runEditorCommand(command: EditorCommand, value = ""): void {
  if (!editor.runCommand(command, value)) return;
  scheduleAssistUpdate();
  scheduleCursorPositionSave();
  setStatus(command.replace(/-/g, " "));
}

function runHistoryCommand(kind: "undo" | "redo"): void {
  const active = document.activeElement instanceof HTMLElement ? document.activeElement : null;
  // When focus is inside an embedded Lean editor, document.activeElement is the
  // shadow host (it cannot pierce shadow DOM), so delegate to the inner editor's
  // own history instead of the outer markdown editor.
  const leanHost = active?.closest<HTMLElement>(".cm-lean-placeholder-widget");
  const leanHistory = (leanHost as (HTMLElement & { __leanHistory?: { undo: () => boolean; redo: () => boolean } }) | null)?.__leanHistory;
  if (leanHistory) {
    if (kind === "undo") leanHistory.undo();
    else leanHistory.redo();
    return;
  }
  const editable = active?.closest<HTMLElement>("input, textarea, select, [contenteditable='true']");
  if (editable && !editable.classList.contains("cm-content")) {
    document.execCommand(kind);
    return;
  }
  const ok = kind === "undo" ? editor.undo() : editor.redo();
  if (!ok) return;
  scheduleAssistUpdate();
  scheduleCursorPositionSave();
}

function historyShortcutKind(event: KeyboardEvent): "undo" | "redo" | null {
  return historyChordKind(event);
}

function buildShortcutCtx(event: KeyboardEvent): ShortcutCtx {
  const blockingOverlayOpen = !snippetPopup.hidden
    || !quickInsertPopup.hidden
    || !selectionTool.hidden
    || !findTool.hidden
    || linkPreview.isOpen()
    || !relationPanel.hidden;
  return {
    editorOwnsTarget: editorOwnsEventTarget(event),
    fromLeanEmbedded: eventFromLeanEmbeddedEditor(event),
    overlayOpen: blockingOverlayOpen,
    vimMode,
    notesTool: activeNotesTool(),
  };
}

function eventFromLeanEmbeddedEditor(event: Event): boolean {
  return event.composedPath().some((node) =>
    node instanceof HTMLElement
    && (
      node.classList.contains("cm-lean-placeholder-widget")
      || node.classList.contains("lean-card")
      || node.classList.contains("lean-host")
    ));
}

document.addEventListener("keydown", (event) => {
  if (eventFromLeanEmbeddedEditor(event)) return;
  const kind = historyShortcutKind(event);
  if (!kind || !editorOwnsKeyTarget(event)) return;
  event.preventDefault();
  event.stopImmediatePropagation();
  const ok = kind === "undo" ? editor.undo() : editor.redo();
  if (!ok) return;
  scheduleAssistUpdate();
  scheduleCursorPositionSave();
}, { capture: true });

document.addEventListener("keydown", (event) => {
  const ctx = buildShortcutCtx(event);
  const ctrlEnter = matchChord(event, { ctrl: true, meta: false, shift: false, alt: false, key: "enter" });
  // Lean toggle: fires from any context including Lean sub-editors.
  if (matchChord(event, { primary: true, shift: false, alt: false, key: "l" })) {
    event.preventDefault();
    event.stopPropagation();
    toggleLeanPanel();
    return;
  }
  // From a Lean sub-editor, only Ctrl+Enter (toggle notes/editor page) passes through.
  if (!ctrlEnter && ctx.fromLeanEmbedded) return;

  if (handleLeanLocationsPickerKey(event)) { event.stopPropagation(); return; }
  if (handleCommandPaletteKey(event)) { event.stopPropagation(); return; }
  if (handleJumpModeKey(event)) return;

  if (matchChord(event, { primary: true, shift: true, alt: false, key: "p" })) {
    event.preventDefault(); event.stopPropagation(); openCommandPalette(); return;
  }
  if (matchChord(event, { primary: true, shift: true, alt: false, key: "s" })) {
    event.preventDefault(); event.stopPropagation(); void checkProse(); return;
  }
  if (matchChord(event, { primary: true, shift: false, alt: false, key: "j" })) {
    event.preventDefault(); event.stopPropagation(); toggleJumpStackPanel(); return;
  }
  if (matchChord(event, { primary: true, shift: true, alt: false, key: "t" })) {
    event.preventDefault(); event.stopPropagation(); void openTodayDaily(); return;
  }
  if (matchChord(event, { primary: true, shift: true, alt: false, key: "l" })) {
    event.preventDefault(); event.stopPropagation(); void insertLeanBlock(); return;
  }

  const plainEscape = matchChord(event, { meta: false, ctrl: false, alt: false, shift: false, key: "escape" });
  const shouldDismissMathPreview = !mathPreview.hidden && !ctx.editorOwnsTarget;
  if (plainEscape && (ctx.overlayOpen || shouldDismissMathPreview)) {
    event.preventDefault(); event.stopPropagation();
    hideEditorOverlays(); closeRelationPanel(); editor.focus();
    return;
  }

  if (ctrlEnter) {
    event.preventDefault(); event.stopPropagation();
    if (!notesPage.hidden && (ctx.notesTool === "filesystem" || ctx.notesTool === "recent")) {
      showEditorPage();
    } else {
      openFilesystemPage();
    }
    return;
  }

  if (
    !notesPage.hidden
    && (ctx.notesTool === "filesystem" || ctx.notesTool === "recent")
    && matchChord(event, { meta: false, ctrl: false, shift: false, alt: false, key: "tab" })
  ) {
    event.preventDefault(); event.stopPropagation();
    showNotesTool(ctx.notesTool === "filesystem" ? "recent" : "filesystem");
    return;
  }

  if (
    ctx.editorOwnsTarget
    && !event.metaKey && !event.ctrlKey && !event.altKey
    && (event.key.length === 1 || ["Backspace", "Delete", "Enter", "Tab"].includes(event.key))
  ) {
    snippetMouseSuppressed = false;
  }

  if (matchChord(event, { primary: true, shift: false, alt: false, key: "f" })) {
    event.preventDefault(); event.stopPropagation(); openFindTool(); return;
  }
  if (matchChord(event, { primary: true, shift: false, alt: false, key: "/" })) {
    event.preventDefault(); event.stopPropagation(); toggleSourceMode(); return;
  }

  if (ctx.vimMode !== "insert" && ctx.editorOwnsTarget
    && matchChord(event, { meta: false, ctrl: false, alt: false, shift: false, key: "/" })) {
    event.preventDefault(); event.stopPropagation(); openFindTool(); return;
  }
  if (matchChord(event, { alt: true, ctrl: false, meta: false, shift: false, key: "t" })) {
    event.preventDefault(); event.stopPropagation(); toc.classList.toggle("is-collapsed"); return;
  }

  if (handleQuickInsertKey(event)) { event.stopPropagation(); return; }
  if (handleSnippetPopupKey(event)) { event.stopPropagation(); return; }

  if (ctx.vimMode === "normal" && ctx.editorOwnsTarget
    && matchChord(event, { meta: false, ctrl: false, alt: false, shift: false, key: "s" })) {
    event.preventDefault(); event.stopPropagation(); startJumpMode(); return;
  }

  if (ctx.editorOwnsTarget) {
    const editorCmd = matchChord(event, { primary: true, alt: false, shift: false, key: "b" }) ? "bold"
      : matchChord(event, { primary: true, alt: false, shift: false, key: "i" }) ? "italic"
      : matchChord(event, { primary: true, alt: false, shift: false, key: "k" }) ? "link"
      : null;
    if (editorCmd) { event.preventDefault(); event.stopPropagation(); runEditorCommand(editorCmd); return; }
  }

  if (matchChord(event, { meta: true, ctrl: false, shift: false, alt: false, key: "t" })) {
    const jupyterTocTag = handleJupyterTocTagCommand();
    event.preventDefault(); event.stopPropagation();
    if (jupyterTocTag === "miss") void handleTagCommand();
    return;
  }

  if (matchChord(event, { meta: true, ctrl: false, shift: false, alt: false, key: "]" })) {
    if (jumpSnippetTabstop()) { event.preventDefault(); event.stopPropagation(); return; }
  }
  if (matchChord(event, { meta: true, ctrl: false, shift: false, alt: false, key: "[" })) {
    if (jumpSnippetTabstopBack()) { event.preventDefault(); event.stopPropagation(); return; }
  }

  if (vim.handleKeyDown(event)) { updateVimCursorNow(); event.stopPropagation(); return; }

  if (matchChord(event, { meta: true, ctrl: false, shift: false, alt: false, key: "s" })) {
    event.preventDefault(); event.stopPropagation(); save(); return;
  }
  if (matchChord(event, { meta: true, ctrl: false, shift: false, alt: false, key: "p" })) {
    event.preventDefault(); event.stopPropagation(); void exportPdf(); return;
  }
}, true);

document.addEventListener("aaronnote:open-url", (event) => {
  const custom = event as CustomEvent<{ href?: string; newWindow?: boolean }>;
  const href = custom.detail?.href;
  if (!href) return;
  event.preventDefault();
  openExternalUrl(href, { newWindow: custom.detail?.newWindow === true });
});

document.addEventListener("aaronnote:preview-url", (event) => {
  const custom = event as CustomEvent<{ href?: string; x?: number; y?: number }>;
  const href = custom.detail?.href;
  if (!href) return;
  event.preventDefault();
  linkPreview.show(href, Number(custom.detail?.x) || window.innerWidth / 2, Number(custom.detail?.y) || 80);
});

document.addEventListener("aaronnote:book-toc-open", (event) => {
  const custom = event as CustomEvent<{ item?: BookTocItem | BookEditorTocItem }>;
  const item = custom.detail?.item;
  if (!item) return;
  event.preventDefault();
  openBookTocItem(item);
});

document.addEventListener("aaronnote:book-include-open", (event) => {
  const custom = event as CustomEvent<{ ref?: string }>;
  const ref = custom.detail?.ref || "";
  if (!ref) return;
  event.preventDefault();
  openBookIncludeRef(ref);
});

document.addEventListener("aaronnote:attachment-context-menu", (event) => {
  const custom = event as CustomEvent<{ href?: string }>;
  const href = custom.detail?.href;
  if (!href) return;
  event.preventDefault();
  void api.shell.showAttachmentMenu(hrefPath(href), currentFile, { href })
    .catch((err) => setStatus(err instanceof Error ? err.message : "Attachment menu failed"));
});

host.addEventListener("contextmenu", (event) => {
  if (event.defaultPrevented) return;
  const anchor = (event.target as Element | null)?.closest<HTMLAnchorElement>("a[href]");
  if (anchor && host.contains(anchor) && primaryPointerModifier(event)) {
    event.preventDefault();
    event.stopPropagation();
    linkPreview.show(anchor.getAttribute("href") || anchor.href, event.clientX, event.clientY);
    return;
  }
  if (!host.contains(event.target as Node | null)) return;
  const attachmentHref = attachmentHrefFromContextMenu(event);
  if (attachmentHref) {
    event.preventDefault();
    event.stopPropagation();
    void api.shell.showAttachmentMenu(hrefPath(attachmentHref), currentFile, { href: attachmentHref })
      .catch((err) => setStatus(err instanceof Error ? err.message : "Attachment menu failed"));
    return;
  }
  const pos = editor.view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (pos != null) {
    const selection = editor.getMarkdownSelection();
    if (pos < selection.from || pos > selection.to) editor.setMarkdownSelection(pos);
  }
  const proseDiagnostics = pos == null ? [] : proseDiagnosticsAt(editor.view, pos);
  event.preventDefault();
  event.stopPropagation();
  void (async () => {
    const leanOptions = leanContextMenuOptions(pos);
    const diagnostics = proseDiagnostics.length > 0
      ? {
        diagnostics: proseDiagnostics.map((diag) => ({
          source: diag.source,
          from: diag.from,
          to: diag.to,
          message: diag.message,
          suggestions: diag.suggestions ?? [],
        })),
      }
      : {};
    const selection = editor.getMarkdownSelection();
    const block = editor.getBlockContext();
    const anchor = (event.target as Element | null)?.closest<HTMLAnchorElement>("a[href]");
    const linkHref = pos == null
      ? ""
      : markdownHrefAt(editor.view.state, pos) || (anchor && host.contains(anchor) ? anchor.getAttribute("href") || anchor.href : "");
    await api.shell.showEditorContextMenu({
      ...leanOptions,
      ...diagnostics,
      x: event.clientX,
      y: event.clientY,
      linkHref,
      hasSelection: selection.from !== selection.to,
      allowRoamIdlink: !currentStandalone,
      blockType: block.type,
      blockCommands: block.commands,
    });
  })()
    .catch((err) => setStatus(err instanceof Error ? err.message : "Context menu failed"));
});

document.addEventListener("mousedown", (event) => {
  const target = event.target as Node | null;
  if (!target) return;
  if (!relationPanel.hidden && !relationPanel.contains(target) && !relationButton.contains(target)) closeRelationPanel();
  if (linkPreview.isOpen() && !linkPreview.element.contains(target)) linkPreview.dismissTransient();
});

document.addEventListener("focusin", (event) => {
  const target = event.target as Node | null;
  if (!target || linkPreview.element.contains(target)) return;
  linkPreview.dismissTransient();
});

document.addEventListener("knowledge:apply-tag", (event) => {
  const tag = String((event as CustomEvent<{ tag?: string }>).detail?.tag || "").trim().replace(/^#/, "");
  if (!tag) return;
  if (graphToolVisible()) {
    graphFilter.value = `tag:${tag}`;
    renderGraph();
    setStatus(`#${tag}`);
    return;
  }
  openTagFilter(tag);
});

window.addEventListener("aaronnote:command", (event) => {
  const detail = (event as CustomEvent<{ command?: string; from?: unknown; to?: unknown; replacement?: unknown; selector?: unknown; editorCommand?: unknown; href?: unknown; text?: unknown; newWindow?: unknown; x?: unknown; y?: unknown }>).detail ?? {};
  const command = detail.command;
  if (command === "new-markdown-note") void createMarkdownNote();
  if (command === "new-roam-node") void createRoamNode();
  if (command === "new-node") void createNode();
  if (command === "open-roam-node") void openRoamNode();
  if (command === "open-today-daily") void openTodayDaily();
  if (command === "insert-roam-idlink") void insertRoamIdLink();
  if (command === "ensure-roam-id") void ensureRoamId();
  if (command === "delete-node") void deleteCurrentNote();
  if (command === "add-meta") void quickAddMeta();
  if (command === "remove-meta") void unregisterMeta();
  if (command === "hide-roam") void hideCurrentRoam();
  if (command === "activate-roam") void activateCurrentRoam();
  if (command === "add-tag") void addTag();
  if (command === "manage-note-tags") void openTagManager();
  if (command === "insert-inline-tag") void insertInlineTag();
  if (command === "tag-context") void handleTagCommand();
  if (command === "tag-manager") void handleTagCommand();
  if (command === "sync-roamdb") void syncRoamDb();
  if (command === "sync-roamdb-full") void syncRoamDbFull();
  if (command === "roam-restore-file-version") void restoreCurrentFileVersion();
  if (command === "roam-git-log") void roamGitLog();
  if (command === "roam-git-status") void roamGitStatus();
  if (command === "roam-commit-now") void roamCommitNow();
  if (command === "roam-push") void roamPush();
  if (command === "undo") runHistoryCommand("undo");
  if (command === "redo") runHistoryCommand("redo");
  if (command === "reload-snippets") void reloadSnippets();
  if (command === "enable-snippet-suggestions") setSnippetSuggestionsEnabled(true);
  if (command === "disable-snippet-suggestions") setSnippetSuggestionsEnabled(false);
  if (command === "reset-snippet-suggestions") clearSnippetSuggestionState();
  if (command === "open-filesystem") openFilesystemPage();
  if (command === "open-roam-graph") showNotesPage("graph");
  if (command === "jump-stack") toggleJumpStackPanel();
  if (command === "jump-back") jumpBack();
  if (command === "open-block-menu") openBlockMenu();
  if (command === "open-lean-block-manager") void openLeanBlockManager();
  if (command === "insert-lean-block") void insertLeanBlock({ selector: String(detail?.selector ?? "") });
  if (command === "clean-lean-block") void cleanCurrentLeanBlock({ tag: String(detail?.tag ?? ""), selector: String(detail?.selector ?? "") });
  if (command === "toggle-lean-panel") toggleLeanPanel();
  if (command === "toggle-jupyter-preview") toggleJupyterPanel();
  if (command === "open-jupyter-preview") void openJupyterPreviewFromHref(String((detail as Record<string, unknown>).href ?? ""));
  if (command === "editor-command") runEditorCommand(String(detail.editorCommand ?? "") as EditorCommand);
  if (command === "preview-link") linkPreview.show(String(detail.href ?? ""), Number(detail.x) || window.innerWidth / 2, Number(detail.y) || 80);
  if (command === "open-link") openExternalUrl(String(detail.href ?? ""), { newWindow: detail.newWindow === true });
  if (command === "copy-text") void copyText(String(detail.text ?? "")).then(() => setStatus("Copied"));
  if (command === "copy-markdown") void copyMarkdownSelection();
  if (command === "paste-plain-text") void navigator.clipboard.readText().then((text) => {
    if (!text) return;
    editor.insertText(text);
    scheduleAssistUpdate();
    scheduleCursorPositionSave();
    setStatus("Pasted plain text");
  }).catch(() => setStatus("Paste failed"));
  if (command === "find") openFindTool();
  if (command === "toggle-source") toggleSourceMode();
  if (command === "restart-lean-server") void restartLeanServerForCurrentNote();
  if (command === "check-prose") void checkProse();
  if (command === "apply-prose-fix") applyProseFixFromCommand(detail);
  if (command === "lean-editor-menu-action") handleLeanEditorMenuAction(detail as Record<string, unknown>);
  if (command === "save-now") save();
  if (command === "flush-state") flushState({ keepalive: true });
});

// Record the current cursor before a Lean LSP navigation so `jumpBack` returns here.
window.addEventListener("aaronnote:lean-push-jump", () => pushJumpPoint());

// Surface Lean navigation/editor status (no results, external-open failures) in the status bar.
window.addEventListener("aaronnote:lean-status", (event) => {
  const message = String((event as CustomEvent<{ message?: string }>).detail?.message ?? "").trim();
  if (message) setStatus(message);
});

// Cross-note Lean jumps: the target region lives in a note that isn't open. Open
// it, then re-dispatch the region-jump a few times while the embedded editor and
// its region (loaded async) come up; the now-mounted widget consumes it.
window.addEventListener("aaronnote:lean-region-jump", (event) => {
  const detail = (event as CustomEvent<{ notePath?: string; leanPath?: string; line?: number; character?: number; tag?: string; selector?: string }>).detail;
  if (!detail?.notePath || detail.notePath === currentFile) return;
  const target = { ...detail };
  void openStandaloneFile(target.notePath as string)
    .then(() => {
      for (const delay of [120, 350, 700, 1200]) {
        window.setTimeout(() => {
          if (currentFile !== target.notePath) return;
          window.dispatchEvent(new CustomEvent("aaronnote:lean-region-jump", { detail: target }));
        }, delay);
      }
    })
    .catch((err) => console.warn("[lean] region jump navigation failed", err));
});

notesButton.addEventListener("click", () => showNotesPage());
relationButton.addEventListener("click", toggleRelationPanel);
agendaButton.addEventListener("click", () => showNotesPage("agenda"));
focusModeButton.addEventListener("click", toggleFocusMode);
syncButton.addEventListener("click", () => void syncRoamDb());
renameRoamTagButton.addEventListener("click", () => void renameRoamTagTool());
deleteRoamTagButton.addEventListener("click", () => void deleteRoamTagTool());
tagOverlapReportButton.addEventListener("click", () => void tagOverlapReportTool());
rewritePathRefsButton.addEventListener("click", () => void rewritePathRefsTool());
ensureRoamIdButton.addEventListener("click", () => void ensureRoamId());
notesCollapseAllButton.addEventListener("click", collapseFilesystemGroups);
notesExpandAllButton.addEventListener("click", expandFilesystemGroups);
notesShowAllButton.addEventListener("click", () => {
  showAllFilesystemEntries = !showAllFilesystemEntries;
  syncShowAllButton();
  renderNotes();
  focusFilesystemRangerSoon();
});
scanUnusedAssetsButton.addEventListener("click", () => void unusedAssetsManager.scan());
trashUnusedAssetsButton.addEventListener("click", () => void unusedAssetsManager.trashSelected());
unusedAssetsSelectAll.addEventListener("change", unusedAssetsManager.toggleSelectAll);
sourceButton.addEventListener("click", toggleSourceMode);
forceSaveButton.addEventListener("click", () => void forceSaveStandalone());
draftRecoverButton.addEventListener("click", recoverPendingDraft);
draftDiscardButton.addEventListener("click", discardPendingDraft);
editorButton.addEventListener("click", showEditorPage);
editorInlineButton.addEventListener("click", showEditorPage);
notesPage.addEventListener("click", (event) => {
  const button = (event.target as Element | null)?.closest<HTMLButtonElement>("[data-notes-tab]");
  if (!button || !notesPage.contains(button)) return;
  showNotesTool(button.dataset.notesTab || "filesystem");
});
leanProjectRoot.addEventListener("click", (event) => {
  const refresh = (event.target as Element | null)?.closest<HTMLButtonElement>("[data-lean-project-refresh]");
  if (refresh) {
    void refreshLeanProjectInfo();
    return;
  }
  const command = (event.target as Element | null)?.closest<HTMLButtonElement>("[data-lean-project-command]");
  if (!command) return;
  void runLeanProjectCommand(command.dataset.leanProjectCommand || "info");
});
tocToggle.addEventListener("click", () => {
  floatingTocPanel.toggle();
});
bookTocToggle.addEventListener("click", () => {
  bookToc.classList.toggle("is-collapsed");
  bookTocToggle.setAttribute("aria-expanded", bookToc.classList.contains("is-collapsed") ? "false" : "true");
  document.body.classList.toggle("book-toc-open", !bookToc.classList.contains("is-collapsed") && !bookToc.hidden);
  bookTocRenderKey = "";
  updateBookToc();
});
relationRefresh.addEventListener("click", () => renderRelationPanel(true));
relationClose.addEventListener("click", closeRelationPanel);
relationPanel.addEventListener("mousedown", (event) => event.stopPropagation());
selectionTool.addEventListener("mousedown", (event) => event.preventDefault());
selectionTool.addEventListener("click", (event) => {
  const button = (event.target as Element | null)?.closest<HTMLButtonElement>("[data-selection-command]");
  if (!button) return;
  event.preventDefault();
  event.stopPropagation();
  runSelectionCommand(button.dataset.selectionCommand || "");
});
findTool.addEventListener("mousedown", (event) => event.stopPropagation());
findTool.addEventListener("keydown", (event) => {
  if (event.key === "Escape") {
    event.preventDefault();
    closeFindTool();
    return;
  }
  if (event.key === "Enter") {
    event.preventDefault();
    findNext(event.shiftKey ? -1 : 1);
  }
});
findQuery.addEventListener("input", () => {
  scheduleFindRefresh();
});
findRegex.addEventListener("change", () => {
  refreshFindMatches();
  if (findMatches.length) selectFindMatch(0);
});
findScope.addEventListener("change", () => {
  refreshFindMatches();
  if (findMatches.length) selectFindMatch(0);
});
findTool.addEventListener("click", (event) => {
  const button = (event.target as Element | null)?.closest<HTMLButtonElement>("[data-find-action]");
  if (!button) return;
  event.preventDefault();
  const action = button.dataset.findAction || "";
  if (action === "prev") findNext(-1);
  if (action === "next") findNext(1);
  if (action === "replace") replaceCurrentFindMatch();
  if (action === "all") replaceAllFindMatches();
  if (action === "close") closeFindTool();
});
commandPalette.addEventListener("mousedown", (event) => {
  const target = event.target as Element | null;
  if (target?.closest("[data-command-close]")) {
    event.preventDefault();
    closeCommandPalette();
  }
});
commandQuery.addEventListener("input", () => {
  commandPaletteIndex = 0;
  commandPaletteRenderKey = "";
  renderCommandPalette();
});
noteFilter.addEventListener("input", scheduleRenderNotes);
agendaFilter.addEventListener("input", scheduleRenderAgenda);
agendaSort.addEventListener("change", scheduleRenderAgenda);
agendaGroup.addEventListener("change", scheduleRenderAgenda);
agendaDone.addEventListener("change", scheduleRenderAgenda);
agendaRefresh.addEventListener("click", () => void loadAgendaTodos(true));
graphFilter.addEventListener("input", () => scheduleRenderGraph());
document.addEventListener("keyup", (event) => {
  linkPreview.dismissTransient();
  if (!editorSurfaceVisible()) return;
  if (event.key !== "Escape") snippetSuppressedPrefix = "";
  if (event.key !== "Escape") quickInsertSuppressedPrefix = "";
  scheduleCursorPositionSave();
  scheduleAssistUpdate({ mathPreview: true, cursor: true });
});
document.addEventListener("mousedown", (event) => {
  const target = event.target as Node | null;
  if (!jumpStackPanel.hidden && target && !jumpStackPanel.contains(target)) {
    jumpStackPanel.hidden = true;
  }
  if (!snippetPopup.hidden && target && !snippetPopup.contains(target)) {
    if (host.contains(target)) snippetMouseSuppressed = true;
    hideSnippetPopup();
  }
  if (quickInsertMode !== "block" || quickInsertPopup.hidden) return;
  if (!target) return;
  if (quickInsertPopup.contains(target)) return;
  hideQuickInsertPopup();
});
document.addEventListener("selectionchange", () => {
  linkPreview.dismissTransient();
  if (!editorSurfaceVisible()) return;
  updateVimCursorNow();
  scheduleCursorPositionSave();
  scheduleAssistUpdate({ mathPreview: true, selectionTool: true, cursor: true });
});
document.addEventListener("mouseup", () => {
  linkPreview.dismissTransient();
  if (!editorSurfaceVisible()) return;
  scheduleCursorPositionSave();
  scheduleAssistUpdate({ mathPreview: true, selectionTool: true, cursor: true });
});
window.addEventListener("resize", () => {
  if (!editorSurfaceVisible()) return;
  updateVimCursorNow();
  if (!jumpStackPanel.hidden) positionJumpStackPanel();
  scheduleAssistUpdate({
    selectionTool: !selectionTool.hidden,
    cursor: true,
  });
});
window.addEventListener("resize", () => {
  if (graphToolVisible()) scheduleRenderGraph(180);
});
window.addEventListener("scroll", () => {
  if (jumpMode) hideJumpOverlay();
  if (!jumpStackPanel.hidden) jumpStackPanel.hidden = true;
  if (!editorSurfaceVisible()) return;
  updateVimCursorNow();
  scheduleCursorPositionSave(700);
  scheduleAssistUpdate({
    selectionTool: !selectionTool.hidden,
    cursor: true,
  });
}, true);
window.addEventListener("beforeunload", () => {
  flushState({ keepalive: true });
});
window.addEventListener("pagehide", () => {
  flushState({ keepalive: true });
});
document.addEventListener("visibilitychange", () => {
  if (document.visibilityState === "hidden") flushState({ keepalive: true });
});

syncShowAllButton();

void Promise.allSettled([loadServerRecentNotes(), loadServerCursorPositions()]).finally(() => {
  void bootstrapStandalone();
});

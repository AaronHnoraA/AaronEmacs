import { type InfoviewActionKind } from '@leanprover/infoview-api'
import { createLeanOfficialInfoviewHost, type LeanOfficialInfoviewHost } from './lean-infoview-host'
import { apiLean } from './api-lean'
import '../node_modules/@leanprover/infoview/dist/index.css'
import './style.css'

function installWebKitPolyfills() {
  const arrayProto = Array.prototype as unknown as {
    toSorted?: <T>(this: T[], compareFn?: (a: T, b: T) => number) => T[]
  }
  if (!arrayProto.toSorted) {
    Object.defineProperty(Array.prototype, 'toSorted', {
      configurable: true, writable: true,
      value<T>(this: T[], compareFn?: (a: T, b: T) => number): T[] {
        return [...this].sort(compareFn)
      },
    })
  }
}

installWebKitPolyfills()

type EmacsTheme = Record<string, string>

const themeProperties: Record<string, string> = {
  fontFamily: '--lean-font-family',
  fontSize: '--lean-font-size',
  bg: '--lean-bg',
  fg: '--lean-fg',
  surface: '--lean-surface',
  surfaceRaised: '--lean-surface-raised',
  border: '--lean-border',
  muted: '--lean-muted',
  accent: '--lean-accent',
  cyan: '--lean-cyan',
  green: '--lean-green',
  yellow: '--lean-yellow',
  red: '--lean-red',
  selection: '--lean-selection',
}

function applyEmacsTheme(theme: EmacsTheme) {
  const root = document.documentElement
  for (const [key, property] of Object.entries(themeProperties)) {
    const value = theme[key]
    if (value) root.style.setProperty(property, key === 'fontSize' ? `${value}px` : value)
  }
  document.body.classList.toggle('vscode-light', theme.mode === 'light')
  document.body.classList.toggle('vscode-dark', theme.mode !== 'light')
}

;(window as unknown as Record<string, unknown>).applyEmacsTheme = applyEmacsTheme

document.body.classList.add('vscode-dark', 'lean-iv-body')
applyEmacsTheme(Object.fromEntries(new URLSearchParams(window.location.search)))

const host = document.getElementById('root')!
host.innerHTML = `
  <div class="lean-shell">
    <header class="lean-toolbar">
      <div class="lean-toolbar-group">
        <button class="lean-tool" data-action="pin" title="Pin current state" aria-label="Pin current state">
          <span class="codicon codicon-pin"></span>
        </button>
        <button class="lean-tool" data-action="pause" title="Pause current state" aria-label="Pause current state">
          <span class="codicon codicon-debug-pause"></span>
        </button>
        <button class="lean-tool" data-action="all-messages" title="Toggle all messages" aria-label="Toggle all messages">
          <span class="codicon codicon-list-unordered"></span>
        </button>
        <button class="lean-tool" data-action="expected" title="Toggle expected type" aria-label="Toggle expected type">
          <span class="codicon codicon-symbol-interface"></span>
        </button>
      </div>
      <div class="lean-status" aria-live="polite">
        <span id="lean-server-state" class="lean-status-pill">starting</span>
        <span id="lean-cursor-state" class="lean-status-text">:1:0</span>
      </div>
      <div class="lean-toolbar-group">
        <button class="lean-tool" data-action="copy" title="Copy tactic state to comment" aria-label="Copy tactic state to comment">
          <span class="codicon codicon-clippy"></span>
        </button>
        <button class="lean-tool" data-action="restart" title="Restart file" aria-label="Restart file">
          <span class="codicon codicon-debug-restart"></span>
        </button>
        <button class="lean-tool is-active" data-action="follow" title="Follow Emacs cursor" aria-label="Follow Emacs cursor">
          <span class="codicon codicon-eye"></span>
        </button>
      </div>
    </header>
    <main id="lean-infoview-root" class="lean-infoview-root"></main>
  </div>
`

const infoviewRoot = document.getElementById('lean-infoview-root')!
const serverStateEl = document.getElementById('lean-server-state')!
const cursorStateEl = document.getElementById('lean-cursor-state')!
const followButton = host.querySelector<HTMLButtonElement>('[data-action="follow"]')!

let infoviewHost: LeanOfficialInfoviewHost | null = null
let followCursor = true
let paused = false
let currentUri: string | null = null

function displayUri(uri: string): string {
  try {
    const path = decodeURIComponent(new URL(uri).pathname)
    const parts = path.split('/').filter(Boolean)
    return parts.length > 0 ? parts[parts.length - 1] : uri
  } catch { return uri }
}

function updateCursorState(uri: string, line: number, character: number) {
  cursorStateEl.textContent = `${displayUri(uri)}:${line + 1}:${character}`
  currentUri = uri
}

function setPaused(next: boolean) {
  paused = next
  const btn = host.querySelector<HTMLButtonElement>('[data-action="pause"]')
  btn?.classList.toggle('is-active', paused)
  btn?.querySelector('.codicon')?.classList.toggle('codicon-debug-continue', paused)
  btn?.querySelector('.codicon')?.classList.toggle('codicon-debug-pause', !paused)
  if (btn) {
    btn.title = paused ? 'Resume current state' : 'Pause current state'
    btn.setAttribute('aria-label', btn.title)
  }
}

function requestAction(kind: InfoviewActionKind) {
  infoviewHost?.requestAction(kind)
}

// Reverse-channel callbacks: infoview → Emacs via standard LSP
async function handleShowDocument(show: unknown) {
  await fetch('/editor/show-document', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(show),
  }).catch(() => {})
}

async function handleApplyEdit(edit: unknown) {
  await fetch('/editor/apply-edit', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ edit }),
  }).catch(() => {})
}

async function handleInsertText(
  text: string,
  kind: unknown,
  pos?: unknown,
) {
  await fetch('/editor/insert-text', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ text, kind, pos }),
  }).catch(() => {})
}

async function handleRestartFile(uri: string) {
  await fetch('/editor/restart-file', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ uri }),
  }).catch(() => {})
}

// Mount infoview host
infoviewHost = createLeanOfficialInfoviewHost(infoviewRoot, {
  showDocument: handleShowDocument,
  applyEdit: handleApplyEdit,
  insertText: handleInsertText,
  restartFile: handleRestartFile,
  onReady: () => { serverStateEl.textContent = 'ready' },
  onContentChange: () => {},
})

// Status updates
apiLean.onStatus((raw) => {
  const data = raw as { kind?: string; message?: string }
  if (data.kind === 'Ready' || data.kind === 'Normal') {
    serverStateEl.textContent = 'ready'
  } else if (data.kind === 'Error' || data.kind === 'Inactive') {
    serverStateEl.textContent = 'stopped'
  } else {
    serverStateEl.textContent = 'starting'
  }
})

// Cursor from Emacs
apiLean.onCursor((raw) => {
  const cursor = raw as { uri: string; line: number; character: number }
  updateCursorState(cursor.uri, cursor.line, cursor.character)
  if (!followCursor || paused) return
  infoviewHost?.setLocation({ uri: cursor.uri, line: cursor.line, character: cursor.character })
})

// Toolbar
host.addEventListener('click', (ev) => {
  const button = (ev.target as HTMLElement).closest<HTMLButtonElement>('.lean-tool')
  if (!button) return
  const action = button.dataset.action
  if (action === 'pin') {
    requestAction('togglePin')
  } else if (action === 'pause') {
    setPaused(!paused)
    requestAction('togglePaused')
  } else if (action === 'all-messages') {
    requestAction('toggleAllMessages')
  } else if (action === 'expected') {
    requestAction('toggleExpectedType')
  } else if (action === 'copy') {
    requestAction('copyToComment')
  } else if (action === 'restart' && currentUri) {
    void handleRestartFile(currentUri)
  } else if (action === 'follow') {
    followCursor = !followCursor
    followButton.classList.toggle('is-active', followCursor)
    followButton.title = followCursor ? 'Follow Emacs cursor' : 'Infoview cursor follow is locked'
    followButton.setAttribute('aria-label', followButton.title)
  }
})

// Expose window.updateCursor for xwidget-webkit-execute-script fast path
;(window as unknown as Record<string, unknown>).updateCursor = (
  uri: string,
  line: number,
  character: number,
) => {
  updateCursorState(uri, line, character)
  if (!followCursor || paused) return
  infoviewHost?.setLocation({ uri, line, character })
}

;(window as unknown as Record<string, unknown>).requestInfoviewAction = (
  kind: InfoviewActionKind,
) => {
  requestAction(kind)
}

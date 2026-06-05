import {
  defaultInfoviewConfig,
  renderInfoview,
  type InfoviewActionKind,
  type InfoviewConfig,
} from '@leanprover/infoview'
import type { InitializeResult } from 'vscode-languageserver-protocol'
import { createEditorApi } from './editor-api'
import '../node_modules/@leanprover/infoview/dist/index.css'
import './style.css'

function installWebKitPolyfills() {
  const arrayProto = Array.prototype as unknown as {
    toSorted?: <T>(this: T[], compareFn?: (a: T, b: T) => number) => T[]
  }

  if (!arrayProto.toSorted) {
    Object.defineProperty(Array.prototype, 'toSorted', {
      configurable: true,
      writable: true,
      value<T>(this: T[], compareFn?: (a: T, b: T) => number): T[] {
        return [...this].sort(compareFn)
      },
    })
  }
}

installWebKitPolyfills()

const editorApi = createEditorApi()

document.body.classList.add('vscode-dark', 'lean-iv-body')

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

// Mount official infoview inside the xwidget shell.
const infoviewApi = renderInfoview(editorApi, infoviewRoot)

let initialized = false
let serverReady = false
let pendingCursor: { uri: string; line: number; character: number } | null = null
let currentCursor: { uri: string; line: number; character: number } | null = null
let followCursor = true
let paused = false
let currentConfig: InfoviewConfig = loadConfig()

function loadConfig(): InfoviewConfig {
  try {
    const raw = localStorage.getItem('lean.infoview.config')
    return raw ? { ...defaultInfoviewConfig, ...JSON.parse(raw) } : defaultInfoviewConfig
  } catch {
    return defaultInfoviewConfig
  }
}

function displayUri(uri: string): string {
  try {
    const path = decodeURIComponent(new URL(uri).pathname)
    const parts = path.split('/').filter(Boolean)
    return parts.length > 0 ? parts[parts.length - 1] : uri
  } catch {
    return uri
  }
}

function updateStatus() {
  if (!currentCursor) {
    cursorStateEl.textContent = ':1:0'
    return
  }
  cursorStateEl.textContent =
    `${displayUri(currentCursor.uri)}:${currentCursor.line + 1}:${currentCursor.character}`
}

async function pushCursor(cursor: { uri: string; line: number; character: number }) {
  currentCursor = cursor
  updateStatus()
  if (!followCursor) return
  const location = {
    uri: cursor.uri,
    range: {
      start: { line: cursor.line, character: cursor.character },
      end: { line: cursor.line, character: cursor.character },
    },
  }
  await infoviewApi.changedCursorLocation(location).catch((err) => {
    console.warn('changedCursorLocation failed', err)
  })
}

async function initializeAtCursor(cursor: { uri: string; line: number; character: number }) {
  if (initialized) {
    await pushCursor(cursor)
    return
  }
  initialized = true
  const location = {
    uri: cursor.uri,
    range: {
      start: { line: cursor.line, character: cursor.character },
      end: { line: cursor.line, character: cursor.character },
    },
  }
  await infoviewApi.initialize(location)
  await pushCursor(cursor)
}

async function acceptCursor(cursor: { uri: string; line: number; character: number }) {
  pendingCursor = cursor
  currentCursor = cursor
  updateStatus()
  if (!serverReady) return
  await initializeAtCursor(cursor)
}

function requestAction(kind: InfoviewActionKind) {
  void infoviewApi.requestedAction({ kind }).catch((err) => {
    console.warn(`infoview action failed: ${kind}`, err)
  })
}

function setPaused(next: boolean) {
  paused = next
  const button = host.querySelector<HTMLButtonElement>('[data-action="pause"]')
  button?.classList.toggle('is-active', paused)
  button?.querySelector('.codicon')?.classList.toggle('codicon-debug-continue', paused)
  button?.querySelector('.codicon')?.classList.toggle('codicon-debug-pause', !paused)
  if (button) {
    button.title = paused ? 'Resume current state' : 'Pause current state'
    button.setAttribute('aria-label', button.title)
  }
}

async function applyConfig() {
  await infoviewApi.changedInfoviewConfig(currentConfig).catch((err) => {
    console.warn('changedInfoviewConfig failed', err)
  })
}

host.addEventListener('click', async (ev) => {
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
  } else if (action === 'restart' && currentCursor?.uri) {
    await editorApi.restartFile(currentCursor.uri)
  } else if (action === 'follow') {
    followCursor = !followCursor
    followButton.classList.toggle('is-active', followCursor)
    followButton.title = followCursor ? 'Follow Emacs cursor' : 'Infoview cursor follow is locked'
    followButton.setAttribute('aria-label', followButton.title)
    if (followCursor && pendingCursor) await pushCursor(pendingCursor)
  }
})

// Forward bridge SSE events → InfoviewApi
editorApi.onServerNotification(async (method, params) => {
  if (method === 'lsp:ready') {
    serverReady = true
    serverStateEl.textContent = 'ready'
    // LSP server initialized — kick off the infoview
    await infoviewApi.serverRestarted(params as InitializeResult)
    await applyConfig()
    if (pendingCursor) await initializeAtCursor(pendingCursor)
  } else if (method === 'emacs:cursor') {
    const cursor = params as { uri: string; line: number; character: number }
    await acceptCursor(cursor)
  } else if (method.startsWith('client:')) {
    await infoviewApi
      .sentClientNotification(method.slice('client:'.length), params)
      .catch(() => {})
  } else {
    await infoviewApi.gotServerNotification(method, params).catch(() => {})
  }
})

// Expose updateCursor for Emacs → xwidget-webkit-execute-script
;(window as unknown as Record<string, unknown>).updateCursor = async (
  uri: string,
  line: number,
  character: number,
) => {
  await acceptCursor({ uri, line, character })
}

;(window as unknown as Record<string, unknown>).requestInfoviewAction = (kind: InfoviewActionKind) => {
  requestAction(kind)
}

;(window as unknown as Record<string, unknown>).setInfoviewConfig = async (
  patch: Partial<InfoviewConfig>,
) => {
  currentConfig = { ...currentConfig, ...patch }
  localStorage.setItem('lean.infoview.config', JSON.stringify(currentConfig))
  await applyConfig()
}

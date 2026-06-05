import type { ClientRequestOptions, EditorApi, InfoviewConfig } from '@leanprover/infoview-api'

type NotificationListener = (method: string, params: unknown) => void

function isRpcError(value: unknown): value is { code: number; message: string } {
  if (!value || typeof value !== 'object') return false
  const candidate = value as { code?: unknown; message?: unknown }
  return typeof candidate.code === 'number' && typeof candidate.message === 'string'
}

// Build an EditorApi backed by the bridge HTTP server (same origin).
// Also returns `onServerNotification` so main.tsx can forward notifications
// to the InfoviewApi once it's set up.
export function createEditorApi(): EditorApi & {
  onServerNotification(cb: NotificationListener): void
} {
  const notifListeners: NotificationListener[] = []
  let lastReadyParams: unknown | null = null

  // SSE stream: receive server → client notifications from bridge
  let es: EventSource | null = null
  function connectSSE() {
    es = new EventSource('/events')
    es.onmessage = (ev) => {
      try {
        const { method, params } = JSON.parse(ev.data) as { method: string; params: unknown }
        if (method === 'lsp:ready') lastReadyParams = params
        notifListeners.forEach((fn) => fn(method, params))
      } catch {}
    }
    es.onerror = () => {
      es?.close()
      setTimeout(connectSSE, 2000)
    }
  }
  connectSSE()

  async function post(path: string, body: unknown, signal?: AbortSignal): Promise<unknown> {
    const r = await fetch(path, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
      signal,
    })
    if (!r.ok) throw Object.assign(new Error(`HTTP ${r.status}`), { status: r.status })
    const text = await r.text()
    return text ? JSON.parse(text) : undefined
  }

  const api: EditorApi = {
    async saveConfig(config: InfoviewConfig) {
      localStorage.setItem('lean.infoview.config', JSON.stringify(config))
    },

    // uri is included for routing but all LSP calls go to the same lake serve
    async sendClientRequest(
      uri: string,
      method: string,
      params: unknown,
      options?: ClientRequestOptions,
    ) {
      const result = await post('/rpc', { uri, method, params }, options?.abortSignal)
      if (isRpcError(result)) throw result
      return result
    },

    async sendClientNotification(uri: string, method: string, params: unknown) {
      await post('/notify', { uri, method, params }).catch(() => {})
    },

    async subscribeServerNotifications(method: string) {
      await post('/subscribe', { method }).catch(() => {})
    },
    async unsubscribeServerNotifications(method: string) {
      await post('/unsubscribe', { method }).catch(() => {})
    },
    async subscribeClientNotifications(_method: string) {},
    async unsubscribeClientNotifications(_method: string) {},

    async copyToClipboard(text: string) {
      await navigator.clipboard.writeText(text).catch(() => {})
    },

    async insertText(text: string, kind: unknown, pos?: unknown) {
      await post('/editor/insert-text', { text, kind, pos }).catch(() => {})
    },
    async applyEdit(te: unknown) {
      await post('/editor/apply-edit', { edits: te }).catch(() => {})
    },
    async showDocument(show: unknown) {
      await post('/editor/show-document', show).catch(() => {})
    },
    async restartFile(uri: string) {
      await post('/editor/restart-file', { uri }).catch(() => {})
    },

    // Bridge server manages RPC sessions and keepAlive timers
    async createRpcSession(uri: string): Promise<string> {
      const result = await post('/create-session', { uri })
      if (isRpcError(result)) throw result
      if (typeof result === 'string') return result
      const sessionId = (result as { sessionId?: unknown } | undefined)?.sessionId
      if (typeof sessionId !== 'string' || sessionId.length === 0) {
        throw new Error('invalid RPC session response')
      }
      return sessionId
    },
    async closeRpcSession(sessionId: string) {
      await post('/close-session', { sessionId }).catch(() => {})
    },
  }

  return {
    ...api,
    onServerNotification(cb: NotificationListener) {
      notifListeners.push(cb)
      if (lastReadyParams !== null) queueMicrotask(() => cb('lsp:ready', lastReadyParams))
    },
  }
}

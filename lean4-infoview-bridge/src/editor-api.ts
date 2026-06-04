import type { EditorApi, InfoviewConfig } from '@leanprover/infoview-api'

type NotificationListener = (method: string, params: unknown) => void

// Build an EditorApi backed by the bridge HTTP server (same origin).
// Also returns `onServerNotification` so main.tsx can forward notifications
// to the InfoviewApi once it's set up.
export function createEditorApi(): EditorApi & {
  onServerNotification(cb: NotificationListener): void
} {
  const notifListeners: NotificationListener[] = []

  // SSE stream: receive server → client notifications from bridge
  let es: EventSource | null = null
  function connectSSE() {
    es = new EventSource('/events')
    es.onmessage = (ev) => {
      try {
        const { method, params } = JSON.parse(ev.data) as { method: string; params: unknown }
        notifListeners.forEach((fn) => fn(method, params))
      } catch {}
    }
    es.onerror = () => {
      es?.close()
      setTimeout(connectSSE, 2000)
    }
  }
  connectSSE()

  async function post(path: string, body: unknown): Promise<unknown> {
    const r = await fetch(path, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    })
    if (!r.ok) throw Object.assign(new Error(`HTTP ${r.status}`), { status: r.status })
    const text = await r.text()
    return text ? JSON.parse(text) : undefined
  }

  const api: EditorApi = {
    async saveConfig(_config: InfoviewConfig) {},

    // uri is included for routing but all LSP calls go to the same lake serve
    async sendClientRequest(uri: string, method: string, params: unknown) {
      return post('/rpc', { uri, method, params })
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

    async insertText(_text: string, _kind: unknown, _pos?: unknown) {},
    async applyEdit(_te: unknown) {},
    async showDocument(_show: unknown) {},
    async restartFile(_uri: string) {},

    // Bridge server manages RPC sessions and keepAlive timers
    async createRpcSession(uri: string): Promise<string> {
      const result = (await post('/create-session', { uri })) as { sessionId: string }
      return result.sessionId
    },
    async closeRpcSession(sessionId: string) {
      await post('/close-session', { sessionId }).catch(() => {})
    },
  }

  return {
    ...api,
    onServerNotification(cb: NotificationListener) {
      notifListeners.push(cb)
    },
  }
}

// api-lean.ts — HTTP/SSE shim matching the surface lean-infoview-host.ts needs.
// All requests go to the same-origin lean-proxy.mjs HTTP server.

type Listener = (data: unknown) => void

const sseListeners = new Map<string, Set<Listener>>()
let sseSource: EventSource | null = null
let reconnectTimer: ReturnType<typeof setTimeout> | null = null

function connectSSE() {
  if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null }
  sseSource = new EventSource('/events')
  sseSource.onmessage = (ev) => {
    try {
      const { method, params } = JSON.parse(ev.data) as { method: string; params: unknown }
      sseListeners.get(method)?.forEach(fn => { try { fn(params) } catch {} })
      sseListeners.get('*')?.forEach(fn => { try { fn({ method, params }) } catch {} })
    } catch {}
  }
  sseSource.onerror = () => {
    sseSource?.close()
    sseSource = null
    reconnectTimer = setTimeout(connectSSE, 2000)
  }
}

function onSSE(method: string, fn: Listener): () => void {
  if (!sseSource && !reconnectTimer) connectSSE()
  if (!sseListeners.has(method)) sseListeners.set(method, new Set())
  sseListeners.get(method)!.add(fn)
  return () => sseListeners.get(method)?.delete(fn)
}

async function post(path: string, body: unknown): Promise<unknown> {
  const res = await fetch(path, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  })
  const text = await res.text()
  return text ? JSON.parse(text) : null
}

export const apiLean = {
  // LSP request from infoview → lake (via proxy)
  async lspRequest(params: { method: string; params?: unknown; timeoutMs?: number }) {
    return post('/rpc', params)
  },

  // LSP notification from infoview → lake (via proxy)
  async lspNotify(params: { method: string; params?: unknown }) {
    await post('/notify', params)
  },

  // RPC session management
  async createRpcSession(params: { uri: string }) {
    return post('/create-session', params)
  },
  async closeRpcSession(params: { sessionId: string }) {
    await post('/close-session', params)
  },

  // Initial status query
  async status(): Promise<unknown> {
    return post('/status', {})
  },

  // Server notifications ($/lean/fileProgress, publishDiagnostics, etc.)
  onNotification(fn: Listener): () => void {
    return onSSE('*', (raw) => {
      const { method, params } = raw as { method: string; params: unknown }
      // Only pass through server notifications — filter out control/client events
      if (method
        && !method.startsWith('client:')
        && method !== 'lsp:ready'
        && method !== 'emacs:cursor'
        && method !== 'lean:status') {
        fn({ method, params })
      }
    })
  },

  // Client notifications Eglot sent to lake (didOpen, didChange, etc.)
  onClientNotification(fn: Listener): () => void {
    return onSSE('*', (raw) => {
      const { method, params } = raw as { method: string; params: unknown }
      if (method?.startsWith('client:')) {
        fn({ method: method.slice('client:'.length), params })
      }
    })
  },

  // Lean server status changes (kind: Ready | Normal | Error | Inactive)
  onStatus(fn: Listener): () => void {
    return onSSE('lean:status', fn)
  },

  // Emacs cursor position updates
  onCursor(fn: Listener): () => void {
    return onSSE('emacs:cursor', fn)
  },
}

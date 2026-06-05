#!/usr/bin/env node
// Lean4 infoview bridge server — no npm deps beyond Node.js built-ins
// Usage: node server.mjs <port> <project-root>

import { createServer } from 'node:http'
import { spawn } from 'node:child_process'
import { readFileSync, existsSync } from 'node:fs'
import { fileURLToPath, pathToFileURL } from 'node:url'
import { dirname, join, extname } from 'node:path'

const __dir = dirname(fileURLToPath(import.meta.url))
const PORT = parseInt(process.argv[2] ?? '0')
const ROOT  = process.argv[3] ?? process.cwd()

function log(message) {
  process.stdout.write(`[lean-iv ${new Date().toISOString()}] ${message}\n`)
}

// ── LSP stdio client ─────────────────────────────────────────────────────────

class LspClient {
  constructor(cmd, args, cwd) {
    this._buf  = Buffer.alloc(0)
    this._pending  = new Map()   // id → {res,rej,timer}
    this._listeners = new Map()  // method → Set<fn>
    this._id = 1
    log(`spawning LSP: ${cmd} ${args.join(' ')} cwd=${cwd}`)
    this.proc = spawn(cmd, args, { cwd, stdio: ['pipe', 'pipe', 'pipe'] })
    this.proc.stdout.on('data', c => this._ingest(c))
    this.proc.stderr.on('data', d => process.stderr.write(d))
    this.proc.on('error', err => {
      log(`LSP process error: ${err.message}`)
      this._rejectAll(err)
    })
    this.proc.on('exit', (code, signal) => {
      log(`LSP process exited: code=${code} signal=${signal}`)
      this._rejectAll(new Error(`server exit ${code ?? signal}`))
    })
  }

  _ingest(chunk) {
    this._buf = Buffer.concat([this._buf, chunk])
    while (true) {
      const sep = this._buf.indexOf('\r\n\r\n')
      if (sep < 0) break
      const hdr = this._buf.slice(0, sep).toString()
      const m   = hdr.match(/Content-Length:\s*(\d+)/i)
      if (!m) { this._buf = this._buf.slice(sep + 4); continue }
      const len = parseInt(m[1])
      if (this._buf.length < sep + 4 + len) break
      const body = this._buf.slice(sep + 4, sep + 4 + len).toString('utf8')
      this._buf  = this._buf.slice(sep + 4 + len)
      try { this._dispatch(JSON.parse(body)) } catch {}
    }
  }

  _dispatch(msg) {
    if (msg.id != null && this._pending.has(msg.id)) {
      const {res,rej,timer} = this._pending.get(msg.id)
      this._pending.delete(msg.id)
      clearTimeout(timer)
      msg.error ? rej(msg.error) : res(msg.result)
    } else if (msg.method) {
      this._listeners.get(msg.method)?.forEach(fn => { try { fn(msg.params) } catch {} })
    }
  }

  _rejectAll(err) {
    for (const {rej,timer} of this._pending.values()) {
      clearTimeout(timer)
      rej(err)
    }
    this._pending.clear()
  }

  _frame(msg) {
    const body  = JSON.stringify(msg)
    const frame = `Content-Length: ${Buffer.byteLength(body, 'utf8')}\r\n\r\n${body}`
    this.proc.stdin.write(frame, 'utf8')
  }

  request(method, params, timeoutMs = 30_000) {
    const id = this._id++
    return new Promise((res, rej) => {
      const timer = setTimeout(() => {
        this._pending.delete(id)
        rej({ code: -32800, message: `${method} timed out after ${timeoutMs}ms` })
      }, timeoutMs)
      this._pending.set(id, {res, rej, timer})
      this._frame({ jsonrpc: '2.0', id, method, params })
    })
  }

  notify(method, params) {
    this._frame({ jsonrpc: '2.0', method, params })
  }

  on(method, fn) {
    if (!this._listeners.has(method)) this._listeners.set(method, new Set())
    this._listeners.get(method).add(fn)
    return () => this._listeners.get(method)?.delete(fn)
  }
}

// ── State ────────────────────────────────────────────────────────────────────

let lsp
let initResult = null          // cached initialize result
let lastCursor = null          // latest Emacs cursor location for late SSE clients
const openDocs     = new Map() // uri → {version, text}
const rpcSessions  = new Map() // sessionId → keepAliveTimer
const sseClients   = new Set() // active SSE response objects

function sseEmit(method, params) {
  const payload = `data: ${JSON.stringify({method, params})}\n\n`
  for (const res of sseClients) {
    try { res.write(payload) } catch { sseClients.delete(res) }
  }
}

// ── LSP lifecycle ─────────────────────────────────────────────────────────────

async function startLsp() {
  log(`initializing LSP for root ${ROOT}`)
  lsp = new LspClient('lake', ['serve'], ROOT)
  lsp.on('$/lean/fileProgress',            p => sseEmit('$/lean/fileProgress', p))
  lsp.on('textDocument/publishDiagnostics', p => sseEmit('textDocument/publishDiagnostics', p))

  initResult = await lsp.request('initialize', {
    processId: process.pid,
    rootUri:  pathToFileURL(ROOT).href,
    capabilities: {
      textDocument: { synchronization: { dynamicRegistration: false } },
    },
  }, 180_000)
  lsp.notify('initialized', {})
  log('LSP initialized')
  // Notify the page that LSP is ready (passes the InitializeResult)
  sseEmit('lsp:ready', initResult)
}

function ensureOpen(uri) {
  if (openDocs.has(uri)) return
  const path = uriToPath(uri)
  if (!existsSync(path)) return
  const text = readFileSync(path, 'utf8')
  openDocs.set(uri, { version: 1, text })
  log(`didOpen ${uri} bytes=${Buffer.byteLength(text, 'utf8')}`)
  const params = {
    textDocument: { uri, languageId: 'lean4', version: 1, text },
  }
  lsp.notify('textDocument/didOpen', params)
  sseEmit('client:textDocument/didOpen', params)
}

function uriToPath(uri) {
  try {
    return fileURLToPath(uri)
  } catch {
    return decodeURIComponent(uri.replace(/^file:\/\//, ''))
  }
}

function syncDoc(uri, text) {
  if (!openDocs.has(uri)) {
    openDocs.set(uri, { version: 1, text })
    log(`didOpen ${uri} bytes=${Buffer.byteLength(text, 'utf8')}`)
    const params = {
      textDocument: { uri, languageId: 'lean4', version: 1, text },
    }
    lsp.notify('textDocument/didOpen', params)
    sseEmit('client:textDocument/didOpen', params)
    return
  }
  const prev = openDocs.get(uri)
  if (prev.text === text) return
  const version = prev.version + 1
  openDocs.set(uri, { version, text })
  log(`didChange ${uri} version=${version} bytes=${Buffer.byteLength(text, 'utf8')}`)
  const params = {
    textDocument: { uri, version },
    contentChanges: [{ text }],
  }
  lsp.notify('textDocument/didChange', params)
  sseEmit('client:textDocument/didChange', params)
}

// ── HTTP helpers ──────────────────────────────────────────────────────────────

const MIME = {
  '.html': 'text/html; charset=utf-8',
  '.js':   'application/javascript; charset=utf-8',
  '.css':  'text/css; charset=utf-8',
  '.svg':  'image/svg+xml',
  '.ttf':  'font/ttf',
  '.woff2':'font/woff2',
  '.json': 'application/json; charset=utf-8',
  '.ico':  'image/x-icon',
  '.png':  'image/png',
}

function serveStatic(res, relPath) {
  const full = join(__dir, 'dist', relPath)
  if (!existsSync(full)) { res.writeHead(404); res.end('not found'); return }
  res.setHeader('Content-Type', MIME[extname(full)] ?? 'application/octet-stream')
  res.setHeader(
    'Cache-Control',
    extname(full) === '.html'
      ? 'no-cache'
      : 'public, max-age=31536000, immutable',
  )
  res.writeHead(200); res.end(readFileSync(full))
}

function jsonResp(res, data, status = 200) {
  res.setHeader('Content-Type', 'application/json')
  res.writeHead(status); res.end(JSON.stringify(data))
}

function readJSON(req) {
  return new Promise((res, rej) => {
    let s = ''
    req.on('data', d => s += d)
    req.on('end', () => { try { res(JSON.parse(s)) } catch(e) { rej(e) } })
    req.on('error', rej)
  })
}

// ── HTTP server ───────────────────────────────────────────────────────────────

const http = createServer(async (req, res) => {
  res.setHeader('Access-Control-Allow-Origin', '*')
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type')
  if (req.method === 'OPTIONS') { res.writeHead(204); res.end(); return }

  const url = new URL(req.url, 'http://x')

  // SSE stream ────────────────────────────────────────────────────────────────
  if (url.pathname === '/events' && req.method === 'GET') {
    res.setHeader('Content-Type', 'text/event-stream')
    res.setHeader('Cache-Control', 'no-cache')
    res.setHeader('Connection', 'keep-alive')
    res.writeHead(200)
    sseClients.add(res)
    // Late xwidget clients need the cursor before lsp:ready so the official
    // infoview is initialized at a real Lean location, not at an empty :1:0.
    if (lastCursor) res.write(`data: ${JSON.stringify({method:'emacs:cursor',params:lastCursor})}\n\n`)
    // If LSP already initialized, notify immediately.
    if (initResult) res.write(`data: ${JSON.stringify({method:'lsp:ready',params:initResult})}\n\n`)
    req.on('close', () => sseClients.delete(res))
    return
  }

  // LSP request ───────────────────────────────────────────────────────────────
  if (url.pathname === '/rpc' && req.method === 'POST') {
    try {
      const { uri, method, params } = await readJSON(req)
      if (!lsp) {
        jsonResp(res, { code: -32098, message: 'Lean server is not ready' })
        return
      }
      if (uri) ensureOpen(uri)
      log(`rpc ${method} uri=${uri ?? ''}`)
      const result = await lsp.request(method, params)
      jsonResp(res, result)
    } catch (err) {
      log(`rpc error: ${String(err?.message ?? err)}`)
      const code = err?.code ?? -32000
      jsonResp(res, { code, message: String(err?.message ?? err) }, 200)
    }
    return
  }

  // LSP notification ──────────────────────────────────────────────────────────
  if (url.pathname === '/notify' && req.method === 'POST') {
    try {
      const { uri, method, params } = await readJSON(req)
      if (lsp) { if (uri) ensureOpen(uri); lsp.notify(method, params) }
      log(`notify ${method} uri=${uri ?? ''}`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Subscribe / unsubscribe (SSE handles all, these are no-ops) ───────────────
  if ((url.pathname === '/subscribe' || url.pathname === '/unsubscribe') && req.method === 'POST') {
    res.writeHead(200); res.end(); return
  }

  // RPC session management (bridge manages keepAlive) ─────────────────────────
  if (url.pathname === '/create-session' && req.method === 'POST') {
    try {
      const { uri } = await readJSON(req)
      if (!lsp) {
        jsonResp(res, { code: -32098, message: 'Lean server is not ready' })
        return
      }
      ensureOpen(uri)
      log(`create RPC session uri=${uri}`)
      const result = await lsp.request('$/lean/rpc/connect', { uri })
      const sessionId = typeof result === 'string' ? result : result?.sessionId
      if (!sessionId) {
        throw { code: -32000, message: `invalid RPC session response: ${JSON.stringify(result)}` }
      }
      const timer = setInterval(() => {
        lsp.notify('$/lean/rpc/keepAlive', { uri, sessionId })
      }, 20_000)
      rpcSessions.set(sessionId, timer)
      jsonResp(res, { sessionId })
    } catch (err) {
      log(`create RPC session error: ${String(err?.message ?? err)}`)
      const code = err?.code ?? -32000
      jsonResp(res, { code, message: String(err?.message ?? err) }, 200)
    }
    return
  }

  if (url.pathname === '/close-session' && req.method === 'POST') {
    try {
      const { sessionId } = await readJSON(req)
      const timer = rpcSessions.get(sessionId)
      if (timer) { clearInterval(timer); rpcSessions.delete(sessionId) }
      log(`close RPC session id=${sessionId}`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Cursor sync from Emacs (open + sync doc, no response needed) ──────────────
  if (url.pathname === '/cursor' && req.method === 'POST') {
    try {
      const { uri, text, line, character } = await readJSON(req)
      if (lsp && uri) {
        typeof text === 'string' ? syncDoc(uri, text) : ensureOpen(uri)
      }
      if (uri && Number.isFinite(line) && Number.isFinite(character)) {
        lastCursor = { uri, line, character }
        sseEmit('emacs:cursor', lastCursor)
      }
      log(`cursor uri=${uri ?? ''} line=${line ?? ''} char=${character ?? ''} text=${typeof text === 'string' ? Buffer.byteLength(text, 'utf8') : 0}`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Editor reverse channel (infoview → Emacs via stdout EMACS_CMD lines) ─────
  if (url.pathname.startsWith('/editor/') && req.method === 'POST') {
    try {
      const body = await readJSON(req)
      const cmd  = url.pathname.slice('/editor/'.length)  // e.g. 'show-document'
      process.stdout.write(`EMACS_CMD=${JSON.stringify({ cmd, ...body })}\n`)
      log(`editor reverse: cmd=${cmd}`)
    } catch (err) {
      log(`editor reverse error: ${String(err?.message ?? err)}`)
    }
    res.writeHead(200); res.end(); return
  }

  // Static files (Vite dist/) ─────────────────────────────────────────────────
  const p = url.pathname === '/' ? 'index.html' : url.pathname.replace(/^\//, '')
  serveStatic(res, p)
})

// ── Start ─────────────────────────────────────────────────────────────────────

http.listen(PORT, '127.0.0.1', async () => {
  const port = http.address().port
  process.stdout.write(`LEAN_INFOVIEW_PORT=${port}\n`)
  log(`HTTP bridge listening on 127.0.0.1:${port}`)
  try {
    await startLsp()
    process.stderr.write(`lean4-infoview-bridge :${port}  root: ${ROOT}\n`)
  } catch (e) {
    process.stderr.write(`LSP init failed: ${e.message}\n`)
    process.exit(1)
  }
})

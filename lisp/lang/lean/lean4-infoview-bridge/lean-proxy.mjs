#!/usr/bin/env node
// lean-proxy.mjs — transparent LSP proxy between Eglot and lake serve.
// Eglot connects to this process via stdio; this process spawns lake serve
// and forwards all LSP traffic in both directions.  The same Lean session is
// tapped to drive the official @leanprover/infoview over HTTP+SSE.
//
// Usage: node lean-proxy.mjs --root ROOT --gateway-url URL
//        --gateway-binding ID [-- lake serve]
//
// Stdout carries only LSP frames for Eglot.  All logging goes to stderr.
// Control-plane readiness and cursor messages use Emacs' shared gateway.

import { createServer } from 'node:http'
import { spawn } from 'node:child_process'
import { readFileSync, existsSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { dirname, join, extname, resolve } from 'node:path'

const __dir = dirname(fileURLToPath(import.meta.url))

// ── Arg parsing ───────────────────────────────────────────────────────────────

function parseArgs(argv) {
  const r = {
    root: process.cwd(),
    gatewayUrl: null,
    gatewayBinding: null,
    gatewayClientId: 'lean-infoview',
    downstream: [],
  }
  let i = 0
  while (i < argv.length) {
    if (argv[i] === '--root')      { r.root = argv[++i]; i++; continue }
    if (argv[i] === '--gateway-url') { r.gatewayUrl = argv[++i]; i++; continue }
    if (argv[i] === '--gateway-binding') { r.gatewayBinding = argv[++i]; i++; continue }
    if (argv[i] === '--gateway-client-id') { r.gatewayClientId = argv[++i]; i++; continue }
    if (argv[i] === '--')          { r.downstream = argv.slice(i + 1); break }
    i++
  }
  return r
}

const args = parseArgs(process.argv.slice(2))
const ROOT          = resolve(args.root)
const DOWNSTREAM    = args.downstream.length ? args.downstream : ['lake', 'serve']

const log = (...p) => process.stderr.write(`[lean-proxy] ${p.join(' ')}\n`)

let gatewaySocket = null
let gatewayRetry = null

function gatewaySend(message) {
  if (gatewaySocket?.readyState === WebSocket.OPEN)
    gatewaySocket.send(JSON.stringify(message))
}

function handleGatewayMessage(raw) {
  let message
  try { message = JSON.parse(String(raw)) } catch { return }
  if (!message?.method) return
  const respond = (result) => {
    if (Object.prototype.hasOwnProperty.call(message, 'id'))
      gatewaySend({ jsonrpc: '2.0', id: message.id, result })
  }
  if (message.method === 'lean.cursor') {
    const { uri, line, character } = message.params ?? {}
    if (uri && Number.isFinite(line) && Number.isFinite(character)) {
      lastCursor = { uri, line, character }
      sseEmit('emacs:cursor', lastCursor)
    }
    respond({ ok: true })
  } else if (Object.prototype.hasOwnProperty.call(message, 'id')) {
    gatewaySend({
      jsonrpc: '2.0',
      id: message.id,
      error: { code: -32601, message: 'Method not found' },
    })
  }
}

function connectGateway(port) {
  if (!args.gatewayUrl || !args.gatewayBinding) {
    log('gateway arguments missing')
    return
  }
  clearTimeout(gatewayRetry)
  gatewaySocket = new WebSocket(args.gatewayUrl)
  gatewaySocket.addEventListener('open', () => {
    gatewaySend({
      jsonrpc: '2.0',
      id: 'register',
      method: 'gateway.register',
      params: {
        bindingId: args.gatewayBinding,
        clientId: args.gatewayClientId,
        instanceId: `lean-${process.pid}`,
        provides: ['lean.cursor'],
        endpoint: {
          host: '127.0.0.1',
          port,
          url: `http://127.0.0.1:${port}`,
        },
      },
    })
    log('registered with Emacs gateway')
  })
  gatewaySocket.addEventListener(
    'message', event => handleGatewayMessage(event.data))
  gatewaySocket.addEventListener(
    'error', event =>
      log(`gateway error: ${event?.message ?? 'connection failed'}`))
  gatewaySocket.addEventListener('close', () => {
    gatewaySocket = null
    gatewayRetry = setTimeout(() => connectGateway(port), 1000)
  })
}

// ── LSP framing ───────────────────────────────────────────────────────────────

class LspFramer {
  constructor(onMsg) { this._buf = Buffer.alloc(0); this._cb = onMsg }
  feed(chunk) {
    this._buf = Buffer.concat([this._buf, Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk)])
    for (;;) {
      const sep = this._buf.indexOf('\r\n\r\n')
      if (sep < 0) break
      const m = this._buf.subarray(0, sep).toString().match(/Content-Length:\s*(\d+)/i)
      if (!m) { this._buf = this._buf.subarray(sep + 4); continue }
      const len = +m[1]
      if (this._buf.length < sep + 4 + len) break
      const body = this._buf.subarray(sep + 4, sep + 4 + len).toString('utf8')
      this._buf = this._buf.subarray(sep + 4 + len)
      try { this._cb(JSON.parse(body)) } catch(e) { log('parse error:', e.message) }
    }
  }
}

function lspFrame(msg) {
  const body = JSON.stringify(msg)
  return `Content-Length: ${Buffer.byteLength(body, 'utf8')}\r\n\r\n${body}`
}

// ── Channel: Eglot (stdin/stdout of this process) ─────────────────────────────

function toEglot(msg) {
  process.stdout.write(lspFrame(msg))
}

// ── Lake process ──────────────────────────────────────────────────────────────

let lakeProc = null

function toLake(msg) {
  if (lakeProc?.stdin?.writable) lakeProc.stdin.write(lspFrame(msg))
}

// ── Shared session state ──────────────────────────────────────────────────────

let initResult   = null          // cached from lake's initialize response
let initEglotId  = null          // Eglot's initialize request id (to capture response)
let clientShutdown = false       // client asked for LSP shutdown/exit — lake may go
const PROXY_EXIT_DELAY_MS = 250  // grace period so the final SSE status frame lands
let ivsrvSeq     = 0             // proxy→Eglot server→client request ids
let ivSeq        = 0             // proxy→lake infoview request ids (string "iv:N")

const ivPending   = new Map()   // "iv:N"    → {resolve,reject,timer}
const ivsrvPending = new Map()  // "ivsrv:N" → {resolve}

const eglotOpenDocs = new Set() // URIs opened by Eglot (didOpen forwarded to lake)

const sseClients   = new Set()
const rpcSessions  = new Map()  // sessionId → keepAlive timer
const progressCache    = new Map()
const diagnosticsCache = new Map()
const pendingEglotProgress    = new Map()
const pendingEglotDiagnostics = new Map()
let progressFlushTimer = null
let diagnosticsFlushTimer = null
let lastCursor = null

// ── SSE helpers ───────────────────────────────────────────────────────────────

function sseEmit(method, params) {
  const payload = `data: ${JSON.stringify({ method, params })}\n\n`
  for (const res of sseClients) {
    try { res.write(payload) } catch { sseClients.delete(res) }
  }
}

function flushEglotProgress() {
  progressFlushTimer = null
  for (const params of pendingEglotProgress.values())
    toEglot({ jsonrpc: '2.0', method: '$/lean/fileProgress', params })
  pendingEglotProgress.clear()
}

function flushEglotDiagnostics() {
  diagnosticsFlushTimer = null
  for (const params of pendingEglotDiagnostics.values())
    toEglot({ jsonrpc: '2.0', method: 'textDocument/publishDiagnostics', params })
  pendingEglotDiagnostics.clear()
}

function queueEglotNotification(msg) {
  if (msg.method === '$/lean/fileProgress') {
    const uri = msg.params?.textDocument?.uri ?? ''
    if (!uri) return false
    pendingEglotProgress.set(uri, msg.params)
    progressFlushTimer ??= setTimeout(flushEglotProgress, 75)
    return true
  }
  if (msg.method === 'textDocument/publishDiagnostics') {
    const uri = msg.params?.uri ?? ''
    if (!uri) return false
    pendingEglotDiagnostics.set(uri, diagnosticsCache.get(uri) ?? msg.params)
    if (diagnosticsFlushTimer) clearTimeout(diagnosticsFlushTimer)
    diagnosticsFlushTimer = setTimeout(flushEglotDiagnostics, 100)
    return true
  }
  return false
}

// ── Tap lake → Eglot notifications ───────────────────────────────────────────

function tapServerMsg(msg) {
  if (!msg.method) return
  if (msg.method === '$/lean/fileProgress') {
    const uri = msg.params?.textDocument?.uri ?? ''
    if (uri) progressCache.set(uri, msg.params)
    sseEmit('$/lean/fileProgress', msg.params)
  } else if (msg.method === 'textDocument/publishDiagnostics') {
    const uri = msg.params?.uri ?? ''
    if (uri) {
      const previous = diagnosticsCache.get(uri)?.diagnostics ?? []
      const incoming = msg.params?.diagnostics ?? []
      diagnosticsCache.set(uri, {
        ...msg.params,
        diagnostics: msg.params?.isIncremental ? [...previous, ...incoming] : incoming,
        isIncremental: false,
      })
    }
    sseEmit('textDocument/publishDiagnostics', msg.params)
  }
}

// ── Tap Eglot → lake client notifications ────────────────────────────────────

function tapClientMsg(msg) {
  if (!msg.method) return
  if (msg.method === 'textDocument/didOpen') {
    const uri = msg.params?.textDocument?.uri
    if (uri) eglotOpenDocs.add(uri)
    sseEmit('client:textDocument/didOpen', msg.params)
  } else if (msg.method === 'textDocument/didChange') {
    sseEmit('client:textDocument/didChange', msg.params)
  } else if (msg.method === 'textDocument/didClose') {
    const uri = msg.params?.textDocument?.uri
    if (uri) {
      eglotOpenDocs.delete(uri)
      progressCache.delete(uri)
      diagnosticsCache.delete(uri)
      pendingEglotProgress.delete(uri)
      pendingEglotDiagnostics.delete(uri)
    }
    sseEmit('client:textDocument/didClose', msg.params)
  }
}

// ── From lake ─────────────────────────────────────────────────────────────────

function fromLake(msg) {
  const hasId     = Object.prototype.hasOwnProperty.call(msg, 'id')
  const hasMethod = Object.prototype.hasOwnProperty.call(msg, 'method')
  const hasResult = Object.prototype.hasOwnProperty.call(msg, 'result')
    || Object.prototype.hasOwnProperty.call(msg, 'error')

  if (hasResult && !hasMethod) {
    // Response
    const id = msg.id
    if (typeof id === 'string' && id.startsWith('iv:')) {
      // Response to proxy's infoview request — resolve pending
      const p = ivPending.get(id)
      if (p) {
        ivPending.delete(id)
        clearTimeout(p.timer)
        msg.error ? p.reject(msg.error) : p.resolve(msg.result)
      }
      return  // do NOT forward to Eglot
    }
    // Response to Eglot's request — forward; tap initialize result
    if (id === initEglotId && initResult === null) {
      initResult = msg.result
      initEglotId = null
      log('LSP initialized, infoview ready')
      sseEmit('lsp:ready', initResult)
      sseEmit('lean:status', { kind: 'Ready', message: 'Lean server ready', initializeResult: initResult })
    }
    toEglot(msg)
    return
  }

  if (hasMethod && hasId) {
    // Server→client request from lake — forward to Eglot for handling
    toEglot(msg)
    return
  }

  if (hasMethod && !hasId) {
    // Notification from lake — forward to Eglot, tap for SSE
    tapServerMsg(msg)
    if (!queueEglotNotification(msg)) toEglot(msg)
    return
  }
}

// ── From Eglot ────────────────────────────────────────────────────────────────

function fromEglot(msg) {
  const hasId     = Object.prototype.hasOwnProperty.call(msg, 'id')
  const hasMethod = Object.prototype.hasOwnProperty.call(msg, 'method')
  const hasResult = Object.prototype.hasOwnProperty.call(msg, 'result')
    || Object.prototype.hasOwnProperty.call(msg, 'error')

  if (hasResult && !hasMethod) {
    // Response from Eglot
    const id = msg.id
    if (typeof id === 'string' && id.startsWith('ivsrv:')) {
      // Response to proxy's server→client request (showDocument/applyEdit)
      ivsrvPending.get(id)?.resolve(msg.result ?? null)
      ivsrvPending.delete(id)
      return  // do NOT forward to lake
    }
    // Response to lake's server→client request — forward to lake
    toLake(msg)
    return
  }

  if (hasMethod && hasId) {
    // Request from Eglot — record initialize id, forward to lake
    if (msg.method === 'initialize' && initResult === null) {
      initEglotId = msg.id
    }
    // An orderly `shutdown' means lake's coming exit is expected, not a crash.
    if (msg.method === 'shutdown') clientShutdown = true
    toLake(msg)
    return
  }

  if (hasMethod && !hasId) {
    // Notification from Eglot — tap, forward to lake
    if (msg.method === 'exit') clientShutdown = true
    tapClientMsg(msg)
    toLake(msg)
    return
  }
}

// ── Proxy→lake infoview request (iv: namespace) ───────────────────────────────

function ivRequest(method, params, timeoutMs = 30_000) {
  if (!lakeProc?.stdin?.writable) {
    return Promise.reject(new Error('lake serve is not running'))
  }
  const id = `iv:${++ivSeq}`
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      ivPending.delete(id)
      reject({ code: -32800, message: `${method} timed out after ${timeoutMs}ms` })
    }, timeoutMs)
    ivPending.set(id, { resolve, reject, timer })
    toLake({ jsonrpc: '2.0', id, method, params })
  })
}

// ── Proxy→Eglot server→client request (ivsrv: namespace) ─────────────────────

function ivsrvRequest(method, params) {
  const id = `ivsrv:${++ivsrvSeq}`
  return new Promise((resolve) => {
    ivsrvPending.set(id, { resolve })
    toEglot({ jsonrpc: '2.0', id, method, params })
  })
}

function ivsrvNotify(method, params) {
  toEglot({ jsonrpc: '2.0', method, params })
}

// ── Ensure URI open (for infoview /rpc requests to non-Eglot files) ──────────

const proxyOpenDocs = new Set()

async function ensureOpen(uri) {
  if (eglotOpenDocs.has(uri) || proxyOpenDocs.has(uri)) return
  const path = uriToPath(uri)
  if (!path || !existsSync(path)) return
  const text = readFileSync(path, 'utf8')
  proxyOpenDocs.add(uri)
  toLake({ jsonrpc: '2.0', method: 'textDocument/didOpen',
           params: { textDocument: { uri, languageId: 'lean4', version: 1, text } } })
  sseEmit('client:textDocument/didOpen',
          { textDocument: { uri, languageId: 'lean4', version: 1, text } })
  log(`proxy didOpen ${uri}`)
}

function uriToPath(uri) {
  try { return fileURLToPath(uri) } catch {
    if (uri.startsWith('file://')) return decodeURIComponent(uri.slice(7))
    return null
  }
}

// ── Lake startup ──────────────────────────────────────────────────────────────

function killDownstream(signal = 'SIGTERM') {
  if (!lakeProc || lakeProc.pid == null) return
  // lakeProc (lean --server / lake serve) forks a `lean --worker` per open
  // file; killing only the direct child leaves those workers running.
  // Since lakeProc is spawned detached (own process group), signal the
  // whole group so workers die with it.
  try { process.kill(-lakeProc.pid, signal) }
  catch { try { lakeProc.kill(signal) } catch { /* already gone */ } }
}

// The client's transport is this proxy, not lake.  Surviving lake's death would
// leave a live stdio pipe that keeps the client believing its workspace is
// healthy while every request stalls forever.  The proxy caches no document
// text, so it cannot re-sync a fresh lake behind the client's back either — the
// honest recovery is to die with lake and let the client's own restart policy
// start a new pair.  The delay lets the final SSE status frame reach any
// connected infoview page.
let proxyExiting = false

function exitWithLake(reason) {
  if (clientShutdown || proxyExiting) return
  proxyExiting = true
  log(`${reason}; exiting so the client can restart the server`)
  setTimeout(() => {
    gatewaySocket?.close()
    process.exit(1)
  }, PROXY_EXIT_DELAY_MS)
}

function startLake() {
  const [cmd, ...cmdArgs] = DOWNSTREAM
  log(`spawning: ${cmd} ${cmdArgs.join(' ')} cwd=${ROOT}`)
  lakeProc = spawn(cmd, cmdArgs, { cwd: ROOT, stdio: ['pipe','pipe','pipe'], detached: true })
  const lakeFramer = new LspFramer(fromLake)
  lakeProc.stdout.on('data', c => lakeFramer.feed(c))
  lakeProc.stderr.on('data', d => process.stderr.write(d))
  lakeProc.on('error', err => {
    log(`lake error: ${err.message}`)
    sseEmit('lean:status', { kind: 'Error', message: err.message })
    // A failed spawn never reaches the `exit' handler below, so report it the
    // same way instead of idling as a transport with nothing behind it.
    exitWithLake(`lake could not be started (${err.message})`)
  })
  lakeProc.on('exit', (code, signal) => {
    log(`lake exited: code=${code} signal=${signal}`)
    lakeProc = null
    initResult = null
    for (const { reject, timer } of ivPending.values()) {
      clearTimeout(timer); reject(new Error(`lake exited (${signal ?? code ?? 'unknown'})`))
    }
    ivPending.clear()
    if (progressFlushTimer) clearTimeout(progressFlushTimer)
    if (diagnosticsFlushTimer) clearTimeout(diagnosticsFlushTimer)
    progressFlushTimer = null
    diagnosticsFlushTimer = null
    pendingEglotProgress.clear()
    pendingEglotDiagnostics.clear()
    for (const t of rpcSessions.values()) clearInterval(t)
    rpcSessions.clear()
    sseEmit('lean:status', {
      kind: 'Error',
      message: `Lean server exited (${signal ?? code ?? 'unknown'})`,
    })
    exitWithLake(`lake is gone (${signal ?? code ?? 'unknown'})`)
  })
  // Eglot stdin feeds the proxy; proxy feeds lake
  const eglotFramer = new LspFramer(fromEglot)
  process.stdin.on('data', c => eglotFramer.feed(c))
  process.stdin.on('end', () => { killDownstream(); process.exit(0) })
}

// ── HTTP helpers ──────────────────────────────────────────────────────────────

const MIME = {
  '.html': 'text/html; charset=utf-8',
  '.js':   'application/javascript; charset=utf-8',
  '.css':  'text/css; charset=utf-8',
  '.svg':  'image/svg+xml', '.ttf': 'font/ttf', '.woff2': 'font/woff2',
  '.json': 'application/json; charset=utf-8', '.ico': 'image/x-icon', '.png': 'image/png',
}

function serveStatic(res, relPath) {
  const full = join(__dir, 'dist', relPath)
  if (!existsSync(full)) { res.writeHead(404); res.end('not found'); return }
  res.setHeader('Content-Type', MIME[extname(full)] ?? 'application/octet-stream')
  res.setHeader('Cache-Control', extname(full) === '.html' ? 'no-cache' : 'public,max-age=31536000,immutable')
  res.writeHead(200); res.end(readFileSync(full))
}

function jsonResp(res, data, status = 200) {
  res.setHeader('Content-Type', 'application/json')
  res.writeHead(status); res.end(JSON.stringify(data))
}

function readJSON(req) {
  return new Promise((ok, fail) => {
    let s = ''
    req.on('data', d => s += d)
    req.on('end', () => { try { ok(JSON.parse(s)) } catch(e) { fail(e) } })
    req.on('error', fail)
  })
}

// ── HTTP server ───────────────────────────────────────────────────────────────

const httpServer = createServer(async (req, res) => {
  res.setHeader('Access-Control-Allow-Origin', '*')
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type')
  if (req.method === 'OPTIONS') { res.writeHead(204); res.end(); return }

  const url = new URL(req.url, 'http://x')

  // SSE ───────────────────────────────────────────────────────────────────────
  if (url.pathname === '/events' && req.method === 'GET') {
    res.setHeader('Content-Type', 'text/event-stream')
    res.setHeader('Cache-Control', 'no-cache')
    res.setHeader('Connection', 'keep-alive')
    res.writeHead(200)
    sseClients.add(res)
    if (lastCursor)
      res.write(`data: ${JSON.stringify({ method: 'emacs:cursor', params: lastCursor })}\n\n`)
    if (initResult) {
      res.write(`data: ${JSON.stringify({ method: 'lsp:ready', params: initResult })}\n\n`)
      res.write(`data: ${JSON.stringify({ method: 'lean:status',
        params: { kind: 'Ready', message: 'Lean server ready', initializeResult: initResult } })}\n\n`)
    }
    for (const p of progressCache.values())
      res.write(`data: ${JSON.stringify({ method: '$/lean/fileProgress', params: p })}\n\n`)
    for (const p of diagnosticsCache.values())
      res.write(`data: ${JSON.stringify({ method: 'textDocument/publishDiagnostics', params: p })}\n\n`)
    req.on('close', () => sseClients.delete(res))
    return
  }

  // Status ────────────────────────────────────────────────────────────────────
  if (url.pathname === '/status' && req.method === 'POST') {
    jsonResp(res, {
      running: Boolean(lakeProc),
      initializeResult: initResult,
      kind: initResult ? 'Ready' : (lakeProc ? 'Normal' : 'Inactive'),
      message: initResult ? 'Lean server ready' : (lakeProc ? 'Starting...' : 'Not started'),
    })
    return
  }

  // LSP request (infoview → lake) ────────────────────────────────────────────
  if (url.pathname === '/rpc' && req.method === 'POST') {
    try {
      const { uri, method, params, timeoutMs } = await readJSON(req)
      if (!lakeProc) { jsonResp(res, { code: -32098, message: 'Lean server not ready' }); return }
      if (uri) await ensureOpen(uri)
      log(`rpc ${method} uri=${uri ?? ''}`)
      const result = await ivRequest(method, params, timeoutMs ?? 30_000)
      jsonResp(res, result)
    } catch(err) {
      const code = err?.code ?? -32000
      jsonResp(res, { code, message: String(err?.message ?? err) })
    }
    return
  }

  // LSP notification (infoview → lake) ────────────────────────────────────────
  if (url.pathname === '/notify' && req.method === 'POST') {
    try {
      const { uri, method, params } = await readJSON(req)
      if (lakeProc) { if (uri) await ensureOpen(uri); toLake({ jsonrpc: '2.0', method, params }) }
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Subscribe / unsubscribe (SSE handles all, no-ops) ─────────────────────────
  if ((url.pathname === '/subscribe' || url.pathname === '/unsubscribe') && req.method === 'POST') {
    res.writeHead(200); res.end(); return
  }

  // RPC session ───────────────────────────────────────────────────────────────
  if (url.pathname === '/create-session' && req.method === 'POST') {
    try {
      const { uri } = await readJSON(req)
      if (!lakeProc) { jsonResp(res, { code: -32098, message: 'Lean server not ready' }); return }
      await ensureOpen(uri)
      log(`create RPC session uri=${uri}`)
      const raw = await ivRequest('$/lean/rpc/connect', { uri }, 30_000)
      const sessionId = typeof raw === 'string' ? raw : raw?.sessionId
      if (!sessionId) throw { code: -32000, message: `invalid RPC session: ${JSON.stringify(raw)}` }
      const docUri = uri
      const timer = setInterval(() => {
        if (lakeProc) toLake({ jsonrpc: '2.0', method: '$/lean/rpc/keepAlive', params: { uri: docUri, sessionId } })
      }, 20_000)
      rpcSessions.set(sessionId, { timer, uri: docUri })
      jsonResp(res, { sessionId })
    } catch(err) {
      jsonResp(res, { code: err?.code ?? -32000, message: String(err?.message ?? err) })
    }
    return
  }

  if (url.pathname === '/close-session' && req.method === 'POST') {
    try {
      const { sessionId } = await readJSON(req)
      const entry = rpcSessions.get(sessionId)
      if (entry) {
        clearInterval(entry.timer)
        rpcSessions.delete(sessionId)
        if (lakeProc) toLake({ jsonrpc: '2.0', method: '$/lean/rpc/release',
          params: { uri: entry.uri, sessionId, refs: [] } })
      }
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Cursor from Emacs (cursor-only, no text — Eglot owns document sync) ───────
  if (url.pathname === '/cursor' && req.method === 'POST') {
    try {
      const { uri, line, character } = await readJSON(req)
      if (uri && Number.isFinite(line) && Number.isFinite(character)) {
        lastCursor = { uri, line, character }
        sseEmit('emacs:cursor', lastCursor)
        log(`cursor uri=${uri} line=${line} char=${character}`)
      }
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Reverse channel: infoview → Emacs (via standard LSP toward Eglot) ─────────
  if (url.pathname === '/editor/show-document' && req.method === 'POST') {
    try {
      const body = await readJSON(req)
      // window/showDocument is a standard LSP request; Eglot handles it natively
      ivsrvRequest('window/showDocument', {
        uri: body.uri ?? '',
        external: false,
        takeFocus: true,
        selection: body.selection ?? undefined,
      }).catch(() => {})
      log(`editor show-document: ${body.uri ?? ''}`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  if (url.pathname === '/editor/apply-edit' && req.method === 'POST') {
    try {
      const body = await readJSON(req)
      // workspace/applyEdit is standard LSP; Eglot handles it natively
      ivsrvRequest('workspace/applyEdit', {
        label: 'Lean infoview edit',
        edit: body.edits ?? body.edit ?? {},
      }).catch(() => {})
      log(`editor apply-edit`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  if (url.pathname === '/editor/insert-text' && req.method === 'POST') {
    try {
      const body = await readJSON(req)
      const text = body.text ?? ''
      const pos  = body.pos   // optional {line, character}
      // Map insertText to workspace/applyEdit if a cursor position is known
      const cursor = lastCursor
      const targetLine = (pos?.line ?? cursor?.line) ?? 0
      const targetChar = (pos?.character ?? cursor?.character) ?? 0
      const targetUri  = cursor?.uri ?? ''
      if (targetUri) {
        const lspPos = { line: targetLine, character: targetChar }
        ivsrvRequest('workspace/applyEdit', {
          label: 'Lean infoview insert',
          edit: {
            changes: {
              [targetUri]: [{ range: { start: lspPos, end: lspPos }, newText: text }],
            },
          },
        }).catch(() => {})
      }
      log(`editor insert-text bytes=${Buffer.byteLength(text, 'utf8')}`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  if (url.pathname === '/editor/restart-file' && req.method === 'POST') {
    try {
      const body = await readJSON(req)
      // Custom notification toward Eglot — init-lean-eglot.el handles it
      ivsrvNotify('lean/restartFile', { uri: body.uri ?? '' })
      log(`editor restart-file: ${body.uri ?? ''}`)
    } catch {}
    res.writeHead(200); res.end(); return
  }

  // Static assets ─────────────────────────────────────────────────────────────
  const p = url.pathname === '/' ? 'index.html' : url.pathname.replace(/^\//, '')
  serveStatic(res, p)
})

// ── Start ─────────────────────────────────────────────────────────────────────

httpServer.listen(0, '127.0.0.1', async () => {
  const port = httpServer.address().port
  log(`HTTP listening on 127.0.0.1:${port}`)
  connectGateway(port)
  startLake()
})

process.on('SIGTERM', () => { killDownstream(); gatewaySocket?.close(); process.exit(0) })
process.on('SIGINT',  () => { killDownstream(); gatewaySocket?.close(); process.exit(0) })
process.on('exit', () => { killDownstream('SIGKILL') })

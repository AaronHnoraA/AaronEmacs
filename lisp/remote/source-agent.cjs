// Workspace-owned file snapshots. Node builtins only; no sockets or polling.
const fs = require('node:fs');
const fsp = require('node:fs/promises');
const path = require('node:path');
const crypto = require('node:crypto');
const readline = require('node:readline');
const digest = (text) => crypto.createHash('sha256').update(text).digest('hex');
const failure = (message, code = 'ECONFLICT') => Object.assign(new Error(message), { code });

function createSourceAgent(notify = () => {}) {
  let root, options, closed = false, queue = Promise.resolve();
  const watches = new Map();
  const relevant = (rel) => {
    const parts = rel.split(path.sep);
    return !(options.hidden === false && parts.some((s) => s.startsWith('.')))
      && !options.exclude.some((glob) => path.matchesGlob(rel, glob) || path.matchesGlob(`${rel}/`, glob));
  };
  const included = (rel) => relevant(rel) && (!options.extensions.length || options.extensions.includes(path.extname(rel).toLowerCase()));
  function check() { if (closed || !root) throw failure('Source lease is closed', 'ECLOSED'); }
  function lexical(rel, fileOnly = false) {
    check();
    if (typeof rel !== 'string' || path.isAbsolute(rel) || rel.split(/[\\/]/).includes('..') || rel.includes('\0')) throw failure('Path must remain inside its source root', 'EACCES');
    rel = rel === '.' ? '' : rel;
    if (rel && (!relevant(rel) || (fileOnly && !included(rel)))) throw failure('Source path is excluded', 'EACCES');
    return path.join(root, rel);
  }
  async function checked(rel, { missing = false, fileOnly = false } = {}) {
    const file = lexical(rel, fileOnly);
    const rootInfo = await fsp.lstat(root);
    if (!rootInfo.isDirectory() || rootInfo.isSymbolicLink()) throw failure('Source root changed', 'EACCES');
    let current = root;
    for (const part of path.relative(root, file).split(path.sep).filter(Boolean)) {
      current = path.join(current, part);
      let info;
      try { info = await fsp.lstat(current); }
      catch (error) { if (missing && error.code === 'ENOENT') break; throw error; }
      if (info.isSymbolicLink()) throw failure('Source path must not traverse a symbolic link', 'EACCES');
    }
    check();
    return file;
  }
  async function read(rel) {
    const file = await checked(rel, { fileOnly: true });
    const handle = await fsp.open(file, fs.constants.O_RDONLY | (fs.constants.O_NOFOLLOW || 0));
    try {
      const info = await handle.stat();
      if (!info.isFile()) throw failure('Source is not a regular file', 'EACCES');
      if (info.size > options.maxBytes) throw failure('Source exceeds size limit', 'EFBIG');
      const content = await handle.readFile({ encoding: 'utf8' });
      if (Buffer.byteLength(content) > options.maxBytes) throw failure('Source exceeds size limit', 'EFBIG');
      check();
      return { content, revision: digest(content), mtimeMs: info.mtimeMs };
    } finally { await handle.close(); }
  }
  function watchDirectory(directory) {
    if (!options.watch || watches.has(directory)) return;
    const watch = fs.watch(directory, { persistent: false }, (kind, filename) => {
      if (closed) return;
      if (!filename) { notify({ event: 'rescan' }); return; }
      const rel = path.relative(root, path.join(directory, String(filename)));
      if (!relevant(rel)) return;
      // Rename may be a dotted directory. Let the next explicit snapshot
      // reconcile directories rather than guessing from a filename suffix.
      if (kind === 'rename') notify({ event: 'rescan' });
      else if (included(rel)) notify({ event: 'changed', paths: [rel] });
    });
    watch.on('error', (error) => notify({ event: 'error', message: error.message }));
    watches.set(directory, watch);
  }
  async function list() {
    check();
    const files = [], directories = new Set();
    async function walk(directory) {
      check();
      await checked(path.relative(root, directory));
      directories.add(directory);
      watchDirectory(directory); // Subscribe before reading to cover changes during discovery.
      for (const entry of await fsp.readdir(directory, { withFileTypes: true })) {
        check();
        const file = path.join(directory, entry.name), rel = path.relative(root, file);
        if (!relevant(rel)) continue;
        if (entry.isDirectory()) await walk(file);
        else if (entry.isFile() && included(rel)) files.push(rel);
      }
    }
    await walk(root);
    for (const [directory, watch] of watches) if (!directories.has(directory)) { watch.close(); watches.delete(directory); }
    return { files: files.sort(), directories: directories.size };
  }
  async function write(body) {
    const file = await checked(body.path, { missing: true, fileOnly: true });
    if (typeof body.content !== 'string' || Buffer.byteLength(body.content) > options.maxBytes) throw failure('Invalid source content or size', 'EFBIG');
    if (body.expectedRevision !== null && !/^[a-f0-9]{64}$/.test(body.expectedRevision || '')) throw failure('A source revision is required', 'ECONFLICT');
    let current = null, modes = 0o644;
    try { current = await read(body.path); modes = (await fsp.stat(file)).mode & 0o777; }
    catch (error) { if (error.code !== 'ENOENT') throw error; }
    if ((current?.revision ?? null) !== body.expectedRevision) throw failure('Source changed before write');
    if (current?.content === body.content) return { ...current, changed: false };
    await fsp.mkdir(path.dirname(file), { recursive: true });
    const temporary = path.join(path.dirname(file), `.source-${crypto.randomUUID()}.tmp`);
    try {
      const handle = await fsp.open(temporary, 'wx', modes);
      try { await handle.writeFile(body.content, 'utf8'); await handle.sync(); }
      finally { await handle.close(); }
      // Recheck after staging. External edits and parent substitutions must
      // not get overwritten merely because this process serializes requests.
      await checked(body.path, { missing: true, fileOnly: true });
      const latest = await read(body.path).catch((error) => { if (error.code === 'ENOENT') return null; throw error; });
      if ((latest?.revision ?? null) !== body.expectedRevision) throw failure('Source changed while staging write');
      check();
      if (body.expectedRevision === null) {
        await fsp.link(temporary, file); // Exclusive publication; never clobber a new file.
      } else await fsp.rename(temporary, file);
      notify({ event: 'changed', paths: [body.path] });
      return { revision: digest(body.content), changed: true, mtimeMs: (await fsp.stat(file)).mtimeMs };
    } finally { await fsp.rm(temporary, { force: true }); }
  }
  async function dispatch(body) {
    if (body.op === 'open') {
      if (root || closed) throw failure('Source agent cannot be reopened', 'ECLOSED');
      if (typeof path.matchesGlob !== 'function') throw failure('Source agent requires Node with path.matchesGlob', 'ENOSYS');
      if (!path.isAbsolute(body.root || '')) throw failure('Absolute source root required', 'EINVAL');
      options = { hidden: body.hidden !== false, exclude: body.exclude || [], extensions: body.extensions || [], maxBytes: Math.min(body.maxBytes || 16 * 1024 * 1024, 16 * 1024 * 1024), watch: body.watch !== false };
      if (!Array.isArray(options.exclude) || !options.exclude.every((v) => typeof v === 'string') || !Array.isArray(options.extensions)) throw failure('Invalid source filters', 'EINVAL');
      root = await fsp.realpath(body.root);
      if (!(await fsp.stat(root)).isDirectory()) throw failure('Source root is not a directory', 'ENOTDIR');
      return { root, protocol: 1 };
    }
    check();
    if (body.op === 'list') return list();
    if (body.op === 'read') return read(body.path);
    if (body.op === 'write') return write(body);
    if (body.op === 'canonical') { await checked(body.path); return { path: body.path }; }
    if (body.op === 'status') return { watches: watches.size };
    throw failure('Unknown source operation', 'EINVAL');
  }
  return {
    request(body) { const task = queue.then(() => dispatch(body)); queue = task.catch(() => {}); return task; },
    close() { closed = true; for (const watcher of watches.values()) watcher.close(); watches.clear(); },
  };
}

function serve() {
  const send = (value) => process.stdout.write(JSON.stringify(value) + '\n');
  const agent = createSourceAgent(send);
  const input = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
  input.on('line', (line) => {
    if (Buffer.byteLength(line) > 32 * 1024 * 1024) { agent.close(); process.exitCode = 1; input.close(); return; }
    let body;
    try { body = JSON.parse(line); } catch { send({ id: null, error: { code: 'EINVAL', message: 'Invalid JSON' } }); return; }
    agent.request(body).then((result) => send({ id: body.id, result }),
      (error) => send({ id: body.id, error: { code: error.code || 'EIO', message: error.message } }));
  });
  input.on('close', () => agent.close());
  process.on('SIGTERM', () => { agent.close(); process.exit(0); });
}
module.exports = { createSourceAgent, serve };
if (require.main === module) serve();

const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs/promises');
const path = require('node:path');
const os = require('node:os');
const { createSourceAgent } = require('../lisp/remote/source-agent.cjs');

async function fixture(run) {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), 'remote-source-'));
  const events = [], agent = createSourceAgent((event) => events.push(event));
  try {
    await agent.request({ op: 'open', root, hidden: false, exclude: ['**/vendor/**'], extensions: ['.md', '.noema'] });
    await run({ root, agent, events });
  } finally { agent.close(); await fs.rm(root, { recursive: true, force: true }); }
}
test('source filters prune discovery, reads and directory watches', () => fixture(async ({root,agent}) => {
  for (const directory of ['.lake/packages', 'vendor/child', 'notes']) await fs.mkdir(path.join(root,directory), {recursive:true});
  for (const file of ['.lake/packages/README.md', 'vendor/child/a.md', 'notes/live.md', 'notes/work.noema', 'code.js']) await fs.writeFile(path.join(root,file), 'content');
  assert.deepEqual((await agent.request({op:'list'})).files, ['notes/live.md','notes/work.noema']);
  assert.equal((await agent.request({op:'status'})).watches, 2);
  for (const file of ['../outside.md', '.lake/packages/README.md', 'vendor/child/a.md', 'code.js']) {
    await assert.rejects(agent.request({op:'read',path:file}), {code:'EACCES'});
  }
}));
test('revision writes preserve Unicode, permissions, no-ops and competing edits', () => fixture(async ({root,agent}) => {
  await fs.writeFile(path.join(root,'task.md'), '原文🚀\n', {mode:0o600});
  const original = await agent.request({op:'read',path:'task.md'});
  const next = await agent.request({op:'write',path:'task.md',content:'修改🚀\n',expectedRevision:original.revision});
  assert.equal(next.changed,true);
  assert.equal((await fs.stat(path.join(root,'task.md'))).mode & 0o777,0o600);
  assert.equal((await agent.request({op:'write',path:'task.md',content:'修改🚀\n',expectedRevision:next.revision})).changed,false);
  await assert.rejects(agent.request({op:'write',path:'task.md',content:'stale',expectedRevision:original.revision}),{code:'ECONFLICT'});
  await fs.writeFile(path.join(root,'task.md'),'external');
  await assert.rejects(agent.request({op:'write',path:'task.md',content:'overwrite',expectedRevision:next.revision}),{code:'ECONFLICT'});
  assert.equal(await fs.readFile(path.join(root,'task.md'),'utf8'),'external');
  await agent.request({op:'write',path:'nested/new.md',content:'capture',expectedRevision:null});
  await assert.rejects(agent.request({op:'write',path:'nested/new.md',content:'other',expectedRevision:null}),{code:'ECONFLICT'});
  assert(!((await fs.readdir(root)).some((name)=>name.startsWith('.source-'))));
}));
test('symlinks and closed leases never become implicit source roots', () => fixture(async ({root,agent}) => {
  await fs.mkdir(path.join(root,'real'));
  await fs.writeFile(path.join(root,'real','a.md'),'real');
  await fs.symlink(path.join(root,'real'),path.join(root,'link'));
  assert.deepEqual((await agent.request({op:'list'})).files,['real/a.md']);
  await assert.rejects(agent.request({op:'read',path:'link/a.md'}),{code:'EACCES'});
  await assert.rejects(agent.request({op:'write',path:'link/new.md',content:'bad',expectedRevision:null}),{code:'EACCES'});
  agent.close(); agent.close();
  await assert.rejects(agent.request({op:'read',path:'real/a.md'}),{code:'ECLOSED'});
}));
test('OS events report file changes and dotted directory creation without polling', async () => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(),'remote-source-watch-'));
  let resolveEvent;
  const event = new Promise((resolve)=>{resolveEvent=resolve;});
  const agent=createSourceAgent(resolveEvent);
  const timer=setTimeout(()=>resolveEvent({event:'timeout'}),3000);
  try {
    await agent.request({op:'open',root,hidden:false,extensions:['.md']});
    await agent.request({op:'list'});
    await fs.mkdir(path.join(root,'notes.v2'));
    assert.equal((await event).event,'rescan');
    await fs.writeFile(path.join(root,'notes.v2','task.md'),'task');
    assert.deepEqual((await agent.request({op:'list'})).files,['notes.v2/task.md']);
  } finally {clearTimeout(timer);agent.close();await fs.rm(root,{recursive:true,force:true});}
});

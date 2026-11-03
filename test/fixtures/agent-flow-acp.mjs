// Deterministic ACP process for native agent-shell end-to-end verification.
// No external requests, credentials, shells, or project mutations.
import { createInterface } from 'node:readline';
import { readFileSync, writeFileSync, existsSync, appendFileSync } from 'node:fs';
import { hostname } from 'node:os';
const store = process.argv[2];
const log = `${store}.events`;
const sessions = existsSync(store) ? JSON.parse(readFileSync(store, 'utf8')) : {};
const pending = new Map();
const active = new Map();
let seq = 10000;
const send = msg => process.stdout.write(JSON.stringify({jsonrpc:'2.0', ...msg})+'\n');
const reply = (id, result={}) => send({id,result});
const update = (sid, data) => send({method:'session/update',params:{sessionId:sid,update:data}});
const text = (sid, value) => update(sid,{sessionUpdate:'agent_message_chunk',content:{type:'text',text:value}});
const persist = () => writeFileSync(store, JSON.stringify(sessions));
const session = id => ({sessionId:id, models:{currentModelId:'fixture',availableModels:[{modelId:'fixture',name:'Local fixture'},{modelId:'other',name:'Alternate fixture'}]}, modes:{currentModeId:'ask',availableModes:[{id:'ask',name:'Ask'},{id:'edit',name:'Edit'}]}});
const finish = (sid, reason='end_turn') => { const req=active.get(sid); if(!req)return;active.delete(sid);reply(req, {stopReason:reason}); };
const ask = (method, params, callback) => {const id=++seq;pending.set(id,callback);send({id,method,params});};
createInterface({input:process.stdin}).on('line', line => {
  try {
    const msg=JSON.parse(line);
    appendFileSync(log, JSON.stringify({method:msg.method,id:msg.id,params:msg.params,result:msg.result})+'\n');
    if (!msg.method) {const cb=pending.get(msg.id);pending.delete(msg.id);cb?.(msg.result,msg.error);return;}
    const p=msg.params || {}, sid=p.sessionId;
    switch(msg.method) {
      case 'initialize':reply(msg.id,{protocolVersion:1,agentInfo:{name:'agent-flow-fixture',version:'1'},authMethods:[],agentCapabilities:{loadSession:true,promptCapabilities:{embeddedContext:true}},sessionCapabilities:{list:{},resume:{},fork:{}},...(process.env.AGENT_FLOW_STEERING?{_meta:{steering:{supported:true}}}:{})});break;
      case 'authenticate':reply(msg.id);break;
      case 'session/new': {const id=`fixture-${Object.keys(sessions).length+1}`;sessions[id]={cwd:p.cwd,turns:[]};persist();reply(msg.id,session(id));break;}
      case 'session/load':case 'session/resume':
        if(!sessions[sid])throw new Error('Unknown session');
        for(const turn of sessions[sid].turns) {update(sid,{sessionUpdate:'user_message_chunk',content:{type:'text',text:turn}});text(sid,`Restored: ${turn}\n`);}
        reply(msg.id,session(sid));break;
      case 'session/list': {
        const result={sessions:Object.entries(sessions).map(([id,s])=>({sessionId:id,cwd:s.cwd,title:s.title??id,updatedAt:new Date().toISOString()}))};
        const delay=Number(process.env.AGENT_FLOW_LIST_DELAY_MS||0);
        if(delay>0)setTimeout(()=>reply(msg.id,result),delay);else reply(msg.id,result);
        break;
      }
      case 'session/fork': {const id=`fixture-${Object.keys(sessions).length+1}`;sessions[id]=structuredClone(sessions[sid]);persist();reply(msg.id,session(id));break;}
      case 'session/set_model':case 'session/set_mode':reply(msg.id);break;
      case '_session/steering': {
        // Opt-in through AGENT_FLOW_STEERING.  A steer whose text starts with
        // "decline" is refused the way Codex answers; any other joins the turn.
        const steer=p.prompt.map(x=>x.text||'').join('');
        if(steer.startsWith('error')){send({id:msg.id,error:{code:-32000,message:'Steer rejected'}});break;}
        if(steer.startsWith('decline')){reply(msg.id,{outcome:'failed'});break;}
        text(sid,`Steered: ${steer}\n`);reply(msg.id,{outcome:'injected'});break;
      }
      case 'session/cancel':text(sid,'Turn cancelled.\n');finish(sid,'cancelled');break;
      case 'session/prompt': {
        active.set(sid,msg.id);
        const prompt=p.prompt.map(x=>x.text||'').join('');
        sessions[sid].turns.push(prompt);persist();
        if(prompt==='runtime'){
          text(sid,JSON.stringify({cwd:process.cwd(),hostname:hostname(),executable:process.execPath,marker:process.env.AGENT_REMOTE_MARKER??null})+'\n');
          finish(sid);break;
        }
        if(prompt==='fs-read'){
          ask('fs/read_text_file',{sessionId:sid,path:`${sessions[sid].cwd}/editor.txt`},(result,error)=>{
            text(sid,error?`FS_ERROR:${JSON.stringify(error)}\n`:`FS_READ:${result.content}\n`);finish(sid);
          });break;
        }
        if(prompt==='fs-read-range'){
          ask('fs/read_text_file',{sessionId:sid,path:`${sessions[sid].cwd}/editor.txt`,line:2,limit:1},(result,error)=>{
            text(sid,error?`FS_ERROR:${JSON.stringify(error)}\n`:`FS_RANGE:${JSON.stringify(result.content)}\n`);finish(sid);
          });break;
        }
        if(prompt==='fs-missing'){
          ask('fs/read_text_file',{sessionId:sid,path:`${sessions[sid].cwd}/missing.txt`},(_result,error)=>{
            text(sid,`FS_MISSING_CODE:${error?.code??'none'}\n`);finish(sid);
          });break;
        }
        if(prompt==='fs-write'){
          ask('fs/write_text_file',{sessionId:sid,path:`${sessions[sid].cwd}/editor.txt`,content:'written by ACP\n'},(_result,error)=>{
            text(sid,error?`FS_ERROR:${JSON.stringify(error)}\n`:'FS_WRITE_OK\n');finish(sid);
          });break;
        }
        if(prompt==='crash'){process.exit(23);break;}
        if(prompt==='hold'){text(sid,'Working; waiting for cancellation.\n');break;}
        if(prompt==='refuse'){text(sid,'Refusing.\n');finish(sid,'refusal');break;}
        if(prompt==='crash-later'){text(sid,'Working; the process will exit.\n');setTimeout(()=>process.exit(24),150);break;}
        if(prompt==='titled'){sessions[sid].title='Fixture title';persist();update(sid,{sessionUpdate:'session_info_update',title:'Fixture title'});text(sid,'Titled.\n');finish(sid);break;}
        if(prompt==='edit'){
          update(sid,{sessionUpdate:'tool_call',toolCallId:'edit-fixture',title:'Edit changed.txt',kind:'edit',status:'in_progress',locations:[{path:`${sessions[sid].cwd}/changed.txt`}],content:[{type:"diff",path:`${sessions[sid].cwd}/changed.txt`,oldText:'one\n',newText:'one\ntwo\n'}]});
          update(sid,{sessionUpdate:'tool_call_update',toolCallId:'edit-fixture',status:'completed'});
          text(sid,'Edited.\n');finish(sid);break;
        }
        if(prompt==='permission') {
          update(sid,{sessionUpdate:'tool_call',toolCallId:'read-fixture',title:'Read fixture.txt',kind:'read',status:'pending'});
          ask('session/request_permission',{sessionId:sid,toolCall:{toolCallId:'read-fixture',title:'Read fixture.txt',kind:'read',status:'pending'},options:[{optionId:'allow',name:'Allow once',kind:'allow_once'},{optionId:'deny',name:'Reject',kind:'reject_once'}]}, result=>{
            const allowed=result?.outcome?.optionId==='allow';
            update(sid,{sessionUpdate:'tool_call_update',toolCallId:'read-fixture',status:allowed?'completed':'failed',content:[{type:'content',content:{type:'text',text:allowed?'Fixture allowed.':'Fixture rejected.'}}]});
            text(sid,allowed?'APPROVAL_OK\n':'APPROVAL_REJECTED\n');finish(sid);
          });break;
        }
        if(prompt==='question') {
          ask('elicitation/create',{sessionId:sid,mode:'form',message:'Choose the verification scope',requestedSchema:{type:'object',properties:{scope:{type:'string',title:'Scope',enum:['focused','all'],default:'focused'}},required:['scope']}},result=>{text(sid,`ANSWER_${result?.action}_${result?.content?.scope}\n`);finish(sid);});break;
        }
        text(sid,`Reply: ${prompt}\n`);
        update(sid,{sessionUpdate:'usage_update',used:1200,size:32000,cost:{amount:0,currency:'USD'}});
        finish(sid);break;
      }
      default: if(msg.id!==undefined)send({id:msg.id,error:{code:-32601,message:`Unsupported ${msg.method}`}});
    }
  } catch(error) {process.stderr.write(`${error.stack}\n`);process.exitCode=1;}
});

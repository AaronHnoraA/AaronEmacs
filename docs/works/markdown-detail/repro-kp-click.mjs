// Repro: click a task box, Escape, click prose; does the caret move?
const repo = "/Users/hc/.config/emacs/site-lisp/noema/";
const { createServer } = await import(repo + "node_modules/vite/dist/vite/node/index.js");
const playwright = await import(process.env.PW);
const root = "/@fs" + repo;
const prose = (n) => Array.from({ length: n }, (_, i) =>
  `Paragraph ${i}. Ordinary prose keeps the document long enough to scroll; ` + "the old caret sits far outside the viewport. ".repeat(4)).join("\n\n");
const markdown = [prose(40), "A task list:\n\n- [ ] open item\n- [x] done item", prose(40)].join("\n\n");
const server = await createServer({ configFile: repo + "vite.pages.config.ts", root: repo + "pages", logLevel: "error", server: { host: "127.0.0.1", port: 0, hmr: false } });
await server.listen();
const browser = await playwright.webkit.launch({ headless: true });
try {
  const page = await browser.newPage({ viewport: { width: 1100, height: 820 } });
  page.on("pageerror", (e) => console.log("pageerror", e.message));
  await page.route("**/@vite/client", (r) => r.fulfill({ contentType: "application/javascript", body: "export function createHotContext(){return {accept(){},prune(){},dispose(){},on(){},invalidate(){}}} export function updateStyle(id,css){let s=document.createElement('style');s.textContent=css;document.head.append(s)} export function removeStyle(){} export function injectQuery(u){return u}" }));
  await page.route("**/repro", (r) => r.fulfill({ contentType: "text/html", body: `<!doctype html><html><head><link rel="stylesheet" href="${root}src/styles/widgets.css"><link rel="stylesheet" href="${root}pages/style.css"></head><body style="margin:0"><div id="host" class="noema-focused-editor" style="height:800px;width:1000px;overflow:auto;--noema-visual-zoom:1"></div></body></html>` }));
  await page.goto(new URL("repro", server.resolvedUrls.local[0]).href);
  await page.evaluate(async ({ root, markdown }) => {
    const { createEditor } = await import(root + "src/editor-api.ts");
    window.host = document.querySelector("#host");
    window.editor = createEditor(host, { initialContent: markdown });
    await document.fonts.ready;
  }, { root, markdown });
  await page.waitForTimeout(600);
  const state = (label) => page.evaluate((label) => ({ label, head: editor.view.state.selection.main.head,
    active: document.activeElement?.tagName + "." + (document.activeElement?.className || "").toString().slice(0, 40),
    hasFocus: editor.view.hasFocus }), label);
  if (process.env.TOP_ONLY) {
    const t = await page.evaluate(() => { const line = [...editor.view.contentDOM.querySelectorAll(".cm-line")].find((l) => /Paragraph 3\./.test(l.textContent)); const walker = document.createTreeWalker(line, NodeFilter.SHOW_TEXT); let node; while ((node = walker.nextNode())) if (node.length > 30) break; const range = document.createRange(); range.setStart(node, 10); range.setEnd(node, 11); const r = range.getBoundingClientRect(); return { x: r.left + 0.5, y: (r.top + r.bottom) / 2, pos: editor.view.posAtDOM(node, 10), kp: line.className, html: line.innerHTML.slice(0, 160) }; });
    console.log("top text", t);
    console.log("mapping at the point", await page.evaluate(async ({ root, t }) => {
      const { pointerPosition } = await import(root + "src/cm6/pointer-position.ts");
      const r = document.caretRangeFromPoint(t.x, t.y);
      let native = null; try { native = editor.view.posAtDOM(r.startContainer, r.startOffset); } catch {}
      return { expected: t.pos, nativeCaretRange: native, cmPosAtCoords: editor.view.posAtCoords({ x: t.x, y: t.y }), noemaPointerPosition: pointerPosition(editor.view, t.x, t.y) };
    }, { root, t }));
    await page.mouse.click(t.x, t.y); await page.waitForTimeout(400);
    console.log(await state("after top click (unfocused before)"));
    await page.mouse.click(t.x, t.y); await page.waitForTimeout(400);
    console.log(await state("after same-point click while focused"));
    console.log("mapping at x+40", await page.evaluate(async ({ root, t }) => { const { pointerPosition } = await import(root + "src/cm6/pointer-position.ts"); return { cm: editor.view.posAtCoords({ x: t.x + 40, y: t.y }), noema: pointerPosition(editor.view, t.x + 40, t.y) }; }, { root, t }));
    await page.mouse.click(t.x + 40, t.y); await page.waitForTimeout(400);
    console.log(await state("after x+40 click"));
    process.exit(0);
  }
  // Old caret at start, focused, then bring the task into view.
  const taskFrom = markdown.indexOf("- [ ] open");
  await page.evaluate(() => { editor.view.focus(); editor.view.dispatch({ selection: { anchor: 0 } }); }, taskFrom);
  await page.evaluate((at) => { host.dispatchEvent(new WheelEvent("wheel", { bubbles: true })); host.scrollTop = Math.max(0, editor.view.lineBlockAt(at).top - 300); }, taskFrom);
  await page.waitForTimeout(600);
  console.log(await state("before"));
  console.log("viewport", await page.evaluate((at) => ({ vp: [editor.view.viewport.from, editor.view.viewport.to], at, line: [...editor.view.contentDOM.querySelectorAll(".cm-line")].find((l) => /open item/.test(l.textContent))?.outerHTML.slice(0, 300) }), taskFrom));
  const box = await page.evaluate((from) => { let best = null; for (const el of editor.view.contentDOM.querySelectorAll("[contenteditable=false]")) { if (el.parentElement?.closest("[contenteditable=false]")) continue; let at; try { at = editor.view.posAtDOM(el); } catch { continue; } if (at < from - 1 || at >= from + 40) continue; const r = el.getBoundingClientRect(); if (r.height < 4 || r.width < 4) continue; if (!best || r.width * r.height > best.area) best = { r, area: r.width * r.height, cls: el.className, tag: el.tagName }; } return best && { x: best.r.left + best.r.width / 2, y: best.r.top + best.r.height / 2, cls: best.cls, tag: best.tag }; }, taskFrom);
  console.log("task element", box);
  if (!process.env.SKIP_TASK) await page.mouse.click(box.x, box.y);
  await page.waitForTimeout(500);
  console.log(await state("after task click"));
  await page.keyboard.press("Escape");
  await page.waitForTimeout(200);
  console.log(await state("after Escape"));
  // Log what the prose mousedown meets.
  await page.evaluate(() => {
    window.__events = [];
    for (const type of ["mousedown", "mouseup", "click", "focusin", "selectionchange"]) {
      document.addEventListener(type, (e) => window.__events.push(`${type} target=${e.target?.tagName}.${(e.target?.className || "").toString().slice(0, 30)} defaultPrevented=${e.defaultPrevented}`), true);
      document.addEventListener(type, (e) => window.__events.push(`${type}(bubble) defaultPrevented=${e.defaultPrevented}`), false);
    }
  });
  const text = await page.evaluate(() => {
    for (const line of editor.view.contentDOM.querySelectorAll(".cm-line")) {
      const node = [...line.childNodes].find((n) => n.nodeType === 3 && n.length > 30);
      if (!node) continue;
      const range = document.createRange(); range.setStart(node, 10); range.setEnd(node, 11);
      const rect = range.getBoundingClientRect();
      if (rect.top < 120 || rect.bottom > 700 || !rect.width) continue;
      return { x: rect.left + 0.5, y: (rect.top + rect.bottom) / 2, pos: editor.view.posAtDOM(node, 10) };
    }
  });
  console.log("click text at", text);
  await page.evaluate(() => {
    window.__tx = [];
    const v = editor.view, original = v.dispatch.bind(v);
    v.dispatch = (...args) => {
      const before = v.state.selection.main.head;
      const r = original(...args);
      const after = v.state.selection.main.head;
      if (before !== after || args[0]?.selection) {
        const stack = (new Error().stack || "").split(String.fromCharCode(10)).slice(1, 7)
          .map((l) => l.trim().replace(/https?:\/\/[^ ]*\/(src|pages|node_modules)\//, "$1/").slice(0, 110)).join(" < ");
        window.__tx.push(`sel ${before} -> ${after} | ${stack}`);
      }
      return r;
    };
  });
  console.log("mapping at text point", await page.evaluate(async ({ root, text }) => {
    const { pointerPosition, renderedTextOffsetAtPoint } = await import(root + "src/cm6/pointer-position.ts");
    const el = document.elementFromPoint(text.x, text.y);
    return { cm: editor.view.posAtCoords({ x: text.x, y: text.y }), noema: pointerPosition(editor.view, text.x, text.y),
      element: el?.tagName + "." + (el?.className || ""), inContent: editor.view.contentDOM.contains(el) };
  }, { root, text }));
  await page.evaluate(() => {
    window.__calls = [];
    const short = () => (new Error().stack || "").split(String.fromCharCode(10)).slice(2, 6)
      .map((l) => l.trim().replace(/https?:\/\/[^ ]*\/(src|pages|node_modules)\//, "$1/").slice(0, 90)).join(" < ");
    const wrap = (proto, name) => { const o = proto[name]; proto[name] = function (...a) { window.__calls.push(`${name}(${this.className ?? ""}) | ${short()}`); return o.apply(this, a); }; };
    wrap(HTMLElement.prototype, "focus");
    for (const n of ["setBaseAndExtent", "addRange", "removeAllRanges"]) wrap(Selection.prototype, n);
    const desc = (node) => node ? `${node.nodeName}${node.nodeType === 3 ? JSON.stringify(node.data.slice(0, 20)) : "." + (node.className || "")} in ${node.parentElement?.tagName}.${(node.parentElement?.className || "").toString().slice(0, 40)}` : "null";
    const oc = Selection.prototype.collapse;
    Selection.prototype.collapse = function (node, offset) {
      const r = oc.call(this, node, offset);
      window.__calls.push(`collapse(${desc(node)}, ${offset}) -> now focus ${desc(this.focusNode)}@${this.focusOffset} rangeCount=${this.rangeCount}`);
      return r;
    };
  });
  const domSel = () => page.evaluate(() => { const s = getSelection(); try { return editor.view.posAtDOM(s.focusNode, s.focusOffset); } catch { return null; } });
  await page.mouse.move(text.x, text.y);
  await page.mouse.down({ button: "left" });
  console.log("after mousedown", await page.evaluate(() => ({ head: editor.view.state.selection.main.head, mouseSel: !!editor.view.inputState?.mouseSelection })), "domSel", await domSel());
  console.log("internals after mousedown", await page.evaluate(() => {
    const v = editor.view, d = (n, o) => n ? `${n.nodeName}${n.nodeType === 3 ? JSON.stringify(n.data.slice(0, 16)) : "." + (n.className || "")}@${o} in ${n.parentElement?.className?.toString().slice(0, 30)}` : "null";
    const cached = v.observer.selectionRange;
    const raw = getSelection();
    let near = null; try { const p = v.docView.inlineDOMNearPos(v.state.selection.main.head, 1); near = d(p.node, p.offset); } catch (e) { near = "err " + e.message; }
    return { cachedFocus: d(cached.focusNode, cached.focusOffset), rawFocus: d(raw.focusNode, raw.focusOffset), inlineNear: near, active: document.activeElement?.className?.toString().slice(0, 30) };
  }));
  await page.mouse.up({ button: "left" });
  console.log("after mouseup domSel", await domSel());
  console.log("CALLS\n" + (await page.evaluate(() => window.__calls.join("\n"))));
  await page.waitForTimeout(600);
  console.log(await state("after text click"));
  console.log("TXLOG\n" + (await page.evaluate(() => (window.__tx || []).join("\n"))));
  console.log((await page.evaluate(() => window.__events)).join("\n"));
} finally {
  await browser.close();
  await server.close();
}

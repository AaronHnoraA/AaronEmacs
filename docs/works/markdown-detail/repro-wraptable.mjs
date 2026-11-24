// Measure a wrapped table in preview and in cell editing.
const repo = "/Users/hc/.config/emacs/site-lisp/noema/";
const { createServer } = await import(repo + "node_modules/vite/dist/vite/node/index.js");
const playwright = await import(process.env.PW);
const root = "/@fs" + repo;
const prose = (n) => Array.from({ length: n }, (_, i) =>
  `Paragraph ${i}. Ordinary prose keeps the document long enough to scroll; ` + "the old caret sits far outside the viewport. ".repeat(4)).join("\n\n");
const side = process.env.SIDE || "left";
const markdown = [prose(10), `| wrap ${side} | formula |\n|---|---|\n| label | \\(z+1\\) |\n{align=${side} wrap=true width=40%}`, prose(6)].join("\n\n");
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
  await page.waitForTimeout(700);
  const lines = (label) => page.evaluate(({ side, label, markdown }) => {
    const v = editor.view, from = markdown.indexOf(`| wrap ${side} |`), to = markdown.indexOf("Paragraph 0.", from);
    const out = [];
    for (let pos = from; pos < to; ) { const b = v.lineBlockAt(pos); out.push([v.state.doc.lineAt(pos).number, Math.round(b.height * 10) / 10, v.state.doc.lineAt(pos).text.slice(0, 24)]); pos = b.to + 1; }
    const doms = [...v.contentDOM.children].filter((el) => { try { const p = v.posAtDOM(el); return p >= from && p < to; } catch { return false; } })
      .map((el) => `${el.tagName}.${(el.className || "").toString().slice(0, 60)}:${Math.round(el.getBoundingClientRect().height)}`);
    return { label, contentHeight: Math.round(v.contentHeight), blocks: out, doms };
  }, { side, label, markdown });
  const measure = (label) => page.evaluate(({ side, label }) => {
    const block = document.querySelector(`.cm-table-block.noema-table-wrap.noema-table-align-${side}`);
    if (!block) return { label, block: null };
    const box = (el) => el ? Math.round(el.getBoundingClientRect().height * 10) / 10 : null;
    const rows = [...block.querySelectorAll("tr")].map((tr) => box(tr));
    const cs = getComputedStyle(block);
    const table = block.querySelector("table");
    return { label, blockH: box(block), tableH: box(table), rows, classes: block.className.slice(0, 160),
      padding: cs.padding, margin: cs.margin, cellPad: table ? getComputedStyle(table.querySelector("td,th")).padding : null,
      cellLH: table ? getComputedStyle(table.querySelector("td,th")).lineHeight : null,
      firstChildren: [...block.children].map((c) => `${c.tagName}.${(c.className || "").toString().slice(0, 50)}:${box(c)}`) };
  }, { side, label });
  console.log(JSON.stringify(await lines("preview")));
  const f = await page.locator(`.cm-table-block.noema-table-wrap.noema-table-align-${side} .cm-markdown-table-preview .noema-math-inline`).boundingBox();
  await page.mouse.click(f.x + f.width / 2, f.y + f.height / 2);
  await page.waitForTimeout(600);
  console.log(JSON.stringify(await lines("editing")));
} finally {
  await browser.close();
  await server.close();
}

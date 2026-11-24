// Time render-html.ts in WebKit for a given source tree.
// usage: TREE=/path/to/noema PW=... node bench-webkit.mjs NOTE.md
import { readFileSync } from "node:fs";
const tree = process.env.TREE.replace(/\/?$/, "/");
const vite = await import("/Users/hc/.config/emacs/site-lisp/noema/node_modules/vite/dist/vite/node/index.js");
const playwright = await import(process.env.PW);
const note = readFileSync(process.argv[2], "utf8");
const root = "/@fs" + tree;
const server = await vite.createServer({ configFile: tree + "vite.pages.config.ts", root: tree + "pages", logLevel: "error", server: { host: "127.0.0.1", port: 0, hmr: false, fs: { allow: [tree, "/Users/hc/.config/emacs/site-lisp/noema/"] } } });
await server.listen();
const browser = await playwright.webkit.launch({ headless: true });
try {
  const page = await browser.newPage();
  page.on("pageerror", (e) => console.log("pageerror", e.message));
  await page.route("**/@vite/client", (r) => r.fulfill({ contentType: "application/javascript", body: "export function createHotContext(){return {accept(){},prune(){},dispose(){},on(){},invalidate(){}}} export function updateStyle(id,css){let s=document.createElement('style');s.textContent=css;document.head.append(s)} export function removeStyle(){} export function injectQuery(u){return u}" }));
  await page.route("**/bench", (r) => r.fulfill({ contentType: "text/html", body: "<!doctype html><html><body></body></html>" }));
  await page.goto(new URL("bench", server.resolvedUrls.local[0]).href);
  const result = await page.evaluate(async ({ root, note }) => {
    const m = await import(root + "src/render-html.ts");
    const median = (xs) => xs.sort((a, b) => a - b)[Math.floor(xs.length / 2)];
    // Whole note with an option, so a render cache (if any) does not apply.
    m.renderMarkdownHTML(note, { allowHtml: false });
    const whole = [];
    for (let i = 0; i < 5; i++) { const t = performance.now(); m.renderMarkdownHTML(note, { allowHtml: false }); whole.push(performance.now() - t); }
    // Widget-sized pieces: the note's paragraphs, as cells and bodies are.
    const pieces = note.split(/\n{2,}/).filter((p) => p.trim()).slice(0, 200);
    const t1 = performance.now(); for (const p of pieces) m.renderMarkdownHTML(p); const firstPass = performance.now() - t1;
    const t2 = performance.now(); for (const p of pieces) m.renderMarkdownHTML(p); const secondPass = performance.now() - t2;
    return { wholeMs: median(whole), pieces: pieces.length, widgetFirstPassMs: firstPass, widgetRepeatMs: secondPass };
  }, { root, note });
  console.log(JSON.stringify(result));
} finally {
  await browser.close();
  await server.close();
}

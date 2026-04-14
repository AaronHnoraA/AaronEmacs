#!/usr/bin/env npx tsx
/**
 * KaTeX parser wrapper: read LaTeX expressions from stdin (one per line),
 * parse each with KaTeX's internal Parser, and output the AST as JSON
 * (one JSON object per line).
 *
 * Uses KaTeX source (Parser.ts, Settings.ts) via tsx.
 * Reuses node_modules from ../lexer_compare to avoid duplication.
 */
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { createInterface } from "readline";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Reuse KaTeX installed in lexer_compare
const katexRoot = join(__dirname, "..", "lexer_compare", "node_modules", "katex");

// Dynamic import of KaTeX source TypeScript modules (tsx resolves .ts)
const SettingsModule = await import(join(katexRoot, "src", "Settings.ts"));
const Settings = SettingsModule.default;

const ParserModule = await import(join(katexRoot, "src", "Parser.ts"));
const Parser = ParserModule.default;

/**
 * Deep-clone an AST to break shared references, then strip
 * internal/circular fields before serialization.
 */
function cleanAST(node) {
  if (node === null || node === undefined) return node;
  if (typeof node !== "object") return node;
  if (Array.isArray(node)) return node.map(cleanAST);
  const out = {};
  for (const [k, v] of Object.entries(node)) {
    if (k === "loc" || k === "lexer" || k === "gullet" || k === "settings") continue;
    out[k] = cleanAST(v);
  }
  return out;
}

async function main() {
  const rl = createInterface({ input: process.stdin });

  // Stub console.log/console.error so \message and \errmessage don't pollute stdout/stderr
  const noop = () => {};
  const origLog = console.log;
  const origError = console.error;

  for await (const line of rl) {
    const expr = line.trim();
    if (expr === "") continue;

    console.log = noop;
    console.error = noop;
    try {
      const settings = new Settings({ trust: true });
      const parser = new Parser(expr, settings);
      const ast = parser.parse();
      const json = JSON.stringify(cleanAST(ast));
      process.stdout.write(json + "\n");
    } catch (err) {
      const errObj = {
        error: true,
        message: err.message || String(err),
        input: expr,
      };
      process.stdout.write(JSON.stringify(errObj) + "\n");
    } finally {
      console.log = origLog;
      console.error = origError;
    }
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});

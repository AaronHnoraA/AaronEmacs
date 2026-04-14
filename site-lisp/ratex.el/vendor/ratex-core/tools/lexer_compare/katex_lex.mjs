/**
 * KaTeX lexer wrapper: read LaTeX from stdin, print one token per line.
 * Uses KaTeX source (Lexer.ts) via tsx. Requires: npm install katex tsx
 */
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const katexRoot = join(__dirname, "node_modules", "katex");

// Dynamic import of KaTeX source (TypeScript). tsx resolves .ts
const LexerModule = await import(join(katexRoot, "src", "Lexer.ts"));
const Lexer = LexerModule.default;

// Minimal settings: Lexer only uses settings.reportNonstrict for comment-at-EOF
const settings = { reportNonstrict: () => {} };

async function main() {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  const input = Buffer.concat(chunks).toString("utf8").replace(/\n$/, "");

  const lexer = new Lexer(input, settings);
  while (true) {
    const tok = lexer.lex();
    const text = tok.text === "EOF" ? "EOF" : tok.text.replace(/\n/g, "\\n");
    process.stdout.write(text + "\n");
    if (tok.text === "EOF") break;
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});

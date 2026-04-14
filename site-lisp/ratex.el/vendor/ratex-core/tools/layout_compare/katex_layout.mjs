#!/usr/bin/env node
/**
 * Extract KaTeX layout dimensions for comparison with ratex-layout.
 *
 * Uses the compiled KaTeX dist for renderToString (reliable),
 * plus KaTeX source internals for direct box dimensions.
 *
 * For each LaTeX expression on stdin, outputs one JSON line to stdout.
 */
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { createInterface } from "readline";
import { createRequire } from "module";

const __dirname = dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);

// Use compiled KaTeX dist for renderToString
const katexDist = join(__dirname, "..", "lexer_compare", "node_modules", "katex", "dist", "katex.mjs");
const katex = (await import(katexDist)).default;

/**
 * Extract em values from KaTeX HTML output with their context.
 */
function extractEmFromHtml(html) {
    const results = [];

    // Match style attributes and extract em values
    const styleRegex = /class="([^"]*)"[^>]*style="([^"]*)"/g;
    let match;
    while ((match = styleRegex.exec(html)) !== null) {
        const classes = match[1];
        const style = match[2];
        const emValues = {};

        const propRegex = /([\w-]+)\s*:\s*(-?\d+\.?\d*)em/g;
        let propMatch;
        while ((propMatch = propRegex.exec(style)) !== null) {
            emValues[propMatch[1]] = parseFloat(propMatch[2]);
        }

        if (Object.keys(emValues).length > 0) {
            results.push({ classes, ...emValues });
        }
    }

    // Extract strut info (the most reliable dimension data)
    const strutRegex = /class="strut"[^>]*style="height:\s*(-?\d+\.?\d*)em;?(?:vertical-align:\s*(-?\d+\.?\d*)em)?;?"/g;
    const struts = [];
    while ((match = strutRegex.exec(html)) !== null) {
        const height = parseFloat(match[1]);
        const verticalAlign = match[2] ? parseFloat(match[2]) : 0;
        struts.push({
            totalHeight: round(height),
            depth: round(-verticalAlign),
            ascent: round(height + verticalAlign),
        });
    }

    return { emElements: results, struts };
}

function round(v) {
    return Math.round(v * 100000) / 100000;
}

async function main() {
    const rl = createInterface({ input: process.stdin });

    for await (const line of rl) {
        const expr = line.trim();
        if (expr === "") continue;

        try {
            const html = katex.renderToString(expr, {
                displayMode: true,
                trust: true,
                output: "html",
                throwOnError: true,
            });
            const { emElements, struts } = extractEmFromHtml(html);

            process.stdout.write(JSON.stringify({
                input: expr,
                struts,
                emElements,
                html: html.length < 10000 ? html : undefined,
            }) + "\n");
        } catch (err) {
            process.stdout.write(JSON.stringify({
                error: true,
                message: err.message || String(err),
                input: expr,
            }) + "\n");
        }
    }
}

main().catch((err) => {
    console.error(err);
    process.exit(1);
});

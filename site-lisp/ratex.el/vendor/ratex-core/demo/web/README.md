# RaTeX Web Demo

Static HTML pages that render LaTeX using the RaTeX WASM engine.

## Files

| File | Description |
|------|-------------|
| `index.html` | Interactive formula renderer — type LaTeX, see output instantly |
| `support_table.html` | Table of supported LaTeX commands and symbols |

## Run locally

Any static file server works. Quickest options:

```bash
# Python
python3 -m http.server 8080 --directory demo/web

# Node
npx serve demo/web
```

Then open `http://localhost:8080`.

## Build the WASM bundle first

The demos depend on the compiled WASM output in `platforms/web/dist/`.
Build it from the repo root:

```bash
cd platforms/web && npm install && npm run build
```

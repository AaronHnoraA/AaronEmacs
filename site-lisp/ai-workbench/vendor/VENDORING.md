# Vendoring Notes

This directory holds local vendored copies of upstream packages that
`ai-workbench` will build on.

Current upstream import sources:

- `site-lisp/ai-workbench/magent/` — full embedded Magent source; see
  `../AI-WORKBENCH-UPSTREAM.md`
- `elpa/claude-code-ide/`
- `elpa/codex-cli/`
- `https://github.com/karthink/gptel`

Current vendoring policy:

- keep source files, tests, licenses, and relevant static assets or scripts
- exclude `.git`, compiled `.elc`, autoload files, and `*-pkg.el`
- do not edit the `elpa/` copies in place during migration work
- keep Magent integration changes listed in `AI-WORKBENCH-UPSTREAM.md` so the
  next upstream refresh remains reviewable

Magent itself is never added to `package-selected-packages`; only its supporting
libraries are normal locked dependencies.

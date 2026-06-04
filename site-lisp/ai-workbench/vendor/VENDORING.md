# Vendoring Notes

This directory holds local vendored copies of upstream packages that
`ai-workbench` will build on.

Current upstream import sources:

- `elpa/claude-code-ide/`
- `elpa/codex-cli/`

Current vendoring policy:

- keep source files, tests, licenses, and relevant static assets or scripts
- exclude `.git`, compiled `.elc`, autoload files, and `*-pkg.el`
- do not edit the `elpa/` copies in place during migration work

The next migration step is to load these vendored trees from `ai-workbench`
instead of from the existing `elpa/` package setup.

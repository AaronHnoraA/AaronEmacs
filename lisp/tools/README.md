# tools

Development tools: LSP, Git, projects, run/test/debug, Jupyter, AI.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`agent`](agent/) | yes | AI-assisted IDE integrations | `config.el` |
| [`clutch`](clutch/) | yes | Clutch database client integration | `config.el` |
| [`code-actions`](code-actions/) | yes | Unified code action menu | `config.el` |
| [`connectboard`](connectboard/) | yes | Compatibility entry points for Remote | `config.el` |
| [`copilot`](copilot/) | yes | AI-assisted IDE integrations | `config.el` |
| [`debug-profile`](debug-profile/) | yes | Project debug profiles for Dape | `config.el` |
| [`debugger`](debugger/) | yes | DAP debugging workflow | `config.el` |
| [`direnv`](direnv/) | yes | Routed project environment integration | `config.el`, `direnv.el` |
| [`git`](git/) | yes | Load Git configuration modules | `config.el`, `core.el`, `board.el`, `diff.el`, `tree.el`, `github.el` |
| [`jupyter`](jupyter/) | yes | Jupyter kernelspec management board | `config.el`, `command.el`, `management.el` |
| [`lsp`](lsp/) | yes | The completion engine and lsp client | `config.el`, `runtime.el`, `toolchain.el`, `change-batch.el`, `prewarm.el` |
| [`lsp-hub`](lsp-hub/) | yes | Language server dashboard and diagnostics | `config.el` |
| [`lsp-ops`](lsp-ops/) | yes | Language server operations | `config.el` |
| [`menus`](menus/) | yes | We all like productive tools | `config.el` |
| [`output`](output/) | yes | Unified output buffers | `config.el` |
| [`project`](project/) | yes | Project navigation and integration | `config.el` |
| [`project-local`](project-local/) | yes | Project-local workflow overrides | `config.el` |
| [`run`](run/) | yes | Project run profiles | `config.el` |
| [`symbols`](symbols/) | yes | Buffer and project symbol search | `config.el` |
| [`task`](task/) | yes | Project task runner | `config.el` |
| [`test`](test/) | yes | Project test workflow | `config.el` |
| [`tree-sitter`](tree-sitter/) | yes | The necessary settings | `config.el` |
| [`workspace-symbol`](workspace-symbol/) | yes | Workspace symbol lookup | `config.el` |
| [`zoxide`](zoxide/) | yes | zoxide integration | `config.el` |

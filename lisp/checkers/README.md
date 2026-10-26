# checkers

Diagnostics boards, spelling and grammar.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`diagnostics`](diagnostics/) | yes | Persistent diagnostics buffers | `config.el` |
| [`diagnostics-extra`](diagnostics-extra/) | yes | Extra diagnostics workflows | `config.el` |
| [`grammar`](grammar/) | no — loaded by another module | LanguageTool entry point | `config.el`, `flymake.el` |
| [`problems`](problems/) | yes | Diagnostics pickers | `config.el` |
| [`spell`](spell/) | yes | Spell checker | `config.el` |

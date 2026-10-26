# noema

The Emacs side of Noema: Markdown pages, roam, search, Jupyter bridge, publishing.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`activity`](activity/) | yes | Noema activity snapshots | `config.el` |
| [`bridge`](bridge/) | yes | Noema Web page bridge | `config.el`, `portal.el`, `publish.el`, `agenda-apple.el`, `agenda-source.el`, `note-code.el`, `agent-bridge.el` |
| [`jupyter`](jupyter/) | no — loaded by another module |  | `notebook.el`, `cell.el`, `runtime.el`, `server.el`, `project.el`, `lsp.el`, `debug.el`, `files.el` |
| [`roam`](roam/) | yes | Markdown roam note navigation | `config.el`, `ui.el` |
| [`search`](search/) | yes | Knowledge search answered by the Noema kernel | `config.el` |

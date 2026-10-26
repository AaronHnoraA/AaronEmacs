# remote

The Remote framework library (`remote-*.el`, `backend/`) and its modules; loaded right after the core.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`framework`](framework/) | yes | Unified logical file-system entry point | `config.el`, `tramp.el` |
| [`gateway`](gateway/) | yes | Unified Emacs external gateway | `config.el` |

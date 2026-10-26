# app

Applications inside Emacs.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`browser`](browser/) | yes | Browser integration | `config.el` |
| [`calendar`](calendar/) | yes | Calendar workbench | `config.el` |
| [`joplin`](joplin/) | no — loaded by another module | Joplin integration | `config.el` |

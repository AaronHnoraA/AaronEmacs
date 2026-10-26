# os

Platform and build compatibility (macOS, Neomacs).

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`macos`](macos/) | yes | Tweaks for MacOS | `config.el` |
| [`macos-tools`](macos-tools/) | yes | Lazy macOS utility commands | `config.el` |
| [`neomacs`](neomacs/) | yes | Neomacs runtime compatibility | `config.el` |

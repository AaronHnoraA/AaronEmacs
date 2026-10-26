# config

Defaults, maintenance, health, performance, background app and key layers.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`background`](background/) | yes | Emacs as a background application | `config.el` |
| [`compile`](compile/) | no — loaded by another module | Compile helpers for the local Emacs config | `config.el` |
| [`default`](default/) | yes | Doom-inspired foundation extras | `config.el` |
| [`function-keys`](function-keys/) | yes | Global function-key shortcuts | `config.el` |
| [`health`](health/) | yes | Config health checks | `config.el` |
| [`hyper`](hyper/) | yes | Personal Hyper command surfaces | `config.el` |
| [`keyfreq`](keyfreq/) | yes | Command frequency statistics | `config.el` |
| [`maintenance`](maintenance/) | yes | Migration and state maintenance helpers | `config.el` |
| [`mouse`](mouse/) | yes | Mouse-triggered shortcut layer | `config.el` |
| [`performance`](performance/) | yes | Runtime performance watch board | `config.el` |
| [`server`](server/) | yes | Emacs server & browser editing | `config.el` |

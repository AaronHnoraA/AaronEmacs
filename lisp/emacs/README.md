# emacs

Configuration of built-in Emacs features: Dired, files, scratch, directory locals.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`dir-locals`](dir-locals/) | yes | Dir-locals management and templates | `config.el` |
| [`dired`](dired/) | yes | dired tweaks | `config.el` |
| [`dirvish`](dirvish/) | yes | yazi/ranger keys for Dirvish | `config.el` |
| [`files`](files/) | yes | File and Dired workbench | `config.el` |
| [`rename`](rename/) | yes | Safer file rename helpers | `config.el` |
| [`scratch`](scratch/) | yes | Persistent scratch buffers | `config.el` |
| [`smerge`](smerge/) | yes | Merge conflict helpers | `config.el` |

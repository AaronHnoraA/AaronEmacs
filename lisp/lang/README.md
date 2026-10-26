# lang

Language support.

Modules are declared, in load order, in [`lisp/init-modules.el`](../init-modules.el); see [docs/design/module-system.md](../../docs/design/module-system.md). This index is generated from each module's file headers.

| Module | Declared | Purpose | Files |
| --- | --- | --- | --- |
| [`auctex`](auctex/) | yes | AUCTeX settings | `config.el` |
| [`bazel`](bazel/) | no — loaded by another module | Target-aware Bazel workbench | `config.el` |
| [`beancount`](beancount/) | no — loaded by another module | Beancount configuration | `config.el`, `tool.el`, `dashboard.el`, `entry.el`, `frame.el` |
| [`cpp`](cpp/) | no — loaded by another module | C/C++ config (lsp-mode + clangd) | `config.el` |
| [`elisp`](elisp/) | no — loaded by another module | Emacs Lisp config | `config.el` |
| [`filetypes`](filetypes/) | yes | Common file type routing | `config.el` |
| [`haskell`](haskell/) | no — loaded by another module | haskell | `config.el` |
| [`java`](java/) | no — loaded by another module | Java config | `config.el` |
| [`javascript`](javascript/) | no — loaded by another module | JavaScript mode integration | `config.el` |
| [`latex`](latex/) | yes | LaTeX/BibTeX LSP configuration | `config.el` |
| [`lean`](lean/) | no — loaded by another module | Lean 4 on lsp-mode | `config.el`, `infoview.el`, `jump.el`, `lsp.el` |
| [`markdown`](markdown/) | yes | Open Markdown files in Noema | `config.el` |
| [`nix`](nix/) | no — loaded by another module | Nix language support | `config.el` |
| [`ocaml`](ocaml/) | no — loaded by another module | ocaml | `config.el` |
| [`org`](org/) | yes | Small Org editing tools | `config.el` |
| [`python`](python/) | no — loaded by another module | Python | `config.el` |
| [`rust`](rust/) | no — loaded by another module | Rust configuration | `config.el` |
| [`sh`](sh/) | no — loaded by another module | Shell scripts | `config.el` |
| [`web`](web/) | no — loaded by another module | HTML LSP configuration | `config.el` |

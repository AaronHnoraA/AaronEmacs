# Shared agent implementation sources

Emacs owns these complete source trees and their load paths through
`lisp/tools/agent/upstream.el`. Noema consumes them through the shared agent
layer. Keep the original licenses, source and supporting files intact.

| Directory | Source | Revision |
| --- | --- | --- |
| `gptel/` | https://github.com/karthink/gptel | `ec25a41fb8bebf5ea08341a9d8c70c0ee907ee23` |
| `magent/` | https://github.com/Jamie-Cui/magent | upstream base `412a12cbe9151d11eb66ce3dbdd893324fb36825`, with the existing audited local integration |

These trees were transferred intact from Noema. agent-shell, acp.el,
shell-maker and agent-shell-fork-tree remain pristine, pinned package-vc
dependencies declared in `lisp/tools/agent/config.el` and `package-lock.el`.
ECA is a source study and is not a runtime dependency.

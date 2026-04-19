# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A personal Emacs configuration targeting **Emacs 31** on macOS. Uses `package.el` + `use-package` (no straight/elpaca). Native compilation is enabled and expected.

## Reload / Compile Workflow

After editing any `lisp/init-*.el` file, the change takes effect on next Emacs restart. To apply within a running session:

- **Reload a single file**: `M-x load-file RET <path> RET`  
- **Byte-compile the lisp/ dir**: `SPC c e` (`my/byte-recompile-lisp-dir`) or `M-x my/byte-recompile-lisp-dir`
- **Native-compile the lisp/ dir**: `SPC c E` (`my/native-compile-lisp-dir`)
- **Compile board** (overview + all actions): `SPC c b` (`my/compile-board`)
- **Compile dispatch** (transient menu): `SPC c ?`

There are no shell build scripts. All compilation is driven from within Emacs.

## Architecture

### Load Order

```
early-init.el          → frame params, GC tuning, warning suppression
init.el                → package.el bootstrap, use-package, load-path setup
  └─ init-modules.el   → require all init-*.el in dependency order
       └─ init-compile.el  → loaded last, provides byte/native compile commands
```

`init-modules.el` is the authoritative load order. All `init-*.el` files are `require`d eagerly unless noted. Language-specific modules under `lisp/lang/` and `lisp/org/` are loaded lazily via `my/require-module-after-any-feature`.

### Key Module Responsibilities

| File | Role |
|------|------|
| `init-funcs.el` | Core predicates and utilities used by all other modules (`my/buffer-large-p`, `my/rich-ui-buffer-p`, escape hook, etc.) |
| `init-package-utils.el` | `package.el` helpers, lock file management (`package-lock.el`), VC package registration |
| `init-base.el` | Emacs built-in settings (backup dirs, scroll, uniquify, popper, etc.) |
| `init-ui.el` | Theme (`aaron-ui`/kanagawa wave), doom-modeline, tab-bar workspace styling, shackle popup rules, chunlian dashboard decoration |
| `init-evil.el` | Evil mode + all `SPC` leader keybindings via `define-leader-key` |
| `init-workspaces.el` | tab-bar ↔ perspective sync; workspace create/rename/kill/cycle |
| `init-windows.el` | Buffer/frame helpers, ibuffer, bookmark manager, ace-window, dirvish |
| `init-lsp.el` | Eglot-first LSP routing; `my/lsp-mode-preferred-modes` opts specific modes into lsp-mode |
| `init-project.el` | project.el + perspective integration; winner-ring persistence per workspace |
| `init-compile.el` | Byte/native compile commands and the `*Compile Board*` UI |

### Workspace / Navigation Layers

Four layers, each with its own `SPC` prefix:

- **Frame** (`SPC F`): `n` new, `d` delete, `o`/`O` next/previous, `r` rename
- **Workspace / tab-bar** (`SPC t`): backed by `perspective` + `tab-bar` kept in sync via `init-workspaces.el`; `[`/`]` cycle, `n`/`N` new, `d` close, `tt` pick, `ti` workspace-buffer switch
- **Window** (`SPC w`): `evil-window-map`; `u`/`U` winner-undo/redo; `-`/`/` split
- **Buffer** (`SPC b`): `bb` switch, `bo`/`bO` previous/next, `bk` smart kill, `bK` kill others in workspace, `bi` ibuffer

### Package Management & Lock File

`package-lock.el` at the config root pins exact package versions. To regenerate after adding or upgrading packages:

```
M-x my/package-export-lock-file
```

VC packages (installed via `package-vc`) are registered with `my/package-register-vc` and also tracked in the lock file.

### Adding a New Module

1. Create `lisp/init-foo.el` with `(provide 'init-foo)` at the bottom.
2. Add `(require 'init-foo)` at the appropriate position in `lisp/init-modules.el`.
3. Use `declare-function` at the top of any file that calls into `init-foo` to keep byte-compilation clean.

### Naming Conventions

- All custom functions: `my/` prefix (e.g., `my/workspace-switch-left`)
- All custom faces: `my/` prefix
- All custom groups/variables: `my/` prefix  
- Hooks added by this config: `*-h` suffix (e.g., `my/evil-force-normal-state-h`)
- `init-funcs.el` contains shared primitives; avoid adding module-specific logic there

### LSP Routing

The default is **Eglot** for all modes. To force a mode to use `lsp-mode`, add it to `my/lsp-mode-preferred-modes` in `init-lsp.el`. Both backends coexist; the router picks based on that list.

### Emacs 31 Compatibility

`early-init.el` and `init-ui.el` contain several Emacs 31 workarounds:
- `set-face-attribute` advice to convert `nil` → `'unspecified` for face attributes
- Warning suppression for obsolete API noise from ELPA packages
- `byte-compile-warnings` set to `'(not obsolete)` to silence third-party warnings

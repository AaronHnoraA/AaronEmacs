# dape-toolbar

A debug toolbar for [dape](https://github.com/svaante/dape). It provides a visual toolbar UI that supports mouse clicks, `RET` (when cursor is on button), and keymap shortcuts.

## Requirements

- Emacs 29.2
- dape 0.26.0
- nerd-icons 0.1.0
- compat 30.1.0.1

## Screenshot

<img src="https://github.com/user-attachments/assets/f24a101e-bb9a-4aca-8411-79b2c6be3803" width=70% height=70%>

## Installation

### use-package (Emacs 30+)

```elisp
(use-package dape-toolbar
  :vc (:url "https://github.com/zsxh/dape-toolbar"
       :rev :newest)
  :after dape
  :config
  (dape-toolbar-mode))
```

### Manual

Download `dape-toolbar.el` and add it to your `load-path` or install it via `package-vc-install`:

```elisp
(unless (package-installed-p 'dape-toolbar)
  (package-vc-install
   '(dape-toolbar :url "https://github.com/zsxh/dape-toolbar")))
(require 'dape-toolbar)
;; Enable the toolbar
(dape-toolbar-mode)
```

## Customization

### Built-in Buttons

| Key          | Command                | Description        |
|--------------|------------------------|--------------------|
| `continue`   | `dape-continue`        | Continue execution |
| `step-over`  | `dape-next`            | Step over          |
| `step-in`    | `dape-step-in`         | Step into          |
| `step-out`   | `dape-step-out`        | Step out           |
| `restart`    | `dape-restart`         | Restart debugging  |
| `quit`       | `dape-quit`            | Quit debugging     |
| `disconnect` | `dape-disconnect-quit` | Disconnect adapter |

### Modify Buttons

Each button entry has the structure: `(KEY . (ICON COMMAND HELP-STRING FACE [PREDICATE]))`

- `KEY` - Symbol to identify the button
- `ICON` - Nerd Icons codicon name (e.g., `"nf-cod-debug_continue"`)
- `COMMAND` - Interactive function to call
- `HELP-STRING` - Tooltip text
- `FACE` - Face for the button color
- `PREDICATE` - Optional function that returns non-nil to show the button

```elisp
;; Modify a specific button
(setf (alist-get 'continue dape-toolbar-buttons)
      '("nf-cod-debug_continue" dape-continue "Continue" nerd-icons-green))

;; Add a new button
(push '(my-action . ("nf-cod-run_all" my-command "Run" nerd-icons-green))
      dape-toolbar-buttons)

;; Add a button with visibility predicate
(push '(conditional-btn . ("nf-cod-check" my-command "Check" nerd-icons-blue
                          (lambda () (boundp 'some-var))))
      dape-toolbar-buttons)

;; Remove a button
(setq dape-toolbar-buttons
      (assq-delete-all 'quit dape-toolbar-buttons))
```

### Button Height

```elisp
(setq dape-toolbar-button-height 1.3)
```

## License

GPLv3

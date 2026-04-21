;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)

(declare-function my/bookmark-delete-dwim "init-windows")
(declare-function my/bookmark-dispatch "init-windows")
(declare-function my/bookmark-jump-dwim "init-windows")
(declare-function my/bookmark-jump-other-window-dwim "init-windows")
(declare-function my/bookmark-list "init-windows")
(declare-function my/bookmark-next-line "init-windows")
(declare-function my/bookmark-previous-line "init-windows")
(declare-function my/bookmark-rename-dwim "init-windows")
(declare-function my/bookmark-set "init-windows")
(declare-function my/bookmark-set-no-overwrite "init-windows")
(declare-function my/bookmark-set-line "init-windows")
(declare-function my/bookmark-toggle-line "init-windows")
(declare-function my/kill-buffer-dwim "init-windows")
(declare-function my/kill-workspace-other-buffers "init-windows")
(declare-function my/frame-new "init-windows")
(declare-function my/frame-kill "init-windows")
(declare-function my/frame-other "init-windows")
(declare-function my/frame-previous "init-windows")
(declare-function my/tab-close "init-workspaces")
(declare-function my/tab-close-other-tabs "init-workspaces")
(declare-function my/tab-new "init-workspaces")
(declare-function my/tab-new-named "init-workspaces")
(declare-function my/tab-rename "init-workspaces")
(declare-function my/tab-switch "init-workspaces")
(declare-function my/workspace-dispatch "init-workspaces")
(declare-function my/workspace-other "init-workspaces")
(declare-function my/workspace-switch-buffer "init-workspaces")
(declare-function my/workspace-switch-left "init-workspaces")
(declare-function my/workspace-switch-right "init-workspaces")
(declare-function avy-goto-char-in-line "avy" (&optional arg))
(declare-function avy-goto-char-timer "avy" (&optional arg))
(declare-function evil-emacs-state "evil")
(declare-function evil-force-normal-state "evil")
(declare-function evil-local-mode "evil")
(declare-function evil-save-state "evil")

(defun my/evil-clear-ex-highlights-h ()
  "Clear Evil ex highlights from `my/escape'."
  (when (and (fboundp 'evil-ex-nohighlight)
             (fboundp 'evil-ex-hl-active-p)
             (evil-ex-hl-active-p 'evil-ex-search))
    (evil-ex-nohighlight)
    t))

(defun my/evil-force-normal-state-h ()
  "Return to normal state before `my/escape' falls back to `keyboard-quit'."
  (when (and (bound-and-true-p evil-local-mode)
             (fboundp 'evil-force-normal-state)
             (memq evil-state '(insert replace visual operator)))
    (evil-force-normal-state)
    t))

(defun my/evil--first-indent-width (&rest variables)
  "Return the first positive integer value among VARIABLES."
  (catch 'width
    (dolist (variable variables)
      (when (boundp variable)
        (let ((value (symbol-value variable)))
          (when (and (integerp value) (> value 0))
            (throw 'width value)))))))

(defun my/evil-current-indent-width ()
  "Return the current buffer's effective indentation width."
  (or
   (cond
    ((derived-mode-p 'js-mode 'js2-mode 'js-ts-mode 'js-jsx-mode
                     'typescript-mode 'typescript-ts-mode 'tsx-ts-mode)
     (my/evil--first-indent-width
      'typescript-ts-mode-indent-offset
      'typescript-indent-level
      'js2-basic-offset
      'js-indent-level
      'standard-indent
      'tab-width))
    ((derived-mode-p 'c-mode 'c++-mode 'java-mode 'c-ts-mode 'c++-ts-mode 'java-ts-mode)
     (my/evil--first-indent-width
      'c-ts-mode-indent-offset
      'c-basic-offset
      'standard-indent
      'tab-width))
    ((derived-mode-p 'python-mode 'python-ts-mode)
     (my/evil--first-indent-width
      'python-indent-offset
      'standard-indent
      'tab-width))
    ((derived-mode-p 'rust-mode 'rust-ts-mode)
     (my/evil--first-indent-width
      'rust-indent-offset
      'standard-indent
      'tab-width))
    ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode 'lisp-mode)
     (my/evil--first-indent-width
      'lisp-body-indent
      'standard-indent
      'tab-width))
    (t
     (my/evil--first-indent-width 'standard-indent 'tab-width)))
   4))

(defun my/evil-sync-shift-width ()
  "Keep `evil-shift-width' aligned with the current buffer's indent width."
  (when (boundp 'evil-shift-width)
    (setq-local evil-shift-width (my/evil-current-indent-width))))

(defun my/evil-sync-shift-width-existing-buffers ()
  "Sync `evil-shift-width' for existing buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/evil-sync-shift-width))))

(defun my/evil-avy-goto-char-in-line (&optional arg)
  "Jump to a character on the current line using `avy'."
  (interactive "P")
  (unless (require 'avy nil t)
    (user-error "Avy is unavailable"))
  (avy-goto-char-in-line arg))

(defun my/evil-avy-goto-char-timer (&optional arg)
  "Jump using timed `avy' input."
  (interactive "P")
  (unless (require 'avy nil t)
    (user-error "Avy is unavailable"))
  (avy-goto-char-timer arg))

(defun my/evil-recover ()
  "Re-enable Evil globally and refresh local state in existing buffers."
  (interactive)
  (unless (featurep 'evil)
    (require 'evil))
  (evil-mode 1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'evil-local-mode)
                 (not (minibufferp buffer))
                 (not evil-local-mode))
        (evil-local-mode 1))
      (my/evil-sync-shift-width)))
  (message "Evil recovered"))

(defun my/evil-disable-local-mode-h ()
  "Disable Evil in the current buffer."
  (when (bound-and-true-p evil-local-mode)
    (evil-local-mode -1)))

(defvar my/evil-special-buffer-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<up>") #'previous-line)
    map)
  "Lightweight navigation keymap for special buffers kept in Emacs state.")

(define-minor-mode my/evil-special-buffer-navigation-mode
  "Restore light Vim-style movement in special buffers using Emacs state."
  :init-value nil
  :lighter nil
  :keymap my/evil-special-buffer-navigation-map)

(defun my/evil-special-buffer-setup-h ()
  "Use Emacs state plus local `j/k' navigation in interactive special buffers."
  (when (bound-and-true-p evil-local-mode)
    (my/evil-special-buffer-navigation-mode 1)
    (when (fboundp 'evil-emacs-state)
      (evil-emacs-state))))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (([remap evil-quit] . kill-current-buffer)
         :map evil-motion-state-map
         ([escape] . my/escape)
         ("f" . my/evil-avy-goto-char-in-line)
         :map evil-normal-state-map
         ([escape] . my/escape)
         :map evil-visual-state-map
         ([escape] . my/escape)
         :map evil-operator-state-map
         ([escape] . my/escape)
         :map evil-insert-state-map
         ([escape] . my/escape)
         :map evil-emacs-state-map
         ([escape] . my/escape)
         :map evil-replace-state-map
         ([escape] . my/escape)
         :map evil-normal-state-map
         ("s" . my/evil-avy-goto-char-timer))
  :config
  (unless (bound-and-true-p evil-mode)
    (evil-mode 1))
  (define-advice set-auto-mode (:around (fn &rest args) my/evil-preserve-state)
    "Preserve the current Evil state while `set-auto-mode' changes major modes."
    (if (bound-and-true-p evil-local-mode)
        (evil-save-state (apply fn args))
      (apply fn args)))
  (dolist (mode '(my/bookmark-list-mode))
    (evil-set-initial-state mode 'normal))
  (dolist (mode '(ibuffer-mode
                  debugger-mode
                  dired-mode
                  my/diagnostics-mode
                  my/language-server-manager-mode
                  my/language-server-doctor-mode
                  my/jupyter-manager-mode
                  my/jupyter-doctor-mode
                  my/compile-board-mode
                  my/health-mode))
    (evil-set-initial-state mode 'emacs))
  (add-to-list 'evil-buffer-regexps '("^\\*Appine Window\\*$" . nil))
  (add-to-list 'evil-buffer-regexps '("^\\*vterm.*\\*$" . nil))
  (dolist (hook '(ibuffer-mode-hook
                  debugger-mode-hook
                  my/diagnostics-mode-hook
                  my/language-server-manager-mode-hook
                  my/language-server-doctor-mode-hook
                  my/jupyter-manager-mode-hook
                  my/jupyter-doctor-mode-hook
                  my/compile-board-mode-hook
                  my/health-mode-hook))
    (add-hook hook #'my/evil-special-buffer-setup-h))
  ;; Keep file managers and buffer menus fully in Emacs state instead of
  ;; letting Evil / evil-collection take them over.
  (add-hook 'ibuffer-mode-hook #'my/evil-disable-local-mode-h)
  (add-hook 'dired-mode-hook #'my/evil-disable-local-mode-h)
  (add-hook 'dirvish-mode-hook #'my/evil-disable-local-mode-h)
  (add-hook 'eww-mode-hook #'my/evil-disable-local-mode-h)
  (add-hook 'xwidget-webkit-mode-hook #'my/evil-disable-local-mode-h)
  ;; Silence line out of range error.
  (shut-up! #'evil-indent)
  (add-hook 'my/escape-hook #'my/evil-force-normal-state-h)
  (add-hook 'my/escape-hook #'my/evil-clear-ex-highlights-h)
  (add-hook 'after-change-major-mode-hook #'my/evil-sync-shift-width)
  (add-hook 'hack-local-variables-hook #'my/evil-sync-shift-width)
  (my/evil-sync-shift-width-existing-buffers)
  :custom
  ;; Keep Evil on `undo-tree' so reopened files can reuse persisted undo history.
  (evil-undo-system 'undo-tree)
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-C-g-bindings t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t))
(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))
(use-package evil-collection
  :ensure t
  :init
  (with-eval-after-load 'evil-collection
    (setq evil-collection-mode-list
          (seq-remove
           (lambda (mode)
             (memq (if (consp mode) (car mode) mode)
                   '(vterm eshell ibuffer dired)))
           evil-collection-mode-list)))
  :hook (evil-mode . evil-collection-init)
  :bind (([remap evil-show-marks] . evil-collection-consult-mark)
         ([remap evil-show-jumps] . evil-collection-consult-jump-list))
  :config
  ;; Make `evil-collection-consult-mark' and `evil-collection-consult-jump-list'
  ;; immediately available.
  (evil-collection-require 'consult)
  :custom
  (evil-collection-setup-debugger-keys nil)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-unimpaired-want-repeat-mode-integration t))

;; evil leader map
(use-package evil
  :ensure nil
  :config
  (with-no-warnings
    ;; We use "SPC" as the leader key, "SPC m" as the localleader key. Due to the
    ;; limitation of `evil-set-leader', we can't easily set localleader key with
    ;;
    ;; ``` elisp
    ;; (evil-set-leader 'normal (kbd "SPC m") :localleader)
    ;; ```
    ;;
    ;; An error is prompted:
    ;;
    ;; ``` elisp
    ;; (error "Key sequence SPC m starts with non-prefix key SPC")
    ;; ```
    ;;
    ;; If you know how to fix that, let me know. Thanks.
    (evil-set-leader 'normal (kbd "SPC"))
    (evil-set-leader 'normal (kbd "<leader>m") :localleader)

    (defun define-leader-key (state map localleader &rest bindings)
      "Define leader key in MAP when STATE, a wrapper for
`evil-define-key*'. All BINDINGS are prefixed with \"<leader>\"
if LOCALLEADER is nil, otherwise \"<localleader>\"."
      (cl-assert (cl-evenp (length bindings)))
      (let ((prefix (if localleader "<localleader>" "<leader>"))
            wk-replacements)
        (while bindings
          (let ((key (pop bindings))
                (def (pop bindings)))
            (when (symbolp def)
              (evil-define-key* state map (kbd (concat prefix key)) def))
            ;; Save which-key (key . replacement).
            (pcase def
              (`(:wk ,replacement)
               (push (cons (concat "SPC " key) replacement) wk-replacements)))))
        ;; which-key integration.
        ;; XXX: replacement for localleader NOT supported.
        (cl-loop for (key . replacement) in wk-replacements
                 unless localleader
                 do (which-key-add-key-based-replacements key replacement))))

    (define-leader-key 'normal 'global nil
      ;; SPC, quit minibuffer.
      "SPC" 'keyboard-escape-quit

      ;; Clear highlights
      "S-SPC" 'lazy-highlight-cleanup

      ;; Resume
      "'" 'vertico-repeat
      ";" 'avy-resume

      ;; quit
      "q"  '(:wk "quit")
      "qq" 'save-buffers-kill-terminal
      "qE" 'my/evil-recover

      ;; file
      "f"  '(:wk "files")
      "ff" 'find-file
      "fF" 'find-file-other-window
      "f/" 'find-file-other-window
      "fC" '+copy-current-file
      "fD" '+delete-current-file
      "fy" '+copy-current-filename
      "fR" '+rename-current-file
      "fr" 'recentf-open-files
      "fl" 'find-file-literally
      "fo" 'find-sibling-file
      "fj" 'dired-jump
      "fJ" 'dired-jump-other-window

      ;; buffer
      "b" '(:wk "buffer")
      "bb" 'switch-to-buffer
      "bB" 'switch-to-buffer-other-window
      "bo" 'previous-buffer
      "bO" 'next-buffer
      "bc" 'clone-indirect-buffer
      "bC" 'clone-indirect-buffer-other-window
      "by" '+copy-current-buffer-name
      "bv" 'revert-buffer-quick
      "bz" 'bury-buffer
      "bk" 'my/kill-buffer-dwim
      "bK" 'my/kill-workspace-other-buffers
      "bx" 'kill-buffer-and-window
      "bi" 'ibuffer
      ;; bookmarks
      "b." 'my/bookmark-dispatch
      "bm" 'my/bookmark-set
      "bM" 'my/bookmark-set-no-overwrite
      "br" 'my/bookmark-rename-dwim
      "bj" 'my/bookmark-jump-dwim
      "bJ" 'my/bookmark-jump-other-window-dwim
      "bl" 'my/bookmark-list
      "bn" 'my/bookmark-next-line
      "bp" 'my/bookmark-previous-line
      "bt" 'my/bookmark-toggle-line
      "bL" 'my/bookmark-set-line
      "bs" 'bookmark-save

      ;; edit
      "e"  '(:wk "edit")
      "ed" 'duplicate-line-or-region-below
      "eD" 'duplicate-line-or-region-above
      "eo" 'open-newline-below
      "eO" 'open-newline-above
      "ej" 'move-text-down
      "ek" 'move-text-up
      "e1" 'toggle-one-window

      ;; help
      "h"  '(:wk "help")
      "hf" 'helpful-callable
      "hc" 'helpful-command
      "hv" 'helpful-variable
      "hk" 'helpful-key
      "hw" 'my/show-warnings-buffer
      "hF" 'describe-face
      "hd" 'devdocs-lookup
      "ht" 'tldr

      ;; code
      "c" '(:wk "code")
      "ca" 'my/language-server-code-actions
      "cd" 'rmsbolt-compile
      "cc" 'compile
      "cC" 'recompile
      "cf" 'my/language-server-format-buffer
      "ci" 'show-imenu
      "cI" 'eldoc-box-help-at-point
      "ck" 'kill-compilation
      "cl" '+switch-to-compilation
      "cj" 'dape
      "cr" 'my/language-server-rename
      "cw" 'delete-trailing-whitespace
      "cx" 'quickrun
      "ce" 'my/byte-recompile-lisp-dir
      "cE" 'my/native-compile-lisp-dir

      ;; window
      "w" 'evil-window-map
      "wx" 'kill-buffer-and-window
      "wu" 'winner-undo
      "wU" 'winner-redo
      "w-" 'split-window-vertically
      "w/" 'split-window-horizontally

      ;; frame
      "F" '(:wk "frame")
      "Fn" 'my/frame-new
      "Fd" 'my/frame-kill
      "Fo" 'my/frame-other
      "FO" 'my/frame-previous
      "Fr" 'set-frame-name

      ;; workspace / tab-bar
      "t" '(:wk "workspace")
      "td" 'my/tab-close
      "tD" 'my/tab-close-other-tabs
      "tg" 'my/workspace-dispatch
      "ti" 'my/workspace-switch-buffer
      "tn" 'my/tab-new
      "tN" 'my/tab-new-named
      "to" 'my/workspace-other
      "tt" 'my/tab-switch
      "t'" 'tab-bar-switch-to-recent-tab
      "tr" 'my/tab-rename
      "t[" 'my/workspace-switch-left
      "t]" 'my/workspace-switch-right

      ;; search
      "s" '(:wk "search")
      "sj" 'evil-show-jumps
      "sm" 'evil-show-marks
      "sr" 'evil-show-registers
      "si" 'imenu
      "sp" 'consult-ripgrep
      "ss" 'consult-line

      ;; project
      "p"  '(:wk "project")
      "p." 'my/project-dispatch
      "pp" 'my/project-switch
      "po" 'my/project-open-workbench
      "pf" 'my/project-find-file
      "pr" 'my/project-recent-file
      "pb" 'my/project-switch-buffer
      "ps" 'my/project-ripgrep
      "pd" 'my/project-open-root
      "pm" 'my/project-magit-status
      "pv" 'my/project-vterm
      "pa" 'my/project-add-known-project
      "pD" 'my/project-discover-projects-in-directory
      "pk" 'my/project-kill-buffers
      "px" 'my/project-remove-known-project

      ;; app
      "a" '(:wk "app")
      "aa" 'org-agenda
      "ap" '(:wk "appine")
      "apa" 'my/appine-open-url
      "apf" 'my/appine-open-file
      "app" 'my/appine-open-at-point
      "apr" 'my/appine-reload
      "aph" 'my/appine-back
      "apl" 'my/appine-forward
      "ap[" 'my/appine-prev-tab
      "ap]" 'my/appine-next-tab
      "apc" 'my/appine-close-tab
      "apk" 'my/appine-kill-all
      "apR" 'my/appine-restart
      "aps" 'my/browser-switch-to
      "apS" 'my/browser-open-search
      "ac" 'calendar
      "ag" 'gnus
      "ai" 'rcirc

      ;; open
      "o" '(:wk "open")
      "oc" 'org-capture
      "od" 'dirvish-dwim
      "oD" 'dirvish-fd
      "ol" 'org-store-link
      "ob" 'browse-url
      "oa" 'my/appine-open-url
      "oe" 'vterm-toggle
      "oE" 'my/vterm-popup-cycle
      "oF" 'my/vterm-toggle-fixed
      "ot" 'vterm-toggle
      "oT" 'ansi-term
      "ov" 'vterm
      "oV" 'my/vterm-named
      "oS" 'my/vterm-ssh
      "ow" 'eww-browse-url
      "oW" 'my/browser-open-search
      "ox" 'xwidget-webkit-browse-url
      "oB" 'my/browser-switch-to
      "os" 'shell-toggle)

    (with-eval-after-load 'org
      (define-leader-key 'normal org-mode-map :localleader
        "." 'org-goto
        "a" 'org-archive-subtree
        "d" 'org-deadline
        "e" 'org-set-effort
        "f" 'org-footnote-action
        "l" 'org-lint
        "n" 'org-roam-node-insert
        "N" 'org-roam-node-find
        "o" 'org-toggle-ordered-property
        "p" 'org-set-property
        "q" 'org-set-tags-command
        "r" 'org-refile
        "s" 'org-schedule
        "t" 'org-todo
        "T" 'org-todo-list
        "v" 'my/org-latex-preview-visible-now
        "z" 'my/org-zotero-fill-metadata

        ;; babel
        "bp" 'org-babel-previous-src-block
        "bn" 'org-babel-next-src-block
        "be" 'org-babel-expand-src-block
        "bg" 'org-babel-goto-named-src-block
        "bs" 'org-babel-execute-subtree
        "bb" 'org-babel-execute-buffer
        "bt" 'org-babel-tangle
        "bf" 'org-babel-tangle-file
        "bc" 'org-babel-check-src-block
        "bi" 'org-babel-insert-header-arg
        "bI" 'org-babel-view-src-block-info
        "bk" 'org-babel-remove-result-one-or-many

        ;; clock
        "cc" 'org-clock-in
        "cC" 'org-clock-out
        "cd" 'org-clock-mark-default-task
        "ce" 'org-clock-modify-effort-estimate
        "cg" 'org-clock-goto
        "cl" 'org-clock-in-last
        "cr" 'org-clock-report
        "cs" 'org-clock-display
        "cx" 'org-clock-cancel
        "c=" 'org-clock-timestamps-up
        "c-" 'org-clock-timestamps-down

        ;; insert
        "id" 'org-insert-drawer
        "in" 'org-add-note
        "it" 'org-time-stamp-inactive
        "iT" 'org-time-stamp))

    (with-eval-after-load 'elisp-mode
      (dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
        (define-leader-key 'normal keymap :localleader
          "i" 'info-lookup-symbol

          ;; eval
          "ed" 'eval-defun
          "ee" 'eval-last-sexp
          "el" 'load-library

          ;; goto
          "gf" 'find-function
          "gv" 'find-variable
          "gl" 'find-library)))))








(provide 'init-evil)
;;; init-evil.el ends here

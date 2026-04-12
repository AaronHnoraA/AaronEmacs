;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

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
(declare-function evil-force-normal-state "evil")
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
         ("f" . evil-avy-goto-char-in-line)
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
         ("s" . evil-avy-goto-char-timer))
  :config
  (unless (bound-and-true-p evil-mode)
    (evil-mode 1))
  (define-advice set-auto-mode (:around (fn &rest args) my/evil-preserve-state)
    "Preserve the current Evil state while `set-auto-mode' changes major modes."
    (if (bound-and-true-p evil-local-mode)
        (evil-save-state (apply fn args))
      (apply fn args)))
  (add-to-list 'evil-buffer-regexps '("^\\*vterm.*\\*$" . nil))
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
                   '(vterm eshell)))
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

      ;; buffer & bookmark
      "b" '(:wk "bufmark")
      "bb" 'switch-to-buffer
      "bB" 'switch-to-buffer-other-window
      "bc" 'clone-indirect-buffer
      "bC" 'clone-indirect-buffer-other-window
      "by" '+copy-current-buffer-name
      "bv" 'revert-buffer-quick
      "bz" 'bury-buffer
      ;; --------------
      "b." 'my/bookmark-dispatch
      "bm" 'my/bookmark-set
      "bM" 'my/bookmark-set-no-overwrite
      "bi" 'bookmark-insert
      "br" 'my/bookmark-rename-dwim
      "bw" 'bookmark-write
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
      "wu" '+transient-tab-bar-history
      "w-" 'split-window-vertically
      "w/" 'split-window-horizontally

      ;; tab
      "t" '(:wk "tab")
      "tc" 'tab-bar-close-tab
      "tC" 'tab-bar-close-group-tabs
      "tg" 'tab-bar-change-tab-group
      "ti" 'tab-switcher
      "tn" 'tab-bar-new-tab
      "to" 'tab-bar-close-other-tabs
      "tt" 'tab-bar-switch-to-tab
      "t'" 'tab-bar-switch-to-recent-tab
      "tr" 'tab-bar-rename-tab
      "t[" 'centaur-tabs-backward
      "t]" 'centaur-tabs-forward
      "t{" 'centaur-tabs-backward-group
      "t}" 'centaur-tabs-forward-group

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
      "oe" 'vterm-toggle
      "oE" 'my/vterm-popup-cycle
      "oF" 'my/vterm-toggle-fixed
      "ot" 'vterm-toggle
      "oT" 'ansi-term
      "ov" 'vterm
      "oV" 'my/vterm-named
      "oS" 'my/vterm-ssh
      "ow" 'eww-browse-url
      "ox" 'xwidget-webkit-browse-url
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

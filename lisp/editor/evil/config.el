;;; editor/evil/config.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'config)
(require 'aaron-funcs)

(declare-function bookmark-actions-menu "ui/windows/config")
(declare-function +bookmark/next-line "ui/windows/config")
(declare-function +bookmark/previous-line "ui/windows/config")
(declare-function +bookmark/set-line "ui/windows/config")
(declare-function +bookmark/toggle-line "ui/windows/config")
(declare-function debug-menu "tools/debugger/config" ())
(declare-function +windows/kill-buffer-dwim "ui/windows/config")
(declare-function +windows/window-bury-current-buffer "ui/windows/config" ())
(declare-function +windows/kill-workspace-other-buffers "ui/windows/config")
(declare-function +windows/frame-new "ui/windows/config")
(declare-function +windows/frame-kill "ui/windows/config")
(declare-function +windows/frame-other "ui/windows/config")
(declare-function +windows/frame-previous "ui/windows/config")
(declare-function +background/frame-hide "config/background/config")
(declare-function +workspaces/tab-new "ui/workspaces/config")
(declare-function +workspaces/tab-new-named "ui/workspaces/config")
(declare-function +workspaces/tab-rename "ui/workspaces/config")
(declare-function workspace-menu "ui/workspaces/config")
(declare-function +workspace/other "ui/workspaces/config")
(declare-function +workspace/switch-buffer "ui/workspaces/config")
(declare-function +workspace/switch-left "ui/workspaces/config")
(declare-function +workspace/switch-right "ui/workspaces/config")
(declare-function avy-goto-char-in-line "avy" (char))
(declare-function avy-goto-char-timer "avy" (&optional arg))
(declare-function evil-emacs-state "evil")
(declare-function evil-force-normal-state "evil")
(declare-function evil-local-mode "evil")
(declare-function evil-normal-state "evil")
(declare-function evil-save-state "evil")
(declare-function flymake-goto-next-error "flymake" (&optional n filter interactive))
(declare-function flymake-goto-prev-error "flymake" (&optional n filter interactive))
(declare-function wgrep-mark-deletion "wgrep" ())

(defun +evil-clear-ex-highlights-h ()
  "Clear Evil ex highlights from `aaron/escape'."
  (when (and (fboundp 'evil-ex-nohighlight)
             (fboundp 'evil-ex-hl-active-p)
             (evil-ex-hl-active-p 'evil-ex-search))
    (evil-ex-nohighlight)
    t))

(defun +evil-escape-state-h ()
  "Handle Evil state and search cleanup with a single Escape.
Both actions must run before reporting success: `aaron-escape-hook' stops after
the first non-nil result, so separate hooks could let an active search
highlight swallow Escape before Evil returned to normal state."
  (let ((state-changed (+evil-normal-state-maybe))
        (highlight-cleared (+evil-clear-ex-highlights-h)))
    (or state-changed highlight-cleared)))

(defconst +evil-emacs-state-modes
  '(ibuffer-mode
    debugger-mode
    dired-mode
    dirvish-mode
    eww-mode
    xwidget-webkit-mode
    +diagnostics-mode
    +language-server-manager-mode
    +language-server-doctor-mode
    +compile-board-mode
    org-agenda-mode
    +health-mode
	gptel-mode)
  "Major modes that intentionally stay outside Evil normal-state editing.")

(defun +evil-normal-editing-buffer-p ()
  "Return non-nil when the current buffer should recover to Evil normal state."
  (and (not (minibufferp))
       (not (apply #'derived-mode-p +evil-emacs-state-modes))
       (not (string-match-p "\\` \\|\\`\\*"
                            (buffer-name)))))

(defun +evil-normal-state-maybe ()
  "Recover Evil normal state in ordinary editing buffers.
This is deliberately conservative so special buffers that are configured for
Emacs state keep their local behavior."
  (when (and (bound-and-true-p evil-mode)
             (+evil-normal-editing-buffer-p))
    (unless (bound-and-true-p evil-local-mode)
      (when (fboundp 'evil-local-mode)
        (evil-local-mode 1)))
    (when (and (bound-and-true-p evil-local-mode)
               (boundp 'evil-state)
               (memq evil-state '(insert replace visual operator emacs motion)))
      (cond
       ((fboundp 'evil-force-normal-state)
        (evil-force-normal-state))
       ((fboundp 'evil-normal-state)
        (evil-normal-state)))
      t)))

(defun +evil--first-indent-width (&rest variables)
  "Return the first positive integer value among VARIABLES."
  (catch 'width
    (dolist (variable variables)
      (when (boundp variable)
        (let ((value (symbol-value variable)))
          (when (and (integerp value) (> value 0))
            (throw 'width value)))))))

(defun +evil-current-indent-width ()
  "Return the current buffer's effective indentation width."
  (or
   (cond
    ((derived-mode-p 'js-mode 'js2-mode 'js-ts-mode 'js-jsx-mode
                     'typescript-mode 'typescript-ts-mode 'tsx-ts-mode)
     (+evil--first-indent-width
      'typescript-ts-mode-indent-offset
      'typescript-indent-level
      'js2-basic-offset
      'js-indent-level
      'standard-indent
      'tab-width))
    ((derived-mode-p 'c-mode 'c++-mode 'java-mode 'c-ts-mode 'c++-ts-mode 'java-ts-mode)
     (+evil--first-indent-width
      'c-ts-mode-indent-offset
      'c-basic-offset
      'standard-indent
      'tab-width))
    ((derived-mode-p 'python-mode 'python-ts-mode)
     (+evil--first-indent-width
      'python-indent-offset
      'standard-indent
      'tab-width))
    ((derived-mode-p 'rust-mode 'rust-ts-mode)
     (+evil--first-indent-width
      'rust-indent-offset
      'standard-indent
      'tab-width))
    ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode 'lisp-mode)
     (+evil--first-indent-width
      'lisp-body-indent
      'standard-indent
      'tab-width))
    (t
     (+evil--first-indent-width 'standard-indent 'tab-width)))
   4))

(defun +evil-sync-shift-width ()
  "Keep `evil-shift-width' aligned with the current buffer's indent width."
  (when (boundp 'evil-shift-width)
    (setq-local evil-shift-width (+evil-current-indent-width))))

(defun +evil-sync-shift-width-existing-buffers ()
  "Sync `evil-shift-width' for existing buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (+evil-sync-shift-width))))

;; Native Vim (Neovim's default `formatoptions' has `j') drops the comment
;; leader of a joined line when both lines are comments: "// a" J "// b"
;; gives "// a b", where Evil keeps "// a // b".  Doom patches the same.
(defun +evil--drop-joined-comment-leader ()
  "Delete the next line's comment leader when this line ends in a comment."
  (when (and comment-start
             (save-excursion (nth 4 (syntax-ppss (line-end-position)))))
    (save-excursion
      (forward-line 1)
      (skip-chars-forward " \t")
      (let ((leader-start (point)))
        (cond
         ((and comment-start-skip
               (eq (save-excursion
                     (comment-search-forward (line-end-position) t))
                   leader-start))
          (comment-search-forward (line-end-position) t)
          (delete-region leader-start (point)))
         ;; A middle line of a block comment: the mode's `comment-continue',
         ;; or the conventional "*" of C-like comments (not a closing "*/").
         ((and (nth 4 (syntax-ppss leader-start))
               (not (looking-at "\\*/"))
               (looking-at
                (concat (if (and comment-continue
                                 (not (string-blank-p comment-continue)))
                            (regexp-quote (string-trim comment-continue))
                          "\\*+")
                        "[ \t]*")))
          (delete-region leader-start (match-end 0))))))))

(defun +evil-join-comments-a (fn &rest args)
  "Run the `evil-join' FN with ARGS, dropping comment leaders as Vim does."
  (letf! (defadvice join-line (:before (&rest _) +evil--comment-leader)
           (+evil--drop-joined-comment-leader))
    (apply fn args)))

;; Vim's gq and gw keep the spacing inside a line; `fill-region' squeezes it.
(defun +evil-fill-no-squeeze-a (fn &rest args)
  "Run the Evil fill FN with ARGS without squeezing spaces."
  (letf! (defadvice fill-region (:around (fill from to &optional justify _nosqueeze to-eop)
                                         +evil--no-squeeze)
           (funcall fill from to justify t to-eop))
    (apply fn args)))

(defun +evil-delete-selection-off-h ()
  "Leave `delete-selection-mode' when Insert state ends."
  (delete-selection-mode -1))

(defun +evil/avy-goto-char-in-line (char)
  "Jump to a character on the current line using `avy'."
  (interactive (list (read-char "char: " t)))
  (unless (require 'avy nil t)
    (user-error "Avy is unavailable"))
  (avy-goto-char-in-line char))

(defun +evil/avy-goto-char-timer (&optional arg)
  "Jump using timed `avy' input."
  (interactive "P")
  (unless (require 'avy nil t)
    (user-error "Avy is unavailable"))
  (avy-goto-char-timer arg))

(defun +evil/recover ()
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
      (+evil-sync-shift-width)))
  (message "Evil recovered"))

(defun +evil-disable-local-mode-h ()
  "Disable Evil in the current buffer."
  (when (bound-and-true-p evil-local-mode)
    (evil-local-mode -1)))

(defvar +evil-special-buffer-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<up>") #'previous-line)
    map)
  "Lightweight navigation keymap for special buffers kept in Emacs state.")

(define-minor-mode +evil-special-buffer-navigation-mode
  "Restore light Vim-style movement in special buffers using Emacs state."
  :init-value nil
  :lighter nil
  :keymap +evil-special-buffer-navigation-map)

(defun +evil-special-buffer-setup-h ()
  "Use Emacs state plus local `j/k' navigation in interactive special buffers."
  (+evil-special-buffer-navigation-mode 1)
  (when (bound-and-true-p evil-local-mode)
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
         ([escape] . aaron/escape)
         ("f" . +evil/avy-goto-char-in-line)
         :map evil-normal-state-map
         ([escape] . aaron/escape)
         :map evil-visual-state-map
         ([escape] . aaron/escape)
         :map evil-operator-state-map
         ([escape] . aaron/escape)
         :map evil-insert-state-map
         ([escape] . aaron/escape)
         :map evil-emacs-state-map
         ([escape] . aaron/escape)
         :map evil-replace-state-map
         ([escape] . aaron/escape)
         :map evil-normal-state-map
         ("s" . +evil/avy-goto-char-timer))
  :config
  (unless (bound-and-true-p evil-mode)
    (evil-mode 1))
  (define-advice set-auto-mode (:around (fn &rest args) +evil-preserve-state)
    "Preserve the current Evil state while `set-auto-mode' changes major modes."
    (if (bound-and-true-p evil-local-mode)
        (evil-save-state (apply fn args))
      (apply fn args)))
  ;; Built-in and package modes; a module declares its own modes' state
  ;; with `set-evil-initial-state!'.
  (dolist (mode '(ibuffer-mode
                  bookmark-bmenu-mode
                  debugger-mode
                  dired-mode
                  org-agenda-mode
                  gptel-mode))
    (evil-set-initial-state mode 'emacs))
  (add-to-list 'evil-buffer-regexps '("^\\*portal.*\\*$" . nil))
  (dolist (hook '(ibuffer-mode-hook
                  debugger-mode-hook
                  +diagnostics-mode-hook
                  +language-server-manager-mode-hook
                  +language-server-doctor-mode-hook
                  +compile-board-mode-hook
                  org-agenda-mode-hook
				  +health-mode-hook
				  gptel-mode-hook))
    (add-hook hook #'+evil-special-buffer-setup-h))
  ;; Keep file managers and buffer menus fully in Emacs state instead of
  ;; letting Evil / evil-collection take them over.
  (add-hook 'ibuffer-mode-hook #'+evil-disable-local-mode-h)
  (add-hook 'dired-mode-hook #'+evil-disable-local-mode-h)
  (add-hook 'dirvish-mode-hook #'+evil-disable-local-mode-h)
  (add-hook 'eww-mode-hook #'+evil-disable-local-mode-h)
  (add-hook 'xwidget-webkit-mode-hook #'+evil-disable-local-mode-h)
  (dolist (hook '(org-agenda-mode-hook))
    (add-hook hook #'+evil-disable-local-mode-h))
	(dolist (hook '(gptel-mode-hook))
    (add-hook hook #'+evil-disable-local-mode-h))
  ;; Silence line out of range error.
  (shut-up! #'evil-indent)
  (advice-add #'evil-join :around #'+evil-join-comments-a)
  (advice-add #'evil-fill :around #'+evil-fill-no-squeeze-a)
  (advice-add #'evil-fill-and-move :around #'+evil-fill-no-squeeze-a)
  ;; Remove the former split handlers as well, so reloading this module fixes
  ;; an already-running Emacs instead of leaving their short-circuit order.
  (remove-hook 'aaron-escape-hook #'+evil-force-normal-state-h)
  (remove-hook 'aaron-escape-hook #'+evil-clear-ex-highlights-h)
  (add-hook 'aaron-escape-hook #'+evil-escape-state-h)
  ;; From Doom: in Insert state, typing over a selection (made with the mouse
  ;; or shifted motion) replaces it, as everywhere else on the system.
  (add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
  (add-hook 'evil-insert-state-exit-hook #'+evil-delete-selection-off-h)
  (add-hook 'after-change-major-mode-hook #'+evil-sync-shift-width)
  (add-hook 'hack-local-variables-hook #'+evil-sync-shift-width)
  (+evil-sync-shift-width-existing-buffers)
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
;;; Neovim's default mappings

;; Neovim 0.10 and 0.11 ship mappings that Vim users otherwise copy from
;; vim-unimpaired and vim-commentary; Evil has none of them.  Doom binds most
;; of the same keys, but where Doom's choice differs from Neovim's (]f for the
;; next file, gp to reselect a paste, ]c for comments) Neovim wins, because
;; native Vim is this layer's reference.

(defun +evil--blank-lines (count above)
  "Add COUNT blank lines below the current line, or ABOVE it."
  (let ((origin (point-marker)))
    (set-marker-insertion-type origin (not above))
    (unwind-protect
        (progn
          (if above (forward-line 0) (end-of-line))
          (insert (make-string count ?\n))
          (goto-char origin))
      (set-marker origin nil))))

(defun +evil/insert-blank-line-below (count)
  "Add COUNT blank lines below the current line, as Neovim's ] SPC."
  (interactive "p")
  (+evil--blank-lines count nil))

(defun +evil/insert-blank-line-above (count)
  "Add COUNT blank lines above the current line, as Neovim's [ SPC."
  (interactive "p")
  (+evil--blank-lines count t))

(defun +evil/next-buffer (count)
  "Show the COUNTth next buffer, as Neovim's ]b."
  (interactive "p")
  (dotimes (_ count) (next-buffer)))

(defun +evil/previous-buffer (count)
  "Show the COUNTth previous buffer, as Neovim's [b."
  (interactive "p")
  (dotimes (_ count) (previous-buffer)))

(defun +evil/next-error (count)
  "Go to the COUNTth next error or match, as Neovim's ]q over quickfix."
  (interactive "p")
  (next-error count))

(defun +evil/previous-error (count)
  "Go to the COUNTth previous error or match, as Neovim's [q."
  (interactive "p")
  (previous-error count))

(defun +evil/next-diagnostic (count)
  "Go to the COUNTth next diagnostic, as Neovim's ]d."
  (interactive "p")
  (flymake-goto-next-error count nil t))

(defun +evil/previous-diagnostic (count)
  "Go to the COUNTth previous diagnostic, as Neovim's [d."
  (interactive "p")
  (flymake-goto-prev-error count nil t))

(evil-define-operator +evil:comment (beg end)
  "Comment or uncomment the lines from BEG to END, as Neovim's gc.
Lines that are all comments are uncommented; otherwise all are commented."
  :type line
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(defun +evil--visual-search (forward)
  "Search for the Visual selection as literal text, FORWARD or backward."
  (let ((beg (region-beginning))
        (text (buffer-substring-no-properties (region-beginning) (region-end))))
    (evil-exit-visual-state)
    (goto-char beg)
    (setq isearch-forward forward)
    (evil-search (regexp-quote text) forward t)))

(defun +evil/visual-search-forward ()
  "Search forward for the selected text, as Neovim's Visual *."
  (interactive)
  (+evil--visual-search t))

(defun +evil/visual-search-backward ()
  "Search backward for the selected text, as Neovim's Visual #."
  (interactive)
  (+evil--visual-search nil))

(defun +evil/visual-execute-macro ()
  "Run a register on every selected line, as Neovim's linewise Visual @.
In a charwise or block selection this is the ordinary `evil-execute-macro'."
  (interactive)
  (if (not (eq (evil-visual-type) 'line))
      (call-interactively #'evil-execute-macro)
    (let ((beg (region-beginning))
          (end (region-end))
          (register (evil-read-key)))
      (evil-exit-visual-state)
      (evil-ex-normal beg end (concat "@" (char-to-string register))))))

(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
  "Select the whole buffer (Doom's ig and ag)."
  (evil-range (point-min) (point-max) type))

;; Vim's file name modifiers after % and # (`:e %:h/other.el'); Evil
;; expands the bare characters only.  Applied left to right, as in Vim.
(defun +evil--file-name-modifiers (name modifiers)
  "Return file NAME after applying the Vim MODIFIERS string, such as \":p:h\"."
  (dolist (modifier (split-string modifiers ":" t) name)
    (setq name
          (pcase modifier
            ("p" (expand-file-name name))
            ("~" (abbreviate-file-name (expand-file-name name)))
            ("." (file-relative-name name))
            ("h" (if (file-name-directory name)
                     (directory-file-name (file-name-directory name))
                   "."))
            ("t" (file-name-nondirectory name))
            ("r" (file-name-sans-extension name))
            ("e" (or (file-name-extension name) ""))))))

(defun +evil-expand-file-name-modifiers-a (fn file-name)
  "Expand %:MODS and #:MODS in FILE-NAME before FN expands bare % and #."
  (let ((names `(("%" . ,(buffer-file-name))
                 ("#" . ,(and (other-buffer) (buffer-file-name (other-buffer))))))
        (start 0))
    (while (string-match "\\(?:^\\|[^\\\\]\\)\\(\\([%#]\\)\\(\\(?::[p~.htre]\\)+\\)\\)"
                         file-name start)
      (let ((name (cdr (assoc (match-string 2 file-name) names))))
        (if (not name)
            (setq start (match-end 0))
          (let* ((beg (match-beginning 1))
                 (end (match-end 1))
                 (expanded (+evil--file-name-modifiers
                            name (match-string 3 file-name))))
            (setq file-name (concat (substring file-name 0 beg) expanded
                                    (substring file-name end))
                  start (+ beg (length expanded)))))))
    (funcall fn file-name)))

;; In wgrep, `d' on a line whose file name prefix is read-only marks the
;; line for deletion instead of signalling an error (from Doom).
(evil-define-operator +evil:wgrep-delete (beg end type register yank-handler)
  "Delete from BEG to END, or mark the lines for deletion in wgrep."
  (interactive "<R><x><y>")
  (condition-case nil
      (evil-delete beg end type register yank-handler)
    (text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))

(advice-add #'evil-ex-replace-special-filenames
            :around #'+evil-expand-file-name-modifiers-a)
(map! :n "] SPC" #'+evil/insert-blank-line-below
      :n "[ SPC" #'+evil/insert-blank-line-above
      :n "]b"    #'+evil/next-buffer
      :n "[b"    #'+evil/previous-buffer
      :m "]q"    #'+evil/next-error
      :m "[q"    #'+evil/previous-error
      :m "]d"    #'+evil/next-diagnostic
      :m "[d"    #'+evil/previous-diagnostic
      :nv "gc"   #'+evil:comment
      :v "*"     #'+evil/visual-search-forward
      :v "#"     #'+evil/visual-search-backward
      :v "@"     #'+evil/visual-execute-macro
      :textobj "g" #'+evil:whole-buffer-txtobj #'+evil:whole-buffer-txtobj)
(after! wgrep
  (define-key wgrep-mode-map [remap evil-delete] #'+evil:wgrep-delete))

;; Neovim's `vim.hl.on_yank()': the yanked text flashes in the IncSearch
;; face for 150 ms, not while a macro runs.
(config-defvar +evil-yank-highlight-duration 0.15
  "Seconds the yanked text stays highlighted; nil turns it off."
  :type '(choice (const :tag "Off" nil) number)
  :group 'evil)
(defvar +evil--yank-highlight-overlays nil)
(defvar +evil--yank-highlight-timer nil)

(defun +evil--clear-yank-highlight ()
  "Remove the yank highlight and its timer."
  (when (timerp +evil--yank-highlight-timer)
    (cancel-timer +evil--yank-highlight-timer))
  (setq +evil--yank-highlight-timer nil)
  (mapc #'delete-overlay +evil--yank-highlight-overlays)
  (setq +evil--yank-highlight-overlays nil))

(defadvice! +evil-highlight-yank-a (beg end &optional type &rest _)
  "Flash the text `evil-yank' just copied, as Neovim's on_yank does."
  :after #'evil-yank
  (when (and +evil-yank-highlight-duration
             (not executing-kbd-macro)
             (< beg end))
    (+evil--clear-yank-highlight)
    (let ((ranges (if (eq type 'block)
                      (let (acc)
                        (evil-apply-on-block
                         (lambda (b e) (push (cons b e) acc)) beg end nil)
                        acc)
                    (list (cons beg end)))))
      (dolist (range ranges)
        (let ((ov (make-overlay (car range) (cdr range))))
          (overlay-put ov 'face 'isearch)
          (overlay-put ov 'priority 1000)
          (push ov +evil--yank-highlight-overlays))))
    (setq +evil--yank-highlight-timer
          (run-with-timer +evil-yank-highlight-duration nil
                          #'+evil--clear-yank-highlight))))

;; Picking a place from a list is a jump, as `/' is: C-o comes back.
(evil-add-command-properties #'imenu :jump t)
(evil-add-command-properties #'outline-up-heading :jump t)
(after! consult
  (dolist (command '(consult-line consult-line-multi consult-imenu
                     consult-imenu-multi consult-outline consult-mark))
    (evil-add-command-properties command :jump t)))

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
                   '(portal-terminal eshell ibuffer dired xwidget)))
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
  (evil-collection-calendar-want-org-bindings nil)
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
    (evil-set-leader 'normal (kbd "<leader>m") :localleader))

  (define-leader-key!
    ;; Clear highlights
    "S-SPC" 'lazy-highlight-cleanup

    ;; Resume
    "'" 'vertico-repeat
    ";" 'avy-resume

    ;; quit
    "q"  '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qE" '+evil/recover

    ;; file
    "f"  '(:ignore t :which-key "files")
    "ff" 'file-open
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
    "f." 'file-menu

    ;; buffer
    "b"  '(:ignore t :which-key "buffer")
    "bb" 'consult-buffer
    "bB" 'consult-buffer-other-window
    "bF" 'buffer-open-new-frame
    "bo" 'previous-buffer
    "bO" 'next-buffer
    "bc" 'clone-indirect-buffer
    "bC" 'clone-indirect-buffer-other-window
    "by" '+copy-current-buffer-name
    "bv" 'revert-buffer-quick
    "bz" '(:def +windows/window-bury-current-buffer :which-key "bury buffer")
    "bk" '+windows/kill-buffer-dwim
    "bK" '+windows/kill-workspace-other-buffers
    "bi" 'ibuffer
    ;; bookmarks
    "b." 'bookmark-actions-menu
    "bm" 'bookmark-set
    "bM" 'bookmark-set-no-overwrite
    "br" 'bookmark-rename
    "bj" 'consult-bookmark
    "bJ" 'bookmark-jump-other-window
    "bl" 'list-bookmarks
    "bn" '+bookmark/next-line
    "bp" '+bookmark/previous-line
    "bt" '+bookmark/toggle-line
    "bL" '+bookmark/set-line
    "bs" 'bookmark-save

    ;; edit
    "e"  '(:ignore t :which-key "edit")
    "ed" 'duplicate-line-or-region-below
    "eD" 'duplicate-line-or-region-above
    "eo" 'open-newline-below
    "eO" 'open-newline-above
    "ej" 'move-text-down
    "ek" 'move-text-up
    "eb" '+pairs/cycle-delimiters
    "e1" 'toggle-one-window
    "e." 'editing-menu

    ;; help
    "h"  '(:ignore t :which-key "help")
    "hf" 'helpful-callable
    "hC" 'helpful-command
    "hc" '(:def config-dispatch :which-key "config")
    "hv" 'helpful-variable
    "hk" 'helpful-key
    "hK" 'aaron/open-keybinding-guide
    "hm" '+menus/mode-keybindings
    "hw" 'aaron/show-warnings-buffer
    "hF" 'describe-face
    "hd" 'devdocs-lookup
    "ht" 'tldr

    ;; code
    "c"  '(:ignore t :which-key "code")
    "ca" '+language-server/code-actions
    "cd" 'rmsbolt-compile
    "cc" 'compile
    "cC" 'recompile
    "cf" '+language-server/format-buffer
    "ci" 'show-imenu
    "cI" 'lsp-ui-doc-glance
    "ck" 'kill-compilation
    "cl" '+switch-to-compilation
    "cL" 'lsp-lens-mode
    "cr" '+language-server/rename
    "cw" 'delete-trailing-whitespace
    "cx" 'quickrun
    "ce" '+compile/byte-recompile-lisp-dir
    "cE" '+compile/native-compile-lisp-dir

    ;; specialized run workbenches
    "rz" 'bazel-target-menu

    ;; window
    "w"  'evil-window-map
    "wx" 'kill-buffer-and-window
    "wu" 'winner-undo
    "wU" 'winner-redo
    "w-" 'split-window-vertically
    "w/" 'split-window-horizontally

    ;; frame
    "F"  '(:ignore t :which-key "frame")
    "Fn" '+windows/frame-new
    "Fd" '+windows/frame-kill
    "Fh" '+background/frame-hide
    "Fo" '+windows/frame-other
    "FO" '+windows/frame-previous
    "Fr" 'set-frame-name

    ;; workspace / tab-bar
    "t"  '(:ignore t :which-key "workspace")
    "td" 'tab-close
    "tD" 'tab-close-other
    "tg" 'workspace-menu
    "ti" '+workspace/switch-buffer
    "tn" '+workspaces/tab-new
    "tN" '+workspaces/tab-new-named
    "to" '+workspace/other
    "tt" 'tab-switch
    "t'" 'tab-bar-switch-to-recent-tab
    "tr" '+workspaces/tab-rename
    "t[" '+workspace/switch-left
    "t]" '+workspace/switch-right

    ;; search
    "s"  '(:ignore t :which-key "search")
    "sj" 'evil-show-jumps
    "sm" 'evil-show-marks
    "sr" 'evil-show-registers
    "si" 'imenu
    "sp" '+project/ripgrep
    "ss" 'consult-line

    ;; project
    "p"  '(:ignore t :which-key "project")
    "p." 'project-menu
    "pp" '+project/switch
    "po" '+project/open-workbench
    "pf" '+project/find-file
    "pr" '+project/recent-file
    "pb" '+project/switch-buffer
    "ps" '+project/ripgrep
    "pd" '+project/open-root
    "pm" '+project/magit-status
    "pv" '+project/portal
    "pG" '+project/portal
    "pa" '+project/add-known-project
    "pD" '+project/discover-projects-in-directory
    "pk" '+project/kill-buffers

    ;; app
    "a"  '(:ignore t :which-key "app")
    "as" '+browser/switch-to
    "aS" '+browser/open-search
    "ac" 'calendar
    "ay" '+calendar/calendar-year
    "am" 'macos-tools-menu
    "ag" 'gnus
    "ai" 'rcirc

    ;; open
    "o"  '(:ignore t :which-key "open")
    "od" 'dirvish-dwim
    "oD" 'dirvish-fd
    "ob" 'browse-url
    "oe" '+portal-popup/toggle
    "og" '+portal/terminal
    "oE" '+portal-popup/cycle
    "oF" '+portal/toggle-fixed
    "oq" 'clutch-query-console
    "ot" '+portal-popup/toggle
    "oT" 'ansi-term
    "ov" '+portal/terminal
    "oV" '+portal/named
    "oS" '+portal/ssh
    "ow" 'aaron/open-eww-url
    "oW" '+browser/open-search
    "ox" '+xwidget/open-url
    "oB" '+browser/switch-to
    "os" '(:def +eshell/toggle :which-key "command shell")
    "op" 'proced
    "oP" '+portal/terminal
    "ol" '+beancount/beancount)

)

(provide 'editor/evil/config)
;;; editor/evil/config.el ends here

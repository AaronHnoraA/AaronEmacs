;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui)
(require 'init-open)

(defvar my/file-open-new-frame-prompt nil
  "Non-nil while a file-open prompt accepts a new-frame exit key.")

(defvar my/file-open-new-frame-requested nil
  "Set by the file-open minibuffer's Cmd+Shift+Enter key.")

(defvar my/file-open-placement 'current
  "Where to visit the file chosen by the current file-open prompt.")

(defvar my/terminal-selector-active nil)

(defvar my/consult-buffer-active nil
  "Non-nil while a normal Consult buffer switcher is reading a choice.")

(defvar my/consult-buffer-placement 'current
  "Placement requested in the current Consult buffer switcher.")

(defvar my/consult-buffer-origin nil
  "Window that opened the current Consult buffer switcher.")

(declare-function file-open-new-frame "init-windows" (file &optional wildcards))
(declare-function my/open-entry-in-split "init-windows" (side open))
(declare-function my/terminal-selector-exit "init-portal" (placement))
(declare-function my/buffer-display-new-frame "init-windows"
                  (buffer &optional norecord))
(declare-function consult--frame-buffer-list "consult" ())
(declare-function my/frame-workspace-buffers "init-tabbar"
                  (&optional frame-buffers))
(defvar consult--buffer-display)
(defvar consult-buffer-list-function)

(defun my/consult-buffer-list ()
  "List this frame's buffers in the active Perspective for Consult.
Use Consult's frame history, including buried buffers.  Consult's hidden
Other Buffer source still provides access to buffers outside this scope."
  (my/frame-workspace-buffers (consult--frame-buffer-list)))

(defun my/consult-buffer-scoped-a (original &optional sources)
  "Call ORIGINAL with frame/workspace buffers for the default SOURCES.
Commands such as `consult-project-buffer' pass explicit sources and keep
Consult's normal project-wide buffer list."
  (let* ((my/consult-buffer-active
          (eq consult--buffer-display #'switch-to-buffer))
         (my/consult-buffer-placement 'current)
         (my/consult-buffer-origin (selected-window))
         (consult--buffer-display
          (if my/consult-buffer-active
              #'my/consult-buffer-display
            consult--buffer-display)))
    (if sources
        (funcall original sources)
      (let ((consult-buffer-list-function #'my/consult-buffer-list))
        (funcall original)))))

(defun my/consult-buffer-display (buffer &optional norecord)
  "Display Consult BUFFER in the selected pane requested by the user."
  (when (window-live-p my/consult-buffer-origin)
    (select-window my/consult-buffer-origin))
  (pcase my/consult-buffer-placement
    ((or 'right 'below)
     (my/open-entry-in-split
      my/consult-buffer-placement
      (lambda () (switch-to-buffer buffer norecord))))
    (_ (switch-to-buffer buffer norecord))))

(defun my/buffer-open-new-frame ()
  "Choose a buffer with Consult and open it in a new Emacs frame.
Only buffer sources are offered.  Narrow with `<o' for buffers outside
the current frame or workspace."
  (interactive)
  (require 'consult)
  (let ((consult-buffer-list-function #'my/consult-buffer-list)
        (consult--buffer-display #'my/buffer-display-new-frame))
    (consult-buffer '(consult-source-buffer
                      consult-source-other-buffer
                      consult-source-hidden-buffer))))

(defalias 'buffer-open-new-frame #'my/buffer-open-new-frame
  "Choose a buffer and open it in a newly created Emacs frame.")

(defun my/find-file-with-frame-choice ()
  "Read a file name and visit it with native `find-file'.
Shift+Return opens right.  Command+Return opens an existing file below or
accepts a new file name exactly.  Command+Shift+Return opens a new frame."
  (interactive)
  (let ((my/file-open-new-frame-prompt t)
        (my/file-open-new-frame-requested nil)
        (my/file-open-placement 'current))
    (pcase-let ((`(,file ,wildcards)
                 (find-file-read-args "Find file: "
                                      (confirm-nonexistent-file-or-buffer))))
      (let ((file (my/open-resolve-file-name file)))
        (cond
         (my/file-open-new-frame-requested
          (file-open-new-frame file wildcards))
         ((memq my/file-open-placement '(right below))
          (my/open-entry-in-split
           my/file-open-placement
           (lambda () (my/open-visit-file file wildcards))))
         (t (my/open-visit-file file wildcards)))))))

(defalias 'file-open #'my/find-file-with-frame-choice
  "Read a file name and choose its pane with Return modifiers.")

(defun my/minibuffer-file-open-new-frame ()
  "Accept the typed file name and request a new frame when supported."
  (interactive)
  (when (and my/file-open-new-frame-prompt
             (my/minibuffer-file-completion-p))
    (setq my/file-open-new-frame-requested t))
  (if (and (fboundp 'vertico-exit-input)
           (bound-and-true-p vertico--input))
      (vertico-exit-input)
    (exit-minibuffer)))

(defun my/minibuffer-open-with-placement (placement)
  "Accept a file or terminal candidate and request PLACEMENT."
  (interactive)
  (cond
   (my/terminal-selector-active
    (my/terminal-selector-exit placement))
   (my/consult-buffer-active
    (setq my/consult-buffer-placement placement)
    (if (bound-and-true-p vertico--input)
        (vertico-exit)
      (exit-minibuffer)))
   ((and my/file-open-new-frame-prompt
         (my/minibuffer-file-completion-p))
    (if (and (eq placement 'below)
             (not (file-exists-p
                   (expand-file-name (minibuffer-contents-no-properties)))))
        ;; Command+Return has always accepted a new name literally.  The
        ;; highlighted similar file must not replace that name.
        (if (bound-and-true-p vertico--input)
            (vertico-exit-input)
          (exit-minibuffer))
      (setq my/file-open-placement placement)
      (if (bound-and-true-p vertico--input)
          (vertico-exit)
        (exit-minibuffer))))
   ((bound-and-true-p vertico--input)
    (if (eq placement 'below)
        (vertico-exit-input)
      (vertico-exit)))
   (t (exit-minibuffer))))

(defun my/minibuffer-open-right ()
  "Accept the selected item and open it to the right when supported."
  (interactive)
  (my/minibuffer-open-with-placement 'right))

(defun my/minibuffer-open-below ()
  "Accept the selected item and open it below when supported."
  (interactive)
  (my/minibuffer-open-with-placement 'below))

(keymap-global-set "C-x C-f" #'file-open)
(dolist (entry '(("S-RET" . my/minibuffer-open-right)
                 ("S-<return>" . my/minibuffer-open-right)
                 ("M-RET" . my/minibuffer-open-below)
                 ("M-<return>" . my/minibuffer-open-below)
                 ("s-RET" . my/minibuffer-open-below)
                 ("s-<return>" . my/minibuffer-open-below)))
  (define-key minibuffer-local-filename-completion-map
    (kbd (car entry)) (cdr entry)))
(dolist (key '("M-S-RET" "M-S-<return>"))
  (define-key minibuffer-local-filename-completion-map (kbd key)
    #'my/minibuffer-file-open-new-frame))

(defun my/minibuffer-file-completion-p ()
  "Return non-nil when the active minibuffer is completing file names."
  (and (minibufferp)
       (or (bound-and-true-p minibuffer-completing-file-name)
           (eq (completion-metadata-get
                (completion-metadata (minibuffer-contents)
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate)
                'category)
               'file)
           (eq minibuffer-completion-table #'read-file-name-internal))))

(defun my/vertico-tab-dwim ()
  "Insert the selected completion candidate, with a native fallback."
  (interactive)
  (cond
   ((and (fboundp 'vertico-insert)
         (bound-and-true-p vertico--input))
    (vertico-insert))
   ((and (my/minibuffer-file-completion-p)
         (fboundp 'completion--selected-candidate)
         (completion--selected-candidate))
    (minibuffer-choose-completion t t))
   (t
    (minibuffer-complete))))

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :bind (:map vertico-map
         ("TAB" . my/vertico-tab-dwim)
         ("<tab>" . my/vertico-tab-dwim)
         ("S-RET" . my/minibuffer-open-right)
         ("S-<return>" . my/minibuffer-open-right)
         ("M-RET" . my/minibuffer-open-below)
         ("M-<return>" . my/minibuffer-open-below)
         ("s-RET" . my/minibuffer-open-below)
         ("s-<return>" . my/minibuffer-open-below)
         ;; Keep an explicit way to accept the typed name without selecting a
         ;; similar completion candidate.
         ("C-RET" . vertico-exit-input)
         ("C-<return>" . vertico-exit-input)
         ("M-S-RET" . my/minibuffer-file-open-new-frame)
         ("<M-S-return>" . my/minibuffer-file-open-new-frame))
  :custom
  ;; 最近使用过的候选优先，M-x/Consult/普通补全都会更贴近使用历史。
  (vertico-sort-function #'vertico-sort-history-length-alpha))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))

(defconst my/file-name-completion-ignored-extensions
  '("~" "#" ".~undo-tree~"
    ".swp" ".swo" ".swn" ".swx"
    ".un~" ".bak" ".tmp" ".temp"
    ".DS_Store" ".localized" "Icon\r"
    ".use-package-keywords.md" ".projectile" ".dir-locals-2.el"
    ".coverage" "coverage" ".classpath" ".project" ".envrc"
    ".cache/" "__pycache__/" ".mypy_cache/" ".pytest_cache/" ".ruff_cache/"
    "node_modules/" "dist/" "build/" "out/" ".parcel-cache/" ".turbo/"
    "target/" ".gradle/" ".idea/" ".vscode/" ".settings/"
    ".ccls-cache/" ".clangd/" ".cache-clangd/"
    ".venv/" "venv/" ".git/" ".hg/" ".svn/"
    ".Trash/" ".DocumentRevisions-V100/" ".TemporaryItems/"
    ".fseventsd/" ".Spotlight-V100/")
  "Extra suffixes hidden by native file-name completion.")

(setq-default completion-ignored-extensions
              (delete-dups
               (append my/file-name-completion-ignored-extensions
                       completion-ignored-extensions)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-defaults nil
        completion-category-overrides
        '((buffer (styles . (orderless flex)))
          (file (styles . (basic partial-completion orderless)))
          (imenu (styles . (orderless substring)))
          (my-workspace-symbol (styles . (orderless flex basic)))
          (lsp-capf (styles . (orderless basic partial-completion)))
          (kill-ring (styles . (orderless substring))))))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export)
         ("C-c C-o" . embark-collect)))

(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap yank-pop]               . consult-yank-pop)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap jump-to-register]       . consult-register-load)
         ([remap point-to-register]      . consult-register-store))
  :config
  (advice-add 'consult-buffer :around #'my/consult-buffer-scoped-a)
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-recent-file
                       consult-buffer
                       :preview-key nil)
    (consult-customize consult-bookmark
                       :preview-key '(:debounce 0.2 any))
    (consult-customize consult-xref
                       :preview-key '(:debounce 0.15 any))
    (consult-customize consult-line
                       :prompt "Search: "))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-narrow-key "<")
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

(defun my/completion-in-region-with-minibuffer (&rest args)
  "Use the minibuffer completion UI instead of opening `*Completions*'."
  (apply (if (bound-and-true-p vertico-mode)
             #'consult-completion-in-region
           #'completion--in-region)
         args))

(setq completion-in-region-function #'my/completion-in-region-with-minibuffer)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(defvar my/minibuffer-ui--theme-signature nil
  "Last theme signature applied by `my/minibuffer-apply-ui'.")

(defun my/minibuffer-apply-ui ()
  "Apply the active Aaron palette to completion and minibuffer faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/minibuffer-ui--theme-signature)
        (setq my/minibuffer-ui--theme-signature signature)
        (when (facep 'vertico-current)
          (aaron-ui-set-face 'vertico-current
                             :background 'bg-panel
                             :foreground 'fg-strong
                             :extend t
                             :weight 'medium))
        (when (facep 'vertico-group-title)
          (aaron-ui-set-face 'vertico-group-title
                             :foreground 'accent-cyan
                             :weight 'medium
                             :height 0.92))
        (when (facep 'vertico-group-separator)
          (aaron-ui-set-face 'vertico-group-separator
                             :foreground 'border-muted))
        (when (facep 'vertico-multiline)
          (aaron-ui-set-face 'vertico-multiline
                             :foreground 'line-number))
        (when (facep 'completions-first-difference)
          (aaron-ui-set-face 'completions-first-difference
                             :foreground 'fg-dim
                             :weight 'medium))
        (when (facep 'completions-annotations)
          (aaron-ui-set-face 'completions-annotations
                             :foreground 'fg-faint
                             :slant 'normal))
        (when (facep 'completions-common-part)
          (aaron-ui-set-face 'completions-common-part
                             :foreground 'fg-soft
                             :weight 'regular))
        (when (facep 'completions-highlight)
          (aaron-ui-set-face 'completions-highlight
                             :background 'bg-panel
                             :foreground 'fg-strong
                             :extend t))
        (when (facep 'marginalia-documentation)
          (aaron-ui-set-face 'marginalia-documentation
                             :foreground 'fg-faint))
        (when (facep 'marginalia-key)
          (aaron-ui-set-face 'marginalia-key
                             :foreground 'accent-cyan
                             :weight 'medium))
        (when (facep 'marginalia-type)
          (aaron-ui-set-face 'marginalia-type
                             :foreground 'line-number))
        (when (facep 'marginalia-file-name)
          (aaron-ui-set-face 'marginalia-file-name
                             :foreground 'line-number))
        (when (facep 'marginalia-size)
          (aaron-ui-set-face 'marginalia-size
                             :foreground 'fg-faint))
        (when (facep 'consult-preview-line)
          (aaron-ui-set-face 'consult-preview-line
                             :background 'bg-preview
                             :extend t))
        (when (facep 'consult-preview-match)
          (aaron-ui-set-face 'consult-preview-match
                             :background 'bg-panel-strong
                             :foreground 'fg-strong
                             :weight 'medium))
        (when (facep 'consult-highlight-match)
          (aaron-ui-set-face 'consult-highlight-match
                             :foreground 'fg-dim
                             :weight 'medium))
        (when (facep 'child-frame-border)
          (aaron-ui-set-face 'child-frame-border
                             :background 'border-subtle
                             :foreground 'border-subtle))))))

(add-hook 'after-init-hook #'my/minibuffer-apply-ui)
(add-hook 'server-after-make-frame-hook #'my/minibuffer-apply-ui)
(add-hook 'after-load-theme-hook #'my/minibuffer-apply-ui)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

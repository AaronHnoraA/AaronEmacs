;;; completion/minibuffer/config.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui)
(require 'aaron-open)

(defvar +file-open-new-frame-prompt nil
  "Non-nil while a file-open prompt accepts a new-frame exit key.")

(defvar +file-open-new-frame-requested nil
  "Set by the file-open minibuffer's Cmd+Shift+Enter key.")

(defvar +file-open-placement 'current
  "Where to visit the file chosen by the current file-open prompt.")

(defvar +terminal-selector-active nil)

(defvar +consult-buffer-active nil
  "Non-nil while a normal Consult buffer switcher is reading a choice.")

(defvar +consult-buffer-placement 'current
  "Placement requested in the current Consult buffer switcher.")

(defvar +consult-buffer-origin nil
  "Window that opened the current Consult buffer switcher.")

(declare-function file-open-new-frame "ui/windows/config" (file &optional wildcards))
(declare-function +open-entry-in-split "ui/windows/config" (side open))
(declare-function +terminal-selector-exit "term/portal/config" (placement))
(declare-function +buffer-display-new-frame "ui/windows/config"
                  (buffer &optional norecord))
(declare-function consult--frame-buffer-list "consult" ())
(declare-function +frame-workspace-buffers "ui/tabbar/config"
                  (&optional frame-buffers))
(defvar consult--buffer-display)
(defvar consult-buffer-list-function)

(defun +consult-buffer-list ()
  "List this frame's buffers in the active Perspective for Consult.
Use Consult's frame history, including buried buffers.  Consult's hidden
Other Buffer source still provides access to buffers outside this scope."
  (+frame-workspace-buffers (consult--frame-buffer-list)))

(defun +consult-buffer-scoped-a (original &optional sources)
  "Call ORIGINAL with frame/workspace buffers for the default SOURCES.
Commands such as `consult-project-buffer' pass explicit sources and keep
Consult's normal project-wide buffer list."
  (let* ((+consult-buffer-active
          (eq consult--buffer-display #'switch-to-buffer))
         (+consult-buffer-placement 'current)
         (+consult-buffer-origin (selected-window))
         (consult--buffer-display
          (if +consult-buffer-active
              #'+consult-buffer-display
            consult--buffer-display)))
    (if sources
        (funcall original sources)
      (let ((consult-buffer-list-function #'+consult-buffer-list))
        (funcall original)))))

(defun +consult-buffer-display (buffer &optional norecord)
  "Display Consult BUFFER in the selected pane requested by the user."
  (when (window-live-p +consult-buffer-origin)
    (select-window +consult-buffer-origin))
  (pcase +consult-buffer-placement
    ((or 'right 'below)
     (+open-entry-in-split
      +consult-buffer-placement
      (lambda () (switch-to-buffer buffer norecord))))
    ('frame (+buffer-display-new-frame buffer norecord))
    (_ (switch-to-buffer buffer norecord))))

(defun +minibuffer/buffer-open-new-frame ()
  "Choose a buffer with Consult and open it in a new Emacs frame.
Only buffer sources are offered.  Narrow with `<o' for buffers outside
the current frame or workspace."
  (interactive)
  (require 'consult)
  (let ((consult-buffer-list-function #'+consult-buffer-list)
        (consult--buffer-display #'+buffer-display-new-frame))
    (consult-buffer '(consult-source-buffer
                      consult-source-other-buffer
                      consult-source-hidden-buffer))))

(defalias 'buffer-open-new-frame #'+minibuffer/buffer-open-new-frame
  "Choose a buffer and open it in a newly created Emacs frame.")

(defun +minibuffer/find-file-with-frame-choice ()
  "Read a file name and visit it with native `find-file'.
Shift+Return opens right.  Command+Return opens an existing file below or
accepts a new file name exactly.  Command+Shift+Return opens a new frame."
  (interactive)
  (let ((+file-open-new-frame-prompt t)
        (+file-open-new-frame-requested nil)
        (+file-open-placement 'current))
    (pcase-let ((`(,file ,wildcards)
                 (find-file-read-args "Find file: "
                                      (confirm-nonexistent-file-or-buffer))))
      (let ((file (aaron-open-resolve-file-name file)))
        (cond
         (+file-open-new-frame-requested
          (file-open-new-frame file wildcards))
         ((memq +file-open-placement '(right below))
          (+open-entry-in-split
           +file-open-placement
           (lambda () (aaron-open-visit-file file wildcards))))
         (t (aaron-open-visit-file file wildcards)))))))

(defalias 'file-open #'+minibuffer/find-file-with-frame-choice
  "Read a file name and choose its pane with Return modifiers.")

(defun +minibuffer/open-new-frame ()
  "Accept the current choice and open it in a new frame when supported."
  (interactive)
  (cond
   (+terminal-selector-active
    (+terminal-selector-exit 'frame))
   (+consult-buffer-active
    (setq +consult-buffer-placement 'frame)
    (if (bound-and-true-p vertico--input)
        (vertico-exit)
      (exit-minibuffer)))
   ((and +file-open-new-frame-prompt
         (+minibuffer-file-completion-p))
    (setq +file-open-new-frame-requested t)
    (if (and (fboundp 'vertico-exit-input)
             (bound-and-true-p vertico--input))
        (vertico-exit-input)
      (exit-minibuffer)))
   ((bound-and-true-p vertico--input)
    (vertico-exit))
   (t (exit-minibuffer))))

(defun +minibuffer/open-with-placement (placement)
  "Accept a file or terminal candidate and request PLACEMENT."
  (interactive)
  (cond
   (+terminal-selector-active
    (+terminal-selector-exit placement))
   (+consult-buffer-active
    (setq +consult-buffer-placement placement)
    (if (bound-and-true-p vertico--input)
        (vertico-exit)
      (exit-minibuffer)))
   ((and +file-open-new-frame-prompt
         (+minibuffer-file-completion-p))
    (if (and (eq placement 'below)
             (not (file-exists-p
                   (expand-file-name (minibuffer-contents-no-properties)))))
        ;; Command+Return has always accepted a new name literally.  The
        ;; highlighted similar file must not replace that name.
        (if (bound-and-true-p vertico--input)
            (vertico-exit-input)
          (exit-minibuffer))
      (setq +file-open-placement placement)
      (if (bound-and-true-p vertico--input)
          (vertico-exit)
        (exit-minibuffer))))
   ((bound-and-true-p vertico--input)
    (if (eq placement 'below)
        (vertico-exit-input)
      (vertico-exit)))
   (t (exit-minibuffer))))

(defun +minibuffer/open-right ()
  "Accept the selected item and open it to the right when supported."
  (interactive)
  (+minibuffer/open-with-placement 'right))

(defun +minibuffer/open-below ()
  "Accept the selected item and open it below when supported."
  (interactive)
  (+minibuffer/open-with-placement 'below))

(keymap-global-set "C-x C-f" #'file-open)
(dolist (entry '(("S-RET" . +minibuffer/open-right)
                 ("S-<return>" . +minibuffer/open-right)
                 ("M-RET" . +minibuffer/open-below)
                 ("M-<return>" . +minibuffer/open-below)
                 ("s-RET" . +minibuffer/open-below)
                 ("s-<return>" . +minibuffer/open-below)))
  (define-key minibuffer-local-filename-completion-map
    (kbd (car entry)) (cdr entry)))
(dolist (key '("M-S-RET" "M-S-<return>"))
  (define-key minibuffer-local-filename-completion-map (kbd key)
    #'+minibuffer/open-new-frame))

(defun +minibuffer-file-completion-p ()
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

(defun +minibuffer/vertico-tab-dwim ()
  "Insert the selected completion candidate, with a native fallback."
  (interactive)
  (cond
   ((and (fboundp 'vertico-insert)
         (bound-and-true-p vertico--input))
    (vertico-insert))
   ((and (+minibuffer-file-completion-p)
         (fboundp 'completion--selected-candidate)
         (completion--selected-candidate))
    (minibuffer-choose-completion t t))
   (t
    (minibuffer-complete))))

;; From Doom: the cursor cannot enter the read-only prompt, where every key
;; would only answer "Text is read-only".
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :bind (:map vertico-map
         ("TAB" . +minibuffer/vertico-tab-dwim)
         ("<tab>" . +minibuffer/vertico-tab-dwim)
         ("S-RET" . +minibuffer/open-right)
         ("S-<return>" . +minibuffer/open-right)
         ("M-RET" . +minibuffer/open-below)
         ("M-<return>" . +minibuffer/open-below)
         ("s-RET" . +minibuffer/open-below)
         ("s-<return>" . +minibuffer/open-below)
         ;; Keep an explicit way to accept the typed name without selecting a
         ;; similar completion candidate.
         ("C-RET" . vertico-exit-input)
         ("C-<return>" . vertico-exit-input)
         ("M-S-RET" . +minibuffer/open-new-frame)
         ("<M-S-return>" . +minibuffer/open-new-frame))
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

(defconst +file-name-completion-ignored-extensions
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
               (append +file-name-completion-ignored-extensions
                       completion-ignored-extensions)))

(use-package orderless
  :ensure t
  :init
  ;; `completion-styles' itself is a `config' setting (etc/config-store.el).
  (setq completion-category-defaults nil
        completion-category-overrides
        '((buffer (styles . (orderless flex)))
          (file (styles . (basic partial-completion orderless)))
          (imenu (styles . (orderless substring)))
          (+workspace-symbol (styles . (orderless flex basic)))
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
  (advice-add 'consult-buffer :around #'+consult-buffer-scoped-a)
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

(defun +completion-in-region-with-minibuffer (&rest args)
  "Use the minibuffer completion UI instead of opening `*Completions*'."
  (apply (if (bound-and-true-p vertico-mode)
             #'consult-completion-in-region
           #'completion--in-region)
         args))

(setq completion-in-region-function #'+completion-in-region-with-minibuffer)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(defvar +minibuffer-ui--theme-signature nil
  "Last theme signature applied by `+minibuffer-apply-ui'.")

(defun +minibuffer-apply-ui ()
  "Apply the active Aaron palette to completion and minibuffer faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature +minibuffer-ui--theme-signature)
        (setq +minibuffer-ui--theme-signature signature)
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

(add-hook 'after-init-hook #'+minibuffer-apply-ui)
(add-hook 'server-after-make-frame-hook #'+minibuffer-apply-ui)
(add-hook 'after-load-theme-hook #'+minibuffer-apply-ui)

(provide 'completion/minibuffer/config)
;;; completion/minibuffer/config.el ends here

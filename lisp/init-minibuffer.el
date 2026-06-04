;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui)

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
         ;; 在 `C-x C-f` 这类文件补全场景，M-RET 直接使用你当前输入的文本，
         ;; 不选中/不补全候选（用于创建与已有文件“相似名字”的新文件）。
         ("M-RET" . vertico-exit-input)
         ("<M-return>" . vertico-exit-input))
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
          (eglot-capf (styles . (orderless basic partial-completion)))
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
  "Apply the local dark UI to completion and minibuffer faces."
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

;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :bind (:map vertico-map
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

(defun my/minibuffer-apply-ui ()
  "Apply the local dark UI to completion and minibuffer faces."
  (when (display-graphic-p)
    (when (facep 'vertico-current)
      (set-face-attribute 'vertico-current nil
                          :background "#31384a"
                          :foreground "#edf2f7"
                          :extend t
                          :weight 'medium))
    (when (facep 'vertico-group-title)
      (set-face-attribute 'vertico-group-title nil
                          :foreground "#8aa6c1"
                          :weight 'medium
                          :height 0.92))
    (when (facep 'vertico-group-separator)
      (set-face-attribute 'vertico-group-separator nil
                          :foreground "#4b556b"))
    (when (facep 'vertico-multiline)
      (set-face-attribute 'vertico-multiline nil
                          :foreground "#7f849c"))
    (when (facep 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil
                          :foreground "#a9bed3"
                          :weight 'medium))
    (when (facep 'completions-annotations)
      (set-face-attribute 'completions-annotations nil
                          :foreground "#6f748b"
                          :slant 'normal))
    (when (facep 'completions-common-part)
      (set-face-attribute 'completions-common-part nil
                          :foreground "#d8dee9"
                          :weight 'regular))
    (when (facep 'completions-highlight)
      (set-face-attribute 'completions-highlight nil
                          :background "#31384a"
                          :foreground "#edf2f7"
                          :extend t))
    (when (facep 'marginalia-documentation)
      (set-face-attribute 'marginalia-documentation nil
                          :foreground "#6f748b"))
    (when (facep 'marginalia-key)
      (set-face-attribute 'marginalia-key nil
                          :foreground "#8aa6c1"
                          :weight 'medium))
    (when (facep 'marginalia-type)
      (set-face-attribute 'marginalia-type nil
                          :foreground "#7f849c"))
    (when (facep 'marginalia-file-name)
      (set-face-attribute 'marginalia-file-name nil
                          :foreground "#7f849c"))
    (when (facep 'marginalia-size)
      (set-face-attribute 'marginalia-size nil
                          :foreground "#6f748b"))
    (when (facep 'consult-preview-line)
      (set-face-attribute 'consult-preview-line nil
                          :background "#293241"
                          :extend t))
    (when (facep 'consult-preview-match)
      (set-face-attribute 'consult-preview-match nil
                          :background "#384154"
                          :foreground "#edf2f7"
                          :weight 'medium))
    (when (facep 'consult-highlight-match)
      (set-face-attribute 'consult-highlight-match nil
                          :foreground "#a9bed3"
                          :weight 'medium))
    (when (facep 'child-frame-border)
      (set-face-attribute 'child-frame-border nil
                          :background "#3a4154"
                          :foreground "#3a4154"))))

(add-hook 'after-init-hook #'my/minibuffer-apply-ui)
(add-hook 'server-after-make-frame-hook #'my/minibuffer-apply-ui)
(add-hook 'after-load-theme-hook #'my/minibuffer-apply-ui)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

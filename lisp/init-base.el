;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

(add-to-list 'exec-path (expand-file-name "tools" user-emacs-directory))
(setenv "PATH"
        (concat (expand-file-name "tools" user-emacs-directory)
                ":" (getenv "PATH")))
;;; Code:

(require 'cl-lib)
(require 'aaron-ui)

(declare-function tempo-expand-if-complete "tempo")
(declare-function magit-toplevel "magit" (&optional directory))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function diff-hl-mode "diff-hl" (&optional arg))
(declare-function vc-git-root "vc-git" (file))
(declare-function my/project-current-root "init-project")

(defvar project-current-directory-override)

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-underline-at-descent-line t)

;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
;; for the key passphrase.
(setq epg-pinentry-mode 'loopback)

;; Optimize for very long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Keep recovery files out of projects while preserving crash protection.
(defconst my/state-dir
  (file-name-as-directory (expand-file-name "var" user-emacs-directory)))
(defconst my/backup-dir
  (file-name-as-directory (expand-file-name "backup" my/state-dir)))
(defconst my/auto-save-dir
  (file-name-as-directory (expand-file-name "auto-save" my/state-dir)))
(defconst my/auto-save-session-dir
  (file-name-as-directory (expand-file-name "sessions" my/auto-save-dir)))
(defconst my/lockfile-dir
  (file-name-as-directory (expand-file-name "lockfiles" my/state-dir)))
(defconst my/tramp-state-dir
  (file-name-as-directory (expand-file-name "tramp" my/state-dir)))
(defconst my/tramp-auto-save-dir
  (file-name-as-directory (expand-file-name "autosave" my/tramp-state-dir)))
(defconst my/eshell-state-dir
  (file-name-as-directory (expand-file-name "eshell" my/state-dir)))
(defconst my/undo-tree-history-state-dir
  (file-name-as-directory (expand-file-name "undo-tree-history" my/state-dir)))
(defconst my/treesit-state-dir
  (file-name-as-directory (expand-file-name "tree-sitter" my/state-dir)))
(defconst my/org-state-dir
  (file-name-as-directory (expand-file-name "org" my/state-dir)))
(defconst my/dape-state-dir
  (file-name-as-directory (expand-file-name "dape" my/state-dir)))

(defvar org-id-locations-file)

(dolist (dir (list my/state-dir
                   my/backup-dir
                   my/auto-save-dir
                   my/auto-save-session-dir
                   my/lockfile-dir
                   my/tramp-state-dir
                   my/tramp-auto-save-dir
                   my/eshell-state-dir
                   my/undo-tree-history-state-dir
                   my/treesit-state-dir
                   my/org-state-dir
                   my/dape-state-dir))
  (make-directory dir t))

(setq org-id-locations-file
      (expand-file-name "id-locations.el" my/org-state-dir))

(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 10
      kept-old-versions 3
      backup-directory-alist `(("." . ,my/backup-dir))
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200
      auto-save-file-name-transforms `((".*" ,my/auto-save-dir t))
      auto-save-list-file-prefix (expand-file-name ".saves-" my/auto-save-session-dir))

(if (boundp 'lock-file-name-transforms)
    (setq create-lockfiles t
          lock-file-name-transforms `((".*" ,my/lockfile-dir t)))
  (setq create-lockfiles nil))

;; Always load the newest file
(setq load-prefer-newer t)

;; Keep the system clipboard stable when selecting text with the mouse.
(setq select-enable-primary nil
      select-enable-clipboard t
      select-active-regions nil)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

(use-package ligature
  :demand t
  :config
  (ligature-set-ligatures
   't
   '("==" "===" "!=" "->" "<-" "<->"
     "=>" "<=" ">=" "::" ":="
     "&&" "||" ">>" "<<"))
  (global-ligature-mode t))



;; 使用绝对行号
(setq display-line-numbers-type 'absolute)


;; 可选：关闭次刻度（避免干扰）
(setq display-line-numbers-minor-tick 0)

;; ========= 行号显示策略 =========
(setq display-line-numbers-minor-tick 0)

;; 1. 设置触发频率（例如每 5 行高亮一次）
(setq display-line-numbers-major-tick 20)

;; 2. 启用行号（如果还没启用）
(global-display-line-numbers-mode 1)

(defun my/disable-display-line-numbers ()
  "Disable line numbers in buffers where they add cost but little value."
  (display-line-numbers-mode -1))

(defvar my/line-number-style display-line-numbers-type
  "Preferred line-number display style for `my/toggle-line-numbers'.")

(defun my/toggle-line-numbers ()
  "Cycle line numbers between absolute, relative/visual, and disabled."
  (interactive)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type
                      (remq display-line-numbers-type styles)))
         (queue (memq my/line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (cadr queue))))
    (setq my/line-number-style next)
    (setq display-line-numbers next)
    (message "Line numbers: %s"
             (pcase next
               (`t "absolute")
               (`nil "off")
               (_ (symbol-name next))))))

;; 可选：在一些模式禁用（终端、目录、帮助、仪表盘等）
(dolist (hook '(term-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                dashboard-mode-hook
                ibuffer-mode-hook
                dired-mode-hook
                helpful-mode-hook
                help-mode-hook
                magit-mode-hook
                magit-status-mode-hook
                compilation-mode-hook))
  (add-hook hook #'my/disable-display-line-numbers))

(with-eval-after-load 'evil
  (define-key evil-window-map (kbd "l") #'my/toggle-line-numbers))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC w l" "line numbers"))




;; 80 列竖线
(setq-default fill-column 80)
;; 只在编程模式显示竖线
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)



;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)


(setopt hs-hide-comments-when-hiding-all t
        hs-isearch-open t)


(setq global-disable-point-adjustment nil)

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 'always)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; No tabs
;; Indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(when (member "JetBrainsMono Nerd Font" (font-family-list))
  ;; 只给 symbol / unicode 区域加 fallback，不动中文、英文正文字体
  (dolist (charset '(symbol))
    (set-fontset-font t charset "JetBrainsMono Nerd Font" nil 'append)))

(use-package mixed-pitch
  :ensure t
  :commands (mixed-pitch-mode)
  :custom
  (mixed-pitch-set-height t)
  :config
  ;; 只让代码和结构标记保持等宽，标题仍然交给各模式自己的 face。
  (dolist (face '(font-latex-verbatim-face
                  markdown-header-delimiter-face
                  markdown-markup-face
                  markdown-pre-face
                  markdown-table-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  (setq mixed-pitch-fixed-pitch-faces
        (delq 'font-latex-sectioning-5-face mixed-pitch-fixed-pitch-faces)))

;; ======================================================================
;; 1. 字体变量 (用户自定义区域)
;; ======================================================================

(defgroup my/typography nil
  "Typography helpers shared by UI and writing modes."
  :group 'faces)

;; 英文正文 (Body) - prose / variable-pitch
(defcustom my/font-body "Merriweather"
  "Serif family used for prose."
  :type 'string
  :group 'my/typography)

(defcustom my/h-body 212
  "Base height for prose text."
  :type 'integer
  :group 'my/typography)

(defcustom my/font-body-width 'normal
  "Preferred width for prose Latin text.
Keep the prose font at its native width so long English lines stay tight."
  :type 'symbol
  :group 'my/typography)

;; 代码/表格 (Code) - 默认界面 + 代码环境（fixed-pitch）
(defcustom my/font-code "Fira Code"
  "Monospace family used for UI and code."
  :type 'string
  :group 'my/typography)

(defcustom my/h-code 172
  "Base height for code and UI text."
  :type 'integer
  :group 'my/typography)

;; 标题 (Title) - Org / Markdown / LaTeX 标题
(defcustom my/font-title "Excalifont"
  "Display family used for document titles and headings."
  :type 'string
  :group 'my/typography)

(defcustom my/h-title 236
  "Base height for first-level document headings."
  :type 'integer
  :group 'my/typography)

;; 中文 (Chinese)
(defcustom my/font-cn "FZLiuGongQuanKaiShuJF"
  "Primary Chinese family."
  :type 'string
  :group 'my/typography)

(defcustom my/scale-cn 1.3
  "Scale factor for Chinese glyphs."
  :type 'number
  :group 'my/typography)

(defcustom my/font-code-weight 'regular
  "Preferred weight for monospace UI and code."
  :type 'symbol
  :group 'my/typography)

(defcustom my/font-ui-weight 'regular
  "Preferred weight for general UI text."
  :type 'symbol
  :group 'my/typography)

(defcustom my/font-body-weight 'regular
  "Preferred weight for prose text."
  :type 'symbol
  :group 'my/typography)

(defcustom my/font-title-weight 'medium
  "Preferred weight for document headings."
  :type 'symbol
  :group 'my/typography)

(defcustom my/font-strong-weight 'medium
  "Preferred weight for emphasis that should remain readable on transparent backgrounds."
  :type 'symbol
  :group 'my/typography)

(defcustom my/font-popout-weight 'semibold
  "Preferred weight for the strongest visible accents such as top headings."
  :type 'symbol
  :group 'my/typography)

(defcustom my/prose-line-spacing 0.19
  "Line spacing used in prose buffers."
  :type 'number
  :group 'my/typography)

;; ======================================================================
;; 2. 核心：应用字体（全局 + 中文绑定）
;; ======================================================================

(defun my/font--set-face (face &rest attrs)
  "Apply ATTRS to FACE when it exists."
  (when (facep face)
    (apply #'set-face-attribute face nil attrs)))

(defun my/font--title-heights ()
  "Return a compact heading scale derived from `my/h-title'."
  (list (+ my/h-title 24)
        my/h-title
        (- my/h-title 6)
        (- my/h-title 12)
        (- my/h-title 18)
        (- my/h-title 24)
        (- my/h-title 28)
        (- my/h-title 32)
        (- my/h-title 36)))

(defun my/font--set-prose-face (face &rest attrs)
  "Make FACE use the prose font plus ATTRS."
  (apply #'my/font--set-face face
         :family my/font-body
         :width my/font-body-width
         :weight my/font-body-weight
         attrs))

(defun my/font--set-code-face (face &rest attrs)
  "Make FACE use the monospace font plus ATTRS."
  (apply #'my/font--set-face face
         :family my/font-code
         :weight my/font-code-weight
         attrs))

(defun my/font--set-title-face (face height &optional weight)
  "Make FACE use the title font with HEIGHT and optional WEIGHT."
  (my/font--set-face face
                     :inherit 'variable-pitch
                     :family my/font-title
                     :height height
                     :weight (or weight my/font-title-weight)))

(defun my/font--apply-ui-faces ()
  "Keep UI monospace consistent without using heavy bold weights."
  (my/font--set-code-face 'line-number
                          :height 0.85
                          :weight my/font-ui-weight
                          :foreground (aaron-ui-color 'line-number))
  (my/font--set-face 'line-number-current-line
                     :inherit 'line-number
                     :foreground (aaron-ui-color 'line-number-current)
                     :background (aaron-ui-color 'line-number-current-bg)
                     :weight my/font-strong-weight)
  (my/font--set-face 'line-number-major-tick
                     :inherit 'line-number
                     :foreground (aaron-ui-color 'line-number-major)))

(defun my/font--apply-core-faces ()
  "设置 default/fixed/variable 三类 face。"
  ;; 默认 = 代码字体（稳定，且界面/代码/表格都不乱）
  (my/font--set-face 'default
                     :family my/font-code
                     :height my/h-code
                     :weight my/font-code-weight)

  ;; 固定宽度 = 代码字体（确保 org-block/org-table 等继承后稳定）
  (my/font--set-face 'fixed-pitch
                     :family my/font-code
                     :height my/h-code
                     :weight my/font-code-weight)

  ;; 变宽 = 正文字体（mixed-pitch 会让 Org 正文用它）
  (my/font--set-face 'variable-pitch
                     :family my/font-body
                     :height (+ my/h-body 8)
                     :width my/font-body-width
                     :weight my/font-body-weight))

(defun my/font--bind-chinese-to-fontset ()
  "把中文相关字符集强制绑定到 my/font-cn。"
  (when (member my/font-cn (font-family-list))
    ;; t 表示当前 frame 的 fontset；也会影响后续 frame 的默认 fontset 选择
    (dolist (charset '(han cjk-misc bopomofo kana hangul))
      ;; 'prepend：把该字体放在 fallback 优先级前面，避免被系统中文字体截胡
      (set-fontset-font t charset
                        (font-spec :family my/font-cn
                                   :weight 'regular
                                   :slant 'normal)
                        nil
                        'prepend))))

(defun my/font--apply-rescale ()
  "设置中文缩放（只调大小，不负责选字体）。"
  (setq face-font-rescale-alist
        (assq-delete-all my/font-cn face-font-rescale-alist))
  (add-to-list 'face-font-rescale-alist (cons my/font-cn my/scale-cn)))

(defun my/font--apply-org-faces ()
  "Apply typography for Org headings and code blocks."
  (let ((heights (my/font--title-heights)))
    (cl-mapc #'my/font--set-title-face
             '(org-document-title
               org-level-1 org-level-2 org-level-3 org-level-4
               org-level-5 org-level-6 org-level-7 org-level-8)
             heights))
  (my/font--set-face 'org-document-title
                     :weight my/font-popout-weight)
  (dolist (face '(org-level-1 org-level-2))
    (my/font--set-face face :weight my/font-popout-weight))
  (dolist (face '(org-level-3 org-level-4))
    (my/font--set-face face :weight my/font-strong-weight))
  (dolist (face '(org-level-5 org-level-6 org-level-7 org-level-8))
    (my/font--set-face face :weight my/font-title-weight))
  (dolist (face '(org-block
                  org-block-begin-line
                  org-block-end-line
                  org-table
                  org-formula
                  org-code
                  org-verbatim
                  org-meta-line
                  org-checkbox
                  org-document-info-keyword
                  org-indent
                  org-latex-and-related))
    (my/font--set-code-face face :inherit 'fixed-pitch)))

(defun my/font--apply-markdown-faces ()
  "Apply typography for Markdown headings and code spans."
  (let ((heights (cdr (my/font--title-heights))))
    (cl-mapc #'my/font--set-title-face
             '(markdown-header-face-1
               markdown-header-face-2
               markdown-header-face-3
               markdown-header-face-4
               markdown-header-face-5
               markdown-header-face-6)
             heights))
  (my/font--set-title-face 'markdown-header-face (- my/h-title 12))
  (dolist (face '(markdown-code-face
                  markdown-comment-face
                  markdown-header-delimiter-face
                  markdown-inline-code-face
                  markdown-language-info-face
                  markdown-language-keyword-face
                  markdown-markup-face
                  markdown-math-face
                  markdown-pre-face
                  markdown-table-face))
    (my/font--set-code-face face :inherit 'fixed-pitch)))

(defun my/font--apply-latex-faces ()
  "Apply typography for LaTeX sectioning and literal faces."
  (let ((heights (cdr (my/font--title-heights))))
    (cl-mapc #'my/font--set-title-face
             '(font-latex-sectioning-0-face
               font-latex-sectioning-1-face
               font-latex-sectioning-2-face
               font-latex-sectioning-3-face
               font-latex-sectioning-4-face
               font-latex-sectioning-5-face)
             heights))
  (my/font--set-title-face 'font-latex-slide-title-face (+ my/h-title 12))
  (dolist (face '(font-latex-math-face
                  font-latex-sedate-face
                  font-latex-string-face
                  font-latex-verbatim-face
                  font-latex-warning-face))
    (my/font--set-code-face face :inherit 'fixed-pitch)))

(defun my/font--apply-dashboard-faces ()
  "Keep dashboard readable without breaking nerd-icons glyph faces."
  (my/font--set-title-face 'dashboard-banner-logo-title (+ my/h-title 18) my/font-strong-weight)
  ;; Heading / navigator / item lines may mix nerd-icons with text, so only
  ;; tune size and weight here and leave font family resolution to the icon faces.
  (my/font--set-face 'dashboard-heading
                     :height (- my/h-title 10)
                     :weight my/font-strong-weight)
  (my/font--set-face 'dashboard-items-face
                     :height my/h-body
                     :weight my/font-body-weight)
  (my/font--set-face 'dashboard-footer-face
                     :height (- my/h-body 10)
                     :weight my/font-body-weight)
  (my/font--set-face 'dashboard-navigator
                     :height my/h-code
                     :weight my/font-ui-weight)
  (my/font--set-face 'dashboard-text-banner
                     :inherit 'variable-pitch
                     :family my/font-body
                     :height (+ my/h-body 6)
                     :weight my/font-strong-weight))

(defun my/font--apply-document-faces ()
  "Refresh mode-specific faces for already loaded writing packages."
  (when (featurep 'org)
    (my/font--apply-org-faces))
  (when (featurep 'markdown-mode)
    (my/font--apply-markdown-faces))
  (when (featurep 'font-latex)
    (my/font--apply-latex-faces))
  (when (featurep 'dashboard)
    (my/font--apply-dashboard-faces)))

(defun my/typography-setup-prose-buffer ()
  "Enable mixed-pitch and spacing for prose-oriented buffers."
  (when (and (display-graphic-p)
             (fboundp 'mixed-pitch-mode))
    (mixed-pitch-mode 1)
    (setq-local line-spacing my/prose-line-spacing))
  (my/font--bind-chinese-to-fontset))

(with-eval-after-load 'doom-modeline
  (when (member "JetBrainsMono Nerd Font Mono" (font-family-list))
    (set-face-attribute 'doom-modeline nil
                        :family "JetBrainsMono Nerd Font Mono")))

(defun my/apply-font-config (&optional frame)
  "应用全部字体设置。可用于 daemon 新 frame。
若传入 FRAME，则在该 frame 上应用。"
  (interactive)
  (when (frame-live-p frame)
    (select-frame frame))

  (my/font--apply-core-faces)
  (my/font--apply-ui-faces)
  (my/font--bind-chinese-to-fontset)
  (my/font--apply-rescale)
  (my/font--apply-document-faces)

  ;; 保险：让 Emacs 重新评估字体缓存（可选但很稳）
  (when (fboundp 'font-cache-reset)
    (font-cache-reset)))

;; 启动立即生效
(my/apply-font-config)

;; Daemon / 新 frame 也生效
(add-hook 'server-after-make-frame-hook #'my/apply-font-config)

(with-eval-after-load 'org
  (my/font--apply-org-faces))

(with-eval-after-load 'markdown-mode
  (my/font--apply-markdown-faces))

(with-eval-after-load 'font-latex
  (my/font--apply-latex-faces))

(with-eval-after-load 'dashboard
  (my/font--apply-dashboard-faces))

;; ======================================================================
;; 3. 主题切换 / 字体调试
;; ======================================================================

(defun my/font--refresh-after-theme (&rest _)
  "Re-apply typography after theme changes."
  (my/apply-font-config))

(advice-add 'load-theme :after #'my/font--refresh-after-theme)

;; ======================================================================
;; 4. 可选：一键刷新（调字体时用）
;; ======================================================================

(defun my/font-reset-all ()
  "清 Emacs 字体缓存并重应用配置。"
  (interactive)
  (when (fboundp 'font-cache-reset) (font-cache-reset))
  (my/apply-font-config)
  (redraw-display))




;; Sane defaults
(setq use-short-answers t)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Enable the disabled `list-timers', `list-threads' commands
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

;; Keep clean but enable `menu-bar' in MacOS
(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Show line/column number and more
(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  ;; No visual feedback on copy/delete.
  (copy-region-blink-delay 0)
  (delete-pair-blink-delay 0)
  ;; confusing if no fringes (GUI only).
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; preserve the previous clipboard entry in the kill ring before replacing it
  (save-interprogram-paste-before-kill t)
  ;; kill last word if there is no active region. C-w behaves more like vim.
  (kill-region-dwim 'unix-word)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n' when point starts at the beginning-of-line
  (kill-whole-line t)
  ;; show cwd when `shell-command' and `async-shell-command'
  (shell-command-prompt-show-cwd t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t)
  ;; List only applicable commands.
  ;;
  ;; ``` elisp
  ;; (defun foo ()
  ;;   (interactive nil org-mode)
  ;;   (message "foo"))
  ;; ```
  ;;
  ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end-double-space nil))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Highlight current line in GUI
(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init . global-hl-line-mode))

;; Enable `repeat-mode' to reduce key sequence length
;;
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
(use-package repeat
  :ensure nil
  :custom
  (repeat-mode t)
  (repeat-exit-timeout 1)
  (repeat-exit-key (kbd "RET")))

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; Completion engine
(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-local-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-ns-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-completion-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-must-match-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-isearch-map
         ([escape] . abort-recursive-edit))
  :custom
  ;; Default minibuffer is fine-tuned since Emacs 29
  (completion-auto-help nil)
  (completion-show-help nil)
  (completion-auto-select nil)
  (completion-cycle-threshold nil)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  ;; shorten " (default %s)" => " [%s]"
  (minibuffer-default-prompt-format " [%s]")
  (minibuffer-electric-default-mode t)
  ;; Don't insert completion at point into minibuffer
  (minibuffer-completion-auto-choose nil)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; `selectrum', `vertico' and `icomplete' will honoring
  (completion-styles '(basic partial-completion substring flex))
  (completion-category-overrides '((buffer (styles . (flex)))
                                   (file (styles . (substring)))
                                   (eglot-capf (styles . (basic partial-completion)))
                                   (kill-ring (styles . (substring)))
                                   (imenu (styles . (substring)))))
  (completion-pcm-leading-wildcard t)
  ;; vertical view
  (completions-format 'one-column)
  (completions-max-height 13)
  (completions-sort nil)
  (completions-detailed t))

;; Holidays
(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-chinese-all-holidays-flag t)
  (holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                            (holiday-fixed 3 12 "Arbor Day")
                            ,@(cl-loop for i from 1 to 3
                                       collect `(holiday-fixed 5 ,i "International Workers' Day"))
                            (holiday-fixed 5 4  "Chinese Youth Day")
                            (holiday-fixed 6 1  "Children's Day")
                            (holiday-fixed 9 10 "Teachers' Day")
                            ,@(cl-loop for i from 1 to 7
                                       collect `(holiday-fixed 10 ,i "National Day"))
                            (holiday-fixed 10 24 "Programmers' Day")
                            (holiday-fixed 11 11 "Singles' Day")))
  (holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
                            (holiday-fixed 4 23 "World Book Day")
                            (holiday-sexp '(if (or (zerop (% year 400))
                                                   (and (% year 100) (zerop (% year 4))))
                                               (list 9 12 year)
                                             (list 9 13 year))
                                          "World Programmers' Day")
                            (holiday-fixed 10 10 "World Mental Health Day")))
  (calendar-holidays `(,@holiday-general-holidays
                       ,@holiday-oriental-holidays
                       ,@holiday-christian-holidays
                       ,@holiday-other-holidays
                       ,@holiday-local-holidays))
  (calendar-mark-holidays-flag t)
  (calendar-mark-diary-entries-flag nil)
  ;; Prefer +0800 over CST
  (calendar-time-zone-style 'numeric)
  ;; year/month/day
  (calendar-date-style 'iso))

;; Appointment / diary reminders are intentionally disabled.
(use-package appt
  :ensure nil
  :defer t
  :custom
  (appt-display-diary nil)
  (appt-display-mode-line nil))

;; Build regexp with visual feedback
(use-package re-builder
  :ensure nil
  :commands re-builder
  :bind (:map reb-mode-map
         ("C-c C-k" . reb-quit)
         ("C-c C-p" . reb-prev-match)
         ("C-c C-n" . reb-next-match))
  :custom
  (reb-re-syntax 'string))

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    "Comment or uncomment the current line or region.

If the region is active and `transient-mark-mode' is on, call
`comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (comment-dwim nil)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  ;; `auto-fill' inside comments.
  ;;
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (comment-auto-fill-only-comments t))

;; TRAMP configuration lives in init-tramp.el, loaded from init-modules.el.


;; Command line interpreter
(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
         ([remap kill-region]   . backward-kill-word))
  :custom
  ;; No paging, `eshell' and `shell' will honoring.
  (comint-pager "cat")
  ;; Make the prompt of "*Python*" buffer readonly
  (comint-prompt-read-only t)
  (comint-history-isearch 'dwim)
  ;; Colorize
  (comint-terminfo-terminal "dumb-emacs-ansi"))

;; Better abbrev expansion
(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (defun try-expand-tempo (_old)
    (require 'tempo)
    (tempo-expand-if-complete))
  :custom
  (hippie-expand-try-functions-list '(try-expand-tempo
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

;; Buffer index
(use-package imenu
  :hook (imenu-after-jump . recenter)
  :custom
  (imenu-flatten 'group))

;; Needed by `webpaste'
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-generic-program (or (executable-find "firefox")
                                  (executable-find "chromium")
                                  (executable-find "google-chrome-stable")
                                  (executable-find "google-chrome")
                                  (when (eq system-type 'darwin) "open")
                                  (when (eq system-type 'gnu/linux) "xdg-open")))
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Buffer manager
;;
;; `sR': switch to saved filter groups
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("Browser" (or (mode . eww-mode)
                     (mode . xwidget-webkit-mode)))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Repl" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Term" (or (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Mail" (or (mode . mail-mode)
                  (mode . message-mode)
                  (derived-mode . gnus-mode)))
      ("Conf" (or (mode . yaml-mode)
                  (mode . yaml-ts-mode)
                  (mode . conf-mode)))
      ("Dict" (or (mode . fanyi-mode)
                  (mode . dictionary-mode)))
      ("Text" (and (derived-mode . text-mode)
                   (not (starred-name))))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)
                   (mode . vc-annotate-mode)))
      ("VC" (or (mode . diff-mode)
                (derived-mode . log-view-mode)))
      ("Prog" (and (derived-mode . prog-mode)
                   (not (starred-name))))
      ("Dired" (mode . dired-mode))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))))))

(defvar my/ibuffer-ui--theme-signature nil
  "Last theme signature applied by `my/ibuffer-apply-ui'.")

(defun my/ibuffer-apply-ui ()
  "Apply local UI styling to Ibuffer."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/ibuffer-ui--theme-signature)
        (setq my/ibuffer-ui--theme-signature signature)
        (when (facep 'ibuffer-title-face)
          (aaron-ui-set-face 'ibuffer-title-face
                             :foreground 'fg-strong
                             :weight 'medium))
        (when (facep 'ibuffer-filter-group-name-face)
          (aaron-ui-set-face 'ibuffer-filter-group-name-face
                             :foreground 'fg-dim
                             :weight 'medium))
        (when (facep 'ibuffer-marked-face)
          (aaron-ui-set-face 'ibuffer-marked-face
                             :foreground 'accent-yellow-soft
                             :weight 'medium))
        (when (facep 'ibuffer-modified-buffer)
          (aaron-ui-set-face 'ibuffer-modified-buffer
                             :foreground 'accent-cyan))
        (when (facep 'ibuffer-read-only-buffer)
          (aaron-ui-set-face 'ibuffer-read-only-buffer
                             :foreground 'line-number))
        (when (facep 'ibuffer-locked-buffer)
          (aaron-ui-set-face 'ibuffer-locked-buffer
                             :foreground 'accent-red-soft))))))

(add-hook 'ibuffer-mode-hook #'my/ibuffer-apply-ui)
(add-hook 'after-load-theme-hook #'my/ibuffer-apply-ui)

;; Notifications
;;
;; Actually, `notify-send' is not defined in notifications package, but the
;; autoload cookie will make Emacs load `notifications' first, then our
;; `defalias' will be evaluated.
(pcase system-type
  ('gnu/linux
   (use-package notifications
     :ensure nil
     :commands notify-send
     :config
     (defalias 'notify-send 'notifications-notify)))
  ('darwin
   (defun notify-send (&rest params)
     "Send notifications via `terminal-notifier'."
     (let ((title (plist-get params :title))
           (body (plist-get params :body)))
       (start-process "terminal-notifier"
                      nil
                      "terminal-notifier"
                      "-group" "Emacs"
                      "-title" title
                      "-message" body
                      "-activate" "org.gnu.Emacs"))))
  (_
   (defalias 'notify-send 'ignore)))

;; Recently opened files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(;; Folders on MacOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on MacOS end
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   (kill-ring . 50))))

;; Try out emacs package without installing
(use-package try
  :ensure t
  :commands try try-and-refresh)

(use-package vlf
  :ensure t
  :commands (vlf vlf-mode)
  :init
  (with-eval-after-load 'vlf
    (define-key vlf-prefix-map "\C-xv" vlf-mode-map)))

;; 关闭无用 UI，打开有用能力
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 像素级滚动（Emacs 29）
;(pixel-scroll-precision-mode 1)


(setq scroll-conservatively most-positive-fixnum
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; 高质量渲染
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(setq treesit-font-lock-level 4)
(setq compilation-scroll-output t)

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(use-package amx
  :ensure t
  :defer 2
  :custom
  (amx-history-length 20)
  :config
  (amx-mode 1))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))


(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra) 



;;; ============================
;;; undo-tree: 历史文件统一集中存放到本地 var（含 TRAMP）
;;; ============================
(use-package undo-tree
  :ensure t
  :init
  ;; 1) 开启全局 undo-tree
  (global-undo-tree-mode 1)

  ;; 2) 本地历史文件统一放到一个目录（只在本机路径生效）
  ;;    你可以改成你喜欢的位置：例如 ~/.emacs.d/undo-tree/
  (defvar my/undo-tree-history-dir
    (or (and (boundp 'my/undo-tree-history-state-dir)
             my/undo-tree-history-state-dir)
        (expand-file-name "var/undo-tree-history/" user-emacs-directory)))

  (unless (file-directory-p my/undo-tree-history-dir)
    (make-directory my/undo-tree-history-dir t))

  ;; 3) 让 undo-tree 把历史文件写入该目录（而不是到处散落）
  ;;    undo-tree 用的是 `undo-tree-history-directory-alist`
  (setq undo-tree-history-directory-alist
        `(("." . ,my/undo-tree-history-dir)))

  (defun my/undo-tree-history-enabled-p (&optional file)
    "Return non-nil when FILE should persist undo-tree history locally."
    (and (stringp file)
         (not (string-empty-p file))))

  (defun my/undo-tree-sanitize-history-label (label)
    "Return LABEL sanitized for use in a local undo history filename."
    (let ((sanitized (replace-regexp-in-string "[^[:alnum:]._-]+" "_" label)))
      (if (string-empty-p sanitized) "buffer" sanitized)))

  (defun my/undo-tree-normalize-file-name (file)
    "Return a stable canonical file name for FILE when saving undo history."
    (let ((expanded-file (expand-file-name file)))
      (if (file-exists-p expanded-file)
          (file-truename expanded-file)
        expanded-file)))

  (defun my/undo-tree-history-key (file)
    "Return a stable key for FILE used to derive local history filenames."
    (if-let* ((remote (file-remote-p file 'method)))
        (progn
          (require 'tramp)
          (let* ((vec (tramp-dissect-file-name file))
                 (method (or (tramp-file-name-method vec) "remote"))
                 (user (or (tramp-file-name-user vec) "nouser"))
                 (host (or (tramp-file-name-host vec) "nohost"))
                 (local (file-local-name file)))
            (format "remote:%s:%s@%s:%s"
                    method user host
                    (my/undo-tree-normalize-file-name local))))
      (format "local:%s" (my/undo-tree-normalize-file-name file))))

  (defun my/undo-tree-history-file-name-v2 (file)
    "Return the v2 local undo-tree history path for FILE."
    (let* ((key (my/undo-tree-history-key file))
           (remote-p (file-remote-p file))
           (category (if remote-p "remote" "local"))
           (leaf (or (file-name-nondirectory
                      (directory-file-name (if remote-p
                                               (file-local-name file)
                                             file)))
                     "buffer"))
           (label (my/undo-tree-sanitize-history-label leaf))
           (hash (secure-hash 'sha1 key))
           (dir (expand-file-name category my/undo-tree-history-dir))
           (history-file (expand-file-name
                          (format "%s-%s.~undo-tree~" label hash)
                          dir)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      history-file))

  (defun my/undo-tree-history-file-name (file &optional legacy)
    "Return the undo-tree history path for FILE.
When LEGACY is non-nil, keep FILE unchanged instead of canonicalizing it."
    (if legacy
        (let* ((backup-directory-alist undo-tree-history-directory-alist)
               (target-file (my/undo-tree-normalize-file-name file))
               (name (make-backup-file-name-1 target-file))
               (history-file
                (concat (file-name-directory name) "."
                        (file-name-nondirectory name)
                        ".~undo-tree~")))
          (when-let* ((dir (file-name-directory history-file)))
            (unless (file-directory-p dir)
              (make-directory dir t)))
          history-file)
      (my/undo-tree-history-file-name-v2 file)))

  (defun my/undo-tree-make-history-save-file-name (_orig-fun file)
    "Use a stable history filename for FILE."
    (my/undo-tree-history-file-name file))

  (defun my/undo-tree-load-history-core (&optional filename noerror)
    "Load undo-tree history for the current buffer from FILENAME.
When FILENAME is nil, resolve the canonical local history path."
    (let* ((canonical-file (and buffer-file-name
                                (my/undo-tree-history-file-name buffer-file-name)))
           (legacy-file (and buffer-file-name
                             (my/undo-tree-history-file-name buffer-file-name 'legacy)))
           (resolved-file
            (cond
             (filename filename)
             ((and canonical-file (file-exists-p canonical-file)) canonical-file)
             ((and legacy-file (file-exists-p legacy-file)) legacy-file)
             (t canonical-file))))
      (condition-case err
          (let ((inhibit-message t)
                (message-log-max nil))
            (undo-tree-load-history resolved-file noerror))
        (error
         ;; Broken history files should not keep polluting every reopen.
         (when (and resolved-file
                    (file-exists-p resolved-file))
           (ignore-errors
             (delete-file resolved-file)))
         (display-warning
          'undo-tree
          (format "Discarded unreadable undo-tree history %s: %s"
                  resolved-file
                  (error-message-string err))
          :warning)
         nil))))

  (defun my/undo-tree-load-history (orig-fun &optional filename noerror)
    "Load undo history through ORIG-FUN, with path fallbacks for FILENAME."
    (cond
     ((and (null filename)
           buffer-file-name
           (not (my/undo-tree-history-enabled-p buffer-file-name)))
      nil)
     ((and (null filename)
           buffer-file-name
           (buffer-modified-p))
      ;; Some modes touch the buffer during file-open hooks. Loading persisted
      ;; history after that triggers undo-tree's hash mismatch warning and the
      ;; history cannot be attached safely anyway.
      nil)
     ((or filename (not buffer-file-name))
      (funcall orig-fun filename noerror))
     (t
      (my/undo-tree-load-history-core nil noerror))))

  (defun my/undo-tree-save-history (orig-fun &optional filename overwrite)
    "Save undo history through ORIG-FUN into the local var history dir."
    (if (and (null filename)
             buffer-file-name
             (not (my/undo-tree-history-enabled-p buffer-file-name)))
        nil
      (funcall orig-fun filename overwrite)))

  (defun my/undo-tree-load-history-early-a (orig-fun &rest args)
    "Restore persistent undo history before `after-find-file' runs hooks.
This avoids hash mismatches when file-open hooks mutate the buffer."
    (when (and buffer-file-name
               undo-tree-auto-save-history
               (not revert-buffer-in-progress-p)
               (not (eq buffer-undo-list t))
               (my/undo-tree-history-enabled-p buffer-file-name)
               (not (buffer-modified-p)))
      (unless undo-tree-mode
        (undo-tree-mode 1))
      (my/undo-tree-load-history-core nil 'noerror))
    (apply orig-fun args))

  ;; 4) 让它在“本地文件”自动把历史写盘（崩溃也可恢复）
  (setq undo-tree-auto-save-history t)

  :after hydra
  :bind ("C-x C-h u" . hydra-undo-tree/body)
  :hydra (hydra-undo-tree (:hint nil)
  "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue))
  :config
  (setq undo-tree-auto-save-history t)
  (advice-add 'undo-tree-make-history-save-file-name
              :around #'my/undo-tree-make-history-save-file-name)
  (advice-add 'undo-tree-load-history :around #'my/undo-tree-load-history)
  (advice-add 'undo-tree-save-history :around #'my/undo-tree-save-history)
  (advice-add 'after-find-file :around #'my/undo-tree-load-history-early-a)
  ;; Only persist history for on-disk file states. Saving on `kill-buffer-hook'
  ;; can poison the history file when the buffer has unsaved edits, causing the
  ;; next reopen to fail the hash check and drop persistent undo history.
  (remove-hook 'kill-buffer-hook #'undo-tree-save-history-from-hook)
  (remove-hook 'find-file-hook #'undo-tree-load-history-from-hook))


(use-package outline-indent
  :ensure t
  :hook (prog-mode . outline-indent-minor-mode))


;;(setq debug-on-quit t)

(defvar pv/spinner nil)

(declare-function spinner-start "spinner" (&optional spinner-type))
(declare-function spinner-stop "spinner" (spinner))

(defun pv/spinner-start ()
  (require 'spinner)
  (setq pv/spinner (spinner-start 'progress-bar-filled))
  (force-mode-line-update t))

(defun pv/spinner-stop ()
  (when pv/spinner
    (spinner-stop pv/spinner)
    (setq pv/spinner nil)
    (force-mode-line-update t)))

(defun pv/with-spinner (orig &rest args)
  (pv/spinner-start)
  (unwind-protect
      (apply orig args)
    (pv/spinner-stop)))

;; 只给远程文件转圈（避免本地也烦）
(defun pv/find-file-with-spinner (orig filename &rest args)
  (if (file-remote-p filename)
      (pv/with-spinner (lambda () (apply orig filename args)))
    (apply orig filename args)))

(advice-add 'find-file :around #'pv/find-file-with-spinner)


(keymap-global-unset "C-x m")
(keymap-global-unset "C-x 4 m")
(keymap-global-unset "C-x 5 m")
(keymap-global-unset "C-x 5 m")


(setq print-escape-newlines t  ; 字符串中的 换行 打印成‘\n’.  注意, 推荐用‘?\s’表示空格的字符常量.
      ;; "打印成‘^C’而非‘\3’, 但‘\n’和‘\f’仍受‘print-escape-newlines’控制.
      print-escape-control-characters nil
      ctl-arrow t
      ;; 不把 multibyte 打印成‘\xXXXX’.
      print-escape-multibyte nil
      ;; 若不启用‘ctl-arrow’, 则‘\x80’而非‘\200’.
      display-raw-bytes-as-hex t)

(setq print-length nil  ; 当打印的 列表 元素数 > 该值时, 超出部分用省略号表示.
      eval-expression-print-length nil)

(setq print-level nil
      eval-expression-print-level nil)

(setq print-circle t  ; 使用 “#N=(#N#)” 语法 打印 递归结构.
      ;; 允许 (字面上) 读取循环结构.
      read-circle t)

(setq print-integers-as-characters nil  ; 打印 字符常量 的方式: “115 (#o163, ...)” instea of “?s (#o163, ...)”.
      ;; 打印 字符常量 时 括号内: “(#o163, #x73)” instead of “(#o163, #x73, ?s)”.
      eval-expression-print-maximum-character most-positive-fixnum)

;; Debugger 以 C 风格 显示 函数调用, 而不是 Lisp 风格.
(setopt debugger-stack-frame-as-list nil)

;; GC 时在 echo area 显示信息, 但不会并入到 “*Messages*” 中.
(setopt garbage-collection-messages t)

(setq auto-mode-case-fold t)

;; 如有必要, 会在 写入/重命名 文件后 执行 ‘normal-mode’ 以使用恰当的 major mode.
(setq change-major-mode-with-file-name t)


;;; Minibuffer:

;; 默认情况下, 点击 “echo area” 会打开 “*Messages*”, 在此关闭这个功能.
(keymap-unset minibuffer-inactive-mode-map "<mouse-1>")  ; ‘view-echo-area-messages’

(add-hook 'minibuffer-mode-hook
          (lambda ()
            (keymap-set minibuffer-local-completion-map "SPC"
                        #'self-insert-command)
            (keymap-set minibuffer-local-completion-map "?"
                        #'self-insert-command)))

;;; Invoke Command
(setq meta-prefix-char ?\e)

(keymap-global-unset "C-x ESC ESC")  ; ‘repeat-complex-command’

;;; Read
(setq read-extended-command-predicate #'command-completion-default-include-p
      read-file-name-completion-ignore-case t
      ;; “C-q” 后接 16 进制.
      read-quoted-char-radix 16
      read-buffer-completion-ignore-case t)

(setq enable-recursive-minibuffers t)

;; 大部分情况下, 保留从 ‘read-from-minibuffer’ 获取的文本的属性.
(setq minibuffer-allow-text-properties t)
(setq minibuffer-default-prompt-format #(" (default %s)"
                                         10 12 (face (underline (:foreground "VioletRed1")))))
(setq file-name-shadow-mode t)  ; ‘find-file’时, 若输入绝对路径, 则调暗默认值的前景.

;; 获取输入之后, 恢复进入 minibuffer 之前 当前 frame 的 window-configurations.
(setq read-minibuffer-restore-windows t)

;;; 性能相关:

;; 不清除 字体 缓存.
(setq inhibit-compacting-font-caches t)

;; 32G 机器优先减少前台 GC 频率, 让大块编辑/渲染工作更少被打断.
(setq gc-cons-threshold (* 96 1024 1024)
      gc-cons-percentage 0.3)



;;; Evaluation:

(setopt debug-on-quit nil  ; 按下 “C-g” 时是否要进入 debugger.
        ;; 在 ‘eval-expression’ 时暂时地将 ‘debug-on-error’ 设置为 t.
        eval-expression-debug-on-error t)

;;; 与 宿主 OS 交互

;;; Process:

(setq read-process-output-max (min (pcase system-type
                                     ('gnu/linux
                                      (string-to-number  (shell-command-to-string "cat /proc/sys/fs/pipe-max-size")))
                                     (_
                                      most-positive-fixnum))
                                   (* 8 1024 1024))
      process-adaptive-read-buffering t

      w32-pipe-buffer-size read-process-output-max
      w32-pipe-read-delay 0)
;;; File System:


(setopt delete-by-moving-to-trash t)


;;; 图片:

;; 居然没有 ‘image-mode-hook’, 简直逆天!  只能用下面这个作为 work-around 了.
(add-hook 'image-mode-new-window-functions
          (lambda (_)
            (display-line-numbers-mode -1)))
;;; 视频:

;;; PDF:


(setopt user-full-name    "Chang He (Aaron)"
        user-mail-address "mail")

(defun my/delete-frame-dwim ()
  "Close the current frame when possible, otherwise bury the current buffer."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (bury-buffer)))

(defcustom my/warning-popup-minimum-level :error
  "Minimum warning severity that may auto-display the warnings buffer.
Warnings below this level are still logged to `*Warnings*', but they do not
interrupt the current window layout."
  :type '(choice (const :tag "Debug" :debug)
                 (const :tag "Warning" :warning)
                 (const :tag "Error" :error)
                 (const :tag "Emergency" :emergency))
  :group 'convenience)

(defcustom my/warning-suppress-elpa-noise t
  "Whether to suppress low-severity startup noise from third-party ELPA packages."
  :type 'boolean
  :group 'convenience)

(defconst my/warnings-buffer-name "*Warnings*")

(defun my/warning-level-rank (level)
  "Return a comparable numeric rank for warning LEVEL."
  (pcase level
    (:debug 0)
    (:warning 1)
    (:error 2)
    (:emergency 3)
    (_ 1)))

(defun my/warning-level-at-least-p (level threshold)
  "Return non-nil when warning LEVEL is at least THRESHOLD."
  (>= (my/warning-level-rank level)
      (my/warning-level-rank threshold)))

(defun my/warning-type-list (type)
  "Normalize warning TYPE into the list form used by `warning-suppress-types'."
  (if (consp type) type (list type)))

(defun my/warning-should-suppress-p (type message level)
  "Return non-nil when TYPE/MESSAGE/LEVEL should be suppressed entirely."
  (let ((level (or level :warning)))
    (and my/warning-suppress-elpa-noise
         (not (my/warning-level-at-least-p level :error))
         (stringp message)
         (or (string-match-p "/elpa/" message)
             (string-match-p "\\`Package cl is deprecated\\'" message)
             (string-match-p
              "setting attribute .+:foreground.+'font-lock-variable-name-face'.+nil value is invalid"
              message)
             (let ((type-list (my/warning-type-list type)))
               (and (member 'package type-list)
                    (string-match-p "deprecated" message)))))))

(defun my/suppress-warning-popups-a (orig-fn type message &optional level buffer-name)
  "Keep lower-severity warnings in the log without auto-popping a window."
  (let ((level (or level :warning)))
    (cond
     ((my/warning-should-suppress-p type message level)
      nil)
     ((my/warning-level-at-least-p level my/warning-popup-minimum-level)
      (funcall orig-fn type message level buffer-name))
     (t
      (let ((warning-suppress-types
             (cons (my/warning-type-list type) warning-suppress-types)))
        (funcall orig-fn type message level buffer-name))))))

(defun my/show-warnings-buffer ()
  "Show the warning log buffer if it exists."
  (interactive)
  (if-let* ((buffer (get-buffer my/warnings-buffer-name)))
      (pop-to-buffer buffer)
    (user-error "No warnings buffer yet")))

(unless (advice-member-p #'my/suppress-warning-popups-a 'display-warning)
  (advice-add 'display-warning :around #'my/suppress-warning-popups-a))


;; 不要在任何情况下自动显示 diary
(setq calendar-mark-diary-entries-flag nil)  ;; 你已经设了
(setq calendar-view-diary-initially-flag nil)
(setq calendar-mark-holidays-flag nil)

;; org agenda 里也不要混入 diary
(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file nil)



(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(with-eval-after-load 'popper
  (defun my/escape-close-popup-h ()
    "Close the selected Popper popup when present."
    (when (and (bound-and-true-p popper-mode)
               (fboundp 'popper-popup-p)
               (window-live-p (selected-window))
               (popper-popup-p (window-buffer (selected-window))))
      (if (fboundp 'popper-close-latest)
          (popper-close-latest)
        (quit-window nil (selected-window)))
      t))
  (add-hook 'my/escape-hook #'my/escape-close-popup-h))



(require 'fingertip)
(require 'thing-edit)
(require 'move-text)
(require 'open-newline)
(require 'duplicate-line)
(require 'toggle-one-window)
(require 'winpoint)
(require 'basic-toolkit)
(require 'symbol-overlay)


;; 确保所有文件路径都解析 symlink
(setq find-file-visit-truename t)

(provide 'init-base)

;;; init-base.el ends here

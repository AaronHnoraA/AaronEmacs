;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

(add-to-list 'exec-path (expand-file-name "tools" user-emacs-directory))
(setenv "PATH"
        (concat (expand-file-name "tools" user-emacs-directory)
                ":" (getenv "PATH")))
;;; Code:

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

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;(unless (package-installed-p 'ligature) 
;  (package-vc-install '
;                      (ligture
;                      :url "https://github.com/mickeynp/ligature.el.git" 
;                      :rev 
;                      :last-release)
;                      )
;)

(require 'ligature)

(ligature-set-ligatures
 't
 '("==" "===" "!=" "->" "<-" "<->"
   "=>" "<=" ">=" "::" ":="
   "&&" "||" ">>" "<<"))

(global-ligature-mode t)



;; 使用绝对行号
(setq display-line-numbers-type 'absolute)


;; 可选：关闭次刻度（避免干扰）
(setq display-line-numbers-minor-tick 0)

;; ========= 行号显示策略 =========
(setq display-line-numbers-minor-tick 0)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; ========= 行号字体 =========
(set-face-attribute 'line-number nil
                    :family "Fira Code"
                    :height 0.85
                    :foreground "#6b7280")

(set-face-attribute 'line-number-current-line nil
                    :family "Fira Code"
                    :height 0.85
                    :foreground "#ffffff")


;; 1. 设置触发频率（例如每 5 行高亮一次）
(setq display-line-numbers-major-tick 20)

;; 2. 直接配置内置的 Face
;; 注意：Emacs 30 已经内置了 line-number-major-tick 这个 Face
(custom-set-faces
 '(line-number-major-tick ((t :inherit line-number 
                              :weight bold 
                              :foreground "#ff9e64"))))

;; 3. 启用行号（如果还没启用）
(global-display-line-numbers-mode 1)

;; 启用行号（全局或按需）
(global-display-line-numbers-mode 1)

;; 只在编程模式开（你也可以换成 text-mode / 全局）
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
;; 可选：在一些模式禁用（终端、shell 等）
(dolist (hook '(term-mode-hook vterm-mode-hook eshell-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))




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

(setq-default fill-column 80)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; No tabs
;; Indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;;; -*- lexical-binding: t; -*-
(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-fontset-font t '(#xe000 . #xf8ff) "JetBrainsMono Nerd Font" nil 'append))
;; ======================================================================
;; 1. 变量定义 (用户自定义区域)
;; ======================================================================

;; 英文正文 (Body) - Org 普通文本（variable-pitch）
(defvar my/font-body  "Futura")
(defvar my/h-body     200)

;; 代码/表格 (Code) - 默认界面 + 代码环境（fixed-pitch）
(defvar my/font-code  "Fira Code")
(defvar my/h-code     160)

;; 标题 (Title) - Org 标题
(defvar my/font-title "Excalifont")
(defvar my/h-title    220)

;; 中文 (Chinese)
(defvar my/font-cn    "FZLiuGongQuanKaiShuJF")
(defvar my/scale-cn   1.3)

;; ======================================================================
;; 2. 核心：应用字体（全局 + 中文绑定）
;; ======================================================================

(defun my/font--apply-core-faces ()
  "设置 default/fixed/variable 三类 face。"
  ;; 默认 = 代码字体（稳定，且界面/代码/表格都不乱）
  (set-face-attribute 'default nil
                      :family my/font-code
                      :height my/h-code
                      :weight 'semi-bold)

  ;; 固定宽度 = 代码字体（确保 org-block/org-table 等继承后稳定）
  (set-face-attribute 'fixed-pitch nil
                      :family my/font-body
                      :height my/h-body
                      :weight 'regular)

  ;; 变宽 = 正文字体（mixed-pitch 会让 Org 正文用它）
  (set-face-attribute 'variable-pitch nil
                      :family my/font-body
                      :height my/h-body
                      :weight 'regular))

(defun my/font--bind-chinese-to-fontset ()
  "把中文相关字符集强制绑定到 my/font-cn。"
  (when (member my/font-cn (font-family-list))
    ;; t 表示当前 frame 的 fontset；也会影响后续 frame 的默认 fontset 选择
    (dolist (charset '(han cjk-misc bopomofo kana hangul))
      ;; 'prepend：把该字体放在 fallback 优先级前面，避免被系统中文字体截胡
      (set-fontset-font t charset (font-spec :family my/font-cn) nil 'prepend))))

(defun my/font--apply-rescale ()
  "设置中文缩放（只调大小，不负责选字体）。"
  (setq face-font-rescale-alist
        (assq-delete-all my/font-cn face-font-rescale-alist))
  (add-to-list 'face-font-rescale-alist (cons my/font-cn my/scale-cn)))

(defun my/apply-font-config (&optional frame)
  "应用全部字体设置。可用于 daemon 新 frame。
若传入 FRAME，则在该 frame 上应用。"
  (interactive)
  (when (frame-live-p frame)
    (select-frame frame))

  (my/font--apply-core-faces)
  (my/font--bind-chinese-to-fontset)
  (my/font--apply-rescale)

  ;; 保险：让 Emacs 重新评估字体缓存（可选但很稳）
  (when (fboundp 'font-cache-reset)
    (font-cache-reset)))

;; 启动立即生效
(my/apply-font-config)

;; Daemon / 新 frame 也生效
(add-hook 'server-after-make-frame-hook #'my/apply-font-config)

;; ======================================================================
;; 3. Org Mode & mixed-pitch 适配
;; ======================================================================

;; 你如果用 use-package：确保 mixed-pitch 可用
;; (use-package mixed-pitch :ensure t)

(defun my/org-font-setup ()
  "Org 模式：启用 mixed-pitch，设置标题字体，保护代码/表格等 fixed-pitch。"
  (when (fboundp 'mixed-pitch-mode)
    (mixed-pitch-mode 1))

  ;; 标题：只改 org 的标题 faces，不要去动 default
  (dolist (face '(org-level-1 org-level-2 org-level-3
                  org-level-4 org-level-5 org-level-6
                  org-level-7 org-level-8 org-document-title))
    (set-face-attribute face nil
                        :family my/font-title
                        :height my/h-title
                        :weight 'bold))

  ;; 保护：这些必须固定宽度（对齐/可读性）
  (dolist (face '(org-block
                  org-block-begin-line
                  org-block-end-line
                  org-table
                  org-formula
                  org-code
                  org-verbatim
                  org-meta-line
                  org-checkbox
                  line-number
                  line-number-current-line))
    ;; 继承 fixed-pitch 即可；高度可按需统一到代码高度
    (set-face-attribute face nil
                        :inherit 'fixed-pitch
                        :height my/h-code))

  (my/apply-font-config)
  ;; 确保中文绑定在 Org 里也不会被覆盖（有些主题/包会动 fontset）
  (my/font--bind-chinese-to-fontset))

(add-hook 'org-mode-hook #'my/org-font-setup)

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
  ;; don't save current clipboard text before replacing it
  (save-interprogram-paste-before-kill nil)
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

;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

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
  (completion-auto-help t)
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

;; Appointment
(use-package appt
  :ensure nil
  :hook (after-init . appt-activate)
  :config
  (defun appt-display-with-notification (min-to-app new-time appt-msg)
    (notify-send :title (format "Appointment in %s minutes" min-to-app)
                 :body appt-msg
                 :urgency 'critical)
    (appt-disp-window min-to-app new-time appt-msg))
  :custom
  (appt-audible nil)
  (appt-display-diary nil)
  (appt-display-interval 5)
  (appt-display-mode-line t)
  (appt-message-warning-time 15)
  (appt-disp-window-function #'appt-display-with-notification))

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

;; window layout manager
;;
;; gt next-tab
;; gT prev-tab
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated))

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

;; transparent remote access
(use-package tramp
  :ensure t
  :defer t
  :custom
  ;; Always use file cache when using tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-default-method "ssh"))

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
  (recentf-max-saved-items 300)
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

;; MacOS specific
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))


(use-package vlf
  :ensure t)
(eval-after-load "vlf"
  '(define-key vlf-prefix-map "\C-xv" vlf-mode-map))

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

(save-place-mode 1)

;; 保存 minibuffer 历史
;; 保存最近打开的文件记录
(unless (package-installed-p 'buttercup) 
  (package-vc-install '
                      (buttercup
                      :url "https://github.com/jorgenschaefer/emacs-buttercup.git" 
                      :rev 
                      :last-release)
                      )
)
(require 'buttercup)
(recentf-mode 1)
(setq recentf-max-saved-items 200) ; 设置记录的文件数量

(unless (package-installed-p 'multiple-cursors) 
  (package-vc-install '
                      (multiple-cursors
                      :url "https://github.com/jwiegley/emacs-async.git" 
                      :rev 
                      :last-release)
                      )
)

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(use-package amx
  :ensure t
  :init (amx-mode))

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
;;; undo-tree: 本地集中存放历史文件；远程(ssh/TRAMP)不落盘
;;; ============================
(use-package undo-tree
  :ensure t
  :init
  ;; 1) 开启全局 undo-tree
  (global-undo-tree-mode 1)

  ;; 2) 本地历史文件统一放到一个目录（只在本机路径生效）
  ;;    你可以改成你喜欢的位置：例如 ~/.emacs.d/undo-tree/
  (defvar my/undo-tree-history-dir
    (expand-file-name "undo-tree-history/" user-emacs-directory))

  (unless (file-directory-p my/undo-tree-history-dir)
    (make-directory my/undo-tree-history-dir t))

  ;; 3) 让 undo-tree 把历史文件写入该目录（而不是到处散落）
  ;;    undo-tree 用的是 `undo-tree-history-directory-alist`
  (setq undo-tree-history-directory-alist
        `(("." . ,my/undo-tree-history-dir)))

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
  
  ;; 关键:在打开文件后立即加载历史
  (add-hook 'find-file-hook #'undo-tree-load-history-hook)
  )


(use-package outline-indent
  :ensure t
  :hook (prog-mode . outline-indent-minor-mode))

(with-eval-after-load 'tramp
  ;; 0 最安静，6 最啰嗦；建议 3~4
  (setq tramp-verbose 4)
  ;; 让 TRAMP 的消息更容易被看到
  (setq tramp-message-show-message t))

(defun pv/with-find-file-feedback (orig filename &rest args)
  (let* ((remote (file-remote-p filename))
         (label  (if remote "TRAMP/SSH" "Local"))
         (t0 (float-time)))
    (message "[%s] Opening: %s" label filename)
    (prog1 (apply orig filename args)
      (message "[%s] Opened in %.2fs: %s"
               label (- (float-time) t0) filename))))

(advice-add 'find-file :around #'pv/with-find-file-feedback)

;;(setq debug-on-quit t)

(use-package spinner
  :ensure t)

(defvar pv/spinner nil)

(defun pv/spinner-start ()
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

(global-so-long-mode)



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
                                      most-positive-fixnum)) (* 1024 1024))
      process-adaptive-read-buffering "急切读取"

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





(provide 'init-base)

;;; init-base.el ends here

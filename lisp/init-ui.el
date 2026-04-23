;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function on-screen-mode "on-screen" (&optional arg))
(defvar kanagawa-themes-custom-colors)
(defvar my/theme-unspecified 'unspecified
  "Literal face attribute value used by some themes during runtime expansion.")
(defvar my/dashboard-ui--theme-signature nil
  "Last theme signature applied by `my/dashboard-apply-ui'.")
(defvar my/chunlian--visibility-timer nil
  "Idle timer used to coalesce dashboard visibility checks.")
;; Emacs 31 rejects nil as a face attribute value; themes written for earlier
;; Emacs versions may still pass nil.  Convert nil → 'unspecified at the call
;; site so the "nil value is invalid" warning is never emitted.
(when (>= emacs-major-version 31)
  (define-advice set-face-attribute (:filter-args (args) fix-nil-to-unspecified)
    "Convert nil face attribute values to \\='unspecified (Emacs 31 compatibility)."
    (let ((face  (nth 0 args))
          (frame (nth 1 args))
          (rest  (nthcdr 2 args))
          fixed)
      (while rest
        (push (car rest) fixed)
        (push (if (null (cadr rest)) 'unspecified (cadr rest)) fixed)
        (setq rest (cddr rest)))
      (cons face (cons frame (nreverse fixed))))))


(defgroup my/chunlian nil
  "Decorative Chunlian UI for dashboard."
  :group 'faces)

;; Use Iosvkem in terminals
(use-package aaron-ui
  :load-path "~/.emacs.d/site-lisp/aaron-ui"
  :demand t
  :config
  (aaron-ui-load-theme 'wave))

(defun my/theme-normalize-faces ()
  "Normalize theme face attributes that Emacs 31 rejects when set to nil."
  (when (and (facep 'font-lock-variable-name-face)
             (null (face-attribute 'font-lock-variable-name-face :foreground nil 'default)))
    (set-face-attribute 'font-lock-variable-name-face nil :foreground 'unspecified)))

(add-hook 'after-load-theme-hook #'my/theme-normalize-faces)

(use-package doom-modeline
  :ensure t
  :defer 1
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 0)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-modal nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-persp-name t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :defer 1
  :config
  (shackle-mode 1)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((vc-annotate-mode         :select t :inhibit-window-quit t :same t)
                   ("*quickrun*"             :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode     :select t)
                   (xwidget-webkit-mode      :select t :same t)
                   (comint-mode              :select t :align t :size 0.4)
                   (grep-mode                :select t :align t)
                   (rg-mode                  :select t :align t)
                   ;; See also `help-window-select'
                   (apropos-mode             :select nil :align t :size 0.4)
                   (help-mode                :select nil :align t :size 0.4)
                   ("*Backtrace*"               :select t   :align t :size 15)
                   ("*Shell Command Output*"    :select nil :align t :size 0.4)
                   ("*Async Shell Command*"     :select nil :align t :size 0.4)
                   ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
                   ("*Process List*"            :select t   :align t :size 0.3)
                   ("*Occur*"                   :select t   :align t)
                   ("\\*eldoc\\( for \\)?.*\\*" :select t   :align t :size 15 :regexp t))))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-window-keep-selected t)
  (help-enable-variable-value-editing t))

(use-package helpful
  :ensure t
  :after help
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key))
  :custom
  (helpful-switch-buffer-function #'pop-to-buffer))

;; Windows layout recorder
;;
;; You can still use `winner-mode' on Emacs 26 or early. On Emacs 27, it's
;; preferred over `winner-mode' for better compatibility with `tab-bar-mode'.
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-history-mode)
  :custom
  (tab-bar-history-buttons-show nil)
  (tab-bar-show 0))



(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(defun my/dashboard-upgrade-packages (&rest _)
  "Upgrade installed packages from the dashboard button."
  (interactive)
  (my/package-upgrade-all-noninteractive))

(use-package dashboard
  :ensure t
  :init
  ;; 导航按钮配置 (保持不变)
  (setq dashboard-navigator-buttons
        `(((,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-mark_github") "★")
            "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
           (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-heart") "♥")
            "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
           (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-alert") "⚑")
            "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
           (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-download") "♺")
            "Upgrade" "Upgrade archive and VC packages, then refresh the lock file"
            ,#'my/dashboard-upgrade-packages success))))
  
  (dashboard-setup-startup-hook)

  :config
  (defconst homepage-url "https://git.pwo101.top/")
  (defconst stars-url (concat "https://github.com/AaronHnoraA/AaronEmacs" "/stargazers"))
  (defconst issue-url (concat "https://github.com/AaronHnoraA/AaronEmacs" "/issues/new"))

  :custom
  ;; Keep the dashboard banner in `etc/` with other local UI config.
  (dashboard-banner-logo-title "Aaron's Emacs")
  (dashboard-startup-banner
   (expand-file-name "etc/dashboard.txt" user-emacs-directory))

  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents  . 10)
                     (projects . 7)))
  (dashboard-startupify-list '(
                               dashboard-insert-newline
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-banner
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-footer)))

;;; Menu Bar:

(keymap-global-unset "<menu-bar> <file> <close-tab>")
(keymap-global-unset "<menu-bar> <file> <delete-this-frame>")
(keymap-global-unset "<menu-bar> <file> <kill-buffer>")
(keymap-global-unset "<menu-bar> <file> <make-frame>")
(keymap-global-unset "<menu-bar> <file> <make-tab>")
(keymap-global-unset "<menu-bar> <file> <new-window-below>")
(keymap-global-unset "<menu-bar> <file> <new-window-on-right>")
(keymap-global-unset "<menu-bar> <file> <one-window>")
(keymap-global-unset "<menu-bar> <file> <open-file>")
(keymap-global-unset "<menu-bar> <file> <save-buffer>")

(keymap-global-unset "<menu-bar> <edit> <copy>")
(keymap-global-unset "<menu-bar> <edit> <cut>")
(keymap-global-unset "<menu-bar> <edit> <mark-whole-buffer>")
(keymap-global-unset "<menu-bar> <edit> <paste>")
(keymap-global-unset "<menu-bar> <edit> <undo-redo>")
(keymap-global-unset "<menu-bar> <edit> <undo>")

(keymap-global-unset "<menu-bar> <options> <cua-mode>")
(keymap-global-unset "<menu-bar> <options> <customize> <customize-saved>")
(keymap-global-unset "<menu-bar> <options> <save>")
(keymap-global-unset "<menu-bar> <options> <uniquify>")
(keymap-global-unset "<menu-bar> <options> <save-place>")
(keymap-global-unset "<menu-bar> <options> <transient-mark-mode>")
(keymap-global-unset "<menu-bar> <options> <highlight-paren-mode>")

(keymap-global-unset "<menu-bar> <buffer> <select-named-buffer>")

(keymap-global-unset "<menu-bar> <tools> <browse-web>")
(keymap-global-unset "<menu-bar> <tools> <gnus>")


(keymap-global-unset "<menu-bar> <help-menu> <about-emacs>")
(keymap-global-unset "<menu-bar> <help-menu> <about-gnu-project>")
(keymap-global-unset "<menu-bar> <help-menu> <describe-copying>")
(keymap-global-unset "<menu-bar> <help-menu> <describe-no-warranty>")
(keymap-global-unset "<menu-bar> <help-menu> <emacs-manual>")
(keymap-global-unset "<menu-bar> <help-menu> <emacs-tutorial>")
(keymap-global-unset "<menu-bar> <help-menu> <external-packages>")
(keymap-global-unset "<menu-bar> <help-menu> <getting-new-versions>")
(keymap-global-unset "<menu-bar> <help-menu> <more-manuals> <order-emacs-manuals>")

;;; Tab Line:

(my/package-ensure-vc 'all-the-icons "https://github.com/domtronn/all-the-icons.el.git")

;;; Text Area:


;; 渲染成对的单引号时, 尽可能使用 ‘curve’ 这种样式, 退而求此次地可以使用 `grave' 这种样式.
(setopt text-quoting-style nil)
;;; Fringe:

(set-fringe-mode '(0 . nil))  ; Right-only.

;;; Scroll Bar:

(setopt scroll-bar-mode 'right)

;; 滚动条落至底部 (overscrolling) 时的行为.
(setopt scroll-bar-adjust-thumb-portion nil)

;;; Mode Line:

;; Face ‘mode-line-inactive’ for non-selected window’s mode line.
(setopt mode-line-in-non-selected-windows t)

(setopt mode-line-compact nil)  ; 不要设 t, 否则即使有多余的空间, 它也倾向于挤在一起.
(setopt mode-line-right-align-edge 'window)  ; 与 window 的边缘对齐.


(size-indication-mode)  ; 在 mode line 上显示 buffer 大小.
(setq mode-line-column-line-number-mode-map ())  ; 使某些可点击文本不作出应答.

;; 当 buffer 对应的文件名相同时, 在 buffer 名字之前补全文件的路径, 使 buffer 的名字互异.
(setopt uniquify-buffer-name-style 'forward
        ;; 当‘uniquify-buffer-name-style’的设置涉及补全文件路径时, 保留显示路径名之间相同的部分.
        uniquify-strip-common-suffix t)

(line-number-mode -1)  ; Mode line 上不要显示行号, 因为 window 左边缘已经显示行号了.

(defun my/ui-apply-polish ()
  "Apply non-color UI polish on top of the active theme."
  (interactive)
  (when (display-graphic-p)
    (setq-default mode-line-format mode-line-format)
    (setq-default tab-bar-separator "  ")
    (setopt tab-bar-auto-width t)
    (setopt tab-bar-close-button-show nil)
    (setopt tab-bar-new-button-show nil)
    (setopt tab-bar-tab-hints nil)
    (setopt window-divider-default-right-width 10)
    (setopt window-divider-default-bottom-width 1)
    (window-divider-mode 1)))

(add-hook 'after-init-hook #'my/ui-apply-polish)
(add-hook 'server-after-make-frame-hook #'my/ui-apply-polish)
(add-hook 'after-load-theme-hook #'my/ui-apply-polish)

(defun my/dashboard-apply-ui ()
  "Apply local UI styling to the dashboard."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/dashboard-ui--theme-signature)
        (setq my/dashboard-ui--theme-signature signature)
        (when (facep 'dashboard-banner-logo-title)
          (set-face-attribute 'dashboard-banner-logo-title nil
                              :foreground "#edf2f7"
                              :weight 'medium))
        (when (facep 'dashboard-heading)
          (set-face-attribute 'dashboard-heading nil
                              :foreground "#a9bed3"
                              :weight 'medium))
        (when (facep 'dashboard-items-face)
          (set-face-attribute 'dashboard-items-face nil
                              :foreground "#d8dee9"))
        (when (facep 'dashboard-footer-face)
          (set-face-attribute 'dashboard-footer-face nil
                              :foreground "#6f748b"))
        (when (facep 'dashboard-navigator)
          (set-face-attribute 'dashboard-navigator nil
                              :foreground "#8aa6c1"))
        (when (facep 'dashboard-text-banner)
          (set-face-attribute 'dashboard-text-banner nil
                              :foreground "#8b90a8"))))))

(add-hook 'dashboard-mode-hook #'my/dashboard-apply-ui)
(add-hook 'after-load-theme-hook #'my/dashboard-apply-ui)

;;; End of Line
(setopt eol-mnemonic-unix " LF "
        eol-mnemonic-mac  " CR "
        eol-mnemonic-dos  " CRLF "
        eol-mnemonic-undecided " ?EOL ")

;;; Minibuffer & Echo Area:

(setopt max-mini-window-height 0.3)

;; 由 输入 的 字符串 的 行数 决定如何 resize.
(setopt resize-mini-windows t)

;; Trim 首尾的空行.
(setopt resize-mini-frames #'fit-frame-to-buffer)
;;; Mouse:

(setq mouse-fine-grained-tracking nil)

(setopt display-hourglass t  ; When Emacs is busy, 将鼠标指针显示为 漏斗.
        ;; When Emacs is busy, 立刻将鼠标指针显示为漏斗.
        hourglass-delay 0)

;; 输入文本时不需要隐藏鼠标指针, 因为可以使用 ‘mouse-avoidance-mode’.
(setopt make-pointer-invisible nil)
(setopt mouse-avoidance-animation-delay 0.05)
(setopt mouse-avoidance-threshold  2  ; >=2
        mouse-avoidance-nudge-var  1  ; >=1
        mouse-avoidance-nudge-dist 2)
;;; Cursor:

(setopt cursor-type 'box
        ;; 在 non-selected window 中也 展示 cursor,
        ;; 但是 是 镂空的.
        cursor-in-non-selected-windows t)
(setopt x-stretch-cursor t)  ; 在 TAB 字符上拉长 cursor.

(blink-cursor-mode -1)

;; TUI 下, 尽可能地 使 cursor 外形或特征 更加显著.
(setopt visible-cursor t)

;;; Click:

(setopt double-click-fuzz 3  ; 双击时, 两次 button-down 之间 允许 的 位移/像素.
        double-click-time 400)

;;; Scroll:

(setq jit-lock-defer-time 0.3  ; Scroll 之后 延迟 fontify.
      ;; Scroll 时, 假定滚过的文本有 default face, 从而避免 fontify 它们.  当那些滚过的文本的 size 不一致时, 可能导致终点位置有偏差.
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      ;; TUI 下, recenter 时不 redraw frame, 可能造成屏幕有少许显示错误.  所以 此处仅考虑 TTY.
      recenter-redisplay 'tty)

(setopt mouse-wheel-follow-mouse t)
;; 匀速滚屏, 而不是滚轮越快, 滚屏速度越越越越快.
(setopt mouse-wheel-progressive-speed nil)
(setopt mouse-wheel-tilt-scroll t
        mouse-wheel-scroll-amount-horizontal 1)
(mouse-wheel-mode)

;; 禁用鼠标/触控板缩放, 避免误触修改 text scale.
(dolist (event '([pinch]
                 [C-wheel-up]
                 [C-wheel-down]
                 [C-mouse-4]
                 [C-mouse-5]))
  (define-key global-map event nil))

;; Scroll 以使 window 底端的 N 行呈现到顶端.
(setopt next-screen-context-lines 5)

;; 无法再 scroll 时 就 停住, 而不是继续移动至 buffer 首/尾.
(setopt scroll-error-top-bottom nil)

(setopt scroll-margin 1
        ;; ‘scroll-margin’的上界.
        maximum-scroll-margin 0.5)

(setq scroll-conservatively most-positive-fixnum
      ;; Minibuffer 永远 一行一行地 automatically scroll.
      scroll-minibuffer-conservatively t)

;; Scroll 时 通过 高亮 即将 滚走/来 的 篇幅 以 提示 滚动方向.
;; (仅在翻阅 ‘*Completions*’ buffer 的候选词时启用.)
(setopt on-screen-inverse-flag t
        on-screen-highlight-method 'shadow
        on-screen-delay 0.4)
(add-hook 'completion-list-mode-hook #'on-screen-mode)

;; 若非 nil, 则 scroll 时 (e.g., ‘C-v’) 保持 point 在屏幕上的位置 (有点像打字机), 但这样会扯坏 region.
(setopt scroll-preserve-screen-position nil)

;;; Horizontal
(setopt hscroll-margin 5
        hscroll-step 1)

(tooltip-mode -1)
;;; Dialo Box:

(setopt use-dialog-box t
        use-file-dialog t)

;; 在 GTK+ 的 file-chooser-dialog 中显示隐藏文件.
(setq x-gtk-show-hidden-files t)
;;; Render:

(setopt no-redraw-on-reenter t)



;;; ============================================================
;;; Indent Guide —— 稳定版配置（推荐）
;;; ============================================================

(use-package indent-guide
  :ensure t
  :hook (prog-mode . indent-guide-mode)
  :custom
  ;; 延迟一点点绘制，避免每个字符都重算（0.1~0.2 比较稳）
  (indent-guide-delay 0.15)

  ;; 不递归高亮 parent indent（递归会明显增加 redraw 负担）
  (indent-guide-recursive nil)

  ;; 字符宽度对齐（防止等宽/变宽字体下轻微抖动）
  (indent-guide-char "|")

  ;; 禁用在空行上画线（减少 overlay 数量）
  (indent-guide-disable-on-empty-lines t)

  ;; 不在注释/字符串里画（更干净，也更稳）
  (indent-guide-ignore-comments t)
  (indent-guide-ignore-strings t))

(with-eval-after-load 'indent-guide
  (custom-set-faces
   ;; 普通缩进线：淡灰蓝，不抢正文
   '(indent-guide-face
     ((t (:foreground "#5E81AC"))))))






(defface chunlian-face
  '((t :background "red" :foreground "yellow" :weight bold :height 2.0))
  "春联样式。"
  :group 'my/chunlian)
;; 新增一个专门用来填充红底的 Face
(defface chunlian-blank-face
  '((t :background "red"))
  "春联间隙的纯红背景。"
  :group 'my/chunlian)

(defvar chunlian-right "霜蹄千里辞寒岁")
(defvar chunlian-left "锦辔乘风纳清祥")
(defvar chunlian-top "乘影追风")

(defvar chunlian-top-offset 14 "向下偏移的行数。避开顶部的 Logo/Banner 空白。")
(defvar chunlian-margin-width 6 "左右 Margin 的宽度，确保放大的字能完整显示。")
(defvar chunlian-line-step 2 "每隔几行挂一个字。")

;; 【关键新增】：填充红条用的宽度。
;; 因为汉字放大了 2.0 倍，宽度也是原来的两倍（相当于 4 个英文字母或 2 个全角空格的宽度）。
;; 这里默认使用 2 个全角空格。如果不齐，可以改成 4 个半角空格 "    "
(defvar chunlian-blank-string "　　" "间隙填充字符，用纯红底色填补上下字的空隙。")

(defvar-local chunlian--overlays nil)

(defun chunlian--apply-window-margins (win)
  "Apply Chunlian margins to WIN while preserving previous margins."
  (when (window-live-p win)
    (unless (window-parameter win 'chunlian--saved-margins)
      (set-window-parameter win 'chunlian--saved-margins (window-margins win)))
    (set-window-parameter win 'chunlian--active t)
    (set-window-margins win chunlian-margin-width chunlian-margin-width)))

(defun chunlian--restore-window-margins (&optional win)
  "Restore margins previously changed by Chunlian.
When WIN is nil, restore every tracked window."
  (dolist (target (if (window-live-p win)
                      (list win)
                    (window-list nil 'no-minibuffer)))
    (when (and (window-live-p target)
               (window-parameter target 'chunlian--active))
      (let* ((saved (window-parameter target 'chunlian--saved-margins))
             (left (car-safe saved))
             (right (cdr-safe saved)))
        (set-window-margins target left right)
        (set-window-parameter target 'chunlian--active nil)
        (set-window-parameter target 'chunlian--saved-margins nil)))))

(defun chunlian--setup-overlays ()
  "在 buffer 两侧设置静态的春联 overlays。"
  ;; 确保只有在有窗口展示当前 buffer 时才进行渲染
  (when-let* ((wins (get-buffer-window-list (current-buffer) nil t)))
    (dolist (win wins)
      (chunlian--apply-window-margins win))
    (save-excursion
      ;; 清除旧的 overlays
      (mapc #'delete-overlay chunlian--overlays)
      (setq chunlian--overlays nil)

      (goto-char (point-min))
      ;; 往下空几行，跳过 Dashboard 顶部
      (forward-line chunlian-top-offset)

      (let ((len (min (length chunlian-left) (length chunlian-right))))
        (dotimes (i len)
          (when (not (eobp))
            ;; 1. 渲染带字的 Overlay
            (let* ((start (line-beginning-position))
                   (ov-l (make-overlay start start))
                   (ov-r (make-overlay start start))
                   (char-l (substring chunlian-left i (1+ i)))
                   (char-r (substring chunlian-right i (1+ i))))

              (overlay-put ov-l 'before-string
                           (propertize " " 'display
                                       `((margin left-margin)
                                         ,(propertize char-l 'face 'chunlian-face))))
              (overlay-put ov-r 'after-string
                           (propertize " " 'display
                                       `((margin right-margin)
                                         ,(propertize char-r 'face 'chunlian-face))))

              (push ov-l chunlian--overlays)
              (push ov-r chunlian--overlays))

            ;; 2. 【关键修复】如果不是最后一个字，填充红底的间隙
            (when (< i (1- len))
              (dotimes (step chunlian-line-step)
                (when (= (forward-line 1) 0)
                  ;; 仅在被作为“行距”跳过的行，塞入纯红空白块
                  (when (< step (1- chunlian-line-step))
                    (let* ((start (line-beginning-position))
                           (ov-l (make-overlay start start))
                           (ov-r (make-overlay start start))
                           ;; 涂上纯红背景的空格
                           (blank (propertize chunlian-blank-string 'face 'chunlian-blank-face)))

                      (overlay-put ov-l 'before-string
                                   (propertize " " 'display
                                               `((margin left-margin) ,blank)))
                      (overlay-put ov-r 'after-string
                                   (propertize " " 'display
                                               `((margin right-margin) ,blank)))

                      (push ov-l chunlian--overlays)
                      (push ov-r chunlian--overlays))))))))))))

(defun chunlian--setup-display ()
  "初始化春联显示。"
  (setq header-line-format
        (list (propertize " " 'display `(space :align-to (- center ,(string-width chunlian-top))))
              (propertize chunlian-top 'face 'chunlian-face)))
  (chunlian--setup-overlays))

(defun chunlian--clear-display ()
  "清除春联显示。"
  (setq header-line-format nil)
  (chunlian--restore-window-margins)
  (mapc #'delete-overlay chunlian--overlays)
  (setq chunlian--overlays nil))

(defun chunlian--maybe-disable ()
  "Disable `chunlian-mode' when the dashboard buffer is no longer visible."
  (when (and chunlian-mode
             (derived-mode-p 'dashboard-mode)
             (null (get-buffer-window (current-buffer) t)))
    (chunlian-mode -1)))

(defun my/chunlian-schedule-visibility-check (&rest _)
  "Coalesce repeated buffer-list changes before checking dashboard visibility."
  (when (timerp my/chunlian--visibility-timer)
    (cancel-timer my/chunlian--visibility-timer))
  (setq my/chunlian--visibility-timer
        (run-with-idle-timer
         0.1 nil
         (lambda ()
           (setq my/chunlian--visibility-timer nil)
           (when-let* ((buffer (get-buffer "*dashboard*")))
             (with-current-buffer buffer
               (when chunlian-mode
                 (chunlian--maybe-disable))))))))

(defun chunlian--cleanup-current-buffer ()
  "Release Chunlian overlays, margins, and buffer-local hooks."
  (remove-hook 'change-major-mode-hook #'chunlian--cleanup-current-buffer t)
  (remove-hook 'kill-buffer-hook #'chunlian--cleanup-current-buffer t)
  (chunlian--clear-display))

;;;###autoload
(define-minor-mode chunlian-mode
  "一个简单的 Minor Mode，用于在 buffer 两侧固定显示春联。"
  :lighter " 🧧"
  (if chunlian-mode
      (progn
        (add-hook 'change-major-mode-hook #'chunlian--cleanup-current-buffer nil t)
        (add-hook 'kill-buffer-hook #'chunlian--cleanup-current-buffer nil t)
        (chunlian--setup-display))
    (chunlian--cleanup-current-buffer)))


;; 卸载旧的，挂载安全的 Hook
(remove-hook 'dashboard-mode-hook #'chunlian-mode)
(add-hook 'buffer-list-update-hook #'my/chunlian-schedule-visibility-check)
(add-hook 'dashboard-after-initialize-hook
          (lambda ()
            (chunlian-mode 1)
            (let ((dashboard-buffer (current-buffer)))
              (run-with-timer
               0.1 nil
               (lambda (buffer)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when chunlian-mode
                       (chunlian--setup-overlays)))))
               dashboard-buffer))))

































(provide 'init-ui)

;;; init-ui.el ends here

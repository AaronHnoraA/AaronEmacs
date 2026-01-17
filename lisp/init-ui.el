;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Use Iosvkem in terminals

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'kanagawa-wave t)


(use-package doom-themes)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((vc-annotate-mode         :select t :inhibit-window-quit t :same t)
                   ("*quickrun*"             :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode     :select t)
                   (xwidget-webkit-mode      :select t :same t)
                   (flycheck-error-list-mode :select t :align t :size 10)
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

;; Windows layout recorder
;;
;; You can still use `winner-mode' on Emacs 26 or early. On Emacs 27, it's
;; preferred over `winner-mode' for better compatibility with `tab-bar-mode'.
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-history-mode)
  :custom
  (tab-bar-history-buttons-show nil))

(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(use-package dashboard
  :ensure t
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-mark_github") "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-heart") "♥")
                                        "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                       (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-alert") "⚑")
                                        "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
                                       (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-download") "♺")
                                        "Upgrade" "Upgrade packages synchronously" (lambda (&rest _) (package-upgrade-all nil)) success))))
  (dashboard-setup-startup-hook)
  :config
  (defconst homepage-url "https://git.pwo101.top/")
  (defconst stars-url (concat homepage-url "/stargazers"))
  (defconst issue-url (concat homepage-url "/issues/new"))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents   . 10)
                     (projects  . 7)))
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-footer)))



;;; Menu Bar:

(keymap-global-unset "<menu-bar> <file> <close-tab>")
(keymap-global-unset "<menu-bar> <file> <delete-this-frame>")
(keymap-global-unset "<menu-bar> <file> <exit-emacs>")
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

(unless (package-installed-p 'all-the-icons)
  (package-vc-install
   '(all-the-icons
     :url "https://github.com/domtronn/all-the-icons.el.git"
     :rev :last-release)))

;;; Text Area:


;; 渲染成对的单引号时, 尽可能使用 ‘curve’ 这种样式, 退而求此次地可以使用 `grave' 这种样式.
(setopt text-quoting-style nil)
;;; Fringe:

(set-fringe-mode '(0 . nil))  ; Right-only.

;; 若开启, buffer 尾行之后的区域的右流苏区域会显示密集的刻度线.
(setopt indicate-empty-lines nil)
(setopt overflow-newline-into-fringe t)

;;; Scroll Bar:

(setopt scroll-bar-mode 'right)

;; 滚动条落至底部 (overscrolling) 时的行为.
(setopt scroll-bar-adjust-thumb-portion nil)

(setq-default scroll-bar-width 28)

;;; Mode Line:

;; Face ‘mode-line-inactive’ for non-selected window’s mode line.
(setopt mode-line-in-non-selected-windows t)

(setopt mode-line-compact nil)  ; 不要设 t, 否则即使有多余的空间, 它也倾向于挤在一起.
(setopt mode-line-right-align-edge 'window)  ; 与 window 的边缘对齐.

(setopt doom-modeline-display-misc-in-all-mode-lines t  ; 没看出有什么区别, 先设 t, 继续观察...
        doom-modeline-minor-modes nil)
(setopt doom-modeline-bar-width 3  ; 左侧 小竖条 (装饰品) 的 宽度.
        ;; 尽可能地窄.
        doom-modeline-height 1
        ;; 即使当前窗口宽度很小, 也尽量显示所有信息.
        doom-modeline-window-width-limit nil)
(doom-modeline-mode)


(size-indication-mode)  ; 在 mode line 上显示 buffer 大小.
(setq mode-line-column-line-number-mode-map ())  ; 使某些可点击文本不作出应答.

;; 当 buffer 对应的文件名相同时, 在 buffer 名字之前补全文件的路径, 使 buffer 的名字互异.
(setopt uniquify-buffer-name-style 'forward
        ;; 当‘uniquify-buffer-name-style’的设置涉及补全文件路径时, 保留显示路径名之间相同的部分.
        uniquify-strip-common-suffix t)

(line-number-mode -1)  ; Mode line 上不要显示行号, 因为 window 左边缘已经显示行号了.
;; 从 1 开始计数.
(setopt mode-line-position-column-format '("C%C")
        doom-modeline-column-zero-based nil)
(column-number-mode)

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
(mouse-avoidance-mode 'animate)

;;; Cursor:

(setopt cursor-type 'box
        ;; 在 non-selected window 中也 展示 cursor,
        ;; 但是 是 镂空的.
        cursor-in-non-selected-windows t)
(setopt x-stretch-cursor t)  ; 在 TAB 字符上拉长 cursor.

(blink-cursor-mode -1)
;; 以下设置无效, 因为‘blink-cursor-mode’关掉了.
(setopt blink-cursor-delay  0  ; Cursor 静止一段时间之后开始闪烁.
        blink-cursor-blinks 0  ; 闪烁次数.
        blink-cursor-interval 0.5
        ;; 映射: ‘cursor-type’ -> 光标黯淡时的造型.
        blink-cursor-alist '((box  . nil)
                             (bar  . box)
                             (hbar . bar)))

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

(pixel-scroll-precision-mode)

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

;;; Tooltip:
(require 'tooltip)

(require 'let-alist)
(let-alist tooltip-frame-parameters
  "让 compiler 闭嘴.")

(setopt tooltip-delay       0
        tooltip-short-delay 0
        tooltip-hide-delay  most-positive-fixnum)

(tooltip-mode)
;;; Dialog Box:

(setopt use-dialog-box t
        use-file-dialog t)

;; 在 GTK+ 的 file-chooser-dialog 中显示隐藏文件.
(setq x-gtk-show-hidden-files t)
;;; Render:

(setopt no-redraw-on-reenter t)


(provide 'init-ui)

;;; init-ui.el ends here

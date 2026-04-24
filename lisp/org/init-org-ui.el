;;; init-org-ui.el --- Org UI and presentation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'aaron-ui)
(require 'init-org-core)
(require 'org-element)

(defvar-local my/org--olivetti-auto-state nil)
(defvar-local my/org-pretty-block-cache nil)
(defvar my/org-ui--face-theme-signature nil)
(defvar my/org--olivetti-sync-timer nil)
(defvar-local my/org--suspended-in-insert nil)
(defvar-local my/org--insert-suspended-modes nil)

(defcustom my/org-pretty-block-cache-max-entries 256
  "Maximum cached pretty-block signatures per Org buffer.
The cache is only an optimization; clearing it is safe and prevents stale
integer position keys from accumulating during long editing sessions."
  :type 'integer
  :group 'my/org-ui)

(declare-function evil-insert-state-p "evil")
(declare-function my/org-toc-insert-or-update "init-org-core")

(defface my/org-hl-line
  '((t :inherit hl-line))
  "Current line face used in Org buffers."
  :group 'my/org-ui)

(defun my/org--unspecified-color-p (value)
  "Return non-nil when VALUE is an unspecified face color."
  (or (null value)
      (eq value 'unspecified)
      (and (stringp value)
           (string-prefix-p "unspecified" value))))

(defun my/org--hex-to-rgb (color)
  "Return COLOR as an RGB float list when it is a hex string, else nil."
  (when (and (stringp color)
             (string-prefix-p "#" color))
    (pcase (length color)
      (4
       (mapcar (lambda (component)
                 (/ (string-to-number (make-string 2 component) 16) 255.0))
               (cdr (string-to-list color))))
      (7
       (list (/ (string-to-number (substring color 1 3) 16) 255.0)
             (/ (string-to-number (substring color 3 5) 16) 255.0)
             (/ (string-to-number (substring color 5 7) 16) 255.0)))
      (13
       (list (/ (string-to-number (substring color 1 5) 16) 65535.0)
             (/ (string-to-number (substring color 5 9) 16) 65535.0)
             (/ (string-to-number (substring color 9 13) 16) 65535.0))))))

(defun my/org--color-to-rgb (color)
  "Return COLOR as an RGB float list."
  (or (my/org--hex-to-rgb color)
      (color-name-to-rgb color)))

(defun my/org-enable-valign-maybe ()
  "Enable `valign-mode' for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (valign-mode 1)))

(defun my/org-enable-org-appear-maybe ()
  "Enable `org-appear-mode' for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (org-appear-mode 1)))

(defun my/org-enable-org-fragtog-maybe ()
  "Enable `org-fragtog-mode' for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (org-fragtog-mode 1)))

(defun my/org--suspend-mode-for-insert (mode)
  "Disable MODE temporarily in the current Org buffer during Evil insert state."
  (when (and (fboundp mode)
             (boundp mode)
             (symbol-value mode))
    (push mode my/org--insert-suspended-modes)
    (funcall mode -1)))

(defun my/org-suspend-expensive-modes-in-insert ()
  "Keep Org editing responsive without tearing down visual rendering.

This intentionally no longer disables `org-modern-mode', `org-indent-mode',
`valign-mode', `org-fragtog-mode' or pretty-block overlays: turning those off
made the buffer lose its rendered appearance in insert state."
  (when (and (derived-mode-p 'org-mode)
             (not my/org--suspended-in-insert)
             (fboundp 'evil-insert-state-p)
             (evil-insert-state-p))
    (setq-local my/org--suspended-in-insert t)
    (setq-local my/org--insert-suspended-modes nil)))

(defun my/org-resume-expensive-modes-after-insert ()
  "Re-enable Org UI helpers suspended during Evil insert state."
  (when (and (derived-mode-p 'org-mode)
             my/org--suspended-in-insert)
    (setq-local my/org--suspended-in-insert nil)
    (prog1 nil
      (dolist (mode (nreverse my/org--insert-suspended-modes))
        (when (fboundp mode)
          (funcall mode 1)))
      (setq-local my/org--insert-suspended-modes nil))))

(defun my/org-setup-evil-insert-performance ()
  "Install buffer-local Evil hooks that keep Org insert mode responsive."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-entry-hook
              #'my/org-suspend-expensive-modes-in-insert nil t)
    (add-hook 'evil-insert-state-exit-hook
              #'my/org-resume-expensive-modes-after-insert nil t)
    (when (and (fboundp 'evil-insert-state-p)
               (evil-insert-state-p))
      (my/org-suspend-expensive-modes-in-insert))))

(defun my/org-setup-polished-document-frame ()
  "Apply small buffer-local polish for reading and note-taking."
  (when (display-graphic-p)
    (setq-local line-spacing (max (or line-spacing 0) 0.18))
    (setq-local cursor-type 'bar)
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1)
    (setq-local hl-line-face 'my/org-hl-line)
    (set-window-buffer (selected-window) (current-buffer))))

;; 3.1 写作专注模式 (自动居中)
(use-package olivetti
  :ensure t
  :diminish
  :bind ("<f8>" . olivetti-mode)
  :init 
  (setq olivetti-body-width 0.618) 
  :config
  (defun xs-toggle-olivetti-for-org ()
    "If current buffer is Org and only one window is visible, enable olivetti."
    (let ((desired (and (derived-mode-p 'org-mode)
                        (eq (length (window-list nil nil nil)) 1))))
      (unless (eq desired my/org--olivetti-auto-state)
        (setq-local my/org--olivetti-auto-state desired)
        (if desired
            (olivetti-mode 1)
          (when (bound-and-true-p olivetti-mode)
            (olivetti-mode 0))))))

  (defun my/org-sync-visible-olivetti-buffers ()
    "Synchronize Olivetti state for visible Org buffers."
    (setq my/org--olivetti-sync-timer nil)
    (dolist (window (window-list nil 'no-minibuf))
      (when-let* ((buffer (window-buffer window)))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-mode)
            (xs-toggle-olivetti-for-org))))))

  (defun my/org-schedule-olivetti-sync (&rest _)
    "Coalesce repeated window changes before syncing Org Olivetti state."
    (unless (timerp my/org--olivetti-sync-timer)
      (setq my/org--olivetti-sync-timer
            (run-with-idle-timer 0.05 nil #'my/org-sync-visible-olivetti-buffers))))
  
  (add-hook 'org-mode-hook #'xs-toggle-olivetti-for-org)
  (add-hook 'window-configuration-change-hook #'my/org-schedule-olivetti-sync))

;; 3.2 表格对齐
(use-package valign
  :ensure t
  :hook (org-mode . my/org-enable-valign-maybe))

;; 3.3 Org Modern (全面增强版)
(use-package org-modern
  :ensure t
  :after org  ; [IMPORTANT] 修复加载顺序，确保在 org 之后加载
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda-mode))
  :custom
  ;; 1. 基础美化
  (org-modern-hide-stars 'leading)
  (org-modern-star 'replace)
  (org-modern-cycle-stars t)
  (org-modern-replace-stars '("◉" "◎" "○" "◌" "◆" "◇" "✦" "·"))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((45 . "•") (43 . "◦") (42 . "∙")))
  
  ;; 2. 标签按钮化 (Tag Buttons)
  (org-modern-tag t) 
  (org-modern-label-border 4)
  
  ;; 3. 统计与进度条美化
  (org-modern-statistics t) 
  (org-modern-progress 14)
  
  ;; 4. 时间戳美化
  (org-modern-timestamp t)
  (org-modern-internal-target '(" ⟦" t "⟧ "))
  (org-modern-radio-target '(" ⟪" t "⟫ "))
  
  ;; 5. 关键词美化 (集成你的图标)
  (org-modern-keyword
   '(("title"        . "◇")
     ("subtitle"     . "◆")
     ("author"       . "✎")
     ("email"        . "@")
     ("date"         . "◷")
     ;; Use the emoji variation sequence to force colorful tag rendering.
     ("filetags"     . "⌗")
     ("language"     . "文")
     ("options"      . "⛭")
     ("startup"      . "✲")
     ("property"     . "☷")
     ("macro"        . "𝓜")
     ("bind"         . "⌘")
     ("setupfile"    . "📝")
     ("downloaded"   . "⇊")
     ("attr_latex"   . "🄛")
     ("attr_html"    . "🄗")
     ("attr_org"     . "🄞")
     ("name"         . "🄝")
     ("caption"      . "🄒")
     ("results"      . "☰")
     ("print_bibliography" . "§")))
  
  ;; 6. 复选框美化
  (org-modern-checkbox
   '((?X . "☑")
     (?- . "◩")
     (?\s . "☐")))

  ;; 7. 其他装饰
  (org-modern-horizontal-rule t)
  (org-modern-block-name
   '(("src" . ("λ" "□"))
     ("example" . ("EX" "□"))
     ("quote" . ("❝" "❞"))
     ("verse" . ("❦" "❦"))
     (t . ("╭" "╰"))))
  (org-modern-todo t)
  (org-modern-priority '((?A . "A")
                         (?B . "B")
                         (?C . "C")
                         (?D . "D")))
  :config
  (setq org-modern-todo-faces
        `(("TODO" :background ,(aaron-ui-color 'accent-red-soft)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight medium)
          ("NEXT" :background ,(aaron-ui-color 'accent-green)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight semibold)
          ("WIP" :background ,(aaron-ui-color 'accent-cyan)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight semibold)
          ("HOLD" :background ,(aaron-ui-color 'accent-orange)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight medium)
          ("WAIT" :background ,(aaron-ui-color 'accent-yellow)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight medium)
          ("DONE" :background ,(aaron-ui-color 'accent-blue)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight medium)
          ("CANCELLED" :background ,(aaron-ui-color 'fg-faint)
           :foreground ,(aaron-ui-color 'bg-base)
           :weight medium)))
  (setq org-modern-tag-faces
        `((t :background ,(aaron-ui-color 'bg-surface-strong)
             :foreground ,(aaron-ui-color 'accent-teal)
             :weight medium)))
  (setq org-modern-priority-faces
        `((?A :background ,(aaron-ui-color 'accent-red)
              :foreground ,(aaron-ui-color 'bg-base)
              :weight semibold)
          (?B :background ,(aaron-ui-color 'accent-orange)
              :foreground ,(aaron-ui-color 'bg-base)
              :weight semibold)
          (?C :background ,(aaron-ui-color 'accent-yellow-soft)
              :foreground ,(aaron-ui-color 'bg-base)
              :weight medium)
          (t :background ,(aaron-ui-color 'bg-surface-strong)
             :foreground ,(aaron-ui-color 'fg-main)))))

(defun my/org-apply-ui ()
  "Apply the local document UI to Org and Org Modern faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/org-ui--face-theme-signature)
        (setq my/org-ui--face-theme-signature signature)
        (let* ((base-bg (face-attribute 'default :background nil t))
               (base-bg (if (my/org--unspecified-color-p base-bg)
                            (aaron-ui-color 'bg-base)
                          base-bg))
               (title-weight (if (boundp 'my/font-title-weight) my/font-title-weight 'medium))
               (strong-weight (if (boundp 'my/font-strong-weight) my/font-strong-weight 'medium))
               (popout-weight (if (boundp 'my/font-popout-weight) my/font-popout-weight 'semibold))
               (mantle (aaron-ui-color 'bg-code))
               (surface0 (aaron-ui-color 'bg-surface))
               (surface1 (aaron-ui-color 'bg-surface-strong))
               (surface2 (aaron-ui-color 'bg-surface-stronger))
               (border-subtle (aaron-ui-color 'border-subtle))
               (fg-strong (aaron-ui-color 'fg-strong))
               (fg-faint (aaron-ui-color 'fg-faint))
               (overlay1 (aaron-ui-color 'fg-overlay))
               (subtext0 (aaron-ui-color 'fg-subtle))
               (subtext1 (aaron-ui-color 'fg-soft))
               (text (aaron-ui-color 'fg-main))
               (rosewater (aaron-ui-color 'accent-rose))
               (red (aaron-ui-color 'accent-red))
               (orange (aaron-ui-color 'accent-orange))
               (yellow (aaron-ui-color 'accent-yellow))
               (blue (aaron-ui-color 'accent-blue))
               (lavender (aaron-ui-color 'accent-lavender))
               (mauve (aaron-ui-color 'accent-mauve))
               (cyan (aaron-ui-color 'accent-cyan))
               (teal (aaron-ui-color 'accent-teal))
               (green (aaron-ui-color 'accent-green))
               (meta-bg (aaron-ui-color 'bg-meta))
               (meta-fg (aaron-ui-color 'accent-green))
               (headline-1-bg (my/org-blend-colors yellow base-bg 0.085))
               (headline-2-bg (my/org-blend-colors blue base-bg 0.075))
               (headline-3-bg (my/org-blend-colors mauve base-bg 0.06))
               (headline-4-bg (my/org-blend-colors teal base-bg 0.05))
               (inline-code-bg (my/org-blend-colors yellow base-bg 0.11))
               (verbatim-bg (my/org-blend-colors blue base-bg 0.1))
               (latex-bg (my/org-blend-colors mauve base-bg 0.075))
               (checkbox-bg (my/org-blend-colors green base-bg 0.12))
               (hl-line-bg (my/org-blend-colors blue base-bg 0.055))
               (block-name-bg (my/org-blend-colors yellow base-bg 0.1))
               (target-bg (my/org-blend-colors lavender base-bg 0.12))
               (done-bg (my/org-blend-colors green base-bg 0.1))
               (property-bg (my/org-blend-colors teal base-bg 0.055))
               (quote-bg (my/org-blend-colors blue base-bg 0.05))
               (verse-bg (my/org-blend-colors mauve base-bg 0.05))
               (table-bg (my/org-blend-colors blue base-bg 0.025))
               (tag-bg (my/org-blend-colors teal base-bg 0.12))
               (priority-bg (my/org-blend-colors rosewater base-bg 0.12))
               (stats-done-bg (my/org-blend-colors green base-bg 0.15))
               (stats-todo-bg (my/org-blend-colors yellow base-bg 0.14))
               (agenda-structure-bg (my/org-blend-colors mauve base-bg 0.12))
               (agenda-today-bg (my/org-blend-colors blue base-bg 0.12)))
          (when (facep 'my/org-hl-line)
            (set-face-attribute 'my/org-hl-line nil
                                :background hl-line-bg
                                :extend t))
          (when (facep 'org-document-title)
            (set-face-attribute 'org-document-title nil
                                :foreground fg-strong
                                :weight popout-weight
                                :height 1.34))
          (when (facep 'org-level-1)
            (set-face-attribute 'org-level-1 nil
                                :foreground yellow
                                :background headline-1-bg
                                :weight popout-weight
                                :height 1.18
                                :extend t
                                :overline `(:color ,(my/org-blend-colors yellow base-bg 0.34)
                                            :style line)))
          (when (facep 'org-level-2)
            (set-face-attribute 'org-level-2 nil
                                :foreground blue
                                :background headline-2-bg
                                :weight popout-weight
                                :height 1.12
                                :extend t))
          (when (facep 'org-level-3)
            (set-face-attribute 'org-level-3 nil
                                :foreground mauve
                                :background headline-3-bg
                                :weight strong-weight
                                :height 1.07
                                :extend t))
          (when (facep 'org-level-4)
            (set-face-attribute 'org-level-4 nil
                                :foreground teal
                                :background headline-4-bg
                                :weight strong-weight
                                :height 1.04
                                :extend t))
          (when (facep 'org-level-5)
            (set-face-attribute 'org-level-5 nil :foreground rosewater :weight title-weight :height 1.01))
          (when (facep 'org-level-6)
            (set-face-attribute 'org-level-6 nil :foreground lavender :weight title-weight))
          (when (facep 'org-level-7)
            (set-face-attribute 'org-level-7 nil :foreground mauve :weight title-weight))
          (when (facep 'org-level-8)
            (set-face-attribute 'org-level-8 nil :foreground teal :weight title-weight))
          (when (facep 'org-document-info)
            (set-face-attribute 'org-document-info nil :foreground subtext1))
          (when (facep 'org-meta-line)
            (set-face-attribute 'org-meta-line nil
                                :background meta-bg
                                :foreground meta-fg
                                :extend t))
          (when (facep 'org-document-info-keyword)
            (set-face-attribute 'org-document-info-keyword nil
                                :background meta-bg
                                :foreground meta-fg))
          (when (facep 'org-property-value)
            (set-face-attribute 'org-property-value nil
                                :background property-bg
                                :foreground subtext1
                                :weight title-weight))
          (when (facep 'org-archived)
            (set-face-attribute 'org-archived nil
                                :foreground fg-faint
                                :slant 'italic))
          (when (facep 'org-block)
            (set-face-attribute 'org-block nil
                                :background mantle
                                :foreground text
                                :extend t
                                :box `(:line-width -1 :color ,border-subtle)))
          (when (facep 'org-code)
            (set-face-attribute 'org-code nil
                                :background inline-code-bg
                                :foreground yellow
                                :weight strong-weight
                                :box `(:line-width 2 :color ,inline-code-bg)))
          (when (facep 'org-block-begin-line)
            (set-face-attribute 'org-block-begin-line nil
                                :background meta-bg
                                :foreground meta-fg
                                :height 0.88
                                :weight title-weight
                                :extend t))
          (when (facep 'org-block-end-line)
            (set-face-attribute 'org-block-end-line nil
                                :background meta-bg
                                :foreground subtext0
                                :height 0.82
                                :extend t))
          (when (facep 'org-verbatim)
            (set-face-attribute 'org-verbatim nil
                                :background verbatim-bg
                                :foreground blue
                                :weight title-weight
                                :box `(:line-width 2 :color ,verbatim-bg)))
          (when (facep 'org-quote)
            (set-face-attribute 'org-quote nil
                                :background quote-bg
                                :foreground text
                                :slant 'italic
                                :extend t))
          (when (facep 'org-verse)
            (set-face-attribute 'org-verse nil
                                :background verse-bg
                                :foreground text
                                :slant 'italic
                                :extend t))
          (when (facep 'org-table)
            (set-face-attribute 'org-table nil
                                :foreground subtext1
                                :background table-bg))
          (when (facep 'org-formula)
            (set-face-attribute 'org-formula nil
                                :foreground mauve
                                :weight title-weight))
          (when (facep 'org-latex-and-related)
            (set-face-attribute 'org-latex-and-related nil
                                :background latex-bg
                                :foreground lavender
                                :weight title-weight
                                :box `(:line-width 2 :color ,latex-bg)))
          (when (facep 'org-special-keyword)
            (set-face-attribute 'org-special-keyword nil
                                :background meta-bg
                                :foreground meta-fg
                                :weight title-weight))
          (when (facep 'org-date)
            (set-face-attribute 'org-date nil
                                :background (my/org-blend-colors blue base-bg 0.105)
                                :foreground blue
                                :weight title-weight
                                :box `(:line-width 2 :color ,(my/org-blend-colors blue base-bg 0.105))))
          (when (facep 'org-link)
            (set-face-attribute 'org-link nil
                                :foreground blue
                                :underline `(:color ,(my/org-blend-colors blue base-bg 0.56)
                                             :style line)))
          (when (facep 'org-footnote)
            (set-face-attribute 'org-footnote nil
                                :foreground lavender
                                :weight title-weight))
          (when (facep 'org-drawer)
            (set-face-attribute 'org-drawer nil
                                :foreground overlay1
                                :weight title-weight))
          (when (facep 'org-ellipsis)
            (set-face-attribute 'org-ellipsis nil
                                :foreground yellow
                                :weight strong-weight
                                :background base-bg))
          (when (facep 'org-list-dt)
            (set-face-attribute 'org-list-dt nil
                                :foreground orange
                                :background (my/org-blend-colors orange base-bg 0.08)
                                :weight strong-weight
                                :box `(:line-width 2 :color ,(my/org-blend-colors orange base-bg 0.08))))
          (when (facep 'org-checkbox)
            (set-face-attribute 'org-checkbox nil
                                :background checkbox-bg
                                :foreground green
                                :weight strong-weight
                                :box `(:line-width 2 :color ,checkbox-bg)))
          (when (facep 'org-indent)
            (set-face-attribute 'org-indent nil :foreground base-bg :background base-bg))
          (when (facep 'org-hide)
            (set-face-attribute 'org-hide nil :foreground base-bg :background base-bg))
          (when (facep 'org-tag)
            (set-face-attribute 'org-tag nil
                                :background tag-bg
                                :foreground teal
                                :weight title-weight))
          (when (facep 'org-todo)
            (set-face-attribute 'org-todo nil
                                :foreground red
                                :weight strong-weight))
          (when (facep 'org-done)
            (set-face-attribute 'org-done nil
                                :background done-bg
                                :foreground green
                                :weight title-weight
                                :box `(:line-width 2 :color ,done-bg)))
          (when (facep 'org-priority)
            (set-face-attribute 'org-priority nil
                                :background priority-bg
                                :foreground rosewater
                                :weight strong-weight))
          (when (facep 'org-checkbox-statistics-done)
            (set-face-attribute 'org-checkbox-statistics-done nil
                                :background stats-done-bg
                                :foreground green
                                :weight strong-weight))
          (when (facep 'org-checkbox-statistics-todo)
            (set-face-attribute 'org-checkbox-statistics-todo nil
                                :background stats-todo-bg
                                :foreground yellow
                                :weight strong-weight))
          (when (facep 'org-headline-done)
            (set-face-attribute 'org-headline-done nil
                                :foreground subtext1
                                :strike-through t))
          (when (facep 'org-warning)
            (set-face-attribute 'org-warning nil
                                :foreground red
                                :weight strong-weight))
          (when (facep 'org-modern-symbol)
            (set-face-attribute 'org-modern-symbol nil
                                :foreground fg-faint
                                :weight title-weight))
          (when (facep 'org-modern-label)
            (set-face-attribute 'org-modern-label nil
                                :background surface1
                                :foreground text
                                :box `(:line-width 4 :color ,surface1)))
          (when (facep 'org-modern-keyword)
            (set-face-attribute 'org-modern-keyword nil
                                :background meta-bg
                                :foreground meta-fg
                                :box nil))
          (when (facep 'org-modern-tag)
            (set-face-attribute 'org-modern-tag nil
                                :background surface1
                                :foreground text
                                :box `(:line-width 3 :color ,surface1)))
          (when (facep 'org-modern-date-active)
            (set-face-attribute 'org-modern-date-active nil
                                :background surface0
                                :foreground blue
                                :box `(:line-width 3 :color ,surface0)))
          (when (facep 'org-modern-date-inactive)
            (set-face-attribute 'org-modern-date-inactive nil
                                :background mantle
                                :foreground subtext0
                                :box `(:line-width 3 :color ,mantle)))
          (when (facep 'org-modern-block-name)
            (set-face-attribute 'org-modern-block-name nil
                                :background block-name-bg
                                :foreground yellow
                                :height 0.9
                                :weight strong-weight
                                :box `(:line-width 2 :color ,block-name-bg)))
          (when (facep 'org-modern-todo)
            (set-face-attribute 'org-modern-todo nil
                                :inherit 'org-modern-label
                                :weight strong-weight
                                :inverse-video nil))
          (when (facep 'org-modern-done)
            (set-face-attribute 'org-modern-done nil
                                :background done-bg
                                :foreground green
                                :weight title-weight
                                :box `(:line-width 3 :color ,done-bg)))
          (when (facep 'org-modern-time-active)
            (set-face-attribute 'org-modern-time-active nil
                                :background (my/org-blend-colors blue base-bg 0.18)
                                :foreground fg-strong
                                :weight strong-weight
                                :box `(:line-width 3 :color ,(my/org-blend-colors blue base-bg 0.18))))
          (when (facep 'org-modern-time-inactive)
            (set-face-attribute 'org-modern-time-inactive nil
                                :background (my/org-blend-colors overlay1 base-bg 0.11)
                                :foreground subtext0
                                :box `(:line-width 3 :color ,(my/org-blend-colors overlay1 base-bg 0.11))))
          (when (facep 'org-modern-internal-target)
            (set-face-attribute 'org-modern-internal-target nil
                                :background target-bg
                                :foreground lavender
                                :weight title-weight
                                :box `(:line-width 3 :color ,target-bg)))
          (when (facep 'org-modern-radio-target)
            (set-face-attribute 'org-modern-radio-target nil
                                :background target-bg
                                :foreground mauve
                                :weight title-weight
                                :box `(:line-width 3 :color ,target-bg)))
          (when (facep 'org-modern-horizontal-rule)
            (set-face-attribute 'org-modern-horizontal-rule nil
                                :foreground (my/org-blend-colors cyan base-bg 0.42)))
          (when (facep 'org-modern-priority)
            (set-face-attribute 'org-modern-priority nil
                                :background priority-bg
                                :foreground rosewater
                                :weight strong-weight))
          (when (facep 'org-modern-progress-complete)
            (set-face-attribute 'org-modern-progress-complete nil
                                :foreground green
                                :background stats-done-bg))
          (when (facep 'org-modern-progress-incomplete)
            (set-face-attribute 'org-modern-progress-incomplete nil
                                :foreground surface2
                                :background surface0))
          (when (facep 'org-agenda-structure)
            (set-face-attribute 'org-agenda-structure nil
                                :background agenda-structure-bg
                                :foreground mauve
                                :weight popout-weight))
          (when (facep 'org-agenda-date)
            (set-face-attribute 'org-agenda-date nil
                                :foreground blue
                                :weight title-weight))
          (when (facep 'org-agenda-date-today)
            (set-face-attribute 'org-agenda-date-today nil
                                :background agenda-today-bg
                                :foreground blue
                                :weight popout-weight))
          (when (facep 'org-agenda-date-weekend)
            (set-face-attribute 'org-agenda-date-weekend nil
                                :foreground lavender))
          (when (facep 'org-agenda-done)
            (set-face-attribute 'org-agenda-done nil
                                :foreground subtext1
                                :strike-through t))
          (when (facep 'org-scheduled)
            (set-face-attribute 'org-scheduled nil
                                :foreground teal))
          (when (facep 'org-scheduled-today)
            (set-face-attribute 'org-scheduled-today nil
                                :foreground green
                                :weight strong-weight))
          (when (facep 'org-upcoming-deadline)
            (set-face-attribute 'org-upcoming-deadline nil
                                :foreground yellow
                                :weight title-weight)))))))

(add-hook 'org-mode-hook #'my/org-apply-ui)
(add-hook 'after-load-theme-hook #'my/org-apply-ui)
(add-hook 'org-mode-hook #'my/org-setup-polished-document-frame)
(add-hook 'org-mode-hook #'my/org-setup-evil-insert-performance)

;; 3.4 自动显示强调符
(my/package-ensure-vc 'org-appear "https://github.com/awth13/org-appear.git")

(use-package org-appear
  :after org
  :hook (org-mode . my/org-enable-org-appear-maybe)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  ;; Keep Org's pretty entities/compositions stable while editing formulas.
  ;; Let the LaTeX preview pipeline handle formula rendering instead.
  (org-appear-inside-latex nil)
  ;; 光标进入 `^{}' / `_{}' 时显示标记，避免编辑时只看到渲染结果。
  (org-appear-autosubmarkers t)
  (org-appear-delay 0.08))

;; 3.5 优先级美化
(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; ===========================================================
;; 1. 样式配置 (可自定义颜色和标签)
;; ===========================================================
(defvar my/org-special-block-styles
  `(("overview"   . (:label "概览" :emoji "☷" :footer "☷" :color ,(aaron-ui-color 'accent-blue)))
    ("toc"        . (:label "目录" :emoji "☷" :footer "☷" :color ,(aaron-ui-color 'accent-blue)))
    ("definition" . (:label "定义" :emoji "◆" :footer "◇" :color ,(aaron-ui-color 'accent-yellow)))
    ("defn"       . (:label "定义" :emoji "◆" :footer "◇" :color ,(aaron-ui-color 'accent-yellow)))
    ("theorem"    . (:label "定理" :emoji "∎" :footer "◈" :color ,(aaron-ui-color 'accent-green)))
    ("thm"        . (:label "定理" :emoji "∎" :footer "◈" :color ,(aaron-ui-color 'accent-green)))
    ("lemma"      . (:label "引理" :emoji "◇" :footer "◌" :color ,(aaron-ui-color 'accent-blue)))
    ("corollary"  . (:label "推论" :emoji "⇒" :footer "✦" :color ,(aaron-ui-color 'accent-lavender)))
    ("cor"        . (:label "推论" :emoji "⇒" :footer "✦" :color ,(aaron-ui-color 'accent-lavender)))
    ("proposition" . (:label "命题" :emoji "◉" :footer "◆" :color ,(aaron-ui-color 'accent-rose)))
    ("prop"       . (:label "命题" :emoji "◉" :footer "◆" :color ,(aaron-ui-color 'accent-rose)))
    ("property"   . (:label "性质" :emoji "◍" :footer "◍" :color ,(aaron-ui-color 'accent-lavender)))
    ("proof"      . (:label "证明" :emoji "∵" :footer "■" :footer-style square :color ,(aaron-ui-color 'accent-cyan)))
    ("solution"   . (:label "解答" :emoji "∴" :footer "□" :footer-style square :color ,(aaron-ui-color 'accent-cyan)))
    ("answer"     . (:label "答案" :emoji "✓" :footer "✓" :color ,(aaron-ui-color 'accent-green)))
    ("exercise"   . (:label "练习" :emoji "✦" :footer "✦" :color ,(aaron-ui-color 'accent-orange)))
    ("problem"    . (:label "题目" :emoji "?" :footer "?" :color ,(aaron-ui-color 'accent-orange)))
    ("question"   . (:label "问题" :emoji "?" :footer "?" :color ,(aaron-ui-color 'accent-yellow-soft)))
    ("example"    . (:label "例子" :emoji "◎" :footer "◦" :color ,(aaron-ui-color 'accent-yellow-soft)))
    ("attention"  . (:label "注意" :emoji "!" :footer "!" :color ,(aaron-ui-color 'accent-red)))
    ("important"  . (:label "重点" :emoji "!" :footer "!" :color ,(aaron-ui-color 'accent-red-strong)))
    ("tip"        . (:label "提示" :emoji "✧" :footer "✧" :color ,(aaron-ui-color 'accent-teal)))
    ("summary"    . (:label "小结" :emoji "☰" :footer "☰" :color ,(aaron-ui-color 'accent-blue)))
    ("note"       . (:label "笔记" :emoji "✎" :footer "✎" :color ,(aaron-ui-color 'accent-teal)))
    ("info"       . (:label "信息" :emoji "i" :footer "⊙" :color ,(aaron-ui-color 'accent-teal)))
    ("warning"    . (:label "警告" :emoji "△" :footer "▲" :color ,(aaron-ui-color 'accent-red)))))

(defun my/org-special-block-config (type)
  "Return the configured style plist for special block TYPE."
  (cdr (assoc (downcase (or type "")) my/org-special-block-styles)))

(defface my/org-block-title-face
  '((t :weight regular :height 1.05 :inherit default))
  "Block 标题的字体样式。"
  :group 'my/org-ui)

;; ===========================================================
;; 2. 颜色计算辅助函数
;; ===========================================================
(defun my/org-blend-colors (color1 color2 alpha)
  "混合颜色: color1(前景) * alpha + color2(背景) * (1-alpha)。"
  (condition-case nil
      (let ((c1 (my/org--color-to-rgb color1))
            (c2 (my/org--color-to-rgb color2)))
        (apply #'color-rgb-to-hex
               (append
                (cl-mapcar (lambda (x y) (+ (* x alpha) (* y (- 1.0 alpha))))
                           c1 c2)
                '(2))))
    (error color1)))

(defun my/org-default-background ()
  "Return the resolved default background color used by Org block UI."
  (let ((background (face-attribute 'default :background nil t)))
    (if (my/org--unspecified-color-p background)
        (aaron-ui-color 'bg-base)
      background)))

(defun my/org-special-block-palette (type &optional default-bg)
  "Return the UI palette for special block TYPE.
DEFAULT-BG defaults to `my/org-default-background'."
  (when-let* ((config (my/org-special-block-config type))
              (base-color (plist-get config :color))
              (background (or default-bg (my/org-default-background)))
              (label (plist-get config :label))
              (emoji (plist-get config :emoji))
              (footer (or (plist-get config :footer) emoji)))
    (list :label label
          :emoji emoji
          :title (concat label (when emoji (concat " " emoji)))
          :footer footer
          :footer-style (plist-get config :footer-style)
          :base-color base-color
          :type (downcase (or type ""))
          :header-bg (my/org-blend-colors base-color background 0.18)
          :body-bg (my/org-blend-colors base-color background 0.075)
          :badge-bg (my/org-blend-colors base-color background 0.24)
          :divider-color (my/org-blend-colors base-color background 0.54)
          :gutter-color (my/org-blend-colors base-color background 0.46)
          :footer-color (my/org-blend-colors base-color background 0.72)
          :params-color (my/org-blend-colors (aaron-ui-color 'fg-soft) background 0.78))))

(defun my/org-special-block-at-point (&optional pos)
  "Return the styled Org special block containing POS, or nil."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((element (org-element-context))
          found)
      (while (and element (not found))
        (when (and (eq (org-element-type element) 'special-block)
                   (my/org-special-block-palette
                    (org-element-property :type element)))
          (setq found element))
        (setq element (org-element-property :parent element)))
      found)))

(defun my/org-special-block-background-at-point (&optional pos)
  "Return the body background color for the styled special block at POS."
  (when-let* ((block (my/org-special-block-at-point pos))
              (type (org-element-property :type block))
              (palette (my/org-special-block-palette type)))
    (plist-get palette :body-bg)))

(defun my/org-line-background-end (&optional pos)
  "Return the position that lets a face extend through POS's full line."
  (save-excursion
    (when pos
      (goto-char pos))
    (min (point-max) (line-beginning-position 2))))

(defun my/org-delete-pretty-block-overlays (begin end block-id)
  "Delete pretty-block overlays between BEGIN and END matching BLOCK-ID."
  (dolist (ov (overlays-in begin end))
    (when (and (overlay-get ov 'my/org-pretty-block)
               (equal (overlay-get ov 'my/org-pretty-block-id) block-id))
      (delete-overlay ov))))

(defun my/org-special-block-footer-marker-display (palette header-bg)
  "Return the styled footer marker string for a styled special block PALETTE."
  (when-let* ((footer-marker (plist-get palette :footer))
              (footer-color (plist-get palette :footer-color)))
    (pcase (plist-get palette :footer-style)
      ('square
       (propertize footer-marker
                   'face `(:background ,header-bg
                           :foreground ,footer-color
                           :height 0.92
                           :weight semibold)
                   'display '(raise -0.05)))
      (_
       (propertize footer-marker
                   'face `(:background ,header-bg
                           :foreground ,footer-color
                           :weight medium))))))

(defun my/org-special-block-params-display (params palette header-bg)
  "Return a polished parameter badge string for special block PARAMS."
  (when (and (stringp params)
             (string-match-p "\\S-" params))
    (let ((badge-bg (plist-get palette :badge-bg))
          (divider-color (plist-get palette :divider-color))
          (params-color (plist-get palette :params-color)))
      (concat
       (propertize "  ·  "
                   'face `(:background ,header-bg
                           :foreground ,divider-color))
       (propertize (format " %s " params)
                   'face `(:inherit my/org-block-title-face
                           :background ,badge-bg
                           :foreground ,params-color
                           :height 0.94))))))

(defun my/org-special-block-guide-display (bar-color background
                                                     &optional bar-width gap-width)
  "Return a slim guide strip using BAR-COLOR on BACKGROUND."
  (concat
   (propertize " "
               'display `(space :width ,(or bar-width 0.22))
               'face `(:background ,bar-color))
   (propertize " "
               'display `(space :width ,(or gap-width 0.72))
               'face `(:background ,background))))

(defun my/org-special-block-inner-padding (background &optional width)
  "Return horizontal inner padding on BACKGROUND."
  (propertize " "
              'display `(space :width ,(or width 1.05))
              'face `(:background ,background)))

(defun my/org-special-block-prefix-width (&optional pos)
  "Return the existing visual indent width at POS."
  ;; Read text properties only so overlay redraws do not feed back into the
  ;; next width computation and cause visible jitter.
  (let ((prefix (or (get-text-property (or pos (point)) 'line-prefix)
                    (get-text-property (or pos (point)) 'wrap-prefix))))
    (if (stringp prefix)
        (string-width prefix)
      0)))

(defun my/org-special-block-fixed-prefix (prefix-width background tail)
  "Return a prefix of PREFIX-WIDTH on BACKGROUND ending with TAIL."
  (when (> prefix-width 0)
    (let* ((tail-width (if (stringp tail) (string-width tail) 0))
           (padding-width (max 0 (- prefix-width tail-width))))
      (concat
       (when (> padding-width 0)
         (propertize " "
                     'display `(space :width ,padding-width)
                     'face `(:background ,background)))
       tail))))

(defun my/org-special-block-header-prefix (palette header-bg prefix-width)
  "Return the header prefix string for a styled special block."
  (my/org-special-block-fixed-prefix
   prefix-width header-bg
   (propertize "╭"
               'face `(:background ,header-bg
                       :foreground ,(plist-get palette :base-color)
                       :weight medium))))

(defun my/org-special-block-body-line-prefix (palette body-bg prefix-width)
  "Return the body guide prefix string for a styled special block."
  (let* ((bar (propertize " "
                          'display '(space :width 0.2)
                          'face `(:background ,(plist-get palette :gutter-color))))
         (gap (propertize " "
                          'display '(space :width 0.55)
                          'face `(:background ,body-bg)))
         (inner (my/org-special-block-inner-padding body-bg 1.0))
         (tail-width (+ (string-width bar)
                        (string-width gap)
                        (string-width inner)))
         (padding-width (max 0 (- prefix-width tail-width))))
    (concat
     bar
     (when (> padding-width 0)
       (propertize " "
                   'display `(space :width ,padding-width)
                   'face `(:background ,body-bg)))
     gap
     inner)))

(defun my/org-special-block-footer-prefix (palette header-bg prefix-width)
  "Return the footer prefix string for a styled special block."
  (my/org-special-block-fixed-prefix
   prefix-width header-bg
   (propertize "╰"
               'face `(:background ,header-bg
                       :foreground ,(plist-get palette :footer-color)
                       :weight medium))))

(defun my/org-special-block-refresh-toc-button (header-bg base-color)
  "Return a clickable refresh button for managed TOC blocks."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'my/org-special-block-refresh-toc-at-point)
    (define-key map (kbd "RET") #'my/org-special-block-refresh-toc-at-point)
    (propertize "  🍵"
                'face `(:background ,header-bg
                        :foreground ,base-color
                        :weight medium)
                'mouse-face `(:background ,header-bg
                              :foreground ,base-color
                              :weight bold)
                'help-echo "刷新当前 Org TOC"
                'keymap map)))

(defun my/org-special-block-refresh-toc-at-point (&optional event)
  "Refresh the managed TOC in the Org buffer clicked by EVENT."
  (interactive "e")
  (let ((buffer (cond
                 ((and event (eventp event))
                  (window-buffer (posn-window (event-start event))))
                 (t
                  (current-buffer)))))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (my/org-toc-insert-or-update))
        (when (bound-and-true-p my/org-pretty-block-cache)
          (clrhash my/org-pretty-block-cache))
        (jit-lock-refontify)
        (message "TOC refreshed")))))

(defun my/org-special-block-toc-p (palette)
  "Return non-nil when PALETTE describes a managed TOC block."
  (let ((type (plist-get palette :type))
        (params (or (plist-get palette :params) "")))
    (or (string= type "toc")
        (and (string= type "overview")
             (string-match-p "\\(?:^\\|[[:space:]]\\):toc\\(?:[[:space:]]\\|$\\)"
                             params)))))

(defun my/org-special-block-header-display (palette header-bg)
  "Return the rendered header string for a styled special block PALETTE."
  (let* ((base-color (plist-get palette :base-color))
         (title (plist-get palette :title))
         (params (plist-get palette :params))
         (params-display (my/org-special-block-params-display params palette header-bg))
         (refresh-button (when (my/org-special-block-toc-p palette)
                           (my/org-special-block-refresh-toc-button
                            header-bg base-color))))
    (concat
     (propertize "╭"
                 'face `(:background ,header-bg
                         :foreground ,base-color
                         :weight medium))
     (my/org-special-block-inner-padding header-bg 0.92)
     (propertize title
                 'face `(:inherit my/org-block-title-face
                         :background ,header-bg
                         :foreground ,base-color
                         :weight medium))
     params-display
     refresh-button)))

(defun my/org-special-block-line-range (element)
  "Return ELEMENT's full visible line range as (BEGIN . END)."
  (let ((begin (save-excursion
                 (goto-char (org-element-property :post-affiliated element))
                 (line-beginning-position)))
        (end (save-excursion
               (goto-char (org-element-property :end element))
               (skip-chars-backward " \t\n")
               (my/org-line-background-end))))
    (cons begin end)))

(defun my/org-special-block-body-line-range (contents-begin contents-end)
  "Return the visible body line range from CONTENTS-BEGIN to CONTENTS-END."
  (when (and contents-begin contents-end (> contents-end contents-begin))
    (cons (save-excursion
            (goto-char contents-begin)
            (line-beginning-position))
          (save-excursion
            (goto-char contents-end)
            (skip-chars-backward " \t\n")
            (my/org-line-background-end)))))

(defun my/org-special-block-body-segments (element contents-begin contents-end)
  "Return body segments for ELEMENT excluding nested styled special blocks."
  (when-let* ((body-range (my/org-special-block-body-line-range
                           contents-begin contents-end))
              (body-begin (car body-range))
              (body-end (cdr body-range)))
    (let (ranges segments
                (cursor body-begin))
      (dolist (child (org-element-map element 'special-block #'identity))
        (when (and (not (eq child element))
                   (my/org-special-block-palette
                    (org-element-property :type child)))
          (pcase-let ((`(,child-begin . ,child-end)
                       (my/org-special-block-line-range child)))
            (push (cons (max body-begin child-begin)
                        (min body-end child-end))
                  ranges))))
      (setq ranges
            (sort ranges (lambda (left right) (< (car left) (car right)))))
      (let (merged)
        (dolist (range ranges)
          (pcase-let ((`(,range-begin . ,range-end) range))
            (when (< range-begin range-end)
              (if (and merged (<= range-begin (cdar merged)))
                  (setcdr (car merged) (max range-end (cdar merged)))
                (push (cons range-begin range-end) merged)))))
        (setq ranges (nreverse merged)))
      (dolist (range ranges)
        (pcase-let ((`(,range-begin . ,range-end) range))
          (when (< cursor range-begin)
            (push (cons cursor range-begin) segments))
          (setq cursor (max cursor range-end))))
      (when (< cursor body-end)
        (push (cons cursor body-end) segments))
      (nreverse segments))))

(defun my/org-special-block-footer-display (palette header-bg)
  "Return the rendered footer string for a styled special block PALETTE."
  (let* ((footer-color (plist-get palette :footer-color))
         (footer-marker (plist-get palette :footer))
         (footer-marker-display
          (my/org-special-block-footer-marker-display palette header-bg))
         ;; Start the footer rule directly after the corner so the bottom
         ;; border lines up with the frame without shifting the body guide.
         (rule (propertize "────────────────────────"
                           'face `(:background ,header-bg
                                   :foreground ,footer-color
                                   :height 0.8))))
    (if (and (stringp footer-marker)
             (> (length footer-marker) 0))
        (concat
         (propertize "╰"
                     'face `(:background ,header-bg
                             :foreground ,footer-color
                             :weight medium))
         rule
         (propertize " "
                     'face `(:background ,header-bg)
                     'display `(space :align-to (- right ,(+ 1 (string-width footer-marker)))))
         footer-marker-display)
      (concat
       (propertize "╰"
                   'face `(:background ,header-bg
                           :foreground ,footer-color
                           :weight medium))
       rule))))

;; ===========================================================
;; 3. 核心渲染逻辑：只处理单个 Element
;; ===========================================================
(defun my/org-prettify-element (element)
  "渲染单个 Org Element 节点，应用 Overlay 样式。"
  (let* ((type (downcase (or (org-element-property :type element) "")))
         (config (my/org-special-block-config type)))
    
    (when config
      (let* ((begin-pos (org-element-property :begin element))
             (end-pos (org-element-property :end element))
             (contents-begin (org-element-property :contents-begin element))
             (contents-end (org-element-property :contents-end element))
             (post-affiliated (org-element-property :post-affiliated element))
             
             ;; 1. 计算深度 (用于处理嵌套 Block 的堆叠顺序)
             (lineage (org-element-lineage element '(special-block)))
             (depth (length lineage))
             ;; 深度越深，priority 越高，确保内层盖在外层上
             (priority (+ 50 (* 10 depth))) 
             
             ;; 2. 颜色准备
             (default-bg (my/org-default-background))
             (palette (my/org-special-block-palette type default-bg))
             (base-color (plist-get palette :base-color))
             (title (plist-get palette :title))
             (footer-marker (plist-get palette :footer))
             (footer-style (plist-get palette :footer-style))
             (params (org-element-property :parameters element))
             (badge-bg (plist-get palette :badge-bg))
             (divider-color (plist-get palette :divider-color))
             (gutter-color (plist-get palette :gutter-color))
             (header-text-beg (save-excursion
                                (goto-char post-affiliated)
                                (back-to-indentation)
                                (point)))
             (header-bg (plist-get palette :header-bg))
             (body-bg (plist-get palette :body-bg))
             (footer-color (plist-get palette :footer-color))
             (params-color (plist-get palette :params-color))
             (body-prefix-width
              (max (save-excursion
                     (goto-char (or contents-begin post-affiliated))
                     (my/org-special-block-prefix-width))
                   (save-excursion
                     (goto-char (or header-text-beg post-affiliated))
                     (my/org-special-block-prefix-width))))
             (body-prefix (my/org-special-block-body-line-prefix
                           palette body-bg body-prefix-width))
             (header-display (my/org-special-block-header-display
                              (plist-put (copy-sequence palette) :params params)
                              header-bg))
             (body-segments (my/org-special-block-body-segments
                             element contents-begin contents-end))
             (footer-display (my/org-special-block-footer-display palette header-bg))
             (signature (list type begin-pos end-pos contents-begin contents-end
                              post-affiliated params default-bg title
                              footer-marker footer-style base-color header-bg body-bg
                              footer-color params-color badge-bg
                              divider-color gutter-color body-prefix-width
                              body-prefix
                              body-segments header-display footer-display))
             (cached-signature (and (hash-table-p my/org-pretty-block-cache)
                                    (gethash begin-pos my/org-pretty-block-cache)))
             (already-rendered
              (cl-some
               (lambda (ov)
                 (overlay-get ov 'my/org-pretty-block))
               (overlays-in begin-pos (min end-pos (1+ begin-pos))))))

        (unless (and already-rendered
                     (equal signature cached-signature))
          (unless (hash-table-p my/org-pretty-block-cache)
            (setq my/org-pretty-block-cache (make-hash-table :test #'equal)))
          (when (and (integerp my/org-pretty-block-cache-max-entries)
                     (> my/org-pretty-block-cache-max-entries 0)
                     (> (hash-table-count my/org-pretty-block-cache)
                        my/org-pretty-block-cache-max-entries))
            (clrhash my/org-pretty-block-cache))
          (puthash begin-pos signature my/org-pretty-block-cache)

        ;; 3. 清理区域
        (my/org-delete-pretty-block-overlays begin-pos end-pos begin-pos)

        ;; -------------------------------------------------------
        ;; A. Header Overlay (#+begin_xxx)
        ;; -------------------------------------------------------
        (let* ((header-bol (save-excursion
                             (goto-char post-affiliated)
                             (line-beginning-position)))
               (header-end (save-excursion
                             (goto-char post-affiliated)
                             (line-end-position)))
               (header-bg-end (my/org-line-background-end post-affiliated)))
          (let ((ov (make-overlay header-bol header-bg-end)))
            (overlay-put ov 'my/org-pretty-block t)
            (overlay-put ov 'my/org-pretty-block-id begin-pos)
            (overlay-put ov 'face `(:background ,header-bg :extend t))
            (overlay-put ov 'line-prefix "")
            (overlay-put ov 'wrap-prefix "")
            (overlay-put ov 'priority priority)
            (overlay-put ov 'evaporate t))
          ;; Do not replace leading indentation characters: Org indentation
          ;; helpers use display properties there for visual indentation.
          (let ((ov (make-overlay header-text-beg header-end)))
            (overlay-put ov 'my/org-pretty-block t)
            (overlay-put ov 'my/org-pretty-block-id begin-pos)
            (overlay-put ov 'face `(:background ,header-bg :extend t))
            (overlay-put ov 'priority (1+ priority))
            (overlay-put ov 'display header-display)
            (overlay-put ov 'line-prefix "")
            (overlay-put ov 'wrap-prefix "")
            (overlay-put ov 'evaporate t)))

        ;; -------------------------------------------------------
        ;; B. Body Overlay (内容区域)
        ;; -------------------------------------------------------
        (dolist (segment body-segments)
          (pcase-let ((`(,segment-begin . ,segment-end) segment))
            (when (< segment-begin segment-end)
              (let ((ov (make-overlay segment-begin segment-end)))
              (overlay-put ov 'my/org-pretty-block t)
              (overlay-put ov 'my/org-pretty-block-id begin-pos)
              (overlay-put ov 'face `(:background ,body-bg :extend t))
              (overlay-put ov 'line-prefix body-prefix)
              (overlay-put ov 'wrap-prefix body-prefix)
              (overlay-put ov 'priority priority)
              (overlay-put ov 'evaporate t)))))

        ;; -------------------------------------------------------
        ;; C. Footer Overlay (#+end_xxx)
        ;; -------------------------------------------------------
        (save-excursion
          (goto-char end-pos)
          (skip-chars-backward " \t\n")
          (beginning-of-line)
          ;; 确保是对应的 end 标签
          (when (looking-at (format "^[ \t]*#\\+end_%s" (regexp-quote type)))
            (let* ((footer-bol (line-beginning-position))
                   (footer-text-beg (save-excursion
                                      (back-to-indentation)
                                      (point)))
                   (footer-end (line-end-position))
                   (footer-bg-end (my/org-line-background-end)))
              (let ((ov (make-overlay footer-bol footer-bg-end)))
                (overlay-put ov 'my/org-pretty-block t)
                (overlay-put ov 'my/org-pretty-block-id begin-pos)
                (overlay-put ov 'face `(:background ,header-bg :extend t))
                (overlay-put ov 'line-prefix "")
                (overlay-put ov 'wrap-prefix "")
                (overlay-put ov 'priority priority)
                (overlay-put ov 'evaporate t))
              (let ((ov (make-overlay footer-text-beg footer-end)))
                (overlay-put ov 'my/org-pretty-block t)
                (overlay-put ov 'my/org-pretty-block-id begin-pos)
                (overlay-put ov 'face `(:background ,header-bg :extend t))
                (overlay-put ov 'priority (1+ priority))
                (overlay-put ov 'display footer-display)
                (overlay-put ov 'line-prefix "")
                (overlay-put ov 'wrap-prefix "")
                (overlay-put ov 'evaporate t))))))))))

;; ===========================================================
;; 4. JIT-Lock 引擎：连续扫描 (支持嵌套)
;; ===========================================================
(defun my/org-jit-prettify-blocks (start end)
  "JIT-Lock callback: render styled special blocks around START and END."
  (unless (and (fboundp 'evil-insert-state-p)
               (evil-insert-state-p))
    (save-excursion
      (save-match-data
        (let (scan-start scan-end)
          (goto-char start)
          (setq scan-start
                (if (re-search-backward "^[ \t]*#\\+begin_" nil t)
                    (line-beginning-position)
                  start))
          (goto-char end)
          (setq scan-end
                (if (re-search-forward "^[ \t]*#\\+end_" nil t)
                    (min (point-max) (line-beginning-position 2))
                  end))
          (when (save-excursion
                  (goto-char scan-start)
                  (re-search-forward "^[ \t]*#\\+begin_" scan-end t))
            (save-restriction
              (narrow-to-region scan-start scan-end)
              (dolist (el (org-element-map (org-element-parse-buffer) 'special-block
                            (lambda (el)
                              (when (and (my/org-special-block-palette
                                          (org-element-property :type el))
                                         (< (org-element-property :begin el) end)
                                         (> (org-element-property :end el) start))
                                el))))
                (my/org-prettify-element el)))))))))

;; ===========================================================
;; 5. 激活机制
;; ===========================================================

(defun my/org-enable-jit-pretty-blocks ()
  "在当前 Buffer 启用 JIT 渲染机制。"
  (when (and (derived-mode-p 'org-mode)
             (my/org-rich-ui-buffer-p)
             (<= (buffer-size) my/org-pretty-block-max-buffer-size))
    (unless (hash-table-p my/org-pretty-block-cache)
      (setq-local my/org-pretty-block-cache (make-hash-table :test #'equal)))
    (add-hook 'change-major-mode-hook #'my/org-disable-jit-pretty-blocks nil t)
    (add-hook 'kill-buffer-hook #'my/org-disable-jit-pretty-blocks nil t)
    (unless (memq #'my/org-jit-prettify-blocks jit-lock-functions)
      (jit-lock-register #'my/org-jit-prettify-blocks t))
    (jit-lock-refontify)))

(defun my/org-clear-pretty-block-state ()
  "Clear Org pretty-block overlays and cache in the current buffer."
  (remove-overlays (point-min) (point-max) 'my/org-pretty-block t)
  (when (hash-table-p my/org-pretty-block-cache)
    (clrhash my/org-pretty-block-cache)))

(defun my/org-disable-jit-pretty-blocks ()
  "Disable Org pretty-block JIT state in the current buffer."
  (when (and (boundp 'jit-lock-functions)
             (memq #'my/org-jit-prettify-blocks jit-lock-functions))
    (jit-lock-unregister #'my/org-jit-prettify-blocks))
  (remove-hook 'change-major-mode-hook #'my/org-disable-jit-pretty-blocks t)
  (remove-hook 'kill-buffer-hook #'my/org-disable-jit-pretty-blocks t)
  (my/org-clear-pretty-block-state)
  (setq-local my/org-pretty-block-cache nil))

(defun my/org-reset-overlays ()
  "调试用：强制清除所有 Overlay 并重绘。"
  (interactive)
  (my/org-clear-pretty-block-state)
  (jit-lock-refontify))

(add-hook 'org-mode-hook #'my/org-enable-jit-pretty-blocks)

(provide 'init-org-ui)
;;; init-org-ui.el ends here

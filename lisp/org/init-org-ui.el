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

(declare-function evil-insert-state-p "evil")

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

(defun my/org-enable-org-modern-indent ()
  "Enable `org-modern-indent-mode' for every Org buffer."
  (when (derived-mode-p 'org-mode)
    (org-indent-mode 1)
    (org-modern-indent-mode 1)))

(defun my/org-enable-org-modern-indent-in-existing-buffers ()
  "Enable `org-modern-indent-mode' for Org buffers that already exist."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/org-enable-org-modern-indent))))

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
  "Temporarily suspend expensive dynamic Org UI helpers during Evil insert state."
  (when (and (derived-mode-p 'org-mode)
             (not my/org--suspended-in-insert)
             (fboundp 'evil-insert-state-p)
             (evil-insert-state-p))
    (setq-local my/org--suspended-in-insert t)
    (setq-local my/org--insert-suspended-modes nil)
    (my/org--suspend-mode-for-insert 'org-appear-mode)
    ;; org-fragtog manages formula overlay visibility and is specifically
    ;; designed for insert-time editing; toggling it causes overlay height
    ;; changes that appear as vertical scrolling when entering/leaving formulas.
    (my/org--suspend-mode-for-insert 'valign-mode)))

(defun my/org-resume-expensive-modes-after-insert ()
  "Re-enable Org UI helpers suspended by `my/org-suspend-expensive-modes-in-insert'."
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
  (org-modern-star '("◉" "○" "✸" "✿" "◈" "◇" "❀" "✜"))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((45 . "∙") (43 . "∙") (42 . "∙"))) 
  
  ;; 2. 标签按钮化 (Tag Buttons)
  (org-modern-tag t) 
  (org-modern-label-border 4)
  
  ;; 3. 统计与进度条美化
  (org-modern-statistics t) 
  (org-modern-progress t)   
  
  ;; 4. 时间戳美化
  (org-modern-timestamp t)
  
  ;; 5. 关键词美化 (集成你的图标)
  (org-modern-keyword
   '(("title"        . "➲")
     ("subtitle"     . "⮊")
     ("author"       . "💁")
     ("email"        . "📧")
     ("date"         . "📅")
     ("language"     . "🖹")
     ("options"      . "⛭")
     ("startup"      . "✲")
     ("macro"        . "Maps")
     ("bind"         . "Key")
     ("setupfile"    . "📝")
     ("downloaded"   . "⇊")
     ("attr_latex"   . "🄛")
     ("attr_html"    . "🄗")
     ("attr_org"     . "🄞")
     ("name"         . "🄝")
     ("caption"      . "🄒")
     ("results"      . "☰")
     ("print_bibliography" . "📚")))
  
  ;; 6. 复选框美化
  (org-modern-checkbox
   '((?X . "🗹")
     (?- . "⊟")
     (?\s . "□")))

  ;; 7. 其他装饰
  (org-modern-horizontal-rule t)
  (org-modern-block-name nil)
  (org-modern-todo nil) ; 禁用 todo 美化，以免覆盖你在 org-mode 中自定义的颜色
  (org-modern-priority t))

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
               (crust (aaron-ui-color 'bg-base))
               (mantle (aaron-ui-color 'bg-code))
               (surface0 (aaron-ui-color 'bg-surface))
               (surface1 (aaron-ui-color 'bg-surface-strong))
               (surface2 (aaron-ui-color 'bg-surface-stronger))
               (overlay1 (aaron-ui-color 'fg-overlay))
               (subtext0 (aaron-ui-color 'fg-subtle))
               (subtext1 (aaron-ui-color 'fg-soft))
               (text (aaron-ui-color 'fg-main))
               (rosewater (aaron-ui-color 'accent-rose))
               (yellow (aaron-ui-color 'accent-yellow))
               (blue (aaron-ui-color 'accent-blue))
               (lavender (aaron-ui-color 'accent-lavender))
               (mauve (aaron-ui-color 'accent-mauve))
               (teal (aaron-ui-color 'accent-teal))
               (green (aaron-ui-color 'accent-green))
               (meta-bg (aaron-ui-color 'bg-meta))
               (meta-bg-strong (aaron-ui-color 'bg-meta-strong))
               (meta-fg (aaron-ui-color 'accent-green)))
          (when (facep 'org-document-title)
            (set-face-attribute 'org-document-title nil
                                :foreground rosewater
                                :weight popout-weight))
          (when (facep 'org-level-1)
            (set-face-attribute 'org-level-1 nil :foreground yellow :weight popout-weight))
          (when (facep 'org-level-2)
            (set-face-attribute 'org-level-2 nil :foreground blue :weight popout-weight))
          (when (facep 'org-level-3)
            (set-face-attribute 'org-level-3 nil :foreground mauve :weight strong-weight))
          (when (facep 'org-level-4)
            (set-face-attribute 'org-level-4 nil :foreground teal :weight strong-weight))
          (when (facep 'org-level-5)
            (set-face-attribute 'org-level-5 nil :foreground rosewater :weight title-weight))
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
          (when (facep 'org-block)
            (set-face-attribute 'org-block nil
                                :background crust
                                :foreground text
                                :extend t))
          (when (facep 'org-code)
            (set-face-attribute 'org-code nil
                                :background mantle
                                :foreground yellow
                                :weight strong-weight))
          (when (facep 'org-verbatim)
            (set-face-attribute 'org-verbatim nil
                                :background mantle
                                :foreground blue
                                :weight title-weight))
          (when (facep 'org-table)
            (set-face-attribute 'org-table nil
                                :foreground subtext1
                                :background base-bg))
          (when (facep 'org-formula)
            (set-face-attribute 'org-formula nil :foreground mauve))
          (when (facep 'org-special-keyword)
            (set-face-attribute 'org-special-keyword nil
                                :background meta-bg
                                :foreground meta-fg))
          (when (facep 'org-date)
            (set-face-attribute 'org-date nil :foreground blue :weight title-weight))
          (when (facep 'org-drawer)
            (set-face-attribute 'org-drawer nil :foreground overlay1))
          (when (facep 'org-ellipsis)
            (set-face-attribute 'org-ellipsis nil :foreground yellow :weight strong-weight))
          (when (facep 'org-indent)
            (set-face-attribute 'org-indent nil :foreground base-bg :background base-bg))
          (when (facep 'org-hide)
            (set-face-attribute 'org-hide nil :foreground base-bg :background base-bg))
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
                                :background surface0
                                :foreground text))
          (when (facep 'org-modern-date-active)
            (set-face-attribute 'org-modern-date-active nil
                                :background surface0
                                :foreground blue))
          (when (facep 'org-modern-date-inactive)
            (set-face-attribute 'org-modern-date-inactive nil
                                :background mantle
                                :foreground subtext0))
          (when (facep 'org-modern-block-name)
            (set-face-attribute 'org-modern-block-name nil
                                :foreground yellow
                                :weight strong-weight))
          (when (facep 'org-modern-progress-complete)
            (set-face-attribute 'org-modern-progress-complete nil :foreground green))
          (when (facep 'org-modern-progress-incomplete)
            (set-face-attribute 'org-modern-progress-incomplete nil :foreground surface2)))))))

(add-hook 'org-mode-hook #'my/org-apply-ui)
(add-hook 'after-load-theme-hook #'my/org-apply-ui)
(add-hook 'org-mode-hook #'my/org-setup-evil-insert-performance)

;; 3.4 Org Modern Indent
(my/package-ensure-vc 'org-modern-indent "https://github.com/jdtsmith/org-modern-indent.git")

(use-package org-modern-indent
  :after org
  :hook (org-mode . my/org-enable-org-modern-indent)
  :config
  (setq org-modern-indent-width 4)
  (my/org-enable-org-modern-indent-in-existing-buffers))

;; 3.5 自动显示强调符
(my/package-ensure-vc 'org-appear "https://github.com/awth13/org-appear.git")

(use-package org-appear
  :after org
  :hook (org-mode . my/org-enable-org-appear-maybe)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  ;; 光标进入 `^{}' / `_{}' 时显示标记，避免编辑时只看到渲染结果。
  (org-appear-autosubmarkers t))

;; 3.6 优先级美化
(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; ===========================================================
;; 1. 样式配置 (可自定义颜色和标签)
;; ===========================================================
(defvar my/org-special-block-styles
  `(("definition" . (:label "定义" :color ,(aaron-ui-color 'accent-yellow)))
    ("defn"       . (:label "定义" :color ,(aaron-ui-color 'accent-yellow)))
    ("theorem"    . (:label "定理" :color ,(aaron-ui-color 'accent-green)))
    ("thm"        . (:label "定理" :color ,(aaron-ui-color 'accent-green)))
    ("lemma"      . (:label "引理" :color ,(aaron-ui-color 'accent-blue)))
    ("cor"        . (:label "推论" :color ,(aaron-ui-color 'accent-lavender)))
    ("prop"       . (:label "命题" :color ,(aaron-ui-color 'accent-rose)))
    ("property"   . (:label "性质" :color ,(aaron-ui-color 'accent-lavender)))
    ("proof"      . (:label "证明" :color ,(aaron-ui-color 'accent-cyan)))
    ("example"    . (:label "例子" :color ,(aaron-ui-color 'accent-yellow-soft)))
    ("attention"  . (:label "注意" :color ,(aaron-ui-color 'accent-red)))
    ("note"       . (:label "笔记" :color ,(aaron-ui-color 'accent-teal)))
    ("info"       . (:label "信息" :color ,(aaron-ui-color 'accent-teal)))
    ("warning"    . (:label "警告" :color ,(aaron-ui-color 'accent-red)))))

(defface my/org-block-title-face
  '((t :weight regular :height 1.05 :inherit default))
  "Block 标题的字体样式。")

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
  (when-let* ((config (cdr (assoc (downcase (or type ""))
                                  my/org-special-block-styles)))
              (base-color (plist-get config :color))
              (background (or default-bg (my/org-default-background))))
    (list :label (plist-get config :label)
          :base-color base-color
          :header-bg (my/org-blend-colors base-color background 0.18)
          :body-bg (my/org-blend-colors base-color background 0.075)
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

;; ===========================================================
;; 3. 核心渲染逻辑：只处理单个 Element
;; ===========================================================
(defun my/org-prettify-element (element)
  "渲染单个 Org Element 节点，应用 Overlay 样式。"
  (let* ((type (downcase (or (org-element-property :type element) "")))
         (config (cdr (assoc type my/org-special-block-styles))))
    
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
             (label (plist-get palette :label))
             (params (org-element-property :parameters element))
             (title-text (concat " " label))
             (params-text (and params (concat "  " params)))
             (header-bg (plist-get palette :header-bg))
             (body-bg (plist-get palette :body-bg))
             (footer-color (plist-get palette :footer-color))
             (params-color (plist-get palette :params-color))
             (signature (list type begin-pos end-pos contents-begin contents-end
                              post-affiliated params default-bg))
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
          (puthash begin-pos signature my/org-pretty-block-cache)

        ;; 3. 清理区域
        (remove-overlays begin-pos end-pos 'my/org-pretty-block t)

        ;; -------------------------------------------------------
        ;; A. Header Overlay (#+begin_xxx)
        ;; -------------------------------------------------------
        (let* ((header-bol (save-excursion
                             (goto-char post-affiliated)
                             (line-beginning-position)))
               (header-text-beg (save-excursion
                                  (goto-char post-affiliated)
                                  (back-to-indentation)
                                  (point)))
               (header-end (save-excursion
                             (goto-char post-affiliated)
                             (line-end-position)))
               (header-bg-end (my/org-line-background-end post-affiliated)))
          (let ((ov (make-overlay header-bol header-bg-end)))
            (overlay-put ov 'my/org-pretty-block t)
            (overlay-put ov 'face `(:background ,header-bg :extend t))
            (overlay-put ov 'priority priority)
            (overlay-put ov 'evaporate t))
          ;; Do not replace leading indentation characters: org-modern-indent
          ;; uses display properties there for its left bracket/guide.
          (let ((ov (make-overlay header-text-beg header-end)))
            (overlay-put ov 'my/org-pretty-block t)
            (overlay-put ov 'face `(:background ,header-bg :extend t))
            (overlay-put ov 'priority (1+ priority))
            (overlay-put ov 'display
                         (concat (propertize "▎" 'face `(:background ,header-bg :foreground ,base-color :weight bold))
                                 (propertize title-text 'face `(:inherit my/org-block-title-face :background ,header-bg :foreground ,base-color :weight medium))
                                 (when params-text
                                   (propertize params-text 'face `(:inherit my/org-block-title-face :background ,header-bg :foreground ,params-color)))))
            (overlay-put ov 'evaporate t)))

        ;; -------------------------------------------------------
        ;; B. Body Overlay (内容区域)
        ;; -------------------------------------------------------
        (when (and contents-begin contents-end (> contents-end contents-begin))
          (let ((true-body-end contents-end))
            (let ((ov (make-overlay contents-begin true-body-end)))
              (overlay-put ov 'my/org-pretty-block t)
              (overlay-put ov 'face `(:background ,body-bg :extend t))
              (overlay-put ov 'priority priority)
              (overlay-put ov 'evaporate t))))

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
                (overlay-put ov 'face `(:background ,header-bg :extend t))
                (overlay-put ov 'priority priority)
                (overlay-put ov 'evaporate t))
              (let ((ov (make-overlay footer-text-beg footer-end)))
                (overlay-put ov 'my/org-pretty-block t)
                (overlay-put ov 'face `(:background ,header-bg :extend t))
                (overlay-put ov 'priority (1+ priority))
                (overlay-put ov 'display
                             (propertize "╰────────────────────────"
                                         'face `(:background ,header-bg :foreground ,footer-color :height 0.8)))
                (overlay-put ov 'evaporate t))))))))))

;; ===========================================================
;; 4. JIT-Lock 引擎：连续扫描 (支持嵌套)
;; ===========================================================
(defun my/org-jit-prettify-blocks (start end)
  "JIT-Lock 调用的函数：扫描 start 之后的块，确保完整渲染。"
  (save-excursion
    (save-match-data
      (goto-char start)
      (if (re-search-backward "^[ \t]*#\\+begin_" nil t)
          (let ((el (org-element-at-point)))
            (when (and (eq (org-element-type el) 'special-block)
                       (> (org-element-property :end el) start))
              (setq start (org-element-property :begin el))))
        (goto-char start))
      
      (goto-char start)
      (while (re-search-forward "^[ \t]*#\\+begin_\\(\\w+\\)" end t)
        (let ((el (org-element-at-point)))
          (when (eq (org-element-type el) 'special-block)
            (my/org-prettify-element el)))))))

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

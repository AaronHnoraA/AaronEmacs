;;; init-org-ui.el --- Org UI and presentation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'init-org-core)
(require 'org-element)

(defvar-local my/org--olivetti-auto-state nil)
(defvar-local my/org-pretty-block-cache nil)
(defvar my/org-ui--face-theme-signature nil)
(defvar my/org--olivetti-sync-timer nil)
(defvar-local my/org--suspended-in-insert nil)
(defvar-local my/org--insert-suspended-modes nil)

(declare-function evil-insert-state-p "evil")

(defun my/org-enable-valign-maybe ()
  "Enable `valign-mode' for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (valign-mode 1)))

(defun my/org-enable-org-modern-indent-maybe ()
  "Enable `org-modern-indent-mode' for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (org-modern-indent-mode 1)))

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
    (when (facep 'org-document-title)
      (set-face-attribute 'org-document-title nil
                          :foreground "#f6f1df"
                          :weight 'medium))
    (when (facep 'org-level-1)
      (set-face-attribute 'org-level-1 nil :foreground "#e7cf8b" :weight 'medium))
    (when (facep 'org-level-2)
      (set-face-attribute 'org-level-2 nil :foreground "#98ccf2" :weight 'medium))
    (when (facep 'org-level-3)
      (set-face-attribute 'org-level-3 nil :foreground "#c7b7ec" :weight 'medium))
    (when (facep 'org-level-4)
      (set-face-attribute 'org-level-4 nil :foreground "#96d4be" :weight 'medium))
    (when (facep 'org-level-5)
      (set-face-attribute 'org-level-5 nil :foreground "#d9be96"))
    (when (facep 'org-level-6)
      (set-face-attribute 'org-level-6 nil :foreground "#a3bdd8"))
    (when (facep 'org-level-7)
      (set-face-attribute 'org-level-7 nil :foreground "#c1b4d7"))
    (when (facep 'org-level-8)
      (set-face-attribute 'org-level-8 nil :foreground "#9dc3b6"))
    (when (facep 'org-document-info)
      (set-face-attribute 'org-document-info nil :foreground "#c7cfdb"))
    (when (facep 'org-meta-line)
      (set-face-attribute 'org-meta-line nil
                          :background "#24362b"
                          :foreground "#98c379"
                          :extend t))
    (when (facep 'org-document-info-keyword)
      (set-face-attribute 'org-document-info-keyword nil
                          :background "#24362b"
                          :foreground "#8fbc72"))
    (when (facep 'org-block)
      (set-face-attribute 'org-block nil
                          :background "#2d3240"
                          :foreground "#e8edf4"
                          :extend t))
    (when (facep 'org-block-begin-line)
      (set-face-attribute 'org-block-begin-line nil
                          :background "#394150"
                          :foreground "#c6d0dc"
                          :extend t))
    (when (facep 'org-block-end-line)
      (set-face-attribute 'org-block-end-line nil
                          :background "#394150"
                          :foreground "#aeb8c7"
                          :extend t))
    (when (facep 'org-code)
      (set-face-attribute 'org-code nil
                          :background "#2d3140"
                          :foreground "#f0d58a"))
    (when (facep 'org-verbatim)
      (set-face-attribute 'org-verbatim nil
                          :background "#2d3140"
                          :foreground "#95d5ff"))
    (when (facep 'org-table)
      (set-face-attribute 'org-table nil :foreground "#cad3e0"))
    (when (facep 'org-formula)
      (set-face-attribute 'org-formula nil :foreground "#d9b8ff"))
    (when (facep 'org-special-keyword)
      (set-face-attribute 'org-special-keyword nil
                          :background "#24362b"
                          :foreground "#8fbc72"))
    (when (facep 'org-date)
      (set-face-attribute 'org-date nil :foreground "#9fd7ff"))
    (when (facep 'org-drawer)
      (set-face-attribute 'org-drawer nil :foreground "#b0bbcd"))
    (when (facep 'org-ellipsis)
      (set-face-attribute 'org-ellipsis nil :foreground "#f0c674"))
    (when (facep 'org-indent)
      (set-face-attribute 'org-indent nil :foreground "#313949" :background "#282a36"))
    (when (facep 'org-hide)
      (set-face-attribute 'org-hide nil :foreground "#282a36" :background "#282a36"))
    (when (facep 'org-modern-label)
      (set-face-attribute 'org-modern-label nil
                          :background "#445065"
                          :foreground "#edf2f7"
                          :box '(:line-width 4 :color "#445065")))
    (when (facep 'org-modern-keyword)
      (set-face-attribute 'org-modern-keyword nil
                          :background "#24362b"
                          :foreground "#8fbc72"
                          :box nil))
    (when (facep 'org-modern-tag)
      (set-face-attribute 'org-modern-tag nil
                          :background "#38435a"
                          :foreground "#dce4ee"))
    (when (facep 'org-modern-date-active)
      (set-face-attribute 'org-modern-date-active nil
                          :background "#3a465c"
                          :foreground "#c0e3f8"))
    (when (facep 'org-modern-date-inactive)
      (set-face-attribute 'org-modern-date-inactive nil
                          :background "#343a49"
                          :foreground "#b1bac9"))
    (when (facep 'org-modern-block-name)
      (set-face-attribute 'org-modern-block-name nil
                          :foreground "#f0d58a"
                          :weight 'medium))
    (when (facep 'org-modern-progress-complete)
      (set-face-attribute 'org-modern-progress-complete nil :foreground "#9ed0a4"))
    (when (facep 'org-modern-progress-incomplete)
      (set-face-attribute 'org-modern-progress-incomplete nil :foreground "#66728a"))))))

(add-hook 'org-mode-hook #'my/org-apply-ui)
(add-hook 'after-load-theme-hook #'my/org-apply-ui)
(add-hook 'org-mode-hook #'my/org-setup-evil-insert-performance)

;; 3.4 Org Modern Indent
(my/package-ensure-vc 'org-modern-indent "https://github.com/jdtsmith/org-modern-indent.git")

(use-package org-modern-indent
  :after org-modern
  :hook (org-mode . my/org-enable-org-modern-indent-maybe)
  :config
  (setq org-modern-indent-width 4))

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
  '(("definition" . (:label "定义" :color "#e6c57d"))
    ("defn"       . (:label "定义" :color "#e6c57d"))
    ("theorem"    . (:label "定理" :color "#9fd694"))
    ("thm"        . (:label "定理" :color "#9fd694"))
    ("lemma"      . (:label "引理" :color "#8bbfe8"))
    ("cor"        . (:label "推论" :color "#c4b1e2"))
    ("prop"       . (:label "命题" :color "#e59bb4"))
    ("property"   . (:label "性质" :color "#c4b1e2"))
    ("proof"      . (:label "证明" :color "#7fb2e6"))
    ("example"    . (:label "例子" :color "#deae8d"))
    ("attention"  . (:label "注意" :color "#de8ea0"))
    ("note"       . (:label "笔记" :color "#88cddd"))
    ("info"       . (:label "信息" :color "#88cddd"))
    ("warning"    . (:label "警告" :color "#de8ea0"))))

(defface my/org-block-title-face
  '((t :weight regular :height 1.05 :inherit default))
  "Block 标题的字体样式。")

;; ===========================================================
;; 2. 颜色计算辅助函数
;; ===========================================================
(defun my/org-blend-colors (color1 color2 alpha)
  "混合颜色: color1(前景) * alpha + color2(背景) * (1-alpha)。"
  (condition-case nil
      (let ((c1 (color-name-to-rgb color1))
            (c2 (color-name-to-rgb color2)))
        (apply 'color-rgb-to-hex
               (cl-mapcar (lambda (x y) (+ (* x alpha) (* y (- 1.0 alpha))))
                          c1 c2)))
    (error color1)))

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
             (base-color (plist-get config :color))
             (label (plist-get config :label))
             (params (org-element-property :parameters element))
             (title-text (concat " " label))
             (params-text (and params (concat "  " params)))
             
             (default-bg (face-attribute 'default :background nil t))
             (default-bg (if (or (not default-bg) (string= default-bg "unspecified"))
                             "#1a1b26" default-bg))
             (header-bg (my/org-blend-colors base-color default-bg 0.13))
             (body-bg (my/org-blend-colors base-color default-bg 0.045))
             (footer-color (my/org-blend-colors base-color default-bg 0.62))
             (params-color (my/org-blend-colors "#d8dee9" default-bg 0.68))
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
        (let ((header-end (save-excursion 
                            (goto-char post-affiliated) 
                            (line-end-position))))
          (let ((ov (make-overlay post-affiliated header-end)))
            (overlay-put ov 'my/org-pretty-block t)
            (overlay-put ov 'face `(:background ,header-bg :extend t))
            (overlay-put ov 'priority priority)
            (overlay-put ov 'display 
                         (concat (propertize "▎" 'face `(:foreground ,base-color :weight bold))
                                 (propertize title-text 'face `(:inherit my/org-block-title-face :foreground ,base-color :weight medium))
                                 (when params-text
                                   (propertize params-text 'face `(:inherit my/org-block-title-face :foreground ,params-color)))))
            (overlay-put ov 'evaporate t)))

        ;; -------------------------------------------------------
        ;; B. Body Overlay (内容区域)
        ;; -------------------------------------------------------
        (when (and contents-begin contents-end (> contents-end contents-begin))
          ;; 修正：内容末尾通常是换行符，Overlay 退一格以防覆盖 Footer
          (let ((true-body-end (if (= (char-before contents-end) ?\n)
                                   (1- contents-end)
                                 contents-end)))
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
            (let ((ov (make-overlay (point) (line-end-position))))
              (overlay-put ov 'my/org-pretty-block t)
              (overlay-put ov 'face `(:background ,body-bg :extend t))
              (overlay-put ov 'priority priority)
              (overlay-put ov 'display 
                           (propertize "╰────────────────────────"
                                       'face `(:foreground ,footer-color :height 0.8)))
              (overlay-put ov 'evaporate t)))))))))

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
    (unless (memq #'my/org-jit-prettify-blocks jit-lock-functions)
      (jit-lock-register #'my/org-jit-prettify-blocks t))
    (jit-lock-refontify)))

(defun my/org-reset-overlays ()
  "调试用：强制清除所有 Overlay 并重绘。"
  (interactive)
  (remove-overlays (point-min) (point-max) 'my/org-pretty-block t)
  (when (hash-table-p my/org-pretty-block-cache)
    (clrhash my/org-pretty-block-cache))
  (jit-lock-refontify))

(add-hook 'org-mode-hook #'my/org-enable-jit-pretty-blocks)

(provide 'init-org-ui)
;;; init-org-ui.el ends here

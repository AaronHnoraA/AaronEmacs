;;; init-org-ui.el --- Org UI and presentation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'init-org-core)
(require 'org-element)

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
    (if (and (eq (length (window-list nil nil nil)) 1)
             (derived-mode-p 'org-mode))
        (olivetti-mode 1)
      (olivetti-mode 0)))
  
  (add-hook 'org-mode-hook #'xs-toggle-olivetti-for-org)
  (add-hook 'window-configuration-change-hook #'xs-toggle-olivetti-for-org))

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
  (org-appear-autosubmarkers nil))

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
  '(("definition" . (:label "定义" :color "#e0af68"))
    ("defn"       . (:label "定义" :color "#e0af68"))
    ("theorem"    . (:label "定理" :color "#9ece6a"))
    ("thm"        . (:label "定理" :color "#9ece6a"))
    ("lemma"      . (:label "引理" :color "#7aa2f7"))
    ("cor"        . (:label "推论" :color "#bb9af7"))
    ("prop"       . (:label "命题" :color "#ff75a0"))
    ("property"   . (:label "性质" :color "#bb9af7"))
    ("proof"      . (:label "证明" :color "#7aa2f7"))
    ("example"    . (:label "例子" :color "#d08770"))
    ("attention"  . (:label "注意" :color "#f7768e"))
    ("note"       . (:label "笔记" :color "#0db9d7"))
    ("info"       . (:label "信息" :color "#0db9d7"))
    ("warning"    . (:label "警告" :color "#f7768e"))))

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
             (title-text (concat " " label (if params (concat " : " params) "")))
             
             (default-bg (face-attribute 'default :background nil t))
             (default-bg (if (or (not default-bg) (string= default-bg "unspecified"))
                             "#1a1b26" default-bg))
             (header-bg (my/org-blend-colors base-color default-bg 0.15))
             (body-bg (my/org-blend-colors base-color default-bg 0.05)))

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
                         (concat (propertize "▍" 'face `(:foreground ,base-color :weight bold))
                                 (propertize title-text 'face `(:inherit my/org-block-title-face :foreground ,base-color))))
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
                           (propertize "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" 
                                       'face `(:foreground ,base-color :height 0.7)))
              (overlay-put ov 'evaporate t))))))))

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
  (when (derived-mode-p 'org-mode)
    (unless (memq #'my/org-jit-prettify-blocks jit-lock-functions)
      (jit-lock-register #'my/org-jit-prettify-blocks t))
    (jit-lock-refontify)))

(defun my/org-reset-overlays ()
  "调试用：强制清除所有 Overlay 并重绘。"
  (interactive)
  (remove-overlays (point-min) (point-max) 'my/org-pretty-block t)
  (jit-lock-refontify))

(add-hook 'org-mode-hook #'my/org-enable-jit-pretty-blocks)

(provide 'init-org-ui)
;;; init-org-ui.el ends here

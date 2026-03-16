;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Refactored Org Configuration.
;; 1. Fixed load order: org-modern loads after org.
;; 2. Optimized directory creation and path handling.
;; 3. Enhanced robustness of special block rendering (nested blocks support).
;; 4. Performance tuning for on-demand LaTeX previews.

;;; Code:

(declare-function my/typography-setup-prose-buffer "init-base")
(declare-function my/refresh-environment-from-shell nil)
(declare-function my/shell-command-executable "init-utils")

(require 'init-funcs)
(require 'org)

;;; ----------------------------------------------------------------------------
;;; 1. Global Variables & Paths (全局路径配置)
;;; ----------------------------------------------------------------------------

(defvar my-org-root (file-truename "~/HC/Org/")
  "Root directory for all Org files.")

(defvar my-org-roam-dir (expand-file-name "roam/" my-org-root))
(defvar my-org-daily-dir (expand-file-name "daily/" my-org-root))
(defvar my-org-notes-file (expand-file-name "notes.org" my-org-root))
(defvar my-org-diary-file (expand-file-name "diary.org" my-org-root))

;; References
(defvar pv/org-refile-file (expand-file-name "refile.org" my-org-root))
(defvar pv/org-bibtex-dir (expand-file-name "references/" my-org-root))
(defvar pv/org-bibtex-files (list (expand-file-name "references.bib" pv/org-bibtex-dir)))

;; Ensure core directories exist (Optimized)
(dolist (dir (list my-org-root my-org-roam-dir my-org-daily-dir pv/org-bibtex-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defgroup my/org-ui nil
  "Org UI and note-taking ergonomics."
  :group 'org)

(defcustom my/org-rich-ui-max-buffer-size (* 512 1024)
  "Compatibility knob kept for older logic.
Rich Org UI is no longer disabled based on buffer size."
  :type 'integer)

(defcustom my/org-pretty-block-max-buffer-size (* 256 1024)
  "Compatibility knob kept for older logic.
Special block overlays are no longer disabled based on buffer size."
  :type 'integer)

(defun my/org-rich-ui-buffer-p ()
  "Return non-nil when the current Org buffer should use rich UI helpers."
  (my/rich-ui-buffer-p nil my/org-rich-ui-max-buffer-size))

(defun my/org-enable-typography-maybe ()
  "Enable shared prose typography for Org buffers in graphical sessions."
  (when (my/org-rich-ui-buffer-p)
    (my/typography-setup-prose-buffer)))

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

;;; ----------------------------------------------------------------------------
;;; 2. Org Core Configuration (核心设置)
;;; ----------------------------------------------------------------------------

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)        ; 自动换行
         (org-mode . org-indent-mode)
         (org-mode . my/org-enable-typography-maybe)) ; 缩进模式
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-q" . counsel-org-tag))

  :custom
  ;; --- Directories ---
  (org-directory my-org-root)
  (org-default-notes-file my-org-notes-file)
  (org-archive-location "%s_archive::datetree/")

  ;; --- Appearance Basics ---
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-image-actual-width nil)
  (org-startup-with-inline-images t)
  (org-display-remote-inline-images t)
  (org-imenu-depth 4)
  
  ;; --- Navigation & Editing ---
  (org-return-follows-link nil)
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; --- Todo Keywords & Faces ---
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "HOLD(h!)" "WIP(i!)" "WAIT(w@/!)" "|" 
               "DONE(d!)" "CANCELLED(c@/!)")))

  (org-todo-keyword-faces
   '(("TODO"      . (:foreground "#ff6c6b" :weight medium))
     ("NEXT"      . (:foreground "#98be65" :weight medium))
     ("HOLD"      . (:foreground "#feb24c" :weight medium))
     ("WIP"       . (:foreground "#0098dd" :weight medium))
     ("WAIT"      . (:foreground "#ecbe7b" :weight medium))
     ("DONE"      . (:foreground "#51afef" :weight medium :strike-through t))
     ("CANCELLED" . (:foreground "#ff6480" :weight medium :strike-through t))))

  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-closed-keep-when-no-todo t)
  (org-log-repeat 'time)
  (org-priority-faces
   '((?A :foreground "red" :weight medium)
     (?B :foreground "orange")
     (?C :foreground "yellow")))

  ;; --- Properties ---
  (org-global-properties
   '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("STYLE_ALL" . "habit")))
  (org-cycle-hide-drawer-startup t) 

  ;; --- Refiling ---
  (org-refile-use-cache nil)
  (org-refile-targets '((nil . (:maxlevel . 9))
                        (org-agenda-files . (:maxlevel . 9))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  
  ;; --- Tags & Search ---
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)

  ;; --- Links ---
  (org-link-abbrev-alist
   '(("GitHub" . "https://github.com/")
     ("Google" . "https://google.com/search?q=")
     ("RFCs"   . "https://tools.ietf.org/html/")))
  
  ;; --- Citations ---
  (org-cite-global-bibliography pv/org-bibtex-files))

;;; ----------------------------------------------------------------------------
;;; 3. Modern UI & Aesthetics (UI美化)
;;; ----------------------------------------------------------------------------

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
  (org-modern-priority t)
  
  :config
  (setq-default line-spacing 0.1))

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
  (org-appear-autosubmarkers t))

;; 3.6 优先级美化
(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; ===========================================================
;; 0. 依赖加载
;; ===========================================================
(require 'color)
(require 'cl-lib)
(require 'org-element)

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

;;; ----------------------------------------------------------------------------
;;; 4. Agenda (日程管理)
;;; ----------------------------------------------------------------------------

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-files nil)
  (setq org-agenda-diary-file nil)

  :custom
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-outline-path t)
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " %i %-12:c %s")
     (tags   . " %i %-12:c %s")
     (search . " %i %-12:c %s")))
  (org-agenda-hide-tags-regexp ".")

  (org-agenda-custom-commands
   '(("o" "Overview / Dashboard"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-overriding-header "⚡ Today's Schedule & Deadlines")))
       (tags-todo "+uni/!TODO|NEXT"
                  ((org-agenda-overriding-header "🎓 University Tasks")))
       (tags-todo "+math+cs+qc+research/!TODO|NEXT"
                  ((org-agenda-overriding-header "🔬 Research & Gaps")))
       (todo "NEXT"
             ((org-agenda-overriding-header "🚀 Next Actions")))
       (todo "WAIT"
             ((org-agenda-overriding-header "⏳ Waiting")))
       (tags "inbox"
             ((org-agenda-overriding-header "📥 Unprocessed Inbox"))))))))

;;; ----------------------------------------------------------------------------
;;; 5. Capture (快速记录)
;;; ----------------------------------------------------------------------------

(defvar my-daily-subdirs '("idea" "inbox" "mail" "note" "meeting" "protocol" "uni" "life"))
(dolist (dir my-daily-subdirs)
  (make-directory (expand-file-name dir my-org-daily-dir) t))

(defun my/get-daily-capture-path (subdir)
  "Prompt for filename, append date, return path."
  (let* ((name (read-string "File Name (slug): "))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (downcase name)))
         (date (format-time-string "%Y%m%d"))
         (fname (format "%s-%s.org" slug date)))
    (expand-file-name fname (expand-file-name subdir my-org-daily-dir))))

(setq org-capture-use-agenda-date t)
(setq org-capture-templates
      '(("i" "Idea" plain (file (lambda () (my/get-daily-capture-path "idea")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :idea:\n\n* Idea:\n%?\n" :unnarrowed t)
        ("b" "Inbox" plain (file (lambda () (my/get-daily-capture-path "inbox")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :inbox:\n\n* Details\n%?\n" :unnarrowed t)
        ("m" "Mail" plain (file (lambda () (my/get-daily-capture-path "mail")))
         "#+title: Mail: %^{Subject}\n#+date: %u\n#+filetags: :mail:\n\n* To/From: %^{Recipient}\n* Status: TODO\n\n%?\n" :unnarrowed t)
        ("n" "Note" plain (file (lambda () (my/get-daily-capture-path "note")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :note:\n\n%?\n" :unnarrowed t)
        ("t" "Meeting" plain (file (lambda () (my/get-daily-capture-path "meeting")))
         "#+title: Meeting: %^{Topic}\n#+date: %u\n#+filetags: :meeting:\n\n* Who: %^{Who}\n* Time: %^T\n\n* Agenda\n%?\n" :unnarrowed t)
        ("u" "Uni Task" plain (file (lambda () (my/get-daily-capture-path "uni")))
         "#+title: %^{Task}\n#+date: %u\n#+filetags: :uni:\n\n* Course: %^{Code}\n* Deadline: %^t\n\n* Req\n%?\n" :unnarrowed t)
        ("l" "Life Task" plain (file (lambda () (my/get-daily-capture-path "life")))
         "#+title: %^{Task}\n#+date: %u\n#+filetags: :life:\n\n* Type: %^{Type}\n%?\n" :unnarrowed t)))

(defcustom my/org-roam-background-init-delay 2
  "Idle delay before Org Roam starts its background services."
  :type 'number
  :group 'my/org-ui)

(defvar my/org-roam--background-timer nil)
(defvar my/org-roam--initialized nil)

(defun pv/org-set-last-modified ()
  "Update the `#+last_modified` field before saving an Org buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified:" nil t)
        (delete-region (point) (line-end-position))
        (insert (format " [%s]" (format-time-string "%Y-%m-%d %a %H:%M")))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'pv/org-set-last-modified nil t)))

(defun my/org-roam-background-init ()
  "Load Org Roam on idle so startup stays responsive."
  (setq my/org-roam--background-timer nil)
  (unless (featurep 'org-roam)
    (require 'org-roam nil t)))

;;; ----------------------------------------------------------------------------
;;; 6. Org Roam (知识库)
;;; ----------------------------------------------------------------------------

(use-package org-roam
  :ensure t
  :defer t
  :commands (org-roam-node-find
             org-roam-node-insert
             org-roam-tag-add
             org-roam-alias-add
             org-roam-buffer-toggle
             org-roam-db-autosync-mode)
  :init
  (setq org-roam-directory my-org-roam-dir)
  (setq org-roam-v2-ack t)
  
  ;; 定义前缀命令
  (define-prefix-command 'my-org-roam-command-map)
  (global-set-key (kbd "C-c n") 'my-org-roam-command-map)

  :bind 
  (:map my-org-roam-command-map
        ("f" . org-roam-node-find)
        ("i" . org-roam-node-insert)
        ("t" . org-roam-tag-add)
        ("a" . org-roam-alias-add)
        ("o" . org-id-get-create)
        ("l" . org-roam-buffer-toggle))
  
  :config
  (unless my/org-roam--initialized
    (org-roam-setup)
    (org-roam-db-autosync-mode 1)
    (setq my/org-roam--initialized t))
  (setq org-roam-capture-templates
        '(("m" "Math" plain "%?" :if-new (file+head "math/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :math:\n") :unnarrowed t)
          ("c" "CS" plain "%?" :if-new (file+head "CS/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :cs:\n") :unnarrowed t)
          ("q" "Quantum" plain "%?" :if-new (file+head "QC/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :qc:\n") :unnarrowed t)
          ("p" "Phil" plain "%?" :if-new (file+head "philosophy/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :phil:\n") :unnarrowed t)
          ("i" "Index" plain "%?" :if-new (file+head "index/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :index:\n") :unnarrowed t)
          ("r" "Paper" plain "%?" :if-new (file+head "papers/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :paper:\n") :unnarrowed t))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (or noninteractive
                        my/org-roam--background-timer
                        my/org-roam--initialized)
              (setq my/org-roam--background-timer
                    (run-with-idle-timer my/org-roam-background-init-delay nil
                                         #'my/org-roam-background-init)))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

;;; ----------------------------------------------------------------------------
;;; 7. Source Blocks & Babel (代码执行)
;;; ----------------------------------------------------------------------------

(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (dot . t) (emacs-lisp . t) (python . t) (shell . t)))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;;; ----------------------------------------------------------------------------
;;; 8. LaTeX, Reference & Export (学术写作)
;;; ----------------------------------------------------------------------------

(use-package cdlatex
  :ensure t
  :hook (org-mode . org-cdlatex-mode))

(use-package org-fragtog
  :ensure t
  :custom
  (org-fragtog-preview-delay 0.15)
  :hook (org-mode . my/org-enable-org-fragtog-maybe))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography pv/org-bibtex-files)
  (bibtex-completion-library-path (list pv/org-bibtex-dir))
  (bibtex-completion-pdf-open-function
   (lambda (fpath) (call-process "open" nil 0 nil fpath))))

(use-package org-ref
  :ensure t
  :after org)

;;;; 按需渲染：滚动停止后 idle 0.3s 预览可见区域（节流 + 去重）

(defgroup my/org-latex-preview nil
  "On-demand LaTeX preview helpers."
  :group 'org)

(defconst my/org-latex-export-magic-comment
  "% !TEX program = xelatex\n"
  "Magic comment prepended to Org-exported LaTeX files.")

(defconst my/org-latex-export-cjk-preamble
  "\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage{amsthm}\n\\usepackage{xeCJK}\n\\setCJKmainfont{Songti SC}\n\\setCJKsansfont{Hiragino Sans GB}\n"
  "Chinese package and font setup injected into Org-exported LaTeX preambles.")

(defconst my/org-latex-export-default-packages-alist
  '(("" "graphicx" t)
    ("" "longtable" nil)
    ("" "wrapfig" nil)
    ("" "rotating" nil)
    ("normalem" "ulem" t)
    ("" "capt-of" nil)
    ("" "hyperref" nil))
  "Package set used for Org LaTeX export.")

(defcustom my/org-latex-embedded-class-alist nil
  "Alist mapping LaTeX class names to embedded class sources.
Each value may be a readable `.cls' file path or literal class source."
  :type '(alist :key-type string :value-type string)
  :group 'org)

(defcustom my/org-latex-default-class-name "default"
  "Default LaTeX class used by Org export helpers."
  :type 'string
  :group 'org)

(defconst my/org-latex-default-sectioning
  '(("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "Fallback sectioning used for embedded Org LaTeX classes.")

(defcustom my/org-latex-preview-idle-delay 0.4
  "Idle delay (seconds) before previewing visible region after scrolling."
  :type 'number
  :group 'my/org-latex-preview)

(defcustom my/org-latex-preview-min-chars 400
  "Minimum visible region size (chars) required to trigger preview."
  :type 'integer
  :group 'my/org-latex-preview)

(defcustom my/org-latex-preview-max-concurrency 1
  "Maximum number of concurrent LaTeX preview render jobs."
  :type 'integer
  :group 'my/org-latex-preview)

(defcustom my/latex-preview-math-font "GFS Neohellenic Math"
  "Fallback math font family used by XeLaTeX-based preview pipelines."
  :type 'string
  :group 'my/org-latex-preview)

(defun my/org-latex--source-directory (&optional info)
  "Return the source directory for Org export INFO."
  (let ((input-file (plist-get info :input-file)))
    (file-name-as-directory
     (expand-file-name
      (or (and input-file (file-name-directory input-file))
          default-directory)))))

(defun my/org-latex--project-root (&optional info)
  "Return the current project root for Org export INFO."
  (let* ((source-dir (my/org-latex--source-directory info))
         (project (when (fboundp 'project-current)
                    (project-current nil source-dir))))
    (file-name-as-directory
     (expand-file-name
      (or (and project
               (if (fboundp 'project-root)
                   (project-root project)
                 (car project)))
          source-dir)))))

(defun my/org-latex--class-search-directories (&optional info)
  "Return class search directories for Org export INFO."
  (delete-dups
   (delq nil
         (list
          (expand-file-name "latex" (my/org-latex--project-root info))
          (expand-file-name "latex" user-emacs-directory)))))

(defun my/org-latex--find-class-file (class &optional info)
  "Return the first readable `.cls' file for CLASS in export INFO."
  (catch 'file
    (dolist (dir (my/org-latex--class-search-directories info))
      (let ((candidate (expand-file-name (concat class ".cls") dir)))
        (when (file-readable-p candidate)
          (throw 'file candidate))))))

(defun my/org-latex--resolve-embedded-class-source (class info)
  "Return embedded class source for CLASS in export INFO, if configured."
  (let* ((configured-source (cdr (assoc class my/org-latex-embedded-class-alist)))
         (base-dir (my/org-latex--source-directory info))
         (class-file
          (cond
           ((and (stringp configured-source)
                 (not (string-match-p "\n" configured-source))
                 (file-readable-p configured-source))
            configured-source)
           ((and (stringp configured-source)
                 (not (string-match-p "\n" configured-source))
                 (file-readable-p (expand-file-name configured-source base-dir)))
            (expand-file-name configured-source base-dir))
           (t
            (my/org-latex--find-class-file class info)))))
    (cond
     (class-file
      (with-temp-buffer
        (insert-file-contents class-file)
        (buffer-string)))
     ((stringp configured-source)
      configured-source))))

(defun my/org-latex--make-filecontents-block (class source)
  "Return a `filecontents*' block that embeds CLASS with SOURCE."
  (concat "\\begin{filecontents*}[overwrite]{" class ".cls}\n"
          source
          (unless (string-suffix-p "\n" source)
            "\n")
          "\\end{filecontents*}\n"))

(defun my/org-latex--inject-embedded-class (latex info)
  "Embed a configured class definition into exported LATEX using INFO."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (when (re-search-forward "^\\\\documentclass\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}" nil t)
      (let* ((class (match-string 1))
             (source (my/org-latex--resolve-embedded-class-source class info)))
        (when (and source
                   (not (save-excursion
                          (goto-char (point-min))
                          (re-search-forward
                           (format "\\\\begin{filecontents\\*?}\\(?:\\[[^]]*\\]\\)?{%s\\.cls}"
                                   (regexp-quote class))
                           nil t))))
          (goto-char (match-beginning 0))
          (insert (my/org-latex--make-filecontents-block class source)))))
    (buffer-string)))

(defun my/org-latex--ensure-embedded-class-registered (info)
  "Register an embedded LaTeX class from INFO in `org-latex-classes'."
  (when-let* ((class (plist-get info :latex-class))
              (_ (my/org-latex--resolve-embedded-class-source class info)))
    (let ((classes (or (plist-get info :latex-classes) org-latex-classes)))
      (unless (assoc class classes)
        (let* ((class-options (plist-get info :latex-class-options))
               (documentclass
                (if class-options
                    (format "\\documentclass[%s]{%s}" class-options class)
                  (format "\\documentclass{%s}" class)))
               (article (assoc "article" classes))
               (sectioning (or (and article (cddr article))
                               my/org-latex-default-sectioning))
               (updated-classes (cons `(,class ,documentclass ,@sectioning)
                                      classes)))
          (setq org-latex-classes updated-classes)
          (setf (plist-get info :latex-classes) updated-classes))))))

(defun my/org-latex--latex-package-present-p (latex package)
  "Return non-nil when LATEX already loads PACKAGE."
  (catch 'found
    (dolist (line (split-string latex "\n"))
      (when (or (string-match-p "\\\\usepackage" line)
                (string-match-p "\\\\RequirePackage" line))
        (let ((open (string-match "{" line)))
          (when open
            (let ((close (string-match "}" line open)))
              (when close
                (dolist (pkg (split-string (substring line (1+ open) close)
                                           "," t "[[:space:]]*"))
                  (when (string= package (string-trim pkg))
                    (throw 'found t)))))))))
    nil))

(defun my/org-latex--latex-cjk-present-p (latex)
  "Return non-nil when LATEX already configures Chinese support."
  (or (my/org-latex--latex-package-present-p latex "xeCJK")
      (catch 'found
        (dolist (line (split-string latex "\n"))
          (when (and (or (string-match-p "\\\\documentclass" line)
                         (string-match-p "\\\\LoadClass" line))
                     (string-match-p "{ctex" line))
            (throw 'found t)))
        nil)))

(defun my/org-latex--keyword-value (keyword)
  "Return the first Org keyword value for KEYWORD in the current buffer."
  (when-let* ((entry (assoc keyword (org-collect-keywords (list keyword))))
              (raw (car (cdr entry))))
    (if (listp raw) (car raw) raw)))

(defconst my/org-latex-assignment-keyword-command-alist
  '(("ASSIGNMENT_INSTITUTION" . "assignmentinstitution")
    ("ASSIGNMENT_COURSE_CODE" . "assignmentcoursecode")
    ("ASSIGNMENT_TERM" . "assignmentterm")
    ("ASSIGNMENT_TITLE" . "assignmenttitle")
    ("ASSIGNMENT_DATE" . "assignmentdate")
    ("ASSIGNMENT_STUDENT_NAME" . "studentname")
    ("ASSIGNMENT_STUDENT_ID" . "studentid")
    ("ASSIGNMENT_AFFILIATION" . "assignmentaffiliation")
    ("ASSIGNMENT_AUTHOR_URL" . "authorurl"))
  "Org keyword to LaTeX command mapping for the `assignment' class.")

(defun my/org-latex--class-command-lines (info)
  "Return class-specific command lines derived from export INFO."
  (pcase (plist-get info :latex-class)
    ("assignment"
     (delq nil
           (mapcar
            (lambda (entry)
              (when-let* ((value (my/org-latex--keyword-value (car entry))))
                (format "\\%s{%s}" (cdr entry) value)))
            my/org-latex-assignment-keyword-command-alist)))
    (_ nil)))

(defun my/org-latex--inject-class-commands (latex info)
  "Insert class-specific LaTeX commands into exported LATEX using INFO."
  (if-let* ((commands (my/org-latex--class-command-lines info)))
      (with-temp-buffer
        (insert latex)
        (goto-char (point-min))
        (when (re-search-forward "^\\\\documentclass.*$" nil t)
          (end-of-line)
          (insert "\n" (mapconcat #'identity commands "\n") "\n"))
        (buffer-string))
    latex))

(defun my/org-latex--inject-export-preamble (latex)
  "Add XeLaTeX, math, and Chinese support boilerplate to exported LATEX."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (unless (looking-at-p (regexp-quote my/org-latex-export-magic-comment))
      (insert my/org-latex-export-magic-comment))
    (let (lines)
      (unless (my/org-latex--latex-package-present-p (buffer-string) "amsmath")
        (setq lines (append lines '("\\usepackage{amsmath}"))))
      (unless (my/org-latex--latex-package-present-p (buffer-string) "amssymb")
        (setq lines (append lines '("\\usepackage{amssymb}"))))
      (unless (my/org-latex--latex-package-present-p (buffer-string) "amsthm")
        (setq lines (append lines '("\\usepackage{amsthm}"))))
      (unless (my/org-latex--latex-cjk-present-p (buffer-string))
        (setq lines
              (append
               lines
               '("\\usepackage{xeCJK}"
                 "\\setCJKmainfont{Songti SC}"
                 "\\setCJKsansfont{Hiragino Sans GB}"))))
      (when lines
        (goto-char (point-min))
        (when (re-search-forward "^\\\\documentclass.*$" nil t)
          (end-of-line)
          (insert "\n" (mapconcat #'identity lines "\n") "\n"))))
    (buffer-string)))

(defun my/org-latex--strip-redundant-assignment-toc (latex info)
  "Remove Org's standalone TOC when the `assignment' class already provides one."
  (if (string= (plist-get info :latex-class) "assignment")
      (replace-regexp-in-string
       "\\\\maketitle[[:space:]\n\r]*\\\\tableofcontents[[:space:]\n\r]*"
       "\\\\maketitle\n"
       latex)
    latex))

(defun my/org-latex-template-advice (orig contents info)
  "Force Org LaTeX export through XeLaTeX-friendly Chinese defaults."
  (my/org-latex--ensure-embedded-class-registered info)
  (my/org-latex--inject-export-preamble
   (my/org-latex--strip-redundant-assignment-toc
    (my/org-latex--inject-class-commands
     (my/org-latex--inject-embedded-class
      (funcall orig contents info)
      info)
     info)
    info)))

(defun my/org-latex--available-class-names (&optional info)
  "Return available LaTeX class names for Org export INFO."
  (let (classes)
    (dolist (dir (my/org-latex--class-search-directories info))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir nil "\\.cls\\'"))
          (push (file-name-sans-extension file) classes))))
    (dolist (entry my/org-latex-embedded-class-alist)
      (push (car entry) classes))
    (setq classes (delete-dups (delq nil classes)))
    (sort classes #'string-lessp)))

(defun my/org-latex--default-class (&optional info)
  "Return the preferred LaTeX class name for Org export INFO."
  (let ((explicit (my/org-latex--keyword-value "LATEX_CLASS")))
    (or explicit
        (and (my/org-latex--find-class-file my/org-latex-default-class-name info)
             my/org-latex-default-class-name)
        my/org-latex-default-class-name)))

(defun my/org-latex--read-class (&optional info)
  "Read a LaTeX class name for Org export INFO."
  (let* ((default-class (my/org-latex--default-class info))
         (choices (or (my/org-latex--available-class-names info)
                      (list default-class))))
    (completing-read
     (format "LaTeX class (default %s): " default-class)
     choices nil t nil nil default-class)))

(defun my/org-latex--export-ext-plist (class)
  "Return export plist for LaTeX CLASS."
  `(:latex-class ,class :latex-compiler "xelatex"))

(defun my/org-latex--export-to-tex-with-class (class)
  "Export current Org buffer to LaTeX using CLASS."
  (let ((outfile (org-export-output-file-name ".tex" nil)))
    (org-export-to-file 'latex outfile nil nil nil nil
                        (my/org-latex--export-ext-plist class))))

(defun my/org-latex--latexmk-command (texfile)
  "Return (PROJECT-ROOT . COMMAND) used to compile TEXFILE with latexmk."
  (let* ((project-root (my/org-latex--project-root))
         (relative-texfile (file-relative-name texfile project-root))
         (command
          (format "%s -xelatex -file-line-error -synctex=1 -interaction=nonstopmode %s"
                  (my/shell-command-executable "latexmk")
                  (shell-quote-argument relative-texfile))))
    (cons project-root command)))

(defun my/org-latex--compile-tex-in-project (texfile)
  "Compile TEXFILE to PDF with latexmk in the current project root."
  (when (fboundp 'my/refresh-environment-from-shell)
    (my/refresh-environment-from-shell))
  (pcase-let* ((`(,project-root . ,command)
                (my/org-latex--latexmk-command texfile))
               (default-directory project-root))
    (compilation-start command 'compilation-mode
                       (lambda (_mode)
                         (format "*Org LaTeX Export: %s*"
                                 (file-name-nondirectory texfile))))))

(defun my/org-latex-export-to-tex (&optional class)
  "Export current Org buffer to LaTeX using CLASS or the default class."
  (interactive)
  (let* ((resolved-class (or class (my/org-latex--default-class)))
         (texfile (my/org-latex--export-to-tex-with-class resolved-class)))
    (message "Exported %s with class %s" texfile resolved-class)
    texfile))

(defun my/org-latex-export-to-tex-with-class (class)
  "Prompt for CLASS and export current Org buffer to LaTeX."
  (interactive (list (my/org-latex--read-class)))
  (my/org-latex-export-to-tex class))

(defun my/org-latex-export-to-pdf (&optional class)
  "Export current Org buffer to PDF using CLASS or the default class."
  (interactive)
  (let* ((resolved-class (or class (my/org-latex--default-class)))
         (texfile (my/org-latex--export-to-tex-with-class resolved-class))
         (pdf-file (concat (file-name-sans-extension texfile) ".pdf")))
    (my/org-latex--compile-tex-in-project texfile)
    (message "Exporting %s with class %s via project latexmk" pdf-file resolved-class)
    pdf-file))

(defun my/org-latex-export-to-pdf-with-class (class)
  "Prompt for CLASS and export current Org buffer to PDF."
  (interactive (list (my/org-latex--read-class)))
  (my/org-latex-export-to-pdf class))

(defun my/latex-preview--preferred-math-font-file ()
  "Return the first preferred math-font file available on this machine."
  (let ((candidates
         (list (expand-file-name "~/Library/Fonts/GFSNeohellenicMath.otf")
               (expand-file-name "~/Library/Fonts/STIXTwoMath-Regular.ttf")
               (expand-file-name "~/Library/Fonts/LibertinusMath-Regular.otf")
               "/System/Library/Fonts/Supplemental/STIXTwoMath.otf")))
    (or
     (catch 'font
       (dolist (candidate candidates)
         (when (file-exists-p candidate)
           (throw 'font candidate))))
     (when (executable-find "kpsewhich")
       (let ((lm-math
              (string-trim
               (shell-command-to-string "kpsewhich latinmodern-math.otf 2>/dev/null"))))
         (unless (string-empty-p lm-math)
           lm-math))))))

(defun my/latex-preview-math-font-line ()
  "Return the `\\setmathfont' line used in preview snippet headers."
  (if-let* ((font-file (my/latex-preview--preferred-math-font-file)))
      (format "\\setmathfont[Path=%s]{%s}"
              (file-name-as-directory (file-name-directory font-file))
              (file-name-nondirectory font-file))
    (format "\\setmathfont{%s}" my/latex-preview-math-font)))

(defvar-local my/org-latex--preview-timer nil)
(defvar-local my/org-latex--last-preview-range nil)
(defvar-local my/org-latex--render-queue nil)
(defvar-local my/org-latex--render-running 0)
(defvar-local my/org-latex--render-processes nil)
(defvar-local my/org-latex--pending-renders nil)
(defvar my/org-latex--allow-native-preview nil)

(defun my/org-latex--visible-range ()
  "Return (beg . end) for the current window's visible range."
  (cons (window-start) (window-end nil t)))

(defun my/org-latex--range-similar-p (r1 r2)
  "Return non-nil if ranges R1 and R2 are similar enough to skip re-preview."
  (when (and r1 r2)
    (let ((b1 (car r1)) (e1 (cdr r1))
          (b2 (car r2)) (e2 (cdr r2)))
      (let* ((span (max 1 (- e2 b2)))
             (tol  (max 200 (/ span 6))))
        (and (<= (abs (- b1 b2)) tol)
             (<= (abs (- e1 e2)) tol))))))

(defun my/org-latex--ensure-state ()
  "Initialize async preview state for the current Org buffer."
  (unless (hash-table-p my/org-latex--pending-renders)
    (setq my/org-latex--pending-renders (make-hash-table :test 'equal))))

(defun my/org-latex--preview-base-directory ()
  "Return the directory used to store Org preview images."
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (base (if (or (not file) (file-remote-p file))
                   temporary-file-directory
                 default-directory)))
    (file-name-as-directory (file-truename base))))

(defun my/org-latex--make-waiter (spec)
  "Build a waiter from fragment SPEC."
  (list :beg (copy-marker (plist-get spec :beg))
        :end (copy-marker (plist-get spec :end) t)
        :value (plist-get spec :value)))

(defun my/org-latex--release-waiters (job)
  "Release marker resources tracked by JOB."
  (dolist (waiter (plist-get job :waiters))
    (set-marker (plist-get waiter :beg) nil)
    (set-marker (plist-get waiter :end) nil)))

(defun my/org-latex--waiter-present-p (job spec)
  "Return non-nil when JOB already tracks SPEC."
  (catch 'found
    (dolist (waiter (plist-get job :waiters))
      (let ((beg (plist-get waiter :beg))
            (end (plist-get waiter :end)))
        (when (and (marker-buffer beg)
                   (marker-buffer end)
                   (= (marker-position beg) (plist-get spec :beg))
                   (= (marker-position end) (plist-get spec :end))
                   (equal (plist-get waiter :value) (plist-get spec :value)))
          (throw 'found t))))
    nil))

(defun my/org-latex--add-waiter (job spec)
  "Attach SPEC to JOB unless it is already tracked."
  (unless (my/org-latex--waiter-present-p job spec)
    (setf (plist-get job :waiters)
          (cons (my/org-latex--make-waiter spec)
                (plist-get job :waiters)))))

(defun my/org-latex--place-preview (beg end value file imagetype)
  "Overlay FILE as preview between BEG and END when VALUE is unchanged."
  (when (and (file-exists-p file)
             (< beg end)
             (<= end (point-max))
             (string= (buffer-substring-no-properties beg end) value))
    (org-clear-latex-preview beg end)
    (org--make-preview-overlay beg end file imagetype)))

(defun my/org-latex--place-waiter-preview (waiter file imagetype)
  "Place preview FILE for WAITER using IMAGETYPE."
  (let ((beg-marker (plist-get waiter :beg))
        (end-marker (plist-get waiter :end)))
    (when (and (marker-buffer beg-marker)
               (marker-buffer end-marker)
               (eq (marker-buffer beg-marker) (current-buffer)))
      (let ((beg (marker-position beg-marker))
            (end (marker-position end-marker)))
        (when (and beg end)
          (my/org-latex--place-preview
           beg end (plist-get waiter :value) file imagetype))))))

(defun my/org-latex--fragment-spec (beg end value)
  "Return render metadata for LaTeX fragment VALUE between BEG and END."
  (save-excursion
    (goto-char beg)
    (let* ((processing-type org-preview-latex-default-process)
           (processing-info
            (or (cdr (assq processing-type org-preview-latex-process-alist))
                (user-error "Unknown Org LaTeX preview process: %s" processing-type)))
           (dir (my/org-latex--preview-base-directory))
           (prefix (concat org-preview-latex-image-directory "org-ltximg"))
           (face (or (face-at-point nil t) 'default))
           (fg
            (let ((color (plist-get org-format-latex-options :foreground)))
              (cond
               ((eq color 'auto) (face-attribute face :foreground nil 'default))
               ((eq color 'default) (face-attribute 'default :foreground nil))
               (t color))))
           (bg
            (let ((color (plist-get org-format-latex-options :background)))
              (cond
               ((eq color 'auto) (face-attribute face :background nil 'default))
               ((eq color 'default) (face-attribute 'default :background nil))
               (t color))))
           (hash (sha1 (prin1-to-string
                        (list org-format-latex-header
                              nil
                              nil
                              org-format-latex-options
                              t value fg bg))))
           (imagetype (or (plist-get processing-info :image-output-type) "png"))
           (movefile (format "%s_%s.%s"
                             (expand-file-name prefix dir)
                             hash
                             imagetype))
           (options
            (org-combine-plists
             org-format-latex-options
             `(:foreground ,fg :background ,bg))))
      (list :beg beg
            :end end
            :value value
            :dir dir
            :file movefile
            :imagetype imagetype
            :options options
            :processing-type processing-type))))

(defun my/org-latex--collect-fragments (beg end)
  "Collect LaTeX fragments between BEG and END."
  (let ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
        fragments)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward math-regexp end t)
        (let* ((context (org-element-context))
               (type (org-element-type context)))
          (when (memq type '(latex-environment latex-fragment))
            (let* ((frag-beg (org-element-begin context))
                   (frag-end (save-excursion
                               (goto-char (org-element-end context))
                               (skip-chars-backward " \r\t\n")
                               (point)))
                   (value (org-element-property :value context)))
              (push (my/org-latex--fragment-spec frag-beg frag-end value) fragments)
              (goto-char (max (point) frag-end))))))
      (nreverse fragments))))

(defun my/org-latex--cleanup-job-files (job)
  "Delete temporary files created for JOB."
  (let ((texfilebase (plist-get job :texfilebase)))
    (when texfilebase
      (dolist (ext (plist-get job :post-clean))
        (let ((target (concat texfilebase ext)))
          (when (file-exists-p target)
            (delete-file target)))))))

(defun my/org-latex--prepare-render (job)
  "Materialize the external render command for JOB."
  (let* ((processing-type (plist-get job :processing-type))
         (processing-info (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (image-output-type (plist-get processing-info :image-output-type))
         (post-clean (or (plist-get processing-info :post-clean)
                         '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                           ".svg" ".png" ".jpg" ".jpeg" ".out")))
         (latex-header
          (or (plist-get processing-info :latex-header)
              (let ((org-latex-default-packages-alist nil)
                    (org-latex-packages-alist nil))
                (org-latex-make-preamble
                 (org-export-get-environment (org-export-get-backend 'latex))
                 org-format-latex-header
                 'snippet))))
         (latex-header
          (if (listp latex-header)
              (mapconcat #'identity latex-header "")
            latex-header))
         (latex-compiler (plist-get processing-info :latex-compiler))
         (tmpdir (file-name-as-directory (file-truename temporary-file-directory)))
         (texfilebase (make-temp-name (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (options (plist-get job :options))
         (scale (* (car image-size-adjust)
                   (or (plist-get options :scale) 1.0)))
         (dpi (* scale (if (display-graphic-p) (org--get-display-dpi) 140.0)))
         (fg (or (plist-get options :foreground) "Black"))
         (bg (or (plist-get options :background) "Transparent"))
         (image-converter
          (or (and (string= bg "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (snippet (copy-sequence (plist-get job :value))))
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
        (setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (setq bg (cond
              ((eq bg 'default) (org-latex-color :background))
              ((string= bg "Transparent") nil)
              (t (org-latex-color-format bg))))
    (if (string-suffix-p "\n" snippet)
        (aset snippet (1- (length snippet)) ?%)
      (setq snippet (concat snippet "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" fg "}%\n"
              (if bg
                  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
                          "\n\\pagecolor{bg}%\n")
                "")
              "\n{\\color{fg}\n"
              snippet
              "\n}\n"
              "\n\\end{document}\n"))
    (let* ((err-msg
            (format "Please adjust `%s' part of `org-preview-latex-process-alist'."
                    processing-type))
           (image-input-file (concat texfilebase "." image-input-type))
           (converter-spec
            `((?D . ,(shell-quote-argument (format "%s" dpi)))
              (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))
           (command
            (let ((default-directory (plist-get job :dir)))
              (mapconcat
               #'identity
               (append
                (org-compile-file-commands texfile latex-compiler image-input-type nil err-msg)
                (org-compile-file-commands image-input-file image-converter image-output-type
                                           converter-spec err-msg))
               " && "))))
      (setf (plist-get job :command) command)
      (setf (plist-get job :texfilebase) texfilebase)
      (setf (plist-get job :image-output-file) (concat texfilebase "." image-output-type))
      (setf (plist-get job :post-clean) post-clean)
      job)))

(defun my/org-latex--render-sentinel (process _event)
  "Finalize PROCESS for an async Org LaTeX preview render."
  (when (memq (process-status process) '(exit signal))
    (let* ((buffer (process-get process 'my/org-latex-buffer))
           (job (process-get process 'my/org-latex-job))
           (target (plist-get job :file))
           (success (and (eq (process-status process) 'exit)
                         (= (process-exit-status process) 0)
                         (file-exists-p (plist-get job :image-output-file)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq my/org-latex--render-processes
                (delq process my/org-latex--render-processes))
          (setq my/org-latex--render-running
                (max 0 (1- my/org-latex--render-running)))
          (when (hash-table-p my/org-latex--pending-renders)
            (remhash target my/org-latex--pending-renders))
          (when success
            (make-directory (file-name-directory target) t)
            (copy-file (plist-get job :image-output-file) target t)
            (dolist (waiter (plist-get job :waiters))
              (my/org-latex--place-waiter-preview waiter target
                                                  (plist-get job :imagetype))))
          (unless success
            (message "[org-latex] Preview failed for %s"
                     (file-name-nondirectory target)))
          (my/org-latex--pump-render-queue)))
      (my/org-latex--release-waiters job)
      (my/org-latex--cleanup-job-files job))))

(defun my/org-latex--start-render (job)
  "Start an async render JOB for the current buffer."
  (let* ((buffer (current-buffer))
         (job (my/org-latex--prepare-render job))
         (default-directory (plist-get job :dir))
         (log-buffer (get-buffer-create "*Org Async LaTeX Preview*"))
         (process
          (make-process
           :name (format "org-latex-preview-%s"
                         (substring (sha1 (plist-get job :file)) 0 8))
           :buffer log-buffer
           :command (list shell-file-name shell-command-switch
                          (plist-get job :command))
           :noquery t
           :sentinel #'my/org-latex--render-sentinel)))
    (process-put process 'my/org-latex-buffer buffer)
    (process-put process 'my/org-latex-job job)
    (push process my/org-latex--render-processes)))

(defun my/org-latex--pump-render-queue ()
  "Start queued Org LaTeX renders up to the concurrency limit."
  (while (and my/org-latex--render-queue
              (< my/org-latex--render-running my/org-latex-preview-max-concurrency))
    (let ((job (pop my/org-latex--render-queue)))
      (setq my/org-latex--render-running (1+ my/org-latex--render-running))
      (condition-case err
          (my/org-latex--start-render job)
        (error
         (setq my/org-latex--render-running
               (max 0 (1- my/org-latex--render-running)))
         (when (hash-table-p my/org-latex--pending-renders)
           (remhash (plist-get job :file) my/org-latex--pending-renders))
         (my/org-latex--release-waiters job)
         (my/org-latex--cleanup-job-files job)
         (message "[org-latex] %s" (error-message-string err)))))))

(defun my/org-latex--enqueue-fragment (spec)
  "Place or queue preview work described by SPEC."
  (let ((target (plist-get spec :file)))
    (if (file-exists-p target)
        (my/org-latex--place-preview
         (plist-get spec :beg)
         (plist-get spec :end)
         (plist-get spec :value)
         target
         (plist-get spec :imagetype))
      (progn
        (make-directory (file-name-directory target) t)
        (my/org-latex--ensure-state)
        (let ((job (gethash target my/org-latex--pending-renders)))
          (if job
              (my/org-latex--add-waiter job spec)
            (setq job (list :dir (plist-get spec :dir)
                            :file target
                            :imagetype (plist-get spec :imagetype)
                            :options (plist-get spec :options)
                            :processing-type (plist-get spec :processing-type)
                            :value (plist-get spec :value)
                            :waiters (list (my/org-latex--make-waiter spec))))
            (puthash target job my/org-latex--pending-renders)
            (setq my/org-latex--render-queue
                  (nconc my/org-latex--render-queue (list job)))
            (my/org-latex--pump-render-queue)))))))

(defun my/org-latex--preview-range (beg end)
  "Queue LaTeX preview work between BEG and END."
  (dolist (spec (my/org-latex--collect-fragments beg end))
    (my/org-latex--enqueue-fragment spec)))

(defun my/org-latex--section-range ()
  "Return the current Org section range as (BEG . END)."
  (cons (if (org-before-first-heading-p) (point-min)
          (save-excursion
            (org-with-limited-levels (org-back-to-heading t) (point))))
        (org-with-limited-levels (org-entry-end-position))))

(defun my/org-latex-cancel-pending-renders ()
  "Cancel all queued and running async LaTeX preview renders."
  (interactive)
  (when (timerp my/org-latex--preview-timer)
    (cancel-timer my/org-latex--preview-timer))
  (dolist (process my/org-latex--render-processes)
    (when (process-live-p process)
      (delete-process process)))
  (setq my/org-latex--preview-timer nil
        my/org-latex--last-preview-range nil
        my/org-latex--render-processes nil
        my/org-latex--render-queue nil
        my/org-latex--render-running 0)
  (when (hash-table-p my/org-latex--pending-renders)
    (maphash (lambda (_key job)
               (my/org-latex--release-waiters job))
             my/org-latex--pending-renders)
    (clrhash my/org-latex--pending-renders)))

(defun my/org-latex-preview-command (&optional arg)
  "Asynchronously preview Org LaTeX fragments like `org-latex-preview'."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ((and untrusted-content (not org--latex-preview-when-risky)) nil)
   ((equal arg '(64))
    (my/org-latex-cancel-pending-renders)
    (org-clear-latex-preview (point-min) (point-max))
    (message "LaTeX previews removed from buffer"))
   ((equal arg '(16))
    (my/org-latex--preview-range (point-min) (point-max))
    (message "Queueing LaTeX previews for buffer..."))
   ((equal arg '(4))
    (pcase-let ((`(,beg . ,end) (if (use-region-p)
                                    (cons (region-beginning) (region-end))
                                  (my/org-latex--section-range))))
      (my/org-latex-cancel-pending-renders)
      (org-clear-latex-preview beg end)
      (message "LaTeX previews removed")))
   ((use-region-p)
    (my/org-latex--preview-range (region-beginning) (region-end))
    (message "Queueing LaTeX previews for region..."))
   ((let ((datum (org-element-context)))
      (and (org-element-type-p datum '(latex-environment latex-fragment))
           (let ((beg (org-element-begin datum))
                 (end (org-element-end datum)))
             (if (org-clear-latex-preview beg end)
                 (message "LaTeX preview removed")
               (my/org-latex--preview-range beg end)
               (message "Queueing LaTeX preview..."))
             t))))
   (t
    (pcase-let ((`(,beg . ,end) (my/org-latex--section-range)))
      (my/org-latex--preview-range beg end)
      (message "Queueing LaTeX previews for section...")))))

(defun my/org-latex-preview-advice (orig &optional arg)
  "Route `org-latex-preview' through the async renderer in Org buffers."
  (if (or my/org-latex--allow-native-preview
          (not (derived-mode-p 'org-mode)))
      (funcall orig arg)
    (my/org-latex-preview-command arg)))

(defun my/org-latex-preview-visible-now ()
  "Preview visible Org LaTeX fragments asynchronously."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((range (my/org-latex--visible-range))
           (beg (car range))
           (end (cdr range))
           (force (called-interactively-p 'interactive)))
      (when (and (or force
                     (> (- end beg) my/org-latex-preview-min-chars))
                 (or force
                     (not (my/org-latex--range-similar-p
                           my/org-latex--last-preview-range
                           range))))
        (setq my/org-latex--last-preview-range range)
        (my/org-latex--preview-range beg end)))))

(defun my/org-latex-preview-visible-debounced ()
  "Debounced preview of visible area after scrolling stops."
  (when (derived-mode-p 'org-mode)
    (when (timerp my/org-latex--preview-timer)
      (cancel-timer my/org-latex--preview-timer))
    (setq my/org-latex--preview-timer
          (run-with-idle-timer my/org-latex-preview-idle-delay nil
                               #'my/org-latex-preview-visible-now))))

(defun my/org-latex-cleanup-scroll-preview ()
  "Stop async scroll-preview state in the current Org buffer."
  (interactive)
  (my/org-latex-cancel-pending-renders))

(defun my/org-latex-enable-scroll-preview ()
  "Enable on-demand LaTeX preview for visible area after scrolling."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (my/org-latex--ensure-state)
    (add-hook 'window-scroll-functions (lambda (_win _start) (my/org-latex-preview-visible-debounced)) nil t)
    (add-hook 'window-size-change-functions (lambda (_frame) (my/org-latex-preview-visible-debounced)) nil t)
    (add-hook 'kill-buffer-hook #'my/org-latex-cleanup-scroll-preview nil t)))

(add-hook 'org-mode-hook #'my/org-latex-enable-scroll-preview)

;; 手动刷新绑定
(with-eval-after-load 'org
  (advice-add 'org-latex-preview :around #'my/org-latex-preview-advice)
  (define-key org-mode-map (kbd "C-c C-x C-l") #'my/org-latex-preview-command)
  (define-key org-mode-map (kbd "C-c C-x v") #'my/org-latex-preview-visible-now))

(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "xelatex")
  (setq org-latex-default-class my/org-latex-default-class-name)
  (unless (assoc my/org-latex-default-class-name org-latex-classes)
    (add-to-list 'org-latex-classes
                 `(,my/org-latex-default-class-name
                   ,(format "\\documentclass{%s}" my/org-latex-default-class-name)
                   ,@my/org-latex-default-sectioning)))
  (setq org-latex-default-packages-alist
        (copy-tree my/org-latex-export-default-packages-alist))
  (setq org-latex-packages-alist nil)
  (advice-add 'org-latex-template :around #'my/org-latex-template-advice))

(with-eval-after-load 'org
  (setq org-latex-default-packages-alist
        (copy-tree my/org-latex-export-default-packages-alist))
  (setq org-latex-packages-alist nil)

  ;; 2. 定义处理程序 (保持不变)
  (let ((tool (expand-file-name "tools/org-xdvisvgm-hires" user-emacs-directory)))
    (add-to-list 'org-preview-latex-process-alist
                 `(xdvisvgm-hires-script
                   :programs ("xelatex" "dvisvgm")
                   :description "xelatex -> xdv -> (dvisvgm via script) -> svg"
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.0 . 1.0)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -halt-on-error -output-directory %o %f")
                   :image-converter (,(format "%s %%f %%O" (shell-quote-argument tool))))))

  (setq org-preview-latex-default-process 'xdvisvgm-hires-script)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; 3. 极简 Header (确保没有占位符)
  (setq org-format-latex-header
        (format "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{fontspec}
\\usepackage{unicode-math}
%s
\\pagestyle{empty}
"
                (my/latex-preview-math-font-line))))


;; External App Links (Zotero, MarginNote)
(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
    :follow (lambda (path)
              (let ((url (concat "zotero:" path))
                    (command (if (eq system-type 'darwin) "open" "xdg-open")))
                (start-process "zotero-opener" nil command url))))

  (let ((marginnote-link-types
         '("marginnote1app" "marginnote2app" "marginnote3app" "marginnote4app")))
    (dolist (type marginnote-link-types)
      (org-link-set-parameters
       type
       :follow
       (lambda (path)
         (if (eq system-type 'darwin)
             (let ((url (concat "marginnote4app:" path)))
               (start-process "marginnote" nil "open" url))
           (message "[org] MarginNote link only supported on macOS (got %s)" system-type)))))))

(require 'org-tempo) 


(provide 'init-org)
;;; init-org.el ends here

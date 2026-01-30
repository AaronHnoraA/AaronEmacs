;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Refactored Org Configuration (Fixed Version).
;; 1. Fixed 'void-variable title-bg' bug in special block rendering.
;; 2. Full Org-Modern integration (Buttons, Icons, Progress bars).
;; 3. Includes Org-Roam fixes and Olivetti auto-toggle.

;;; Code:

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

;; Ensure core directories exist
(make-directory my-org-root t)
(make-directory my-org-roam-dir t)
(make-directory my-org-daily-dir t)

;;; ----------------------------------------------------------------------------
;;; 2. Org Core Configuration (核心设置)
;;; ----------------------------------------------------------------------------

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)        ; 自动换行
         (org-mode . org-indent-mode))        ; 缩进模式
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-q" . counsel-org-tag))
  
  :custom-face
  ;; --- Heading Typography (标题排版) ---
  (org-document-title ((t (:height 1.75 :weight bold :inherit variable-pitch))))
  (org-level-1 ((t (:height 1.40 :weight bold :inherit variable-pitch))))
  (org-level-2 ((t (:height 1.35 :weight bold :inherit variable-pitch))))
  (org-level-3 ((t (:height 1.30 :weight bold :inherit variable-pitch))))
  (org-level-4 ((t (:height 1.25 :weight bold :inherit variable-pitch))))
  (org-level-5 ((t (:height 1.20 :weight bold :inherit variable-pitch))))
  (org-level-6 ((t (:height 1.15 :weight bold :inherit variable-pitch))))
  (org-level-7 ((t (:height 1.10 :weight bold :inherit variable-pitch))))
  (org-level-8 ((t (:height 1.05 :weight bold :inherit variable-pitch))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))

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
   '(("TODO"      . (:foreground "#ff6c6b" :weight bold))
     ("NEXT"      . (:foreground "#98be65" :weight bold))
     ("HOLD"      . (:foreground "#feb24c" :weight bold))
     ("WIP"       . (:foreground "#0098dd" :weight bold))
     ("WAIT"      . (:foreground "#ecbe7b" :weight bold))
     ("DONE"      . (:foreground "#51afef" :weight bold :strike-through t))
     ("CANCELLED" . (:foreground "#ff6480" :weight bold :strike-through t))))

  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-closed-keep-when-no-todo t)
  (org-log-repeat 'time)
  (org-priority-faces
   '((?A :foreground "red" :weight bold)
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
  (org-cite-global-bibliography pv/org-bibtex-files)
)

;;; ----------------------------------------------------------------------------
;;; 3. Modern UI & Aesthetics (UI美化 - 修复版)
;;; ----------------------------------------------------------------------------

;; 3.1 混合字体
(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode))

;; 3.2 写作专注模式 (自动居中)
(use-package olivetti
  :ensure t
  :diminish
  :bind ("<f8>" . olivetti-mode)
  :init 
  (setq olivetti-body-width 0.618) 
  :config
  (defun xs-toggle-olivetti-for-org ()
    "If current buffer is org and only one visible buffer, enable olivetti mode."
    (if (and (eq (length (window-list nil nil nil)) 1)
             (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode))
        (olivetti-mode 1)
      (olivetti-mode 0)))
  
  (add-hook 'org-mode-hook #'xs-toggle-olivetti-for-org)
  (add-hook 'window-configuration-change-hook #'xs-toggle-olivetti-for-org))

;; 3.3 表格对齐
(use-package valign
  :ensure t
  :hook (org-mode . valign-mode))

;; 3.4 Org Modern (全面增强版)
(use-package org-modern
  :ensure t
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
   '(("title"       . "➲")
     ("subtitle"    . "⮊")
     ("author"      . "💁")
     ("email"       . "📧")
     ("date"        . "📅")
     ("language"    . "🖹")
     ("options"     . "⛭")
     ("startup"     . "✲")
     ("macro"       . "Maps")
     ("bind"        . "Key")
     ("setupfile"   . "📝")
     ("downloaded"  . "⇊")
     ("attr_latex"  . "🄛")
     ("attr_html"   . "🄗")
     ("attr_org"    . "🄞")
     ("name"        . "🄝")
     ("caption"     . "🄒")
     ("results"     . "☰")
     ("print_bibliography" . "📚")))
  
  ;; 6. 复选框美化
  (org-modern-checkbox
   '((?X . "🗹")
     (?- . "⊟")
     (?\s . "□")))

  ;; 7. 其他装饰
  (org-modern-horizontal-rule t)
  (org-modern-block-name nil)
  (org-modern-todo nil)
  (org-modern-priority t)
  
  :config
  (setq-default line-spacing 0.1))

;; 3.5 Org Modern Indent
(unless (package-installed-p 'org-modern-indent)
  (package-vc-install
   '(org-modern-indent :url "https://github.com/jdtsmith/org-modern-indent.git")))
(use-package org-modern-indent
  :hook (org-mode . org-modern-indent-mode)
  :config
  (setq org-modern-indent-width 4))

;; 3.6 自动显示强调符
(unless (package-installed-p 'org-appear)
  (package-vc-install
   '(org-appear :url "https://github.com/awth13/org-appear.git")))
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; 3.7 优先级美化
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
  '((t :weight bold :height 1.05 :inherit default))
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
        ;; 这里的关键逻辑：渲染当前 Block 时，先清除这个区域内已有的 Overlay
        ;; 如果这是个内层 Block，这一步会把外层 Block 在这里的背景色“挖掉”
        ;; 从而实现完美的图层嵌套，而不是颜色混合
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
      ;; 1. [回溯修正]：防止 start 切在 Block 中间
      ;; 如果 JIT 起点在 Block 内部，尝试回退到 begin 处，保证该 Block 被完整处理
      (goto-char start)
      (if (re-search-backward "^[ \t]*#\\+begin_" nil t)
          (let ((el (org-element-at-point)))
            (when (and (eq (org-element-type el) 'special-block)
                       (> (org-element-property :end el) start))
              (setq start (org-element-property :begin el))))
        (goto-char start))
      
      ;; 2. 循环扫描
      (goto-char start)
      ;; 搜索所有的 #+begin_，即使它在另一个 Block 内部
      (while (re-search-forward "^[ \t]*#\\+begin_\\(\\w+\\)" end t)
        
        (let ((el (org-element-at-point)))
          (when (eq (org-element-type el) 'special-block)
            ;; 渲染它
            (my/org-prettify-element el)
            
            ;; ===========================================================
            ;; [重要]：这里不再执行 (goto-char (org-element-property :end el))
            ;; 我们让正则搜索自然向下进行。
            ;; 这样如果 Block 内部还有嵌套的 #+begin_，下一次循环就能捉到它。
            ;; ===========================================================
            ))))))

;; ===========================================================
;; 5. 激活机制
;; ===========================================================

(defun my/org-enable-jit-pretty-blocks ()
  "在当前 Buffer 启用 JIT 渲染机制。"
  (when (derived-mode-p 'org-mode)
    (jit-lock-register #'my/org-jit-prettify-blocks t)
    (jit-lock-refontify)))

(defun my/org-reset-overlays ()
  "调试用：强制清除所有 Overlay 并重绘。"
  (interactive)
  (remove-overlays (point-min) (point-max) 'my/org-pretty-block t)
  (jit-lock-refontify))

;; 添加 Hook
(add-hook 'org-mode-hook #'my/org-enable-jit-pretty-blocks)

;; 如果你是重新加载配置，运行下面这行来立即生效当前 buffer
;; (my/org-reset-overlays)

;;; ----------------------------------------------------------------------------
;;; 4. Agenda (日程管理)
;;; ----------------------------------------------------------------------------

(use-package org-agenda
  :ensure nil
  :bind ("C-c r" . my/reload-agenda)
  :init
  (setq org-agenda-files nil)
  (setq org-agenda-diary-file my-org-diary-file)
  :config
  (appt-activate 1)
  (defun my/reload-agenda ()
    "Reload all org files under root into agenda."
    (interactive)
    (let ((files (directory-files-recursively my-org-root "\\.org$")))
      (setq files (cl-remove-if (lambda (path) (string-match-p "/ltximg/" path)) files))
      (setq org-agenda-files files)
      (org-agenda-to-appt)
      (message "Agenda refreshed: %d files loaded." (length files))))

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

;;; ----------------------------------------------------------------------------
;;; 6. Org Roam (知识库)
;;; ----------------------------------------------------------------------------

(use-package org-roam
  :ensure t
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
  (org-roam-setup)
  (org-roam-db-autosync-mode 1)
  
  (defun pv/org-set-last-modified ()
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+last_modified:" nil t)
          (delete-region (point) (line-end-position))
          (insert (format " [%s]" (format-time-string "%Y-%m-%d %a %H:%M")))))))
  (add-hook 'before-save-hook #'pv/org-set-last-modified)

  (setq org-roam-capture-templates
        '(("m" "Math" plain "%?" :if-new (file+head "math/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :math:\n") :unnarrowed t)
          ("c" "CS" plain "%?" :if-new (file+head "CS/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :cs:\n") :unnarrowed t)
          ("q" "Quantum" plain "%?" :if-new (file+head "QC/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :qc:\n") :unnarrowed t)
          ("p" "Phil" plain "%?" :if-new (file+head "philosophy/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :phil:\n") :unnarrowed t)
          ("i" "Index" plain "%?" :if-new (file+head "index/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :index:\n") :unnarrowed t)
          ("r" "Paper" plain "%?" :if-new (file+head "papers/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :paper:\n") :unnarrowed t))))

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
  :hook (org-mode . org-fragtog-mode))

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

(defcustom my/org-latex-preview-idle-delay 0.3
  "Idle delay (seconds) before previewing visible region after scrolling."
  :type 'number
  :group 'my/org-latex-preview)

(defcustom my/org-latex-preview-min-chars 400
  "Minimum visible region size (chars) required to trigger preview.
Helps avoid tiny-window edge cases."
  :type 'integer
  :group 'my/org-latex-preview)

(defvar-local my/org-latex--preview-timer nil)
(defvar-local my/org-latex--last-preview-range nil)
(defvar-local my/org-latex--preview-in-progress nil)

(defun my/org-latex--visible-range ()
  "Return (beg . end) for the current window's visible range."
  (cons (window-start) (window-end nil t)))

(defun my/org-latex--range-similar-p (r1 r2)
  "Return non-nil if ranges R1 and R2 are similar enough to skip re-preview."
  (when (and r1 r2)
    (let ((b1 (car r1)) (e1 (cdr r1))
          (b2 (car r2)) (e2 (cdr r2)))
      ;; 允许小幅滚动不触发重复预览：阈值按 1/6 屏宽估算（很保守）
      (let* ((span (max 1 (- e2 b2)))
             (tol  (max 200 (/ span 6))))
        (and (<= (abs (- b1 b2)) tol)
             (<= (abs (- e1 e2)) tol))))))

(defun my/org-latex-preview-visible-now ()
  "Preview LaTeX fragments in visible area, with guardrails."
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (not my/org-latex--preview-in-progress))
    (let* ((range (my/org-latex--visible-range))
           (beg (car range))
           (end (cdr range)))
      (when (and (> (- end beg) my/org-latex-preview-min-chars)
                 (not (my/org-latex--range-similar-p my/org-latex--last-preview-range range)))
        (setq my/org-latex--preview-in-progress t)
        (setq my/org-latex--last-preview-range range)
        (unwind-protect
            (save-excursion
              ;; 只预览可见区域：用 region 触发 org-latex-preview 的 region 行为
              (goto-char beg)
              (push-mark end nil t)
              (activate-mark)
              (condition-case _err
                  (org-latex-preview)
                (error nil))
              (deactivate-mark))
          (setq my/org-latex--preview-in-progress nil))))))

(defun my/org-latex-preview-visible-debounced ()
  "Debounced preview of visible area after scrolling stops."
  (when (derived-mode-p 'org-mode)
    (when (timerp my/org-latex--preview-timer)
      (cancel-timer my/org-latex--preview-timer))
    (setq my/org-latex--preview-timer
          (run-with-idle-timer my/org-latex-preview-idle-delay nil
                               #'my/org-latex-preview-visible-now))))

;; 启用：对滚动、翻页等导致窗口内容变化的命令做“事后触发”
(defun my/org-latex-enable-scroll-preview ()
  "Enable on-demand LaTeX preview for visible area after scrolling."
  (interactive)
  (when (derived-mode-p 'org-mode)
    ;; 这些 hook 在窗口滚动/重绘后跑，配合 idle timer 达到“滚动停了才渲染”
    (add-hook 'window-scroll-functions (lambda (_win _start) (my/org-latex-preview-visible-debounced)) nil t)
    (add-hook 'window-size-change-functions (lambda (_frame) (my/org-latex-preview-visible-debounced)) nil t)))

(defun my/org-latex-disable-scroll-preview ()
  "Disable on-demand LaTeX preview after scrolling."
  (interactive)
  (remove-hook 'window-scroll-functions (lambda (_win _start) (my/org-latex-preview-visible-debounced)) t)
  (remove-hook 'window-size-change-functions (lambda (_frame) (my/org-latex-preview-visible-debounced)) t)
  (when (timerp my/org-latex--preview-timer)
    (cancel-timer my/org-latex--preview-timer))
  (setq my/org-latex--preview-timer nil))

(add-hook 'org-mode-hook #'my/org-latex-enable-scroll-preview)

;; 备用快捷键：手动刷新当前可见区域
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x v") #'my/org-latex-preview-visible-now))

(with-eval-after-load 'org
  (let ((tool (expand-file-name "tools/org-dvipng-hires" user-emacs-directory)))
    (add-to-list 'org-preview-latex-process-alist
                 `(dvipng-hires-script
                   :programs ("latex")
                   :description "latex -> dvi -> (dvipng+convert) -> png"
                   :image-input-type "dvi"
                   :image-output-type "svg"
                   :image-size-adjust (1.0 . 1.0)
                   :latex-compiler ("latex -interaction nonstopmode -halt-on-error -output-directory %o %f")
                   :image-converter (,(format "%s %%f %%O" (shell-quote-argument tool))))))
  (setq org-preview-latex-default-process 'dvipng-hires-script)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0)))

(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
    :follow (lambda (path)
              (let ((url (concat "zotero:" path))
                    (command (if (eq system-type 'darwin) "open" "xdg-open")))
                (start-process "zotero-opener" nil command url)))))


(with-eval-after-load 'org
  (let ((marginnote-link-types
         '("marginnote1app" "marginnote2app" "marginnote3app" "marginnote4app")))
    (dolist (type marginnote-link-types)
      (org-link-set-parameters
       type
       :follow
       (lambda (path)
         (if (eq system-type 'darwin)
             (let ((url (concat "marginnote4app:" path)))
               (start-process
                "marginnote"
                nil
                "open"
                url))
           (message
            "[org] MarginNote link only supported on macOS (got %s)"
            system-type)))))))

(require 'org-tempo) 

(provide 'init-org)
;;; init-org.el ends here

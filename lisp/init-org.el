;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

;;; ----------------------------------------------------------------------------
;;; 1. Global Variables & Paths
;;; ----------------------------------------------------------------------------

;; Define the root org directory first
(defvar my-org-root (file-truename "~/HC/Org/"))

;; specific files/folders
(defvar pv/org-refile-file (expand-file-name "refile.org" my-org-root))
(defvar pv/org-bibtex-dir (expand-file-name "references/" my-org-root))
(defvar pv/org-bibtex-files (list (expand-file-name "references.bib" pv/org-bibtex-dir)))

;; Ensure directories exist (optional safety)
(make-directory my-org-root t)
(make-directory (expand-file-name "roam" my-org-root) t)

;;; ----------------------------------------------------------------------------
;;; 2. Org Core (Merged)
;;; ----------------------------------------------------------------------------
(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)    ; Soft wrapping
         (org-mode . org-cdlatex-mode))   ; Turn on CDLaTeX
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-q" . counsel-org-tag))   ; Assuming you use counsel/ivy
  :config
  ;; Load languages for Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (dot        . t)
     (emacs-lisp . t)
     (eshell     . t)
     (python     . t)
     (shell      . t)))

  :custom
  ;; --- Directories & Files ---
  (org-directory my-org-root)
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-archive-location "%s_archive::datetree/")

  ;; --- Appearance & formatting ---
  (org-startup-indented t)
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  (org-use-sub-superscripts '{})        ; Require {} for sub/super script
  (org-image-actual-width nil)          ; Allow resizing
  (org-startup-with-inline-images t)
  (org-display-remote-inline-images t)
  (org-imenu-depth 4)

  ;; --- Navigation & Editing ---
  (org-return-follows-link nil)         ; C-c C-o to open links, Enter for newline
  (org-clone-delete-id t)
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  
  ;; --- TODO & Task Management ---
  (org-todo-keywords
   '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("TODO"      :foreground "#FF9800" :weight bold)
     ("HOLD"      :foreground "#feb24c" :weight bold)
     ("WIP"       :foreground "#0098dd" :weight bold)
     ("WAIT"      :foreground "#9f7efe" :weight bold)
     ("DONE"      :foreground "#50a14f" :weight bold)
     ("CANCELLED" :foreground "#ff6480" :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces
   '((?A :foreground "red")
     (?B :foreground "orange")
     (?C :foreground "yellow")))
  (org-closed-keep-when-no-todo t)
  (org-log-repeat 'time)

  ;; --- Properties & Columns ---
  (org-global-properties
   '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
     ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")

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
     ("GitLab" . "https://gitlab.com/")
     ("Google" . "https://google.com/search?q=")
     ("RFCs"   . "https://tools.ietf.org/html/")
     ("LWN"    . "https://lwn.net/Articles/")
     ("WG21"   . "https://wg21.link/")))

  ;; --- Citations (Built-in) ---
  (org-cite-global-bibliography pv/org-bibtex-files)

  ;; --- Capture Templates (Merged) ---
  ;; Note: I have organized your two sets of templates into a single structure
  (org-capture-use-agenda-date t)
  (org-capture-templates-contexts nil)
  )

;; 1. 定义根目录和子目录结构
(defvar my-daily-root (expand-file-name "daily/" my-org-root))
(defvar my-daily-subdirs '("idea" "inbox" "mail" "note" "meeting" "protocol" "uni" "life"))

;; 自动创建所有需要的子文件夹，防止报错
(make-directory my-daily-root t)
(dolist (dir my-daily-subdirs)
  (make-directory (expand-file-name dir my-daily-root) t))

;; 2. 核心辅助函数：询问文件名并生成路径
;; 格式：~/.org/daily/文件夹/输入名-时间.org
(defun my/get-daily-capture-path (subdir)
  "Prompt for a filename, append date, and return full path."
  (let* ((name (read-string "File Name (slug): ")) ;; 询问文件名
         (slug (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (downcase name))) ;; 简单的 slug 处理
         (date (format-time-string "%Y%m%d")) ;; 日期后缀
         (fname (format "%s-%s.org" slug date))) ;; 拼接：名字-日期.org
    (expand-file-name fname (expand-file-name subdir my-daily-root))))

(setq org-capture-templates
      '(
        ;; --- 核心分类 (Core) ---
        
        ("i" "Idea (灵感)" plain 
         (file (lambda () (my/get-daily-capture-path "idea")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :idea:\n\n* Idea:\n%?\n"
         :unnarrowed t)

        ("b" "Inbox (收集箱)" plain 
         (file (lambda () (my/get-daily-capture-path "inbox")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :inbox:\n\n* Details\n%?\n"
         :unnarrowed t)

        ("m" "Mail (邮件/通信)" plain 
         (file (lambda () (my/get-daily-capture-path "mail")))
         "#+title: Mail: %^{Subject}\n#+date: %u\n#+filetags: :mail:\n\n* To/From: %^{Recipient}\n* Status: TODO\n\n%?\n"
         :unnarrowed t)

        ("n" "Note (随手记)" plain 
         (file (lambda () (my/get-daily-capture-path "note")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :note:\n\n%?\n"
         :unnarrowed t)

        ("t" "Meeting (会议)" plain 
         (file (lambda () (my/get-daily-capture-path "meeting")))
         "#+title: Meeting: %^{Topic}\n#+date: %u\n#+filetags: :meeting:\n\n* Participants: %^{Who}\n* Time: %^T\n\n* Agenda\n%?\n"
         :unnarrowed t)

        ;; Protocol 通常由浏览器触发，这里保留手动触发作为备份
        ;; 这里的逻辑稍有不同，如果通过 org-protocol 抓取，通常会有专门的设置
        ("p" "Protocol (网页/链接)" plain 
         (file (lambda () (my/get-daily-capture-path "protocol")))
         "#+title: %:description\n#+source: %:link\n#+date: %u\n#+filetags: :protocol:\n\n* Summary\n%i\n%?\n"
         :unnarrowed t)

        ;; --- 学业与生活 (Life & Work) ---

        ("u" "Uni/Academic (学业)" plain 
         (file (lambda () (my/get-daily-capture-path "uni")))
         "#+title: %^{Course/Task}\n#+date: %u\n#+filetags: :uni:\n\n* Course: %^{Course Code|COMP|MATH|PHYS|PHIL}\n* Deadline: %^t\n\n* Requirements\n%?\n"
         :unnarrowed t)

        ("l" "Life/Admin (生活事务)" plain 
         (file (lambda () (my/get-daily-capture-path "life")))
         "#+title: %^{Task}\n#+date: %u\n#+filetags: :life:\n\n* Category: %^{Type|Finance|Shopping|Health|Travel}\n* Action\n%?\n"
         :unnarrowed t)
      ))

;;; ----------------------------------------------------------------------------
;;; 3. Org Agenda
;;; ----------------------------------------------------------------------------
(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c a" . org-agenda)     ;; 打开 Agenda 主界面
  ("C-c r" . my/reload-agenda) ;; 【新增】绑定手动刷新快捷键
  
  :init
  ;; 初始设为空，或者只包含 daily 入口，保证启动秒开
  (setq org-agenda-files nil)
  (setq org-agenda-diary-file (expand-file-name "diary.org" my-org-root))

  :config
  (appt-activate 1)

  ;; --- 【核心】手动刷新函数 ---
  (defun my/reload-agenda ()
    "手动扫描 root 下所有 org 文件更新 Agenda，并同步提醒。"
    (interactive)
    (let ((files (directory-files-recursively my-org-root "\\.org$")))
      ;; 过滤掉 ltximg 文件夹 (LaTeX 预览缓存)，防止污染日程
      (setq files (cl-remove-if (lambda (path) 
                                  (string-match-p "/ltximg/" path)) 
                                files))
      (setq org-agenda-files files)
      
      ;; 顺便同步一下系统提醒 (Appt)
      (org-agenda-to-appt)
      (message "已刷新 Agenda：加载了 %d 个文件 (包含 Roam 和 Daily)" (length files))))

  :custom
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-inhibit-startup t)
  (org-agenda-time-leading-zero t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1))


;; 定义任务状态流程
(setq org-todo-keywords
      '((sequence "TODO(t)"       ; 待办：还没开始
                  "NEXT(n)"       ; 下一步：现在就可以动手
                  "WAIT(w@/!)"    ; 等待：等回复/被阻塞 (记录时间戳)
                  "|"             ; 管道符右边是完成状态
                  "DONE(d!)"      ; 完成 (记录时间戳)
                  "CANCELLED(c@)"))) ; 取消 (强制写说明原因)

;; 给不同状态上色，一眼识别
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff6c6b" :weight bold))      ; 红色：待办
        ("NEXT" . (:foreground "#98be65" :weight bold))      ; 绿色：立即执行
        ("WAIT" . (:foreground "#ecbe7b" :weight bold))      ; 黄色：阻塞
        ("DONE" . (:foreground "#51afef" :weight bold :strike-through t))))


;; 优化 Agenda 显示格式
;; %-12c: 显示分类(Category)占12格
;; %-12t: 显示时间
;; %s: 任务标题
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c %s")
        (tags   . " %i %-12:c %s")
        (search . " %i %-12:c %s")))

;; 紧凑视图：不在 Agenda 里显示这一堆标签，因为我们已经用分类区分了
;; 保持界面清爽，鼠标放上去还是能看到的
(setq org-agenda-hide-tags-regexp ".")

;; 在日程视图中显示任务的“面包屑导航” (Breadcrumbs)
;; 这样你知道 "Review" 是属于 "Math/Linear Algebra" 还是 "Life/Shopping"
(setq org-agenda-show-outline-path t)


(setq org-agenda-custom-commands
      '(("o" "Overview / Dashboard"
         (
          ;; --- 板块 1: 必须关注的危机 (Deadline & Urgent) ---
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-overriding-header "⚡ Today's Schedule & Deadlines")))

          ;; --- 板块 2: 学业任务 (Uni) ---
          ;; 扫描所有打着 :uni: 标签，或者是 TODO 状态的任务
          (tags-todo "+uni/!TODO|NEXT"
                     ((org-agenda-overriding-header "🎓 University Tasks (Assignments & Exams)")))

          ;; --- 板块 3: 研究与思考 (Math/CS/QC) ---
          ;; 这里会把你 Roam 笔记里散落的 TODO 聚合起来
          (tags-todo "+math+cs+qc+research/!TODO|NEXT"
                     ((org-agenda-overriding-header "🔬 Research & Knowledge Gaps")))

          ;; --- 板块 4: 下一步行动 (Ready to Execute) ---
          ;; 所有标记为 NEXT 的任务，通常是琐事
          (todo "NEXT"
                ((org-agenda-overriding-header "🚀 Next Actions (Ready to go)")))

          ;; --- 板块 5: 阻塞中 (Waiting) ---
          (todo "WAIT"
                ((org-agenda-overriding-header "⏳ Waiting for others...")))
          
          ;; --- 板块 6: 收集箱 (Inbox) ---
          ;; 提醒你去整理那些还没归类的东西
          (tags "inbox"
                ((org-agenda-overriding-header "📥 Unprocessed Inbox Items")))
          )
         ;; 导出设置（可选）
         nil)))

;;; ----------------------------------------------------------------------------
;;; 4. Org Source Blocks
;;; ----------------------------------------------------------------------------
(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
         ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("python"    . python)
                        ("dot"    . graphviz-dot)
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh))))


;;; ----------------------------------------------------------------------------
;;; 5. Org Roam (Merged & Complete)
;;; ----------------------------------------------------------------------------
(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-directory (file-truename (expand-file-name "roam" org-directory)))
  (setq org-roam-v2-ack t)
  
  ;; Define keymap prefix
  (define-prefix-command 'org-roam-map)
  (global-set-key (kbd "C-c n") 'org-roam-map)
  
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n o" . org-id-get-create)
         ("C-c n l" . org-roam-buffer-toggle))
  
  :custom
  (org-roam-directory (file-truename (expand-file-name "roam" org-directory)))
  :config
  (org-roam-setup)
  ;; 强制走外部 sqlite3
  ;;(org-roam-database-connector 'sqlite3)
  (org-roam-db-autosync-mode 1)

  ;; --- Last Modified Timestamp Logic ---
  (defun pv/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun pv/org-has-time-file-property-p (property &optional anywhere)
    "Return position of property or -1 if exists but empty."
    (when-let ((pos (pv/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun pv/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble."
    (when-let ((pos (or pos (pv/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ") (forward-char) (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun pv/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (pv/org-set-time-file-property "last_modified")))

  ;; Hook the modification function to save
  (add-hook 'before-save-hook #'pv/org-set-last-modified)

  ;; --- Visuals & Preview ---
  ;; Refresh inline images after Roam buffer render
  (add-hook 'org-roam-buffer-postrender-functions
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (ignore-errors (org-display-inline-images))
                (ignore-errors (org-latex-preview '(64)))))))

;; UI Extension
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))



(setq org-roam-capture-templates
      '(
        ;; --- 学科基础层 (Base Concepts) ---
        ;; 自动归类到对应文件夹，文件名直接使用概念名 (Slug)，内容留白

        ("m" "Math Concept" plain
         "%?"
         :if-new (file+head "math/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :math:\n")
         :unnarrowed t)

        ("c" "CS Concept" plain
         "%?"
         :if-new (file+head "CS/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :cs:\n")
         :unnarrowed t)

        ("q" "Quantum Concept" plain
         "%?"
         :if-new (file+head "QC/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :qc:\n")
         :unnarrowed t)

        ("p" "Philosophy Concept" plain
         "%?"
         :if-new (file+head "philosophy/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :phil:\n")
         :unnarrowed t)

        ;; --- 知识融合层 (Integration) ---
        ;; Index 是跨学科的“集散地”，用来把 math/cs/qc 里的点连成线

        ("i" "Index (Integration)" plain
         "%?"
         :if-new (file+head "index/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :index:\n")
         :unnarrowed t)

        ("I" "Idea (Fleeting notes)" plain
         "%?"
         :if-new (file+head "idea/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :idea:\n")
         :unnarrowed t)

        ;; --- 外部输入层 (Source) ---
        ;; 论文依旧单独存放，保持纯洁性

        ("r" "Paper/Reference" plain
         "%?"
         :if-new (file+head "papers/${slug}.org"
                            "#+title: ${title}\n#+date: %u\n#+filetags: :paper:\n")
         :unnarrowed t)
      ))

(unless (package-installed-p 'org-modern-indent)
  (package-vc-install
   '(org-modern-indent
     :url "https://github.com/jdtsmith/org-modern-indent.git"
     :rev :last-release)))
(use-package org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(defconst org-modern-indent-begin (propertize "┌"  'face 'org-modern-indent-bracket-line))
(defconst org-modern-indent-guide (propertize "│ " 'face 'org-modern-indent-bracket-line))
(defconst org-modern-indent-end   (propertize "└"  'face 'org-modern-indent-bracket-line))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; 自定义你的标题符号
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿")))
(unless (package-installed-p 'org-appear)
  (package-vc-install
   '(org-appear
     :url "https://github.com/awth13/org-appear.git"
     :rev :last-release)))
(require 'org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)
(use-package olivetti
  :ensure t)
(use-package mixed-pitch
  :ensure t
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))
(use-package valign
  :ensure t)
(add-hook 'org-mode-hook #'valign-mode)
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

(defun my/org-clean-ui ()
  "让 Org 的元数据不那么刺眼"
  ;; 1. 隐藏多余的强调符（/斜体/，*粗体* 两边的符号）
  (setq org-hide-emphasis-markers t)
  
  ;; 2. 让 #+BEGIN_SRC 这种元数据行变小、变淡
  (set-face-attribute 'org-meta-line nil :inherit 'shadow :height 0.8)
  (set-face-attribute 'org-block-begin-line nil :inherit 'shadow :height 0.8 :background nil)
  (set-face-attribute 'org-block-end-line nil :inherit 'shadow :height 0.8 :background nil)

  ;; 3. 给代码块加一个淡淡的背景色（类似 Notion）
  (set-face-attribute 'org-block nil :background (if (eq (frame-parameter nil 'background-mode) 'dark)
                                                     "#232323" ;; 深色模式背景
                                                   "#f5f5f5")) ;; 浅色模式背景
  ;; 4. 自动折叠 Properties 抽屉，眼不见心不烦
  (setq org-cycle-hide-drawer-startup t))

(add-hook 'org-mode-hook #'my/org-clean-ui)

(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))) ;; 用 emoji 代替 A/B/C


;;; ----------------------------------------------------------------------------
;;; 6. Bibliography & References
;;; ----------------------------------------------------------------------------
(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography pv/org-bibtex-files)
  (bibtex-completion-library-path (list pv/org-bibtex-dir))
  (bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process "open" nil 0 nil fpath))))

(use-package org-ref
  :ensure t
  :after org)

;;; ----------------------------------------------------------------------------
;;; 7. LaTeX & Math
;;; ----------------------------------------------------------------------------
(use-package cdlatex
  :ensure t
  ;; Hook is already handled in 'org' use-package above
  )

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;; Global LaTeX Preview Settings
(with-eval-after-load 'org
  (let ((tool (expand-file-name "tools/org-dvipng-hires" user-emacs-directory)))
    (add-to-list
     'org-preview-latex-process-alist
     `(dvipng-hires-script
       :programs ("latex")
       :description "latex -> dvi -> (dvipng+convert via script) -> png"
       :message "Need latex, dvipng, imagemagick, and org-dvipng-hires."
       :image-input-type "dvi"
       :image-output-type "svg"
       :image-size-adjust (1.0 . 1.0)
       :latex-compiler
       ("latex -interaction nonstopmode -halt-on-error -output-directory %o %f")
       :image-converter
       (,(format "%s %%f %%O" (shell-quote-argument tool))))))

  (setq org-preview-latex-default-process 'dvipng-hires-script)

  ;; 显示层先别再缩放，避免叠加
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.0)))




(require 'org-tempo)
(setq org-pretty-entities t)



(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
    :follow (lambda (path)
              (let ((url (concat "zotero:" path))
                    (command (if (eq system-type 'darwin) "open" "xdg-open")))
                (start-process "zotero-opener" nil command url)))))



;; ============================================================
;;  Org Special Blocks 卡片化美化 (Fix: org-indent & End line)
;; ============================================================

(with-eval-after-load 'org
  ;; 1. 定义颜色和标签映射
  (defvar my/org-special-block-styles
    '(;; --- 数学 (Math) ---
      ("definition" . (:label "定义" :color "#e0af68"))
      ("defn"       . (:label "定义" :color "#e0af68"))
      ("theorem"    . (:label "定理" :color "#9ece6a"))
      ("lemma"      . (:label "引理" :color "#7aa2f7"))
      ("cor"        . (:label "推论" :color "#bb9af7"))
      ("prop"       . (:label "命题" :color "#ff75a0"))
      ("property"   . (:label "性质" :color "#bb9af7"))
      ("proof"      . (:label "证明" :color "#565f89"))
      ;; --- 杂项 (Misc) ---
      ("example"    . (:label "例子" :color "#d08770"))
      ("attention"  . (:label "注意" :color "#f7768e"))
      ("note"       . (:label "注意" :color "#f7768e"))
      ("warning"    . (:label "警告" :color "#f7768e")))
    "Alist mapping block types to their display label and color.")

  ;; 2. 标题面的基本属性
  (defface my/org-block-title-face
    '((t :weight bold :height 1.05 :inherit default)) 
    "Face for the custom block title.")

  ;; 3. 核心辅助函数：获取 Org Indent 的前缀并拼接边框
  (defun my/org--make-merged-prefix (base-prefix color)
    "获取 org-indent 的原有缩进 (base-prefix)，并在其后拼接一根 colored 竖线"
    (let ((bar (propertize "▍ " 'face `(:foreground ,color :weight bold)))) ;; 竖条图案
      (if base-prefix
          ;; 如果有 org-indent 缩进，拼接在后面
          (concat base-prefix bar)
        ;; 如果没有缩进，直接返回竖条
        bar)))

  ;; 4. 核心渲染逻辑
  (defun my/org--pretty-special-blocks (&rest _)
    "Render special blocks as cards with background."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (remove-overlays (point-min) (point-max) 'my/org-pretty-block t)
        
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward "^[ \t]*#\\+begin_\\([a-zA-Z0-9_-]+\\)\\(.*\\)$" nil t)
            (let* ((type (downcase (match-string 1)))
                   (rest (string-trim (match-string 2)))
                   (config (cdr (assoc type my/org-special-block-styles))))
              
              (when config
                (let* ((label (plist-get config :label))
                       (color (plist-get config :color))
                       ;; 统一背景色：标题和内容都使用淡化的背景
                       (bg-color (color-darken-name color 85)) 
                       (title-bg bg-color) ;; 【修改点】标题背景与卡片背景一致
                       (beg-line (line-beginning-position))
                       (beg-line-end (line-end-position))
                       ;; 获取当前行的 org-indent 属性 (如果有)
                       (base-indent (get-text-property beg-line 'line-prefix))
                       end-line end-line-end content-beg content-end)

                  (when (re-search-forward (format "^[ \t]*#\\+end_%s\\s-*$" (regexp-quote type)) nil t)
                    (setq end-line (line-beginning-position))
                    (setq end-line-end (line-end-position))
                    (setq content-beg (save-excursion (goto-char beg-line-end) (min (point-max) (1+ (point)))))
                    (setq content-end (max content-beg (1- end-line)))

                    ;; --- A. 处理 Begin 行 (标题栏) ---
                    (let* ((title-text (concat " " label (if (string-empty-p rest) "" (concat " : " rest))))
                           (ov (make-overlay beg-line beg-line-end)))
                      (overlay-put ov 'my/org-pretty-block t)
                      (overlay-put ov 'face `(:background ,title-bg :foreground ,color :extend t))
                      ;; 关键：保留原有的缩进，同时加上我们的标题
                      (overlay-put ov 'display (concat 
                                                (or base-indent "") 
                                                (propertize " " 'display (propertize "┏ " 'face `(:foreground ,color))) ;; 左上角装饰
                                                (propertize title-text 'face 'my/org-block-title-face)))
                      (overlay-put ov 'evaporate t))

                    ;; --- B. 处理 内容区域 (卡片主体) ---
                    (let ((ov (make-overlay content-beg content-end)))
                      (overlay-put ov 'my/org-pretty-block t)
                      (overlay-put ov 'face `(:background ,bg-color :extend t))
                      ;; 【关键修改】：获取内容第一行的缩进属性，拼接到我们的边框上
                      (let ((content-indent (or (get-text-property content-beg 'line-prefix) base-indent)))
                        (overlay-put ov 'line-prefix (my/org--make-merged-prefix content-indent color))
                        (overlay-put ov 'wrap-prefix (my/org--make-merged-prefix content-indent color)))
                      (overlay-put ov 'evaporate t))

                    ;; --- C. 处理 End 行 (底部闭合线) ---
                    (let ((ov (make-overlay end-line end-line-end)))
                      (overlay-put ov 'my/org-pretty-block t)
                      ;; 确保 End 行也有背景色，看起来是一体的
                      (overlay-put ov 'face `(:background ,bg-color :foreground ,color :extend t))
                      ;; 用一条细线或者底边框字符替换 #+end_...
                      (overlay-put ov 'display (concat 
                                                (or base-indent "") 
                                                (propertize "┗━━━━━━━━━━━━━━━━━━━━━━━━━━" 'face `(:foreground ,color :height 0.7)))) 
                      (overlay-put ov 'evaporate t)))))))))))

  ;; 5. 钩子与激活
  (defun my/org-refresh-pretty-blocks ()
    (interactive)
    (my/org--pretty-special-blocks))

  (add-hook 'org-mode-hook #'my/org-refresh-pretty-blocks)
  ;; 监听 org-indent 的变化，确保缩进改变时重绘
  (add-hook 'org-indent-mode-hook #'my/org-refresh-pretty-blocks)
  
  (advice-add 'org-indent-refresh-maybe :after #'my/org--pretty-special-blocks)
  (advice-add 'org-cycle :after #'my/org--pretty-special-blocks)
  ;; 在保存后重绘，防止编辑导致错位
  (add-hook 'before-save-hook (lambda () (remove-overlays (point-min) (point-max) 'my/org-pretty-block t)))
  (add-hook 'after-save-hook (lambda () (when (derived-mode-p 'org-mode) (my/org-refresh-pretty-blocks)))))


(provide 'init-org)
;;; init-org.el ends here

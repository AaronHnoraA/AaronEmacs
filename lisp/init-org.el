;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
    (setq org-directory (file-truename "~/.org"))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  ;; prettify
  (org-startup-indented t)
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; image
  (org-image-actual-width nil)
  ;; more user-friendly
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  ;; call C-c C-o explicitly
  (org-return-follows-link nil)
  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WIP"        :foreground "#0098dd" :weight bold)
                            ("WAIT"       :foreground "#9f7efe" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; log
  (org-log-repeat 'time)
  ;; refile
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; goto. We use minibuffer to filter instead of isearch.
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; archive
  (org-archive-location "%s_archive::datetree/")
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; abbreviation for url
  (org-link-abbrev-alist '(("GitHub" . "https://github.com/")
                           ("GitLab" . "https://gitlab.com/")
                           ("Google" . "https://google.com/search?q=")
                           ("RFCs"   . "https://tools.ietf.org/html/")
                           ("LWN"    . "https://lwn.net/Articles/")
                           ("WG21"   . "https://wg21.link/"))))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :config
  ;; update appt list every 5 minutes
  (run-at-time t 300 #'org-agenda-to-appt)
  (shut-up! #'org-agenda-to-appt)
  :custom
  (org-agenda-files (list (expand-file-name "tasks.org" org-directory)))
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-inhibit-startup t)
  (org-agenda-time-leading-zero t)
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
         ;; consistent with separedit/magit
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
                        ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t))))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . org-capture-setup)
  :config
  (with-no-warnings
    (defun org-capture-setup ()
      (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
  :custom
  (org-capture-use-agenda-date t)
  (org-capture-templates-contexts nil)
  (org-capture-templates `(;; Tasks
                           ("t" "Tasks")
                           ("tt" "Today" entry (file+olp+datetree "tasks.org")
                            "* %? %^{EFFORT}p"
                            :prepend t)
                           ("ti" "Inbox" entry (file+headline "tasks.org" "Inbox")
                            "* %?\n%i\n")
                           ("tm" "Mail" entry (file+headline "tasks.org" "Inbox")
                            "* TODO %^{type|reply to|contact} %^{recipient} about %^{subject} :MAIL:\n")
                           ;; Capture
                           ("c" "Capture")
                           ("cn" "Note" entry (file+headline "capture.org" "Notes")
                            "* %? %^g\n%i\n"))))

(use-package org
  :ensure nil
  :init
  ;; 确保在任何引用 org-directory 之前，它一定存在
  (defvar org-directory nil "Directory for Org files.")
  :custom
  (org-directory (file-truename "~/.org")))

(define-prefix-command 'org-roam-map)
(global-set-key (kbd "C-c n") 'org-roam-map)
(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
    (setq org-roam-directory (file-truename "~/.org/roam"))
    (setq org-directory (file-truename "~/.org"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n o" . org-id-get-create))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode 1))

(use-package org-roam
   :ensure t
   :after org
   :init
   (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
   :config
   (org-roam-setup)
   ;;--------------------------
   ;; Handling file properties for ‘LAST_MODIFIED’
   ;;--------------------------
   (defun pv/org-find-time-file-property (property &optional anywhere)
     "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
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
     "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
     (when-let ((pos (pv/org-find-time-file-property property anywhere)))
       (save-excursion
         (goto-char pos)
         (if (and (looking-at-p " ")
                  (progn (forward-char)
                         (org-at-timestamp-p 'lax)))
             pos
           -1))))
   (defun pv/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (pv/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun pv/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (pv/org-set-time-file-property "last_modified")))
   :hook
   (before-save . pv/org-set-last-modified) ; 保存文件时调用
   :custom
   (org-roam-directory (concat org-directory "roam/"))  ; 设置 org-roam 目录
   ;; 自定义默认模板
   (org-roam-capture-templates
    '(("d" "default" plain "%?"
       :if-new
       (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                  "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
       :immediate-finish t)))
   :bind (("C-c n f" . org-roam-node-find)
          (:map org-mode-map
            (("C-c n i" . org-roam-node-insert)
            ("C-c n o" . org-id-get-create)
            ("C-c n t" . org-roam-tag-add)
            ("C-c n a" . org-roam-alias-add)
            ("C-c n l" . org-roam-buffer-toggle)))))

(use-package org-roam-ui
  :ensure t ;; 自动安装
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
  (org-roam-ui-follow t) ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))

(setq pv/org-refile-file (concat org-directory "refile.org"))

(use-package org
  :custom
  ;; ...
  (org-capture-templates
   (quote (("t" "todo" entry (file pv/org-refile-file)
            "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
           ("r" "respond" entry (file pv/org-refile-file)
            "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
           ("n" "note" entry (file pv/org-refile-file)
            "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
           ("w" "org-protocol" entry (file pv/org-refile-file)
            "* TODO Review %c\n%U\n" :immediate-finish t)
           ("m" "Meeting" entry (file pv/org-refile-file)
            "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))
  (org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path 'file)
    ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes (quote confirm))
  (org-cite-global-bibliography pv/org-bibtex-files)
  :bind
  ;; ...
  (("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))

(setq pv/org-bibtex-library `(,(concat org-directory "References/")))
(setq pv/org-bibtex-files `(,(concat org-directory "References/references.bib")))

(use-package org
  :custom
  ;; ...
  (org-cite-global-bibliography pv/org-bibtex-files))

(use-package bibtex-completion
  :custom
  (bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process "open" nil 0 nil fpath))) ; 配置打开 PDF 的方式
  (bibtex-completion-bibliography pv/org-bibtex-files)
  (bibtex-completion-library-path pv/org-bibtex-library))

(use-package org-ref
  :ensure t)

(provide 'init-org)
;;; init-org.el ends here

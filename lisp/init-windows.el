;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function my/project-current-root "init-project")
(declare-function bookmark-bmenu-list "bookmark")
(declare-function bookmark-completing-read "bookmark" (prompt &optional default))
(declare-function bookmark-delete "bookmark" (bookmark))
(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-position "bookmark" (bookmark-name-or-record))
(declare-function bookmark-insert "bookmark" (bookmark))
(declare-function bookmark-jump "bookmark" (bookmark))
(declare-function bookmark-jump-other-window "bookmark" (bookmark))
(declare-function bookmark-location "bookmark" (bookmark-name-or-record))
(declare-function bookmark-make-record-default "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-maybe-load-default-file "bookmark" ())
(declare-function bookmark-rename "bookmark" (old new))
(declare-function bookmark-save "bookmark" (&optional parg))
(declare-function bookmark-set-no-overwrite "bookmark" (&optional name no-overwrite))
(declare-function bookmark-store "bookmark" (name alist no-overwrite))
(declare-function consult--jump-preview "consult" ())
(declare-function consult--lookup-prop "consult" (prop candidates input))
(declare-function consult--read "consult" (table &rest options))
(declare-function evil-define-key "evil" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil" (mode state))

(defvar my/bookmark-history nil)
(defvar my/bookmark-list-buffer-name "*Bookmarks*")
(defvar my/bookmark-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'my/bookmark-list-refresh)
    (define-key map (kbd "RET") #'my/bookmark-list-visit)
    (define-key map (kbd "D") #'my/bookmark-list-delete)
    (define-key map (kbd "r") #'my/bookmark-list-rename)
    map))

(defun my/bookmark--load ()
  "Load bookmark data for the current session."
  (require 'bookmark)
  (bookmark-maybe-load-default-file))

(defun my/bookmark--records ()
  "Return the current bookmark records."
  (my/bookmark--load)
  bookmark-alist)

(defun my/bookmark--current-project-root ()
  "Return the current project root, or nil."
  (when (fboundp 'my/project-current-root)
    (my/project-current-root)))

(defun my/bookmark--name (bookmark)
  "Return BOOKMARK's name."
  (car bookmark))

(defun my/bookmark--kind (bookmark)
  "Return BOOKMARK's kind."
  (or (alist-get 'my-kind (cdr bookmark))
      'bookmark))

(defun my/bookmark--project-root-for-file (file)
  "Return the project root for FILE, or nil."
  (when file
    (with-current-buffer (find-file-noselect file)
      (let ((default-directory (file-name-directory file)))
        (my/bookmark--current-project-root)))))

(defun my/bookmark--project-root (bookmark)
  "Return BOOKMARK's project root, if any."
  (or (alist-get 'my-project-root (cdr bookmark))
      (my/bookmark--project-root-for-file (my/bookmark--file bookmark))))

(defun my/bookmark--project-name (bookmark)
  "Return a short project label for BOOKMARK."
  (when-let* ((root (my/bookmark--project-root bookmark)))
    (file-name-nondirectory (directory-file-name root))))

(defun my/bookmark--current-project-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK belongs to the current project."
  (let ((current (my/bookmark--current-project-root))
        (bookmark-root (my/bookmark--project-root bookmark)))
    (and current bookmark-root
         (equal (file-truename current)
                (file-truename bookmark-root)))))

(defun my/bookmark--sort-records (records)
  "Sort RECORDS with current-project bookmarks first."
  (sort
   (copy-sequence records)
   (lambda (left right)
     (let ((left-current (my/bookmark--current-project-bookmark-p left))
           (right-current (my/bookmark--current-project-bookmark-p right)))
       (cond
        ((and left-current (not right-current)) t)
        ((and right-current (not left-current)) nil)
        (t
         (string-lessp (my/bookmark--name left)
                       (my/bookmark--name right))))))))

(defun my/bookmark--file (bookmark)
  "Return BOOKMARK's filename, if any."
  (bookmark-get-filename bookmark))

(defun my/bookmark--position (bookmark)
  "Return BOOKMARK's file position, if any."
  (bookmark-get-position bookmark))

(defun my/bookmark--line (bookmark)
  "Return BOOKMARK's 1-based line number, if available."
  (when-let* ((file (my/bookmark--file bookmark))
              (pos (my/bookmark--position bookmark)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (min pos (point-max)))
        (line-number-at-pos)))))

(defun my/bookmark--line-text (&optional pos)
  "Return trimmed text for line at POS or point."
  (save-excursion
    (when pos
      (goto-char pos))
    (string-trim
     (buffer-substring-no-properties
      (line-beginning-position)
      (line-end-position)))))

(defun my/bookmark--snippet (bookmark)
  "Return a human-readable snippet for BOOKMARK."
  (if-let* ((file (my/bookmark--file bookmark))
            (pos (my/bookmark--position bookmark)))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (min pos (point-max)))
          (my/bookmark--line-text)))
    (bookmark-location bookmark)))

(defun my/bookmark--format-candidate (bookmark)
  "Return a picker candidate string for BOOKMARK."
  (let* ((name (my/bookmark--name bookmark))
         (kind (symbol-name (my/bookmark--kind bookmark)))
         (project (or (my/bookmark--project-name bookmark) ""))
         (file (or (my/bookmark--file bookmark) ""))
         (line (or (my/bookmark--line bookmark) ""))
         (snippet (or (my/bookmark--snippet bookmark) "")))
    (propertize
     (format "%-24s %-8s %-16s %-30s %-6s %s"
             name
             kind
             project
             (if (string-empty-p file)
                 ""
               (abbreviate-file-name file))
             line
             snippet)
     'my-bookmark-record bookmark)))

(defun my/bookmark--marker (bookmark)
  "Return a marker suitable for previewing BOOKMARK."
  (when-let* ((file (my/bookmark--file bookmark))
              (pos (my/bookmark--position bookmark)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (min pos (point-max)))
        (point-marker)))))

(defun my/bookmark--preview-state (&optional other-window)
  "Return a preview state for bookmarks.
If OTHER-WINDOW is non-nil, jump in another window on final selection."
  (require 'consult)
  (let ((preview (consult--jump-preview))
        (jump-fn (if other-window
                     #'bookmark-jump-other-window
                   #'bookmark-jump)))
    (lambda (action cand)
      (funcall preview action (and cand (my/bookmark--marker cand)))
      (when (and cand (eq action 'return))
        (funcall jump-fn (my/bookmark--name cand))))))

(defun my/bookmark--read-record (prompt &optional other-window)
  "Read a bookmark record with PROMPT.
If OTHER-WINDOW is non-nil, jump in another window on final selection."
  (require 'consult)
  (let ((records (my/bookmark--sort-records (my/bookmark--records))))
    (if records
        (consult--read
         (mapcar #'my/bookmark--format-candidate records)
         :prompt prompt
         :require-match t
         :sort nil
         :history 'my/bookmark-history
         :category 'bookmark
         :preview-key '(:debounce 0.2 any)
         :state (my/bookmark--preview-state other-window)
         :lookup (apply-partially #'consult--lookup-prop 'my-bookmark-record))
      (my/bookmark-list)
      nil)))

(defun my/bookmark--read-name (prompt)
  "Read bookmark name with PROMPT after loading bookmark data."
  (my/bookmark--load)
  (bookmark-completing-read prompt))

(defun my/bookmark-jump-dwim ()
  "Jump to a bookmark with a preview-capable UI."
  (interactive)
  (my/bookmark--read-record "Bookmark: "))

(defun my/bookmark-jump-other-window-dwim ()
  "Jump to a bookmark in another window."
  (interactive)
  (my/bookmark--read-record "Bookmark other window: " t))

(defun my/bookmark-delete-dwim ()
  "Delete a bookmark chosen via completion."
  (interactive)
  (let ((name (my/bookmark--read-name "Delete bookmark: ")))
    (bookmark-delete name)
    (bookmark-save)
    (message "Deleted bookmark: %s" name)))

(defun my/bookmark-rename-dwim ()
  "Rename a bookmark chosen via completion."
  (interactive)
  (let* ((old (my/bookmark--read-name "Rename bookmark: "))
         (new (read-string (format "Rename %s to: " old) old)))
    (bookmark-rename old new)
    (bookmark-save)
    (message "Renamed bookmark: %s -> %s" old new)))

(defun my/bookmark--line-name ()
  "Return an auto-generated line bookmark name for point."
  (format "line:%s:%d %s"
          (file-relative-name buffer-file-name default-directory)
          (line-number-at-pos)
          (truncate-string-to-width (my/bookmark--line-text) 60 nil nil t)))

(defun my/bookmark--line-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a line bookmark."
  (eq (my/bookmark--kind bookmark) 'line))

(defun my/bookmark--current-line-bookmark ()
  "Return the line bookmark at the current line, if any."
  (when buffer-file-name
    (let ((file buffer-file-name)
          (line (line-number-at-pos)))
      (seq-find
       (lambda (bookmark)
         (and (my/bookmark--line-bookmark-p bookmark)
              (equal (my/bookmark--file bookmark) file)
              (= (or (my/bookmark--line bookmark) -1) line)))
       (my/bookmark--records)))))

(defun my/bookmark-set-line ()
  "Create a line bookmark at the current line."
  (interactive)
  (unless buffer-file-name
    (user-error "Line bookmarks require a file-backed buffer"))
  (my/bookmark--load)
  (save-excursion
    (beginning-of-line)
    (let* ((name (my/bookmark--line-name))
           (record (bookmark-make-record-default nil nil (point))))
      (setf (alist-get 'my-kind record) 'line)
      (when-let* ((project-root (my/bookmark--current-project-root)))
        (setf (alist-get 'my-project-root record) project-root))
      (bookmark-store name record nil)
      (bookmark-save)
      (message "Saved line bookmark: %s" name))))

(defun my/bookmark-set ()
  "Set a bookmark and tag it with current project metadata."
  (interactive)
  (call-interactively #'bookmark-set)
  (when-let* ((bookmark (car bookmark-alist))
              (record (cdr bookmark))
              (project-root (my/bookmark--current-project-root)))
    (setf (alist-get 'my-project-root record) project-root)
    (bookmark-save)))

(defun my/bookmark-set-no-overwrite ()
  "Set a bookmark without overwriting and tag it with project metadata."
  (interactive)
  (call-interactively #'bookmark-set-no-overwrite)
  (when-let* ((bookmark (car bookmark-alist))
              (record (cdr bookmark))
              (project-root (my/bookmark--current-project-root)))
    (setf (alist-get 'my-project-root record) project-root)
    (bookmark-save)))

(defun my/bookmark-toggle-line ()
  "Toggle a line bookmark at the current line."
  (interactive)
  (if-let* ((bookmark (my/bookmark--current-line-bookmark)))
      (progn
        (bookmark-delete (my/bookmark--name bookmark))
        (bookmark-save)
        (message "Removed line bookmark: %s" (my/bookmark--name bookmark)))
    (my/bookmark-set-line)))

(defun my/bookmark--line-bookmarks-in-current-file ()
  "Return current-file line bookmarks sorted by line."
  (unless buffer-file-name
    (user-error "Line bookmark navigation requires a file-backed buffer"))
  (sort
   (seq-filter
    (lambda (bookmark)
      (and (my/bookmark--line-bookmark-p bookmark)
           (equal (my/bookmark--file bookmark) buffer-file-name)))
    (my/bookmark--records))
   (lambda (a b)
     (< (or (my/bookmark--line a) 0)
        (or (my/bookmark--line b) 0)))))

(defun my/bookmark-next-line ()
  "Jump to the next line bookmark in the current file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (bookmarks (my/bookmark--line-bookmarks-in-current-file))
         (target (or (seq-find
                      (lambda (bookmark)
                        (> (or (my/bookmark--line bookmark) 0) line))
                      bookmarks)
                     (car bookmarks))))
    (unless target
      (user-error "No line bookmarks in the current file"))
    (bookmark-jump (my/bookmark--name target))))

(defun my/bookmark-previous-line ()
  "Jump to the previous line bookmark in the current file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (bookmarks (reverse (my/bookmark--line-bookmarks-in-current-file)))
         (target (or (seq-find
                      (lambda (bookmark)
                        (< (or (my/bookmark--line bookmark) most-positive-fixnum) line))
                      bookmarks)
                     (car bookmarks))))
    (unless target
      (user-error "No line bookmarks in the current file"))
    (bookmark-jump (my/bookmark--name target))))

(define-derived-mode my/bookmark-list-mode tabulated-list-mode "Bookmarks"
  "Major mode for bookmark management."
  (setq tabulated-list-format [("Name" 24 t)
                               ("Kind" 8 t)
                               ("Project" 16 t)
                               ("File" 30 t)
                               ("Line" 6 t)
                               ("Text" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(with-eval-after-load 'evil
  (evil-set-initial-state 'my/bookmark-list-mode 'normal)
  (evil-define-key 'normal my/bookmark-list-mode-map
    (kbd "RET") #'my/bookmark-list-visit
    (kbd "<return>") #'my/bookmark-list-visit
    (kbd "D") #'my/bookmark-list-delete
    (kbd "d") #'my/bookmark-list-delete
    (kbd "r") #'my/bookmark-list-rename
    (kbd "g") #'my/bookmark-list-refresh))

(defun my/bookmark-list-refresh ()
  "Refresh the bookmark list buffer."
  (interactive)
  (setq tabulated-list-entries
        (mapcar
         (lambda (bookmark)
           (list
            (my/bookmark--name bookmark)
            (vector
             (my/bookmark--name bookmark)
             (symbol-name (my/bookmark--kind bookmark))
             (or (my/bookmark--project-name bookmark) "")
             (abbreviate-file-name (or (my/bookmark--file bookmark) ""))
             (format "%s" (or (my/bookmark--line bookmark) ""))
             (or (my/bookmark--snippet bookmark) ""))))
         (my/bookmark--sort-records (my/bookmark--records))))
  (tabulated-list-print t))

(defun my/bookmark-list ()
  "Show bookmarks in a dedicated list buffer."
  (interactive)
  (let ((buffer (get-buffer-create my/bookmark-list-buffer-name)))
    (with-current-buffer buffer
      (my/bookmark-list-mode)
      (my/bookmark-list-refresh))
    (pop-to-buffer buffer)))

(defun my/bookmark-list-visit ()
  "Visit the bookmark at point in the list buffer."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (bookmark-jump id)))

(defun my/bookmark-list-delete ()
  "Delete the bookmark at point in the list buffer."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (bookmark-delete id)
    (bookmark-save)
    (my/bookmark-list-refresh)
    (message "Deleted bookmark: %s" id)))

(defun my/bookmark-list-rename ()
  "Rename the bookmark at point in the list buffer."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (new (read-string (format "Rename %s to: " id) id)))
    (bookmark-rename id new)
    (bookmark-save)
    (my/bookmark-list-refresh)
    (message "Renamed bookmark: %s -> %s" id new)))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-c p F" . consult-find)
         ("C-c p g" . consult-ripgrep)
         ("C-x r j" . my/bookmark-jump-dwim)
         ("C-c k"   . kill-current-buffer)))

(use-package transient
  :ensure nil
  :bind (("C-x r ." . my/bookmark-dispatch))
  :after bookmark
  :config
  (transient-define-prefix my/bookmark-dispatch ()
    "Bookmark management."
    [["Jump"
      ("j" "jump" my/bookmark-jump-dwim :transient transient--do-exit)
      ("o" "other window" my/bookmark-jump-other-window-dwim :transient transient--do-exit)
      ("l" "list" my/bookmark-list :transient transient--do-exit)
      ("n" "next line" my/bookmark-next-line :transient transient--do-exit)
      ("p" "prev line" my/bookmark-previous-line :transient transient--do-exit)]
     ["Manage"
      ("m" "set" my/bookmark-set :transient transient--do-exit)
      ("t" "toggle line" my/bookmark-toggle-line :transient transient--do-exit)
      ("L" "set line" my/bookmark-set-line :transient transient--do-exit)
      ("r" "rename" my/bookmark-rename-dwim :transient transient--do-exit)
      ("i" "insert" bookmark-insert :transient transient--do-exit)
      ("s" "save" bookmark-save :transient transient--do-exit)]]))

(use-package ibuffer
  :ensure nil
  :custom (ibuffer-expert t)
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Code"  (or (mode . c-mode) (mode . c-ts-mode)
                        (mode . c++-mode) (mode . c++-ts-mode)
                        (mode . python-mode) (mode . python-ts-mode)
                        (mode . emacs-lisp-mode)
                        (mode . rust-mode) (mode . rust-ts-mode)
                        (mode . go-mode) (mode . go-ts-mode)
                        (mode . java-mode) (mode . java-ts-mode)
                        (mode . js-mode) (mode . js-ts-mode)
                        (mode . typescript-mode) (mode . typescript-ts-mode)))
           ("Dired" (mode . dired-mode))
           ("Shell" (or (mode . eshell-mode) (mode . shell-mode) (mode . term-mode)))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*Backtrace\\*$"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-do-sort-by-recency))))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package winner
  :ensure nil
  :config (winner-mode 1))

(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings 'meta))

;;;; =========================
;;;; Dirvish (Dired UI) - practical + pretty + icons
;;;; =========================

;; 图标依赖（必须先装）
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :defer 1)

(use-package dirvish
  :ensure t
  :after dired
  :init
 (add-to-list 'load-path
               (expand-file-name "extensions"
                                 (file-name-directory (locate-library "dirvish"))))
  ;; 让 Dirvish 接管 dired（放 init 最合适）
  (dirvish-override-dired-mode)

  :bind (("C-c o d" . dirvish-dwim)
         ("C-c o f" . dirvish-fd)
         :map dirvish-mode-map
         ("q" . dirvish-quit)
         ("a" . dirvish-quick-access)
         ("f" . dirvish-file-info-menu)
         ("y" . dirvish-yank-menu)
         ("s" . dirvish-quicksort)
         ("TAB" . dirvish-subtree-toggle)
         ("M-t" . dirvish-layout-toggle)
         ("M-b" . dirvish-history-go-backward)
         ("M-f" . dirvish-history-go-forward)
         ("M-n" . dirvish-narrow)
         ("M-m" . dirvish-mark-menu)
         ("?" . dirvish-dispatch)
         ;("P" . dirvish-peek-mode) ; 如果没有该函数，下面 config 会兜底处理
         ;; 鼠标（可选）
         ("<mouse-1>" . dirvish-subtree-toggle-or-open)
         ("<mouse-2>" . dired-mouse-find-file-other-window)
         ("<mouse-3>" . dired-mouse-find-file))
  :custom
  (dirvish-side-width 36)
  (dirvish-header-line-height '(26 . 36))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))

  :config
  ;; 1) 先给一个“基础属性集合”（绝对不会触发 dirvish-vc）
  (setq dirvish-attributes
        '(nerd-icons file-size subtree-state collapse file-time))

  ;; 2) Peek / Side-follow：有就开（没有就跳过，不报错）
  (when (fboundp 'dirvish-side-follow-mode)
    (dirvish-side-follow-mode 1))

  ;; 3) VC/Git：尝试静默加载 dirvish-vc，成功才追加属性
  (when (require 'dirvish-vc nil t)
    (setq dirvish-attributes
          '(nerd-icons vc-state git-msg file-size subtree-state collapse file-time)))

  ;; 4) 如果你希望 P 键在没有 peek 函数时也不报错，做一个安全绑定
  (unless (fboundp 'dirvish-peek-mode)
    (define-key dirvish-mode-map (kbd "P") #'ignore)))

;; Nerd Font 图标区（PUA）专用回退：只管图标，不污染符号/数学
(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-fontset-font t '(#xe000 . #xf8ff) "JetBrainsMono Nerd Font" nil 'append)
  ;; 有些 Nerd Fonts 还用到更高的 PUA 扩展区（可选但建议加）
  (set-fontset-font t '(#xf0000 . #xffffd) "JetBrainsMono Nerd Font" nil 'append))

(use-package centaur-tabs
  :ensure t
  :defer 2
  :config
  (setq centaur-tabs-style "bar"
    centaur-tabs-height 22
    centaur-tabs-set-icons t
    centaur-tabs-plain-icons t
    centaur-tabs-gray-out-icons t
    centaur-tabs-set-close-button t
    centaur-tabs-set-modified-marker t
    centaur-tabs-show-navigation-buttons t
    centaur-tabs-set-bar 'left
    centaur-tabs-cycle-scope 'tabs
    x-underline-at-descent-line nil)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((ignore-errors
     (and (string= "*xwidget" (substring (buffer-name) 0 8))
          (not (string= "*xwidget-log*" (buffer-name)))))
       "Xwidget")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
       (memq major-mode '(magit-process-mode
                  magit-status-mode
                  magit-diff-mode
                  magit-log-mode
                  magit-file-mode
                  magit-blob-mode
                  magit-blame-mode
                  )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
              help-mode))
       "Help")
      ((memq major-mode '(org-mode
              org-agenda-clockreport-mode
              org-src-mode
              org-agenda-mode
              org-beamer-mode
              org-indent-mode
              org-bullets-mode
              org-cdlatex-mode
              org-agenda-log-mode
              diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)
  :bind (("C-x r m" . my/bookmark-set)
         ("C-x r l" . my/bookmark-toggle-line)
         ("C-x r n" . my/bookmark-next-line)
         ("C-x r p" . my/bookmark-previous-line)
         ("C-x r j" . my/bookmark-jump-dwim)
         ("C-x r o" . my/bookmark-jump-other-window-dwim)))



(provide 'init-windows)
;;; init-base.el ends here

;;; init-doom-extra.el --- Doom-inspired foundation extras -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'init-funcs)

(declare-function editorconfig-find-current-editorconfig "editorconfig")
(declare-function link-hint-copy-link "link-hint" ())
(declare-function link-hint-open-link "link-hint" ())

(defvar vertico-map)

(defgroup my/doom-extra nil
  "Doom-inspired UI and editing enhancements."
  :group 'convenience)

(defconst my/dtrt-indent-excluded-modes
  '(pascal-mode
    so-long-mode
    emacs-lisp-mode
    org-mode)
  "Major modes where indentation should not be guessed automatically.")

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-interval 2)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(with-eval-after-load 'saveplace
  (define-advice save-place-find-file-hook (:after (&rest _) my/recenter-after-save-place)
    "Recenter after jumping to a remembered place."
    (when buffer-file-name
      (ignore-errors (recenter))))
  (define-advice save-place-to-alist (:around (fn &rest args) my/inhibit-save-place-in-long-files)
    "Skip save-place bookkeeping for very large buffers."
    (unless (bound-and-true-p so-long-minor-mode)
      (apply fn args))))

(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode)
  :config
  (when (boundp 'editorconfig-trim-whitespaces-mode)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))
  ;; Built-in/editorconfig variants differ across Emacs releases.
  (when (boundp 'editorconfig-exclude-regexps)
    (add-to-list 'editorconfig-exclude-regexps
                 "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))
  (add-hook 'editorconfig-after-apply-functions
            (lambda (props)
              (when (and (bound-and-true-p dtrt-indent-mode)
                         (or (gethash 'indent_style props)
                             (gethash 'indent_size props)))
                (dtrt-indent-mode -1))
              (when (and (gethash 'indent_size props)
                         (derived-mode-p 'org-mode)
                         (not (fboundp 'org--set-tab-width)))
                (setq tab-width 8))
              (when (bound-and-true-p whitespace-mode)
                (whitespace-mode -1)
                (whitespace-mode 1)))))

(use-package dtrt-indent
  :ensure t
  :defer 2
  :config
  (defun my/dtrt-indent-enable-maybe ()
    "Enable `dtrt-indent-mode' when the current buffer looks suitable."
    (unless (or buffer-read-only
                (bound-and-true-p so-long-minor-mode)
                (member major-mode my/dtrt-indent-excluded-modes)
                (and (boundp 'buffer-file-name) (not buffer-file-name))
                (and (fboundp 'project-current)
                     (project-current nil default-directory)
                     (locate-dominating-file default-directory ".editorconfig")))
      (let ((inhibit-message t))
        (dtrt-indent-mode 1))))

  (add-hook 'change-major-mode-after-body-hook #'my/dtrt-indent-enable-maybe)
  (add-hook 'read-only-mode-hook #'my/dtrt-indent-enable-maybe)
  (setq dtrt-indent-run-after-smie t
        dtrt-indent-max-lines 2000)
  (when (boundp 'dtrt-indent-hook-generic-mapping-list)
    (dolist (var (get 'tab-width 'indent-vars))
      (cl-callf2 rassq-delete-all var dtrt-indent-hook-generic-mapping-list))))

(use-package whitespace
  :ensure nil
  :config
  (defun my/whitespace-highlight-incorrect-indentation-h ()
    "Highlight indentation inconsistent with the current tab policy."
    (unless (or (eq major-mode 'fundamental-mode)
                (bound-and-true-p global-whitespace-mode)
                (null buffer-file-name)
                buffer-read-only)
      (setq-local whitespace-style
                  (cl-union (if indent-tabs-mode
                                '(face indentation)
                              '(face tabs tab-mark))
                            '(trailing)))
      (whitespace-mode 1)))

  (add-hook 'after-change-major-mode-hook #'my/whitespace-highlight-incorrect-indentation-h))

(use-package consult-dir
  :ensure t
  :after consult
  :bind (("C-x C-d" . consult-dir)))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-x C-d") #'consult-dir)
  (define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file))

(use-package rainbow-mode
  :ensure t
  :hook ((css-mode
          css-ts-mode
          html-mode
          html-ts-mode
          mhtml-mode
          nxml-mode
          web-mode
          emacs-lisp-mode) . rainbow-mode))

(use-package link-hint
  :ensure t
  :bind (("M-o" . link-hint-open-link))
  :commands (link-hint-open-link
             link-hint-copy-link))

(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

(use-package ibuffer-vc
  :ensure t
  :after ibuffer)

(use-package ibuffer-projectile
  :ensure t
  :after (ibuffer projectile)
  :commands ibuffer-projectile-set-filter-groups)

(with-eval-after-load 'ibuffer
  (define-ibuffer-column vc-status
    (:name "VC" :inline t)
    (if-let* ((buffer-file-name (buffer-local-value 'buffer-file-name buffer))
              (backend (vc-backend buffer-file-name)))
        (symbol-name backend)
      ""))

  (setq ibuffer-formats
        '((mark modified read-only locked
                " " (name 30 30 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 18 18 :left :elide)
                " " (vc-status 10 10 :left)
                " " filename-and-process)
          (mark " " (name 30 -1) " " filename)))

  (define-key ibuffer-mode-map (kbd "V") #'ibuffer-vc-set-filter-groups-by-vc-root)
  (define-key ibuffer-mode-map (kbd "P") #'ibuffer-projectile-set-filter-groups))

(defun my/sudo-file-path (file)
  "Return a TRAMP sudo path for FILE."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let* ((user (file-remote-p file 'user)))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun my/sudo-find-file (file)
  "Open FILE as root."
  (interactive (list (read-file-name "Open file as root: ")))
  (find-file (my/sudo-file-path (expand-file-name file))))

(defun my/sudo-this-file ()
  "Reopen the current file as root."
  (interactive)
  (my/sudo-find-file
   (or (buffer-file-name (buffer-base-buffer))
       (when (derived-mode-p 'dired-mode 'wdired-mode)
         default-directory)
       (user-error "Current buffer isn't visiting a file"))))

(defun my/sudo-save-buffer ()
  "Write the current buffer as root, then reload the original file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer isn't visiting a file"))
  (let ((source (current-buffer))
        (sudo-file (my/sudo-file-path buffer-file-name)))
    (with-current-buffer (find-file-noselect sudo-file)
      (erase-buffer)
      (insert-buffer-substring source)
      (save-buffer)
      (kill-buffer))
    (revert-buffer t t)
    (message "Saved %s as root" buffer-file-name)))

(my/leader!
  "f d" '(:def consult-dir :which-key "consult dir")
  "f u" '(:def my/sudo-find-file :which-key "sudo find file")
  "f U" '(:def my/sudo-this-file :which-key "sudo this file")
  "f S" '(:def my/sudo-save-buffer :which-key "sudo save")
  "f t" '(:def my/template-switch :which-key "switch template")
  "f T" '(:def my/template-debug :which-key "debug template")
  "o o" '(:def link-hint-open-link :which-key "open link")
  "o O" '(:def link-hint-copy-link :which-key "copy link")
  "p c" '(:def editorconfig-find-current-editorconfig :which-key "editorconfig")
  "q r" '(:def restart-emacs :which-key "restart emacs"))

(provide 'init-doom-extra)
;;; init-doom-extra.el ends here

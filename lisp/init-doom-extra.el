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
(declare-function yas-abort-snippet "yasnippet" ())
(declare-function yas-expand-snippet "yasnippet" (content &optional start end expand-env))
(declare-function yas-lookup-snippet "yasnippet" (name &optional mode noerror))
(declare-function yas-reload-all "yasnippet" (&optional no-jit interactive))

(defvar vertico-map)

(defgroup my/doom-extra nil
  "Doom-inspired UI and editing enhancements."
  :group 'convenience)

(defcustom my/file-templates-directory
  (expand-file-name "doomemacs/modules/editor/file-templates/templates"
                    user-emacs-directory)
  "Directory containing Yasnippet-based file templates."
  :type 'directory
  :group 'my/doom-extra)

(defconst my/file-template-rules
  '(("/main\\.c\\(?:c\\|pp\\)\\'" :mode c++-mode :trigger "__main.cpp")
    ("/win32_\\.c\\(?:c\\|pp\\)\\'" :mode c++-mode :trigger "__winmain.cpp")
    ("\\.c\\(?:c\\|pp\\)\\'" :mode c++-mode :trigger "__cpp")
    ("\\.h\\(?:h\\|pp\\|xx\\)\\'" :mode c++-mode :trigger "__hpp")
    ("\\.h\\'" :mode c-mode :trigger "__h")
    (c-mode :trigger "__c")
    ("/main\\.go\\'" :mode go-mode :trigger "__main.go")
    (go-mode :trigger "__.go")
    ("\\.html\\'" :mode web-mode :trigger "__.html")
    (markdown-mode :trigger "__")
    (nxml-mode :trigger "__")
    (python-mode :trigger "__")
    ("/main\\.rs\\'" :mode rust-mode :trigger "__main.rs")
    ("/Cargo\\.toml\\'" :mode rust-mode :trigger "__Cargo.toml")
    ("/package\\.json\\'" :mode json-mode :trigger "__package.json")
    ("/bower\\.json\\'" :mode json-mode :trigger "__bower.json")
    ("\\.zunit\\'" :mode sh-mode :trigger "__zunit")
    (sh-mode :trigger "__")
    (org-mode :trigger "__")
    ("\\.el\\'" :mode emacs-lisp-mode :trigger "__package"))
  "Subset of Doom file template rules adapted to this config.")

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

(defun my/file-template--available-p ()
  "Return non-nil when file templates can be expanded."
  (and (file-directory-p my/file-templates-directory)
       (featurep 'yasnippet)))

(defun my/file-template--match-p (predicate)
  "Return non-nil when PREDICATE matches the current buffer."
  (cond
   ((symbolp predicate)
    (or (eq major-mode predicate)
        (derived-mode-p predicate)))
   ((and (stringp predicate) buffer-file-name)
    (string-match-p predicate buffer-file-name))
   (t nil)))

(defun my/file-template-rule ()
  "Return the first applicable file template rule for the current buffer."
  (cl-find-if (lambda (rule)
                (my/file-template--match-p (car rule)))
              my/file-template-rules))

(defun my/file-template-apply ()
  "Expand a Yasnippet-backed file template for the current buffer."
  (interactive)
  (unless (my/file-template--available-p)
    (user-error "File templates are unavailable"))
  (when-let* ((rule (my/file-template-rule))
              (mode (or (plist-get (cdr rule) :mode) major-mode))
              (trigger (or (plist-get (cdr rule) :trigger) "__"))
              (snippet (yas-lookup-snippet trigger mode 'noerror)))
    (let ((yas-indent-line 'fixed))
      (yas-expand-snippet snippet (point-min) (point-max))
      t)))

(defun my/file-template-check-h ()
  "Apply a file template when opening a new, empty file."
  (when (and (my/file-template--available-p)
             buffer-file-name
             (not buffer-read-only)
             (bobp) (eobp)
             (not (file-exists-p buffer-file-name))
             (not (buffer-modified-p))
             (null (buffer-base-buffer))
             (not (bound-and-true-p org-capture-current-plist))
             (not (member (substring (buffer-name) 0 1) '("*" " "))))
    (my/file-template-apply)))

(defun my/file-template-debug ()
  "Report the matching template rule for the current buffer."
  (interactive)
  (if-let* ((rule (my/file-template-rule)))
      (message "File template: %S" rule)
    (message "File template: none")))

(with-eval-after-load 'yasnippet
  (when (file-directory-p my/file-templates-directory)
    (add-to-list 'yas-snippet-dirs my/file-templates-directory 'append #'eq)
    (yas-reload-all))
  (add-hook 'find-file-hook #'my/file-template-check-h)
  (add-hook 'my/escape-hook #'yas-abort-snippet))

(my/evil-global-leader-set "f d" #'consult-dir "consult dir")
(my/evil-global-leader-set "f u" #'my/sudo-find-file "sudo find file")
(my/evil-global-leader-set "f U" #'my/sudo-this-file "sudo this file")
(my/evil-global-leader-set "f S" #'my/sudo-save-buffer "sudo save")
(my/evil-global-leader-set "f t" #'my/file-template-apply "apply template")
(my/evil-global-leader-set "f T" #'my/file-template-debug "debug template")
(my/evil-global-leader-set "o o" #'link-hint-open-link "open link")
(my/evil-global-leader-set "o O" #'link-hint-copy-link "copy link")
(my/evil-global-leader-set "p c" #'editorconfig-find-current-editorconfig "editorconfig")
(my/evil-global-leader-set "q r" #'restart-emacs "restart emacs")

(provide 'init-doom-extra)
;;; init-doom-extra.el ends here

;;; init-open.el --- Central open routing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep URL/file/PDF default open policy in one place.  Browser modules should
;; call these helpers instead of hard-coding their own default backend.

;;; Code:

(require 'config)

(require 'browse-url)
(require 'general)
(require 'subr-x)

(defgroup my/open nil
  "Central routing for opening URLs and files."
  :group 'convenience)

(config-defvar my/open-routes nil
  "Open-route DSL.

Each entry has the shape:

  (KIND :default BACKEND :menu-default BACKEND :backends (BACKEND...))

Use `menu' as a default when the route should ask every time.  `system' means
macOS `open', Linux `xdg-open', Windows shell open, or `browse-url' fallback."
  :type '(repeat sexp)
  :group 'my/open)

(config-defvar my/open-browser-window-size nil
  "Width ratio for side browser windows created by open routes."
  :type 'number
  :group 'my/open)

(defconst my/open-backend-aliases
  '((open . system)
    (system-open . system)
    (xweight . xwidget))
  "Accepted aliases for open backends.")

(declare-function appine-open-file "appine" (path))
(declare-function appine-open-url "appine" (url))
(declare-function eww-browse-url "eww" (url &optional new-window))
(declare-function eww "eww" (url &optional new-buffer))
(declare-function my/appine-open-file "init-appine" (path))
(declare-function my/appine-open-url "init-appine" (url))
(declare-function my/macos-open-target "init-macos" (target))
(declare-function my/xwidget-open-url "init-browser" (url &rest args))

(defvar eww-browse-url-new-window-is-tab)
(defvar my/aaronnote--inhibit-redirect)

;;;###autoload
(defun my/open-keybinding-guide ()
  "Open the maintained keybinding guide as a read-only Markdown buffer."
  (interactive)
  (let ((file (locate-user-emacs-file "docs/daily-usage.md")))
    (unless (file-readable-p file)
      (user-error "Keybinding guide is missing: %s" file))
    (let* ((my/aaronnote--inhibit-redirect t)
           (buffer (find-file file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (view-mode 1))
      buffer)))

(defun my/open-normalize-backend (backend)
  "Normalize BACKEND into the canonical backend symbol."
  (general-route-normalize-choice backend my/open-backend-aliases))

(defun my/open-read-backend (kind &optional prompt default)
  "Read and return a backend for route KIND.
PROMPT overrides the default prompt.  DEFAULT overrides the route's
`:menu-default'."
  (general-route-read-choice
   my/open-routes kind prompt default my/open-backend-aliases))

(defun my/open-resolve-backend (kind &optional backend)
  "Return the concrete backend for KIND and optional BACKEND.
If the selected backend is `menu', prompt with `my/open-read-backend'."
  (or (general-route-resolve-choice
       my/open-routes kind backend my/open-backend-aliases)
      'system))

(defun my/open-kind-for-file (file)
  "Return the route kind for FILE."
  (general-route-match-kind my/open-routes file 'file))

(defun my/open-normalize-url (url)
  "Return URL with an explicit scheme when needed."
  (let ((url (string-trim (substring-no-properties url))))
    (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
        url
      (concat "https://" url))))

(defun my/open-normalize-target (target)
  "Return TARGET in a form suitable for a system opener."
  (let* ((target (string-trim (substring-no-properties target)))
         (target
          (if (and (string-prefix-p "fs://" target)
                   (fboundp 'remote-uri-to-file-name))
              (remote-uri-to-file-name target)
            target))
         (expanded (and (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target))
                        (expand-file-name target))))
    (cond
     ((string-empty-p target) nil)
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target) target)
     ((and expanded (file-exists-p expanded)) expanded)
     ((and expanded (file-name-absolute-p target)) expanded)
     (t target))))

(defun my/open--base-window ()
  "Return the non-minibuffer window that should be split for browser opens."
  (let* ((selected (selected-window))
         (base (if (window-minibuffer-p selected)
                   (ignore-errors (minibuffer-selected-window))
                 selected)))
    (cond
     ((and (window-live-p base)
           (not (window-minibuffer-p base)))
      base)
     ((frame-live-p (last-nonminibuffer-frame))
      (frame-selected-window (last-nonminibuffer-frame)))
     (t
      (user-error "No non-minibuffer window available for browser open")))))

(defun my/open--side-browser-window (_mode &optional reuse-selected)
  "Return (WINDOW . CREATEDP) for a browser buffer.
When REUSE-SELECTED is non-nil, use the selected window."
  (let ((base (my/open--base-window)))
    (if reuse-selected
        (cons base nil)
      (let* ((total (window-total-width base))
             split-right-p
             (new (or (condition-case nil
                          (prog1 (split-window base nil 'right)
                            (setq split-right-p t))
                        (error nil))
                      (condition-case nil
                          (split-window base nil 'below)
                        (error nil)))))
        (unless (window-live-p new)
          (user-error "No room to display a separate browser buffer"))
        (set-window-dedicated-p new nil)
        (when split-right-p
          (let ((target (max 20 (floor (* total my/open-browser-window-size)))))
            (ignore-errors
              (window-resize new (- target (window-total-width new)) t))))
        (cons new t)))))

(defun my/open--with-browser-window (mode reuse-selected open-fn)
  "Run OPEN-FN in a browser window for MODE.
If opening fails after creating a new window, delete that window so failed
browser opens do not leave empty splits behind."
  (let* ((result (my/open--side-browser-window mode reuse-selected))
         (window (car result))
         (created (cdr result))
         success)
    (unwind-protect
        (progn
          (select-window window)
          (prog1 (funcall open-fn)
            (setq success t)))
      (when (and created
                 (not success)
                 (window-live-p window)
                 (> (length (window-list nil 'no-minibuf)) 1))
        (ignore-errors (delete-window window))))))

(defun my/open-eww-url (url &optional reuse-selected)
  "Open URL in a separate EWW buffer shown in a browser side window."
  (interactive (browse-url-interactive-arg "EWW URL: "))
  (require 'eww)
  (my/open--with-browser-window
   'eww-mode reuse-selected
   (lambda ()
     (let ((eww-browse-url-new-window-is-tab nil))
       (eww-browse-url (my/open-normalize-url url) t)))))

(defun my/open-xwidget-url (url &optional reuse-selected)
  "Compatibility wrapper for `my/xwidget-open-url'."
  (unless (fboundp 'my/xwidget-open-url)
    (require 'init-browser))
  (my/xwidget-open-url url :display 'side :reuse-selected reuse-selected))

(defun my/open-system-target (target)
  "Open TARGET with the operating system."
  (interactive "sSystem open: ")
  (if-let* ((target (my/open-normalize-target target)))
      (let ((target
             (if (and (fboundp 'remote-fs-file-name-p)
                      (remote-fs-file-name-p target))
                 (progn
                   (unless (equal (remote-file-name-target target) "local")
                     (user-error
                      "System opener cannot directly access target %s"
                      (remote-file-name-target target)))
                 (remote-file-local-name target))
               target)))
        (cond
         ((fboundp 'my/macos-open-target)
          (my/macos-open-target target))
         ((and (eq system-type 'darwin) (executable-find "open"))
          (start-process "system-open" nil "open" target)
          (message "open: %s" target))
         ((and (eq system-type 'gnu/linux) (executable-find "xdg-open"))
          (start-process "system-open" nil "xdg-open" target)
          (message "xdg-open: %s" target))
         ((eq system-type 'windows-nt)
          (w32-shell-execute "open" target))
         ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target)
          (browse-url-default-browser target))
         ((file-exists-p target)
          (browse-url-of-file target))
         (t
          (user-error "No system opener for %s" target))))
    (user-error "No target to open")))

(defun my/open-url-with-backend (url backend &optional reuse-selected)
  "Open URL with BACKEND."
  (let ((url (my/open-normalize-url url))
        (backend (my/open-normalize-backend backend)))
    (pcase backend
      ('eww (my/open-eww-url url reuse-selected))
      ('xwidget
       (unless (fboundp 'my/xwidget-open-url)
         (require 'init-browser))
       (my/xwidget-open-url url :display 'side :reuse-selected reuse-selected))
      ('appine
       (cond
        ((fboundp 'my/appine-open-url) (my/appine-open-url url))
        ((fboundp 'appine-open-url) (appine-open-url url))
        (t (user-error "Appine is not available"))))
      ('system (my/open-system-target url))
      (_ (user-error "Unsupported URL backend: %s" backend)))))

(defun my/open-url (url &optional backend)
  "Open URL through the central URL route.
When BACKEND is nil, use the `url' route in `my/open-routes'."
  (interactive (list (read-string "URL: ")))
  (my/open-url-with-backend url (my/open-resolve-backend 'url backend)))

(defun my/open-file-with-backend (file backend)
  "Open FILE with BACKEND."
  (let ((file (expand-file-name file))
        (backend (my/open-normalize-backend backend)))
    (pcase backend
      ('emacs (find-file file))
      ('appine
       (cond
        ((fboundp 'my/appine-open-file) (my/appine-open-file file))
        ((fboundp 'appine-open-file) (appine-open-file file))
        (t (user-error "Appine is not available"))))
      ('system (my/open-system-target file))
      (_ (user-error "Unsupported file backend: %s" backend)))))

(defun my/open-file (file &optional kind backend)
  "Open FILE through the central file route.
KIND defaults to the first matching route, e.g. `pdf'.  BACKEND overrides the
route default."
  (interactive "fOpen file: ")
  (let* ((kind (or kind (my/open-kind-for-file file)))
         (backend (my/open-resolve-backend kind backend)))
    (my/open-file-with-backend file backend)))

(provide 'init-open)
;;; init-open.el ends here

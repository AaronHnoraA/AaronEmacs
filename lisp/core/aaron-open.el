;;; core/aaron-open.el --- Central open routing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep URL/file/PDF default open policy in one place.  Browser modules should
;; call these helpers instead of hard-coding their own default backend.

;;; Code:

(require 'config)

(require 'browse-url)
(require 'general)
(require 'seq)
(require 'subr-x)

(defgroup aaron-open nil
  "Central routing for opening URLs and files."
  :group 'convenience)

(config-defvar aaron-open-routes nil
  "Open-route DSL.

Each entry has the shape:

  (KIND :default BACKEND :menu-default BACKEND :backends (BACKEND...))

Use `menu' as a default when the route should ask every time.  `system' means
macOS `open', Linux `xdg-open', Windows shell open, or `browse-url' fallback."
  :type '(repeat sexp)
  :group 'aaron-open)

(config-defvar aaron-open-browser-window-size nil
  "Width ratio for side browser windows created by open routes."
  :type 'number
  :group 'aaron-open)

(defconst aaron-open-backend-aliases
  '((open . system)
    (system-open . system)
    (xweight . xwidget))
  "Accepted aliases for open backends.")

(declare-function eww-browse-url "eww" (url &optional new-window))
(declare-function eww "eww" (url &optional new-buffer))
(declare-function +macos/open-target "os/macos/config" (target))
(declare-function +xwidget/open-url "app/browser/config" (url &rest args))
(declare-function +portal--ensure "term/portal/config" ())
(declare-function portal-open-url "portal-web" (url))
(declare-function portal-input-focus "portal-input" ())
(declare-function remote-canonicalize-file-name "remote-fs"
                  (file-name &optional directory))
(declare-function remote-fs-file-name-p "remote-fs" (file-name))
(declare-function remote-require-on-client "remote-core"
                  (feature &optional filename noerror))

(defvar eww-browse-url-new-window-is-tab)
(defvar +noema--inhibit-redirect)

(defun aaron-open-resolve-file-name (file &optional directory)
  "Expand FILE for a user-facing visit from DIRECTORY.
An explicit `/fs:' name retains its target.  A native absolute name always
belongs to this machine, even when the current buffer is remote.  Relative
names still belong to DIRECTORY, which defaults to the current buffer."
  (unless (featurep 'remote-fs)
    (remote-require-on-client 'remote-fs))
  (remote-resolve-emacs-file-name file directory))

(defun aaron-open-call-with-file-context (file function &rest arguments)
  "Call FUNCTION on resolved FILE and ARGUMENTS on FILE's own machine.
A native absolute FILE is visited from a temporary client buffer so the
source buffer's remote directory and environment cannot alter its identity."
  (apply #'remote-call-with-emacs-file-context file function arguments))

(defun aaron-open-visit-file (file &optional wildcards)
  "Visit resolved FILE without inheriting the source buffer's target.
WILDCARDS has the same meaning as for `find-file'."
  (if wildcards
      (aaron-open-call-with-file-context file #'find-file wildcards)
    (aaron-open-call-with-file-context file #'find-file)))

(defun aaron-open-visit-file-noselect (file)
  "Visit resolved FILE without displaying it or borrowing a remote directory."
  (aaron-open-call-with-file-context file #'find-file-noselect))

(defun aaron-open-client-file (file &optional view allow-missing)
  "Visit FILE on the Emacs client, enabling `view-mode' when VIEW is non-nil.
Relative FILE names are resolved under `user-emacs-directory'.  When
ALLOW-MISSING is non-nil, permit visiting a new file for editing."
  (let ((directory (file-name-as-directory
                    (expand-file-name user-emacs-directory)))
        (process-environment (default-value 'process-environment))
        (exec-path (default-value 'exec-path))
        (+noema--inhibit-redirect t))
    ;; Documentation is client state even when invoked from a Remote buffer.
    (with-temp-buffer
      (setq default-directory directory)
      (let ((file (expand-file-name file directory)))
        (unless (or (file-readable-p file)
                    (and allow-missing (not (file-exists-p file))))
          (user-error "Local file is missing: %s" file))
        (let ((buffer (find-file file)))
          (when view
            (with-current-buffer buffer
              (goto-char (point-min))
              (view-mode 1)))
          buffer)))))

(defun aaron-open-client-document (file &optional view)
  "Visit local documentation FILE, enabling `view-mode' when VIEW is non-nil."
  (aaron-open-client-file file view))

;;;###autoload
(defun aaron/open-keybinding-guide ()
  "Open the maintained keybinding guide as a read-only Markdown buffer."
  (interactive)
  (aaron-open-client-document "docs/guide/daily-usage.md" t))

(defalias 'keybinding-guide #'aaron/open-keybinding-guide
  "Open the maintained Emacs keybinding guide.")

(defun aaron-open-normalize-backend (backend)
  "Normalize BACKEND into the canonical backend symbol."
  (general-route-normalize-choice backend aaron-open-backend-aliases))

(defun aaron-open-read-backend (kind &optional prompt default)
  "Read and return a backend for route KIND.
PROMPT overrides the default prompt.  DEFAULT overrides the route's
`:menu-default'."
  (general-route-read-choice
   aaron-open-routes kind prompt default aaron-open-backend-aliases))

(defun aaron-open-resolve-backend (kind &optional backend)
  "Return the concrete backend for KIND and optional BACKEND.
If the selected backend is `menu', prompt with `aaron-open-read-backend'."
  (or (general-route-resolve-choice
       aaron-open-routes kind backend aaron-open-backend-aliases)
      'system))

(defun aaron-open-kind-for-file (file)
  "Return the route kind for FILE."
  (general-route-match-kind aaron-open-routes file 'file))

(defun aaron-open-normalize-url (url)
  "Return URL with an explicit scheme when needed."
  (let ((url (string-trim (substring-no-properties url))))
    (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
        url
      (concat "https://" url))))

(defun aaron-open-normalize-target (target)
  "Return TARGET in a form suitable for a system opener."
  (let* ((target (string-trim (substring-no-properties target)))
         (target
          (if (and (string-prefix-p "fs://" target)
                   (fboundp 'remote-uri-to-file-name))
              (remote-uri-to-file-name target)
            target))
         (expanded (and (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target))
                        (aaron-open-resolve-file-name target))))
    (cond
     ((string-empty-p target) nil)
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target) target)
     ((and expanded (file-exists-p expanded)) expanded)
     ((and expanded (file-name-absolute-p target)) expanded)
     (t target))))

(defun aaron-open--base-window ()
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

(defun aaron-open--side-browser-window (_mode &optional reuse-selected)
  "Return (WINDOW . CREATEDP) for a browser buffer.
When REUSE-SELECTED is non-nil, use the selected window."
  (let ((base (aaron-open--base-window)))
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
          (let ((target (max 20 (floor (* total aaron-open-browser-window-size)))))
            (ignore-errors
              (window-resize new (- target (window-total-width new)) t))))
        (cons new t)))))

(defun aaron-open--with-browser-window (mode reuse-selected open-fn)
  "Run OPEN-FN in a browser window for MODE.
If opening fails after creating a new window, delete that window so failed
browser opens do not leave empty splits behind."
  (let* ((result (aaron-open--side-browser-window mode reuse-selected))
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

(defun aaron/open-eww-url (url &optional reuse-selected)
  "Open URL in a separate EWW buffer shown in a browser side window."
  (interactive (browse-url-interactive-arg "EWW URL: "))
  (require 'eww)
  (aaron-open--with-browser-window
   'eww-mode reuse-selected
   (lambda ()
     (let ((eww-browse-url-new-window-is-tab nil))
       (eww-browse-url (aaron-open-normalize-url url) t)))))

(defun aaron-open-xwidget-url (url &optional reuse-selected)
  "Compatibility wrapper for `+xwidget/open-url'."
  (unless (fboundp '+xwidget/open-url)
    (require 'app/browser/config))
  (+xwidget/open-url url :display 'side :reuse-selected reuse-selected))

(defun aaron/open-portal-url (url &optional reuse-selected)
  "Open URL in a Portal web view in a browser window."
  (interactive (browse-url-interactive-arg "Portal URL: "))
  (require 'term/portal/config)
  (+portal--ensure)
  (require 'portal-web)
  (aaron-open--with-browser-window
   'portal-web-mode reuse-selected
   (lambda ()
     (portal-open-url (aaron-open-normalize-url url))
     (portal-input-focus)
     (current-buffer))))

(defun aaron/open-system-target (target)
  "Open TARGET with the operating system."
  (interactive "sSystem open: ")
  (if-let* ((target (aaron-open-normalize-target target)))
      (let ((target
             (if (and (fboundp 'remote-fs-file-name-p)
                      (remote-fs-file-name-p target))
                 ;; The system opener runs on this machine.  Whether it can
                 ;; reach the file is a placement question, not a target name.
                 (or (remote-client-file-name target)
                     (user-error
                      "System opener cannot directly access target %s"
                      (remote-file-name-target target)))
               target)))
        (cond
         ((fboundp '+macos/open-target)
          (+macos/open-target target))
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

(defun aaron-open-url-with-backend (url backend &optional reuse-selected)
  "Open URL with BACKEND."
  (let ((url (aaron-open-normalize-url url))
        (backend (aaron-open-normalize-backend backend)))
    (pcase backend
      ('eww (aaron/open-eww-url url reuse-selected))
      ('portal (aaron/open-portal-url url reuse-selected))
      ('xwidget
       (unless (fboundp '+xwidget/open-url)
         (require 'app/browser/config))
       (+xwidget/open-url url :display 'side :reuse-selected reuse-selected))
      ('system (aaron/open-system-target url))
      (_ (user-error "Unsupported URL backend: %s" backend)))))

(defun aaron/open-url (url &optional backend)
  "Open URL through the central URL route.
When BACKEND is nil, use the `url' route in `aaron-open-routes'."
  (interactive (list (read-string "URL: ")))
  (aaron-open-url-with-backend url (aaron-open-resolve-backend 'url backend)))

(defun aaron-open-file-with-backend (file backend)
  "Open FILE with BACKEND."
  (let ((file (aaron-open-resolve-file-name file))
        (backend (aaron-open-normalize-backend backend)))
    (pcase backend
      ('emacs (aaron-open-visit-file file))
      ('system (aaron/open-system-target file))
      (_ (user-error "Unsupported file backend: %s" backend)))))

(defun aaron/open-file (file &optional kind backend)
  "Open FILE through the central file route.
KIND defaults to the first matching route, e.g. `pdf'.  BACKEND overrides the
route default."
  (interactive "fOpen file: ")
  (let* ((kind (or kind (aaron-open-kind-for-file file)))
         (backend (aaron-open-resolve-backend kind backend)))
    (aaron-open-file-with-backend file backend)))

;;; Placed visits for agents and scripts (`bin/emacsopen')

(declare-function +windows/file-open-new-frame "ui/windows/config" (file &optional wildcards))
(declare-function +background/show "config/background/config" ())
(declare-function +background-status "config/background/config" ())

(defconst aaron-open-placements '(here other left right above below frame)
  "Where `aaron-open-place' may show a file.")

(defun aaron-open--visible-window (buffer)
  "Return a live window on a visible frame showing BUFFER, or nil."
  (and buffer
       (seq-find (lambda (window)
                   (frame-visible-p (window-frame window)))
                 (get-buffer-window-list buffer nil t))))

(defun aaron-open--placement-window (placement base)
  "Return the window PLACEMENT designates relative to BASE, creating it.
`frame' is handled by the caller."
  (pcase placement
    ('here base)
    ('other (let ((next (next-window base 'nomini)))
              (if (eq next base) (split-window base nil 'right) next)))
    ((or 'left 'right 'above 'below) (split-window base nil placement))
    (_ (user-error "Unknown placement: %s" placement))))

(defun aaron-open--goto (buffer line column)
  "Move point in BUFFER's windows to LINE and COLUMN (both 1-based).
Return non-nil when the buffer holds the text, so the position means
something; a Noema page placeholder does not."
  (when (and line (buffer-live-p buffer))
    (with-current-buffer buffer
      (unless (derived-mode-p 'xwidget-webkit-mode)
        (goto-char (point-min))
        (forward-line (1- (max 1 line)))
        (when column (move-to-column (1- (max 1 column))))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window (point)))
        t))))

(defun aaron-open-place (file &optional placement line column no-focus)
  "Visit FILE at PLACEMENT and return what happened as an alist.
PLACEMENT is one of `aaron-open-placements' (default `here', the selected
window).  `here' and `other' reuse a window already showing FILE; the
splits and `frame' always make a new one.  LINE and COLUMN are 1-based.
With NO-FOCUS the user's selected window and frame are kept and a
background Emacs stays hidden.  Never prompts."
  (let* ((placement (or placement 'here))
         (file (aaron-open-resolve-file-name file))
         (existing (find-buffer-visiting file))
         (shown (and (memq placement '(here other))
                     (aaron-open--visible-window existing)))
         (origin (selected-window))
         window buffer)
    (unless (memq placement aaron-open-placements)
      (user-error "Unknown placement: %s" placement))
    (cond
     (shown (setq window shown buffer existing))
     ((eq placement 'frame)
      (setq buffer (+windows/file-open-new-frame file)
            window (get-buffer-window buffer t)))
     (t
      (setq window (aaron-open--placement-window placement (aaron-open--base-window)))
      (with-selected-window window
        (setq buffer (aaron-open-visit-file file)
              ;; A Noema page may move itself to another window.
              window (or (get-buffer-window buffer t) window)))))
    (let ((positioned (aaron-open--goto buffer line column)))
      (if no-focus
          (when (window-live-p origin)
            (select-frame-set-input-focus (window-frame origin))
            (select-window origin))
        (when (and (fboundp '+background-status)
                   (equal (+background-status) "background"))
          (+background/show))
        (when (window-live-p window)
          (select-frame-set-input-focus (window-frame window))
          (select-window window)))
      `((file . ,file)
        (buffer . ,(if (buffer-live-p buffer) (buffer-name buffer) :null))
        (placement . ,(symbol-name placement))
        (reused . ,(if shown t :false))
        (window . ,(if (window-live-p window) (format "%s" window) :null))
        (frame . ,(if (window-live-p window)
                      (frame-parameter (window-frame window) 'name)
                    :null))
        (line . ,(or line :null))
        (column . ,(or column :null))
        (positioned . ,(if positioned t :false))))))

(defun aaron-open-place-from-client (encoded-file placement line column no-focus)
  "Run `aaron-open-place' for `bin/emacsopen' and return base64 JSON.
ENCODED-FILE is the base64 UTF-8 file name; the other arguments are already
validated by the script.  An error is returned as {\"error\": MESSAGE}
rather than signalled, so the caller always gets one parseable answer."
  (base64-encode-string
   (json-serialize
    (condition-case err
        (let ((inhibit-message t))
          (aaron-open-place (decode-coding-string
                          (base64-decode-string encoded-file) 'utf-8)
                         (intern placement) line column no-focus))
      (error `((error . ,(error-message-string err))))))
   t))

(provide 'aaron-open)
;;; core/aaron-open.el ends here

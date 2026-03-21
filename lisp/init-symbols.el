;;; init-symbols.el --- Buffer and project symbol search -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'subr-x)

(declare-function consult--dynamic-collection "consult" (fun &rest args))
(declare-function consult--format-file-line-match "consult" (file line match))
(declare-function consult--jump-preview "consult" ())
(declare-function consult--lookup-prop "consult" (prop candidates input))
(declare-function consult--project-root "consult" ())
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--temporary-files "consult" ())
(declare-function consult--marker-from-line-column "consult" (buffer line column))
(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function consult-imenu "consult-imenu")
(declare-function consult-imenu-multi "consult-imenu" (&optional query))
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/python-setup-imenu "init-python")
(declare-function xref-backend-apropos "xref" (backend pattern))
(declare-function xref-find-backend "xref" ())
(declare-function xref-item-location "xref" (item))
(declare-function xref-item-summary "xref" (item))
(declare-function xref-find-apropos "xref" (pattern))
(declare-function xref-file-location-column "xref" (location))
(declare-function xref-location-group "xref" (location))
(declare-function xref-location-line "xref" (location))
(declare-function xref-location-marker "xref" (location))
(declare-function xref-pop-to-location "xref" (xref &optional window))

(defvar imenu--index-alist)
(defvar my/symbols-workspace-history nil)

(defun my/symbols--prepare-buffer-imenu ()
  "Refresh the current buffer's imenu data before symbol search."
  (when (and (fboundp 'my/python-setup-imenu)
             (derived-mode-p 'python-mode 'python-ts-mode))
    (my/python-setup-imenu))
  (when (boundp 'imenu--index-alist)
    (setq imenu--index-alist nil)))

(defun my/symbols-buffer ()
  "Search symbols in the current buffer."
  (interactive)
  (my/symbols--prepare-buffer-imenu)
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (imenu nil)))

(defun my/symbols-xref-apropos ()
  "Run `xref-find-apropos' through Consult with preview enabled."
  (interactive)
  (require 'consult)
  (let ((xref-show-xrefs-function #'consult-xref)
        (xref-show-definitions-function #'consult-xref)
        (consult-preview-key '(:debounce 0.15 any)))
    (call-interactively #'xref-find-apropos)))

(defun my/symbols--workspace-xrefs (backend input)
  "Return workspace symbol xrefs for BACKEND and INPUT."
  (let ((query (string-trim (or input ""))))
    (when backend
      (condition-case nil
          (xref-backend-apropos backend query)
        (error nil)))))

(defun my/symbols--workspace-candidates (backend input)
  "Format workspace symbol candidates from BACKEND matching INPUT."
  (require 'consult)
  (let ((root (consult--project-root)))
    (mapcar
     (lambda (xref)
       (let* ((loc (xref-item-location xref))
              (group (xref-location-group loc))
              (group (if root (string-remove-prefix root group) group))
              (cand (consult--format-file-line-match
                     group
                     (or (xref-location-line loc) 0)
                     (xref-item-summary xref))))
         (add-text-properties
          0 1 `(my-symbol-xref ,xref consult--prefix-group ,group)
          cand)
         cand))
     (my/symbols--workspace-xrefs backend input))))

(defun my/symbols--workspace-preview-state ()
  "Build the preview state function for workspace symbol candidates."
  (require 'consult)
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (action cand)
      (unless (and cand (eq action 'preview))
        (funcall open))
      (let ((consult--buffer-display #'switch-to-buffer))
        (funcall
         preview action
         (when-let* ((input
                      (and (active-minibuffer-window)
                           (with-selected-window (active-minibuffer-window)
                             (string-trim
                              (minibuffer-contents-no-properties)))))
                     ((not (string-empty-p input)))
                     (xref (and cand
                                (eq action 'preview)
                                cand))
                     (loc (xref-item-location xref))
                     (type (type-of loc))
                     ((memq type '(xref-buffer-location
                                   xref-file-location
                                   xref-etags-location))))
           (pcase type
             ((or 'xref-file-location 'xref-etags-location)
              (consult--marker-from-line-column
               (funcall open
                        (let ((xref-file-name-display 'abs))
                          (xref-location-group loc)))
               (xref-location-line loc)
               (if (eq type 'xref-file-location)
                   (xref-file-location-column loc)
                 0)))
             (_
              (xref-location-marker loc)))))))))

(defun my/symbols-workspace-dynamic ()
  "Search workspace symbols with local filtering and preview.
Prefer a broad workspace snapshot for tolerant matching.  Fall
back to live backend queries when the server does not return
results for an empty workspace-symbol query."
  (interactive)
  (require 'consult)
  (let ((backend (xref-find-backend)))
    (unless backend
      (user-error "No active xref backend in the current buffer"))
    (let* ((broad-candidates
            (let ((message-log-max nil))
              (message "Loading workspace symbols...")
              (prog1
                  (my/symbols--workspace-candidates backend "")
                (message nil))))
           (source
            (or broad-candidates
                (consult--dynamic-collection
                 (lambda (input)
                   (my/symbols--workspace-candidates backend input))
                 :min-input 1
                 :throttle 0.0
                 :debounce 0.15)))
           (candidate
            (consult--read
             source
             :prompt "Workspace symbol: "
             :command #'my/symbols-workspace-dynamic
             :history 'my/symbols-workspace-history
             :require-match t
             :sort nil
             :category 'my-workspace-symbol
             :group #'consult--prefix-group
             :state (my/symbols--workspace-preview-state)
             :preview-key '(:debounce 0.15 any)
             :lookup (apply-partially #'consult--lookup-prop 'my-symbol-xref))))
      (xref-pop-to-location candidate))))

(defun my/symbols-project ()
  "Search project symbols.
Prefer workspace symbols from the active language server.  Fall
back to `consult-imenu-multi', which only covers opened buffers."
  (interactive)
  (cond
   ((and (fboundp 'my/current-language-server-backend)
         (my/current-language-server-backend))
    (call-interactively #'my/symbols-workspace-dynamic))
   ((fboundp 'consult-imenu-multi)
    (consult-imenu-multi))
   (t
    (my/symbols-buffer))))

(global-set-key (kbd "M-g i") #'my/symbols-buffer)

(my/evil-global-leader-set "s b" #'my/symbols-buffer "buffer symbols")
(my/evil-global-leader-set "s I" #'my/symbols-project "project symbols")

(provide 'init-symbols)
;;; init-symbols.el ends here

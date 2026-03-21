;;; init-symbols.el --- Buffer and project symbol search -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
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
(declare-function eglot--workspace-symbols "eglot" (pat &optional buffer))
(declare-function eglot--xref-make-match "eglot" (name uri range))
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/project-current-root "init-project")
(declare-function my/python-setup-imenu "init-python")
(declare-function lsp-feature? "lsp-mode" (method))
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
(defvar my/symbols-file-line-history nil)
(defvar my/symbols-project-fallback-alist nil)
(defvar my/symbols-workspace-history nil)
(defvar transient--original-buffer)

(defun my/symbols--candidate-xref (cand)
  "Return the xref object stored on workspace symbol candidate CAND."
  (cond
   ((stringp cand)
    (get-text-property 0 'my-symbol-xref cand))
   ((and cand
         (memq (type-of cand)
               '(xref-item xref-match-item)))
    cand)
   (t nil)))

(defun my/symbols--origin-buffer ()
  "Return the source buffer that initiated the current symbol command."
  (cond
   ((and (boundp 'transient--original-buffer)
         (buffer-live-p transient--original-buffer))
    transient--original-buffer)
   ((and (active-minibuffer-window)
         (window-live-p (minibuffer-selected-window)))
    (window-buffer (minibuffer-selected-window)))
   (t
    (current-buffer))))

(defun my/symbols--call-in-origin-buffer (fn)
  "Call FN in the source buffer that initiated the current command."
  (let ((buffer (my/symbols--origin-buffer)))
    (if (eq buffer (current-buffer))
        (funcall fn)
      (if-let* ((window (get-buffer-window buffer t)))
          (with-selected-window window
            (funcall fn))
        (with-current-buffer buffer
          (funcall fn))))))

(defun my/symbols-register-project-fallback (mode function)
  "Register FUNCTION as a project-symbol fallback for MODE."
  (setq my/symbols-project-fallback-alist
        (cons (cons mode function)
              (cl-remove-if
               (lambda (entry)
                 (eq (car entry) mode))
               my/symbols-project-fallback-alist))))

(defun my/symbols--project-fallback-function ()
  "Return the project-symbol fallback function for the current buffer."
  (cl-loop for (mode . function) in my/symbols-project-fallback-alist
           when (derived-mode-p mode)
           return function))

(defun my/symbols--project-root ()
  "Return the active project root, or `default-directory' as fallback."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (and (fboundp 'consult--project-root)
           (consult--project-root))
      default-directory))

(defun my/symbols-make-file-line-candidate (root file line match)
  "Build a Consult candidate rooted at ROOT for FILE, LINE and MATCH."
  (let* ((group (if root
                    (string-remove-prefix root file)
                  file))
         (cand (copy-sequence match)))
    (add-text-properties
     0 1 `(my-symbol-file ,file
                          my-symbol-line ,line
                          consult--prefix-group ,group)
     cand)
    cand))

(defun my/symbols--file-line-marker (cand &optional opener)
  "Return the marker stored by file-line candidate CAND.
Use OPENER to open the file temporarily when provided."
  (when-let* ((file (and (stringp cand)
                         (get-text-property 0 'my-symbol-file cand)))
              (line (get-text-property 0 'my-symbol-line cand)))
    (consult--marker-from-line-column
     (funcall (or opener #'find-file-noselect) file)
     line
     0)))

(defun my/symbols--file-line-preview-state ()
  "Build the preview state function for file-line symbol candidates."
  (require 'consult)
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (action cand)
      (unless (and cand (eq action 'preview))
        (funcall open))
      (let ((consult--buffer-display #'switch-to-buffer))
        (funcall preview action
                 (and (eq action 'preview)
                      (my/symbols--file-line-marker cand open)))))))

(defun my/symbols-read-file-line-candidates
    (candidates &optional prompt history category)
  "Read static file-line CANDIDATES with live preview and fuzzy filtering."
  (when candidates
    (require 'consult)
    (let ((candidate
           (let ((completion-styles '(flex orderless basic))
                 (completion-category-defaults nil)
                 (completion-category-overrides
                  `((,(or category 'my-symbol-file-line)
                     (styles . (flex orderless basic))))))
             (consult--read
              candidates
              :prompt (or prompt "Project symbol: ")
              :history (or history 'my/symbols-file-line-history)
              :require-match t
              :sort nil
              :category (or category 'my-symbol-file-line)
              :group #'consult--prefix-group
              :state (my/symbols--file-line-preview-state)
              :preview-key '(:debounce 0.15 any)))))
      (when-let* ((marker (my/symbols--file-line-marker candidate)))
        (pop-to-buffer-same-window (marker-buffer marker))
        (goto-char marker)
        (recenter)
        t))))

(defun my/symbols--prepare-buffer-imenu ()
  "Refresh the current buffer's imenu data before symbol search."
  (when (and (fboundp 'my/python-setup-imenu)
             (derived-mode-p 'python-mode 'python-ts-mode))
    (my/python-setup-imenu))
  (when (boundp 'imenu--index-alist)
    (setq imenu--index-alist nil)))

(defun my/symbols--buffer ()
  "Search symbols in the current buffer."
  (my/symbols--prepare-buffer-imenu)
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (imenu nil)))

(defun my/symbols-buffer ()
  "Search symbols in the current buffer."
  (interactive)
  (my/symbols--call-in-origin-buffer #'my/symbols--buffer))

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

(defun my/symbols--workspace-query-seeds (input)
  "Return progressively broader workspace-symbol queries for INPUT."
  (let* ((query (string-trim (or input "")))
         (length (length query))
         (first-token (car (split-string query "[^[:alnum:]_:/.-]+" t)))
         seeds)
    (unless (string-empty-p query)
      (setq seeds
            (delete-dups
             (delq nil
                   (list query
                         first-token
                         (and (> length 2) (substring query 0 2))
                         (substring query 0 1))))))
    seeds))

(defun my/symbols--workspace-query-xrefs (backend input)
  "Return workspace symbol xrefs for BACKEND matching INPUT.
When the backend does not handle fuzzy or incomplete queries well,
fall back to broader prefixes and let the minibuffer do the final
filtering."
  (or (my/symbols--workspace-xrefs backend input)
      (cl-loop for seed in (my/symbols--workspace-query-seeds input)
               for xrefs = (unless (equal seed (string-trim (or input "")))
                             (my/symbols--workspace-xrefs backend seed))
               thereis xrefs)))

(defun my/symbols--workspace-candidates (backend input)
  "Format workspace symbol candidates from BACKEND matching INPUT."
  (require 'consult)
  (let ((root (consult--project-root)))
    (cond
     ((and (eq backend 'eglot)
           (fboundp 'eglot--workspace-symbols)
           (fboundp 'eglot--xref-make-match))
      (mapcar
       (lambda (item)
         (let* ((meta (get-text-property 0 'eglot--lsp-workspaceSymbol item))
                (name (plist-get meta :name))
                (location (plist-get meta :location))
                (xref (and name
                           location
                           (eglot--xref-make-match
                            name
                            (plist-get location :uri)
                            (plist-get location :range))))
                (loc (and xref (xref-item-location xref)))
                (group (and loc (xref-location-group loc)))
                (group (if (and root group)
                           (string-remove-prefix root group)
                         group))
                (cand (copy-sequence item)))
           (add-text-properties
            0 1 `(my-symbol-xref ,xref consult--prefix-group ,group)
            cand)
           cand))
       (eglot--workspace-symbols input (current-buffer))))
     (t
      (mapcar
       (lambda (xref)
         (let* ((loc (xref-item-location xref))
                (group (xref-location-group loc))
                (group (if root (string-remove-prefix root group) group))
                (cand (copy-sequence (xref-item-summary xref))))
           (add-text-properties
            0 1 `(my-symbol-xref ,xref consult--prefix-group ,group)
            cand)
           cand))
       (my/symbols--workspace-query-xrefs backend input))))))

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
                                (my/symbols--candidate-xref cand)))
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

(defun my/symbols--workspace-open-query (backend query)
  "Open workspace symbol QUERY through BACKEND.
When QUERY resolves to a single symbol, jump directly.  When it
resolves to multiple symbols, show them through `consult-xref'."
  (let* ((query (string-trim (or query "")))
         (xrefs (and (not (string-empty-p query))
                     (my/symbols--workspace-query-xrefs backend query))))
    (unless xrefs
      (user-error "No workspace symbols matching: %s" query))
    (if (cdr xrefs)
        (consult-xref (lambda () xrefs))
      (xref-pop-to-location (car xrefs)))))

(defun my/symbols--workspace-capable-p ()
  "Return non-nil when workspace-symbol search should be attempted."
  (pcase (and (fboundp 'my/current-language-server-backend)
              (my/current-language-server-backend))
    ('lsp-mode
     (if (fboundp 'lsp-feature?)
         (lsp-feature? "workspace/symbol")
       t))
    (_ t)))

(defun my/symbols--project-fallback ()
  "Run the project-symbol fallback for the current buffer, if available."
  (when-let* ((function (my/symbols--project-fallback-function)))
    (funcall function)))

(defun my/symbols--workspace-dynamic (&optional fallback-on-empty)
  "Search workspace symbols with local filtering and preview.
Prefer a broad workspace snapshot for tolerant matching.  Fall
back to live backend queries when the server does not return
results for an empty workspace-symbol query.
When FALLBACK-ON-EMPTY is non-nil, run the registered project fallback
instead of opening an empty workspace symbol picker."
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
           (fallback-used
            (and fallback-on-empty
                 (null broad-candidates)
                 (my/symbols--project-fallback)))
           (source
            (or fallback-used
                broad-candidates
                (consult--dynamic-collection
                 (lambda (input)
                   (my/symbols--workspace-candidates backend input))
                 :min-input 0
                 :throttle 0.0
                 :debounce 0.15)))
           (candidate
            (unless fallback-used
              (let ((completion-styles '(flex orderless basic))
                    (completion-category-defaults nil)
                    (completion-category-overrides
                     '((my-workspace-symbol (styles . (flex orderless basic))))))
                (consult--read
                 source
                 :prompt "Workspace symbol: "
                 :command #'my/symbols-workspace-dynamic
                 :history 'my/symbols-workspace-history
                 :require-match nil
                 :sort nil
                 :category 'my-workspace-symbol
                 :group #'consult--prefix-group
                 :state (my/symbols--workspace-preview-state)
                 :preview-key '(:debounce 0.15 any)
                 :lookup (apply-partially #'consult--lookup-prop 'my-symbol-xref))))))
      (cond
       (fallback-used
        t)
       ((if-let* ((xref (my/symbols--candidate-xref candidate)))
            (xref-pop-to-location xref)
          (my/symbols--workspace-open-query backend candidate))
        t)))))

(defun my/symbols-workspace-dynamic ()
  "Search workspace symbols with local filtering and preview."
  (interactive)
  (my/symbols--call-in-origin-buffer #'my/symbols--workspace-dynamic))

(defun my/symbols--project ()
  "Search project symbols.
Prefer workspace symbols from the active language server.  Fall
back to `consult-imenu-multi', which only covers opened buffers."
  (cond
   ((and (fboundp 'my/current-language-server-backend)
         (my/current-language-server-backend)
         (my/symbols--workspace-capable-p))
    (my/symbols--workspace-dynamic t))
   ((my/symbols--project-fallback))
   ((fboundp 'consult-imenu-multi)
    (consult-imenu-multi))
   (t
    (my/symbols--buffer))))

(defun my/symbols-project ()
  "Search project symbols."
  (interactive)
  (my/symbols--call-in-origin-buffer #'my/symbols--project))

(global-set-key (kbd "M-g i") #'my/symbols-buffer)

(my/evil-global-leader-set "s b" #'my/symbols-buffer "buffer symbols")
(my/evil-global-leader-set "s I" #'my/symbols-project "project symbols")

(provide 'init-symbols)
;;; init-symbols.el ends here

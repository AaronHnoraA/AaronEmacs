;;; tools/symbols/config.el --- Buffer and project symbol search -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'aaron-funcs)
(require 'aaron-open)
(require 'subr-x)

(defvar consult--buffer-display)
(defvar consult-preview-key)
(defvar xref-file-name-display)
(defvar xref-show-definitions-function)
(defvar xref-show-xrefs-function)

(declare-function consult--format-file-line-match "consult" (file line match))
(declare-function consult--dynamic-collection "consult" (&rest args))
(declare-function consult--jump-preview "consult" ())
(declare-function consult--lookup-prop "consult" (prop candidates input narrowing))
(declare-function consult--prefix-group "consult" (candidate transform))
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--temporary-files "consult" ())
(declare-function consult--marker-from-line-column "consult" (buffer line column))
(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function consult-imenu "consult-imenu")
(declare-function consult-imenu-multi "consult-imenu" (&optional query))
(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +project-current-root "tools/project/config")
(declare-function +python/setup-imenu "lang/python/config")
(declare-function lsp-feature? "lsp-mode" (method))
(declare-function xref-backend-apropos "xref" (backend pattern))
(declare-function xref-file-location-column "xref" (xref-file-location))
(declare-function xref-make "xref" (summary location))
(declare-function xref-find-backend "xref" ())
(declare-function xref-item-location "xref" (xref))
(declare-function xref-item-summary "xref" (xref))
(declare-function xref-location-group "xref" (location))
(declare-function xref-location-line "xref" (location))
(declare-function xref-location-marker "xref" (location))
(declare-function xref-pop-to-location "xref" (location &optional display-action always-show))
(declare-function xref-find-apropos "xref" (pattern))

(defgroup +symbols nil
  "Buffer and project symbol search."
  :group 'convenience)

(config-defvar +symbols-preview-key nil
  "Preview strategy for symbol pickers."
  :type 'sexp
  :group '+symbols)

(defvar imenu--index-alist)
(defvar +symbols-file-line-history nil)
(defvar +symbols-workspace-history nil)
(defvar +symbols-project-fallback-alist nil)
(defvar transient--original-buffer)

(defun +symbols--location-summary (location)
  "Build a readable summary line for xref LOCATION."
  (condition-case nil
      (let* ((marker (xref-location-marker location))
             (buffer (marker-buffer marker)))
        (if (not (buffer-live-p buffer))
            ""
          (with-current-buffer buffer
            (save-excursion
              (goto-char marker)
              (let ((summary
                     (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))))
                (if (string-empty-p summary)
                    (or (xref-location-group location) "")
                  summary))))))
    (error
     (or (ignore-errors (xref-location-group location))
         ""))))

(defun +symbols--ensure-xref-item (object)
  "Return OBJECT as an xref item.
Some backends return bare xref locations for apropos results
instead of `xref-item' values."
  (cond
   ((ignore-errors
      (xref-item-location object))
    object)
   ((ignore-errors
      (xref-location-group object))
    (xref-make (+symbols--location-summary object) object))
   (t nil)))

(defun +symbols--origin-buffer ()
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

(defun +symbols--call-in-origin-buffer (fn)
  "Call FN in the source buffer that initiated the current command."
  (let ((buffer (+symbols--origin-buffer)))
    (if (eq buffer (current-buffer))
        (funcall fn)
      (if-let* ((window (get-buffer-window buffer t)))
          (with-selected-window window
            (funcall fn))
        (with-current-buffer buffer
          (funcall fn))))))

(defun +symbols-register-project-fallback (mode function)
  "Register FUNCTION as a project-symbol fallback for MODE."
  (setq +symbols-project-fallback-alist
        (cons (cons mode function)
              (cl-remove-if
               (lambda (entry)
                 (eq (car entry) mode))
               +symbols-project-fallback-alist))))

(defun +symbols--project-root ()
  "Return the active project root, or `default-directory' as fallback."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      default-directory))

(defun +symbols--project-fallback-function ()
  "Return the project-symbol fallback function for the current buffer."
  (cl-loop for (mode . function) in +symbols-project-fallback-alist
           when (derived-mode-p mode)
           return function))

(defun +symbols-make-file-line-candidate (root file line match)
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

(defun +symbols--file-line-marker (cand &optional opener)
  "Return the marker stored by file-line candidate CAND.
Use OPENER to open the file temporarily when provided."
  (when-let* ((file (and (stringp cand)
                         (get-text-property 0 'my-symbol-file cand)))
              (line (get-text-property 0 'my-symbol-line cand)))
    (consult--marker-from-line-column
     (aaron-open-call-with-file-context
      file (or opener #'find-file-noselect))
     line
     0)))

(defun +symbols--file-line-preview-state ()
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
                      (+symbols--file-line-marker cand open)))))))

(defun +symbols-read-file-line-candidates
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
              :history (or history '+symbols-file-line-history)
              :require-match t
              :sort nil
              :category (or category 'my-symbol-file-line)
              :group #'consult--prefix-group
              :state (+symbols--file-line-preview-state)
              :preview-key '(:debounce 0.15 any)))))
      (when-let* ((marker (+symbols--file-line-marker candidate)))
        (pop-to-buffer-same-window (marker-buffer marker))
        (goto-char marker)
        (recenter)
        t))))

(defun +symbols--xref-candidate (xref root)
  "Build a Consult candidate for XREF relative to ROOT."
  (when-let* ((xref (+symbols--ensure-xref-item xref))
              (loc (xref-item-location xref))
              (group (xref-location-group loc))
              (group (if (and root (stringp group))
                         (string-remove-prefix root group)
                       group))
              (cand (consult--format-file-line-match
                     (or group "")
                     (or (xref-location-line loc) 0)
                     (or (xref-item-summary xref) ""))))
    (add-text-properties
     0 1 `(my-symbol-xref ,xref
                          consult--prefix-group ,group)
     cand)
    cand))

(defun +symbols--workspace-preview-state ()
  "Build the preview state function for workspace symbol candidates."
  (require 'consult)
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (action cand)
      (unless (and cand (eq action 'preview))
        (funcall open))
      (let ((consult--buffer-display #'switch-to-buffer))
        (funcall preview action
                 (when-let* ((xref (and (eq action 'preview) cand))
                             (loc (xref-item-location xref)))
                   (pcase (type-of loc)
                     ((or 'xref-file-location 'xref-etags-location)
                      (consult--marker-from-line-column
                       (funcall open
                                (let ((xref-file-name-display 'abs))
                                  (xref-location-group loc)))
                       (xref-location-line loc)
                       (if (eq (type-of loc) 'xref-file-location)
                           (xref-file-location-column loc)
                         0)))
                     (_
                      (xref-location-marker loc)))))))))

(defun +symbols--workspace-candidates (buffer backend root input)
  "Return workspace symbol candidates for INPUT from BACKEND in BUFFER."
  (when (and (buffer-live-p buffer)
             (not (string-empty-p input)))
    (with-current-buffer buffer
      (condition-case nil
          (delq nil
                (mapcar (lambda (xref)
                          (+symbols--xref-candidate xref root))
                        (xref-backend-apropos backend input)))
        (error nil)))))

(defun +symbols--prepare-buffer-imenu ()
  "Refresh the current buffer's imenu data before symbol search."
  (when (and (fboundp '+python/setup-imenu)
             (derived-mode-p 'python-mode 'python-ts-mode))
    (+python/setup-imenu))
  (when (boundp 'imenu--index-alist)
    (setq imenu--index-alist nil)))

(defun +symbols--buffer ()
  "Search symbols in the current buffer."
  (+symbols--prepare-buffer-imenu)
  (let ((consult-preview-key +symbols-preview-key))
    (if (fboundp 'consult-imenu)
        (consult-imenu)
      (imenu nil))))

(defun +symbols/buffer ()
  "Search symbols in the current buffer."
  (interactive)
  (+symbols--call-in-origin-buffer #'+symbols--buffer))

(defun +symbols/xref-apropos ()
  "Search workspace symbols with live preview while typing."
  (interactive)
  (require 'consult)
  (let* ((buffer (+symbols--origin-buffer))
         (backend (with-current-buffer buffer
                    (xref-find-backend)))
         (root (with-current-buffer buffer
                 (+symbols--project-root)))
         (initial (with-current-buffer buffer
                    (thing-at-point 'symbol t))))
    (unless backend
      (user-error "No xref backend in current buffer"))
    (when-let* ((xref
                 (consult--read
                  (consult--dynamic-collection
                    (lambda (input)
                      (+symbols--workspace-candidates
                       buffer backend root input))
                    :min-input 1
                    :throttle 0.15
                    :debounce 0.1
                    :highlight t)
                  :prompt "Workspace symbol: "
                  :history '+symbols-workspace-history
                  :add-history initial
                  :require-match t
                  :sort nil
                  :category 'my-workspace-symbol
                  :group #'consult--prefix-group
                  :state (+symbols--workspace-preview-state)
                  :preview-key +symbols-preview-key
                  :lookup (apply-partially #'consult--lookup-prop
                                           'my-symbol-xref))))
      (xref-pop-to-location xref))))

(defun +symbols--workspace-capable-p ()
  "Return non-nil when workspace-symbol search should be attempted."
  (pcase (and (fboundp '+current-language-server-backend)
              (+current-language-server-backend))
    ('lsp-mode
     (if (fboundp 'lsp-feature?)
         (lsp-feature? "workspace/symbol")
       t))
    (_ t)))

(defun +symbols--project-fallback ()
  "Run the project-symbol fallback for the current buffer, if available."
  (when-let* ((function (+symbols--project-fallback-function)))
    (funcall function)))

(defun +symbols--project ()
  "Search project symbols.
Prefer workspace symbols from the active language server.  Fall
back to `consult-imenu-multi', which only covers opened buffers."
  (cond
   ((and (fboundp '+current-language-server-backend)
         (+current-language-server-backend)
         (+symbols--workspace-capable-p))
    (+symbols/xref-apropos))
   ((+symbols--project-fallback))
   ((fboundp 'consult-imenu-multi)
    (let ((consult-preview-key +symbols-preview-key))
      (consult-imenu-multi)))
   (t
    (+symbols--buffer))))

(defun +symbols/project ()
  "Search project symbols."
  (interactive)
  (+symbols--call-in-origin-buffer #'+symbols--project))

(global-set-key (kbd "M-g i") #'+symbols/buffer)

(define-leader-key!
  "s b" '(:def +symbols/buffer :which-key "buffer symbols")
  "s I" '(:def +symbols/project :which-key "project symbols"))

(provide 'tools/symbols/config)
;;; tools/symbols/config.el ends here

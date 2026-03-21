;;; init-symbols.el --- Buffer and project symbol search -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'subr-x)

(declare-function consult--format-file-line-match "consult" (file line match))
(declare-function consult--jump-preview "consult" ())
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--temporary-files "consult" ())
(declare-function consult--marker-from-line-column "consult" (buffer line column))
(declare-function consult-xref "consult" (fetcher &optional alist))
(declare-function consult-imenu "consult-imenu")
(declare-function consult-imenu-multi "consult-imenu" (&optional query))
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/python-setup-imenu "init-python")
(declare-function lsp-feature? "lsp-mode" (method))
(declare-function xref-find-apropos "xref" (pattern))

(defvar imenu--index-alist)
(defvar my/symbols-file-line-history nil)
(defvar my/symbols-project-fallback-alist nil)
(defvar transient--original-buffer)

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

(defun my/symbols--project ()
  "Search project symbols.
Prefer workspace symbols from the active language server.  Fall
back to `consult-imenu-multi', which only covers opened buffers."
  (cond
   ((and (fboundp 'my/current-language-server-backend)
         (my/current-language-server-backend)
         (my/symbols--workspace-capable-p))
    (my/symbols-xref-apropos))
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

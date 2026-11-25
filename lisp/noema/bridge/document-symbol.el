;;; document-symbol.el --- Live Emacs help for Noema documents -*- lexical-binding: t; -*-

(require 'find-func)
(require 'help-fns)
(require 'seq)
(require 'subr-x)

(defun +noema--document-symbol-kind (symbol)
  "Return the primary help kind of SYMBOL."
  (cond ((commandp symbol) "Command")
        ((macrop symbol) "Macro")
        ((fboundp symbol) "Function")
        ((custom-variable-p symbol) "Option")
        ((boundp symbol) "Variable")
        ((facep symbol) "Face")))

(defun +noema--document-symbol-summary (symbol type)
  "Read the first help paragraph of SYMBOL of TYPE."
  (let* ((property (pcase type
                     ('defun 'function-documentation)
                     ('defvar 'variable-documentation)
                     ('defface 'face-documentation)))
         (owner (if (and (eq type 'defvar) (not (get symbol property)))
                    (indirect-variable symbol)
                  symbol))
         (stored (get owner property))
         (doc (cond ((documentation-stringp stored) (documentation-property owner property t))
                    ((and (eq type 'defun) (not stored)) (documentation symbol t)))))
    (when (and (stringp doc) (not (string-blank-p doc)))
      (let* ((trimmed (string-trim doc))
             (end (string-match "\n[ \t]*\n" trimmed))
             (paragraph (substring trimmed 0 end)))
        (truncate-string-to-width
         (replace-regexp-in-string "[ \t\n]+" " " (substitute-command-keys paragraph))
         480 nil nil "…")))))

(defun +noema--document-symbol-signature (symbol)
  "Return a printable help signature for SYMBOL."
  (let* ((doc (get symbol 'function-documentation))
         (args (help-function-arglist symbol (or (not doc) (documentation-stringp doc))))
         (print-circle t)
         (print-level 4)
         (print-length 24))
    (when (listp args)
      (truncate-string-to-width (prin1-to-string (cons symbol args)) 240 nil nil "…"))))

(defun +noema--document-symbol-details (symbol kind type source)
  "Return the lookup details for SYMBOL, of KIND and TYPE, loaded from SOURCE."
  (let ((documentation-dynamic-reload nil)
        (keys (when (commandp symbol)
                (mapcar #'key-description (seq-take (where-is-internal symbol (current-global-map)) 4))))
        (file (and (stringp source)
                   (replace-regexp-in-string "\\.elc\\'" ".el" (file-name-nondirectory source)))))
    (append '((ok . t) (found . t))
            (list (cons 'kind kind) (cons 'keys (vconcat keys)))
            (when-let* ((signature (and (eq type 'defun) (+noema--document-symbol-signature symbol))))
              (list (cons 'signature signature)))
            (when-let* ((summary (ignore-errors (+noema--document-symbol-summary symbol type))))
              (list (cons 'summary summary)))
            (and file (list (cons 'file file))))))

(defun +noema--document-symbol-api (params _client)
  "Inspect a loaded Emacs symbol or open its native help from PARAMS."
  (let* ((name (alist-get 'name params))
         (action (or (alist-get 'action params) "inspect")))
    (unless (and (stringp name) (<= 1 (length name) 160)
                 (not (string-match-p "[[:space:][:cntrl:]()\"']" name)))
      (user-error "Use one Emacs symbol name"))
    (let* ((symbol (intern-soft name))
           (kind (and symbol (+noema--document-symbol-kind symbol)))
           (type (cond ((and kind (fboundp symbol)) 'defun)
                       ((and kind (boundp symbol)) 'defvar)
                       ((and kind (facep symbol)) 'defface)))
           (source (and type (symbol-file symbol type))))
      (pcase action
        ("inspect"
         (if kind
             (+noema--document-symbol-details symbol kind type source)
           '((ok . t) (found . :json-false))))
        ("help"
         (unless kind (user-error "Symbol is unavailable in this Emacs session"))
         (describe-symbol symbol)
         '((ok . t)))
        ("definition"
         (unless source (user-error "The loaded definition has no Lisp source"))
         (pcase type
           ('defun (find-function-other-window symbol))
           ('defvar (find-variable-other-window symbol))
           ('defface (find-face-definition symbol)))
         '((ok . t)))
        (_ (user-error "Unknown symbol action"))))))

(provide 'noema/bridge/document-symbol)
;;; document-symbol.el ends here

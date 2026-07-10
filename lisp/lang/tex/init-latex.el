;;; init-latex.el --- LaTeX/BibTeX LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `texlab' when installed, otherwise fall back to `digestif'.
;; This keeps LaTeX/BibTeX buffers on the same Eglot-based workflow as the
;; rest of the configuration while preserving the existing latexmk/XeLaTeX
;; build setup from AUCTeX.

;;; Code:

(require 'aaron-ui nil t)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'url)
(require 'url-http)

(declare-function aaron-ui-color "aaron-ui" (token &optional fallback variant))
(declare-function my/executable-or-name "init-utils")
(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

(defcustom my/zotero-better-bibtex-rpc-url
  "http://127.0.0.1:23119/better-bibtex/json-rpc"
  "Local Better BibTeX JSON-RPC endpoint."
  :type 'string
  :group 'zotero)

(defcustom my/zotero-better-bibtex-picker-url
  "http://127.0.0.1:23119/better-bibtex/cayw"
  "Local Better BibTeX citation picker endpoint."
  :type 'string
  :group 'zotero)

(defcustom my/zotero-reference-cache-ttl 600
  "Seconds to retain resolved Zotero reference links."
  :type 'integer
  :group 'zotero)

(defcustom my/zotero-reference-cache-limit 96
  "Maximum number of resolved Zotero reference links to retain."
  :type 'integer
  :group 'zotero)

(defvar my/zotero-reference-cache (make-hash-table :test #'equal)
  "Short-lived cache of citation metadata to Zotero select links.")

(defvar my/zotero-better-bibtex-request-id 0)

(defun my/zotero-better-bibtex--response ()
  "Parse the HTTP response in the current buffer as JSON."
  (when (and (boundp 'url-http-response-status)
             (numberp url-http-response-status)
             (not (<= 200 url-http-response-status 299)))
    (error "Better BibTeX returned HTTP %s" url-http-response-status))
  (goto-char (or (and (boundp 'url-http-end-of-headers)
                      url-http-end-of-headers)
                 (point-min)))
  (json-parse-buffer :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun my/zotero-better-bibtex-rpc (method params)
  "Call Better BibTeX JSON-RPC METHOD with vector PARAMS."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Zotero-Allowed-Request" . "true")))
         (url-request-data
          (json-serialize
           (list :jsonrpc "2.0"
                 :method method
                 :params params
                 :id (cl-incf my/zotero-better-bibtex-request-id))))
         (buffer (url-retrieve-synchronously
                  my/zotero-better-bibtex-rpc-url t t 4)))
    (unless (buffer-live-p buffer)
      (error "Better BibTeX is unavailable; start Zotero"))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((reply (my/zotero-better-bibtex--response))
                 (rpc-error (alist-get 'error reply)))
            (when rpc-error
              (error "Better BibTeX: %s"
                     (or (alist-get 'message rpc-error) "request failed")))
            (alist-get 'result reply)))
      (kill-buffer buffer))))

(defun my/zotero-normalize-doi (doi)
  "Return DOI without a URL or `doi:' prefix."
  (let ((value (string-trim (or doi ""))))
    (setq value (replace-regexp-in-string
                 "\\`https?://\\(?:dx\\.\\)?doi\\.org/" "" value t t))
    (replace-regexp-in-string "\\`doi:[[:space:]]*" "" value t t)))

(defun my/zotero-reference-cache-key (payload)
  "Return a stable cache key for reference PAYLOAD."
  (mapconcat
   #'identity
   (list (downcase (string-trim (or (alist-get 'key payload) "")))
         (downcase (my/zotero-normalize-doi (alist-get 'doi payload)))
         (downcase (string-trim (or (alist-get 'title payload) ""))))
   "\0"))

(defun my/zotero-reference-cache-prune ()
  "Remove expired and excess reference cache entries."
  (let ((now (float-time))
        entries)
    (maphash
     (lambda (key value)
       (if (> (- now (car value)) my/zotero-reference-cache-ttl)
           (remhash key my/zotero-reference-cache)
         (push (cons key (car value)) entries)))
     my/zotero-reference-cache)
    (when (> (hash-table-count my/zotero-reference-cache)
             my/zotero-reference-cache-limit)
      (setq entries (sort entries (lambda (a b) (< (cdr a) (cdr b)))))
      (dotimes (index (- (length entries) my/zotero-reference-cache-limit))
        (remhash (car (nth index entries)) my/zotero-reference-cache)))))

(defun my/zotero-reference-cache-get (payload)
  "Return a cached Zotero link for PAYLOAD, or nil."
  (my/zotero-reference-cache-prune)
  (let ((value (gethash (my/zotero-reference-cache-key payload)
                        my/zotero-reference-cache)))
    (and value (cdr value))))

(defun my/zotero-reference-cache-put (payload uri)
  "Cache URI for reference PAYLOAD."
  (puthash (my/zotero-reference-cache-key payload)
           (cons (float-time) uri)
           my/zotero-reference-cache)
  (my/zotero-reference-cache-prune)
  uri)

(defun my/zotero-better-bibtex-search (terms)
  "Search all local Zotero libraries using Better BibTeX TERMS."
  (my/zotero-better-bibtex-rpc
   "item.search"
   (vector terms "*")))

(defun my/zotero-result-label (result)
  "Return a completion label for Better BibTeX RESULT."
  (format "%s - %s - %s%s"
          (or (alist-get 'citekey result)
              (alist-get 'citation-key result)
              "no citekey")
          (or (alist-get 'title result) "Untitled")
          (or (alist-get 'library result) "Zotero")
          (let ((doi (or (alist-get 'DOI result) (alist-get 'doi result))))
            (if (and doi (not (string-empty-p doi)))
                (format " - %s" doi)
              ""))))

(defun my/zotero-choose-result (results)
  "Return one RESULT, prompting when RESULTS is ambiguous."
  (setq results
        (seq-uniq results
                  (lambda (left right)
                    (equal (alist-get 'id left) (alist-get 'id right)))))
  (pcase (length results)
    (0 nil)
    (1 (car results))
    (_
     (let* ((candidates
             (cl-loop for result in results
                      for index from 1
                      collect (cons (format "%s  #%d"
                                            (my/zotero-result-label result)
                                            index)
                                    result)))
            (choice (completing-read "Zotero reference: " candidates nil t)))
       (cdr (assoc choice candidates))))))

(defun my/zotero-reference-result (payload)
  "Resolve citation PAYLOAD to one Better BibTeX search result."
  (let ((doi (my/zotero-normalize-doi (alist-get 'doi payload)))
        (key (string-trim (or (alist-get 'key payload) "")))
        (title (string-trim (or (alist-get 'title payload) "")))
        doi-results
        key-results)
    (when (not (string-empty-p doi))
      (setq doi-results
            (my/zotero-better-bibtex-search
             (vector (vector "DOI" "is" doi)))))
    (cond
     ((= (length doi-results) 1)
      (car doi-results))
     (t
      (when (not (string-empty-p key))
        (setq key-results
              (my/zotero-better-bibtex-search
               (vector (vector "citationKey" "is" key)))))
      (cond
       ((= (length key-results) 1)
        (car key-results))
       (t
        (let ((candidates (append doi-results key-results)))
          (when (and (null candidates) (not (string-empty-p title)))
            (setq candidates (my/zotero-better-bibtex-search title)))
          (my/zotero-choose-result candidates))))))))

(defun my/zotero-result-select-uri (result)
  "Return a `zotero://select' URI for Better BibTeX RESULT."
  (let ((id (or (alist-get 'id result) "")))
    (cond
     ((string-match "/groups/\\([0-9]+\\)/items/\\([[:alnum:]]+\\)\\'" id)
      (format "zotero://select/groups/%s/items/%s"
              (match-string 1 id) (match-string 2 id)))
     ((string-match "/users/[0-9]+/items/\\([[:alnum:]]+\\)\\'" id)
      (format "zotero://select/library/items/%s" (match-string 1 id)))
     (t nil))))

(defun my/zotero-system-open (target)
  "Open Zotero TARGET using the configured system opener."
  (if (progn (require 'init-open nil t)
             (fboundp 'my/open-system-target))
      (my/open-system-target target)
    (browse-url target)))

(defun my/zotero-open-reference (payload)
  "Find citation PAYLOAD in Zotero and select it in the native application."
  (let* ((explicit (string-trim (or (alist-get 'uri payload) "")))
         (cached (and (string-empty-p explicit)
                      (my/zotero-reference-cache-get payload)))
         (result (and (string-empty-p explicit)
                      (not cached)
                      (my/zotero-reference-result payload)))
         (uri (or (and (string-match-p "\\`zotero://" explicit) explicit)
                  cached
                  (and result (my/zotero-result-select-uri result)))))
    (unless uri
      (user-error "No unique Zotero item found for %s"
                  (or (alist-get 'key payload)
                      (alist-get 'doi payload)
                      (alist-get 'title payload)
                      "reference")))
    (unless (or (not result) cached (not (string-empty-p explicit)))
      (my/zotero-reference-cache-put payload uri))
    (my/zotero-system-open uri)
    (message "Zotero: %s"
             (or (and result (my/zotero-result-label result))
                 (alist-get 'key payload)
                 uri))))

(defun my/zotero-better-bibtex-pick (callback)
  "Open Zotero's native picker and call CALLBACK with BIBTEX and ERROR."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("Zotero-Allowed-Request" . "true")))
        (url-request-data
         (json-serialize
          '(:format "translate"
            :translator "Better BibTeX"
            :select t))))
    (url-retrieve
     my/zotero-better-bibtex-picker-url
     (lambda (status callback)
       (unwind-protect
           (condition-case err
               (if-let* ((request-error (plist-get status :error)))
                   (funcall callback nil (format "%S" request-error))
                 (let* ((reply (my/zotero-better-bibtex--response))
                        (output (alist-get 'output reply)))
                   (funcall callback output nil)))
             (error
              (funcall callback nil (error-message-string err))))
         (kill-buffer (current-buffer))))
     (list callback) t t)))

(defun my/zotero-bibtex-entry-key (bibtex)
  "Return the entry key from BIBTEX text."
  (require 'bibtex)
  (with-temp-buffer
    (insert bibtex)
    (bibtex-mode)
    (goto-char (point-min))
    (when (re-search-forward bibtex-entry-head nil t)
      (goto-char (match-beginning 0))
      (cdr (assoc "=key=" (bibtex-parse-entry t))))))

(defun my/zotero-append-bibtex (target bibtex)
  "Append BIBTEX to TARGET unless its key is already present."
  (let ((key (my/zotero-bibtex-entry-key bibtex))
        (target (expand-file-name target)))
    (unless (and key (not (string-empty-p key)))
      (error "Zotero returned BibTeX without a citation key"))
    (cl-labels
        ((append-in-current-buffer ()
           (unless (derived-mode-p 'bibtex-mode)
             (bibtex-mode))
           (save-excursion
             (save-restriction
               (widen)
               (if (bibtex-search-entry key nil)
                   (progn
                     (message "BibTeX key %s already exists in %s" key target)
                     nil)
                 (goto-char (point-max))
                 (unless (bolp) (insert "\n"))
                 (unless (= (point) (point-min)) (insert "\n"))
                 (insert (string-trim-right bibtex) "\n")
                 t)))))
      (make-directory (file-name-directory target) t)
      (if-let* ((buffer (get-file-buffer target)))
          (with-current-buffer buffer
            (when (append-in-current-buffer)
              (let ((inhibit-message t))
                (save-buffer))
              (message "Imported Zotero key %s into %s" key target)))
        (with-temp-buffer
          (when (file-exists-p target)
            (insert-file-contents target))
          (when (append-in-current-buffer)
            (let ((inhibit-message t))
              (write-region (point-min) (point-max) target nil 'silent))
            (message "Imported Zotero key %s into %s" key target)))))))

(defun my/zotero-default-bib-file (current-file target-file)
  "Return a sensible BibTeX target near CURRENT-FILE or TARGET-FILE."
  (let* ((note-dir (file-name-directory (expand-file-name current-file)))
         (hint (and target-file (not (string-empty-p target-file))
                    (expand-file-name target-file note-dir)))
         (bib-dir (expand-file-name "bib" note-dir))
         (existing (and (file-directory-p bib-dir)
                        (directory-files bib-dir t "\\.bib\\'" t))))
    (or hint
        (and (= (length existing) 1) (car existing))
        (expand-file-name "references.bib" bib-dir))))

(defun my/zotero-import-bibtex (payload)
  "Use Zotero's picker to append one BibTeX entry described by PAYLOAD."
  (let* ((current-file (or (alist-get 'currentFile payload) default-directory))
         (default (my/zotero-default-bib-file
                   current-file (alist-get 'targetFile payload)))
         (target (expand-file-name
                  (read-file-name "Import Zotero BibTeX into: "
                                  (file-name-directory default)
                                  default nil
                                  (file-name-nondirectory default)))))
    (unless (string-match-p "\\.bib\\'" target)
      (user-error "Zotero import target must be a .bib file"))
    (make-directory (file-name-directory target) t)
    (message "Waiting for Zotero citation picker...")
    (my/zotero-better-bibtex-pick
     (lambda (bibtex error-message)
       (cond
        (error-message
         (message "Zotero BibTeX import failed: %s" error-message))
        ((string-empty-p (or bibtex ""))
         (message "Zotero BibTeX import cancelled"))
        (t
         (condition-case err
             (my/zotero-append-bibtex target bibtex)
           (error
            (message "Zotero BibTeX import failed: %s"
                     (error-message-string err))))))))))

(defun my/latex-ratex-color (token fallback)
  "Return Aaron UI color TOKEN, or FALLBACK when the theme helper is absent."
  (if (fboundp 'aaron-ui-color)
      (aaron-ui-color token fallback)
    fallback))

(add-to-list 'load-path
             (expand-file-name "site-lisp/ratex.el/lisp" user-emacs-directory))

(defun my/latex-eglot-available-p ()
  "Return non-nil when a LaTeX language server is available."
  (or (executable-find "texlab")
      (executable-find "digestif")))

(defun my/latex-eglot-workspace-configuration ()
  "Return workspace settings for LaTeX language servers."
  `(:texlab
    (:build (:executable ,(my/executable-or-name "latexmk")
             :args ["-xelatex"
                    "-interaction=nonstopmode"
                    "-synctex=1"
                    "-file-line-error"
                    "-outdir=%OUTDIR%"
                    "%f"]
             :onSave nil
             :forwardSearchAfter nil)
     :chktex (:onOpenAndSave t
              :onEdit nil)
     :diagnosticsDelay 300)))

(defun my/latex-eglot-ensure ()
  "Start Eglot for LaTeX-related buffers when a server is available."
  (when (my/latex-eglot-available-p)
    (setq-local eglot-workspace-configuration
                (my/latex-eglot-workspace-configuration))
    (my/eglot-ensure-unless-lsp-mode)))

(use-package eglot
  :ensure nil
  :defer t
  :hook ((latex-mode . my/latex-eglot-ensure)
         (LaTeX-mode . my/latex-eglot-ensure)
         (tex-mode . my/latex-eglot-ensure)
         (TeX-mode . my/latex-eglot-ensure)
         (plain-tex-mode . my/latex-eglot-ensure)
         (plain-TeX-mode . my/latex-eglot-ensure)
         (docTeX-mode . my/latex-eglot-ensure)
         (bibtex-mode . my/latex-eglot-ensure)))

(with-eval-after-load 'eglot
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(latex-mode LaTeX-mode
       tex-mode TeX-mode
       plain-tex-mode plain-TeX-mode
       docTeX-mode
       bibtex-mode)
     (eglot-alternatives
      '(("texlab")
        ("digestif")))
     :label "texlab/digestif"
     :executables '("texlab" "digestif")
     :note "LaTeX and BibTeX buffers prefer texlab, then fall back to digestif.")))

(defun my/bibtex-entry-field-value (fields names)
  "Return the first non-empty value in FIELDS for any field in NAMES."
  (catch 'value
    (dolist (name names)
      (let ((value (cdr (assoc-string name fields t))))
        (when (and (stringp value) (not (string-empty-p value)))
          (throw 'value value))))
    nil))

(defun my/bibtex-entry-zotero-link ()
  "Return a Zotero link for the BibTeX entry at point, when present."
  (save-excursion
    (ignore-errors
      (bibtex-beginning-of-entry)
      (let ((fields (bibtex-parse-entry t)))
        (my/bibtex-entry-field-value
         fields
         '("zotero" "zoteroselect" "zotero_select" "zotero-link" "zotero_link"))))))

(defun my/bibtex-open-zotero-link ()
  "Find the current BibTeX entry in Zotero and select it."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((fields (bibtex-parse-entry t)))
      (my/zotero-open-reference
       `((uri . ,(or (my/bibtex-entry-zotero-link) ""))
         (key . ,(or (my/bibtex-entry-field-value fields '("=key=")) ""))
         (doi . ,(or (my/bibtex-entry-field-value fields '("doi")) ""))
         (title . ,(or (my/bibtex-entry-field-value fields '("title")) ""))
         (bibFile . ,(or buffer-file-name "")))))))

(defun my/bibtex-setup ()
  "Personal BibTeX editing defaults."
  (setq-local fill-column 100)
  (setq-local bibtex-align-at-equal-sign t)
  (setq-local bibtex-entry-format
              '(opts-or-alts numerical-fields whitespace last-comma delimiters sort-fields))
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1)))

(use-package bibtex
  :ensure nil
  :mode ("\\.bib\\'" . bibtex-mode)
  :hook (bibtex-mode . my/bibtex-setup)
  :bind (:map bibtex-mode-map
              ("C-c C-z" . my/bibtex-open-zotero-link)))

(use-package zotero
  :defer t
  :commands (zotero-search-items))

(use-package zotero-browser
  :ensure nil
  :defer t
  :commands (zotero-browser))

(use-package ratex
  :commands (ratex-mode
             ratex-turn-on
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend
             ratex-toggle-preview-command)
  :init
  (setq ratex-edit-preview 'posframe
        ratex-edit-preview-idle-delay 0.30
        ratex-edit-preview-max-staleness 1.0
        ratex-edit-preview-scan-lines 2
        ratex-font-size 32.0
        ratex-inline-preview nil
        ratex-initial-render-scope 'visible
        ratex-visible-region-margin 1
        ratex-debug nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ratex-render-color (my/latex-ratex-color 'fg-soft "#D8DEE9")
        ratex-posframe-background-color (my/latex-ratex-color 'bg-ratex "#2B3140")
        ratex-posframe-border-color (my/latex-ratex-color 'border-ratex "#5F6F8F"))
  :hook ((latex-mode . ratex-turn-on)
         (LaTeX-mode . ratex-turn-on)
         (tex-mode . ratex-turn-on)
         (TeX-mode . ratex-turn-on)
         (plain-tex-mode . ratex-turn-on)
         (plain-TeX-mode . ratex-turn-on)
         (docTeX-mode . ratex-turn-on)))

(provide 'init-latex)
;;; init-latex.el ends here

;;; lang/latex/config.el --- LaTeX/BibTeX LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Prefer `texlab' when installed, otherwise fall back to `digestif'.
;; This keeps LaTeX/BibTeX buffers on the same lsp-mode workflow as the
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
(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(declare-function +register-language-server "tools/lsp/config")

(defcustom +zotero-better-bibtex-rpc-url
  "http://127.0.0.1:23119/better-bibtex/json-rpc"
  "Local Better BibTeX JSON-RPC endpoint."
  :type 'string
  :group 'zotero)

(defcustom +zotero-better-bibtex-picker-url
  "http://127.0.0.1:23119/better-bibtex/cayw"
  "Local Better BibTeX citation picker endpoint."
  :type 'string
  :group 'zotero)

(defcustom +zotero-reference-cache-ttl 600
  "Seconds to retain resolved Zotero reference links."
  :type 'integer
  :group 'zotero)

(defcustom +zotero-reference-cache-limit 96
  "Maximum number of resolved Zotero reference links to retain."
  :type 'integer
  :group 'zotero)

(defvar +zotero-reference-cache (make-hash-table :test #'equal)
  "Short-lived cache of citation metadata to Zotero select links.")

(defvar +zotero-better-bibtex-request-id 0)

(defun +zotero-better-bibtex--response ()
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

(defun +zotero-better-bibtex-rpc (method params)
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
                 :id (cl-incf +zotero-better-bibtex-request-id))))
         (buffer (url-retrieve-synchronously
                  +zotero-better-bibtex-rpc-url t t 4)))
    (unless (buffer-live-p buffer)
      (error "Better BibTeX is unavailable; start Zotero"))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((reply (+zotero-better-bibtex--response))
                 (rpc-error (alist-get 'error reply)))
            (when rpc-error
              (error "Better BibTeX: %s"
                     (or (alist-get 'message rpc-error) "request failed")))
            (alist-get 'result reply)))
      (kill-buffer buffer))))

(defun +zotero-normalize-doi (doi)
  "Return DOI without a URL or `doi:' prefix."
  (let ((value (string-trim (or doi ""))))
    (setq value (replace-regexp-in-string
                 "\\`https?://\\(?:dx\\.\\)?doi\\.org/" "" value t t))
    (replace-regexp-in-string "\\`doi:[[:space:]]*" "" value t t)))

(defun +zotero-reference-cache-key (payload)
  "Return a stable cache key for reference PAYLOAD."
  (mapconcat
   #'identity
   (list (downcase (string-trim (or (alist-get 'key payload) "")))
         (downcase (+zotero-normalize-doi (alist-get 'doi payload)))
         (downcase (string-trim (or (alist-get 'title payload) ""))))
   "\0"))

(defun +zotero-reference-cache-prune ()
  "Remove expired and excess reference cache entries."
  (let ((now (float-time))
        entries)
    (maphash
     (lambda (key value)
       (if (> (- now (car value)) +zotero-reference-cache-ttl)
           (remhash key +zotero-reference-cache)
         (push (cons key (car value)) entries)))
     +zotero-reference-cache)
    (when (> (hash-table-count +zotero-reference-cache)
             +zotero-reference-cache-limit)
      (setq entries (sort entries (lambda (a b) (< (cdr a) (cdr b)))))
      (dotimes (index (- (length entries) +zotero-reference-cache-limit))
        (remhash (car (nth index entries)) +zotero-reference-cache)))))

(defun +zotero-reference-cache-get (payload)
  "Return a cached Zotero link for PAYLOAD, or nil."
  (+zotero-reference-cache-prune)
  (let ((value (gethash (+zotero-reference-cache-key payload)
                        +zotero-reference-cache)))
    (and value (cdr value))))

(defun +zotero-reference-cache-put (payload uri)
  "Cache URI for reference PAYLOAD."
  (puthash (+zotero-reference-cache-key payload)
           (cons (float-time) uri)
           +zotero-reference-cache)
  (+zotero-reference-cache-prune)
  uri)

(defun +zotero-better-bibtex-search (terms)
  "Search all local Zotero libraries using Better BibTeX TERMS."
  (+zotero-better-bibtex-rpc
   "item.search"
   (vector terms "*")))

(defun +zotero-result-label (result)
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

(defun +zotero-choose-result (results)
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
                                            (+zotero-result-label result)
                                            index)
                                    result)))
            (choice (completing-read "Zotero reference: " candidates nil t)))
       (cdr (assoc choice candidates))))))

(defun +zotero-reference-result (payload)
  "Resolve citation PAYLOAD to one Better BibTeX search result."
  (let ((doi (+zotero-normalize-doi (alist-get 'doi payload)))
        (key (string-trim (or (alist-get 'key payload) "")))
        (title (string-trim (or (alist-get 'title payload) "")))
        doi-results
        key-results)
    (when (not (string-empty-p doi))
      (setq doi-results
            (+zotero-better-bibtex-search
             (vector (vector "DOI" "is" doi)))))
    (cond
     ((= (length doi-results) 1)
      (car doi-results))
     (t
      (when (not (string-empty-p key))
        (setq key-results
              (+zotero-better-bibtex-search
               (vector (vector "citationKey" "is" key)))))
      (cond
       ((= (length key-results) 1)
        (car key-results))
       (t
        (let ((candidates (append doi-results key-results)))
          (when (and (null candidates) (not (string-empty-p title)))
            (setq candidates (+zotero-better-bibtex-search title)))
          (+zotero-choose-result candidates))))))))

(defun +zotero-result-select-uri (result)
  "Return a `zotero://select' URI for Better BibTeX RESULT."
  (let ((id (or (alist-get 'id result) "")))
    (cond
     ((string-match "/groups/\\([0-9]+\\)/items/\\([[:alnum:]]+\\)\\'" id)
      (format "zotero://select/groups/%s/items/%s"
              (match-string 1 id) (match-string 2 id)))
     ((string-match "/users/[0-9]+/items/\\([[:alnum:]]+\\)\\'" id)
      (format "zotero://select/library/items/%s" (match-string 1 id)))
     (t nil))))

(defun +zotero-system-open (target)
  "Open Zotero TARGET using the configured system opener."
  (if (progn (require 'aaron-open nil t)
             (fboundp 'aaron/open-system-target))
      (aaron/open-system-target target)
    (browse-url target)))

(defun +zotero-open-reference (payload)
  "Find citation PAYLOAD in Zotero and select it in the native application."
  (let* ((explicit (string-trim (or (alist-get 'uri payload) "")))
         (cached (and (string-empty-p explicit)
                      (+zotero-reference-cache-get payload)))
         (result (and (string-empty-p explicit)
                      (not cached)
                      (+zotero-reference-result payload)))
         (uri (or (and (string-match-p "\\`zotero://" explicit) explicit)
                  cached
                  (and result (+zotero-result-select-uri result)))))
    (unless uri
      (user-error "No unique Zotero item found for %s"
                  (or (alist-get 'key payload)
                      (alist-get 'doi payload)
                      (alist-get 'title payload)
                      "reference")))
    (unless (or (not result) cached (not (string-empty-p explicit)))
      (+zotero-reference-cache-put payload uri))
    (+zotero-system-open uri)
    (message "Zotero: %s"
             (or (and result (+zotero-result-label result))
                 (alist-get 'key payload)
                 uri))))

(defun +zotero-better-bibtex-pick (callback)
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
     +zotero-better-bibtex-picker-url
     (lambda (status callback)
       (unwind-protect
           (condition-case err
               (if-let* ((request-error (plist-get status :error)))
                   (funcall callback nil (format "%S" request-error))
                 (let* ((reply (+zotero-better-bibtex--response))
                        (output (alist-get 'output reply)))
                   (funcall callback output nil)))
             (error
              (funcall callback nil (error-message-string err))))
         (kill-buffer (current-buffer))))
     (list callback) t t)))

(defun +zotero-bibtex-entry-key (bibtex)
  "Return the entry key from BIBTEX text."
  (require 'bibtex)
  (with-temp-buffer
    (insert bibtex)
    (bibtex-mode)
    (goto-char (point-min))
    (when (re-search-forward bibtex-entry-head nil t)
      (goto-char (match-beginning 0))
      (cdr (assoc "=key=" (bibtex-parse-entry t))))))

(defun +zotero-append-bibtex (target bibtex)
  "Append BIBTEX to TARGET unless its key is already present."
  (let ((key (+zotero-bibtex-entry-key bibtex))
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

(defun +zotero-default-bib-file (current-file target-file)
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

(defun +zotero-import-bibtex (payload)
  "Use Zotero's picker to append one BibTeX entry described by PAYLOAD."
  (let* ((current-file (or (alist-get 'currentFile payload) default-directory))
         (default (+zotero-default-bib-file
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
    (+zotero-better-bibtex-pick
     (lambda (bibtex error-message)
       (cond
        (error-message
         (message "Zotero BibTeX import failed: %s" error-message))
        ((string-empty-p (or bibtex ""))
         (message "Zotero BibTeX import cancelled"))
        (t
         (condition-case err
             (+zotero-append-bibtex target bibtex)
           (error
            (message "Zotero BibTeX import failed: %s"
                     (error-message-string err))))))))))

(defun +latex-ratex-color (token fallback)
  "Return Aaron UI color TOKEN, or FALLBACK when the theme helper is absent."
  (if (fboundp 'aaron-ui-color)
      (aaron-ui-color token fallback)
    fallback))

(add-to-list 'load-path
             (expand-file-name "site-lisp/ratex.el/lisp" user-emacs-directory))

(defun +latex-language-server-selection ()
  "Return (SERVER . EXECUTABLE) for the best server on this target.
Prefer TexLab for its build diagnostics and use Digestif as the lightweight
fallback.  Resolve each candidate once so remote executable discovery is not
repeated by one predicate call."
  (if-let* ((texlab (+language-server-executable-find "texlab")))
      (cons 'texlab texlab)
    (when-let* ((digestif (+language-server-executable-find "digestif")))
      (cons 'digestif digestif))))

(defun +latex-language-server-available-p ()
  "Return non-nil when a LaTeX language server is available."
  (and (+latex-language-server-selection) t))

(defun +latex-language-server-workspace-configuration (&optional selection)
  "Return settings for LaTeX server SELECTION.
Digestif has no TexLab configuration section, so return nil for it."
  (let ((selection (or selection (+latex-language-server-selection))))
    (when (eq (car selection) 'texlab)
      `(:texlab
        (:build
         (:executable ,(or (+language-server-executable-find "latexmk")
                           "latexmk")
          ;; TexLab documents `%f'; output layout remains owned by latexmkrc.
          :args ["-xelatex"
                 "-interaction=nonstopmode"
                 "-synctex=1"
                 "-file-line-error"
                 "%f"]
          :onSave :json-false
          :forwardSearchAfter :json-false)
         :chktex
         (:onOpenAndSave ,(if (+language-server-executable-find "chktex")
                              t
                            :json-false)
          :onEdit :json-false)
         :diagnosticsDelay 300)))))

(defconst +latex-language-server-modes
  '(latex-mode LaTeX-mode
    tex-mode TeX-mode
    plain-tex-mode plain-TeX-mode
    docTeX-mode
    bibtex-mode)
  "Major modes served by the LaTeX language server.")

(defun +latex-language-server-setup-h ()
  "Select one LaTeX client and install its settings before startup."
  (when-let* ((selection (+latex-language-server-selection)))
    ;; Keep lsp-mode's stock TexLab/Digestif clients from racing the target-
    ;; aware client registered below.
    (setq-local lsp-enabled-clients '(my-latex))
    (when-let* ((configuration
                 (+latex-language-server-workspace-configuration selection))
                ((fboundp '+language-server-set-workspace-configuration)))
      (+language-server-set-workspace-configuration configuration))))

(defun +latex-language-server-command ()
  "Return the preferred LaTeX language server command for this target.
texlab is preferred; digestif is the fallback."
  (if-let* ((selection (+latex-language-server-selection)))
      (list (cdr selection))
    (user-error "Neither texlab nor digestif is available on this target")))

(dolist (mode +latex-language-server-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'+latex-language-server-setup-h))

(with-eval-after-load 'lsp-mode
  (when (fboundp '+register-language-server)
    (+register-language-server
     +latex-language-server-modes
     #'+latex-language-server-command
     :server-id 'my-latex
     :priority 1
     :label "texlab/digestif"
     :activation-fn
     (lambda (&rest _) (+latex-language-server-available-p))
     :executables '("texlab" "digestif")
     :note "LaTeX and BibTeX buffers prefer texlab, then fall back to digestif.")))

(defun +bibtex-entry-field-value (fields names)
  "Return the first non-empty value in FIELDS for any field in NAMES."
  (catch 'value
    (dolist (name names)
      (let ((value (cdr (assoc-string name fields t))))
        (when (and (stringp value) (not (string-empty-p value)))
          (throw 'value value))))
    nil))

(defun +bibtex-entry-zotero-link ()
  "Return a Zotero link for the BibTeX entry at point, when present."
  (save-excursion
    (ignore-errors
      (bibtex-beginning-of-entry)
      (let ((fields (bibtex-parse-entry t)))
        (+bibtex-entry-field-value
         fields
         '("zotero" "zoteroselect" "zotero_select" "zotero-link" "zotero_link"))))))

(defun +latex/bibtex-open-zotero-link ()
  "Find the current BibTeX entry in Zotero and select it."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((fields (bibtex-parse-entry t)))
      (+zotero-open-reference
       `((uri . ,(or (+bibtex-entry-zotero-link) ""))
         (key . ,(or (+bibtex-entry-field-value fields '("=key=")) ""))
         (doi . ,(or (+bibtex-entry-field-value fields '("doi")) ""))
         (title . ,(or (+bibtex-entry-field-value fields '("title")) ""))
         (bibFile . ,(or buffer-file-name "")))))))

(defun +bibtex-setup ()
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
  :hook (bibtex-mode . +bibtex-setup)
  :bind (:map bibtex-mode-map
              ("C-c C-z" . +latex/bibtex-open-zotero-link)))

(use-package zotero
  :defer t
  :commands (zotero-search-items))

(use-package zotero-browser
  :ensure nil
  :defer t
  :commands (zotero-browser))

;; ---------------------------------------------------------------------------
;; Shared LaTeX assets: one macro set for the Emacs preview and for Noema
;; ---------------------------------------------------------------------------
;;
;; `site-lisp/noema/resources/' is the single source of truth for math macros
;; and TeX compatibility rules; `etc/katex-macros' links there.  Noema parses
;; those files for KaTeX, and RaTeX is a KaTeX-compatible engine, so feeding
;; both from the same files is what makes the preview in a buffer agree with
;; the rendered note.  See `docs/design/latex-preview.md'.

(defvar +latex-katex-macros-directory
  (expand-file-name "etc/katex-macros" user-emacs-directory)
  "Directory of .tex files defining the shared math macro environment.
Kept in sync with `+noema--katex-macros-dir' -- the same files, read by
both renderers.")

(defvar +latex-tex-compat-rules-file
  (expand-file-name "site-lisp/noema/shared/tex-compat-rules.json"
                    user-emacs-directory)
  "JSON file listing TeX constructs rewritten before rendering.")

(defconst +latex--macro-definition-re
  (concat "\\\\\\(newcommand\\|renewcommand\\|providecommand"
          "\\|DeclareMathOperator\\|def\\)\\(\\*?\\)")
  "Regexp matching the macro definition forms in the shared preamble.")

(defvar +latex--katex-preamble-cache nil
  "Cons of (SIGNATURE . PREAMBLE) for the last compiled macro preamble.")

(defun +latex--strip-tex-comments ()
  "Remove unescaped `%' comments from the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "%" nil t)
    (let ((start (match-beginning 0)))
      (if (and (> start (point-min))
               (eq (char-before start) ?\\))
          nil
        (delete-region start (line-end-position))))))

(defun +latex--read-brace-group ()
  "Read a balanced `{...}' group at point and return its contents, or nil.
Point is left just past the closing brace.  Backslash escapes do not nest."
  (when (eq (char-after) ?{)
    (let ((depth 0)
          (start (1+ (point)))
          result)
      (while (and (null result) (not (eobp)))
        (pcase (char-after)
          (?\\ (forward-char 1))
          (?{ (setq depth (1+ depth)))
          (?} (setq depth (1- depth))
              (when (zerop depth)
                (setq result (buffer-substring-no-properties start (point)))))
          (_ nil))
        (forward-char 1))
      result)))

(defun +latex--read-macro-name ()
  "Read a macro name at point, as `{\\name}' or a bare `\\name'."
  (skip-chars-forward " \t\n")
  (cond
   ((eq (char-after) ?{)
    (let ((group (+latex--read-brace-group)))
      (when (and group (string-match-p "\\`\\\\\\([A-Za-z]+\\|.\\)\\'"
                                       (string-trim group)))
        (string-trim group))))
   ((eq (char-after) ?\\)
    (let ((start (point)))
      (forward-char 1)
      (if (looking-at "[A-Za-z]+")
          (goto-char (match-end 0))
        (forward-char 1))
      (buffer-substring-no-properties start (point))))
   (t nil)))

(defun +latex--read-optional-arity ()
  "Read consecutive `[...]' groups at point, returning the first as a number."
  (let ((arity 0)
        (first t))
    (while (progn (skip-chars-forward " \t\n") (eq (char-after) ?\[))
      (let ((start (1+ (point))))
        (when (search-forward "]" nil t)
          (when first
            (setq arity (or (ignore-errors
                              (string-to-number
                               (string-trim
                                (buffer-substring-no-properties
                                 start (1- (point))))))
                            0))
            (setq first nil)))))
    arity))

(defun +latex--read-def-parameters ()
  "Read a plain-TeX `#1#2...' parameter text at point, returning its arity."
  (let ((arity 0))
    (while (and (eq (char-after) ?#)
                (looking-at "#\\([0-9]\\)"))
      (setq arity (max arity (string-to-number (match-string 1))))
      (goto-char (match-end 0)))
    arity))

(defun +latex-parse-macro-definitions (text)
  "Parse TEXT as a LaTeX preamble and return an alist of (NAME ARITY BODY).

Recognizes the same subset as `site-lisp/noema/shared/katex-macros.mjs':
`\\newcommand' and its re-/provide- variants, `\\DeclareMathOperator', and
`\\def'.  Later definitions win, matching both other implementations."
  (with-temp-buffer
    (insert text)
    (+latex--strip-tex-comments)
    (goto-char (point-min))
    (let (definitions)
      (while (re-search-forward +latex--macro-definition-re nil t)
        (let ((kind (match-string 1))
              (starred (string= (match-string 2) "*")))
          (when-let* ((name (+latex--read-macro-name)))
            (pcase kind
              ("DeclareMathOperator"
               (skip-chars-forward " \t\n")
               (when-let* ((body (+latex--read-brace-group)))
                 ;; KaTeX has no \DeclareMathOperator; normalize the way the
                 ;; other parsers do so all three agree on the expansion.
                 (push (list name 0
                             (format "\\operatorname%s{%s}"
                                     (if starred "*" "") body))
                       definitions)))
              ("def"
               (let ((arity (+latex--read-def-parameters)))
                 (skip-chars-forward " \t\n")
                 (when-let* ((body (+latex--read-brace-group)))
                   (push (list name arity body) definitions))))
              (_
               (let ((arity (+latex--read-optional-arity)))
                 (skip-chars-forward " \t\n")
                 (when-let* ((body (+latex--read-brace-group)))
                   (push (list name arity body) definitions))))))))
      (let (result)
        (dolist (definition (nreverse definitions))
          (setq result (cons definition (assoc-delete-all (car definition) result))))
        (nreverse result)))))

(defun +latex--macro-files (&optional directory)
  "Return the shared macro .tex files in DIRECTORY, then the user's file.
The shared files are sorted for stability; the user's global layer
\(`+latex-user-macros-file') comes last so it wins, as in Noema."
  (let ((directory (or directory
                       ;; Same variable Noema is handed, when it is loaded, so
                       ;; there is one path and not two that happen to agree.
                       (and (boundp '+noema--katex-macros-dir)
                            +noema--katex-macros-dir)
                       +latex-katex-macros-directory)))
    (append
     (when (file-directory-p directory)
       (sort (directory-files directory t "\\.tex\\'" t) #'string<))
     ;; The user's own global layer, managed from Noema's formula palette.
     ;; Read last so it overrides shared names, as it does in Noema.
     (let ((user (+latex-user-macros-file)))
       (when (file-readable-p user) (list user))))))

(defun +latex-user-macros-file ()
  "Return the user's global math macro file, managed by Noema."
  (expand-file-name "katex-macros.tex"
                    (or (getenv "NOEMA_CONFIG_DIR")
                        (expand-file-name "~/.config/noema"))))

(defun +latex--macro-signature (files)
  "Return a value that changes when any file in FILES changes."
  (mapcar (lambda (file)
            (cons file (file-attribute-modification-time
                        (file-attributes file))))
          files))

(defun +latex-tex-compat-rules ()
  "Return the shared TeX compatibility rules as an alist, or nil."
  (when (file-readable-p +latex-tex-compat-rules-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents +latex-tex-compat-rules-file)
        (json-parse-buffer :object-type 'alist :array-type 'list
                           :false-object nil :null-object nil)))))

(defun +latex--compat-macro-definitions ()
  "Return (NAME ARITY BODY) entries for the shared compatibility macros."
  (let ((macros (alist-get 'macros (+latex-tex-compat-rules))))
    (mapcar (lambda (entry)
              (let ((name (format "%s" (car entry)))
                    (body (cdr entry)))
                (list name
                      (if (string-match-p "#1" body) 1 0)
                      body)))
            macros)))

(defun +latex-compile-macro-preamble (definitions)
  "Return a preamble string defining DEFINITIONS for the RaTeX engine.

Emitted as `\\def' rather than `\\newcommand' deliberately: several shared
names (`\\N', `\\vec', ...) already exist in the engine, and `\\newcommand'
errors on redefinition, which would fail the whole preamble and with it every
preview."
  (mapconcat
   (lambda (definition)
     (pcase-let ((`(,name ,arity ,body) definition))
       (format "\\def%s%s{%s}"
               name
               (mapconcat (lambda (index) (format "#%d" index))
                          (number-sequence 1 arity)
                          "")
               body)))
   definitions
   ""))

(defun +latex-katex-preamble ()
  "Return the shared math macro preamble for the RaTeX renderer.

Recompiled only when the underlying .tex files change."
  (let* ((files (+latex--macro-files))
         (signature (+latex--macro-signature files)))
    (if (and +latex--katex-preamble-cache
             (equal (car +latex--katex-preamble-cache) signature))
        (cdr +latex--katex-preamble-cache)
      (let* ((text (mapconcat (lambda (file)
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string)))
                              files
                              "\n"))
             (definitions (append (+latex-parse-macro-definitions text)
                                  (+latex--compat-macro-definitions)))
             (preamble (+latex-compile-macro-preamble definitions)))
        (setq +latex--katex-preamble-cache (cons signature preamble))
        preamble))))

(defun +latex-tex-compat-rewrite (latex)
  "Rewrite LATEX for constructs the renderer does not implement natively."
  (let ((environments (alist-get 'environments (+latex-tex-compat-rules))))
    (dolist (entry environments latex)
      (let ((from (format "%s" (car entry)))
            (to (cdr entry)))
        (setq latex
              (replace-regexp-in-string
               (format "\\\\\\(begin\\|end\\){%s}" (regexp-quote from))
               (lambda (match)
                 (format "\\%s{%s}"
                         (if (string-prefix-p "\\begin" match) "begin" "end")
                         to))
               latex t t))))))

(use-package ratex
  :commands (ratex-mode
             ratex-turn-on
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend
             ratex-stop-backend
             ratex-debug-open-buffer
             ratex-toggle-preview-command)
  :init
  (setq ratex-edit-preview 'posframe
        ratex-edit-preview-idle-delay 0.30
        ratex-edit-preview-max-staleness 1.0
        ratex-edit-preview-scan-lines 2
        ratex-font-size 32.0
        ;; Preview is the popup above point, not inline overlays.
        ratex-inline-preview nil
        ratex-initial-render-scope 'visible
        ratex-visible-region-margin 1
        ratex-debug nil
        ratex-render-cache-limit 24
        ratex-render-cache-ttl 60
        ;; The backend is built from the vendored `ratex-core' in this repo.
        ;; Auto-download would delete that binary on any launch failure and
        ;; replace it with an upstream release of a possibly different
        ;; version, so pin both the root and the policy.
        ratex-backend-root (expand-file-name "site-lisp/ratex.el"
                                             user-emacs-directory)
        ratex-auto-download-backend nil
        ;; One macro set and one compatibility table for this preview and for
        ;; Noema's KaTeX renderer; both read `site-lisp/noema/resources/'.
        ratex-preamble-function #'+latex-katex-preamble
        ratex-compat-rewrite-function #'+latex-tex-compat-rewrite
        ratex-render-color (+latex-ratex-color 'fg-soft "#D8DEE9")
        ratex-posframe-background-color (+latex-ratex-color 'bg-ratex "#2B3140")
        ratex-posframe-border-color (+latex-ratex-color 'border-ratex "#5F6F8F"))
  :hook ((latex-mode . ratex-turn-on)
         (noema-research-mode . ratex-turn-on)
         (LaTeX-mode . ratex-turn-on)
         (tex-mode . ratex-turn-on)
         (TeX-mode . ratex-turn-on)
         (plain-tex-mode . ratex-turn-on)
         (plain-TeX-mode . ratex-turn-on)
         (docTeX-mode . ratex-turn-on)))

;; ---------------------------------------------------------------------------
;; Preview doctor
;; ---------------------------------------------------------------------------

(declare-function ratex-backend-live-p "ratex-core")
(declare-function ratex-diagnose-backend "ratex-core")
(declare-function ratex-fragment-at-point "ratex-math-detect")
(declare-function ratex-preamble "ratex-core")
(defvar ratex--pending)
(defvar ratex--pending-timers)
(defvar ratex--render-cache)
(defvar ratex--inflight-requests)
(defvar ratex--last-error)
(defvar ratex-mode)

(defun +latex-preview--fragment-summary ()
  "Return a one-line description of the math fragment at point."
  (if (not (bound-and-true-p ratex-mode))
      "ratex-mode is off in this buffer"
    (if-let* ((fragment (ratex-fragment-at-point)))
        (format "%s%s  %s"
                (plist-get fragment :open)
                (if-let* ((env (plist-get fragment :environment)))
                    (format " (environment %s)" env)
                  "")
                (truncate-string-to-width
                 (string-trim (or (plist-get fragment :content) "")) 48 nil nil t))
      "none at point")))

(defun +latex-preview--macro-summary ()
  "Return a description of the shared macro preamble state."
  (let* ((files (+latex--macro-files))
         (definitions
          (condition-case err
              (+latex-parse-macro-definitions
               (mapconcat (lambda (file)
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))
                          files "\n"))
            (error (list (list (format "PARSE ERROR: %s"
                                       (error-message-string err))
                               0 ""))))))
    (list :files (length files)
          :macros (length definitions)
          :compat (length (+latex--compat-macro-definitions)))))

(defun +latex-preview-report-string ()
  "Return a diagnostic report for the LaTeX math preview stack."
  (require 'ratex nil t)
  (let* ((macros (+latex-preview--macro-summary))
         (directory (or (and (boundp '+noema--katex-macros-dir)
                             +noema--katex-macros-dir)
                        +latex-katex-macros-directory))
         (wired (and (boundp 'ratex-preamble-function)
                     (eq ratex-preamble-function #'+latex-katex-preamble)))
         (preamble (+latex-katex-preamble)))
    (string-join
     (list
      "LaTeX math preview (RaTeX)"
      "--------------------------"
      (format "backend live      : %s"
              (if (and (fboundp 'ratex-backend-live-p) (ratex-backend-live-p))
                  "yes" "NO"))
      (format "pending requests  : %s"
              (if (hash-table-p (bound-and-true-p ratex--pending))
                  (hash-table-count ratex--pending) "n/a"))
      (format "pending timeouts  : %s"
              (if (hash-table-p (bound-and-true-p ratex--pending-timers))
                  (hash-table-count ratex--pending-timers) "n/a"))
      (format "buffer cache      : %s entries, %s in flight"
              (if (hash-table-p (bound-and-true-p ratex--render-cache))
                  (hash-table-count ratex--render-cache) "n/a")
              (if (hash-table-p (bound-and-true-p ratex--inflight-requests))
                  (hash-table-count ratex--inflight-requests) "n/a"))
      (format "last error        : %s"
              (or (bound-and-true-p ratex--last-error) "none"))
      ""
      "Shared assets (source of truth for Emacs and Noema)"
      "--------------------------------------------------"
      (format "macro directory   : %s%s"
              directory
              (if (file-directory-p directory) "" "   MISSING (dangling link?)"))
      (format "macro files       : %s" (plist-get macros :files))
      (format "macros defined    : %s (+%s compatibility)"
              (plist-get macros :macros) (plist-get macros :compat))
      (format "compat rules      : %s"
              (if (file-readable-p +latex-tex-compat-rules-file)
                  +latex-tex-compat-rules-file
                (format "%s   MISSING" +latex-tex-compat-rules-file)))
      (format "preamble size     : %s chars" (length (or preamble "")))
      (format "preamble wired    : %s"
              (if wired "yes" "NO -- ratex-preamble-function is not set"))
      (format "compat wired      : %s"
              (if (and (boundp 'ratex-compat-rewrite-function)
                       (eq ratex-compat-rewrite-function
                           #'+latex-tex-compat-rewrite))
                  "yes" "NO -- ratex-compat-rewrite-function is not set"))
      ""
      "This buffer"
      "-----------"
      (format "major mode        : %s" major-mode)
      (format "fragment at point : %s" (+latex-preview--fragment-summary))
      ""
      (if (fboundp 'ratex-diagnose-backend)
          (ratex-diagnose-backend)
        "ratex-diagnose-backend unavailable"))
     "\n")))

;;;###autoload
(defun +latex/preview-doctor ()
  "Report the state of the LaTeX math preview stack.

Covers the three things that go wrong in practice: the backend process, the
shared macro preamble, and whether the detector actually sees a fragment
where the cursor is."
  (interactive)
  (let ((report (+latex-preview-report-string)))
    (if (called-interactively-p 'interactive)
        (with-current-buffer (get-buffer-create "*LaTeX Preview Doctor*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert report)
            (goto-char (point-min)))
          (special-mode)
          (display-buffer (current-buffer)))
      report)))

(defalias 'latex-preview-doctor #'+latex/preview-doctor
  "Inspect the LaTeX math preview backend, macros and fragment at point.")

(with-eval-after-load 'general
  ;; Localleader rather than a global leader key: the doctor only means
  ;; anything in a TeX buffer, where it can report the fragment at point.
  (dolist (map '(LaTeX-mode-map TeX-mode-map latex-mode-map tex-mode-map))
    (aaron-local-leader!
      :keymaps map
      "d" '+latex/preview-doctor
      "p" 'ratex-refresh-previews)))

(provide 'lang/latex/config)
;;; lang/latex/config.el ends here

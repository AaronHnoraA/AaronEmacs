;;; noema/search/config.el --- Knowledge search answered by the Noema kernel -*- lexical-binding: t; -*-

;;; Commentary:

;; Every page lookup in Emacs asks the same search the Web surfaces ask: the
;; kernel's, reached through the host's `knowledge:search' channel.  Emacs
;; holds no copy of the page list and does no matching of its own.  The kernel
;; has already decided what matches and in what order -- a misspelled title,
;; a namespace prefix, the page text -- so the minibuffer shows its answer as
;; it is, through a completion style that filters nothing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function +noema--api-call-sync "noema/bridge/config" (channel args &optional timeout))
(defvar +noema--ready)

(defgroup +noema-search nil
  "Knowledge search answered by the Noema kernel."
  :group '+noema)

(defcustom +noema-search-timeout 1.0
  "Seconds a search typed in the minibuffer may wait for the kernel."
  :type 'number
  :group '+noema-search)

(defcustom +noema-search-limit 40
  "How many pages one search shows."
  :type 'integer
  :group '+noema-search)

(defface +noema-search-match '((t :inherit completions-common-part))
  "Face for the part of a title or excerpt that matched."
  :group '+noema-search)

(defface +noema-search-detail '((t :inherit completions-annotations))
  "Face for a result's location and excerpt."
  :group '+noema-search)

(defconst +noema-search-channel "noema:api:knowledge:search"
  "Host channel that forwards a search to the kernel.")

(defun +noema-search-available-p ()
  "Return non-nil when the Noema host can answer a search."
  (and (bound-and-true-p +noema--ready)
       (fboundp '+noema--api-call-sync)))

(defun +noema-search (query &rest options)
  "Search Knowledge for QUERY and return the response hash table, or nil.
OPTIONS is a plist of request fields: :mode, :limit, :sort, :namespace,
:partition, :repositoryId.  Nil when the host is down or did not answer
within `+noema-search-timeout'."
  (when (+noema-search-available-p)
    (let ((body (make-hash-table :test #'equal)))
      (puthash "query" (or query "") body)
      (puthash "limit" (or (plist-get options :limit) +noema-search-limit) body)
      (cl-loop for (key value) on options by #'cddr
               unless (eq key :limit)
               when value
               do (puthash (substring (symbol-name key) 1) value body))
      (condition-case nil
          (+noema--api-call-sync +noema-search-channel (vector body)
                                   +noema-search-timeout)
        (error nil)))))

(defun +noema-search--get (item key)
  "Return string field KEY of result ITEM, or the empty string."
  (let ((value (and (hash-table-p item) (gethash key item))))
    (if (stringp value) value "")))

(defun +noema-search--items (response)
  "Return the result items of RESPONSE as a list."
  (and (hash-table-p response)
       (append (gethash "items" response) nil)))

;;; What the minibuffer shows

(defun +noema-search--label (item)
  "Return the candidate text for ITEM: its title under its namespace."
  (let* ((title (+noema-search--get item "title"))
         (namespace (+noema-search--get item "namespace"))
         (label (if (string-empty-p namespace) title (concat namespace ":" title)))
         (offset (- (length label) (length title))))
    (setq label (copy-sequence label))
    ;; The gateway hands a JSON array over as a list or a vector.
    (seq-doseq (range (and (hash-table-p item) (gethash "titleRanges" item)))
      (when (and (sequencep range) (= (length range) 2))
        (let ((from (+ offset (elt range 0)))
              (to (+ offset (elt range 1))))
          (when (<= 0 from to (length label))
            (add-face-text-property from to '+noema-search-match nil label)))))
    label))

(defun +noema-search--excerpt (text)
  "Return excerpt TEXT with its [[marked]] hits shown by face."
  (with-temp-buffer
    (insert (string-trim (or text "")))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\]" nil t)
      (replace-match (propertize (match-string 1) 'face '+noema-search-match) t t))
    (buffer-string)))

(defun +noema-search--detail (item)
  "Return the location and excerpt shown beside ITEM."
  (let ((via (+noema-search--get item "via"))
        (path (+noema-search--get item "repositoryPath"))
        (excerpt (+noema-search--excerpt (+noema-search--get item "excerpt"))))
    (concat
     (propertize (concat "  " (if (string-empty-p via) path (concat "↳ " via)))
                 'face '+noema-search-detail)
     (unless (string-empty-p excerpt)
       (concat (propertize "  · " 'face '+noema-search-detail)
               (truncate-string-to-width excerpt 90 nil nil "…"))))))

(defun +noema-search--group-name (item response)
  "Return the heading ITEM is listed under, from how RESPONSE found it."
  (pcase (+noema-search--get item "matchKind")
    ((or "exact" "title" "alias" "path") "Title")
    ("fuzzy" "Similar title")
    ("content" "Text")
    ("corrected"
     (format "Text, reading it as “%s”"
             (+noema-search--get (gethash "corrected" response) "to")))
    ("partial" "Part of the query")
    (_ "Pages")))

(defun +noema-search--candidates (state input)
  "Return the candidates the kernel answers INPUT with, remembered in STATE.
STATE maps an input to its candidate list and a candidate to its item."
  (let ((known (gethash (cons 'input input) state 'none)))
    (if (not (eq known 'none))
        known
      (let* ((response (if (string-empty-p (string-trim input))
                           (+noema-search "" :sort "recent")
                         (+noema-search input :mode "suggest"
                                          :limit +noema-search-limit)))
             (seen (make-hash-table :test #'equal))
             candidates)
        (dolist (item (+noema-search--items response))
          (let ((label (+noema-search--label item)))
            ;; Two pages may share a title in one namespace.
            (when (gethash label seen)
              (setq label (concat label "  "
                                  (+noema-search--get item "repositoryPath"))))
            (puthash label t seen)
            (puthash label (list item (+noema-search--group-name item response)) state)
            (push label candidates)))
        (when-let* ((create (and (hash-table-p response) (gethash "create" response)))
                    (title (+noema-search--get create "title"))
                    ((not (string-empty-p title))))
          (let* ((namespace (+noema-search--get create "namespace"))
                 (label (format "+ Create “%s”%s" title
                                (if (string-empty-p namespace) ""
                                  (concat " in " namespace)))))
            (puthash label (list (list :create title namespace) "New page") state)
            (push label candidates)))
        (setq candidates (nreverse candidates))
        ;; Remember an answer, not a failure: the host may be back next key.
        (when response
          (puthash (cons 'input input) candidates state))
        candidates))))

(defun +noema-search--table (state)
  "Return a completion table whose candidates are the kernel's answer."
  (lambda (string pred action)
    (pcase action
      ('metadata
       `(metadata
         (category . noema-page)
         (display-sort-function . identity)
         (cycle-sort-function . identity)
         (group-function
          . ,(lambda (candidate transform)
               (if transform candidate (cadr (gethash candidate state)))))
         (annotation-function
          . ,(lambda (candidate)
               (let ((item (car (gethash candidate state))))
                 (and (hash-table-p item) (+noema-search--detail item)))))))
      (`(boundaries . ,_) nil)
      ('lambda (and (gethash string state) t))
      ('nil (and (+noema-search--candidates state string) string))
      (_ (let ((candidates (+noema-search--candidates state string)))
           (if pred (seq-filter pred candidates) candidates))))))

;; The kernel matched "maintian" to "maintain"; a completion style that
;; compares the input with the candidate again would throw that answer away.
(defun +noema-search--style-try (string table pred point)
  "Keep STRING as typed while TABLE has an answer for it."
  (and (all-completions string table pred)
       (cons string point)))

(defun +noema-search--style-all (string table pred _point)
  "Return TABLE's answer for STRING unfiltered."
  (all-completions string table pred))

(add-to-list 'completion-styles-alist
             '(+noema-search +noema-search--style-try +noema-search--style-all
                               "Show the Noema kernel's search results as they are."))

(defun +noema-search-read-page (prompt &optional initial)
  "Read a Wiki page with PROMPT, searching as the person types.
Return the chosen result as a hash table, (:create TITLE NAMESPACE) when the
person asked for a page that does not exist, or nil.  INITIAL is the first
input.  Signals `user-error' when the Noema host is not running."
  (unless (+noema-search-available-p)
    (user-error "Noema is not running; start it to search pages"))
  (let* ((state (make-hash-table :test #'equal))
         (completion-styles '(+noema-search))
         (completion-category-overrides nil)
         (completion-category-defaults nil)
         (choice (completing-read prompt (+noema-search--table state)
                                  nil nil initial '+noema-search-history)))
    (+noema-search--choice state choice)))

(defun +noema-search--choice (state choice)
  "Return what confirming CHOICE means, given the answers held in STATE."
  (or (car (gethash choice state))
      ;; Typed and confirmed without choosing.  No page has that title when
      ;; the answer offers to create it, and that is then what was meant;
      ;; otherwise go to the best match, as a wiki's search box does.
      (let ((candidates (+noema-search--candidates state choice)))
        (car (gethash (or (seq-find (lambda (candidate)
                                      (eq (car-safe (car (gethash candidate state)))
                                          :create))
                                    candidates)
                          (car candidates))
                      state)))))

(defvar +noema-search-history nil
  "Minibuffer history of page searches.")

;;; References: pages, headings, tags and citations

(defun +noema-search--reference-candidates (state input file content)
  "Return what the kernel offers to refer to for INPUT, remembered in STATE.
FILE and CONTENT are the note the reference is made from."
  (let ((known (gethash (cons 'input input) state 'none)))
    (if (not (eq known 'none))
        known
      (let ((response (+noema-search input :mode "references"
                                       :file file :content content :limit 30))
            (seen (make-hash-table :test #'equal))
            candidates)
        (dolist (item (+noema-search--items response))
          (let* ((label (copy-sequence (+noema-search--get item "label")))
                 (level (gethash "level" item)))
            (seq-doseq (range (gethash "titleRanges" item))
              (when (and (sequencep range) (= (length range) 2)
                         (<= 0 (elt range 0) (elt range 1) (length label)))
                (add-face-text-property (elt range 0) (elt range 1)
                                        '+noema-search-match nil label)))
            (when (and (integerp level) (> level 1))
              (setq label (concat (make-string (* 2 (1- level)) ?\s) label)))
            ;; Two headings may share a name; the kernel numbers their links.
            (when (gethash label seen)
              (setq label (concat label "  " (+noema-search--get item "fragment"))))
            (puthash label t seen)
            (puthash label (list item (+noema-search--get item "group")) state)
            (push label candidates)))
        (setq candidates (nreverse candidates))
        (when response
          (puthash (cons 'input input) candidates state))
        candidates))))

(defun +noema-search-read-reference (prompt &optional file content)
  "Read something to refer to with PROMPT and return the kernel's item.
The kernel lists headings, tags and blocks of the note FILE with text
CONTENT, then pages.  The prefixes are roam's: `#' asks for tags, `@' for
headings, `Page#' and `Page@' for the inside of a page; `@@' asks for
citation keys and `/' for files.  The item's \"insert\" field is the text
to insert."
  (unless (+noema-search-available-p)
    (user-error "Noema is not running; start it to insert a reference"))
  (let* ((state (make-hash-table :test #'equal))
         (candidates (lambda (input)
                       (+noema-search--reference-candidates state input file content)))
         (table
          (lambda (string pred action)
            (pcase action
              ('metadata
               `(metadata
                 (category . noema-reference)
                 (display-sort-function . identity)
                 (cycle-sort-function . identity)
                 (group-function
                  . ,(lambda (candidate transform)
                       (if transform candidate (cadr (gethash candidate state)))))
                 (annotation-function
                  . ,(lambda (candidate)
                       (when-let* ((item (car (gethash candidate state))))
                         (propertize (concat "  " (+noema-search--get item "detail"))
                                     'face '+noema-search-detail))))))
              (`(boundaries . ,_) nil)
              ('lambda (and (gethash string state) t))
              ('nil (and (funcall candidates string) string))
              (_ (let ((found (funcall candidates string)))
                   (if pred (seq-filter pred found) found))))))
         (completion-styles '(+noema-search))
         (completion-category-overrides nil)
         (completion-category-defaults nil)
         (choice (completing-read prompt table nil nil nil '+noema-search-history)))
    (car (or (gethash choice state)
             (gethash (car (funcall candidates choice)) state)))))

(declare-function +noema/command "noema/bridge/config" (command &rest args))
(defvar +noema-keys-mode)
(defvar +noema-keys-mode-map)

(defun +noema/insert-reference ()
  "Insert a link to a page, heading, tag or citation.
In a Noema pane the page opens its reference picker at the caret.  In any
other buffer the same lookup runs in the minibuffer and the link is inserted
at point.  Either way the kernel finds the candidates and writes the link."
  (interactive)
  (if (bound-and-true-p +noema-keys-mode)
      (+noema/command "reference")
    (barf-if-buffer-read-only)
    (let ((item (+noema-search-read-reference
                 "Refer to: " buffer-file-name
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (unless (hash-table-p item)
        (user-error "Nothing to refer to"))
      (insert (+noema-search--with-display-text
               (+noema-search--get item "insert")
               (gethash "labelRange" item))))))

(defun +noema-search--display-text (link range)
  "Return (FROM . TO), the character span of LINK's display text, or nil.
RANGE is the kernel's (FROM TO) in UTF-16 units, as an editor counts; Emacs
counts characters, and one outside the basic plane is two units."
  (when (and (sequencep range) (= (length range) 2))
    (let ((position (lambda (units)
                      (let ((index 0) (seen 0))
                        (while (and (< seen units) (< index (length link)))
                          (setq seen (+ seen (if (> (aref link index) #xFFFF) 2 1))
                                index (1+ index)))
                        index))))
      (cons (funcall position (elt range 0))
            (funcall position (elt range 1))))))

(defun +noema-search--replace-display-text (link range text)
  "Return LINK with the display text at RANGE replaced by TEXT.
Blank TEXT, or a LINK without display text of its own, leaves LINK as it is."
  (let ((span (+noema-search--display-text link range)))
    (if (or (null span) (string-empty-p (string-trim (or text ""))))
        link
      (concat (substring link 0 (car span)) text (substring link (cdr span))))))

(defun +noema-search--with-display-text (link range)
  "Return LINK with its display text as the person words it.
The kernel's label is offered as the default."
  (let ((span (+noema-search--display-text link range)))
    (if (null span)
        link
      (let ((default (substring link (car span) (cdr span))))
        (+noema-search--replace-display-text
         link range
         (read-string (format "Display text (default %s): " default)
                      nil nil default))))))

(defun +noema/tag-reference ()
  "Tag what is at the caret of the Noema page and copy the reference to it.
The page tags an equation or the current place inline, as its tool
\"Tag / copy reference\" does."
  (interactive)
  (+noema/command "tag-reference"))

;; Command-R refers to something, Command-Shift-R makes the place at the
;; caret referable.  The page binds both keys for itself; these cover the
;; pane while Emacs holds the keyboard.
(with-eval-after-load 'noema/bridge/config
  (define-key +noema-keys-mode-map (kbd "M-r") #'+noema/insert-reference)
  (define-key +noema-keys-mode-map (kbd "M-R") #'+noema/tag-reference))

(provide 'noema/search/config)
;;; noema/search/config.el ends here

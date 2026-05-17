;;; Aaronnote.el --- Emacs bridge for Aaronnote -*- lexical-binding: t; -*-

;;; Commentary:
;; Start the local Aaronnote Vite app, open it in xwidget-webkit, and
;; exchange file contents over a localhost WebSocket bridge.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(declare-function websocket-server "websocket" (port &rest plist))
(declare-function websocket-server-close "websocket" (server))
(declare-function websocket-frame-payload "websocket" (frame))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-goto-url "xwidget" (url))

(defgroup Aaronnote nil
  "Emacs-hosted Typora-style note editor."
  :group 'applications)

(defcustom Aaronnote-port 5179
  "Local Vite dev server port for Aaronnote."
  :type 'integer
  :group 'Aaronnote)

(defcustom Aaronnote-npm-command "npm"
  "Command used to run Aaronnote's Node scripts."
  :type 'string
  :group 'Aaronnote)

(defcustom Aaronnote-default-file
  (expand-file-name "var/Aaronnote/scratch.md" user-emacs-directory)
  "Fallback file opened by `Aaronnote-up' when the current buffer has no file."
  :type 'file
  :group 'Aaronnote)

(defcustom Aaronnote-markdown-extensions '("md" "markdown")
  "File extensions opened in rendered Markdown mode.
Other files, including Typst files, open in raw source mode so Aaronnote
does not reinterpret their syntax on first load."
  :type '(repeat string)
  :group 'Aaronnote)

(defcustom Aaronnote-note-directories nil
  "Note directories exported to the Aaronnote frontend.
When nil, use `~/HC/Org/roam' when it exists."
  :type '(repeat directory)
  :group 'Aaronnote)

(defcustom Aaronnote-note-scan-extensions '("typ" "md" "markdown")
  "File extensions used by the fallback note scanner."
  :type '(repeat string)
  :group 'Aaronnote)

(defcustom Aaronnote-note-excluded-directories
  '("_typst" "public" "var" ".git" ".direnv" ".venv" "node_modules")
  "Directory names ignored by Aaronnote's fallback note scanner."
  :type '(repeat string)
  :group 'Aaronnote)

(defcustom Aaronnote-snippet-directories nil
  "Snippet directories exported to the Aaronnote frontend.
When nil, use `yas-snippet-dirs' if it is bound, otherwise
`~/.emacs.d/snippets'."
  :type '(repeat directory)
  :group 'Aaronnote)

(defvar Aaronnote--process nil
  "Running Aaronnote Vite process, or nil.")

(defvar Aaronnote--ws-server nil
  "Running Aaronnote WebSocket server, or nil.")

(defvar Aaronnote--ws-port nil
  "Port of the Aaronnote WebSocket bridge.")

(defvar Aaronnote--token nil
  "Current WebSocket bridge token.")

(defvar Aaronnote--active-file nil
  "File currently served to the Aaronnote frontend.")

(defvar Aaronnote--xwidget-buffer nil
  "xwidget buffer currently showing Aaronnote.")

(defvar-local Aaronnote--xwidget-owned nil
  "Non-nil in xwidget buffers owned by Aaronnote.")

(defun Aaronnote--directory ()
  "Return the Aaronnote project directory."
  (file-name-directory
   (or load-file-name
       buffer-file-name
       (locate-library "Aaronnote")
       (expand-file-name "site-lisp/Aaronnote/Aaronnote.el"
                         user-emacs-directory))))

(defun Aaronnote--live-process-p ()
  "Return non-nil when the Vite process is alive."
  (and Aaronnote--process
       (process-live-p Aaronnote--process)))

(defun Aaronnote--ensure-token ()
  "Return the current bridge token, creating one if needed."
  (or Aaronnote--token
      (setq Aaronnote--token
            (secure-hash
             'sha256
             (format "%s:%s:%s"
                     (emacs-pid)
                     (float-time)
                     (random most-positive-fixnum))))))

(defun Aaronnote--json-encode (value)
  "Encode VALUE as JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode value)))

(defun Aaronnote--send (ws payload)
  "Send JSON PAYLOAD to WS."
  (websocket-send-text ws (Aaronnote--json-encode payload)))

(defun Aaronnote--valid-message-p (message)
  "Return non-nil when MESSAGE carries the current token."
  (equal (alist-get 'token message) (Aaronnote--ensure-token)))

(defun Aaronnote--markdown-file-p (file)
  "Return non-nil when FILE should open in rendered Markdown mode."
  (member (downcase (or (file-name-extension file) ""))
          Aaronnote-markdown-extensions))

(defun Aaronnote--file-mode (file)
  "Return frontend mode for FILE."
  (if (Aaronnote--markdown-file-p file) "markdown" "source"))

(defun Aaronnote--ensure-default-file ()
  "Ensure `Aaronnote-default-file' exists and return it."
  (unless (file-exists-p Aaronnote-default-file)
    (make-directory (file-name-directory Aaronnote-default-file) t)
    (with-temp-file Aaronnote-default-file
      (insert "# Aaronnote\n\nType here.\n\nInline math: $x^2 + y^2$.\n\n$$\nE = mc^2\n$$\n")))
  Aaronnote-default-file)

(defun Aaronnote--buffer-content (file)
  "Return current content for FILE, preferring a live buffer."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun Aaronnote--write-content (file content)
  "Write CONTENT to FILE and sync the note index when available."
  (make-directory (file-name-directory file) t)
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (let ((point-pos (point))
              (inhibit-read-only t))
          (erase-buffer)
          (insert content)
          (goto-char (min point-pos (point-max)))
          (save-buffer)))
  (with-temp-file file
      (insert content)))
  file)

(defun Aaronnote--note-dirs ()
  "Return readable note directories for Aaronnote export."
  (let ((dirs (or Aaronnote-note-directories
                  (list (expand-file-name "~/HC/Org/roam")))))
    (when (stringp dirs)
      (setq dirs (list dirs)))
    (seq-filter #'file-directory-p
                (mapcar #'expand-file-name dirs))))

(defun Aaronnote--meta-block ()
  "Return md `#+begin meta' fields in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (fields)
      (when (re-search-forward "^[ \t]*#\\+begin[ \t]+meta[ \t]*$" nil t)
        (let ((end (save-excursion
                     (re-search-forward "^[ \t]*#\\+end[ \t]+meta[ \t]*$" nil t))))
          (while (and end (< (point) end))
            (when (looking-at "^[ \t]*\\([A-Za-z0-9_-]+\\)[ \t]*:[ \t]*\\(.*\\)$")
              (push (cons (downcase (match-string 1))
                          (string-trim (match-string 2)))
                    fields))
            (forward-line 1))))
      fields)))

(defun Aaronnote--meta-value (metadata key)
  "Return KEY value from md METADATA."
  (cdr (assoc key metadata)))

(defun Aaronnote--split-meta-list (value)
  "Return VALUE parsed as a comma/space separated metadata list."
  (seq-filter
   (lambda (item) (not (string-empty-p item)))
   (mapcar #'string-trim
           (split-string (or value "") "[, ]+" t))))

(defun Aaronnote--typst-metadata-body ()
  "Return Typst #metadata body from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#metadata[ \t\n]*((" nil t)
      (let ((start (point)))
        (when (re-search-forward "))[ \t\n]*<note>" nil t)
          (buffer-substring-no-properties start (match-beginning 0)))))))

(defun Aaronnote--typst-field (body key)
  "Return Typst metadata field KEY from BODY."
  (when body
    (let ((case-fold-search nil))
      (when (string-match
             (format "\\_<%s[ \t\n]*:[ \t\n]*\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\|(\\(?:.\\|\n\\)*?)\\|[^,\n]+\\)" (regexp-quote key))
             body)
        (string-trim (match-string 1 body))))))

(defun Aaronnote--typst-string-value (raw)
  "Return unquoted Typst string RAW."
  (when raw
    (if (string-match "\\`\"\\(\\(?:[^\"\\]\\|\\\\.\\)*\\)\"\\'" raw)
        (replace-regexp-in-string "\\\\\"" "\"" (match-string 1 raw))
      raw)))

(defun Aaronnote--typst-list-value (raw)
  "Return Typst string tuple RAW as a list."
  (let (items)
    (when raw
      (with-temp-buffer
        (insert raw)
        (goto-char (point-min))
        (while (re-search-forward "\"\\(\\(?:[^\"\\]\\|\\\\.\\)*\\)\"" nil t)
          (push (replace-regexp-in-string "\\\\\"" "\"" (match-string 1)) items))))
    (nreverse items)))

(defun Aaronnote--ignored-note-path-p (file root)
  "Return non-nil when FILE under ROOT is in an excluded directory."
  (let* ((relative (file-relative-name file (file-name-as-directory root)))
         (parts (split-string relative "/" t)))
    (seq-some (lambda (part)
                (member part Aaronnote-note-excluded-directories))
              parts)))

(defun Aaronnote--scan-note-title ()
  "Return a title for the current buffer note."
  (let* ((md-meta (Aaronnote--meta-block))
         (typst-body (Aaronnote--typst-metadata-body))
         (meta-title (or (Aaronnote--meta-value md-meta "title")
                         (Aaronnote--typst-string-value
                          (Aaronnote--typst-field typst-body "title")))))
    (or (and meta-title (not (string-empty-p meta-title)) meta-title)
        (save-excursion
          (goto-char (point-min))
          (cond
           ((re-search-forward "^=+[ \t]+\\(.+\\)$" nil t)
            (string-trim (match-string 1)))
           ((re-search-forward "^#+[ \t]+\\(.+\\)$" nil t)
            (string-trim (match-string 1))))))))

(defun Aaronnote--scan-note-id ()
  "Return a note id for the current buffer, when present."
  (let ((md-meta (Aaronnote--meta-block))
        (typst-body (Aaronnote--typst-metadata-body)))
    (or (Aaronnote--meta-value md-meta "id")
        (Aaronnote--typst-string-value
         (Aaronnote--typst-field typst-body "id")))))

(defun Aaronnote--scan-note-tags ()
  "Return current buffer note tags."
  (let ((md-meta (Aaronnote--meta-block))
        (typst-body (Aaronnote--typst-metadata-body)))
    (or (Aaronnote--split-meta-list (Aaronnote--meta-value md-meta "tags"))
        (Aaronnote--typst-list-value
         (Aaronnote--typst-field typst-body "tags"))
        nil)))

(defun Aaronnote--scan-note-refs ()
  "Return current buffer note outgoing refs."
  (let ((refs (Aaronnote--split-meta-list
               (Aaronnote--meta-value (Aaronnote--meta-block) "refs"))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#note(\"\\([^\"]+\\)\")" nil t)
        (push (match-string 1) refs))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]\n]+\\)\\]\\]" nil t)
        (push (string-trim (match-string 1)) refs)))
    (delete-dups (seq-filter (lambda (ref) (not (string-empty-p ref))) refs))))

(defun Aaronnote--scan-note-summary ()
  "Return a compact summary for the current buffer note."
  (save-excursion
    (goto-char (point-min))
    (let (parts in-meta)
      (while (and (< (length (string-join (nreverse parts) " ")) 220)
                  (not (eobp)))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (cond
           ((string-match-p "\\`#\\+begin[ \t]+meta\\'" line)
            (setq in-meta t))
           ((string-match-p "\\`#\\+end[ \t]+meta\\'" line)
            (setq in-meta nil))
           ((or in-meta
                (string-empty-p line)
                (string-match-p "\\`#\\(import\\|show\\|set\\|metadata\\)" line)))
           (t
            (push (string-trim
                   (replace-regexp-in-string
                    "[#*_`$()[\\]{}]" " "
                    (replace-regexp-in-string "#note(\"[^\"]+\")\\[\\([^]]+\\)\\]" "\\1" line)))
                  parts))))
        (forward-line 1))
      (truncate-string-to-width (string-join (nreverse parts) " ") 220 nil nil "..."))))

(defun Aaronnote--scan-note-date ()
  "Return current buffer note date."
  (let ((md-meta (Aaronnote--meta-block))
        (typst-body (Aaronnote--typst-metadata-body)))
    (or (Aaronnote--meta-value md-meta "date")
        (Aaronnote--typst-string-value
         (Aaronnote--typst-field typst-body "date"))
        "")))

(defun Aaronnote--scan-note-source ()
  "Return current buffer source metadata."
  (Aaronnote--meta-value (Aaronnote--meta-block) "source"))

(defun Aaronnote--scan-note-file (file root)
  "Return a frontend note summary for FILE below ROOT."
  (with-temp-buffer
    (insert-file-contents file nil 0 12000)
    (let* ((rel (file-relative-name file (file-name-as-directory root)))
           (group-key (let ((dir (file-name-directory rel)))
                        (if (or (null dir) (string= dir "")) "Root" (directory-file-name dir))))
           (group-label (if (string= group-key "Root")
                            "Root"
                          (file-name-nondirectory group-key)))
           (title (or (Aaronnote--scan-note-title)
                      (file-name-base file)))
           (id (or (Aaronnote--scan-note-id) rel)))
      `(("key" . ,id)
        ("id" . ,id)
        ("title" . ,title)
        ("file" . ,(file-truename file))
        ("path" . ,rel)
        ("link" . ,rel)
        ("date" . ,(Aaronnote--scan-note-date))
        ("groupKey" . ,group-key)
        ("groupLabel" . ,group-label)
        ("section" . ,(car (split-string group-key "/" t)))
        ("source" . ,(or (Aaronnote--scan-note-source) ""))
        ("summary" . ,(Aaronnote--scan-note-summary))
        ("tags" . ,(vconcat (Aaronnote--scan-note-tags)))
        ("refs" . ,(vconcat (Aaronnote--scan-note-refs)))
        ("backlinks" . [])))))

(defun Aaronnote--scanned-note-summaries ()
  "Return note summaries by scanning note directories."
  (let* ((ext-regexp (regexp-opt Aaronnote-note-scan-extensions t))
         (file-regexp (concat "\\." ext-regexp "\\'"))
         notes)
    (dolist (root (Aaronnote--note-dirs))
      (dolist (file (directory-files-recursively root file-regexp nil))
        (when (and (file-regular-p file)
                   (not (Aaronnote--ignored-note-path-p file root)))
          (push (Aaronnote--scan-note-file file root) notes))))
    (let ((by-id (make-hash-table :test #'equal)))
      (dolist (note notes)
        (let* ((id (cdr (assoc "id" note)))
               (current (gethash id by-id))
               (note-ext (downcase (or (file-name-extension (cdr (assoc "file" note))) "")))
               (current-ext (downcase (or (and current (file-name-extension (cdr (assoc "file" current)))) ""))))
          (when (or (null current)
                    (and (string= note-ext "md")
                         (not (string= current-ext "md"))))
            (puthash id note by-id))))
      (setq notes (hash-table-values by-id)))
    (let ((by-id (make-hash-table :test #'equal))
          (by-path (make-hash-table :test #'equal)))
      (dolist (note notes)
        (puthash (cdr (assoc "id" note)) note by-id)
        (puthash (cdr (assoc "path" note)) note by-path)
        (puthash (file-name-nondirectory (cdr (assoc "path" note))) note by-path))
      (dolist (note notes)
        (let (resolved)
          (dolist (ref (append (cdr (assoc "refs" note)) nil))
            (when-let* ((target (or (gethash ref by-id)
                                    (gethash ref by-path)
                                    (gethash (string-remove-prefix "./" ref) by-path))))
              (let ((target-id (cdr (assoc "id" target)))
                    (source-id (cdr (assoc "id" note))))
                (unless (equal target-id source-id)
                  (push target-id resolved)
                  (setcdr (assoc "backlinks" target)
                          (vconcat
                           (append (append (cdr (assoc "backlinks" target)) nil)
                                   (list source-id)))))))
          (setcdr (assoc "refs" note)
                  (vconcat (delete-dups (nreverse resolved))))))
      (dolist (note notes)
        (setcdr (assoc "backlinks" note)
                (vconcat (delete-dups (append (cdr (assoc "backlinks" note)) nil)))))))
    (vconcat
     (sort notes
           (lambda (a b)
             (string<
              (downcase (or (cdr (assoc "title" a)) ""))
              (downcase (or (cdr (assoc "title" b)) ""))))))))

(defun Aaronnote--note-summaries ()
  "Return note summaries for the frontend."
  (or (Aaronnote--scanned-note-summaries) []))

(defun Aaronnote--snippet-dirs ()
  "Return readable snippet directories for Aaronnote export."
  (let* ((raw (or Aaronnote-snippet-directories
                  (and (boundp 'yas-snippet-dirs)
                       (symbol-value 'yas-snippet-dirs))
                  (list (expand-file-name "snippets" user-emacs-directory))))
         (dirs nil))
    (cl-labels
        ((collect (entry)
           (cond
            ((stringp entry)
             (push (expand-file-name entry) dirs))
            ;; yasnippet-snippets intentionally appends the symbol
            ;; `yasnippet-snippets-dir' to `yas-snippet-dirs'.  Resolve
            ;; bound symbols instead of treating them as path strings.
            ((and (symbolp entry) (boundp entry))
             (collect (symbol-value entry)))
            ((listp entry)
             (mapc #'collect entry)))))
      (collect raw))
    (seq-filter #'file-directory-p
                (delete-dups (nreverse dirs)))))

(defun Aaronnote--snippet-mode-for-file (file root)
  "Return the snippet mode name for FILE below ROOT."
  (let* ((relative (file-relative-name file root))
         (parts (split-string relative "/" t)))
    (car parts)))

(defun Aaronnote--snippet-header-value (headers key)
  "Return KEY value from snippet HEADERS."
  (cdr (assoc key headers)))

(defun Aaronnote--parse-snippet-file (file root)
  "Parse yasnippet FILE below ROOT into an alist, or nil."
  (unless (or (string-prefix-p "." (file-name-nondirectory file))
              (string-match-p "\\.el\\'" file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((mode (Aaronnote--snippet-mode-for-file file root))
            (headers nil)
            (body-start nil))
        (goto-char (point-min))
        (while (and (not body-start) (not (eobp)))
          (cond
           ((looking-at "^# --[ \t]*$")
            (setq body-start (line-beginning-position 2)))
           ((looking-at "^# *\\([^:\n]+\\):[ \t]*\\(.*\\)$")
            (push (cons (downcase (match-string 1))
                        (string-trim (match-string 2)))
                  headers)))
          (forward-line 1))
        (let* ((body (string-trim-right
                      (buffer-substring-no-properties
                       (or body-start (point-min))
                       (point-max))))
               (key (or (Aaronnote--snippet-header-value headers "key")
                        (file-name-nondirectory file)))
               (name (or (Aaronnote--snippet-header-value headers "name")
                         key))
               (group (Aaronnote--snippet-header-value headers "group")))
          (when (and mode key (not (string-empty-p body)))
            `(("key" . ,key)
              ("name" . ,name)
              ("mode" . ,mode)
              ("group" . ,(or group ""))
              ("body" . ,body)
              ("source" . ,file))))))))

(defun Aaronnote--snippet-summaries ()
  "Return yasnippet snippets as a JSON vector for the frontend."
  (let (snippets)
    (dolist (root (Aaronnote--snippet-dirs))
      (dolist (file (directory-files-recursively root ".*" nil))
        (when (file-regular-p file)
          (when-let* ((snippet (ignore-errors
                                 (Aaronnote--parse-snippet-file file root))))
            (push snippet snippets)))))
    (vconcat
     (sort snippets
           (lambda (a b)
             (string<
              (format "%s/%s"
                      (or (cdr (assoc "mode" a)) "")
                      (or (cdr (assoc "key" a)) ""))
              (format "%s/%s"
                      (or (cdr (assoc "mode" b)) "")
                      (or (cdr (assoc "key" b)) ""))))))))

(defun Aaronnote--open-payload ()
  "Return an `open' payload for the active file."
  (let* ((file (or Aaronnote--active-file
                   (Aaronnote--ensure-default-file)))
         (truename (file-truename file)))
    (setq Aaronnote--active-file truename)
    `(("type" . "open")
      ("file" . ,truename)
      ("title" . ,(file-name-nondirectory truename))
      ("mode" . ,(Aaronnote--file-mode truename))
      ("content" . ,(Aaronnote--buffer-content truename))
      ("notes" . ,(or (Aaronnote--note-summaries) []))
      ("snippets" . ,(Aaronnote--snippet-summaries)))))

(defun Aaronnote--handle-message (ws message)
  "Handle frontend MESSAGE from WS."
  (unless (Aaronnote--valid-message-p message)
    (Aaronnote--send ws '(("type" . "saved")
                          ("ok" . :json-false)
                          ("message" . "Invalid token"))))
  (when (Aaronnote--valid-message-p message)
    (let ((type (alist-get 'type message)))
      (cond
       ((equal type "hello")
        (Aaronnote--send ws (Aaronnote--open-payload)))
       ((equal type "save")
        (let ((file (alist-get 'file message))
              (content (alist-get 'content message)))
          (if (and (stringp file) (stringp content))
              (condition-case err
                  (progn
                    (Aaronnote--write-content file content)
                    (Aaronnote--send
                     ws `(("type" . "saved")
                          ("ok" . t)
                          ("file" . ,file)
                          ("message" . "Saved"))))
                (error
                 (Aaronnote--send
                  ws `(("type" . "saved")
                       ("ok" . :json-false)
                       ("message" . ,(error-message-string err))))))
            (Aaronnote--send
             ws '(("type" . "saved")
                  ("ok" . :json-false)
                  ("message" . "Bad save payload"))))))
       ((equal type "open-file")
        (when-let* ((file (alist-get 'file message)))
          (run-at-time 0 nil #'find-file file)))
       ((equal type "open-url")
        (when-let* ((url (alist-get 'url message)))
          (Aaronnote--open-system-url url)))))))

(defun Aaronnote--ws-on-message (ws frame)
  "Decode WebSocket FRAME from WS."
  (let ((payload (websocket-frame-payload frame)))
    (when (stringp payload)
      (condition-case err
          (Aaronnote--handle-message
           ws (json-parse-string payload :object-type 'alist))
        (error
         (message "Aaronnote websocket decode error: %s" err))))))

(defun Aaronnote--ws-ensure ()
  "Start the WebSocket bridge if needed and return its port."
  (require 'websocket)
  (Aaronnote--ensure-token)
  (unless (and Aaronnote--ws-server
               (process-live-p Aaronnote--ws-server))
    (setq Aaronnote--ws-server
          (websocket-server
           0
           :host 'local
           :on-message #'Aaronnote--ws-on-message))
    (setq Aaronnote--ws-port
          (process-contact Aaronnote--ws-server :service)))
  Aaronnote--ws-port)

(defun Aaronnote--ws-stop ()
  "Stop the WebSocket bridge."
  (when (and Aaronnote--ws-server
             (process-live-p Aaronnote--ws-server))
    (ignore-errors
      (websocket-server-close Aaronnote--ws-server)))
  (setq Aaronnote--ws-server nil
        Aaronnote--ws-port nil
        Aaronnote--token nil))

(defun Aaronnote--url ()
  "Return the Aaronnote frontend URL."
  (format "http://127.0.0.1:%d/?emacsPort=%d&token=%s"
          Aaronnote-port
          (Aaronnote--ws-ensure)
          (url-hexify-string (Aaronnote--ensure-token))))

(defun Aaronnote--setup-xwidget-buffer ()
  "Install local xwidget bindings for Aaronnote."
  (setq-local Aaronnote--xwidget-owned t)
  (local-set-key (kbd "M-w") #'Aaronnote-stop)
  (add-hook 'kill-buffer-hook #'Aaronnote--xwidget-kill-h nil t))

(defun Aaronnote--xwidget-kill-h ()
  "Clear xwidget state when the Aaronnote buffer is killed."
  (when Aaronnote--xwidget-owned
    (setq Aaronnote--xwidget-buffer nil)))

(defun Aaronnote--open-xwidget (url)
  "Open URL in the Aaronnote xwidget buffer."
  (require 'xwidget)
  (unless (featurep 'xwidget-internal)
    (user-error "This Emacs build has no xwidget support"))
  (if (and Aaronnote--xwidget-buffer
           (buffer-live-p Aaronnote--xwidget-buffer))
      (with-current-buffer Aaronnote--xwidget-buffer
        (Aaronnote--setup-xwidget-buffer)
        (xwidget-webkit-goto-url url)
        (pop-to-buffer (current-buffer)))
    (xwidget-webkit-browse-url url t)
    (Aaronnote--setup-xwidget-buffer)
    (setq Aaronnote--xwidget-buffer (current-buffer))))

(defun Aaronnote--open-system-url (url)
  "Open URL with the operating system's default browser."
  (cond
   ((eq system-type 'darwin)
    (start-process "Aaronnote-open" nil "open" url))
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" url))
   ((executable-find "xdg-open")
    (start-process "Aaronnote-open" nil "xdg-open" url))
   (t
    (browse-url-default-browser url))))

;;;###autoload
(defun Aaronnote-start (&optional port)
  "Start the Aaronnote frontend and Emacs bridge.
With prefix argument PORT, prompt for the Vite server port."
  (interactive
   (list (when current-prefix-arg
           (read-number "Aaronnote port: " Aaronnote-port))))
  (when port
    (setq Aaronnote-port port))
  (Aaronnote--ws-ensure)
  (unless (Aaronnote--live-process-p)
    (let ((default-directory (Aaronnote--directory)))
      (unless (file-directory-p (expand-file-name "node_modules" default-directory))
        (user-error "Missing node_modules in %s; run npm install there first"
                    default-directory))
      (setq Aaronnote--process
            (make-process
             :name "Aaronnote"
             :buffer (get-buffer-create "*Aaronnote*")
             :command (list Aaronnote-npm-command
                            "run" "start" "--"
                            "--port" (number-to-string Aaronnote-port)
                            "--strictPort")
             :connection-type 'pipe
             :noquery t))))
  (message "Aaronnote running at %s" (Aaronnote--url)))

;;;###autoload
(defun Aaronnote-up (&optional file)
  "Start Aaronnote and open FILE, or the current buffer file, in the system browser."
  (interactive)
  (Aaronnote-open-system file))

;;;###autoload
(defun Aaronnote-up-xwidget (&optional file)
  "Start Aaronnote and open FILE, or the current buffer file, in xwidget."
  (interactive)
  (let ((target (or file
                    buffer-file-name
                    (Aaronnote--ensure-default-file))))
    (setq Aaronnote--active-file (file-truename target))
    (Aaronnote-start)
    (Aaronnote--open-xwidget (Aaronnote--url))))

;;;###autoload
(defun Aaronnote-open-system (&optional file)
  "Start Aaronnote and open FILE in the system browser instead of xwidget."
  (interactive)
  (let ((target (or file
                    buffer-file-name
                    (Aaronnote--ensure-default-file))))
    (setq Aaronnote--active-file (file-truename target))
    (Aaronnote-start)
    (let ((url (Aaronnote--url)))
      (Aaronnote--open-system-url url)
      (message "Aaronnote system URL: %s" url))))

;;;###autoload
(defun Aaronnote-up-system (&optional file)
  "Alias command for opening Aaronnote in the system browser."
  (interactive)
  (Aaronnote-open-system file))

;;;###autoload
(defun Aaronnote-open-file (file)
  "Open FILE in Aaronnote."
  (interactive
   (list (read-file-name "Aaronnote file: "
                         (or (and buffer-file-name
                                  (file-name-directory buffer-file-name))
                             default-directory)
                         nil t)))
  (Aaronnote-up file))

;;;###autoload
(defun Aaronnote-stop ()
  "Stop Aaronnote and its bridge."
  (interactive)
  (when (Aaronnote--live-process-p)
    (delete-process Aaronnote--process))
  (setq Aaronnote--process nil)
  (Aaronnote--ws-stop)
  (when (and Aaronnote--xwidget-buffer
             (buffer-live-p Aaronnote--xwidget-buffer))
    (kill-buffer Aaronnote--xwidget-buffer))
  (setq Aaronnote--xwidget-buffer nil)
  (message "Aaronnote stopped"))

;;;###autoload
(defun Aaronnote-restart ()
  "Restart Aaronnote and reopen the current active file."
  (interactive)
  (let ((file Aaronnote--active-file))
    (Aaronnote-stop)
    (Aaronnote-up file)))

;;;###autoload
(defun Aaronnote-status ()
  "Report Aaronnote process and bridge status."
  (interactive)
  (message "Aaronnote process=%s ws=%s url=%s"
           (if (Aaronnote--live-process-p) "running" "stopped")
           (if (and Aaronnote--ws-server
                    (process-live-p Aaronnote--ws-server))
               (number-to-string Aaronnote--ws-port)
             "stopped")
           (if (and Aaronnote--ws-server
                    (process-live-p Aaronnote--ws-server))
               (Aaronnote--url)
             "n/a")))

(add-hook 'kill-emacs-hook #'Aaronnote-stop)

(provide 'Aaronnote)
;;; Aaronnote.el ends here

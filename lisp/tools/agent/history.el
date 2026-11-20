;;; tools/agent/history.el --- One conversation history across agents -*- lexical-binding: t; -*-

;;; Commentary:
;; Claude Code, Codex, OpenCode and Antigravity keep conversations on the machine
;; where they run.  This file reads those stores, read-only and without a copy
;; or index of its own, so one picker lists their conversations
;; (C63 in docs/design/agent-management.md):
;;
;;   claude    ~/.claude/projects/<folder>/<id>.jsonl   (one file per session)
;;   codex     ~/.codex/state_5.sqlite, table threads  (index of rollout files)
;;   opencode  ~/.local/share/opencode/opencode.db     (sessions and messages)
;;   agy       ~/.gemini/antigravity-cli/conversation_summaries.db
;;
;; A conversation of the session's own agent resumes natively.  One of another
;; agent, or one whose native restore fails, opens in a new session that is
;; asked to read its transcript: Codex can continue what Claude Code started.
;; The stores are read on the target where the agent runs, through the Remote
;; file and process APIs, so a remote workspace lists its own history.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'hex-util)
(require 'url-parse)
(require 'url-util)
(require 'tools/agent/acp)
(require 'noema-agent-text)

(declare-function +agent-shell-emacs-file-name "tools/agent/config" (logical))
(declare-function +agent-context-content-blocks "tools/agent/context" (prompt references))
(declare-function +agent-context--deliver "tools/agent/context" (buffer blocks &optional instructions no-focus))
(defvar +programmatic-file-visit)
(declare-function vertico-exit "vertico" (&optional arg))
(declare-function markdown-view-mode "markdown-mode" ())
(declare-function consult--read "consult" (table &rest options))
(declare-function posframe-show "posframe" (buffer-or-name &rest args))
(declare-function posframe-delete-frame "posframe" (buffer-or-name))
(declare-function posframe-workable-p "posframe" ())
(declare-function consult--lookup-cdr "consult" (selected candidates &rest _))
(declare-function noema-sessions-native-binding "noema-sessions" (id root))
(declare-function +agent-acp-mark-session-buffer "tools/agent/acp" (buffer name agent directory))
(defvar noema-agent-promote--session-id)
(declare-function remote-path-probe "remote-path" (&optional context force callback))
(declare-function remote-path-facts-home "remote-path" (facts))
(declare-function remote-context "remote-fs" (&optional path))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-make-file-name "remote-fs" (target-id localname))
(declare-function remote-file-local-name "remote-fs" (file-name))
(declare-function remote-canonicalize-emacs-file-name "remote-fs" (file-name &optional directory))

(defgroup +agent-history nil
  "One conversation history across Claude Code, Codex, OpenCode and Antigravity."
  :group '+agent-session)

(defcustom +agent-history-limit 300
  "Most conversations listed from one agent's store."
  :type 'integer)

(defcustom +agent-history-head-bytes 65536
  "Bytes read from the start of a transcript file to describe it."
  :type 'integer)

(defcustom +agent-history-preview-bytes 262144
  "Bytes read from the end of a transcript file to describe it."
  :type 'integer)

(defcustom +agent-history-page-bytes 131072
  "Bytes of a transcript read per preview page, walking back from its end."
  :type 'integer)

;;;; ── Where the stores are ─────────────────────────────────────────────────

(defun +agent-history--target (directory)
  "Return DIRECTORY's target ID."
  (remote-file-name-target (remote-canonicalize-emacs-file-name directory)))

(defun +agent-history--emacs-name (target native)
  "Return the Emacs name of NATIVE, a path on TARGET."
  (+agent-shell-emacs-file-name (remote-make-file-name target native)))

(defun +agent-history--native (file)
  "Return FILE's path on its own machine."
  (remote-file-local-name (remote-canonicalize-emacs-file-name file)))

(defun +agent-history--home (directory)
  "Return the home directory of DIRECTORY's target as an Emacs name, or nil."
  (let ((home (ignore-errors
                (remote-path-facts-home (remote-path-probe (remote-context directory))))))
    (and (stringp home)
         (file-name-as-directory
          (+agent-history--emacs-name (+agent-history--target directory) home)))))

;;;; ── Reading the stores ───────────────────────────────────────────────────

(defun +agent-history--sqlite-name (db &optional closed-wal)
  "Return DB's native SQLite name, optionally reading a CLOSED-WAL snapshot.
Use immutable mode only when no WAL file exists, so active writes stay visible."
  (let ((native (+agent-history--native db)))
    (if (and closed-wal (not (file-exists-p (concat db "-wal"))))
        (concat (url-encode-url (concat "file://" native)) "?immutable=1")
      native)))

(defun +agent-history--sqlite (db query &optional closed-wal)
  "Return QUERY's rows on database file DB as alists, or nil.
sqlite3 runs read-only on the machine that holds DB.  CLOSED-WAL permits a
retry without sidecars when the native reader cannot open a closed WAL file."
  (when (file-readable-p db)
    (let ((default-directory (file-name-directory db)))
      (with-temp-buffer
        (let* ((native (+agent-history--native db))
               (status (ignore-errors
                         (process-file "sqlite3" nil '(t nil) nil
                                       "-readonly" "-json" native query)))
               (snapshot (and closed-wal (not (eq status 0))
                              (+agent-history--sqlite-name db t))))
          (when (and snapshot (not (equal snapshot native)))
            (erase-buffer)
            (setq status (ignore-errors
                           (process-file "sqlite3" nil '(t nil) nil
                                         "-readonly" "-json" snapshot query))))
          (when (eq status 0)
            (goto-char (point-min))
            (and (re-search-forward "\\S-" nil t)
                 (progn (goto-char (point-min))
                        (ignore-errors
                          (json-parse-buffer :object-type 'alist :array-type 'list
                                             :null-object nil :false-object nil))))))))))

(defun +agent-history--sql-string (text)
  "Return TEXT as an SQL string literal."
  (concat "'" (string-replace "'" "''" text) "'"))

(defun +agent-history--sql-scope (column dirs)
  "Return an SQL condition keeping COLUMN inside one of native DIRS, or 1."
  (if (null dirs) "1"
    (concat "("
            (mapconcat (lambda (dir)
                         (let ((dir (directory-file-name dir)))
                           (format "%s = %s OR %s LIKE %s"
                                   column (+agent-history--sql-string dir)
                                   column (+agent-history--sql-string (concat dir "/%")))))
                       dirs " OR ")
            ")")))

(defun +agent-history--in-scope-p (cwd dirs)
  "Return non-nil when native CWD lies inside one of native DIRS, or DIRS is nil."
  (or (null dirs)
      (and (stringp cwd)
           (let ((cwd (file-name-as-directory cwd)))
             (seq-some (lambda (dir) (string-prefix-p (file-name-as-directory dir) cwd))
                       dirs)))))

(defun +agent-history--json-lines (text)
  "Return the JSON objects of TEXT's complete lines."
  (delq nil (mapcar (lambda (line)
                      (ignore-errors
                        (json-parse-string line :object-type 'alist :array-type 'list
                                           :null-object nil :false-object nil)))
                    (split-string text "\n" t))))

(defun +agent-history--read-range (file start end)
  "Return FILE's bytes START to END as text, or nil."
  (ignore-errors
    (with-temp-buffer
      (set-buffer-multibyte t)
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file nil start end))
      (buffer-string))))

(defun +agent-history--file-head (file)
  "Return the complete lines at the start of FILE."
  (when-let* ((text (+agent-history--read-range file 0 +agent-history-head-bytes)))
    (if (< (length text) +agent-history-head-bytes) text
      (substring text 0 (or (+agent-history--last-index "\n" text) (length text))))))

(defun +agent-history--file-tail (file)
  "Return the complete lines at the end of FILE."
  (let* ((size (file-attribute-size (file-attributes file)))
         (start (max 0 (- (or size 0) +agent-history-preview-bytes)))
         (text (+agent-history--read-range file start size)))
    (when text
      (if (zerop start) text
        (substring text (min (length text) (1+ (or (string-search "\n" text) -1))))))))

(defun +agent-history--last-index (needle haystack)
  "Return the index of the last NEEDLE in HAYSTACK, or nil."
  (let ((index nil) (start 0))
    (while (when-let* ((found (string-search needle haystack start)))
             (setq index found start (1+ found))))
    index))

(defun +agent-history--text (content)
  "Return the text of a message CONTENT: a string or a list of parts."
  (cond ((stringp content) content)
        ((listp content)
         (string-join (delq nil (mapcar (lambda (part)
                                          (and (listp part)
                                               (member (map-elt part 'type)
                                                       '("text" "Text" "input_text" "output_text"))
                                               (map-elt part 'text)))
                                        content))
                      "\n"))))

(defun +agent-history--prompt-p (text)
  "Return non-nil when TEXT is something a person wrote, not injected context."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (not (string-prefix-p "<" (string-trim-left text)))))

;;; Claude Code

(defun +agent-history--claude-folder (native)
  "Return Claude Code's project folder name for NATIVE directory."
  (replace-regexp-in-string "[^[:alnum:]]" "-" (directory-file-name native)))

(defun +agent-history--claude-title (file)
  "Return the last title Claude Code wrote near the end of FILE, or nil.
It names the conversation once it has one, so it comes after the opening."
  (let (title)
    (dolist (line (+agent-history--json-lines (or (+agent-history--file-tail file) "")))
      (pcase (map-elt line 'type)
        ("ai-title" (setq title (or (map-elt line 'aiTitle) title)))
        ("custom-title" (setq title (or (map-elt line 'customTitle) title)))
        ("summary" (setq title (or (map-elt line 'summary) title)))))
    title))

(defun +agent-history--claude-row (file)
  "Describe Claude Code transcript FILE."
  (let ((lines (+agent-history--json-lines (or (+agent-history--file-head file) "")))
        id cwd branch title prompt (turns 0))
    (dolist (line lines)
      (setq id (or id (map-elt line 'sessionId))
            cwd (or cwd (map-elt line 'cwd))
            branch (or branch (let ((b (map-elt line 'gitBranch)))
                                (and (stringp b) (not (string-empty-p b)) b))))
      (pcase (map-elt line 'type)
        ("ai-title" (setq title (or title (map-elt line 'aiTitle))))
        ("summary" (setq title (or title (map-elt line 'summary))))
        ("user"
         (unless (map-elt line 'isMeta)
           (let ((text (+agent-history--text (map-nested-elt line '(message content)))))
             (when (+agent-history--prompt-p text)
               (cl-incf turns)
               (setq prompt (or prompt text))))))))
    (when (and id (> turns 0))
      (list :agent "claude" :id id :cwd cwd :branch branch
            :title (or (+agent-history--claude-title file) title prompt) :prompt prompt
            :updated (float-time (file-attribute-modification-time (file-attributes file)))
            :file file))))

(defun +agent-history--claude (home dirs)
  "Return Claude Code conversations under HOME inside native DIRS."
  (let* ((root (expand-file-name ".claude/projects/" home))
         (prefixes (mapcar #'+agent-history--claude-folder dirs))
         (folders (and (file-directory-p root)
                       (seq-filter
                        (lambda (folder)
                          (let ((name (file-name-nondirectory folder)))
                            (or (null prefixes)
                                (seq-some (lambda (prefix)
                                            (or (equal name prefix)
                                                (string-prefix-p (concat prefix "-") name)))
                                          prefixes))))
                        (directory-files root t "\\`[^.]"))))
         (files (seq-take
                 (sort (mapcan (lambda (folder)
                                 (directory-files-and-attributes folder t "\\.jsonl\\'" t))
                               folders)
                       (lambda (a b) (time-less-p (file-attribute-modification-time (cdr b))
                                                  (file-attribute-modification-time (cdr a)))))
                 +agent-history-limit)))
    (seq-filter (lambda (row) (+agent-history--in-scope-p (plist-get row :cwd) dirs))
                (delq nil (mapcar (lambda (entry) (+agent-history--claude-row (car entry)))
                                  files)))))

;;; Codex

(defun +agent-history--codex (home dirs)
  "Return Codex conversations under HOME inside native DIRS."
  (mapcar (lambda (row)
            (list :agent "codex" :id (map-elt row 'id) :cwd (map-elt row 'cwd)
                  :branch (map-elt row 'git_branch)
                  :title (let ((title (map-elt row 'title)))
                           (if (and (stringp title) (not (string-empty-p title))) title
                             (map-elt row 'first_user_message)))
                  :prompt (map-elt row 'first_user_message)
                  :updated (float (or (map-elt row 'updated_at) 0))
                  :file (let ((path (map-elt row 'rollout_path)))
                          (and (stringp path)
                               (+agent-history--emacs-name (+agent-history--target home) path)))))
          (+agent-history--sqlite
           (expand-file-name ".codex/state_5.sqlite" home)
           (format "SELECT id, title, first_user_message, cwd, updated_at, git_branch, rollout_path
FROM threads WHERE archived = 0 AND source NOT LIKE '{%%' AND source <> 'exec'
 AND coalesce(first_user_message, '') <> '' AND %s
ORDER BY updated_at DESC LIMIT %d"
                   (+agent-history--sql-scope "cwd" dirs) +agent-history-limit))))

;;; OpenCode

(defun +agent-history--opencode-db (home)
  "Return OpenCode's database under HOME."
  (expand-file-name ".local/share/opencode/opencode.db" home))

(defun +agent-history--opencode (home dirs)
  "Return OpenCode conversations under HOME inside native DIRS."
  (mapcar (lambda (row)
            (list :agent "opencode" :id (map-elt row 'id) :cwd (map-elt row 'directory)
                  :title (map-elt row 'title) :turns (map-elt row 'turns)
                  :updated (/ (float (or (map-elt row 'time_updated) 0)) 1000)
                  :db (+agent-history--opencode-db home)))
          (+agent-history--sqlite
           (+agent-history--opencode-db home)
           (format "SELECT s.id, s.title, s.directory, s.time_updated,
 (SELECT count(*) FROM message m WHERE m.session_id = s.id
   AND json_extract(m.data, '$.role') = 'user') AS turns
FROM session s WHERE s.parent_id IS NULL AND s.time_archived IS NULL AND %s
 AND turns > 0
ORDER BY s.time_updated DESC LIMIT %d"
                   (+agent-history--sql-scope "s.directory" dirs) +agent-history-limit))))

;;; Antigravity

(defun +agent-history--agy-db (home)
  "Return Antigravity's native summary database under HOME."
  (expand-file-name ".gemini/antigravity-cli/conversation_summaries.db" home))

(defun +agent-history--agy-scope (dirs)
  "Return a summary SQL condition matching any workspace URI in native DIRS."
  (if (null dirs) "1"
    (format
     "EXISTS (SELECT 1 FROM json_each(s.workspace_uris) w WHERE %s)"
     (mapconcat
      (lambda (dir)
        (let ((uri (url-encode-url (concat "file://" (directory-file-name dir)))))
          (format "w.value = %s OR instr(w.value, %s) = 1"
                  (+agent-history--sql-string uri)
                  (+agent-history--sql-string
                   (if (string-suffix-p "/" uri) uri (concat uri "/"))))))
      dirs " OR "))))

(defun +agent-history--agy (home dirs)
  "Read Antigravity's native conversations under HOME, scoped to native DIRS."
  (let ((db (+agent-history--agy-db home)))
    (delq nil
          (mapcar
           (lambda (row)
             (let* ((uris (ignore-errors (json-parse-string
                                         (map-elt row 'workspace_uris) :array-type 'list)))
                    (directories
                     (delq nil
                           (mapcar (lambda (uri)
                                     (let ((url (url-generic-parse-url uri)))
                                       (when (and (equal (url-type url) "file")
                                                  (member (url-host url) '(nil "" "localhost")))
                                         (decode-coding-string
                                          (url-unhex-string (url-filename url)) 'utf-8))))
                                   uris)))
                    (cwd (seq-find (lambda (dir) (+agent-history--in-scope-p dir dirs))
                                   directories)))
               (when cwd
                 (list :agent "agy" :id (map-elt row 'conversation_id) :cwd cwd
                       :title (if (string-empty-p (or (map-elt row 'title) ""))
                                  (map-elt row 'preview) (map-elt row 'title))
                       :prompt (map-elt row 'preview) :turns (map-elt row 'turns)
                       :updated (float (or (map-elt row 'updated) 0))
                       :db (expand-file-name
                            (concat "conversations/" (map-elt row 'conversation_id) ".db")
                            (file-name-directory db))))))
           (+agent-history--sqlite
            db
            (format "SELECT s.conversation_id, s.title, s.preview, s.workspace_uris,
 CAST(strftime('%%s', s.last_modified_time) AS INTEGER) AS updated,
 (SELECT sum(json_array_length(CAST(f.user_step_byte_offsets AS TEXT)))
  FROM conversations_fts f WHERE f.conversation_id = s.conversation_id
  AND f.chunk_index >= 0) AS turns
FROM conversation_summaries s
WHERE s.step_count > 0 AND s.parent_conversation_id = '' AND s.nesting_depth = 0
 AND NOT EXISTS (SELECT 1 FROM conversations_fts f
 WHERE f.conversation_id = s.conversation_id AND f.archived = 1) AND %s
ORDER BY s.last_modified_time DESC LIMIT %d"
                    (+agent-history--agy-scope dirs) +agent-history-limit) t)))))

;;;; ── The history ──────────────────────────────────────────────────────────

(defconst +agent-history-readers
  '(("claude" . +agent-history--claude)
    ("codex" . +agent-history--codex)
    ("opencode" . +agent-history--opencode)
    ("agy" . +agent-history--agy))
  "Each agent and the function that lists its conversations (HOME DIRS).")

(defun +agent-history-scope (directory &optional buffer)
  "Return the native folders whose conversations DIRECTORY's history shows.
DIRECTORY itself, BUFFER's project root and the enclosing project (C55)."
  (delete-dups
   (mapcar (lambda (dir) (file-name-as-directory (+agent-history--native dir)))
           (cons directory (+agent-acp--saved-homes directory buffer)))))

(defun +agent-history--scratch-p (cwd home)
  "Return non-nil when native CWD is a throwaway folder under native HOME.
Side sessions, exports and provider probes run in temporary directories that
are gone afterwards; their conversations are not something to resume."
  (and (stringp cwd)
       (or (string-match-p "\\`/\\(?:private/\\)?\\(?:tmp\\|var/folders\\)/" cwd)
           (string-prefix-p (concat home ".config/emacs/var/noema/tmp/") cwd)
           (string-prefix-p (concat home ".emacs.d/var/noema/tmp/") cwd))))

(defun +agent-history-list (directory &optional all buffer)
  "Return the conversations of every agent on DIRECTORY's target, newest first.
They are those of `+agent-history-scope', or of every folder when ALL;
conversations in throwaway folders are left out (`+agent-history--scratch-p')."
  (let* ((home (+agent-history--home directory))
         (native-home (and home (+agent-history--native home)))
         (dirs (unless all (+agent-history-scope directory buffer))))
    (when home
      (sort (seq-remove
             (lambda (row) (+agent-history--scratch-p (plist-get row :cwd) native-home))
             (mapcan (lambda (reader)
                      (condition-case err
                          (funcall (cdr reader) home dirs)
                        (error (message "History of %s unavailable: %s"
                                        (car reader) (error-message-string err))
                               nil)))
                    +agent-history-readers))
            (lambda (a b) (> (plist-get a :updated) (plist-get b :updated)))))))

(defun +agent-history--when (time)
  "Return float TIME as a short age."
  (let ((age (- (float-time) time)))
    (cond ((< age 60) "now")
          ((< age 3600) (format "%dm ago" (/ age 60)))
          ((< age 86400) (format "%dh ago" (/ age 3600)))
          ((< age (* 7 86400)) (format "%dd ago" (/ age 86400)))
          (t (format-time-string "%b %d" time)))))

(defun +agent-history--cell (text width)
  "Return TEXT on one line, cut or padded to WIDTH columns."
  (truncate-string-to-width
   (replace-regexp-in-string "[\n\t ]+" " " (string-trim (or text ""))) width nil ?\s "…"))

(defun +agent-history-label (row)
  "Return ROW's picker line: agent, age, turns, branch, folder and title."
  (concat
   (propertize (+agent-history--cell (plist-get row :agent) 8) 'face 'font-lock-keyword-face)
   " " (propertize (+agent-history--cell (+agent-history--when (plist-get row :updated)) 8)
                   'face 'font-lock-comment-face)
   " " (propertize (+agent-history--cell (if-let* ((n (plist-get row :turns))) (format "%d↵" n) "") 4)
                   'face 'font-lock-comment-face)
   " " (propertize (+agent-history--cell (plist-get row :branch) 12) 'face 'font-lock-constant-face)
   " " (propertize (+agent-history--cell (and (plist-get row :cwd)
                                              (file-name-nondirectory
                                               (directory-file-name (plist-get row :cwd))))
                                         14)
                   'face 'font-lock-type-face)
   " " (+agent-history--cell (plist-get row :title) 70)
   (if (+agent-acp--session-open-p (plist-get row :id))
       (propertize "  ● open" 'face 'success)
     "")
   ;; Invisible, so two conversations with the same title stay distinct.
   (propertize (concat " " (plist-get row :id)) 'invisible t)))

;;;; ── Preview ──────────────────────────────────────────────────────────────

(defconst +agent-history--page-messages 6
  "Messages one preview page asks for.")

(defconst +agent-history--page-windows 4
  "Most byte windows one page reads looking for messages; bounds a page's cost
when the end of a transcript is all tool output.")

(defvar +agent-history--message-chars 2000
  "Characters of one message kept: the preview's cap; the view binds more.")

(defun +agent-history--read-bytes (file start end)
  "Return FILE's raw bytes START to END as a unibyte string, or nil."
  (ignore-errors
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil start end)
      (buffer-string))))

(defun +agent-history--message (agent line)
  "Return the (ROLE . TEXT) a person or agent wrote on transcript LINE of AGENT.
LINE is raw bytes; lines that cannot be a message are skipped before decoding."
  (when (pcase agent
          ("claude" (or (string-search "\"type\":\"user\"" line)
                        (string-search "\"type\":\"assistant\"" line)))
          ("codex" (or (string-search "\"type\":\"message\"" line)
                       (string-search "item_completed" line))))
    (when-let* ((object (ignore-errors
                          (json-parse-string (decode-coding-string line 'utf-8)
                                             :object-type 'alist :array-type 'list
                                             :null-object nil :false-object nil)))
                (message
                 (pcase agent
                   ("claude"
                    (let ((type (map-elt object 'type)))
                      (and (member type '("user" "assistant"))
                           (not (map-elt object 'isMeta))
                           (cons type (+agent-history--text
                                       (map-nested-elt object '(message content)))))))
                   ("codex"
                    ;; A message is recorded as a response item and, in newer
                    ;; files, again as a completed item.
                    (let* ((payload (map-elt object 'payload))
                           (item (map-elt payload 'item)))
                      (cond
                       ((and (equal (map-elt object 'type) "response_item")
                             (equal (map-elt payload 'type) "message")
                             (member (map-elt payload 'role) '("user" "assistant")))
                        (cons (map-elt payload 'role)
                              (+agent-history--text (map-elt payload 'content))))
                       ((and (equal (map-elt payload 'type) "item_completed")
                             (member (map-elt item 'type) '("UserMessage" "AgentMessage")))
                        (cons (if (equal (map-elt item 'type) "UserMessage") "user" "assistant")
                              (+agent-history--text (map-elt item 'content))))))))))
      (when (+agent-history--prompt-p (cdr message))
        (cons (car message)
              (truncate-string-to-width (string-trim (cdr message))
                                        +agent-history--message-chars nil nil "…"))))))

(defun +agent-history--file-page (row end)
  "Return (MESSAGES . NEXT) from ROW's transcript before byte END, newest first.
END nil means the end of the file.  NEXT is the END of the following page, or
nil at the start of the file.  Only fixed-size windows are read, never the
whole file."
  (let* ((file (plist-get row :file))
         (agent (plist-get row :agent))
         (end (or end (and file (file-attribute-size (file-attributes file)))))
         messages (windows 0))
    (while (and end (> end 0)
                (< (length messages) +agent-history--page-messages)
                (< windows +agent-history--page-windows))
      (cl-incf windows)
      (let* ((start (max 0 (- end +agent-history-page-bytes)))
             (bytes (or (+agent-history--read-bytes file start end) ""))
             (newline (and (> start 0)
                           (let ((index (string-search "\n" bytes)))
                             ;; A newline that ends the window starts no line in it.
                             (and index (< (1+ index) (length bytes)) index)))))
        ;; Before the first newline is a partial line, read with the next page.
        ;; A window inside one long line holds no whole line: step over it.
        (unless (and (> start 0) (null newline))
          (dolist (line (nreverse (split-string (substring bytes (if newline (1+ newline) 0))
                                                "\n" t)))
            (when-let* ((message (+agent-history--message agent line)))
              (unless (equal message (car (last messages)))
                (setq messages (append messages (list message)))))))
        (setq end (and (> start 0) (if newline (+ start newline 1) start)))))
    (cons messages end)))

(defun +agent-history--opencode-page (row before)
  "Return (MESSAGES . NEXT) of ROW's OpenCode session before time BEFORE."
  (let* ((rows (+agent-history--sqlite
                (plist-get row :db)
                (format "SELECT json_extract(m.data, '$.role') AS role,
 substr(json_extract(p.data, '$.text'), 1, %d) AS text, p.time_created AS t
FROM part p JOIN message m ON m.id = p.message_id
WHERE p.session_id = %s AND json_extract(p.data, '$.type') = 'text'%s
ORDER BY p.time_created DESC LIMIT %d"
                        (1+ +agent-history--message-chars)
                        (+agent-history--sql-string (plist-get row :id))
                        (if before (format " AND p.time_created < %d" before) "")
                        +agent-history--page-messages)))
         (messages (delq nil (mapcar (lambda (part)
                                       (let ((text (map-elt part 'text)))
                                         (when (+agent-history--prompt-p text)
                                           (cons (map-elt part 'role)
                                                 (truncate-string-to-width
                                                  (string-trim text)
                                                  +agent-history--message-chars nil nil "…")))))
                                     rows))))
    (cons messages
          (and (= (length rows) +agent-history--page-messages)
               (map-elt (car (last rows)) 't)))))

(defun +agent-history--agy-fields (bytes wanted)
  "Return length-delimited protobuf fields WANTED from unibyte BYTES.
Skip other wire fields without decoding them.  A bounded read may cut the
last field short; complete earlier fields remain usable."
  (let ((index 0) values)
    (cl-labels ((varint ()
                  (let ((value 0) (shift 0) byte)
                    (while (progn
                             (when (or (>= index (length bytes)) (>= shift 70))
                               (error "Truncated or invalid protobuf varint"))
                             (setq byte (aref bytes index) index (1+ index)
                                   value (logior value (ash (logand byte 127) shift))
                                   shift (+ shift 7))
                             (>= byte 128)))
                    value)))
      (ignore-errors
        (while (< index (length bytes))
          (let* ((tag (varint)) (field (ash tag -3)) (wire (logand tag 7)))
            (when (zerop field) (error "Invalid protobuf field"))
            (pcase wire
              (0 (varint))
              (1 (setq index (+ index 8)))
              (2 (let* ((size (varint)) (end (+ index size)))
                   (when (= field wanted)
                     (push (substring bytes index (min end (length bytes))) values))
                   (setq index end)))
              (5 (setq index (+ index 4)))
              (_ (error "Unsupported protobuf wire type")))))))
    (nreverse values)))

(defun +agent-history--agy-message (entry)
  "Decode ENTRY's original user text or public assistant reply, never thoughts.
Native steps 14/15 wrap their payload in protobuf fields 19/20 respectively."
  (ignore-errors
    (let* ((user (= (map-elt entry 'step_type) 14))
           (payload (decode-hex-string (map-elt entry 'payload)))
           (body (car (+agent-history--agy-fields payload (if user 19 20))))
           (text (if user
                     (or (when-let* ((parts (+agent-history--agy-fields body 3)))
                           (mapconcat (lambda (part)
                                        (or (car (+agent-history--agy-fields part 1)) ""))
                                      parts "\n"))
                         (car (+agent-history--agy-fields body 2)))
                   (or (car (+agent-history--agy-fields body 1))
                       (car (+agent-history--agy-fields body 8)))))
           (text (and text (decode-coding-string text 'utf-8))))
      (when (+agent-history--prompt-p text)
        (cons (if user "user" "assistant")
              (truncate-string-to-width (string-trim text)
                                        +agent-history--message-chars nil nil "…"))))))

(defun +agent-history--agy-page (row before)
  "Return (MESSAGES . NEXT) of ROW's native database before step BEFORE.
Read bounded step windows; the search index may not contain this conversation."
  (let ((next before) (windows 0) (more t) messages)
    (while (and more (< windows +agent-history--page-windows)
                (< (length messages) +agent-history--page-messages))
      (cl-incf windows)
      (let* ((count (- +agent-history--page-messages (length messages)))
             (rows (+agent-history--sqlite
                    (plist-get row :db)
                    (format "SELECT idx, step_type, hex(substr(step_payload, 1, %d)) AS payload
FROM steps WHERE step_type IN (14,15)%s ORDER BY idx DESC LIMIT %d"
                            +agent-history-page-bytes
                            (if next (format " AND idx < %d" next) "") count) t)))
        (setq messages (append messages (delq nil (mapcar #'+agent-history--agy-message rows)))
              more (= (length rows) count)
              next (and more (map-elt (car (last rows)) 'idx)))))
    (cons messages next)))

(defun +agent-history--page (row &optional next)
  "Return (MESSAGES . NEXT) of ROW, newest first, starting at NEXT."
  (or (ignore-errors
        (pcase (plist-get row :agent)
          ((or "claude" "codex") (and (plist-get row :file) (+agent-history--file-page row next)))
          ("opencode" (+agent-history--opencode-page row next))
          ("agy" (+agent-history--agy-page row next))))
      (list nil)))

(defun +agent-history--exchanges (row)
  "Return ROW's last messages as (ROLE . TEXT), oldest first."
  (reverse (car (+agent-history--page row))))

;; The preview uses one child frame per picker, without taking a layout window
;; or the keyboard.  Exiting the picker deletes that frame and empties its
;; reusable buffer; the next picker creates a new frame.  Moving through the
;; list is debounced and each conversation is read once per picker.

(defconst +agent-history-preview-buffer " *agent-history-preview*")

(defconst +agent-history--preview-width 84
  "Columns of the preview box.")

(defconst +agent-history--label-width 132
  "Columns a picker line takes before its `● open' mark, from `+agent-history-label'.")

(declare-function +agent-history--preview-wheel "tools/agent/history" (event))

(defvar +agent-history--preview-mouse-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '([wheel-down] [wheel-up] [mouse-4] [mouse-5]))
      (define-key map key #'+agent-history--preview-wheel))
    ;; A click must not select the box's window and take the picker's keyboard.
    (dolist (key '([down-mouse-1] [mouse-1] [drag-mouse-1] [double-mouse-1]
                   [down-mouse-2] [mouse-2] [down-mouse-3] [mouse-3]))
      (define-key map key #'ignore))
    map)
  "Mouse keys of the preview box: the wheel scrolls it in place.")

(defconst +agent-history--preview-max-messages 60
  "Most messages the preview holds for one conversation; open it for the rest.")

(defvar-local +agent-history--preview-entry nil
  "The conversation in the preview: a cons (ROW . PAGES).
PAGES is (MESSAGES . NEXT) as `+agent-history--page' returns, accumulated; it is
the same object the picker caches, so a revisit keeps what was loaded.")

(defun +agent-history--preview-insert-messages (messages)
  "Insert MESSAGES, newest first, at point."
  (dolist (message messages)
    (let ((user (equal (car message) "user")))
      (insert (propertize (if user "You" "Agent")
                          'face (if user 'font-lock-keyword-face 'success))
              "\n" (cdr message) "\n\n"))))

(defun +agent-history--preview-footer (pages)
  "Insert the line that ends the preview of PAGES."
  (insert (propertize (cond ((null (cdr pages)) "— start of conversation —")
                            ((>= (length (car pages)) +agent-history--preview-max-messages)
                             "— earlier messages: open the conversation —")
                            (t "↓ earlier messages load as you scroll"))
                      'face 'font-lock-comment-face
                      '+agent-history-footer t)))

(defun +agent-history--preview-render (row pages)
  "Fill the preview buffer with ROW's header and PAGES; return the buffer.
PAGES is (MESSAGES . NEXT), newest first."
  (let ((width (- +agent-history--preview-width 2)))
    (with-current-buffer (get-buffer-create +agent-history-preview-buffer)
      (unless (eq major-mode 'special-mode)
        (special-mode)
        (setq-local truncate-lines nil word-wrap t mode-line-format nil
                    cursor-type nil
                    fringe-indicator-alist '((continuation nil nil)))
        (use-local-map (make-composed-keymap +agent-history--preview-mouse-map
                                             special-mode-map)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (truncate-string-to-width
                             (replace-regexp-in-string
                              "[\n\t ]+" " " (or (plist-get row :title) "(untitled)"))
                             width nil nil "…")
                            'face 'bold)
                "\n"
                (propertize (format "%s · %s · %s · newest first\n%s\n\n"
                                    (plist-get row :agent) (plist-get row :id)
                                    (+agent-history--when (plist-get row :updated))
                                    (truncate-string-to-width
                                     (or (plist-get row :cwd) "") width nil nil "…"))
                            'face 'font-lock-comment-face))
        (+agent-history--preview-insert-messages
         (or (car pages)
             (and (plist-get row :prompt)
                  (list (cons "user" (truncate-string-to-width
                                      (plist-get row :prompt)
                                      +agent-history--message-chars nil nil "…"))))))
        (+agent-history--preview-footer pages)
        (goto-char (point-min)))
      (setq +agent-history--preview-entry (cons row pages))
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (set-window-start window (point-min))
        (set-window-point window (point-min)))
      (current-buffer))))

(defun +agent-history--preview-more ()
  "Append the next older page to the preview; return non-nil when it did.
The page is added to the cached entry, so the picker keeps it."
  (with-current-buffer +agent-history-preview-buffer
    (pcase-let* ((`(,row . ,pages) +agent-history--preview-entry))
      (when (and pages (cdr pages)
                 (< (length (car pages)) +agent-history--preview-max-messages))
        (pcase-let ((`(,messages . ,next) (+agent-history--page row (cdr pages))))
          (setcar pages (append (car pages) messages))
          (setcdr pages next)
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (or (text-property-any (point-min) (point-max)
                                                '+agent-history-footer t)
                             (point-max)))
              (delete-region (point) (point-max))
              (+agent-history--preview-insert-messages messages)
              (+agent-history--preview-footer pages)))
          t)))))

(defun +agent-history--preview-maybe-more (window)
  "Load an older page when WINDOW shows the end of the preview."
  (when (and (window-live-p window)
             (>= (window-end window t)
                 (with-current-buffer (window-buffer window)
                   (or (text-property-any (point-min) (point-max)
                                          '+agent-history-footer t)
                       (point-max)))))
    (+agent-history--preview-more)))

(defun +agent-history--preview-window ()
  "Return the window of the preview box while it is shown."
  (when-let* ((buffer (get-buffer +agent-history-preview-buffer))
              (window (get-buffer-window buffer t)))
    (and (frame-visible-p (window-frame window)) window)))

(defun +agent-history--preview-scroll-window (window lines)
  "Scroll WINDOW by LINES without selecting it, so the picker keeps the keyboard."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((start (save-excursion
                     (goto-char (window-start window))
                     (vertical-motion lines window)
                     (point))))
        ;; Point goes with the start: redisplay would otherwise scroll back to
        ;; the box's point at the top.
        (setq start (min start (max (point-min) (1- (point-max)))))
        (set-window-point window start)
        (set-window-start window start))
      (when (> lines 0)
        (+agent-history--preview-maybe-more window)))))

(defun +agent-history/preview-scroll-up (&optional lines)
  "Scroll the preview box forward by LINES, or most of its height."
  (interactive "P")
  (when-let* ((window (+agent-history--preview-window)))
    (+agent-history--preview-scroll-window
     window (if lines (prefix-numeric-value lines) (max 1 (- (window-body-height window) 2))))))

(defun +agent-history/preview-scroll-down (&optional lines)
  "Scroll the preview box back by LINES, or most of its height."
  (interactive "P")
  (when-let* ((window (+agent-history--preview-window)))
    (+agent-history--preview-scroll-window
     window (- (if lines (prefix-numeric-value lines) (max 1 (- (window-body-height window) 2)))))))

(defun +agent-history--preview-wheel (event)
  "Scroll the preview box under the wheel EVENT, leaving selection alone."
  (interactive (list last-input-event))
  (let ((window (posn-window (event-start event))))
    (+agent-history--preview-scroll-window
     window (if (memq (event-basic-type event) '(wheel-up mouse-4)) -3 3))))

(defun +agent-history--preview-frame-p (frame)
  "Return non-nil when FRAME is the frame of the preview box."
  (and (frame-live-p frame)
       (eq (window-buffer (frame-root-window frame))
           (get-buffer +agent-history-preview-buffer))))

(defun +agent-history--switch-frame-a (event)
  "Ignore the frame switch EVENT when it goes into the preview box.
macOS precedes a wheel event over the box with a switch to its frame; taking it
would move the picker's keyboard into the box.  Installed only while a picker
is open (`+agent-history-read')."
  (+agent-history--preview-frame-p (and (consp event) (cadr event))))

(defun +agent-history--wheel-filter (command)
  "Return COMMAND when the wheel event being read is over the preview box.
Otherwise nil, so the picker's own wheel binding applies.  With the frame switch
into the box ignored, its wheel events are looked up in the picker's keymaps."
  (let ((window (and (consp last-input-event)
                     (posn-window (event-start last-input-event)))))
    (and (windowp window)
         (+agent-history--preview-frame-p (window-frame window))
         command)))

(defvar +agent-history-picker-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '([wheel-down] [wheel-up] [mouse-4] [mouse-5]))
      (define-key map key '(menu-item "" +agent-history--preview-wheel
                                      :filter +agent-history--wheel-filter)))
    (dolist (key '("M-RET" "M-<return>" "s-RET" "s-<return>"))
      (define-key map (kbd key) #'+agent-history/picker-view))
    (define-key map (kbd "C-M-v") #'+agent-history/preview-scroll-up)
    (define-key map (kbd "C-M-S-v") #'+agent-history/preview-scroll-down)
    (define-key map (kbd "M-<next>") #'+agent-history/preview-scroll-up)
    (define-key map (kbd "M-<prior>") #'+agent-history/preview-scroll-down)
    map)
  "Picker keys: Cmd-RET views the conversation as a buffer; the wheel over the
box and the other-window scroll keys scroll the preview.")

(defvar +agent-history--picker-action nil
  "What the picker's caller does with the chosen row: nil opens it, `view' views it.")

(defun +agent-history/picker-view ()
  "Accept the selected conversation to view it as a buffer instead of opening it."
  (interactive)
  (setq +agent-history--picker-action 'view)
  (if (and (fboundp 'vertico-exit) (bound-and-true-p vertico--input))
      (vertico-exit)
    (exit-minibuffer)))

(defun +agent-history--preview-geometry (frame)
  "Return (X Y WIDTH HEIGHT) in pixels and lines for the preview on FRAME.
Beside the candidates inside the minibuffer area when the list is tall and the
frame wide enough, otherwise right-aligned just above the minibuffer."
  (let* ((mini (minibuffer-window frame))
         (cw (frame-char-width frame))
         (ch (frame-char-height frame))
         (frame-w (frame-pixel-width frame))
         (mini-top (if (eq (window-frame mini) frame)
                       (nth 1 (window-pixel-edges mini))
                     (frame-pixel-height frame)))
         (mini-lines (if (eq (window-frame mini) frame) (window-total-height mini) 1))
         (box-w (* cw (+ +agent-history--preview-width 2)))
         (beside (and (>= mini-lines 8)
                      (>= (- frame-w box-w (* 2 cw))
                          (* cw (+ +agent-history--label-width 4))))))
    (if beside
        (list (- frame-w box-w (* 2 cw)) mini-top
              +agent-history--preview-width (max 4 (- mini-lines 1)))
      (let ((lines (max 4 (min 20 (- (/ mini-top ch) 3)))))
        (list (- frame-w box-w (* 2 cw))
              (max 0 (- mini-top (* ch (1+ lines)) (/ ch 2)))
              +agent-history--preview-width lines)))))

(defvar +agent-history--preview-geometry nil
  "Geometry of the preview being shown, from `+agent-history--preview-geometry'.")

(defun +agent-history--preview-poshandler (_info)
  "Place the preview box where `+agent-history--preview-geometry' says."
  (cons (nth 0 +agent-history--preview-geometry)
        (nth 1 +agent-history--preview-geometry)))

(defun +agent-history--show-preview (buffer)
  "Show preview BUFFER in the hover box, or in a window without a GUI.
Return non-nil when it is the box, which leaves the window layout alone."
  (if (and (require 'posframe nil t) (posframe-workable-p))
      (let* ((frame (window-frame (minibuffer-selected-window)))
             (geometry (+agent-history--preview-geometry
                        (or (frame-parent frame) frame))))
        (setq +agent-history--preview-geometry geometry)
        (posframe-show buffer
                       :poshandler #'+agent-history--preview-poshandler
                       :width (nth 2 geometry) :height (nth 3 geometry)
                       :left-fringe 8 :right-fringe 8
                       :border-width 1
                       :border-color (face-background 'child-frame-border nil t)
                       :background-color (face-background
                                          (if (facep 'lsp-ui-doc-background)
                                              'lsp-ui-doc-background
                                            'default)
                                          nil t)
                       :accept-focus nil)
        t)
    (display-buffer buffer '((display-buffer-reuse-window display-buffer-use-some-window
                                                          display-buffer-pop-up-window)
                             (inhibit-same-window . t)))
    nil))

(defun +agent-history--hide-preview ()
  "Delete the preview frame and empty its reusable buffer."
  (when-let* ((buffer (get-buffer +agent-history-preview-buffer)))
    (when (fboundp 'posframe-delete-frame)
      (posframe-delete-frame buffer))
    (dolist (window (get-buffer-window-list buffer nil t))
      (unless (frame-parameter (window-frame window) 'posframe-buffer)
        (ignore-errors (quit-restore-window window 'bury))))
    (with-current-buffer buffer
      (setq +agent-history--preview-entry nil)
      (let ((inhibit-read-only t)) (erase-buffer)))))

(defconst +agent-history--preview-cache-size 24
  "Conversations whose loaded pages one picker keeps.")

(defun +agent-history--preview-state ()
  "Return a consult state function previewing the conversation at point.
A conversation's first page is read once per picker and older pages only as the
box is scrolled; at most `+agent-history--preview-cache-size' conversations are
kept.  The preview frame is deleted on exit."
  (let ((windows (current-window-configuration))
        (cache (make-hash-table :test #'equal))
        windowed)
    (lambda (action row)
      (pcase action
        ('preview
         (when row
           (let* ((key (cons (plist-get row :agent) (plist-get row :id)))
                  (pages (or (gethash key cache)
                             (progn
                               (when (>= (hash-table-count cache)
                                         +agent-history--preview-cache-size)
                                 (clrhash cache))
                               (puthash key (+agent-history--page row) cache)))))
             (unless (+agent-history--show-preview
                      (+agent-history--preview-render row pages))
               (setq windowed t)))))
        ('exit
         (clrhash cache)
         (+agent-history--hide-preview)
         ;; Only the window fallback changed the layout.
         (when windowed
           (set-window-configuration windows)))))))

(defun +agent-history-read (rows &optional prompt)
  "Read one of ROWS with PROMPT, newest first, previewing the one at point."
  (unless rows (user-error "No saved conversations here; C-u lists every folder"))
  (let ((candidates (mapcar (lambda (row) (cons (+agent-history-label row) row)) rows)))
    (if (require 'consult nil t)
        (let ((owner (not (advice-member-p #'+agent-history--switch-frame-a
                                           'handle-switch-frame))))
          (unwind-protect
              (progn
                (when owner
                  (advice-add 'handle-switch-frame :before-until
                              #'+agent-history--switch-frame-a))
                (consult--read candidates
                               :prompt (or prompt "Resume conversation: ")
                               :require-match t :sort nil
                               :lookup #'consult--lookup-cdr
                               :keymap +agent-history-picker-map
                               :preview-key '(:debounce 0.08 any)
                               :state (+agent-history--preview-state)))
            ;; The outermost picker removes it, so the advice never outlives one.
            (when owner
              (advice-remove 'handle-switch-frame #'+agent-history--switch-frame-a))))
      (cdr (assoc (completing-read (or prompt "Resume conversation: ")
                                   (lambda (string predicate action)
                                     (if (eq action 'metadata)
                                         '(metadata (display-sort-function . identity))
                                       (complete-with-action action candidates string predicate)))
                                   nil t)
                  candidates)))))

;;;; ── Opening a conversation ───────────────────────────────────────────────

(defun +agent-history--directory (row fallback)
  "Return the Emacs directory ROW ran in, or FALLBACK when it is gone."
  (let ((dir (and (plist-get row :cwd)
                  (file-name-as-directory
                   (+agent-history--emacs-name (+agent-history--target fallback)
                                               (plist-get row :cwd))))))
    (if (and dir (file-directory-p dir)) dir fallback)))

(defun +agent-history--transcript-reference (row)
  "Return a path or read-only command for ROW's native transcript."
  (cond
   ((plist-get row :file)
    (format "transcript (JSON lines): %s"
            (+agent-history--native (plist-get row :file))))
   ((equal (plist-get row :agent) "agy")
    (format
     (concat "transcript (SQLite): `sqlite3 -readonly %s %s`. "
             "step_payload is protobuf (hex): user text is in field 19, "
             "repeated field 3, field 1 (fallback: 19.2); public assistant text "
             "is in 20.1 (fallback: 20.8). Other fields are not conversation text.")
     (shell-quote-argument (+agent-history--sqlite-name (plist-get row :db) t))
     (shell-quote-argument
      "SELECT idx, step_type, hex(step_payload) FROM steps WHERE step_type IN (14,15) ORDER BY idx")))
   (t (format "transcript: `opencode session export %s` (JSON)"
              (plist-get row :id)))))

(defun +agent-history-transcript-prompt (row)
  "Return the short prompt pointing a session at ROW's transcript.
It carries a link and a one-line title, never the transcript or its first
message: the session looks up only what a later request needs."
  (noema-agent-text
   "resume-transcript"
   :agent (plist-get row :agent)
   :id (plist-get row :id)
   :cwd (or (plist-get row :cwd) "")
   :title (truncate-string-to-width
           (replace-regexp-in-string "[\n\t ]+" " " (or (plist-get row :title) ""))
           80 nil nil "…")
   :read (+agent-history--transcript-reference row)))

(defun +agent-history-continue (row agent directory)
  "Open a new AGENT session in DIRECTORY that reads ROW's transcript."
  (require 'tools/agent/context)
  (let* ((config (or (+agent-acp-config-for (intern agent))
                     (user-error "No agent configuration for %s" agent)))
         (buffer (+agent-acp-start :config config :directory directory
                                   :origin 'manual :focus t)))
    (+agent-acp-adopt buffer :agent (intern agent) :origin 'manual
                      :root (+agent-acp-project-root directory))
    (+agent-context--deliver
     buffer (+agent-context-content-blocks (+agent-history-transcript-prompt row) nil))
    (message "Continuing %s conversation %s in a new %s session"
             (plist-get row :agent) (substring (plist-get row :id) 0 8) agent)
    buffer))

(defun +agent-history--restore (row directory)
  "Restore ROW natively in a new session in DIRECTORY; its failure falls back."
  (let* ((agent (plist-get row :agent))
         (config (or (+agent-acp-config-for (intern agent))
                     (user-error "No agent configuration for %s" agent)))
         (buffer (+agent-acp-start :config config :directory directory
                                   :session-id (plist-get row :id) :strict-restore t
                                   :origin 'manual :focus t)))
    (+agent-acp-adopt buffer :agent (intern agent) :origin 'manual
                      :root (+agent-acp-project-root directory))
    buffer))

(defun +agent-history--live-buffer (id)
  "Return the live agent buffer already showing native session ID, or nil."
  (seq-find (lambda (buffer)
              (and (+agent-acp-agent-buffer-p buffer)
                   (equal id (+agent-acp-state-value buffer '(:session :id)))
                   (not (+agent-acp-client-exited-p buffer))))
            (buffer-list)))

(defun +agent-history--restore-in (source id directory)
  "Restore native session ID in agent buffer SOURCE, moved to DIRECTORY.
The restart keeps SOURCE's window, popup pool and origin; a failure falls back
to the transcript (`+agent-history-restore-failed-h').  A Noema session bound
to ID keeps its name and logical identity."
  (let* ((agent (buffer-local-value '+agent-acp-session-agent source))
         (root (buffer-local-value '+agent-acp-session-root source))
         (binding (and (fboundp 'noema-sessions-native-binding)
                       (noema-sessions-native-binding id (or root directory)))))
    (with-current-buffer source (setq-local +agent-acp-strict-restore t))
    (unless (equal directory (buffer-local-value 'default-directory source))
      (+agent-acp-move-to-saved-home source directory))
    (let ((target (+agent-acp-restart-native source id)))
      (when (and binding (buffer-live-p target) (fboundp '+agent-acp-mark-session-buffer))
        (+agent-acp-mark-session-buffer target (plist-get binding :name) agent
                                        (or root directory))
        (with-current-buffer target
          (setq-local noema-agent-promote--session-id (plist-get binding :session-id))))
      target)))

(defvar +agent-history--rows (make-hash-table :test #'equal)
  "The rows of the conversations last offered, by ID, for a failed restore.")

(defun +agent-history--agent-id (agent)
  "Normalize AGENT's upstream identity for the shared history."
  (if (equal agent "antigravity") "agy" agent))

(defun +agent-history-open (row &optional agent source)
  "Open conversation ROW with AGENT, by default its own.
Its own agent restores it natively: in SOURCE, an agent session of that
agent, or else in a new one.  Another agent continues it from its transcript."
  (let* ((agent (+agent-history--agent-id (or agent (plist-get row :agent))))
         (id (plist-get row :id))
         (directory (+agent-history--directory
                     row (if (buffer-live-p source)
                             (buffer-local-value 'default-directory source)
                           default-directory)))
         (live (+agent-history--live-buffer id)))
    (puthash id row +agent-history--rows)
    (cond
     ((and live (equal agent (plist-get row :agent)))
      (+agent-acp-show-buffer live)
      live)
     ((not (equal agent (plist-get row :agent)))
      (+agent-history-continue row agent directory))
     ((and (buffer-live-p source)
           (equal agent (+agent-history--agent-id
                         (buffer-local-value '+agent-acp-session-agent source))))
      (+agent-history--restore-in source id directory))
     (t (+agent-history--restore row directory)))))

(defun +agent-history-restore-failed-h (buffer session-id)
  "Continue SESSION-ID from its transcript after BUFFER could not restore it."
  (when-let* ((row (gethash session-id +agent-history--rows))
              (agent (buffer-local-value '+agent-acp-session-agent buffer))
              (directory (buffer-local-value 'default-directory buffer)))
    (remhash session-id +agent-history--rows)
    ;; The failed client never opened a conversation; the new session replaces it.
    (run-at-time 0 nil
                 (lambda ()
                   (let ((new (+agent-history-continue row agent directory)))
                     (when (and (buffer-live-p buffer) (not (eq buffer new)))
                       (+agent-acp-kill buffer)))))
    t))

(add-hook '+agent-acp-restore-failed-functions #'+agent-history-restore-failed-h)

;;;###autoload
;;;; ── Viewing a conversation ───────────────────────────────────────────────

;; A conversation read as a document, oldest at the top: it holds the newest
;; pages first and earlier ones only when asked, so a long session is never
;; read whole.  The buffer is an ordinary one; killing it frees everything.

(defconst +agent-history--view-chars 20000
  "Characters of one message the view keeps.")

(defconst +agent-history--view-initial 20
  "Messages the view loads when it opens.")

(defconst +agent-history--view-pages 24
  "Most pages one load of the view reads.")

(defvar-local +agent-history--view-row nil
  "The conversation this view shows.")

(defvar-local +agent-history--view-next nil
  "Where the view's earlier messages start, for `+agent-history--page'; nil at the start.")

(defvar-local +agent-history--view-top nil
  "Marker after the view's header, where earlier messages are inserted.")

(defvar +agent-history-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<") #'+agent-history/view-earlier)
    (define-key map (kbd "r") #'+agent-history/view-resume)
    (define-key map (kbd "o") #'+agent-history/view-transcript)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keys of a conversation view.")

(define-minor-mode +agent-history-view-mode
  "A saved agent conversation, read only: `<' earlier, `r' resume, `o' raw transcript."
  :lighter " History" :keymap +agent-history-view-mode-map)

(defun +agent-history--view-insert (messages)
  "Insert MESSAGES, given newest first, oldest at point first."
  (dolist (message (reverse messages))
    (insert (if (equal (car message) "user") "## You\n\n" "## Agent\n\n")
            (cdr message) "\n\n")))

(defun +agent-history--view-banner ()
  "Insert the line saying whether earlier messages remain."
  (insert (propertize (if +agent-history--view-next
                          "<!-- earlier messages: press < -->\n\n"
                        "<!-- start of conversation -->\n\n")
                      'face 'font-lock-comment-face
                      '+agent-history-banner t)))

(defun +agent-history--view-load (count)
  "Insert about COUNT earlier messages at the top; return how many came."
  (let ((+agent-history--message-chars +agent-history--view-chars)
        (inhibit-read-only t)
        messages)
    ;; A stretch of tool output yields empty pages; a bounded number of pages
    ;; per load keeps one keypress from walking a huge file.
    (let ((pages 0))
      (while (and +agent-history--view-next (< (length messages) count)
                  (< pages +agent-history--view-pages))
        (cl-incf pages)
        (pcase-let ((`(,page . ,next)
                     (+agent-history--page +agent-history--view-row
                                           (and (not (eq +agent-history--view-next t))
                                                +agent-history--view-next))))
          (setq messages (append messages page)
                +agent-history--view-next next))))
    (save-excursion
      (goto-char +agent-history--view-top)
      (let ((end (text-property-any (point) (point-max) '+agent-history-banner t)))
        (when end
          (delete-region end (or (next-single-property-change end '+agent-history-banner)
                                 (point-max)))))
      (+agent-history--view-banner)
      (+agent-history--view-insert messages))
    (length messages)))

(defun +agent-history-view (row)
  "Show ROW as a read-only Markdown buffer, its newest messages loaded."
  (let* ((name (format "*History: %s*"
                       (truncate-string-to-width
                        (replace-regexp-in-string
                         "[\n\t ]+" " " (or (plist-get row :title) (plist-get row :id)))
                        48 nil nil "…")))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (equal (plist-get +agent-history--view-row :id) (plist-get row :id))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (fboundp 'markdown-view-mode) (markdown-view-mode) (special-mode))
          (+agent-history-view-mode 1)
          (setq-local +agent-history--view-row row
                      +agent-history--view-next t
                      default-directory (or (and (plist-get row :cwd)
                                                 (file-directory-p (plist-get row :cwd))
                                                 (file-name-as-directory (plist-get row :cwd)))
                                            default-directory))
          (insert "# " (replace-regexp-in-string
                        "[\n\t ]+" " " (or (plist-get row :title) "(untitled)"))
                  "\n\n"
                  (format "%s · `%s` · %s · %s\n\n"
                          (plist-get row :agent) (plist-get row :id)
                          (+agent-history--when (plist-get row :updated))
                          (or (plist-get row :cwd) ""))
                  (if (plist-get row :file)
                      (format "Transcript: %s\n\n" (+agent-history--native (plist-get row :file)))
                    "")
                  "`<` earlier · `r` resume · `o` raw transcript · `q` close\n\n")
          (setq-local +agent-history--view-top (point-marker))
          (+agent-history--view-load +agent-history--view-initial)
          (set-buffer-modified-p nil)
          (goto-char (point-max)))))
    (pop-to-buffer buffer)
    buffer))

(defun +agent-history/view-earlier (&optional count)
  "Load COUNT (default 20) earlier messages above the ones shown."
  (interactive "P")
  (if (not +agent-history--view-next)
      (message "This is the start of the conversation")
    (let ((from-end (- (point-max) (point))))
      (message "Loaded %d earlier messages"
               (+agent-history--view-load (if count (prefix-numeric-value count)
                                            +agent-history--view-initial)))
      (goto-char (- (point-max) from-end)))))

(defun +agent-history/view-resume ()
  "Open the viewed conversation as `+agent/history' would."
  (interactive)
  (+agent-history-open +agent-history--view-row))

(defun +agent-history/view-transcript ()
  "Visit the viewed conversation's raw transcript, read only."
  (interactive)
  (if-let* ((file (plist-get +agent-history--view-row :file)))
      (let ((+programmatic-file-visit t))
        (view-file file))
    (user-error "This conversation is in a database: %s"
                (+agent-history--transcript-reference +agent-history--view-row))))

(defun +agent/history (&optional all)
  "Choose a saved conversation of Claude, Codex, OpenCode or Antigravity.
In an agent session the conversation opens with that session's agent: its own
conversations resume natively, another agent's continue from the transcript.
Elsewhere each opens with its own agent.  With a prefix argument ALL, list every
folder rather than this project's."
  (interactive "P")
  (let* ((source (and (+agent-acp-agent-buffer-p (current-buffer)) (current-buffer)))
         (directory (if source default-directory
                      (or (ignore-errors (+agent-workspace (+agent-acp-project-root)))
                          default-directory)))
         (rows (+agent-history-list directory all source))
         (rows (if source
                   (seq-remove (lambda (row)
                                 (equal (plist-get row :id)
                                        (+agent-acp-state-value source '(:session :id))))
                               rows)
                 rows))
         (rows (or rows (and (not all)
                             (progn (message "Nothing saved in this project; listing every folder")
                                    (+agent-history-list directory t source)))))
         (+agent-history--picker-action nil)
         (row (+agent-history-read rows)))
    (if (eq +agent-history--picker-action 'view)
        (+agent-history-view row)
      (+agent-history-open
       row (and source (buffer-local-value '+agent-acp-session-agent source)) source))))

(provide 'tools/agent/history)
;;; tools/agent/history.el ends here

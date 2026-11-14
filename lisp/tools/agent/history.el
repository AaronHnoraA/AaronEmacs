;;; tools/agent/history.el --- One conversation history across agents -*- lexical-binding: t; -*-

;;; Commentary:
;; Claude Code, Codex and OpenCode each keep every conversation on the machine
;; where they run.  This file reads those stores, read-only and without a copy
;; or index of its own, so one picker lists the conversations of all three
;; (C63 in docs/design/agent-management.md):
;;
;;   claude    ~/.claude/projects/<folder>/<id>.jsonl   (one file per session)
;;   codex     ~/.codex/state_5.sqlite, table threads  (index of rollout files)
;;   opencode  ~/.local/share/opencode/opencode.db     (sessions and messages)
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
(require 'tools/agent/acp)
(require 'noema-agent-text)

(declare-function +agent-shell-emacs-file-name "tools/agent/config" (logical))
(declare-function +agent-context-content-blocks "tools/agent/context" (prompt references))
(declare-function +agent-context--deliver "tools/agent/context" (buffer blocks &optional instructions no-focus))
(declare-function consult--read "consult" (table &rest options))
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
  "One conversation history across Claude Code, Codex and OpenCode."
  :group '+agent-session)

(defcustom +agent-history-limit 300
  "Most conversations listed from one agent's store."
  :type 'integer)

(defcustom +agent-history-head-bytes 65536
  "Bytes read from the start of a transcript file to describe it."
  :type 'integer)

(defcustom +agent-history-preview-bytes 262144
  "Bytes read from the end of a transcript file for its preview."
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

(defun +agent-history--sqlite (db query)
  "Return QUERY's rows on database file DB as alists, or nil.
sqlite3 runs read-only on the machine that holds DB."
  (when (file-readable-p db)
    (let ((default-directory (file-name-directory db)))
      (with-temp-buffer
        (when (eq 0 (ignore-errors
                      (process-file "sqlite3" nil '(t nil) nil
                                    "-readonly" "-json" (+agent-history--native db) query)))
          (goto-char (point-min))
          (and (re-search-forward "\\S-" nil t)
               (progn (goto-char (point-min))
                      (ignore-errors
                        (json-parse-buffer :object-type 'alist :array-type 'list
                                           :null-object nil :false-object nil)))))))))

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

;;;; ── The history ──────────────────────────────────────────────────────────

(defconst +agent-history-readers
  '(("claude" . +agent-history--claude)
    ("codex" . +agent-history--codex)
    ("opencode" . +agent-history--opencode))
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

(defun +agent-history--exchanges (row)
  "Return ROW's last messages as (ROLE . TEXT), oldest first."
  (pcase (plist-get row :agent)
    ("claude"
     (delq nil (mapcar (lambda (line)
                         (let ((type (map-elt line 'type)))
                           (when (and (member type '("user" "assistant"))
                                      (not (map-elt line 'isMeta)))
                             (let ((text (+agent-history--text
                                          (map-nested-elt line '(message content)))))
                               (when (+agent-history--prompt-p text)
                                 (cons type text))))))
                       (+agent-history--json-lines
                        (or (+agent-history--file-tail (plist-get row :file)) "")))))
    ("codex"
     ;; A message is recorded as a response item and, in newer files, again as
     ;; a completed item; keep one of each.
     (let (messages)
       (dolist (line (+agent-history--json-lines
                      (or (and (plist-get row :file)
                               (+agent-history--file-tail (plist-get row :file)))
                          "")))
         (let* ((payload (map-elt line 'payload))
                (item (map-elt payload 'item))
                (message
                 (cond
                  ((and (equal (map-elt line 'type) "response_item")
                        (equal (map-elt payload 'type) "message")
                        (member (map-elt payload 'role) '("user" "assistant")))
                   (cons (map-elt payload 'role) (+agent-history--text (map-elt payload 'content))))
                  ((and (equal (map-elt payload 'type) "item_completed")
                        (member (map-elt item 'type) '("UserMessage" "AgentMessage")))
                   (cons (if (equal (map-elt item 'type) "UserMessage") "user" "assistant")
                         (+agent-history--text (map-elt item 'content)))))))
           (when (and message (+agent-history--prompt-p (cdr message))
                      (not (member message messages)))
             (push message messages))))
       (nreverse messages)))
    ("opencode"
     (nreverse
      (mapcar (lambda (part) (cons (map-elt part 'role) (map-elt part 'text)))
              (+agent-history--sqlite
               (plist-get row :db)
               (format "SELECT json_extract(m.data, '$.role') AS role,
 json_extract(p.data, '$.text') AS text
FROM part p JOIN message m ON m.id = p.message_id
WHERE p.session_id = %s AND json_extract(p.data, '$.type') = 'text'
ORDER BY p.time_created DESC LIMIT 8"
                       (+agent-history--sql-string (plist-get row :id)))))))))

(defconst +agent-history-preview-buffer "*Agent history*")

(defun +agent-history--render-preview (row)
  "Show ROW's header and last exchanges in the preview buffer."
  (with-current-buffer (get-buffer-create +agent-history-preview-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (or (plist-get row :title) "(untitled)") 'face 'bold) "\n"
              (propertize (format "%s · %s · %s\n%s\n\n"
                                  (plist-get row :agent) (plist-get row :id)
                                  (+agent-history--when (plist-get row :updated))
                                  (or (plist-get row :cwd) ""))
                          'face 'font-lock-comment-face))
      (let ((exchanges (last (+agent-history--exchanges row) 6)))
        (when (and (null exchanges) (plist-get row :prompt))
          (setq exchanges (list (cons "user" (plist-get row :prompt)))))
        (dolist (exchange exchanges)
        (insert (propertize (if (equal (car exchange) "user") "You" "Agent")
                            'face (if (equal (car exchange) "user") 'font-lock-keyword-face 'success))
                "\n"
                (truncate-string-to-width (string-trim (cdr exchange)) 700 nil nil "…")
                "\n\n")))
      (goto-char (point-min))
      (special-mode))
    (current-buffer)))

(defun +agent-history--preview-state ()
  "Return a consult state function previewing the conversation at point."
  (let ((windows (current-window-configuration)))
    (lambda (action row)
      (pcase action
        ('preview
         (when row
           (display-buffer (+agent-history--render-preview row)
                           '((display-buffer-reuse-window display-buffer-use-some-window
                                                          display-buffer-pop-up-window)
                             (inhibit-same-window . t)))))
        ('exit
         (set-window-configuration windows)
         (when (get-buffer +agent-history-preview-buffer)
           (kill-buffer +agent-history-preview-buffer)))))))

(defun +agent-history-read (rows &optional prompt)
  "Read one of ROWS with PROMPT, newest first, previewing the one at point."
  (unless rows (user-error "No saved conversations here; C-u lists every folder"))
  (let ((candidates (mapcar (lambda (row) (cons (+agent-history-label row) row)) rows)))
    (if (require 'consult nil t)
        (consult--read candidates
                       :prompt (or prompt "Resume conversation: ")
                       :require-match t :sort nil
                       :lookup #'consult--lookup-cdr
                       :state (+agent-history--preview-state))
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

(defun +agent-history-transcript-prompt (row)
  "Return the prompt asking a session to read ROW's transcript."
  (noema-agent-text
   "resume-transcript"
   :agent (plist-get row :agent)
   :id (plist-get row :id)
   :cwd (or (plist-get row :cwd) "")
   :title (or (plist-get row :title) "")
   :read (if (plist-get row :file)
             (format "Its transcript is the JSON-lines file %s."
                     (+agent-history--native (plist-get row :file)))
           (format "Run `opencode session export %s` to read it as JSON."
                   (plist-get row :id)))))

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

(defun +agent-history-open (row &optional agent source)
  "Open conversation ROW with AGENT, by default its own.
Its own agent restores it natively: in SOURCE, an agent session of that
agent, or else in a new one.  Another agent continues it from its transcript."
  (let* ((agent (or agent (plist-get row :agent)))
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
           (equal agent (buffer-local-value '+agent-acp-session-agent source)))
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
(defun +agent/history (&optional all)
  "Choose any saved conversation of Claude Code, Codex or OpenCode and open it.
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
         (row (+agent-history-read rows)))
    (+agent-history-open
     row (and source (buffer-local-value '+agent-acp-session-agent source)) source)))

(provide 'tools/agent/history)
;;; tools/agent/history.el ends here

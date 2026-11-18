;;; tools/agent/collab.el --- A document's threads, suggestions and direct changes -*- lexical-binding: t; -*-

;;; Commentary:
;; The information flow between a document and its agents.  Only Emacs and
;; Noema change: the agents keep their own logic.
;;
;; A comment is a task (C79): a `@@comment' standing on its own in the note, or a
;; task recorded for a file that cannot hold one.  Handing it to an agent sends
;; one request; the agent changes the file through Noema's API and marks the
;; comment done.  The folder's `.noema-tasks.jsonl' records who took what.
;; The collaboration panel (`H-t', `tools/agent/panel') is where comments are
;; listed and handed over.
;;
;; A suggestion is a proposed change to a file that cannot hold an inline review
;; card; the Noema host writes it under `.agent/collab/suggestions/' (C66) and
;; the person accepts or rejects it here.
;;
;; A direct change is one an Agent applied itself with its own tools, as its
;; ACP tool call reported it.  It can be put back while it is still in place.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'tools/agent/acp)
(require 'tools/agent/binding)

(declare-function +agent-context--session "tools/agent/context" (&optional pick))
(declare-function +agent-context--deliver "tools/agent/context" (buffer blocks &optional instructions no-focus))
(declare-function +agent-context-content-blocks "tools/agent/context" (prompt references))
(declare-function +noema-api-call "noema/bridge/config" (channel args callback &optional timeout))

(defvar +agent-collab-changed-functions nil
  "Functions called with a document FILE after its threads or suggestions changed.")

;;;; Store

(defun +agent-collab-root (file)
  "Return the Project directory whose `.agent/' keeps FILE's collaboration."
  (file-name-as-directory
   (or (+agent-acp-project-root (file-name-directory (expand-file-name file)))
       (file-name-directory (expand-file-name file)))))

(defun +agent-collab--dir (root kind)
  "Return ROOT's directory for collaboration records of KIND."
  (expand-file-name (format ".agent/collab/%s/" kind) root))

(defun +agent-collab--read (file)
  "Read one JSON record FILE as an alist, or nil when it is unreadable."
  (condition-case nil
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
        (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil))
    (error nil)))

(defun +agent-collab--jsonable (value)
  "Return VALUE with its lists of objects as vectors, for `json-serialize'."
  (cond ((and (consp value) (consp (car value)) (symbolp (caar value)))
         (mapcar (lambda (pair) (cons (car pair) (+agent-collab--jsonable (cdr pair)))) value))
        ((consp value) (vconcat (mapcar #'+agent-collab--jsonable value)))
        (t value)))

(defun +agent-collab--write (root kind record)
  "Write RECORD, keyed by its id, as ROOT's collaboration record of KIND."
  (let* ((dir (+agent-collab--dir root kind))
         (target (expand-file-name (concat (alist-get 'id record) ".json") dir))
         (temporary (concat target ".tmp")))
    (make-directory dir t)
    (with-temp-file temporary
      (setq buffer-file-coding-system 'utf-8-unix)
      (json-insert (+agent-collab--jsonable record) :null-object :null :false-object :false))
    (rename-file temporary target t)
    (run-hook-with-args '+agent-collab-changed-functions (alist-get 'file record))
    record))

(defun +agent-collab--records (root kind)
  "Return every collaboration record of KIND under ROOT."
  (let ((dir (+agent-collab--dir root kind)))
    (when (file-directory-p dir)
      (delq nil (mapcar #'+agent-collab--read
                        (directory-files dir t "\\.json\\'"))))))

(defun +agent-collab--same-file-p (a b)
  "Whether A and B name the same file."
  (equal (+agent-binding-canonical a) (+agent-binding-canonical b)))

(defun +agent-collab--now ()
  "Return the current time as an ISO 8601 string."
  (format-time-string "%FT%T%z"))

(defun +agent-collab--id ()
  "Return a fresh record id."
  (format "%s-%06x" (format-time-string "%Y%m%dT%H%M%S") (random #xffffff)))

;;;; Comments are tasks (C79)
;;
;; A comment stands on its own in the note, `@@comment [text] {id: …}'
;; (`shared/comments.mjs'; this file mirrors its scanner, pinned by
;; `comment-fixtures.json').  `@@comment(done)' is a finished one.  Handing a
;; comment to an agent is a task: the folder's hidden `.noema-tasks.jsonl'
;; records who took which comment, one line per handover, and nothing else:
;; the agent changes the file through Noema's API and marks the comment done;
;; there are no replies.  A file that cannot hold a comment (code) keeps its
;; task, text included, in the same journal.  Every change to a note goes
;; through Noema's `agent:document:edit', so an open page follows; without a
;; host the file (or its buffer) is edited here.

(defconst +agent-collab-tasks-file ".noema-tasks.jsonl"
  "The hidden journal of a folder's comment tasks (C79).")

(defun +agent-collab--markdown-p (file)
  "Whether FILE is a note."
  (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file))

(defun +agent-collab--text (file)
  "Return FILE's text: its visiting buffer's, or the file's."
  (let ((file (+agent-collab--client-file file)))
    (if-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
      (when (file-readable-p file)
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
          (buffer-string))))))

(defun +agent-collab--mask (text)
  "Return TEXT with code fences, code spans and HTML comments blanked."
  (let ((chars (copy-sequence text)) (fence nil) (start 0))
    (cl-flet ((blank (from to) (cl-loop for i from from below to
                                         unless (eq (aref chars i) ?\n) do (aset chars i ?\s))))
      (dolist (line (split-string text "\n"))
        (let ((marker (and (string-match "\\`[ \t]\\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)" line) (match-string 1 line))))
          (cond ((if fence (and marker (eq (aref marker 0) (aref fence 0)) (>= (length marker) (length fence))) marker)
                 (blank start (+ start (length line)))
                 (setq fence (if fence nil marker)))
                (fence (blank start (+ start (length line))))))
        (setq start (+ start (length line) 1)))
      (let ((masked (copy-sequence chars)) (pos 0))
        (while (string-match "<!--\\(?:.\\|\n\\)*?-->\\|\\(`+\\)[^`\n]*?\\1" masked pos)
          (blank (match-beginning 0) (match-end 0))
          (setq pos (match-end 0)))))
    chars))

(defun +agent-collab--body-end (text start)
  "Return the index after the `]' closing the body opened at START, or nil."
  (let ((depth 1) (i start) (end nil))
    (while (and (not end) (< i (length text)))
      (let ((c (aref text i)))
        (cond ((eq c ?\\) (setq i (1+ i)))
              ((eq c ?\[) (setq depth (1+ depth)))
              ((eq c ?\]) (setq depth (1- depth)) (when (zerop depth) (setq end (1+ i))))
              ((eq c ?\n) (setq i (length text)))))
      (setq i (1+ i)))
    end))

(defun +agent-collab-scan (text)
  "Return the comments of TEXT, as `shared/comments.mjs' reads them.
Each is an alist: id (\"\" when none), done, text, line, from, to (0-based
characters), source and quote, the text before it on its line."
  (let ((masked (+agent-collab--mask text)) (pos 0) comments)
    (while (string-match "@@comment\\(?:(\\([^)\n]*\\))\\)?[ \t]*\\[" masked pos)
      (let* ((from (match-beginning 0))
             (switch (match-string 1 masked))
             (body-start (match-end 0))
             (body-end (+agent-collab--body-end masked body-start)))
        (if (or (not body-end) (and (> from 0) (eq (aref masked (1- from)) ?\\)))
            (setq pos (match-end 0))
          (let* ((to body-end) (attrs ""))
            (when (and (string-match "[ \t]*\\({[^{}\n]*}\\)" masked body-end) (= (match-beginning 0) body-end))
              (setq attrs (match-string 1 masked) to (match-end 0)))
            (let* ((id (and (string-match "\\(?:\\`\\|[{;,]\\)[ \t]*id[ \t]*[:=][ \t]*[\"']?\\([A-Za-z0-9]\\{4,32\\}\\)" attrs)
                            (downcase (match-string 1 attrs))))
                   (line-start (let ((nl (cl-position ?\n text :end from :from-end t))) (if nl (1+ nl) 0)))
                   (quote (string-trim (substring text line-start from))))
              (push `((id . ,(or id "")) (done . ,(equal (downcase (string-trim (or switch ""))) "done"))
                      (text . ,(replace-regexp-in-string "\\\\\\([]\\\\]\\)" "\\1" (substring text body-start (1- body-end))))
                      (line . ,(1+ (cl-count ?\n text :end from))) (from . ,from) (to . ,to)
                      (source . ,(substring text from to))
                      (quote . ,(if (> (length quote) 160) (substring quote -160) quote)))
                    comments))
            (setq pos to)))))
    (nreverse comments)))

(defun +agent-collab--journal (file)
  "Return the task journal of FILE's folder."
  (expand-file-name +agent-collab-tasks-file (file-name-directory (+agent-collab--client-file file))))

(defun +agent-collab--now-utc ()
  "Return the current time as the journal writes it."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" nil t))

(defun +agent-collab--record (file record)
  "Append RECORD about FILE to its folder's task journal and announce it."
  (let ((journal (+agent-collab--journal file)))
    (with-temp-buffer
      (setq buffer-file-coding-system 'utf-8-unix)
      (json-insert `((v . 1) ,@record (note . ,(file-name-nondirectory (+agent-collab--client-file file)))
                     (at . ,(+agent-collab--now-utc))))
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil journal t 'silent))))
  (run-hook-with-args '+agent-collab-changed-functions file))

(defun +agent-collab--tasks (file)
  "Return FILE's task records by comment id: its latest assignment and task."
  (let ((journal (+agent-collab--journal file))
        (note (file-name-nondirectory (+agent-collab--client-file file)))
        (tasks (make-hash-table :test #'equal)))
    (when (file-readable-p journal)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents journal))
        (dolist (line (split-string (buffer-string) "\n" t))
          (when-let* ((event (ignore-errors (json-parse-string line :object-type 'alist :null-object nil :false-object nil)))
                      ((equal (alist-get 'note event) note))
                      (id (alist-get 'id event)))
            (puthash id (append event (gethash id tasks)) tasks)))))
    tasks))

(defun +agent-collab-comments (file)
  "Return FILE's comment tasks, oldest first.
A note's are its comments; another file's are the tasks recorded for it.
Each carries file, and assignee (name . session) when handed over."
  (let* ((client (+agent-collab--client-file file))
         (tasks (+agent-collab--tasks client))
         (comments (if (+agent-collab--markdown-p client)
                       (+agent-collab-scan (or (+agent-collab--text client) ""))
                     (let (items)
                       (maphash (lambda (id record)
                                  (when (alist-get 'text record)
                                    (push `((id . ,id) (done . ,(eq (alist-get 'done record) t))
                                            (text . ,(alist-get 'text record)) (line . ,(alist-get 'line record))
                                            (quote . ,(or (alist-get 'quote record) "")))
                                          items)))
                                tasks)
                       (sort items (lambda (a b) (< (or (alist-get 'line a) 0) (or (alist-get 'line b) 0))))))))
    (mapcar (lambda (comment)
              (let ((record (gethash (alist-get 'id comment) tasks)))
                (append `((file . ,client)
                          (assignee . ,(and (alist-get 'session record)
                                            (cons (alist-get 'name record) (alist-get 'session record)))))
                        comment)))
            comments)))

(defun +agent-collab-comment (file id)
  "Return FILE's comment ID, or nil."
  (seq-find (lambda (comment) (equal (alist-get 'id comment) id)) (+agent-collab-comments file)))

(defun +agent-collab--new-id ()
  "Return a fresh comment id: six base36 characters."
  (let ((digits "0123456789abcdefghijklmnopqrstuvwxyz"))
    (apply #'string (cl-loop repeat 6 collect (aref digits (random 36))))))

(defun +agent-collab--head-edit (comment done id)
  "Return COMMENT's source with DONE and ID; its text is kept as written."
  (let ((next (replace-regexp-in-string "\\`@@comment\\(?:([^)\n]*)\\)?" (if done "@@comment(done)" "@@comment")
                                        (alist-get 'source comment) t t)))
    (if (not (string-empty-p (alist-get 'id comment))) next
      (if (string-match "\\(\\]\\)[ \t]*{\\([^{}\n]*\\)}[ \t]*\\'" next)
          (concat (substring next 0 (match-beginning 2)) "id: " id "; " (substring next (match-beginning 2)))
        (format "%s {id: %s}" next id)))))

(defun +agent-collab--apply-locally (file edits)
  "Apply EDITS ((OLD . NEW) ...) to FILE's buffer or file, each OLD unique."
  (let* ((client (+agent-collab--client-file file))
         (buffer (find-buffer-visiting client)))
    (cl-flet ((apply-here ()
                (dolist (edit edits)
                  (goto-char (point-min))
                  (unless (search-forward (car edit) nil t) (user-error "The comment moved; refresh"))
                  (replace-match (cdr edit) t t))))
      (if buffer
          (with-current-buffer buffer
            (let ((clean (not (buffer-modified-p))))
              (save-excursion (save-restriction (widen) (apply-here)))
              (when clean (let ((save-silently t)) (basic-save-buffer)))))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8)) (insert-file-contents client))
          (apply-here)
          (let ((coding-system-for-write 'utf-8-unix)) (write-region nil nil client nil 'silent)))))
    (run-hook-with-args '+agent-collab-changed-functions client)))

(defun +agent-collab--edit-note (file edits)
  "Change note FILE by EDITS ((OLD . NEW) ...) through Noema, an open page following.
Without a host the note is edited here."
  (let ((client (+agent-collab--client-file file)))
    (if (not (bound-and-true-p +noema--ready))
        (+agent-collab--apply-locally client edits)
      (+noema-api-call
       "noema:api:agent:document:edit"
       (vector `((file . ,client)
                 (edits . ,(vconcat (mapcar (lambda (edit) `((oldText . ,(car edit)) (newText . ,(cdr edit)))) edits)))))
       (lambda (result failure)
         (cond (failure (+agent-collab--apply-locally client edits))
               ((eq (alist-get 'ok result) :json-false)
                (message "Noema: %s" (or (alist-get 'message result) "the note could not be saved")))
               (t (run-hook-with-args '+agent-collab-changed-functions client))))))))

(defvar +noema--ready)

(defun +agent-collab--ensure-id (comment)
  "Return COMMENT's id, giving an id-less comment one in its note."
  (let ((id (alist-get 'id comment)))
    (if (not (string-empty-p (or id ""))) id
      (let ((id (+agent-collab--new-id)))
        (+agent-collab--edit-note (alist-get 'file comment)
                                  (list (cons (alist-get 'source comment) (+agent-collab--head-edit comment (alist-get 'done comment) id))))
        id))))

(defun +agent-collab-set-done (comment done)
  "Mark COMMENT done, or open again when DONE is nil."
  (let ((file (alist-get 'file comment)))
    (if (+agent-collab--markdown-p file)
        (+agent-collab--edit-note file (list (cons (alist-get 'source comment)
                                                   (+agent-collab--head-edit comment done (+agent-collab--new-id)))))
      (+agent-collab--record file `((op . "task") (id . ,(alist-get 'id comment)) (done . ,(if done t :json-false)))))))

(defun +agent-collab--escape-body (text)
  "Return TEXT for a command body: one line, `\\' and `]' escaped."
  (thread-last (string-trim text)
               (replace-regexp-in-string "[ \t]*\n[ \t]*" " ")
               (string-replace "\\" "\\\\")
               (string-replace "]" "\\]")))

(defun +agent-collab-add (file text &optional begin end)
  "Add a comment task TEXT to FILE about BEGIN..END (0-based characters), return its id.
A note gains a comment after the passage, or at its end without one;
another file keeps the task in the journal."
  (let* ((client (+agent-collab--client-file file))
         (id (+agent-collab--new-id))
         (source (or (+agent-collab--text client) ""))
         (passage (and begin end (< begin end) (<= end (length source)) (substring source begin end)))
         (line (if begin (1+ (cl-count ?\n source :end (min begin (length source)))) 1)))
    (if (+agent-collab--markdown-p client)
        (let ((comment (format "@@comment [%s] {id: %s}"
                               (+agent-collab--escape-body text)
                               id)))
          (if passage
              (+agent-collab--edit-note client (list (cons passage (concat passage " " comment))))
            (+agent-collab--edit-note client (list (cons source (concat (string-trim-right source) "\n\n" comment "\n"))))))
      (+agent-collab--record client `((op . "task") (id . ,id) (text . ,text) (line . ,line)
                                      (quote . ,(truncate-string-to-width (or passage "") 160 nil nil "…")))))
    id))

(defun +agent-collab--collaborator (buffer)
  "Return the task record naming session BUFFER."
  `((name . ,(or (buffer-local-value '+agent-acp-session-name buffer) "agent"))
    (session . ,(+agent-acp-session-token buffer))))

;;;; Handing a comment to an agent (C73, C79)

(defvar +agent-collab-session-instructions-function nil
  "Function returning (KEY . TEXT), the tools guide a session learns once.
Noema provides it: how to edit, propose and comment.")

(defconst +agent-collab-task-prompt
  "%s\n\n(Task from comment %s on %s, line %d, after «%s».  Do it by changing the file through Noema's API: agent:document:edit, or agent:document:propose when the person should judge.  When it is done, mark the comment done: agent:document:comment {\"file\",\"id\"}.  Your chat answer is not collected.)"
  "How a task reaches its agent: request, comment id, file, line, passage.")

(defconst +agent-collab-file-task-prompt
  "%s\n\n(Task %s on %s, line %d, about «%s».  Change the file through Noema's API: agent:file:edit.  Your chat answer is not collected.)"
  "How a task on a file that is not a note reaches its agent.")

(defun +agent-collab--deliver (buffer prompt)
  "Send PROMPT to session BUFFER without taking the keyboard.
The first task of a native session carries the tools guide."
  (require 'tools/agent/context)
  (+agent-context--deliver buffer (+agent-context-content-blocks prompt nil)
                           (and +agent-collab-session-instructions-function
                                (ignore-errors (funcall +agent-collab-session-instructions-function)))
                           t
                           (lambda ()
                             (message "%s refused the task: %s"
                                      (buffer-local-value '+agent-acp-session-name buffer)
                                      (or (buffer-local-value '+agent-acp-last-error buffer) "see its chat")))))

(defun +agent-collab--task-prompt (comment id)
  "Return the prompt handing COMMENT, named ID, to an agent."
  (let ((file (alist-get 'file comment)))
    (format (if (+agent-collab--markdown-p file) +agent-collab-task-prompt +agent-collab-file-task-prompt)
            (alist-get 'text comment) id file (or (alist-get 'line comment) 0)
            (truncate-string-to-width (or (alist-get 'quote comment) "") 200 nil nil "…"))))

(defun +agent-collab-dispatch (comment session)
  "Hand COMMENT to SESSION as a task: record it and send the request once."
  (let* ((id (+agent-collab--ensure-id comment))
         (file (alist-get 'file comment)))
    (+agent-collab--record file `((op . "assign") (id . ,id) ,@(+agent-collab--collaborator session)))
    (+agent-collab--deliver session (+agent-collab--task-prompt comment id))
    (message "Handed to %s" (buffer-local-value '+agent-acp-session-name session))))

(defun +agent-collab-task-delivery (file line anchor &optional begin end)
  "Return a `+agent-context--send' delivery that makes the send a task (C76).
The request becomes a comment task on FILE about BEGIN..END (0-based) at LINE
with ANCHOR, assigned to the session; the turn carries the references, the
request and the task, and the keyboard stays in the document."
  (lambda (session blocks session-instructions question)
    (let* ((text (if (string-empty-p (string-trim (or question ""))) "Look at this place" question))
           (id (+agent-collab-add file text begin end))
           (comment `((file . ,(+agent-collab--client-file file)) (id . ,id) (text . ,text)
                      (line . ,(or line 0)) (quote . ,(or anchor ""))))
           (content (copy-tree blocks)))
      (+agent-collab--record file `((op . "assign") (id . ,id) ,@(+agent-collab--collaborator session)))
      (setf (alist-get 'text (car content))
            (concat (alist-get 'text (car content)) "\n\n"
                    (string-trim (substring (+agent-collab--task-prompt comment id) (length text)))))
      (+agent-context--deliver
       session content
       (or session-instructions
           (and +agent-collab-session-instructions-function
                (ignore-errors (funcall +agent-collab-session-instructions-function))))
       t
       (lambda () (message "%s refused the task: %s" (buffer-local-value '+agent-acp-session-name session)
                           (or (buffer-local-value '+agent-acp-last-error session) "see its chat"))))
      (message "Handed to %s as a task on %s" (buffer-local-value '+agent-acp-session-name session)
               (file-name-nondirectory (+agent-collab--client-file file)))
      id)))

;;;; Commands

(defun +agent-collab--document ()
  "Return the current document file, or signal a user error.
In a collaborator's chat it is that collaborator's primary file."
  (or (+agent-document-file)
      (and (+agent-acp-agent-buffer-p (current-buffer))
           (when-let* ((file (+agent-binding-file (current-buffer))))
             (+agent-collab--client-file file)))
      (user-error "Not in a document, and this session has no primary file")))

(defvar +agent-collab-ask-functions nil
  "Functions tried, without arguments, before asking in a plain buffer.
A surface with its own comment input (a Noema page) opens it and returns non-nil.")

;;;###autoload
(defun +agent-collab/ask (text)
  "Comment TEXT on the region, or at point, of this document (C79).
A Noema page opens its own comment input instead.  The comment is a task in
the collaboration panel (`H-t'), handed to an agent from there."
  (interactive (list (unless (run-hook-with-args-until-success '+agent-collab-ask-functions)
                       (read-string "Comment: "))))
  (when text
    (let ((file (+agent-collab--document)))
      (if (use-region-p)
          (+agent-collab-add file text (1- (region-beginning)) (1- (region-end)))
        (+agent-collab-add file text (1- (line-beginning-position)) (1- (line-end-position))))
      (deactivate-mark)
      (message "Comment added; hand it to an agent from H-t"))))

(defvar-local +agent-collab--file nil
  "Document file this collaboration list shows.")
(defvar-local +agent-collab--refresh nil
  "Function refreshing this collaboration list.")

(defun +agent-collab--list-changed-h (file)
  "Refresh the collaboration lists of FILE."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and +agent-collab--refresh +agent-collab--file
                 (+agent-collab--same-file-p +agent-collab--file file))
        (funcall +agent-collab--refresh)))))

(add-hook '+agent-collab-changed-functions #'+agent-collab--list-changed-h)

(defun +agent-collab--visit-line (file line)
  "Show FILE at LINE in another window."
  (find-file-other-window (+agent-collab--client-file file))
  (goto-char (point-min))
  (forward-line (1- (max 1 (or line 1)))))

(defun +agent-collab--client-file (file)
  "Return FILE as Emacs visits it: a client path when this machine has it.
A file of another target keeps its framework name, which the /fs handler opens."
  (or (and (fboundp 'remote-client-file-name) (ignore-errors (remote-client-file-name file)))
      file))

(defun +agent-collab--live-collaborator (file)
  "Return FILE's most recent live collaborator, or nil."
  (seq-find #'+agent-session-live-p
            (mapcar (lambda (session) (plist-get session :buffer)) (+agent-binding-sessions file))))

(defun +agent-collab--comment-session (comment &optional noerror)
  "Return COMMENT's live assignee, else its document's collaborator, or signal.
With NOERROR return nil instead."
  (or (+agent-acp-session-buffer-for-token (cdr (alist-get 'assignee comment)))
      (+agent-collab--live-collaborator (alist-get 'file comment))
      (unless noerror (user-error "No live agent for this comment"))))

(defun +agent-collab--show-list (mode name file refresh)
  "Show a collaboration list in MODE called NAME for FILE, drawn by REFRESH."
  (let ((buffer (get-buffer-create (format "*%s: %s*" name (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (funcall mode)
      (setq +agent-collab--file file +agent-collab--refresh refresh)
      (funcall refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun +agent-collab/threads ()
  "Show this document's comment tasks: the collaboration panel (C79)."
  (interactive)
  (require 'tools/agent/panel)
  (+agent-panel-show-tasks (+agent-collab--document) 'file))

(declare-function +agent-panel-show-tasks "tools/agent/panel" (file scope))

;;;; Suggestions (C66)

(defun +agent-collab-suggestion (root id)
  "Return ROOT's suggestion ID, or nil."
  (+agent-collab--read (expand-file-name (concat id ".json") (+agent-collab--dir root "suggestions"))))

(defun +agent-collab-suggestions (file &optional all)
  "Return FILE's pending suggestions, oldest first; ALL includes resolved ones."
  (sort (seq-filter (lambda (record)
                      (and (+agent-collab--same-file-p (alist-get 'file record) file)
                           (or all (equal (alist-get 'status record) "pending"))))
                    (+agent-collab--records (+agent-collab-root file) "suggestions"))
        (lambda (a b) (string< (or (alist-get 'createdAt a) "") (or (alist-get 'createdAt b) "")))))

(defun +agent-collab--position (line column)
  "Move to 1-based LINE and UTF-16 COLUMN in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line))
  (let ((units 0))
    (while (and (< units column) (not (eolp)))
      (setq units (+ units (if (> (char-after) #xffff) 2 1)))
      (forward-char 1)))
  (point))

(defun +agent-collab--locate (old line column)
  "Return the start of OLD in the current buffer: at LINE/COLUMN, else where it occurs once."
  (let ((at (+agent-collab--position line column)))
    (if (and (<= (+ at (length old)) (point-max))
             (equal (buffer-substring-no-properties at (+ at (length old))) old))
        at
      (goto-char (point-min))
      (let ((first (search-forward old nil t)))
        (when (and first (not (search-forward old nil t)) (> (length old) 0))
          (- first (length old)))))))

(defun +agent-collab--edit-file (file function)
  "Call FUNCTION in a buffer holding FILE's text, then keep the result.
A buffer already visiting FILE is edited and saved, unless it has unsaved
edits of the person's, which are never saved for them; otherwise the file is
read and written directly, so no question is asked."
  (let* ((file (+agent-collab--client-file file))
         (visiting (find-buffer-visiting file)))
    (if visiting
        (with-current-buffer visiting
          (when (buffer-modified-p) (user-error "Save or revert %s first" (buffer-name)))
          (save-excursion (funcall function))
          (save-buffer))
      (with-temp-buffer
        (insert-file-contents file)
        (funcall function)
        (let ((coding-system-for-write (or buffer-file-coding-system 'utf-8-unix)))
          (write-region nil nil file nil 'silent))))))

(defun +agent-collab--set-status (root kind id status)
  "Set the STATUS of ROOT's record ID of KIND."
  (let ((record (or (+agent-collab--read (expand-file-name (concat id ".json") (+agent-collab--dir root kind)))
                    (user-error "The suggestion is gone"))))
    (setf (alist-get 'status record) status)
    (+agent-collab--write root kind record)))

(defun +agent-collab-suggestion-accept (root id)
  "Apply ROOT's suggestion ID to its file where its old text still is."
  (let* ((record (or (+agent-collab-suggestion root id) (user-error "The suggestion is gone")))
         (from (alist-get 'from record))
         (old (alist-get 'oldText record)))
    (+agent-collab--edit-file
     (alist-get 'file record)
     (lambda ()
       (let ((at (+agent-collab--locate old (alist-get 'line from) (alist-get 'column from))))
         (unless at (user-error "The suggested text is no longer where it was; reject it or ask again"))
         (goto-char at)
         (delete-region at (+ at (length old)))
         (insert (alist-get 'newText record)))))
    (+agent-collab--set-status root "suggestions" id "accepted")))

(defun +agent-collab-suggestion-reject (root id)
  "Reject ROOT's suggestion ID, leaving its file alone."
  (+agent-collab--set-status root "suggestions" id "rejected"))

(define-derived-mode +agent-collab-suggestions-mode tabulated-list-mode "Suggestions"
  "Pending suggestions of one file (C66)."
  (setq tabulated-list-format [("Line" 5 t) ("Author" 18 t) ("Title" 24 t) ("Change" 0 nil)])
  (tabulated-list-init-header))

(defun +agent-collab--suggestions-refresh ()
  "Redraw this suggestion list."
  (let ((root (+agent-collab-root +agent-collab--file)))
    (setq tabulated-list-entries
          (mapcar (lambda (record)
                    (list (cons root (alist-get 'id record))
                          (vector (number-to-string (alist-get 'line (alist-get 'from record)))
                                  (or (alist-get 'name (alist-get 'author record)) "")
                                  (alist-get 'title record)
                                  (format "%s → %s"
                                          (car (split-string (alist-get 'oldText record) "\n"))
                                          (car (split-string (alist-get 'newText record) "\n"))))))
                  (+agent-collab-suggestions +agent-collab--file)))
    (tabulated-list-print t)))

(defun +agent-collab--at-suggestion ()
  "Return (ROOT . ID) of the suggestion at point."
  (or (tabulated-list-get-id) (user-error "No suggestion here")))

(defun +agent-collab-suggestions-accept ()
  "Accept the suggestion at point."
  (interactive)
  (pcase-let ((`(,root . ,id) (+agent-collab--at-suggestion)))
    (+agent-collab-suggestion-accept root id)))

(defun +agent-collab-suggestions-reject ()
  "Reject the suggestion at point."
  (interactive)
  (pcase-let ((`(,root . ,id) (+agent-collab--at-suggestion)))
    (+agent-collab-suggestion-reject root id)))

(defun +agent-collab-suggestions-diff ()
  "Show the suggestion at point as a diff."
  (interactive)
  (pcase-let* ((`(,root . ,id) (+agent-collab--at-suggestion))
               (record (+agent-collab-suggestion root id)))
    (+agent-collab--show-diff (alist-get 'oldText record) (alist-get 'newText record)
                              (alist-get 'title record))))

(defun +agent-collab--show-diff (old new title)
  "Show OLD against NEW in a diff buffer named after TITLE."
  (let ((a (make-temp-file "agent-collab-old")) (b (make-temp-file "agent-collab-new")))
    (unwind-protect
        (progn
          (with-temp-file a (insert old "\n"))
          (with-temp-file b (insert new "\n"))
          (let ((buffer (diff-no-select a b "-u" t (get-buffer-create (format "*Change: %s*" title)))))
            (display-buffer buffer)))
      (delete-file a) (delete-file b))))

(let ((map +agent-collab-suggestions-mode-map))
  (define-key map (kbd "RET") #'+agent-collab-suggestions-diff)
  (define-key map (kbd "a") #'+agent-collab-suggestions-accept)
  (define-key map (kbd "x") #'+agent-collab-suggestions-reject))

;;;###autoload
(defun +agent-collab/suggestions ()
  "List this file's pending suggestions from its collaborators (C66)."
  (interactive)
  (+agent-collab--show-list #'+agent-collab-suggestions-mode "Suggestions" (+agent-collab--document)
                            #'+agent-collab--suggestions-refresh))

;;;; Direct changes (C68)

(defun +agent-collab-direct-changes (file)
  "Return the changes Agents applied themselves to FILE, newest first.
Each is a plist with :file :old :new :at and the session :buffer."
  (let (changes)
    (dolist (buffer (buffer-list))
      (when (+agent-acp-agent-buffer-p buffer)
        (dolist (edit (buffer-local-value '+agent-acp--direct-edits buffer))
          (when (+agent-collab--same-file-p (plist-get edit :file) file)
            (push (append (list :buffer buffer) edit) changes)))))
    (sort changes (lambda (a b) (> (plist-get a :at) (plist-get b :at))))))

(defun +agent-collab--line-column (position)
  "Return ((line . L) (column . C)) of POSITION, with a UTF-16 column."
  (save-excursion
    (goto-char position)
    `((line . ,(line-number-at-pos nil t))
      (column . ,(cl-loop for character across (buffer-substring-no-properties (line-beginning-position) position)
                          sum (if (> character #xffff) 2 1))))))

(defun +agent-collab--revert-note (file old new)
  "Put back OLD for the one NEW in note FILE through Noema, so its page follows."
  (let (edit)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)) (insert-file-contents (+agent-collab--client-file file)))
      (goto-char (point-min))
      (let ((first (search-forward new nil t)))
        (unless first (user-error "The change is no longer in the note"))
        (when (search-forward new nil t)
          (user-error "The changed text occurs more than once; revert it by hand"))
        (setq edit `((from . ,(+agent-collab--line-column (- first (length new))))
                     (to . ,(+agent-collab--line-column first))
                     (oldText . ,new) (newText . ,old)))))
    (+noema-api-call "noema:api:agent:document:edit"
                     (vector `((file . ,(+agent-collab--client-file file)) (edits . ,(vector edit))))
                     (lambda (reply failure)
                       (if (and (not failure) (eq (alist-get 'ok reply) t))
                           (message "Put back in %s" (file-name-nondirectory file))
                         (message "Could not put back the change: %s"
                                  (or failure (alist-get 'message reply) reply)))))))

(defun +agent-collab-revert-direct-change (change)
  "Put back CHANGE's old text, only where its new text occurs exactly once.
A Markdown note goes through Noema's document edit, so an open page follows."
  (let ((old (plist-get change :old)) (new (plist-get change :new)))
    (when (string-empty-p new) (user-error "This change wrote a whole new file; remove it by hand"))
    (if (and (string-match-p "\\.\\(?:md\\|markdown\\)\\'" (plist-get change :file))
             (fboundp '+noema-api-call))
        (+agent-collab--revert-note (plist-get change :file) old new)
    (+agent-collab--edit-file
     (plist-get change :file)
     (lambda ()
       (goto-char (point-min))
       (let ((first (search-forward new nil t)))
         (unless first (user-error "The change is no longer in the file"))
         (when (search-forward new nil t)
           (user-error "The changed text occurs more than once; revert it by hand"))
         (goto-char (- first (length new)))
         (delete-region (point) first)
         (insert old)))))))

(define-derived-mode +agent-collab-direct-mode tabulated-list-mode "Direct changes"
  "Changes Agents applied themselves to one file (C68)."
  (setq tabulated-list-format [("When" 9 t) ("Collaborator" 18 t) ("Lines" 7 nil) ("Change" 0 nil)])
  (tabulated-list-init-header))

(defun +agent-collab--direct-refresh ()
  "Redraw this direct-change list."
  (setq tabulated-list-entries
        (mapcar (lambda (change)
                  (list change
                        (vector (format-time-string "%H:%M:%S" (plist-get change :at))
                                (or (buffer-local-value '+agent-acp-session-name (plist-get change :buffer)) "")
                                (format "-%d +%d"
                                        (length (split-string (plist-get change :old) "\n" t))
                                        (length (split-string (plist-get change :new) "\n" t)))
                                (car (split-string (plist-get change :new) "\n")))))
                (seq-filter (lambda (change) (buffer-live-p (plist-get change :buffer)))
                            (+agent-collab-direct-changes +agent-collab--file))))
  (tabulated-list-print t))

(defun +agent-collab-direct-revert ()
  "Put back the change at point."
  (interactive)
  (let ((change (or (tabulated-list-get-id) (user-error "No change here"))))
    (when (y-or-n-p "Put back the old text of this change? ")
      (+agent-collab-revert-direct-change change)
      (message "Put back"))))

(defun +agent-collab-direct-diff ()
  "Show the change at point as a diff."
  (interactive)
  (let ((change (or (tabulated-list-get-id) (user-error "No change here"))))
    (+agent-collab--show-diff (plist-get change :old) (plist-get change :new) "direct change")))

(let ((map +agent-collab-direct-mode-map))
  (define-key map (kbd "RET") #'+agent-collab-direct-diff)
  (define-key map (kbd "u") #'+agent-collab-direct-revert))

;;;###autoload
(defun +agent-collab/direct-changes ()
  "List the changes Agents applied to this document themselves (C68)."
  (interactive)
  (+agent-collab--show-list #'+agent-collab-direct-mode "Direct changes" (+agent-collab--document)
                            #'+agent-collab--direct-refresh))

(provide 'tools/agent/collab)
;;; collab.el ends here

;;; tools/agent/collab.el --- A document's threads, suggestions and direct changes -*- lexical-binding: t; -*-

;;; Commentary:
;; The information flow between a document and its collaborators (C66-C68).
;; Only Emacs and Noema change: the Agents keep their own logic.
;;
;; A thread is a question asked at a place in a document.  It is stored beside
;; the Project under `.agent/collab/threads/', sent to the collaborator through
;; the ordinary session, and the reply of the turn it started is put back in it.
;; A collaborator takes thread turns one at a time: while it is busy the next
;; one waits here, so a reply always belongs to the thread that asked.
;;
;; A suggestion is a proposed change to a file that cannot hold an inline review
;; card; the Noema host writes it under `.agent/collab/suggestions/' (C66) and
;; the person accepts or rejects it here.
;;
;; A direct change is one an Agent applied itself with its own tools, as its
;; ACP tool call reported it.  It can be put back while it is still in place.
;;
;; The views here are plain lists; the in-document presentation is a later
;; design (the collaboration notes' section 6).

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

(defconst +agent-collab-thread-prompt
  "%s\n\n(A comment on %s line %d about «%s».  Your answer becomes the reply in that comment; a change to the document goes through agent:document:propose.)"
  "How a thread question reaches its collaborator: question, file, line, anchor.")

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

;;;; Threads (C70 store: the folder's comment journal)
;;
;; Threads are kept beside the document in its folder's hidden
;; `.noema-comments.jsonl', the same journal the Noema host writes for page
;; comments: one appended event per line, folded on read, so Emacs and the host
;; can both write without losing each other's work.

(defconst +agent-collab-comments-file ".noema-comments.jsonl"
  "Name of a folder's comment journal.")

(defun +agent-collab--journal (file)
  "Return the comment journal of FILE's folder."
  (expand-file-name +agent-collab-comments-file
                    (file-name-directory (+agent-collab--client-file file))))

(defun +agent-collab--notify-host (file)
  "Tell open Noema pages that FILE's threads changed, when Noema runs."
  (when (and (fboundp '+noema-api-call) (bound-and-true-p +noema--ready))
    (ignore-errors
      (+noema-api-call "noema:api:comments:changed"
                       (vector `((file . ,(+agent-collab--client-file file))))
                       #'ignore))))

(defun +agent-collab--append (file event)
  "Append EVENT, an alist, to FILE's comment journal and announce the change."
  (let ((journal (+agent-collab--journal file))
        (event (append '((v . 1)) event `((at . ,(+agent-collab--now-utc))))))
    (with-temp-buffer
      (json-insert (+agent-collab--jsonable event) :null-object :null :false-object :false)
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil journal 'append 'silent)))
    (run-hook-with-args '+agent-collab-changed-functions file)
    (+agent-collab--notify-host file)))

(defun +agent-collab--now-utc ()
  "Return the current UTC time as the host writes it."
  (format-time-string "%FT%T.000Z" nil t))

(defun +agent-collab--author (value)
  "Return the author alist VALUE with defaults."
  `((name . ,(or (alist-get 'name value) "You")) (kind . ,(or (alist-get 'kind value) "person"))
    ,@(when (alist-get 'session value) `((session . ,(alist-get 'session value))))))

(defun +agent-collab--fold (file)
  "Return a hash table of the threads in FILE's folder journal, by id."
  (let ((threads (make-hash-table :test #'equal))
        (journal (+agent-collab--journal file)))
    (when (file-readable-p journal)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents journal))
        (dolist (line (split-string (buffer-string) "\n" t))
          (when-let* ((event (ignore-errors
                               (json-parse-string line :object-type 'alist :array-type 'list
                                                  :null-object nil :false-object nil)))
                      (id (alist-get 'id event)))
            (let ((thread (gethash id threads)))
              (pcase (alist-get 'op event)
                ("thread"
                 (unless thread
                   (puthash id `((id . ,id) (note . ,(alist-get 'note event))
                                 (quote . ,(or (alist-get 'quote event) ""))
                                 (line . ,(alist-get 'line event))
                                 (status . "open") (assignee . ,(alist-get 'assignee event))
                                 (createdAt . ,(alist-get 'at event))
                                 (messages . (((mid . "m0") (author . ,(+agent-collab--author (alist-get 'author event)))
                                               (text . ,(or (alist-get 'text event) "")) (at . ,(alist-get 'at event))))))
                            threads)))
                ("reply"
                 (when thread
                   (setf (alist-get 'messages thread)
                         (append (alist-get 'messages thread)
                                 (list `((mid . ,(alist-get 'mid event))
                                         (author . ,(+agent-collab--author (alist-get 'author event)))
                                         (text . ,(or (alist-get 'text event) "")) (at . ,(alist-get 'at event))))))
                   (puthash id thread threads)))
                ("resolve" (when thread (setf (alist-get 'status thread) "resolved") (puthash id thread threads)))
                ("reopen" (when thread (setf (alist-get 'status thread) "open") (puthash id thread threads)))
                ("edit"
                 (when-let* ((message (seq-find (lambda (m) (equal (alist-get 'mid m) (or (alist-get 'mid event) "m0")))
                                                (alist-get 'messages thread))))
                   (setf (alist-get 'text message) (or (alist-get 'text event) ""))))
                ("delete"
                 (when thread
                   (if (member (alist-get 'mid event) '(nil "m0"))
                       (remhash id threads)
                     (setf (alist-get 'messages thread)
                           (seq-remove (lambda (m) (equal (alist-get 'mid m) (alist-get 'mid event)))
                                       (alist-get 'messages thread)))
                     (puthash id thread threads))))
                ("assign" (when thread (setf (alist-get 'assignee thread) (alist-get 'assignee event))
                                (puthash id thread threads)))
                ("move" (when thread (setf (alist-get 'note thread) (alist-get 'note event))
                              (puthash id thread threads)))))))))
    threads))

(defun +agent-collab--with-file (thread file)
  "Return THREAD with its document FILE and display status."
  (let* ((last (car (last (alist-get 'messages thread))))
         (status (cond ((equal (alist-get 'status thread) "resolved") "resolved")
                       ((equal (alist-get 'kind (alist-get 'author last)) "agent")
                        (if (string-prefix-p "(" (alist-get 'text last)) "stopped" "replied"))
                       ((alist-get 'assignee thread) "waiting")
                       (t "open"))))
    (append `((file . ,(+agent-collab--client-file file)) (state . ,status)) thread)))

(defun +agent-collab-thread (file id)
  "Return FILE's thread ID, or nil."
  (when-let* ((thread (gethash id (+agent-collab--fold file))))
    (+agent-collab--with-file thread file)))

(defun +agent-collab-threads (file)
  "Return FILE's threads, newest first."
  (let ((note (file-name-nondirectory (+agent-collab--client-file file))) threads)
    (maphash (lambda (_id thread)
               (when (equal (alist-get 'note thread) note)
                 (push (+agent-collab--with-file thread file) threads)))
             (+agent-collab--fold file))
    (sort threads (lambda (a b) (string> (or (alist-get 'createdAt a) "") (or (alist-get 'createdAt b) ""))))))

(defun +agent-collab--collaborator (buffer)
  "Describe session BUFFER as a thread's assignee."
  `((name . ,(or (buffer-local-value '+agent-acp-session-name buffer) (buffer-name buffer)))
    (session . ,(+agent-acp-session-token buffer))))

(defun +agent-collab--comment-id ()
  "Return a fresh comment id, of the page's form."
  (format "c-%s-%06x" (format-time-string "%Y%m%d%H%M%S" nil t) (random #xffffff)))

(cl-defun +agent-collab-thread-create (&key file line anchor session text id)
  "Store a new thread on FILE at LINE about ANCHOR, asked of SESSION with TEXT."
  (let ((id (or id (+agent-collab--comment-id))))
    (+agent-collab--append
     file `((op . "thread") (id . ,id) (note . ,(file-name-nondirectory (+agent-collab--client-file file)))
            (quote . ,(or anchor "")) (line . ,line)
            (author . ((name . "You") (kind . "person"))) (text . ,text)
            ,@(when session `((assignee . ,(+agent-collab--collaborator session))))))
    (+agent-collab-thread file id)))

(defun +agent-collab--assign (file id session)
  "Make SESSION the assignee of FILE's thread ID."
  (+agent-collab--append file `((op . "assign") (id . ,id) (assignee . ,(+agent-collab--collaborator session)))))

(defun +agent-collab--add-message (file id session text)
  "Append TEXT to FILE's thread ID, from SESSION's agent, or from you when nil."
  (+agent-collab--append
   file `((op . "reply") (id . ,id) (mid . ,(format "m-%08x" (random #xffffffff)))
          (author . ,(if session
                         `((name . ,(or (buffer-local-value '+agent-acp-session-name session) "agent"))
                           (kind . "agent") (session . ,(+agent-acp-session-token session)))
                       '((name . "You") (kind . "person"))))
          (text . ,(or text "")))))

(defun +agent-collab--set-resolved (file id resolved)
  "Resolve FILE's thread ID, or reopen it when RESOLVED is nil."
  (+agent-collab--append file `((op . ,(if resolved "resolve" "reopen")) (id . ,id)
                                (author . ((name . "You") (kind . "person"))))))

;;;; Turns

(defvar-local +agent-collab--current nil
  "(FILE . ID) of the thread whose turn this collaborator is taking.")
(defvar-local +agent-collab--waiting nil
  "Thread turns waiting for this collaborator, oldest first, as (FILE ID SEND).
SEND is a function that delivers the turn.")

(defun +agent-collab--idle-p (buffer)
  "Whether session BUFFER can take a new turn now."
  (and (eq (+agent-acp-state-value buffer '(:init-finished)) t)
       (not (+agent-acp-busy-p buffer))
       (zerop (+agent-acp-pending-prompt-count buffer))))

(defun +agent-collab--deliver (buffer prompt)
  "Send PROMPT to session BUFFER without taking the keyboard."
  (require 'tools/agent/context)
  (+agent-context--deliver buffer (+agent-context-content-blocks prompt nil) nil t
                           (lambda () (+agent-collab--turn-failed buffer))))

(defun +agent-collab--turn-failed (buffer)
  "Record that BUFFER's thread turn could not run, and go on to the next.
Without this a refused prompt would hold the thread and every one behind it."
  (when (buffer-live-p buffer)
    (let ((current (buffer-local-value '+agent-collab--current buffer))
          (reason (buffer-local-value '+agent-acp-last-error buffer)))
      (when current
        (with-current-buffer buffer
          (setq +agent-collab--current nil))
        (+agent-collab--defer
         (lambda ()
           (+agent-collab--add-message (car current) (cdr current) buffer
                                       (format "(The request failed%s.)"
                                               (if reason (format ": %s" reason) "")))
           (when (buffer-live-p buffer) (+agent-collab--pump buffer))))))))

(defun +agent-collab--last-reply (buffer)
  "Return the reply of BUFFER's thread turn, from the ACP stream, or nil."
  (+agent-acp-turn-reply buffer))

(defun +agent-collab--defer (function)
  "Run FUNCTION outside the ACP event callback."
  (run-at-time 0 nil function))

(defun +agent-collab--pump (buffer)
  "Start BUFFER's next waiting thread turn when it is free."
  (with-current-buffer buffer
    (when (and (not +agent-collab--current) +agent-collab--waiting (+agent-collab--idle-p buffer))
      (pcase-let ((`(,file ,id ,send) (pop +agent-collab--waiting)))
        (setq +agent-collab--current (cons file id))
        (funcall send)))))

(defun +agent-collab--submit (buffer file id send)
  "Queue FILE's thread ID for collaborator BUFFER; SEND delivers its turn."
  (with-current-buffer buffer
    (setq +agent-collab--waiting (append +agent-collab--waiting (list (list file id send)))))
  (+agent-collab--pump buffer))

(defun +agent-collab--turn-complete-h (buffer _event)
  "Put the reply of BUFFER's finished thread turn in its thread, then go on."
  (when (buffer-live-p buffer)
    (let ((current (buffer-local-value '+agent-collab--current buffer)))
      (with-current-buffer buffer (setq +agent-collab--current nil))
      (if (not current)
          (+agent-collab--pump buffer)
        (let ((ended (equal (buffer-local-value '+agent-acp--stop-reason buffer) "end_turn")))
          (+agent-collab--defer
           (lambda ()
             (when (buffer-live-p buffer)
               (let ((reply (+agent-collab--last-reply buffer)))
                 ;; Only a finished turn with words in it answered the thread.
                 (+agent-collab--add-message
                  (car current) (cdr current) buffer
                  (if (and ended reply) reply
                    (or reply (format "(No reply%s.)"
                                      (if-let* ((reason (buffer-local-value '+agent-acp-last-error buffer)))
                                          (format ": %s" reason) ""))))))
               (+agent-collab--pump buffer)))))))))

(add-hook '+agent-acp-turn-complete-functions #'+agent-collab--turn-complete-h)

(cl-defun +agent-collab-thread-deliver (&key file line anchor session text send)
  "Open a thread on FILE at LINE about ANCHOR, asked of SESSION with TEXT.
SEND delivers the turn when SESSION is free; its reply comes back to the thread."
  (let ((thread (+agent-collab-thread-create :file file :line line :anchor anchor
                                             :session session :text text)))
    (+agent-collab--submit session file (alist-get 'id thread) send)
    thread))

(cl-defun +agent-collab-ask-region (&key file line anchor session text)
  "Ask SESSION TEXT about ANCHOR at LINE of FILE in a new thread, and return it."
  (let ((prompt (format +agent-collab-thread-prompt text (expand-file-name file) line
                        (truncate-string-to-width (or anchor "") 200 nil nil "…"))))
    (+agent-collab-thread-deliver :file file :line line :anchor anchor :session session :text text
                                  :send (lambda () (+agent-collab--deliver session prompt)))))

;;;; Commands

(defun +agent-collab--document ()
  "Return the current document file, or signal a user error.
In a collaborator's chat it is that collaborator's primary file."
  (or (+agent-document-file)
      (and (+agent-acp-agent-buffer-p (current-buffer))
           (when-let* ((file (+agent-binding-file (current-buffer))))
             (+agent-collab--client-file file)))
      (user-error "Not in a document, and this session has no primary file")))

(defun +agent-collab--anchor ()
  "Return (LINE . TEXT) of the region or the current line."
  (if (use-region-p)
      (cons (line-number-at-pos (region-beginning) t)
            (buffer-substring-no-properties (region-beginning) (region-end)))
    (cons (line-number-at-pos nil t)
          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defvar +agent-collab-ask-functions nil
  "Functions tried, without arguments, before asking in a plain buffer.
A surface with its own comment box (a Noema page) opens it and returns non-nil.")

;;;###autoload
(defun +agent-collab/ask (text)
  "Ask this document's collaborator TEXT about the region or the current line.
The question becomes a thread of the document; the reply comes back into it.
A Noema page opens its own comment box instead."
  (interactive (list (unless (run-hook-with-args-until-success '+agent-collab-ask-functions)
                       (read-string "Ask about this place: "))))
  (when text
    (+agent-collab--ask text)))

(defun +agent-collab--ask (text)
  "Ask this document's collaborator TEXT about the region or the current line."
  (let* ((file (+agent-collab--document))
         (anchor (+agent-collab--anchor))
         (session (progn (require 'tools/agent/context) (+agent-context--session))))
    (unless (bufferp session) (user-error "A thread needs a live session"))
    (+agent-collab-ask-region :file file :line (car anchor) :anchor (cdr anchor)
                              :session session :text text)
    (message "Asked %s; the reply lands in this document's threads"
             (buffer-local-value '+agent-acp-session-name session))))

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

(defun +agent-collab--assignee-name (thread)
  "Return the name of THREAD's assignee, or an empty string."
  (or (alist-get 'name (alist-get 'assignee thread)) ""))

(defun +agent-collab--last-text (thread)
  "Return the first line of THREAD's last message."
  (car (split-string (or (alist-get 'text (car (last (alist-get 'messages thread)))) "") "\n")))

(define-derived-mode +agent-collab-threads-mode tabulated-list-mode "Threads"
  "Threads of one document (C67)."
  (setq tabulated-list-format [("Status" 9 t) ("Collaborator" 18 t) ("Line" 5 t) ("Last message" 0 nil)])
  (tabulated-list-init-header))

(defun +agent-collab--threads-refresh ()
  "Redraw this thread list."
  (let ((file +agent-collab--file))
    (setq tabulated-list-entries
          (mapcar (lambda (thread)
                    (list (cons file (alist-get 'id thread))
                          (vector (alist-get 'state thread)
                                  (+agent-collab--assignee-name thread)
                                  (number-to-string (or (+agent-collab--thread-line thread) 0))
                                  (+agent-collab--last-text thread))))
                  (+agent-collab-threads file)))
    (tabulated-list-print t)))

(defun +agent-collab--thread-line (thread)
  "Return THREAD's line: stored, or where its anchor is in the document."
  (or (alist-get 'line thread)
      (let ((file (alist-get 'file thread)) (marker (format "{>>#%s<<}" (alist-get 'id thread))))
        (when (file-readable-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (when (search-forward marker nil t) (line-number-at-pos)))))))

(defun +agent-collab--at-thread ()
  "Return the thread at point in a thread list."
  (pcase-let ((`(,file . ,id) (or (tabulated-list-get-id) (user-error "No thread here"))))
    (or (+agent-collab-thread file id) (user-error "The thread is gone"))))

(defun +agent-collab-threads-visit ()
  "Show the place this thread was asked about."
  (interactive)
  (let ((thread (+agent-collab--at-thread)))
    (+agent-collab--visit-line (alist-get 'file thread) (+agent-collab--thread-line thread))))

(defun +agent-collab-threads-read ()
  "Show this thread's whole conversation."
  (interactive)
  (let ((thread (+agent-collab--at-thread)))
    (with-current-buffer (get-buffer-create "*Agent thread*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s line %s — %s\n\n> %s\n\n" (alist-get 'file thread)
                        (+agent-collab--thread-line thread) (alist-get 'state thread)
                        (alist-get 'quote thread)))
        (dolist (message (alist-get 'messages thread))
          (insert (format "## %s\n\n%s\n\n" (alist-get 'name (alist-get 'author message))
                          (alist-get 'text message))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun +agent-collab--live-collaborator (file)
  "Return FILE's most recent live collaborator, or nil."
  (seq-find #'+agent-session-live-p
            (mapcar (lambda (session) (plist-get session :buffer)) (+agent-binding-sessions file))))

(defun +agent-collab--thread-session (thread &optional noerror)
  "Return THREAD's live assignee, else its document's collaborator, or signal.
With NOERROR return nil instead."
  (or (+agent-acp-session-buffer-for-token (alist-get 'session (alist-get 'assignee thread)))
      (+agent-collab--live-collaborator (alist-get 'file thread))
      (unless noerror (user-error "No live collaborator for this thread"))))

(defun +agent-collab--thread-prompt (thread text)
  "Return the prompt asking THREAD's question TEXT."
  (format +agent-collab-thread-prompt text (alist-get 'file thread)
          (or (+agent-collab--thread-line thread) 0)
          (truncate-string-to-width (or (alist-get 'quote thread) "") 200 nil nil "…")))

(defun +agent-collab-ask-thread (file id)
  "Ask FILE's thread ID's latest question of its collaborator.
A page comment arrives here from the Noema host (C70).  Without a live
collaborator the comment simply stays for the person."
  (when-let* ((thread (+agent-collab-thread file id)))
    (if-let* ((session (+agent-collab--thread-session thread t)))
        (progn
          (unless (equal (alist-get 'session (alist-get 'assignee thread))
                         (+agent-acp-session-token session))
            (+agent-collab--assign file id session))
          (let ((prompt (+agent-collab--thread-prompt
                         thread (alist-get 'text (car (last (alist-get 'messages thread)))))))
            (+agent-collab--submit session file id (lambda () (+agent-collab--deliver session prompt)))))
      (message "No collaborator works on %s; the comment stays for you"
               (file-name-nondirectory file)))))

(defun +agent-collab-threads-reply (text)
  "Continue this thread with TEXT, asked of the same collaborator."
  (interactive (list (read-string "Reply: ")))
  (let* ((thread (+agent-collab--at-thread))
         (file (alist-get 'file thread)))
    (+agent-collab--add-message file (alist-get 'id thread) nil text)
    (+agent-collab-ask-thread file (alist-get 'id thread))))

(defun +agent-collab-threads-resolve ()
  "Mark this thread resolved."
  (interactive)
  (let ((thread (+agent-collab--at-thread)))
    (+agent-collab--set-resolved (alist-get 'file thread) (alist-get 'id thread) t)))

(defun +agent-collab-threads-chat ()
  "Open this thread's collaborator chat for a longer discussion."
  (interactive)
  (+agent-acp-show-buffer (+agent-collab--thread-session (+agent-collab--at-thread))))

(let ((map +agent-collab-threads-mode-map))
  (define-key map (kbd "RET") #'+agent-collab-threads-visit)
  (define-key map (kbd "v") #'+agent-collab-threads-read)
  (define-key map (kbd "r") #'+agent-collab-threads-reply)
  (define-key map (kbd "c") #'+agent-collab-threads-resolve)
  (define-key map (kbd "o") #'+agent-collab-threads-chat))

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
  "List this document's threads (C67)."
  (interactive)
  (+agent-collab--show-list #'+agent-collab-threads-mode "Threads" (+agent-collab--document)
                            #'+agent-collab--threads-refresh))

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

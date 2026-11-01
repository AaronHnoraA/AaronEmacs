;;; tools/agent/context.el --- Hand buffer context to an agent session -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs tracks selected files and regions; requests send path references and
;; ACP resource links. Selection is independent of model clients. ACP and
;; provider details remain behind tools/agent/acp.

;;; Code:

(require 'cl-lib)
(defvar +agent-editor-action-functions nil
  "Functions receiving OPERATION and arguments; non-nil means a domain editor handled it.")
(defun +agent-editor-action (operation &rest arguments)
  "Offer OPERATION to domain editors without loading one implicitly."
  (apply #'run-hook-with-args-until-success '+agent-editor-action-functions operation arguments))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/acp)
(require 'tools/agent/sessions)

(require 'tools/agent/worktree)

(require 'tools/agent/selection)
(declare-function which-function "which-func" ())

(defun +agent-context-state (&optional window)
  "Query WINDOW's editor source as references, without saving or copying text.
Agent views use their own frame's origin.  Page-owned Markdown buffers expose
only their known file identity, never the placeholder's point or contents."
  (let* ((window (or window (selected-window)))
         (origin (+agent-view-origin-window window))
         (record (window-parameter window '+agent-view))
         (previous (and (eq (window-buffer window) (plist-get record :buffer))
                        (plist-get record :previous)))
         (buffer (if origin (window-buffer origin)
                   (if (buffer-live-p (car previous)) (car previous)
                     (window-buffer window))))
         (position (cond (origin (window-point origin))
                         (previous (cadr previous))
                         (t (window-point window)))))
    (with-current-buffer buffer
      (let* ((source-available (not (+agent-acp-agent-buffer-p buffer)))
             (page (and (fboundp 'noema-md-bridge-pane-p)
                        (noema-md-bridge-pane-p buffer)))
             (file (and source-available
                        (if page (bound-and-true-p +noema-buffer-file-name) buffer-file-name)))
             (directory (and source-available default-directory))
             (root (and directory (+agent-acp-project-root directory))))
        (append
         `((buffer . ,(buffer-name buffer)) (mode . ,(symbol-name major-mode))
           (file . ,file) (directory . ,directory) (root . ,root)
           (sourceAvailable . ,(if source-available t :json-false))
           (target . ,(when (or file directory) (remote-file-name-target (or file directory))))
           (logicalFile . ,(when file (remote-canonicalize-emacs-file-name file)))
           (activeProject . ,(bound-and-true-p +project-active-root))
           (pageOwned . ,(if page t :json-false)))
         (when (and source-available (not page))
           (save-restriction
             (widen)
             `((modified . ,(if (buffer-modified-p) t :json-false))
               (line . ,(line-number-at-pos position t))
               (selection . ,(when (and origin (use-region-p))
                               `((startLine . ,(line-number-at-pos (region-beginning) t))
                                 (endLine . ,(+agent-context--end-line buffer (region-end))))))))))))))

(defgroup +agent-context nil
  "Handing editor context to an agent session by reference."
  :group '+agent-session)

(defcustom +agent-context-save-before-send 'ask
  "What to do with unsaved changes in a buffer that is being referenced.
A reference points the agent at a file on disk, so unsaved changes would send
it to stale text.  `ask' offers to save, t saves without asking, and nil
skips the entry and says so."
  :type '(choice (const :tag "Ask to save" ask)
                 (const :tag "Save without asking" t)
                 (const :tag "Skip unsaved buffers" nil))
  :group '+agent-context)

(defcustom +agent-context-always-ask-session t
  "Whether every send asks which agent session receives it.
When non-nil the prompt always appears, with the session this project last
used listed first and preselected, so RET repeats the previous choice.  When
nil the project's last session is reused silently and only a prefix argument
asks."
  :type 'boolean
  :group '+agent-context)

(defcustom +agent-context-clear-after-send nil
  "Whether sending clears the shared editor selection.
Off by default: the same selection usually serves several questions, and
`+agent-selection-clear' clears it on purpose."
  :type 'boolean
  :group '+agent-context)


;;;; ── Turning the shared selection into references ─────────────────────────

(defun +agent-context--agent-file (file session)
  "Return FILE as the path SESSION's agent opens, or signal.
SESSION nil means an agent on this machine.  A file on a machine that agent
cannot reach -- a local file for an agent on a server, or the reverse -- is
an error rather than a path the agent would silently fail to read."
  (or (+agent-acp-agent-file file session)
      (user-error "Context is not reachable by the agent: %s" file)))

(defun +agent-context--relative (file root)
  "Return FILE relative to ROOT when it is inside it, else its absolute path.
This is the rule `agent-shell--get-region-context' applies to its own
references, so a Noema reference reads exactly like a native one."
  (let* ((root (and root (file-name-as-directory (expand-file-name root))))
         (relative (and root
                        (file-in-directory-p file root)
                        (file-relative-name file root))))
    ;; `file-in-directory-p' resolves symlinks and `file-relative-name' does
    ;; not, so a root reached through a link (/tmp on macOS, a linked
    ;; worktree) otherwise yields a relative name climbing out of the project.
    ;; Compare both in resolved terms before giving up on a readable name.
    (when (and root (or (null relative) (string-prefix-p "../" relative)))
      (let ((true-root (file-name-as-directory (file-truename root)))
            (true-file (file-truename file)))
        (setq relative (and (file-in-directory-p true-file true-root)
                            (file-relative-name true-file true-root)))))
    (or relative file)))

(defun +agent-context--buffer-file (buffer)
  "Return the file BUFFER references, or nil when it cannot be referenced.
Honours `+agent-context-save-before-send' for unsaved changes."
  (with-current-buffer buffer
    (when buffer-file-name
      (when (and (buffer-modified-p)
                 (pcase +agent-context-save-before-send
                   ('ask (y-or-n-p (format "Save %s before referencing it? "
                                           (buffer-name))))
                   ('nil nil)
                   (_ t)))
        (save-buffer))
      (and (not (buffer-modified-p))
           (file-readable-p buffer-file-name)
           buffer-file-name))))

(defun +agent-context--end-line (buffer end)
  "Return the last line of BUFFER a region ending at END actually covers.
A region that stops at the beginning of a line does not include that line."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (if (and (> end (point-min))
                 (save-excursion (goto-char end) (bolp)))
            (line-number-at-pos (1- end) t)
          (line-number-at-pos end t))))))

(defun +agent-context--whole-buffer-p (buffer regions)
  "Return non-nil when REGIONS already cover all of BUFFER."
  (and (= (length regions) 1)
       (with-current-buffer buffer
         (save-restriction
           (widen)
           (and (<= (car (car regions)) (point-min))
                (>= (cdr (car regions)) (point-max)))))))

(defun +agent-context--reference (file root &optional buffer region session)
  "Return the reference plist for FILE, limited to REGION of BUFFER when given.
:file is FILE's Emacs name, :agent-file the path SESSION's agent opens, and
:relative is relative to ROOT when FILE is inside it, else :agent-file.
A file of the main checkout sent to a session in a linked worktree names the
worktree's copy, as `+agent-worktree-redirect' explains."
  (let* ((file (expand-file-name file))
         (agent-file (+agent-context--agent-file file session))
         (redirect (and session (+agent-worktree-redirect file session)))
         (agent-file (if redirect (car redirect) agent-file))
         (relative (if redirect (cdr redirect) (+agent-context--relative file root))))
    (let ((first (and region (with-current-buffer buffer
                               (line-number-at-pos (car region) t))))
          (last (and region (+agent-context--end-line buffer (cdr region)))))
      (when (and redirect region)
        (+agent-worktree-check-region buffer first last (cdr redirect) session))
      (append (list :file file
                    :agent-file agent-file
                    :relative (if (equal relative file) agent-file relative)
                    :kind (if region 'region 'file))
              (when region
                (list :line-start first :line-end last))))))

(defun +agent-context--resolve (&optional context root session)
  "Return (REFERENCES . SKIPPED) for CONTEXT, relative to session ROOT.
Paths are the ones SESSION's agent opens; nil means an agent on this machine.
CONTEXT defaults to the shared editor selection.  SKIPPED describes entries
that cannot be referenced -- a buffer with no file, or one left unsaved --
because a reference to text that is not on disk would mislead the agent."
  (let (references skipped)
    (dolist (entry (+agent-selection-collect (or context +agent-selection)))
      (let ((source (car entry))
            (spec (cdr entry)))
        (cond
         ((bufferp source)
          (if-let* ((file (+agent-context--buffer-file source)))
              (let ((regions (+agent-selection-regions source spec)))
                (if (or (null regions)
                        (+agent-context--whole-buffer-p source regions))
                    (push (+agent-context--reference file root nil nil session)
                          references)
                  (dolist (region regions)
                    (push (+agent-context--reference file root source region session)
                          references))))
            (push (format "%s (%s)" (buffer-name source)
                          (if (buffer-local-value 'buffer-file-name source)
                              "unsaved changes"
                            "not visiting a file"))
                  skipped)))
         ((stringp source)
          (if (file-readable-p source)
              (push (+agent-context--reference source root nil nil session)
                    references)
            (push (format "%s (unreadable)" source) skipped))))))
    (cons (nreverse references) (nreverse skipped))))

(defun +agent-context-references (&optional context root session)
  "Return the reference plists for CONTEXT relative to session ROOT.
Each has :file, :agent-file, :relative and :kind, plus :line-start and
:line-end for a region.  SESSION is the agent buffer the paths are for; nil
means an agent on this machine.  See `+agent-context--resolve' for what
cannot be referenced."
  (car (+agent-context--resolve context root session)))


;;;; ── Reference blocks: a link and a line, never a copy ────────────────────

(defun +agent-context--reference-line (reference)
  "Return REFERENCE as one line of prompt text.
The `path:START-END' form is the one agent-shell writes for its own regions."
  (if (eq (plist-get reference :kind) 'region)
      (format "%s:%d-%d" (plist-get reference :relative)
              (plist-get reference :line-start)
              (plist-get reference :line-end))
    (plist-get reference :relative)))

(defun +agent-context--file-blocks (references)
  "Return one `resource_link' content block per distinct file in REFERENCES.
This is the block agent-shell sends for a file it does not embed: the agent
receives a location it can open rather than a copy of the text.  ACP has no
capability gate on resource links, so every agent understands them."
  (let ((seen (make-hash-table :test #'equal))
        blocks)
    (dolist (reference references (nreverse blocks))
      (let ((file (plist-get reference :file)))
        (unless (gethash file seen)
          (puthash file t seen)
          (let ((meta (+agent-acp-file-metadata file)))
            (push (delq nil
                        (list (cons 'type "resource_link")
                              (cons 'uri (concat "file://"
                                                 (plist-get reference :agent-file)))
                              (cons 'name (plist-get reference :relative))
                              (when-let* ((mime (plist-get meta :mime-type)))
                                (cons 'mimeType mime))
                              (when-let* ((size (plist-get meta :size)))
                                (cons 'size size))))
                  blocks)))))))

(defun +agent-context--prompt-text (prompt references)
  "Return PROMPT with REFERENCES listed after it, as plain text."
  (if (null references)
      prompt
    (concat prompt
            "\n\nContext (open these yourself; they are not included inline):\n"
            (mapconcat (lambda (reference)
                         (concat "- " (+agent-context--reference-line reference)))
                       references "\n")
            "\n")))

(defun +agent-context-content-blocks (prompt references)
  "Return the ACP content blocks that send PROMPT with REFERENCES attached.
One text block names every reference, then one `resource_link' per file.  No
file content is ever put in the request."
  (cons (list (cons 'type "text")
              (cons 'text (+agent-context--prompt-text prompt references)))
        (+agent-context--file-blocks references)))


;;;; ── Choosing the session and delivering the turn ─────────────────────────

(defvar +agent-context--last-session
  (make-hash-table :test #'equal :weakness 'value)
  "Project root to the agent buffer its editor context last went to.")

(defun +agent-context--session (&optional pick)
  "Return the agent buffer editor context goes to, or `clipboard'.
With `+agent-context-always-ask-session' (the default) every send asks, and the
session this project used last is preselected.  Otherwise that live session is
reused until PICK.  Only running ACP sessions are offered; this command does
not resume a recorded conversation or start a new agent.  The clipboard choice
is always last and does not replace the remembered session."
  (let* ((root (+agent-acp-project-root))
         (remembered (gethash root +agent-context--last-session))
         (live (and (buffer-live-p remembered)
                    (+agent-session-live-p remembered)
                    (equal (buffer-local-value '+agent-acp-session-root remembered)
                           root)
                    remembered))
         (buffer (if (and live (not pick) (not +agent-context-always-ask-session))
                     live
                   (+agent-session-read
                    :prompt "Send context to session: "
                    :root root :live-only t :default live
                    :copy-to-clipboard t))))
    (unless (eq buffer 'clipboard)
      (puthash root buffer +agent-context--last-session))
    buffer))

(defvar +agent-context--session-instructions (make-hash-table :test #'equal)
  "Bounded record of operation guidance delivered to native ACP sessions.")

(defun +agent-context--instruction-key (buffer instructions)
  "Identify INSTRUCTIONS by native session, independently of buffer names."
  (when-let* ((id (+agent-acp-state-value buffer '(:session :id))))
    (list (buffer-local-value '+agent-acp-session-root buffer)
          (buffer-local-value '+agent-acp-session-agent buffer)
          id (car instructions))))

(defun +agent-context--with-instructions (blocks instructions)
  "Copy BLOCKS and prepend the short INSTRUCTIONS entry to its first text."
  (let ((copy (copy-tree blocks)))
    (setf (alist-get 'text (car copy))
          (concat (cdr instructions) "\n\n" (map-elt (car copy) 'text)))
    copy))

(defun +agent-context--deliver (buffer blocks &optional instructions no-focus)
  "Send BLOCKS when BUFFER can take a turn; NO-FOCUS preserves the editor."
  (cond
   ;; A session started for this send is still shaking hands with its agent.
   ((not (+agent-acp-state-value buffer '(:session :id)))
    (let (subscription)
      (setq subscription
            (+agent-acp-subscribe
             :buffer buffer :event 'init-finished
             :callback (lambda (_event)
                         (ignore-errors
                           (+agent-acp-unsubscribe
                            :buffer buffer :subscription subscription))
                         (when (buffer-live-p buffer)
                           (+agent-context--deliver buffer blocks instructions no-focus))))))
    (message "Agent: the session is starting; your context follows"))
   ;; Mid-turn: agent-shell's own queue sends it when the turn finishes.  A
   ;; queued prompt is plain text, so the references travel as the same
   ;; `path:START-END' lines without the resource links.
   (t
    (let* ((key (and instructions
                     (+agent-context--instruction-key buffer instructions)))
           (first (and key (not (gethash key +agent-context--session-instructions))))
           (content (if first (+agent-context--with-instructions blocks instructions)
                      blocks))
           (accepted t))
      (if (or (+agent-acp-busy-p buffer)
              (> (+agent-acp-pending-prompt-count buffer) 0))
          (progn
            (+agent-acp-enqueue buffer (map-elt (car content) 'text))
            (message "Agent: the session is busy; your context is queued"))
        (+agent-acp-prompt
         :buffer buffer :content content :process-queue t
         :on-failure (lambda (&rest _)
                       (setq accepted nil)
                       (when first (remhash key +agent-context--session-instructions)))))
      (when (and first accepted)
        (when (>= (hash-table-count +agent-context--session-instructions) 256)
          (remhash (car (hash-table-keys +agent-context--session-instructions))
                   +agent-context--session-instructions))
        (puthash key t +agent-context--session-instructions)))))
  (unless no-focus (+agent-acp-show-buffer buffer)))

(defun +agent-context--report-skipped (skipped)
  "Say which entries in SKIPPED could not be turned into a reference."
  (when skipped
    (message "Agent context skipped %d entr%s: %s"
             (length skipped)
             (if (= (length skipped) 1) "y" "ies")
             (string-join skipped "; "))))

(cl-defun +agent-context--send (&key prompt pick draft references instructions
                                   session-instructions)
  "Send editor context to a session or copy it, reading PROMPT when nil.
The context is the shared editor selection unless REFERENCES is given: a
function of (ROOT SESSION) returning the reference plists for exactly what the
command names, such as one region, so a send never drags along whatever was
gathered earlier.  PICK asks which session to use when sessions are not always
asked for.  DRAFT puts the turn in the session's input without submitting it.
INSTRUCTIONS adds per-request guidance. SESSION-INSTRUCTIONS is (KEY . TEXT)
of short capability guidance delivered once per native session. Drafts and
clipboard copies include it without consuming that session's initialization."
  (let* ((buffer (+agent-context--session pick))
         (clipboard (eq buffer 'clipboard))
         (root (if clipboard
                   (+agent-acp-project-root)
                 (buffer-local-value '+agent-acp-session-root buffer)))
         (resolved (if references
                       (cons (funcall references root (unless clipboard buffer)) nil)
                     (+agent-context--resolve nil root (unless clipboard buffer))))
         (shared (null references))
         (references (car resolved))
         (prompt (or prompt
                     (read-string
                      (format "Ask %s (%d reference%s): "
                              (if clipboard "clipboard"
                                (or (buffer-local-value '+agent-acp-session-name buffer)
                                    "the agent"))
                              (length references)
                              (if (= (length references) 1) "" "s"))))))
    (+agent-context--report-skipped (cdr resolved))
    (when (and (string-empty-p (string-trim prompt)) (null references))
      (user-error "Nothing to send: no question and no context"))
    (when instructions
      (setq prompt (concat prompt "\n\n" instructions)))
    (cond
     (clipboard
      (kill-new (+agent-context--prompt-text
                 (if session-instructions
                     (concat (cdr session-instructions) "\n\n" prompt) prompt)
                 references))
      (message "Agent: prompt copied to clipboard"))
     (draft
      (+agent-acp-draft buffer (+agent-context--prompt-text
                               (if session-instructions
                                   (concat (cdr session-instructions) "\n\n" prompt) prompt)
                               references))
      (+agent-acp-show-buffer buffer))
     (t (let ((blocks (+agent-context-content-blocks prompt references)))
          (if session-instructions
              (+agent-context--deliver buffer blocks session-instructions)
            (+agent-context--deliver buffer blocks)))))
    (when (and (not clipboard) +agent-context-clear-after-send shared)
          (+agent-selection-clear))
    (unless clipboard buffer)))

(defun +agent-context--region-references (buffer begin end)
  "Return a REFERENCES function naming only BEGIN..END of BUFFER.
A region covering the whole buffer is a plain file reference."
  (let ((file (or (+agent-context--buffer-file buffer)
                  (user-error "%s is not a saved file" (buffer-name buffer)))))
    (lambda (root session)
      (list (if (+agent-context--whole-buffer-p buffer (list (cons begin end)))
                (+agent-context--reference file root nil nil session)
              (+agent-context--reference file root buffer (cons begin end)
                                        session))))))

(defun +agent-context--file-references (file)
  "Return a REFERENCES function naming only FILE."
  (lambda (root session)
    (list (+agent-context--reference file root nil nil session))))

;;;; ── Commands ─────────────────────────────────────────────────────────────

;;;###autoload
(cl-defun +agent-context-send-prompt (prompt &key files draft pick)
  "Send explicit PROMPT and FILES to an existing agent session.
Reuse normal session selection, queueing and clipboard delivery.  DRAFT
leaves the task in the session input; PICK forces the session chooser.
FILES must be readable, saved files reachable by the selected agent.
Neither read nor clear the shared editor context."
  (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
    (user-error "A task prompt is required"))
  (let ((files (delete-dups (mapcar #'expand-file-name files)))
        (+agent-context-clear-after-send nil))
    (dolist (file files)
      (unless (and (file-regular-p file) (file-readable-p file))
        (user-error "Task reference is not a readable file: %s" file))
      (when-let* ((buffer (find-buffer-visiting file)))
        (when (buffer-modified-p buffer)
          (user-error "Save task reference first: %s" file))))
    (+agent-context--send
     :prompt prompt :pick pick :draft draft
     :references (lambda (root session)
                   (mapcar (lambda (file)
                             (+agent-context--reference file root nil nil session))
                           files)))))

;;;###autoload
(defun +agent-context-send (&optional pick)
  "Send the current editor context to an agent session as references.
The context is the shared editor selection, the one `+agent-selection-add' builds.  With
a prefix argument PICK, choose the session instead of reusing this project's
last one."
  (interactive "P")
  (+agent-context--send :pick pick))

;;;###autoload
(defun +agent-context-draft (&optional pick)
  "Put the current editor context in a session's input without sending it.
With a prefix argument PICK, choose the session."
  (interactive "P")
  (+agent-context--send :pick pick :draft t))

;;;###autoload
(defun +agent-context-send-region (&optional pick)
  "Send exactly the active region to a session.
Only `file:LINE-LINE' travels; the agent reads the lines itself.  The shared
editor selection is left alone: this sends the region, not everything gathered
so far (that is `+agent-context-send').  In a Noema Markdown pane the page's own
selection is used.  PICK is as for `+agent-context-send'."
  (interactive "P")
  (if (+agent-editor-action 'send-region pick)
      nil
    (unless (use-region-p) (user-error "No active region"))
    (let ((references (+agent-context--region-references
                       (current-buffer) (region-beginning) (region-end))))
      (deactivate-mark)
      (+agent-context--send :pick pick :references references))))

;;;###autoload
(defun +agent-context-send-buffer (&optional pick)
  "Send the current buffer's file, and nothing else, to a session.
In a Noema Markdown pane this is the note the pane shows.  PICK is as for
`+agent-context-send'."
  (interactive "P")
  (if (+agent-editor-action 'send-buffer pick)
      nil
    (+agent-context--send
     :pick pick
     :references (+agent-context--file-references
                  (or (+agent-context--buffer-file (current-buffer))
                    (user-error "%s is not a saved file" (buffer-name)))))))

;;;###autoload
(defun +agent-context-send-file (file &optional pick)
  "Send FILE, and nothing else, to a session.
PICK is as for `+agent-context-send'."
  (interactive (list (read-file-name "Reference file: " nil nil t) current-prefix-arg))
  (+agent-context--send
   :pick pick
   :references (+agent-context--file-references (expand-file-name file))))

;;;###autoload
(defun +agent-context-send-at-point (&optional pick)
  "Tell a session exactly where point is, without sending any text.
The turn names the file, line, column and enclosing definition, so a question
like \"why does this branch run?\" has somewhere to land.  With a prefix
argument PICK, choose the session."
  (interactive "P")
  (if (+agent-editor-action 'send-at-point pick)
      ;; The page owns the cursor: it reports its line as a one-line range.
      nil
    (+agent-context--send-at-point pick)))

(defun +agent-context--send-at-point (pick)
  "Send point's location in this file buffer to a session.
PICK is as for `+agent-context-send-at-point'."
  ;; Read point before anything else: choosing a session can start one and
  ;; show its buffer, which would move `point' out from under us.
  (let* ((file (or (+agent-context--buffer-file (current-buffer))
                   (user-error "Point is not in a saved file")))
         (line (line-number-at-pos (point) t))
         (column (current-column))
         (defun-name (or (and (require 'which-func nil t)
                              (ignore-errors (which-function)))
                         (ignore-errors (add-log-current-defun))))
         (buffer (+agent-context--session pick))
         (clipboard (eq buffer 'clipboard))
         (root (if clipboard (+agent-acp-project-root)
                 (buffer-local-value '+agent-acp-session-root buffer)))
         (reference (+agent-context--reference file root nil nil
                                               (unless clipboard buffer)))
         (where (format "%s:%d:%d" (plist-get reference :relative) line column))
         (prompt (read-string (format "Ask about %s: " where)))
         (text (concat prompt "\n\nPoint is at " where
                       (if defun-name (format ", in `%s'" defun-name) "")
                       ".\n")))
    (when (string-empty-p (string-trim prompt))
      (user-error "Nothing to ask"))
    (if clipboard
        (progn (kill-new text)
               (message "Agent: prompt copied to clipboard"))
      (+agent-context--deliver
       buffer (list (list (cons 'type "text") (cons 'text text))
                    (car (+agent-context--file-blocks (list reference))))))))

(defun +agent-context-review-send ()
  "Send the reviewed references through the normal session chooser."
  (interactive)
  (quit-window)
  (+agent-context-send))

;;;###autoload
(defun +agent-context-inspect ()
  "Inspect selected references without loading a model client."
  (interactive)
  (unless (+agent-selection-collect)
    (user-error "No context selected; use C-c A c . first"))
  (let ((root default-directory))
    (with-current-buffer (get-buffer-create "*Agent Context*")
      (+agent-selection-review-mode)
      (setq default-directory root)
      (local-set-key (kbd "C-c C-c") #'+agent-context-review-send)
      (+agent-selection-review-refresh)
      (pop-to-buffer (current-buffer)))))

(provide 'tools/agent/context)
;;; tools/agent/context.el ends here

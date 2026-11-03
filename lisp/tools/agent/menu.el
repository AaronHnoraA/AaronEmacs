;;; menu.el --- Discoverable actions for real agent sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; One keyboard menu for the manager and the original agent-shell buffer.
;; The scope captures an entity, never a name or a copied transcript.

;;; Code:
(require 'transient)
(require 'tools/agent/actions)
(require 'tools/agent/sessions)

(defvar +agent-manager--selected)
(defvar +agent-manager--model)
(declare-function +agent-manager-hide "tools/agent/manager" ())
(declare-function +agent-manager-new "tools/agent/manager" ())

(defun +agent-menu--buffer ()
  "Resolve the captured session again before every operation."
  (let ((buffer (or (transient-scope '+agent-session-menu)
                    (and (bound-and-true-p +agent-manager--model)
                         +agent-manager--selected)
                    (+agent-acp-command-buffer))))
    (unless (+agent-acp-agent-buffer-p buffer) (user-error "This session has ended"))
    buffer))

(defun +agent-menu--scope ()
  "Return the session in question, or nil where none exists yet."
  (ignore-errors (+agent-menu--buffer)))

(defun +agent-menu--title ()
  "Name the menu after its session."
  (if-let* ((buffer (+agent-menu--scope)))
      (format "Agent · %s" (+agent-acp--tab-label buffer))
    "Agent"))

(defun +agent-menu--session-p ()
  (and (+agent-menu--scope) t))

(defun +agent-menu--jump-p () (+agent-menu--supported-p 'jump))
(defun +agent-menu--compact-p () (+agent-menu--supported-p 'compact))
(defun +agent-menu--archive-p () (+agent-menu--supported-p 'archive))

(defun +agent-menu--ready-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (and (+agent-session-live-p buffer)
         (eq (+agent-acp-state-value buffer '(:init-finished)) t)
         (not (+agent-acp-busy-p buffer)))))

(defun +agent-menu--review-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (memq (plist-get (+agent-session-snapshot buffer) :attention-kind) '(permission input))))

(defun +agent-menu--model-p ()
  (and (+agent-menu--ready-p) (+agent-acp-available-model-ids (+agent-menu--buffer))))

(defun +agent-menu--mode-p ()
  (and (+agent-menu--ready-p)
       (+agent-acp-state-value (+agent-menu--buffer) '(:available-modes :modes))))

(defun +agent-menu--busy-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer)))) (+agent-acp-busy-p buffer)))

(defun +agent-menu--fork-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (and (+agent-menu--ready-p) (+agent-session-operation-supported-p 'fork buffer))))

(defun +agent-menu--changes-p ()
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (buffer-local-value '+agent-acp--changes buffer)))

(defun +agent-menu--supported-p (operation)
  (when-let* ((buffer (ignore-errors (+agent-menu--buffer))))
    (+agent-session-operation-supported-p operation buffer)))

(defun +agent-menu--stop-group (buffer)
  "Stop the entire project pool through each session's execution owner."
  (let ((snapshot (+agent-session-snapshot buffer)))
    (+agent-pool-stop (plist-get snapshot :root) (plist-get snapshot :target))))

(autoload '+agent/resume-saved "tools/agent/acp" nil t)

(defun +agent-menu-new ()
  "Create a conversation through its management owner, or an execution agent."
  (interactive)
  (if (bound-and-true-p +agent-manager--model)
      (+agent-manager-new)
    (let ((buffer (or (transient-scope '+agent-session-menu) (current-buffer))))
      (unless (run-hook-with-args-until-success '+agent-session-action-functions 'new buffer)
        (call-interactively #'+agent/start)))))
(autoload '+agent/queue "tools/agent/queue" nil t)

(defun +agent-menu--show (buffer)
  "Leave the navigation surface before entering native interactive controls."
  (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
  (+agent-acp-show-buffer buffer))

(defun +agent-menu-history ()
  "Pick a saved conversation, in this session when it can list them."
  (interactive)
  (let ((buffer (+agent-menu--scope)))
    (cond
     ((and buffer (eq (+agent-session-kind buffer) 'pi))
      (with-current-buffer buffer (call-interactively #'+agent-acp-session-manager)))
     ((and buffer (+agent-menu--ready-p)
             (+agent-session-capability-p buffer :list)
             (or (+agent-session-capability-p buffer :load)
                 (+agent-session-capability-p buffer :resume)))
        (progn (+agent-menu--show buffer)
               (with-current-buffer buffer (call-interactively #'+agent-shell/resume))))
     (t
      (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
      (call-interactively #'+agent/resume-saved)))))

(defun +agent-menu-next (&optional project)
  "Go to the next waiting session, from any surface; PROJECT limits the pool."
  (interactive "P")
  (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
  (+agent/next-attention project))

(declare-function shell-maker-mark-output "shell-maker" ())
(declare-function agent-shell-markdown-reconstruct "agent-shell-markdown" (beg end &optional deepen))
(declare-function aaron-open-place "aaron-open"
                  (file &optional placement line column no-focus))

(defvar +agent-menu-reply-directory (locate-user-emacs-file "var/agent/replies/")
  "Where `+agent-menu-page' writes replies opened as Markdown pages.")

(defconst +agent-menu-reply-limit 20
  "How many reply pages `+agent-menu-reply-directory' keeps; older ones go.")

(defun +agent-menu--reply-in (beg end)
  "Return the Markdown of the last agent message between BEG and END.
A turn interleaves thoughts and tool calls with message fragments; only a
fragment whose ID names an `agent_message_chunk' is the agent talking, and
the last message is its answer."
  (let ((pos beg) last parts)
    (while (< pos end)
      (let* ((next (min (next-single-property-change pos 'agent-shell-ui-section nil end)
                        (next-single-property-change pos 'agent-shell-ui-state nil end)))
             (id (map-elt (get-text-property pos 'agent-shell-ui-state) :qualified-id)))
        (when (and (eq (get-text-property pos 'agent-shell-ui-section) 'body)
                   (stringp id) (string-suffix-p "agent_message_chunk" id))
          (unless (equal id last) (setq last id parts nil))
          (push (agent-shell-markdown-reconstruct pos next) parts))
        (setq pos next)))
    (string-trim (apply #'concat (nreverse parts)))))

(defun +agent-menu--last-reply (buffer)
  "Return BUFFER's last agent reply as its original Markdown."
  (with-current-buffer buffer
    (let ((process (or (get-buffer-process buffer) (user-error "No agent running"))))
      (save-mark-and-excursion
        (goto-char (process-mark process))
        (shell-maker-mark-output)
        (+agent-menu--reply-in (region-beginning) (region-end))))))

(defun +agent-last-reply (&optional buffer)
  "Return the Markdown of BUFFER's last reply, or nil when there is none.
BUFFER defaults to the session the agent menu and slash commands mean.  A
domain that keeps a reply (Noema filing it as a Wiki page) asks here."
  (let ((text (string-trim (or (+agent-menu--last-reply (or buffer (+agent-menu--buffer))) ""))))
    (unless (string-empty-p text) text)))

(defun +agent-menu--reply-file (buffer text)
  "Write TEXT, BUFFER's reply, to a bounded reply directory and return the file.
The same reply maps to the same file, so opening it twice reuses one page."
  (let* ((directory +agent-menu-reply-directory)
         (stem (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" (string-trim (buffer-name buffer) "[ *]+" "[ *]+")))
         (file (expand-file-name (format "%s-%s.md" stem (substring (secure-hash 'sha1 text) 0 8))
                                 directory)))
    (make-directory directory t)
    (unless (file-exists-p file)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file nil 'silent)))
    (set-file-times file)
    (dolist (old (nthcdr +agent-menu-reply-limit
                         (sort (directory-files directory t "\\.md\\'")
                               #'file-newer-than-file-p)))
      (unless (find-buffer-visiting old) (delete-file old)))
    file))

(defun +agent-menu--page (buffer)
  "Open BUFFER's last reply as a Markdown page beside the chat."
  (let ((text (string-trim (or (+agent-menu--last-reply buffer) ""))))
    (when (string-empty-p text) (user-error "No reply yet"))
    (aaron-open-place (+agent-menu--reply-file buffer (concat text "\n")) 'other)))

(defun +agent-menu--act (operation)
  "Apply OPERATION to the real scoped session through its public owner."
  (let ((buffer (+agent-menu--buffer)))
    (pcase operation
      ('stop (+agent-acp-stop buffer))
      ('stop-group (+agent-menu--stop-group buffer))
      ('rename (+agent-session-rename buffer))
      ('close (+agent-acp-close buffer))
      ('copy (with-current-buffer buffer (agent-shell-copy-last-output)))
      ('page (+agent-menu--page buffer))
      ((or 'queue 'changes 'tree 'side-chat 'close-others)
       (when (bound-and-true-p +agent-manager--model) (+agent-manager-hide))
       (pcase operation
         ('queue (+agent/queue buffer))
         ('changes (+agent-session-changes buffer))
         ('tree (+agent-acp-conversation-tree buffer))
         ('side-chat (+agent-session-side-chat buffer))
         ('close-others (+agent-acp-close-others buffer))))
      (_
       (+agent-menu--show buffer)
       (pcase operation
         ('input (+agent-acp-focus-input buffer))
         ('review (+agent-acp-review-request buffer))
         ('restart (+agent-session-restart buffer))
         ('fork (+agent-session-fork buffer))
         ('jump (+agent-session-jump buffer))
         ('archive (+agent-session-archive buffer))
         ('compact (+agent-session-compact buffer))
         (_ (with-current-buffer buffer
              (call-interactively
               (pcase operation
                 ('model #'agent-shell-set-session-model)
                 ('mode #'agent-shell-set-session-mode)
                 ('usage #'+agent-top-report))))))))))

(autoload '+agent-top-report "tools/agent/top" nil t)

(dolist (operation '(input review stop stop-group rename close restart fork model mode queue changes tree side-chat close-others copy page jump archive compact usage))
  (let ((action operation))
    (defalias (intern (format "+agent-menu-%s" action))
      (lambda () (interactive) (+agent-menu--act action)))))

;; The layout is generated: `tools/agent/actions' is the only list of actions.
(eval-and-compile
  (defun +agent-menu--suffix (row)
    "Return the Transient suffix of action ROW."
    (pcase-let ((`(,key ,label ,command . ,properties) row))
      (append (list key label command)
              (when-let* ((predicate (plist-get properties :if))) (list :if predicate))))))

(defmacro +agent-menu--define (name doc table &rest extra)
  "Define the Transient prefix NAME with DOC from action TABLE and EXTRA suffixes."
  `(transient-define-prefix ,name ()
     ,doc
     [:description +agent-menu--title
      ,@(mapcar (lambda (group)
                  (vconcat (list (car group))
                           (mapcar #'+agent-menu--suffix (cdr group))))
                (symbol-value table))
      ,@extra]
     (interactive)
     (let ((buffer (+agent-menu--scope)))
       (if (and (eq ',name '+agent-session-menu) buffer (eq (+agent-session-kind buffer) 'pi))
           (with-current-buffer buffer (call-interactively #'+agent-acp-session-manager))
         (transient-setup ',name nil nil :scope buffer)))))

(+agent-menu--define +agent-send-menu "Hand material to an agent." +agent-send-actions)

;;;###autoload (autoload '+agent-session-menu "tools/agent/menu" nil t)
(+agent-menu--define +agent-session-menu
  "Every agent action; opening this menu never sends or approves a request."
  +agent-actions
  ["Send" ("c" "Send…" +agent-send-menu)])

(provide 'tools/agent/menu)
;;; menu.el ends here

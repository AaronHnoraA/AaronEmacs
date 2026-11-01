;;; tools/agent/actions.el --- The one table of agent actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Every agent action is one row here, so a letter means the same thing wherever
;; it is pressed.  `C-c A', the session menu (`?', `C-c C-z', the manager's `.')
;; and the input's local slash commands are all generated from this table; add
;; an action here, never to one of those surfaces.  `agent-shell' opens a session
;; and `C-c a' recalls one.  See C47-C49 in docs/design/agent-management.md.
;;
;; This file loads at startup and requires nothing: `+agent-menu-*' commands
;; come from `tools/agent/menu', which loads agent-shell only when first used.

;;; Code:

(defconst +agent-actions
  '(("Conversation"
     ("i" "Write" +agent-menu-input :if +agent-menu--session-p)
     ("a" "Review request" +agent-menu-review :if +agent-menu--review-p)
     ("j" "Next waiting session" +agent-menu-next :slash "next"
      :doc "Go to the next session waiting on you")
     ("y" "Copy last reply" +agent-menu-copy :if +agent-menu--session-p)
     ("h" "History…" +agent-menu-history :slash "resume"
      :doc "Resume a saved conversation"))
    ("Session"
     ("r" "Rename" +agent-menu-rename :if +agent-menu--session-p :slash "rename"
      :doc "Rename this session")
     ("m" "Model" +agent-menu-model :if +agent-menu--model-p)
     ("v" "Mode" +agent-menu-mode :if +agent-menu--mode-p)
     ("f" "Fork" +agent-menu-fork :if +agent-menu--fork-p :slash "fork"
      :doc "Fork this conversation")
     ("q" "Queue…" +agent-menu-queue :if +agent-menu--session-p :slash "queue"
      :doc "Review, edit or resume queued prompts")
     ("l" "Changed files" +agent-menu-changes :if +agent-menu--changes-p :slash "changes"
      :doc "Open a file this session changed")
     ("T" "Conversation tree" +agent-menu-tree :if +agent-menu--session-p)
     ("t" "Side chat" +agent-menu-side-chat :if +agent-menu--session-p :slash "side"
      :doc "Ask beside this session without interrupting it"))
    ("Lifecycle"
     ("s" "Stop turn" +agent-menu-stop :if +agent-menu--busy-p)
     ("X" "Stop project pool" +agent-menu-stop-group :if +agent-menu--session-p)
     ("R" "Reconnect / restart" +agent-menu-restart :if +agent-menu--session-p :slash "restart"
      :doc "Restart the client, keeping this conversation")
     ("x" "End session…" +agent-menu-close :if +agent-menu--session-p)
     ("K" "End other sessions…" +agent-menu-close-others :if +agent-menu--session-p))
    ("Sessions"
     ("RET" "Manager" +agent/manager :slash "agents" :doc "Open the Agent Manager")
     ("S" "All sessions" +agent/sessions)
     ("G" "Inbox" +agent/inbox)
     ("U" "Usage" +agent/usage)
     ("u" "Detailed usage" +agent-menu-usage :if +agent-menu--session-p)
     ("n" "New session…" +agent-menu-new :slash "new" :doc "Start another session in this management scope")
     ("w" "New in a worktree…" +agent-worktree-start)
     ("N" "Parallel attempts…" +agent-worktree-parallel)
     ("A" "Attempts board" +agent-worktree-board)
     ("?" "Key help" +agent-acp-help))
    ("Noema"
     ("W" "Noema" noema)
     ("P" "Pi" noema-pi-open)
     ("D" "Pi doctor" noema-pi-doctor)
     ("I" "Research attention" noema-research-attention)
     ("O" "Orchestration" noema-orchestration)
     ("g" "Agent cache" noema-project-graph)
     ("p" "Promote to Research" noema-agent-promote-current-session :if +agent-menu--session-p)
     ("e" "Edit page source" noema-md-bridge-edit-source)
     ("J" "Associated work" +agent-menu-jump :if +agent-menu--jump-p)
     ("C" "Context rollover" +agent-menu-compact :if +agent-menu--compact-p)
     ("z" "Archive Research…" +agent-menu-archive :if +agent-menu--archive-p)))
  "Groups of agent actions, each (KEY LABEL COMMAND . PROPERTIES).
PROPERTIES may give :if, a predicate shown in the menu, and :slash with :doc,
a local command typed at the agent's input.")

(defconst +agent-send-actions
  '(("Compose"
     ("c" "Prompt document" +agent/compose)
     ("s" "Send document" +agent/compose-send)
     ("m" "Document menu" +agent/compose-menu)
     ("." "Add context" +agent/compose-add-context)
     ("r" "Rewrite selection" +agent/compose-rewrite))
    ("Send to a session"
     ("x" "Context here" +agent-context-send)
     ("v" "Region" +agent-context-send-region)
     ("B" "Buffer" +agent-context-send-buffer)
     ("f" "File…" +agent-context-send-file)
     ("@" "Point" +agent-context-send-at-point)
     ("," "Inspect first" +agent-context-inspect)
     ("d" "Draft only" +agent-context-draft)))
  "The actions under `c', which all hand material to an agent.")

(autoload '+agent-session-menu "tools/agent/menu" nil t)

(defun +agent-actions--rows (table)
  "Return every action row of TABLE."
  (apply #'append (mapcar #'cdr table)))

(defun +agent-actions--bind (map rows)
  "Bind ROWS in MAP with their labels, autoloading menu operations."
  (dolist (row rows map)
    (pcase-let ((`(,key ,label ,command . ,_) row))
      (when (string-prefix-p "+agent-menu-" (symbol-name command))
        (autoload command "tools/agent/menu" nil t))
      (define-key map (kbd key) (cons label command)))))

(defun +agent-actions-keymap ()
  "Return the `C-c A' prefix map, generated from the action tables."
  (let ((map (make-sparse-keymap "Agent")))
    (+agent-actions--bind map (+agent-actions--rows +agent-actions))
    (define-key map (kbd ".") (cons "Session menu" '+agent-session-menu))
    (define-key map (kbd "c")
                (cons "Send…" (+agent-actions--bind (make-sparse-keymap "Send to agent")
                                                    (+agent-actions--rows +agent-send-actions))))
    map))

(defun +agent-actions-slash-commands ()
  "Return the local input commands as (NAME COMMAND . DOC)."
  (delq nil (mapcar (lambda (row)
                      (when-let* ((name (plist-get (nthcdr 3 row) :slash)))
                        (cons name (cons (nth 2 row) (plist-get (nthcdr 3 row) :doc)))))
                    (+agent-actions--rows +agent-actions))))

(provide 'tools/agent/actions)
;;; actions.el ends here

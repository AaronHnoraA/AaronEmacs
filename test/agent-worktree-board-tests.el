;;; +agent-worktree-board-tests.el --- Tests for parallel attempts -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Real repositories, stand-in agent buffers: starting an agent is replaced by
;; a buffer whose directory is the attempt's worktree.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tools/agent/worktree-board)
(unless (featurep 'agent-worktree-tests)
  (load (expand-file-name "agent-worktree-tests.el" (file-name-directory (or load-file-name buffer-file-name))) nil t))

(defmacro +agent-worktree-board-tests--with-agents (&rest body)
  "Run BODY in a repository where starting an agent makes a stand-in buffer.
`prompts' collects (BUFFER . TEXT) sent; the agent `broken' cannot start."
  (declare (indent 0) (debug t))
  `(+agent-worktree-tests--with-repo
     (let ((prompts '())
           (default-directory repo))
       (cl-letf (((symbol-function '+agent-acp-project-root) (lambda (&rest _) repo))
                 ((symbol-function 'noema-project-workspace) (lambda (root) root))
                 ((symbol-function '+agent-acp-config-for)
                  (lambda (agent) (unless (eq agent 'broken) (list :identifier agent))))
                 ((symbol-function '+agent-acp-start)
                  (cl-function
                   (lambda (&key directory &allow-other-keys)
                     (let ((buffer (+agent-worktree-tests--session directory)))
                       (push buffer sessions)
                       buffer))))
                 ((symbol-function '+agent-acp-adopt) #'ignore)
                 ((symbol-function '+agent-acp--unique-name) (lambda (base _) base))
                 ((symbol-function '+agent-acp-agent-buffer-p)
                  (lambda (buffer) (memq buffer sessions)))
                 ((symbol-function '+agent-acp-state-value) (lambda (&rest _) "sid"))
                 ((symbol-function '+agent-acp-prompt)
                  (cl-function (lambda (&key buffer content &allow-other-keys)
                                 (push (cons buffer (alist-get 'text (car content)))
                                       prompts))))
                 ((symbol-function 'pop-to-buffer) #'ignore))
         ,@body))))

(ert-deftest +agent-worktree-parallel-starts-every-attempt ()
  (+agent-worktree-board-tests--with-agents
    (let* ((started (+agent-worktree-parallel
                     "Fix parser" '((codex . "fix it") (codex . "fix it")
                                    (broken . "fix it") (claude . "fix it"))))
           (members (seq-filter (lambda (w) (plist-get w :group))
                                (+agent-worktree-list repo))))
      ;; The broken agent is reported; the others still start.
      (should (= (length started) 3))
      (should (equal (sort (mapcar (lambda (w) (plist-get w :branch)) members) #'string<)
                     '("noema/fix-parser-1-codex" "noema/fix-parser-2-codex"
                       "noema/fix-parser-4-claude")))
      (should (seq-every-p (lambda (w) (equal (plist-get w :group) "fix-parser")) members))
      (should (equal (plist-get (seq-find (lambda (w) (string-suffix-p "claude" (plist-get w :branch)))
                                          members)
                                :agent)
                     "claude"))
      ;; Each attempt got the prompt in its own session.
      (should (= (length prompts) 3))
      (should (equal (delete-dups (mapcar #'cdr prompts)) '("fix it")))
      (should (equal (length (delete-dups (mapcar #'car prompts))) 3))
      ;; The same group cannot be started twice.
      (should-error (+agent-worktree-parallel "fix parser" '((codex . "again")))
                    :type 'user-error)
      (kill-buffer +agent-worktree-board-buffer-name))))

(ert-deftest +agent-worktree-group-state-follows-members ()
  (+agent-worktree-board-tests--with-agents
    (+agent-worktree-parallel "race" '((codex . "") (claude . "")))
    (let* ((members (seq-filter (lambda (w) (plist-get w :group))
                                (+agent-worktree-list repo)))
           (busy '()) (marks '()))
      (cl-letf (((symbol-function '+agent-acp-busy-p) (lambda (b) (memq b busy)))
                ((symbol-function '+agent-acp-attention-mark)
                 (lambda (b) (cdr (assq b marks)))))
        (let ((first (+agent-worktree-board--session (car members)))
              (second (+agent-worktree-board--session (cadr members))))
          (should (and first second (not (eq first second))))
          (setq busy (list first))
          (should (eq (+agent-worktree-member-state (car members)) 'working))
          (should (eq (+agent-worktree-member-state (cadr members)) 'idle))
          (should (eq (+agent-worktree-group-state members) 'working))
          ;; One attempt waiting holds up the whole group.
          (setq marks (list (cons second "!approve")))
          (should (eq (+agent-worktree-group-state members) 'waiting))
          ;; Finishing one stops nothing; the group settles when none is left.
          (setq busy nil marks (list (cons first "done")))
          (should (eq (+agent-worktree-member-state (car members)) 'done))
          (should (eq (+agent-worktree-group-state members) 'settled))
          (kill-buffer second)
          (should (eq (+agent-worktree-member-state (cadr members)) 'closed))
          (should (eq (+agent-worktree-group-state members) 'settled))))
      (kill-buffer +agent-worktree-board-buffer-name))))

(ert-deftest +agent-worktree-member-stat-counts-commits-edits-and-new-files ()
  (+agent-worktree-tests--with-repo
    (let* ((path (plist-get (+agent-worktree-create repo "stat") :path))
           (member (car (+agent-worktree-list repo))))
      (should (equal (+agent-worktree-member-stat member) "0 commits"))
      (with-temp-file (expand-file-name "src/a.el" path) (insert "one\nagent\n"))
      (+agent-worktree--git-ok path "commit" "-q" "-am" "agent")
      (with-temp-file (expand-file-name "src/a.el" path) (insert "one\nagent\nmore\n"))
      (with-temp-file (expand-file-name "src/new.el" path) (insert "x\n"))
      (should (equal (+agent-worktree-member-stat member)
                     "1 commit, 1 file +2 -1, 1 new")))))

(ert-deftest +agent-worktree-board-lists-and-removes-groups ()
  (+agent-worktree-board-tests--with-agents
    (+agent-worktree-parallel "one" '((codex . "")))
    (+agent-worktree-parallel "two" '((codex . "") (claude . "")))
    (+agent-worktree-create repo "solo")
    (with-current-buffer +agent-worktree-board-buffer-name
      (cl-letf (((symbol-function '+agent-acp-busy-p) #'ignore)
                ((symbol-function '+agent-acp-attention-mark) #'ignore)
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (+agent-worktree-board-refresh)
        ;; Only grouped attempts appear, the plain worktree does not.
        (should (equal (mapcar #'car tabulated-list-entries)
                       '("noema/one-1-codex" "noema/two-1-codex" "noema/two-2-claude")))
        (should (equal (aref (cadr (nth 1 tabulated-list-entries)) 0) "two (settled)"))
        (goto-char (point-min))
        ;; The native table now includes a visible column heading; select
        ;; the attempt by identity instead of its physical line number.
        (while (not (equal (tabulated-list-get-id) "noema/two-1-codex"))
          (forward-line 1))
        ;; Sessions still open block the group's removal.
        (should-error (+agent-worktree-board-remove-group) :type 'user-error)
        (dolist (buffer sessions) (kill-buffer buffer))
        ;; So do uncommitted edits, until forced.
        (let ((path (plist-get (+agent-worktree-board--member) :path)))
          (with-temp-file (expand-file-name "src/a.el" path) (insert "edit\n")))
        (should-error (+agent-worktree-board-remove-group) :type 'user-error)
        (+agent-worktree-board-remove-group t)
        (should (equal (mapcar #'car tabulated-list-entries) '("noema/one-1-codex")))
        ;; Branches survive removal.
        (should (eq 0 (car (+agent-worktree--git
                            repo "rev-parse" "--verify" "--quiet"
                            "refs/heads/noema/two-2-claude"))))))
    (kill-buffer +agent-worktree-board-buffer-name)
    ;; Killing the board stops it following sessions.
    (should-not (memq #'+agent-worktree-board--session-changed
                      +agent-acp-changed-functions))))

(ert-deftest +agent-worktree-board-coalesces-session-changes ()
  "Events inside an ACP handler schedule one redraw; none runs synchronously."
  (let ((board (get-buffer-create +agent-worktree-board-buffer-name))
        (+agent-worktree-board--timer nil)
        (renders 0))
    (unwind-protect
        (with-current-buffer board
          (cl-letf (((symbol-function '+agent-worktree-board--render)
                     (lambda () (cl-incf renders)))
                    ((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
            (+agent-worktree-board--session-changed nil)
            (+agent-worktree-board--session-changed nil)
            (should (= renders 0))
            (should (timerp +agent-worktree-board--timer))
            (+agent-worktree-board--redraw)
            (should (= renders 1))
            (should-not +agent-worktree-board--timer)))
      (when (timerp +agent-worktree-board--timer)
        (cancel-timer +agent-worktree-board--timer))
      (kill-buffer board))))

(provide 'agent-worktree-board-tests)
;;; +agent-worktree-board-tests.el ends here

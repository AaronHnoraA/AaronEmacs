;;; +agent-worktree-tests.el --- Tests for worktree sessions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Each test builds a real repository in a temporary directory and drives Git
;; through the same `process-file' path a session uses.  No agent starts:
;; an agent session is stood in for by a buffer whose directory is the
;; worktree, which is all the redirect and removal guards read.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tools/agent/worktree)
(require 'tools/agent/context)

(defmacro +agent-worktree-tests--with-repo (&rest body)
  "Run BODY with `repo' bound to a fresh repository with one commit.
`parent' is the directory holding it, removed afterwards with its worktrees."
  (declare (indent 0) (debug t))
  `(let* ((parent (file-name-as-directory
                   (file-truename (make-temp-file "noema-worktree-" t))))
          (repo (file-name-as-directory (expand-file-name "repo" parent)))
          (process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                     "GIT_AUTHOR_NAME=Noema" "GIT_AUTHOR_EMAIL=noema@example.invalid"
                     "GIT_COMMITTER_NAME=Noema" "GIT_COMMITTER_EMAIL=noema@example.invalid")
                   process-environment))
          (sessions '()))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src" repo) t)
           (with-temp-file (expand-file-name "src/a.el" repo) (insert "one\ntwo\n"))
           (+agent-worktree--git-ok repo "init" "-q" "-b" "main")
           (+agent-worktree--git-ok repo "add" ".")
           (+agent-worktree--git-ok repo "commit" "-q" "-m" "init")
           ,@body)
       (dolist (buffer sessions) (when (buffer-live-p buffer) (kill-buffer buffer)))
       (delete-directory parent t))))

(defun +agent-worktree-tests--session (directory)
  "Return a stand-in agent buffer working in DIRECTORY."
  (let ((buffer (generate-new-buffer " *noema-worktree-session*")))
    (with-current-buffer buffer (setq default-directory directory))
    buffer))

(ert-deftest +agent-worktree-create-beside-repository ()
  (+agent-worktree-tests--with-repo
    (let* ((worktree (+agent-worktree-create repo "Fix Parser!"))
           (path (plist-get worktree :path))
           (checkout (+agent-worktree-checkout path)))
      (should (equal path (expand-file-name "repo.noema-worktrees/fix-parser/" parent)))
      (should (equal (plist-get worktree :branch) "noema/fix-parser"))
      (should (equal (plist-get worktree :base) "main"))
      (should (file-exists-p (expand-file-name "src/a.el" path)))
      (should (plist-get checkout :linked))
      (should (equal (plist-get checkout :toplevel) path))
      (should (equal (plist-get checkout :native-primary) repo))
      (should (equal (plist-get checkout :base) "main"))
      ;; The main checkout is not linked and has no recorded base.
      (should-not (plist-get (+agent-worktree-checkout repo) :linked))
      (should-not (plist-get (+agent-worktree-checkout repo) :base))
      (should-error (+agent-worktree-create repo "fix parser") :type 'user-error))))

(ert-deftest +agent-worktree-checkout-from-subdirectory ()
  (+agent-worktree-tests--with-repo
    (should (equal (plist-get (+agent-worktree-checkout
                               (expand-file-name "src/" repo))
                              :toplevel)
                   repo))
    (should-not (+agent-worktree-checkout parent))))

(ert-deftest +agent-worktree-redirects-main-checkout-files ()
  (+agent-worktree-tests--with-repo
    (let* ((path (plist-get (+agent-worktree-create repo "w") :path))
           (session (+agent-worktree-tests--session path))
           (main (+agent-worktree-tests--session repo)))
      (setq sessions (list session main))
      (should (equal (+agent-worktree-redirect
                      (expand-file-name "src/a.el" repo) session)
                     (cons (expand-file-name "src/a.el" path) "src/a.el")))
      ;; Already the worktree's own file, or a session on the main checkout.
      (should-not (+agent-worktree-redirect (expand-file-name "src/a.el" path) session))
      (should-not (+agent-worktree-redirect (expand-file-name "src/a.el" repo) main))
      ;; A file outside the repository is left alone.
      (should-not (+agent-worktree-redirect (expand-file-name "other" parent) session))
      ;; A file only the main checkout has is an error, never a fallback.
      (with-temp-file (expand-file-name "src/new.el" repo) (insert "x"))
      (should-error (+agent-worktree-redirect
                     (expand-file-name "src/new.el" repo) session)
                    :type 'user-error))))

(ert-deftest +agent-worktree-redirects-through-symbolic-links ()
  ;; Git prints resolved paths; a buffer may name the file through a link.
  (+agent-worktree-tests--with-repo
    (let* ((path (plist-get (+agent-worktree-create repo "link") :path))
           (link (expand-file-name "repo-link" parent))
           (session (+agent-worktree-tests--session path)))
      (setq sessions (list session))
      (make-symbolic-link (directory-file-name repo) link)
      (should (equal (+agent-worktree-redirect
                      (expand-file-name "repo-link/src/a.el" parent) session)
                     (cons (expand-file-name "src/a.el" path) "src/a.el"))))))

(ert-deftest +agent-worktree-git-keeps-caller-environment ()
  ;; A workspace capsule is buffer-local; Git must still see it.
  (+agent-worktree-tests--with-repo
    (with-temp-buffer
      (setq-local process-environment
                  (cons "NOEMA_WORKTREE_PROBE=capsule" process-environment))
      (should (equal (+agent-worktree--git-ok
                      repo "-c" "alias.probe=!printf %s \"$NOEMA_WORKTREE_PROBE\"" "probe")
                     "capsule")))))

(ert-deftest +agent-worktree-context-reference-names-worktree-copy ()
  (+agent-worktree-tests--with-repo
    (let* ((path (plist-get (+agent-worktree-create repo "ctx") :path))
           (session (+agent-worktree-tests--session path))
           (reference (+agent-context--reference
                       (expand-file-name "src/a.el" repo) repo nil nil session)))
      (setq sessions (list session))
      (should (equal (plist-get reference :agent-file) (expand-file-name "src/a.el" path)))
      (should (equal (plist-get reference :relative) "src/a.el"))
      ;; The reference still names the buffer the person selected in.
      (should (equal (plist-get reference :file) (expand-file-name "src/a.el" repo))))))

(ert-deftest +agent-worktree-region-must-match-worktree-copy ()
  (+agent-worktree-tests--with-repo
    (let* ((path (plist-get (+agent-worktree-create repo "lines") :path))
           (session (+agent-worktree-tests--session path))
           (source (find-file-noselect (expand-file-name "src/a.el" repo)))
           (region (with-current-buffer source
                     ;; "two\n": ends at the start of the next line.
                     (cons (progn (goto-char (point-min)) (forward-line 1) (point))
                           (point-max)))))
      (setq sessions (list session source))
      (let ((reference (+agent-context--reference
                        (expand-file-name "src/a.el" repo) repo source region session)))
        (should (equal (list (plist-get reference :line-start)
                             (plist-get reference :line-end))
                       '(2 2))))
      ;; The agent's copy moved on: the same line now holds other text.
      (with-temp-file (expand-file-name "src/a.el" path) (insert "one\nchanged\n"))
      (should-error (+agent-context--reference
                     (expand-file-name "src/a.el" repo) repo source region session)
                    :type 'user-error)
      ;; A whole-file reference is still fine.
      (should (+agent-context--reference (expand-file-name "src/a.el" repo) repo
                                        nil nil session)))))

(ert-deftest +agent-worktree-list-and-remove ()
  (+agent-worktree-tests--with-repo
    (let* ((worktree (progn (+agent-worktree-create repo "keep")
                            (+agent-worktree-create repo "gone")
                            (seq-find (lambda (w) (equal (plist-get w :branch) "noema/gone"))
                                      (+agent-worktree-list repo))))
           (path (plist-get worktree :path)))
      ;; Only Noema's worktrees are listed, never the main checkout.
      (should (equal (sort (mapcar (lambda (w) (plist-get w :branch))
                                   (+agent-worktree-list repo))
                           #'string<)
                     '("noema/gone" "noema/keep")))
      (should (equal (plist-get worktree :repo) repo))
      (with-temp-file (expand-file-name "src/a.el" path) (insert "changed\n"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function '+agent-acp-agent-buffer-p) #'bufferp))
        ;; A session still working there blocks removal, even when forced.
        (let ((session (+agent-worktree-tests--session path)))
          (setq sessions (list session))
          (should-error (+agent-worktree-remove worktree t) :type 'user-error)
          (kill-buffer session))
        ;; Uncommitted changes need the force argument.
        (should-error (+agent-worktree-remove worktree) :type 'user-error)
        (should (file-exists-p path))
        (+agent-worktree-remove worktree t))
      (should-not (file-exists-p path))
      ;; The branch, and so any committed work, survives.
      (should (eq 0 (car (+agent-worktree--git
                          repo "rev-parse" "--verify" "--quiet" "refs/heads/noema/gone"))))
      (should (equal (mapcar (lambda (w) (plist-get w :branch))
                             (+agent-worktree-list repo))
                     '("noema/keep"))))))

(ert-deftest +agent-worktree-diff-starts-at-branch-point ()
  (+agent-worktree-tests--with-repo
    (let* ((path (plist-get (+agent-worktree-create repo "diff") :path))
           (branch-point (+agent-worktree--git-ok repo "rev-parse" "main"))
           requested)
      ;; Both checkouts move on; the review still starts where the branch began.
      (with-temp-file (expand-file-name "src/a.el" path) (insert "agent\n"))
      (+agent-worktree--git-ok path "commit" "-q" "-am" "agent work")
      (with-temp-file (expand-file-name "src/a.el" repo) (insert "main\n"))
      (+agent-worktree--git-ok repo "commit" "-q" "-am" "main work")
      (cl-letf (((symbol-function '+agent-worktree--require-magit) #'ignore)
                ((symbol-function 'magit-diff-working-tree)
                 (lambda (&optional rev &rest _)
                   (setq requested (list rev default-directory)))))
        (+agent-worktree-magit-diff (expand-file-name "src/" path))
        (should (equal requested (list branch-point path)))
        ;; Outside a worktree session it is the uncommitted edits.
        (+agent-worktree-magit-diff repo)
        (should (equal requested (list nil repo)))))))

(ert-deftest +agent-worktree-slug ()
  (should (equal (+agent-worktree--slug "  Try: the NEW idea  ") "try-the-new-idea"))
  (should-error (+agent-worktree--slug "!!!") :type 'user-error))

(provide 'agent-worktree-tests)
;;; +agent-worktree-tests.el ends here

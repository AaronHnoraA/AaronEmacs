;;; tools/git/core.el --- Core Git UI and workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui)
(require 'aaron-funcs)
(require 'transient)

(declare-function browse-at-remote "browse-at-remote" (&optional kill))
(declare-function browse-at-remote-kill "browse-at-remote" ())
(declare-function diff-hl-next-hunk "diff-hl" (&optional backward))
(declare-function diff-hl-previous-hunk "diff-hl" ())
(declare-function diff-hl-revert-hunk "diff-hl" ())
(declare-function diff-hl-show-hunk "diff-hl-show-hunk" ())
(declare-function diff-hl-stage-current-hunk "diff-hl" ())
(declare-function diff-hl-dired-status-files "diff-hl-dired"
                  (backend dir files update-function))
(declare-function magit-blame-addition "magit-blame" ())
(declare-function magit-blame-quit "magit-blame" ())
(declare-function magit-dispatch "magit" ())
(declare-function magit-file-dispatch "magit" ())
(declare-function magit-log-buffer-file "magit-log" (&optional follow beg end))
(declare-function magit-stage-files "magit-apply" (files &optional force))
(declare-function magit-status "magit" (&optional directory))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function remote-canonicalize-file-name "remote-fs"
                  (file-name &optional directory))
(declare-function remote-client-file-name "remote-fs"
                  (file-name &optional adapter))
(declare-function remote-file-local-name "remote-fs" (file-name))
(declare-function remote-fs-file-name-p "remote-fs" (file-name))
(declare-function remote-fs-target-id "remote-fs" (file-name))
(declare-function remote-make-file-name "remote-fs" (target-id localname))
(declare-function remote-workspace-for-path "remote-workspace" (path))
(declare-function remote-workspace-live-p "remote-workspace" (workspace))
(declare-function magit-unstage-files "magit-apply" (files))
(declare-function git-commit-setup-flyspell "git-commit" ())
(declare-function git-commit-turn-on-flyspell "git-commit" ())
(declare-function +git/board "tools/git/board" ())
(declare-function +git-diff/file-with-branch-base "tools/git/diff" ())
(declare-function +git-diff/file-with-head "tools/git/diff" ())
(declare-function +git-diff/file-with-revision "tools/git/diff" (revision))
(declare-function +git/tree "tools/git/tree" ())
(declare-function merge-conflict-menu "emacs/smerge/config" ())

(defvar magit-blame-mode)
(defvar vc-dir-process-output-limit)

(defvar-local +git-tool-origin-window-configuration nil
  "Window configuration captured before opening the current Git tool buffer.")

(defvar-local +git-tool-cleanup-functions nil
  "Cleanup functions to run when the current Git tool buffer is killed.")

(defun +git-tool-register-cleanup (fn)
  "Register cleanup function FN for the current Git tool buffer."
  (add-hook 'kill-buffer-hook fn nil t)
  (push fn +git-tool-cleanup-functions))

(defun +git-tool-clear-origin-references-h ()
  "Clear Git tool references to the buffer being killed."
  (let ((origin (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (var '(+git-board-origin-buffer
                         +git-diff-origin-buffer
                         +git-tree-origin-buffer))
            (when (and (boundp var)
                       (eq (symbol-value var) origin))
              (set var nil))))))))

(defun +git-tool-watch-origin-buffer (origin-buffer)
  "Install origin-buffer cleanup for Git tool buffers."
  (when (buffer-live-p origin-buffer)
    (with-current-buffer origin-buffer
      (add-hook 'kill-buffer-hook #'+git-tool-clear-origin-references-h nil t))))

(defun +git-tool-prepare-buffer (&optional origin-buffer)
  "Capture window configuration and watch ORIGIN-BUFFER for Git tool cleanup."
  (+git-tool-watch-origin-buffer origin-buffer)
  (setq-local +git-tool-origin-window-configuration
              (current-window-configuration)))

(defun +git-tool-quit-buffer (origin-buffer)
  "Quit the current Git tool buffer and restore ORIGIN-BUFFER when possible."
  (let ((tool-buffer (current-buffer))
        (window-config +git-tool-origin-window-configuration))
    (cond
     ((and (window-configuration-p window-config)
           (buffer-live-p origin-buffer))
      (set-window-configuration window-config)
      (when (eq (current-buffer) tool-buffer)
        (switch-to-buffer origin-buffer))
      (kill-buffer tool-buffer))
     ((buffer-live-p origin-buffer)
      (switch-to-buffer origin-buffer)
      (kill-buffer tool-buffer))
     (t
      (quit-window 'kill)))))

(defun +git-commit-setup-flyspell-h ()
  "Enable Flyspell in commit buffers across Magit versions."
  (if (fboundp 'git-commit-setup-flyspell)
      (git-commit-setup-flyspell)
    (git-commit-turn-on-flyspell)))

(defun +diff-hl-dired-status-files-with-full-output-a
    (fn backend dir files update-function)
  "Call Diff-HL status FN without the VC-Dir output limit.

Emacs 32's Git backend assumes that a limited status command belongs to a
VC-Dir process buffer and tries to update its `vc-parent-buffer'.  Diff-HL
uses the same backend from its own temporary buffer, where that variable is
nil.  Large repositories would consequently fail from the process sentinel
with `wrong-type-argument stringp nil'.

Diff-HL needs the complete status result to paint Dired accurately, so make
the limit nil only for this consumer.  Keep the user's global VC-Dir limit
unchanged."
  (let ((vc-dir-process-output-limit nil))
    (funcall fn backend dir files update-function)))

(defun +git--current-file-context ()
  "Return current file Git context as (REPO-ROOT RELATIVE-PATH).

Signal a user error if the current buffer is not visiting a tracked file inside
a Git repository."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((file (file-truename buffer-file-name))
         (repo-root (magit-toplevel (file-name-directory file))))
    (unless repo-root
      (user-error "Current file is not inside a Git repository"))
    (unless (file-in-directory-p file repo-root)
      (user-error "Current file is outside the repository root"))
    (list repo-root (file-relative-name file repo-root))))

(defun +git/file-log ()
  "Show the Git log for the current file."
  (interactive)
  (require 'magit-log)
  (magit-log-buffer-file))

(defun +git/file-blame-toggle ()
  "Toggle blame annotations for the current file."
  (interactive)
  (require 'magit-blame)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-quit)
    (magit-blame-addition)))

(defun +git/stage-current-file ()
  "Stage the current file."
  (interactive)
  (pcase-let ((`(,repo-root ,relative-path) (+git--current-file-context)))
    (require 'magit-apply)
    (let ((default-directory repo-root))
      (magit-stage-files (list relative-path)))
    (message "Staged %s" relative-path)))

(defun +git/unstage-current-file ()
  "Unstage the current file."
  (interactive)
  (pcase-let ((`(,repo-root ,relative-path) (+git--current-file-context)))
    (require 'magit-apply)
    (let ((default-directory repo-root))
      (magit-unstage-files (list relative-path)))
    (message "Unstaged %s" relative-path)))

(transient-define-prefix git-menu ()
  "Git workbench for repository and current-file operations."
  [["Repository"
    ("g" "status" magit-status :transient transient--do-exit)
    ("w" "git board" +git/board :transient transient--do-exit)
    ("t" "git tree" +git/tree :transient transient--do-exit)
    ("." "magit dispatch" magit-dispatch :transient transient--do-exit)
    ("m" "merge conflict" merge-conflict-menu :transient transient--do-exit)]
   ["Current file"
    ("d" "diff vs revision" +git-diff/file-with-revision :transient transient--do-exit)
    ("=" "diff vs HEAD" +git-diff/file-with-head :transient transient--do-exit)
    ("b" "diff vs branch base" +git-diff/file-with-branch-base :transient transient--do-exit)
    ("l" "file log" +git/file-log :transient transient--do-exit)
    ("B" "blame toggle" +git/file-blame-toggle :transient transient--do-exit)
    ("s" "stage file" +git/stage-current-file :transient transient--do-exit)
    ("u" "unstage file" +git/unstage-current-file :transient transient--do-exit)]
   ["Changes"
    ("[" "prev hunk" diff-hl-previous-hunk :transient transient--do-exit)
    ("]" "next hunk" diff-hl-next-hunk :transient transient--do-exit)
    ("h" "show hunk" diff-hl-show-hunk :transient transient--do-exit)
    ("r" "revert hunk" diff-hl-revert-hunk :transient transient--do-exit)
    ("S" "stage hunk" diff-hl-stage-current-hunk :transient transient--do-exit)]
   ["Remote"
    ("y" "copy remote url" browse-at-remote-kill :transient transient--do-exit)
    ("Y" "browse remote url" browse-at-remote :transient transient--do-exit)
    ("f" "file dispatch" magit-file-dispatch :transient transient--do-exit)]
   ["GitHub"
    ("i" "open issue / PR" +github/topic-open :transient transient--do-exit)
    ("I" "insert #reference" +github/topic-insert :transient transient--do-exit)
    ("a" "topics by author" +github/topic-open-by-author :transient transient--do-exit)
    ("@" "insert contributor" +github/contributor-insert :transient transient--do-exit)
    ("P" "preview PR patch" +github/pr-patch-preview :transient transient--do-exit)
    ("R" "refresh topics" +github/refresh :transient transient--do-exit)]])

;; The awesome git client
;;
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
(use-package magit
  :ensure t
  :hook (git-commit-setup . +git-commit-setup-flyspell-h)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-paint-whitespace nil)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-section-visibility-indicator '("▸" . "▾")))

;; From Doom: URLs in Git's output (the pull-request link a push prints) can
;; be followed from the process buffer.
(add-hook 'magit-process-mode-hook #'goto-address-mode)

;; From Doom (magit/magit#5320): Emacs's long-line shortcuts narrow a large
;; status buffer around point, which breaks section motion and TAB there.
(setq-hook! 'magit-status-mode-hook long-line-threshold nil)

(defvar-local +magit--logical-root nil
  "Logical repository root captured when a Magit status opens from `/fs:'.")

(defun +magit-status-logical-root-a (function &rest arguments)
  "Remember the logical origin of a Magit status buffer.
Magit may choose a physical TRAMP path for its Git processes.  Keeping the
source target ID lets later worktree visits return to the original namespace
even when no Remote workspace has been opened explicitly."
  (let* ((origin
          (cond
           ((remote-fs-file-name-p (car arguments))
            (car arguments))
           ((remote-fs-file-name-p default-directory)
            default-directory)))
         (buffer (apply function arguments)))
    (when (and origin (buffer-live-p buffer))
      (with-current-buffer buffer
        (setq-local +magit--logical-root
                    (remote-make-file-name
                     (remote-fs-target-id origin)
                     (remote-file-local-name
                      (or (ignore-errors (magit-toplevel)) origin))))))
    buffer))

(defun +magit-find-worktree-file-logical-a (function &rest arguments)
  "Visit Magit worktree files through their owning logical identity.
Magit may keep its status buffer on a physical TRAMP route for Git process
compatibility.  Source buffers opened from that status must retain the stable
logical file identity; a shared local filesystem still uses its native path.
Unknown revisions and changed upstream call shapes use FUNCTION unchanged."
  (let ((revision (car arguments))
        (file (cadr arguments)))
    (if (and (memq (length arguments) '(2 3 4))
             (or (null revision) (equal revision "{worktree}"))
             (stringp file))
        (let* ((topdir (ignore-errors (magit-toplevel)))
               (absolute (and topdir (expand-file-name file topdir)))
               (origin +magit--logical-root)
               (logical
                (and absolute
                     (ignore-errors
                       (if origin
                           (remote-make-file-name
                            (remote-fs-target-id origin)
                            (remote-file-local-name absolute))
                         (remote-canonicalize-emacs-file-name absolute)))))
               (owner (and logical (not origin)
                           (remote-workspace-for-path logical))))
          (if (and logical
                   (or origin (remote-workspace-live-p owner)))
              (apply function revision
                     (or (ignore-errors (remote-client-file-name logical))
                         logical)
                     (cddr arguments))
            (apply function arguments)))
      (apply function arguments))))

(with-eval-after-load 'magit-files
  (require 'remote-workspace)
  (when (and (fboundp 'magit-find-file-noselect)
             (not (advice-member-p
                   #'+magit-find-worktree-file-logical-a
                   'magit-find-file-noselect)))
    (advice-add 'magit-find-file-noselect :around
                #'+magit-find-worktree-file-logical-a)))

(with-eval-after-load 'magit-status
  (when (and (fboundp 'magit-status-setup-buffer)
             (not (advice-member-p
                   #'+magit-status-logical-root-a
                   'magit-status-setup-buffer)))
    (advice-add 'magit-status-setup-buffer :around
                #'+magit-status-logical-root-a
                '((depth . -100)))))

;; NOTE: `diff-hl' depends on `vc'
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-allow-async-diff t)
  (vc-handled-backends '(Git)))

;; diff-hl sizes its fringe bitmaps to the frame's character height, so a
;; taller line (CJK text, an Org heading or table) is only partly covered and
;; consecutive changed lines show gaps.  Emacs crops a bitmap taller than its
;; row (centred, or from the top or bottom edge as aligned), so taller hunk
;; bitmaps fill any row.  Single-line hunks have no neighbour and keep theirs.
(defun +git--diff-hl-tall-bitmaps-a (&rest _)
  "Redefine diff-hl's top, middle and bottom bitmaps taller than any line."
  (when (and (display-graphic-p) (boundp 'diff-hl-bmp-max-width))
    (let* ((w (min (or (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                       0)
                   diff-hl-bmp-max-width))
           (w (if (zerop w) diff-hl-bmp-max-width w))
           (h (min 255 (max 64 (* 6 (frame-char-height)))))
           (middle (make-vector h (expt 2 (1- w))))
           (top (copy-sequence middle))
           (bottom (copy-sequence middle)))
      (aset top 0 (1- (expt 2 w)))
      (aset bottom (1- h) (1- (expt 2 w)))
      (define-fringe-bitmap 'diff-hl-bmp-top top h w 'top)
      (define-fringe-bitmap 'diff-hl-bmp-middle middle h w 'center)
      (define-fringe-bitmap 'diff-hl-bmp-bottom bottom h w 'bottom))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure t
  :defer 2
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (advice-add 'diff-hl-define-bitmaps :after #'+git--diff-hl-tall-bitmaps-a)
  (+git--diff-hl-tall-bitmaps-a)
  (global-diff-hl-mode 1)
  (when (fboundp 'diff-hl-show-hunk-mouse-mode)
    (diff-hl-show-hunk-mouse-mode 1))
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  :custom
  (diff-hl-side 'left)
  (diff-hl-update-async t))

(with-eval-after-load 'diff-hl-dired
  (advice-add 'diff-hl-dired-status-files
              :around #'+diff-hl-dired-status-files-with-full-output-a))

(defvar +git-ui--face-theme-signature nil
  "Last theme signature applied by `+git-apply-ui'.")

(defun +git-apply-ui ()
  "Apply local diff indicator faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature +git-ui--face-theme-signature)
        (setq +git-ui--face-theme-signature signature)
        (when (facep 'magit-section-heading)
          (aaron-ui-set-face 'magit-section-heading
                             :foreground 'fg-dim
                             :weight 'medium))
        (when (facep 'magit-section-highlight)
          (aaron-ui-set-face 'magit-section-highlight
                             :background 'bg-overlay
                             :extend t))
        (when (facep 'magit-diff-context)
          (aaron-ui-set-face 'magit-diff-context
                             :foreground 'fg-muted
                             :background 'bg-elevated))
        (when (facep 'magit-diff-context-highlight)
          (aaron-ui-set-face 'magit-diff-context-highlight
                             :foreground 'fg-soft
                             :background 'bg-elevated-strong))
        (when (facep 'magit-diff-added)
          (aaron-ui-set-face 'magit-diff-added
                             :foreground 'accent-green-soft
                             :background 'bg-success))
        (when (facep 'magit-diff-added-highlight)
          (aaron-ui-set-face 'magit-diff-added-highlight
                             :foreground 'accent-green-strong
                             :background 'bg-success-strong))
        (when (facep 'magit-diff-removed)
          (aaron-ui-set-face 'magit-diff-removed
                             :foreground 'accent-red
                             :background 'bg-danger))
        (when (facep 'magit-diff-removed-highlight)
          (aaron-ui-set-face 'magit-diff-removed-highlight
                             :foreground 'accent-red-strong
                             :background 'bg-danger-strong))
        (when (facep 'magit-branch-local)
          (aaron-ui-set-face 'magit-branch-local
                             :foreground 'accent-cyan
                             :weight 'medium))
        (when (facep 'magit-branch-remote)
          (aaron-ui-set-face 'magit-branch-remote
                             :foreground 'accent-green-soft
                             :weight 'medium))
        (when (facep 'magit-hash)
          (aaron-ui-set-face 'magit-hash
                             :foreground 'fg-faint))
        (when (facep 'diff-hl-insert)
          (aaron-ui-set-face 'diff-hl-insert
                             :foreground 'accent-green-soft
                             :background 'accent-green-soft))
        (when (facep 'diff-hl-change)
          (aaron-ui-set-face 'diff-hl-change
                             :foreground 'accent-cyan
                             :background 'accent-cyan))
        (when (facep 'diff-hl-delete)
          (aaron-ui-set-face 'diff-hl-delete
                             :foreground 'accent-red-soft
                             :background 'accent-red-soft))))))

(add-hook 'after-init-hook #'+git-apply-ui)
(add-hook 'server-after-make-frame-hook #'+git-apply-ui)
(add-hook 'after-load-theme-hook #'+git-apply-ui)

;; Setup gitignore mode
(use-package conf-mode
  :ensure nil
  :mode (("\\.gitignore\\'"     . conf-unix-mode)
         ("\\.gitconfig\\'"     . conf-unix-mode)
         ("\\.gitattributes\\'" . conf-unix-mode)))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

(use-package browse-at-remote
  :ensure t
  :commands (browse-at-remote
             browse-at-remote-kill))

(define-leader-key!
  "g"   '(:ignore t :which-key "git")
  "g ." '(:def git-menu :which-key "git hub")
  "g g" '(:def magit-status :which-key "status")
  "g w" '(:def +git/board :which-key "git board")
  "g t" '(:def +git/tree :which-key "git tree")
  "g l" '(:def +git/file-log :which-key "file log")
  "g B" '(:def +git/file-blame-toggle :which-key "blame toggle")
  "g S" '(:def +git/stage-current-file :which-key "stage file")
  "g U" '(:def +git/unstage-current-file :which-key "unstage file")
  "g ]" '(:def diff-hl-next-hunk :which-key "next hunk")
  "g [" '(:def diff-hl-previous-hunk :which-key "previous hunk")
  "g r" '(:def diff-hl-revert-hunk :which-key "revert hunk")
  "g s" '(:def diff-hl-stage-current-hunk :which-key "stage hunk")
  "g h" '(:def diff-hl-show-hunk :which-key "show hunk")
  "g y" '(:def browse-at-remote-kill :which-key "copy remote url")
  "g Y" '(:def browse-at-remote :which-key "browse remote url"))

(provide 'tools/git/core)
;;; tools/git/core.el ends here

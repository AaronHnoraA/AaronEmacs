;;; tools/git/tree.el --- Git history tree view -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'ansi-color)

(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function magit-show-commit "magit-diff" (rev &optional args files module))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function +git-tool-prepare-buffer "tools/git/core" (&optional origin-buffer))
(declare-function +git-tool-quit-buffer "tools/git/core" (origin-buffer))

(defvar +git-tree-buffer-name "*Git Tree*"
  "Buffer name used by the Git tree view.")

(defvar-local +git-tree-origin-buffer nil
  "Origin buffer for the current Git tree view.")

(defvar +git-tree-commit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'+git-tree/visit-commit)
    (define-key map (kbd "RET") #'+git-tree/visit-commit)
    map)
  "Keymap used on commit hashes inside Git tree buffers.")

(define-derived-mode +git-tree-mode special-mode "Git-Tree"
  "Major mode for visualizing the Git commit tree.")

(defun +git-tree--current-repo-root ()
  "Return the current repository root or signal a user error."
  (require 'magit-process)
  (require 'magit-git)
  (let ((repo-root (magit-toplevel default-directory)))
    (unless repo-root
      (user-error "Current buffer is not inside a Git repository"))
    repo-root))

(defun +git-tree--commit-at-point ()
  "Return the commit hash at point, or nil if none is present."
  (or (get-text-property (point) '+git-commit)
      (save-excursion
        (beginning-of-line)
        (or (get-text-property (point) '+git-commit)
            (let ((line-end (line-end-position)))
              (when (re-search-forward "\\b[0-9a-f]\\{7,40\\}\\b" line-end t)
                (match-string-no-properties 0)))))))

(defun +git-tree/visit-commit ()
  "Open the commit at point in Magit."
  (interactive)
  (let ((commit (+git-tree--commit-at-point)))
    (unless commit
      (user-error "No commit at point"))
    (require 'magit-process)
    (require 'magit-diff)
    (magit-show-commit commit)))

(defun +git-tree/next-commit ()
  "Move point to the next commit line."
  (interactive)
  (let ((start (point)))
    (catch 'done
      (while (not (eobp))
        (forward-line 1)
        (when (+git-tree--commit-at-point)
          (throw 'done t)))
      (goto-char start)
      (user-error "No next commit"))))

(defun +git-tree/previous-commit ()
  "Move point to the previous commit line."
  (interactive)
  (let ((start (point)))
    (catch 'done
      (while (> (line-beginning-position) (point-min))
        (forward-line -1)
        (when (+git-tree--commit-at-point)
          (throw 'done t)))
      (goto-char start)
      (user-error "No previous commit"))))

(defun +git-tree/copy-hash ()
  "Copy the commit hash at point."
  (interactive)
  (let ((commit (+git-tree--commit-at-point)))
    (unless commit
      (user-error "No commit at point"))
    (kill-new commit)
    (message "Copied %s" commit)))

(defun +git-tree/quit ()
  "Quit the Git tree buffer and return to its origin buffer."
  (interactive)
  (+git-tool-quit-buffer +git-tree-origin-buffer))

(defun +git-tree--annotate-commits ()
  "Annotate commit hashes in the current Git tree buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (not (eobp))
      (let ((line-beg (line-beginning-position))
            (line-end (line-end-position)))
        (goto-char line-beg)
        (when (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" line-end t)
          (let ((commit (match-string-no-properties 1))
                (beg (match-beginning 1))
                (end (match-end 1)))
            ;; Mark the whole line with the commit id so `RET' works anywhere
            ;; on that commit line, while keeping the hash itself clickable.
            (add-text-properties line-beg line-end
                                 `(+git-commit ,commit))
            (add-text-properties beg end
                                 `(+git-commit ,commit
                                   keymap ,+git-tree-commit-keymap
                                   mouse-face highlight
                                   help-echo "RET/mouse-1: show commit"))))
        (goto-char line-end)
        (forward-line 1)))))

(defun +git-tree/refresh ()
  "Refresh the Git tree view for the current repository."
  (interactive)
  (let ((inhibit-read-only t)
        (repo-root default-directory))
    (erase-buffer)
    (insert (format "Repository: %s\n" repo-root))
    (insert "Keys: RET/o show commit, g refresh, q quit\n\n")
    (let ((exit-code
           (process-file "git" nil t nil
                         "log" "--graph" "--decorate" "--oneline" "--all"
                         "--color=always" "--date-order")))
      (unless (zerop exit-code)
        (goto-char (point-max))
        (insert (format "\n[git log exited with code %s]\n" exit-code))))
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (+git-tree--annotate-commits)
    (goto-char (point-min))
    (forward-line 2)))

(defun +git/tree ()
  "Open a visual Git commit tree for the current repository."
  (interactive)
  (require 'magit-process)
  (require 'magit-git)
  (let* ((origin-buffer (current-buffer))
         (repo-root (+git-tree--current-repo-root))
         (buffer (get-buffer-create +git-tree-buffer-name)))
    (with-current-buffer buffer
      (+git-tree-mode)
      (setq-local default-directory repo-root)
      (setq-local +git-tree-origin-buffer origin-buffer)
      (+git-tool-prepare-buffer origin-buffer)
      (let ((map (copy-keymap special-mode-map)))
        (use-local-map map)
        (local-set-key (kbd "g") #'+git-tree/refresh)
        (local-set-key (kbd "n") #'+git-tree/next-commit)
        (local-set-key (kbd "p") #'+git-tree/previous-commit)
        (local-set-key (kbd "y") #'+git-tree/copy-hash)
        (local-set-key (kbd "q") #'+git-tree/quit)
        (local-set-key (kbd "RET") #'+git-tree/visit-commit)
        (local-set-key (kbd "<return>") #'+git-tree/visit-commit)
        (local-set-key (kbd "o") #'+git-tree/visit-commit))
      (when (featurep 'evil)
        (dolist (state '(normal motion visual))
          (evil-local-set-key state (kbd "q") #'+git-tree/quit)
          (evil-local-set-key state (kbd "g") #'+git-tree/refresh)
          (evil-local-set-key state (kbd "n") #'+git-tree/next-commit)
          (evil-local-set-key state (kbd "p") #'+git-tree/previous-commit)
          (evil-local-set-key state (kbd "y") #'+git-tree/copy-hash)
          (evil-local-set-key state (kbd "RET") #'+git-tree/visit-commit)
          (evil-local-set-key state (kbd "<return>") #'+git-tree/visit-commit)
          (evil-local-set-key state (kbd "o") #'+git-tree/visit-commit)))
      (+git-tree/refresh))
    (switch-to-buffer buffer)))

(provide 'tools/git/tree)
;;; tools/git/tree.el ends here

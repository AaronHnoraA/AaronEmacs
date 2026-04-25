;;; ai-workbench-profile.el --- Profile prompt loading for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loads editable ai-workbench profile prompts from etc/ai-workbench/profiles.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-vendor)

(defgroup ai-workbench-profile nil
  "Profile prompt support for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-profile-")

(defcustom ai-workbench-profile-search-path
  (list (expand-file-name "etc/ai-workbench/profiles/" user-emacs-directory))
  "Directories searched for ai-workbench profile prompt text files.
Earlier entries take precedence over later ones."
  :type '(repeat directory)
  :group 'ai-workbench-profile)

(defcustom ai-workbench-profile-snippet-search-path
  (list (expand-file-name "etc/ai-workbench/snippets/" user-emacs-directory))
  "Directories searched for shared ai-workbench snippet text files.
Earlier entries take precedence over later ones."
  :type '(repeat directory)
  :group 'ai-workbench-profile)

(defcustom ai-workbench-profile-default-name "default"
  "Default ai-workbench profile name."
  :type 'string
  :group 'ai-workbench-profile)

(defcustom ai-workbench-profile-summary-max-width 72
  "Maximum width used when displaying one-line profile summaries."
  :type 'integer
  :group 'ai-workbench-profile)

(defconst ai-workbench-profile--fallback-default
  "You are working inside this Emacs project.

Treat the current terminal working directory as authoritative. When Emacs provides @file, @line, @range, @defun, @block, @project-root, @git-status, or @diagnostics references, resolve them against this project instead of asking the user to paste code again.

Prefer direct execution, explain risky changes briefly, and keep edits scoped to the active project."
  "Fallback default profile text when no etc profile file exists.")

(defconst ai-workbench-profile--git-policy
  (string-join
   '("Git usage rules:"
     "- Treat the current worktree as user-owned; do not discard, overwrite, or revert user changes."
     "- Do not run destructive Git operations such as reset --hard, checkout --, clean, rebase, amend, or force push unless the user explicitly asks."
     "- Do not create commits, branches, tags, stashes, or pushes unless the user explicitly asks."
     "- Prefer direct file edits in the active project over Git-heavy workflows."
     "- When Git context is relevant, inspect status first and explain any repo risk briefly.")
   "\n")
  "Standard Git policy injected into every ai-workbench profile prompt.")

(defconst ai-workbench-profile--activation-policy
  (string-join
   '("Activation reply protocol:"
     "After reading this profile injection, your immediate next reply must begin with:"
     "已开启 Emacs 特调模式"
     "Then briefly confirm the active profile, working directory, and that you will follow the Git usage rules above."
     "Keep that activation reply concise.")
   "\n")
  "Standard activation protocol injected into every ai-workbench profile prompt.")

(defconst ai-workbench-profile--write-policy
  (string-join
   '("Write approval rules:"
     "- In Emacs special-tuned mode, do not write code immediately."
     "- Before each modification block, first state the target files or ranges and the exact planned change."
     "- Wait for explicit user approval before writing each modification block."
     "- If approval is not given, stop at the plan and do not modify files.")
   "\n")
  "Standard write approval policy injected into every ai-workbench profile prompt.")

(defun ai-workbench-profile--normalized-directories (directories)
  "Return normalized DIRECTORY list with duplicates removed."
  (delete-dups
   (mapcar #'file-name-as-directory
           (seq-filter #'identity directories))))

(defun ai-workbench-profile-directories ()
  "Return normalized profile directories."
  (ai-workbench-profile--normalized-directories
   ai-workbench-profile-search-path))

(defun ai-workbench-profile-snippet-directories ()
  "Return normalized shared snippet directories."
  (ai-workbench-profile--normalized-directories
   ai-workbench-profile-snippet-search-path))

(defun ai-workbench-profile-primary-directory ()
  "Return the preferred directory for editing and creating profiles."
  (or (car (ai-workbench-profile-directories))
      (expand-file-name "etc/ai-workbench/profiles/" user-emacs-directory)))

(defun ai-workbench-profile-file (profile)
  "Return the preferred text file path for PROFILE."
  (expand-file-name (format "%s.txt" profile)
                    (ai-workbench-profile-primary-directory)))

(defun ai-workbench-profile-locate-file (profile)
  "Return the first readable text file path for PROFILE."
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" profile) dir))
    (ai-workbench-profile-directories))))

(defun ai-workbench-profile-snippet-file (name)
  "Return the preferred text file path for shared snippet NAME."
  (expand-file-name (format "%s.txt" name)
                    (or (car (ai-workbench-profile-snippet-directories))
                        (expand-file-name "etc/ai-workbench/snippets/"
                                          user-emacs-directory))))

(defun ai-workbench-profile-locate-snippet-file (name)
  "Return the first readable text file path for shared snippet NAME."
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" name) dir))
    (ai-workbench-profile-snippet-directories))))

(defun ai-workbench-profile-read-snippet (name fallback)
  "Return shared snippet NAME, or FALLBACK when no file is found."
  (let ((file (ai-workbench-profile-locate-snippet-file name)))
    (if (and file (file-readable-p file))
        (string-trim
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))
      fallback)))

(defun ai-workbench-profile-snippet-names ()
  "Return available shared snippet names."
  (let ((names
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (mapcar
               (lambda (file)
                 (file-name-base file))
               (directory-files dir t "\\.txt\\'"))))
          (ai-workbench-profile-snippet-directories))))
    (delete-dups
     (append '("git-policy" "activation-policy" "write-policy")
             names))))

(defun ai-workbench-profile-edit-snippet (name)
  "Open shared snippet NAME for editing."
  (interactive
   (list (completing-read "Edit snippet: "
                          (ai-workbench-profile-snippet-names)
                          nil t nil nil "git-policy")))
  (let ((dir (or (car (ai-workbench-profile-snippet-directories))
                 (expand-file-name "etc/ai-workbench/snippets/"
                                   user-emacs-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file (expand-file-name (format "%s.txt" name) dir))))

(defun ai-workbench-profile-read (profile)
  "Return the prompt text for PROFILE."
  (let ((file (ai-workbench-profile-locate-file profile)))
    (if (and file (file-readable-p file))
        (string-trim
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))
      (if (string= profile ai-workbench-profile-default-name)
          ai-workbench-profile--fallback-default
        ""))))

(defun ai-workbench-profile-names ()
  "Return available profile names."
  (let ((profiles
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (mapcar
               (lambda (file)
                 (file-name-base file))
               (directory-files dir t "\\.txt\\'"))))
          (ai-workbench-profile-directories))))
    (delete-dups
     (cons ai-workbench-profile-default-name profiles))))

(defun ai-workbench-profile-ensure-directory ()
  "Ensure the primary profile directory exists and return it."
  (let ((dir (ai-workbench-profile-primary-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun ai-workbench-profile-read-name (&optional prompt default-name)
  "Read a profile name with PROMPT and DEFAULT-NAME."
  (completing-read (or prompt "Profile: ")
                   (ai-workbench-profile-names)
                   nil nil nil nil
                   (or default-name ai-workbench-profile-default-name)))

(defun ai-workbench-profile-open (profile)
  "Open PROFILE for editing, creating it in the primary profile directory when needed."
  (interactive
   (list (ai-workbench-profile-read-name
          "Edit profile: "
          (ai-workbench-session-profile))))
  (find-file (expand-file-name (format "%s.txt" profile)
                               (ai-workbench-profile-ensure-directory))))

(defun ai-workbench-profile-summary (profile)
  "Return a one-line summary for PROFILE."
  (let* ((text (ai-workbench-profile-read profile))
         (lines (split-string text "\n" t "[ \t]+"))
         (summary (or (car lines) "")))
    (if (string-empty-p summary)
        "Empty profile"
      (truncate-string-to-width summary
                                ai-workbench-profile-summary-max-width
                                nil nil t))))

(defun ai-workbench-profile-candidates ()
  "Return completion candidates with inline summaries."
  (mapcar
   (lambda (name)
     (cons (format "%s  --  %s"
                   name
                   (ai-workbench-profile-summary name))
           name))
   (ai-workbench-profile-names)))

(defun ai-workbench-profile-read-name-with-summary (&optional prompt default-name)
  "Read a profile name with PROMPT, DEFAULT-NAME, and inline summary text."
  (let* ((candidates (ai-workbench-profile-candidates))
         (display (completing-read (or prompt "Profile: ")
                                   candidates
                                   nil nil nil nil
                                   (when default-name
                                     (car (rassoc default-name candidates))))))
    (or (cdr (assoc display candidates))
        default-name
        ai-workbench-profile-default-name)))

(defun ai-workbench-profile-preview (profile)
  "Preview PROFILE in a read-only buffer."
  (interactive
   (list (ai-workbench-profile-read-name-with-summary
          "Preview profile: "
          (ai-workbench-session-profile))))
  (let ((buffer (get-buffer-create (format "*AI Profile: %s*" profile))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Profile: %s\n\n" profile))
      (insert (format "Source: %s\n\n"
                      (abbreviate-file-name
                       (or (ai-workbench-profile-locate-file profile)
                           (ai-workbench-profile-file profile)))))
      (insert (or (ai-workbench-profile-read profile) ""))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(defun ai-workbench-profile-create (name &optional base-profile)
  "Create NAME from BASE-PROFILE when provided, then open it."
  (interactive
   (list (read-string "New profile name: ")
         (let ((base (ai-workbench-profile-read-name-with-summary
                      "Base profile: "
                      ai-workbench-profile-default-name)))
           (unless (string-empty-p base)
             base))))
  (when (string-empty-p (string-trim name))
    (user-error "Profile name cannot be empty"))
  (let ((file (ai-workbench-profile-file name))
        (base-text (and base-profile
                        (ai-workbench-profile-read base-profile))))
    (ai-workbench-profile-ensure-directory)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (if (string-empty-p (or base-text ""))
                    (format "Describe the intended workflow for profile `%s'.\n" name)
                  base-text))
        (unless (bolp)
          (insert "\n"))))
    (find-file file)))

(defun ai-workbench-profile-build-prompt (&optional project-root)
  "Return the one-time profile prompt for PROJECT-ROOT."
  (let* ((root (or project-root default-directory))
         (profile (or (ai-workbench-session-profile root)
                      ai-workbench-profile-default-name))
         (profile-text (ai-workbench-profile-read profile))
         (git-policy (ai-workbench-profile-read-snippet
                      "git-policy"
                      ai-workbench-profile--git-policy))
         (write-policy (ai-workbench-profile-read-snippet
                        "write-policy"
                        ai-workbench-profile--write-policy))
         (activation-policy (ai-workbench-profile-read-snippet
                             "activation-policy"
                             ai-workbench-profile--activation-policy)))
    (string-join
     (delq nil
           (list "AI Workbench global profile"
                 (format "Working directory: %s" (abbreviate-file-name root))
                 (format "Profile: %s" profile)
                 git-policy
                 write-policy
                 activation-policy
                 (unless (string-empty-p profile-text)
                   profile-text)))
     "\n\n")))

(defun ai-workbench-profile-bootstrap-prompt (&optional project-root)
  "Return the bootstrap handshake prompt for PROJECT-ROOT."
  (string-join
   (list "Read and remember the following AI Workbench profile now."
         "Do not execute any task yet."
         "Reply only with the required activation acknowledgement, then stop."
         (ai-workbench-profile-build-prompt project-root))
   "\n\n"))

(defun ai-workbench-profile-wrap-user-prompt (prompt &optional project-root)
  "Return PROMPT wrapped with profile instructions for PROJECT-ROOT."
  (string-join
   (list "Before handling the user task below, read and apply this AI Workbench profile immediately."
         "Your reply must start with the required activation acknowledgement, then continue with the approved scope only."
         (ai-workbench-profile-build-prompt project-root)
         "User task:"
         prompt)
   "\n\n"))

(provide 'ai-workbench-profile)
;;; ai-workbench-profile.el ends here

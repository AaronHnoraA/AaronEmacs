;;; ai-workbench-profile.el --- Profile prompt loading for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loads editable ai-workbench profile prompts from etc/ai-workbench/profiles.

;;; Code:

(require 'subr-x)
(require 'ai-workbench-session)

(defgroup ai-workbench-profile nil
  "Profile prompt support for ai-workbench."
  :group 'ai-workbench
  :prefix "ai-workbench-profile-")

(defcustom ai-workbench-profile-directory
  (expand-file-name "etc/ai-workbench/profiles/" user-emacs-directory)
  "Directory containing ai-workbench profile prompt text files."
  :type 'directory
  :group 'ai-workbench-profile)

(defcustom ai-workbench-profile-default-name "default"
  "Default ai-workbench profile name."
  :type 'string
  :group 'ai-workbench-profile)

(defconst ai-workbench-profile--fallback-default
  "You are working inside this Emacs project.

Treat the current terminal working directory as authoritative. When Emacs provides @file, @line, @range, @defun, @block, @project-root, @git-status, or @diagnostics references, resolve them against this project instead of asking the user to paste code again.

Prefer concise diffs, explain risky changes briefly, and keep edits scoped to the active project."
  "Fallback default profile text when no etc profile file exists.")

(defun ai-workbench-profile-file (profile)
  "Return the text file path for PROFILE."
  (expand-file-name (format "%s.txt" profile)
                    ai-workbench-profile-directory))

(defun ai-workbench-profile-read (profile)
  "Return the prompt text for PROFILE."
  (let ((file (ai-workbench-profile-file profile)))
    (if (file-readable-p file)
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
         (when (file-directory-p ai-workbench-profile-directory)
           (mapcar
            (lambda (file)
              (file-name-base file))
            (directory-files ai-workbench-profile-directory t "\\.txt\\'")))))
    (delete-dups
     (cons ai-workbench-profile-default-name profiles))))

(defun ai-workbench-profile-build-prompt (&optional project-root)
  "Return the one-time profile prompt for PROJECT-ROOT."
  (let* ((root (or project-root default-directory))
         (profile (or (ai-workbench-session-profile root)
                      ai-workbench-profile-default-name))
         (profile-text (ai-workbench-profile-read profile)))
    (string-join
     (delq nil
           (list "AI Workbench global profile"
                 (format "Working directory: %s" (abbreviate-file-name root))
                 (format "Profile: %s" profile)
                 (unless (string-empty-p profile-text)
                   profile-text)))
     "\n\n")))

(provide 'ai-workbench-profile)
;;; ai-workbench-profile.el ends here

;;; ai-workbench-profile.el --- Profile prompt loading for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loads editable ai-workbench profile prompts from etc/ai-workbench/profiles.

;;; Code:

(require 'subr-x)
(require 'seq)
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

(defcustom ai-workbench-profile-template-search-path
  (list (expand-file-name "etc/ai-workbench/templates/" user-emacs-directory))
  "Directories searched for ai-workbench prompt template text files.
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

(defcustom ai-workbench-profile-max-text-file-bytes (* 1024 1024)
  "Maximum size of editable ai-workbench text files.
This applies to profiles, snippets, and templates."
  :type 'integer
  :group 'ai-workbench-profile)

(defconst ai-workbench-profile--empty-fallback ""
  "Fallback used only when editable etc text files are unavailable.")

(defconst ai-workbench-profile--profile-template-fallback
  "{{git-policy}}\n\n{{write-policy}}\n\n{{activation-policy}}\n\n{{profile-text}}"
  "Minimal profile template used only when editable templates are unavailable.")

(defconst ai-workbench-profile--bootstrap-template-fallback "{{profile-prompt}}"
  "Minimal bootstrap template used only when editable templates are unavailable.")

(defconst ai-workbench-profile--user-template-fallback
  "{{profile-prompt}}\n\n{{user-prompt}}"
  "Minimal user wrapper template used only when editable templates are unavailable.")

(defvar ai-workbench-profile--file-cache (make-hash-table :test 'equal)
  "Cache for editable profile, snippet, and template text files.")

(defun ai-workbench-profile--safe-name-p (name)
  "Return non-nil when NAME is safe for an etc text file basename."
  (and (stringp name)
       (string-match-p "\\`[[:alnum:]_.-]+\\'" name)
       (not (member name '("." "..")))))

(defun ai-workbench-profile--validate-name (name kind)
  "Signal an error unless NAME is a safe KIND basename."
  (unless (ai-workbench-profile--safe-name-p name)
    (user-error "Invalid ai-workbench %s name: %s" kind name))
  name)

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

(defun ai-workbench-profile-template-directories ()
  "Return normalized template directories."
  (ai-workbench-profile--normalized-directories
   ai-workbench-profile-template-search-path))

(defun ai-workbench-profile-primary-directory ()
  "Return the preferred directory for editing and creating profiles."
  (or (car (ai-workbench-profile-directories))
      (expand-file-name "etc/ai-workbench/profiles/" user-emacs-directory)))

(defun ai-workbench-profile-file (profile)
  "Return the preferred text file path for PROFILE."
  (ai-workbench-profile--validate-name profile "profile")
  (expand-file-name (format "%s.txt" profile)
                    (ai-workbench-profile-primary-directory)))

(defun ai-workbench-profile-locate-file (profile)
  "Return the first readable text file path for PROFILE."
  (ai-workbench-profile--validate-name profile "profile")
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" profile) dir))
    (ai-workbench-profile-directories))))

(defun ai-workbench-profile-snippet-file (name)
  "Return the preferred text file path for shared snippet NAME."
  (ai-workbench-profile--validate-name name "snippet")
  (expand-file-name (format "%s.txt" name)
                    (or (car (ai-workbench-profile-snippet-directories))
                        (expand-file-name "etc/ai-workbench/snippets/"
                                          user-emacs-directory))))

(defun ai-workbench-profile-locate-snippet-file (name)
  "Return the first readable text file path for shared snippet NAME."
  (ai-workbench-profile--validate-name name "snippet")
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" name) dir))
    (ai-workbench-profile-snippet-directories))))

(defun ai-workbench-profile-template-file (name)
  "Return the preferred text file path for template NAME."
  (ai-workbench-profile--validate-name name "template")
  (expand-file-name (format "%s.txt" name)
                    (or (car (ai-workbench-profile-template-directories))
                        (expand-file-name "etc/ai-workbench/templates/"
                                          user-emacs-directory))))

(defun ai-workbench-profile-locate-template-file (name)
  "Return the first readable text file path for template NAME."
  (ai-workbench-profile--validate-name name "template")
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" name) dir))
    (ai-workbench-profile-template-directories))))

(defun ai-workbench-profile-template-names ()
  "Return available prompt template names."
  (let ((names
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (seq-filter
               #'ai-workbench-profile--safe-name-p
               (mapcar
                (lambda (file)
                  (file-name-base file))
                (directory-files dir t "\\.txt\\'")))))
          (ai-workbench-profile-template-directories))))
    (delete-dups
     (append '("profile-prompt"
               "bootstrap-prompt"
               "user-prompt-wrapper"
               "context-prompt"
               "writing-prompt")
             names))))

(defun ai-workbench-profile--read-text-file (file)
  "Return trimmed contents of FILE."
  (let* ((expanded (expand-file-name file))
         (attributes (file-attributes expanded))
         (size (file-attribute-size attributes))
         (signature (list (file-attribute-modification-time attributes) size))
         (cached (gethash expanded ai-workbench-profile--file-cache)))
    (when (and size
               ai-workbench-profile-max-text-file-bytes
               (> size ai-workbench-profile-max-text-file-bytes))
      (user-error "ai-workbench text file is too large: %s" expanded))
    (if (and cached (equal (car cached) signature))
        (cdr cached)
      (let ((text (string-trim
                   (with-temp-buffer
                     (insert-file-contents expanded)
                     (buffer-string)))))
        (puthash expanded (cons signature text) ai-workbench-profile--file-cache)
        text))))

(defun ai-workbench-profile-clear-cache ()
  "Clear cached editable ai-workbench text files."
  (interactive)
  (clrhash ai-workbench-profile--file-cache))

(defun ai-workbench-profile-read-snippet (name fallback)
  "Return shared snippet NAME, or FALLBACK when no file is found."
  (let ((file (ai-workbench-profile-locate-snippet-file name)))
    (if (and file (file-readable-p file))
        (ai-workbench-profile--read-text-file file)
      fallback)))

(defun ai-workbench-profile-read-template (name fallback)
  "Return template NAME, or FALLBACK when no file is found."
  (let ((file (ai-workbench-profile-locate-template-file name)))
    (if (and file (file-readable-p file))
        (ai-workbench-profile--read-text-file file)
      fallback)))

(defun ai-workbench-profile-render-template (template variables)
  "Render TEMPLATE by replacing VARIABLES of the form {{name}}."
  (let ((rendered template))
    (dolist (pair variables)
      (setq rendered
            (replace-regexp-in-string
             (regexp-quote (format "{{%s}}" (car pair)))
             (or (cdr pair) "")
             rendered t t)))
    (string-trim rendered)))

(defun ai-workbench-profile-snippet-names ()
  "Return available shared snippet names."
  (let ((names
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (seq-filter
               #'ai-workbench-profile--safe-name-p
               (mapcar
                (lambda (file)
                  (file-name-base file))
                (directory-files dir t "\\.txt\\'")))))
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
  (ai-workbench-profile--validate-name name "snippet")
  (let ((dir (or (car (ai-workbench-profile-snippet-directories))
                 (expand-file-name "etc/ai-workbench/snippets/"
                                   user-emacs-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file (expand-file-name (format "%s.txt" name) dir))))

(defun ai-workbench-profile-edit-template (name)
  "Open prompt template NAME for editing."
  (interactive
   (list (completing-read "Edit template: "
                          (ai-workbench-profile-template-names)
                          nil t nil nil "context-prompt")))
  (ai-workbench-profile--validate-name name "template")
  (let ((dir (or (car (ai-workbench-profile-template-directories))
                 (expand-file-name "etc/ai-workbench/templates/"
                                   user-emacs-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file (expand-file-name (format "%s.txt" name) dir))))

(defun ai-workbench-profile-read (profile)
  "Return the prompt text for PROFILE."
  (let ((file (ai-workbench-profile-locate-file profile)))
    (if (and file (file-readable-p file))
        (ai-workbench-profile--read-text-file file)
      (if (string= profile ai-workbench-profile-default-name)
          ai-workbench-profile--empty-fallback
        ""))))

(defun ai-workbench-profile-names ()
  "Return available profile names."
  (let ((profiles
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (seq-filter
               #'ai-workbench-profile--safe-name-p
               (mapcar
                (lambda (file)
                  (file-name-base file))
                (directory-files dir t "\\.txt\\'")))))
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
  "Open PROFILE for editing.
Create it in the primary profile directory when needed."
  (interactive
   (list (ai-workbench-profile-read-name
          "Edit profile: "
          (ai-workbench-session-profile))))
  (ai-workbench-profile--validate-name profile "profile")
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
  (ai-workbench-profile--validate-name name "profile")
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
                      ai-workbench-profile--empty-fallback))
         (write-policy (ai-workbench-profile-read-snippet
                        "write-policy"
                        ai-workbench-profile--empty-fallback))
         (activation-policy (ai-workbench-profile-read-snippet
                             "activation-policy"
                             ai-workbench-profile--empty-fallback))
         (template (ai-workbench-profile-read-template
                    "profile-prompt"
                    ai-workbench-profile--profile-template-fallback)))
    (ai-workbench-profile-render-template
     template
     `(("working-directory" . ,(abbreviate-file-name root))
       ("profile" . ,profile)
       ("git-policy" . ,git-policy)
       ("write-policy" . ,write-policy)
       ("activation-policy" . ,activation-policy)
       ("profile-text" . ,profile-text)))))

(defun ai-workbench-profile-bootstrap-prompt (&optional project-root)
  "Return the bootstrap handshake prompt for PROJECT-ROOT."
  (ai-workbench-profile-render-template
   (ai-workbench-profile-read-template
    "bootstrap-prompt"
    ai-workbench-profile--bootstrap-template-fallback)
   `(("profile-prompt" . ,(ai-workbench-profile-build-prompt project-root)))))

(defun ai-workbench-profile-wrap-user-prompt (prompt &optional project-root)
  "Return PROMPT wrapped with profile instructions for PROJECT-ROOT."
  (ai-workbench-profile-render-template
   (ai-workbench-profile-read-template
    "user-prompt-wrapper"
    ai-workbench-profile--user-template-fallback)
   `(("profile-prompt" . ,(ai-workbench-profile-build-prompt project-root))
     ("user-prompt" . ,prompt))))

(provide 'ai-workbench-profile)
;;; ai-workbench-profile.el ends here

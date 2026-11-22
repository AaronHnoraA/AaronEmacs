;;; tools/agent/interaction-profile.el --- Profile prompt loading for +agent-interaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loads editable +agent-interaction profile prompts from etc/noema/profiles.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'tools/agent/interaction-session)
(require 'noema-upstream)

(defgroup +agent-interaction-profile nil
  "Profile prompt support for +agent-interaction."
  :group '+agent-interaction
  :prefix "+agent-interaction-profile-")

(defcustom +agent-interaction-profile-search-path
  (list (expand-file-name "etc/noema/profiles/" user-emacs-directory))
  "Directories searched for +agent-interaction profile prompt text files.
Earlier entries take precedence over later ones."
  :type '(repeat directory)
  :group '+agent-interaction-profile)

(defcustom +agent-interaction-profile-snippet-search-path
  (list (expand-file-name "etc/noema/snippets/" user-emacs-directory))
  "Directories searched for shared +agent-interaction snippet text files.
Earlier entries take precedence over later ones."
  :type '(repeat directory)
  :group '+agent-interaction-profile)

(defcustom +agent-interaction-profile-template-search-path
  (list (expand-file-name "etc/noema/templates/" user-emacs-directory))
  "Directories searched for +agent-interaction prompt template text files.
Earlier entries take precedence over later ones."
  :type '(repeat directory)
  :group '+agent-interaction-profile)

(defcustom +agent-interaction-profile-default-name "default"
  "Default +agent-interaction profile name."
  :type 'string
  :group '+agent-interaction-profile)

(defcustom +agent-interaction-profile-summary-max-width 72
  "Maximum width used when displaying one-line profile summaries."
  :type 'integer
  :group '+agent-interaction-profile)

(defcustom +agent-interaction-profile-max-text-file-bytes (* 1024 1024)
  "Maximum size of editable +agent-interaction text files.
This applies to profiles, snippets, and templates."
  :type 'integer
  :group '+agent-interaction-profile)

(defconst +agent-interaction-profile--empty-fallback ""
  "Fallback used only when editable etc text files are unavailable.")

(defconst +agent-interaction-profile--profile-template-fallback
  "{{git-policy}}\n\n{{write-policy}}\n\n{{activation-policy}}\n\n{{profile-text}}"
  "Minimal profile template used only when editable templates are unavailable.")

(defconst +agent-interaction-profile--bootstrap-template-fallback "{{profile-prompt}}"
  "Minimal bootstrap template used only when editable templates are unavailable.")

(defconst +agent-interaction-profile--user-template-fallback
  "{{profile-prompt}}\n\n{{user-prompt}}"
  "Minimal user wrapper template used only when editable templates are unavailable.")

(defvar +agent-interaction-profile--file-cache (make-hash-table :test 'equal)
  "Cache for editable profile, snippet, and template text files.")

(defun +agent-interaction-profile--safe-name-p (name)
  "Return non-nil when NAME is safe for an etc text file basename."
  (and (stringp name)
       (string-match-p "\\`[[:alnum:]_.-]+\\'" name)
       (not (member name '("." "..")))))

(defun +agent-interaction-profile--validate-name (name kind)
  "Signal an error unless NAME is a safe KIND basename."
  (unless (+agent-interaction-profile--safe-name-p name)
    (user-error "Invalid +agent-interaction %s name: %s" kind name))
  name)

(defun +agent-interaction-profile--normalized-directories (directories)
  "Return normalized DIRECTORY list with duplicates removed."
  (delete-dups
   (mapcar #'file-name-as-directory
           (seq-filter #'identity directories))))

(defun +agent-interaction-profile-directories ()
  "Return normalized profile directories."
  (+agent-interaction-profile--normalized-directories
   +agent-interaction-profile-search-path))

(defun +agent-interaction-profile-snippet-directories ()
  "Return normalized shared snippet directories."
  (+agent-interaction-profile--normalized-directories
   +agent-interaction-profile-snippet-search-path))

(defun +agent-interaction-profile-template-directories ()
  "Return normalized template directories."
  (+agent-interaction-profile--normalized-directories
   +agent-interaction-profile-template-search-path))

(defun +agent-interaction-profile-primary-directory ()
  "Return the preferred directory for editing and creating profiles."
  (or (car (+agent-interaction-profile-directories))
      (expand-file-name "etc/noema/profiles/" user-emacs-directory)))

(defun +agent-interaction-profile-file (profile)
  "Return the preferred text file path for PROFILE."
  (+agent-interaction-profile--validate-name profile "profile")
  (expand-file-name (format "%s.txt" profile)
                    (+agent-interaction-profile-primary-directory)))

(defun +agent-interaction-profile-locate-file (profile)
  "Return the first readable text file path for PROFILE."
  (+agent-interaction-profile--validate-name profile "profile")
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" profile) dir))
    (+agent-interaction-profile-directories))))

(defun +agent-interaction-profile-snippet-file (name)
  "Return the preferred text file path for shared snippet NAME."
  (+agent-interaction-profile--validate-name name "snippet")
  (expand-file-name (format "%s.txt" name)
                    (or (car (+agent-interaction-profile-snippet-directories))
                        (expand-file-name "etc/noema/snippets/"
                                          user-emacs-directory))))

(defun +agent-interaction-profile-locate-snippet-file (name)
  "Return the first readable text file path for shared snippet NAME."
  (+agent-interaction-profile--validate-name name "snippet")
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" name) dir))
    (+agent-interaction-profile-snippet-directories))))

(defun +agent-interaction-profile-template-file (name)
  "Return the preferred text file path for template NAME."
  (+agent-interaction-profile--validate-name name "template")
  (expand-file-name (format "%s.txt" name)
                    (or (car (+agent-interaction-profile-template-directories))
                        (expand-file-name "etc/noema/templates/"
                                          user-emacs-directory))))

(defun +agent-interaction-profile-locate-template-file (name)
  "Return the first readable text file path for template NAME."
  (+agent-interaction-profile--validate-name name "template")
  (seq-find
   #'file-readable-p
   (mapcar
    (lambda (dir)
      (expand-file-name (format "%s.txt" name) dir))
    (+agent-interaction-profile-template-directories))))

(defun +agent-interaction-profile-template-names ()
  "Return available prompt template names."
  (let ((names
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (seq-filter
               #'+agent-interaction-profile--safe-name-p
               (mapcar
                (lambda (file)
                  (file-name-base file))
                (directory-files dir t "\\.txt\\'")))))
          (+agent-interaction-profile-template-directories))))
    (delete-dups
     (append '("profile-prompt"
               "bootstrap-prompt"
               "user-prompt-wrapper"
               "context-prompt"
               "writing-prompt")
             names))))

(defun +agent-interaction-profile--read-text-file (file)
  "Return trimmed contents of FILE."
  (let* ((expanded (expand-file-name file))
         (attributes (file-attributes expanded))
         (size (file-attribute-size attributes))
         (signature (list (file-attribute-modification-time attributes) size))
         (cached (gethash expanded +agent-interaction-profile--file-cache)))
    (when (and size
               +agent-interaction-profile-max-text-file-bytes
               (> size +agent-interaction-profile-max-text-file-bytes))
      (user-error "+agent-interaction text file is too large: %s" expanded))
    (if (and cached (equal (car cached) signature))
        (cdr cached)
      (let ((text (string-trim
                   (with-temp-buffer
                     (insert-file-contents expanded)
                     (buffer-string)))))
        (puthash expanded (cons signature text) +agent-interaction-profile--file-cache)
        text))))

(defun +agent-interaction-profile-clear-cache ()
  "Clear cached editable +agent-interaction text files."
  (interactive)
  (clrhash +agent-interaction-profile--file-cache))

(defun +agent-interaction-profile-read-snippet (name fallback)
  "Return shared snippet NAME, or FALLBACK when no file is found."
  (let ((file (+agent-interaction-profile-locate-snippet-file name)))
    (if (and file (file-readable-p file))
        (+agent-interaction-profile--read-text-file file)
      fallback)))

(defun +agent-interaction-profile-read-template (name fallback)
  "Return template NAME, or FALLBACK when no file is found."
  (let ((file (+agent-interaction-profile-locate-template-file name)))
    (if (and file (file-readable-p file))
        (+agent-interaction-profile--read-text-file file)
      fallback)))

(defun +agent-interaction-profile-render-template (template variables)
  "Render TEMPLATE by replacing VARIABLES of the form {{name}}."
  (let ((rendered template))
    (dolist (pair variables)
      (setq rendered
            (replace-regexp-in-string
             (regexp-quote (format "{{%s}}" (car pair)))
             (or (cdr pair) "")
             rendered t t)))
    (string-trim rendered)))

(defun +agent-interaction-profile-snippet-names ()
  "Return available shared snippet names."
  (let ((names
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (seq-filter
               #'+agent-interaction-profile--safe-name-p
               (mapcar
                (lambda (file)
                  (file-name-base file))
                (directory-files dir t "\\.txt\\'")))))
          (+agent-interaction-profile-snippet-directories))))
    (delete-dups
     (append '("git-policy" "activation-policy" "write-policy")
             names))))

(defun +agent-interaction-profile-edit-snippet (name)
  "Open shared snippet NAME for editing."
  (interactive
   (list (completing-read "Edit snippet: "
                          (+agent-interaction-profile-snippet-names)
                          nil t nil nil "git-policy")))
  (+agent-interaction-profile--validate-name name "snippet")
  (let ((dir (or (car (+agent-interaction-profile-snippet-directories))
                 (expand-file-name "etc/noema/snippets/"
                                   user-emacs-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file (expand-file-name (format "%s.txt" name) dir))))

(defun +agent-interaction-profile-edit-template (name)
  "Open prompt template NAME for editing."
  (interactive
   (list (completing-read "Edit template: "
                          (+agent-interaction-profile-template-names)
                          nil t nil nil "context-prompt")))
  (+agent-interaction-profile--validate-name name "template")
  (let ((dir (or (car (+agent-interaction-profile-template-directories))
                 (expand-file-name "etc/noema/templates/"
                                   user-emacs-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file (expand-file-name (format "%s.txt" name) dir))))

(defun +agent-interaction-profile-read (profile)
  "Return the prompt text for PROFILE."
  (let ((file (+agent-interaction-profile-locate-file profile)))
    (if (and file (file-readable-p file))
        (+agent-interaction-profile--read-text-file file)
      (if (string= profile +agent-interaction-profile-default-name)
          +agent-interaction-profile--empty-fallback
        ""))))

(defun +agent-interaction-profile-names ()
  "Return available profile names."
  (let ((profiles
         (mapcan
          (lambda (dir)
            (when (file-directory-p dir)
              (seq-filter
               #'+agent-interaction-profile--safe-name-p
               (mapcar
                (lambda (file)
                  (file-name-base file))
                (directory-files dir t "\\.txt\\'")))))
          (+agent-interaction-profile-directories))))
    (delete-dups
     (cons +agent-interaction-profile-default-name profiles))))

(defun +agent-interaction-profile-ensure-directory ()
  "Ensure the primary profile directory exists and return it."
  (let ((dir (+agent-interaction-profile-primary-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun +agent-interaction-profile-read-name (&optional prompt default-name)
  "Read a profile name with PROMPT and DEFAULT-NAME."
  (completing-read (or prompt "Profile: ")
                   (+agent-interaction-profile-names)
                   nil nil nil nil
                   (or default-name +agent-interaction-profile-default-name)))

(defun +agent-interaction-profile-open (profile)
  "Open PROFILE for editing.
Create it in the primary profile directory when needed."
  (interactive
   (list (+agent-interaction-profile-read-name
          "Edit profile: "
          (+agent-interaction-session-profile))))
  (+agent-interaction-profile--validate-name profile "profile")
  (find-file (expand-file-name (format "%s.txt" profile)
                               (+agent-interaction-profile-ensure-directory))))

(defun +agent-interaction-profile-summary (profile)
  "Return a one-line summary for PROFILE."
  (let* ((text (+agent-interaction-profile-read profile))
         (lines (split-string text "\n" t "[ \t]+"))
         (summary (or (car lines) "")))
    (if (string-empty-p summary)
        "Empty profile"
      (truncate-string-to-width summary
                                +agent-interaction-profile-summary-max-width
                                nil nil t))))

(defun +agent-interaction-profile-candidates ()
  "Return completion candidates with inline summaries."
  (mapcar
   (lambda (name)
     (cons (format "%s  --  %s"
                   name
                   (+agent-interaction-profile-summary name))
           name))
   (+agent-interaction-profile-names)))

(defun +agent-interaction-profile-read-name-with-summary (&optional prompt default-name)
  "Read a profile name with PROMPT, DEFAULT-NAME, and inline summary text."
  (let* ((candidates (+agent-interaction-profile-candidates))
         (display (completing-read (or prompt "Profile: ")
                                   candidates
                                   nil nil nil nil
                                   (when default-name
                                     (car (rassoc default-name candidates))))))
    (or (cdr (assoc display candidates))
        default-name
        +agent-interaction-profile-default-name)))

(defun +agent-interaction-profile-preview (profile)
  "Preview PROFILE in a read-only buffer."
  (interactive
   (list (+agent-interaction-profile-read-name-with-summary
          "Preview profile: "
          (+agent-interaction-session-profile))))
  (let ((buffer (get-buffer-create (format "*AI Profile: %s*" profile))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Profile: %s\n\n" profile))
      (insert (format "Source: %s\n\n"
                      (abbreviate-file-name
                       (or (+agent-interaction-profile-locate-file profile)
                           (+agent-interaction-profile-file profile)))))
      (insert (or (+agent-interaction-profile-read profile) ""))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buffer)))

(defun +agent-interaction-profile-create (name &optional base-profile)
  "Create NAME from BASE-PROFILE when provided, then open it."
  (interactive
   (list (read-string "New profile name: ")
         (let ((base (+agent-interaction-profile-read-name-with-summary
                      "Base profile: "
                      +agent-interaction-profile-default-name)))
           (unless (string-empty-p base)
             base))))
  (when (string-empty-p (string-trim name))
    (user-error "Profile name cannot be empty"))
  (+agent-interaction-profile--validate-name name "profile")
  (let ((file (+agent-interaction-profile-file name))
        (base-text (and base-profile
                        (+agent-interaction-profile-read base-profile))))
    (+agent-interaction-profile-ensure-directory)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (if (string-empty-p (or base-text ""))
                    (format "Describe the intended workflow for profile `%s'.\n" name)
                  base-text))
        (unless (bolp)
          (insert "\n"))))
    (find-file file)))

(defun +agent-interaction-profile-build-prompt (&optional project-root)
  "Return the one-time profile prompt for PROJECT-ROOT."
  (let* ((root (or project-root default-directory))
         (profile (or (+agent-interaction-session-profile root)
                      +agent-interaction-profile-default-name))
         (profile-text (+agent-interaction-profile-read profile))
         (git-policy (+agent-interaction-profile-read-snippet
                      "git-policy"
                      +agent-interaction-profile--empty-fallback))
         (write-policy (+agent-interaction-profile-read-snippet
                        "write-policy"
                        +agent-interaction-profile--empty-fallback))
         (activation-policy (+agent-interaction-profile-read-snippet
                             "activation-policy"
                             +agent-interaction-profile--empty-fallback))
         (template (+agent-interaction-profile-read-template
                    "profile-prompt"
                    +agent-interaction-profile--profile-template-fallback)))
    (+agent-interaction-profile-render-template
     template
     `(("working-directory" . ,(abbreviate-file-name root))
       ("profile" . ,profile)
       ("git-policy" . ,git-policy)
       ("write-policy" . ,write-policy)
       ("activation-policy" . ,activation-policy)
       ("profile-text" . ,profile-text)))))

(defun +agent-interaction-profile-bootstrap-prompt (&optional project-root)
  "Return the bootstrap handshake prompt for PROJECT-ROOT."
  (+agent-interaction-profile-render-template
   (+agent-interaction-profile-read-template
    "bootstrap-prompt"
    +agent-interaction-profile--bootstrap-template-fallback)
   `(("profile-prompt" . ,(+agent-interaction-profile-build-prompt project-root)))))

(defun +agent-interaction-profile-wrap-user-prompt (prompt &optional project-root)
  "Return PROMPT wrapped with profile instructions for PROJECT-ROOT."
  (+agent-interaction-profile-render-template
   (+agent-interaction-profile-read-template
    "user-prompt-wrapper"
    +agent-interaction-profile--user-template-fallback)
   `(("profile-prompt" . ,(+agent-interaction-profile-build-prompt project-root))
     ("user-prompt" . ,prompt))))

(provide 'tools/agent/interaction-profile)
;;; tools/agent/interaction-profile.el ends here

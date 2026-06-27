;;; init-navigation-extra.el --- Extra code navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'project)
(require 'subr-x)

(autoload 'citre-peek-restore "citre-ui-peek" nil t)

(declare-function eglot-find-declaration "eglot")
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function lsp-find-declaration "lsp-mode" (&key display-action))
(declare-function lsp-treemacs-call-hierarchy "lsp-treemacs")
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/navigation--push-jump "init-navigation")
(declare-function my/navigation-find-definition "init-navigation")

(defvar my/navigation-location-history nil
  "History for file location strings opened by `my/navigation-open-location'.")

(defun my/navigation-location-at-point ()
  "Return a file location from the active region or point."
  (let ((value
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'filename t))))
    (and value (string-trim value))))

(defun my/navigation-parse-location (location)
  "Parse LOCATION into a plist with :file, :line and :column.
Supported forms include FILE, FILE:LINE, FILE:LINE:COLUMN and the GitHub-style
FILE#LLINE or FILE#LLINECCOLUMN.  Colons inside FILE, including TRAMP paths,
are preserved."
  (let* ((text (string-trim (substring-no-properties (or location ""))))
         (text (if (and (> (length text) 1)
                        (memq (aref text 0) '(?\" ?' ?`))
                        (eq (aref text 0) (aref text (1- (length text)))))
                   (substring text 1 -1)
                 text))
         (text (if (string-prefix-p "file://" text)
                   (substring text 7)
                 text))
         file line column)
    (cond
     ((string-match
       "\\`\\(.+\\)#L\\([0-9]+\\)\\(?:C\\([0-9]+\\)\\)?\\'" text)
      (setq file (match-string 1 text)
            line (string-to-number (match-string 2 text))
            column (and (match-string 3 text)
                        (string-to-number (match-string 3 text)))))
     ((string-match "\\`\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):?\\'" text)
      (setq file (match-string 1 text)
            line (string-to-number (match-string 2 text))
            column (string-to-number (match-string 3 text))))
     ((string-match "\\`\\(.+\\):\\([0-9]+\\):?\\'" text)
      (setq file (match-string 1 text)
            line (string-to-number (match-string 2 text))))
     ((not (string-empty-p text))
      (setq file text)))
    (unless (and file (not (string-empty-p file)))
      (user-error "No file location found"))
    (when (and line (< line 1))
      (user-error "Line number must be positive"))
    (when (and column (< column 1))
      (user-error "Column number must be positive"))
    (list :file file :line line :column column)))

(defun my/navigation--resolve-location-file (file)
  "Resolve FILE against the current directory and project root."
  (let* ((absolute (file-name-absolute-p file))
         (project (and (not absolute)
                       (project-current nil default-directory)))
         (candidates
          (delete-dups
           (delq nil
                 (list (expand-file-name file default-directory)
                       (and project
                            (expand-file-name file (project-root project)))))))
         (resolved
          (cl-find-if (lambda (candidate)
                        (and (file-exists-p candidate)
                             (not (file-directory-p candidate))))
                      candidates)))
    (or resolved
        (user-error "Location file does not exist: %s" file))))

;;;###autoload
(defun my/navigation-open-location (location &optional other-window)
  "Open LOCATION and jump to its optional line and column.
With OTHER-WINDOW non-nil, visit the target in another window.  Interactively,
use the active region or filename at point, prompting only when neither exists."
  (interactive
   (let ((candidate (my/navigation-location-at-point)))
     (list (read-string "File location: " candidate
                        'my/navigation-location-history candidate)
           current-prefix-arg)))
  (let* ((parsed (my/navigation-parse-location location))
         (file (my/navigation--resolve-location-file (plist-get parsed :file)))
         (line (plist-get parsed :line))
         (column (plist-get parsed :column)))
    (when (fboundp 'my/navigation--push-jump)
      (my/navigation--push-jump))
    (let ((buffer (funcall (if other-window
                               #'find-file-other-window
                             #'find-file)
                           file)))
      (with-current-buffer buffer
        (widen)
        (goto-char (point-min))
        (when (and line (not (zerop (forward-line (1- line)))))
          (user-error "Line %d is outside %s" line (abbreviate-file-name file)))
        (when column
          (move-to-column (1- column))))
      buffer)))

(defun my/navigation-find-declaration ()
  "Jump to the declaration at point, falling back to definition."
  (interactive)
  (my/navigation--push-jump)
  (condition-case nil
      (pcase (and (fboundp 'my/current-language-server-backend)
                  (my/current-language-server-backend))
        ('eglot
         (if (fboundp 'eglot-find-declaration)
             (call-interactively #'eglot-find-declaration)
           (call-interactively #'my/navigation-find-definition)))
        ('lsp-mode
         (if (fboundp 'lsp-find-declaration)
             (call-interactively #'lsp-find-declaration)
           (call-interactively #'my/navigation-find-definition)))
        (_
         (call-interactively #'my/navigation-find-definition)))
    ((user-error error)
     (call-interactively #'my/navigation-find-definition))))

(defun my/navigation-peek-restore ()
  "Restore the last Citre peek session."
  (interactive)
  (if (fboundp 'citre-peek-restore)
      (call-interactively #'citre-peek-restore)
    (user-error "Citre peek restore is unavailable")))

(defun my/navigation-call-hierarchy ()
  "Open call hierarchy when supported by the active LSP backend."
  (interactive)
  (cond
   ((and (eq (and (fboundp 'my/current-language-server-backend)
                  (my/current-language-server-backend))
             'lsp-mode)
         (fboundp 'lsp-treemacs-call-hierarchy))
    (call-interactively #'lsp-treemacs-call-hierarchy))
   (t
    (user-error "Call hierarchy requires lsp-mode with lsp-treemacs"))))

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "gD") #'my/navigation-find-declaration))

(global-set-key (kbd "C-c C-j") #'my/navigation-open-location)

(my/leader!
  "n D" '(:def my/navigation-find-declaration :which-key "declaration")
  "n R" '(:def my/navigation-peek-restore :which-key "restore peek")
  "n h" '(:def my/navigation-call-hierarchy :which-key "call hierarchy")
  "n l" '(:def my/navigation-open-location :which-key "file location"))

(provide 'init-navigation-extra)
;;; init-navigation-extra.el ends here

;;; editor/navigation-extra/config.el --- Extra code navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'aaron-funcs)
(require 'aaron-open)
(require 'project)
(require 'subr-x)

(autoload 'citre-peek-restore "citre-ui-peek" nil t)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function lsp-find-declaration "lsp-mode" (&key display-action))
(declare-function lsp-treemacs-call-hierarchy "lsp-treemacs")
(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +navigation--push-jump "editor/navigation/config")
(declare-function +navigation/find-definition "editor/navigation/config")

(defvar +navigation-location-history nil
  "History for file location strings opened by `+navigation/open-location'.")

(defun +navigation-location-at-point ()
  "Return a file location from the active region or point."
  (let ((value
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'filename t))))
    (and value (string-trim value))))

(defun +navigation-parse-location (location)
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

(defun +navigation--resolve-location-file (file)
  "Resolve FILE against the current directory and project root."
  (let* ((absolute (file-name-absolute-p file))
         (project (and (not absolute)
                       (project-current nil default-directory)))
         (candidates
          (delete-dups
           (delq nil
                 (list (aaron-open-resolve-file-name file default-directory)
                       (and project
                            (aaron-open-resolve-file-name
                             file (project-root project)))))))
         (resolved
          (cl-find-if (lambda (candidate)
                        (and (file-exists-p candidate)
                             (not (file-directory-p candidate))))
                      candidates)))
    (or resolved
        (user-error "Location file does not exist: %s" file))))

;;;###autoload
(defun +navigation/open-location (location &optional other-window)
  "Open LOCATION and jump to its optional line and column.
With OTHER-WINDOW non-nil, visit the target in another window.  Interactively,
use the active region or filename at point, prompting only when neither exists."
  (interactive
   (let ((candidate (+navigation-location-at-point)))
     (list (read-string "File location: " candidate
                        '+navigation-location-history candidate)
           current-prefix-arg)))
  (let* ((parsed (+navigation-parse-location location))
         (file (+navigation--resolve-location-file (plist-get parsed :file)))
         (line (plist-get parsed :line))
         (column (plist-get parsed :column))
         (previous-buffers (buffer-list))
         buffer position)
    (condition-case err
        (progn
          (setq buffer (aaron-open-visit-file-noselect file))
          (setq position
                (with-current-buffer buffer
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (when (and line (not (zerop (forward-line (1- line)))))
                        (user-error "Line %d is outside %s"
                                    line (abbreviate-file-name file)))
                      (when column
                        (move-to-column (1- column)))
                      (point))))))
      ((error quit)
       (when (and (buffer-live-p buffer)
                  (not (memq buffer previous-buffers))
                  (not (buffer-modified-p buffer))
                  (not (get-buffer-window buffer t)))
         (kill-buffer buffer))
       (signal (car err) (cdr err))))
    (when (fboundp '+navigation--push-jump)
      (+navigation--push-jump))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window)
             buffer)
    (with-current-buffer buffer
      (widen)
      (goto-char position))
    buffer))

(defalias 'open-file-location #'+navigation/open-location)

(defun +navigation/find-declaration ()
  "Jump to the declaration at point, falling back to definition."
  (interactive)
  (+navigation--push-jump)
  (condition-case nil
      (if (and (fboundp '+current-language-server-backend)
               (+current-language-server-backend)
               (fboundp 'lsp-find-declaration))
          (call-interactively #'lsp-find-declaration)
        (call-interactively #'+navigation/find-definition))
    ((user-error error)
     (call-interactively #'+navigation/find-definition))))

(defun +navigation/peek-restore ()
  "Restore the last Citre peek session."
  (interactive)
  (if (fboundp 'citre-peek-restore)
      (call-interactively #'citre-peek-restore)
    (user-error "Citre peek restore is unavailable")))

(defun +navigation/call-hierarchy ()
  "Open call hierarchy when supported by the active LSP backend."
  (interactive)
  (cond
   ((and (eq (and (fboundp '+current-language-server-backend)
                  (+current-language-server-backend))
             'lsp-mode)
         (fboundp 'lsp-treemacs-call-hierarchy))
    (call-interactively #'lsp-treemacs-call-hierarchy))
   (t
    (user-error "Call hierarchy requires lsp-mode with lsp-treemacs"))))

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "gD") #'+navigation/find-declaration))

(global-set-key (kbd "C-c C-j") #'open-file-location)

(aaron-leader!
  "n D" '(:def +navigation/find-declaration :which-key "declaration")
  "n R" '(:def +navigation/peek-restore :which-key "restore peek")
  "n h" '(:def +navigation/call-hierarchy :which-key "call hierarchy")
  "n l" '(:def open-file-location :which-key "file location"))

(provide 'editor/navigation-extra/config)
;;; editor/navigation-extra/config.el ends here

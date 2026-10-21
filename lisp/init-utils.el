;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(declare-function my/portal-create "init-portal" (&rest args))

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/%s/%s.md")

;; Silence compile warnings
(defvar url-http-end-of-headers)

(defun my/executable-or-name (program)
  "Return PROGRAM's absolute path when available, otherwise PROGRAM itself."
  (or (executable-find program) program))

(defun my/shell-command-executable (program)
  "Return a shell-safe command string for PROGRAM."
  (shell-quote-argument (my/executable-or-name program)))

(defun my/portal-wrap--command-name (command)
  "Return a short display name derived from COMMAND."
  (let* ((candidate
          (cond
           ((and (consp command) (stringp (car command)))
            (car command))
           ((stringp command)
            (when (string-match "\\`[[:space:]]*\\([^[:space:]]+\\)" command)
              (match-string 1 command)))))
         (name (and candidate
                    (file-name-nondirectory candidate))))
    (if (and name (not (string-empty-p name)))
        (replace-regexp-in-string "[^[:alnum:]_.+-]" "-" name)
      "command")))

;;;###autoload
(cl-defun my/portal-wrap (command &key directory buffer-name (display t))
  "Run COMMAND in a fresh Portal terminal.

COMMAND may be a shell string, or a list of program arguments.  Prefer an
argument list from Lisp callers because every argument is shell-quoted.  A
string intentionally supports shell syntax such as pipes and redirections.

DIRECTORY defaults to `default-directory'.  BUFFER-NAME defaults to a name
derived from COMMAND.  Return the terminal buffer.

Examples:

  (my/portal-wrap \='(\"claude\" \"--model\" \"sonnet\"))
  (my/portal-wrap \"ollama run qwen3\" :directory project-root)"
  (interactive (list (read-shell-command "Portal command: ")))
  (unless (or (and (stringp command)
                   (not (string-empty-p (string-trim command))))
              (and (consp command)
                   (cl-every #'stringp command)
                   (not (string-empty-p (car command)))))
    (user-error "COMMAND must be a non-empty string or argv string list"))
  (require 'init-portal)
  (my/portal-create
   :directory (or directory default-directory)
   :name (generate-new-buffer-name
          (or buffer-name
              (format "*portal:wrap:%s*"
                      (my/portal-wrap--command-name command))))
   :command command :display display))

;;;###autoload
(defun tldr (cmd &optional op)
  "View tldr page of CMD.
If OP is non-nil and search failed, OP will be used as platform
name and search again. Typically OP is nil or \"common\"."
  (interactive "sCommand: ")
  (let* ((platform (or op
                     (pcase system-type
                       ('gnu "linux")
                       ('gnu/linux "linux")
                       ('darwin "osx")
                       ('ms-dos "windows"))))
         (url (format tldr-url-template platform cmd)))
    (url-retrieve url
                  (lambda (status)
                    (let ((response-buffer (current-buffer)))
                      (unwind-protect
                          (if (or (not status) (plist-member status :error))
                              (if (not op)
                                  (tldr cmd "common")
                                (user-error "Something went wrong.\n\n%s"
                                            (pp-to-string (plist-get status :error))))
                            (goto-char url-http-end-of-headers)
                            (let* ((req (json-read))
                                   (encoding (alist-get 'encoding req))
                                   (content (alist-get 'content req)))
                              (cl-assert (string= encoding "base64"))
                              (let ((buf (get-buffer-create tldr-buffer-name))
                                    (inhibit-read-only t))
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert (base64-decode-string content))
                                  (text-mode)
                                  (view-mode +1)
                                  (pop-to-buffer buf)))))
                        (when (buffer-live-p response-buffer)
                          (kill-buffer response-buffer))))))))

(provide 'init-utils)
;;; init-utils.el ends here

;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send-return "vterm" ())
(declare-function my/vterm-send-command "init-shell"
                  (buffer command &optional retries))
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-shell)

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

(defun my/vterm-wrap--command-name (command)
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

(defun my/vterm-wrap--shell-command (command)
  "Return a shell command that replaces its shell with COMMAND.
COMMAND may be a shell command string or a non-empty list of argv strings.
Lists are quoted argument by argument; strings are evaluated by a fresh shell."
  (cond
   ((and (stringp command) (not (string-empty-p (string-trim command))))
    (let ((shell (or (and (boundp 'vterm-shell) vterm-shell)
                     shell-file-name
                     (getenv "SHELL")
                     "/bin/sh")))
      (format "exec %s -lc %s"
              (shell-quote-argument shell)
              (shell-quote-argument command))))
   ((and (consp command)
         (cl-every #'stringp command)
         (not (string-empty-p (car command))))
    (format "exec %s" (mapconcat #'shell-quote-argument command " ")))
   (t
    (user-error "COMMAND must be a non-empty string or argv string list"))))

;;;###autoload
(cl-defun my/vterm-wrap (command &key directory buffer-name (display t))
  "Run COMMAND as the foreground process of a fresh VTerm.

COMMAND may be a shell string, or a list of program arguments.  Prefer an
argument list from Lisp callers because every argument is shell-quoted.  A
string intentionally supports shell syntax such as pipes and redirections.

DIRECTORY defaults to `default-directory'.  BUFFER-NAME defaults to a name
derived from COMMAND; a unique buffer is always created.  When DISPLAY is
non-nil, show the buffer with `pop-to-buffer'.  Return the VTerm buffer.

Examples:

  (my/vterm-wrap \='(\"claude\" \"--model\" \"sonnet\"))
  (my/vterm-wrap \"ollama run qwen3\" :directory project-root)
  (my/vterm-wrap \='(\"ssh\" \"build-host\") :buffer-name \"*build host*\")"
  (interactive (list (read-shell-command "VTerm command: ")))
  (require 'vterm)
  (let* ((shell-command (my/vterm-wrap--shell-command command))
         (default-directory
          (file-name-as-directory
           (expand-file-name (or directory default-directory))))
         (name (or buffer-name
                   (format "*vterm:wrap:%s*"
                           (my/vterm-wrap--command-name command))))
         (buffer (save-window-excursion
                   (vterm (generate-new-buffer-name name)))))
    (with-current-buffer buffer
      (setq-local vterm-kill-buffer-on-exit t))
    (if (fboundp 'my/vterm-send-command)
        (my/vterm-send-command buffer shell-command)
      (with-current-buffer buffer
        (vterm-send-string shell-command)
        (vterm-send-return)))
    (when display
      (pop-to-buffer buffer))
    buffer))

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

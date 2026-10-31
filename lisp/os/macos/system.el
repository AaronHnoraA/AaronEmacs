;;; os/macos/system.el --- The boundary between Emacs and macOS -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Everything macOS hands to Emacs and everything Emacs hands to macOS crosses
;; here.  Contracts: docs/design/macos-boundary.md.

;;; Code:

(require 'aaron-keybinds)
(require 'aaron-lib)
(require 'aaron-open)
(require 'browse-url)
(require 'dnd)
(require 'general)
(require 'subr-x)

(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-fs-file-name-p "remote-fs" (file-name))
(declare-function remote-uri-to-file-name "remote-fs" (uri))
(defvar +macos-file-drop-handlers)

;;; From macOS

(defvar +macos-present-hook nil
  "Run before Emacs handles something macOS gave it that the user must see.
A module that hides Emacs puts what brings it back here.")

(defadvice! +macos--present-before-open-a (&rest _)
  "Show Emacs before visiting a file opened from Finder or the Dock."
  :before #'ns-find-file
  (run-hooks '+macos-present-hook))

(defun +macos/system-quit ()
  "Quit because macOS asked, showing Emacs first for its questions."
  (interactive)
  (run-hooks '+macos-present-hook)
  (save-buffers-kill-emacs))

(general-define-key "<ns-power-off>" #'+macos/system-quit)

(defun +macos--drop-handler ()
  "Return the file drop handler declared for the current buffer, or nil."
  (cdr (seq-find (lambda (entry) (derived-mode-p (car entry)))
                 +macos-file-drop-handlers)))

(defun +macos-drop-files (urls action)
  "Give the files in URLS to the current buffer's handler, or visit them.
ACTION is the drag action, passed on to the default visit."
  (if-let* ((handler (+macos--drop-handler)))
      (funcall handler (delq nil (mapcar (lambda (url)
                                           (dnd-get-local-file-name url t))
                                         urls)))
    (dolist (url urls)
      (dnd-open-local-file url action)))
  'private)

(put '+macos-drop-files 'dnd-multiple-handler t)

(defun +macos-drop-url (url _action)
  "Open the dropped URL through the open routes."
  (aaron/open-url url)
  'private)

;; Dired and Org prepend their own handlers buffer-locally, so these two only
;; decide for every other buffer.
(add-to-list 'dnd-protocol-alist '("\\`file:" . +macos-drop-files))
(add-to-list 'dnd-protocol-alist
             '("\\`\\(?:https?\\|ftp\\|nfs\\)://" . +macos-drop-url))

(defun +macos-quote-path (path)
  "Return PATH quoted for a POSIX shell, unchanged when that is not needed."
  (if (string-match-p "\\`[-[:alnum:]_./+,:@%=]+\\'" path)
      path
    (concat "'" (string-replace "'" "'\\''" path) "'")))

(defun +macos-drop-text (files &optional quote)
  "Return FILES as one line of shell words, each quoted by QUOTE."
  (concat (mapconcat (or quote #'+macos-quote-path) files " ") " "))

(defun +macos-drop-insert-paths (files &optional quote)
  "Insert FILES at the end of this shell buffer, each quoted by QUOTE.
The paths are this machine's, so a shell on another target refuses them."
  (unless (remote-client-file-name default-directory)
    (user-error "Dropped files are on this machine, this shell is not"))
  (goto-char (point-max))
  (insert (+macos-drop-text files quote)))

;;; To macOS

(defun +macos-client-path (file)
  "Return FILE as a path of this machine, refusing a file of another target."
  (or (remote-client-file-name file)
      (user-error "Not a file on this machine: %s" file)))

(defun +macos-call (program &rest arguments)
  "Run PROGRAM with ARGUMENTS on this machine; return non-nil on success."
  (let ((default-directory temporary-file-directory))
    (eql 0 (apply #'call-process program nil nil nil arguments))))

(defun +macos-reveal (file)
  "Select FILE in a Finder window."
  (let ((path (+macos-client-path (expand-file-name file))))
    (unless (+macos-call "open" "-R" path)
      (user-error "Finder could not show %s" path))
    path))

(defun notify-send (&rest params)
  "Show a macOS notification with the :title and :body of PARAMS."
  (let ((default-directory temporary-file-directory))
    (start-process "macos-notify" nil "osascript"
                   "-e" "on run argv"
                   "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
                   "-e" "end run"
                   (or (plist-get params :title) "Emacs")
                   (or (plist-get params :body) ""))))

(defun +macos-open--normalize-target (target)
  "Return TARGET in a form suitable for the macOS open command."
  (let* ((target (string-trim (substring-no-properties target)))
         (target
          (if (and (string-prefix-p "fs://" target)
                   (fboundp 'remote-uri-to-file-name))
              (remote-uri-to-file-name target)
            target))
         (expanded (and (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target))
                        (aaron-open-resolve-file-name target)))
         (expanded
          (if (and expanded
                   (fboundp 'remote-fs-file-name-p)
                   (remote-fs-file-name-p expanded))
              (or (remote-client-file-name expanded)
                  (user-error
                   "macOS open cannot directly access target %s"
                   (remote-file-name-target expanded)))
            expanded)))
    (cond
     ((string-empty-p target) nil)
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target) target)
     ((and expanded (file-exists-p expanded)) expanded)
     ((and expanded (file-name-absolute-p target)) expanded)
     (t target))))

(defun +macos/open-target (target)
  "Open TARGET with macOS open."
  (interactive "sOpen with macOS open: ")
  (unless (eq system-type 'darwin)
    (user-error "macOS open is only available on Darwin"))
  (unless (executable-find "open")
    (user-error "Cannot find macOS open command"))
  (if-let* ((target (+macos-open--normalize-target target)))
      (let ((process-connection-type nil))
        (start-process "macos-open" nil "open" target)
        (message "open: %s" target))
    (user-error "No target to open")))

(defun +macos/open-url (url)
  "Open URL with macOS open."
  (interactive (browse-url-interactive-arg "URL: "))
  (+macos/open-target
   (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
       url
     (concat "https://" url))))

(defun +macos/open-file (file)
  "Open FILE with macOS open."
  (interactive "fOpen file with macOS open: ")
  (+macos/open-target file))

(defun +macos-open--region-target ()
  "Return the active region as an open target, or nil."
  (when (use-region-p)
    (string-trim
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun +macos-open--org-link-target ()
  "Return the Org link at point as an open target, or nil."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-element-context))
    (let ((context (ignore-errors (org-element-context))))
      (when (eq (ignore-errors (org-element-type context)) 'link)
        (let ((link-type (org-element-property :type context))
              (path (org-element-property :path context)))
          (cond
           ((member link-type '("http" "https"))
            (concat link-type ":" path))
           ((equal link-type "file")
            (aaron-open-resolve-file-name path))
           ((and link-type path
                 (not (member link-type '("custom-id" "fuzzy" "id"))))
            (concat link-type ":" path))))))))

(defun +macos-open--target-at-point ()
  "Return a file or link target near point."
  (or (+macos-open--region-target)
      (+macos-open--org-link-target)
      (thing-at-point 'url t)
      (and (derived-mode-p 'dired-mode)
           (ignore-errors (dired-get-file-for-visit)))
      (let ((file (thing-at-point 'filename t)))
        (and file
             (file-exists-p (aaron-open-resolve-file-name file))
             file))
      (and (fboundp '+browser-current-url)
           (ignore-errors (+browser-current-url)))
      buffer-file-name))

(defun macos-open-at-point ()
  "Open the file or link at point with macOS open.
When point has no obvious target, open the current browser URL or current
buffer file."
  (interactive)
  (if-let* ((target (+macos-open--target-at-point)))
      (+macos/open-target target)
    (call-interactively #'+macos/open-target)))

(defun +macos/copy-file-to-clipboard ()
  "Put the current file itself on the macOS clipboard.
Pasting into another application (a chat, Mail, Finder) attaches the file
rather than inserting its path.  Only a file this machine can read
directly qualifies; a file on another target is refused."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Current buffer is not visiting a file")))
         (path (+macos-client-path file)))
    (unless (file-exists-p path)
      (user-error "File does not exist: %s" path))
    (unless (+macos-call "osascript" "-e"
                         (format "set the clipboard to (POSIX file %s)"
                                 (prin1-to-string path)))
      (user-error "Failed to copy file to clipboard: %s" path))
    (message "Copied file to clipboard: %s" path)))

(define-leader-key! "fg" '(:def +macos/copy-file-to-clipboard
                           :which-key "copy file to clipboard"))

(provide 'os/macos/system)
;;; os/macos/system.el ends here

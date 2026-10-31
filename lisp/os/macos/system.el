;;; os/macos/system.el --- The boundary between Emacs and macOS -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Everything macOS hands to Emacs and everything Emacs hands to macOS crosses
;; here.  Contracts: docs/design/macos-boundary.md.

;;; Code:

(require 'aaron-keybinds)
(require 'aaron-lib)
(require 'aaron-notify)
(require 'aaron-open)
(require 'browse-url)
(require 'general)
(require 'subr-x)

(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-fs-file-name-p "remote-fs" (file-name))
(declare-function remote-uri-to-file-name "remote-fs" (uri))
(declare-function macos-clipboard-files "ext:macos-window" ())
(declare-function macos-clipboard-set-files "ext:macos-window" (files))
(declare-function macos-dock-badge "ext:macos-window" (label))
(declare-function macos-notify-post "ext:macos-window" (id title body thread))
(declare-function macos-notify-ready-p "ext:macos-window" ())
(declare-function macos-notify-start "ext:macos-window" (channel))
(declare-function aaron-notify-activate "aaron-notify" (id))

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

(defun +macos/quick-look ()
  "Show the Dired entry at point, or the current file, in Quick Look."
  (interactive)
  (let ((path (+macos-client-path
               (or (and (derived-mode-p 'dired-mode)
                        (dired-get-filename nil t))
                   buffer-file-name
                   (user-error "Nothing to look at here"))))
        (default-directory temporary-file-directory))
    (call-process "qlmanage" nil 0 nil "-p" path)))

;;; Native services

(defvar +macos-native-module
  (expand-file-name "var/macos-background/macos-window.dylib"
                    user-emacs-directory)
  "The module built by `bin/emacs-app build'.")

(defun +macos-native-p ()
  "Return non-nil when the native module is loaded or loadable."
  (and (display-graphic-p)
       (file-exists-p +macos-native-module)
       (or (featurep 'macos-window)
           (ignore-errors (module-load +macos-native-module) t))))

;;; Notifications

(defvar +macos--notify-channel nil
  "Pipe process on which the native module reports clicked notifications.")

(defun +macos--notify-clicked (_process output)
  "Act on the notification ids in OUTPUT, one per line."
  (run-hooks '+macos-present-hook)
  (mapc #'aaron-notify-activate (split-string output "\n" t)))

(defun +macos--notify-native-p ()
  "Return non-nil when notifications can go through the native center."
  (and (+macos-native-p)
       (progn
         (unless (process-live-p +macos--notify-channel)
           (setq +macos--notify-channel
                 (make-pipe-process :name "macos-notify" :noquery t
                                    :filter #'+macos--notify-clicked))
           (macos-notify-start +macos--notify-channel))
         (macos-notify-ready-p))))

(defun +macos--notify-script (entry)
  "Show ENTRY with `osascript', under the script icon; a click does nothing."
  (let ((default-directory temporary-file-directory))
    (start-process "macos-notify-script" nil "osascript"
                   "-e" "on run argv"
                   "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
                   "-e" "end run"
                   (aaron-notify-entry-title entry)
                   (or (aaron-notify-entry-body entry) ""))))

(defun +macos-notify (entry)
  "Show the `aaron-notify-entry' ENTRY as a system notification."
  (if (+macos--notify-native-p)
      (macos-notify-post (aaron-notify-entry-id entry)
                         (aaron-notify-entry-title entry)
                         (or (aaron-notify-entry-body entry) "")
                         (format "%s" (or (aaron-notify-entry-source entry)
                                          "emacs")))
    (+macos--notify-script entry)))

(setq aaron-notify-system-function #'+macos-notify)

;; Asked at startup so the first notification already carries Emacs's icon: the
;; answer arrives after the request, and only the native center shows that icon.
(add-hook 'window-setup-hook #'+macos--notify-native-p)

(defun +macos--dock-badge-h (count)
  "Show COUNT unread notifications on the Dock icon."
  (when (+macos-native-p)
    (macos-dock-badge (if (zerop count) "" (number-to-string count)))))

(add-hook 'aaron-notify-unread-functions #'+macos--dock-badge-h)

;;; Files on the clipboard

(defun +macos-clipboard-files ()
  "Return the files on the system clipboard, as a Finder copy leaves them."
  (and (+macos-native-p) (macos-clipboard-files)))

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
  "Put the current file, or Dired's marked files, on the macOS clipboard.
Pasting into another application (a chat, Mail, Finder) attaches the files
rather than inserting their paths.  Only a file this machine can read
directly qualifies; a file on another target is refused."
  (interactive)
  (let* ((files (or (and (derived-mode-p 'dired-mode)
                         (dired-get-marked-files))
                    (list (or buffer-file-name
                              (user-error "Current buffer is not visiting a file")))))
         (paths (mapcar #'+macos-client-path files)))
    (dolist (path paths)
      (unless (file-exists-p path)
        (user-error "File does not exist: %s" path)))
    (unless (and (+macos-native-p) (macos-clipboard-set-files paths))
      (user-error "Failed to copy to the clipboard: %s" (car paths)))
    (message "Copied to clipboard: %s"
             (if (cdr paths) (format "%d files" (length paths)) (car paths)))))

(define-leader-key! "fg" '(:def +macos/copy-file-to-clipboard
                           :which-key "copy file to clipboard"))

(require 'os/macos/drop)

(provide 'os/macos/system)
;;; os/macos/system.el ends here

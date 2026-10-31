;;; os/macos/drop.el --- What a drop on Emacs means -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A drop goes through three steps: where it landed, what to do, doing it.
;; Contracts: docs/design/macos-boundary.md.

;;; Code:

(require 'aaron-lib)
(require 'aaron-open)
(require 'config)
(require 'dnd)
(require 'seq)

(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function dired-current-directory "dired" (&optional localp))
(defvar +macos-file-drop-handlers)

(config-defvar +macos-file-drop-ask 'auto
  "When a dropped file asks what to do.
`auto' asks for a buffer on another machine or for many files, `always' for
every drop, `never' only with Option held, which always asks."
  :type '(choice (const auto) (const always) (const never))
  :group '+macos)

(defconst +macos-file-drop-many 8
  "Dropping more files than this asks instead of opening them all.")

(defvar +macos-drop-receive-function nil
  "Function putting files in a directory, called with FILES METHOD DIRECTORY.
FILES are native paths, METHOD is `copy', `move' or `symlink'.  A file manager
module sets it.")

;;; Where it landed

(defvar +macos--drop-operations nil
  "The operations macOS offers for the drop being handled.")

(defadvice! +macos--remember-drop-operations-a (orig-fn event)
  "Keep the operations of the drop EVENT for the handlers.
`dnd-handle-multiple-urls' does not pass them on."
  :around #'ns-drag-n-drop
  (let ((+macos--drop-operations (nth 1 (nth 2 event))))
    (funcall orig-fn event)))

(defun +macos--drop-option-p ()
  "Return non-nil when Option is held: macOS then offers copying only."
  (and (consp +macos--drop-operations)
       (not (memq 'ns-drag-operation-generic +macos--drop-operations))))

(defun +macos--drop-elsewhere-p ()
  "Return non-nil when the current buffer is on another machine."
  (not (remote-client-file-name default-directory)))

(defun +macos--drop-directory ()
  "Return the directory a drop on the current buffer lands in."
  (if (derived-mode-p 'dired-mode)
      (dired-current-directory)
    default-directory))

(defun +macos--drop-handler ()
  "Return the file drop handler declared for the current buffer, or nil."
  (cdr (seq-find (lambda (entry) (derived-mode-p (car entry)))
                 +macos-file-drop-handlers)))

(defun +macos--drop-into-minibuffer (text &optional replace)
  "Give TEXT to the minibuffer being read, replacing its input with REPLACE."
  (when-let* ((window (active-minibuffer-window)))
    (with-selected-window window
      (when replace
        (delete-minibuffer-contents))
      (insert text))
    ;; The drop selected the window it landed on, and typing goes on here.
    (run-at-time 0 nil #'select-window window)
    t))

(defun +macos--drop-minibuffer-file (file)
  "Move the file name being read to FILE: a folder is entered, a file named."
  (+macos--drop-into-minibuffer
   (if (file-directory-p file) (file-name-as-directory file) file)
   t))

;;; What to do

(defun +macos-quote-path (path)
  "Return PATH quoted for a POSIX shell, unchanged when that is not needed."
  (if (string-match-p "\\`[-[:alnum:]_./+,:@%=]+\\'" path)
      path
    (concat "'" (string-replace "'" "'\\''" path) "'")))

(defun +macos-drop-text (files &optional quote)
  "Return FILES as one line of shell words, each quoted by QUOTE."
  (concat (mapconcat (or quote #'+macos-quote-path) files " ") " "))

(defun +macos--drop-actions (files)
  "Return the (KEY NAME DESCRIPTION) actions that make sense for FILES here."
  (let ((elsewhere (+macos--drop-elsewhere-p))
        (directory (abbreviate-file-name (+macos--drop-directory)))
        (writable (not buffer-read-only)))
    (delq nil
          (list
           '(?o "open" "open here")
           (list ?c "copy" (format "copy to %s" directory))
           (unless elsewhere (list ?m "move" (format "move to %s" directory)))
           (unless elsewhere (list ?l "link" (format "symlink in %s" directory)))
           (and writable '(?i "path" "insert the path"))
           (and writable (not elsewhere) (null (cdr files))
                '(?r "relative" "insert the relative path"))
           '(?q "cancel" "do nothing")))))

(defun +macos--drop-ask-p (files)
  "Return non-nil when a drop of FILES on this buffer should ask."
  (or (+macos--drop-option-p)
      (pcase +macos-file-drop-ask
        ('always t)
        ('never nil)
        (_ (or (+macos--drop-elsewhere-p)
               (> (length files) +macos-file-drop-many))))))

(defun +macos--drop-choose (files)
  "Return the action key for a drop of FILES on this buffer."
  (if (+macos--drop-ask-p files)
      (car (read-multiple-choice
            (format "Dropped %s" (if (cdr files)
                                     (format "%d files" (length files))
                                   (file-name-nondirectory
                                    (directory-file-name (car files)))))
            (+macos--drop-actions files)))
    ?o))

;;; Doing it

(defun +macos--drop-open-window ()
  "Select the window to open a drop in.
A side window or a dedicated one keeps what it shows."
  (let ((window (selected-window)))
    (when (or (window-dedicated-p window) (window-parameter window 'window-side))
      (select-window (get-mru-window nil nil t)))))

(defun +macos--drop-open (files)
  "Open FILES in Emacs, a folder in Dired.
Dropping on Emacs chooses Emacs, so the open routes do not apply.  These are
paths of this machine, whatever the buffer dropped on is showing."
  (+macos--drop-open-window)
  (let ((default-directory temporary-file-directory))
    (dolist (file files)
      (if (file-directory-p file)
          (dired file)
        (aaron-open-file-with-backend file 'emacs)))))

(defun +macos--drop-receive (files method)
  "Hand FILES to the file manager to put here by METHOD."
  (unless +macos-drop-receive-function
    (user-error "No file manager is loaded"))
  (funcall +macos-drop-receive-function files method (+macos--drop-directory)))

(defun +macos--drop-do (key files)
  "Do the action KEY of `+macos--drop-actions' with FILES."
  (pcase key
    (?o (+macos--drop-open files))
    (?c (+macos--drop-receive files 'copy))
    (?m (+macos--drop-receive files 'move))
    (?l (+macos--drop-receive files 'symlink))
    (?i (insert (mapconcat #'identity files " ")))
    (?r (insert (file-relative-name (car files) default-directory)))))

(defun +macos-drop-files (urls _action)
  "Handle the files in URLS dropped on the current buffer."
  (let ((files (delq nil (mapcar (lambda (url) (dnd-get-local-file-name url t))
                                 urls)))
        handler)
    (cond
     ((null files) (user-error "Dropped files cannot be read"))
     ((and (active-minibuffer-window) minibuffer-completing-file-name)
      (+macos--drop-minibuffer-file (car files)))
     ((active-minibuffer-window)
      (+macos--drop-into-minibuffer (+macos-drop-text files)))
     ((setq handler (+macos--drop-handler))
      (funcall handler files))
     (t (+macos--drop-do (+macos--drop-choose files) files))))
  'private)

(put '+macos-drop-files 'dnd-multiple-handler t)

(defun +macos-drop-url (url _action)
  "Give the dropped URL to a minibuffer being read, or open it by the routes."
  (or (+macos--drop-into-minibuffer url)
      (aaron/open-url url))
  'private)

(defun +macos-drop-insert-paths (files &optional quote)
  "Insert FILES at the end of this shell buffer, each quoted by QUOTE.
A shell on another machine cannot use this machine's paths: offer to copy the
files to its directory and insert their names instead."
  (let ((text (if (not (+macos--drop-elsewhere-p))
                  (+macos-drop-text files quote)
                (unless (y-or-n-p (format "Copy %d file(s) to %s? "
                                          (length files) default-directory))
                  (user-error "Dropped files are on this machine"))
                (+macos--drop-receive files 'copy)
                (+macos-drop-text (mapcar (lambda (file)
                                            (file-name-nondirectory
                                             (directory-file-name file)))
                                          files)
                                  quote))))
    (goto-char (point-max))
    (insert text)))

;; Org keeps its own buffer-local handlers.  Dired's would copy without
;; asking, so Dired takes the same steps as any other buffer.
(add-to-list 'dnd-protocol-alist '("\\`file:" . +macos-drop-files))
(add-to-list 'dnd-protocol-alist
             '("\\`\\(?:https?\\|ftp\\|nfs\\)://" . +macos-drop-url))
(after! dired
  (setq dired-dnd-protocol-alist nil))

(provide 'os/macos/drop)
;;; os/macos/drop.el ends here

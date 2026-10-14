;;; init-dirvish-keys.el --- yazi/ranger keys for Dirvish -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dirvish driven with the keys of ~/.config/yazi/keymap.toml: h/l walk the
;; tree, SPC selects, y/x/p copy, cut and paste, d trashes, r renames, and the
;; g, c, m, o, p and `,' prefixes mean what they mean there.  Where yazi and
;; Dired disagree, yazi wins; a Dired command that lost its key is listed in
;; docs/daily-usage.md with where it went.
;;
;; All bindings live in `dirvish-mode-map', which shadows `dired-mode-map',
;; so a plain Dired without Dirvish keeps Emacs' own keys.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)

(declare-function dired-dotfiles-toggle "init-dired" (&rest _))
(declare-function dired-do-shell-command "dired-aux"
                  (command &optional arg file-list))
(declare-function dired-do-async-shell-command "dired-aux"
                  (command &optional arg file-list))
(declare-function dired-rename-file "dired-aux"
                  (file newname ok-if-already-exists))
(declare-function dired-create-empty-file "dired-aux" (file))
(declare-function dired-isearch-filenames "dired-aux" ())
(declare-function dirvish-yank "dirvish-yank" (&optional dest))
(declare-function dirvish-move "dirvish-yank" (&optional dest))
(declare-function my/open-system-target "init-open" (target))
(declare-function dired-jump-with-zoxide "init-zoxide" (&optional other-window))
(declare-function remote-files-terminal "remote-files" ())
(declare-function remote-files--queue "remote-files" (files destination))
(declare-function remote-canonicalize-file-name "remote-fs"
                  (file-name &optional directory))
(declare-function remote-file-name-target "remote-fs" (file-name))

(defvar dirvish-mode-map)
(defvar dirvish-yank-overwrite-existing-files)
(defvar dired-isearch-filenames)

(defvar my/dirvish--paste-method nil
  "What `my/dirvish-paste' does with the marked files: `copy' or `cut'.")

(defun my/dirvish--command (name docstring function)
  "Define FUNCTION as the command NAME with DOCSTRING and return its symbol.
Keys bound to a named command stay readable in help and which-key."
  (let ((symbol (intern name)))
    (defalias symbol function docstring)
    symbol))

;;; Movement

(defun my/dirvish-leave ()
  "Go to the parent folder, as yazi's `leave'."
  (interactive)
  (dired-up-directory))

(defun my/dirvish-enter ()
  "Enter the folder at point, or open the file there, as yazi's `enter'."
  (interactive)
  (dired-find-file))

(defun my/dirvish-top ()
  "Go to the first file."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun my/dirvish-bottom ()
  "Go to the last file."
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun my/dirvish-half-page (direction)
  "Move half a window of lines in DIRECTION, 1 down or -1 up."
  (dired-next-line (* direction (max 1 (/ (window-body-height) 2)))))

(defun my/dirvish-half-page-down ()
  "Move half a page down."
  (interactive)
  (my/dirvish-half-page 1))

(defun my/dirvish-half-page-up ()
  "Move half a page up."
  (interactive)
  (my/dirvish-half-page -1))

(defun my/dirvish-page-down ()
  "Move a page down."
  (interactive)
  (dired-next-line (max 1 (window-body-height))))

(defun my/dirvish-page-up ()
  "Move a page up."
  (interactive)
  (dired-next-line (- (max 1 (window-body-height)))))

(defun my/dirvish-go (directory)
  "Return a command that shows DIRECTORY of this machine."
  (my/dirvish--command
   (format "my/dirvish-go:%s" directory)
   (format "Show %s." directory)
   (lambda ()
     (interactive)
     ;; Resolved from a folder of this machine: `~' in a target's buffer
     ;; must not become a path of that target.
     (let* ((default-directory temporary-file-directory)
            (directory (expand-file-name directory)))
       (unless (file-directory-p directory)
         (user-error "No such folder: %s" (abbreviate-file-name directory)))
       (dired directory)))))

(defun my/dirvish-go-interactive (directory)
  "Show DIRECTORY, read with completion from the current folder."
  (interactive (list (read-directory-name "Go to: ")))
  (dired directory))

(defun my/dirvish-follow-link ()
  "Go to what the symbolic link at point names."
  (interactive)
  (let ((target (file-truename (dired-get-filename))))
    (dired-jump nil target)))

;;; Selection

(defun my/dirvish-toggle-select ()
  "Toggle the mark of the file at point and move down."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (eq (char-after) dired-marker-char))
      (dired-unmark 1)
    (dired-mark 1)))

(defun my/dirvish-select-all ()
  "Mark every file."
  (interactive)
  (dired-unmark-all-marks)
  (dired-toggle-marks))

(defun my/dirvish-escape ()
  "Clear the selection and the pending copy or cut."
  (interactive)
  (setq my/dirvish--paste-method nil)
  (dired-unmark-all-marks))

;;; Copy, cut, paste

(defun my/dirvish--select-for-paste (method)
  "Remember METHOD for the marked files, marking the one at point if none."
  (unless (cdr (dired-get-marked-files nil nil nil t))
    (dired-mark 1))
  (setq my/dirvish--paste-method method)
  (message "%s: go to the destination and paste"
           (if (eq method 'cut) "Cut" "Copied")))

(defun my/dirvish-yank ()
  "Select the marked files, or the one at point, for copying."
  (interactive)
  (my/dirvish--select-for-paste 'copy))

(defun my/dirvish-cut ()
  "Select the marked files, or the one at point, for moving."
  (interactive)
  (my/dirvish--select-for-paste 'cut))

(defun my/dirvish-unyank ()
  "Forget the pending copy or cut and its marks."
  (interactive)
  (my/dirvish-escape)
  (message "Nothing pending"))

(defun my/dirvish--dired-buffers ()
  "Return the live Dired buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer (derived-mode-p 'dired-mode)))
              (buffer-list)))

(defun my/dirvish--marked-everywhere ()
  "Return the logical names of the files marked in any Dired buffer.
Each name is resolved in its own buffer, which is what tells a bare path of
this machine from the same path on a target."
  (delete-dups
   (cl-loop
    for buffer in (my/dirvish--dired-buffers) append
    (with-current-buffer buffer
      (when (save-excursion
              (goto-char (point-min))
              (re-search-forward (dired-marker-regexp) nil t))
        (mapcar #'remote-canonicalize-file-name
                (dired-map-over-marks (dired-get-filename) nil)))))))

(defun my/dirvish-paste (&optional force)
  "Copy or move the selected files into this folder.
With FORCE, existing files are overwritten without a question.  Files that
come from another machine are not copied by Emacs, which would block until
done: they are queued as a bulk transfer."
  (interactive)
  (let* ((here (remote-canonicalize-file-name
                (expand-file-name default-directory)))
         (sources (my/dirvish--marked-everywhere))
         (target (remote-file-name-target here)))
    (unless sources
      (user-error "Nothing selected; mark files and press y or x first"))
    (if (seq-some (lambda (file)
                    (not (equal (remote-file-name-target file) target)))
                  sources)
        (progn
          (when (eq my/dirvish--paste-method 'cut)
            (user-error
             "Moving between machines is not offered; copy, then delete the originals"))
          (require 'remote-files)
          (remote-files--queue sources here)
          (dolist (buffer (my/dirvish--dired-buffers))
            (with-current-buffer buffer (dired-unmark-all-marks))))
      (require 'dirvish-yank)
      (let ((dirvish-yank-overwrite-existing-files
             (if force 'always dirvish-yank-overwrite-existing-files)))
        (if (eq my/dirvish--paste-method 'cut)
            (dirvish-move)
          (dirvish-yank)))))
  (setq my/dirvish--paste-method nil))

(defun my/dirvish-paste-force ()
  "Paste, overwriting existing files."
  (interactive)
  (my/dirvish-paste t))

;;; Delete, create, rename

(defun my/dirvish-trash ()
  "Move the marked files, or the one at point, to the trash."
  (interactive)
  (let ((delete-by-moving-to-trash t))
    (dired-do-delete)))

(defun my/dirvish-delete-permanently ()
  "Delete the marked files, or the one at point, for good."
  (interactive)
  (let ((delete-by-moving-to-trash nil))
    (dired-do-delete)))

(defun my/dirvish-create (name)
  "Create the file NAME here, or the folder when NAME ends with a slash."
  (interactive (list (read-string "Create (end with / for a folder): ")))
  (when (string-empty-p name)
    (user-error "No name given"))
  (let ((path (expand-file-name name)))
    (if (directory-name-p name)
        (make-directory path t)
      (make-directory (file-name-directory path) t)
      (dired-create-empty-file path))
    (revert-buffer)
    (dired-goto-file (directory-file-name path))))

(defun my/dirvish-rename (cursor &optional empty)
  "Rename the file at point, with the minibuffer cursor at CURSOR.
CURSOR is `before-ext', `start' or `end'.  EMPTY is `all' to start from
nothing or `stem' to keep only the extension."
  (let* ((old (dired-get-filename))
         (name (file-name-nondirectory (directory-file-name old)))
         (extension (if (file-directory-p old)
                        ""
                      (or (file-name-extension name t) "")))
         (initial (pcase empty
                    ('all "")
                    ('stem extension)
                    (_ name)))
         (position (pcase cursor
                     ('start 0)
                     ('end (length initial))
                     (_ (- (length initial)
                           (if (eq empty 'all) 0 (length extension))))))
         (new (minibuffer-with-setup-hook
                  (lambda ()
                    (goto-char (+ (minibuffer-prompt-end) position)))
                (read-string (format "Rename %s to: " name) initial))))
    (when (or (string-empty-p new) (equal new name))
      (user-error "Name unchanged"))
    (dired-rename-file
     old (expand-file-name new (file-name-directory (directory-file-name old)))
     nil)
    (revert-buffer)))

(defun my/dirvish-rename-before-extension ()
  "Rename with the cursor before the extension."
  (interactive)
  (my/dirvish-rename 'before-ext))

(defun my/dirvish-rename-from-start ()
  "Rename with the cursor at the start of the name."
  (interactive)
  (my/dirvish-rename 'start))

(defun my/dirvish-rename-from-end ()
  "Rename with the cursor at the end of the name."
  (interactive)
  (my/dirvish-rename 'end))

(defun my/dirvish-rename-empty ()
  "Rename starting from an empty name."
  (interactive)
  (my/dirvish-rename 'end 'all))

(defun my/dirvish-rename-stem ()
  "Rename keeping only the extension."
  (interactive)
  (my/dirvish-rename 'start 'stem))

;;; Copy names

(defun my/dirvish--copy (text)
  "Put TEXT on the kill ring and say so."
  (kill-new text)
  (message "Copied: %s" text))

(defun my/dirvish-copy-path ()
  "Copy the path of the file at point."
  (interactive)
  (my/dirvish--copy (dired-get-filename)))

(defun my/dirvish-copy-dirname ()
  "Copy the path of the folder shown."
  (interactive)
  (my/dirvish--copy (directory-file-name (dired-current-directory))))

(defun my/dirvish-copy-filename ()
  "Copy the name of the file at point."
  (interactive)
  (my/dirvish--copy (file-name-nondirectory
                     (directory-file-name (dired-get-filename)))))

(defun my/dirvish-copy-name-without-extension ()
  "Copy the name of the file at point without its extension."
  (interactive)
  (my/dirvish--copy (file-name-base (directory-file-name (dired-get-filename)))))

;;; Open, find, sort, view

(defun my/dirvish-open-externally ()
  "Open the file at point with the operating system."
  (interactive)
  (my/open-system-target (dired-get-filename)))

(defun my/dirvish-find-next ()
  "Go to the next match of the last file-name search."
  (interactive)
  (isearch-repeat-forward))

(defun my/dirvish-find-previous ()
  "Go to the previous match of the last file-name search."
  (interactive)
  (isearch-repeat-backward))

(defun my/dirvish-find-backward ()
  "Search file names backward."
  (interactive)
  (let ((dired-isearch-filenames t))
    (isearch-backward)))

(defun my/dirvish-sort (switches)
  "Return a command that lists the folder with extra ls SWITCHES."
  (my/dirvish--command
   (format "my/dirvish-sort:%s" (if (string-empty-p switches) "name" switches))
   (format "List this folder with ls switches %S." switches)
   (lambda ()
     (interactive)
     (dired-sort-other
      (string-trim (concat dired-listing-switches " " switches))))))

(defun my/dirvish-details (show)
  "Return a command that shows file details when SHOW, else hides them."
  (my/dirvish--command
   (if show "my/dirvish-details-show" "my/dirvish-details-hide")
   (if show
       "Show size, time, permissions and owner."
     "Show file names only.")
   (lambda ()
     (interactive)
     (dired-hide-details-mode (if show -1 1)))))

(defun my/dirvish-shell-here ()
  "Open a terminal in the folder shown, on the machine it is on."
  (interactive)
  (require 'remote-files)
  (remote-files-terminal))

(defun my/dirvish-tab (number)
  "Return a command that selects tab NUMBER."
  (my/dirvish--command
   (format "my/dirvish-tab:%d" number)
   (format "Select tab %d." number)
   (lambda ()
     (interactive)
     (tab-bar-select-tab number))))

(defun my/dirvish-jump-file ()
  "Find a file under this folder by fuzzy name and go to it."
  (interactive)
  (call-interactively
   (if (fboundp 'consult-fd) 'consult-fd 'dirvish-fd)))

(defun my/dirvish-tab-move-left ()
  "Swap this tab with the previous one."
  (interactive)
  (tab-bar-move-tab -1))

(defun my/dirvish-tab-move-right ()
  "Swap this tab with the next one."
  (interactive)
  (tab-bar-move-tab 1))

;;; Keys

(defconst my/dirvish-yazi-keys
  `(;; Movement
    ("j" . dired-next-line) ("k" . dired-previous-line)
    ("h" . my/dirvish-leave) ("l" . my/dirvish-enter)
    ("<left>" . my/dirvish-leave) ("<right>" . my/dirvish-enter)
    ("S-<left>" . dirvish-history-go-backward)
    ("S-<right>" . dirvish-history-go-forward)
    ("C-u" . my/dirvish-half-page-up) ("C-d" . my/dirvish-half-page-down)
    ("C-b" . my/dirvish-page-up) ("C-f" . my/dirvish-page-down)
    ("G" . my/dirvish-bottom)
    ("g g" . my/dirvish-top) ("g b" . my/dirvish-bottom)
    ("g h" . ,(my/dirvish-go "~/"))
    ("g a" . ,(my/dirvish-go "~/Desktop/"))
    ("g d" . ,(my/dirvish-go "~/Downloads/"))
    ("g p" . ,(my/dirvish-go "~/Pictures/"))
    ("g v" . ,(my/dirvish-go "~/Movies/"))
    ("g m" . ,(my/dirvish-go "~/Music/"))
    ("g w" . ,(my/dirvish-go "~/Documents/"))
    ("g u" . ,(my/dirvish-go "~/Public/"))
    ("g c" . ,(my/dirvish-go "~/.config/"))
    ("g l" . ,(my/dirvish-go "~/.local/"))
    ("g SPC" . my/dirvish-go-interactive)
    ("g f" . my/dirvish-follow-link)
    ("g r" . revert-buffer)
    ;; Selection
    ("SPC" . my/dirvish-toggle-select)
    ("C-a" . my/dirvish-select-all) ("C-r" . dired-toggle-marks)
    ("<escape>" . my/dirvish-escape)
    ;; Open
    ("o o" . my/dirvish-open-externally)
    ("o c" . dired-do-shell-command)
    ("O" . dired-do-async-shell-command)
    ;; Copy, cut, paste, link
    ("y" . my/dirvish-yank) ("x" . my/dirvish-cut)
    ("Y" . my/dirvish-unyank) ("X" . my/dirvish-unyank)
    ("p p" . my/dirvish-paste) ("p f" . my/dirvish-paste-force)
    ("p l" . dirvish-symlink) ("p L" . dirvish-relative-symlink)
    ("p h" . dirvish-hardlink) ("p m" . dirvish-yank-menu)
    ;; Delete, create, rename
    ("d" . my/dirvish-trash) ("D" . my/dirvish-delete-permanently)
    ("a" . my/dirvish-create)
    ("r r" . my/dirvish-rename-before-extension)
    ("r i" . my/dirvish-rename-from-start)
    ("r a" . my/dirvish-rename-from-end)
    ("r f" . my/dirvish-rename-empty) ("r c" . my/dirvish-rename-empty)
    ;; Copy names
    ("c c" . my/dirvish-copy-path) ("c d" . my/dirvish-copy-dirname)
    ("c f" . my/dirvish-copy-filename)
    ("c n" . my/dirvish-copy-name-without-extension)
    ("c m" . my/dirvish-rename-stem) ("c r" . my/dirvish-create)
    ;; Shell
    (";" . dired-do-async-shell-command) (":" . dired-do-shell-command)
    ("S" . my/dirvish-shell-here)
    ;; Find and filter
    ("f" . dirvish-narrow)
    ("/" . dired-isearch-filenames) ("?" . my/dirvish-find-backward)
    ("n" . my/dirvish-find-next) ("N" . my/dirvish-find-previous)
    ("s" . dirvish-fd)
    ("z" . my/dirvish-jump-file) ("Z" . dired-jump-with-zoxide)
    ;; View
    ("." . dired-dotfiles-toggle)
    ("m s" . ,(my/dirvish-details t)) ("m p" . ,(my/dirvish-details t))
    ("m b" . ,(my/dirvish-details t)) ("m m" . ,(my/dirvish-details t))
    ("m o" . ,(my/dirvish-details t)) ("m n" . ,(my/dirvish-details nil))
    (", m" . ,(my/dirvish-sort "-tr")) (", M" . ,(my/dirvish-sort "-t"))
    (", b" . ,(my/dirvish-sort "--time=birth -tr"))
    (", B" . ,(my/dirvish-sort "--time=birth -t"))
    (", e" . ,(my/dirvish-sort "-X")) (", E" . ,(my/dirvish-sort "-Xr"))
    (", a" . ,(my/dirvish-sort "")) (", A" . ,(my/dirvish-sort "-r"))
    (", n" . ,(my/dirvish-sort "-v")) (", N" . ,(my/dirvish-sort "-vr"))
    (", s" . ,(my/dirvish-sort "-Sr")) (", S" . ,(my/dirvish-sort "-S"))
    (", ," . dirvish-quicksort)
    ("TAB" . dirvish-file-info-menu)
    ("<backtab>" . dirvish-subtree-toggle)
    ;; Tabs
    ("t" . tab-bar-new-tab)
    ,@(mapcar (lambda (number)
                (cons (number-to-string number) (my/dirvish-tab number)))
              (number-sequence 1 9))
    ("-" . tab-bar-switch-to-prev-tab) ("=" . tab-bar-switch-to-next-tab)
    ("_" . my/dirvish-tab-move-left) ("+" . my/dirvish-tab-move-right)
    ;; Managers
    ("w" . my/activity-board)
    ("M" . remote-files)
    ("'" . dirvish-quick-access)
    ("~" . dirvish-dispatch)
    ("q" . dirvish-quit) ("Q" . dirvish-quit))
  "Dirvish keys following ~/.config/yazi/keymap.toml.")

(defun my/dirvish-apply-yazi-keys ()
  "Bind `my/dirvish-yazi-keys' in `dirvish-mode-map'."
  ;; A key that becomes a prefix must first lose the command it had.
  (dolist (prefix '("g" "o" "p" "r" "c" "m" ","))
    (unless (keymapp (lookup-key dirvish-mode-map (kbd prefix)))
      (define-key dirvish-mode-map (kbd prefix) nil)))
  (dolist (binding my/dirvish-yazi-keys)
    (define-key dirvish-mode-map (kbd (car binding)) (cdr binding))))

(with-eval-after-load 'dirvish
  (my/dirvish-apply-yazi-keys))

(provide 'init-dirvish-keys)
;;; init-dirvish-keys.el ends here

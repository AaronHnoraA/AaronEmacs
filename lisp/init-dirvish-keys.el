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
(require 'remote-core)

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
(declare-function dirvish-symlink "dirvish-yank" (&optional dest))
(declare-function dirvish-relative-symlink "dirvish-yank" (&optional dest))
(declare-function dirvish-hardlink "dirvish-yank" (&optional dest))
(declare-function my/open-system-target "init-open" (target))
(declare-function file-open-new-frame "init-windows" (file &optional wildcards))
(declare-function dired-jump-with-zoxide "init-zoxide" (&optional other-window))
(declare-function remote-files-terminal "remote-files" ())
(declare-function remote-files--queue "remote-files" (files destination))
(declare-function remote-files--finished "remote-files" (job))
(declare-function remote-files--revert-directory "remote-files" (directory))
(declare-function remote-canonicalize-file-name "remote-fs"
                  (file-name &optional directory))
(declare-function remote-file-name-target "remote-fs" (file-name))
(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))
(declare-function remote-transfer-command "remote-transfer" (&rest args))
(declare-function remote-transfer-plan-file-operation "remote-transfer" (&rest args))
(declare-function remote-transfer-start-file-operation "remote-transfer" (&rest args))
(declare-function remote-transfer-job-kind "remote-transfer" (job))
(declare-function remote-transfer-job-sources "remote-transfer" (job))
(declare-function remote-transfer-job-state "remote-transfer" (job))

(defvar dirvish-mode-map)
(defvar dirvish-yank-overwrite-existing-files)
(defvar dirvish-yank-sources)

(defvar my/dirvish--paste-method nil
  "What `my/dirvish-paste' does with the marked files: `copy' or `cut'.")

(defvar my/dirvish--paste-sources nil
  "Logical files captured by the most recent explicit copy or cut.")

(defvar my/dirvish--paste-source-buffer nil
  "Dired buffer whose marks belong to `my/dirvish--paste-sources'.")

(defun my/dirvish--command (name docstring function)
  "Define FUNCTION as a key-only command NAME with DOCSTRING.
The uninterned symbol stays readable in key help without entering M-x."
  (let ((symbol (make-symbol name)))
    (defalias symbol function docstring)
    symbol))

;;; Movement

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
   (format "dirvish-go:%s" directory)
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
  "Show DIRECTORY, read with completion from the directory at point."
  (interactive (list (read-directory-name "Go to: "
                                           (my/dirvish--directory-at-point))))
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
  (when (and (buffer-live-p my/dirvish--paste-source-buffer)
             (not (eq my/dirvish--paste-source-buffer (current-buffer))))
    (with-current-buffer my/dirvish--paste-source-buffer
      (dired-unmark-all-marks)))
  (setq my/dirvish--paste-method nil
        my/dirvish--paste-sources nil
        my/dirvish--paste-source-buffer nil)
  (dired-unmark-all-marks))

;;; Copy, cut, paste

(defun my/dirvish--select-for-paste (method)
  "Remember METHOD for the marked files, marking the one at point if none."
  (unless (cdr (dired-get-marked-files nil nil nil t))
    (dired-mark 1))
  (setq my/dirvish--paste-method method
        my/dirvish--paste-sources
        (mapcar #'remote-canonicalize-file-name (dired-get-marked-files))
        my/dirvish--paste-source-buffer (current-buffer))
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

(defun my/dirvish--marked-everywhere ()
  "Return pending logical files, or marks in the current Dired buffer.
Unrelated marks in another machine's Dired buffer are never implicit sources."
  (or (and my/dirvish--paste-method my/dirvish--paste-sources)
      (when (and (derived-mode-p 'dired-mode)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward (dired-marker-regexp) nil t)))
        (mapcar #'remote-canonicalize-file-name
                (dired-map-over-marks (dired-get-filename) nil)))))

(defun my/dirvish--yank-method (command)
  "Return the target file operation corresponding to Dirvish COMMAND."
  (cond ((eq command #'dirvish-yank) 'copy)
        ((eq command #'dirvish-move) 'move)
        ((eq command #'dirvish-symlink) 'symlink)
        ((eq command #'dirvish-relative-symlink) 'relative-symlink)
        ((eq command #'dirvish-hardlink) 'hardlink)
        (t (error "Unsupported Dirvish file operation: %s" command))))

(defun my/dirvish--clear-marks ()
  "Unmark the source Dired buffer after an operation is queued."
  (let ((buffer (or my/dirvish--paste-source-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (dired-unmark-all-marks))))))

(defun my/dirvish--target-operation-finished (job)
  "Refresh source folders after a routed move and report JOB's result."
  (when (and (eq (remote-transfer-job-state job) 'done)
             (eq (remote-transfer-job-kind job) 'move))
    (dolist (source (remote-transfer-job-sources job))
      (remote-files--revert-directory (file-name-directory source))))
  (remote-files--finished job))

(defun my/dirvish--client-yank (command destination sources &optional force plan)
  "Run Dirvish COMMAND on SOURCES into DESTINATION.
Use Dirvish's stock worker for client-visible paths.  Otherwise queue PLAN,
or preflight and queue a routed operation on the target."
  (unless sources
    (user-error "No files are marked"))
  (let* ((destination (remote-canonicalize-file-name destination))
         (target (remote-file-name-target destination))
         (client-destination (remote-client-file-name destination))
         (client-sources (mapcar #'remote-client-file-name sources)))
    (unless (cl-every (lambda (source)
                        (equal (remote-file-name-target source) target))
                      sources)
      (user-error "Dirvish file operation requires one source target"))
    (if (and client-destination (cl-every #'identity client-sources))
        (progn
          (remote-require-on-client 'dirvish-yank)
          (let ((dirvish-yank-sources (lambda () client-sources))
                (dirvish-yank-overwrite-existing-files
                 (if force 'always dirvish-yank-overwrite-existing-files)))
            (funcall command client-destination)))
      (remote-require-on-client 'remote-files)
      (remote-require-on-client 'remote-transfer)
      (let ((job
             (remote-transfer-start-file-operation
              (or plan
                  (remote-transfer-plan-file-operation
                   (my/dirvish--yank-method command) sources destination
                   :overwrite (if force t 'ask)))
              :callback #'my/dirvish--target-operation-finished)))
        (my/dirvish--clear-marks)
        job))))

(defun my/dirvish--group-sources (sources)
  "Group SOURCES by target, preserving target and file order."
  (let (groups)
    (dolist (source sources)
      (let* ((target (remote-file-name-target source))
             (group (assoc target groups)))
        (if group
            (push source (cdr group))
          (push (cons target (list source)) groups))))
    (mapcar (lambda (group)
              (cons (car group) (nreverse (cdr group))))
            (nreverse groups))))

(defun my/dirvish--check-transfer-names (sources)
  "Reject ambiguous SOURCE basenames before independent transfer jobs start."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (source sources)
      (let ((name (file-name-nondirectory (directory-file-name source))))
        (when (gethash name seen)
          (user-error "Multiple selected sources are named %s" name))
        (puthash name t seen)))))

(defun my/dirvish-paste (&optional force)
  "Copy or move the selected files into the directory at point.
With FORCE, existing files are overwritten without a question.  Files that
come from another machine are not copied by Emacs, which would block until
done: they are queued as a bulk transfer."
  (interactive)
  (let* ((here (remote-canonicalize-file-name
                (my/dirvish--directory-at-point)))
         (sources (my/dirvish--marked-everywhere))
         (target (remote-file-name-target here))
         (groups (my/dirvish--group-sources sources))
         (same (cdr (assoc target groups)))
         (foreign (seq-remove (lambda (group) (equal (car group) target))
                              groups))
         same-plan)
    (unless sources
      (user-error "Nothing selected; mark files and press y or x first"))
    (when (and foreign (eq my/dirvish--paste-method 'cut))
      (user-error
       "Moving between machines is not offered; copy, then delete the originals"))
    ;; Validate every route before starting any of the independent jobs.
    (when foreign
      (my/dirvish--check-transfer-names sources)
      (remote-require-on-client 'remote-files)
      (remote-require-on-client 'remote-transfer)
      (dolist (group foreign)
        (remote-transfer-command (cdr group) here)))
    (when same
      (unless (and (remote-client-file-name here)
                   (cl-every #'remote-client-file-name same))
        (remote-require-on-client 'remote-transfer)
        (setq same-plan
              (remote-transfer-plan-file-operation
               (if (eq my/dirvish--paste-method 'cut) 'move 'copy)
               same here :overwrite (if force t 'ask)))))
    (dolist (group foreign)
      (remote-files--queue (cdr group) here))
    (when same
      (my/dirvish--client-yank
       (if (eq my/dirvish--paste-method 'cut)
           #'dirvish-move #'dirvish-yank)
       here same force same-plan))
    (when foreign
      (my/dirvish--clear-marks)))
  (setq my/dirvish--paste-method nil
        my/dirvish--paste-sources nil
        my/dirvish--paste-source-buffer nil))

(defun my/dirvish-paste-force ()
  "Paste, overwriting existing files."
  (interactive)
  (my/dirvish-paste t))

(defun my/dirvish-copy-here ()
  "Copy marked files to the directory at point from the yank menu."
  (interactive)
  (setq my/dirvish--paste-method 'copy)
  (my/dirvish-paste))

(defun my/dirvish-move-here ()
  "Move marked files to the directory at point from the yank menu."
  (interactive)
  (setq my/dirvish--paste-method 'cut)
  (my/dirvish-paste))

(defun my/dirvish-symlink (&optional destination)
  "Link marked files in the directory at point.
With a prefix argument, read DESTINATION explicitly."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Symlink files to: "
                                            (my/dirvish--directory-at-point)))))
  (my/dirvish--client-yank
   #'dirvish-symlink (or destination (my/dirvish--directory-at-point))
   (my/dirvish--marked-everywhere)))

(defun my/dirvish-relative-symlink (&optional destination)
  "Link marked files relatively in the directory at point.
With a prefix argument, read DESTINATION explicitly."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Relative symlink files to: "
                                            (my/dirvish--directory-at-point)))))
  (my/dirvish--client-yank
   #'dirvish-relative-symlink
   (or destination (my/dirvish--directory-at-point))
   (my/dirvish--marked-everywhere)))

(defun my/dirvish-hardlink (&optional destination)
  "Hardlink marked files in the directory at point.
With a prefix argument, read DESTINATION explicitly."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Hardlink files to: "
                                            (my/dirvish--directory-at-point)))))
  (my/dirvish--client-yank
   #'dirvish-hardlink (or destination (my/dirvish--directory-at-point))
   (my/dirvish--marked-everywhere)))

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

(defun my/dirvish--directory-at-point ()
  "Return the directory of the current Dired row.
A directory row names itself; a file row names its parent."
  (let ((file (when dired-subdir-alist
                (dired-get-filename nil t))))
    (if (and file
             (file-directory-p file)
             (not (member (file-name-nondirectory
                           (directory-file-name file)) '("." ".."))))
        (file-name-as-directory file)
      (or (and file (file-name-directory file))
          (and dired-subdir-alist (dired-current-directory))
          default-directory))))

(defun my/dirvish-create (name)
  "Create NAME beside the file at point, or inside the directory at point.
Append a slash to NAME to create a directory.  The prompt completes paths
from the selected row's directory, including entries in expanded subtrees."
  (interactive
   (list (read-file-name "Create (end with / for a folder): "
                         (my/dirvish--directory-at-point))))
  (let* ((directory (my/dirvish--directory-at-point))
         (path (expand-file-name name directory)))
    (when (or (string-empty-p name)
              (equal (directory-file-name path)
                     (directory-file-name directory)))
      (user-error "No name given"))
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
  "Copy the directory at point, or the parent of the file at point."
  (interactive)
  (my/dirvish--copy (directory-file-name
                     (my/dirvish--directory-at-point))))

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

(defun my/dirvish-open-new-frame ()
  "Open the file or directory at point in a new Emacs frame."
  (interactive)
  (file-open-new-frame (dired-get-file-for-visit)))

(defalias 'dirvish-open-new-frame #'my/dirvish-open-new-frame
  "Open the Dirvish entry at point in a new Emacs frame.")

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

(defun my/dirvish-sort (switches)
  "Return a command that lists the folder with extra ls SWITCHES."
  (my/dirvish--command
   (format "dirvish-sort:%s" (if (string-empty-p switches) "name" switches))
   (format "List this folder with ls switches %S." switches)
   (lambda ()
     (interactive)
     (dired-sort-other
      (string-trim (concat dired-listing-switches " " switches))))))

(defun my/dirvish-details (show)
  "Return a command that shows file details when SHOW, else hides them."
  (my/dirvish--command
   (if show "dirvish-details-show" "dirvish-details-hide")
   (if show
       "Show size, time, permissions and owner."
     "Show file names only.")
   (lambda ()
     (interactive)
     (dired-hide-details-mode (if show -1 1)))))

(defun my/dirvish-shell-here ()
  "Open a terminal in the directory at point, on its machine."
  (interactive)
  (require 'remote-files)
  (let ((default-directory (my/dirvish--directory-at-point)))
    (remote-files-terminal)))

(defun my/dirvish-tab (number)
  "Return a command that selects tab NUMBER."
  (my/dirvish--command
   (format "dirvish-tab:%d" number)
   (format "Select tab %d." number)
   (lambda ()
     (interactive)
     (tab-bar-select-tab number))))

(defun my/dirvish-jump-file ()
  "Find a file under the directory at point and go to it."
  (interactive)
  (let ((default-directory (my/dirvish--directory-at-point)))
    (call-interactively
     (if (fboundp 'consult-fd) 'consult-fd 'dirvish-fd))))

(defun my/dirvish-fd-here ()
  "Run Dirvish fd search from the directory at point."
  (interactive)
  (let ((default-directory (my/dirvish--directory-at-point)))
    (call-interactively #'dirvish-fd)))

(defun my/dirvish-tab-move-left ()
  "Swap this tab with the previous one."
  (interactive)
  (tab-bar-move-tab -1))

(defun my/dirvish-tab-move-right ()
  "Swap this tab with the next one."
  (interactive)
  (tab-bar-move-tab 1))

;;; Key hints

(defconst my/dirvish-key-hints
  '(("移动"
     ("j / k" . "下 / 上")
     ("h / l" . "上级目录 / 进入或打开")
     ("g g / g b" . "顶部 / 底部")
     ("C-u / C-d" . "上 / 下半页")
     ("C-b / C-f" . "上 / 下一页")
     ("S-← / S-→" . "历史后退 / 前进")
     ("TAB" . "展开 / 折叠子目录"))
    ("跳转  g"
     ("g h" . "Home")
     ("g a / g d" . "Desktop / Downloads")
     ("g w / g p" . "Documents / Pictures")
     ("g v / g m" . "Movies / Music")
     ("g c / g l" . "~/.config / ~/.local")
     ("g SPC" . "输入目录")
     ("g f" . "跟随符号链接")
     ("g r" . "刷新"))
    ("选择"
     ("SPC" . "选中 / 取消并下移")
     ("C-a" . "全选")
     ("C-r" . "反选")
     ("Esc" . "清除选择"))
    ("复制 / 粘贴"
     ("y / x" . "复制 / 剪切")
     ("p p / p f" . "粘贴 / 强制覆盖")
     ("Y / X" . "取消复制或剪切")
     ("p l / p L" . "符号链接：绝对 / 相对")
     ("p h" . "硬链接")
     ("p m" . "Dirvish 粘贴菜单"))
    ("文件"
     ("a" . "新建（以 / 结尾为目录）")
     ("d / D" . "回收站 / 永久删除")
     ("r r" . "重命名（光标在扩展名前）")
     ("r i / r a" . "重命名（光标在开头 / 末尾）")
     ("r f" . "重命名（清空）")
     ("c m" . "重命名（保留扩展名）")
     ("C-c C-e" . "wdired 批量改名"))
    ("复制名字  c"
     ("c c" . "路径")
     ("c d" . "目录路径")
     ("c f" . "文件名")
     ("c n" . "不含扩展名"))
    ("打开 / 运行"
     ("RET" . "在 Emacs 里打开")
     ("Cmd+Shift+RET" . "在新的 Emacs 窗口打开")
     ("o o" . "系统默认程序")
     (": / o c" . "运行命令（等待）")
     ("; / O" . "运行命令（异步）")
     ("S" . "在这里开终端"))
    ("查找"
     ("f" . "过滤")
     ("/" . "搜文件名")
     ("n / N" . "下一个 / 上一个匹配")
     ("s" . "fd 搜索")
     ("z / Z" . "模糊查找 / zoxide"))
    ("显示 / 排序"
     ("." . "隐藏文件")
     ("m s / m n" . "显示详情 / 只显示名字")
     ("m l / M-t" . "普通 / 上级目录视图")
     (", m / , M" . "按修改时间：正 / 倒序")
     (", s / , S" . "按大小")
     (", e / , E" . "按扩展名")
     (", a / , n" . "按字母 / 自然序")
     (", ," . "排序菜单")
     ("S-TAB" . "文件信息"))
    ("标签页"
     ("t" . "新标签页")
     ("1 … 9" . "第 N 个")
     ("- / =" . "上一个 / 下一个")
     ("_ / +" . "与左 / 右交换"))
    ("管理"
     ("M" . "文件传输（上传 / 下载 / 同步）")
     ("w" . "任务管理（皮卡丘）")
     ("'" . "快速访问")
     ("~" . "Dirvish 总菜单")
     ("?" . "这张表")
     ("q" . "退出")))
  "What `my/dirvish-key-help' shows: groups of (KEYS . MEANING).")

(defun my/dirvish--hint-columns (width)
  "Return the hint groups as lines laid out in columns for WIDTH."
  (let* ((cell 44)
         (columns (max 1 (/ width cell)))
         (blocks
          (mapcar
           (lambda (group)
             (cons (propertize (car group) 'face 'font-lock-keyword-face)
                   (mapcar
                    (lambda (hint)
                      (concat
                       "  "
                       (propertize (car hint) 'face 'help-key-binding)
                       (make-string
                        (max 1 (- 13 (string-width (car hint)))) ?\s)
                       (cdr hint)))
                    (cdr group))))
           my/dirvish-key-hints))
         (per-column (ceiling (/ (float (apply #'+ (mapcar (lambda (block)
                                                             (1+ (length block)))
                                                           blocks)))
                                 columns)))
         (stacks (list nil)))
    ;; Fill one column after another, never splitting a group.
    (dolist (block blocks)
      (when (and (car stacks)
                 (> (+ (length (car stacks)) (length block)) per-column)
                 (< (length stacks) columns))
        (push nil stacks))
      (setcar stacks (append (car stacks) block (list ""))))
    (setq stacks (nreverse stacks))
    (cl-loop
     for row from 0 below (apply #'max (mapcar #'length stacks))
     collect
     (string-trim-right
      (mapconcat
       (lambda (stack)
         (let ((text (or (nth row stack) "")))
           (concat text
                   (make-string (max 1 (- cell (string-width text))) ?\s))))
       stacks "")))))

(defun my/dirvish-key-help ()
  "Show the Dirvish keys in a window below; q closes it."
  (interactive)
  (let ((buffer (get-buffer-create "*Dirvish Keys*"))
        (width (frame-width)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join (my/dirvish--hint-columns width) "\n") "\n"))
      (special-mode)
      (setq-local truncate-lines t
                  cursor-type nil
                  mode-line-format nil)
      (goto-char (point-min)))
    (select-window
     (display-buffer
      buffer
      '((display-buffer-in-side-window)
        (side . bottom)
        (window-height . fit-window-to-buffer))))))

;;; Searchable commands for the actions that extend Dired/Dirvish.

(defalias 'dirvish-select-for-copy #'my/dirvish-yank
  "Select marked files for copying with `dirvish-paste-here'.")
(defalias 'dirvish-select-for-move #'my/dirvish-cut
  "Select marked files for moving with `dirvish-paste-here'.")
(defalias 'dirvish-paste-here #'my/dirvish-paste
  "Copy or move selected files into the directory at point.")
(defalias 'dirvish-paste-force #'my/dirvish-paste-force
  "Paste selected files into the directory at point, overwriting files.")
(defalias 'dirvish-copy-here #'my/dirvish-copy-here
  "Copy marked files into the directory at point.")
(defalias 'dirvish-move-here #'my/dirvish-move-here
  "Move marked files into the directory at point.")
(defalias 'dirvish-symlink-here #'my/dirvish-symlink
  "Create symbolic links in the directory at point.")
(defalias 'dirvish-relative-symlink-here #'my/dirvish-relative-symlink
  "Create relative symbolic links in the directory at point.")
(defalias 'dirvish-hardlink-here #'my/dirvish-hardlink
  "Create hard links in the directory at point.")
(defalias 'dirvish-create-here #'my/dirvish-create
  "Create a file or directory in the directory at point.")
(defalias 'dirvish-trash #'my/dirvish-trash
  "Move selected files to the trash.")
(defalias 'dirvish-delete-permanently #'my/dirvish-delete-permanently
  "Permanently delete selected files after confirmation.")
(defalias 'dirvish-key-help #'my/dirvish-key-help
  "Show the Dirvish key reference in a temporary window.")
(defalias 'dirvish-go-directory #'my/dirvish-go-interactive
  "Read a directory from the current row and open it in Dired.")

;;; Keys

(defconst my/dirvish-yazi-keys
  `(;; Movement
    ("j" . dired-next-line) ("k" . dired-previous-line)
    ("h" . dired-up-directory) ("l" . dired-find-file)
    ("<left>" . dired-up-directory) ("<right>" . dired-find-file)
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
    ("g SPC" . dirvish-go-directory)
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
    ("y" . dirvish-select-for-copy) ("x" . dirvish-select-for-move)
    ("Y" . my/dirvish-unyank) ("X" . my/dirvish-unyank)
    ("p p" . dirvish-paste-here) ("p f" . dirvish-paste-force)
    ("p l" . dirvish-symlink-here)
    ("p L" . dirvish-relative-symlink-here)
    ("p h" . dirvish-hardlink-here) ("p m" . dirvish-yank-menu)
    ;; Delete, create, rename
    ("d" . dirvish-trash) ("D" . dirvish-delete-permanently)
    ("a" . dirvish-create-here)
    ("r r" . my/dirvish-rename-before-extension)
    ("r i" . my/dirvish-rename-from-start)
    ("r a" . my/dirvish-rename-from-end)
    ("r f" . my/dirvish-rename-empty) ("r c" . my/dirvish-rename-empty)
    ;; Copy names
    ("c c" . my/dirvish-copy-path) ("c d" . my/dirvish-copy-dirname)
    ("c f" . my/dirvish-copy-filename)
    ("c n" . my/dirvish-copy-name-without-extension)
    ("c m" . my/dirvish-rename-stem) ("c r" . dirvish-create-here)
    ;; Shell
    (";" . dired-do-async-shell-command) (":" . dired-do-shell-command)
    ("S" . my/dirvish-shell-here)
    ;; Find and filter
    ("f" . dirvish-narrow)
    ("/" . dired-isearch-filenames) ("?" . dirvish-key-help)
    ("n" . my/dirvish-find-next) ("N" . my/dirvish-find-previous)
    ("s" . my/dirvish-fd-here)
    ("z" . my/dirvish-jump-file) ("Z" . dired-jump-with-zoxide)
    ;; View
    ("." . dired-dotfiles-toggle)
    ("m s" . ,(my/dirvish-details t)) ("m p" . ,(my/dirvish-details t))
    ("m b" . ,(my/dirvish-details t)) ("m m" . ,(my/dirvish-details t))
    ("m o" . ,(my/dirvish-details t)) ("m n" . ,(my/dirvish-details nil))
    ("m l" . dirvish-layout-toggle)
    (", m" . ,(my/dirvish-sort "-tr")) (", M" . ,(my/dirvish-sort "-t"))
    (", b" . ,(my/dirvish-sort "--time=birth -tr"))
    (", B" . ,(my/dirvish-sort "--time=birth -t"))
    (", e" . ,(my/dirvish-sort "-X")) (", E" . ,(my/dirvish-sort "-Xr"))
    (", a" . ,(my/dirvish-sort "")) (", A" . ,(my/dirvish-sort "-r"))
    (", n" . ,(my/dirvish-sort "-v")) (", N" . ,(my/dirvish-sort "-vr"))
    (", s" . ,(my/dirvish-sort "-Sr")) (", S" . ,(my/dirvish-sort "-S"))
    (", ," . dirvish-quicksort)
    ;; yazi has no tree view; TAB keeps Dirvish's fold, and yazi's TAB (file
    ;; info) moves to S-TAB.
    ("TAB" . dirvish-subtree-toggle)
    ("<backtab>" . dirvish-file-info-menu)
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

(with-eval-after-load 'dirvish-yank
  ;; Keep Dirvish's native transient, with the same destination semantics as
  ;; the direct keys.  `setopt' invokes its public menu setter.
  (setopt dirvish-yank-keys
          '(("y" "Copy to directory at point" dirvish-copy-here)
            ("m" "Move to directory at point" dirvish-move-here)
            ("s" "Symlink in directory at point" dirvish-symlink-here)
            ("r" "Relative symlink in directory at point"
             dirvish-relative-symlink-here)
            ("h" "Hardlink in directory at point" dirvish-hardlink-here))))

(provide 'init-dirvish-keys)
;;; init-dirvish-keys.el ends here

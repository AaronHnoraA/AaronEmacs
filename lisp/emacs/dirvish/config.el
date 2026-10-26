;;; emacs/dirvish/config.el --- yazi/ranger keys for Dirvish -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dirvish driven with the keys of ~/.config/yazi/keymap.toml: h/l walk the
;; tree, SPC selects, y/x/p copy, cut and paste, d trashes, r renames, and the
;; g, c, m, o, p and `,' prefixes mean what they mean there.  Where yazi and
;; Dired disagree, yazi wins; a Dired command that lost its key is listed in
;; docs/guide/daily-usage.md with where it went.
;;
;; All bindings live in `dirvish-mode-map', which shadows `dired-mode-map',
;; so a plain Dired without Dirvish keeps Emacs' own keys.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'aaron-open)
(require 'remote-core)

(declare-function dired-dotfiles-toggle "emacs/dired/config" (&rest _))
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
(declare-function aaron/open-system-target "aaron-open" (target))
(declare-function file-open-new-frame "ui/windows/config" (file &optional wildcards))
(declare-function dired-jump-with-zoxide "tools/zoxide/config" (&optional other-window))
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

(defvar +dirvish--paste-method nil
  "What `+dirvish/paste' does with the marked files: `copy' or `cut'.")

(defvar +dirvish--paste-sources nil
  "Logical files captured by the most recent explicit copy or cut.")

(defvar +dirvish--paste-source-buffer nil
  "Dired buffer whose marks belong to `+dirvish--paste-sources'.")

(defun +dirvish--command (name docstring function)
  "Define FUNCTION as a key-only command NAME with DOCSTRING.
The uninterned symbol stays readable in key help without entering M-x."
  (let ((symbol (make-symbol name)))
    (defalias symbol function docstring)
    symbol))

;;; Movement

(defun +dirvish/top ()
  "Go to the first file."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun +dirvish/bottom ()
  "Go to the last file."
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun +dirvish-half-page (direction)
  "Move half a window of lines in DIRECTION, 1 down or -1 up."
  (dired-next-line (* direction (max 1 (/ (window-body-height) 2)))))

(defun +dirvish/half-page-down ()
  "Move half a page down."
  (interactive)
  (+dirvish-half-page 1))

(defun +dirvish/half-page-up ()
  "Move half a page up."
  (interactive)
  (+dirvish-half-page -1))

(defun +dirvish/page-down ()
  "Move a page down."
  (interactive)
  (dired-next-line (max 1 (window-body-height))))

(defun +dirvish/page-up ()
  "Move a page up."
  (interactive)
  (dired-next-line (- (max 1 (window-body-height)))))

(defun +dirvish-go (directory)
  "Return a command that shows DIRECTORY of this machine."
  (+dirvish--command
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

(defun +dirvish/go-interactive (directory)
  "Show DIRECTORY, read with completion from the directory at point."
  (interactive (list (read-directory-name "Go to: "
                                           (+dirvish--directory-at-point))))
  (aaron-open-call-with-file-context
   (aaron-open-resolve-file-name directory) #'dired))

(defun +dirvish/follow-link ()
  "Go to what the symbolic link at point names."
  (interactive)
  (let ((target (file-truename (dired-get-filename))))
    (dired-jump nil target)))

;;; Selection

(defun +dirvish/toggle-select ()
  "Toggle the mark of the file at point and move down."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (eq (char-after) dired-marker-char))
      (dired-unmark 1)
    (dired-mark 1)))

(defun +dirvish/select-all ()
  "Mark every file."
  (interactive)
  (dired-unmark-all-marks)
  (dired-toggle-marks))

(defun +dirvish/escape ()
  "Clear the selection and the pending copy or cut."
  (interactive)
  (when (and (buffer-live-p +dirvish--paste-source-buffer)
             (not (eq +dirvish--paste-source-buffer (current-buffer))))
    (with-current-buffer +dirvish--paste-source-buffer
      (dired-unmark-all-marks)))
  (setq +dirvish--paste-method nil
        +dirvish--paste-sources nil
        +dirvish--paste-source-buffer nil)
  (dired-unmark-all-marks))

;;; Copy, cut, paste

(defun +dirvish--select-for-paste (method)
  "Remember METHOD for the marked files, marking the one at point if none."
  (unless (cdr (dired-get-marked-files nil nil nil t))
    (dired-mark 1))
  (setq +dirvish--paste-method method
        +dirvish--paste-sources
        (mapcar #'remote-canonicalize-file-name (dired-get-marked-files))
        +dirvish--paste-source-buffer (current-buffer))
  (message "%s: go to the destination and paste"
           (if (eq method 'cut) "Cut" "Copied")))

(defun +dirvish/yank ()
  "Select the marked files, or the one at point, for copying."
  (interactive)
  (+dirvish--select-for-paste 'copy))

(defun +dirvish/cut ()
  "Select the marked files, or the one at point, for moving."
  (interactive)
  (+dirvish--select-for-paste 'cut))

(defun +dirvish/unyank ()
  "Forget the pending copy or cut and its marks."
  (interactive)
  (+dirvish/escape)
  (message "Nothing pending"))

(defun +dirvish--marked-everywhere ()
  "Return pending logical files, or marks in the current Dired buffer.
Unrelated marks in another machine's Dired buffer are never implicit sources."
  (or (and +dirvish--paste-method +dirvish--paste-sources)
      (when (and (derived-mode-p 'dired-mode)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward (dired-marker-regexp) nil t)))
        (mapcar #'remote-canonicalize-file-name
                (dired-map-over-marks (dired-get-filename) nil)))))

(defun +dirvish--yank-method (command)
  "Return the target file operation corresponding to Dirvish COMMAND."
  (cond ((eq command #'dirvish-yank) 'copy)
        ((eq command #'dirvish-move) 'move)
        ((eq command #'dirvish-symlink) 'symlink)
        ((eq command #'dirvish-relative-symlink) 'relative-symlink)
        ((eq command #'dirvish-hardlink) 'hardlink)
        (t (error "Unsupported Dirvish file operation: %s" command))))

(defun +dirvish--clear-marks ()
  "Unmark the source Dired buffer after an operation is queued."
  (let ((buffer (or +dirvish--paste-source-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (dired-unmark-all-marks))))))

(defun +dirvish--target-operation-finished (job)
  "Refresh source folders after a routed move and report JOB's result."
  (when (and (eq (remote-transfer-job-state job) 'done)
             (eq (remote-transfer-job-kind job) 'move))
    (dolist (source (remote-transfer-job-sources job))
      (remote-files--revert-directory (file-name-directory source))))
  (remote-files--finished job))

(defun +dirvish--client-yank (command destination sources &optional force plan)
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
                   (+dirvish--yank-method command) sources destination
                   :overwrite (if force t 'ask)))
              :callback #'+dirvish--target-operation-finished)))
        (+dirvish--clear-marks)
        job))))

(defun +dirvish--group-sources (sources)
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

(defun +dirvish--check-transfer-names (sources)
  "Reject ambiguous SOURCE basenames before independent transfer jobs start."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (source sources)
      (let ((name (file-name-nondirectory (directory-file-name source))))
        (when (gethash name seen)
          (user-error "Multiple selected sources are named %s" name))
        (puthash name t seen)))))

(defun +dirvish/paste (&optional force)
  "Copy or move the selected files into the directory at point.
With FORCE, existing files are overwritten without a question.  Files that
come from another machine are not copied by Emacs, which would block until
done: they are queued as a bulk transfer."
  (interactive)
  (let* ((here (remote-canonicalize-file-name
                (+dirvish--directory-at-point)))
         (sources (+dirvish--marked-everywhere))
         (target (remote-file-name-target here))
         (groups (+dirvish--group-sources sources))
         (same (cdr (assoc target groups)))
         (foreign (seq-remove (lambda (group) (equal (car group) target))
                              groups))
         same-plan)
    (unless sources
      (user-error "Nothing selected; mark files and press y or x first"))
    (when (and foreign (eq +dirvish--paste-method 'cut))
      (user-error
       "Moving between machines is not offered; copy, then delete the originals"))
    ;; Validate every route before starting any of the independent jobs.
    (when foreign
      (+dirvish--check-transfer-names sources)
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
               (if (eq +dirvish--paste-method 'cut) 'move 'copy)
               same here :overwrite (if force t 'ask)))))
    (dolist (group foreign)
      (remote-files--queue (cdr group) here))
    (when same
      (+dirvish--client-yank
       (if (eq +dirvish--paste-method 'cut)
           #'dirvish-move #'dirvish-yank)
       here same force same-plan))
    (when foreign
      (+dirvish--clear-marks)))
  (setq +dirvish--paste-method nil
        +dirvish--paste-sources nil
        +dirvish--paste-source-buffer nil))

(defun +dirvish/paste-force ()
  "Paste, overwriting existing files."
  (interactive)
  (+dirvish/paste t))

(defun +dirvish/copy-here ()
  "Copy marked files to the directory at point from the yank menu."
  (interactive)
  (setq +dirvish--paste-method 'copy)
  (+dirvish/paste))

(defun +dirvish/move-here ()
  "Move marked files to the directory at point from the yank menu."
  (interactive)
  (setq +dirvish--paste-method 'cut)
  (+dirvish/paste))

(defun +dirvish/symlink (&optional destination)
  "Link marked files in the directory at point.
With a prefix argument, read DESTINATION explicitly."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Symlink files to: "
                                            (+dirvish--directory-at-point)))))
  (+dirvish--client-yank
   #'dirvish-symlink (or destination (+dirvish--directory-at-point))
   (+dirvish--marked-everywhere)))

(defun +dirvish/relative-symlink (&optional destination)
  "Link marked files relatively in the directory at point.
With a prefix argument, read DESTINATION explicitly."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Relative symlink files to: "
                                            (+dirvish--directory-at-point)))))
  (+dirvish--client-yank
   #'dirvish-relative-symlink
   (or destination (+dirvish--directory-at-point))
   (+dirvish--marked-everywhere)))

(defun +dirvish/hardlink (&optional destination)
  "Hardlink marked files in the directory at point.
With a prefix argument, read DESTINATION explicitly."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Hardlink files to: "
                                            (+dirvish--directory-at-point)))))
  (+dirvish--client-yank
   #'dirvish-hardlink (or destination (+dirvish--directory-at-point))
   (+dirvish--marked-everywhere)))

;;; Delete, create, rename

(defun +dirvish/trash ()
  "Move the marked files, or the one at point, to the trash."
  (interactive)
  (let ((delete-by-moving-to-trash t))
    (dired-do-delete)))

(defun +dirvish/delete-permanently ()
  "Delete the marked files, or the one at point, for good."
  (interactive)
  (let ((delete-by-moving-to-trash nil))
    (dired-do-delete)))

(defun +dirvish--directory-at-point ()
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

(defun +dirvish/create (name)
  "Create NAME beside the file at point, or inside the directory at point.
Append a slash to NAME to create a directory.  The prompt completes paths
from the selected row's directory, including entries in expanded subtrees."
  (interactive
   (list (read-file-name "Create (end with / for a folder): "
                         (+dirvish--directory-at-point))))
  (let* ((directory (+dirvish--directory-at-point))
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

(defun +dirvish-rename (cursor &optional empty)
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

(defun +dirvish/rename-before-extension ()
  "Rename with the cursor before the extension."
  (interactive)
  (+dirvish-rename 'before-ext))

(defun +dirvish/rename-from-start ()
  "Rename with the cursor at the start of the name."
  (interactive)
  (+dirvish-rename 'start))

(defun +dirvish/rename-from-end ()
  "Rename with the cursor at the end of the name."
  (interactive)
  (+dirvish-rename 'end))

(defun +dirvish/rename-empty ()
  "Rename starting from an empty name."
  (interactive)
  (+dirvish-rename 'end 'all))

(defun +dirvish/rename-stem ()
  "Rename keeping only the extension."
  (interactive)
  (+dirvish-rename 'start 'stem))

;;; Copy names

(defun +dirvish--copy (text)
  "Put TEXT on the kill ring and say so."
  (kill-new text)
  (message "Copied: %s" text))

(defun +dirvish/copy-path ()
  "Copy the path of the file at point."
  (interactive)
  (+dirvish--copy (dired-get-filename)))

(defun +dirvish/copy-dirname ()
  "Copy the directory at point, or the parent of the file at point."
  (interactive)
  (+dirvish--copy (directory-file-name
                     (+dirvish--directory-at-point))))

(defun +dirvish/copy-filename ()
  "Copy the name of the file at point."
  (interactive)
  (+dirvish--copy (file-name-nondirectory
                     (directory-file-name (dired-get-filename)))))

(defun +dirvish/copy-name-without-extension ()
  "Copy the name of the file at point without its extension."
  (interactive)
  (+dirvish--copy (file-name-base (directory-file-name (dired-get-filename)))))

;;; Open, find, sort, view

(defun +dirvish/open-new-frame ()
  "Open the file or directory at point in a new Emacs frame."
  (interactive)
  (file-open-new-frame (dired-get-file-for-visit)))

(defalias 'dirvish-open-new-frame #'+dirvish/open-new-frame
  "Open the Dirvish entry at point in a new Emacs frame.")

(defun +dirvish/open-externally ()
  "Open the file at point with the operating system."
  (interactive)
  (aaron/open-system-target (dired-get-filename)))

(defun +dirvish/find-next ()
  "Go to the next match of the last file-name search."
  (interactive)
  (isearch-repeat-forward))

(defun +dirvish/find-previous ()
  "Go to the previous match of the last file-name search."
  (interactive)
  (isearch-repeat-backward))

(defun +dirvish-sort (switches)
  "Return a command that lists the folder with extra ls SWITCHES."
  (+dirvish--command
   (format "dirvish-sort:%s" (if (string-empty-p switches) "name" switches))
   (format "List this folder with ls switches %S." switches)
   (lambda ()
     (interactive)
     (dired-sort-other
      (string-trim (concat dired-listing-switches " " switches))))))

(defun +dirvish-details (show)
  "Return a command that shows file details when SHOW, else hides them."
  (+dirvish--command
   (if show "dirvish-details-show" "dirvish-details-hide")
   (if show
       "Show size, time, permissions and owner."
     "Show file names only.")
   (lambda ()
     (interactive)
     (dired-hide-details-mode (if show -1 1)))))

(defun +dirvish/shell-here ()
  "Open a terminal in the directory at point, on its machine."
  (interactive)
  (require 'remote-files)
  (let ((default-directory (+dirvish--directory-at-point)))
    (remote-files-terminal)))

(defun +dirvish-tab (number)
  "Return a command that selects tab NUMBER."
  (+dirvish--command
   (format "dirvish-tab:%d" number)
   (format "Select tab %d." number)
   (lambda ()
     (interactive)
     (tab-bar-select-tab number))))

(defun +dirvish/jump-file ()
  "Find a file under the directory at point and go to it."
  (interactive)
  (let ((default-directory (+dirvish--directory-at-point)))
    (call-interactively
     (if (fboundp 'consult-fd) 'consult-fd 'dirvish-fd))))

(defun +dirvish/fd-here ()
  "Run Dirvish fd search from the directory at point."
  (interactive)
  (let ((default-directory (+dirvish--directory-at-point)))
    (call-interactively #'dirvish-fd)))

(defun +dirvish/tab-move-left ()
  "Swap this tab with the previous one."
  (interactive)
  (tab-bar-move-tab -1))

(defun +dirvish/tab-move-right ()
  "Swap this tab with the next one."
  (interactive)
  (tab-bar-move-tab 1))

;;; Key hints

(defconst +dirvish-key-hints
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
     ("m l" . "普通 / 上级目录视图")
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
  "What `+dirvish/key-help' shows: groups of (KEYS . MEANING).")

(defun +dirvish--hint-columns (width)
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
           +dirvish-key-hints))
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

(defun +dirvish/key-help ()
  "Show the Dirvish keys in a window below; q closes it."
  (interactive)
  (let ((buffer (get-buffer-create "*Dirvish Keys*"))
        (width (frame-width)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join (+dirvish--hint-columns width) "\n") "\n"))
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

(defalias 'dirvish-select-for-copy #'+dirvish/yank
  "Select marked files for copying with `dirvish-paste-here'.")
(defalias 'dirvish-select-for-move #'+dirvish/cut
  "Select marked files for moving with `dirvish-paste-here'.")
(defalias 'dirvish-paste-here #'+dirvish/paste
  "Copy or move selected files into the directory at point.")
(defalias 'dirvish-paste-force #'+dirvish/paste-force
  "Paste selected files into the directory at point, overwriting files.")
(defalias 'dirvish-copy-here #'+dirvish/copy-here
  "Copy marked files into the directory at point.")
(defalias 'dirvish-move-here #'+dirvish/move-here
  "Move marked files into the directory at point.")
(defalias 'dirvish-symlink-here #'+dirvish/symlink
  "Create symbolic links in the directory at point.")
(defalias 'dirvish-relative-symlink-here #'+dirvish/relative-symlink
  "Create relative symbolic links in the directory at point.")
(defalias 'dirvish-hardlink-here #'+dirvish/hardlink
  "Create hard links in the directory at point.")
(defalias 'dirvish-create-here #'+dirvish/create
  "Create a file or directory in the directory at point.")
(defalias 'dirvish-trash #'+dirvish/trash
  "Move selected files to the trash.")
(defalias 'dirvish-delete-permanently #'+dirvish/delete-permanently
  "Permanently delete selected files after confirmation.")
(defalias 'dirvish-key-help #'+dirvish/key-help
  "Show the Dirvish key reference in a temporary window.")
(defalias 'dirvish-go-directory #'+dirvish/go-interactive
  "Read a directory from the current row and open it in Dired.")

;;; Keys

(defconst +dirvish-yazi-keys
  `(;; Movement
    ("j" . dired-next-line) ("k" . dired-previous-line)
    ("h" . dired-up-directory) ("l" . dired-find-file)
    ("<left>" . dired-up-directory) ("<right>" . dired-find-file)
    ("S-<left>" . dirvish-history-go-backward)
    ("S-<right>" . dirvish-history-go-forward)
    ("C-u" . +dirvish/half-page-up) ("C-d" . +dirvish/half-page-down)
    ("C-b" . +dirvish/page-up) ("C-f" . +dirvish/page-down)
    ("G" . +dirvish/bottom)
    ("g g" . +dirvish/top) ("g b" . +dirvish/bottom)
    ("g h" . ,(+dirvish-go "~/"))
    ("g a" . ,(+dirvish-go "~/Desktop/"))
    ("g d" . ,(+dirvish-go "~/Downloads/"))
    ("g p" . ,(+dirvish-go "~/Pictures/"))
    ("g v" . ,(+dirvish-go "~/Movies/"))
    ("g m" . ,(+dirvish-go "~/Music/"))
    ("g w" . ,(+dirvish-go "~/Documents/"))
    ("g u" . ,(+dirvish-go "~/Public/"))
    ("g c" . ,(+dirvish-go "~/.config/"))
    ("g l" . ,(+dirvish-go "~/.local/"))
    ("g SPC" . dirvish-go-directory)
    ("g f" . +dirvish/follow-link)
    ("g r" . revert-buffer)
    ;; Selection
    ("SPC" . +dirvish/toggle-select)
    ("C-a" . +dirvish/select-all) ("C-r" . dired-toggle-marks)
    ("<escape>" . +dirvish/escape)
    ;; Open
    ("o o" . +dirvish/open-externally)
    ("o c" . dired-do-shell-command)
    ("O" . dired-do-async-shell-command)
    ;; Copy, cut, paste, link
    ("y" . dirvish-select-for-copy) ("x" . dirvish-select-for-move)
    ("Y" . +dirvish/unyank) ("X" . +dirvish/unyank)
    ("p p" . dirvish-paste-here) ("p f" . dirvish-paste-force)
    ("p l" . dirvish-symlink-here)
    ("p L" . dirvish-relative-symlink-here)
    ("p h" . dirvish-hardlink-here) ("p m" . dirvish-yank-menu)
    ;; Delete, create, rename
    ("d" . dirvish-trash) ("D" . dirvish-delete-permanently)
    ("a" . dirvish-create-here)
    ("r r" . +dirvish/rename-before-extension)
    ("r i" . +dirvish/rename-from-start)
    ("r a" . +dirvish/rename-from-end)
    ("r f" . +dirvish/rename-empty) ("r c" . +dirvish/rename-empty)
    ;; Copy names
    ("c c" . +dirvish/copy-path) ("c d" . +dirvish/copy-dirname)
    ("c f" . +dirvish/copy-filename)
    ("c n" . +dirvish/copy-name-without-extension)
    ("c m" . +dirvish/rename-stem) ("c r" . dirvish-create-here)
    ;; Shell
    (";" . dired-do-async-shell-command) (":" . dired-do-shell-command)
    ("S" . +dirvish/shell-here)
    ;; Find and filter
    ("f" . dirvish-narrow)
    ("/" . dired-isearch-filenames) ("?" . dirvish-key-help)
    ("n" . +dirvish/find-next) ("N" . +dirvish/find-previous)
    ("s" . +dirvish/fd-here)
    ("z" . +dirvish/jump-file) ("Z" . dired-jump-with-zoxide)
    ;; View
    ("." . dired-dotfiles-toggle)
    ("m s" . ,(+dirvish-details t)) ("m p" . ,(+dirvish-details t))
    ("m b" . ,(+dirvish-details t)) ("m m" . ,(+dirvish-details t))
    ("m o" . ,(+dirvish-details t)) ("m n" . ,(+dirvish-details nil))
    ("m l" . dirvish-layout-toggle)
    (", m" . ,(+dirvish-sort "-tr")) (", M" . ,(+dirvish-sort "-t"))
    (", b" . ,(+dirvish-sort "--time=birth -tr"))
    (", B" . ,(+dirvish-sort "--time=birth -t"))
    (", e" . ,(+dirvish-sort "-X")) (", E" . ,(+dirvish-sort "-Xr"))
    (", a" . ,(+dirvish-sort "")) (", A" . ,(+dirvish-sort "-r"))
    (", n" . ,(+dirvish-sort "-v")) (", N" . ,(+dirvish-sort "-vr"))
    (", s" . ,(+dirvish-sort "-Sr")) (", S" . ,(+dirvish-sort "-S"))
    (", ," . dirvish-quicksort)
    ;; yazi has no tree view; TAB keeps Dirvish's fold, and yazi's TAB (file
    ;; info) moves to S-TAB.
    ("TAB" . dirvish-subtree-toggle)
    ("<backtab>" . dirvish-file-info-menu)
    ;; Tabs
    ("t" . tab-bar-new-tab)
    ,@(mapcar (lambda (number)
                (cons (number-to-string number) (+dirvish-tab number)))
              (number-sequence 1 9))
    ("-" . tab-bar-switch-to-prev-tab) ("=" . tab-bar-switch-to-next-tab)
    ("_" . +dirvish/tab-move-left) ("+" . +dirvish/tab-move-right)
    ;; Managers
    ("w" . +activity/board)
    ("M" . remote-files)
    ("'" . dirvish-quick-access)
    ("~" . dirvish-dispatch)
    ("q" . dirvish-quit) ("Q" . dirvish-quit))
  "Dirvish keys following ~/.config/yazi/keymap.toml.")

(defun +dirvish-apply-yazi-keys ()
  "Bind `+dirvish-yazi-keys' in `dirvish-mode-map'."
  ;; A key that becomes a prefix must first lose the command it had.
  (dolist (prefix '("g" "o" "p" "r" "c" "m" ","))
    (unless (keymapp (lookup-key dirvish-mode-map (kbd prefix)))
      (define-key dirvish-mode-map (kbd prefix) nil)))
  (dolist (binding +dirvish-yazi-keys)
    (define-key dirvish-mode-map (kbd (car binding)) (cdr binding))))

(with-eval-after-load 'dirvish
  (+dirvish-apply-yazi-keys))

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

(provide 'emacs/dirvish/config)
;;; emacs/dirvish/config.el ends here

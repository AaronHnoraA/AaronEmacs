;;; init-windows.el --- Window and bookmark helpers -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'init-open)
(require 'transient)

;; `evil-define-key' is a macro. Load Evil at compile time so byte-compiled
;; config expands it correctly instead of emitting a runtime function call.
(eval-when-compile
  (require 'evil))

(declare-function remote-files-mode-line-text "remote-files" ())
(declare-function my/background-make-frame "init-background" (&optional parameters))
(declare-function my/ibuffer-noema-file-name "init-base" (buffer))
(declare-function my/noema-open-file-split-frame "init-aaronnote" (file))
(declare-function bookmark-delete "bookmark" (bookmark))
(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-position "bookmark" (bookmark-name-or-record))
(declare-function bookmark-insert "bookmark" (bookmark))
(declare-function bookmark-jump "bookmark" (bookmark))
(declare-function bookmark-jump-other-window "bookmark" (bookmark))
(declare-function bookmark-make-record-default "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-maybe-load-default-file "bookmark" ())
(declare-function bookmark-save "bookmark" (&optional parg))
(declare-function bookmark-store "bookmark" (name alist no-overwrite))
(declare-function consult-bookmark "consult" (name))
(declare-function persp-current-buffers "perspective" ())
(declare-function persp-current-name "perspective" ())
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function toggle-one-window "toggle-one-window" ())

(defun my/bookmark--load ()
  "Load bookmark data for the current session."
  (require 'bookmark)
  (bookmark-maybe-load-default-file))

(defun my/bookmark-write-file-on-client-a (orig file)
  "Call native bookmark writer ORIG for FILE in a client file context.
Explicit Remote bookmark files and relative paths retain their own placement."
  (if (and (file-name-absolute-p file)
           (not (file-remote-p file))
           (not (and (fboundp 'remote-fs-file-name-p)
                     (remote-fs-file-name-p file))))
      (let* ((client-directory
              (file-name-as-directory
               (expand-file-name user-emacs-directory "/")))
             (default-directory client-directory))
        ;; `bookmark-write-file' uses this internal buffer for `write-file'.
        ;; It may have been created earlier from a Remote buffer.
        (with-current-buffer (get-buffer-create " *Bookmarks*")
          (setq default-directory client-directory))
        (funcall orig file))
    (funcall orig file)))

(defun my/bookmark--records ()
  "Return the current bookmark records."
  (my/bookmark--load)
  bookmark-alist)

(defun my/bookmark--name (bookmark)
  "Return BOOKMARK's name."
  (car bookmark))

(defun my/bookmark--kind (bookmark)
  "Return BOOKMARK's kind."
  (or (alist-get 'my-kind (cdr bookmark))
      'bookmark))

(defun my/bookmark--file (bookmark)
  "Return BOOKMARK's filename, if any."
  (bookmark-get-filename bookmark))

(defun my/bookmark--position (bookmark)
  "Return BOOKMARK's file position, if any."
  (bookmark-get-position bookmark))

(defun my/bookmark--line (bookmark)
  "Return BOOKMARK's line in the current file without visiting it again."
  (when-let* ((file (my/bookmark--file bookmark))
              (pos (my/bookmark--position bookmark))
              ((equal file buffer-file-name)))
    (line-number-at-pos (min pos (point-max)))))

(defun my/bookmark--line-text (&optional pos)
  "Return trimmed text for line at POS or point."
  (save-excursion
    (when pos
      (goto-char pos))
    (string-trim
     (buffer-substring-no-properties
      (line-beginning-position)
      (line-end-position)))))

(defun my/bookmark--line-name ()
  "Return an auto-generated line bookmark name for point."
  (format "line:%s:%d %s"
          (file-relative-name buffer-file-name default-directory)
          (line-number-at-pos)
          (truncate-string-to-width (my/bookmark--line-text) 60 nil nil t)))

(defun my/bookmark--line-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a line bookmark."
  (eq (my/bookmark--kind bookmark) 'line))

(defun my/bookmark--current-line-bookmark ()
  "Return the line bookmark at the current line, if any."
  (when buffer-file-name
    (let ((file buffer-file-name)
          (line (line-number-at-pos)))
      (seq-find
       (lambda (bookmark)
         (and (my/bookmark--line-bookmark-p bookmark)
              (equal (my/bookmark--file bookmark) file)
              (= (or (my/bookmark--line bookmark) -1) line)))
       (my/bookmark--records)))))

(defun my/bookmark-set-line ()
  "Create a line bookmark at the current line."
  (interactive)
  (unless buffer-file-name
    (user-error "Line bookmarks require a file-backed buffer"))
  (my/bookmark--load)
  (save-excursion
    (beginning-of-line)
    (let* ((name (my/bookmark--line-name))
           (record (bookmark-make-record-default nil nil (point))))
      (setf (alist-get 'my-kind record) 'line)
      (bookmark-store name record nil)
      (bookmark-save)
      (message "Saved line bookmark: %s" name))))

(defun my/bookmark-toggle-line ()
  "Toggle a line bookmark at the current line."
  (interactive)
  (if-let* ((bookmark (my/bookmark--current-line-bookmark)))
      (progn
        (bookmark-delete (my/bookmark--name bookmark))
        (bookmark-save)
        (message "Removed line bookmark: %s" (my/bookmark--name bookmark)))
    (my/bookmark-set-line)))

(defun my/bookmark--line-bookmarks-in-current-file ()
  "Return current-file line bookmarks sorted by line."
  (unless buffer-file-name
    (user-error "Line bookmark navigation requires a file-backed buffer"))
  (sort
   (seq-filter
    (lambda (bookmark)
      (and (my/bookmark--line-bookmark-p bookmark)
           (equal (my/bookmark--file bookmark) buffer-file-name)))
    (my/bookmark--records))
   (lambda (a b)
     (< (or (my/bookmark--line a) 0)
        (or (my/bookmark--line b) 0)))))

(defun my/bookmark-next-line ()
  "Jump to the next line bookmark in the current file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (bookmarks (my/bookmark--line-bookmarks-in-current-file))
         (target (or (seq-find
                      (lambda (bookmark)
                        (> (or (my/bookmark--line bookmark) 0) line))
                      bookmarks)
                     (car bookmarks))))
    (unless target
      (user-error "No line bookmarks in the current file"))
    (bookmark-jump (my/bookmark--name target))))

(defun my/bookmark-previous-line ()
  "Jump to the previous line bookmark in the current file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (bookmarks (reverse (my/bookmark--line-bookmarks-in-current-file)))
         (target (or (seq-find
                      (lambda (bookmark)
                        (< (or (my/bookmark--line bookmark) most-positive-fixnum) line))
                      bookmarks)
                     (car bookmarks))))
    (unless target
      (user-error "No line bookmarks in the current file"))
    (bookmark-jump (my/bookmark--name target))))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-c p F" . consult-find)
         ("C-c p g" . consult-ripgrep)
         ("C-x r j" . consult-bookmark)
         ("C-c k"   . kill-current-buffer)))

(transient-define-prefix bookmark-actions-menu ()
  "Bookmark management."
  [["Jump"
    ("j" "jump" consult-bookmark :transient transient--do-exit)
    ("o" "other window" bookmark-jump-other-window :transient transient--do-exit)
    ("l" "list" list-bookmarks :transient transient--do-exit)
    ("n" "next line" my/bookmark-next-line :transient transient--do-exit)
    ("p" "prev line" my/bookmark-previous-line :transient transient--do-exit)]
   ["Manage"
    ("m" "set" bookmark-set :transient transient--do-exit)
    ("M" "set without overwrite" bookmark-set-no-overwrite :transient transient--do-exit)
    ("t" "toggle line" my/bookmark-toggle-line :transient transient--do-exit)
    ("L" "set line" my/bookmark-set-line :transient transient--do-exit)
    ("r" "rename" bookmark-rename :transient transient--do-exit)
    ("d" "delete" bookmark-delete :transient transient--do-exit)
    ("i" "insert" bookmark-insert :transient transient--do-exit)
    ("s" "save" bookmark-save :transient transient--do-exit)]])

(general-define-key :keymaps 'global "C-x r ." #'bookmark-actions-menu)

(defun my/swap-window-dwim ()
  "Swap the current window with another window.
When only two windows are visible, swap with the other one directly.
Otherwise, use `ace-window' to choose the target window."
  (interactive)
  (unless (> (count-windows) 1)
    (user-error "Need at least two windows to swap"))
  (let ((source (selected-window))
        (target nil))
    (setq target
          (if (= (count-windows) 2)
              (next-window source nil t)
            (progn
              (require 'ace-window)
              (aw-select "Swap with window"))))
    (unless (and (window-live-p target)
                 (not (eq source target)))
      (user-error "No target window selected"))
    (window-swap-states source target)))

(defvar my/noema--ace-hint-buffers nil
  "Noema xwidget buffers currently showing ace-window hints.")

(defun my/noema--ace-window-lead (path leaf)
  "Show ace-window PATH inside Noema xwidgets or on Emacs window LEAF."
  (let* ((window (cdr leaf))
         (buffer (window-buffer window)))
    (if (and (fboundp 'my/noema--xwidget-buffer-p)
             (my/noema--xwidget-buffer-p buffer)
             (fboundp 'my/noema-command))
        (let ((label (if (eq aw-leading-char-style 'char)
                         (string (avy--key-to-char (car (last path))))
                       (mapconcat (lambda (key) (string (avy--key-to-char key)))
                                  (reverse path) ""))))
          (cl-pushnew buffer my/noema--ace-hint-buffers)
          (with-current-buffer buffer
            (my/noema-command "window-hint" `((label . ,label)))))
      (aw--lead-overlay path leaf))))

(defun my/noema--ace-window-remove ()
  "Remove ace-window hints from Emacs and Noema windows."
  (aw--remove-leading-chars)
  (dolist (buffer my/noema--ace-hint-buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (my/noema-command "window-hint-clear"))))
  (setq my/noema--ace-hint-buffers nil))

(use-package ace-window
  :bind (("M-o" . ace-window)
         ("M-O" . my/swap-window-dwim)
         ("C-c w s" . my/swap-window-dwim))
  :config
  (setq aw--lead-overlay-fn #'my/noema--ace-window-lead
        aw--remove-leading-chars-fn #'my/noema--ace-window-remove))

;; split proportionally from all sibling windows, not just the split one
(setopt window-combination-resize t)

(defun my/toggle-delete-other-windows ()
  "Delete other windows, or restore the previous layout if already sole window.
Relies on `winner-mode' (enabled below) to undo a prior delete."
  (interactive)
  (if (and winner-mode
           (eq (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(general-define-key
 :keymaps 'global
 "C-x 1" #'my/toggle-delete-other-windows
 "C-x SPC" #'toggle-one-window
 "M-w" #'delete-window)

(use-package winner
  :ensure nil
  :config (winner-mode 1))

(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings 'meta))

;;; Buffer management helpers

(defun my/window-dashboard-buffer-p (buffer)
  "Return non-nil if BUFFER is the startup dashboard."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'dashboard-mode))))

(defun my/window-skip-dashboard-and-xwidget-p (_window buffer bury-or-kill)
  "Skip dashboard and xwidget BUFFER during automatic window fallback.
BURY-OR-KILL is non-nil when `switch-to-prev-buffer' is replacing a
buffer that is being buried, killed, or quit.  Keep these buffers
available to explicit buffer-switching commands."
  (and bury-or-kill
       (buffer-live-p buffer)
       (or (my/window-dashboard-buffer-p buffer)
           (with-current-buffer buffer
             (derived-mode-p 'xwidget-webkit-mode)))))

(setopt switch-to-prev-buffer-skip
        #'my/window-skip-dashboard-and-xwidget-p)

(defun my/window--return-buffer (window closing)
  "Return WINDOW's newest valid previous buffer other than CLOSING.
Respect the same frame predicate and bury/kill skip rule that Emacs uses
when choosing a fallback.  Exact popup restoration remains native."
  (let ((predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    (seq-find
     (lambda (buffer)
       (and (buffer-live-p buffer)
            (not (eq buffer closing))
            (or (null predicate) (funcall predicate buffer))
            (not (my/window-skip-dashboard-and-xwidget-p window buffer t))
            (not (equal (buffer-name buffer) "*scratch*"))
            (not (string-prefix-p " " (buffer-name buffer)))))
     (mapcar #'car (window-prev-buffers window)))))

(defun my/window--repair-after-close (window closing previous)
  "Complete WINDOW's close transition after CLOSING, using PREVIOUS.
The native close action owns exact popup restoration and window deletion.
Only a generic scratch/dashboard fallback needs further routing."
  (when (and (window-live-p window)
             (not (eq (window-buffer window) closing))
             (or (equal (buffer-name (window-buffer window)) "*scratch*")
                 (my/window-dashboard-buffer-p (window-buffer window))))
    (cond
     ((buffer-live-p previous)
      (set-window-buffer window previous))
     ((window-parent window)
      (delete-window window))
     ((and (not (my/window-dashboard-buffer-p closing))
           (fboundp 'dashboard-open))
      (with-selected-window window (dashboard-open))))))

(defun my/window-quit-return-a (orig-fn &optional kill window)
  "Make ORIG-FN return to this window's previous page when possible."
  (let* ((window (or window (selected-window)))
         (closing (and (window-live-p window) (window-buffer window)))
         (previous (and closing (my/window--return-buffer window closing))))
    (prog1 (funcall orig-fn kill window)
      (when closing
        (my/window--repair-after-close window closing previous)))))

(let ((native-comp-enable-subr-trampolines nil))
  (unless (advice-member-p #'my/window-quit-return-a 'quit-window)
    (advice-add 'quit-window :around #'my/window-quit-return-a)))

(defun my/window--repair-killed-buffer (closing windows)
  "Settle WINDOWS after CLOSING is actually killed."
  (unless (buffer-live-p closing)
    (dolist (entry windows)
      (my/window--repair-after-close (car entry) closing (cdr entry)))))

(defun my/window-kill-buffer-h ()
  "Record visible windows before the current buffer is killed."
  (let* ((closing (current-buffer))
         (windows (mapcar (lambda (window)
                            (cons window
                                  (my/window--return-buffer window closing)))
                          (get-buffer-window-list closing nil t))))
    (when windows
      (run-at-time 0 nil #'my/window--repair-killed-buffer closing windows))))

(add-hook 'kill-buffer-hook #'my/window-kill-buffer-h)

(defun my/window-bury-current-buffer ()
  "Bury the current buffer and settle a scratch/dashboard fallback."
  (interactive)
  (let* ((window (selected-window))
         (closing (window-buffer window))
         (previous (my/window--return-buffer window closing)))
    (bury-buffer)
    (my/window--repair-after-close window closing previous)))

(defun my/kill-buffer-dwim ()
  "Kill the current buffer, or if it's a special/read-only buffer, bury it."
  (interactive)
  (if (or buffer-read-only
          (and (string-prefix-p "*" (buffer-name))
               (not (buffer-file-name))))
      (let* ((closing (current-buffer))
             (restore (window-parameter nil 'quit-restore))
             (previous (window-parameter nil 'quit-restore-prev)))
        ;; Preserve the existing bury/quit choice.  Only repair fallback
        ;; after burying, since quit-window is handled by its own advice.
        (if (or (eq (nth 3 restore) closing)
                (eq (nth 3 previous) closing))
            (quit-window)
          (my/window-bury-current-buffer)))
    (kill-current-buffer)))

(defun my/kill-other-buffers ()
  "Kill all buffers except the current one and important special buffers."
  (interactive)
  (let ((current (current-buffer))
        (killed 0))
    (dolist (buf (buffer-list))
      (unless (or (eq buf current)
                  (string-prefix-p " " (buffer-name buf))
                  (get-buffer-process buf))
        (kill-buffer buf)
        (cl-incf killed)))
    (message "Killed %d buffer(s)" killed)))

(defun my/kill-workspace-other-buffers ()
  "Kill all buffers in the current workspace except the current buffer."
  (interactive)
  (if (and (bound-and-true-p persp-mode) (fboundp 'persp-current-buffers))
      (let ((current (current-buffer))
            (bufs (persp-current-buffers))
            (killed 0))
        (dolist (buf bufs)
          (unless (or (eq buf current)
                      (get-buffer-process buf))
            (kill-buffer buf)
            (cl-incf killed)))
        (message "Killed %d buffer(s) in workspace" killed))
    (my/kill-other-buffers)))

;;; Frame management helpers

(defun my/frame-new ()
  "Create a new Emacs frame."
  (interactive)
  (select-frame (if (fboundp 'my/background-make-frame)
                    (my/background-make-frame)
                  (make-frame))))

(defun my/frame-open-with (visit)
  "Create a neutral frame and run VISIT there.
Delete the new frame if VISIT fails, then restore focus to the origin."
  (let ((origin (selected-frame))
        frame completed result)
    (unwind-protect
        (progn
          (setq frame (my/frame-new))
          (setq result (with-selected-frame frame (funcall visit)))
          (setq completed t)
          (select-frame-set-input-focus frame)
          result)
      (unless completed
        (when (and frame (frame-live-p frame))
          (delete-frame frame t))
        (when (frame-live-p origin)
          (select-frame-set-input-focus origin))))))

(defun my/file-open-new-frame (file &optional wildcards)
  "Visit FILE in a newly created Emacs frame.
WILDCARDS has the same meaning as in `find-file'.  Release the new frame if
visiting FILE fails.  Resolve FILE before frame creation so a logical
source buffer keeps its target."
  (interactive
   (find-file-read-args "Find file in new frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((file (my/open-resolve-file-name file)))
    (my/frame-open-with
     (lambda ()
       (let ((find-file-run-dired t))
         (my/open-visit-file file wildcards))))))

(defalias 'file-open-new-frame #'my/file-open-new-frame
  "Visit a file or directory in a newly created Emacs frame.")

(defun my/buffer-display-new-frame (buffer &optional norecord)
  "Display BUFFER in a fresh Emacs frame and return the displayed buffer.
NORECORD has the same meaning as in `switch-to-buffer'.  A Noema page gets
its own editable pane because its xwidget cannot be shared between frames."
  (let ((existing (get-buffer buffer)))
    (cond
     ((and existing
           (eq (buffer-local-value 'major-mode existing)
               'xwidget-webkit-mode)
           (fboundp 'my/ibuffer-noema-file-name)
           (my/ibuffer-noema-file-name existing))
      (unless (fboundp 'my/noema-open-file-split-frame)
        (user-error "Noema frame split is unavailable"))
      (my/noema-open-file-split-frame
       (my/ibuffer-noema-file-name existing)))
     ((and existing
           (eq (buffer-local-value 'major-mode existing)
               'xwidget-webkit-mode))
      (user-error "Open this xwidget page through its own new-window command"))
     (t
      (my/frame-open-with
       (lambda () (switch-to-buffer buffer norecord)))))))

(defun my/frame-kill ()
  "Delete the current frame, or quit Emacs if it's the last one."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))

(defun my/frame-other ()
  "Switch to the next Emacs frame."
  (interactive)
  (other-frame 1))

(defun my/frame-previous ()
  "Switch to the previous Emacs frame."
  (interactive)
  (other-frame -1))

;;;; =========================
;;;; Dirvish (Dired UI) - practical + pretty + icons
;;;; =========================

(use-package dirvish
  :ensure t
  :after dired
  :init
 (add-to-list 'load-path
               (expand-file-name "extensions"
                                 (file-name-directory (locate-library "dirvish"))))
  ;; 让 Dirvish 接管 dired（放 init 最合适）
  (dirvish-override-dired-mode)

  :bind (("C-c o d" . dirvish-dwim)
         ("C-c o f" . dirvish-fd)
         :map dirvish-mode-map
         ;; Single-key bindings follow yazi and live in init-dirvish-keys.el.
         ("M-t" . dirvish-layout-toggle)
         ("M-b" . dirvish-history-go-backward)
         ("M-f" . dirvish-history-go-forward)
         ("M-n" . dirvish-narrow)
         ("M-m" . dirvish-mark-menu)
         ;("P" . dirvish-peek-mode) ; 如果没有该函数，下面 config 会兜底处理
         ;; 鼠标（可选）
         ("<mouse-1>" . dirvish-subtree-toggle-or-open)
         ("<mouse-2>" . dired-mouse-find-file-other-window)
         ("<mouse-3>" . dired-mouse-find-file))
  :custom
  (dirvish-side-width 36)
  (dirvish-header-line-height '(26 . 36))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format
   '(:left (sort symlink remote-files) :right (vc-info yank index)))

  :config
  ;; The pending choice of a pop-up folder, or the transfers running on this
  ;; folder's target.  The library is only consulted once something loaded it.
  (dirvish-define-mode-line remote-files
    "Bulk transfer state of this folder."
    (when (fboundp 'remote-files-mode-line-text)
      (remote-files-mode-line-text)))
  (define-key dirvish-mode-map (kbd "RET") #'dired-find-file)
  (define-key dirvish-mode-map (kbd "<return>") #'dired-find-file)
  (dolist (key '("M-S-RET" "M-S-<return>"))
    (define-key dirvish-mode-map (kbd key) #'dirvish-open-new-frame))
  (with-eval-after-load 'evil
    (evil-define-key* 'normal dirvish-mode-map (kbd "RET") #'dired-find-file)
    (evil-define-key* 'normal dirvish-mode-map (kbd "<return>") #'dired-find-file)
    (dolist (key '("M-S-RET" "M-S-<return>"))
      (evil-define-key* 'normal dirvish-mode-map (kbd key)
        #'dirvish-open-new-frame)))
  ;; 1) 先给一个“基础属性集合”（绝对不会触发 dirvish-vc）
  (setq dirvish-attributes
        '(file-size subtree-state collapse file-time))

  ;; 2) Peek / Side-follow：有就开（没有就跳过，不报错）
  (when (fboundp 'dirvish-side-follow-mode)
    (dirvish-side-follow-mode 1))

  ;; 3) VC/Git：尝试静默加载 dirvish-vc，成功才追加属性
  (when (require 'dirvish-vc nil t)
    (setq dirvish-attributes
          '(vc-state git-msg file-size subtree-state collapse file-time)))

  ;; Dirvish normally starts a second Emacs to collect attributes.  A target
  ;; buffer can send that helper to the wrong machine; a logical local buffer
  ;; can race its sentinel against Dirvish setup.  Skip the helper for both
  ;; forms of Remote identity and use the listing Dired already supplied.
  (defun my/dirvish-dir-data-async-remote-a (function &rest args)
    "Avoid Dirvish's helper for a logical Remote Dired buffer.
Dirvish may pass the target's native directory to this private function,
including for `/fs:local:'; inspect the owning buffer as well.  Unknown
upstream call shapes delegate unchanged."
    (let ((directory (car args))
          (buffer (cadr args)))
      (if (and (<= 2 (length args) 3)
               (stringp directory)
               (bufferp buffer)
               (or (file-remote-p directory)
                   (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (and (stringp default-directory)
                               (string-prefix-p "/fs:" default-directory))))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (dirvish-prop :vc-backend 0)
              (dirvish-data-for-dir directory buffer (nth 2 args))))
        (apply function args))))

  (advice-remove
   'dirvish--dir-data-async #'my/dirvish-dir-data-async-remote-a)
  (advice-add
   'dirvish--dir-data-async
   :around #'my/dirvish-dir-data-async-remote-a)

  ;; 4) 如果你希望 P 键在没有 peek 函数时也不报错，做一个安全绑定
  (unless (fboundp 'dirvish-peek-mode)
    (define-key dirvish-mode-map (kbd "P") #'ignore)))

; Nerd Font 图标区（PUA）专用回退：只管图标，不污染符号/数学
(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-fontset-font t '(#xe000 . #xf8ff) "JetBrainsMono Nerd Font" nil 'append)
  (set-fontset-font t '(#xf0000 . #xffffd) "JetBrainsMono Nerd Font" nil 'append))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)
  :bind (("C-x r m" . bookmark-set)
         ("C-x r l" . list-bookmarks)
         ("C-x r n" . my/bookmark-next-line)
         ("C-x r p" . my/bookmark-previous-line)
         ("C-x r o" . bookmark-jump-other-window))
  :config
  (unless (advice-member-p #'my/bookmark-write-file-on-client-a
                           'bookmark-write-file)
    (advice-add 'bookmark-write-file :around
                #'my/bookmark-write-file-on-client-a)))



(provide 'init-windows)
;;; init-base.el ends here

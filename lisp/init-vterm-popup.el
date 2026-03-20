;;; init-vterm-popup.el --- Popup vterm pool -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm-send-string "vterm" (string))
(declare-function vterm-send-return "vterm" ())
(declare-function project-root "project" (project))
(declare-function my/project-current-root "init-project")

(defvar vterm-kill-buffer-on-exit)

(defgroup my/vterm-popup nil
  "Popup vterm pool helpers."
  :group 'term
  :prefix "my/vterm-popup-")

(defcustom my/vterm-popup-window-height 0.2
  "Height of the popup vterm side window."
  :type 'number
  :group 'my/vterm-popup)

(defcustom my/project-popup-vterm-apps
  '(("lazygit" . "lazygit")
    ("btop" . "btop")
    ("yazi" . "yazi")
    ("codex" . "codex"))
  "Preset terminal apps launched in a fresh popup vterm at project root."
  :type '(alist :key-type string :value-type string)
  :group 'my/vterm-popup)

(defvar my/vterm-popup-buffers nil
  "Popup vterm buffers in creation order.")

(defvar my/vterm-popup-current-buffer nil
  "Current popup vterm buffer.")

(defvar-local my/vterm-popup-fixed nil
  "Whether this popup vterm buffer should stay visible after focus changes.")

(defvar-local my/vterm-popup-instance-p nil
  "Whether the current buffer belongs to the popup vterm pool.")

(defun my/vterm-popup--buffer-p (buffer)
  "Return non-nil when BUFFER is a live popup vterm buffer."
  (and (buffer-live-p buffer)
       (buffer-local-value 'my/vterm-popup-instance-p buffer)))

(defun my/vterm-popup--live-buffers ()
  "Return live popup vterm buffers, pruning dead entrbuffer: init-vterm-popup.el
file: ~/.config/emacs/lisp/init-vterm-popup.el
major mode: emacs-lisp-mode
default-directory: ~/.config/emacs/lisp/
project root: ~/.config/emacs/
route policy: eglot (prog-mode default)
active backend: -
required lsp feature: - (ready)
matching custom eglot mapping: -
flymake/company/breadcrumb: on / off / on
local eglot workspace config: unset
ies."
  (setq my/vterm-popup-buffers
        (cl-remove-if-not #'my/vterm-popup--buffer-p my/vterm-popup-buffers)))

(defun my/vterm-popup--current-buffer (&optional create)
  "Return the current popup vterm buffer.
When CREATE is non-nil, create one if the pool is empty."
  (let ((buffers (my/vterm-popup--live-buffers)))
    (unless (my/vterm-popup--buffer-p my/vterm-popup-current-buffer)
      (setq my/vterm-popup-current-buffer (car buffers)))
    (or my/vterm-popup-current-buffer
        (when create
          (my/vterm-popup--create-buffer)))))

(defun my/vterm-popup--buffer-name (index)
  "Return the default popup vterm buffer name for INDEX."
  (if (= index 1)
      "*vterm-popup*"
    (format "*vterm-popup:%d*" index)))

(defun my/vterm-popup--next-buffer-name ()
  "Return the next available popup vterm buffer name."
  (let ((index 1)
        name)
    (while
        (progn
          (setq name (my/vterm-popup--buffer-name index))
          (setq index (1+ index))
          (get-buffer name)))
    name))

(defun my/vterm-popup--window (&optional buffer)
  "Return the popup vterm window on the selected frame.
When BUFFER is non-nil, return the window displaying BUFFER."
  (catch 'window
    (dolist (window (window-list (selected-frame) 'no-minibuf))
      (when (and (window-parameter window 'my-vterm-popup)
                 (or (null buffer)
                     (eq (window-buffer window) buffer)))
        (throw 'window window)))))

(defun my/vterm-popup--show-buffer (buffer)
  "Show popup vterm BUFFER in the shared side window."
  (setq my/vterm-popup-current-buffer buffer)
  (let ((window (or (my/vterm-popup--window)
                    (display-buffer-in-side-window
                     buffer
                     `((side . top)
                       (slot . 1)
                       (window-height . ,my/vterm-popup-window-height))))))
    (set-window-buffer window buffer)
    (set-window-parameter window 'my-vterm-popup t)
    (set-window-parameter
     window 'my-vterm-fixed
     (buffer-local-value 'my/vterm-popup-fixed buffer))
    (set-window-parameter window 'no-delete-other-windows t)
    (window-preserve-size window nil t)
    window))

(defun my/vterm-popup--on-kill ()
  "Clean up popup vterm state when the current buffer is killed."
  (let* ((buffer (current-buffer))
         (buffers (my/vterm-popup--live-buffers))
         (tail (member buffer buffers))
         (next (or (cadr tail) (car buffers)))
         (window (my/vterm-popup--window buffer)))
    (setq my/vterm-popup-buffers (delq buffer my/vterm-popup-buffers))
    (when (eq my/vterm-popup-current-buffer buffer)
      (setq my/vterm-popup-current-buffer
            (and next
                 (not (eq next buffer))
                 next)))
    (when window
      (ignore-errors (delete-window window)))))

(defun my/vterm-popup--create-buffer (&optional directory buffer-name)
  "Create and register a new popup vterm buffer.
Use DIRECTORY as `default-directory' when non-nil.
Use BUFFER-NAME when non-nil."
  (require 'vterm)
  (let ((default-directory (or directory default-directory))
        (target-name (generate-new-buffer-name
                      (or buffer-name
                          (my/vterm-popup--next-buffer-name)))))
    (with-current-buffer (save-window-excursion
                           (vterm target-name))
      (setq-local my/vterm-popup-instance-p t)
      (setq-local my/vterm-popup-fixed nil)
      (add-hook 'kill-buffer-hook #'my/vterm-popup--on-kill nil t)
      (setq my/vterm-popup-buffers
            (append (my/vterm-popup--live-buffers) (list (current-buffer))))
      (setq my/vterm-popup-current-buffer (current-buffer))
      (current-buffer))))

(defun my/vterm-popup--project-root ()
  "Return the current project root.
Signal a user error when outside a project."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      (when (fboundp 'project-current)
        (when-let* ((project (project-current nil default-directory)))
          (project-root project)))
      (user-error "Not inside a project")))

(defun my/vterm-popup--project-name (project-root)
  "Return a short project name for PROJECT-ROOT."
  (file-name-nondirectory
   (directory-file-name
    (file-name-as-directory
     (expand-file-name project-root)))))

(defun my/project-popup-vterm-app (app)
  "Run APP in a fresh popup vterm rooted at the current project."
  (interactive
   (list
    (completing-read "Project terminal app: "
                     (mapcar #'car my/project-popup-vterm-apps)
                     nil t)))
  (let* ((project-root (file-name-as-directory
                        (expand-file-name (my/vterm-popup--project-root))))
         (command (or (cdr (assoc app my/project-popup-vterm-apps))
                      (user-error "Unknown project terminal app: %s" app)))
         (buffer-name (format "*vterm-popup:%s:%s*"
                              app
                              (my/vterm-popup--project-name project-root)))
         (buffer (my/vterm-popup--create-buffer project-root buffer-name)))
    (with-current-buffer buffer
      (setq-local vterm-kill-buffer-on-exit t)
      (vterm-send-string (format "clear; %s; exit" command))
      (vterm-send-return))
    (select-window (my/vterm-popup--show-buffer buffer))
    buffer))

(defun my/vterm-popup--next-buffer ()
  "Return the next popup vterm buffer in creation order."
  (let* ((buffers (my/vterm-popup--live-buffers))
         (current (my/vterm-popup--current-buffer)))
    (cond
     ((null buffers)
      nil)
     ((null current)
      (car buffers))
     (t
      (or (cadr (member current buffers))
          (car buffers))))))

(defun my/vterm-popup--read-buffer (prompt)
  "Read a popup vterm buffer with PROMPT."
  (let* ((buffers (my/vterm-popup--live-buffers))
         (current (my/vterm-popup--current-buffer))
         (default (and current (buffer-name current))))
    (unless buffers
      (user-error "No popup vterm buffers"))
    (get-buffer
     (completing-read prompt
                      (mapcar #'buffer-name buffers)
                      nil t nil nil default))))

(defun my/vterm-hide-popup ()
  "Hide the current popup vterm window."
  (interactive)
  (when-let* ((window (my/vterm-popup--window)))
    (ignore-errors (delete-window window))))

(defun my/vterm-show-popup ()
  "Show the current popup vterm buffer."
  (interactive)
  (my/vterm-popup--show-buffer (my/vterm-popup--current-buffer t)))

(defun my/vterm-popup-cycle (arg)
  "Cycle popup vterm buffers.
With prefix ARG, create a new popup vterm and switch to it."
  (interactive "P")
  (let ((buffer (if arg
                    (my/vterm-popup--create-buffer)
                  (or (my/vterm-popup--next-buffer)
                      (my/vterm-popup--create-buffer)))))
    (select-window (my/vterm-popup--show-buffer buffer))
    buffer))

(defun my/vterm-popup-kill (buffer)
  "Kill popup vterm BUFFER."
  (interactive
   (list (if current-prefix-arg
             (my/vterm-popup--read-buffer "Kill popup vterm: ")
           (or (my/vterm-popup--current-buffer)
               (user-error "No popup vterm buffers")))))
  (kill-buffer buffer))

(defun my/vterm-popup-rename (buffer name)
  "Rename popup vterm BUFFER to NAME."
  (interactive
   (let* ((buffer (if current-prefix-arg
                      (my/vterm-popup--read-buffer "Rename popup vterm: ")
                    (or (my/vterm-popup--current-buffer)
                        (user-error "No popup vterm buffers"))))
          (current-name (buffer-name buffer)))
     (list buffer
           (read-string "New popup vterm name: " current-name nil current-name))))
  (when-let* ((existing (get-buffer name)))
    (unless (eq existing buffer)
      (user-error "Buffer %s already exists" name)))
  (with-current-buffer buffer
    (rename-buffer name))
  buffer)

(defun my/vterm-toggle-fixed ()
  "Toggle fixed mode for the current popup vterm buffer."
  (interactive)
  (let* ((buffer (my/vterm-popup--current-buffer t))
         (fixed (with-current-buffer buffer
                  (setq-local my/vterm-popup-fixed (not my/vterm-popup-fixed))
                  my/vterm-popup-fixed))
         (window (my/vterm-popup--show-buffer buffer)))
    (set-window-parameter window 'my-vterm-fixed fixed)
    (select-window window)
    (message "Popup vterm %s %s"
             (buffer-name buffer)
             (if fixed "fixed" "temporary"))))

(defun my/vterm-popup--auto-hide (&rest _)
  "Hide the popup vterm window when a temporary instance loses focus."
  (when-let* ((window (my/vterm-popup--window)))
    (let ((buffer (window-buffer window)))
      (unless (or (active-minibuffer-window)
                  (eq (selected-window) window)
                  (buffer-local-value 'my/vterm-popup-fixed buffer))
        (my/vterm-hide-popup)))))

(defun vterm-toggle ()
  "Toggle the current popup vterm buffer."
  (interactive)
  (let* ((buffer (my/vterm-popup--current-buffer t))
         (window (my/vterm-popup--window)))
    (cond
     ((and window
           (eq (window-buffer window) buffer)
           (eq (selected-window) window))
      (my/vterm-hide-popup))
     ((and window
           (eq (window-buffer window) buffer))
      (select-window window))
     (t
      (select-window (my/vterm-popup--show-buffer buffer))))))

(global-set-key (kbd "M-`") #'vterm-toggle)
(global-set-key (kbd "C-c e") #'vterm-toggle)
(global-set-key (kbd "C-c E") #'my/vterm-popup-cycle)
(global-set-key (kbd "C-c M-e") #'my/vterm-toggle-fixed)

(add-hook 'window-selection-change-functions #'my/vterm-popup--auto-hide)

(provide 'init-vterm-popup)
;;; init-vterm-popup.el ends here

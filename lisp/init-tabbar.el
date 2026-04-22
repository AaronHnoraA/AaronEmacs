;;; init-tabbar.el --- Top tab and buffer bars -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar persp-mode)
(defvar tab-line-exclude-modes)
(defvar tab-line-tab-map)

(declare-function centaur-tabs-mode "centaur-tabs" (&optional arg))
(declare-function global-tab-line-mode "tab-line" (&optional arg))
(declare-function persp-current-buffers* "perspective" (&optional include-global))
(declare-function persp-current-name "perspective" ())
(declare-function tab-line-force-update "tab-line" (all))
(declare-function tab-bar-tabs "tab-bar" ())

(defface my/tab-bar-buffer-face
  '((t (:inherit tab-bar-tab
        :foreground "#d8dee9"
        :weight semibold)))
  "Face used for the current buffer label in the top tab bar."
  :group 'tab-bar)

(defface my/tab-bar-buffer-path-face
  '((t (:inherit tab-bar
        :foreground "#8f96ad")))
  "Face used for secondary path text in the top tab bar."
  :group 'tab-bar)

(defface my/tab-line-current-face
  '((t (:foreground "#e7d6a5"
        :weight semibold)))
  "Face used for the selected buffer in the centered tab line."
  :group 'tab-line)

(defface my/tab-line-inactive-face
  '((t (:foreground "#8f96ad")))
  "Face used for inactive buffers in the centered tab line."
  :group 'tab-line)

(defface my/tab-line-separator-face
  '((t (:foreground "#5f6578")))
  "Face used for separators in the centered tab line."
  :group 'tab-line)

(defun my/tab-bar--current-buffer ()
  "Return the buffer shown in the selected window."
  (window-buffer (minibuffer-selected-window)))

(defun my/tab-bar--buffer-title ()
  "Return the current buffer title for the custom top tab bar."
  (with-current-buffer (my/tab-bar--current-buffer)
    (cond
     ((and buffer-file-name default-directory)
      (let ((dir (file-name-nondirectory
                  (directory-file-name
                   (expand-file-name default-directory)))))
        (concat
         (propertize (format "%s/" dir) 'face 'my/tab-bar-buffer-path-face)
         (propertize (file-name-nondirectory buffer-file-name)
                     'face 'my/tab-bar-buffer-face))))
     (t
      (propertize (buffer-name) 'face 'my/tab-bar-buffer-face)))))

(defun my/tab-bar-format-buffer ()
  "Render the left-side current buffer label."
  (list
   (concat
    (propertize " " 'face 'tab-bar)
    (my/tab-bar--buffer-title)
    (propertize " " 'face 'tab-bar))))

(defun my/tab-bar-tab-name-format (tab _index)
  "Format TAB as a restrained workspace pill."
  (let* ((name (alist-get 'name tab))
         (current (eq (car tab) 'current-tab))
         (face (if current 'tab-bar-tab 'tab-bar-tab-inactive)))
    (concat
     (propertize " " 'face face)
     (propertize name 'face face)
     (propertize " " 'face face))))

(defun my/tab-bar-ensure-visible (&optional frame)
  "Force the top-level tab bar to be visible on FRAME.
When FRAME is nil, use the selected frame."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (with-selected-frame frame
        (setopt tab-bar-show 0)
        (tab-bar-mode 1)
        (set-frame-parameter frame 'tab-bar-lines 1)
        (modify-frame-parameters frame '((tab-bar-lines . 1)))
        (force-mode-line-update t)
        (redraw-frame frame)))))

(defun my/tab-line-buffer-list ()
  "Return buffers that belong to the current workspace."
  (let* ((buffers (if (and (bound-and-true-p persp-mode)
                           (fboundp 'persp-current-buffers*))
                      (persp-current-buffers* t)
                    (buffer-list)))
         (current (current-buffer))
         (filtered
          (seq-filter
           (lambda (buffer)
             (and (buffer-live-p buffer)
                  (or (eq buffer current)
                      (with-current-buffer buffer
                        (and (not (derived-mode-p 'dashboard-mode))
                             (or buffer-file-name
                                 (derived-mode-p 'dired-mode
                                                 'eshell-mode
                                                 'shell-mode
                                                 'term-mode
                                                 'vterm-mode)
                                 (let ((name (buffer-name buffer)))
                                   (and (stringp name)
                                        (not (string-prefix-p " " name))
                                        (not (string-prefix-p "*" name))))))))))
           buffers)))
    (delete-dups
     (if (memq current filtered)
         (copy-sequence filtered)
       (cons current filtered)))))

(defun my/tab-line-hidden-p ()
  "Return non-nil when the centered buffer tab line should stay hidden."
  (or (derived-mode-p 'dashboard-mode)
      (minibufferp)))

(defun my/tab-line-tab-string (buffer)
  "Return the clickable tab label for BUFFER."
  (let ((current (eq buffer (current-buffer)))
        (name (buffer-name buffer)))
    (propertize
     name
     'face (if current
               'my/tab-line-current-face
             'my/tab-line-inactive-face)
     'tab buffer
     'keymap tab-line-tab-map
     'mouse-face 'tab-line-highlight
     'follow-link 'ignore
     'help-echo (or (buffer-file-name buffer)
                    name))))

(defun my/tab-line-separator ()
  "Return the muted separator used between buffer tabs."
  (propertize " | " 'face 'my/tab-line-separator-face))

(defun my/tab-line-content ()
  "Return the centered buffer tabs content string."
  (let ((buffers (my/tab-line-buffer-list)))
    (when buffers
      (let ((parts (list (my/tab-line-separator))))
        (dolist (buffer buffers)
          (push (my/tab-line-tab-string buffer) parts)
          (push (my/tab-line-separator) parts))
        (apply #'concat (nreverse parts))))))

(defun my/tab-line-format ()
  "Render a centered, minimal buffer tab line."
  (unless (my/tab-line-hidden-p)
    (when-let* ((content (my/tab-line-content)))
      (let* ((content-width (string-width content))
             (window-width (window-body-width))
             (offset (if (< content-width (max 1 (- window-width 4)))
                         (/ content-width 2)
                       0)))
        (list
         (if (> offset 0)
             (propertize " "
                         'face 'tab-line
                         'display `(space :align-to (- center ,offset)))
           (propertize " " 'face 'tab-line))
         content)))))

(defun my/tab-line-refresh (&rest _)
  "Refresh the centered buffer tab line."
  (when (fboundp 'tab-line-force-update)
    (tab-line-force-update t)))

(defun my/tab-line-apply-ui ()
  "Apply the local visual treatment for the centered buffer tab line."
  (let ((bg (face-background 'default nil t)))
    (set-face-attribute 'tab-line nil
                        :background bg
                        :foreground "#8f96ad"
                        :box nil
                        :overline nil
                        :underline nil
                        :height 0.95)
    (set-face-attribute 'tab-line-active nil
                        :background bg
                        :foreground "#8f96ad"
                        :box nil
                        :overline nil
                        :underline nil)
    (set-face-attribute 'tab-line-inactive nil
                        :background bg
                        :foreground "#8f96ad"
                        :box nil
                        :overline nil
                        :underline nil)
    (set-face-attribute 'tab-line-tab nil
                        :background bg
                        :foreground "#8f96ad"
                        :box nil
                        :overline nil
                        :underline nil)
    (set-face-attribute 'tab-line-tab-current nil
                        :background bg
                        :foreground "#e7d6a5"
                        :box nil
                        :overline nil
                        :underline nil
                        :weight 'semibold)
    (set-face-attribute 'tab-line-tab-inactive nil
                        :background bg
                        :foreground "#8f96ad"
                        :box nil
                        :overline nil
                        :underline nil)
    (set-face-attribute 'tab-line-highlight nil
                        :background bg
                        :foreground "#e7d6a5"
                        :box nil
                        :overline nil
                        :underline nil)))

(use-package tab-bar
  :ensure nil
  :hook ((after-init . my/tab-bar-ensure-visible)
         (server-after-make-frame . my/tab-bar-ensure-visible))
  :custom
  (tab-bar-show 0)
  (tab-bar-tab-hints nil)
  (tab-bar-auto-width t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-format '(my/tab-bar-format-buffer
                    tab-bar-format-align-right
                    tab-bar-format-tabs))
  (tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format)
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  :config
  (when (display-graphic-p)
    (my/tab-bar-ensure-visible)))

(use-package tab-line
  :ensure nil
  :demand t
  :hook ((after-init . global-tab-line-mode)
         (after-init . my/tab-line-apply-ui)
         (server-after-make-frame . my/tab-line-apply-ui)
         (after-load-theme . my/tab-line-apply-ui))
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show nil)
  :config
  (when (fboundp 'centaur-tabs-mode)
    (ignore-errors (centaur-tabs-mode -1)))
  (advice-add 'tab-line-format :override #'my/tab-line-format)
  (add-to-list 'tab-line-exclude-modes 'dashboard-mode)
  (global-tab-line-mode 1)
  (with-eval-after-load 'perspective
    (add-hook 'persp-activated-functions #'my/tab-line-refresh))
  (my/tab-line-apply-ui)
  (my/tab-line-refresh))

(provide 'init-tabbar)
;;; init-tabbar.el ends here

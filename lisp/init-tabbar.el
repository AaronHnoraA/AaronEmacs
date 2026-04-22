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
(defvar my/vterm-popup-instance-p)

(declare-function centaur-tabs-mode "centaur-tabs" (&optional arg))
(declare-function global-tab-line-mode "tab-line" (&optional arg))
(declare-function persp-current-buffers* "perspective" (&optional include-global))
(declare-function tab-line--get-tab-property "tab-line" (prop string))
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

(defconst my/tab-line-separator-string "|"
  "Separator used between centered buffer tabs.")

(defvar my/tab-line-click-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] #'ignore)
    (define-key map [tab-line mouse-1] #'my/tab-line-select-buffer)
    (define-key map (kbd "RET") #'my/tab-line-select-current)
    map)
  "Keymap for clickable centered buffer tabs.")

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
                    (buffer-list))))
    (delete-dups
     (seq-filter #'my/tab-line-visible-buffer-p buffers))))

(defun my/tab-line-starred-buffer-p (buffer)
  "Return non-nil when BUFFER uses the conventional `*name*' wrapper."
  (let ((name (buffer-name buffer)))
    (and (stringp name)
         (string-prefix-p "*" name)
         (string-suffix-p "*" name))))

(defun my/tab-line-visible-buffer-p (buffer)
  "Return non-nil when BUFFER should appear in the centered tab line."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((name (buffer-name buffer)))
           (and (not (derived-mode-p 'dashboard-mode))
                (not (bound-and-true-p my/vterm-popup-instance-p))
                (not (and (stringp name)
                          (string-match-p "vterm-pop" name)))
                (not (my/tab-line-starred-buffer-p buffer))
                (or buffer-file-name
                    (derived-mode-p 'dired-mode
                                    'eshell-mode
                                    'shell-mode
                                    'term-mode
                                    'vterm-mode)
                    (and (stringp name)
                         (not (string-prefix-p " " name)))))))))

(defun my/tab-line-hidden-p ()
  "Return non-nil when the centered buffer tab line should stay hidden."
  (or (derived-mode-p 'dashboard-mode)
      (minibufferp)
      (not (my/tab-line-visible-buffer-p (current-buffer)))))

(defun my/tab-line-current-buffer ()
  "Return the currently selected visible buffer for the centered tab line."
  (or (current-buffer)
      (car (my/tab-line-buffer-list))))

(defun my/tab-line-select-current ()
  "No-op keyboard entrypoint for the current centered tab item."
  (interactive))

(defun my/tab-line-select-buffer (event)
  "Select the clicked buffer tab from EVENT."
  (interactive "e")
  (let* ((posnp (event-start event))
         (string (car-safe (posn-string posnp)))
         (buffer (and string
                      (tab-line--get-tab-property 'tab string))))
    (when (buffer-live-p buffer)
      (with-selected-window (posn-window posnp)
        (let ((switch-to-buffer-obey-display-actions nil))
          (switch-to-buffer buffer))
        (my/tab-line-refresh)))))

(defun my/tab-line-tab-string (buffer)
  "Return the clickable tab label for BUFFER."
  (let ((current (eq buffer (my/tab-line-current-buffer)))
        (name (buffer-name buffer)))
    (propertize
     (format " %s " name)
     'face (if current
               'my/tab-line-current-face
             'my/tab-line-inactive-face)
     'tab buffer
     'local-map my/tab-line-click-map
     'keymap my/tab-line-click-map
     'pointer 'hand
     'mouse-face 'tab-line-highlight
     'follow-link 'ignore
     'help-echo (or (buffer-file-name buffer)
                    name))))

(defun my/tab-line-separator ()
  "Return the muted separator used between buffer tabs."
  (propertize my/tab-line-separator-string
              'face 'my/tab-line-separator-face))

(defun my/tab-line-content ()
  "Return the centered buffer tabs content segments."
  (let ((buffers (my/tab-line-buffer-list)))
    (when buffers
      (let ((segments (list (propertize "(" 'face 'my/tab-line-separator-face))))
        (dolist (buffer buffers)
          (when (> (length segments) 1)
            (setq segments
                  (append segments
                          (list (my/tab-line-separator)))))
          (setq segments
                (append segments
                        (list (my/tab-line-tab-string buffer)))))
        (append segments
                (list (propertize ")" 'face 'my/tab-line-separator-face)))))))

(defun my/tab-line-format ()
  "Render a centered, minimal buffer tab line."
  (unless (my/tab-line-hidden-p)
    (when-let* ((content (my/tab-line-content)))
      (let* ((content-width (string-width (format-mode-line content)))
             (window-width (window-body-width))
             (offset (if (< content-width (max 1 (- window-width 4)))
                         (/ content-width 2)
                       0)))
        (append
         (list
          (if (> offset 0)
              (propertize " "
                          'face 'tab-line
                          'display `(space :align-to (- center ,offset)))
            (propertize " " 'face 'tab-line)))
         content)))))

(defun my/tab-line-refresh (&rest _)
  "Refresh the centered buffer tab line."
  (when (fboundp 'tab-line-force-update)
    (tab-line-force-update t)))

(defun my/tab-line-apply-ui ()
  "Apply the local visual treatment for the centered buffer tab line."
  (set-face-attribute 'tab-line nil
                      :background 'unspecified
                      :foreground "#8f96ad"
                      :box nil
                      :overline nil
                      :underline nil
                      :height 0.95)
  (set-face-attribute 'tab-line-active nil
                      :background 'unspecified
                      :foreground "#8f96ad"
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'tab-line-inactive nil
                      :background 'unspecified
                      :foreground "#8f96ad"
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'tab-line-tab nil
                      :background 'unspecified
                      :foreground "#8f96ad"
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'tab-line-tab-current nil
                      :background 'unspecified
                      :foreground "#e7d6a5"
                      :box nil
                      :overline nil
                      :underline nil
                      :weight 'semibold)
  (set-face-attribute 'tab-line-tab-inactive nil
                      :background 'unspecified
                      :foreground "#8f96ad"
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'tab-line-highlight nil
                      :background 'unspecified
                      :foreground "#e7d6a5"
                      :box nil
                      :overline nil
                      :underline nil))

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
  :hook ((server-after-make-frame . my/tab-line-apply-ui)
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

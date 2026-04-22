;;; init-tabbar.el --- Top tab bar UI -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar persp-mode)

(declare-function centaur-tabs-buffer-update-groups "centaur-tabs" ())
(declare-function centaur-tabs-current-tabset "centaur-tabs" (&optional update))
(declare-function centaur-tabs-display-update "centaur-tabs" ())
(declare-function centaur-tabs-mode "centaur-tabs" (&optional arg))
(declare-function centaur-tabs-selected-p "centaur-tabs" (tab tabset))
(declare-function persp-current-buffers* "perspective" (&optional include-global))
(declare-function persp-current-name "perspective" ())
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
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

(defface my/centaur-tabs-active-buffer-face
  '((t (:foreground "#e7d6a5"
        :weight semibold)))
  "Face used for the primary selected buffer label."
  :group 'centaur-tabs)

(defface my/centaur-tabs-meta-face
  '((t (:foreground "#8f96ad")))
  "Face used for secondary metadata in buffer tabs."
  :group 'centaur-tabs)

(defface my/centaur-tabs-divider-face
  '((t (:foreground "#5f6578")))
  "Face used for separators inside buffer tabs."
  :group 'centaur-tabs)

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

(defun my/tab-bar-disable-tab-line ()
  "Disable stale `tab-line-mode' buffers left by earlier experiments."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p tab-line-mode)
        (tab-line-mode -1)))))

(defun my/centaur-tabs-buffer-list ()
  "Return buffers that belong to the current workspace."
  (let* ((buffers (if (and (bound-and-true-p persp-mode)
                           (fboundp 'persp-current-buffers*))
                      (persp-current-buffers* t)
                    (buffer-list)))
         (live-buffers (cl-remove-if-not #'buffer-live-p buffers))
         (current (current-buffer)))
    (delete-dups
     (if (memq current live-buffers)
         (copy-sequence live-buffers)
       (cons current live-buffers)))))

(defun my/centaur-tabs-buffer-groups ()
  "Keep the visible buffer tabs aligned with the current workspace."
  (list
   (or (and (bound-and-true-p persp-mode)
            (fboundp 'persp-current-name)
            (persp-current-name))
       "Buffers")))

(defun my/centaur-tabs--project-name (buffer)
  "Return a short project or directory name for BUFFER."
  (with-current-buffer buffer
    (when-let* ((root (or (when (fboundp 'project-current)
                            (when-let* ((project (ignore-errors
                                                   (project-current nil default-directory))))
                              (project-root project)))
                          default-directory)))
      (file-name-nondirectory
       (directory-file-name (expand-file-name root))))))

(defun my/centaur-tabs-tab-label (tab)
  "Render TAB with a restrained file-plus-project label."
  (let* ((buffer (car tab))
         (selected (centaur-tabs-selected-p tab (centaur-tabs-current-tabset)))
         (name (buffer-name buffer)))
    (if (not selected)
        (format " %s" name)
      (let ((project (my/centaur-tabs--project-name buffer)))
        (concat
         " "
         (propertize name 'face 'my/centaur-tabs-active-buffer-face)
         (if (and (stringp project)
                  (not (string-empty-p project))
                  (not (equal project name)))
             (concat
              (propertize "  |  " 'face 'my/centaur-tabs-divider-face)
              (propertize project 'face 'my/centaur-tabs-meta-face))
           ""))))))

(defun my/centaur-tabs-hide-p ()
  "Return non-nil when the buffer tab bar should stay hidden."
  (derived-mode-p 'dashboard-mode))

(defun my/centaur-tabs-refresh (&rest _)
  "Refresh the buffer tab bar after workspace changes."
  (when (bound-and-true-p centaur-tabs-mode)
    (centaur-tabs-buffer-update-groups)
    (centaur-tabs-display-update)))

(defun my/centaur-tabs-apply-ui ()
  "Apply the local visual treatment for buffer tabs."
  (when (display-graphic-p)
    (set-face-attribute 'centaur-tabs-default nil
                        :background "#1d212b"
                        :foreground "#1d212b")
    (set-face-attribute 'centaur-tabs-unselected nil
                        :background "#202533"
                        :foreground "#7d8598"
                        :box '(:line-width 5 :color "#202533")
                        :height 0.95)
    (set-face-attribute 'centaur-tabs-unselected-modified nil
                        :background "#202533"
                        :foreground "#95a0b8"
                        :box '(:line-width 5 :color "#202533")
                        :height 0.95)
    (set-face-attribute 'centaur-tabs-selected nil
                        :background "#2a3140"
                        :foreground "#e7d6a5"
                        :box '(:line-width 5 :color "#2a3140")
                        :height 0.98
                        :weight 'medium)
    (set-face-attribute 'centaur-tabs-selected-modified nil
                        :background "#2a3140"
                        :foreground "#e7d6a5"
                        :box '(:line-width 5 :color "#2a3140")
                        :height 0.98
                        :weight 'medium)
    (set-face-attribute 'centaur-tabs-modified-marker-selected nil
                        :foreground "#d7b76f"
                        :background "#2a3140")
    (set-face-attribute 'centaur-tabs-modified-marker-unselected nil
                        :foreground "#7d8598"
                        :background "#202533")
    (set-face-attribute 'centaur-tabs-active-bar-face nil
                        :background "#d7b76f")))

(defun my/centaur-tabs-ensure-visible (&optional frame)
  "Ensure the buffer tab bar is enabled on FRAME."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (unless (bound-and-true-p centaur-tabs-mode)
        (centaur-tabs-mode 1))
      (my/centaur-tabs-refresh))))

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
  (my/tab-bar-disable-tab-line)
  (when (display-graphic-p)
    (my/tab-bar-ensure-visible)))

(use-package centaur-tabs
  :ensure t
  :demand t
  :hook ((after-init . my/centaur-tabs-ensure-visible)
         (server-after-make-frame . my/centaur-tabs-ensure-visible)
         (after-load-theme . my/centaur-tabs-apply-ui))
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 30)
  (centaur-tabs-set-bar nil)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-icons nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•")
  (centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-left-edge-margin " ")
  (centaur-tabs-right-edge-margin " ")
  (centaur-tabs-buffer-list-function #'my/centaur-tabs-buffer-list)
  (centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)
  :config
  (centaur-tabs-mode 1)
  (setq centaur-tabs-tab-label-function #'my/centaur-tabs-tab-label
        centaur-tabs-hide-predicate #'my/centaur-tabs-hide-p)
  (with-eval-after-load 'perspective
    (add-hook 'persp-activated-functions #'my/centaur-tabs-refresh))
  (my/centaur-tabs-apply-ui)
  (my/centaur-tabs-refresh))

(provide 'init-tabbar)
;;; init-tabbar.el ends here

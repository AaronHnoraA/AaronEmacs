;;; init-tabbar.el --- Top tab and buffer bars -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'seq)
(require 'subr-x)

(defvar persp-mode)
(defvar tab-line-exclude-modes)
(defvar tab-line-tab-map)
(defvar my/vterm-popup-instance-p)

(declare-function centaur-tabs-mode "centaur-tabs" (&optional arg))
(declare-function global-tab-line-mode "tab-line" (&optional arg))
(declare-function get-current-persp "perspective" ())
(declare-function persp-parameter "perspective" (parameter &optional persp))
(declare-function persp-current-buffers* "perspective" (&optional include-global))
(declare-function set-persp-parameter "perspective" (parameter value &optional persp))
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

(defface my/tab-line-overflow-face
  '((t (:foreground "#6d7488")))
  "Face used for overflow markers in the centered tab line."
  :group 'tab-line)

(defcustom my/tab-line-max-label-width 18
  "Maximum display width of a single centered buffer tab label."
  :type 'integer
  :group 'tab-line)

(defcustom my/tab-line-min-label-width 8
  "Minimum display width of a single centered buffer tab label."
  :type 'integer
  :group 'tab-line)

(defcustom my/tab-line-wheel-throttle 0.5
  "Minimum seconds between accepted wheel tab switches on one window.
Scrolling in the opposite direction is allowed immediately."
  :type 'number
  :group 'tab-line)

(defconst my/tab-line-separator-string "|"
  "Separator used between centered buffer tabs.")

(defconst my/tab-line-overflow-string "..."
  "Marker used when the centered tab line hides tabs on one side.")

(defvar my/tab-line-global-order nil
  "Fallback buffer order for the centered tab line outside perspective mode.")

(defvar my/tab-line-wheel-state (make-hash-table :test #'eq :weakness 'key)
  "Accepted wheel timestamps keyed by window for centered tab switching.")

(defvar my/tab-line-click-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] #'ignore)
    (define-key map [tab-line mouse-1] #'my/tab-line-select-buffer)
    (define-key map [tab-line down-mouse-3] #'ignore)
    (define-key map [tab-line mouse-3] #'my/tab-line-popup-menu)
    (define-key map [tab-line wheel-up] #'my/tab-line-wheel-up)
    (define-key map [tab-line wheel-down] #'my/tab-line-wheel-down)
    (define-key map [tab-line mouse-4] #'my/tab-line-wheel-up)
    (define-key map [tab-line mouse-5] #'my/tab-line-wheel-down)
    (define-key map [wheel-up] #'my/tab-line-wheel-up)
    (define-key map [wheel-down] #'my/tab-line-wheel-down)
    (define-key map [mouse-4] #'my/tab-line-wheel-up)
    (define-key map [mouse-5] #'my/tab-line-wheel-down)
    (define-key map (kbd "RET") #'my/tab-line-select-current)
    map)
  "Keymap for clickable centered buffer tabs.")

(defun my/tab-line-current-persp ()
  "Return the active perspective object, if any."
  (when (and (bound-and-true-p persp-mode)
             (fboundp 'get-current-persp))
    (ignore-errors
      (get-current-persp))))

(defun my/tab-line-get-order ()
  "Return the persisted tab order for the current scope."
  (let ((persp (my/tab-line-current-persp)))
    (copy-sequence
     (or (and persp
              (fboundp 'persp-parameter)
              (persp-parameter 'tab-line-order persp))
         my/tab-line-global-order))))

(defun my/tab-line-set-order (order)
  "Persist ORDER for the current scope."
  (let ((order (delete-dups (seq-filter #'stringp order)))
        (persp (my/tab-line-current-persp)))
    (if (and persp (fboundp 'set-persp-parameter))
        (set-persp-parameter 'tab-line-order order persp)
      (setq my/tab-line-global-order order))
    order))

(defun my/tab-line-set-buffer-order (buffers)
  "Persist BUFFERS as the current centered tab order."
  (my/tab-line-set-order (mapcar #'buffer-name buffers)))

(defun my/tab-line-sort-buffers (buffers)
  "Return BUFFERS reordered by the persisted centered tab order."
  (let ((order (my/tab-line-get-order)))
    (if (null order)
        buffers
      (let ((seen (make-hash-table :test #'equal))
            sorted)
        (dolist (name order)
          (when-let* ((buffer (seq-find
                               (lambda (candidate)
                                 (string= name (buffer-name candidate)))
                               buffers)))
            (push buffer sorted)
            (puthash name t seen)))
        (dolist (buffer buffers)
          (unless (gethash (buffer-name buffer) seen)
            (push buffer sorted)))
        (nreverse sorted)))))

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
    (my/tab-line-sort-buffers
     (delete-dups
      (seq-filter #'my/tab-line-visible-buffer-p buffers)))))

(defun my/tab-line-starred-buffer-p (buffer)
  "Return non-nil when BUFFER name contains a `*...*' wrapper."
  (let ((name (buffer-name buffer)))
    (and (stringp name)
         (string-match-p "\\*[^*]+\\*" name))))

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

(defun my/tab-line-event-window (event)
  "Return the window associated with mouse EVENT."
  (let* ((posnp (and event (event-start event)))
         (window (and posnp (posn-window posnp))))
    (if (windowp window)
        window
      (selected-window))))

(defun my/tab-line-event-buffer (event)
  "Return the buffer associated with mouse EVENT."
  (let* ((posnp (and event (event-start event)))
         (string (and posnp (car-safe (posn-string posnp))))
         (buffer (and string
                      (tab-line--get-tab-property 'tab string))))
    (or buffer
        (window-buffer (my/tab-line-event-window event)))))

(defun my/tab-line-switch-to-buffer (buffer &optional window)
  "Display BUFFER in WINDOW and refresh the centered tab line."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let ((switch-to-buffer-obey-display-actions nil))
        (switch-to-buffer buffer))
      (my/tab-line-refresh))))

(defun my/tab-line-cycle (step &optional window)
  "Cycle centered tabs by STEP in WINDOW."
  (let ((window (or window (selected-window))))
    (with-selected-window window
      (let* ((buffers (my/tab-line-buffer-list))
             (count (length buffers)))
        (when (> count 1)
          (let* ((current (window-buffer window))
                 (index (or (cl-position current buffers :test #'eq) 0))
                 (target (nth (mod (+ index step) count) buffers)))
            (my/tab-line-switch-to-buffer target window)))))))

(defun my/tab-line-wheel-allow-p (window step &optional now)
  "Return non-nil when a wheel STEP on WINDOW should switch tabs.
NOW is a float timestamp in seconds and defaults to the current time."
  (let* ((window (or window (selected-window)))
         (now (or now (float-time)))
         (state (gethash window my/tab-line-wheel-state))
         (last-time (car-safe state))
         (last-step (cdr-safe state))
         (allowed (or (null last-time)
                      (/= step last-step)
                      (>= (- now last-time) my/tab-line-wheel-throttle))))
    (when allowed
      (puthash window (cons now step) my/tab-line-wheel-state))
    allowed))

(defun my/tab-line-handle-wheel (step event)
  "Handle wheel STEP for centered tabs from mouse EVENT."
  (let ((window (my/tab-line-event-window event)))
    (when (my/tab-line-wheel-allow-p window step)
      (my/tab-line-cycle step window))))

(defun my/tab-line-wheel-up (event)
  "Select the previous centered tab from mouse wheel EVENT."
  (interactive "e")
  (my/tab-line-handle-wheel -1 event))

(defun my/tab-line-wheel-down (event)
  "Select the next centered tab from mouse wheel EVENT."
  (interactive "e")
  (my/tab-line-handle-wheel 1 event))

(defun my/tab-line-move-buffer-to-position (buffer position &optional window)
  "Move BUFFER to POSITION in WINDOW's centered tab order."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let* ((buffers (my/tab-line-buffer-list))
             (without (delq buffer (copy-sequence buffers)))
             (index (max 0 (min (1- position) (length without))))
             (ordered (append (seq-take without index)
                              (list buffer)
                              (seq-drop without index))))
        (my/tab-line-set-buffer-order ordered)
        (my/tab-line-refresh)))))

(defun my/tab-line-move-buffer-left (buffer &optional window)
  "Move BUFFER one slot to the left in WINDOW's centered tab order."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let* ((buffers (my/tab-line-buffer-list))
             (index (cl-position buffer buffers :test #'eq)))
        (when index
          (my/tab-line-move-buffer-to-position buffer index window))))))

(defun my/tab-line-move-buffer-right (buffer &optional window)
  "Move BUFFER one slot to the right in WINDOW's centered tab order."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let* ((buffers (my/tab-line-buffer-list))
             (index (cl-position buffer buffers :test #'eq)))
        (when index
          (my/tab-line-move-buffer-to-position buffer (+ index 2) window))))))

(defun my/tab-line-create-buffer (&optional after-buffer window)
  "Create a new empty buffer after AFTER-BUFFER in WINDOW."
  (let* ((window (or window (selected-window)))
         (buffers (with-selected-window window
                    (my/tab-line-buffer-list)))
         (new-buffer (generate-new-buffer "untitled"))
         (insert-at (if after-buffer
                        (1+ (or (cl-position after-buffer buffers :test #'eq)
                                (length buffers)))
                      (length buffers)))
         (ordered (append (seq-take buffers insert-at)
                          (list new-buffer)
                          (seq-drop buffers insert-at))))
    (with-selected-window window
      (let ((switch-to-buffer-obey-display-actions nil))
        (switch-to-buffer new-buffer)))
    (my/tab-line-set-buffer-order ordered)
    (my/tab-line-refresh)
    new-buffer))

(defun my/tab-line-kill-buffer (buffer &optional window)
  "Kill BUFFER for real and refresh WINDOW's centered tabs."
  (when (buffer-live-p buffer)
    (let* ((window (or window (selected-window)))
           (buffers (with-selected-window window
                      (my/tab-line-buffer-list)))
           (remaining (delq buffer (copy-sequence buffers))))
      (with-selected-window window
        (kill-buffer buffer))
      (my/tab-line-set-buffer-order remaining)
      (my/tab-line-refresh))))

(defun my/tab-line-menu-items (buffer window)
  "Return popup menu entries for BUFFER in WINDOW."
  (let* ((target (or buffer (window-buffer window)))
         (buffers (with-selected-window window
                    (my/tab-line-buffer-list)))
         (count (length buffers))
         (index (cl-position target buffers :test #'eq)))
    (append
     (when (buffer-live-p target)
       (list
        (vector (format "切换到 %s" (buffer-name target))
                `(lambda () (interactive)
                   (my/tab-line-switch-to-buffer ,target ,window))
                t)
        (vector (format "关闭 %s" (buffer-name target))
                `(lambda () (interactive)
                   (my/tab-line-kill-buffer ,target ,window))
                t)))
     (list
      (vector "新增空 Buffer"
              `(lambda () (interactive)
                 (my/tab-line-create-buffer ,target ,window))
              t))
     (when (and (buffer-live-p target) index)
       (list
        (vector "左移"
                `(lambda () (interactive)
                   (my/tab-line-move-buffer-left ,target ,window))
                (> index 0))
        (vector "右移"
                `(lambda () (interactive)
                   (my/tab-line-move-buffer-right ,target ,window))
                (< index (1- count)))
        (cons "移动到位置"
              (mapcar
               (lambda (position)
                 (vector
                  (format "第 %d 个" position)
                  `(lambda () (interactive)
                     (my/tab-line-move-buffer-to-position
                      ,target ,position ,window))
                  (/= position (1+ index))))
               (number-sequence 1 count))))))))

(defun my/tab-line-popup-menu (event)
  "Show the right-click menu for the centered tab under EVENT."
  (interactive "e")
  (let* ((window (my/tab-line-event-window event))
         (buffer (my/tab-line-event-buffer event))
         (title (format "Tab: %s" (buffer-name buffer)))
         (menu (easy-menu-create-menu
                title
                (my/tab-line-menu-items buffer window))))
    (popup-menu menu event)))

(defun my/tab-line-select-buffer (event)
  "Select the clicked buffer tab from EVENT."
  (interactive "e")
  (my/tab-line-switch-to-buffer
   (my/tab-line-event-buffer event)
   (my/tab-line-event-window event)))

(defun my/tab-line-label-width (&optional max-width)
  "Return the display width budget for a single tab label.
When MAX-WIDTH is non-nil, adapt the label width to the current window."
  (let* ((window-width (or max-width (window-body-width)))
         (dynamic-width (max my/tab-line-min-label-width
                             (/ (max 1 (- window-width 4)) 3))))
    (min my/tab-line-max-label-width dynamic-width)))

(defun my/tab-line-display-name (buffer &optional max-width)
  "Return the display name used for BUFFER in the centered tab line.
When MAX-WIDTH is non-nil, use it to shrink labels in narrow windows."
  (let* ((name (buffer-name buffer))
         (label-width (my/tab-line-label-width max-width)))
    (if (<= (string-width name) label-width)
        name
      (concat
       (truncate-string-to-width
        name (max 1 (- label-width 3)) nil nil nil)
       "..."))))

(defun my/tab-line-tab-string (buffer &optional max-width)
  "Return the clickable tab label for BUFFER.
When MAX-WIDTH is non-nil, use it to shrink labels in narrow windows."
  (let ((current (eq buffer (my/tab-line-current-buffer)))
        (name (my/tab-line-display-name buffer max-width)))
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

(defun my/tab-line-passive-string (text face)
  "Return TEXT with FACE and centered tab line mouse bindings."
  (propertize text
              'face face
              'local-map my/tab-line-click-map
              'keymap my/tab-line-click-map
              'follow-link 'ignore))

(defun my/tab-line-separator ()
  "Return the muted separator used between buffer tabs."
  (my/tab-line-passive-string
   my/tab-line-separator-string
   'my/tab-line-separator-face))

(defun my/tab-line-overflow-marker ()
  "Return the overflow marker used when tabs are hidden."
  (my/tab-line-passive-string
   (format " %s " my/tab-line-overflow-string)
   'my/tab-line-overflow-face))

(defun my/tab-line-segments-width (segments)
  "Return the rendered width of SEGMENTS."
  (cond
   ((stringp segments)
    (string-width segments))
   ((listp segments)
    (seq-reduce #'+
                (mapcar #'my/tab-line-segments-width segments)
                0))
   (t 0)))

(defun my/tab-line-build-segments (buffers left right max-width)
  "Return rendered segments for BUFFERS between LEFT and RIGHT.
LEFT and RIGHT are inclusive indexes.  MAX-WIDTH is the available
window width used to keep labels compact."
  (let ((segments (list (my/tab-line-passive-string
                         "("
                         'my/tab-line-separator-face)))
        (last-index (1- (length buffers))))
    (when (> left 0)
      (setq segments
            (append segments
                    (list (my/tab-line-overflow-marker)
                          (my/tab-line-separator)))))
    (dotimes (offset (1+ (- right left)))
      (when (> offset 0)
        (setq segments
              (append segments
                      (list (my/tab-line-separator)))))
      (setq segments
            (append segments
                    (list (my/tab-line-tab-string
                           (nth (+ left offset) buffers)
                           max-width)))))
    (when (< right last-index)
      (setq segments
            (append segments
                    (list (my/tab-line-separator)
                          (my/tab-line-overflow-marker)))))
    (append segments
            (list (my/tab-line-passive-string
                   ")"
                   'my/tab-line-separator-face)))))

(defun my/tab-line-fit-buffers (buffers max-width)
  "Return the visible slice of BUFFERS that fits MAX-WIDTH.
Keep the current buffer visible and expand around it while space allows."
  (let* ((count (length buffers))
         (current (my/tab-line-current-buffer))
         (current-index (or (cl-position current buffers :test #'eq) 0))
         (left current-index)
         (right current-index)
         (segments (my/tab-line-build-segments buffers left right max-width))
         (step 0)
         (changed t))
    (while (and (< step count) changed)
      (setq step (1+ step)
            changed nil)
      (dolist (side '(left right))
        (let* ((next-left (if (eq side 'left) (1- left) left))
               (next-right (if (eq side 'right) (1+ right) right)))
          (when (and (>= next-left 0)
                     (< next-right count))
            (let ((candidate (my/tab-line-build-segments
                              buffers next-left next-right max-width)))
              (when (<= (my/tab-line-segments-width candidate) max-width)
                (setq left next-left
                      right next-right
                      segments candidate
                      changed t)))))))
    segments))

(defun my/tab-line-content ()
  "Return the centered buffer tabs content segments."
  (let ((buffers (my/tab-line-buffer-list)))
    (when buffers
      (my/tab-line-fit-buffers buffers
                               (max 12 (- (window-body-width) 2))))))

(defun my/tab-line-format ()
  "Render a centered, minimal buffer tab line."
  (unless (my/tab-line-hidden-p)
    (when-let* ((content (my/tab-line-content)))
      (let* ((content-width (my/tab-line-segments-width content))
             (window-width (window-body-width))
             (offset (if (< content-width (max 1 (- window-width 4)))
                         (/ content-width 2)
                       0)))
        (append
         (list
          (if (> offset 0)
              (propertize
               (my/tab-line-passive-string " " 'tab-line)
               'display `(space :align-to (- center ,offset)))
            (my/tab-line-passive-string " " 'tab-line)))
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

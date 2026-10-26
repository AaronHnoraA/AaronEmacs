;;; ui/tabbar/config.el --- Top tab and buffer bars -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui)
(require 'cl-lib)
(require 'easymenu)
(require 'seq)
(require 'subr-x)

(defvar persp-mode)
(defvar tab-line-exclude-modes)
(defvar tab-line-tab-map)
(defvar +portal-popup-instance-p)
(defvar +tab-line-terminal-order nil
  "Ordinary Portal terminals in user-selected order, by buffer identity.")

(declare-function global-tab-line-mode "tab-line" (&optional arg))
(declare-function persp-curr "perspective" (&optional frame))
(declare-function +file-icon-for-file "ui/theme/config" (file &rest args))
(declare-function persp-current-buffers* "perspective" (&optional include-global))
(declare-function tab-line--get-tab-property "tab-line" (prop string))
(declare-function tab-line-force-update "tab-line" (all))
(declare-function tab-bar-tabs "tab-bar" ())
(declare-function portal-terminal-buffers "portal-terminal" (&optional project quick))
(declare-function portal-terminal-new "portal-terminal" ())
(declare-function portal-terminal-rename "portal-terminal" (name))

(defface +tab-bar-buffer-face
  `((t (:inherit tab-bar-tab
        :foreground ,(aaron-ui-color 'fg-soft)
        :weight semibold)))
  "Face used for the current buffer label in the top tab bar."
  :group 'tab-bar)

(defface +tab-bar-buffer-path-face
  `((t (:inherit tab-bar
        :foreground ,(aaron-ui-color 'fg-tab))))
  "Face used for secondary path text in the top tab bar."
  :group 'tab-bar)

(defface +tab-line-current-face
  `((t (:foreground ,(aaron-ui-color 'accent-sand)
        :weight semibold)))
  "Face used for the selected buffer in the centered tab line."
  :group 'tab-line)

(defface +tab-line-inactive-face
  `((t (:foreground ,(aaron-ui-color 'fg-tab))))
  "Face used for inactive buffers in the centered tab line."
  :group 'tab-line)

(defface +tab-line-separator-face
  `((t (:foreground ,(aaron-ui-color 'fg-tab-separator))))
  "Face used for separators in the centered tab line."
  :group 'tab-line)

(defface +tab-line-overflow-face
  `((t (:foreground ,(aaron-ui-color 'fg-tab-overflow))))
  "Face used for overflow markers in the centered tab line."
  :group 'tab-line)

(config-defvar +tab-line-max-label-width nil
  "Maximum display width of a single centered buffer tab label."
  :type 'integer
  :group 'tab-line)

(config-defvar +tab-line-min-label-width nil
  "Minimum display width of a single centered buffer tab label."
  :type 'integer
  :group 'tab-line)

(config-defvar +tab-line-wheel-throttle nil
  "Minimum seconds between accepted wheel tab switches on one window.
Scrolling in the opposite direction is allowed immediately."
  :type 'number
  :group 'tab-line)

(defconst +tab-line-separator-string "|"
  "Separator used between centered buffer tabs.")

(defconst +tab-line-overflow-string "..."
  "Marker used when the centered tab line hides tabs on one side.")

(defvar +tab-line-cache-generation 0
  "Revision counter used to invalidate cached centered tab-line strings.")

(defvar +tab-line-cache-invalidation-timer nil
  "Timer used to coalesce centered tab-line cache invalidations.")

(defvar-local +tab-line--format-cache nil
  "Cached centered tab-line value for the current buffer.")

(defvar-local +tab-line--format-cache-generation nil
  "Cache generation used for `+tab-line--format-cache'.")

(defvar-local +tab-line--format-cache-window-width nil
  "Window body width used for `+tab-line--format-cache'.")

(defvar-local +tab-line--format-cache-frame nil
  "Frame for which `+tab-line--format-cache' was rendered.")

(defvar-local +tab-line--format-cache-window nil
  "Window for which `+tab-line--format-cache' was rendered.")

(defvar-local +tab-line--format-cache-scope nil
  "Perspective for which `+tab-line--format-cache' was rendered.")

(defvar-local +tab-line--format-cache-buffer-name nil
  "Buffer name used for `+tab-line--format-cache'.")

(defvar-local +tab-line--format-cache-file-name nil
  "File name used for `+tab-line--format-cache'.")

(defvar-local +tab-line--format-cache-major-mode nil
  "Major mode used for `+tab-line--format-cache'.")

(defvar-local +tab-line--format-cache-portal-popup nil
  "Portal popup state used for `+tab-line--format-cache'.")

(defvar-local +tab-line-leading-segment-functions nil
  "Functions returning contextual strings for the tab line's left edge.
Centered buffer tabs use the full line and paint over this contextual text.")

(defvar +tab-line-wheel-state (make-hash-table :test #'eq :weakness 'key)
  "Accepted wheel timestamps keyed by window for centered tab switching.")

(defvar +tab-line-click-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] #'ignore)
    (define-key map [tab-line mouse-1] #'+tab-line/select-buffer)
    (define-key map [header-line mouse-1] #'+tab-line/select-buffer)
    (define-key map [tab-line down-mouse-2] #'ignore)
    (define-key map [tab-line mouse-2] #'+tab-line/close-buffer)
    (define-key map [header-line mouse-2] #'+tab-line/close-buffer)
    (define-key map [tab-line down-mouse-3] #'ignore)
    (define-key map [tab-line mouse-3] #'+tab-line/popup-menu)
    (define-key map [header-line mouse-3] #'+tab-line/popup-menu)
    (define-key map [tab-line wheel-up] #'+tab-line/wheel-up)
    (define-key map [tab-line wheel-down] #'+tab-line/wheel-down)
    (define-key map [tab-line mouse-4] #'+tab-line/wheel-up)
    (define-key map [tab-line mouse-5] #'+tab-line/wheel-down)
    (define-key map [wheel-up] #'+tab-line/wheel-up)
    (define-key map [wheel-down] #'+tab-line/wheel-down)
    (define-key map [mouse-4] #'+tab-line/wheel-up)
    (define-key map [mouse-5] #'+tab-line/wheel-down)
    (define-key map (kbd "RET") #'+tab-line/select-current)
    map)
  "Keymap for clickable centered buffer tabs.")

;; `defvar' retains the map in an already-running Emacs when this file reloads.
(dolist (entry '(([header-line down-mouse-1] . ignore)
                 ([header-line mouse-1] . +tab-line/select-buffer)
                 ([header-line down-mouse-2] . ignore)
                 ([header-line mouse-2] . +tab-line/close-buffer)
                 ([header-line down-mouse-3] . ignore)
                 ([header-line mouse-3] . +tab-line/popup-menu)))
  (define-key +tab-line-click-map (car entry) (cdr entry)))

(defun +tab-line-current-persp ()
  "Return the active perspective object, if any."
  (when (and (bound-and-true-p persp-mode)
             (fboundp 'persp-curr))
    (ignore-errors
      (persp-curr))))

(defvar +frame-next-short-id 0
  "Next short frame ID used by buffer and output views.")

(defun +frame-short-id (frame)
  "Return FRAME's stable short ID for its lifetime."
  (or (frame-parameter frame '+frame-short-id)
      (let ((id (cl-incf +frame-next-short-id)))
        (set-frame-parameter frame '+frame-short-id id)
        id)))

(defun +frame-buffer-list (&optional frame include-buried)
  "Return buffers visited or displayed in FRAME.
`buffer-list' with a frame argument still includes every other buffer, so
use the frame's native buffer history and its currently displayed windows.
When INCLUDE-BURIED is non-nil, include buffers explicitly buried here."
  (let ((frame (or frame (selected-frame))))
    (when (frame-live-p frame)
      (delete-dups
       (seq-filter #'buffer-live-p
                   (append (mapcar #'window-buffer
                                   (window-list frame 'no-minibuffer))
                           (frame-parameter frame 'buffer-list)
                           (when include-buried
                             (reverse
                              (frame-parameter frame 'buried-buffer-list)))))))))

(defun +tab-line-order-scope ()
  "Return the current Perspective, or the frame's unscoped tab key."
  (or (+tab-line-current-persp) :global))

(defun +tab-line-get-order ()
  "Return the centered tab order for this frame and Perspective."
  (copy-sequence
   (cdr (assq (+tab-line-order-scope)
              (frame-parameter nil '+tab-line-orders)))))

(defun +tab-line-set-order (order)
  "Persist ORDER in the current frame and Perspective."
  (let* ((order (delete-dups (seq-filter #'stringp order)))
         (scope (+tab-line-order-scope))
         (orders (assq-delete-all
                  scope (copy-sequence
                         (frame-parameter nil '+tab-line-orders)))))
    (set-frame-parameter nil '+tab-line-orders
                         (cons (cons scope order) orders))
    order))

(defun +tab-line-forget-persp-order ()
  "Drop the dying Perspective's tab order from its frame."
  (when-let* ((persp (+tab-line-current-persp)))
    (set-frame-parameter
     nil '+tab-line-orders
     (assq-delete-all persp
                      (frame-parameter nil '+tab-line-orders)))
    (+tab-line-invalidate-cache)))

(defun +tab-line-set-buffer-order (buffers)
  "Persist BUFFERS as the current centered tab order."
  (+tab-line-set-order (mapcar #'buffer-name buffers)))

(defun +tab-line-invalidate-cache (&rest _)
  "Invalidate cached centered tab-line strings."
  (when (timerp +tab-line-cache-invalidation-timer)
    (cancel-timer +tab-line-cache-invalidation-timer))
  (setq +tab-line-cache-invalidation-timer nil)
  (setq +tab-line-cache-generation (1+ +tab-line-cache-generation)))

(defun +tab-line-schedule-invalidate-cache (&rest _)
  "Coalesce frequent buffer-list changes before invalidating tab-line caches."
  (unless (timerp +tab-line-cache-invalidation-timer)
    (setq +tab-line-cache-invalidation-timer
          (run-with-idle-timer
           0.05 nil
           (lambda ()
             (setq +tab-line-cache-invalidation-timer nil)
             (+tab-line-invalidate-cache))))))

(defun +tab-line-sort-buffers (buffers)
  "Return BUFFERS reordered by the persisted centered tab order."
  (let ((order (+tab-line-get-order)))
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

(defun +tab-bar--current-buffer ()
  "Return the buffer shown in the selected window."
  (window-buffer (minibuffer-selected-window)))


(defun +tab-bar--buffer-file-name ()
  "Return the current buffer's real file or Noema virtual file."
  (or buffer-file-name
      (and (boundp '+noema-buffer-file-name)
           (stringp +noema-buffer-file-name)
           (not (string-empty-p +noema-buffer-file-name))
           +noema-buffer-file-name)))

(defun +tab-bar--buffer-title ()
  "Return the current buffer title for the custom top tab bar."
  (with-current-buffer (+tab-bar--current-buffer)
    (cond
     ((and (+tab-bar--buffer-file-name) default-directory)
      (let* ((file (+tab-bar--buffer-file-name))
             (dir (file-name-nondirectory
                   (directory-file-name
                    (expand-file-name default-directory)))))
        (concat
         (when-let* ((icon (+file-icon-for-file
                            file
                            :height 0.9 :v-adjust -0.05 :image-height 15)))
           (concat icon " "))
         (propertize (format "%s/" dir) 'face '+tab-bar-buffer-path-face)
         (propertize (file-name-nondirectory file)
                     'face '+tab-bar-buffer-face))))
     (t
      (propertize (buffer-name) 'face '+tab-bar-buffer-face)))))

(defun +tab-bar-format-buffer ()
  "Render the left-side current buffer label."
  ;; A tab-bar format function may return a raw string, but Emacs 31's
  ;; `tab-bar-auto-width' accepts menu-item entries only.  Returning the
  ;; documented menu-item form keeps the custom label compatible with
  ;; automatic tab sizing and prevents redisplay errors from recursively
  ;; reporting themselves.
  (list
   (list
    'buffer-label 'menu-item
    (concat
     (propertize " " 'face 'tab-bar)
     (+tab-bar--buffer-title)
     (propertize " " 'face 'tab-bar))
    #'ignore)))

(defun +tab-bar-tab-name-format (tab _index)
  "Format TAB as a restrained workspace pill."
  (let* ((name (alist-get 'name tab))
         (current (eq (car tab) 'current-tab))
         (face (if current 'tab-bar-tab 'tab-bar-tab-inactive)))
    (concat
     (propertize " " 'face face)
     (propertize name 'face face)
     (propertize " " 'face face))))

(defun +tab-bar-ensure-hidden (&optional frame)
  "Keep tab-bar commands available without adding a top row to FRAME.
When FRAME is nil, use the selected frame."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (with-selected-frame frame
        (setopt tab-bar-show nil)
        (tab-bar-mode 1)
        (set-frame-parameter frame 'tab-bar-lines 0)
        (force-mode-line-update t)
        (redraw-frame frame)))))

(defun +tab-bar-update-terminal-visibility (&rest _)
  "Keep the unused workspace row hidden on graphical frames."
  (dolist (frame (frame-list))
    (when (display-graphic-p frame)
      (unless (eql (frame-parameter frame 'tab-bar-lines) 0)
        (set-frame-parameter frame 'tab-bar-lines 0)
        (redraw-frame frame)))))

(defun +frame-workspace-buffers (&optional frame-buffers)
  "Return FRAME-BUFFERS belonging to the current Perspective.
When FRAME-BUFFERS is nil, use this frame's unburied buffer history.  A
currently displayed buffer remains available even before Perspective has
registered it."
  (let* ((frame-buffers (or frame-buffers (+frame-buffer-list)))
         (workspace-scoped (and (bound-and-true-p persp-mode)
                                (fboundp 'persp-current-buffers*)))
         (workspace-buffers (and workspace-scoped
                                 (persp-current-buffers* t))))
    (if workspace-scoped
        (seq-filter (lambda (buffer)
                      (or (memq buffer workspace-buffers)
                          (get-buffer-window buffer (selected-frame))))
                    frame-buffers)
      frame-buffers)))

(defun +tab-line-all-normal-buffers ()
  "Return all normal tabs in this frame and workspace, including occupied ones."
  (+tab-line-sort-buffers
   (delete-dups
    (seq-filter (lambda (buffer)
                  (and (+tab-line-visible-buffer-p buffer)
                       (not (+tab-line-ordinary-terminal-p buffer))))
                (+frame-workspace-buffers)))))

(defun +tab-line-noema-buffer-p (buffer)
  "Return non-nil when BUFFER is a Noema page shown in normal tabs."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (or (and (boundp '+noema-buffer-file-name)
                  (stringp +noema-buffer-file-name)
                  (not (string-empty-p +noema-buffer-file-name)))
             (and (boundp '+noema--xwidget-forced-name)
                  +noema--xwidget-forced-name)
             (string-prefix-p "*Noema" (buffer-name buffer))))))

(defun +tab-line-occupied-elsewhere-p (buffer &optional window)
  "Return non-nil if BUFFER is displayed outside WINDOW's pane.
For ordinary terminals, check every frame because one native view cannot
appear in two panes at once."
  (let ((window (or window (selected-window)))
        occupied)
    (walk-windows
     (lambda (other)
       (when (and (not (eq other window))
                  (eq (window-buffer other) buffer))
         (setq occupied t)))
     'no-minibuf (if (+tab-line-ordinary-terminal-p buffer)
                    t
                  (window-frame window)))
    occupied))

(defun +tab-line-available-buffers (buffers &optional window)
  "Hide occupied Portal or Noema BUFFERS from WINDOW's tabs."
  (let* ((window (or window (selected-window)))
         (current (window-buffer window)))
    (seq-remove (lambda (buffer)
                  (and (not (eq buffer current))
                       (or (+tab-line-ordinary-terminal-p buffer)
                           (+tab-line-noema-buffer-p buffer))
                       (+tab-line-occupied-elsewhere-p buffer window)))
                buffers)))

(defun +tab-line-buffer-list ()
  "Return this window's available tabs from the shared buffer order."
  (if (+tab-line-ordinary-terminal-p (current-buffer))
      (+tab-line-terminal-buffers)
    (+tab-line-available-buffers (+tab-line-all-normal-buffers))))

(defun +tab-line-ordinary-terminal-p (buffer)
  "Return non-nil if BUFFER is an ordinary Portal terminal."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'portal-terminal-mode)
              (not (bound-and-true-p +portal-popup-instance-p))))))

(defun +tab-line-all-terminal-buffers ()
  "Return all ordinary Portal terminals in user-selected order."
  (let* ((live (and (fboundp 'portal-terminal-buffers)
                    (seq-filter #'+tab-line-ordinary-terminal-p
                                (portal-terminal-buffers))))
         (ordered (seq-filter (lambda (buffer) (memq buffer live))
                              +tab-line-terminal-order)))
    (setq +tab-line-terminal-order
          (append ordered
                  (seq-filter (lambda (buffer) (not (memq buffer ordered)))
                              live)))))

(defun +tab-line-terminal-buffers (&optional window)
  "Return the current terminal and terminals hidden in every frame."
  (let ((current (window-buffer (or window (selected-window)))))
    (seq-filter (lambda (buffer)
                  (or (eq buffer current)
                      (not (get-buffer-window buffer t))))
                (+tab-line-all-terminal-buffers))))

(defun +tab-line-starred-buffer-p (buffer)
  "Return non-nil when BUFFER name contains a `*...*' wrapper."
  (let ((name (buffer-name buffer)))
    (and (stringp name)
         (string-match-p "\\*[^*]+\\*" name))))

(defun +tab-line-visible-buffer-p (buffer)
  "Return non-nil when BUFFER should appear in the centered tab line."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((name (buffer-name buffer))
               (note (+tab-bar--buffer-file-name)))
           (and (not (derived-mode-p 'dashboard-mode))
                ;; Honor the standard tab-line opt-outs so packages that
                ;; mark helper buffers (agent sessions, popups) stay hidden.
                (not (bound-and-true-p tab-line-exclude))
                (not (and (bound-and-true-p tab-line-exclude-modes)
                          (apply #'derived-mode-p tab-line-exclude-modes)))
                (not (bound-and-true-p +portal-popup-instance-p))
                (not (and (stringp name)
                          (string-match-p "portal-pop" name)))
                (or note
                    (derived-mode-p 'portal-terminal-mode)
                    (not (+tab-line-starred-buffer-p buffer)))
                (or note
                    (derived-mode-p 'dired-mode
                                    'eshell-mode
                                    'shell-mode
                                    'term-mode
                                    'portal-terminal-mode)
                    (and (stringp name)
                         (not (string-prefix-p " " name)))))))))

(defun +tab-line-hidden-p ()
  "Return non-nil when the centered buffer tab line should stay hidden."
  (or (derived-mode-p 'dashboard-mode)
      (minibufferp)
      (not (+tab-line-visible-buffer-p (current-buffer)))))

(defun +tab-line-current-buffer ()
  "Return the currently selected visible buffer for the centered tab line."
  (current-buffer))

(defun +tab-line/select-current ()
  "No-op keyboard entrypoint for the current centered tab item."
  (interactive))

(defun +tab-line-event-window (event)
  "Return the window associated with mouse EVENT."
  (let* ((posnp (and event (event-start event)))
         (window (and posnp (posn-window posnp))))
    (if (windowp window)
        window
      (selected-window))))

(defun +tab-line-event-buffer (event)
  "Return the buffer associated with mouse EVENT."
  (let* ((posnp (and event (event-start event)))
         (string (and posnp (car-safe (posn-string posnp))))
         (buffer (and string
                      (tab-line--get-tab-property 'tab string))))
    (or buffer
        (window-buffer (+tab-line-event-window event)))))

(defun +tab-line-switch-to-buffer (buffer &optional window)
  "Display BUFFER only in WINDOW and refresh the centered tab line."
  (when (buffer-live-p buffer)
    (let ((window (or window (selected-window))))
      (unless (window-live-p window)
        (user-error "Tab window is no longer live"))
      (when (and (or (+tab-line-ordinary-terminal-p buffer)
                     (+tab-line-noema-buffer-p buffer))
                 (+tab-line-occupied-elsewhere-p buffer window))
        (user-error "Buffer is already open in another window"))
      (if (or (+tab-line-ordinary-terminal-p buffer)
              (+tab-line-noema-buffer-p buffer))
          (progn
            (set-window-buffer window buffer)
            (select-window window))
        (with-selected-window window
          (let ((switch-to-buffer-obey-display-actions nil))
            (switch-to-buffer buffer))))
      (+tab-line-refresh))))

(defun +tab-line-cycle (step &optional window)
  "Cycle centered tabs by STEP in WINDOW."
  (let ((window (or window (selected-window))))
    (with-selected-window window
      (let* ((buffers (+tab-line-buffer-list))
             (count (length buffers)))
        (when (> count 1)
          (let* ((current (window-buffer window))
                 (index (or (cl-position current buffers :test #'eq) 0))
                 (target (nth (mod (+ index step) count) buffers)))
            (+tab-line-switch-to-buffer target window)))))))

(defun +tab-line/next-buffer ()
  "Switch to the next buffer shown in the centered tab line."
  (interactive)
  (+tab-line-cycle 1))

(defun +tab-line/previous-buffer ()
  "Switch to the previous buffer shown in the centered tab line."
  (interactive)
  (+tab-line-cycle -1))

(defun +tab-line-wheel-allow-p (window step &optional now)
  "Return non-nil when a wheel STEP on WINDOW should switch tabs.
NOW is a float timestamp in seconds and defaults to the current time."
  (let* ((window (or window (selected-window)))
         (now (or now (float-time)))
         (state (gethash window +tab-line-wheel-state))
         (last-time (car-safe state))
         (last-step (cdr-safe state))
         (allowed (or (null last-time)
                      (/= step last-step)
                      (>= (- now last-time) +tab-line-wheel-throttle))))
    (when allowed
      (puthash window (cons now step) +tab-line-wheel-state))
    allowed))

(defun +tab-line-handle-wheel (step event)
  "Handle wheel STEP for centered tabs from mouse EVENT."
  (let ((window (+tab-line-event-window event)))
    (when (+tab-line-wheel-allow-p window step)
      (+tab-line-cycle step window))))

(defun +tab-line/wheel-up (event)
  "Select the previous centered tab from mouse wheel EVENT."
  (interactive "e")
  (+tab-line-handle-wheel -1 event))

(defun +tab-line/wheel-down (event)
  "Select the next centered tab from mouse wheel EVENT."
  (interactive "e")
  (+tab-line-handle-wheel 1 event))

(defun +tab-line-move-buffer-to-position (buffer position &optional window)
  "Move BUFFER to POSITION in WINDOW's centered tab order."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let* ((buffers (+tab-line-buffer-list))
             (without (delq buffer (copy-sequence buffers)))
             (index (max 0 (min (1- position) (length without))))
             (ordered (append (seq-take without index)
                              (list buffer)
                              (seq-drop without index))))
        (if (+tab-line-ordinary-terminal-p buffer)
            (setq +tab-line-terminal-order
                  (+tab-line-merge-visible-order
                   (+tab-line-all-terminal-buffers) ordered))
          (+tab-line-set-buffer-order
           (+tab-line-merge-visible-order
            (+tab-line-all-normal-buffers) ordered)))
        (+tab-line-refresh)))))

(defun +tab-line-merge-visible-order (all visible)
  "Apply reordered VISIBLE tabs to ALL without moving occupied hidden tabs."
  (let ((remaining (copy-sequence visible)))
    (mapcar (lambda (buffer)
              (if (memq buffer visible)
                  (pop remaining)
                buffer))
            all)))

(defun +tab-line-move-buffer-left (buffer &optional window)
  "Move BUFFER one slot to the left in WINDOW's centered tab order."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let* ((buffers (+tab-line-buffer-list))
             (index (cl-position buffer buffers :test #'eq)))
        (when index
          (+tab-line-move-buffer-to-position buffer index window))))))

(defun +tab-line-move-buffer-right (buffer &optional window)
  "Move BUFFER one slot to the right in WINDOW's centered tab order."
  (when (buffer-live-p buffer)
    (with-selected-window (or window (selected-window))
      (let* ((buffers (+tab-line-buffer-list))
             (index (cl-position buffer buffers :test #'eq)))
        (when index
          (+tab-line-move-buffer-to-position buffer (+ index 2) window))))))

(defun +tab-line-create-buffer (&optional after-buffer window)
  "Create a new empty buffer after AFTER-BUFFER in WINDOW."
  (let* ((window (or window (selected-window)))
         (buffers (with-selected-window window
                    (+tab-line-buffer-list)))
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
        (switch-to-buffer new-buffer))
      (+tab-line-set-buffer-order
       (+tab-line-merge-visible-order
        (+tab-line-all-normal-buffers) ordered)))
    (+tab-line-refresh)
    new-buffer))

(defun +tab-line-kill-buffer (buffer &optional window)
  "Kill BUFFER for real and refresh WINDOW's centered tabs."
  (when (buffer-live-p buffer)
    (let* ((window (or window (selected-window)))
           (terminal (+tab-line-ordinary-terminal-p buffer)))
      (with-selected-window window
        (kill-buffer buffer)
        (if terminal
            (setq +tab-line-terminal-order
                  (delq buffer +tab-line-terminal-order))
          (+tab-line-set-buffer-order
           (+tab-line-all-normal-buffers))))
      (+tab-line-refresh))))

(defun +tab-line-rename-terminal (buffer)
  "Prompt for a new name for Portal terminal BUFFER."
  (unless (+tab-line-ordinary-terminal-p buffer)
    (user-error "Not an ordinary Portal terminal"))
  (let ((name (read-string "Terminal name: " (buffer-name buffer))))
    (with-current-buffer buffer
      (portal-terminal-rename name))
    (+tab-line-refresh)))

(defun +tab-line-menu-items (buffer window)
  "Return popup menu entries for BUFFER in WINDOW."
  (let* ((target (or buffer (window-buffer window)))
         (buffers (with-selected-window window
                    (+tab-line-buffer-list)))
         (count (length buffers))
         (index (cl-position target buffers :test #'eq))
         (terminal (+tab-line-ordinary-terminal-p target)))
    (append
     (when (buffer-live-p target)
       (append
        (list
         (vector (format "切换到 %s" (buffer-name target))
                 `(lambda () (interactive)
                    (+tab-line-switch-to-buffer ,target ,window))
                 t)
         (vector (format "关闭 %s" (buffer-name target))
                 `(lambda () (interactive)
                    (+tab-line-kill-buffer ,target ,window))
                 t))
        (when terminal
          (list (vector "重命名终端"
                        `(lambda () (interactive)
                           (+tab-line-rename-terminal ,target))
                        t)))))
     (list
      (if terminal
          (vector "新建终端"
                  `(lambda () (interactive)
                     (with-selected-window ,window (portal-terminal-new)))
                  t)
        (vector "新增空 Buffer"
                `(lambda () (interactive)
                   (+tab-line-create-buffer ,target ,window))
                t)))
     (when (and (buffer-live-p target) index)
       (list
        (vector "左移"
                `(lambda () (interactive)
                   (+tab-line-move-buffer-left ,target ,window))
                (> index 0))
        (vector "右移"
                `(lambda () (interactive)
                   (+tab-line-move-buffer-right ,target ,window))
                (< index (1- count)))
        (cons "移动到位置"
              (mapcar
               (lambda (position)
                 (vector
                  (format "第 %d 个" position)
                  `(lambda () (interactive)
                     (+tab-line-move-buffer-to-position
                      ,target ,position ,window))
                  (/= position (1+ index))))
               (number-sequence 1 count))))))))

(defun +tab-line/popup-menu (event)
  "Show the right-click menu for the centered tab under EVENT."
  (interactive "e")
  (let* ((window (+tab-line-event-window event))
         (buffer (+tab-line-event-buffer event))
         (title (format "Tab: %s" (buffer-name buffer)))
         (menu (easy-menu-create-menu
                title
                (+tab-line-menu-items buffer window))))
    (popup-menu menu event)))

(defun +tab-line/close-buffer (event)
  "Close the clicked centered tab from mouse EVENT."
  (interactive "e")
  (+tab-line-kill-buffer
   (+tab-line-event-buffer event)
   (+tab-line-event-window event)))

(defun +tab-line/select-buffer (event)
  "Select the clicked buffer tab from EVENT."
  (interactive "e")
  (+tab-line-switch-to-buffer
   (+tab-line-event-buffer event)
   (+tab-line-event-window event)))

(defun +tab-line-label-width (&optional max-width)
  "Return the display width budget for a single tab label.
When MAX-WIDTH is non-nil, adapt the label width to the current window."
  (let* ((window-width (or max-width (window-body-width)))
         (dynamic-width (max +tab-line-min-label-width
                             (/ (max 1 (- window-width 4)) 3))))
    (min +tab-line-max-label-width dynamic-width)))

(defun +tab-line-display-name (buffer &optional max-width)
  "Return the display name used for BUFFER in the centered tab line.
When MAX-WIDTH is non-nil, use it to shrink labels in narrow windows."
  (let* ((name (buffer-name buffer))
         (label-width (+tab-line-label-width max-width)))
    (if (<= (string-width name) label-width)
        name
      (concat
       (truncate-string-to-width
        name (max 1 (- label-width 3)) nil nil nil)
       "..."))))

(defun +tab-line-tab-string (buffer &optional max-width)
  "Return the clickable tab label for BUFFER.
When MAX-WIDTH is non-nil, use it to shrink labels in narrow windows."
  (let* ((current (eq buffer (+tab-line-current-buffer)))
         (name (+tab-line-display-name buffer max-width))
         (face (if current
                   '+tab-line-current-face
                 '+tab-line-inactive-face))
         (file (with-current-buffer buffer
                 (+tab-bar--buffer-file-name)))
         (icon (+file-icon-for-file
                file
                :height 0.9 :v-adjust -0.05 :image-height 15))
         (label (concat " "
                        (when icon (concat icon " "))
                        (propertize name 'face face)
                        " ")))
    (propertize
     label
     'tab buffer
     'local-map +tab-line-click-map
     'keymap +tab-line-click-map
     'pointer 'hand
     'mouse-face 'tab-line-highlight
     'follow-link 'ignore
     'help-echo (format "%s\nmouse-1: switch\nmouse-2: close\nmouse-3: menu"
                        (or file name)))))

(defun +tab-line-passive-string (text face)
  "Return TEXT with FACE and centered tab line mouse bindings."
  (propertize text
              'face face
              'local-map +tab-line-click-map
              'keymap +tab-line-click-map
              'follow-link 'ignore))

(defun +tab-line-separator ()
  "Return the muted separator used between buffer tabs."
  (+tab-line-passive-string
   +tab-line-separator-string
   '+tab-line-separator-face))

(defun +tab-line-overflow-marker ()
  "Return the overflow marker used when tabs are hidden."
  (+tab-line-passive-string
   (format " %s " +tab-line-overflow-string)
   '+tab-line-overflow-face))

(defun +tab-line-segments-width (segments)
  "Return the rendered width of SEGMENTS."
  (cond
   ((stringp segments)
    (string-width segments))
   ((listp segments)
    (seq-reduce #'+
                (mapcar #'+tab-line-segments-width segments)
                0))
   (t 0)))

(defun +tab-line-build-segments (buffers left right max-width)
  "Return rendered segments for BUFFERS between LEFT and RIGHT.
LEFT and RIGHT are inclusive indexes.  MAX-WIDTH is the available
window width used to keep labels compact."
  (let ((segments nil)
        (last-index (1- (length buffers))))
    (push (+tab-line-passive-string
           "("
           '+tab-line-separator-face)
          segments)
    (when (> left 0)
      (push (+tab-line-overflow-marker) segments)
      (push (+tab-line-separator) segments))
    (dotimes (offset (1+ (- right left)))
      (when (> offset 0)
        (push (+tab-line-separator) segments))
      (push (+tab-line-tab-string
             (nth (+ left offset) buffers)
             max-width)
            segments))
    (when (< right last-index)
      (push (+tab-line-separator) segments)
      (push (+tab-line-overflow-marker) segments))
    (push (+tab-line-passive-string
           ")"
           '+tab-line-separator-face)
          segments)
    (nreverse segments)))

(defun +tab-line-fit-buffers (buffers max-width)
  "Return the visible slice of BUFFERS that fits MAX-WIDTH.
Keep the current buffer visible and expand around it while space allows."
  (let* ((count (length buffers))
         (current (+tab-line-current-buffer))
         (current-index (or (cl-position current buffers :test #'eq) 0))
         (left current-index)
         (right current-index)
         (segments (+tab-line-build-segments buffers left right max-width))
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
            (let ((candidate (+tab-line-build-segments
                              buffers next-left next-right max-width)))
              (when (<= (+tab-line-segments-width candidate) max-width)
                (setq left next-left
                      right next-right
                      segments candidate
                      changed t)))))))
    segments))

(defun +tab-line-content ()
  "Return the centered buffer tabs content segments."
  (let ((buffers (+tab-line-buffer-list)))
    (when buffers
      (+tab-line-fit-buffers buffers
                               (max 12 (- (window-body-width) 2))))))

(defun +tab-line--flatten-strings (value)
  "Flatten string or nested list VALUE without dropping text properties."
  (cond
   ((stringp value) value)
   ((listp value)
    (apply #'concat (mapcar #'+tab-line--flatten-strings value)))
   (t "")))

(defconst +tab-line-view-only-properties
  '(local-map keymap mouse-face help-echo follow-link pointer)
  "Interaction properties removed from contextual tab-line text.")

(defvar +tab-line-context-action-maps (make-hash-table :test #'eq)
  "Tab-line mouse maps keyed by explicit contextual action commands.")

(defun +tab-line-context-action-map (command)
  "Return a tab-line mouse map which invokes contextual COMMAND."
  (or (gethash command +tab-line-context-action-maps)
      (let ((map (make-sparse-keymap)))
        (define-key map [tab-line down-mouse-1] #'ignore)
        (define-key map [tab-line mouse-1] command)
        (puthash command map +tab-line-context-action-maps)
        map)))

(defun +tab-line-restore-context-actions (text)
  "Restore only repository-owned contextual actions in TEXT.
Provider keymaps are stripped before this runs, so these regions cannot
inherit buffer-tab switching, closing, wheel or popup behavior."
  (let ((position 0)
        (limit (length text)))
    (while (< position limit)
      (let* ((next (next-single-property-change
                    position '+tab-line-context-action text limit))
             (command
              (get-text-property position '+tab-line-context-action text))
             (help
              (get-text-property position '+tab-line-context-help text)))
        (when command
          (let ((map (+tab-line-context-action-map command)))
            (add-text-properties
             position next
             `(local-map ,map
                         keymap ,map
                         mouse-face tab-line-highlight
                         help-echo ,(or help "mouse-1: open context")
                         pointer hand)
             text)))
        (setq position next)))
    text))

(defun +tab-line-leading-content ()
  "Return sanitized leading provider output with explicit actions restored."
  (when +tab-line-leading-segment-functions
    (let ((text
           (+tab-line--flatten-strings
            (delq nil
                  (mapcar
                   (lambda (function)
                     (when (functionp function)
                       (ignore-errors (funcall function))))
                   +tab-line-leading-segment-functions)))))
      (unless (string-empty-p text)
        (let ((copy (copy-sequence text)))
          (remove-list-of-text-properties
           0 (length copy) +tab-line-view-only-properties copy)
          (+tab-line-restore-context-actions copy))))))

(defun +tab-line-format-cache-valid-p (window-width buffer-name)
  "Return non-nil when the centered tab-line cache is valid.
WINDOW-WIDTH and BUFFER-NAME are passed in by the caller so the fast path does
not allocate a fresh compound key during every redisplay."
  (and (eql +tab-line--format-cache-generation +tab-line-cache-generation)
       (eql +tab-line--format-cache-window-width window-width)
       (eq +tab-line--format-cache-frame (selected-frame))
       (eq +tab-line--format-cache-window (selected-window))
       (eq +tab-line--format-cache-scope (+tab-line-order-scope))
       (equal +tab-line--format-cache-buffer-name buffer-name)
       (equal +tab-line--format-cache-file-name
              (+tab-bar--buffer-file-name))
       (eq +tab-line--format-cache-major-mode major-mode)
       (eq +tab-line--format-cache-portal-popup
           (and (bound-and-true-p +portal-popup-instance-p) t))))

(defun +tab-line-store-format-cache (window-width buffer-name value)
  "Cache centered tab-line VALUE for WINDOW-WIDTH and BUFFER-NAME."
  (setq-local +tab-line--format-cache value
              +tab-line--format-cache-generation +tab-line-cache-generation
              +tab-line--format-cache-window-width window-width
              +tab-line--format-cache-frame (selected-frame)
              +tab-line--format-cache-window (selected-window)
              +tab-line--format-cache-scope (+tab-line-order-scope)
              +tab-line--format-cache-buffer-name buffer-name
              +tab-line--format-cache-file-name
              (+tab-bar--buffer-file-name)
              +tab-line--format-cache-major-mode major-mode
              +tab-line--format-cache-portal-popup
              (and (bound-and-true-p +portal-popup-instance-p) t))
  value)

(defun +tab-line-format-uncached ()
  "Render a centered, minimal buffer tab line without consulting the cache."
  (unless (+tab-line-hidden-p)
    (when-let* ((content (+tab-line-content)))
      (let* ((content-width (+tab-line-segments-width content))
             (window-width (window-body-width))
             (offset (if (< content-width (max 1 (- window-width 4)))
                         (/ content-width 2)
                       0)))
        (append
         (list
          (if (> offset 0)
              (propertize
               (+tab-line-passive-string " " 'tab-line)
               'display `(space :align-to (- center ,offset)))
            (+tab-line-passive-string " " 'tab-line)))
         content)))))

(defun +tab-line-overlay-leading-content (leading centered window-width)
  "Place CENTERED tabs over contextual LEADING text in WINDOW-WIDTH.
The tab segment wins both visually and for hit testing where the two meet."
  (cond
   ((null leading) centered)
   ((null centered)
    (list (truncate-string-to-width leading window-width)))
   (t
    (let* ((content (cdr centered))
           (content-width (+tab-line-segments-width content))
           (centered-p (< content-width (max 1 (- window-width 4))))
           (left (if centered-p
                     (max 0 (/ (- window-width content-width) 2))
                   1))
           (right (+ left content-width))
           ;; The contextual string is an underlay: tabs mask exactly their
           ;; own columns.  Keep both sides so a long breadcrumb visibly
           ;; continues after the centered tab group.
           (prefix (truncate-string-to-width leading left nil ?\s))
           (suffix (truncate-string-to-width leading window-width right)))
      (append (list prefix) content (when (not (string-empty-p suffix))
                                      (list suffix)))))))

(defun +tab-line-format ()
  "Render centered tabs over optional contextual text."
  (let* ((window-width (window-body-width))
         (buffer-name (buffer-name))
         (leading (+tab-line-leading-content))
         (centered
          (if (+tab-line-format-cache-valid-p window-width buffer-name)
              +tab-line--format-cache
            (+tab-line-store-format-cache
             window-width buffer-name (+tab-line-format-uncached)))))
    (+tab-line-overlay-leading-content leading centered window-width)))

(defun +tab-line-refresh (&rest _)
  "Refresh the centered buffer tab line."
  (+tab-line-invalidate-cache)
  (when (fboundp 'tab-line-force-update)
    (tab-line-force-update t)))

(defun +tab-line-apply-ui ()
  "Apply the local visual treatment for the centered buffer tab line."
  (aaron-ui-set-face '+tab-bar-buffer-face
                     :foreground 'fg-soft
                     :weight 'semibold)
  (aaron-ui-set-face '+tab-bar-buffer-path-face
                     :foreground 'fg-tab)
  (aaron-ui-set-face '+tab-line-current-face
                     :foreground 'accent-sand
                     :weight 'semibold)
  (aaron-ui-set-face '+tab-line-inactive-face
                     :foreground 'fg-tab)
  (aaron-ui-set-face '+tab-line-separator-face
                     :foreground 'fg-tab-separator)
  (aaron-ui-set-face '+tab-line-overflow-face
                     :foreground 'fg-tab-overflow)
  (aaron-ui-set-face 'tab-line
                     :background 'unspecified
                     :foreground 'fg-tab
                     :box nil
                     :overline nil
                     :underline nil
                     :height 0.95)
  (aaron-ui-set-face 'tab-line-active
                     :background 'unspecified
                     :foreground 'fg-tab
                     :box nil
                     :overline nil
                     :underline nil)
  (aaron-ui-set-face 'tab-line-inactive
                     :background 'unspecified
                     :foreground 'fg-tab
                     :box nil
                     :overline nil
                     :underline nil)
  (aaron-ui-set-face 'tab-line-tab
                     :background 'unspecified
                     :foreground 'fg-tab
                     :box nil
                     :overline nil
                     :underline nil)
  (aaron-ui-set-face 'tab-line-tab-current
                     :background 'unspecified
                     :foreground 'accent-sand
                     :box nil
                     :overline nil
                     :underline nil
                     :weight 'semibold)
  (aaron-ui-set-face 'tab-line-tab-inactive
                     :background 'unspecified
                     :foreground 'fg-tab
                     :box nil
                     :overline nil
                     :underline nil)
  (aaron-ui-set-face 'tab-line-highlight
                     :background 'unspecified
                     :foreground 'accent-sand
                     :box nil
                     :overline nil
                     :underline nil))

(use-package tab-bar
  :ensure nil
  :hook ((after-init . +tab-bar-ensure-hidden)
         (server-after-make-frame . +tab-bar-ensure-hidden))
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-hints nil)
  (tab-bar-auto-width t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-format '(+tab-bar-format-buffer
                    tab-bar-format-align-right
                    tab-bar-format-tabs))
  (tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  :config
  (when (display-graphic-p)
    (+tab-bar-ensure-hidden))
  (remove-hook 'post-command-hook #'+tab-bar-update-terminal-visibility)
  (remove-hook 'server-switch-hook #'+tab-bar-update-terminal-visibility)
  (+tab-bar-update-terminal-visibility))

(use-package tab-line
  :ensure nil
  :demand t
  :hook ((server-after-make-frame . +tab-line-apply-ui)
         (after-load-theme . +tab-line-apply-ui))
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show nil)
  :config
  (advice-add 'tab-line-format :override #'+tab-line-format)
  (global-set-key (kbd "C-<tab>") #'+tab-line/next-buffer)
  (global-set-key (kbd "C-S-<tab>") #'+tab-line/previous-buffer)
  (global-set-key (kbd "C-<backtab>") #'+tab-line/previous-buffer)
  (global-set-key (kbd "C-<iso-lefttab>") #'+tab-line/previous-buffer)
  (when (boundp 'tab-bar-mode-map)
    (define-key tab-bar-mode-map (kbd "C-<tab>") #'+tab-line/next-buffer)
    (define-key tab-bar-mode-map (kbd "C-S-<tab>") #'+tab-line/previous-buffer)
    (define-key tab-bar-mode-map (kbd "C-<backtab>") #'+tab-line/previous-buffer)
    (define-key tab-bar-mode-map (kbd "C-<iso-lefttab>")
                #'+tab-line/previous-buffer))
  (add-to-list 'tab-line-exclude-modes 'dashboard-mode)
  (add-hook 'buffer-list-update-hook #'+tab-line-schedule-invalidate-cache)
  (add-hook 'window-buffer-change-functions #'+tab-line-invalidate-cache)
  (add-hook 'window-state-change-functions #'+tab-line-invalidate-cache)
  (add-hook 'kill-buffer-hook #'+tab-line-invalidate-cache)
  (add-hook 'after-change-major-mode-hook #'+tab-line-invalidate-cache)
  (global-tab-line-mode 1)
  (with-eval-after-load 'perspective
    (add-hook 'persp-killed-hook #'+tab-line-forget-persp-order))
  (+tab-line-apply-ui)
  (+tab-line-refresh))

(provide 'ui/tabbar/config)
;;; ui/tabbar/config.el ends here

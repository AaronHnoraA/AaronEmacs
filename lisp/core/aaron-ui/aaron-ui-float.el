;;; aaron-ui-float.el --- Floating Aaron UI surfaces -*- lexical-binding: t; -*-

;;; Commentary:
;; A child-frame host for ordinary buffers and ETAF Views.  The host owns only
;; display, chrome and focus; callers own the content and decide whether close
;; kills their buffer.  Hidden surfaces keep their content and session alive.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'posframe)
(require 'aaron-ui)

(cl-defstruct (aaron-ui-float--panel (:constructor aaron-ui-float--make))
  buffer frame parent key title width height owned kill-on-close on-close
  on-hide actions green-left green-right green-help
  header-before position size expanded normal-size)

(defvar aaron-ui-float--panels nil "Live Aaron UI floating surfaces.")
(defvar-local aaron-ui-float--current nil "Floating host of this buffer.")

(defvar aaron-ui-float-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'aaron-ui-float-close)
    (define-key map (kbd "<escape>") #'aaron-ui-float-hide)
    (define-key map (kbd "M-w") #'aaron-ui-float-hide)
    (define-key map (kbd "C-c C-=") #'aaron-ui-float-toggle-size)
    map)
  "Keys for ETAF Views owned by the floating host.")

(define-minor-mode aaron-ui-float-view-mode
  "Provide close and hide keys in an Aaron UI ETAF floating View."
  :init-value nil :lighter nil :keymap aaron-ui-float-view-mode-map)

(defun aaron-ui-float-live-p (panel)
  "Return non-nil when PANEL still owns a live buffer."
  (and (aaron-ui-float--panel-p panel)
       (memq panel aaron-ui-float--panels)
       (buffer-live-p (aaron-ui-float--panel-buffer panel))))

(defun aaron-ui-float-panel-frame (panel)
  "Return PANEL's live child frame, or nil."
  (when (aaron-ui-float-live-p panel)
    (let ((frame (aaron-ui-float--panel-frame panel)))
      (and (frame-live-p frame) frame))))

(defun aaron-ui-float-visible-p (panel)
  "Return non-nil when PANEL's child frame is visible."
  (when-let* ((frame (aaron-ui-float-panel-frame panel)))
    (frame-visible-p frame)))

(defun aaron-ui-float-reopen (panel)
  "Show an existing PANEL again and focus it without replacing its content."
  (when (aaron-ui-float-live-p panel)
    (aaron-ui-float-show (aaron-ui-float--panel-buffer panel))))

(defun aaron-ui-float-panel-for-buffer (buffer)
  "Return the floating host of BUFFER, if any."
  (when (buffer-live-p buffer)
    (let ((panel (buffer-local-value 'aaron-ui-float--current buffer)))
      (and (aaron-ui-float-live-p panel) panel))))

(defun aaron-ui-float--parent (frame)
  "Return the ordinary parent frame of FRAME."
  (while (frame-parent frame)
    (setq frame (frame-parent frame)))
  frame)

(defun aaron-ui-float--dimension (value parent axis)
  "Convert VALUE to columns or rows of PARENT along AXIS."
  (let ((maximum (if (eq axis 'width) (frame-width parent)
                   (frame-height parent))))
    (max 4 (min (- maximum 2)
                (cond ((numberp value) (truncate value))
                      ((and (consp value) (eq (car value) 'fraction))
                       (truncate (* maximum (cadr value))))
                      (t (truncate (* maximum
                                      (if (eq axis 'width) 0.65 0.55)))))))))

(defun aaron-ui-float--button (label help action &optional color right-action)
  "Return a clickable LABEL with HELP, ACTION, COLOR and RIGHT-ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (_event) (interactive "e") (funcall action)))
    (define-key map [header-line down-mouse-1] #'ignore)
    (when right-action
      (define-key map [header-line mouse-3]
                  (lambda (_event) (interactive "e") (funcall right-action)))
      (define-key map [header-line down-mouse-3] #'ignore))
    (propertize label 'keymap map 'local-map map 'mouse-face 'highlight
                'pointer 'hand 'help-echo help
                'face (and color `(:foreground ,color :weight bold)))))

(defun aaron-ui-float--header ()
  "Render the current surface's shared title bar."
  (when-let* ((panel aaron-ui-float--current))
    (let* ((title (or (aaron-ui-float--panel-title panel) "浮窗"))
           (lights
            (concat
             (aaron-ui-float--button "●" "关闭浮窗"
                                     (lambda () (aaron-ui-float-close panel))
                                     "#FF5F57")
             "  "
             (aaron-ui-float--button "●" "隐藏浮窗，保留内容"
                                     (lambda () (aaron-ui-float-hide panel))
                                     "#FEBC2E")
             "  "
             (aaron-ui-float--button "●"
                                     (or (aaron-ui-float--panel-green-help panel)
                                         "放大或恢复浮窗")
                                     (or (aaron-ui-float--panel-green-left panel)
                                         (lambda () (aaron-ui-float-toggle-size panel)))
                                     "#28C840"
                                     (aaron-ui-float--panel-green-right panel))))
           (actions
            (mapconcat (lambda (entry)
                         (aaron-ui-float--button
                          (car entry) (car entry) (cdr entry)))
                       (aaron-ui-float--panel-actions panel) "  "))
           (width (max 1 (window-total-width)))
           (space (max 1 (- width (string-width title)
                            (string-width lights) (string-width actions) 3)))
           (face `(:background ,(aaron-ui-color 'bg-panel)
                    :foreground ,(aaron-ui-color 'fg-main)
                    :weight bold :box nil :extend t)))
      (concat (propertize " " 'face face)
              lights
              (propertize (concat "  " title (make-string space ?\s)) 'face face)
              actions " "))))

(defun aaron-ui-float--remember (panel)
  "Remember PANEL's position and size before hiding it."
  (when-let* ((frame (aaron-ui-float-panel-frame panel)))
    (setf (aaron-ui-float--panel-position panel) (frame-position frame)
          (aaron-ui-float--panel-size panel)
          (cons (frame-width frame) (frame-height frame)))))

(defun aaron-ui-float--focus-parent (panel)
  "Restore input focus after PANEL loses visibility."
  (let ((parent (aaron-ui-float--panel-parent panel)))
    (when (frame-live-p parent)
      (select-frame-set-input-focus parent))))

(defun aaron-ui-float-hide (&optional panel)
  "Hide PANEL while keeping its buffer and session alive."
  (interactive)
  (let ((panel (or panel aaron-ui-float--current)))
    (when (aaron-ui-float-live-p panel)
      (aaron-ui-float--remember panel)
      (posframe-hide (aaron-ui-float--panel-buffer panel))
      (aaron-ui-float--focus-parent panel)
      (when-let* ((callback (aaron-ui-float--panel-on-hide panel)))
        (condition-case err (funcall callback panel)
          (error (message "Aaron float hide callback: %s"
                          (error-message-string err)))))
      t)))

(defun aaron-ui-float--buffer-killed ()
  "Release this buffer's floating host during buffer teardown."
  (when (aaron-ui-float-live-p aaron-ui-float--current)
    (aaron-ui-float-close aaron-ui-float--current t)))

(defun aaron-ui-float-close (&optional panel no-kill)
  "Close PANEL; with NO-KILL leave its buffer alone.
An owned ETAF View is unmounted.  A caller's buffer is killed only when its
`:kill-on-close' option was supplied."
  (interactive)
  (let ((panel (or panel aaron-ui-float--current)))
    (when (aaron-ui-float-live-p panel)
      (let ((buffer (aaron-ui-float--panel-buffer panel)))
        (if (and (not no-kill)
                 (or (aaron-ui-float--panel-owned panel)
                     (aaron-ui-float--panel-kill-on-close panel)))
            ;; The kill hook finishes teardown.  If a query refuses the kill,
            ;; the panel and its buffer stay together.
            (progn (kill-buffer buffer) (not (buffer-live-p buffer)))
          (aaron-ui-float--remember panel)
          (setq aaron-ui-float--panels (delq panel aaron-ui-float--panels))
          (unless aaron-ui-float--panels
            (remove-hook 'window-size-change-functions
                         #'aaron-ui-float--window-size-changed))
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook #'aaron-ui-float--buffer-killed t)
            (aaron-ui-float-view-mode -1)
            (setq header-line-format (aaron-ui-float--panel-header-before panel)
                  aaron-ui-float--current nil))
          (ignore-errors (posframe-hide buffer))
          (ignore-errors (posframe-delete-frame buffer))
          (aaron-ui-float--focus-parent panel)
          (when (and (aaron-ui-float--panel-owned panel)
                     (fboundp 'etaf-runtime-for-buffer))
            (when-let* ((runtime (etaf-runtime-for-buffer buffer)))
              (ignore-errors (etaf-unmount runtime))))
          (when-let* ((callback (aaron-ui-float--panel-on-close panel)))
            (condition-case err (funcall callback panel)
              (error (message "Aaron float close callback: %s"
                              (error-message-string err)))))
          t)))))

(defun aaron-ui-float-toggle-size (&optional panel)
  "Expand or restore PANEL without changing the parent window layout."
  (interactive)
  (let* ((panel (or panel aaron-ui-float--current))
         (frame (aaron-ui-float-panel-frame panel))
         (parent (and frame (aaron-ui-float--panel-parent panel))))
    (when (and frame (frame-live-p parent))
      (if (aaron-ui-float--panel-expanded panel)
          (let ((size (aaron-ui-float--panel-normal-size panel)))
            (set-frame-size frame (car size) (cdr size))
            (setf (aaron-ui-float--panel-expanded panel) nil))
        (setf (aaron-ui-float--panel-normal-size panel)
              (cons (frame-width frame) (frame-height frame))
              (aaron-ui-float--panel-expanded panel) t)
        (set-frame-size frame
                        (max 4 (truncate (* (frame-width parent) 0.88)))
                        (max 4 (truncate (* (frame-height parent) 0.84)))))
      (set-frame-position
       frame
       (+ (car (frame-position parent))
          (max 0 (/ (- (frame-pixel-width parent)
                       (frame-pixel-width frame)) 2)))
       (+ (cdr (frame-position parent))
          (max 0 (/ (- (frame-pixel-height parent)
                       (frame-pixel-height frame)) 2))))
      (aaron-ui-float--refit panel)
      t)))

(defun aaron-ui-float--refit (panel)
  "Reflow an ETAF View hosted by PANEL after a frame resize."
  (when (and (aaron-ui-float-live-p panel)
             (aaron-ui-float--panel-owned panel)
             (fboundp 'etaf-runtime-for-buffer))
    (when-let* ((frame (aaron-ui-float-panel-frame panel))
                (runtime (etaf-runtime-for-buffer
                          (aaron-ui-float--panel-buffer panel))))
      (condition-case err
          (etaf-runtime-refit runtime
                              (window-body-width (frame-selected-window frame) t)
                              nil)
        (error (message "Aaron float View refit: %s"
                        (error-message-string err)))))))

(defun aaron-ui-float--window-size-changed (frame)
  "Reflow ETAF panels when FRAME changes size."
  (dolist (panel (copy-sequence aaron-ui-float--panels))
    (when (eq frame (aaron-ui-float-panel-frame panel))
      (aaron-ui-float--refit panel))))

(defun aaron-ui-float-show (buffer &rest options)
  "Display ordinary BUFFER in an Aaron UI child frame.
OPTIONS: :key, :title, :width, :height, :actions (label . function),
:green-left and :green-right override the green light's mouse actions;
:kill-on-close, :on-close, :on-hide, :owned.  Width and height are character
counts or (fraction N).  Return a panel; hidden panels can be shown again.
This GUI-only API does not change the parent frame's window layout."
  (unless (and (buffer-live-p buffer) (display-graphic-p)
               (posframe-workable-p))
    (user-error "Aaron UI floating surfaces require a live buffer and GUI"))
  (let* ((parent (aaron-ui-float--parent (selected-frame)))
         (key (plist-get options :key))
         (previous (or (aaron-ui-float-panel-for-buffer buffer)
                       (and key
                            (cl-find-if
                             (lambda (item)
                               (equal key (aaron-ui-float--panel-key item)))
                             aaron-ui-float--panels))))
         (panel (if (and previous
                         (eq buffer (aaron-ui-float--panel-buffer previous)))
                    previous
                  (aaron-ui-float--make
                   :buffer buffer :parent parent :key key
                   :title (plist-get options :title)
                   :width (plist-get options :width)
                   :height (plist-get options :height)
                   :actions (plist-get options :actions)
                   :owned (plist-get options :owned)
                   :kill-on-close (plist-get options :kill-on-close)
                   :on-close (plist-get options :on-close)
                   :on-hide (plist-get options :on-hide)
                   :green-left (plist-get options :green-left)
                   :green-right (plist-get options :green-right)
                   :green-help (plist-get options :green-help)))))
    (when (and previous (not (eq previous panel)))
      (aaron-ui-float-close previous))
    (unless (memq panel aaron-ui-float--panels)
      (with-current-buffer buffer
        (setf (aaron-ui-float--panel-header-before panel) header-line-format)
        (setq-local aaron-ui-float--current panel)
        (setq-local header-line-format '(:eval (aaron-ui-float--header)))
        (when (aaron-ui-float--panel-owned panel)
          (aaron-ui-float-view-mode 1))
        (add-hook 'kill-buffer-hook #'aaron-ui-float--buffer-killed nil t))
      (unless aaron-ui-float--panels
        (add-hook 'window-size-change-functions
                  #'aaron-ui-float--window-size-changed))
      (push panel aaron-ui-float--panels))
    (when (aaron-ui-float-visible-p panel)
      (aaron-ui-float--remember panel))
    (let* ((size (aaron-ui-float--panel-size panel))
           (width (or (car-safe size)
                      (aaron-ui-float--dimension
                       (aaron-ui-float--panel-width panel) parent 'width)))
           (height (or (cdr-safe size)
                       (aaron-ui-float--dimension
                        (aaron-ui-float--panel-height panel) parent 'height)))
           (position (aaron-ui-float--panel-position panel))
           frame)
      (condition-case err
          (progn
            (setq frame
                  (with-selected-frame parent
                    (posframe-show
                     buffer :position 1 :poshandler
                     (if position
                         (lambda (_info) position)
                       #'posframe-poshandler-frame-center)
                     :width width :height height
                     :window-point (with-current-buffer buffer (point))
                     :respect-header-line t :respect-mode-line nil
                     :left-fringe 0 :right-fringe 0
                     :internal-border-width 2
                     :border-width 1
                     :border-color (aaron-ui-color 'border-subtle)
                     :accept-focus t
                     :cursor (unless (aaron-ui-float--panel-owned panel) 'bar)
                     :override-parameters '((drag-with-header-line . t)
                                            (horizontal-scroll-bars . nil)
                                            (vertical-scroll-bars . nil)))))
            (unless (frame-live-p frame)
              (error "Posframe did not return a live frame"))
            (setf (aaron-ui-float--panel-frame panel) frame
                  (aaron-ui-float--panel-parent panel) parent)
            (aaron-ui-float--refit panel)
            (select-frame frame)
            (select-frame-set-input-focus frame)
            panel)
        (error
         (aaron-ui-float-close panel t)
         (signal (car err) (cdr err)))))))

(defun aaron-ui-float-show-view (view &rest options)
  "Mount ETAF VIEW and show it through the same Aaron UI host.
The host owns the generated buffer and unmounts the Runtime on close."
  (require 'etaf)
  (let* ((buffer (generate-new-buffer " *Aaron UI View*"))
         (parent (aaron-ui-float--parent (selected-frame)))
         (width (aaron-ui-float--dimension (plist-get options :width)
                                            parent 'width))
         (height (aaron-ui-float--dimension (plist-get options :height)
                                             parent 'height)))
    (condition-case err
        (progn
          (etaf-mount buffer view
                      (list :viewport-width (* width (frame-char-width parent))
                            :viewport-height height))
          (apply #'aaron-ui-float-show buffer
                 :owned t :width width :height height options))
      (error
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (signal (car err) (cdr err))))))

(provide 'aaron-ui-float)
;;; aaron-ui-float.el ends here

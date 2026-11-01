;;; views.el --- Agent presentation belongs to windows -*- lexical-binding: t; -*-

;;; Commentary:
;; One display controller over the real agent-shell buffers.  A session has
;; one process and project identity; its source, location and return point
;; belong to a window, recorded by this controller alone.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'window)
(defvar +window-buffer-replacement)
(defvar +agent-view-replacement-windows nil)

(defun +agent-view--replacement-deletable-p (window _frame)
  "Keep a synchronously replaced execution view through native buffer kill."
  (not (memq window +agent-view-replacement-windows)))

(declare-function +agent-acp-agent-buffer-p "tools/agent/acp" (buffer))
(declare-function +agent-acp--install-workspace-tabs "tools/agent/acp" (buffer))
(declare-function +portal-popup-display-buffer "term/portal/popup" (buffer))
(declare-function +portal-popup-hide-buffer "term/portal/popup" (buffer))
(declare-function +portal/hide-popup "term/portal/popup" ())
(declare-function +window--repair-after-close "ui/windows/config"
                  (window closing previous &optional directory listing))

(defun +agent-view-parent-frame (&optional frame)
  "Resolve FRAME to its ordinary parent, never a child-frame dock."
  (let ((frame (or frame (selected-frame))))
    (while (frame-parent frame) (setq frame (frame-parent frame)))
    frame))

(defun +agent-view--agent-window-p (window)
  (and (window-live-p window) (+agent-acp-agent-buffer-p (window-buffer window))
       (not (window-parameter window 'aaron-ui-preview))))

(defun +agent-view--editor-window-p (window frame)
  (and (window-live-p window) (eq (window-frame window) frame)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (+agent-acp-agent-buffer-p (window-buffer window)))))

(defun +agent-view--editor-window (frame)
  "Return FRAME's most recently used editor without displaying anything."
  (car (sort (seq-filter (lambda (window) (+agent-view--editor-window-p window frame))
                         (window-list frame 'nomini))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun +agent-view-origin-window (&optional window)
  "Resolve WINDOW's source in its own frame; never use another session's past."
  (let* ((window (or window (selected-window)))
         (frame (+agent-view-parent-frame (window-frame window)))
         (record (window-parameter window '+agent-view))
         (origin (and (eq (window-buffer window) (plist-get record :buffer))
                      (plist-get record :origin))))
    (cond ((+agent-view--editor-window-p origin frame) origin)
          ((+agent-view--editor-window-p window frame) window)
          (t (+agent-view--editor-window frame)))))

(defun +agent-view--managed-p (window)
  "A current agent view, rather than a stale tag on a new editing buffer."
  (and (+agent-view--agent-window-p window)
       (or (window-parameter window 'agent-workspace)
           (window-parameter window '+portal-popup)
           (let ((record (window-parameter window '+agent-view)))
             (and (eq (window-buffer window) (plist-get record :buffer))
                  (plist-get record :agent-kind))))))

(defun +agent-view--kind (window)
  (cond ((window-parameter window '+portal-popup) 'popup)
        ((window-parameter window 'agent-workspace) 'dock)
        (t 'embedded)))

(defun +agent-view--record (window buffer origin &optional previous)
  "Record BUFFER's presentation in WINDOW, retaining its non-agent source."
  (let* ((old (window-parameter window '+agent-view))
         (old-valid (and (eq (window-buffer window) (plist-get old :buffer)) old))
         (source (or origin (plist-get old-valid :origin)))
         (previous (or previous (plist-get old-valid :previous))))
    (set-window-parameter window '+agent-view
                          (list :buffer buffer :origin source :previous previous
                                :agent-kind (+agent-view--kind window)))))

(defun +agent-view--previous (window)
  "Capture a non-agent buffer in WINDOW before an explicit embedded display."
  (let ((buffer (window-buffer window)))
    (unless (+agent-acp-agent-buffer-p buffer)
      (list buffer (window-point window) (window-start window)))))

(defun +agent-view-embed (buffer window)
  "Explicitly display BUFFER in WINDOW and remember its previous editor."
  (unless (window-live-p window) (user-error "The requested agent window was closed"))
  (let ((previous (+agent-view--previous window))
        (origin (+agent-view-origin-window window)))
    (+agent-view--record window buffer origin previous)
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (select-frame-set-input-focus (window-frame window))
    (select-window window)
    window))

(defun +agent-view--new-dock (buffer frame)
  "Allocate a free right-hand slot without replacing an existing side view."
  (let ((used (mapcar (lambda (window) (window-parameter window 'window-slot))
                      (seq-filter (lambda (window) (eq (window-parameter window 'window-side) 'right))
                                  (window-list frame 'nomini))))
        (slot 0))
    (while (memq slot used) (cl-incf slot))
    (with-selected-frame frame
      (display-buffer-in-side-window
       buffer `((side . right) (slot . ,slot) (window-width . 0.42)
                (window-parameters . ((agent-workspace . t))))))))

(defvar +agent-view-request nil
  "Dynamically scoped presentation for an explicit physical replacement.
It expires with the restart/resume command and never becomes buffer ownership.")

(defun +agent-view-exit-preview ()
  "Leave a temporary preview before an explicit conversation operation."
  (when-let* ((hide (window-parameter (selected-window) 'aaron-ui-preview-hide-function)))
    (funcall hide)))

(defun +agent-view-capture (buffer)
  "Capture BUFFER's current-frame view for a synchronous physical replacement."
  (when-let* ((window (if (eq (window-buffer (selected-window)) buffer) (selected-window)
                       (get-buffer-window buffer (selected-frame)))))
    (unless (window-parameter window 'aaron-ui-preview)
      (list :window window :frame (window-frame window) :kind (+agent-view--kind window)
            :record (copy-sequence (window-parameter window '+agent-view))
            :origin (+agent-view-origin-window window)))))

(defun +agent-view-show (buffer)
  "Show BUFFER in the current view, a local view, or a new local dock.
A buffer's former popup membership never overrides the active chat window.
User-created agent splits and windows in other frames are left alone."
  ;; An action from any preview session transfers to its parent's controller.
  (+agent-view-exit-preview)
  (let* ((request +agent-view-request)
         (selected (selected-window))
         (frame (if (frame-live-p (plist-get request :frame)) (plist-get request :frame)
                  (+agent-view-parent-frame (window-frame selected))))
         (origin (or (plist-get request :origin) (+agent-view-origin-window selected)))
         (previous (plist-get (plist-get request :record) :previous))
         (custom (buffer-local-value '+agent-acp-display-buffer-function buffer))
         (popup-preference (eq custom #'+portal-popup-display-buffer))
         ;; Pre-controller here/move-out sessions stored a closure over their
         ;; old window.  It is a stale placement, not a domain frame contract.
         (special (and custom (not popup-preference)
                       (or (symbolp custom)
                           (not (memq (buffer-local-value '+agent-acp-session-origin buffer)
                                      '(manual popup here))))))
         (window
          (or (and (window-live-p (plist-get request :window)) (plist-get request :window))
              (and (not request) (eq (window-frame selected) frame)
                   (+agent-view--agent-window-p selected) selected)
              (seq-find (lambda (candidate)
                          (and (+agent-view--agent-window-p candidate)
                               (eq (window-buffer candidate) buffer)))
                        (window-list frame 'nomini))
              (and (not request)
                   (car (sort (seq-filter #'+agent-view--managed-p (window-list frame 'nomini))
                              (lambda (a b) (> (window-use-time a) (window-use-time b)))))))))
    (cond
     ;; Explicit domain intervention displays retain their own frame contract.
     (special
      (funcall custom buffer)
      (when (eq (window-buffer (selected-window)) buffer)
        (+agent-view--record (selected-window) buffer origin))
      (selected-window))
     ((or (eq (plist-get request :kind) 'popup)
          (and window (window-parameter window '+portal-popup)))
      (let ((previous (or previous (and window (+agent-view--previous window)))))
        (select-frame-set-input-focus frame)
        (when (window-live-p window) (select-window window))
        (+portal-popup-display-buffer buffer)
        (setq window (selected-window))
        (+agent-view--record window buffer origin previous)
        (select-window window)
        window))
     (window
      (let ((previous (or previous (+agent-view--previous window))))
        (+agent-view--record window buffer origin previous)
        (set-window-dedicated-p window nil)
        (set-window-buffer window buffer)
        (when (window-parameter window 'agent-workspace)
          (set-window-parameter window 'agent-workspace-root
                                (buffer-local-value '+agent-acp-session-root buffer)))
        (select-frame-set-input-focus frame)
        (select-window window)
        window))
     ((and popup-preference (not request))
      (select-frame frame)
      (+portal-popup-display-buffer buffer)
      (+agent-view--record (selected-window) buffer origin)
      (selected-window))
     (t
      (let ((window (+agent-view--new-dock buffer frame)))
        (set-window-parameter window 'agent-workspace-root
                              (buffer-local-value '+agent-acp-session-root buffer))
        (+agent-view--record window buffer origin)
        (select-frame-set-input-focus frame)
        (select-window window)
        window)))))

(defun +agent-view--restore-editor (window record)
  "Restore WINDOW's non-agent content without resurrecting another session."
  (let* ((previous (plist-get record :previous))
         (buffer (car previous)))
    (unless (and (buffer-live-p buffer) (not (+agent-acp-agent-buffer-p buffer)))
      (setq previous
            (seq-find (lambda (entry)
                        (and (buffer-live-p (car entry))
                             (not (+agent-acp-agent-buffer-p (car entry)))
                             (not (string-prefix-p " " (buffer-name (car entry))))))
                      (window-prev-buffers window))
            buffer (car previous))
      (when previous
        (setq previous (list buffer (nth 2 previous) (nth 1 previous)))))
    (if buffer
        (progn
          (set-window-dedicated-p window nil)
          (set-window-buffer window buffer)
          (when (integer-or-marker-p (cadr previous)) (set-window-point window (cadr previous)))
          (when (integer-or-marker-p (caddr previous)) (set-window-start window (caddr previous) t)))
      ;; A user-created view may have no source history at all.  The shared
      ;; window layer owns the final project/dashboard fallback (C10).
      (if (not (eq window (window-main-window (window-frame window))))
          (delete-window window)
        (let* ((closing (window-buffer window))
               (directory (buffer-local-value 'default-directory closing)))
          (set-window-buffer window (get-buffer-create "*scratch*"))
          (if (fboundp '+window--repair-after-close)
              (+window--repair-after-close window closing nil directory)
            (with-selected-window window (dired directory))))))))

(defun +agent-view-hide (buffer)
  "Hide BUFFER's local view; hiding a background view never changes focus."
  (let* ((selected (selected-window))
         (frame (window-frame selected))
         (window (if (eq (window-buffer selected) buffer) selected
                   (get-buffer-window buffer frame)))
         (foreground (eq selected window)))
    (when (window-live-p window)
      (if-let* ((hide (window-parameter window 'aaron-ui-preview-hide-function)))
          (when foreground (funcall hide))
        (let* ((record (window-parameter window '+agent-view))
               (origin (+agent-view-origin-window window))
               (kind (+agent-view--kind window)))
          (set-window-parameter window '+agent-view nil)
          (pcase kind
            ('popup (+portal-popup-hide-buffer buffer))
            ('dock (delete-window window))
            (_ (+agent-view--restore-editor window record)))
          (when (and foreground (window-live-p origin) (not (eq origin window)))
            (select-frame-set-input-focus (window-frame origin))
            (select-window origin)))))))

(defun +agent-view-abandon (view old-buffer)
  "Release VIEW after a failed physical replacement of OLD-BUFFER.
Only the still-owned window is repaired; an editor repurposed meanwhile is
never reclaimed by an old return record."
  (when-let* ((window (plist-get view :window))
              ((window-live-p window))
              ((eq (plist-get (window-parameter window '+agent-view) :buffer) old-buffer)))
    (let ((record (plist-get view :record)))
      (set-window-parameter window '+agent-view nil)
      (save-selected-window
        (with-selected-window window
          (pcase (plist-get view :kind)
            ('popup (+portal/hide-popup))
            ('dock (delete-window window))
            (_ (+agent-view--restore-editor window record))))))))

(provide 'tools/agent/views)
;;; views.el ends here

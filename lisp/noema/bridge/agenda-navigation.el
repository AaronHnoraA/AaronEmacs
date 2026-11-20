;;; noema/bridge/agenda-navigation.el --- Agenda visits in live note panes -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'noema/bridge/config)
(require 'noema-agenda)

(defvar-local +noema-agenda--pending-visit nil
  "One navigation intent waiting for this exact page to load its note.")

(defun +noema-agenda--clear-pending-visit ()
  "Release this page's pending request when its buffer is killed."
  (when +noema-agenda--pending-visit
    (noema-agenda--finish-visit (plist-get +noema-agenda--pending-visit :request))
    (setq +noema-agenda--pending-visit nil)))

(defun +noema-agenda--cancel-pending-visits ()
  "Discard page intents when native navigation is cancelled or superseded."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer (+noema-agenda--clear-pending-visit))))

(defun +noema-agenda--page-ready (file buffer)
  "Deliver BUFFER's pending visit only to FILE and the window that requested it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((pending +noema-agenda--pending-visit))
        (setq +noema-agenda--pending-visit nil)
        (noema-agenda--finish-visit (plist-get pending :request))
        (let ((window (plist-get pending :window)))
          (when (and (= (plist-get pending :request) noema-agenda--visit-request)
                     (equal file (plist-get pending :file))
                     (equal +noema--loaded-file file)
                     (window-live-p window) (eq window (selected-window))
                     (eq (window-buffer window) buffer))
            ;; Activity normally resumes after a short focus debounce. This
            ;; explicit foreground visit must reach an already resumed page.
            (when +noema--activity-hooks-installed
              (+noema--apply-activity-snapshot (+noema--activity-snapshot)))
            (+noema/command "agenda-visit"
                            (cons (cons 'file (+noema--host-file file))
                                  (delq nil
                                        (mapcar
                                         (lambda (key)
                                           (when-let* ((value (noema-agenda--get (plist-get pending :record) key)))
                                             (cons key value)))
                                         '(id sourceKind sourceRef index source quote)))))))))))

(defun +noema-agenda-visit-record (record request)
  "Visit Markdown RECORD in its canonical pane, guarding delayed REQUEST.
Return non-nil when this adapter owns the visit. Never read or save the note."
  (let ((file (noema-agenda--get record 'file)))
    (when (and (stringp file) (+noema--markdown-file-p file))
      (let* ((file (+noema--canonical-file file))
             (client (noema-agenda--get record 'originClient))
             (origin (if (and (stringp client) (not (string-empty-p client)))
                         (when-let* ((buffer (+noema--buffer-for-client client)))
                           (get-buffer-window buffer 'visible))
                       (selected-window)))
             (source (and (window-live-p origin) (window-buffer origin))))
        (noema-agenda--watch-visit request)
        (+noema--ensure-server
         (lambda ()
           (if (not (and (= request noema-agenda--visit-request)
                         (window-live-p origin) (eq origin (selected-window))
                         (eq source (window-buffer origin))))
               (noema-agenda--finish-visit request)
             (condition-case failure
                 (let* ((existing (+noema--buffer-for-file file))
                        (visible (and (buffer-live-p existing) (get-buffer-window existing 'visible)))
                        (buffer (if (buffer-live-p existing)
                                    (progn
                                      (if visible (select-window visible) (switch-to-buffer existing))
                                      existing)
                                  (+noema--open-url (+noema--app-url file (+noema--xwidget-session-id file)) file))))
                   (unless (buffer-live-p buffer) (user-error "Noema could not open the task's note"))
                   (with-current-buffer buffer
                     (setq +noema-agenda--pending-visit
                           (list :request request :record record :file file :window (selected-window)))
                     (add-hook 'kill-buffer-hook #'+noema-agenda--clear-pending-visit nil t))
                   (noema--focus-xwidget-buffer buffer)
                   (with-current-buffer buffer
                     (when (equal +noema--loaded-file file)
                       (+noema-agenda--page-ready file buffer))))
               (error (noema-agenda--finish-visit request)
                      (noema-agenda--error (error-message-string failure)))))))
        t))))

(add-hook '+noema-current-file-functions #'+noema-agenda--page-ready)
(add-hook 'noema-agenda-navigation-cancel-functions #'+noema-agenda--cancel-pending-visits)

(provide 'noema/bridge/agenda-navigation)
;;; agenda-navigation.el ends here

;;; noema-portal.el --- Portal Web host for Noema -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function my/portal--ensure "init-portal" ())
(declare-function portal-web--open "portal-web" (url options))
(declare-function portal--web-load "portal-web" (target url))
(declare-function portal-input-focus "portal-input" ())
(declare-function my/portal--focus-selected-window
                  "init-portal" (frame &optional selection-changed))
(declare-function my/noema--buffer-for-client "init-noema" (client))
(declare-function my/noema--buffer-for-file "init-noema" (file))
(declare-function my/noema--split-client-p "init-noema" (client))
(declare-function my/noema--split-client-ordinal "init-noema" (client))
(declare-function my/noema--split-buffer-display-name "init-noema" (file ordinal))
(declare-function my/noema--buffer-display-name "init-noema" (file))
(declare-function my/noema--url-with-client "init-noema" (url client))
(declare-function my/noema--app-url "init-noema" (file client))
(declare-function my/noema--track-app-buffer "init-noema" (buffer &optional file client))
(declare-function my/noema--canonical-file "init-noema" (file))
(declare-function my/noema--xwidget-session-id "init-noema" (file))
(declare-function my/noema--setup-portal-chrome "init-noema" ())
(declare-function my/noema-command "init-noema" (command &optional detail))
(declare-function my/noema--notify-client-closed "init-noema" (&optional client file))
(declare-function portal-web-send "portal-web" (plist))
(defvar my/noema--app-buffer)
(defvar my/noema--split-counter)
(defvar my/noema--client-id)
(defvar my/noema-buffer-file-name)
(defvar portal-web--target)
(defvar portal-web--url)

(defun my/noema-portal--focus-gained ()
  "Tell the selected Noema page to focus its editor after native focus."
  (when (and (my/noema-portal-buffer-p)
             (fboundp 'my/noema-command))
    (my/noema-command "focus")))

(add-hook 'my/portal-focus-gained-hook #'my/noema-portal--focus-gained)

(defun my/noema-portal-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a Noema page hosted by Portal."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (eq major-mode 'portal-web-mode)
                (or my/noema--client-id my/noema-buffer-file-name))))))

(defun my/noema-portal--session-buffer (client)
  "Return the live Portal page for CLIENT."
  (let ((buffer (my/noema--buffer-for-client client)))
    (and (my/noema-portal-buffer-p buffer) buffer)))

(defun my/noema-portal--load (buffer url)
  "Load URL into Portal web BUFFER."
  (with-current-buffer buffer
    (unless (and (stringp url) (string-prefix-p "http://127.0.0.1:" url))
      (user-error "Noema Portal accepts only its local web host"))
    (unless (equal portal-web--url url)
      (portal--web-load portal-web--target url)
      (setq portal-web--url url)))
  buffer)

(defun my/noema-portal--new (url id file)
  "Open URL for Noema CLIENT ID and FILE in the selected pane."
  (require 'init-portal)
  (my/portal--ensure)
  (require 'portal-web)
  (let* ((name (if file
                   (if (my/noema--split-client-p id)
                       (my/noema--split-buffer-display-name
                        file (or (my/noema--split-client-ordinal id) 0))
                     (my/noema--buffer-display-name file))
                 (format "*Noema:%s*" id)))
         (buffer (generate-new-buffer name)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (portal-web-mode)
          ;; The Noema host uses a local HTTP origin. Portal's hosted option
          ;; supplies its message bridge and keeps navigation on that origin.
          (portal-web--open url '(:hosted t))
          (my/noema--track-app-buffer buffer file id)
          (my/noema--setup-portal-chrome)
          (my/portal--focus-selected-window (selected-frame) t)
          (setq buffer nil)
          (current-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my/noema-portal-open-client (url id &optional file)
  "Open URL for a fresh Noema Portal page with client ID and FILE."
  (my/noema-portal--new url id file))

(defun my/noema-portal-open (url &optional file)
  "Open Noema URL and optional FILE in Portal, reusing its live page."
  (let* ((file (my/noema--canonical-file file))
         (nominal-id (if file (my/noema--xwidget-session-id file)
                       "noema:surface"))
         (nominal-buffer (my/noema-portal--session-buffer nominal-id))
         (existing (or (and file (my/noema--buffer-for-file file))
                       (and (my/noema-portal-buffer-p nominal-buffer)
                            (equal (buffer-local-value 'my/noema-buffer-file-name
                                                       nominal-buffer)
                                   file)
                            nominal-buffer)))
         (id (if (and (not existing) nominal-buffer)
                 (format "%s:visit:%d" nominal-id
                         (cl-incf my/noema--split-counter))
               nominal-id))
         (url (if file (my/noema--app-url file id)
                (my/noema--url-with-client url id))))
    (if existing
        (progn
          (if-let* ((window (get-buffer-window existing 'visible)))
              (select-window window)
            (switch-to-buffer existing))
          (unless file (my/noema-portal--load existing url))
          (my/noema--track-app-buffer existing file id)
          existing)
      (my/noema-portal--new url id file))))

(defun my/noema-portal-focus (&optional buffer)
  "Give Noema Portal BUFFER's page the keyboard."
  (when (my/noema-portal-buffer-p buffer)
    (let* ((buffer (or buffer (current-buffer)))
           (selected (selected-window))
           (window (if (eq (window-buffer selected) buffer)
                       selected
                     (get-buffer-window buffer 'visible))))
      (when window
        (select-window window)
        (my/portal--focus-selected-window (selected-frame) t)))))

(defun my/noema-portal-send (buffer message)
  "Send MESSAGE to the Noema page in Portal BUFFER."
  (when (my/noema-portal-buffer-p buffer)
    (with-current-buffer buffer
      (portal-web-send message))))

(provide 'noema-portal)
;;; noema-portal.el ends here

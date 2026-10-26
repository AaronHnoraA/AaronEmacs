;;; noema/bridge/portal.el --- Portal Web host for Noema -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function +portal--ensure "term/portal/config" ())
(declare-function portal-web--open "portal-web" (url options))
(declare-function portal--web-load "portal-web" (target url))
(declare-function portal-input-focus "portal-input" ())
(declare-function +portal--focus-selected-window
                  "term/portal/config" (frame &optional selection-changed))
(declare-function +noema--buffer-for-client "noema/bridge/config" (client))
(declare-function +noema--buffer-for-file "noema/bridge/config" (file))
(declare-function +noema--split-client-p "noema/bridge/config" (client))
(declare-function +noema--split-client-ordinal "noema/bridge/config" (client))
(declare-function +noema--split-buffer-display-name "noema/bridge/config" (file ordinal))
(declare-function +noema--buffer-display-name "noema/bridge/config" (file))
(declare-function +noema--url-with-client "noema/bridge/config" (url client))
(declare-function +noema--app-url "noema/bridge/config" (file client))
(declare-function +noema--track-app-buffer "noema/bridge/config" (buffer &optional file client))
(declare-function +noema--canonical-file "noema/bridge/config" (file))
(declare-function +noema--xwidget-session-id "noema/bridge/config" (file))
(declare-function +noema--setup-portal-chrome "noema/bridge/config" ())
(declare-function +noema/command "noema/bridge/config" (command &optional detail))
(declare-function +noema--notify-client-closed "noema/bridge/config" (&optional client file))
(declare-function portal-web-send "portal-web" (plist))
(defvar +noema--app-buffer)
(defvar +noema--split-counter)
(defvar +noema--client-id)
(defvar +noema-buffer-file-name)
(defvar portal-web--target)
(defvar portal-web--url)
(defvar +portal--explicitly-unfocused)

(defun +noema-portal--focus-gained ()
  "Tell the selected Noema page to focus its editor after native focus."
  (when (and (+noema-portal-buffer-p)
             (fboundp '+noema/command))
    (+noema/command "focus")))

(add-hook '+portal-focus-gained-hook #'+noema-portal--focus-gained)

(defun +noema-portal-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a Noema page hosted by Portal."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (eq major-mode 'portal-web-mode)
                (or +noema--client-id +noema-buffer-file-name))))))

(defun +noema-portal--session-buffer (client)
  "Return the live Portal page for CLIENT."
  (let ((buffer (+noema--buffer-for-client client)))
    (and (+noema-portal-buffer-p buffer) buffer)))

(defun +noema-portal--load (buffer url)
  "Load URL into Portal web BUFFER."
  (with-current-buffer buffer
    (unless (and (stringp url) (string-prefix-p "http://127.0.0.1:" url))
      (user-error "Noema Portal accepts only its local web host"))
    (unless (equal portal-web--url url)
      (portal--web-load portal-web--target url)
      (setq portal-web--url url)))
  buffer)

(defun +noema-portal--new (url id file)
  "Open URL for Noema CLIENT ID and FILE in the selected pane."
  (require 'term/portal/config)
  (+portal--ensure)
  (require 'portal-web)
  (let* ((name (if file
                   (if (+noema--split-client-p id)
                       (+noema--split-buffer-display-name
                        file (or (+noema--split-client-ordinal id) 0))
                     (+noema--buffer-display-name file))
                 (format "*Noema:%s*" id)))
         (buffer (generate-new-buffer name)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (portal-web-mode)
          ;; The Noema host uses a local HTTP origin. Portal's hosted option
          ;; supplies its message bridge and keeps navigation on that origin.
          (portal-web--open url '(:hosted t))
          (+noema--track-app-buffer buffer file id)
          (+noema--setup-portal-chrome)
          (+portal--focus-selected-window (selected-frame) t)
          (setq buffer nil)
          (current-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun +noema-portal-open-client (url id &optional file)
  "Open URL for a fresh Noema Portal page with client ID and FILE."
  (+noema-portal--new url id file))

(defun +noema-portal-open (url &optional file)
  "Open Noema URL and optional FILE in Portal, reusing its live page."
  (let* ((file (+noema--canonical-file file))
         (nominal-id (if file (+noema--xwidget-session-id file)
                       "noema:surface"))
         (nominal-buffer (+noema-portal--session-buffer nominal-id))
         (existing (or (and file (+noema--buffer-for-file file))
                       (and (+noema-portal-buffer-p nominal-buffer)
                            (equal (buffer-local-value '+noema-buffer-file-name
                                                       nominal-buffer)
                                   file)
                            nominal-buffer)))
         (id (if (and (not existing) nominal-buffer)
                 (format "%s:visit:%d" nominal-id
                         (cl-incf +noema--split-counter))
               nominal-id))
         (url (if file (+noema--app-url file id)
                (+noema--url-with-client url id))))
    (if existing
        (progn
          (if-let* ((window (get-buffer-window existing 'visible)))
              (select-window window)
            (switch-to-buffer existing))
          (unless file (+noema-portal--load existing url))
          (+noema--track-app-buffer existing file id)
          existing)
      (+noema-portal--new url id file))))

(defun +noema-portal-focus (&optional buffer)
  "Give Noema Portal BUFFER's page the keyboard."
  (when (+noema-portal-buffer-p buffer)
    (let* ((buffer (or buffer (current-buffer)))
           (selected (selected-window))
           (window (if (eq (window-buffer selected) buffer)
                       selected
                     (get-buffer-window buffer 'visible))))
      (when window
        (select-window window)
        (with-current-buffer buffer
          (setq-local +portal--explicitly-unfocused nil))
        (+portal--focus-selected-window (selected-frame) t)))))

(defun +noema-portal-send (buffer message)
  "Send MESSAGE to the Noema page in Portal BUFFER."
  (when (+noema-portal-buffer-p buffer)
    (with-current-buffer buffer
      (portal-web-send message))))

(provide 'noema/bridge/portal)
;;; noema/bridge/portal.el ends here

;;; init-org-roam.el --- Org Roam setup -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-core)

(defcustom my/org-roam-background-init-delay 2
  "Idle delay before Org Roam starts its background services."
  :type 'number
  :group 'my/org-ui)

(defcustom my/org-roam-buffer-redisplay-idle-delay 0.20
  "Idle delay used to coalesce Org Roam side-buffer follow refreshes.
The upstream hook runs from `post-command-hook'; this delay keeps the side
buffer following point while avoiding repeated node lookups during continuous
motion or typing."
  :type 'number
  :group 'my/org-ui)

(defvar my/org-roam--background-timer nil)
(defvar my/org-roam--initialized nil)
(defvar-local my/org-roam--redisplay-timer nil)
(defvar-local my/org-roam--redisplay-key nil)

(defvar my/org-roam-db-file
  (expand-file-name "org-roam.db"
                    (if (boundp 'my/org-state-dir)
                        my/org-state-dir
                      (expand-file-name "var/org/" user-emacs-directory)))
  "Canonical Org Roam database file for this Emacs configuration.")

(declare-function my/org-reference-create-target-dwim "init-org-utility" ())
(declare-function my/org-insert-id-link "init-org-utility" ())
(declare-function my/org-insert-target-link "init-org-utility" ())

(defun my/org-roam-capture-head (tag)
  "Return the default Org Roam capture head with filetag TAG."
  (format "#+title: ${title}
#+date: %%u
#+filetags: :%s:

#+begin_overview :toc t :depth 3
- 还没有标题。
#+end_overview
"
          tag))

(defun my/org-roam--cancel-background-timer ()
  "Cancel the deferred Org Roam background initialization timer."
  (when (timerp my/org-roam--background-timer)
    (cancel-timer my/org-roam--background-timer))
  (setq my/org-roam--background-timer nil))

(defun my/org-roam-enable ()
  "Initialize org-roam once."
  (my/org-roam--cancel-background-timer)
  (unless my/org-roam--initialized
    (org-roam-db-autosync-mode 1)
    (setq my/org-roam--initialized t)))

(defun my/org-roam-background-init ()
  "Load Org Roam on idle so startup stays responsive."
  (my/org-roam--cancel-background-timer)
  (unless (featurep 'org-roam)
    (require 'org-roam nil t)))

(use-package org-roam
  :ensure t
  :defer t
  :commands (org-roam-node-find
             org-roam-node-insert
             org-roam-tag-add
             org-roam-alias-add
             org-roam-buffer-toggle
             org-roam-db-autosync-mode)
  :init
  (setq org-roam-directory my-org-roam-dir)
  (setq org-roam-db-location my/org-roam-db-file)
  (setq org-roam-v2-ack t)
  
  ;; 定义前缀命令
  (define-prefix-command 'my-org-roam-command-map)
  (global-set-key (kbd "C-c n") 'my-org-roam-command-map)

  :bind 
  (:map my-org-roam-command-map
        ("f" . org-roam-node-find)
        ("i" . org-roam-node-insert)
        ("t" . org-roam-tag-add)
        ("a" . org-roam-alias-add)
        ("o" . my/org-reference-create-target-dwim)
        ("I" . my/org-insert-id-link)
        ("T" . my/org-insert-target-link)
        ("l" . org-roam-buffer-toggle))
  
  :config
  (defun my/org-roam-db--first-content-position ()
    "Return the first non-blank buffer position for Org Roam file nodes."
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r")
      (point)))

  (defun org-roam-db-insert-file-node ()
    "Insert the file-level node into the Org-roam cache.
This local override matches upstream Org Roam, but tolerates blank lines before
the file-level property drawer.  Otherwise notes whose first byte is a newline
are added to the files table but never become nodes."
    (org-with-point-at (my/org-roam-db--first-content-position)
      (when (and (= (org-outline-level) 0)
                 (org-roam-db-node-p))
        (when-let* ((id (org-id-get)))
          (let* ((file (buffer-file-name (buffer-base-buffer)))
                 (title (org-roam-db--file-title))
                 (pos (point))
                 (todo nil)
                 (priority nil)
                 (scheduled nil)
                 (deadline nil)
                 (level 0)
                 (tags org-file-tags)
                 (properties (org-entry-properties))
                 (olp nil))
            (org-roam-db-query!
             (lambda (err)
               (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                      (error-message-string err)
                      title id file))
             [:insert :into nodes
              :values $v1]
             (vector id file level pos todo priority
                     scheduled deadline title properties olp))
            (when tags
              (org-roam-db-query
               [:insert :into tags
                :values $v1]
               (mapcar (lambda (tag)
                         (vector id (substring-no-properties tag)))
                       tags)))
            (org-roam-db-insert-aliases)
            (org-roam-db-insert-refs))))))

  (defun org-roam-id-at-point ()
    "Return the nearest Org Roam node ID at point.
This local override matches upstream Org Roam, but treats leading blank lines as
part of the file-level node so links in those files keep their source ID."
    (org-with-wide-buffer
     (org-back-to-heading-or-point-min t)
     (when (bobp)
       (goto-char (my/org-roam-db--first-content-position)))
     (while (and (not (org-roam-db-node-p))
                 (not (bobp)))
       (org-roam-up-heading-or-point-min)
       (when (bobp)
         (goto-char (my/org-roam-db--first-content-position))))
     (when (org-roam-db-node-p)
       (org-id-get))))

  (my/org-roam-enable)
  (defun my/org-roam-buffer--cancel-redisplay-timer ()
    "Cancel the pending Org Roam side-buffer redisplay timer."
    (when (timerp my/org-roam--redisplay-timer)
      (cancel-timer my/org-roam--redisplay-timer))
    (setq-local my/org-roam--redisplay-timer nil))

  (defun my/org-roam-buffer--cleanup-redisplay ()
    "Remove local debounced Org Roam side-buffer redisplay state."
    (my/org-roam-buffer--cancel-redisplay-timer)
    (remove-hook 'post-command-hook
                 #'my/org-roam-buffer--redisplay-debounced-h t)
    (remove-hook 'kill-buffer-hook
                 #'my/org-roam-buffer--cleanup-redisplay t)
    (remove-hook 'change-major-mode-hook
                 #'my/org-roam-buffer--cleanup-redisplay t)
    (setq-local my/org-roam--redisplay-key nil))

  (defun my/org-roam-buffer--follow-context-p ()
    "Return non-nil when the Org Roam side buffer should follow this buffer."
    (and (derived-mode-p 'org-mode)
         (not (minibufferp))
         (boundp 'org-roam-buffer)
         (get-buffer-window org-roam-buffer 'visible)))

  (defun my/org-roam-buffer--redisplay-now (buffer)
    "Redisplay Org Roam side BUFFER after debounced point motion."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local my/org-roam--redisplay-timer nil)
        (when (my/org-roam-buffer--follow-context-p)
          (org-roam-buffer-persistent-redisplay)))))

  (defun my/org-roam-buffer--redisplay-debounced-h ()
    "Debounced replacement for `org-roam-buffer--redisplay-h'."
    (when (my/org-roam-buffer--follow-context-p)
      (let ((key (list (point)
                       (buffer-chars-modified-tick)
                       (window-start))))
        (unless (equal key my/org-roam--redisplay-key)
          (setq-local my/org-roam--redisplay-key key)
          (my/org-roam-buffer--cancel-redisplay-timer)
          (setq-local my/org-roam--redisplay-timer
                      (run-with-idle-timer
                       my/org-roam-buffer-redisplay-idle-delay nil
                       #'my/org-roam-buffer--redisplay-now
                       (current-buffer)))))))

  (define-advice org-roam-buffer--setup-redisplay-h
      (:override () debounce-redisplay)
    "Install a debounced side-buffer follow hook."
    (add-hook 'post-command-hook
              #'my/org-roam-buffer--redisplay-debounced-h nil t)
    (add-hook 'kill-buffer-hook
              #'my/org-roam-buffer--cleanup-redisplay nil t)
    (add-hook 'change-major-mode-hook
              #'my/org-roam-buffer--cleanup-redisplay nil t))

  (define-advice org-roam-buffer-toggle (:after (&rest _) cleanup-hidden-follow)
    "Drop local follow timers when the Org Roam side buffer is hidden."
    (unless (and (boundp 'org-roam-buffer)
                 (get-buffer-window org-roam-buffer 'visible))
      (my/org-roam-buffer--cleanup-redisplay)))

  (setq org-roam-capture-templates
        `(("m" "Math" plain "%?" :if-new (file+head "math/${slug}.org" ,(my/org-roam-capture-head "math")) :unnarrowed t)
          ("c" "CS" plain "%?" :if-new (file+head "CS/${slug}.org" ,(my/org-roam-capture-head "cs")) :unnarrowed t)
          ("q" "Quantum" plain "%?" :if-new (file+head "QC/${slug}.org" ,(my/org-roam-capture-head "qc")) :unnarrowed t)
          ("p" "Phil" plain "%?" :if-new (file+head "philosophy/${slug}.org" ,(my/org-roam-capture-head "phil")) :unnarrowed t)
          ("i" "Index" plain "%?" :if-new (file+head "index/${slug}.org" ,(my/org-roam-capture-head "index")) :unnarrowed t)
          ("r" "Paper" plain "%?" :if-new (file+head "papers/${slug}.org" ,(my/org-roam-capture-head "paper")) :unnarrowed t))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (or noninteractive
                        my/org-roam--background-timer
                        my/org-roam--initialized)
              (setq my/org-roam--background-timer
                    (run-with-idle-timer my/org-roam-background-init-delay nil
                                         #'my/org-roam-background-init)))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :config
  (defun my/org-roam-ui--json-object-end (string)
    "Return the end position of the first JSON object in STRING."
    (let ((i 0)
          (depth 0)
          (in-string nil)
          (escaped nil)
          start end)
      (while (and (< i (length string)) (not start))
        (when (eq (aref string i) ?{)
          (setq start i))
        (setq i (1+ i)))
      (when start
        (setq i start)
        (while (and (< i (length string)) (not end))
          (let ((char (aref string i)))
            (cond
             (escaped
              (setq escaped nil))
             ((and in-string (eq char ?\\))
              (setq escaped t))
             ((eq char ?\")
              (setq in-string (not in-string)))
             ((not in-string)
              (cond
               ((eq char ?{)
                (setq depth (1+ depth)))
               ((eq char ?})
                (setq depth (1- depth))
                (when (= depth 0)
                  (setq end (1+ i))))))))
          (setq i (1+ i))))
      end))

  (defun my/org-roam-ui--parse-message (text)
    "Parse a websocket JSON message from TEXT, ignoring trailing junk."
    (condition-case nil
        (json-parse-string text :object-type 'alist)
      (json-parse-error
       (when-let* ((end (my/org-roam-ui--json-object-end text)))
         (json-parse-string (substring text 0 end) :object-type 'alist)))))

  (defun org-roam-ui--ws-on-message (_ws frame)
    "Handle websocket FRAME from org-roam-ui without failing on trailing bytes."
    (let* ((text (websocket-frame-text frame))
           (msg (my/org-roam-ui--parse-message text))
           (command (alist-get 'command msg))
           (data (alist-get 'data msg)))
      (cond ((string= command "open")
             (org-roam-ui--on-msg-open-node data))
            ((string= command "delete")
             (org-roam-ui--on-msg-delete-node data))
            ((string= command "create")
             (org-roam-ui--on-msg-create-node data))
            (t
             (message "Ignored malformed org-roam-ui websocket message: %S"
                      text))))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here

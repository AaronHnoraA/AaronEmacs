;;; init-org-roam.el --- Org Roam setup -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-core)
(require 'org-id)

(defcustom my/org-roam-background-init-delay 6
  "Idle delay before Org Roam starts its background services."
  :type 'number
  :group 'my/org-ui)

(defcustom my/org-roam-buffer-redisplay-idle-delay 1.00
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
(defvar org-id-locations)

(defvar my/org-roam-db-file
  (expand-file-name "org-roam.db"
                    (if (boundp 'my/org-state-dir)
                        my/org-state-dir
                      (expand-file-name "var/org/" user-emacs-directory)))
  "Canonical Org Roam database file for this Emacs configuration.")

(defconst my/org-roam-default-overview-head
  "#+begin_overview :toc t :depth 3
- 还没有标题。
#+end_overview
"
  "Default overview block used by Org Roam capture heads.")

(defconst my/org-roam-directory-filetag-alist
  '(("CS" . "cs")
    ("QC" . "qc")
    ("index" . "index")
    ("math" . "math")
    ("papers" . "paper")
    ("philosophy" . "phil"))
  "Default filetag inferred from the first path component under roam.")

(declare-function my/org-reference-create-target-dwim "init-org-utility" ())
(declare-function my/org-insert-id-link "init-org-utility" ())
(declare-function my/org-insert-target-link "init-org-utility" ())
(declare-function org-id-locations-save "org-id" ())
(declare-function org-roam-db-clear-file "org-roam-db" (&optional file))
(declare-function org-roam-db-query "org-roam-db" (sql &rest args))
(declare-function org-roam-db-update-file "org-roam-db" (&optional file-path deprecated))
(declare-function org-roam-list-files "org-roam" ())

(defun my/org-roam-capture-head (tag)
  "Return the default Org Roam capture head with filetag TAG."
  (format "#+title: ${title}
#+date: %%u
#+filetags: :%s:

%s"
          tag
          my/org-roam-default-overview-head))

(defun my/org-roam-db--first-content-position ()
  "Return the first non-blank buffer position for Org Roam file nodes."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r")
    (point)))

(defun my/org-roam--file-head-position ()
  "Return the line-beginning position of the first non-blank file content."
  (save-excursion
    (goto-char (my/org-roam-db--first-content-position))
    (line-beginning-position)))

(defun my/org-roam--file-property-drawer-bounds ()
  "Return bounds of the file-level property drawer, or nil."
  (save-excursion
    (goto-char (my/org-roam--file-head-position))
    (when (looking-at-p "[ \t]*:PROPERTIES:[ \t]*$")
      (let ((begin (line-beginning-position)))
        (unless (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
          (user-error "Malformed file-level property drawer"))
        (cons begin (line-end-position))))))

(defun my/org-roam--file-property (property)
  "Return file-level PROPERTY value, or nil."
  (when-let* ((bounds (my/org-roam--file-property-drawer-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward
             (format "^[ \t]*:%s:[ \t]*\\(.+\\)$" (regexp-quote property))
             (cdr bounds)
             t)
        (string-trim (match-string-no-properties 1))))))

(defun my/org-roam--file-property-line-bounds (property)
  "Return line bounds for file-level PROPERTY, or nil."
  (when-let* ((bounds (my/org-roam--file-property-drawer-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward
             (format "^[ \t]*:%s:[ \t]*.*$" (regexp-quote property))
             (cdr bounds)
             t)
        (cons (line-beginning-position)
              (save-excursion
                (forward-line 1)
                (point)))))))

(defun my/org-roam--file-property-drawer-empty-p ()
  "Return non-nil when the file-level property drawer has no properties."
  (when-let* ((bounds (my/org-roam--file-property-drawer-bounds)))
    (let ((end-line (save-excursion
                      (goto-char (cdr bounds))
                      (line-beginning-position))))
      (save-excursion
        (goto-char (car bounds))
        (forward-line 1)
        (catch 'nonempty
          (while (< (point) end-line)
            (unless (looking-at-p "[ \t]*$")
              (throw 'nonempty nil))
            (forward-line 1))
          t)))))

(defun my/org-roam--delete-file-property (property)
  "Delete file-level PROPERTY from the current Org buffer.
Return non-nil when the buffer changed."
  (when-let* ((line-bounds (my/org-roam--file-property-line-bounds property)))
    (delete-region (car line-bounds) (cdr line-bounds))
    (when (my/org-roam--file-property-drawer-empty-p)
      (when-let* ((drawer-bounds (my/org-roam--file-property-drawer-bounds)))
        (delete-region
         (car drawer-bounds)
         (save-excursion
           (goto-char (cdr drawer-bounds))
           (forward-line 1)
           (point)))))
    t))

(defun my/org-roam--property-drawer-end-position ()
  "Return the insertion point just after the file-level property drawer."
  (if-let* ((bounds (my/org-roam--file-property-drawer-bounds)))
      (save-excursion
        (goto-char (cdr bounds))
        (forward-line 1)
        (point))
    (my/org-roam--file-head-position)))

(defun my/org-roam--ensure-file-id (file)
  "Ensure current Org buffer has a file-level ID for FILE.
Return (ID . CHANGED)."
  (if-let* ((id (my/org-roam--file-property "ID")))
      (cons id nil)
    (let ((id (org-id-new)))
      (if-let* ((bounds (my/org-roam--file-property-drawer-bounds)))
          (save-excursion
            (goto-char (car bounds))
            (if (re-search-forward "^[ \t]*:ID:[ \t]*.*$" (cdr bounds) t)
                (replace-match (concat ":ID:       " id) t t)
              (goto-char (my/org-roam--file-head-position))
              (forward-line 1)
              (insert ":ID:       " id "\n")))
        (save-excursion
          (goto-char (my/org-roam--file-head-position))
          (insert ":PROPERTIES:\n:ID:       " id "\n:END:\n")))
      (org-id-add-location id file)
      (cons id t))))

(defun my/org-roam--ensure-leading-blank-line ()
  "Ensure Org Roam heads start after a leading blank line.
Return non-nil when the buffer changed."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p "[ \t]*\n")
      (insert "\n")
      t)))

(defun my/org-roam--keyword-present-p (keyword)
  "Return non-nil if current Org buffer has #+KEYWORD."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward
       (format "^[ \t]*#\\+%s:" (regexp-quote keyword))
       nil
       t))))

(defun my/org-roam--keyword-after-position (keyword)
  "Return the position after the first #+KEYWORD line, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^[ \t]*#\\+%s:.*$" (regexp-quote keyword))
             nil
             t)
        (forward-line 1)
        (point)))))

(defun my/org-roam--title-from-file (file)
  "Derive a readable Org Roam title from FILE."
  (let* ((base (file-name-base file))
         (spaced (replace-regexp-in-string "[-_]+" " " base)))
    (string-join
     (mapcar (lambda (word)
               (if (string= word (upcase word))
                   word
                 (capitalize word)))
             (split-string spaced " +" t))
     " ")))

(defun my/org-roam--sanitize-filetag (tag)
  "Return TAG in a conservative Org filetag form."
  (let ((tag (downcase (or tag ""))))
    (replace-regexp-in-string
     "_+" "_"
     (replace-regexp-in-string "[^[:alnum:]_@#%]+" "_" tag))))

(defun my/org-roam--filetag-from-file (file)
  "Infer the default Org Roam filetag for FILE."
  (let* ((relative (file-relative-name
                    (file-truename file)
                    (file-name-as-directory (file-truename org-roam-directory))))
         (directory (file-name-directory relative))
         (component (and directory
                         (car (split-string (directory-file-name directory)
                                            "/" t))))
         (tag (or (cdr (assoc component my/org-roam-directory-filetag-alist))
                  component
                  "note")))
    (my/org-roam--sanitize-filetag tag)))

(defun my/org-roam--insert-head-keyword (keyword value position)
  "Insert #+KEYWORD with VALUE at POSITION."
  (save-excursion
    (goto-char position)
    (insert "#+" keyword ": " value "\n")))

(defun my/org-roam--head-keyword-end-position ()
  "Return the insertion point after the known Org Roam file head keywords."
  (or (my/org-roam--keyword-after-position "filetags")
      (my/org-roam--keyword-after-position "date")
      (my/org-roam--keyword-after-position "title")
      (my/org-roam--property-drawer-end-position)))

(defun my/org-roam--overview-present-p ()
  "Return non-nil when the buffer already has an overview block."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward "^[ \t]*#\\+begin_overview\\b" nil t))))

(defun my/org-roam--ensure-file-head (file)
  "Ensure current Org buffer has the standard file-level Org Roam head for FILE.
Return a plist with :id and :changed."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((leading-changed (my/org-roam--ensure-leading-blank-line))
             (id-result (my/org-roam--ensure-file-id file))
             (id (car id-result))
             (changed (or leading-changed (cdr id-result))))
        (unless (my/org-roam--keyword-present-p "title")
          (my/org-roam--insert-head-keyword
           "title"
           (my/org-roam--title-from-file file)
           (my/org-roam--property-drawer-end-position))
          (setq changed t))
        (unless (my/org-roam--keyword-present-p "date")
          (my/org-roam--insert-head-keyword
           "date"
           (format-time-string "[%Y-%m-%d %a]")
           (or (my/org-roam--keyword-after-position "title")
               (my/org-roam--property-drawer-end-position)))
          (setq changed t))
        (unless (my/org-roam--keyword-present-p "filetags")
          (my/org-roam--insert-head-keyword
           "filetags"
           (format ":%s:" (my/org-roam--filetag-from-file file))
           (or (my/org-roam--keyword-after-position "date")
               (my/org-roam--keyword-after-position "title")
               (my/org-roam--property-drawer-end-position)))
          (setq changed t))
        (unless (my/org-roam--overview-present-p)
          (save-excursion
            (goto-char (my/org-roam--head-keyword-end-position))
            (insert "\n" my/org-roam-default-overview-head))
          (setq changed t))
        (list :id id :changed changed)))))

(defun my/org-roam--current-org-file ()
  "Return the truename of the current Org buffer file."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (unless file
      (user-error "Current Org buffer is not visiting a file"))
    (file-truename file)))

(defun my/org-roam--file-in-roam-directory-p (file)
  "Return non-nil if FILE is under `org-roam-directory'."
  (and (boundp 'org-roam-directory)
       (stringp org-roam-directory)
       (file-in-directory-p file (file-name-as-directory
                                  (file-truename org-roam-directory)))))

(defun my/org-roam--listed-file-p (file)
  "Return non-nil if FILE is included by `org-roam-list-files'."
  (catch 'found
    (dolist (candidate (org-roam-list-files))
      (when (file-equal-p file candidate)
        (throw 'found t)))))

(defun my/org-roam--file-node-registered-p (file id)
  "Return non-nil if FILE has file-level node ID in Org Roam DB."
  (org-roam-db-query
   [:select id :from nodes
    :where (and (= file $s1) (= id $s2))]
   file
   id))

(defun my/org-roam--remove-id-location (id)
  "Remove ID from `org-id-locations' when it is cached."
  (when (and id
             (boundp 'org-id-locations)
             (hash-table-p org-id-locations))
    (remhash id org-id-locations)
    (when (fboundp 'org-id-locations-save)
      (org-id-locations-save))))

(defun my/org-roam-register-current-file ()
  "Register the current Org file as a file-level Org Roam node.
The file must already live under `org-roam-directory' and be included by
`org-roam-list-files'.  This command ensures the standard roam head
(`ID', title, date, filetags and overview), saves the file, and updates the
Org Roam database for that file."
  (interactive)
  (let ((file (my/org-roam--current-org-file)))
    (require 'org-roam)
    (my/org-roam-enable)
    (unless (my/org-roam--file-in-roam-directory-p file)
      (user-error "Current file is outside org-roam-directory: %s" file))
    (let* ((head (my/org-roam--ensure-file-head file))
           (id (plist-get head :id))
           (changed (plist-get head :changed)))
      (when (or changed (buffer-modified-p))
        (save-buffer))
      (unless (my/org-roam--listed-file-p file)
        (user-error "Current file is not included by org-roam-list-files: %s" file))
      (let ((registered-before (my/org-roam--file-node-registered-p file id)))
        (unless registered-before
          (org-roam-db-clear-file file))
        (org-roam-db-update-file file)
        (unless (my/org-roam--file-node-registered-p file id)
          (user-error "Failed to register Org Roam node %s for %s" id file))
        (org-id-add-location id file)
        (message "Org Roam file node %s: %s"
                 (if registered-before "updated" "registered")
                 id)
        id))))

(defun my/org-roam-unregister-current-file ()
  "Remove the current file-level Org Roam ID and sync the Org Roam DB.
The Org file stays under `org-roam-directory', but without the file-level ID it
does not become a file node when `org-roam-db-sync' runs."
  (interactive)
  (let ((file (my/org-roam--current-org-file)))
    (require 'org-roam)
    (my/org-roam-enable)
    (unless (my/org-roam--file-in-roam-directory-p file)
      (user-error "Current file is outside org-roam-directory: %s" file))
    (let ((id (my/org-roam--file-property "ID")))
      (unless id
        (user-error "Current file has no file-level Org Roam ID"))
      (unless (my/org-roam--delete-file-property "ID")
        (user-error "Failed to delete file-level Org Roam ID"))
      (save-buffer)
      (org-roam-db-clear-file file)
      (org-roam-db-update-file file)
      (my/org-roam--remove-id-location id)
      (message "Org Roam file node unregistered: %s" id)
      id)))

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

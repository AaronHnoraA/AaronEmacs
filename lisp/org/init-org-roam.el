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
  "Initialize org-roam once using the newest available entry point."
  (my/org-roam--cancel-background-timer)
  (unless my/org-roam--initialized
    (cond
     ((fboundp 'org-roam-db-autosync-enable)
      (org-roam-db-autosync-enable))
     (t
      (when (fboundp 'org-roam-setup)
        (org-roam-setup))
      (org-roam-db-autosync-mode 1)))
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
  (org-roam-ui-update-on-save t))

(provide 'init-org-roam)
;;; init-org-roam.el ends here

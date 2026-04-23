;;; init-org-roam.el --- Org Roam setup -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-core)

(defcustom my/org-roam-background-init-delay 2
  "Idle delay before Org Roam starts its background services."
  :type 'number
  :group 'my/org-ui)

(defvar my/org-roam--background-timer nil)
(defvar my/org-roam--initialized nil)

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
        ("o" . org-id-get-create)
        ("l" . org-roam-buffer-toggle))
  
  :config
  (my/org-roam-enable)
  (setq org-roam-capture-templates
        '(("m" "Math" plain "%?" :if-new (file+head "math/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :math:\n") :unnarrowed t)
          ("c" "CS" plain "%?" :if-new (file+head "CS/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :cs:\n") :unnarrowed t)
          ("q" "Quantum" plain "%?" :if-new (file+head "QC/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :qc:\n") :unnarrowed t)
          ("p" "Phil" plain "%?" :if-new (file+head "philosophy/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :phil:\n") :unnarrowed t)
          ("i" "Index" plain "%?" :if-new (file+head "index/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :index:\n") :unnarrowed t)
          ("r" "Paper" plain "%?" :if-new (file+head "papers/${slug}.org" "#+title: ${title}\n#+date: %u\n#+filetags: :paper:\n") :unnarrowed t))))

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

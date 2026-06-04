;;; init-org-capture.el --- Org capture templates -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-core)

(defcustom my/org-last-modified-search-limit (* 16 1024)
  "Maximum characters searched for `#+last_modified` before saving."
  :type 'integer
  :group 'org)

(defvar my-daily-subdirs '("idea" "inbox" "mail" "note" "meeting" "protocol" "uni" "life"))
(dolist (dir my-daily-subdirs)
  (make-directory (expand-file-name dir my-org-daily-dir) t))

(defun my/get-daily-capture-path (subdir)
  "Prompt for filename, append date, return path."
  (let* ((name (read-string "File Name (slug): "))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (downcase name)))
         (date (format-time-string "%Y%m%d"))
         (fname (format "%s-%s.org" slug date)))
    (expand-file-name fname (expand-file-name subdir my-org-daily-dir))))

(setq org-capture-use-agenda-date t)
(setq org-capture-templates
      '(("i" "Idea" plain (file (lambda () (my/get-daily-capture-path "idea")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :idea:\n\n* Idea:\n%?\n" :unnarrowed t)
        ("b" "Inbox" plain (file (lambda () (my/get-daily-capture-path "inbox")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :inbox:\n\n* Details\n%?\n" :unnarrowed t)
        ("m" "Mail" plain (file (lambda () (my/get-daily-capture-path "mail")))
         "#+title: Mail: %^{Subject}\n#+date: %u\n#+filetags: :mail:\n\n* To/From: %^{Recipient}\n* Status: TODO\n\n%?\n" :unnarrowed t)
        ("n" "Note" plain (file (lambda () (my/get-daily-capture-path "note")))
         "#+title: %^{Title}\n#+date: %u\n#+filetags: :note:\n\n%?\n" :unnarrowed t)
        ("t" "Meeting" plain (file (lambda () (my/get-daily-capture-path "meeting")))
         "#+title: Meeting: %^{Topic}\n#+date: %u\n#+filetags: :meeting:\n\n* Who: %^{Who}\n* Time: %^T\n\n* Agenda\n%?\n" :unnarrowed t)
        ("u" "Uni Task" plain (file (lambda () (my/get-daily-capture-path "uni")))
         "#+title: %^{Task}\n#+date: %u\n#+filetags: :uni:\n\n* Course: %^{Code}\n* Deadline: %^t\n\n* Req\n%?\n" :unnarrowed t)
        ("l" "Life Task" plain (file (lambda () (my/get-daily-capture-path "life")))
         "#+title: %^{Task}\n#+date: %u\n#+filetags: :life:\n\n* Type: %^{Type}\n%?\n" :unnarrowed t)))

(defun pv/org-set-last-modified ()
  "Update the `#+last_modified` field before saving an Org buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "^#\\+last_modified:"
             (and (integerp my/org-last-modified-search-limit)
                  (> my/org-last-modified-search-limit 0)
                  (min (point-max)
                       (+ (point-min) my/org-last-modified-search-limit)))
             t)
        (delete-region (point) (line-end-position))
        (insert (format " [%s]" (format-time-string "%Y-%m-%d %a %H:%M")))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'pv/org-set-last-modified nil t)))

(provide 'init-org-capture)
;;; init-org-capture.el ends here

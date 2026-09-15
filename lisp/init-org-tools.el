;;; init-org-tools.el --- Small Org editing tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing helpers only.  No agenda or capture surface is introduced here.

;;; Code:

(require 'init-funcs)

(declare-function org-cliplink "org-cliplink" ())
(declare-function org-element-context "org-element" ())
(declare-function org-element-type "org-element" (element))
(declare-function org-insert-link "ol" (&optional complete-file link description))
(declare-function org-link-make-string "ol" (link &optional description))

(defun my/org--clipboard-url ()
  "Return the newest kill as an HTTP URL, or nil."
  (when-let* ((text (ignore-errors (current-kill 0 t)))
              ((string-match-p "\\`https?://" text)))
    (substring-no-properties text)))

(defun my/org--at-link-p ()
  "Return non-nil when point is on an Org link."
  (when-let* ((context (ignore-errors (org-element-context))))
    (eq (org-element-type context) 'link)))

(defun my/org-insert-link-dwim ()
  "Insert or edit an Org link using the URL in the clipboard when useful."
  (interactive)
  (require 'org)
  (let ((url (my/org--clipboard-url)))
    (cond
     ((my/org--at-link-p)
      (call-interactively #'org-insert-link))
     ((and url (use-region-p))
      (let ((description (buffer-substring-no-properties
                          (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (org-link-make-string url description))))
     (url
      (require 'org-cliplink)
      (org-cliplink))
     (t
      (call-interactively #'org-insert-link)))))

(use-package org-cliplink
  :ensure t
  :commands org-cliplink)

(with-eval-after-load 'org
  (my/local-leader!
    :keymaps 'org-mode-map
    "l" 'my/org-insert-link-dwim))

(provide 'init-org-tools)
;;; init-org-tools.el ends here

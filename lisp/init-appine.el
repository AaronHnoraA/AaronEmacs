;;; init-appine.el --- Appine integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Native macOS browser / document embedding via Appine.
;; The package itself stays lazy; the first interactive command will load it
;; and Appine will then prompt to download or compile its dynamic module.

;;; Code:

(my/package-ensure-vc 'appine "https://github.com/chaoswork/appine.git")

(defconst my/appine-buffer-name "*Appine Window*"
  "Buffer name used by Appine.")

(defvar my/appine-last-url nil
  "Most recent URL opened through the local Appine wrapper.")

(defun my/appine-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is the Appine host buffer."
  (eq (get-buffer (or buffer (current-buffer)))
      (get-buffer my/appine-buffer-name)))

(defun my/appine-visible-p ()
  "Return non-nil when the Appine buffer is visible."
  (and (get-buffer my/appine-buffer-name)
       (get-buffer-window my/appine-buffer-name 'visible)))

(defun my/appine--normalize-url (url)
  "Normalize URL before handing it to Appine."
  (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
      url
    (concat "https://" url)))

(defun my/appine-open-url (url)
  "Open URL in Appine and remember it for backend switching."
  (interactive "sURL: ")
  (setq my/appine-last-url (my/appine--normalize-url url))
  (appine-open-url my/appine-last-url))

(defun my/appine-open-at-point ()
  "Open the URL at point in Appine."
  (interactive)
  (let ((url (or (thing-at-point 'url t)
                 (read-string "URL: "))))
    (unless (and url (not (string-empty-p url)))
      (user-error "当前位置没有可用 URL"))
    (my/appine-open-url url)))

(defun my/appine-open-file (path)
  "Open PATH in Appine and remember the resulting file URL."
  (interactive "fFile: ")
  (setq my/appine-last-url (concat "file://" (expand-file-name path)))
  (appine-open-file path))

(defun my/appine-get-url ()
  "Return the remembered Appine URL when the Appine view is active or visible."
  (when (or (my/appine-buffer-p) (my/appine-visible-p))
    my/appine-last-url))

(defun my/appine-kill-all ()
  "Kill the Appine view and clear remembered URL state."
  (interactive)
  (let ((count (if (get-buffer my/appine-buffer-name) 1 0)))
    (setq my/appine-last-url nil)
    (when (fboundp 'appine-kill)
      (appine-kill))
    (message "已清理 %d 个 Appine Buffer/Window。" count)))

(defun my/appine-restart ()
  "Restart Appine after clearing old native views."
  (interactive)
  (my/appine-kill-all)
  (require 'appine nil t)
  (message "Appine restarted."))

(defun my/appine-back ()
  "Navigate Appine backward."
  (interactive)
  (call-interactively #'appine-web-go-back))

(defun my/appine-forward ()
  "Navigate Appine forward."
  (interactive)
  (call-interactively #'appine-web-go-forward))

(defun my/appine-reload ()
  "Reload the current Appine page."
  (interactive)
  (call-interactively #'appine-web-reload))

(defun my/appine-next-tab ()
  "Switch to the next Appine tab."
  (interactive)
  (call-interactively #'appine-next-tab))

(defun my/appine-prev-tab ()
  "Switch to the previous Appine tab."
  (interactive)
  (call-interactively #'appine-prev-tab))

(defun my/appine-close-tab ()
  "Close the current Appine tab."
  (interactive)
  (call-interactively #'appine-close-tab))

(use-package appine
  :ensure nil
  :commands (appine
             appine-open-url
             appine-open-file
             appine-close
             appine-kill
             appine-toggle-open-in-org-mode
             appine-close-tab
             appine-next-tab
             appine-prev-tab
             appine-web-go-back
             appine-web-go-forward
             appine-web-reload))

(with-eval-after-load 'appine
  (when (boundp 'appine-active-map)
    (define-key appine-active-map (kbd "H") #'my/appine-back)
    (define-key appine-active-map (kbd "L") #'my/appine-forward)
    (define-key appine-active-map (kbd "g") #'my/appine-reload)
    (define-key appine-active-map (kbd "[") #'my/appine-prev-tab)
    (define-key appine-active-map (kbd "]") #'my/appine-next-tab)
    (define-key appine-active-map (kbd "d") #'my/appine-close-tab)
    (define-key appine-active-map (kbd "W") #'my/appine-to-eww)
    (define-key appine-active-map (kbd "X") #'my/appine-to-xwidget)
    (define-key appine-active-map (kbd "Q") #'my/appine-kill-all)))

(provide 'init-appine)
;;; init-appine.el ends here

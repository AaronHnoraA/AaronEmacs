;;; init-previewer.el --- Realtime text preview -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(let ((previewer-dir (locate-user-emacs-file "site-lisp/previewer")))
  (when (file-directory-p previewer-dir)
    (add-to-list 'load-path previewer-dir)))

(defcustom my/previewer-auto-open t
  "Automatically open the Previewer workbench for supported buffers."
  :type 'boolean
  :group 'previewer)

(defvaralias 'my/previewer-auto-open-org 'my/previewer-auto-open)

(defconst my/previewer-auto-open-modes
  '(org-mode markdown-mode markdown-ts-mode html-mode html-ts-mode
             mhtml-mode web-mode vue-html-mode)
  "Major modes that automatically open the Previewer workbench.")

(defun my/previewer-workbench-maybe ()
  "Open Previewer automatically for supported buffers when configured."
  (when (and my/previewer-auto-open
             (member major-mode my/previewer-auto-open-modes)
             (display-graphic-p))
    (run-at-time 0.2 nil
                 (lambda (buffer)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (member major-mode my/previewer-auto-open-modes)
                         (ignore-errors
                           (previewer-workbench))))))
                 (current-buffer))))

(dolist (hook '(org-mode-hook
                markdown-mode-hook
                markdown-ts-mode-hook
                html-mode-hook
                html-ts-mode-hook
                mhtml-mode-hook
                web-mode-hook
                vue-html-mode-hook))
  (add-hook hook #'my/previewer-workbench-maybe))

(use-package previewer
  :ensure nil
  :commands (previewer-mode
             previewer-cleanup
             previewer-workbench
             previewer-org-workbench
             previewer-update-vendor-assets
             my/previewer-org-workbench)
  :custom
  (previewer-browser-backend 'xwidget)
  (previewer-window-side 'auto)
  (previewer-window-size 0.42)
  (previewer-delay 0.8)
  (previewer-sync-scroll-from-browser nil)
  (previewer-render-modes
   '(org-mode markdown-mode markdown-ts-mode html-mode html-ts-mode mhtml-mode web-mode vue-html-mode))
  (previewer-render-alist
   '((org-mode . previewer-org-html-content)
     (web-mode . previewer-raw-content)
     (html-mode . previewer-raw-content)
     (html-ts-mode . previewer-raw-content)
     (mhtml-mode . previewer-raw-content)
     (vue-html-mode . previewer-raw-content)
     (t . previewer-markdown-content)))
  :config
  (defalias 'my/previewer-org-workbench #'previewer-org-workbench))

(provide 'init-previewer)
;;; init-previewer.el ends here

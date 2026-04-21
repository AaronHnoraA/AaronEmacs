;;; init-mouse.el --- Mouse-triggered shortcut layer -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reserve `H-C-M-' combinations for mouse software that can only emit simple
;; shortcuts. These bindings are intentionally kept out of the regular keyboard
;; workflow.

;;; Code:

(autoload 'ibuffer-jump "ibuffer" nil t)
(autoload 'ace-window "ace-window" nil t)
(autoload 'ibuffer-visit-buffer "ibuffer" nil t)
(autoload 'ibuffer-visit-buffer-other-window "ibuffer" nil t)
(autoload 'ibuffer "ibuffer" nil t)
(autoload 'project-find-regexp "project" nil t)
(autoload 'recentf-open-files "recentf" nil t)

(declare-function my/kill-buffer-dwim "init-windows" ())
(declare-function my/navigation-back "init-navigation" ())
(declare-function my/navigation-find-definition "init-navigation" ())
(declare-function my/project-current-root "init-project" ())
(declare-function my/project-dispatch "init-project" ())
(declare-function my/project-open-root "init-project" (project-root))

(defun my/ibuffer-visit-buffer-mouse (event)
  "Visit the buffer at the clicked ibuffer row for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ibuffer-visit-buffer))

(defun my/ibuffer-visit-buffer-other-window-mouse (event)
  "Visit the buffer at the clicked ibuffer row in another window for EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ibuffer-visit-buffer-other-window))

(defun my/mouse-recent-files ()
  "Open the built-in clickable recent-files dialog."
  (interactive)
  (recentf-mode 1)
  (recentf-open-files))

(defun my/mouse-project-root ()
  "Open the current project root in a clickable directory view."
  (interactive)
  (if-let* ((project-root (and (fboundp 'my/project-current-root)
                               (my/project-current-root))))
      (my/project-open-root project-root)
    (call-interactively #'my/project-dispatch)))

(defun my/mouse-project-search (regexp)
  "Search the current project for REGEXP and show clickable results."
  (interactive
   (list
    (read-regexp
     "Project regexp: "
     (thing-at-point 'symbol t))))
  (project-find-regexp regexp))

(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-name-map "<mouse-1>" #'my/ibuffer-visit-buffer-mouse)
  (keymap-set ibuffer-name-map "<mouse-2>" #'my/ibuffer-visit-buffer-other-window-mouse)
  (keymap-set ibuffer-mode-map "<mouse-1>" #'my/ibuffer-visit-buffer-mouse)
  (keymap-set ibuffer-mode-map "<mouse-2>" #'my/ibuffer-visit-buffer-other-window-mouse))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix my/mouse-dispatch ()
    "Mouse-friendly command board. Every suffix can be triggered by click."
    [["Buffers"
      ("a" "ibuffer jump" ibuffer-jump :transient transient--do-exit)
      ("b" "buffer list" ibuffer :transient transient--do-exit)
      ("k" "kill buffer" my/kill-buffer-dwim :transient transient--do-exit)
      ("r" "recent files" my/mouse-recent-files :transient transient--do-exit)]
     ["Project / Search"
      ("f" "project root" my/mouse-project-root :transient transient--do-exit)
      ("g" "project grep" my/mouse-project-search :transient transient--do-exit)
      ("p" "project board" my/project-dispatch :transient transient--do-exit)]
     ["Jump / Window"
      ("d" "definition" my/navigation-find-definition :transient transient--do-exit)
      ("z" "jump back" my/navigation-back :transient transient--do-exit)
      ("w" "ace window" ace-window :transient transient--do-exit)]]))

;; Mouse-friendly direct actions. Logitech MX Master style buttons work best
;; when one press maps to one immediate command.
(global-set-key (kbd "H-C-M-a") #'ibuffer-jump)
(global-set-key (kbd "H-C-M-b") #'ibuffer)
(global-set-key (kbd "H-C-M-d") #'my/navigation-find-definition)
(global-set-key (kbd "H-C-M-f") #'my/mouse-project-root)
(global-set-key (kbd "H-C-M-g") #'my/mouse-project-search)
(global-set-key (kbd "H-C-M-k") #'my/kill-buffer-dwim)
(global-set-key (kbd "H-C-M-m") #'my/mouse-dispatch)
(global-set-key (kbd "H-C-M-p") #'my/project-dispatch)
(global-set-key (kbd "H-C-M-r") #'my/mouse-recent-files)
(global-set-key (kbd "H-C-M-w") #'ace-window)
(global-set-key (kbd "H-C-M-z") #'my/navigation-back)

(provide 'init-mouse)
;;; init-mouse.el ends here

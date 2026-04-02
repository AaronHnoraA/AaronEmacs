;;; init-macos.el --- Tweaks for MacOS -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function consult-buffer "consult" ())
(declare-function dape "dape" (config &optional skip-compile))
(declare-function comment-or-uncomment "init-base" ())
(declare-function duplicate-line-or-region-above "duplicate-line" (&optional reverse))
(declare-function duplicate-line-or-region-below "duplicate-line" ())
(declare-function mc/edit-lines "multiple-cursors" ())
(declare-function mc/mark-all-like-this-dwim "multiple-cursors" ())
(declare-function mc/mark-next-like-this "multiple-cursors" ())
(declare-function mc/mark-previous-like-this "multiple-cursors" ())
(declare-function move-text-down "move-text" (arg))
(declare-function move-text-up "move-text" (arg))
(declare-function my/bookmark-jump-dwim "init-windows" ())
(declare-function my/code-actions-dispatch "init-code-actions" ())
(declare-function my/contract-region "init-expand-region" ())
(declare-function my/diagnostics-dispatch "init-diagnostics-extra" ())
(declare-function my/expand-region "init-expand-region" ())
(declare-function my/gptel-chat "init-gpt" (&optional skip-preset))
(declare-function my/language-server-dispatch "init-lsp-tools" ())
(declare-function magit-status "magit-status" (&optional directory))
(declare-function open-newline-above "open-newline" (arg))
(declare-function open-newline-below "open-newline" (arg))
(declare-function org-agenda "org" (&optional arg keys restriction))
(declare-function popper-toggle "popper" ())
(declare-function my/project-dispatch "init-project" ())
(declare-function my/search-line-forward "init-search" ())
(declare-function my/search-open-recent-file "init-search" ())
(declare-function telescope "init-telescope" ())
(declare-function my/telescope-ripgrep "init-telescope" ())
(declare-function my/test-nearest "init-test" ())
(declare-function show-imenu "init-project" ())

(use-package emacs
  :ensure nil
  :config
  ;; `Option' is mapped to Hyper on macOS. Keep the common clipboard actions
  ;; and use the remaining Option keys for high-frequency global entry points.
  (dolist (binding '(("H-x" . clipboard-kill-region)
                     ("H-c" . clipboard-kill-ring-save)
                     ("H-v" . clipboard-yank)
                     ("H-w" . my/delete-frame-dwim)
                     ("H-f" . find-file)
                     ("H-b" . consult-buffer)
                     ("H-r" . my/search-open-recent-file)
                     ("H-s" . my/search-line-forward)
                     ("H-g" . my/telescope-ripgrep)
                     ("H-p" . my/project-dispatch)
                     ("H-t" . telescope)
                     ("H-m" . magit-status)
                     ("H-a" . org-agenda)
                     ("H-l" . my/gptel-chat)
                     ("H-e" . my/code-actions-dispatch)
                     ("H-d" . my/diagnostics-dispatch)
                     ("H-i" . show-imenu)
                     ("H-u" . my/language-server-dispatch)
                     ("H-j" . dape)
                     ("H-n" . my/test-nearest)
                     ("H-y" . my/bookmark-jump-dwim)
                     ("H-o" . open-newline-below)
                     ("H-O" . open-newline-above)
                     ("H-k" . duplicate-line-or-region-below)
                     ("H-K" . duplicate-line-or-region-above)
                     ("H-;" . comment-or-uncomment)
                     ("H-'" . mc/edit-lines)
                     ("H-[" . mc/mark-previous-like-this)
                     ("H-]" . mc/mark-next-like-this)
                     ("H-/" . mc/mark-all-like-this-dwim)
                     ("H--" . my/contract-region)
                     ("H-=" . my/expand-region)
                     ("H-," . move-text-up)
                     ("H-." . move-text-down)
                     ("H-`" . popper-toggle)))
    (global-set-key (kbd (car binding)) (cdr binding)))

  ;; Make titlebar dark
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  ;; Useful when use an external keyboard
  (defun +mac-swap-option-and-command ()
    "Swap `mac-option-modifier' and `mac-command-modifier'."
    (interactive)
    (cl-rotatef mac-option-modifier mac-command-modifier)
    (message "mac-option-modifier: %s, mac-command-modifier: %s" mac-option-modifier mac-command-modifier))

  ;; Emoji support
  (let ((fonts '("Apple Color Emoji")))
    (cl-loop with script = 'emoji
             for font in fonts
             when (member font (font-family-list))
             return (set-fontset-font t script (font-spec :family font) nil 'prepend)))

  ;; Better variable-pitch font
  (let ((fonts '("Merriweather" "Bookerly" "Overpass" "Verdana" "Lucida Grande")))
    (cl-loop for font in fonts
             when (member font (font-family-list))
             return (custom-set-faces `(variable-pitch ((t (:family ,font)))))))
  :custom
  (mac-option-modifier 'hyper)
  (mac-command-modifier 'meta)
  (delete-by-moving-to-trash t)
  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (ns-use-native-fullscreen nil)
  ;(ns-use-native-fullscreen t)
  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (ns-pop-up-frames nil))


;; 窗口系统启动后，自动执行一次全屏切换（效果等同于按 F11）
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)


(provide 'init-macos)
;;; init-macos.el ends here

;;; editor/navigation/config.el --- Unified code navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)

(declare-function +language-server/find-implementation "tools/lsp/config")
(declare-function +language-server/find-type-definition "tools/lsp/config")
(declare-function better-jumper-jump-backward "better-jumper" (&optional count))
(declare-function better-jumper-jump-forward "better-jumper" (&optional count))
(declare-function better-jumper-set-jump "better-jumper" (&optional pos))
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function evil-jump-backward "evil-commands" (&optional count))
(declare-function evil-jump-forward "evil-commands" (&optional count))
(declare-function evil-set-jump "evil-jumps")
(declare-function evil-collection-consult-jump-list "evil-collection-consult")
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-parent "treesit" (node))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-type "treesit" (node))
(declare-function xref-find-definitions "xref" (identifier))
(declare-function xref-find-references "xref" (identifier))
(declare-function xref-go-back "xref" ())
(declare-function xref-go-forward "xref" ())
(declare-function citre-jump "citre")
(declare-function citre-jump-to-reference "citre")
(declare-function citre-peek "citre" (&optional buf point reference))
(declare-function citre-peek-reference "citre")

(defvar better-jumper-post-jump-hook)

(defconst +navigation-structural-node-regexp
  (rx (or "function"
          "method"
          "lambda"
          "class"
          "interface"
          "struct"
          "enum"
          "trait"
          "impl"
          "namespace"
          "module"
          "block"
          "body"
          "suite"
          "statement"))
  "Tree-sitter node fragments treated as structural navigation anchors.")

(defconst +navigation-pulse-excluded-modes
  '(so-long-mode special-mode comint-mode term-mode portal-terminal-mode)
  "Major modes where jump pulses are suppressed.")

(use-package better-jumper
  :ensure t
  :defer 1
  :init
  (global-set-key [remap evil-jump-forward] #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  :config
  (better-jumper-mode 1)

  (defun +navigation-set-jump-a (fn &rest args)
    "Record a jump before calling FN with ARGS."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun +navigation-set-jump-h ()
    "Record a jump before killing a visible buffer."
    (when (get-buffer-window)
      (better-jumper-set-jump))
    nil)

  (add-hook 'kill-buffer-hook #'+navigation-set-jump-h)
  (advice-add #'imenu :around #'+navigation-set-jump-a)
  (advice-add #'outline-up-heading :around #'+navigation-set-jump-a))

(use-package pulse
  :ensure nil
  :defer 1
  :config
  (defun +navigation-pulse-line-h (&rest _)
    "Momentarily highlight the current line after a jump."
    (when (and (display-graphic-p)
               (not (apply #'derived-mode-p +navigation-pulse-excluded-modes)))
      (pulse-momentary-highlight-one-line (point))))

  (defun +navigation-pulse-line-delayed-h (&rest _)
    "Pulse the current line after a short delay."
    (run-at-time 0.08 nil #'+navigation-pulse-line-h))

  (dolist (hook '(imenu-after-jump-hook
                  consult-after-jump-hook
                  better-jumper-post-jump-hook
                  org-follow-link-hook))
    (add-hook hook #'+navigation-pulse-line-h))

  (with-eval-after-load 'perspective
    (add-hook 'persp-activated-functions #'+navigation-pulse-line-delayed-h))

  (advice-add #'save-place-find-file-hook :after #'+navigation-pulse-line-h))

(defun +navigation--push-jump ()
  "Record the current position in Evil's jump list when available."
  (when (or (fboundp 'better-jumper-set-jump)
            (fboundp 'evil-set-jump))
    (ignore-errors
      (if (fboundp 'better-jumper-set-jump)
          (better-jumper-set-jump)
        (evil-set-jump)))))

(defun +navigation--call-primary-or-fallback (primary &optional fallback)
  "Call PRIMARY interactively, with optional FALLBACK on lookup errors."
  (+navigation--push-jump)
  (condition-case err
      (if (fboundp primary)
          (call-interactively primary)
        (signal 'void-function (list primary)))
    ((user-error error)
     (if (and fallback (fboundp fallback))
         (call-interactively fallback)
       (signal (car err) (cdr err))))))

(defun +navigation/find-definition ()
  "Jump to the definition at point."
  (interactive)
  (+navigation--call-primary-or-fallback #'xref-find-definitions #'citre-jump))

(defun +navigation/find-references ()
  "Jump to references for symbol at point."
  (interactive)
  (+navigation--call-primary-or-fallback #'xref-find-references
                                           #'citre-jump-to-reference))

(defun +navigation/find-implementation ()
  "Jump to implementation using the active language server backend."
  (interactive)
  (+navigation--call-primary-or-fallback #'+language-server/find-implementation))

(defun +navigation/find-type-definition ()
  "Jump to type definition using the active language server backend."
  (interactive)
  (+navigation--call-primary-or-fallback #'+language-server/find-type-definition))

(defun +navigation--history-step (jump-command xref-command)
  "Run JUMP-COMMAND, falling back to XREF-COMMAND if no jump occurred."
  (let ((jumped nil))
    (when (fboundp jump-command)
      (let ((better-jumper-post-jump-hook
             (cons (lambda () (setq jumped t)) better-jumper-post-jump-hook)))
        (funcall jump-command)))
    (unless jumped
      (funcall xref-command))))

(defun +navigation/back ()
  "Go back to the previous navigation location."
  (interactive)
  (+navigation--history-step #'better-jumper-jump-backward #'xref-go-back))

(defun +navigation/forward ()
  "Go forward to the next navigation location."
  (interactive)
  (+navigation--history-step #'better-jumper-jump-forward #'xref-go-forward))

(defun +navigation/peek-definition ()
  "Peek the definition at point using Citre."
  (interactive)
  (if (fboundp 'citre-peek)
      (citre-peek)
    (user-error "Citre peek is unavailable")))

(defun +navigation/peek-references ()
  "Peek references at point using Citre."
  (interactive)
  (if (fboundp 'citre-peek-reference)
      (citre-peek-reference)
    (user-error "Citre peek reference is unavailable")))

(defun +navigation/jump-list ()
  "Show a selectable jump list."
  (interactive)
  (cond
   ((fboundp 'evil-collection-consult-jump-list)
    (call-interactively #'evil-collection-consult-jump-list))
   ((fboundp 'evil-show-jumps)
    (call-interactively #'evil-show-jumps))
   (t
    (user-error "Jump list UI is unavailable"))))

(defun +navigation/beginning-of-defun ()
  "Jump to the beginning of the current defun."
  (interactive)
  (+navigation--push-jump)
  (call-interactively #'beginning-of-defun))

(defun +navigation/end-of-defun ()
  "Jump to the end of the current defun."
  (interactive)
  (+navigation--push-jump)
  (call-interactively #'end-of-defun))

(defun +navigation/previous-defun ()
  "Jump to the previous defun."
  (interactive)
  (+navigation--push-jump)
  (condition-case nil
      (beginning-of-defun 2)
    (error
     (goto-char (point-min)))))

(defun +navigation/next-defun ()
  "Jump to the next defun."
  (interactive)
  (+navigation--push-jump)
  (condition-case nil
      (progn
        (end-of-defun)
        (beginning-of-defun -1))
    (error
     (goto-char (point-max)))))

(defun +navigation/up-structure ()
  "Jump to the nearest enclosing structural form."
  (interactive)
  (let* ((node (and (fboundp 'treesit-node-at)
                    (fboundp 'treesit-node-parent)
                    (or (ignore-errors (treesit-node-at (point)))
                        (and (> (point) (point-min))
                             (ignore-errors (treesit-node-at (1- (point))))))))
         (target
          (catch 'found
            (while node
              (setq node (treesit-node-parent node))
              (when (and node
                         (< (treesit-node-start node) (point))
                         (string-match-p +navigation-structural-node-regexp
                                         (treesit-node-type node)))
                (throw 'found (treesit-node-start node)))))))
    (unless target
      (setq target
            (save-excursion
              (condition-case nil
                  (progn (backward-up-list) (point))
                (scan-error nil)))))
    (unless target
      (user-error "No enclosing structural form"))
    (+navigation--push-jump)
    (goto-char target)))

(defalias 'structure-up #'+navigation/up-structure)

(global-set-key (kbd "M-.") #'+navigation/find-definition)
(global-set-key (kbd "M-?") #'+navigation/find-references)
(global-set-key (kbd "M-,") #'+navigation/back)

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "gd") #'+navigation/find-definition)
  (evil-define-key* 'normal 'global (kbd "gr") #'+navigation/find-references)
  (evil-define-key* 'normal 'global (kbd "gi") #'+navigation/find-implementation)
  (evil-define-key* 'normal 'global (kbd "gy") #'+navigation/find-type-definition)
  (evil-define-key* 'normal 'global (kbd "[f") #'+navigation/previous-defun)
  (evil-define-key* 'normal 'global (kbd "]f") #'+navigation/next-defun))

(aaron-leader!
  "n"   '(:ignore t :which-key "navigate")
  "n d" '(:def +navigation/find-definition :which-key "definition")
  "n r" '(:def +navigation/find-references :which-key "references")
  "n i" '(:def +navigation/find-implementation :which-key "implementation")
  "n t" '(:def +navigation/find-type-definition :which-key "type definition")
  "n b" '(:def +navigation/back :which-key "back")
  "n f" '(:def +navigation/forward :which-key "forward")
  "n j" '(:def +navigation/jump-list :which-key "jump list")
  "n p" '(:def +navigation/peek-definition :which-key "peek definition")
  "n P" '(:def +navigation/peek-references :which-key "peek references")
  "n c" '(:def +last-change/jump :which-key "last change")
  "n n" '(:def narrow-or-widen :which-key "narrow / widen")
  "n a" '(:def +navigation/beginning-of-defun :which-key "defun start")
  "n e" '(:def +navigation/end-of-defun :which-key "defun end")
  "n [" '(:def +navigation/previous-defun :which-key "previous defun")
  "n ]" '(:def +navigation/next-defun :which-key "next defun")
  "n u" '(:def structure-up :which-key "up structure"))

;; Popup rule for this module's buffers (see ui/popup).
(defvar +display-rules-bottom-size)
(set-popup-rule! "\\*xref\\*" :select t :align 'below :size +display-rules-bottom-size :regexp t)

(provide 'editor/navigation/config)
;;; editor/navigation/config.el ends here

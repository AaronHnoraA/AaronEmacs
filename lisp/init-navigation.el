;;; init-navigation.el --- Unified code navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/language-server-find-implementation "init-lsp")
(declare-function my/language-server-find-type-definition "init-lsp")
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

(defconst my/navigation-structural-node-regexp
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
          "statement_block"
          "statement_list"))
  "Tree-sitter node fragments treated as structural navigation anchors.")

(defconst my/navigation-pulse-excluded-modes
  '(so-long-mode special-mode comint-mode term-mode vterm-mode)
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

  (defun my/navigation-set-jump-a (fn &rest args)
    "Record a jump before calling FN with ARGS."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun my/navigation-set-jump-h ()
    "Record a jump before killing a visible buffer."
    (when (get-buffer-window)
      (better-jumper-set-jump))
    nil)

  (add-hook 'kill-buffer-hook #'my/navigation-set-jump-h)
  (advice-add #'imenu :around #'my/navigation-set-jump-a)
  (advice-add #'outline-up-heading :around #'my/navigation-set-jump-a))

(use-package pulse
  :ensure nil
  :defer 1
  :config
  (defun my/navigation-pulse-line-h (&rest _)
    "Momentarily highlight the current line after a jump."
    (when (and (display-graphic-p)
               (not (apply #'derived-mode-p my/navigation-pulse-excluded-modes)))
      (pulse-momentary-highlight-one-line (point))))

  (defun my/navigation-pulse-line-delayed-h (&rest _)
    "Pulse the current line after a short delay."
    (run-at-time 0.08 nil #'my/navigation-pulse-line-h))

  (dolist (hook '(imenu-after-jump-hook
                  consult-after-jump-hook
                  better-jumper-post-jump-hook
                  org-follow-link-hook))
    (add-hook hook #'my/navigation-pulse-line-h))

  (with-eval-after-load 'perspective
    (add-hook 'persp-activated-functions #'my/navigation-pulse-line-delayed-h))

  (advice-add #'save-place-find-file-hook :after #'my/navigation-pulse-line-h))

(defun my/navigation--push-jump ()
  "Record the current position in Evil's jump list when available."
  (when (or (fboundp 'better-jumper-set-jump)
            (fboundp 'evil-set-jump))
    (ignore-errors
      (if (fboundp 'better-jumper-set-jump)
          (better-jumper-set-jump)
        (evil-set-jump)))))

(defun my/navigation--call-primary-or-fallback (primary &optional fallback)
  "Call PRIMARY interactively, with optional FALLBACK on lookup errors."
  (my/navigation--push-jump)
  (condition-case err
      (if (fboundp primary)
          (call-interactively primary)
        (signal 'void-function (list primary)))
    ((user-error error)
     (if (and fallback (fboundp fallback))
         (call-interactively fallback)
       (signal (car err) (cdr err))))))

(defun my/navigation-find-definition ()
  "Jump to the definition at point."
  (interactive)
  (my/navigation--call-primary-or-fallback #'xref-find-definitions #'citre-jump))

(defun my/navigation-find-references ()
  "Jump to references for symbol at point."
  (interactive)
  (my/navigation--call-primary-or-fallback #'xref-find-references
                                           #'citre-jump-to-reference))

(defun my/navigation-find-implementation ()
  "Jump to implementation using the active language server backend."
  (interactive)
  (my/navigation--call-primary-or-fallback #'my/language-server-find-implementation))

(defun my/navigation-find-type-definition ()
  "Jump to type definition using the active language server backend."
  (interactive)
  (my/navigation--call-primary-or-fallback #'my/language-server-find-type-definition))

(defun my/navigation-back ()
  "Go back to the previous navigation location."
  (interactive)
  (condition-case err
      (xref-go-back)
    ((user-error error)
     (if (fboundp 'evil-jump-backward)
         (call-interactively #'evil-jump-backward)
       (signal (car err) (cdr err))))))

(defun my/navigation-forward ()
  "Go forward to the next navigation location."
  (interactive)
  (condition-case err
      (xref-go-forward)
    ((user-error error)
     (if (fboundp 'evil-jump-forward)
         (call-interactively #'evil-jump-forward)
       (signal (car err) (cdr err))))))

(defun my/navigation-peek-definition ()
  "Peek the definition at point using Citre."
  (interactive)
  (if (fboundp 'citre-peek)
      (citre-peek)
    (user-error "Citre peek is unavailable")))

(defun my/navigation-peek-references ()
  "Peek references at point using Citre."
  (interactive)
  (if (fboundp 'citre-peek-reference)
      (citre-peek-reference)
    (user-error "Citre peek reference is unavailable")))

(defun my/navigation-jump-list ()
  "Show a selectable jump list."
  (interactive)
  (cond
   ((fboundp 'evil-collection-consult-jump-list)
    (call-interactively #'evil-collection-consult-jump-list))
   ((fboundp 'evil-show-jumps)
    (call-interactively #'evil-show-jumps))
   (t
    (user-error "Jump list UI is unavailable"))))

(defun my/navigation-beginning-of-defun ()
  "Jump to the beginning of the current defun."
  (interactive)
  (my/navigation--push-jump)
  (call-interactively #'beginning-of-defun))

(defun my/navigation-end-of-defun ()
  "Jump to the end of the current defun."
  (interactive)
  (my/navigation--push-jump)
  (call-interactively #'end-of-defun))

(defun my/navigation-previous-defun ()
  "Jump to the previous defun."
  (interactive)
  (my/navigation--push-jump)
  (condition-case nil
      (beginning-of-defun 2)
    (error
     (goto-char (point-min)))))

(defun my/navigation-next-defun ()
  "Jump to the next defun."
  (interactive)
  (my/navigation--push-jump)
  (condition-case nil
      (progn
        (end-of-defun)
        (beginning-of-defun -1))
    (error
     (goto-char (point-max)))))

(defun my/navigation-up-structure ()
  "Jump to the nearest enclosing structural form."
  (interactive)
  (my/navigation--push-jump)
  (cond
   ((and (fboundp 'treesit-node-at)
         (fboundp 'treesit-node-parent))
    (let ((node (or (ignore-errors (treesit-node-at (point)))
                    (and (> (point) (point-min))
                         (ignore-errors (treesit-node-at (1- (point))))))))
      (catch 'done
        (while node
          (setq node (treesit-node-parent node))
          (when (and node
                     (string-match-p my/navigation-structural-node-regexp
                                     (treesit-node-type node)))
            (goto-char (treesit-node-start node))
            (throw 'done t)))
        (user-error "No enclosing structural form"))))
   ((ignore-errors (backward-up-list) t))
   (t
    (user-error "No enclosing structural form"))))

(global-set-key (kbd "M-.") #'my/navigation-find-definition)
(global-set-key (kbd "M-?") #'my/navigation-find-references)
(global-set-key (kbd "M-,") #'my/navigation-back)

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "gd") #'my/navigation-find-definition)
  (evil-define-key* 'normal 'global (kbd "gr") #'my/navigation-find-references)
  (evil-define-key* 'normal 'global (kbd "gi") #'my/navigation-find-implementation)
  (evil-define-key* 'normal 'global (kbd "gy") #'my/navigation-find-type-definition)
  (evil-define-key* 'normal 'global (kbd "[f") #'my/navigation-previous-defun)
  (evil-define-key* 'normal 'global (kbd "]f") #'my/navigation-next-defun))

(my/leader-key-label "n" "navigate")
(my/evil-global-leader-set "n d" #'my/navigation-find-definition "definition")
(my/evil-global-leader-set "n r" #'my/navigation-find-references "references")
(my/evil-global-leader-set "n i" #'my/navigation-find-implementation "implementation")
(my/evil-global-leader-set "n t" #'my/navigation-find-type-definition "type definition")
(my/evil-global-leader-set "n b" #'my/navigation-back "back")
(my/evil-global-leader-set "n f" #'my/navigation-forward "forward")
(my/evil-global-leader-set "n j" #'my/navigation-jump-list "jump list")
(my/evil-global-leader-set "n p" #'my/navigation-peek-definition "peek definition")
(my/evil-global-leader-set "n P" #'my/navigation-peek-references "peek references")
(my/evil-global-leader-set "n a" #'my/navigation-beginning-of-defun "defun start")
(my/evil-global-leader-set "n e" #'my/navigation-end-of-defun "defun end")
(my/evil-global-leader-set "n [" #'my/navigation-previous-defun "previous defun")
(my/evil-global-leader-set "n ]" #'my/navigation-next-defun "next defun")
(my/evil-global-leader-set "n u" #'my/navigation-up-structure "up structure")

(provide 'init-navigation)
;;; init-navigation.el ends here

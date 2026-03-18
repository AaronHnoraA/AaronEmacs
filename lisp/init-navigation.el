;;; init-navigation.el --- Unified code navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/language-server-find-implementation "init-lsp")
(declare-function my/language-server-find-type-definition "init-lsp")
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function evil-jump-backward "evil-commands" (&optional count))
(declare-function evil-jump-forward "evil-commands" (&optional count))
(declare-function evil-set-jump "evil-jumps")
(declare-function evil-collection-consult-jump-list "evil-collection-consult")
(declare-function xref-find-definitions "xref" (identifier))
(declare-function xref-find-references "xref" (identifier))
(declare-function xref-go-back "xref" ())
(declare-function xref-go-forward "xref" ())
(declare-function citre-jump "citre")
(declare-function citre-jump-to-reference "citre")
(declare-function citre-peek "citre" (&optional buf point reference))
(declare-function citre-peek-reference "citre")

(defun my/navigation--push-jump ()
  "Record the current position in Evil's jump list when available."
  (when (fboundp 'evil-set-jump)
    (ignore-errors
      (evil-set-jump))))

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

(global-set-key (kbd "M-.") #'my/navigation-find-definition)
(global-set-key (kbd "M-?") #'my/navigation-find-references)
(global-set-key (kbd "M-,") #'my/navigation-back)

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "gd") #'my/navigation-find-definition)
  (evil-define-key* 'normal 'global (kbd "gr") #'my/navigation-find-references)
  (evil-define-key* 'normal 'global (kbd "gi") #'my/navigation-find-implementation)
  (evil-define-key* 'normal 'global (kbd "gy") #'my/navigation-find-type-definition))

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

(provide 'init-navigation)
;;; init-navigation.el ends here

;;; init-navigation-extra.el --- Extra code navigation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(autoload 'citre-peek-restore "citre-ui-peek" nil t)

(declare-function eglot-find-declaration "eglot")
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function lsp-find-declaration "lsp-mode" (&key display-action))
(declare-function lsp-treemacs-call-hierarchy "lsp-treemacs")
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/navigation--push-jump "init-navigation")
(declare-function my/navigation-find-definition "init-navigation")

(defun my/navigation-find-declaration ()
  "Jump to the declaration at point, falling back to definition."
  (interactive)
  (my/navigation--push-jump)
  (condition-case nil
      (pcase (and (fboundp 'my/current-language-server-backend)
                  (my/current-language-server-backend))
        ('eglot
         (if (fboundp 'eglot-find-declaration)
             (call-interactively #'eglot-find-declaration)
           (call-interactively #'my/navigation-find-definition)))
        ('lsp-mode
         (if (fboundp 'lsp-find-declaration)
             (call-interactively #'lsp-find-declaration)
           (call-interactively #'my/navigation-find-definition)))
        (_
         (call-interactively #'my/navigation-find-definition)))
    ((user-error error)
     (call-interactively #'my/navigation-find-definition))))

(defun my/navigation-peek-restore ()
  "Restore the last Citre peek session."
  (interactive)
  (if (fboundp 'citre-peek-restore)
      (call-interactively #'citre-peek-restore)
    (user-error "Citre peek restore is unavailable")))

(defun my/navigation-call-hierarchy ()
  "Open call hierarchy when supported by the active LSP backend."
  (interactive)
  (cond
   ((and (eq (and (fboundp 'my/current-language-server-backend)
                  (my/current-language-server-backend))
             'lsp-mode)
         (fboundp 'lsp-treemacs-call-hierarchy))
    (call-interactively #'lsp-treemacs-call-hierarchy))
   (t
    (user-error "Call hierarchy requires lsp-mode with lsp-treemacs"))))

(with-eval-after-load 'evil
  (evil-define-key* 'normal 'global (kbd "gD") #'my/navigation-find-declaration))

(my/leader!
  "n D" '(:def my/navigation-find-declaration :which-key "declaration")
  "n R" '(:def my/navigation-peek-restore :which-key "restore peek")
  "n h" '(:def my/navigation-call-hierarchy :which-key "call hierarchy"))

(provide 'init-navigation-extra)
;;; init-navigation-extra.el ends here

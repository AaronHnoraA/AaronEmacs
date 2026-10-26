;;; editor/multiple-cursors/config.el --- Multi-cursor editing -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(declare-function evil-emacs-state "evil-states")
(declare-function evil-normal-state "evil-states")
(declare-function evil-visual-direction "evil-states")
(declare-function evil-visual-state-p "evil-states")
(declare-function evil-visual-type "evil-states")
(declare-function mc/keyboard-quit "multiple-cursors-core")
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar mc--default-cmds-to-run-once)

(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             mc/mark-all-like-this-dwim
             mc/skip-to-next-like-this
             mc/skip-to-previous-like-this)
  :init
  (setq mc/list-file (expand-file-name "var/mc-lists.el" user-emacs-directory))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         :map evil-visual-state-map
         ("g n"         . mc/mark-next-like-this)
         ("g p"         . mc/mark-previous-like-this)
         ("g a"         . mc/mark-all-like-this-dwim)))

;; multiple-cursors runs Emacs commands at its cursors, not Evil ones, and in
;; charwise Visual state the Emacs region leaves out the last selected
;; character: `viw' on "foo" then `gn' marked "fo".  As in Doom, starting
;; cursors from an Evil state hands the exact Evil range over as the Emacs
;; region and switches to Emacs state; Escape ends them and returns to Normal.
(defvar-local +multiple-cursors--from-evil nil
  "Non-nil when multiple cursors started from an Evil state other than Emacs.")

(defun +multiple-cursors-evil-region-a (&rest _)
  "Turn an Evil selection into the Emacs region, in Emacs state."
  (when (and (bound-and-true-p evil-local-mode)
             (not (memq evil-state '(insert emacs))))
    (setq +multiple-cursors--from-evil t)
    (if (not (evil-visual-state-p))
        (evil-emacs-state)
      (let ((beg evil-visual-beginning)
            (end (if (eq (evil-visual-type) 'line)
                     (1- evil-visual-end)
                   evil-visual-end))
            (forward (> (evil-visual-direction) 0)))
        (evil-emacs-state)
        (push-mark (if forward beg end) t t)
        (goto-char (if forward end beg))))))

(defun +multiple-cursors-restore-evil-state-h ()
  "Return to Normal state when cursors that started in Evil end."
  (when +multiple-cursors--from-evil
    (setq +multiple-cursors--from-evil nil)
    (deactivate-mark)
    (evil-normal-state)))

(defun +multiple-cursors-escape-h ()
  "End multiple cursors from `aaron/escape'."
  (when (bound-and-true-p multiple-cursors-mode)
    (mc/keyboard-quit)
    t))

(with-eval-after-load 'multiple-cursors-core
  (add-to-list 'mc--default-cmds-to-run-once 'aaron/escape)
  (add-hook 'multiple-cursors-mode-disabled-hook
            #'+multiple-cursors-restore-evil-state-h))
(dolist (command '(mc/edit-lines
                   mc/mark-next-like-this
                   mc/mark-previous-like-this
                   mc/mark-all-like-this
                   mc/mark-all-like-this-dwim))
  (advice-add command :before #'+multiple-cursors-evil-region-a))
;; Before Evil's own Escape handler, which would only leave Emacs state.
(add-hook 'aaron-escape-hook #'+multiple-cursors-escape-h -50)

(provide 'editor/multiple-cursors/config)
;;; editor/multiple-cursors/config.el ends here

;;; editor/evil/autoload/evil.el --- Evil registration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `set-evil-initial-state!', Doom's way for a module to declare the Evil
;; state its modes start in.  It is autoloaded, so a module calls it at load
;; time whether or not Evil has loaded; the state is applied once it has.
;;
;; Adapted from Doom Emacs (MIT, Henrik Lissner):
;;   modules/editor/evil/autoload/evil.el @ 42a0479

;;; Code:

(declare-function evil-set-initial-state "evil-core" (mode state))

;;;###autoload
(defun set-evil-initial-state! (modes state)
  "Start MODES, a mode or list of modes, in Evil STATE."
  (if (featurep 'evil)
      (dolist (mode (ensure-list modes))
        (evil-set-initial-state mode state))
    (with-eval-after-load 'evil
      (dolist (mode (ensure-list modes))
        (evil-set-initial-state mode state)))))

;;; editor/evil/autoload/evil.el ends here

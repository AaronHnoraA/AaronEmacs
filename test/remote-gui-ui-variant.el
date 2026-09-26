;;; remote-gui-ui-variant.el --- Isolate GUI chrome redraw cost -*- lexical-binding: t; -*-

;; Opt in with REMOTE_LSP_E2E_UI_VARIANT=none|tab|mode|all.
;; This changes only the synthetic typing interval in the smoke process.

(defun my/remote-gui-ui-variant--around (function rounds)
  "Time FUNCTION for ROUNDS with one requested UI segment suppressed."
  (let* ((variant (getenv "REMOTE_LSP_E2E_UI_VARIANT"))
         (hide-tab (member variant '("tab" "all")))
         (hide-mode (member variant '("mode" "all")))
         (tab-line-format (unless hide-tab tab-line-format))
         (mode-line-format (unless hide-mode mode-line-format))
         (header-line-format (unless (equal variant "all")
                               header-line-format)))
    (redisplay t)
    (funcall function rounds)))

(advice-add 'my/lsp-remote-live-smoke--typing-probe :around
            #'my/remote-gui-ui-variant--around)

(provide 'remote-gui-ui-variant)
;;; remote-gui-ui-variant.el ends here

;;; noema-startup-tests.el --- Cold-start Noema command registration -*- lexical-binding: t; -*-

(require 'ert)

;; Run with the real Emacs init, before any research-mode/UI test requires.
(ert-deftest noema-startup-registers-capability-managers-lazily ()
  (dolist (command '(noema-capability-manager noema-skill-manager noema-mcp-manager))
    (should (commandp command))
    (should (autoloadp (symbol-function command)))
    (should (equal (nth 1 (symbol-function command)) "noema-capability-ui")))
  (should-not (featurep 'noema-capability-ui)))

;;; noema-startup-tests.el ends here

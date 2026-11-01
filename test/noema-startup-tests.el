;;; noema-startup-tests.el --- Cold-start Noema command registration -*- lexical-binding: t; -*-

(require 'ert)

;; Run with the real Emacs init, before any research-mode/UI test requires.
(ert-deftest noema-startup-registers-capability-managers-lazily ()
  (dolist (command '(noema-capability-manager noema-skill noema-skill-manager noema-mcp-manager))
    (should (commandp command))
    (should (autoloadp (symbol-function command)))
    (should (equal (nth 1 (symbol-function command)) "noema-capability-ui")))
  (should-not (featurep 'noema-capability-ui)))

(ert-deftest noema-startup-keeps-project-commands-without-a-work-document-editor ()
  (dolist (entry '((noema-project-overview . "noema-project-overview")
                   (noema-history-search . "noema-history-search")
                   (noema-findings . "noema-findings")))
    (should (commandp (car entry)))
    (should (autoloadp (symbol-function (car entry))))
    (should (equal (nth 1 (symbol-function (car entry))) (cdr entry))))
  (dolist (library '("noema-research-mode" "noema-research-completion"
                     "noema-research-graph" "noema-research-settings"
                     "noema-research-workflow"))
    (should-not (locate-library library)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/legacy.noema.ipynb")
    (+noema-jupyter-notebook-open-mode)
    (should (eq major-mode 'fundamental-mode))
    (should (string-empty-p (buffer-string))))
  (should (fboundp '+noema-jupyter-cell-capf))
  (should (commandp 'company-complete))
  (should (commandp 'lsp)))



;;; noema-startup-tests.el ends here

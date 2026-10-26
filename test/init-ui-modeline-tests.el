;;; init-ui-modeline-tests.el --- Doom and Portal state display -*- lexical-binding: t; -*-

;;; Commentary:
;; C1 and C3 in docs/design/modeline.md.  Run after loading the full config.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'doom-modeline)
(require 'portal-web)
(require 'ui/modeline/config)

(ert-deftest modeline-c1-portal-web-shows-keyboard-owner-instead-of-evil-state ()
  "A Noema Portal Web view has one meaningful input state."
  (with-temp-buffer
    (portal-web-mode)
    (setq-local evil-local-mode t
                evil-state 'normal)
    (should-not (+modeline--state))
    (cl-letf (((symbol-function 'doom-modeline--active) (lambda () t)))
      (should-not (doom-modeline-segment--+modeline-state)))
    (cl-letf (((symbol-function 'portal-input--focused-p) (lambda () t)))
      (should (equal (substring-no-properties (portal-input--mode-line-process))
                     "[app]")))
    (cl-letf (((symbol-function 'portal-input--focused-p) (lambda () nil)))
      (should (equal (substring-no-properties (portal-input--mode-line-process))
                     "[emacs]")))
    (should-not (+modeline--state))))

(ert-deftest modeline-c3-doom-main-upgrades-existing-buffers-and-preserves-special ()
  "Late Doom activation updates only buffers using its old main layout."
  (let ((old (generate-new-buffer " *modeline-old-main*"))
        (special (generate-new-buffer " *modeline-special*")))
    (unwind-protect
        (let ((main (list "%e" (doom-modeline 'main)))
              (new (list "%e" (doom-modeline '+main))))
          (with-current-buffer old (setq-local mode-line-format main))
          (with-current-buffer special
            (doom-modeline-set-modeline 'special))
          (let ((special-format (buffer-local-value 'mode-line-format special)))
            (+modeline-use-main-h)
            (should (equal (buffer-local-value 'mode-line-format old) new))
            (should (equal (buffer-local-value 'mode-line-format special)
                           special-format))))
      (kill-buffer old)
      (kill-buffer special))))

(ert-deftest modeline-c1-ordinary-evil-buffer-keeps-state-block ()
  "The Portal exception leaves ordinary Evil buffers unchanged."
  (with-temp-buffer
    (setq-local evil-local-mode t
                evil-state 'normal)
    (should (eq (+modeline--state) 'normal))
    (cl-letf (((symbol-function 'doom-modeline--active) (lambda () t)))
      (should (equal (substring-no-properties
                      (doom-modeline-segment--+modeline-state))
                     " NORMAL ")))))

(provide 'init-ui-modeline-tests)

;;; init-ui-modeline-tests.el ends here

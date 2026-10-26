;;; init-macos-input-tests.el --- Input source follows Evil states -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-macos-input-tests.el -f ert-run-tests-batch-and-exit
;;
;; The native module is stubbed, so these tests never change the input
;; source of the person at the keyboard.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'evil)
(require 'os/macos/input)

(defmacro init-macos-input-tests--with-source (source &rest body)
  "Run BODY with a stubbed input source starting at SOURCE.
Inside BODY, `current' names the stubbed current source."
  (declare (indent 1))
  `(let ((current ,source)
         (noninteractive nil)
         (+macos-input-switch t)
         (+macos-input-ascii-source "com.apple.keylayout.ABC")
         (+macos-input--insert-source nil))
     (cl-letf (((symbol-function '+macos-input-available-p) #'always)
               ((symbol-function 'macos-input-source) (lambda () current))
               ((symbol-function 'macos-input-select-source)
                (lambda (id) (setq current id) t)))
       ,@body)))

(ert-deftest +macos-input-normal-state-uses-ascii-and-insert-restores ()
  "Leaving Insert selects ABC; entering Insert brings Pinyin back."
  (init-macos-input-tests--with-source "com.apple.inputmethod.SCIM.ITABC"
    (with-temp-buffer
      (evil-local-mode 1)
      (evil-insert-state)
      (evil-normal-state)
      (should (equal current "com.apple.keylayout.ABC"))
      (evil-insert-state)
      (should (equal current "com.apple.inputmethod.SCIM.ITABC"))
      (evil-normal-state))))

(ert-deftest +macos-input-ascii-in-insert-stays-ascii ()
  "Insert state left in ABC is entered again in ABC."
  (init-macos-input-tests--with-source "com.apple.keylayout.ABC"
    (with-temp-buffer
      (evil-local-mode 1)
      (evil-insert-state)
      (evil-normal-state)
      (setq current "com.apple.keylayout.ABC")
      (evil-insert-state)
      (should (equal current "com.apple.keylayout.ABC"))
      (should-not +macos-input--insert-source)
      (evil-normal-state))))

(ert-deftest +macos-input-batch-and-disabled-switch-nothing ()
  "Batch Emacs and a disabled switch leave the source alone."
  (init-macos-input-tests--with-source "com.apple.inputmethod.SCIM.ITABC"
    (with-temp-buffer
      (evil-local-mode 1)
      (let ((noninteractive t))
        (evil-insert-state)
        (evil-normal-state))
      (should (equal current "com.apple.inputmethod.SCIM.ITABC"))
      (let ((+macos-input-switch nil))
        (evil-insert-state)
        (evil-normal-state))
      (should (equal current "com.apple.inputmethod.SCIM.ITABC")))))

(provide 'init-macos-input-tests)
;;; init-macos-input-tests.el ends here

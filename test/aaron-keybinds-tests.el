;;; aaron-keybinds-tests.el --- Tests for the keybinding layer -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make core-test'.  Evil is not loaded here, so only bindings
;; outside Evil states are checked live; state bindings are checked by
;; their expansion.

;;; Code:

(require 'ert)
(require 'aaron-keybinds)

(defvar aaron-keybinds-tests-map (make-sparse-keymap))
(defvar aaron-keybinds-tests-mode-map (make-sparse-keymap))

(defmacro aaron-keybinds-tests--with-maps (&rest body)
  "Run BODY with empty test keymaps."
  (declare (indent 0))
  `(let ((aaron-keybinds-tests-map (make-sparse-keymap))
         (aaron-keybinds-tests-mode-map (make-sparse-keymap)))
     ,@body))

(ert-deftest aaron-keybinds-map-binds-in-keymap ()
  (aaron-keybinds-tests--with-maps
    (map! :map aaron-keybinds-tests-map
          "C-c a" #'ignore
          (:prefix "C-c b"
           "c" #'forward-char))
    (should (eq (keymap-lookup aaron-keybinds-tests-map "C-c a") #'ignore))
    (should (eq (keymap-lookup aaron-keybinds-tests-map "C-c b c") #'forward-char))))

(ert-deftest aaron-keybinds-map-mode-names-the-mode-map ()
  (aaron-keybinds-tests--with-maps
    (map! :mode aaron-keybinds-tests-mode "C-c x" #'ignore)
    (should (eq (keymap-lookup aaron-keybinds-tests-mode-map "C-c x") #'ignore))))

(ert-deftest aaron-keybinds-map-when-and-after ()
  (aaron-keybinds-tests--with-maps
    (map! :map aaron-keybinds-tests-map
          (:when nil "C-c n" #'ignore)
          (:unless nil "C-c u" #'ignore)
          (:after aaron-keybinds-tests-never-loaded "C-c l" #'ignore))
    (should-not (keymap-lookup aaron-keybinds-tests-map "C-c n"))
    (should (eq (keymap-lookup aaron-keybinds-tests-map "C-c u") #'ignore))
    (should-not (keymap-lookup aaron-keybinds-tests-map "C-c l"))))

(ert-deftest aaron-keybinds-map-desc-becomes-which-key-label ()
  (should (equal (macroexpand-1 '(map! :map m :desc "Label" "a" #'ignore))
                 '(general-define-key :keymaps (backquote (m))
                                      "a" (list :def #'ignore :which-key "Label"))))
  (should (equal (macroexpand-1 '(map! :map m (:prefix ("p" . "Prefix") "a" #'ignore)))
                 '(general-define-key :keymaps (backquote (m)) :prefix "p"
                                      "" (list :ignore t :which-key "Prefix")
                                      "a" #'ignore)))
  (should (equal (macroexpand-1 '(map! :map m :desc "Label" "a" '(:def ignore)))
                 '(general-define-key :keymaps (backquote (m))
                                      "a" '(:def ignore :which-key "Label")))))

(ert-deftest aaron-keybinds-map-states-and-leader ()
  (should (equal (macroexpand-1 '(map! :nv "a" #'ignore))
                 '(progn
                    (general-define-key :states 'visual "a" #'ignore)
                    (general-define-key :states 'normal "a" #'ignore))))
  (should (equal (macroexpand-1 '(map! :leader "x" #'ignore))
                 '(define-leader-key! "x" #'ignore)))
  (should (equal (macroexpand-1 '(map! :localleader :map m "x" #'ignore))
                 '(define-localleader-key! :keymaps (backquote (m)) "x" #'ignore))))

(ert-deftest aaron-keybinds-leader-definers-keep-their-defaults ()
  (should (equal (macroexpand-1 '(define-leader-key! "x" #'ignore))
                 '(general-def "x" #'ignore
                               :states 'normal :keymaps 'global :prefix "<leader>")))
  (should (equal (macroexpand-1 '(define-localleader-key! :keymaps 'm "x" #'ignore))
                 '(general-def :keymaps 'm "x" #'ignore
                               :states 'normal :prefix "<localleader>"))))

(ert-deftest aaron-keybinds-map-rejects-unknown-states ()
  (should-error (macroexpand-1 '(map! :nz "a" #'ignore))))

(provide 'aaron-keybinds-tests)
;;; aaron-keybinds-tests.el ends here

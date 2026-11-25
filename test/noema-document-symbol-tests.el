;;; noema-document-symbol-tests.el --- Live Emacs symbol cards -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'noema/bridge/document-symbol)

(defun noema-test-symbol-command (name &optional use-default)
  "Return NAME or the default.

The second paragraph is reserved for the full help buffer."
  (interactive)
  (if use-default "default" name))

(defvar noema-test-symbol-option "live value"
  "Display option used by the synthetic symbol card.")

(defalias 'noema-test-symbol-primitive 'insert-buffer-substring)

(ert-deftest noema-document-symbol-inspect-is-read-only ()
  (let* ((command (+noema--document-symbol-api
                   '((name . "noema-test-symbol-command")) nil))
         (option (+noema--document-symbol-api
                  '((name . "noema-test-symbol-option")) nil)))
    (should (equal (alist-get 'kind command) "Command"))
    (should (equal (alist-get 'signature command)
                   "(noema-test-symbol-command name &optional use-default)"))
    (should (equal (alist-get 'summary command) "Return NAME or the default."))
    (should (equal (alist-get 'file command) "noema-document-symbol-tests.el"))
    (should (equal (alist-get 'kind option) "Variable"))
    (should-not (alist-get 'value option))
    (should
     (eq :json-false
         (alist-get 'found (+noema--document-symbol-api
                            '((name . "noema-test-missing-dc0944")) nil))))))

(ert-deftest noema-document-symbol-skips-computed-variable-documentation ()
  (let ((original (get 'noema-test-symbol-option 'variable-documentation)))
    (unwind-protect
        (progn
          (put 'noema-test-symbol-option 'variable-documentation
               '(setq noema-test-symbol-option "documentation evaluated"))
          (+noema--document-symbol-api '((name . "noema-test-symbol-option")) nil)
          (should (equal noema-test-symbol-option "live value")))
      (put 'noema-test-symbol-option 'variable-documentation original))))

(ert-deftest noema-document-symbol-skips-computed-function-documentation ()
  (let ((original (get 'noema-test-symbol-primitive 'function-documentation)))
    (unwind-protect
        (progn
          (put 'noema-test-symbol-primitive 'function-documentation
               '(setq noema-test-symbol-option "documentation evaluated"))
          (let ((info (+noema--document-symbol-api
                       '((name . "noema-test-symbol-primitive")) nil)))
            (should (equal noema-test-symbol-option "live value"))
            (should-not (alist-get 'summary info))
            (should (stringp (alist-get 'signature info)))))
      (put 'noema-test-symbol-primitive 'function-documentation original))))

(ert-deftest noema-document-symbol-opens-native-help-and-definition ()
  (let (help definition)
    (cl-letf (((symbol-function 'describe-symbol)
               (lambda (name &rest _) (setq help name)))
              ((symbol-function 'find-function-other-window)
               (lambda (name) (setq definition name))))
      (+noema--document-symbol-api
       '((name . "noema-test-symbol-command") (action . "help")) nil)
      (+noema--document-symbol-api
       '((name . "noema-test-symbol-command") (action . "definition")) nil)
      (should (eq help 'noema-test-symbol-command))
      (should (eq definition 'noema-test-symbol-command))))
  (should-error (+noema--document-symbol-api
                 '((name . "(message \"bad\")") (action . "inspect")) nil)
                :type 'user-error)
  (should-not (intern-soft "(message \"bad\")")))

;;; noema-document-symbol-tests.el ends here

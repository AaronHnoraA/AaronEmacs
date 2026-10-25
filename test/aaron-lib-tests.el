;;; aaron-lib-tests.el --- Tests for the core macro library -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make core-test'.

;;; Code:

(require 'ert)
(require 'aaron-lib)

(defvar aaron-lib-tests--hook nil)
(defvar aaron-lib-tests--hook-2 nil)
(defvar aaron-lib-tests--log nil)
(defvar aaron-lib-tests-mode-hook nil)
(defvar aaron-lib-tests--var nil)

(defmacro aaron-lib-tests--with-hooks (&rest body)
  "Run BODY with the test hooks and log reset."
  (declare (indent 0))
  `(let ((aaron-lib-tests--hook nil)
         (aaron-lib-tests--hook-2 nil)
         (aaron-lib-tests-mode-hook nil)
         (aaron-lib-tests--log nil))
     ,@body))

(defun aaron-lib-tests--push (x)
  "Record X in the test log."
  (push x aaron-lib-tests--log))

;;; Hooks

(ert-deftest aaron-lib-add-hook-resolves-modes-and-quoted-hooks ()
  (aaron-lib-tests--with-hooks
    (add-hook! aaron-lib-tests-mode #'ignore)
    (should (memq #'ignore aaron-lib-tests-mode-hook))
    (add-hook! '(aaron-lib-tests--hook aaron-lib-tests--hook-2) #'car #'cdr)
    (should (equal aaron-lib-tests--hook '(car cdr)))
    (should (equal aaron-lib-tests--hook-2 '(car cdr)))))

(ert-deftest aaron-lib-add-hook-accepts-quoted-lists-and-defuns ()
  (aaron-lib-tests--with-hooks
    (add-hook! 'aaron-lib-tests--hook '(car cdr))
    (should (equal aaron-lib-tests--hook '(car cdr)))
    (add-hook! 'aaron-lib-tests--hook-2
      (defun aaron-lib-tests--named-h () (aaron-lib-tests--push 'named)))
    (should (fboundp 'aaron-lib-tests--named-h))
    (run-hooks 'aaron-lib-tests--hook-2)
    (should (equal aaron-lib-tests--log '(named)))))

(ert-deftest aaron-lib-add-hook-wraps-forms-in-one-lambda ()
  (aaron-lib-tests--with-hooks
    (add-hook! 'aaron-lib-tests--hook
      (aaron-lib-tests--push 1)
      (aaron-lib-tests--push 2))
    (should (= (length aaron-lib-tests--hook) 1))
    (run-hooks 'aaron-lib-tests--hook)
    (should (equal aaron-lib-tests--log '(2 1)))))

(ert-deftest aaron-lib-add-hook-honours-append-depth-and-remove ()
  (aaron-lib-tests--with-hooks
    (add-hook 'aaron-lib-tests--hook #'car)
    (add-hook! 'aaron-lib-tests--hook :append #'cdr)
    (should (equal aaron-lib-tests--hook '(car cdr)))
    (add-hook! 'aaron-lib-tests--hook :depth -90 #'cons)
    (should (eq (car aaron-lib-tests--hook) 'cons))
    (remove-hook! 'aaron-lib-tests--hook #'car #'cdr #'cons)
    (should (null aaron-lib-tests--hook))))

(ert-deftest aaron-lib-add-hook-local ()
  (aaron-lib-tests--with-hooks
    (with-temp-buffer
      (add-hook! 'aaron-lib-tests--hook :local #'car)
      (should (equal aaron-lib-tests--hook '(car t))))
    (should (null aaron-lib-tests--hook))))

(ert-deftest aaron-lib-setq-hook-sets-locals-and-unsetq-removes ()
  (aaron-lib-tests--with-hooks
    (setq-hook! aaron-lib-tests-mode aaron-lib-tests--var 42)
    (should (memq 'aaron--setq-aaron-lib-tests--var-for-aaron-lib-tests-mode-h
                  aaron-lib-tests-mode-hook))
    (with-temp-buffer
      (run-hooks 'aaron-lib-tests-mode-hook)
      (should (local-variable-p 'aaron-lib-tests--var))
      (should (= aaron-lib-tests--var 42)))
    (should (null aaron-lib-tests--var))
    (unsetq-hook! aaron-lib-tests-mode aaron-lib-tests--var)
    (should (null aaron-lib-tests-mode-hook))))

(ert-deftest aaron-lib-transient-hook-runs-once ()
  (aaron-lib-tests--with-hooks
    (add-transient-hook! 'aaron-lib-tests--hook (aaron-lib-tests--push 'once))
    (run-hooks 'aaron-lib-tests--hook)
    (run-hooks 'aaron-lib-tests--hook)
    (should (equal aaron-lib-tests--log '(once)))
    (should (null aaron-lib-tests--hook))))

(defun aaron-lib-tests--target (x)
  "Return X."
  x)

(ert-deftest aaron-lib-transient-hook-on-function ()
  (aaron-lib-tests--with-hooks
    (add-transient-hook! #'aaron-lib-tests--target (aaron-lib-tests--push 'advised))
    (should (= (aaron-lib-tests--target 1) 1))
    (should (= (aaron-lib-tests--target 2) 2))
    (should (equal aaron-lib-tests--log '(advised)))
    (should-not (advice-member-p (lambda (&rest _)) 'aaron-lib-tests--target))))

;;; Running hooks

(ert-deftest aaron-lib-run-hooks-names-the-failing-function ()
  (aaron-lib-tests--with-hooks
    (add-hook 'aaron-lib-tests--hook (lambda () (aaron-lib-tests--push 'after)) 90)
    (add-hook 'aaron-lib-tests--hook #'aaron-lib-tests--failing-h)
    (fset 'aaron-lib-tests--failing-h (lambda () (error "boom")))
    (let ((debug-on-error nil)
          (warning-minimum-log-level :emergency))
      (let ((err (should-error (aaron-run-hooks 'aaron-lib-tests--hook)
                               :type 'aaron-hook-error)))
        (should (eq (nth 1 err) 'aaron-lib-tests--hook))
        (should (eq (nth 2 err) 'aaron-lib-tests--failing-h))))
    (should (null aaron-lib-tests--log))))

(ert-deftest aaron-lib-run-hook-on-fires-once-and-cleans-up ()
  (aaron-lib-tests--with-hooks
    (add-hook 'aaron-lib-tests--hook-2 (lambda () (aaron-lib-tests--push 'fired)))
    (let ((after-init-time (current-time))
          (aaron-lib-tests--hook (list #'ignore)))
      (aaron-run-hook-on 'aaron-lib-tests--hook-2 '(aaron-lib-tests--hook))
      (should (= (length aaron-lib-tests--hook) 2))
      (run-hooks 'aaron-lib-tests--hook)
      (run-hooks 'aaron-lib-tests--hook)
      (should (equal aaron-lib-tests--log '(fired)))
      (should (null aaron-lib-tests--hook-2))
      (should (equal aaron-lib-tests--hook (list #'ignore))))))

(ert-deftest aaron-lib-run-hook-on-waits-for-startup-and-predicate ()
  (aaron-lib-tests--with-hooks
    (add-hook 'aaron-lib-tests--hook-2 (lambda () (aaron-lib-tests--push 'fired)))
    (let ((aaron-lib-tests--hook (list #'ignore))
          (ready nil)
          (after-init-time nil))
      (aaron-run-hook-on 'aaron-lib-tests--hook-2 '(aaron-lib-tests--hook)
                      (lambda () ready))
      (run-hooks 'aaron-lib-tests--hook)
      (should (null aaron-lib-tests--log))
      (setq after-init-time (current-time))
      (run-hooks 'aaron-lib-tests--hook)
      (should (null aaron-lib-tests--log))
      (setq ready t)
      (run-hooks 'aaron-lib-tests--hook)
      (should (equal aaron-lib-tests--log '(fired))))))

;;; Advice

(ert-deftest aaron-lib-defadvice-adds-and-undefadvice-removes ()
  (defadvice! aaron-lib-tests--double-a (x)
    "Double X."
    :filter-return #'aaron-lib-tests--target
    (* 2 x))
  (unwind-protect
      (should (= (aaron-lib-tests--target 3) 6))
    (undefadvice! aaron-lib-tests--double-a (x)
      :filter-return #'aaron-lib-tests--target))
  (should (= (aaron-lib-tests--target 3) 3)))

;;; Loading

(ert-deftest aaron-lib-after-runs-for-loaded-features ()
  (aaron-lib-tests--with-hooks
    (after! (:and cl-lib subr-x) (aaron-lib-tests--push 'and))
    (after! (:or aaron-lib-tests--missing cl-lib) (aaron-lib-tests--push 'or))
    (after! (cl-lib) (aaron-lib-tests--push 'list))
    (should (equal (reverse aaron-lib-tests--log) '(and or list)))))

(ert-deftest aaron-lib-after-waits-for-every-feature ()
  (aaron-lib-tests--with-hooks
    (let ((after-load-alist after-load-alist)
          (features features))
      (after! (cl-lib aaron-lib-tests--later) (aaron-lib-tests--push 'both))
      (should (null aaron-lib-tests--log))
      (provide 'aaron-lib-tests--later)
      (should (equal aaron-lib-tests--log '(both))))))

(ert-deftest aaron-lib-defer-until-runs-when-condition-holds ()
  (aaron-lib-tests--with-hooks
    (let ((after-load-functions nil)
          (ready nil))
      (defer-until! ready (aaron-lib-tests--push 'ran))
      (should (null aaron-lib-tests--log))
      (run-hook-with-args 'after-load-functions "x")
      (should (null aaron-lib-tests--log))
      (setq ready t)
      (run-hook-with-args 'after-load-functions "x")
      (should (equal aaron-lib-tests--log '(ran)))
      (should (null after-load-functions)))))

;;; Closures and sugar

(ert-deftest aaron-lib-fn-builds-positional-lambdas ()
  (should (= (funcall (fn! (+ %1 %2)) 1 2) 3))
  (should (= (funcall (fn! (* % 10)) 4) 40))
  (should (= (funcall (fn! %3) 1 2 3) 3))
  (should (equal (funcall (fn! %*) 1 2) '(1 2))))

(ert-deftest aaron-lib-cmd-builds-commands ()
  (let ((cmd (cmd! (aaron-lib-tests--push 'cmd))))
    (should (commandp cmd))
    (aaron-lib-tests--with-hooks
      (funcall cmd)
      (should (equal aaron-lib-tests--log '(cmd))))))

(ert-deftest aaron-lib-cmd-bang-bang-passes-prefix ()
  (let (seen)
    (letf! ((defun aaron-lib-tests--cmd (arg)
              (interactive "P")
              (setq seen arg)))
      (fset 'aaron-lib-tests--cmd-global #'aaron-lib-tests--cmd)
      (call-interactively (cmd!! #'aaron-lib-tests--cmd-global '(4)))
      (should (equal seen '(4))))))

(defvar aaron-lib-tests--flag nil)

(ert-deftest aaron-lib-cmds-dispatches-on-conditions ()
  ;; Conditions are evaluated when the key is pressed, in the global scope.
  (let* ((def (cmds! aaron-lib-tests--flag #'forward-char #'backward-char))
         (filter (plist-get (nthcdr 3 def) :filter)))
    (should (eq (car def) 'menu-item))
    (let ((aaron-lib-tests--flag nil))
      (should (eq (funcall filter) #'backward-char)))
    (let ((aaron-lib-tests--flag t))
      (should (eq (funcall filter) #'forward-char)))))

(ert-deftest aaron-lib-letf-overrides-functions-and-advice ()
  (letf! ((defun! aaron-lib-tests--target (_) 'overridden))
    (should (eq (aaron-lib-tests--target 1) 'overridden)))
  (should (= (aaron-lib-tests--target 1) 1))
  (letf! ((defadvice #'aaron-lib-tests--target :filter-return #'1+))
    (should (= (aaron-lib-tests--target 1) 2)))
  (should (= (aaron-lib-tests--target 1) 1))
  (letf! ((defadvice aaron-lib-tests--target (:around (fn x)) (* 10 (funcall fn x))))
    (should (= (aaron-lib-tests--target 2) 20)))
  (should (= (aaron-lib-tests--target 2) 2))
  (letf! ((defun local-sum (a b) (+ a b)))
    (should (= (local-sum 1 2) 3))))

(ert-deftest aaron-lib-quiet-suppresses-messages ()
  (let ((init-file-debug nil))
    (should (eq (quiet!! (message "hidden") 'done) 'done))))

(defconst aaron-lib-tests--dir (dir!)
  "Directory of this file, expanded while it loads.")

(ert-deftest aaron-lib-dir-names-the-calling-file ()
  (should (string-suffix-p "test/" aaron-lib-tests--dir)))

(provide 'aaron-lib-tests)
;;; aaron-lib-tests.el ends here

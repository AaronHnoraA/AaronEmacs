;;; aaron-modules-tests.el --- Tests for the module system -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make core-test'.  Each test declares modules into a private
;; registry under test-only categories.

;;; Code:

(require 'ert)
(require 'aaron-modules)
(require 'aaron-doctor)
(require 'config)

(defvar aaron-modules-tests--log nil)

(defmacro aaron-modules-tests--with-registry (&rest body)
  "Run BODY with an empty module registry and a scratch load path."
  (declare (indent 0))
  `(let* ((aaron-modules (make-hash-table :test 'equal))
          (aaron-module-ledger (make-hash-table :test 'equal))
          (aaron-module--key-owners (make-hash-table :test 'eq :weakness 'key))
          (aaron-module--file-owners (make-hash-table :test 'equal))
          (aaron-module--path-cache (make-hash-table :test 'equal))
          (aaron-module-key-conflicts nil)
          (aaron-module-load-failures nil)
          (aaron-module-optional-modules nil)
          (aaron-module-deferred-triggers nil)
          (aaron-doctor-results (make-hash-table :test 'equal))
          (aaron-modules-tests--log nil)
          (root (make-temp-file "aaron-modules-" t))
          (aaron-module-load-path (list root))
          (aaron-module-autoloads-file (expand-file-name "autoloads.el" root))
          (load-path (cons root load-path))
          (warning-minimum-level :emergency))
     (unwind-protect (progn ,@body)
       (dolist (g '(:t1 :t2 :t3))
         (setplist g nil))
       (delete-directory root t))))

(defun aaron-modules-tests--feature (root name body)
  "Write feature NAME in ROOT with BODY, then its provide form."
  (with-temp-file (expand-file-name (format "%s.el" name) root)
    (insert ";;; -*- lexical-binding: t; -*-\n" body
            (format "\n(provide '%s)\n" name))))

(defun aaron-modules-tests--dir-module (root group name files)
  "Create directory module GROUP/NAME in ROOT with FILES ((NAME . TEXT))."
  (let ((dir (expand-file-name (format "%s/%s" (substring (symbol-name group) 1) name) root)))
    (make-directory dir t)
    (pcase-dolist (`(,file . ,text) files)
      (let ((path (expand-file-name file dir)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (insert ";;; -*- lexical-binding: t; -*-\n" text)
          (when (equal file "config.el")
            (insert (format "\n(provide '%s/%s/config)\n"
                            (substring (symbol-name group) 1) name))))))
    dir))

;;; Declaration

(ert-deftest aaron-modules-declare-parses-entries-flags-and-properties ()
  (aaron-modules-tests--with-registry
    (aaron! :t1 a (b +x -y :optional t :after (org))
            :t2 (c :autoload (cmd-1 cmd-2) :doctor some-doctor))
    (should (equal (aaron-module-list) '((:t1 . a) (:t1 . b) (:t2 . c))))
    (let ((b (aaron-module-get '(:t1 . b))))
      (should (equal (aaron-module-flags b) '(+x -y)))
      (should (aaron-module-optional b))
      (should (equal (aaron-module-after b) '(org)))
      (should (eq (aaron-module-feature b) 'b)))
    (let ((c (aaron-module-get '(:t2 . c))))
      (should (equal (aaron-module-autoload c) '(cmd-1 cmd-2)))
      (should (eq (aaron-module-doctor c) 'some-doctor)))))

(ert-deftest aaron-modules-declare-conditional-forms ()
  (aaron-modules-tests--with-registry
    (aaron! :t1 (:when t yes-1) (:when nil no-1) (:unless nil yes-2)
            (:if nil no-2 yes-3) (:cond (nil no-3) (t yes-4)))
    (should (equal (mapcar #'cdr (aaron-module-list)) '(yes-1 yes-2 yes-3 yes-4)))))

(ert-deftest aaron-modules-redeclaring-folds-flags ()
  (aaron-modules-tests--with-registry
    (aaron! :t1 (a +x +y))
    (aaron! :t1 (a -x +z))
    (should (equal (aaron-module-flags (aaron-module-get '(:t1 . a))) '(+y -x +z)))
    (should (= (hash-table-count aaron-modules) 1))))

(ert-deftest aaron-modules-modulep-checks-declaration-and-flags ()
  (aaron-modules-tests--with-registry
    (aaron! :t1 (a +x))
    (should (modulep! :t1 a))
    (should (modulep! :t1 a +x))
    (should-not (modulep! :t1 a +y))
    (should (modulep! :t1 a -y))
    (should-not (modulep! :t1 a -x))
    (should-not (modulep! :t1 missing))))

(ert-deftest aaron-modules-modulep-resolves-own-module-from-context ()
  (aaron-modules-tests--with-registry
    (aaron! :t1 (a +x))
    (let ((aaron-module-context (aaron-module-get '(:t1 . a))))
      (should (eval '(modulep! +x) t))
      (should-not (eval '(modulep! +y) t)))))

(ert-deftest aaron-modules-category-depth-puts-foundation-first ()
  (aaron-modules-tests--with-registry
    (let ((aaron-module-category-depths '((:t3 . -100))))
      (aaron! :t1 a :t3 r :t2 b)
      (should (equal (aaron-module-list) '((:t3 . r) (:t1 . a) (:t2 . b)))))))

;;; Loading

(ert-deftest aaron-modules-load-runs-phases-in-order ()
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--feature root 'aaron-mt-feat "(push 'feat aaron-modules-tests--log)")
    (aaron-modules-tests--dir-module
     root :t1 'dirmod
     '(("init.el" . "(push 'dir-init aaron-modules-tests--log)")
       ("config.el" . "(push (list 'dir-config (modulep! +on)) aaron-modules-tests--log)")))
    (aaron! :t1 aaron-mt-feat (dirmod +on))
    (let ((aaron-before-modules-config-hook
           (list (lambda () (push 'before-config aaron-modules-tests--log)))))
      (aaron-modules-load))
    (should (equal (reverse aaron-modules-tests--log)
                   '(dir-init before-config feat (dir-config t))))
    (should (eq (aaron-module-state (aaron-module-get '(:t1 . dirmod))) 'loaded))
    (should (numberp (aaron-module-load-time (aaron-module-get '(:t1 . aaron-mt-feat)))))
    (should (aaron-module-directory-p (aaron-module-get '(:t1 . dirmod))))
    (setq features (delq 'aaron-mt-feat features))))

(ert-deftest aaron-modules-optional-failure-is-recorded-strict-failure-raises ()
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--feature root 'aaron-mt-bad "(error \"broken\")")
    (aaron! :t1 (aaron-mt-bad :optional t))
    (aaron-modules-load)
    (should (equal (alist-get 'aaron-mt-bad aaron-module-load-failures) "broken"))
    (should (eq (aaron-module-state (aaron-module-get '(:t1 . aaron-mt-bad))) 'failed))
    (should (equal aaron-module-optional-modules '(aaron-mt-bad))))
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--feature root 'aaron-mt-bad2 "(error \"broken\")")
    (aaron! :t1 aaron-mt-bad2)
    (should-error (aaron-modules-load))
    (should (eq (aaron-module-state (aaron-module-get '(:t1 . aaron-mt-bad2))) 'failed))))

(ert-deftest aaron-modules-deferred-module-waits-for-trigger ()
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--feature root 'aaron-mt-late "(push 'late aaron-modules-tests--log)")
    (let ((after-load-alist after-load-alist)
          (loaded nil))
      (aaron! :t1 (aaron-mt-late :after aaron-mt-trigger))
      (aaron-modules-load)
      (should (eq (aaron-module-state (aaron-module-get '(:t1 . aaron-mt-late))) 'waiting))
      (should (equal aaron-module-deferred-triggers '((aaron-mt-late aaron-mt-trigger))))
      (cl-letf (((symbol-function 'remote-require-on-client)
                 (lambda (f &rest _) (setq loaded t) (require f))))
        (provide 'aaron-mt-trigger))
      (should loaded)
      (should (eq (aaron-module-state (aaron-module-get '(:t1 . aaron-mt-late))) 'loaded))
      (should (equal aaron-modules-tests--log '(late))))
    (setq features (cl-set-difference features '(aaron-mt-late aaron-mt-trigger)))))

(ert-deftest aaron-modules-autoload-module-only-registers-commands ()
  (aaron-modules-tests--with-registry
    (aaron! :t1 (aaron-mt-lazy :autoload aaron-mt-lazy-command))
    (aaron-modules-load)
    (should (autoloadp (symbol-function 'aaron-mt-lazy-command)))
    (should (eq (aaron-module-state (aaron-module-get '(:t1 . aaron-mt-lazy))) 'autoload))
    (fmakunbound 'aaron-mt-lazy-command)))

(ert-deftest aaron-modules-directory-autoloads-are-generated-and-loaded ()
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--dir-module
     root :t1 'auto
     '(("autoload/cmds.el" . ";;;###autoload\n(defun aaron-mt-auto-cmd () (interactive) 'ok)\n")))
    (aaron! :t1 auto)
    (aaron-modules-load)
    (should (file-exists-p aaron-module-autoloads-file))
    (should (autoloadp (symbol-function 'aaron-mt-auto-cmd)))
    (fmakunbound 'aaron-mt-auto-cmd)))

;;; Manifest

(ert-deftest aaron-modules-manifest-files-add-sources-outside-the-directory ()
  (aaron-modules-tests--with-registry
    (let ((dir (aaron-modules-tests--dir-module root :t1 'withlib '(("config.el" . "")))))
      (with-temp-file (expand-file-name "../lib-a.el" dir) (insert ";; lib\n"))
      (with-temp-file (expand-file-name ".aaronmodule" dir)
        (insert "((files \"../lib-*.el\"))"))
      (aaron! :t1 withlib)
      (should (member (expand-file-name "lib-a.el" (file-name-directory (directory-file-name dir)))
                      (aaron-module-source-files (aaron-module-get '(:t1 . withlib))))))))

;;; Ledger

(ert-deftest aaron-modules-ledger-records-footprint-and-key-conflicts ()
  (aaron-modules-tests--with-registry
    (defvar aaron-mt-map (make-sparse-keymap))
    (defvar aaron-mt-hook nil)
    (aaron-modules-tests--feature
     root 'aaron-mt-one
     "(add-hook 'aaron-mt-hook #'ignore)
      (define-key aaron-mt-map (kbd \"C-c a\") #'forward-char)")
    (aaron-modules-tests--feature
     root 'aaron-mt-two
     "(define-key aaron-mt-map (kbd \"C-c a\") #'backward-char)
      (advice-add 'aaron-mt-target :around #'ignore)")
    (defun aaron-mt-target () nil)
    (aaron! :t1 aaron-mt-one aaron-mt-two)
    (aaron-modules-load)
    (should (equal (aaron-module-ledger '(:t1 . aaron-mt-one) 'hooks)
                   '((aaron-mt-hook . ignore))))
    (should (equal (aaron-module-ledger '(:t1 . aaron-mt-two) 'advice)
                   '((aaron-mt-target :around ignore))))
    (should (= (length aaron-module-key-conflicts) 1))
    (let ((c (car aaron-module-key-conflicts)))
      (should (equal (plist-get c :key) "C-c a"))
      (should (equal (plist-get c :old-module) '(:t1 . aaron-mt-one)))
      (should (eq (plist-get c :new) 'backward-char)))
    (should (member (expand-file-name "aaron-mt-one.el" root)
                    (aaron-module-source-files (aaron-module-get '(:t1 . aaron-mt-one)))))
    ;; The instrumentation is gone once loading ends.
    (should-not (advice-member-p #'aaron-module--ledger-key-a 'define-key))
    (advice-remove 'aaron-mt-target #'ignore)
    (setq features (cl-set-difference features '(aaron-mt-one aaron-mt-two)))))

;;; Doctor

(ert-deftest aaron-modules-doctor-reports-policy-hits-and-doctor-file ()
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--dir-module
     root :t1 'checked
     '(("config.el" . "(defun +checked-f () (file-remote-p default-directory))
(defun +checked-g () (file-remote-p x)) ; aaron-allow: file-remote-p
;; (file-remote-p in-a-comment)
(defun my/legacy () nil)
(setq-local x 'my-property)")
       ("doctor.el" . "(warn! \"needs %s\" \"tool\")
(assert! nil \"broken %d\" 1)")))
    (aaron! :t1 checked)
    (let ((r (aaron-doctor-module '(:t1 . checked))))
      (should (equal (plist-get r :warnings) '("needs tool")))
      (should (equal (plist-get r :errors) '("broken 1")))
      (should (equal (sort (mapcar #'car (plist-get r :violations)) #'string<)
                     '(file-remote-p legacy-name legacy-name))))))

(ert-deftest aaron-modules-doctor-exempts-categories ()
  (aaron-modules-tests--with-registry
    (aaron-modules-tests--dir-module
     root :t1 'remote-ish '(("config.el" . "(file-remote-p default-directory)")))
    (let ((aaron-module-policies
           '((:id file-remote-p :regexp "(file-remote-p\\_>" :message "x"
              :exempt-groups (:t1)))))
      (aaron! :t1 remote-ish)
      (should-not (plist-get (aaron-doctor-module '(:t1 . remote-ish)) :violations)))))

(ert-deftest aaron-modules-doctor-flags-setq-of-config-settings ()
  (aaron-modules-tests--with-registry
    (let ((config--registry (make-hash-table :test 'eq))
          (aaron-doctor--config-setq-cache nil))
      (puthash 'aaron-mt-setting
               '(:name aaron-mt-setting :kind variable :var aaron-mt-setting)
               config--registry)
      (aaron-modules-tests--dir-module
       root :t1 'setter
       '(("config.el" . "(setq aaron-mt-setting 1)
(setq-local aaron-mt-setting 2)
(setq aaron-mt-setting-other 3)
(setq-default aaron-mt-setting 4) ; aaron-allow: config-setq
(setopt aaron-mt-setting 5)
(defun aaron-mt-toggle () (setq aaron-mt-setting 6))
(with-eval-after-load 'x (setq aaron-mt-setting 7))")))
      (aaron! :t1 setter)
      (should (equal (mapcar (lambda (hit) (nth 2 hit))
                             (plist-get (aaron-doctor-module '(:t1 . setter)) :violations))
                     '(2 6 8))))))

;;; Settings

(ert-deftest aaron-modules-config-source-label-is-the-path-under-lisp ()
  (let ((user-emacs-directory "/tmp/cfg/"))
    (should (equal (config-source-label "/tmp/cfg/lisp/tools/lsp/config.el")
                   "tools/lsp/config.el"))
    (should (equal (config-source-label "/tmp/cfg/lisp/tools/lsp/config.elc")
                   "tools/lsp/config.el"))
    (should (equal (config-source-label "/tmp/cfg/etc/x.el") "etc/x.el"))
    (should (equal (config-source-label "/elsewhere/y.el") "y.el"))))

(ert-deftest aaron-module-at-point-reads-declaration-and-module-files ()
  "The module at point comes from the declaration or the module's directory."
  (cl-letf (((symbol-function 'aaron-module-get)
             (lambda (key) (member key '((:tools . lsp) (:emacs . dired))))))
    (with-temp-buffer
      (insert "(aaron!\n :tools  clutch\n         lsp other\n :emacs  dired)\n")
      (goto-char (point-min))
      (search-forward "lsp")
      (backward-char 1)
      (should (equal (aaron-module-at-point) '(:tools . lsp)))
      (search-forward "dired")
      (backward-char 1)
      (should (equal (aaron-module-at-point) '(:emacs . dired)))
      (search-backward "other")
      (should-not (aaron-module-at-point)))
    (with-temp-buffer
      (setq buffer-file-name "/x/emacs/lisp/tools/lsp/config.el")
      (should (equal (aaron-module-at-point) '(:tools . lsp)))
      (setq buffer-file-name nil))))

(provide 'aaron-modules-tests)
;;; aaron-modules-tests.el ends here

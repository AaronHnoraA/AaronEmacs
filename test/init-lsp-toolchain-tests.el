;;; init-lsp-toolchain-tests.el --- Toolchain selector tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-lsp-toolchain)

(define-derived-mode my/toolchain-test-mode prog-mode "Toolchain-Test")

(defmacro my/toolchain-test-with-project (settings &rest body)
  "Evaluate BODY with project-local SETTINGS."
  (declare (indent 1))
  `(let ((my/language-server-toolchain-providers nil)
         (my/language-server-toolchain--overrides (make-hash-table :test #'equal))
         (my/language-server-toolchain--candidate-cache (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'my/project-local-root)
                (lambda () temporary-file-directory))
               ((symbol-function 'my/project-local-value)
                (lambda (key &optional _root) (plist-get ,settings key))))
       ,@body)))

(ert-deftest my/toolchain-project-default-and-session-override ()
  (my/toolchain-test-with-project
      '(:toolchain ((test . second)))
    (my/register-language-server-toolchain-provider
     'test '(my/toolchain-test-mode)
     (lambda (_root)
       '((:id first :label "First" :default t)
         (:id second :label "Second"))))
    (with-temp-buffer
      (my/toolchain-test-mode)
      (should (eq (plist-get (my/language-server-current-toolchain-profile) :id)
                  'second))
      (puthash (my/language-server-toolchain--key) 'first
               my/language-server-toolchain--overrides)
      (should (eq (plist-get (my/language-server-current-toolchain-profile) :id)
                  'first)))))

(ert-deftest my/toolchain-provider-discovery-is-cached-and-refreshable ()
  (my/toolchain-test-with-project nil
    (let ((calls 0))
      (my/register-language-server-toolchain-provider
       'test '(my/toolchain-test-mode)
       (lambda (_root)
         (cl-incf calls)
         '((:id only :label "Only" :default t))))
      (with-temp-buffer
        (my/toolchain-test-mode)
        (my/language-server-toolchain-candidates)
        (my/language-server-toolchain-candidates)
        (should (= calls 1))
        (remhash (my/language-server-toolchain--key)
                 my/language-server-toolchain--candidate-cache)
        (my/language-server-toolchain-candidates)
        (should (= calls 2))))))

(ert-deftest my/toolchain-applies-and-restores-generic-profile ()
  (my/toolchain-test-with-project
      '(:toolchain ((test . selected)))
    (my/register-language-server-toolchain-provider
     'test '(my/toolchain-test-mode)
     (lambda (_root)
       '((:id selected
          :label "Selected"
          :env (("TOOLCHAIN_TEST" . "active"))
          :server-program ("test-language-server" "--stdio")
          :workspace (:test (:enabled t))))))
    (with-temp-buffer
      (my/toolchain-test-mode)
      (setq-local process-environment (copy-sequence process-environment))
      (setenv "TOOLCHAIN_TEST" "original")
      (setq-local my/language-server-toolchain-server-program '("old-server"))
      (setq-local my/language-server--workspace-configuration '(:base (:enabled t)))
      (my/language-server-toolchain-apply-environment)
      (my/language-server-toolchain-apply-lsp-settings)
      (should (equal (getenv "TOOLCHAIN_TEST") "active"))
      (should (equal my/language-server-toolchain-server-program
                     '("test-language-server" "--stdio")))
      (should (equal (plist-get (plist-get my/language-server--workspace-configuration :test)
                                :enabled)
                     t))
      (my/language-server-toolchain-restore-buffer)
      (should (equal (getenv "TOOLCHAIN_TEST") "original"))
      (should (equal my/language-server-toolchain-server-program '("old-server")))
      (should (equal my/language-server--workspace-configuration '(:base (:enabled t)))))))

(ert-deftest my/toolchain-custom-profile-supplies-family-without-provider ()
  (my/toolchain-test-with-project
      '(:toolchain ((custom . local))
        :toolchain-profiles
        ((local . (:label "Local SDK"
                   :family custom
                   :modes (my/toolchain-test-mode)
                   :env (("CUSTOM_SDK" . "yes"))))))
    (with-temp-buffer
      (my/toolchain-test-mode)
      (should (eq (my/language-server-toolchain-family) 'custom))
      (should (eq (plist-get (my/language-server-current-toolchain-profile) :id)
                  'local)))))

(ert-deftest my/toolchain-runtime-profile-is-authoritative ()
  (my/toolchain-test-with-project
      '(:toolchain ((test . configured)))
    (my/register-language-server-toolchain-provider
     'test '(my/toolchain-test-mode)
     (lambda (_root)
       '((:id configured :label "Configured" :default t))))
    (with-temp-buffer
      (my/toolchain-test-mode)
      (setq-local
       my/language-server-runtime-current
       (my/language-server-runtime-create
        :id "kernel"
        :profile '(:id runtime :label "Runtime" :family test)))
      (should (eq (plist-get (my/language-server-current-toolchain-profile) :id)
                  'runtime)))))

(ert-deftest my/toolchain-restores-provider-specific-lsp-variables ()
  (my/toolchain-test-with-project nil
    (my/register-language-server-toolchain-provider
     'test '(my/toolchain-test-mode)
     (lambda (_root)
       '((:id selected :label "Selected" :default t)))
     :apply
     (lambda (_profile _root)
       (my/language-server-toolchain-set-local-variable
        'my/toolchain-test-client-option 'target-value)))
    (with-temp-buffer
      (my/toolchain-test-mode)
      (setq-local my/toolchain-test-client-option 'original)
      (my/language-server-toolchain-apply-environment)
      (should (eq my/toolchain-test-client-option 'target-value))
      (my/language-server-toolchain-restore-buffer)
      (should (local-variable-p 'my/toolchain-test-client-option))
      (should (eq my/toolchain-test-client-option 'original)))))

(provide 'init-lsp-toolchain-tests)
;;; init-lsp-toolchain-tests.el ends here

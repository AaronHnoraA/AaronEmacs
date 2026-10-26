;;; init-lsp-toolchain-tests.el --- Toolchain selector tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/lsp/toolchain)

(define-derived-mode +toolchain-test-mode prog-mode "Toolchain-Test")

(defmacro +toolchain-test-with-project (settings &rest body)
  "Evaluate BODY with project-local SETTINGS."
  (declare (indent 1))
  `(let ((+language-server-toolchain-providers nil)
         (+language-server-toolchain--overrides (make-hash-table :test #'equal))
         (+language-server-toolchain--candidate-cache (make-hash-table :test #'equal))
         (+language-server-toolchain--fast-candidate-cache
          (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function '+project-local-root)
                (lambda () temporary-file-directory))
               ((symbol-function '+project-local-value)
                (lambda (key &optional _root) (plist-get ,settings key))))
       ,@body)))

(ert-deftest +toolchain-project-default-and-session-override ()
  (+toolchain-test-with-project
      '(:toolchain ((test . second)))
    (set-language-server-toolchain-provider!
     'test '(+toolchain-test-mode)
     (lambda (_root)
       '((:id first :label "First" :default t)
         (:id second :label "Second"))))
    (with-temp-buffer
      (+toolchain-test-mode)
      (should (eq (plist-get (+language-server-current-toolchain-profile) :id)
                  'second))
      (puthash (+language-server-toolchain--key) 'first
               +language-server-toolchain--overrides)
      (should (eq (plist-get (+language-server-current-toolchain-profile) :id)
                  'first)))))

(ert-deftest +toolchain-provider-discovery-is-cached-and-refreshable ()
  (+toolchain-test-with-project nil
    (let ((calls 0))
      (set-language-server-toolchain-provider!
       'test '(+toolchain-test-mode)
       (lambda (_root)
         (cl-incf calls)
         '((:id only :label "Only" :default t))))
      (with-temp-buffer
        (+toolchain-test-mode)
        (+language-server-toolchain-candidates)
        (+language-server-toolchain-candidates)
        (should (= calls 1))
        (remhash (+language-server-toolchain--candidate-key
                  (+language-server-toolchain--canonical-root)
                  (+language-server-toolchain-family))
                 +language-server-toolchain--candidate-cache)
        (+language-server-toolchain-candidates)
        (should (= calls 2))
        (set-language-server-toolchain-provider!
         'test '(+toolchain-test-mode)
         (lambda (_root) '((:id replacement :default t))))
        (should (eq (plist-get (car (+language-server-toolchain-candidates))
                               :id)
                    'replacement))))))

(ert-deftest +toolchain-auto-start-skips-expensive-picker-discovery ()
  "Automatic startup uses fast profiles; explicit choices use the full set."
  (let (settings)
    (+toolchain-test-with-project settings
      (let ((fast-calls 0) (full-calls 0))
        (set-language-server-toolchain-provider!
         'test '(+toolchain-test-mode)
         (lambda (_root)
           (cl-incf full-calls)
           '((:id path :default t) (:id conda)))
         :discover-fast
         (lambda (_root)
           (cl-incf fast-calls)
           '((:id path :default t))))
        (with-temp-buffer
          (+toolchain-test-mode)
          (should (eq (plist-get (+language-server-current-toolchain-profile)
                                 :id)
                      'path))
          (should (= fast-calls 1))
          (should (= full-calls 0))
          (should (= (length (+language-server-toolchain-candidates)) 2))
          (should (= full-calls 1))
          (puthash (+language-server-toolchain--key) 'conda
                   +language-server-toolchain--overrides)
          (should (eq (plist-get (+language-server-current-toolchain-profile)
                                 :id)
                      'conda))
          (should (= full-calls 1))
          (remhash (+language-server-toolchain--key)
                   +language-server-toolchain--overrides)
          (setq settings '(:toolchain ((test . conda))))
          (should (eq (plist-get (+language-server-current-toolchain-profile)
                                 :id)
                      'conda))
          (setq settings nil)
          (+language-server/refresh-toolchains)
          (should (eq (plist-get (+language-server-current-toolchain-profile)
                                 :id)
                      'path))
          (should (= fast-calls 2))
          (should (= full-calls 2)))))))

(ert-deftest +toolchain-applies-and-restores-generic-profile ()
  (+toolchain-test-with-project
      '(:toolchain ((test . selected)))
    (set-language-server-toolchain-provider!
     'test '(+toolchain-test-mode)
     (lambda (_root)
       '((:id selected
          :label "Selected"
          :env (("TOOLCHAIN_TEST" . "active"))
          :server-program ("test-language-server" "--stdio")
          :workspace (:test (:enabled t))))))
    (with-temp-buffer
      (+toolchain-test-mode)
      (setq-local process-environment (copy-sequence process-environment))
      (setenv "TOOLCHAIN_TEST" "original")
      (setq-local +language-server-toolchain-server-program '("old-server"))
      (setq-local +language-server--workspace-configuration '(:base (:enabled t)))
      (+language-server-toolchain-apply-environment)
      (+language-server-toolchain-apply-lsp-settings)
      (should (equal (getenv "TOOLCHAIN_TEST") "active"))
      (should (equal +language-server-toolchain-server-program
                     '("test-language-server" "--stdio")))
      (should (equal (plist-get (plist-get +language-server--workspace-configuration :test)
                                :enabled)
                     t))
      (+language-server-toolchain-restore-buffer)
      (should (equal (getenv "TOOLCHAIN_TEST") "original"))
      (should (equal +language-server-toolchain-server-program '("old-server")))
      (should (equal +language-server--workspace-configuration '(:base (:enabled t)))))))

(ert-deftest +toolchain-custom-profile-supplies-family-without-provider ()
  (+toolchain-test-with-project
      '(:toolchain ((custom . local))
        :toolchain-profiles
        ((local . (:label "Local SDK"
                   :family custom
                   :modes (+toolchain-test-mode)
                   :env (("CUSTOM_SDK" . "yes"))))))
    (with-temp-buffer
      (+toolchain-test-mode)
      (should (eq (+language-server-toolchain-family) 'custom))
      (should (eq (plist-get (+language-server-current-toolchain-profile) :id)
                  'local)))))

(ert-deftest +toolchain-runtime-profile-is-authoritative ()
  (+toolchain-test-with-project
      '(:toolchain ((test . configured)))
    (set-language-server-toolchain-provider!
     'test '(+toolchain-test-mode)
     (lambda (_root)
       '((:id configured :label "Configured" :default t))))
    (with-temp-buffer
      (+toolchain-test-mode)
      (setq-local
       +language-server-runtime-current
       (+language-server-runtime-create
        :id "kernel"
        :profile '(:id runtime :label "Runtime" :family test)))
      (should (eq (plist-get (+language-server-current-toolchain-profile) :id)
                  'runtime)))))

(ert-deftest +toolchain-restores-provider-specific-lsp-variables ()
  (+toolchain-test-with-project nil
    (set-language-server-toolchain-provider!
     'test '(+toolchain-test-mode)
     (lambda (_root)
       '((:id selected :label "Selected" :default t)))
     :apply
     (lambda (_profile _root)
       (+language-server-toolchain-set-local-variable
        '+toolchain-test-client-option 'target-value)))
    (with-temp-buffer
      (+toolchain-test-mode)
      (setq-local +toolchain-test-client-option 'original)
      (+language-server-toolchain-apply-environment)
      (should (eq +toolchain-test-client-option 'target-value))
      (+language-server-toolchain-restore-buffer)
      (should (local-variable-p '+toolchain-test-client-option))
      (should (eq +toolchain-test-client-option 'original)))))

(provide 'init-lsp-toolchain-tests)
;;; init-lsp-toolchain-tests.el ends here

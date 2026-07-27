;;; init-lsp-ui-tests.el --- Completion frontend regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-lsp)

(ert-deftest my/company-box-keeps-one-tooltip-frontend ()
  "Company 1.1's child frame must not overlap Company-box."
  (with-temp-buffer
    (setq-local company-box-mode t)
    (setq-local
     company-frontends
     '(company-childframe-unless-just-one-frontend
       company-box-frontend
       company-echo-metadata-frontend
       company-preview-if-just-one-frontend))
    (cl-letf (((symbol-function 'company-childframe-hide) #'ignore))
      (my/company-box-normalize-frontends))
    (should
     (equal company-frontends
            '(company-box-frontend
              company-echo-metadata-frontend
              company-preview-if-just-one-frontend)))))

(provide 'init-lsp-ui-tests)
;;; init-lsp-ui-tests.el ends here

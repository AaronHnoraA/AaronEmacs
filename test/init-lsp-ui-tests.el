;;; init-lsp-ui-tests.el --- Completion frontend regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'flymake)
(require 'init-lsp)
(require 'init-project)
(require 'lsp-mode)

(defun my/lsp-ui-test-object (&rest pairs)
  "Return an LSP hash object initialized from key/value PAIRS."
  (let ((object (make-hash-table :test #'equal)))
    (while pairs
      (puthash (substring (symbol-name (pop pairs)) 1) (pop pairs) object))
    object))

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

(ert-deftest my/lsp-visible-rendering-defaults-are-bounded-and-idle ()
  "LSP decorations should be persistent near windows, not buffer-global."
  (should lsp-lens-enable)
  (should (= lsp-idle-delay 0.35))
  (should (= lsp-lens-debounce-interval 0.35))
  (should-not lsp-update-inlay-hints-on-scroll)
  (should (= my/language-server-visible-render-margin 8))
  (should (eq flymake-show-diagnostics-at-end-of-line 'short)))

(ert-deftest my/lsp-lens-display-filters-to-visible-windows-with-margin ()
  (let* ((before
          (my/lsp-ui-test-object
           :range (my/lsp-ui-test-object
                   :start (my/lsp-ui-test-object :point 5))
           :command "before"))
         (first
          (my/lsp-ui-test-object
           :range (my/lsp-ui-test-object
                   :start (my/lsp-ui-test-object :point 10))
           :command "first"))
         (inside
          (my/lsp-ui-test-object
           :range (my/lsp-ui-test-object
                   :start (my/lsp-ui-test-object :point 17))
           :command "inside"))
         (after
          (my/lsp-ui-test-object
           :range (my/lsp-ui-test-object
                   :start (my/lsp-ui-test-object :point 21))
           :command "after"))
         displayed)
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(window)))
              ((symbol-function 'my/language-server--visible-region)
               (lambda (&optional _window) '(10 . 20)))
              ((symbol-function 'lsp--position-to-point)
               (lambda (position) (gethash "point" position))))
      (my/lsp-lens--display-visible-a
       (lambda (lenses) (setq displayed lenses))
       (list before first inside after)))
    (should
     (equal (mapcar (lambda (lens) (gethash "command" lens)) displayed)
            '("first" "inside")))))

(ert-deftest my/lsp-inlay-hints-request-only-the-visible-warm-region ()
  (let (requested)
    (cl-letf (((symbol-function 'my/language-server--visible-region)
               (lambda (&optional _window) '(40 . 90))))
      (my/lsp-update-inlay-hints-visible-a
       (lambda (start end) (setq requested (cons start end)))
       1 1000))
    (should (equal requested '(40 . 90)))))

(ert-deftest my/lsp-ranged-decoration-responses-render-only-near-windows ()
  (let* ((inside (my/lsp-ui-test-object :range 'inside :name "inside"))
         (outside (my/lsp-ui-test-object :range 'outside :name "outside"))
         rendered)
    (cl-letf (((symbol-function
                'my/language-server--range-visible-with-margin-p)
               (lambda (range) (eq range 'inside))))
      (my/lsp-request-async-visible-decorations-a
       (lambda (_method _params callback &rest _keys)
         (funcall callback (list outside inside)))
       "textDocument/documentColor" nil
       (lambda (result) (setq rendered result))))
    (should (equal (mapcar (lambda (item) (lsp-get item :name)) rendered)
                   '("inside")))))

(ert-deftest my/lsp-semantic-tokens-use-the-shared-visible-warm-region ()
  (let (requested)
    (cl-letf (((symbol-function 'lsp-feature?) (lambda (_feature) t))
              ((symbol-function 'my/language-server--visible-region)
               (lambda (&optional _window) '(30 . 80)))
              ((symbol-function 'lsp--semantic-tokens-request)
               (lambda (region immediate)
                 (setq requested (cons region immediate)))))
      (my/lsp-semantic-tokens-request-visible-a #'ignore))
    (should (equal requested '((30 . 80) . t)))))

(ert-deftest my/lsp-document-color-viewport-refresh-is-idle-and-deduplicated ()
  (let ((my/lsp-document-color-last-visible-region nil)
        (calls 0))
    (cl-letf (((symbol-function 'my/language-server--visible-region)
               (lambda (&optional _window) '(10 . 50)))
              ((symbol-function 'lsp--document-color)
               (lambda () (setq calls (1+ calls)))))
      (my/lsp-document-color-refresh-visible)
      (my/lsp-document-color-refresh-visible))
    (should (= calls 1))))

(ert-deftest my/lsp-imenu-label-retains-exact-symbol-kind ()
  (cl-letf (((symbol-function 'lsp-render-symbol)
             (lambda (_symbol _detailed-p) "Widget")))
    (let ((label
           (my/lsp-imenu--symbol-label
            (my/lsp-ui-test-object :kind 5 :name "Widget"))))
      (should (equal (substring-no-properties label) "Widget"))
      (should (= (get-text-property 0 'my/lsp-symbol-kind label) 5))))
  (should (equal lsp-imenu-sort-methods '(kind position name)))
  (should (eq lsp-imenu-index-function #'my/lsp-imenu-create-vscode-index)))

(ert-deftest my/treemacs-imenu-icons-preserve-kind-hierarchy-and-position ()
  (let* ((class (propertize "Widget" 'my/lsp-symbol-kind 5))
         (method (propertize "run()" 'my/lsp-symbol-kind 6))
         (field (propertize "value" 'my/lsp-symbol-kind 8))
         (result
          (my/treemacs-decorate-imenu-index
           `((,class (,method . 12) (,field . 24)))))
         (class-item (car result))
         (method-item (car (cdr class-item)))
         (field-item (cadr (cdr class-item))))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car class-item)) 5))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car method-item)) 6))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car field-item)) 8))
    (should (= (cdr method-item) 12))
    (should (= (cdr field-item) 24))
    (should (string-suffix-p " Widget" (substring-no-properties (car class-item))))))

(ert-deftest my/treemacs-imenu-native-fallback-distinguishes-members ()
  "Native Imenu remains useful before a language server returns symbols."
  (let* ((result
          (my/treemacs-decorate-imenu-index
           '(("Classes" ("Widget" ("run()" . 12) ("value" . 24))))))
         (category (car result))
         (class-item (car (cdr category)))
         (method-item (car (cdr class-item)))
         (variable-item (cadr (cdr class-item))))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car category)) 5))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car class-item)) 5))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car method-item)) 6))
    (should (= (get-text-property 0 'my/lsp-symbol-kind (car variable-item)) 13))))

(ert-deftest my/treemacs-imenu-outline-is-shallow-and-file-labelled ()
  (let* ((file (make-temp-file "treemacs-outline-" nil ".java"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (let* ((index
                (my/treemacs-get-imenu-index-a
                 (lambda (_file) '(("Widget" . 1)))
                 file))
               (header (caar index))
               (pattern (my/treemacs-imenu-indentation-pattern 6)))
          (should (string-match-p "OUTLINE · treemacs-outline-" header))
          (should (= (get-text-property 0 'my/lsp-symbol-kind header) 1))
          (should (seq-every-p #'string-empty-p (seq-take pattern 6)))
          (should (equal (nth 6 pattern) "  ")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(provide 'init-lsp-ui-tests)
;;; init-lsp-ui-tests.el ends here

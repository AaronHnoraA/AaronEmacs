;;; init-lsp-ui-tests.el --- Completion frontend regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'flymake)
(require 'init-lsp)
(require 'init-project)
(require 'init-tabbar)
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

(ert-deftest my/lsp-tab-line-tabs-overlay-the-view-only-breadcrumb ()
  "Breadcrumb text must not reduce the width available to buffer tabs."
  (with-temp-buffer
    (let* ((map (make-sparse-keymap))
           (tab (propertize "CURRENT" 'local-map my/tab-line-click-map))
           (breadcrumb
            (propertize (make-string 120 ?x)
                        'face 'font-lock-keyword-face
                        'local-map map
                        'mouse-face 'highlight)))
      (setq-local my/tab-line-leading-segment-functions
                  (list (lambda () breadcrumb)))
      (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
                ((symbol-function 'my/tab-line-hidden-p) (lambda () nil))
                ((symbol-function 'my/tab-line-content) (lambda () (list tab))))
        (let* ((rendered (my/tab-line-format))
               (leading (car rendered)))
          (should (= (string-width leading) 36))
          (should (equal (cdr rendered) (list tab)))
          (should-not (get-text-property 0 'local-map leading))
          (should-not (get-text-property 0 'mouse-face leading))
          (should (eq (get-text-property 0 'face leading)
                      'font-lock-keyword-face))
          (should (eq (get-text-property 0 'local-map tab)
                      my/tab-line-click-map)))))))

(ert-deftest my/lsp-breadcrumb-starts-directly-in-the-tab-line ()
  "The upstream header renderer must never become the real buffer value."
  (require 'lsp-headerline)
  (with-temp-buffer
    (setq-local lsp-managed-mode t)
    (setq-local lsp-headerline-breadcrumb-mode nil)
    (setq-local header-line-format "application header")
    (my/lsp-headerline-breadcrumb-mode-a
     (lambda (&optional _argument)
       (setq lsp-headerline-breadcrumb-mode t
             header-line-format (list my/lsp-tab-line-header-entry))))
    (should (equal header-line-format "application header"))
    (should
     (memq #'my/lsp-tab-line-breadcrumb
           my/tab-line-leading-segment-functions))))

(ert-deftest my/generic-breadcrumb-also-starts-directly-in-the-tab-line ()
  "The pre-LSP provider must never flash in header-line on file entry."
  (require 'breadcrumb)
  (with-temp-buffer
    (setq-local breadcrumb-local-mode nil)
    (setq-local header-line-format "application header")
    (breadcrumb-local-mode 1)
    (should breadcrumb-local-mode)
    (should (equal header-line-format "application header"))
    (should
     (memq #'my/breadcrumb-tab-line-content
           my/tab-line-leading-segment-functions))))

(ert-deftest my/lsp-tab-line-breadcrumb-is-view-only ()
  "Moving the breadcrumb must remove its navigation and hover behavior."
  (let* ((map (make-sparse-keymap))
         (breadcrumb
          (propertize "project > symbol"
                      'face 'font-lock-function-name-face
                      'local-map map
                      'keymap map
                      'mouse-face 'highlight
                      'help-echo "jump"
                      'follow-link t
                      'pointer 'hand))
         (rendered (my/breadcrumb-view-only-string breadcrumb)))
    (dolist (property
             '(local-map keymap mouse-face help-echo follow-link pointer))
      (should-not (get-text-property 0 property rendered)))
    (should (eq (get-text-property 0 'face rendered)
                'font-lock-function-name-face))))

(ert-deftest my/lsp-tab-line-breadcrumb-has-only-the-treemacs-click-action ()
  "Breadcrumb clicks must not inherit any buffer-tab behavior."
  (let* ((upstream-map (make-sparse-keymap))
         (breadcrumb
          (propertize "project > symbol"
                      'local-map upstream-map
                      'mouse-face 'header-line-highlight)))
    (setq-local my/tab-line-leading-segment-functions
                (list
                 (lambda ()
                   (my/breadcrumb-tab-line-action-string breadcrumb))))
    (let* ((rendered (my/tab-line-leading-content))
           (map (get-text-property 0 'local-map rendered)))
      (should (keymapp map))
      (should
       (eq (lookup-key map [tab-line mouse-1])
           #'my/show-imenu-from-breadcrumb))
      (should-not (lookup-key map [tab-line mouse-2]))
      (should-not (lookup-key map [tab-line mouse-3]))
      (should-not (lookup-key map [tab-line wheel-up]))
      (should-not (eq map upstream-map)))))

(ert-deftest my/treemacs-follow-keeps-a-successful-imenu-node-selected ()
  "A nil upstream return after tag movement must not fall back to the file."
  (my/project-ensure-treemacs)
  (save-window-excursion
    (let* ((source-window (selected-window))
           (source-buffer (current-buffer))
           (treemacs-window (split-window-right))
           (treemacs-buffer (generate-new-buffer " *treemacs-follow-test*"))
           (tag-calls 0)
           (file-calls 0)
           (calls nil))
      (unwind-protect
          (progn
            (set-window-buffer treemacs-window treemacs-buffer)
            (select-window source-window)
            (with-current-buffer source-buffer
              (setq-local buffer-file-name "/tmp/source.java")
              (cl-letf
                   (((symbol-function 'treemacs-get-local-window)
                    (lambda ()
                      (push 'window calls)
                      treemacs-window))
                   ((symbol-function 'my/treemacs-follow-path)
                    (lambda ()
                      (push 'path calls)
                      "/tmp/source.java"))
                   ((symbol-function 'my/treemacs-project-for-path)
                    (lambda (_path)
                      (push 'project calls)
                      'project))
                   ((symbol-function 'my/treemacs-safe-imenu-index)
                    (lambda ()
                      (push 'index calls)
                      'index))
                   ((symbol-function 'treemacs--find-index-pos)
                    (lambda (_point _index)
                      (push 'tag-path calls)
                      'tag-path))
                   ((symbol-function 'treemacs--do-follow-tag)
                    (lambda (&rest _)
                      (setq tag-calls (1+ tag-calls))
                      ;; This is the real upstream return when recentering is
                      ;; disabled, even though the cursor movement succeeded.
                      nil))
                   ((symbol-function 'treemacs-goto-file-node)
                    (lambda (&rest _)
                      (setq file-calls (1+ file-calls))
                      t))
                   ((symbol-function 'hl-line-highlight) #'ignore)
                   ((symbol-function 'force-window-update) #'ignore))
                (let ((result (my/treemacs-follow-source-silently t)))
                  (should
                   (equal (reverse calls)
                          '(window path project index tag-path)))
                  (should (= tag-calls 1))
                  (should (= file-calls 0))
                  (should (eq result 'tag)))))
            (should (= tag-calls 1))
            (should (= file-calls 0)))
        (when (buffer-live-p treemacs-buffer)
          (kill-buffer treemacs-buffer))))))

(ert-deftest my/show-imenu-current-symbol-pulses-the-treemacs-cursor ()
  (save-window-excursion
    (let* ((source-window (selected-window))
           (treemacs-window (split-window-right))
           (treemacs-buffer (generate-new-buffer " *treemacs-pulse-test*"))
           (pulses 0))
      (unwind-protect
          (progn
            (set-window-buffer treemacs-window treemacs-buffer)
            (select-window source-window)
            (cl-letf (((symbol-function 'hl-line-highlight) #'ignore)
                      ((symbol-function 'treemacs-pulse-on-success)
                       (lambda (&rest _) (setq pulses (1+ pulses)))))
              (should
               (eq (my/treemacs-focus-and-pulse-current-node treemacs-window)
                   treemacs-window)))
            (should (eq (selected-window) treemacs-window))
            (should (= pulses 1)))
        (when (buffer-live-p treemacs-buffer)
          (kill-buffer treemacs-buffer))))))

(ert-deftest my/show-imenu-breadcrumb-click-ensures-open-without-toggling ()
  (save-window-excursion
    (let ((treemacs-window nil)
          (treemacs-buffer (generate-new-buffer " *treemacs-open-test*"))
          (opens 0)
          (follows 0)
          (focuses 0)
          (restores 0))
      (unwind-protect
          (cl-letf
              (((symbol-function 'my/project-ensure-treemacs) #'ignore)
               ((symbol-function 'my/show-imenu-target-root)
                (lambda () "/tmp/"))
               ((symbol-function 'treemacs-get-local-window)
                (lambda () treemacs-window))
               ((symbol-function 'my/show-imenu-enable-treemacs-modes) #'ignore)
               ((symbol-function
                 'treemacs-add-and-display-current-project-exclusively)
                (lambda ()
                  (setq opens (1+ opens)
                        treemacs-window (split-window-right))
                  (set-window-buffer treemacs-window treemacs-buffer)))
               ((symbol-function 'my/treemacs-follow-source-silently)
                (lambda (&optional _prefer-tag)
                  (setq follows (1+ follows))
                  'tag))
               ((symbol-function 'my/show-imenu-restore-breadcrumb)
                (lambda (_buffer) (setq restores (1+ restores))))
               ((symbol-function 'my/treemacs-focus-and-pulse-current-node)
                (lambda (window)
                  (should (eq window treemacs-window))
                  (setq focuses (1+ focuses)))))
            (my/show-imenu-open-current-symbol)
            (my/show-imenu-open-current-symbol)
            (should (= opens 1))
            (should (= follows 2))
            (should (= focuses 2))
            (should (= restores 1)))
        (when (buffer-live-p treemacs-buffer)
          (kill-buffer treemacs-buffer))))))

(ert-deftest my/show-imenu-refresh-does-not-revive-generic-lsp-header ()
  (require 'breadcrumb)
  (with-temp-buffer
    (prog-mode)
    (setq-local lsp-managed-mode t)
    (breadcrumb-local-mode -1)
    (cl-letf (((symbol-function 'imenu--make-index-alist) #'ignore)
              ((symbol-function 'lsp-headerline-check-breadcrumb) #'ignore))
      (my/show-imenu-refresh-breadcrumb (current-buffer)))
    (should-not breadcrumb-local-mode)
    (should-not
     (memq #'my/breadcrumb-tab-line-content
           my/tab-line-leading-segment-functions))))

(ert-deftest my/lsp-tab-line-colors-use-readable-semantic-tokens ()
  (require 'lsp-headerline)
  (my/lsp-tab-line-apply-ui)
  (should
   (equal (face-foreground 'lsp-headerline-breadcrumb-path-face nil t)
          (aaron-ui-color 'fg-dim)))
  (should
   (equal (face-foreground 'lsp-headerline-breadcrumb-symbols-face nil t)
          (aaron-ui-color 'fg-soft))))

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

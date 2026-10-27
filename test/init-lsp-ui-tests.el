;;; init-lsp-ui-tests.el --- Completion frontend regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'flymake)
(require 'tools/lsp/config)
(require 'tools/project/config)
(require 'ui/tabbar/config)
(require 'lsp-mode)

(ert-deftest +company-yasnippet-ignores-capf-documentation-requests ()
  "Grouped backends must not generate empty snippet previews for directives."
  (require 'company-yasnippet)
  (cl-letf (((symbol-function 'company-doc-buffer)
             (lambda (&rest _) (ert-fail "CAPF candidate generated a snippet preview"))))
    (should-not (company-yasnippet 'doc-buffer "@@skill"))
    (should-not (company-yasnippet 'doc-buffer ""))))

(defun +lsp-ui-test-object (&rest pairs)
  "Return an LSP object from key/value PAIRS in the active format."
  (if lsp-use-plists
      pairs
    (let ((object (make-hash-table :test #'equal)))
      (while pairs
        (puthash (substring (symbol-name (pop pairs)) 1) (pop pairs) object))
      object)))

(ert-deftest +company-box-keeps-one-tooltip-frontend ()
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
      (+company-box-normalize-frontends))
    (should
     (equal company-frontends
            '(company-box-frontend
              company-echo-metadata-frontend
              company-preview-if-just-one-frontend)))))

(ert-deftest +lsp-visible-rendering-defaults-are-bounded-and-idle ()
  "LSP decorations should be persistent near windows, not buffer-global."
  (should lsp-lens-enable)
  (should (= lsp-idle-delay 0.35))
  (should (= lsp-lens-debounce-interval 0.35))
  (should-not lsp-update-inlay-hints-on-scroll)
  (should (= +language-server-visible-render-margin 8))
  (should (eq flymake-show-diagnostics-at-end-of-line 'short)))

(ert-deftest +company-tab-routing-matches-the-pre-lsp-mode-workflow ()
  "Company selects its candidate; otherwise TAB remains ordinary indentation."
  (should (eq (lookup-key company-active-map [tab])
              #'company-complete-selection))
  (should (eq tab-always-indent t)))

(ert-deftest +lsp-escape-dismisses-hover-before-editor-state ()
  "A visible lsp-ui doc consumes one Escape and hides immediately."
  (let (hidden)
    (cl-letf (((symbol-function 'lsp-ui-doc--visible-p) (lambda () t))
              ((symbol-function 'lsp-ui-doc-hide)
               (lambda () (setq hidden t))))
      (should (+lsp-dismiss-popup-h))
      (should hidden))))

(ert-deftest +lsp-escape-falls-through-without-transient-ui ()
  "Escape remains available to Evil/editor state when no LSP popup is live."
  (let ((lsp-signature-mode nil)
        (lsp-ui-peek-mode nil))
    (cl-letf (((symbol-function 'lsp-ui-doc--visible-p) (lambda () nil)))
      (should-not (+lsp-dismiss-popup-h)))))

(ert-deftest +citre-navigation-routes-through-lsp-mode-not-eglot ()
  "Citre fallback and peek must use the sole supported LSP client."
  (require 'citre)
  (should (equal citre-find-definition-backends '(lsp-mode tags global)))
  (should (equal citre-find-reference-backends '(lsp-mode global)))
  (with-temp-buffer
    (setq-local lsp-managed-mode t)
    (should (citre-backend-usable-p 'lsp-mode))))

(ert-deftest +citre-remote-helper-lookup-follows-search-path-changes ()
  "A missing helper must not stay missing after the target PATH changes."
  (require 'remote-process)
  (let ((default-directory "/fs:lab:/tmp/")
        (+citre--executable-cache (make-hash-table :test #'equal))
        (paths '("/opt/one"))
        (exec-suffixes '(""))
        (comp-enable-subr-trampolines nil)
        (remote-calls 0)
        (fallback-calls 0)
        seen-path)
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest _) "/fs:lab:"))
              ((symbol-function 'find-file-name-handler)
               (lambda (_file operation)
                 (and (eq operation 'exec-path)
                      (lambda (_operation) paths))))
              ((symbol-function 'remote-context) (lambda (&rest _) nil))
              ((symbol-function 'remote-exec)
               (lambda (_program &rest options)
                 (cl-incf remote-calls)
                 (setq seen-path
                       (alist-get "PATH" (plist-get options :environment)
                                  nil nil #'equal))
                 (remote-exec-result-create
                  :status 0
                  :stdout (if (equal paths '("/opt/two"))
                              "/opt/two/readtags\n"
                            "")))))
      (let ((fallback (lambda (&rest _)
                        (cl-incf fallback-calls)
                        'fallback)))
        (should-not
         (+citre-executable-find-a fallback "readtags" t))
        (should-not
         (+citre-executable-find-a fallback "readtags" t))
        (should (= remote-calls 1))
        (setq paths '("/opt/two"))
        (should
         (equal (+citre-executable-find-a fallback "readtags" t)
                "/opt/two/readtags"))
        (should (equal seen-path "/opt/two"))
        (should (= remote-calls 2))
        (should (= fallback-calls 0))
        (setq paths '("/bad:colon"))
        (should (eq (+citre-executable-find-a fallback "readtags" t)
                    'fallback))
        (should (= fallback-calls 1))))))

(ert-deftest +lsp-client-whitelist-rejections-are-quiet ()
  "Disabled clients must not run remote activation probes or log failures."
  (let ((lsp-enabled-clients '(jdtls))
        (lsp--show-message t)
        seen
        (calls 0))
    (cl-letf (((symbol-function 'lsp--client-server-id)
               (lambda (client) client)))
      (+lsp-mode--quiet-client-whitelist-a
       (lambda (_client)
         (cl-incf calls)
         (setq seen lsp--show-message))
       'semgrep-ls)
      (should (= calls 0))
      (should-not seen)
      (+lsp-mode--quiet-client-whitelist-a
       (lambda (_client)
         (cl-incf calls)
         (setq seen lsp--show-message))
       'jdtls)
      (should (= calls 1))
      (should seen))))

(ert-deftest +lsp-tab-line-tabs-overlay-the-view-only-breadcrumb ()
  "Breadcrumb text must not reduce the width available to buffer tabs."
  (with-temp-buffer
    (let* ((map (make-sparse-keymap))
           (tab (propertize "CURRENT" 'local-map +tab-line-click-map))
           (breadcrumb
            (propertize (make-string 120 ?x)
                        'face 'font-lock-keyword-face
                        'local-map map
                        'mouse-face 'highlight)))
      (setq-local +tab-line-leading-segment-functions
                  (list (lambda () breadcrumb)))
      (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
                ((symbol-function '+tab-line-hidden-p) (lambda () nil))
                ((symbol-function '+tab-line-content) (lambda () (list tab))))
        (let* ((rendered (+tab-line-format))
               (leading-left (car rendered))
               (leading-right (car (last rendered))))
          (should (= (string-width leading-left) 36))
          (should (equal (nth 1 rendered) tab))
          (should (= (string-width leading-right) 37))
          (should (= (+tab-line-segments-width rendered) 80))
          (should-not (get-text-property 0 'local-map leading-left))
          (should-not (get-text-property 0 'mouse-face leading-left))
          (should-not (get-text-property 0 'local-map leading-right))
          (should-not (get-text-property 0 'mouse-face leading-right))
          (should (eq (get-text-property 0 'face leading-left)
                      'font-lock-keyword-face))
          (should (eq (get-text-property 0 'face leading-right)
                      'font-lock-keyword-face))
          (should (eq (get-text-property 0 'local-map tab)
                      +tab-line-click-map)))))))

(ert-deftest +lsp-breadcrumb-starts-directly-in-the-tab-line ()
  "The upstream header renderer must never become the real buffer value."
  (require 'lsp-headerline)
  (with-temp-buffer
    (setq-local lsp-managed-mode t)
    (setq-local lsp-headerline-breadcrumb-mode nil)
    (setq-local header-line-format "application header")
    (+lsp-headerline-breadcrumb-mode-a
     (lambda (&optional _argument)
       (setq lsp-headerline-breadcrumb-mode t
             header-line-format (list +lsp-tab-line-header-entry))))
    (should (equal header-line-format "application header"))
    (should
     (memq #'+lsp-tab-line-breadcrumb
           +tab-line-leading-segment-functions))))

(ert-deftest +lsp-target-only-breadcrumb-skips-workspace-root-on-idle ()
  "A remote breadcrumb must not synchronously test the project root."
  (require 'lsp-headerline)
  (with-temp-buffer
    (setq buffer-file-name "/fs:host:/project/source.py")
    (setq-local lsp-managed-mode t
                lsp-inlay-hint-enable nil
                lsp-enable-text-document-color nil)
    (cl-letf (((symbol-function 'remote-client-file-name)
               (lambda (&rest _) nil)))
      (+lsp-managed-mode-setup))
    (should (equal lsp-headerline-breadcrumb-segments '(file symbols)))
    (cl-letf (((symbol-function 'lsp-workspace-root)
               (lambda (&rest _) (ert-fail "Remote root was queried")))
              ((symbol-function 'lsp-headerline--build-symbol-string)
               (lambda () "symbol")))
      (should (string-match-p "source.py"
                              (lsp-headerline--build-string))))))

(ert-deftest +lsp-client-accessible-breadcrumb-keeps-project-segment ()
  "A shared filesystem retains the full upstream breadcrumb."
  (with-temp-buffer
    (setq buffer-file-name "/fs:local:/tmp/source.py")
    (setq-local lsp-managed-mode t
                lsp-inlay-hint-enable nil
                lsp-enable-text-document-color nil
                lsp-headerline-breadcrumb-segments '(project file symbols))
    (cl-letf (((symbol-function 'remote-client-file-name)
               (lambda (&rest _) "/tmp/source.py")))
      (+lsp-managed-mode-setup))
    (should (equal lsp-headerline-breadcrumb-segments
                   '(project file symbols)))))

(ert-deftest +generic-breadcrumb-also-starts-directly-in-the-tab-line ()
  "The pre-LSP provider must never flash in header-line on file entry."
  (require 'breadcrumb)
  (with-temp-buffer
    (setq-local breadcrumb-local-mode nil)
    (setq-local header-line-format "application header")
    (breadcrumb-local-mode 1)
    (should breadcrumb-local-mode)
    (should (equal header-line-format "application header"))
    (should
     (memq #'+breadcrumb-tab-line-content
           +tab-line-leading-segment-functions))))

(ert-deftest +lsp-tab-line-breadcrumb-is-view-only ()
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
         (rendered (+breadcrumb-view-only-string breadcrumb)))
    (dolist (property
             '(local-map keymap mouse-face help-echo follow-link pointer))
      (should-not (get-text-property 0 property rendered)))
    (should (eq (get-text-property 0 'face rendered)
                'font-lock-function-name-face))))

(ert-deftest +lsp-tab-line-breadcrumb-has-only-the-treemacs-click-action ()
  "Breadcrumb clicks must not inherit any buffer-tab behavior."
  (let* ((upstream-map (make-sparse-keymap))
         (breadcrumb
          (propertize (concat "project > symbol > " (make-string 60 ?x))
                      'local-map upstream-map
                      'mouse-face 'header-line-highlight)))
    (setq-local +tab-line-leading-segment-functions
                (list
                 (lambda ()
                   (+breadcrumb-tab-line-action-string breadcrumb))))
    (let* ((rendered (+tab-line-leading-content))
           (map (get-text-property 0 'local-map rendered)))
      (should (keymapp map))
      (should
       (eq (lookup-key map [tab-line mouse-1])
           #'+project/show-imenu-from-breadcrumb))
      (should-not (lookup-key map [tab-line mouse-2]))
      (should-not (lookup-key map [tab-line mouse-3]))
      (should-not (lookup-key map [tab-line wheel-up]))
      (should-not (eq map upstream-map))
      (let* ((tab (propertize "TAB" 'local-map +tab-line-click-map))
             (overlaid
              (+tab-line-overlay-leading-content
               rendered (list "alignment" tab) 40))
             (right (car (last overlaid)))
             (right-map (get-text-property 0 'local-map right)))
        (should (equal (nth 1 overlaid) tab))
        (should
         (eq (lookup-key right-map [tab-line mouse-1])
             #'+project/show-imenu-from-breadcrumb))
        (should-not (lookup-key right-map [tab-line mouse-2]))))))

(ert-deftest +treemacs-follow-keeps-a-successful-imenu-node-selected ()
  "A nil upstream return after tag movement must not fall back to the file."
  (+project-ensure-treemacs)
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
                   ((symbol-function '+treemacs-follow-path)
                    (lambda ()
                      (push 'path calls)
                      "/tmp/source.java"))
                   ((symbol-function '+treemacs-project-for-path)
                    (lambda (_path)
                      (push 'project calls)
                      'project))
                   ((symbol-function '+treemacs-safe-imenu-index)
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
                (let ((result (+treemacs-follow-source-silently t)))
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

(ert-deftest +show-imenu-current-symbol-pulses-the-treemacs-cursor ()
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
               (eq (+treemacs-focus-and-pulse-current-node treemacs-window)
                   treemacs-window)))
            (should (eq (selected-window) treemacs-window))
            (should (= pulses 1)))
        (when (buffer-live-p treemacs-buffer)
          (kill-buffer treemacs-buffer))))))

(ert-deftest +show-imenu-breadcrumb-click-ensures-open-without-toggling ()
  (save-window-excursion
    (let ((treemacs-window nil)
          (treemacs-buffer (generate-new-buffer " *treemacs-open-test*"))
          (opens 0)
          (follows 0)
          (focuses 0)
          (restores 0))
      (unwind-protect
          (cl-letf
              (((symbol-function '+project-ensure-treemacs) #'ignore)
               ((symbol-function '+show-imenu-target-root)
                (lambda () "/tmp/"))
               ((symbol-function 'treemacs-get-local-window)
                (lambda () treemacs-window))
               ((symbol-function '+show-imenu-enable-treemacs-modes) #'ignore)
               ((symbol-function
                 'treemacs-add-and-display-current-project-exclusively)
                (lambda ()
                  (setq opens (1+ opens)
                        treemacs-window (split-window-right))
                  (set-window-buffer treemacs-window treemacs-buffer)))
               ((symbol-function '+treemacs-follow-source-silently)
                (lambda (&optional _prefer-tag)
                  (setq follows (1+ follows))
                  'tag))
               ((symbol-function '+show-imenu-restore-breadcrumb)
                (lambda (_buffer) (setq restores (1+ restores))))
               ((symbol-function '+treemacs-focus-and-pulse-current-node)
                (lambda (window)
                  (should (eq window treemacs-window))
                  (setq focuses (1+ focuses)))))
            (+project/show-imenu-open-current-symbol)
            (+project/show-imenu-open-current-symbol)
            (should (= opens 1))
            (should (= follows 2))
            (should (= focuses 2))
            (should (= restores 1)))
        (when (buffer-live-p treemacs-buffer)
          (kill-buffer treemacs-buffer))))))

(ert-deftest +show-imenu-refresh-does-not-revive-generic-lsp-header ()
  (require 'breadcrumb)
  (with-temp-buffer
    (prog-mode)
    (setq-local lsp-managed-mode t)
    (breadcrumb-local-mode -1)
    (cl-letf (((symbol-function 'imenu--make-index-alist) #'ignore)
              ((symbol-function 'lsp-headerline-check-breadcrumb) #'ignore))
      (+show-imenu-refresh-breadcrumb (current-buffer)))
    (should-not breadcrumb-local-mode)
    (should-not
     (memq #'+breadcrumb-tab-line-content
           +tab-line-leading-segment-functions))))

(ert-deftest +lsp-tab-line-colors-use-readable-semantic-tokens ()
  (require 'lsp-headerline)
  (+lsp-tab-line-apply-ui)
  (should
   (equal (face-foreground 'lsp-headerline-breadcrumb-path-face nil t)
          (aaron-ui-color 'fg-dim)))
  (should
   (equal (face-foreground 'lsp-headerline-breadcrumb-symbols-face nil t)
          (aaron-ui-color 'fg-soft))))

(ert-deftest +lsp-lens-display-filters-to-visible-windows-with-margin ()
  (let* ((before
          (+lsp-ui-test-object
           :range (+lsp-ui-test-object
                   :start (+lsp-ui-test-object :point 5))
           :command "before"))
         (first
          (+lsp-ui-test-object
           :range (+lsp-ui-test-object
                   :start (+lsp-ui-test-object :point 10))
           :command "first"))
         (inside
          (+lsp-ui-test-object
           :range (+lsp-ui-test-object
                   :start (+lsp-ui-test-object :point 17))
           :command "inside"))
         (after
          (+lsp-ui-test-object
           :range (+lsp-ui-test-object
                   :start (+lsp-ui-test-object :point 21))
           :command "after"))
         displayed)
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) '(window)))
              ((symbol-function '+language-server--visible-region)
               (lambda (&optional _window) '(10 . 20)))
              ((symbol-function 'lsp--position-to-point)
               (lambda (position) (lsp-get position :point))))
      (+lsp-lens--display-visible-a
       (lambda (lenses) (setq displayed lenses))
       (list before first inside after)))
    (should
     (equal (mapcar (lambda (lens) (lsp-get lens :command)) displayed)
            '("first" "inside")))))

(ert-deftest +lsp-inlay-hints-request-only-the-visible-warm-region ()
  (let (requested)
    (cl-letf (((symbol-function '+language-server--visible-region)
               (lambda (&optional _window) '(40 . 90))))
      (+lsp-update-inlay-hints-visible-a
       (lambda (start end) (setq requested (cons start end)))
       1 1000))
    (should (equal requested '(40 . 90)))))

(ert-deftest +lsp-ranged-decoration-responses-render-only-near-windows ()
  (let* ((inside (+lsp-ui-test-object :range 'inside :name "inside"))
         (outside (+lsp-ui-test-object :range 'outside :name "outside"))
         rendered)
    (cl-letf (((symbol-function
                '+language-server--range-visible-with-margin-p)
               (lambda (range) (eq range 'inside))))
      (+lsp-request-async-visible-decorations-a
       (lambda (_method _params callback &rest _keys)
         (funcall callback (list outside inside)))
       "textDocument/documentColor" nil
       (lambda (result) (setq rendered result))))
    (should (equal (mapcar (lambda (item) (lsp-get item :name)) rendered)
                   '("inside")))))

(ert-deftest +lsp-semantic-tokens-use-the-shared-visible-warm-region ()
  (let (requested)
    (cl-letf (((symbol-function 'lsp-feature?) (lambda (_feature) t))
              ((symbol-function '+language-server--visible-region)
               (lambda (&optional _window) '(30 . 80)))
              ((symbol-function 'lsp--semantic-tokens-request)
               (lambda (region immediate)
                 (setq requested (cons region immediate)))))
      (+lsp-semantic-tokens-request-visible-a #'ignore))
    (should (equal requested '((30 . 80) . t)))))

(ert-deftest +lsp-document-color-viewport-refresh-is-idle-and-deduplicated ()
  (let ((+lsp-document-color-last-visible-region nil)
        (calls 0))
    (cl-letf (((symbol-function '+language-server--visible-region)
               (lambda (&optional _window) '(10 . 50)))
              ((symbol-function 'lsp--document-color)
               (lambda () (setq calls (1+ calls)))))
      (+lsp-document-color-refresh-visible)
      (+lsp-document-color-refresh-visible))
    (should (= calls 1))))

(ert-deftest +lsp-imenu-label-retains-exact-symbol-kind ()
  (cl-letf (((symbol-function 'lsp-render-symbol)
             (lambda (_symbol _detailed-p) "Widget")))
    (let ((label
           (+lsp-imenu--symbol-label
            (+lsp-ui-test-object :kind 5 :name "Widget"))))
      (should (equal (substring-no-properties label) "Widget"))
      (should (= (get-text-property 0 '+lsp-symbol-kind label) 5))))
  (should (equal lsp-imenu-sort-methods '(kind position name)))
  (should (eq lsp-imenu-index-function #'+lsp-imenu-create-vscode-index)))

(ert-deftest +treemacs-imenu-icons-preserve-kind-hierarchy-and-position ()
  (let* ((class (propertize "Widget" '+lsp-symbol-kind 5))
         (method (propertize "run()" '+lsp-symbol-kind 6))
         (field (propertize "value" '+lsp-symbol-kind 8))
         (result
          (+treemacs-decorate-imenu-index
           `((,class (,method . 12) (,field . 24)))))
         (class-item (car result))
         (method-item (car (cdr class-item)))
         (field-item (cadr (cdr class-item))))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car class-item)) 5))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car method-item)) 6))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car field-item)) 8))
    (should (= (cdr method-item) 12))
    (should (= (cdr field-item) 24))
    (should (string-suffix-p " Widget" (substring-no-properties (car class-item))))))

(ert-deftest +treemacs-imenu-native-fallback-distinguishes-members ()
  "Native Imenu remains useful before a language server returns symbols."
  (let* ((result
          (+treemacs-decorate-imenu-index
           '(("Classes" ("Widget" ("run()" . 12) ("value" . 24))))))
         (category (car result))
         (class-item (car (cdr category)))
         (method-item (car (cdr class-item)))
         (variable-item (cadr (cdr class-item))))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car category)) 5))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car class-item)) 5))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car method-item)) 6))
    (should (= (get-text-property 0 '+lsp-symbol-kind (car variable-item)) 13))))

(ert-deftest +treemacs-imenu-outline-is-shallow-and-file-labelled ()
  (let* ((file (make-temp-file "treemacs-outline-" nil ".java"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (let* ((index
                (+treemacs-get-imenu-index-a
                 (lambda (_file) '(("Widget" . 1)))
                 file))
               (header (caar index))
               (pattern (+treemacs-imenu-indentation-pattern 6)))
          (should (string-match-p "OUTLINE · treemacs-outline-" header))
          (should (= (get-text-property 0 '+lsp-symbol-kind header) 1))
          (should (seq-every-p #'string-empty-p (seq-take pattern 6)))
          (should (equal (nth 6 pattern) "  ")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest +language-server-upstream-id-reaches-lsp-mode-signature ()
  "A server registered under its own id gets lsp-mode's upstream signature."
  (require 'lsp-mode)
  (require 'lsp-rust)
  (require 'lang/rust/config)
  (should (eq (alist-get '+rust-analyzer +language-server-upstream-ids)
              'rust-analyzer))
  (should (eq (alist-get '+clangd +language-server-upstream-ids) 'clangd))
  (let ((contents (lsp-make-markup-content
                   :kind "markdown"
                   :value "```rust\n// size = 16 (0x10), align = 0x8\npub struct Point\n```")))
    ;; rust-analyzer's own extractor skips the memory-layout comment.
    (should (equal (substring-no-properties
                    (lsp-clients-extract-signature-on-hover contents '+rust-analyzer))
                   "pub struct Point"))))

(ert-deftest +company-dabbrev-skips-raw-pdf-buffers ()
  "Company's dabbrev offers words from prose buffers, not from PDF bytes."
  (require 'company-dabbrev)
  (let ((pdf (get-buffer-create "paper-test.pdf"))
        (notes (get-buffer-create "notes-test.txt")))
    (unwind-protect
        (progn
          (with-current-buffer pdf
            (insert "%PDF-1.7 /Filter /FlateDecodeStream")
            (setq major-mode 'pdf-view-mode))
          (with-current-buffer notes
            (text-mode)
            (insert "Flatland is a novella"))
          (with-temp-buffer
            (text-mode)
            (insert "Flat")
            (let ((candidates (company-dabbrev 'candidates "Flat")))
              (should (member "Flatland" candidates))
              (should-not (member "FlateDecodeStream" candidates)))))
      (kill-buffer pdf)
      (kill-buffer notes))))

(provide 'init-lsp-ui-tests)
;;; init-lsp-ui-tests.el ends here

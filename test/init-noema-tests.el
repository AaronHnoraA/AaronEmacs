;;; init-noema-tests.el --- Noema bridge tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(unless (fboundp 'general-define-key)
  (defalias 'general-define-key #'ignore))
(require 'noema/bridge/config)

(ert-deftest +noema-latex-agent-progress-is-bounded-and-content-free ()
  (let ((request-id "test-latex-progress")
        (clock 100.0)
        sent)
    (puthash request-id (list :deferred 'test)
             +noema--latex-agent-requests)
    (unwind-protect
        (cl-letf (((symbol-function 'remote-gateway-find-client)
                   (lambda (_) 'test-client))
                  ((symbol-function 'remote-gateway-notify)
                   (lambda (_client method params)
                     (push (cons method params) sent)))
                  ((symbol-function 'float-time)
                   (lambda (&optional _) clock)))
          (+noema--latex-agent-progress
           request-id "claude" '((:event . tool-call-update)))
          (+noema--latex-agent-progress
           request-id "claude" '((:event . agent-message-chunk)))
          (should (= (length sent) 1))
          (should (equal (cdar sent)
                         '((requestId . "test-latex-progress")
                           (text . "claude is using a tool…"))))
          (should (= (plist-get (gethash request-id +noema--latex-agent-requests)
                                :last-progress) 100.0))
          (setq clock 116.0)
          (+noema--latex-agent-progress
           request-id "claude" '((:event . agent-message-chunk)))
          (should (= (length sent) 2))
          (should (equal (caar sent) "noema.latex.agent-progress")))
      (remhash request-id +noema--latex-agent-requests))))

(ert-deftest +noema-bulk-tag-change-invalidates-roam-cache-once ()
  (require 'noema/roam/config)
  (let ((+noema-roam--runtime-index-cache 'stale)
        (+noema-roam--runtime-index-cache-key 'stale)
        (+noema-roam--scan-cache 'stale)
        (+noema-roam--all-files-cache 'stale)
        (+noema-roam--all-note-summaries-cache 'stale)
        (dispatch-count 0))
    (cl-letf (((symbol-function '+noema--defer-host-event)
               (lambda (fn &rest args)
                 (cl-incf dispatch-count)
                 (apply fn args))))
      (should (equal (+noema--gateway-event
                      '((type . "notes-mutated")
                        (payload . ((count . 3)))) nil)
                     '((ok . t))))
      (should (= dispatch-count 1))
      (should-not +noema-roam--runtime-index-cache)
      (should-not +noema-roam--runtime-index-cache-key)
      (should-not +noema-roam--scan-cache)
      (should-not +noema-roam--all-files-cache)
      (should-not +noema-roam--all-note-summaries-cache))))

(ert-deftest +noema-note-moved-event-reaches-roam-buffer-owner ()
  (require 'noema/roam/config)
  (let (dispatched)
    (cl-letf (((symbol-function '+noema--defer-host-event)
               (lambda (fn &rest args)
                 (setq dispatched (cons fn args)))))
      (should (equal (+noema--gateway-event
                      '((type . "note-moved")
                        (payload . ((oldFile . "/tmp/old.md")
                                    (file . "/tmp/new.md")))) nil)
                     '((ok . t))))
      (should (eq (car dispatched) #'+noema-roam--handle-moved-note))
      (should (equal (cadr dispatched)
                     '((oldFile . "/tmp/old.md")
                       (file . "/tmp/new.md")))))))

(ert-deftest +noema-note-trashed-event-reaches-roam-buffer-owner ()
  (require 'noema/roam/config)
  (let (dispatched)
    (cl-letf (((symbol-function '+noema--defer-host-event)
               (lambda (fn &rest args)
                 (setq dispatched (cons fn args)))))
      (should (equal (+noema--gateway-event
                      '((type . "note-trashed")
                        (payload . ((file . "/tmp/trashed.md")))) nil)
                     '((ok . t))))
      (should (eq (car dispatched) #'+noema-roam--handle-trashed-note))
      (should (equal (cadr dispatched)
                     '((file . "/tmp/trashed.md")))))))

(ert-deftest noema-trash-note-requires-a-target-page ()
  (let (sent)
    (cl-letf (((symbol-function '+noema--post)
               (lambda (payload) (setq sent payload))))
      (with-temp-buffer
        (setq-local +noema--client-id nil)
        (should-error (noema-trash-note) :type 'user-error)
        (should-not sent)
        (setq-local +noema--client-id "pane-1")
        (noema-trash-note)
        (should (equal (alist-get 'command sent) "trash-current-note"))
        (should (equal (alist-get 'client sent) "pane-1"))))))

(ert-deftest +noema-layout-commands-use-the-shared-editor-command ()
  (let (sent)
    (cl-letf (((symbol-function '+noema--post)
               (lambda (payload) (push payload sent))))
      (with-temp-buffer
        (setq-local +noema--client-id "pane-1")
        (+noema/insert-layout-grid)
        (+noema/insert-layout-three-columns)
        (+noema/insert-layout-flow))
      (should (equal (mapcar (lambda (payload)
                               (alist-get 'value (alist-get 'detail payload)))
                             (nreverse sent))
                     '("grid" "grid:3" "flow")))
      (should (cl-every (lambda (payload)
                          (and (equal (alist-get 'command payload) "insert-layout")
                               (equal (alist-get 'client payload) "pane-1")))
                        sent)))))

(ert-deftest +noema-latex-agent-status-uses-emacs-acp-configs ()
  (require 'noema-agent-acp)
  (cl-letf (((symbol-function 'noema-agent-acp-config-for)
             (lambda (id) (equal id "codex"))))
    (let* ((status (+noema--latex-agent-status nil nil))
           (agents (alist-get 'agents status)))
      (should (= (length agents) 3))
      (should (eq (alist-get 'available (aref agents 0)) t))
      (should (eq (alist-get 'available (aref agents 1)) :json-false)))))

(ert-deftest +noema-latex-agent-run-sends-one-acp-prompt ()
  (require 'noema-agent-acp)
  (let* ((workdir (make-temp-file "noema-latex-acp-" t))
         (buffer (generate-new-buffer " *noema-latex-run-test*"))
         (+noema--latex-agent-requests (make-hash-table :test #'equal))
         ready resolved prompt-content started-origin started-config retired)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "draft.tex" workdir)
            (insert "Draft."))
          (cl-letf (((symbol-function 'noema-agent-acp-config-for)
                     (lambda (_id) '((:identifier . codex))))
                    ((symbol-function 'remote-gateway-defer)
                     (lambda (&rest _) 'deferred))
                    ((symbol-function 'remote-gateway-resolve)
                     (lambda (_deferred result) (setq resolved result)))
                    ((symbol-function 'noema-agent-acp-start)
                     (lambda (&rest args)
                       (setq started-origin (plist-get args :origin))
                       (setq started-config (plist-get args :config))
                       (should (eq (plist-get args :display-function)
                                   #'+noema--latex-export-display))
                       buffer))
                    ((symbol-function 'noema-agent-acp-available-model-ids)
                     (lambda (&optional _) '("gpt-5.6-terra" "gpt-6-astra")))
                    ((symbol-function 'noema-agent-acp-current-model-id)
                     (lambda (&optional _) "gpt-5.6-terra"))
                    ((symbol-function 'noema-agent-acp-subscribe)
                     (lambda (&rest args)
                       (when (eq (plist-get args :event) 'init-finished)
                         (setq ready (plist-get args :callback)))
                       (if (plist-get args :event) 'subscription 'progress)))
                    ((symbol-function 'noema-agent-acp-unsubscribe)
                     (lambda (&rest _) nil))
                    ((symbol-function '+noema--latex-agent-retire-buffer)
                     (lambda (agent-buffer) (setq retired agent-buffer)))
                    ((symbol-function 'noema-agent-acp-prompt)
                     (lambda (&rest args)
                       (setq prompt-content (plist-get args :content))
                       (funcall (plist-get args :on-success)
                                '((stopReason . "end_turn"))))))
            (should (eq (+noema--latex-agent-run
                         `((requestId . "test-1") (backend . "codex")
                           (model . "gpt-5.6-terra")
                           (workdir . ,workdir) (prompt . "Polish the draft"))
                         nil)
                        'deferred))
            (should (functionp ready))
            (should (eq started-origin 'latex-export))
            (should (eq (plist-get (gethash "test-1" +noema--latex-agent-requests)
                                   :buffer) buffer))
            (should (eq (plist-get (gethash "test-1" +noema--latex-agent-requests)
                                   :progress-subscription) 'progress))
            (should (eq (plist-get (gethash "test-1" +noema--latex-agent-requests)
                                   :subscription) 'subscription))
            (should (equal (alist-get :noema-export-model started-config)
                           "gpt-5.6-terra"))
            (should (equal (funcall (alist-get :default-model-id started-config))
                           "gpt-5.6-terra"))
            (funcall ready nil)
            (should (equal (alist-get 'text (car prompt-content))
                           "Polish the draft"))
            (should (eq (alist-get 'ok resolved) t))
            (should (eq retired buffer))
            (should-not (gethash "test-1" +noema--latex-agent-requests))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory workdir t))))

(ert-deftest +noema-latex-agent-rejects-unselected-model-before-prompt ()
  (require 'noema-agent-acp)
  (let* ((workdir (make-temp-file "noema-latex-model-" t))
         (+noema--latex-agent-requests (make-hash-table :test #'equal))
         ready resolved prompted)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "draft.tex" workdir) (insert "Draft."))
          (cl-letf (((symbol-function 'noema-agent-acp-config-for)
                     (lambda (_) '((:identifier . codex))))
                    ((symbol-function 'remote-gateway-defer) (lambda (&rest _) 'deferred))
                    ((symbol-function 'remote-gateway-resolve)
                     (lambda (_deferred result) (setq resolved result)))
                    ((symbol-function 'noema-agent-acp-start) (lambda (&rest _) nil))
                    ((symbol-function 'noema-agent-acp-subscribe)
                     (lambda (&rest args)
                       (when (eq (plist-get args :event) 'init-finished)
                         (setq ready (plist-get args :callback)))
                       'subscription))
                    ((symbol-function 'noema-agent-acp-unsubscribe) (lambda (&rest _) nil))
                    ((symbol-function 'noema-agent-acp-available-model-ids)
                     (lambda (&optional _) '("gpt-5.6-terra" "gpt-6-astra")))
                    ((symbol-function 'noema-agent-acp-current-model-id)
                     (lambda (&optional _) "gpt-6-astra"))
                    ((symbol-function 'noema-agent-acp-prompt)
                     (lambda (&rest _) (setq prompted t))))
            (+noema--latex-agent-run
             `((requestId . "model-mismatch") (backend . "codex")
               (model . "gpt-5.6-terra")
               (workdir . ,workdir) (prompt . "Polish")) nil)
            (funcall ready nil)
            (should-not prompted)
            (should (eq (alist-get 'ok resolved) :json-false))
            (should (string-match-p "not selected" (alist-get 'message resolved)))
            (should-not (gethash "model-mismatch" +noema--latex-agent-requests))))
      (delete-directory workdir t))))

(ert-deftest +noema-latex-agent-open-focuses-only-live-request ()
  (let* ((buffer (generate-new-buffer " *noema-export-open*"))
         (+noema--latex-agent-requests (make-hash-table :test #'equal))
         shown resolved)
    (unwind-protect
        (cl-letf (((symbol-function 'remote-gateway-defer) (lambda (&rest _) 'deferred))
                  ((symbol-function '+noema--defer-host-event)
                   (lambda (function payload) (funcall function payload)))
                  ((symbol-function 'remote-gateway-resolve)
                   (lambda (_deferred result) (setq resolved result)))
                  ((symbol-function 'noema-agent-acp-show-buffer)
                   (lambda (agent-buffer)
                     (should (eq (buffer-local-value
                                  'noema-agent-acp-display-buffer-function agent-buffer)
                                 #'+noema--latex-export-display))
                     (setq shown agent-buffer))))
          (puthash "live" (list :buffer buffer) +noema--latex-agent-requests)
          (+noema--latex-agent-open '((requestId . "live")) nil)
          (should (eq shown buffer))
          (should (eq (alist-get 'ok resolved) t))
          (remhash "live" +noema--latex-agent-requests)
          (setq shown nil)
          (+noema--latex-agent-open '((requestId . "live")) nil)
          (should-not shown)
          (should (eq (alist-get 'ok resolved) :json-false)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-latex-agent-finish-cleans-up-after-gateway-disconnect ()
  (let* ((buffer (generate-new-buffer " *noema-latex-export-cleanup*"))
         (+noema--latex-agent-requests (make-hash-table :test #'equal))
         retired)
    (unwind-protect
        (progn
          (puthash "export-1" (list :buffer buffer :deferred 'deferred)
                   +noema--latex-agent-requests)
          (cl-letf (((symbol-function 'remote-gateway-resolve)
                     (lambda (&rest _) (error "Gateway disconnected")))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'noema-agent-acp-kill)
                     (lambda (agent-buffer)
                       (setq retired agent-buffer)
                       (kill-buffer agent-buffer))))
            (+noema--latex-agent-finish "export-1" nil "host stopped"))
          (should (eq retired buffer))
          (should-not (buffer-live-p buffer))
          (should-not (gethash "export-1" +noema--latex-agent-requests)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-latex-agent-export-completion-retires-orphan ()
  (require 'noema-agent-acp)
  (let* ((workdir (make-temp-file "noema-latex-export-" t))
         (buffer (generate-new-buffer " *noema-latex-export-orphan*"))
         (+noema--latex-agent-requests (make-hash-table :test #'equal))
         retired)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-acp-sessions)
                   (lambda (&optional _)
                     (list (list :buffer buffer :origin 'latex-export
                                 :root workdir))))
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function &rest args)
                     (apply function args)))
                  ((symbol-function 'noema-agent-acp-kill)
                   (lambda (agent-buffer)
                     (setq retired agent-buffer)
                     (kill-buffer agent-buffer))))
          (should (equal (+noema--latex-agent-cancel
                          `((requestId . "finished") (workdir . ,workdir)) nil)
                         '((ok . t))))
          (should (eq retired buffer))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory workdir t))))

(ert-deftest +noema-latex-agent-host-stop-retires-legacy-export-session ()
  (require 'noema-agent-acp)
  (let* ((buffer (generate-new-buffer " *noema-old-latex-export*"))
         (+noema--latex-agent-requests (make-hash-table :test #'equal))
         retired)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-acp-sessions)
                   (lambda (&optional _)
                     (list (list :buffer buffer :origin 'side
                                 :root "/tmp/latex-export--old-task/"))))
                  ((symbol-function '+noema--latex-agent-retire-buffer)
                   (lambda (agent-buffer) (setq retired agent-buffer))))
          (+noema--latex-agent-cancel-all)
          (should (eq retired buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-latex-intervention-opens-independent-editable-session ()
  (let* ((directory (make-temp-file "noema-latex-manual-" t))
         (file (expand-file-name "note.md" directory))
         (buffer (generate-new-buffer " *noema-latex-manual*"))
         (+noema--latex-intervention-buffers (make-hash-table :test #'equal))
         (params `((taskId . "failed-1") (file . ,file)
                   (outputPath . "/tmp/note.tex")
                   (backend . "claude") (error . "Undefined reference")))
         started drafted shown)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-acp-config-for)
                   (lambda (_backend) 'config))
                  ((symbol-function 'noema-agent-acp-start)
                   (lambda (&rest args)
                     (setq started args)
                     buffer))
                  ((symbol-function 'noema-agent-acp-state-value)
                   (lambda (&rest _) "ready"))
                  ((symbol-function 'noema-agent-acp-draft)
                   (lambda (_buffer text) (setq drafted text)))
                  ((symbol-function 'noema-agent-acp-show-buffer)
                   (lambda (agent-buffer) (setq shown agent-buffer))))
          (should (eq (+noema--latex-intervention-open params) buffer))
          (should (eq (plist-get started :display-function)
                      #'+noema--latex-intervention-display))
          (should (equal (directory-file-name (plist-get started :directory))
                         directory))
          (should (string-match-p "Undefined reference" drafted))
          (should (eq shown buffer))
          (setq started nil)
          (should (eq (+noema--latex-intervention-open params) buffer))
          (should-not started))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest +noema-latex-intervention-is-not-an-agent-workspace-tab ()
  (require 'noema-agent-acp)
  (let ((ordinary (generate-new-buffer " *noema-ordinary-agent*"))
        (intervention (generate-new-buffer " *noema-intervention-agent*")))
    (unwind-protect
        (progn
          (with-current-buffer ordinary
            (setq-local noema-agent-acp-session-root "/project/"))
          (with-current-buffer intervention
            (setq-local noema-agent-acp-session-root "/project/"
                        noema-agent-acp-display-buffer-function
                        #'+noema--latex-intervention-display))
          (cl-letf (((symbol-function 'noema-agent-acp-agent-buffer-p)
                     (lambda (buffer) (memq buffer (list ordinary intervention)))))
            (should (equal (noema-agent-acp--project-buffers "/project/")
                           (list ordinary)))))
      (kill-buffer ordinary)
      (kill-buffer intervention))))

(ert-deftest +noema-latex-export-failure-asks-once-and-opens-on-accept ()
  (let ((+noema--latex-failure-notified (make-hash-table :test #'equal))
        (params '((taskId . "failed-1") (file . "/tmp/note.md")
                  (error . "Undefined reference")))
        (warnings 0) (questions 0) opened)
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (cl-incf warnings)))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (cl-incf questions) t))
              ((symbol-function '+noema--latex-intervention-open)
               (lambda (payload) (setq opened payload))))
      (+noema--latex-export-failed params)
      (+noema--latex-export-failed params)
      (should (= warnings 1))
      (should (= questions 1))
      (should (equal opened params)))))

(ert-deftest +noema-input-focus-keeps-the-webkit-first-responder ()
  (let ((buffer (generate-new-buffer " *noema-input-focus-test*"))
        (+noema--input-focused-window nil)
        (+noema--input-focus-host-window nil)
        activity-updated)
    (unwind-protect
        (save-window-excursion
          (let ((page-window (split-window-below)))
            (set-window-buffer page-window buffer)
            (let ((host-window (selected-window)))
              (cl-letf (((symbol-function '+noema--buffer-for-client)
                       (lambda (_client) buffer))
                      ((symbol-function '+noema--update-activity)
                       (lambda () (setq activity-updated t)))
                      ((symbol-function 'frame-focus-state) (lambda (&rest _) t))
                      ((symbol-function 'select-window)
                       (lambda (&rest _) (ert-fail "Emacs selected a window over WebKit")))
                      ((symbol-function 'select-frame-set-input-focus)
                       (lambda (&rest _) (ert-fail "Emacs stole WebKit input focus"))))
                (+noema--handle-input-focus '((client . "test-page")))
                (should (+noema--buffer-active-p buffer))
                (+noema--forget-input-focus-on-command)
                (should-not (+noema--buffer-active-p buffer)))
              (should (eq (selected-window) host-window))
              (should-not +noema--input-focused-window)
              (should (eq (buffer-local-value '+noema--activity-paused buffer)
                          :unknown))
              (should activity-updated))))
      (+noema--forget-input-focus)
      (kill-buffer buffer))))

(ert-deftest +noema-jupyter-native-remote-paths-bypass-local-project-gate ()
  (dolist (file '("/ssh:Aaron-PC:/home/aaron/Desktop/a.ipynb"
                  "/rpc:Aaron-PC:/home/aaron/Desktop/a.ipynb"
                  "/fs:aaron-pc:/home/aaron/Desktop/a.ipynb"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (let* ((canonical "/fs:aaron-pc:/home/aaron/Desktop/a.ipynb")
             (body `((file . ,file) (scriptFile . ,file) (sourceFile . ,file)
                     (kernel . "python3"))))
        (cl-letf (((symbol-function 'noema-project-root)
                   (lambda (&rest _) (ert-fail "Remote notebook entered local project lookup")))
                  ((symbol-function 'noema-project-ensure)
                   (lambda (&rest _) (ert-fail "Remote notebook requested a local project")))
                  ((symbol-function '+noema--host-file) (lambda (_) canonical)))
          (let ((request (+noema-jupyter-cell--with-project-root body t)))
            (dolist (key '(file scriptFile sourceFile))
              (should (equal (alist-get key request) canonical)))
            (should-not (assq 'projectRoot request)))
          (should (equal buffer-file-name file))
          (should (equal (alist-get 'file body) file)))))))

(ert-deftest +noema-workspace-root-follows-desktop-precedence ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NOEMA_ROOT" "/tmp/noema-primary")
    (should (equal (+noema-workspace-root) "/tmp/noema-primary/"))))

(ert-deftest +noema-workspace-root-and-layout-use-app-config ()
  (let ((process-environment (copy-sequence process-environment))
        (workspace (make-hash-table :test #'equal)))
    (setenv "NOEMA_ROOT" nil)
    (setenv "NOEMA_WORKSPACE_LAYOUT" nil)
    (puthash "root" "~/Documents/Noema-test" workspace)
    (puthash "layout" "wiki" workspace)
    (cl-letf (((symbol-function '+noema--workspace-config)
               (lambda () workspace)))
      (should (equal (+noema-workspace-root)
                     (file-name-as-directory
                      (expand-file-name "~/Documents/Noema-test"))))
      (should (equal (+noema-workspace-layout) "wiki")))))

(ert-deftest +noema-open-wiki-view-uses-emacs-hosted-routes ()
  (let (opened)
    (cl-letf (((symbol-function '+noema--ensure-server)
               (lambda (callback) (funcall callback)))
              ((symbol-function '+noema--server-url) #'identity)
              ((symbol-function '+noema--open-url)
               (lambda (url &rest _) (setq opened url))))
      (+noema-open-wiki-view "repositories")
      (should (equal opened "/wiki?view=repositories"))
      (+noema-open-wiki-view nil "new=1")
      (should (equal opened "/wiki?new=1")))))

(ert-deftest +noema-jupyter-output-builds-url-after-host-is-ready ()
  (let ((+noema--port nil)
        observed-port)
    (cl-letf (((symbol-function '+noema--ensure-server)
               (lambda (callback)
                 (setq +noema--port 43210)
                 (funcall callback)))
              ((symbol-function '+noema-jupyter--output-url)
               (lambda (_payload)
                 (setq observed-port +noema--port)
                 (throw 'url-built nil))))
      (catch 'url-built
        (+noema-jupyter-output-open-document
         '((scriptFile . "/tmp/research.noema")
           (cellId . "work-1")))))
    (should (= observed-port 43210))))

(ert-deftest +noema-jupyter-output-startup-does-not-steal-new-focus ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *noema-output-source*"))
           (other (generate-new-buffer " *noema-output-other*"))
           (output (generate-new-buffer " *noema-output-page*"))
           (source-window (selected-window))
           (+noema-backend 'xwidget)
           (+noema--port 4242)
           callback)
      (unwind-protect
          (progn
            (set-window-buffer source-window source)
            (with-current-buffer source
              (cl-letf (((symbol-function '+noema--ensure-server)
                         (lambda (fn) (setq callback fn)))
                        ((symbol-function '+xwidget-session-buffer)
                         (lambda (_id) nil))
                        ((symbol-function '+xwidget/open-url)
                         (lambda (&rest _)
                           (switch-to-buffer output)
                           output)))
                (+noema-jupyter-output-open-document
                 '((scriptFile . "/tmp/research.noema")) t)
                (let ((other-window (split-window-below)))
                  (set-window-buffer other-window other)
                  (select-window other-window)
                  (funcall callback)
                  (should (eq (selected-window) other-window))
                  (should (eq (window-buffer source-window) source))
                  (should (= (length (window-list nil 'no-minibuf)) 3))))))
        (dolist (buffer (list source other output))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest +noema-wiki-refresh-calls-canonical-index-api ()
  (let ((+noema--ready t)
        call)
    (cl-letf (((symbol-function '+noema--api-call)
               (lambda (channel args callback)
                 (setq call (list channel args))
                 (funcall callback '((notes . nil) (generation . "1234567890")
                                     (maintenance . ((mode . "incremental"))))))))
      (+noema/wiki-refresh)
      (should (equal (car call) "noema:api:wiki:refresh"))
      (should (equal (alist-get 'mode (aref (cadr call) 0)) "incremental")))))

(ert-deftest +noema-wiki-index-status-renders-repository-hash-tables ()
  (let ((+noema--ready t)
        (payload (make-hash-table :test #'equal)))
    (puthash "generation" "1234567890" payload)
    (puthash "lastMode" "incremental" payload)
    (puthash "repositories"
             (list (let ((repository (make-hash-table :test #'equal)))
                     (puthash "repositoryId" "private/research" repository)
                     repository))
             payload)
    (cl-letf (((symbol-function '+noema-api-call)
               (lambda (_channel _args callback &optional _timeout)
                 (funcall callback payload nil)))
              ((symbol-function 'display-buffer) #'ignore))
      (+noema/wiki-index-status)
      (with-current-buffer "*Noema Wiki index status*"
        (should (string-match-p "private/research" (buffer-string)))))))

(ert-deftest +noema-retire-legacy-commands-cleans-obarray-and-amx-state ()
  (let ((legacy (intern "my/aaronnote-test-command"))
        (history-symbols
         '(extended-command-history vertico-repeat-history
           amx-history amx-data amx-cache)))
    (fset legacy (lambda () (interactive)))
    (unwind-protect
        (cl-progv
            history-symbols
            (list
             '("my/aaronnote-test-command" "+noema-roam/agenda")
             '((amx "aaronnote" "my/aaronnote-test-command")
               (amx "noema" "+noema-roam/agenda"))
             '(my/aaronnote-test-command +noema-roam/agenda)
             '((my/aaronnote-test-command . 2)
               (+noema-roam/agenda . 1))
             '((my/aaronnote-test-command . 2)
               (+noema-roam/agenda . 1)))
          (aaron-noema-retire-legacy-commands)
          (should-not (fboundp legacy))
          (dolist (variable history-symbols)
            (should-not
             (seq-some
              #'aaron-noema--legacy-command-history-entry-p
              (symbol-value variable)))))
      (when (fboundp legacy)
        (fmakunbound legacy)))))

(ert-deftest +noema-canonical-file-uses-target-aware-home-expansion ()
  (let ((remote-mode t)
        call)
    (cl-letf
        (((symbol-function 'remote-expand-file-name)
          (lambda (file &optional directory target)
            (setq call (list file directory target))
            "/fs:local:/Users/me/Documents/Noema/")))
      (should
       (equal
        (+noema--canonical-file "~/Documents/Noema/")
        "/fs:local:/Users/me/Documents/Noema/"))
      (should
       (equal call
              '("~/Documents/Noema/" nil "local"))))))

(ert-deftest +noema-canonical-file-collapses-local-directory-aliases ()
  "One note spelling must produce one canonical xwidget client identity."
  (let* ((root (make-temp-file "noema-canonical-root" t))
         (alias (concat root "-alias"))
         (note (expand-file-name "note.md" root)))
    (unwind-protect
        (progn
          (with-temp-file note (insert "# note\n"))
          (make-symbolic-link root alias)
          (should
           (equal (+noema--canonical-file note)
                  (+noema--canonical-file
                   (expand-file-name "note.md" alias))))
          (should
           (equal (+noema--xwidget-session-id note)
                  (+noema--xwidget-session-id
                   (expand-file-name "note.md" alias)))))
      (when (file-symlink-p alias) (delete-file alias))
      (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest +noema-host-script-stays-local-from-remote-notebook ()
  (let* ((remote-mode t)
         (script (expand-file-name "site-lisp/noema/web-host.mjs"
                                   user-emacs-directory))
         (expected (file-truename script)))
    (with-temp-buffer
      (setq default-directory "/fs:missing:/work/")
      (should (equal (+noema--host-file script) expected)))))

(ert-deftest +noema-host-file-preserves-remote-logical-identity ()
  (let ((remote-mode t))
    (cl-letf
        (((symbol-function '+noema--canonical-file) #'identity)
         ((symbol-function 'remote-file-name-target)
          (lambda (file)
            (if (string-prefix-p "/fs:local:" file)
                "local"
              "remote")))
         ((symbol-function 'remote-file-local-name)
          (lambda (file)
            (string-remove-prefix "/fs:local:" file))))
      (should
       (equal
        (+noema--host-file "/fs:remote:/srv/note.md")
        "/fs:remote:/srv/note.md"))
      (should
       (equal
        (+noema--host-file "/fs:local:/tmp/note.md")
        "/tmp/note.md")))))

(ert-deftest +noema-external-provider-reads-and-writes-through-logical-file-api ()
  (let* ((native (make-temp-file "noema-remote-provider-" nil ".md"))
         (logical (remote-expand-file-name native nil "local"))
         (canonical (+noema--canonical-file logical)))
    (unwind-protect
        (progn
          (with-temp-file native
            (insert "# Initial\n"))
          (let* ((opened
                  (+noema--external-file-read
                   `((file . ,logical)) nil))
                 (mtime (alist-get 'mtimeMs opened)))
            (should (equal (alist-get 'file opened) canonical))
            (should (equal (alist-get 'content opened) "# Initial\n"))
            (let ((conflict
                   (+noema--external-file-write
                    `((file . ,logical)
                      (content . "# Conflict\n")
                      (baseMtimeMs . ,(- mtime 10000)))
                    nil)))
              (should (eq (alist-get 'conflict conflict) t))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents native)
                  (buffer-string))
                "# Initial\n")))
            (let (staged
                  (original-make-temp-file (symbol-function 'make-temp-file)))
              (cl-letf (((symbol-function 'make-temp-file)
                         (lambda (prefix &rest args)
                           (setq staged prefix)
                           (apply original-make-temp-file prefix args))))
                (let ((saved
                       (+noema--external-file-write
                        `((file . ,logical)
                          (content . "# Saved\n")
                          (baseMtimeMs . ,mtime))
                        nil)))
                  (should (eq (alist-get 'ok saved) t))
                  (should (equal (alist-get 'file saved) canonical))
                  (should (equal (file-name-directory staged)
                                 (file-name-directory canonical)))
                  (should
                   (equal
                    (with-temp-buffer
                      (insert-file-contents native)
                      (buffer-string))
                    "# Saved\n")))))))
      (when (file-exists-p native)
        (delete-file native)))))

(ert-deftest +noema-external-watch-is-workspace-owned-and-bounded ()
  (let ((remote-mode t)
        (+noema--external-file-watches
         (make-hash-table :test #'equal))
        (+noema--external-file-watch-timers
         (make-hash-table :test #'equal))
        (+noema--external-file-watch-suppressed
         (make-hash-table :test #'equal))
        opened closed)
    (cl-letf
        ;; nil means "no client-side tool can open this path", which is what
        ;; makes a framework-owned watch necessary.
        (((symbol-function 'remote-client-file-name)
          (lambda (_file &optional _adapter) nil))
         ((symbol-function 'remote-context)
          (lambda (_file) 'mock-context))
         ((symbol-function 'remote-workspace-open)
          (lambda (_context &rest _args) 'mock-workspace))
         ((symbol-function 'remote-workspace-resource-live-p)
          (lambda (workspace resource)
            (and (eq workspace 'mock-workspace)
                 (eq resource 'mock-resource))))
         ((symbol-function 'remote-workspace-add-file-watch)
          (lambda (workspace file flags _callback &rest _args)
            (should-not opened)
            (setq opened (list workspace file flags))
            'mock-resource))
         ((symbol-function 'remote-workspace-close-resource)
          (lambda (workspace resource &optional reason)
            (setq closed (list workspace resource reason)))))
      (+noema--ensure-external-file-watch
       "/fs:remote:/srv/note.md")
      (+noema--ensure-external-file-watch
       "/fs:remote:/srv/note.md")
      (should
       (equal opened
              '(mock-workspace
                "/fs:remote:/srv/note.md"
                (change attribute-change))))
      (should
       (= (hash-table-count +noema--external-file-watches) 1))
      (+noema--clear-external-file-watches)
      (should
       (equal closed
              '(mock-workspace mock-resource noema-stop)))
      (should
       (zerop
        (hash-table-count +noema--external-file-watches))))))

(ert-deftest +noema-external-watch-ignores-sibling-files ()
  "A note is watched through its directory; only its own events count."
  (let ((+noema--external-file-watch-timers (make-hash-table :test #'equal))
        (+noema--external-file-watch-suppressed
         (make-hash-table :test #'equal))
        (file "/fs:remote:/srv/note.md"))
    (cl-letf (((symbol-function 'remote-canonicalize-file-name) #'identity))
      (unwind-protect
          (progn
            (+noema--external-file-watch-event
             file '(descriptor changed "/fs:remote:/srv/other.md"))
            (+noema--external-file-watch-event
             file '(descriptor created "/fs:remote:/srv/.tmp"))
            (+noema--external-file-watch-event
             file '(descriptor changed "/fs:remote:/elsewhere/note.md"))
            (should (zerop (hash-table-count
                            +noema--external-file-watch-timers)))
            (+noema--external-file-watch-event
             file '(descriptor changed "/fs:remote:/srv/note.md"))
            (should (= 1 (hash-table-count
                          +noema--external-file-watch-timers)))
            ;; A rename onto the note names it as the second file.
            (clrhash +noema--external-file-watch-timers)
            (+noema--external-file-watch-event
             file '(descriptor renamed "/fs:remote:/srv/.tmp"
                               "/fs:remote:/srv/note.md"))
            (should (= 1 (hash-table-count
                          +noema--external-file-watch-timers))))
        (maphash (lambda (_file timer) (when (timerp timer) (cancel-timer timer)))
                 +noema--external-file-watch-timers)))))

(ert-deftest +noema-external-watch-is-renewed-after-its-owner-closes ()
  (let ((remote-mode t)
        (+noema--external-file-watches
         (make-hash-table :test #'equal))
        (+noema--external-file-watch-timers
         (make-hash-table :test #'equal))
        (+noema--external-file-watch-suppressed
         (make-hash-table :test #'equal))
        (file "/fs:remote:/srv/note.md")
        (generation 0)
        (live t)
        closed)
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (_file &optional _adapter) nil))
         ((symbol-function 'remote-context)
          (lambda (_file) 'mock-context))
         ((symbol-function 'remote-workspace-open)
          (lambda (_context &rest _args) (list 'workspace generation)))
         ((symbol-function 'remote-workspace-resource-live-p)
          (lambda (_workspace _resource) live))
         ((symbol-function 'remote-workspace-add-file-watch)
          (lambda (&rest _args)
            (list 'resource (cl-incf generation))))
         ((symbol-function 'remote-workspace-close-resource)
          (lambda (_workspace resource &optional _reason)
            (push resource closed))))
      (+noema--ensure-external-file-watch file)
      (should (= generation 1))
      ;; Disconnecting the target closed the workspace and its watch.
      (setq live nil)
      (+noema--ensure-external-file-watch file)
      (should (= generation 2))
      (should (equal (cdr (gethash file +noema--external-file-watches))
                     '(resource 2)))
      (setq live t)
      (+noema--ensure-external-file-watch file)
      (should (= generation 2))
      ;; The owner already released the first watch; only the live one is
      ;; closed when Noema stops.
      (+noema--clear-external-file-watches)
      (should (equal closed '((resource 2)))))))

(ert-deftest +noema-external-watch-distinguishes-agent-write-after-save ()
  (let* ((file (make-temp-file "noema-watch-self-write-" nil ".md"))
         (replacement (concat file ".replacement"))
         (+noema--external-file-watch-suppressed
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "host"))
          (puthash file
                   (cons (+ (float-time) 2)
                         (+noema--external-file-signature file))
                   +noema--external-file-watch-suppressed)
          (should (+noema--external-file-self-write-p file))
          (with-temp-file replacement (insert "edit"))
          (rename-file replacement file t)
          (should-not (+noema--external-file-self-write-p file)))
      (when (file-exists-p replacement) (delete-file replacement))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest +noema-external-metadata-follows-symlink-target ()
  (let* ((target (make-temp-file "noema-link-target-" nil ".md"))
         (link (concat target ".link.md")))
    (unwind-protect
        (progn
          (make-symbolic-link target link)
          (let ((before (alist-get 'mtimeMs
                                   (+noema--external-file-metadata link))))
            (set-file-times target (time-add (current-time) (seconds-to-time 10)))
            (should (> (alist-get 'mtimeMs
                                  (+noema--external-file-metadata link))
                       before))))
      (when (file-exists-p link) (delete-file link))
      (when (file-exists-p target) (delete-file target)))))

(ert-deftest +noema-external-watch-notifies-host-path-for-native-pane ()
  (let ((+noema--external-file-watch-timers (make-hash-table :test #'equal))
        sent)
    (cl-letf (((symbol-function 'remote-gateway-find-client)
               (lambda (_name) 'mock-client))
              ((symbol-function '+noema--external-file-metadata)
               (lambda (_file) '((mtimeMs . 1234) (size . 7))))
              ((symbol-function '+noema--host-file)
               (lambda (_file) "/private/tmp/standalone.md"))
              ((symbol-function 'remote-gateway-notify)
               (lambda (_client _method payload) (setq sent payload))))
      (+noema--external-file-notify-change
       "/fs:local:/private/tmp/standalone.md")
      (let ((detail (alist-get 'detail sent)))
        (should (equal (alist-get 'file detail) "/private/tmp/standalone.md"))
        (should (equal (alist-get 'clientId detail) "remote-external")))
      (+noema--external-file-notify-change
       "/fs:local:/private/tmp/standalone.md" "agent-turn")
      (should (equal (alist-get 'clientId (alist-get 'detail sent))
                     "agent-turn")))))

(ert-deftest +noema-agent-turn-checks-each-open-markdown-once ()
  (should (memq #'+noema--check-open-notes-after-agent-turn
                +auto-revert-agent-refresh-functions))
  (let ((first (generate-new-buffer " *noema-turn-first*"))
        (second (generate-new-buffer " *noema-turn-second*"))
        (third (generate-new-buffer " *noema-turn-third*"))
        (disconnected (generate-new-buffer " *noema-turn-disconnected*"))
        (+noema--ready t)
        (checked nil))
    (unwind-protect
        (progn
          (dolist (buffer (list first second third disconnected))
            (with-current-buffer buffer
              (setq-local major-mode 'xwidget-webkit-mode)))
          (with-current-buffer first
            (setq-local +noema-buffer-file-name "/tmp/note.md"))
          (with-current-buffer second
            (setq-local +noema-buffer-file-name "/tmp/note.md"))
          (with-current-buffer third
            (setq-local +noema-buffer-file-name "/tmp/graph.html"))
          (with-current-buffer disconnected
            (setq-local +noema-buffer-file-name "/fs:box:/work/draft.md"
                        remote-buffer-disconnected-p t))
          (with-current-buffer first
            ;; Note tracking should use buffer metadata, regardless of mode.
            (setq-local major-mode 'fundamental-mode))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&rest _) (list first second third disconnected)))
                    ((symbol-function '+noema--external-file-notify-change)
                     (lambda (file client-id)
                       (push (list file client-id) checked))))
            (+noema--check-open-notes-after-agent-turn)
            (should (equal checked '(("/tmp/note.md" "agent-turn"))))))
      (dolist (buffer (list first second third disconnected))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +noema-jupyter-defaults-read-project-config-without-eval ()
  (let ((root (make-temp-file "noema-project" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" root)
            (insert "((nil . ((+project-local-settings . "
                    "(:toolchain ((python . sage)) "
                    ":noema-jupyter (:language sage :kernel sagemath :session research))))))\n"))
          (let ((+noema--notes-root root))
            (should
             (equal (+noema--jupyter-defaults)
                    '(:language "sage" :kernel "sagemath" :session "research")))
            (should
             (equal (+noema--jupyter-default-environment)
                    '("NOEMA_JUPYTER_DEFAULT_LANGUAGE=sage"
                      "NOEMA_JUPYTER_DEFAULT_KERNEL=sagemath"
                      "NOEMA_JUPYTER_DEFAULT_SESSION=research")))))
      (delete-directory root t))))

(ert-deftest +noema-prose-check-is-an-editor-command ()
  (should (commandp #'+noema/prose-check))
  (let (sent)
    (cl-letf (((symbol-function '+noema/command)
               (lambda (command &optional detail)
                 (setq sent (list command detail)))))
      (+noema/prose-check)
      (should (equal sent '("prose-check" nil))))))

(ert-deftest +noema-refresh-does-not-send-extra-focus ()
  (let ((+noema--ready t)
        (+noema--app-buffer (current-buffer))
        calls)
    (cl-letf (((symbol-function '+noema/command)
               (lambda (command &optional _detail)
                 (push command calls))))
      (+noema/refresh)
      (should (equal (nreverse calls) '("refresh"))))))

(ert-deftest +noema-keys-mode-binds-history-chords ()
  (should (eq (lookup-key +noema-keys-mode-map (kbd "M-z"))
              #'+noema/undo))
  (should (eq (lookup-key +noema-keys-mode-map (kbd "M-Z"))
              #'+noema/redo))
  (should (eq (lookup-key +noema-keys-mode-map (kbd "M-S-z"))
              #'+noema/redo)))

(ert-deftest +noema-jupyter-cell-point-move-stays-local ()
  (let ((buffer (generate-new-buffer "*Noema-jcell-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "# %% id=one\n"
                  "print(1)\n"
                  "\n# %% id=two\n"
                  "print(2)\n")
          (setq-local +noema-jupyter-cell-source-file "/tmp/note.md")
          (setq-local +noema-jupyter-cell-kernel "python3")
          (setq-local +noema-jupyter-cell-session "default")
          (setq-local +noema-jupyter-cell-storage "ipynb")
          (setq-local +noema-jupyter-cell--host-ready-requested t)
          (+noema-jupyter-cell-mode 1)
          (goto-char (point-min))
          (search-forward "print(2)")
          (+noema-jupyter-cell--post-command-h)
          (should (equal +noema-jupyter-cell-current-id "two")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-jupyter-cell-run-does-not-jump-to-web-output ()
  (let (api-call output-opened)
    (with-temp-buffer
      (cl-letf (((symbol-function 'buffer-modified-p) (lambda (&optional _buffer) nil))
                ((symbol-function '+noema-jupyter-cell--action-body)
                 (lambda (&rest _args) '((action . "run-current") (cellId . "one"))))
                ((symbol-function '+noema-jupyter-cell--api-async)
                 (lambda (&rest args) (setq api-call args)))
                ((symbol-function '+noema-jupyter/output-open)
                 (lambda (&rest _args) (setq output-opened t))))
        (+noema-jupyter-cell--execute "current")))
    (should (equal (car api-call) "noema:api:jupyter:script-action"))
    (should-not output-opened)))

(ert-deftest +noema-jupyter-refresh-uses-authoritative-script-snapshot ()
  (let ((buffer (generate-new-buffer "*Noema-jupyter-snapshot-test*"))
        (+noema--ready t)
        call)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local buffer-file-name "/tmp/notebook.ipynb")
          (setq-local +noema-jupyter-cell-mode t)
          (setq-local +noema-jupyter-cell-kernel "old-kernel")
          (setq-local +noema-jupyter-cell-language "python")
          (cl-letf (((symbol-function '+noema-api-call)
                     (lambda (channel args callback)
                       (setq call (list channel args))
                       (funcall callback
                                '((document . ((scriptFile . "/tmp/notebook.ipynb")
                                               (sourceFile . "/tmp/note.md")
                                               (kernel . "sagemath")
                                               (language . "python")
                                               (session . "default")))
                                  (kernelStatus . "idle"))
                                nil)))
                    ((symbol-function '+language-server-ensure-deferred)
                     #'ignore))
            (+noema-jupyter/cell-refresh-status))
          (should (equal (car call)
                         "noema:api:jupyter:script-snapshot"))
          (should (equal (aref (cadr call) 0)
                         '((scriptFile . "/tmp/notebook.ipynb"))))
          (should (equal +noema-jupyter-cell-kernel "sagemath"))
          (should (equal +noema-jupyter-cell--kernel-status "idle"))
          (should-not +noema-jupyter-cell--session-refresh-pending))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-jupyter-gateway-routes-session-snapshot-directly ()
  (let (seen deferred)
    (cl-letf (((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args)
                 (setq deferred (cons function args))))
              ((symbol-function '+noema-jupyter-cell-handle-session-event)
               (lambda (payload) (setq seen payload))))
      (+noema--gateway-event
       '((type . "jupyter-session")
         (payload . ((document . ((scriptFile . "/tmp/notebook.ipynb")))
                     (kernelStatus . "idle"))))
       nil)
      (should-not seen)
      (apply (car deferred) (cdr deferred)))
    (should (equal (alist-get 'kernelStatus seen) "idle"))))

(ert-deftest +noema-jupyter-gateway-ignores-web-cell-selection ()
  (let (seen)
    (cl-letf (((symbol-function '+noema-jupyter-cell-handle-selection-event)
               (lambda (payload) (setq seen payload))))
      (+noema--gateway-event
       '((type . "jupyter-cell-select")
         (payload . ((scriptFile . "/tmp/notebook.ipynb")
                     (cellId . "cell-two"))))
       nil))
    (should-not seen)))

(ert-deftest +noema-xwidget-redo-routes-only-noema-buffer ()
  (let ((buffer (generate-new-buffer "*Noema-test*"))
        (+noema--app-buffer nil)
        sent passed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode))
          (cl-letf (((symbol-function '+noema/command)
                     (lambda (command &optional detail)
                       (setq sent (list command detail))))
                    ((symbol-function 'xwidget-webkit-pass-command-event)
                     ;; The real command takes no argument; it reads
                     ;; `last-command-event'.  Accept either shape so the test
                     ;; asserts the passthrough, not a particular arity.
                     (lambda (&optional event)
                       (setq passed (or event last-command-event)))))
            (with-current-buffer buffer
              (setq +noema--app-buffer buffer)
              (noema-xwidget-redo 'noema-event))
            (should (equal sent '("redo" nil)))
            (should-not passed)
            (setq sent nil)
            (with-current-buffer buffer
              (setq +noema--app-buffer nil)
              ;; A real key invocation has both: the `interactive "e"' argument
              ;; and `last-command-event', which is what the passthrough reads.
              (let ((last-command-event 'other-event))
                (noema-xwidget-redo 'other-event)))
            (should-not sent)
            (should (eq passed 'other-event))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-xwidget-clipboard-keys-route-to-the-page ()
  "Cmd-c / Cmd-v must reach Noema, not `xwidget-webkit-pass-command-event'.
That command cannot replay a key into WebKit on the macOS xwidget port -- the
Emacs binary has no nsxwidget_perform_lispy_event -- so a clipboard key Emacs
owns is simply lost unless it is routed to the page."
  (let ((buffer (generate-new-buffer "*Noema-clipboard-test*"))
        (+noema--app-buffer nil)
        sent passed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode))
          (cl-letf (((symbol-function '+noema/command)
                     (lambda (command &optional detail)
                       (push (list command detail) sent)))
                    ((symbol-function 'xwidget-webkit-pass-command-event)
                     (lambda (&optional event)
                       (setq passed (or event last-command-event)))))
            (with-current-buffer buffer
              (setq +noema--app-buffer buffer)
              (noema-xwidget-copy 'copy-event)
              (noema-xwidget-paste 'paste-event)
              (noema-xwidget-cut 'cut-event))
            (should (equal (nreverse sent)
                           '(("copy" nil) ("paste" nil) ("cut" nil))))
            (should-not passed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-xwidget-clipboard-keys-win-the-shared-keymaps ()
  "Noema's routing must survive the generic browser configuration.
Both claim M-c / M-v on the same xwidget keymaps from `with-eval-after-load'
blocks with no defined order, so Noema reinstalls from the mode hook."
  (skip-unless (require 'xwidget nil t))
  (let ((xwidget-webkit-mode-map (make-sparse-keymap))
        (xwidget-webkit-edit-mode-map (make-sparse-keymap)))
    (define-key xwidget-webkit-mode-map (kbd "M-c") #'ignore)
    (define-key xwidget-webkit-edit-mode-map (kbd "M-v") #'ignore)
    (noema--install-xwidget-keys)
    (should (eq (lookup-key xwidget-webkit-mode-map (kbd "M-c"))
                #'noema-xwidget-copy))
    (should (eq (lookup-key xwidget-webkit-mode-map (kbd "M-v"))
                #'noema-xwidget-paste))
    (should (eq (lookup-key xwidget-webkit-edit-mode-map (kbd "M-c"))
                #'noema-xwidget-copy))
    (should (eq (lookup-key xwidget-webkit-edit-mode-map (kbd "M-v"))
                #'noema-xwidget-paste))))

(ert-deftest +noema-xwidget-recovery-follows-edit-mode-events ()
  "Recovery keys are active in every Noema pane, editing or not."
  (with-temp-buffer
    (let ((+noema--app-buffer (current-buffer)))
      (cl-letf (((symbol-function 'noema--xwidget-buffer-p)
                 (lambda (&optional _) t)))
        (setq-local xwidget-webkit-edit-mode nil)
        (noema--sync-xwidget-recovery-mode)
        (should noema-xwidget-recovery-mode)
        (dolist (key '("d" "SPC" "q" "<down>" "S-<left>" "M-/" "M-y"
                       "M-=" "M-S-<right>" "C-<tab>"))
          (should (eq (key-binding (kbd key)) #'noema-xwidget-recover-key)))
        ;; Cmd+Arrow is windmove from every window, a Noema pane included.
        (should-not (eq (key-binding (kbd "M-<right>")) #'noema-xwidget-recover-key))
        ;; Host Cmd chords must retain their Emacs bindings in recovery mode.
        (should-not (eq (key-binding (kbd "M-x")) #'noema-xwidget-recover-key))
        (should-not (eq (key-binding (kbd "M-w")) #'noema-xwidget-recover-key))
        (should-not (eq (key-binding (kbd "M-q")) #'noema-xwidget-recover-key))
        (should-not (eq (key-binding (kbd "M-o")) #'noema-xwidget-recover-key))
        (should-not (eq (key-binding (kbd "M-O")) #'noema-xwidget-recover-key))
        ;; Emacs sees a pane's keys only when WebKit lacks the native keyboard
        ;; (the page handed it over), so recovery stays on while editing too.
        (setq-local xwidget-webkit-edit-mode t)
        (noema--sync-xwidget-recovery-mode)
        (should noema-xwidget-recovery-mode)
        (should (eq (key-binding (kbd "d")) #'noema-xwidget-recover-key))
        (should (eq (key-binding (kbd "RET")) #'noema-xwidget-recover-key))))))

(ert-deftest +noema-xwidget-recovery-normalizes-the-shared-key-payload ()
  (let ((plain (noema--xwidget-recovery-detail (aref (kbd "d") 0)))
        (space (noema--xwidget-recovery-detail (aref (kbd "SPC") 0)))
        (tab (noema--xwidget-recovery-detail (aref (kbd "TAB") 0)))
        (enter (noema--xwidget-recovery-detail (aref (kbd "RET") 0)))
        (backspace (noema--xwidget-recovery-detail (aref (kbd "DEL") 0)))
        (source (noema--xwidget-recovery-detail (aref (kbd "M-/") 0)))
        (palette (noema--xwidget-recovery-detail (aref (kbd "M-?") 0)))
        (selection (noema--xwidget-recovery-detail (aref (kbd "S-<left>") 0))))
    (should (equal (alist-get 'key plain) "d"))
    (should (equal (alist-get 'code plain) "KeyD"))
    (should (equal (alist-get 'key space) " "))
    (should (equal (alist-get 'code space) "Space"))
    (should (equal (alist-get 'key tab) "Tab"))
    (should (equal (alist-get 'code tab) "Tab"))
    (should-not (alist-get 'ctrlKey tab))
    (should (equal (alist-get 'key enter) "Enter"))
    (should (equal (alist-get 'code enter) "Enter"))
    (should-not (alist-get 'ctrlKey enter))
    (should (equal (alist-get 'key backspace) "Backspace"))
    (should (equal (alist-get 'key source) "/"))
    (should (eq (alist-get 'metaKey source) t))
    (should (equal (alist-get 'key palette) "?"))
    (should (equal (alist-get 'code palette) "Slash"))
    (should (eq (alist-get 'shiftKey palette) t))
    (should (equal (alist-get 'key selection) "ArrowLeft"))
    (should (eq (alist-get 'shiftKey selection) t))))

(ert-deftest +noema-xwidget-placeholder-runs-no-buffer-local-plugins ()
  (skip-unless (require 'xwidget nil t))
  (with-temp-buffer
    (xwidget-webkit-mode)
    (let ((+noema--app-buffer (current-buffer)))
      (setq-local +noema--client-id "placeholder-client")
      (setq-local completion-at-point-functions '(ignore))
      (setq-local eldoc-documentation-functions '(ignore))
      (setq-local +xwidget-focus-script "sync-js-must-go")
      (when (fboundp 'undo-tree-mode) (undo-tree-mode 1))
      (+noema-keys-mode 1)
      (noema-xwidget-recovery-mode 1)
      (noema--harden-xwidget-placeholder)
      (should buffer-read-only)
      (should (eq buffer-undo-list t))
      (should-not buffer-auto-save-file-name)
      (should-not completion-at-point-functions)
      (should-not eldoc-documentation-functions)
      (should-not syntax-propertize-function)
      (should-not +xwidget-focus-script)
      (should-not cursor-type)
      (should-not bidi-display-reordering)
      (should +noema-keys-mode)
      (should noema-xwidget-recovery-mode)
      (when (boundp 'undo-tree-mode) (should-not undo-tree-mode))
      (dolist (mode minor-mode-list)
        (when (and (local-variable-p mode)
                   (boundp mode)
                   (symbol-value mode)
                   (fboundp mode))
          (should (memq mode noema--xwidget-placeholder-mode-whitelist)))))))

(ert-deftest +noema-xwidget-recovery-keeps-editor-ctrl-keys-in-renderer ()
  "An inert placeholder may own host prefixes, never document Ctrl commands."
  (dolist (key '("C-a" "C-d" "C-r" "C-z" "C-y" "C-0"))
    (should (eq (lookup-key noema-xwidget-recovery-mode-map (kbd key))
                #'noema-xwidget-recover-key)))
  (dolist (key '("C-x" "C-c" "C-g"))
    (should-not (lookup-key noema-xwidget-recovery-mode-map (kbd key))))
  (should (equal (noema--xwidget-key-code "=") "Equal"))
  (should (equal (noema--xwidget-key-code "-") "Minus")))

(ert-deftest +noema-xwidget-recovery-never-evaluates-javascript-inline ()
  (let ((buffer (current-buffer))
        scheduled
        sent)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (setq scheduled (cons function args))
                 'mock-timer))
              ((symbol-function '+noema/command)
               (lambda (command &optional detail)
                 (setq sent (list command detail))))
              ((symbol-function 'xwidget-webkit-execute-script)
               (lambda (&rest _)
                 (ert-fail "recovery must not evaluate JavaScript"))))
      (noema--forward-recovery-key
       buffer '((key . "d") (code . "KeyD")))
      (should (equal sent '("key" ((key . "d") (code . "KeyD")))))
      (setq sent nil)
      (noema-xwidget-recover-key (aref (kbd "d") 0))
      (should (eq (car scheduled) #'noema--forward-recovery-key))
      (apply (car scheduled) (cdr scheduled))
      (should (equal (car sent) "key")))))

(ert-deftest +noema-command-notification-leaves-the-caller-before-io ()
  (let ((+noema--post-queue nil)
        (+noema--post-timer nil)
        (+noema--post-awaiting-ready nil)
        (+noema--ready t)
        scheduled
        sent)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (setq scheduled (cons function args))
                 'mock-timer))
              ((symbol-function 'remote-gateway-find-client)
               (lambda (_name) 'client))
              ((symbol-function '+noema--post-now)
               (lambda (payload) (push payload sent) t))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _)
                 (ert-fail "notification path must never wait")))
              ((symbol-function 'xwidget-webkit-execute-script)
               (lambda (&rest _)
                 (ert-fail "notification path must never evaluate JS"))))
      (+noema--post '((type . "command") (command . "focus")))
      (should-not sent)
      (should (= (length +noema--post-queue) 1))
      (should (eq (car scheduled) #'+noema--drain-post-queue))
      (apply (car scheduled) (cdr scheduled))
      (should (equal (alist-get 'command (car sent)) "focus"))
      (should-not +noema--post-queue))))

(ert-deftest +noema-xwidget-undo-routes-jupyter-buffer-to-generic-xwidget ()
  (let ((buffer (generate-new-buffer "*Noema-jupyter-test*"))
        (+noema--app-buffer nil)
        sent passed undo-called)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local +xwidget--session-id "noema-jupyter"))
          (cl-letf (((symbol-function '+noema/command)
                     (lambda (command &optional detail)
                       (setq sent (list command detail))))
                    ((symbol-function 'xwidget-webkit-pass-command-event)
                     ;; The real command takes no argument; it reads
                     ;; `last-command-event'.  Accept either shape so the test
                     ;; asserts the passthrough, not a particular arity.
                     (lambda (&optional event)
                       (setq passed (or event last-command-event))))
                    ((symbol-function '+xwidget/undo)
                     (lambda ()
                       (setq undo-called t))))
            (with-current-buffer buffer
              (noema-xwidget-undo 'jupyter-event))
            (should undo-called)
            (should-not sent)
            (should-not passed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-editable-split-opens-fresh-client-xwidget ()
  (let* ((file (make-temp-file "noema-split" nil ".md"))
         (source (generate-new-buffer "*Noema-split-source*"))
         (xwidget-buffer (generate-new-buffer "*Noema-split-xwidget*"))
         (+noema-backend 'xwidget)
         (+noema--port 4242)
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--client-buffers (make-hash-table :test #'equal))
         opened-url opened-id opened-display opened-force-new opened-reuse)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local buffer-file-name file))
          (switch-to-buffer source)
          (cl-letf (((symbol-function '+noema--ensure-server)
                     (lambda (callback) (funcall callback)))
                    ((symbol-function '+noema--split-window)
                     (lambda (&optional _direction) (selected-window)))
                    ((symbol-function '+xwidget/open-url)
                     (lambda (url &rest args)
                       (setq opened-url url
                             opened-id (plist-get args :id)
                             opened-display (plist-get args :display)
                             opened-force-new (plist-get args :force-new)
                             opened-reuse (plist-get args :reuse-selected))
                       xwidget-buffer)))
            (+noema/open-current-note-split))
          (should-not (string-match-p "readonly=1" opened-url))
          (should (string-match-p "client=noema-split%3A" opened-url))
          (should (string-prefix-p "noema-split:" opened-id))
          (should (eq opened-display 'current))
          (should opened-force-new)
          (should opened-reuse)
          (should-not (gethash (expand-file-name file) +noema--file-buffers))
          (should (eq (gethash opened-id +noema--client-buffers) xwidget-buffer))
          (with-current-buffer xwidget-buffer
            (should
             (equal +noema-buffer-file-name
                    (+noema--canonical-file file)))
            (should (equal +noema--client-id opened-id))
            (should-not +noema--registered-file)))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p xwidget-buffer) (kill-buffer xwidget-buffer))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest +noema-open-file-after-startup-keeps-the-users-new-focus ()
  (save-window-excursion
    (let* ((file (make-temp-file "noema-delayed-open" nil ".md"))
           (source (generate-new-buffer " *noema-delayed-source*"))
           (other (generate-new-buffer " *noema-delayed-other*"))
           (source-window (selected-window))
           (+noema--port 4242)
           callback opened-window result)
      (unwind-protect
          (progn
            (set-window-buffer source-window source)
            (cl-letf (((symbol-function '+noema--ensure-server)
                       (lambda (fn) (setq callback fn)))
                      ((symbol-function '+noema--open-url)
                       (lambda (&rest _)
                         (setq opened-window (selected-window))
                         (setq result
                               (generate-new-buffer " *noema-delayed-result*"))
                         (switch-to-buffer result))))
              (+noema/open-file file)
              (let ((other-window (split-window-right)))
                (set-window-buffer other-window other)
                (select-window other-window)
                (funcall callback)
                (should (eq opened-window source-window))
                (should (eq (selected-window) other-window)))))
        (when (buffer-live-p source) (kill-buffer source))
        (when (buffer-live-p other) (kill-buffer other))
        (when (buffer-live-p result) (kill-buffer result))
        (when (file-exists-p file) (delete-file file))))))

;; A cold web-host answers after `find-file' has finished: the window that
;; asked now shows the redirected buffer (or, with another-window commands,
;; a different window does), not the buffer captured when the open was queued.
(ert-deftest +noema-open-file-after-cold-start-claims-the-redirect-placeholder ()
  (save-window-excursion
    (let* ((file (make-temp-file "noema-cold-open" nil ".md"))
           (source (generate-new-buffer " *noema-cold-source*"))
           (unrelated (generate-new-buffer " *noema-cold-unrelated*"))
           (placeholder (generate-new-buffer " *noema-cold-placeholder*"))
           (origin (selected-window))
           (+noema--port 4242)
           callback opened-window result)
      (unwind-protect
          (progn
            (set-window-buffer origin source)
            (cl-letf (((symbol-function '+noema--ensure-server)
                       (lambda (fn) (setq callback fn)))
                      ((symbol-function '+noema--open-url)
                       (lambda (&rest _)
                         (setq opened-window (selected-window))
                         (setq result
                               (generate-new-buffer " *noema-cold-result*"))
                         (switch-to-buffer result))))
              (+noema/open-file file)
              (with-current-buffer placeholder
                (setq buffer-file-name file)
                (setq-local +noema--markdown-redirected t)
                (set-buffer-modified-p nil))
              (let ((other-window (split-window-right)))
                (set-window-buffer origin unrelated)
                (set-window-buffer other-window placeholder)
                (funcall callback)
                (should (eq opened-window other-window))
                (should (eq (window-buffer other-window) result))
                (should (eq (window-buffer origin) unrelated))
                ;; With neither the placeholder nor the original buffer on
                ;; screen there is nothing to claim.
                (setq opened-window nil)
                (funcall callback)
                (should-not opened-window))))
        (dolist (buffer (list source unrelated placeholder result))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest +noema-editable-split-rolls-back-on-open-error ()
  (save-window-excursion
    (let* ((file (make-temp-file "noema-split-error" nil ".md"))
           (source (generate-new-buffer " *noema-split-error-source*"))
           (window (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer window source)
            (cl-letf (((symbol-function '+noema--ensure-server)
                       (lambda (callback) (funcall callback)))
                      ((symbol-function '+xwidget/open-url)
                       (lambda (&rest _) (error "WebKit failed"))))
              (should-error (+noema/open-current-note-split file))
              (should (= (length (window-list nil 'no-minibuf)) 1))
              (should (eq (selected-window) window))))
        (when (buffer-live-p source) (kill-buffer source))
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest +noema-link-new-window-keeps-source-pane-and-target-anchor ()
  (let* ((source (generate-new-buffer " *noema-link-source*"))
         (window (selected-window))
         deferred)
    (unwind-protect
        (progn
          (set-window-buffer window source)
          (cl-letf (((symbol-function '+noema--buffer-for-client)
                     (lambda (client) (and (equal client "pane-a") source)))
                    ((symbol-function '+noema--defer-host-event)
                     (lambda (function &rest args)
                       (setq deferred (cons function args)))))
            (+noema--handle-process-line
             "aaronote-event:open:{\"file\":\"/tmp/target.md\",\"client\":\"pane-a\",\"newWindow\":true,\"hash\":\"result\"}"))
          (should (eq (car deferred) #'+noema/open-current-note-split))
          (should (equal (nth 1 deferred) "/tmp/target.md"))
          (should (eq (nth 2 deferred) window))
          (should (equal (nth 3 deferred) '(("hash" . "result") ("dom" . nil)))))
      (set-window-buffer window (other-buffer source t))
      (kill-buffer source))))

(ert-deftest +noema-link-from-closed-pane-does-not-replace-current-window ()
  (let ((+noema--client-buffers (make-hash-table :test #'equal))
        opened)
    (cl-letf (((symbol-function '+noema/open-file)
               (lambda (&rest _) (setq opened t)))
              ((symbol-function '+noema--defer-host-event)
               (lambda (&rest _) (setq opened t))))
      (+noema--handle-process-line
       "aaronote-event:open:{\"file\":\"/tmp/target.md\",\"client\":\"closed\"}")
      (+noema--handle-process-line
       "aaronote-event:open:{\"file\":\"/tmp/target.md\",\"client\":\"closed\",\"newWindow\":true}"))
    (should-not opened)))

(ert-deftest +noema-back-forward-controls-address-the-current-pane ()
  (let (messages)
    (with-temp-buffer
      (setq-local +noema--client-id "pane-a")
      (+noema-keys-mode 1)
      (cl-letf (((symbol-function '+noema--post)
                 (lambda (payload) (push payload messages)))
                ((symbol-function 'xwidget-webkit-back)
                 (lambda () (ert-fail "Browser history bypassed Noema"))))
        (should (eq (lookup-key +noema-keys-mode-map (kbd "b"))
                    #'+noema/back))
        (should (eq (lookup-key +noema-keys-mode-map (kbd "f"))
                    #'+noema/forward))
        (+xwidget/back)
        (+xwidget/forward))
      (should (equal (mapcar (lambda (message) (alist-get 'command message))
                             (nreverse messages))
                     '("back" "forward")))
      (should (equal (alist-get 'client (car messages)) "pane-a")))))

(ert-deftest +noema-back-from-standalone-slides-opens-source-editor ()
  (let (opened posted)
    (with-temp-buffer
      (setq-local major-mode 'xwidget-webkit-mode)
      (cl-letf (((symbol-function '+xwidget-current-url)
                 (lambda (&optional _buffer)
                   "http://127.0.0.1:18456/slides?file=%2Ftmp%2Fordinary%20note.md"))
                ((symbol-function '+noema/open-file)
                 (lambda (file &optional _window) (setq opened file)))
                ((symbol-function '+noema--post)
                 (lambda (payload) (setq posted payload))))
        (+noema/back))
      (should (equal opened "/tmp/ordinary note.md"))
      (should-not posted))))

(ert-deftest +noema-topbar-menu-opens-the-public-transient ()
  (with-temp-buffer
    (setq-local +noema-buffer-file-name "/tmp/noema-menu-test.md")
    (let ((entry (cl-find-if
                  (lambda (item)
                    (and (vectorp item)
                         (equal (aref item 0) "Noema: Pop")))
                  (+noema--xwidget-menu-section))))
      (should entry)
      (should (eq (aref entry 1) #'noema-menu))
      (should (eq (plist-get (cdr (transient-get-suffix 'noema-menu "o"))
                             :command)
                  #'+noema/open-current-note)))))

(ert-deftest +noema-header-history-and-popup-use-noema-actions ()
  (let ((buttons (+noema--header-browser-buttons))
        events history opened)
    (cl-letf (((symbol-function '+xwidget--select-event-window) #'ignore)
              ((symbol-function '+xwidget/show-popup)
               (lambda () (interactive) (setq opened t)))
              ((symbol-function '+noema/switch-buffer-menu)
               (lambda (event) (push event events)))
              ((symbol-function 'easy-menu-create-menu)
               (lambda (_title items) (setq history items)))
              ((symbol-function 'popup-menu) #'ignore))
      (let* ((history-map (get-text-property 0 'local-map (car buttons)))
             (popup-map (get-text-property 0 'local-map (cadr buttons))))
        (funcall (lookup-key history-map [header-line mouse-3]) 'clicked)
        (should (eq (aref (car history) 1) #'+noema/back))
        (should (eq (aref (cadr history) 1) #'+noema/forward))
        (funcall (lookup-key popup-map [header-line mouse-1]) 'clicked)
        (funcall (lookup-key popup-map [header-line mouse-3]) 'clicked)))
    (should opened)
    (should (equal events '(clicked)))))

(ert-deftest +noema-portal-header-shares-xwidget-chrome ()
  "Both Noema backends render the same browser controls."
  (let ((xwidget (+noema--header-browser-buttons))
        (portal (+noema--header-browser-buttons
                 #'+noema/refresh #'+noema/portal-menu)))
    (should (= (length portal) 5))
    (should (equal (mapcar #'substring-no-properties portal)
                   (mapcar #'substring-no-properties xwidget)))))

(ert-deftest +noema-buffer-menu-excludes-displayed-buffers ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *noema-menu-source*"))
           (visible (generate-new-buffer " *noema-menu-visible*"))
           (hidden (generate-new-buffer " *noema-menu-hidden*"))
           (source-window (selected-window))
           (+noema--app-buffer nil)
           (+noema--port nil)
           menu)
      (unwind-protect
          (progn
            (dolist (entry (list (cons source "menu-source")
                                 (cons visible "menu-visible")
                                 (cons hidden "menu-hidden")))
              (with-current-buffer (car entry)
                (setq-local major-mode 'xwidget-webkit-mode)
                (setq-local +noema--client-id (cdr entry))))
            (set-window-buffer source-window source)
            (set-window-buffer (split-window-right) visible)
            (should (memq hidden (+noema--undisplayed-buffers)))
            (should-not (memq source (+noema--undisplayed-buffers)))
            (should-not (memq visible (+noema--undisplayed-buffers)))
            (cl-letf (((symbol-function 'popup-menu)
                       (lambda (items &rest _) (setq menu items)))
                      ((symbol-function 'run-at-time) #'ignore))
              (+noema/switch-buffer-menu nil)
              (should (string-match-p (regexp-quote (buffer-name hidden))
                                      (prin1-to-string menu)))
              (+noema--switch-to-buffer visible)
              (should (eq (window-buffer source-window) source))
              (+noema--switch-to-buffer hidden)
              (should (eq (window-buffer source-window) hidden))))
        (dolist (buffer (list source visible hidden))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest +noema-open-xwidget-keeps-client-identity-after-link-navigation ()
  (let* ((file-a (make-temp-file "noema-visit-a" nil ".md"))
         (file-b (make-temp-file "noema-visit-b" nil ".md"))
         (old (generate-new-buffer " *noema-visit-old*"))
         (fresh (generate-new-buffer " *noema-visit-fresh*"))
         (id-a (+noema--xwidget-session-id file-a))
         (+noema--port 4242)
         opened-id tracked-client)
    (unwind-protect
        (progn
          (with-current-buffer old
            (setq-local +noema-buffer-file-name
                        (+noema--canonical-file file-b))
            (setq-local +noema--client-id id-a))
          (cl-letf (((symbol-function '+noema--buffer-for-file)
                     (lambda (file) (and (equal file (+noema--canonical-file file-b)) old)))
                    ((symbol-function '+xwidget-session-buffer)
                     (lambda (id) (and (equal id id-a) old)))
                    ((symbol-function '+xwidget/open-url)
                     (lambda (_url &rest args)
                       (setq opened-id (plist-get args :id))
                       fresh))
                    ((symbol-function '+noema--track-app-buffer)
                     (lambda (_buffer _file client)
                       (setq tracked-client client)))
                    ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
            ;; Reopening B must address the page's original client, not B's
            ;; nominal id.  Reopening A needs a fresh client because that id
            ;; still belongs to the live page now showing B.
            (+noema--open-xwidget "ignored" file-b)
            (should (equal tracked-client id-a))
            (+noema--open-xwidget "ignored" file-a)
            (should (string-prefix-p (concat id-a ":visit:") opened-id))
            (should (equal tracked-client opened-id))))
      (dolist (buffer (list old fresh))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (dolist (file (list file-a file-b))
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest +noema-hosted-surface-opens-requested-route-without-stealing-note ()
  (let* ((existing (generate-new-buffer " *noema-hosted-surface*"))
         (fresh (generate-new-buffer " *noema-hosted-fresh*"))
         (+noema--port 4242)
         visited-id visited-url)
    (unwind-protect
        (cl-letf (((symbol-function '+xwidget-session-buffer)
                   (lambda (id) (and (equal id "noema:surface") existing)))
                  ((symbol-function '+xwidget-current-url)
                   (lambda (&optional _) "http://127.0.0.1:4242/wiki?client=noema"))
                  ((symbol-function '+xwidget/open-url)
                   (lambda (url &rest args)
                     (setq visited-url url visited-id (plist-get args :id))
                     fresh))
                  ((symbol-function '+noema--track-app-buffer) #'ignore)
                  ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
          (+noema--open-xwidget "http://127.0.0.1:4242/graph" nil)
          (should (equal visited-id "noema:surface"))
          (should (string-match-p (regexp-quote "/graph?client=noema%3Asurface")
                                  visited-url))
          (with-current-buffer existing
            (setq-local +noema-buffer-file-name "/tmp/kept.md"))
          (+noema--open-xwidget "http://127.0.0.1:4242/wiki" nil)
          (should (string-prefix-p "noema:surface:visit:" visited-id))
          (should (equal (+noema-buffer-file existing) "/tmp/kept.md")))
      (dolist (buffer (list existing fresh))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +noema-split-registration-does-not-steal-canonical-file-buffer ()
  (let* ((file (make-temp-file "noema-split-register" nil ".md"))
         (canonical (generate-new-buffer "*Noema-canonical*"))
         (split (generate-new-buffer "*Noema-split*"))
         (client "noema-split:/tmp/note.md:7")
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--client-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (+noema--register-buffer
           canonical file (+noema--xwidget-session-id file) nil)
          (+noema--register-buffer split file client nil)
          (should (eq (gethash (+noema--canonical-file file)
                               +noema--file-buffers)
                      canonical))
          (should (eq (gethash client +noema--client-buffers) split))
          (should (eq (+noema--buffer-for-file file) canonical))
          (with-current-buffer split
            (should (equal +noema--xwidget-forced-name
                           (format "*Noema split 7: %s*"
                                   (file-name-nondirectory file))))))
      (when (buffer-live-p canonical) (kill-buffer canonical))
      (when (buffer-live-p split) (kill-buffer split))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest +noema-run-emacs-key-queues-m-x ()
  (let ((buffer (generate-new-buffer "*Noema-test*"))
        (unread-command-events nil)
        (+noema--app-buffer nil)
        focused-frame
        edit-mode-arg
        scheduled-fn
        scheduled-args)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq +noema--app-buffer buffer)
          (cl-letf (((symbol-function 'select-frame-set-input-focus)
                     (lambda (frame) (setq focused-frame frame)))
                    ((symbol-function 'xwidget-webkit-edit-mode)
                     (lambda (arg) (setq edit-mode-arg arg)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest args)
                       (setq scheduled-fn function)
                       (setq scheduled-args args)
                       'mock-timer)))
            (noema--run-emacs-key "M-x"))
          (should (equal unread-command-events
                         (listify-key-sequence (kbd "M-x"))))
          (should (eq (selected-window) (get-buffer-window buffer 'visible)))
          (should (eq focused-frame (selected-frame)))
          (should (equal edit-mode-arg -1))
          (should (eq scheduled-fn #'noema--focus-forwarded-key-target))
          (should (equal scheduled-args
                         (list (get-buffer-window buffer 'visible)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-run-emacs-key-queues-prefix-sequence ()
  (let ((unread-command-events nil)
        (+noema--app-buffer nil)
        scheduled-fn
        scheduled-args)
    (cl-letf (((symbol-function 'select-frame-set-input-focus)
               (lambda (_frame) nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (setq scheduled-fn function)
                 (setq scheduled-args args)
                 'mock-timer)))
      (noema--run-emacs-key "C-x C-f"))
    (should (equal unread-command-events
                   (listify-key-sequence (kbd "C-x C-f"))))
    (should (eq scheduled-fn #'noema--focus-forwarded-key-target))
    (should (equal scheduled-args (list (selected-window))))))

(ert-deftest +noema-focus-forwarded-key-target-focuses-window-left-from-noema ()
  (let ((source (generate-new-buffer "*Noema-source*"))
        (target (generate-new-buffer "*Noema-target*"))
        (+noema--app-buffer nil)
        focused-frame)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq +noema--app-buffer source)
          (let ((source-window (selected-window))
                (target-window (split-window-right)))
            (set-window-buffer target-window target)
            (select-window target-window)
            (cl-letf (((symbol-function 'select-frame-set-input-focus)
                       (lambda (frame) (setq focused-frame frame))))
              (noema--focus-forwarded-key-target source-window))
            (should (eq (selected-window) target-window))
            (should (eq focused-frame (selected-frame)))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest +noema-portal-focus-keeps-selected-copy-of-page ()
  "A page shown twice keeps the chosen pane when it receives focus."
  (require 'noema/bridge/portal)
  (let ((page (generate-new-buffer "*Noema Portal focus test*"))
        focused)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer page
            (setq-local major-mode 'portal-web-mode
                        +noema--client-id "focus-test"))
          (switch-to-buffer page)
          (let ((right (split-window-right)))
            (set-window-buffer right page)
            (select-window right)
            (cl-letf (((symbol-function '+portal--focus-selected-window)
                       (lambda (_frame &optional _changed)
                         (setq focused (selected-window)))))
              (+noema-portal-focus page))
            (should (eq (selected-window) right))
            (should (eq focused right))))
      (when (buffer-live-p page) (kill-buffer page)))))

(ert-deftest +noema-portal-focus-gained-notifies-editor ()
  "Portal's focus transition sends Noema's existing editor focus command."
  (require 'noema/bridge/portal)
  (let ((page (generate-new-buffer "*Noema Portal focus event*"))
        commands)
    (unwind-protect
        (with-current-buffer page
          (setq-local major-mode 'portal-web-mode
                      +noema--client-id "focus-event-test")
          (cl-letf (((symbol-function '+noema/command)
                     (lambda (command &optional _detail)
                       (push command commands))))
            (run-hooks '+portal-focus-gained-hook))
          (should (equal commands '("focus"))))
      (when (buffer-live-p page) (kill-buffer page)))))

(ert-deftest +noema-windmove-to-portal-uses-shared-focus-manager ()
  "Noema's windmove bridge delegates Portal focus once to its shared path."
  (require 'noema/bridge/portal)
  (let ((source (generate-new-buffer " *Noema windmove source*"))
        (page (generate-new-buffer " *Noema windmove Portal*"))
        focused-frame
        (focus-count 0))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((source-window (selected-window))
                (page-window (split-window-right)))
            (with-current-buffer page
              (setq-local major-mode 'portal-web-mode
                          +noema--client-id "windmove-test"))
            (set-window-buffer page-window page)
            (select-window page-window)
            (cl-letf (((symbol-function '+portal--focus-selected-window)
                       (lambda (frame &optional _changed)
                         (setq focused-frame frame)
                         (cl-incf focus-count))))
              (noema--focus-selected-window-after-move source-window))
            (should (eq focused-frame (selected-frame)))
            (should (= focus-count 1))))
      (dolist (buffer (list source page))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +noema-buffer-for-client-finds-unregistered-split ()
  (let ((buffer (generate-new-buffer "*Noema-split*"))
        (+noema--client-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local +noema--client-id "noema-split:/tmp/note.md:1"))
          (should-not (gethash "noema-split:/tmp/note.md:1"
                               +noema--client-buffers))
          (should (eq (+noema--buffer-for-client
                       "noema-split:/tmp/note.md:1")
                      buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-xwidget-buffer-p-accepts-local-noema-client ()
  (let ((buffer (generate-new-buffer "*Noema-split*"))
        (+noema--app-buffer nil)
        (+noema--port nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local +noema--client-id "noema-split:/tmp/note.md:1"))
          (should (noema--xwidget-buffer-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-run-emacs-key-uses-client-source-window ()
  (let ((main (generate-new-buffer "*Noema-main*"))
        (split (generate-new-buffer "*Noema-split*"))
        (+noema--app-buffer nil)
        (+noema--client-buffers (make-hash-table :test #'equal))
        (unread-command-events nil)
        released-buffer
        scheduled-fn
        scheduled-args)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (with-current-buffer main
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local +noema--client-id "main-client"))
          (setq +noema--app-buffer main)
          (let ((main-window (selected-window))
                (split-window (split-window-right)))
            (set-window-buffer split-window split)
            (with-current-buffer split
              (setq-local major-mode 'xwidget-webkit-mode)
              (setq-local +noema--client-id "split-client"))
            (select-window main-window)
            (cl-letf (((symbol-function 'select-frame-set-input-focus)
                       (lambda (_frame) nil))
                      ((symbol-function 'xwidget-webkit-edit-mode)
                       (lambda (arg)
                         (when (equal arg -1)
                           (setq released-buffer (current-buffer)))))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat function &rest args)
                         (setq scheduled-fn function)
                         (setq scheduled-args args)
                         'mock-timer)))
              (noema--run-emacs-key "M-x" "split-client"))
            (should (eq (selected-window) split-window))
            (should (eq released-buffer split))
            (should (equal unread-command-events
                           (listify-key-sequence (kbd "M-x"))))
            (should (eq scheduled-fn #'noema--focus-forwarded-key-target))
            (should (equal scheduled-args (list split-window)))))
      (when (buffer-live-p main) (kill-buffer main))
      (when (buffer-live-p split) (kill-buffer split)))))

(ert-deftest +noema-key-event-passes-client-to-run-emacs-key ()
  (let (seen deferred)
    (cl-letf (((symbol-function 'json-serialize)
               (lambda (&rest _args)
                 (ert-fail "structured key events must not be reserialized")))
              ((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args)
                 (setq deferred (cons function args))))
              ((symbol-function 'noema--run-emacs-key)
               (lambda (key &optional client host-owned)
                 (setq seen (list key client host-owned)))))
      (+noema--gateway-event
       '((type . "key")
         (payload . ((key . "M-<right>")
                     (client . "split-client"))))
       nil)
      (should-not seen)
      (apply (car deferred) (cdr deferred))
      (should (equal seen '("M-<right>" "split-client" nil)))
      ;; A stray key WebKit got while Emacs owned the keyboard says so.
      (+noema--gateway-event
       '((type . "key")
         (payload . ((key . "<down>") (client . "split-client") (hostOwned . t))))
       nil)
      (apply (car deferred) (cdr deferred))
      (should (equal seen '("<down>" "split-client" t)))
      ;; Characters typed while Emacs owns the keyboard become typed input.
      (let ((unread-command-events nil))
        (+noema--gateway-event
         '((type . "key")
           (payload . ((key . "") (text . "ls") (client . "split-client") (hostOwned . t))))
         nil)
        (apply (car deferred) (cdr deferred))
        (should (equal unread-command-events '(?l ?s)))))))

(ert-deftest +noema-ace-window-hint-renders-inside-xwidget ()
  "Ace labels for native xwidgets travel to their owning page."
  (require 'ace-window)
  (let ((buffer (generate-new-buffer " *Noema ace hint*"))
        (+noema--ace-hint-buffers nil)
        (aw-leading-char-style 'char)
        sent)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (cl-letf (((symbol-function 'noema--xwidget-buffer-p)
                     (lambda (&optional candidate) (eq candidate buffer)))
                    ((symbol-function '+noema/command)
                     (lambda (command &optional detail)
                       (push (cons command detail) sent)))
                    ((symbol-function 'aw--remove-leading-chars) #'ignore))
            (+noema--ace-window-lead '(?a) (cons 1 (selected-window)))
            (should (equal (car sent) '("window-hint" (label . "a"))))
            (+noema--ace-window-remove)
            (should (equal (car sent) '("window-hint-clear")))
            (should-not +noema--ace-hint-buffers)))
      (kill-buffer buffer))))

(ert-deftest +noema-key-event-ignores-non-string-key-data ()
  (let (seen)
    (cl-letf (((symbol-function 'json-serialize)
               (lambda (&rest _args)
                 (ert-fail "structured key events must not be reserialized")))
              ((symbol-function 'noema--run-emacs-key)
               (lambda (&rest args) (setq seen args))))
      (should
       (equal
        (+noema--gateway-event
         '((type . "key")
           (payload . ((key . [1 nil 74])
                       (client . "split-client"))))
         nil)
        '((ok . t)))))
    (should-not seen)))

(ert-deftest +noema-selection-event-stays-with-its-own-pane ()
  "A selected range can only become context for its source xwidget file."
  (require 'noema-md-bridge)
  (let* ((file (make-temp-file "noema-agent-selection-" nil ".md"
                               "one\ntwo\nthree\nfour\nfive\n"))
         (other (make-temp-file "noema-agent-other-" nil ".md"))
         (buffer (generate-new-buffer " *Noema selected pane*"))
         (+noema--client-buffers (make-hash-table :test #'equal))
         deferred sent)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local +noema--client-id "selected-pane")
            (setq-local +noema-buffer-file-name file))
          (puthash "selected-pane" buffer +noema--client-buffers)
          (cl-letf (((symbol-function '+noema--defer-host-event)
                     (lambda (function &rest args)
                       (setq deferred (cons function args))))
                    ((symbol-function 'noema-md-bridge-run)
                     (lambda (action source begin end)
                       (setq sent (list action
                                        (with-current-buffer source
                                          (buffer-substring-no-properties begin end)))))))
            (+noema--gateway-event
             `((type . "selection-to-agent")
               (payload . ((client . "selected-pane") (file . ,file)
                           (lineStart . 2) (lineEnd . 4)))) nil)
            (apply (car deferred) (cdr deferred))
            ;; The action runs as its own step after the event queue.
            (sleep-for 0.05)
            (should (equal sent '("agent" "two\nthree\nfour\n")))
            (setq sent nil)
            (+noema--gateway-event
             `((type . "selection-to-agent")
               (payload . ((client . "selected-pane") (file . ,other)
                           (lineStart . 2) (lineEnd . 4)))) nil)
            ;; A refusal is shown to the person, not raised from a timer.
            (should (equal (apply (car deferred) (cdr deferred))
                           "Noema selection no longer matches its pane"))
            (should-not sent)))
      (when-let* ((visiting (find-buffer-visiting file))) (kill-buffer visiting))
      (kill-buffer buffer)
      (delete-file file)
      (delete-file other))))

(ert-deftest +noema-agent-selection-key-keeps-its-emacs-binding ()
  "The forwarded C-c A v sequence invokes the shared context command."
  (should (eq (key-binding (kbd "C-c A v")) #'noema-context-send-region)))

(ert-deftest +noema-input-focus-corrects-stale-pane-bookkeeping ()
  (let* ((buffer (generate-new-buffer " *Noema stale focus*"))
         (+noema--app-buffer nil)
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--last-activity-signature 'stale)
         selected
         updates)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local +noema--client-id "client-stale")
            (setq-local +noema--activity-paused t))
          (puthash "client-stale" buffer +noema--client-buffers)
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (candidate &optional _all-frames)
                       (and (eq candidate buffer) (selected-window))))
                    ((symbol-function 'select-window)
                     (lambda (window &rest _) (setq selected window)))
                    ((symbol-function 'noema--select-emacs-window)
                     (lambda (&rest _)
                       (ert-fail "Input focus must not steal WebKit's first responder")))
                    ((symbol-function '+noema--update-activity)
                     (lambda (&rest _args) (setq updates (1+ (or updates 0))))))
            (+noema--handle-input-focus
             '((client . "client-stale") (file . "/tmp/note.md"))))
          (should (eq +noema--app-buffer buffer))
          (with-current-buffer buffer
            (should (eq +noema--activity-paused :unknown)))
          ;; The visible owning window was already selected in this harness.
          (should-not selected)
          (should (eq +noema--last-activity-signature :unknown))
          (should (= updates 1)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-ui-state-gateway-does-not-reserialize-status ()
  (let ((+noema-echo-severity 'error)
        (binary-status (unibyte-string 1 255 123))
        seen)
    (cl-letf (((symbol-function 'json-serialize)
               (lambda (&rest _args)
                 (ert-fail "structured UI-state must not be reserialized")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq seen (apply #'format format-string args))))
              ((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args) (apply function args))))
      (+noema--gateway-event
       `((type . "ui-state")
         (payload . ((status . ,binary-status)
                     (severity . "error"))))
       nil))
    (should (string-prefix-p "Noema error: " seen))
    (should-not (string-match-p "parse failed" seen))))

(ert-deftest +noema-ui-state-ignores-malformed-status-fields ()
  (let ((+noema-echo-severity 'error)
        seen)
    (cl-letf (((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args) (apply function args)))
              ((symbol-function 'message)
               (lambda (&rest args) (push args seen))))
      (should
       (equal
        (+noema--gateway-event
         '((type . "ui-state")
           (payload . ((status . [1 nil 123])
                       (severity . ["error"]))))
         nil)
        '((ok . t)))))
    (should-not seen)))

(ert-deftest +noema-zotero-events-dispatch-structured-payloads ()
  (let (seen)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (apply function args)))
              ((symbol-function '+noema--run-zotero-event)
               (lambda (payload import-p)
                 (push (list payload import-p) seen))))
      (+noema--gateway-event
       '((type . "zotero")
         (payload . ((key . "Str87")
                     (doi . "10.1515/example"))))
       nil)
      (+noema--gateway-event
       '((type . "zotero-import")
         (payload . ((currentFile . "/tmp/note.md")
                     (targetFile . "/tmp/bib/test.bib"))))
       nil))
    (should (equal (cadar seen) t))
    (should (equal (alist-get 'targetFile (caar seen))
                   "/tmp/bib/test.bib"))
    (should-not (cadadr seen))
    (should (equal (alist-get 'key (caadr seen)) "Str87"))))

(ert-deftest +zotero-reference-result-falls-back-from-bib-key-to-doi ()
  (let ((expected '((id . "http://zotero.org/users/1/items/C4ULFN2X")
                    (citekey . "Strassen")
                    (DOI . "10.1515/example")))
        calls)
    (cl-letf (((symbol-function '+zotero-better-bibtex-search)
               (lambda (terms)
                 (push terms calls)
                 (if (equal terms [["DOI" "is" "10.1515/example"]])
                     (list expected)
                   nil))))
      (let ((result (+zotero-reference-result
                     '((key . "Str87") (doi . "https://doi.org/10.1515/example")))))
        (should (equal result expected))
        (should (= (length calls) 1))
        (should (equal (+zotero-result-select-uri result)
                       "zotero://select/library/items/C4ULFN2X"))))))

(ert-deftest +zotero-append-bibtex-does-not-duplicate-a-key ()
  (let ((file (make-temp-file "noema-zotero-test-" nil ".bib"))
        (bibtex "@article{UniqueKey,\n  title = {One}\n}\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _args)
                     (error "should not visit BibTeX target")))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (&rest _args)
                     (error "should not display BibTeX target"))))
          (+zotero-append-bibtex file bibtex)
          (+zotero-append-bibtex file bibtex)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (should (re-search-forward "^@article{UniqueKey," nil t))
            (should-not (re-search-forward "^@article{UniqueKey," nil t)))
          (should-not (get-file-buffer file)))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest +noema-windmove-focus-advice-focuses-noema-target ()
  (let ((source (generate-new-buffer "*Noema-source*"))
        (target (generate-new-buffer "*Noema-target*"))
        (+noema--app-buffer nil)
        edit-buffer
        focused-buffer)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((target-window (split-window-right)))
            (set-window-buffer target-window target)
            (with-current-buffer target
              (setq-local major-mode 'xwidget-webkit-mode))
            (setq +noema--app-buffer target)
            (cl-letf (((symbol-function 'xwidget-webkit-edit-mode)
                       (lambda (arg)
                         (when (= arg 1) (setq edit-buffer (current-buffer)))))
                      ((symbol-function '+noema/command)
                       (lambda (command &optional _detail)
                         (when (equal command "focus")
                           (setq focused-buffer (current-buffer))))))
              (should (eq (noema--windmove-focus-advice
                           (lambda ()
                             (select-window target-window)
                             'moved))
                          'moved)))
            (should (eq (selected-window) target-window))
            (should (eq edit-buffer target))
            (should (eq focused-buffer target))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest +noema-focus-move-between-noema-windows-releases-source-and-focuses-target ()
  (let ((source (generate-new-buffer "*Noema-source*"))
        (target (generate-new-buffer "*Noema-split*"))
        (released-buffer nil)
        (focused-buffer nil)
        (armed-buffer nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (with-current-buffer source
            (setq-local major-mode 'xwidget-webkit-mode))
          (let ((source-window (selected-window))
                (target-window (split-window-right)))
            (set-window-buffer target-window target)
            (with-current-buffer target
              (setq-local major-mode 'xwidget-webkit-mode))
            (select-window target-window)
            (cl-letf (((symbol-function 'noema--xwidget-buffer-p)
                       (lambda (&optional buffer)
                         (memq (or buffer (current-buffer)) (list source target))))
                      ((symbol-function 'xwidget-webkit-edit-mode)
                       (lambda (arg)
                         (if (equal arg -1)
                             (setq released-buffer (current-buffer))
                           (when (equal arg 1)
                             (setq armed-buffer (current-buffer))))))
                      ((symbol-function '+noema/command)
                       (lambda (command &optional _detail)
                         (when (equal command "focus")
                           (setq focused-buffer (current-buffer))))))
              (noema--focus-selected-window-after-move source-window))
            (should (eq released-buffer source))
            (should (eq armed-buffer target))
            (should (eq focused-buffer target))
            (should (eq +noema--app-buffer target))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest +noema-gateway-event-handles-ready ()
  (let* ((+noema--port nil)
         (+noema--ready nil)
         (flush-count 0)
         deferred)
    (cl-letf (((symbol-function '+noema--flush-ready-callbacks)
               (lambda () (cl-incf flush-count)))
              ((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args)
                 (setq deferred (cons function args)))))
      (+noema--gateway-event
       '((type . "ready") (payload . ((port . 50815)))) nil)
      (should-not +noema--ready)
      (apply (car deferred) (cdr deferred))
      (should +noema--ready)
      (should (= +noema--port 50815))
      (should (= flush-count 1)))))

(ert-deftest +noema-gateway-event-ignores-invalid-ready-port ()
  (let* ((+noema--port nil)
         (+noema--ready nil)
         (flush-count 0))
    (cl-letf (((symbol-function '+noema--flush-ready-callbacks)
               (lambda () (cl-incf flush-count)))
              ((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args) (apply function args))))
      (+noema--gateway-event
       '((type . "ready") (payload . ((port . "not-a-port")))) nil)
      (should-not +noema--ready)
      (should-not +noema--port)
      (should (= flush-count 0)))))

(ert-deftest +noema-gateway-event-opens-safe-hosted-surface-in-emacs ()
  (let ((+noema--port 50815)
        deferred opened)
    (cl-letf (((symbol-function '+noema--defer-host-event)
               (lambda (function &rest args)
                 (setq deferred (cons function args))))
              ((symbol-function '+noema--ensure-server)
               (lambda (callback) (funcall callback)))
              ((symbol-function '+noema--open-url)
               (lambda (url &rest _args) (setq opened url))))
      (+noema--gateway-event
       '((type . "surface") (payload . ((path . "/wiki?new=1")))) nil)
      (apply (car deferred) (cdr deferred))
      (should (equal opened "http://127.0.0.1:50815/wiki?new=1")))))

(ert-deftest +noema-hosted-surface-reuses-side-pane-and-keeps-editor ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *noema-surface-source*"))
           (surface (generate-new-buffer " *noema-surface-target*"))
           (source-window (selected-window))
           (+noema-backend 'xwidget)
           (+noema--port 4242)
           (+noema--client-buffers (make-hash-table :test #'equal))
           opened)
      (unwind-protect
          (progn
            (set-window-buffer source-window source)
            (with-current-buffer source
              (setq-local +noema--client-id "source-client"))
            (puthash "source-client" source +noema--client-buffers)
            (cl-letf (((symbol-function '+noema--ensure-server)
                       (lambda (callback) (funcall callback)))
                      ((symbol-function '+xwidget-session-buffer)
                       (lambda (_id) (and opened surface)))
                      ((symbol-function '+noema--open-url)
                       (lambda (&rest _)
                         (setq opened t)
                         (set-window-buffer (selected-window) surface))))
              (+noema--open-hosted-surface
               '((path . "/wiki") (client . "source-client")))
              (should (eq (window-buffer source-window) source))
              (should (= (length (window-list nil 'no-minibuf)) 2))
              (select-window source-window)
              (+noema--open-hosted-surface
               '((path . "/agenda") (client . "source-client")))
              (should (= (length (window-list nil 'no-minibuf)) 2))
              (should (eq (window-buffer source-window) source))))
        (when (buffer-live-p source) (kill-buffer source))
        (when (buffer-live-p surface) (kill-buffer surface))))))

(ert-deftest +noema-hosted-surface-rejects-external-or-unknown-pages ()
  (let (opened)
    (cl-letf (((symbol-function '+noema--open-url)
               (lambda (&rest args) (setq opened args))))
      (+noema--open-hosted-surface '((path . "https://example.org/config")))
      (+noema--open-hosted-surface '((path . "/admin")))
      (should-not opened))))

(ert-deftest +noema-hosted-surface-ignores-a-closed-client ()
  (let ((+noema--client-buffers (make-hash-table :test #'equal))
        opened)
    (cl-letf (((symbol-function '+noema--ensure-server)
               (lambda (&rest _) (setq opened t))))
      (+noema--open-hosted-surface
       '((path . "/wiki") (client . "closed"))))
    (should-not opened)))

(ert-deftest +noema-sentinel-resets-dead-current-process ()
  (let* ((buffer (generate-new-buffer " *Noema-sentinel-test*"))
         (proc (make-process
                :name "noema-sentinel-test"
                :buffer buffer
                :command (list "cat")))
         (+noema--process proc)
         (+noema--port 50815)
         (+noema--ready t)
         (+noema--ready-callbacks (list #'ignore))
         (+noema--ready-watchdog nil))
    (unwind-protect
        (progn
          (delete-process proc)
          (+noema--sentinel proc "exited abnormally with code 1\n")
          (should-not +noema--process)
          (should-not +noema--port)
          (should-not +noema--ready)
          (should-not +noema--ready-callbacks))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-active-focus-restarts-core-on-the-same-port ()
  "An active retained page restarts core without killing or refreshing it."
  (let* ((buffer (generate-new-buffer "*Noema-active-reconnect*"))
         (+noema--app-buffer buffer)
         (+noema--process nil)
         (+noema--ready nil)
         (+noema--last-port 50815)
         (+noema--ready-callbacks nil)
         (+noema--ready-watchdog nil)
         started-port)
    (unwind-protect
        (cl-letf (((symbol-function '+noema--app-buffer-visible-p)
                   (lambda () t))
                  ((symbol-function '+noema--start-server)
                   (lambda (&optional port) (setq started-port port)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _args) 'reconnect-watchdog)))
          (+noema--maybe-reconnect-core-on-activity)
          (should (= started-port 50815))
          (should (memq #'+noema--notify-xwidgets-core-ready
                        +noema--ready-callbacks))
          (should (eq +noema--ready-watchdog 'reconnect-watchdog)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-inactive-page-does-not-restart-core ()
  "A dead core remains dormant until the Noema page is actively focused."
  (let* ((buffer (generate-new-buffer "*Noema-inactive-reconnect*"))
         (+noema--app-buffer buffer)
         (+noema--process nil)
         (+noema--ready nil)
         (+noema--last-port 50815)
         (+noema--ready-callbacks nil)
         started)
    (unwind-protect
        (cl-letf (((symbol-function '+noema--app-buffer-visible-p)
                   (lambda () nil))
                  ((symbol-function '+noema--start-server)
                   (lambda (&optional _port) (setq started t))))
          (+noema--maybe-reconnect-core-on-activity)
          (should-not started)
          (should-not +noema--ready-callbacks))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-core-ready-reconnects-retained-xwidget-in-place ()
  (let* ((buffer (generate-new-buffer "*Noema-retained-xwidget*"))
         (+noema--app-buffer buffer)
         (+noema--activity-hooks-installed t)
         (+noema--last-activity-signature 'stale)
         executed-script
         activity-updates)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local +noema--activity-paused t))
          (cl-letf (((symbol-function 'xwidget-webkit-current-session)
                     (lambda () 'mock-session))
                    ((symbol-function 'xwidget-webkit-execute-script)
                     (lambda (_session script &optional _callback)
                       (setq executed-script script)))
                    ((symbol-function '+noema--update-activity)
                     (lambda (&rest _args)
                       (setq activity-updates (1+ (or activity-updates 0))))))
            (+noema--notify-xwidgets-core-ready)
            (should (equal executed-script +noema--core-ready-script))
            (with-current-buffer buffer
              (should (eq +noema--activity-paused :unknown)))
            (should (eq +noema--last-activity-signature :unknown))
            (should (= activity-updates 1))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-runtime-status-renders-debug-payload ()
  (let ((+noema--ready t)
        channel args displayed)
    (unwind-protect
        (cl-letf (((symbol-function '+noema-api-call)
                   (lambda (c a callback &optional _timeout)
                     (setq channel c
                           args a)
                     (let ((payload (make-hash-table :test 'equal))
                           (sync (make-hash-table :test 'equal)))
                       (puthash "type" "runtime-debug" payload)
                       (puthash "queued" :false sync)
                       (puthash "roamDbSync" sync payload)
                       (funcall callback payload nil))))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq displayed buffer)
                     buffer)))
          (+noema/runtime-status)
          (should (equal channel "noema:api:runtime:debug"))
          (should (equal args []))
          (should (eq displayed (get-buffer "*Noema runtime status*")))
          (with-current-buffer "*Noema runtime status*"
            (should (derived-mode-p 'special-mode))
            (should (string-match-p "\"runtime-debug\"" (buffer-string)))
            (should (string-match-p "\"emacsActivity\"" (buffer-string)))))
      (when-let* ((buffer (get-buffer "*Noema runtime status*")))
        (kill-buffer buffer)))))

(ert-deftest +noema-api-call-uses-nonblocking-gateway-request ()
  (let ((+noema--ready t)
        request
        callback-result)
    (cl-letf (((symbol-function 'remote-gateway-find-client)
               (lambda (_client-id) 'mock-client))
              ((symbol-function 'remote-gateway-request-sync)
               (lambda (&rest _args)
                 (ert-fail "async Noema API must not wait synchronously")))
              ((symbol-function 'remote-gateway-request-async)
               (lambda (client method params callback timeout)
                 (setq request (list client method params timeout))
                 (funcall callback '((ok . t)) nil)
                 '(done))))
      (+noema--api-call
       "noema:api:test" [1 2]
       (lambda (result) (setq callback-result result)))
      (should
       (equal request
              '(mock-client "noema.api"
                            ((channel . "noema:api:test")
                             (args . [1 2]))
                            10)))
      (should (equal callback-result '((ok . t)))))))

(ert-deftest +noema-stop-releases-gateway-binding ()
  (let ((+noema--gateway-binding '(:binding-id "binding-test"))
        (+noema--process nil)
        released)
    (cl-letf (((symbol-function 'remote-gateway-release-binding)
               (lambda (binding &optional disconnect)
                 (setq released (list binding disconnect))))
              ((symbol-function '+noema--remove-activity-hooks)
               #'ignore))
      (+noema/stop)
      (should
       (equal released
              '((:binding-id "binding-test") t)))
      (should-not +noema--gateway-binding))))

(ert-deftest +noema-watchdog-clears-stale-ready-callbacks ()
  (let ((+noema--ready nil)
        (+noema--ready-watchdog 'mock-timer)
        (+noema--ready-callbacks (list #'ignore)))
    (+noema--watchdog-fire)
    (should-not +noema--ready-watchdog)
    (should-not +noema--ready-callbacks)))

(ert-deftest +noema-retry-on-unready-live-host-gets-a-new-watchdog ()
  "Retrying a hung live host must have a bounded callback lifetime."
  (let* ((buffer (generate-new-buffer " *noema-unready-host*"))
         (process (make-process :name "noema-unready-host-test"
                                :buffer buffer :command '("cat")))
         (+noema--process process)
         (+noema--ready nil)
         (+noema--ready-watchdog nil)
         (+noema--ready-callbacks nil)
         (callback #'ignore)
         scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (delay repeat fn &rest _args)
                     (setq scheduled (list delay repeat fn))
                     'retry-watchdog)))
          (+noema--ensure-server callback)
          (should (equal scheduled
                         '(10 nil +noema--watchdog-fire)))
          (should (eq +noema--ready-watchdog 'retry-watchdog))
          (should (equal +noema--ready-callbacks (list callback))))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-start-failure-removes-unrunnable-callback ()
  (let ((+noema--process nil)
        (+noema--ready nil)
        (+noema--ready-watchdog nil)
        (+noema--ready-callbacks nil))
    (cl-letf (((symbol-function '+noema--start-server)
               (lambda (&rest _args) (error "Missing host"))))
      (should-error (+noema--ensure-server #'ignore))
      (should-not +noema--ready-callbacks)
      (should-not +noema--ready-watchdog))))

(ert-deftest +noema-activity-hooks-install-idempotently ()
  (let ((after-focus-change-function nil)
        (focus-in-hook nil)
        (focus-out-hook nil)
        (window-buffer-change-functions nil)
        (window-selection-change-functions nil)
        (window-state-change-functions nil)
        (delete-frame-functions nil)
        (+noema--app-buffer nil)
        (+noema--client-buffers (make-hash-table :test #'equal))
        (+noema--file-buffers (make-hash-table :test #'equal))
        (+noema--activity-hooks-installed nil)
        (+noema--activity-timer nil)
        (+noema--paused t)
        (+noema--manual-paused t))
    (+noema--install-activity-hooks)
    (+noema--install-activity-hooks)
    (should +noema--activity-hooks-installed)
    (dolist (hook (list focus-in-hook focus-out-hook
                        window-buffer-change-functions
                        window-selection-change-functions
                        window-state-change-functions
                        delete-frame-functions))
      (should (= (length hook) 1))
      (should (eq (car hook) #'+noema--update-activity)))
    (+noema--remove-activity-hooks)
    (should-not +noema--activity-hooks-installed)
    (should-not after-focus-change-function)
    (should-not focus-in-hook)
    (should-not focus-out-hook)
    (should-not window-buffer-change-functions)
    (should-not window-selection-change-functions)
    (should-not window-state-change-functions)
    (should-not delete-frame-functions)
    (should-not +noema--paused)
    (should-not +noema--manual-paused)))

(ert-deftest +noema-activity-snapshot-follows-selected-focused-pane ()
  (let* ((first (generate-new-buffer " *Noema activity A*"))
         (second (generate-new-buffer " *Noema activity B*"))
         (plain (generate-new-buffer " *Noema activity plain*"))
         (configuration (current-window-configuration))
         (+noema--app-buffer first)
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--file-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash "client-a" first +noema--client-buffers)
          (puthash "client-b" second +noema--client-buffers)
          (delete-other-windows)
          (let* ((first-window (selected-window))
                 (second-window (split-window-right)))
            (set-window-buffer first-window first)
            (set-window-buffer second-window second)
            (select-window first-window)
            (cl-letf (((symbol-function 'frame-visible-p) (lambda (_frame) t))
                      ((symbol-function 'frame-focus-state) (lambda (_frame) t)))
              (should (equal (+noema--activity-snapshot)
                             (list (cons first t) (cons second nil))))
              (select-window second-window)
              (should (equal (+noema--activity-snapshot)
                             (list (cons first nil) (cons second t))))
              (set-window-buffer second-window plain)
              (should (equal (+noema--activity-snapshot)
                             (list (cons first nil) (cons second nil))))
              (cl-letf (((symbol-function 'frame-focus-state)
                         (lambda (_frame) nil)))
                (set-window-buffer second-window second)
                (should (equal (+noema--activity-snapshot)
                               (list (cons first nil) (cons second nil)))))
              (cl-letf (((symbol-function 'frame-visible-p)
                         (lambda (_frame) nil)))
                (should (equal (+noema--activity-snapshot)
                               (list (cons first nil) (cons second nil))))))))
      (set-window-configuration configuration)
      (dolist (buffer (list first second plain))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +noema-per-client-activity-transitions-target-the-shared-renderer ()
  (let* ((first (generate-new-buffer " *Noema client A*"))
         (second (generate-new-buffer " *Noema client B*"))
         (+noema--app-buffer first)
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--manual-paused nil)
         (+noema--paused nil)
         commands)
    (unwind-protect
        (progn
          (with-current-buffer first
            (setq-local +noema--client-id "client-a")
            (setq-local +noema--activity-paused :unknown))
          (with-current-buffer second
            (setq-local +noema--client-id "client-b")
            (setq-local +noema--activity-paused :unknown))
          (puthash "client-a" first +noema--client-buffers)
          (puthash "client-b" second +noema--client-buffers)
          (cl-letf (((symbol-function '+noema--send-command)
                     (lambda (command &optional _detail)
                       (push (list +noema--client-id command) commands))))
            (+noema--apply-activity-snapshot
             (list (cons first t) (cons second nil)))
            (should (equal (nreverse commands)
                           '(("client-a" "resume") ("client-b" "pause"))))
            (should-not +noema--paused)
            (setq commands nil)
            ;; Repeated focus/window events must not resend an unchanged state.
            (+noema--apply-activity-snapshot
             (list (cons first t) (cons second nil)))
            (should-not commands)
            (+noema--apply-activity-snapshot
             (list (cons first nil) (cons second t)))
            (should (equal (nreverse commands)
                           '(("client-a" "pause") ("client-b" "resume"))))
            (setq commands nil)
            (+noema--apply-activity-snapshot
             (list (cons first nil) (cons second nil)))
            (should (equal commands '(("client-b" "pause"))))
            (should +noema--paused)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +noema-manual-pause-blocks-auto-resume-per-client ()
  (let* ((first (generate-new-buffer " *Noema manual A*"))
         (second (generate-new-buffer " *Noema manual B*"))
         (snapshot (list (cons first t) (cons second nil)))
         (+noema--app-buffer first)
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--manual-paused nil)
         (+noema--paused nil)
         commands)
    (unwind-protect
        (progn
          (with-current-buffer first
            (setq-local +noema--client-id "client-a")
            (setq-local +noema--activity-paused :unknown))
          (with-current-buffer second
            (setq-local +noema--client-id "client-b")
            (setq-local +noema--activity-paused :unknown))
          (puthash "client-a" first +noema--client-buffers)
          (puthash "client-b" second +noema--client-buffers)
          (cl-letf (((symbol-function '+noema--activity-snapshot)
                     (lambda () snapshot))
                    ((symbol-function '+noema--send-command)
                     (lambda (command &optional _detail)
                       (push (list +noema--client-id command) commands))))
            (+noema--apply-activity-snapshot snapshot)
            (setq commands nil)
            (+noema/pause)
            (should +noema--manual-paused)
            (should +noema--paused)
            (should (equal commands '(("client-a" "pause"))))
            (setq commands nil)
            (+noema--apply-activity-snapshot snapshot)
            (should-not commands)
            (should +noema--paused)
            (+noema/resume)
            (should-not +noema--manual-paused)
            (should-not +noema--paused)
            (should (equal commands '(("client-a" "resume"))))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest +noema-activity-update-uses-one-shot-event-debounce ()
  (let ((+noema--activity-timer nil)
        (+noema--last-activity-signature :unknown)
        (+noema--ready t)
        (+noema--app-buffer nil)
        (+noema--client-buffers (make-hash-table :test #'equal))
        (+noema--file-buffers (make-hash-table :test #'equal))
        (snapshot '((first . t) (second)))
        (scheduled nil)
        (cancelled nil)
        (applied nil))
    (cl-letf (((symbol-function '+noema--activity-snapshot)
               (lambda () snapshot))
              ((symbol-function '+noema--maybe-reconnect-core-on-activity)
               #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (delay repeat callback &rest args)
                 (should-not repeat)
                 (push (list delay callback args) scheduled)
                 (list 'timer (length scheduled))))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _args)
                 (ert-fail "activity resume must not depend on Emacs idleness")))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer cancelled)))
              ((symbol-function '+noema--apply-activity-snapshot)
               (lambda (value) (setq applied value))))
      (+noema--update-activity)
      (+noema--update-activity)
      (should (= (length scheduled) 1))
      (should (= (caar scheduled) 0.12))
      (setq snapshot '((first) (second)))
      (+noema--update-activity)
      (should (= (length scheduled) 2))
      (should (= (caar scheduled) 0.03))
      (should (= (length cancelled) 1))
      (pcase-let ((`(,_delay ,callback ,args) (car scheduled)))
        (apply callback args))
      (should (equal applied snapshot))
      (should-not +noema--activity-timer))))

(ert-deftest +noema-current-file-reconciles-initial-client-activity ()
  (let* ((buffer (generate-new-buffer " *Noema current-file activity*"))
         (+noema--app-buffer buffer)
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--activity-hooks-installed t)
         (+noema--last-activity-signature 'stale)
         updates)
    (unwind-protect
        (progn
          (puthash "client-a" buffer +noema--client-buffers)
          (with-current-buffer buffer
            (setq-local +noema--client-id "client-a")
            (setq-local +noema--activity-paused t))
          (cl-letf (((symbol-function '+noema--register-buffer)
                     (lambda (&rest _args) buffer))
                    ((symbol-function '+noema--update-activity)
                     (lambda (&rest _args) (setq updates (1+ (or updates 0))))))
            (+noema--sync-app-buffer-file "/tmp/reconciled.md" "client-a"))
          (with-current-buffer buffer
            (should (eq +noema--activity-paused :unknown)))
          (should (eq +noema--last-activity-signature :unknown))
          (should (= updates 1)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest +noema-process-filter-ignores-stale-proc-ready-line ()
  "A dying old process emitting a ready: line must not clobber the new port."
  (let* ((buffer (generate-new-buffer " *Noema-stale-proc-test*"))
         (stale-proc (make-process
                      :name "noema-stale-test"
                      :buffer buffer
                      :command (list "cat")))
         ;; Simulate a new process by using a different proc object as current.
         (new-proc (make-process
                    :name "noema-new-test"
                    :buffer buffer
                    :command (list "cat")))
         (+noema--process new-proc)
         (+noema--port 60000)
         (+noema--ready t)
         (+noema--process-log-queue nil)
         (+noema--process-log-bytes 0)
         (+noema--process-log-timer nil)
         (flush-count 0))
    (unwind-protect
        (cl-letf (((symbol-function '+noema--flush-ready-callbacks)
                   (lambda () (cl-incf flush-count))))
          ;; stale-proc (not current) emits a ready: line with a different port.
          (+noema--process-filter stale-proc "aaronote-web-host:ready:99999\n")
          ;; Port and ready state must be unchanged.
          (should (= +noema--port 60000))
          (should +noema--ready)
          (should (= flush-count 0)))
      (dolist (p (list stale-proc new-proc))
        (when (process-live-p p) (delete-process p)))
      (+noema--clear-process-log-queue)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest +noema-process-filter-defers-diagnostic-buffer-edits ()
  "Child output must leave the process filter before touching its log buffer."
  (let* ((buffer (generate-new-buffer " *Noema-deferred-log-test*"))
         (proc (make-process
                :name "noema-deferred-log-test"
                :buffer buffer
                :command (list "cat")))
         (+noema--process-log-queue nil)
         (+noema--process-log-bytes 0)
         (+noema--process-log-timer nil)
         scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function &rest args)
                     (setq scheduled (cons function args))
                     'mock-log-timer))
                  ((symbol-function 'timerp)
                   (lambda (timer) (eq timer 'mock-log-timer))))
          (+noema--process-filter proc "diagnostic line\n")
          (with-current-buffer buffer
            (should (string-empty-p (buffer-string))))
          (should (eq (car scheduled) #'+noema--flush-process-log))
          (apply (car scheduled) (cdr scheduled))
          (with-current-buffer buffer
            (should (equal (buffer-string) "diagnostic line\n"))))
      (when (process-live-p proc) (delete-process proc))
      (+noema--clear-process-log-queue)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defmacro +noema-test--with-xwidget-mocks (&rest body)
  "Run BODY with mocked xwidget opener state."
  (declare (indent 0))
  `(let ((+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--app-buffer nil)
         (+noema--port 50815)
         opened-urls
         buffers)
     (cl-letf (((symbol-function '+xwidget/open-url)
                (lambda (url &rest _args)
                  (push url opened-urls)
                  (let ((buffer (generate-new-buffer "*mock-xwidget*")))
                    (push buffer buffers)
                    (with-current-buffer buffer
                      (setq-local major-mode 'xwidget-webkit-mode))
                    (switch-to-buffer buffer)
                    buffer)))
               ((symbol-function '+xwidget-session-buffer)
                (lambda (_id) nil))
               ((symbol-function '+xwidget/focus)
                (lambda (&optional _buffer) nil))
               ((symbol-function 'run-at-time)
                (lambda (&rest _args) 'mock-timer))
               ((symbol-function '+noema--refresh-visible-ibuffers)
                (lambda () nil)))
       (unwind-protect
           (progn ,@body)
         (dolist (buffer buffers)
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))))

(ert-deftest +noema-open-xwidget-reuses-file-buffer ()
  (+noema-test--with-xwidget-mocks
    (let* ((file (expand-file-name "note.md" temporary-file-directory))
           (buf1 (+noema--open-xwidget "ignored" file))
           (buf2 (+noema--open-xwidget "ignored-again" file)))
      (should (eq buf1 buf2))
      (should (= (length opened-urls) 1))
      (should
       (equal (+noema-buffer-file buf1)
              (+noema--canonical-file file)))
      (should (string-match-p "\\*Noema: note\\.md\\*" (buffer-name buf1)))
      (should (string-match-p "client=" (car opened-urls))))))

(ert-deftest +noema-open-xwidget-selects-its-existing-window ()
  (save-window-excursion
    (+noema-test--with-xwidget-mocks
      (let* ((file (expand-file-name "visible.md" temporary-file-directory))
             (source-window (selected-window))
             (buffer (+noema--open-xwidget "ignored" file))
             (other (generate-new-buffer " *noema-existing-other*"))
             (other-window (split-window-right)))
        (unwind-protect
            (progn
              (set-window-buffer other-window other)
              (select-window other-window)
              (should (eq (+noema--open-xwidget "ignored" file) buffer))
              (should (eq (selected-window) source-window))
              (should (eq (window-buffer other-window) other))
              (should (= (length opened-urls) 1)))
          (kill-buffer other))))))

(ert-deftest +noema-open-xwidget-keeps-files-distinct ()
  (+noema-test--with-xwidget-mocks
    (let* ((file1 (expand-file-name "one.md" temporary-file-directory))
           (file2 (expand-file-name "two.md" temporary-file-directory))
           (buf1 (+noema--open-xwidget "ignored" file1))
           (buf2 (+noema--open-xwidget "ignored" file2)))
      (should-not (eq buf1 buf2))
      (should (= (length opened-urls) 2))
      (should
       (eq (gethash (+noema--canonical-file file1)
                    +noema--file-buffers)
           buf1))
      (should
       (eq (gethash (+noema--canonical-file file2)
                    +noema--file-buffers)
           buf2)))))

(ert-deftest +noema-kill-buffer-cleans-registries ()
  (+noema-test--with-xwidget-mocks
    (let* ((file (expand-file-name "cleanup.md" temporary-file-directory))
           (client (+noema--xwidget-session-id file))
           (buffer (+noema--open-xwidget "ignored" file))
           (canonical-file (+noema--canonical-file file))
           posted)
      (should
       (eq (gethash canonical-file +noema--file-buffers) buffer))
      (should (eq (gethash client +noema--client-buffers) buffer))
      (cl-letf (((symbol-function '+noema--post)
                 (lambda (payload) (push payload posted))))
        (kill-buffer buffer))
      (should-not (gethash canonical-file +noema--file-buffers))
      (should-not (gethash client +noema--client-buffers))
      (should (equal (alist-get 'type (car posted)) "client-close"))
      (should (equal (alist-get 'client (car posted)) client))
      (should
       (equal (alist-get 'file (car posted))
              (+noema--host-file canonical-file))))))

(ert-deftest +noema-current-file-client-targets-buffer ()
  (+noema-test--with-xwidget-mocks
    (let* ((file1 (expand-file-name "first.md" temporary-file-directory))
           (file2 (expand-file-name "second.md" temporary-file-directory))
           (buf1 (+noema--open-xwidget "ignored" file1))
           (buf2 (+noema--open-xwidget "ignored" nil)))
      (+noema--track-app-buffer buf2 nil "client-two")
      (setq +noema--app-buffer buf1)
      (+noema--sync-app-buffer-file file2 "client-two")
      (should (eq +noema--app-buffer buf2))
      (should
       (equal (+noema-buffer-file buf2)
              (+noema--canonical-file file2)))
      (should
       (eq (gethash (+noema--canonical-file file2)
                    +noema--file-buffers)
           buf2)))))

(ert-deftest +noema-current-file-ignores-a-closed-client ()
  (+noema-test--with-xwidget-mocks
    (let* ((file-a (expand-file-name "alive.md" temporary-file-directory))
           (file-b (expand-file-name "stale.md" temporary-file-directory))
           (buffer (+noema--open-xwidget "ignored" file-a)))
      (puthash "closed-client" buffer +noema--client-buffers)
      (+noema--sync-app-buffer-file file-b "closed-client")
      (should (equal (+noema-buffer-file buffer)
                     (+noema--canonical-file file-a)))
      (should-not (gethash "closed-client" +noema--client-buffers))
      (should-not (+noema--buffer-for-file file-b)))))

(ert-deftest +noema-file-registry-rejects-a-navigated-buffer ()
  (+noema-test--with-xwidget-mocks
    (let* ((file-a (expand-file-name "old.md" temporary-file-directory))
           (file-b (expand-file-name "new.md" temporary-file-directory))
           (buffer (+noema--open-xwidget "ignored" file-a)))
      (with-current-buffer buffer
        (setq-local +noema-buffer-file-name
                    (+noema--canonical-file file-b)))
      (should-not (+noema--buffer-for-file file-a))
      (should-not (gethash (+noema--canonical-file file-a)
                           +noema--file-buffers)))))

(ert-deftest +noema-canonical-buffer-prefers-registered-buffer ()
  (+noema-test--with-xwidget-mocks
    (let* ((file (expand-file-name "duplicate.md" temporary-file-directory))
           (canonical (+noema--open-xwidget "ignored" file))
           (duplicate (generate-new-buffer "*Noema: duplicate.md*")))
      (push duplicate buffers)
      (with-current-buffer duplicate
        (setq-local major-mode 'xwidget-webkit-mode)
        (setq-local +noema-buffer-file-name file))
      (should (eq (+noema-canonical-buffer duplicate) canonical)))))

(ert-deftest +noema-jupyter-notebooks-are-logical-ipynb-files ()
  (should
   (equal
    (+noema-jupyter--file
     '((file . "/fs:local:/tmp/notes/.cell/note.python.default.ipynb")))
    "/fs:local:/tmp/notes/.cell/note.python.default.ipynb"))
  (should-error
   (+noema-jupyter--file
    '((file . "/fs:local:/tmp/notes/note.md")))
   :type 'error)
  (should
   (equal
   (+noema-jupyter--file
    '((file . "/fs:local:/tmp/notes/experiments/note.ipynb")))
   "/fs:local:/tmp/notes/experiments/note.ipynb")))

(ert-deftest +noema-jupyter-normalizes-target-kernelspecs ()
  (let* ((payload
          '((kernelspecs
             (python3
              (resource_dir . "/target/share/jupyter/kernels/python3")
              (spec
               (argv . ["/target/bin/python" "-m" "ipykernel_launcher"
                        "-f" "{connection_file}"])
               (language . "python"))))))
         (entry (car (+noema-jupyter--normalize-kernelspecs payload))))
    (should (equal (+noema-jupyter--get 'name entry) "python3"))
    (should
     (equal (+noema-jupyter--get 'resourceDir entry)
            "/target/share/jupyter/kernels/python3"))
    (should
     (equal
      (append
       (+noema-jupyter--get
        'argv (+noema-jupyter--get 'spec entry))
       nil)
      '("/target/bin/python" "-m" "ipykernel_launcher"
        "-f" "{connection_file}")))))

(ert-deftest +noema-jupyter-project-kernelspec-templates-are-resolved ()
  (cl-letf (((symbol-function 'remote-client-file-name)
             (lambda (&rest _) "/tmp/note.md")))
    (let* ((specs
            (+noema-jupyter--project-kernelspecs
             "/fs:local:/tmp/note.md"))
           (python
            (seq-find
             (lambda (entry)
               (equal (alist-get 'name entry) "python3"))
             specs))
           (spec (alist-get 'spec python))
           (argv (alist-get 'argv spec))
           (env (alist-get 'env spec)))
      (should python)
      (should (file-name-absolute-p (car argv)))
      (should-not
       (string-match-p "@NOEMA" (remote-gateway--encode spec)))
      (should
       (string-suffix-p
        "/var/noema/jupyter/runtime"
        (alist-get 'JUPYTER_RUNTIME_DIR env))))))

(ert-deftest +noema-jupyter-registers-remote-broker-methods ()
  (dolist (method
           '("noema.jupyter.kernels"
             "noema.jupyter.launch"
             "noema.jupyter.status"
             "noema.jupyter.interrupt"
             "noema.jupyter.restart"
             "noema.jupyter.shutdown"
             "noema.jupyter.attach"
             "noema.jupyter.attachment.status"
             "noema.jupyter.attachment.release"
             "noema.jupyter.read-nbextension"
             "noema.jupyter.file.read"
             "noema.jupyter.file.write"
             "noema.jupyter.file.delete"
             "noema.jupyter.file.stat"))
    (should (functionp (gethash method remote-gateway--methods))))
  (should
   (memq #'+noema-jupyter--doctor
         remote-doctor-check-functions)))

(ert-deftest +noema-jupyter-does-not-register-an-emacs-protocol-engine ()
  (dolist (method
           '("noema.jupyter.document.snapshot"
             "noema.jupyter.manager.snapshot"
             "noema.jupyter.script.snapshot"
             "noema.jupyter.script.action"
             "noema.jupyter.session.select"
             "noema.jupyter.kernel.control"
             "noema.jupyter.document.execute"
             "noema.jupyter.document.mutate"
             "noema.jupyter.output.clear"
             "noema.jupyter.output.clear-all"
             "noema.jupyter.variables"
             "noema.jupyter.kernel.interrupt"
             "noema.jupyter.kernel.restart"
             "noema.jupyter.kernel.shutdown"
             "noema.jupyter.kernel.channel"
             "noema.jupyter.board.open"))
    (should-not (functionp (gethash method remote-gateway--methods)))))

(ert-deftest +noema-public-api-callback-reports-offline ()
  (let ((+noema--ready nil) received)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (apply function args))))
      (+noema-api-call
       "noema:api:jupyter-cell:tasks" []
       (lambda (result error) (setq received (list result error)))))
    (should-not (car received))
    (should (equal (alist-get 'code (cadr received)) "offline"))))

(ert-deftest +noema-public-api-callback-reports-success ()
  (let ((+noema--ready t) received)
    (cl-letf (((symbol-function 'remote-gateway-find-client)
               (lambda (_name) 'client))
              ((symbol-function 'remote-gateway-request-async)
               (lambda (_client _method _params callback _timeout)
                 (funcall callback '((ok . t) (kernels . nil)) nil))))
      (+noema-api-call
       "noema:api:jupyter-cell:tasks" []
       (lambda (result error) (setq received (list result error)))))
    (should-not (cadr received))
    (should (eq t (alist-get 'ok (car received))))))

(ert-deftest +noema-jupyter-runtime-snapshot-is-target-filtered ()
  (let* ((+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (context (remote-context (remote-target-file-name "local" "/tmp/")))
         (runtime
          (+noema-jupyter-runtime-create
           :id "runtime-1" :manager-id "kernel-1"
           :context context :kernel "python3"
           :placement-file "/tmp/note.md"
           :pid 123 :generation 2 :state 'idle)))
    (puthash "runtime-1" runtime +noema-jupyter-runtimes)
    (let ((snapshot (+noema-jupyter-runtime-snapshot "local")))
      (should (= (length snapshot) 1))
      (should (equal (plist-get (car snapshot) :runtime-id) "runtime-1"))
      (should (equal (plist-get (car snapshot) :manager-kernel-id) "kernel-1"))
      (should-not (plist-member (car snapshot) :source-file))
      (should (= (plist-get (car snapshot) :generation) 2)))
    (should-not (+noema-jupyter-runtime-snapshot "missing"))))

(ert-deftest +noema-jupyter-cell-file-auto-installs-noema-ui ()
  (let* ((root (make-temp-file "noema-jupyter-direct-" t))
         (store (expand-file-name ".cell" root))
         (source (expand-file-name "note.md" root))
         (script (expand-file-name "note.python.default.ipynb" store))
         buffer)
    (unwind-protect
        (progn
          (make-directory store t)
          (with-temp-file source (insert "# Note\n"))
          (with-temp-file script
            (insert (json-serialize
                     `((cells . [((cell_type . "code") (id . "cell-a")
                                  (metadata . ()) (execution_count . nil)
                                  (outputs . []) (source . "value = 1\n"))])
                       (metadata . ((kernelspec . ((name . "python3")
                                                  (display_name . "Python 3")
                                                  (language . "python")))
                                    (language_info . ((name . "python")))
                                    (noema . ((source_file . ,source)
                                              (session . "default")))))
                       (nbformat . 4) (nbformat_minor . 5)))
                    "\n"))
          (setq buffer (find-file-noselect script))
          (with-current-buffer buffer
            (should +noema-jupyter-cell-mode)
            (should (file-equal-p +noema-jupyter-cell-source-file source))
            (should (equal +noema-jupyter-cell-kernel "python3"))
            (should (equal +noema-jupyter-cell-language "python"))
            (goto-char (point-min))
            (search-forward "value = 1")
            (+noema-jupyter-cell--update-highlight)
            (should (equal +noema-jupyter-cell-current-id "cell-a"))
            (should (eq (key-binding (kbd "C-c C-c"))
                        #'+noema-jupyter/cell-run-current))
            (should (eq (key-binding (kbd "C-c C-i"))
                        #'+noema-jupyter/cell-run-current))
            (should (eq (key-binding (kbd "M-RET"))
                        #'+noema-jupyter/cell-jump-output))
            (should (eq (key-binding (kbd "s-RET"))
                        #'+noema-jupyter/cell-jump-output))
            (should (eq (key-binding (kbd "C-c C-p"))
                        #'+noema-jupyter/output-page))
            (should (equal header-line-format
                           '(:eval (+noema-jupyter-cell--header-line))))
            (goto-char (point-max))
            (+noema-jupyter-cell--update-highlight)
            (should (file-equal-p
                     (alist-get 'scriptFile
                                (+noema-jupyter-cell--command-detail))
                     script))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest +noema-jupyter-cell-starts-web-host-once-per-pending-buffer ()
  (with-temp-buffer
    (let ((+noema--ready nil)
          callback
          (starts 0)
          (refreshes 0))
      (cl-letf (((symbol-function '+noema--ensure-server)
                 (lambda (&optional ready-callback)
                   (setq starts (1+ starts)
                         callback ready-callback)))
                ((symbol-function '+noema-jupyter/cell-refresh-status)
                 (lambda () (setq refreshes (1+ refreshes)))))
        (+noema-jupyter-cell--ensure-noema-host)
        (+noema-jupyter-cell--ensure-noema-host)
        (should (= starts 1))
        (should +noema-jupyter-cell--host-ready-requested)
        (setq +noema--ready t)
        (funcall callback)
        (should-not +noema-jupyter-cell--host-ready-requested)
        (should (= refreshes 1))
        (+noema-jupyter-cell--ensure-noema-host)
        (should (= starts 1))
        (should (= refreshes 2))))))

(ert-deftest +noema-jupyter-cell-ordinary-ipynb-auto-installs-noema-ui ()
  (let* ((file (make-temp-file "ordinary-noema-ui-" nil ".ipynb"))
         (+noema--ready t)
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (json-serialize
              '((cells . [((cell_type . "code") (id . "cell-a")
                           (metadata . ()) (execution_count . nil)
                           (outputs . []) (source . "value = 1\n"))])
                (metadata . ((kernelspec . ((name . "python3")
                                            (display_name . "Python 3")
                                            (language . "python")))
                             (language_info . ((name . "python")))))
                (nbformat . 4) (nbformat_minor . 5)))
             "\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should +noema-jupyter-cell-mode)
            (should (file-equal-p +noema-jupyter-cell-source-file file))
            (should (equal +noema-jupyter-cell-kernel "python3"))
            (should (eq (key-binding (kbd "C-c C-p"))
                        #'+noema-jupyter/output-page))
            ;; Noema persists outputs by rewriting the ipynb.  Auto-revert
            ;; must restore the language projection *and* its NCell controls.
            (revert-buffer :ignore-auto :noconfirm)
            (should +noema-jupyter-cell-mode)
            (should (equal header-line-format
                           '(:eval (+noema-jupyter-cell--header-line))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (ignore-errors (delete-file file)))))

(ert-deftest +noema-jupyter-cell-emacs-snippet-action-uses-noema-id-owner ()
  (with-temp-buffer
    (insert "# %% id=cell-a\njmd")
    (goto-char (point-max))
    (setq-local +noema-jupyter-cell-mode t)
    (let (mutation)
      (cl-letf (((symbol-function 'save-buffer) #'ignore)
                ((symbol-function '+noema-jupyter-cell--mutate)
                 (lambda (operation &optional extra)
                   (setq mutation (list operation extra)))))
        (should (+noema-jupyter-cell-expand-snippet-action)))
      (should (equal mutation
                     '("insertBelow" ((cellType . "markdown")))))
      (should-not (string-match-p "jmd" (buffer-string)))
      ;; The Emacs action deliberately supplies no id; Noema generates it.
      (should-not (assq 'cellId (cadr mutation))))))


;;; Remote Jupyter servers.
;;
;; The rule these guard is the Remote-first one: a server that lives on a
;; Target must be reached through a routed channel, and a Target that cannot
;; provide one must produce an error — never a silent client-side connection,
;; which would either fail confusingly or reach a different server that happens
;; to answer on that port here.

(defmacro +noema-jupyter-server-tests--with (servers &rest body)
  "Evaluate BODY with SERVERS configured and no forwards held open."
  (declare (indent 1))
  `(let ((+noema-jupyter-servers ,servers)
         (+noema-jupyter-server--forwards (make-hash-table :test #'equal))
         (+noema-jupyter-server--workspaces (make-hash-table :test #'equal)))
     ,@body))

(ert-deftest +noema-jupyter-cell-raw-snippet-action-uses-noema-id-owner ()
  (with-temp-buffer
    (insert "# %% id=cell-a\njraw")
    (goto-char (point-max))
    (setq-local +noema-jupyter-cell-mode t)
    (let (mutation)
      (cl-letf (((symbol-function 'save-buffer) #'ignore)
                ((symbol-function '+noema-jupyter-cell--mutate)
                 (lambda (operation &optional extra)
                   (setq mutation (list operation extra)))))
        (should (+noema-jupyter-cell-expand-snippet-action)))
      (should (equal mutation
                     '("insertBelow" ((cellType . "raw")))))
      (should-not (string-match-p "jraw" (buffer-string)))
      ;; The Emacs action deliberately supplies no id; Noema generates it.
      (should-not (assq 'cellId (cadr mutation))))))


;;; Remote Jupyter servers.
;;
;; The rule these guard is the Remote-first one: a server that lives on a
;; Target must be reached through a routed channel, and a Target that cannot
;; provide one must produce an error — never a silent client-side connection,
;; which would either fail confusingly or reach a different server that happens
;; to answer on that port here.

(defmacro +noema-jupyter-server-tests--with (servers &rest body)
  "Evaluate BODY with SERVERS configured and no forwards held open."
  (declare (indent 1))
  `(let ((+noema-jupyter-servers ,servers)
         (+noema-jupyter-server--forwards (make-hash-table :test #'equal))
         (+noema-jupyter-server--workspaces (make-hash-table :test #'equal)))
     ,@body))

(ert-deftest +noema-jupyter-server-local-target-is-used-verbatim ()
  (+noema-jupyter-server-tests--with
      '((:id "lab" :url "http://127.0.0.1:8888/" :target "local" :auth none))
    (cl-letf (((symbol-function '+noema-jupyter-server--forward)
               (lambda (&rest _) (error "must not open a channel for target local"))))
      (let ((resolved
             (+noema-jupyter-server--resolve-entry
              (+noema-jupyter-server--entry "lab"))))
        (should (equal (alist-get 'url resolved) "http://127.0.0.1:8888/"))
        (should (equal (alist-get 'target resolved) "local"))
        (should (equal (alist-get 'kind resolved) "server"))
        (should-not (alist-get 'serverName resolved))))))

(ert-deftest +noema-jupyter-server-remote-target-routes-through-a-channel ()
  (+noema-jupyter-server-tests--with
      '((:id "hpc" :url "https://login.example.org:9999/" :target "cluster" :auth none))
    (cl-letf (((symbol-function '+noema-jupyter-server--forward)
               (lambda (&rest _) 'fake-forward))
              ((symbol-function 'remote-channel-endpoint)
               (lambda (forward side)
                 (should (eq forward 'fake-forward))
                 (should (eq side 'local))
                 '(:host "127.0.0.1" :port 45123))))
      (let ((resolved
             (+noema-jupyter-server--resolve-entry
              (+noema-jupyter-server--entry "hpc"))))
        ;; Noema is handed the client end of the forward...
        (should (equal (alist-get 'url resolved) "https://127.0.0.1:45123/"))
        ;; ...plus the real name, or TLS verification and any virtual-host
        ;; routing on the server would fail against a 127.0.0.1 URL.
        (should (equal (alist-get 'serverName resolved) "login.example.org"))))))

(ert-deftest +noema-jupyter-server-forward-has-one-owner-and-recovers-the-same-port ()
  (+noema-jupyter-server-tests--with
      '((:id "hpc" :url "http://service:8888/" :target "cluster" :auth none))
    (let ((entry (+noema-jupyter-server--entry "hpc"))
          (opened 0)
          open-arguments recover
          (resource (remote-workspace-resource-create :value 'old-forward)))
      (cl-letf (((symbol-function 'remote-make-file-name)
                 (lambda (&rest _) "/fs:cluster:/"))
                ((symbol-function 'remote-context) (lambda (&rest _) 'context))
                ((symbol-function 'remote-workspace-open) (lambda (&rest _) 'workspace))
                ((symbol-function 'remote-channel-live-p) (lambda (&rest _) t))
                ((symbol-function 'remote-port-forward)
                 (lambda (endpoint &rest arguments)
                   (setq opened (1+ opened))
                   (push (cons endpoint arguments) open-arguments)
                   (if (= opened 1) 'old-forward 'new-forward)))
                ((symbol-function 'remote-workspace-ensure-recoverable-resource)
                 (lambda (_workspace _kind _key _value &rest arguments)
                   (setq recover (plist-get arguments :recover))
                   'resource))
                ((symbol-function 'remote-channel-endpoint)
                 (lambda (forward side)
                   (should (eq forward 'old-forward))
                   (should (eq side 'local))
                   '(:host "127.0.0.1" :port 45123))))
        (should (eq (+noema-jupyter-server--forward entry) 'old-forward))
        ;; The generic `forward' resource is deliberately disabled; the keyed
        ;; jupyter-server resource is the sole lifecycle owner.
        (should (plist-member (cdar open-arguments) :register))
        (should-not (plist-get (cdar open-arguments) :register))
        (should (eq (funcall recover resource 'workspace) 'new-forward))
        (should
         (equal (plist-get (cdar open-arguments) :local-endpoint)
                '(:host "127.0.0.1" :port 45123)))
        (should (eq (gethash "hpc" +noema-jupyter-server--forwards)
                    'new-forward))))))

(ert-deftest +noema-jupyter-server-release-survives-config-removal ()
  (+noema-jupyter-server-tests--with nil
    (let (closed)
      (puthash "hpc" 'old-forward +noema-jupyter-server--forwards)
      (puthash "hpc" 'owning-workspace +noema-jupyter-server--workspaces)
      (cl-letf (((symbol-function '+noema-jupyter--defer)
                 (lambda (fn) (funcall fn)))
                ((symbol-function 'remote-workspace-find-resource)
                 (lambda (workspace kind key)
                   (should (eq workspace 'owning-workspace))
                   (should (eq kind 'jupyter-server))
                   (should (equal key "hpc"))
                   'resource))
                ((symbol-function 'remote-workspace-close-resource)
                 (lambda (workspace resource reason)
                   (setq closed (list workspace resource reason))
                   ;; Exercise the registered close callback's observable
                   ;; cleanup, which the real workspace performs here.
                   (remhash "hpc" +noema-jupyter-server--forwards)
                   (remhash "hpc" +noema-jupyter-server--workspaces)
                   resource)))
        (should
         (equal (+noema-jupyter-server--release
                 '((serverId . "hpc")) nil)
                '((ok . t))))
        (should (equal closed
                       '(owning-workspace resource consumer-release)))
        (should-not (gethash "hpc" +noema-jupyter-server--forwards))
        (should-not (gethash "hpc" +noema-jupyter-server--workspaces))))))

(ert-deftest +noema-jupyter-server-registration-failure-rolls-back-forward ()
  (+noema-jupyter-server-tests--with
      '((:id "hpc" :url "http://service:8888/" :target "cluster" :auth none))
    (let ((entry (+noema-jupyter-server--entry "hpc"))
          closed)
      (cl-letf (((symbol-function 'remote-make-file-name)
                 (lambda (&rest _) "/fs:cluster:/"))
                ((symbol-function 'remote-context) (lambda (&rest _) 'context))
                ((symbol-function 'remote-workspace-open) (lambda (&rest _) 'workspace))
                ((symbol-function 'remote-port-forward)
                 (lambda (&rest _) 'unowned-forward))
                ((symbol-function 'remote-workspace-ensure-recoverable-resource)
                 (lambda (&rest _) (error "registration failed")))
                ((symbol-function 'remote-close-channel)
                 (lambda (forward) (setq closed forward))))
        (should-error (+noema-jupyter-server--forward entry) :type 'error)
        (should (eq closed 'unowned-forward))
        (should-not (gethash "hpc" +noema-jupyter-server--forwards))
        (should-not (gethash "hpc" +noema-jupyter-server--workspaces))))))

(ert-deftest +noema-jupyter-server-without-a-channel-fails-instead-of-falling-back ()
  (+noema-jupyter-server-tests--with
      '((:id "hpc" :url "http://127.0.0.1:8888/" :target "cluster" :auth none))
    (cl-letf (((symbol-function '+noema-jupyter-server--forward)
               (lambda (&rest _) (error "no channel capability for target"))))
      ;; The URL is loopback and would "work" if we simply passed it through —
      ;; which is exactly the silent-wrong-server failure this must not do.
      (should-error
       (+noema-jupyter-server--resolve-entry
        (+noema-jupyter-server--entry "hpc"))
       :type 'error))))

(ert-deftest +noema-jupyter-server-channel-without-a-port-is-an-error ()
  (+noema-jupyter-server-tests--with
      '((:id "hpc" :url "http://svc:8888/" :target "cluster" :auth none))
    (cl-letf (((symbol-function '+noema-jupyter-server--forward)
               (lambda (&rest _) 'fake-forward))
              ((symbol-function 'remote-channel-endpoint)
               (lambda (&rest _) '(:host "127.0.0.1"))))
      (should-error
       (+noema-jupyter-server--resolve-entry
        (+noema-jupyter-server--entry "hpc"))
       :type 'error))))

(ert-deftest +noema-jupyter-server-endpoint-applies-default-ports ()
  (should (equal (+noema-jupyter-server--endpoint '(:id "a" :url "https://x.test/"))
                 '("x.test" . 443)))
  (should (equal (+noema-jupyter-server--endpoint '(:id "a" :url "http://x.test/"))
                 '("x.test" . 80)))
  (should (equal (+noema-jupyter-server--endpoint '(:id "a" :url "http://x.test:8888/lab"))
                 '("x.test" . 8888))))

(ert-deftest +noema-jupyter-server-secrets-come-from-auth-source ()
  (+noema-jupyter-server-tests--with
      '((:id "lab" :url "https://lab.example.org/" :target "local" :auth token))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (should (equal (plist-get args :host) "lab.example.org"))
                 (list (list :secret (lambda () "s3cret"))))))
      (let ((resolved
             (+noema-jupyter-server--resolve-entry
              (+noema-jupyter-server--entry "lab"))))
        (should (equal (alist-get 'token resolved) "s3cret"))
        (should-not (alist-get 'password resolved))))))

(ert-deftest +noema-jupyter-server-password-auth-requires-a-secret ()
  (+noema-jupyter-server-tests--with
      '((:id "lab" :url "https://lab.example.org/" :target "local" :auth password))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should-error
       (+noema-jupyter-server--resolve-entry
        (+noema-jupyter-server--entry "lab"))
       :type 'error))))

(ert-deftest +noema-jupyter-server-auth-failure-opens-no-remote-forward ()
  (+noema-jupyter-server-tests--with
      '((:id "lab" :url "https://lab.example.org/" :target "cluster"
         :auth password))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil))
              ((symbol-function '+noema-jupyter-server--forward)
               (lambda (&rest _)
                 (error "forward must not open before credentials resolve"))))
      (should-error
       (+noema-jupyter-server--resolve-entry
        (+noema-jupyter-server--entry "lab"))
       :type 'error))))

(ert-deftest +noema-jupyter-server-listing-never-exposes-secrets ()
  (+noema-jupyter-server-tests--with
      '((:id "lab" :name "Lab" :url "https://lab.example.org/" :target "cluster"
         :auth token :kind gateway))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (list (list :secret (lambda () "s3cret")))))
              ((symbol-function '+noema-jupyter--defer) (lambda (fn) (funcall fn))))
      (let* ((payload (+noema-jupyter-server--list nil nil))
             (entry (car (alist-get 'servers payload))))
        (should (equal (alist-get 'id entry) "lab"))
        (should (equal (alist-get 'displayName entry) "Lab"))
        (should (equal (alist-get 'kind entry) "gateway"))
        (should (equal (alist-get 'target entry) "cluster"))
        ;; The listing is for a kernel picker; it must carry no credential.
        (should-not (alist-get 'token entry))
        (should-not (alist-get 'password entry))))))

(ert-deftest +noema-jupyter-connection-validation-requires-all-five-ports ()
  (let ((connection
         '((transport . "tcp") (ip . "127.0.0.1")
           (key . "secret") (signature_scheme . "hmac-sha256")
           (hb_port . 41001) (control_port . 41002)
           (shell_port . 41003) (stdin_port . 41004)
           (iopub_port . 41005))))
    (should (+noema-jupyter--valid-connection-p connection))
    (setf (alist-get 'stdin_port connection) nil)
    (should-not (+noema-jupyter--valid-connection-p connection))))

(ert-deftest +noema-jupyter-kernel-catalog-includes-target-attach-tokens ()
  (cl-letf (((symbol-function '+noema-jupyter--defer)
             (lambda (fn) (funcall fn)))
            ((symbol-function 'remote-canonicalize-file-name) #'identity)
            ((symbol-function '+noema-jupyter--kernelspecs)
             (lambda (_file) '(((name . "python3") (spec . ((argv . ["python"])))))))
            ((symbol-function '+noema-jupyter--attachable-connections)
             (lambda (_file)
               '(((token . "kernel-remote.json") (mtimeMs . 42.0))))))
    (let ((payload
           (+noema-jupyter--kernels
            '((file . "/fs:cluster:/work/note.md") (includeConnections . t)) nil)))
      (should (equal (alist-get 'connections payload)
                     '(((token . "kernel-remote.json") (mtimeMs . 42.0))))))))

(ert-deftest +noema-jupyter-attachment-recovers-five-stable-client-ports ()
  (let* ((+noema-jupyter-attachments (make-hash-table :test #'equal))
         (connection
          '((transport . "tcp") (ip . "0.0.0.0")
            (key . "secret") (signature_scheme . "hmac-sha256")
            (hb_port . 41001) (control_port . 41002)
            (shell_port . 41003) (stdin_port . 41004)
            (iopub_port . 41005)))
         (locals
          '((hb . (:host "127.0.0.1" :port 42001))
            (control . (:host "127.0.0.1" :port 42002))
            (shell . (:host "127.0.0.1" :port 42003))
            (stdin . (:host "127.0.0.1" :port 42004))
            (iopub . (:host "127.0.0.1" :port 42005))))
         (old-group (remote-channel-group-create :generation 1))
         (new-group (remote-channel-group-create :generation 2))
         (opens 0)
         open-arguments opened-endpoints recover attachment)
    (cl-letf (((symbol-function '+noema-jupyter--context)
               (lambda (&rest _) 'context))
              ((symbol-function 'remote-workspace-open)
               (lambda (&rest _) 'workspace))
              ((symbol-function '+noema-jupyter--target-runtime-directory)
               (lambda (&rest _) "/fs:cluster:/runtime/"))
              ((symbol-function '+noema-jupyter--read-connection-file)
               (lambda (&rest _) connection))
              ((symbol-function 'remote-channel-group-open)
               (lambda (endpoints &rest arguments)
                 (should (= (length endpoints) 5))
                 (setq opens (1+ opens))
                 (push endpoints opened-endpoints)
                 (push arguments open-arguments)
                 (if (= opens 1) old-group new-group)))
              ((symbol-function 'remote-channel-group-endpoints)
               (lambda (_group side)
                 (should (eq side 'local))
                 locals))
              ((symbol-function 'remote-workspace-ensure-recoverable-resource)
               (lambda (_workspace _kind _key _value &rest arguments)
                 (setq recover (plist-get arguments :recover))
                 'resource)))
      (setq attachment
            (+noema-jupyter--attach-runtime
             '((sourceFile . "/fs:cluster:/work/note.md")
               (token . "kernel-remote.json"))))
      (should (eq (+noema-jupyter-attachment-channel-group attachment)
                  old-group))
      (should
       (cl-every
        (lambda (endpoint)
          (equal (plist-get (cdr endpoint) :host) "127.0.0.1"))
        (car (last opened-endpoints))))
      (should
       (equal (alist-get
               'ip (+noema-jupyter-attachment-client-connection attachment))
              "127.0.0.1"))
      (should-not (plist-get (car open-arguments) :register))
      (let ((resource
             (remote-workspace-resource-create :value attachment)))
        (should (eq (funcall recover resource 'workspace) attachment)))
      (should (eq (+noema-jupyter-attachment-channel-group attachment)
                  new-group))
      (should (= (+noema-jupyter-attachment-generation attachment) 2))
      (should (equal (plist-get (car open-arguments) :local-endpoints)
                     locals)))))

(ert-deftest +noema-jupyter-attachment-missing-file-is-not-process-death ()
  (let* ((+noema-jupyter-attachments (make-hash-table :test #'equal))
         (attachment
          (+noema-jupyter-attachment-create
           :id "attachment-a" :workspace 'workspace
           :connection-file "/fs:cluster:/runtime/missing.json"
           :channel-group 'group :generation 1 :state 'open
           :client-connection '((hb_port . 42001)))))
    (puthash "attachment-a" attachment +noema-jupyter-attachments)
    (cl-letf (((symbol-function '+noema-jupyter--defer)
               (lambda (fn) (funcall fn)))
              ((symbol-function 'remote-channel-group-live-p)
               (lambda (group) (should (eq group 'group)) t))
              ((symbol-function 'file-exists-p) (lambda (_file) nil)))
      (let ((status
             (+noema-jupyter--attachment-status
              '((attachmentId . "attachment-a")) nil)))
        ;; A connection file is discovery metadata, not a process handle. Its
        ;; disappearance cannot authorize Noema to discard external state.
        (should (equal (alist-get 'alive status) "unknown"))
        (should (eq (alist-get 'connectionFilePresent status)
                    :json-false))))))

(ert-deftest +noema-jupyter-attachment-adopts-rewritten-five-port-file ()
  (let* ((+noema-jupyter-attachments (make-hash-table :test #'equal))
         (old-connection
          '((transport . "tcp") (ip . "127.0.0.1") (key . "old")
            (signature_scheme . "hmac-sha256")
            (hb_port . 41001) (control_port . 41002)
            (shell_port . 41003) (stdin_port . 41004)
            (iopub_port . 41005)))
         (new-connection
          '((transport . "tcp") (ip . "0.0.0.0") (key . "new")
            (signature_scheme . "hmac-sha256")
            (hb_port . 51001) (control_port . 51002)
            (shell_port . 51003) (stdin_port . 51004)
            (iopub_port . 51005)))
         (locals
          '((hb . (:host "127.0.0.1" :port 42001))
            (control . (:host "127.0.0.1" :port 42002))
            (shell . (:host "127.0.0.1" :port 42003))
            (stdin . (:host "127.0.0.1" :port 42004))
            (iopub . (:host "127.0.0.1" :port 42005))))
         (old-group (remote-channel-group-create :generation 1))
         (new-group (remote-channel-group-create :generation 2))
         (attachment
          (+noema-jupyter-attachment-create
           :id "attachment-a" :context 'context :workspace 'workspace
           :connection-file "/fs:cluster:/runtime/kernel-a.json"
           :target-connection old-connection
           :client-connection old-connection
           :channel-group old-group :generation 1 :state 'open))
         closed opened)
    (puthash "attachment-a" attachment +noema-jupyter-attachments)
    (cl-letf (((symbol-function '+noema-jupyter--defer)
               (lambda (fn) (funcall fn)))
              ((symbol-function 'file-exists-p) (lambda (_file) t))
              ((symbol-function '+noema-jupyter--read-connection-file)
               (lambda (_file) new-connection))
              ((symbol-function 'remote-channel-group-endpoints)
               (lambda (_group side) (should (eq side 'local)) locals))
              ((symbol-function 'remote-channel-group-close)
               (lambda (group) (setq closed group)))
              ((symbol-function 'remote-channel-group-open)
               (lambda (endpoints &rest arguments)
                 (setq opened (list endpoints arguments))
                 new-group))
              ((symbol-function 'remote-channel-group-live-p)
               (lambda (_group) t)))
      (let* ((status
              (+noema-jupyter--attachment-status
               '((attachmentId . "attachment-a")) nil))
             (client (alist-get 'connectionInfo status)))
        (should (eq closed old-group))
        (should (equal (plist-get (cadr opened) :local-endpoints) locals))
        (should
         (equal (mapcar (lambda (entry) (plist-get (cdr entry) :port))
                        (car opened))
                '(51001 51002 51003 51004 51005)))
        (should (equal (alist-get 'key client) "new"))
        (should (equal (alist-get 'ip client) "127.0.0.1"))
        (should (equal (alist-get 'hb_port client) 42001))
        (should (= (alist-get 'generation status) 2))))))

(ert-deftest +noema-jupyter-attachment-release-is-idempotent ()
  (let* ((+noema-jupyter-attachments (make-hash-table :test #'equal))
         (attachment
          (+noema-jupyter-attachment-create
           :id "attachment-a" :workspace 'workspace
           :channel-group 'group :generation 1 :state 'open))
         (closes 0))
    (puthash "attachment-a" attachment +noema-jupyter-attachments)
    (cl-letf (((symbol-function '+noema-jupyter--defer)
               (lambda (fn) (funcall fn)))
              ((symbol-function 'remote-workspace-find-resource)
               (lambda (&rest _) nil))
              ((symbol-function 'remote-channel-group-close)
               (lambda (group)
                 (should (eq group 'group))
                 (setq closes (1+ closes)))))
      (dotimes (_ 2)
        (should
         (equal (+noema-jupyter--release-attachment
                 '((attachmentId . "attachment-a")) nil)
                '((ok . t)))))
      (should (= closes 1))
      (should-not (gethash "attachment-a" +noema-jupyter-attachments)))))

;;; Broker kernel liveness and teardown.
;;
;; These cover the difference between "the target says the kernel is gone" and
;; "we could not ask the target", because every caller treats the first as
;; permission to discard the kernel's in-memory state.

(defun +noema-jupyter-test--runtime (&optional pid id)
  "Return a brokered runtime struct for liveness tests, owning PID and ID."
  (+noema-jupyter-runtime-create
   :id (or id "runtime-liveness") :manager-id "kernel-liveness"
   :context (remote-context (remote-target-file-name "local" "/tmp/"))
   :kernel "python3" :placement-file "/tmp/note.md"
   :pid (or pid 4321) :generation 1 :state 'idle))

(ert-deftest +noema-jupyter-alive-p-reports-a-confirmed-death ()
  (let ((runtime (+noema-jupyter-test--runtime)))
    (cl-letf (((symbol-function '+noema-jupyter--exit-status)
               (lambda (&rest _) 1)))
      (should (eq nil (+noema-jupyter--alive-p runtime))))))

(ert-deftest +noema-jupyter-alive-p-reports-a-live-process-group ()
  (let ((runtime (+noema-jupyter-test--runtime)))
    (cl-letf (((symbol-function '+noema-jupyter--exit-status)
               (lambda (&rest _) 0)))
      (should (eq t (+noema-jupyter--alive-p runtime))))))

(ert-deftest +noema-jupyter-alive-p-does-not-call-a-lost-target-dead ()
  "An unreachable target must be `unknown', never a dead kernel."
  (let ((runtime (+noema-jupyter-test--runtime)))
    (cl-letf (((symbol-function '+noema-jupyter--exit-status)
               (lambda (&rest _) (error "Connection closed by remote host"))))
      (should (eq 'unknown (+noema-jupyter--alive-p runtime))))))

(ert-deftest +noema-jupyter-alive-p-keeps-an-ssh-exit-unknown ()
  "SSH exit 255 reports a failed probe, not the target kernel's death."
  (let ((runtime (+noema-jupyter-test--runtime)))
    (cl-letf (((symbol-function '+noema-jupyter--exit-status)
               (lambda (&rest _) 255)))
      (should (eq 'unknown (+noema-jupyter--alive-p runtime))))))

(ert-deftest +noema-jupyter-alive-p-keeps-another-host-unknown ()
  "A load-balanced alias must not report an old PID missing on a new host."
  (let ((runtime (+noema-jupyter-test--runtime))
        command)
    (setf (+noema-jupyter-runtime-host-name runtime) "vx14")
    (cl-letf (((symbol-function '+noema-jupyter--exit-status)
               (lambda (_context program &rest args)
                 (setq command (cons program args))
                 77)))
      (should (eq 'unknown (+noema-jupyter--alive-p runtime)))
      (should (equal (car command) "sh"))
      (should (member "vx14" command))
      (should (member "-0" command)))))

(ert-deftest +noema-jupyter-host-guard-distinguishes-moved-host-from-death ()
  "Exercise the guarded shell probe on the real local process route."
  (let* ((runtime (+noema-jupyter-test--runtime 999999999))
         (host (+noema-jupyter--current-host
                (+noema-jupyter-runtime-context runtime))))
    (setf (+noema-jupyter-runtime-host-name runtime)
          "not-the-current-host.invalid")
    (should (eq 'unknown (+noema-jupyter--alive-p runtime)))
    (setf (+noema-jupyter-runtime-host-name runtime) host)
    (should-not (+noema-jupyter--alive-p runtime))))

(ert-deftest +noema-jupyter-host-guard-works-with-dash-kill ()
  "Dash's kill builtin rejects `--'; the guarded probe uses external kill."
  (skip-unless (file-executable-p "/bin/dash"))
  (let* ((runtime (+noema-jupyter-test--runtime 999999999))
         (host (+noema-jupyter--current-host
                (+noema-jupyter-runtime-context runtime)))
         (script (nth 2 (progn
                          (setf (+noema-jupyter-runtime-host-name runtime)
                                host)
                          (+noema-jupyter--kill-command runtime "0")))))
    (should (= 77 (call-process
                   "/bin/dash" nil nil nil "-c" script "sh"
                   "another-host.invalid" "-0" "-999999999")))
    (should (= 1 (call-process
                  "/bin/dash" nil nil nil "-c" script "sh"
                  host "-0" "-999999999")))))

(ert-deftest +noema-jupyter-recovery-keeps-uncertain-kernel-state ()
  "A moved host is visible as unknown without claiming state loss."
  (let ((runtime (+noema-jupyter-test--runtime))
        (liveness 'unknown))
    (setf (+noema-jupyter-runtime-state runtime) 'open)
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (_runtime) liveness)))
      (+noema-jupyter--refresh-recovered-runtime runtime)
      (should (eq (+noema-jupyter-runtime-state runtime) 'unknown))
      (should-not (+noema-jupyter-runtime-state-lost runtime))
      (setq liveness t)
      (+noema-jupyter--refresh-recovered-runtime runtime)
      (should (eq (+noema-jupyter-runtime-state runtime) 'open))
      (setq liveness nil)
      (+noema-jupyter--refresh-recovered-runtime runtime)
      (should (eq (+noema-jupyter-runtime-state runtime) 'dead))
      (should (+noema-jupyter-runtime-state-lost runtime)))))

(ert-deftest +noema-jupyter-signal-refuses-unknown-liveness ()
  "An uncertain route may point at a different host with a reused PID."
  (let ((runtime (+noema-jupyter-test--runtime))
        (sent nil))
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (&rest _) 'unknown))
              ((symbol-function '+noema-jupyter--output)
               (lambda (_context program &rest args)
                 (setq sent (cons program args))
                 "")))
      (should-error (+noema-jupyter--signal runtime "TERM"))
      (should-not sent))))

(ert-deftest +noema-jupyter-signal-rechecks-host-in-same-command ()
  "The signal command must guard against a host switch after liveness."
  (let ((runtime (+noema-jupyter-test--runtime))
        command)
    (setf (+noema-jupyter-runtime-host-name runtime) "vx14")
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (_runtime) t))
              ((symbol-function '+noema-jupyter--output)
               (lambda (_context program &rest args)
                 (setq command (cons program args))
                 "")))
      (+noema-jupyter--signal runtime "TERM")
      (should (equal (car command) "sh"))
      (should (member "vx14" command))
      (should (member "-TERM" command)))))

(ert-deftest +noema-jupyter-signal-skips-a-confirmed-death ()
  (let ((runtime (+noema-jupyter-test--runtime))
        (sent nil))
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (&rest _) nil))
              ((symbol-function '+noema-jupyter--output)
               (lambda (&rest _) (setq sent t) "")))
      (+noema-jupyter--signal runtime "TERM")
      (should-not sent))))

(ert-deftest +noema-jupyter-status-refuses-to-guess-for-an-unreachable-target ()
  "Noema shuts a kernel down on `alive: false', so an unanswerable probe errors."
  (let* ((+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (runtime (+noema-jupyter-test--runtime)))
    (puthash "runtime-liveness" runtime +noema-jupyter-runtimes)
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (&rest _) 'unknown)))
      (should-error
       (+noema-jupyter--runtime-status-payload
        (+noema-jupyter--runtime '((runtimeId . "runtime-liveness"))))))))

(ert-deftest +noema-jupyter-launch-registration-failure-forgets-routes ()
  "A failed final registration cannot leave reconnectable channel resources."
  (let* ((directory (make-temp-file "noema-jupyter-rollback-" t))
         (workspace (remote-workspace-create :root directory))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         forgotten stopped closed failed-id)
    (unwind-protect
        (cl-letf (((symbol-function '+noema-jupyter--context)
                   (lambda (_file)
                     (remote-context (remote-make-file-name
                                      "local" "/tmp/note.ipynb"))))
                  ((symbol-function 'remote-workspace-open)
                   (lambda (&rest _) workspace))
                  ((symbol-function '+noema-jupyter--kernelspecs)
                   (lambda (_file)
                     '(((name . "python3")
                        (spec . ((argv . ["python3" "{connection_file}"])))))))
                  ((symbol-function '+noema-jupyter--runtime-directory)
                   (lambda (_workspace) directory))
                  ((symbol-function '+noema-jupyter--json-output)
                   (lambda (&rest _) '(41001 41002 41003 41004 41005)))
                  ((symbol-function '+noema-jupyter--signing-key)
                   (lambda () "test-key"))
                  ((symbol-function '+noema-jupyter--output)
                   (lambda (&rest _) "12345"))
                  ((symbol-function 'remote-file-local-name) #'identity)
                  ((symbol-function 'remote-channel-group-open)
                   (lambda (_channels &rest options)
                     (let ((id (cadr (plist-get options :key))))
                       (should (+noema-jupyter--pending-shutdown-for id)))
                     'test-group))
                  ((symbol-function '+noema-jupyter--client-connection)
                   (lambda (target _group) target))
                  ((symbol-function 'remote-workspace-ensure-recoverable-resource)
                   (lambda (&rest _) (error "Registration failed")))
                  ((symbol-function '+noema-jupyter--forget-runtime-resources)
                   (lambda (runtime)
                     (setq forgotten (+noema-jupyter-runtime-id runtime))))
                  ((symbol-function '+noema-jupyter--signal)
                   (lambda (&rest _) (setq stopped t)))
                  ((symbol-function '+noema-jupyter--alive-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'remote-channel-group-close)
                   (lambda (group) (setq closed group))))
          (let ((failure
                 (should-error
                  (+noema-jupyter--launch-runtime
                   '((kernelId . "kernel-a")
                     (sourceFile . "/tmp/note.ipynb")
                     (kernelName . "python3"))))))
            (should (equal (error-message-string failure)
                           "Registration failed")))
          (should (stringp forgotten))
          (should stopped)
          (should (eq closed 'test-group))
          (should (zerop (hash-table-count +noema-jupyter-runtimes)))
          (should-not
           (directory-files +noema-jupyter-pending-directory nil
                            "\\.json\\'"))
          ;; A failed rollback must retain enough identity for the next
          ;; session, including the target connection file used for PID checks.
          (cl-letf (((symbol-function '+noema-jupyter--signal)
                     (lambda (runtime _signal)
                       (setq failed-id (+noema-jupyter-runtime-id runtime))
                       (error "Target unreachable"))))
            (should-error
             (+noema-jupyter--launch-runtime
              '((kernelId . "kernel-b")
                (sourceFile . "/tmp/note.ipynb")
                (kernelName . "python3")))))
          (let ((pending (gethash failed-id +noema-jupyter-runtimes)))
            (should pending)
            (should (equal (+noema-jupyter-runtime-host-name pending)
                           "12345"))
            (should (eq (+noema-jupyter-runtime-state pending)
                        'shutdown-pending))
            (should (file-exists-p
                     (+noema-jupyter-runtime-connection-file pending)))
            (should (file-exists-p
                     (+noema-jupyter--pending-file failed-id)))
            (should (equal
                     (+noema-jupyter--get
                      'hostName
                      (+noema-jupyter--pending-shutdown-for failed-id))
                     "12345"))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-launch-record-survives-abrupt-emacs-exit ()
  "A detached kernel is recoverable even if shutdown hooks never run."
  (let* ((directory (make-temp-file "noema-jupyter-abrupt-exit-" t))
         (workspace (remote-workspace-create :root directory))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         runtime)
    (unwind-protect
        (cl-letf (((symbol-function '+noema-jupyter--context)
                   (lambda (_file)
                     (remote-context (remote-make-file-name
                                      "local" "/tmp/note.ipynb"))))
                  ((symbol-function 'remote-workspace-open)
                   (lambda (&rest _) workspace))
                  ((symbol-function '+noema-jupyter--kernelspecs)
                   (lambda (_file)
                     '(((name . "python3")
                        (spec . ((argv . ["python3" "{connection_file}"])))))))
                  ((symbol-function '+noema-jupyter--runtime-directory)
                   (lambda (_workspace) directory))
                  ((symbol-function '+noema-jupyter--json-output)
                   (lambda (&rest _) '(41001 41002 41003 41004 41005)))
                  ((symbol-function '+noema-jupyter--signing-key)
                   (lambda () "test-key"))
                  ((symbol-function '+noema-jupyter--output)
                   (lambda (&rest _) "12345"))
                  ((symbol-function 'remote-file-local-name) #'identity)
                  ((symbol-function 'remote-channel-group-open)
                   (lambda (&rest _) 'test-group))
                  ((symbol-function '+noema-jupyter--client-connection)
                   (lambda (target _group) target))
                  ((symbol-function 'remote-workspace-ensure-recoverable-resource)
                   (lambda (&rest _) t)))
          (setq runtime
                (+noema-jupyter--launch-runtime
                 '((kernelId . "kernel-a")
                   (sourceFile . "/tmp/note.ipynb")
                   (kernelName . "python3"))))
          (should (+noema-jupyter--pending-shutdown-for
                   (+noema-jupyter-runtime-id runtime)))
          (should (equal
                   (+noema-jupyter--get
                    'hostName
                    (+noema-jupyter--pending-shutdown-for
                     (+noema-jupyter-runtime-id runtime)))
                   "12345"))
          (should-not (plist-get (car (+noema-jupyter-runtime-snapshot "local"))
                                 :journal-pending))
          ;; A SIGKILL loses the in-memory map without running any hooks.
          (clrhash +noema-jupyter-runtimes)
          (let ((saved (car (+noema-jupyter-runtime-snapshot "local"))))
            (should saved)
            (should (eq (plist-get saved :status) 'shutdown-pending))
            (should (plist-get saved :journal-pending))
            (should (equal (plist-get saved :pid) 12345))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-shutdown-releases-its-workspace-resources ()
  "A kernel that was shut down must not be recovered or signalled again."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (runtime (+noema-jupyter-test--runtime 4321))
         (workspace
          (remote-workspace-open
           (+noema-jupyter-runtime-context runtime) :connect nil))
         (signals 0)
         (recoveries 0))
    (setf (+noema-jupyter-runtime-workspace runtime) workspace)
    (puthash (+noema-jupyter-runtime-id runtime) runtime
             +noema-jupyter-runtimes)
    ;; What launching registers: the runtime and its five-channel group.
    (remote-workspace-ensure-recoverable-resource
     workspace 'jupyter-runtime (+noema-jupyter-runtime-id runtime) runtime
     :close (lambda (value reason)
              (unless (eq reason 'transport-recovery)
                (+noema-jupyter--shutdown-runtime value)))
     :recover (lambda (_resource _owner) (cl-incf recoveries) runtime))
    (remote-workspace-ensure-recoverable-resource
     workspace 'channel-group
     (list 'noema-jupyter (+noema-jupyter-runtime-id runtime)) 'group
     :close #'ignore
     :recover (lambda (_resource _owner) (cl-incf recoveries) 'group))
    (unwind-protect
        (cl-letf (((symbol-function '+noema-jupyter--signal)
                   (lambda (&rest _) (cl-incf signals)))
                  ((symbol-function '+noema-jupyter--alive-p)
                   (lambda (&rest _) nil)))
          (+noema-jupyter--shutdown-runtime runtime)
          (should (= signals 1))
          (should-not (remote-workspace-resources workspace))
          (should (zerop (hash-table-count +noema-jupyter-runtimes)))
          ;; A reconnect has nothing left to reopen for the dead kernel.
          (remote-workspace--recover-after-transport workspace)
          (should (zerop recoveries))
          ;; Its process group ID may belong to something else by now.
          (+noema-jupyter--shutdown-runtime runtime)
          (remote-workspace-close workspace)
          (should (= signals 1)))
      (remote-workspace-close workspace))))

(ert-deftest +noema-jupyter-shutdown-retains-runtime-until-target-accepts-signal ()
  "A transport failure must leave the detached kernel available for retry."
  (let* ((+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (runtime (+noema-jupyter-test--runtime))
         (attempts 0))
    (setf (+noema-jupyter-runtime-host-name runtime) "test-host")
    (puthash (+noema-jupyter-runtime-id runtime) runtime
             +noema-jupyter-runtimes)
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (_runtime) nil))
              ((symbol-function '+noema-jupyter--signal)
               (lambda (&rest _)
                 (cl-incf attempts)
                 (when (= attempts 1)
                   (error "Target is unreachable")))))
      (should-error (+noema-jupyter--shutdown-runtime runtime))
      (should (eq (+noema-jupyter-runtime-state runtime) 'shutdown-pending))
      (should (eq (gethash (+noema-jupyter-runtime-id runtime)
                           +noema-jupyter-runtimes)
                  runtime))
      (+noema-jupyter--shutdown-runtime runtime)
      (should (= attempts 2))
      (should (eq (+noema-jupyter-runtime-state runtime) 'closed))
      (should-not (gethash (+noema-jupyter-runtime-id runtime)
                           +noema-jupyter-runtimes)))))

(ert-deftest +noema-jupyter-shutdown-retains-runtime-until-exit-confirmed ()
  "TERM delivery alone must not discard a still-running detached kernel."
  (let* ((directory (make-temp-file "noema-jupyter-term-pending-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-shutdown-grace-seconds 0)
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (id "noema-0123456789abcdef01234567")
         (connection
          (remote-make-file-name
           "local" (expand-file-name (concat id ".json") directory)))
         (runtime (+noema-jupyter-runtime-create
                   :id id :context (remote-context connection)
                   :placement-file connection :connection-file connection
                   :pid 4321 :state 'open))
         (alive t)
         (signals 0))
    (unwind-protect
        (progn
          (with-temp-file connection (insert "connection"))
          (puthash id runtime +noema-jupyter-runtimes)
          (cl-letf (((symbol-function '+noema-jupyter--alive-p)
                     (lambda (_runtime) alive))
                    ((symbol-function '+noema-jupyter--signal)
                     (lambda (_runtime _signal) (cl-incf signals))))
            (should-error (+noema-jupyter--shutdown-runtime runtime)
                          :type 'error)
            (should (= signals 1))
            (should (eq (+noema-jupyter-runtime-state runtime)
                        'shutdown-pending))
            (should (eq (gethash id +noema-jupyter-runtimes) runtime))
            (should (file-exists-p connection))
            (should (file-exists-p (+noema-jupyter--pending-file id)))
            (setq alive nil)
            (+noema-jupyter--shutdown-runtime runtime)
            (should (eq (+noema-jupyter-runtime-state runtime) 'closed))
            (should-not (gethash id +noema-jupyter-runtimes))
            (should-not (file-exists-p connection))
            (should-not (file-exists-p (+noema-jupyter--pending-file id)))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-shutdown-journal-failure-keeps-runtime ()
  "A local record write failure must stop before signalling the target."
  (let* ((+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (runtime (+noema-jupyter-test--runtime))
         signalled)
    (puthash (+noema-jupyter-runtime-id runtime) runtime
             +noema-jupyter-runtimes)
    (cl-letf (((symbol-function '+noema-jupyter--record-pending-shutdown)
               (lambda (_runtime) (error "State disk unavailable")))
              ((symbol-function '+noema-jupyter--signal)
               (lambda (&rest _) (setq signalled t))))
      (should-error (+noema-jupyter--shutdown-runtime runtime))
      (should-not signalled)
      (should (eq (+noema-jupyter-runtime-state runtime)
                  'shutdown-pending))
      (should (eq (gethash (+noema-jupyter-runtime-id runtime)
                           +noema-jupyter-runtimes)
                  runtime)))))

(ert-deftest +noema-jupyter-shutdown-confirms-a-real-local-process-exit ()
  "TERM a detached local process and remove its journal only after it exits."
  (let* ((directory (make-temp-file "noema-jupyter-real-term-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (id "noema-abcdef0123456789abcdef01")
         (connection (remote-make-file-name
                      "local" (expand-file-name (concat id ".json") directory)))
         (context (remote-context connection))
         (pid (string-to-number
               (+noema-jupyter--output
                context "python3" "-c"
                "import subprocess; p=subprocess.Popen(['sleep','15'],stdin=subprocess.DEVNULL,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL,start_new_session=True); print(p.pid)")))
         (runtime (+noema-jupyter-runtime-create
                   :id id :context context :placement-file connection
                   :connection-file connection :pid pid
                   :host-name (+noema-jupyter--current-host context)
                   :state 'open)))
    (unwind-protect
        (progn
          (should (> pid 0))
          (with-temp-file connection (insert "connection"))
          (puthash id runtime +noema-jupyter-runtimes)
          (+noema-jupyter--shutdown-runtime runtime)
          (should (eq (+noema-jupyter-runtime-state runtime) 'closed))
          (should-not (+noema-jupyter--alive-p runtime))
          (should-not (file-exists-p connection))
          (should-not (file-exists-p (+noema-jupyter--pending-file id))))
      (when (eq (ignore-errors (+noema-jupyter--alive-p runtime)) t)
        (ignore-errors (+noema-jupyter--signal runtime "KILL")))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-pending-pid-check-rechecks-host ()
  "The PID command itself must verify the physical host before matching argv."
  (let* ((runtime (+noema-jupyter-test--runtime))
         (connection (remote-make-file-name
                      "local" "/tmp/noema-test-connection.json"))
         command)
    (setf (+noema-jupyter-runtime-host-name runtime) "original-host"
          (+noema-jupyter-runtime-connection-file runtime) connection)
    (cl-letf (((symbol-function '+noema-jupyter--alive-p)
               (lambda (_runtime) t))
              ((symbol-function '+noema-jupyter--output)
               (lambda (_context program &rest args)
                 (setq command (cons program args))
                 (concat "python -m ipykernel_launcher -f " connection))))
      (should (+noema-jupyter--verify-pending-runtime runtime))
      (should (equal (car command) "sh"))
      (should (member "original-host" command)))))

(ert-deftest +noema-jupyter-remote-file-cleanup-rechecks-host ()
  "The target-side cleanup refuses a changed host before removing a file."
  (let* ((file (make-temp-file "noema-jupyter-cleanup-"))
         (context (remote-context-create
                   :target-id "remote-test" :localname "/"))
         (runtime (+noema-jupyter-runtime-create
                   :id "runtime-cleanup" :context context
                   :host-name "wrong-host.invalid"
                   :connection-file file))
         command)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function '+noema-jupyter--output)
                     (lambda (_context program &rest args)
                       (setq command (cons program args))
                       ""))
                    ((symbol-function 'remote-file-local-name) #'identity))
            (+noema-jupyter--delete-runtime-files runtime))
          (should (equal (car command) "sh"))
          (let ((args (copy-sequence (cdr command))))
            (should (= 77 (apply #'call-process "/bin/sh" nil nil nil args)))
            (should (file-exists-p file))
            (setf (nth 3 args) (string-trim
                                (shell-command-to-string "hostname")))
            (should (= 0 (apply #'call-process "/bin/sh" nil nil nil args)))
            (should-not (file-exists-p file))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest +noema-jupyter-shutdown-all-releases-every-brokered-runtime ()
  "Brokered kernels are detached, so Emacs must close them before it exits."
  (let* ((+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (closed '()))
    (puthash "a" (+noema-jupyter-test--runtime 11 "a") +noema-jupyter-runtimes)
    (puthash "b" (+noema-jupyter-test--runtime 22 "b") +noema-jupyter-runtimes)
    (cl-letf (((symbol-function '+noema-jupyter--shutdown-runtime)
               (lambda (runtime)
                 (push (+noema-jupyter-runtime-pid runtime) closed)
                 runtime)))
      (+noema-jupyter-shutdown-all))
    (should (equal (sort closed #'<) '(11 22)))
    (should (zerop (hash-table-count +noema-jupyter-runtimes)))))

(ert-deftest +noema-jupyter-shutdown-all-survives-one-failing-runtime ()
  "One unreachable target must not strand the rest of the kernels."
  (let* ((+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (closed '()))
    (puthash "a" (+noema-jupyter-test--runtime 11 "a") +noema-jupyter-runtimes)
    (puthash "b" (+noema-jupyter-test--runtime 22 "b") +noema-jupyter-runtimes)
    (cl-letf (((symbol-function '+noema-jupyter--shutdown-runtime)
               (lambda (runtime)
                 (when (= (+noema-jupyter-runtime-pid runtime) 11)
                   (error "Target is unreachable"))
                 (push (+noema-jupyter-runtime-pid runtime) closed)
                 runtime)))
      (+noema-jupyter-shutdown-all))
    (should (equal closed '(22)))
    (should (= 1 (hash-table-count +noema-jupyter-runtimes)))
    (should (gethash "a" +noema-jupyter-runtimes))
    (should-not (gethash "b" +noema-jupyter-runtimes))))

(ert-deftest +noema-jupyter-pending-shutdown-survives-emacs-restart ()
  "An unreachable target remains actionable after the in-memory map is lost."
  (let* ((directory (make-temp-file "noema-jupyter-pending-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (id "noema-0123456789abcdef01234567")
         (placement (remote-make-file-name
                     "local" (expand-file-name "note.ipynb" directory)))
         (native-connection (expand-file-name (concat id ".json") directory))
         (connection (remote-make-file-name "local" native-connection))
         (runtime (+noema-jupyter-runtime-create
                   :id id :manager-id "kernel-test"
                   :context (remote-context placement)
                   :placement-file placement :pid 4321
                   :connection-file connection :state 'open))
         sent)
    (unwind-protect
        (progn
          (with-temp-file native-connection (insert "test connection"))
          (puthash id runtime +noema-jupyter-runtimes)
          (cl-letf (((symbol-function '+noema-jupyter--signal)
                     (lambda (&rest _) (error "Target unavailable"))))
            (should-error (+noema-jupyter--shutdown-runtime runtime)))
          (should (eq (+noema-jupyter-runtime-state runtime)
                      'shutdown-pending))
          (should (file-exists-p (+noema-jupyter--pending-file id)))
          (should (= (logand #o777 (file-modes
                                    (+noema-jupyter--pending-file id))) #o600))
          (should (= (logand #o777 (file-modes
                                    +noema-jupyter-pending-directory)) #o700))
          ;; Simulate a fresh Emacs session: only the local record survives.
          (clrhash +noema-jupyter-runtimes)
          (let* ((snapshot (+noema-jupyter-runtime-snapshot "local"))
                 (entry (+noema-jupyter--pending-shutdown-for id)))
            (should (= (length snapshot) 1))
            (should (eq (plist-get (car snapshot) :status)
                        'shutdown-pending))
            (should (plist-get (car snapshot) :journal-pending))
            (cl-letf (((symbol-function '+noema-jupyter--alive-p)
                       (lambda (_runtime) (if sent nil t)))
                      ((symbol-function '+noema-jupyter--output)
                       (lambda (&rest _)
                         (concat "python -m ipykernel_launcher -f "
                                 native-connection)))
                      ((symbol-function '+noema-jupyter--signal)
                       (lambda (_runtime signal) (setq sent signal))))
              (+noema-jupyter--retry-pending-shutdown entry)))
          (should (equal sent "TERM"))
          (should-not (file-exists-p native-connection))
          (should-not (file-exists-p (+noema-jupyter--pending-file id))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-pending-shutdown-rejects-reused-pid ()
  "A recycled PID may never be signalled from a saved shutdown record."
  (let* ((directory (make-temp-file "noema-jupyter-reused-pid-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (id "noema-abcdef0123456789abcdef01")
         (placement (remote-make-file-name
                     "local" (expand-file-name "note.ipynb" directory)))
         (connection (remote-make-file-name
                      "local" (expand-file-name (concat id ".json") directory)))
         (runtime (+noema-jupyter-runtime-create
                   :id id :context (remote-context placement)
                   :placement-file placement :pid 4321
                   :connection-file connection :state 'shutdown-pending))
         signalled)
    (unwind-protect
        (progn
          (+noema-jupyter--record-pending-shutdown runtime)
          (cl-letf (((symbol-function '+noema-jupyter--alive-p)
                     (lambda (_runtime) t))
                    ((symbol-function '+noema-jupyter--output)
                     (lambda (&rest _) "python another-program.py"))
                    ((symbol-function '+noema-jupyter--signal)
                     (lambda (&rest _) (setq signalled t))))
            (should-error
             (+noema-jupyter--retry-pending-shutdown
              (+noema-jupyter--pending-shutdown-for id))))
          (should-not signalled)
          (should (file-exists-p (+noema-jupyter--pending-file id))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-pending-shutdown-keeps-record-on-host-switch ()
  "A saved broker must not be cleared by probing another physical node."
  (let* ((directory (make-temp-file "noema-jupyter-moved-host-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (id "noema-1234567890abcdef12345678")
         (placement (remote-make-file-name
                     "local" (expand-file-name "note.ipynb" directory)))
         (connection (remote-make-file-name
                      "local" (expand-file-name (concat id ".json")
                                                     directory)))
         (runtime (+noema-jupyter-runtime-create
                   :id id :context (remote-context placement)
                   :placement-file placement :pid 4321
                   :host-name "original-host"
                   :connection-file connection :state 'shutdown-pending))
         signalled)
    (unwind-protect
        (progn
          (+noema-jupyter--record-pending-shutdown runtime)
          (cl-letf (((symbol-function '+noema-jupyter--exit-status)
                     (lambda (&rest _) 77))
                    ((symbol-function '+noema-jupyter--output)
                     (lambda (&rest _) (setq signalled t) "")))
            (should-error
             (+noema-jupyter--retry-pending-shutdown
              (+noema-jupyter--pending-shutdown-for id))))
          (should-not signalled)
          (should (file-exists-p (+noema-jupyter--pending-file id))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-in-memory-shutdown-retry-checks-pid ()
  "The current Emacs session must not signal a reused pending kernel PID."
  (let* ((directory (make-temp-file "noema-jupyter-live-retry-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (id "noema-abcdefabcdefabcdefabcdef")
         (placement (remote-make-file-name
                     "local" (expand-file-name "note.ipynb" directory)))
         (connection (remote-make-file-name
                      "local" (expand-file-name (concat id ".json") directory)))
         (runtime (+noema-jupyter-runtime-create
                   :id id :context (remote-context placement)
                   :placement-file placement :pid 4321
                   :connection-file connection :state 'shutdown-pending))
         signalled)
    (unwind-protect
        (progn
          (puthash id runtime +noema-jupyter-runtimes)
          (cl-letf (((symbol-function '+noema-jupyter--alive-p)
                     (lambda (_runtime) t))
                    ((symbol-function '+noema-jupyter--output)
                     (lambda (&rest _) "python another-program.py"))
                    ((symbol-function '+noema-jupyter--signal)
                     (lambda (&rest _) (setq signalled t))))
            (should-error (+noema-jupyter--shutdown-runtime runtime)))
          (should-not signalled)
          (should (eq (+noema-jupyter-runtime-state runtime)
                      'shutdown-pending))
          (should (eq (gethash id +noema-jupyter-runtimes) runtime))
          (should (file-exists-p (+noema-jupyter--pending-file id))))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-runtime-control-routes-saved-shutdown ()
  "The Board's shutdown action can use a record after the runtime map is lost."
  (let* ((directory (make-temp-file "noema-jupyter-control-" t))
         (+noema-jupyter-pending-directory
          (expand-file-name "pending/" directory))
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (id "noema-123456789abcdef012345678")
         (placement (remote-make-file-name
                     "local" (expand-file-name "note.ipynb" directory)))
         (connection (remote-make-file-name
                      "local" (expand-file-name (concat id ".json") directory)))
         (runtime (+noema-jupyter-runtime-create
                   :id id :context (remote-context placement)
                   :placement-file placement :pid 4321
                   :connection-file connection :state 'shutdown-pending))
         result failure retried)
    (unwind-protect
        (progn
          (+noema-jupyter--record-pending-shutdown runtime)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function '+noema-jupyter--retry-pending-shutdown)
                     (lambda (entry)
                       (setq retried (+noema-jupyter--get 'runtimeId entry))
                       runtime)))
            (+noema-jupyter-runtime-control
             id 'shutdown
             (lambda (value error)
               (setq result value failure error))))
          (should (equal retried id))
          (should (equal result '(:ok t)))
          (should-not failure))
      (delete-directory directory t))))

(ert-deftest +noema-jupyter-invalid-record-does-not-pop-warning-on-board-read ()
  "A damaged local record is reported in the snapshot, without a pop-up."
  (let* ((directory (make-temp-file "noema-jupyter-bad-record-" t))
         (+noema-jupyter-pending-directory directory)
         (+noema-jupyter--pending-record-errors nil)
         (+noema-jupyter-runtimes (make-hash-table :test #'equal))
         (file (expand-file-name (concat (make-string 64 ?a) ".json")
                                 directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{oops"))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest _) (error "Unexpected warning window"))))
            (should-not (+noema-jupyter-runtime-snapshot "local")))
          (should (= (length +noema-jupyter--pending-record-errors) 1))
          (should (string-match-p
                   (regexp-quote file)
                   (car +noema-jupyter--pending-record-errors))))
      (delete-directory directory t))))

(ert-deftest +noema-input-focus-overrides-a-stale-background-belief ()
  "A renderer that proves it owns input must not stay paused under the cursor.
On the macOS xwidget port a click inside WebKit can leave Emacs' window
selection untouched, so the pane keeps being reported as background."
  (let* ((pane (generate-new-buffer " *Noema focused pane*"))
         (+noema--app-buffer nil)
         (+noema--client-buffers (make-hash-table :test #'equal))
         (+noema--file-buffers (make-hash-table :test #'equal))
         (+noema--activity-hooks-installed t)
         (+noema--last-activity-signature 'stale)
         (+noema--input-focused-window nil)
         (+noema--input-focus-host-window nil)
         selected
         updated)
    (unwind-protect
        (progn
          (with-current-buffer pane
            (setq-local +noema--client-id "client-a")
            (setq-local +noema--activity-paused t))
          (puthash "client-a" pane +noema--client-buffers)
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (&rest _) 'pane-window))
                    ((symbol-function 'select-window)
                     (lambda (window &rest _) (setq selected window)))
                    ((symbol-function 'noema--select-emacs-window)
                     (lambda (&rest _)
                       (ert-fail "Input focus must not steal WebKit's first responder")))
                    ((symbol-function '+noema--update-activity)
                     (lambda (&rest _) (setq updated t))))
            (+noema--handle-input-focus '((client . "client-a")
                                            (file . "/notes/a.md"))))
          (should (eq +noema--app-buffer pane))
          (should-not selected)
          (should (eq +noema--input-focused-window 'pane-window))
          (should updated)
          (should (eq +noema--last-activity-signature :unknown))
          ;; The renderer already dropped its own host pause. Leaving the
          ;; local belief at t would deduplicate away both the explicit resume
          ;; and the next genuine background transition, so the Node host keeps
          ;; replaying a stale pause and a hidden page stays fully awake.
          (should (eq (buffer-local-value '+noema--activity-paused pane)
                      :unknown)))
      (+noema--forget-input-focus)
      (when (buffer-live-p pane) (kill-buffer pane)))))

(ert-deftest +noema-input-focus-ignores-an-unknown-client ()
  (let ((+noema--app-buffer nil)
        (+noema--client-buffers (make-hash-table :test #'equal))
        (+noema--file-buffers (make-hash-table :test #'equal))
        (+noema--activity-hooks-installed t)
        updated)
    (cl-letf (((symbol-function '+noema--update-activity)
               (lambda (&rest _) (setq updated t))))
      (+noema--handle-input-focus '((client . "client-gone"))))
    (should-not updated)
    (should-not +noema--app-buffer)))

(ert-deftest +noema-queued-command-keeps-the-issuing-pane-address ()
  "The async payload captures its pane before any later timer can run."
  (let* ((pane (generate-new-buffer " *Noema issuing pane*"))
         posted)
    (unwind-protect
        (progn
          (with-current-buffer pane
            (setq-local +noema--client-id "client-a"))
          (cl-letf (((symbol-function '+noema--ensure-server)
                     (lambda (&rest _)
                       (ert-fail "command enqueue must not start/wait for host")))
                    ((symbol-function '+noema--post)
                     (lambda (payload) (push payload posted))))
            (with-current-buffer pane
              (+noema/command "refresh"))
            (should (equal (alist-get 'client (car posted)) "client-a"))
            (should (equal (alist-get 'command (car posted)) "refresh"))))
      (when (buffer-live-p pane) (kill-buffer pane)))))

(ert-deftest +noema-agenda-resolves-host-placement-through-remote ()
  (should (equal (+noema--agenda-host-project-root "/fs:local:/tmp/agenda/") "/tmp/agenda/"))
  (let ((default-directory "/fs:box:/work/"))
    (should (equal (+noema--agenda-host-project-root "/Users/test/agenda/")
                   "/Users/test/agenda/")))
  (cl-letf (((symbol-function 'remote-client-file-name) (lambda (_root) "/shared/agenda/")))
    (should (equal (+noema--agenda-host-project-root "/fs:shared:/agenda/") "/shared/agenda/")))
  (cl-letf (((symbol-function 'remote-client-file-name) (lambda (_root) nil)))
    (should (equal (+noema--agenda-host-project-root "/fs:box:/agenda/") "/fs:box:/agenda/"))))

(ert-deftest +noema-agenda-protection-checks-buffer-identities-without-source-io ()
  (with-temp-buffer
    (setq buffer-file-name "/var/example/work.noema"
          buffer-file-truename "/private/var/example/work.noema")
    (insert "unsaved prompt")
    (cl-letf (((symbol-function 'file-truename) (lambda (&rest _) (ert-fail "source IO")))
              ((symbol-function 'file-exists-p) (lambda (&rest _) (ert-fail "source IO"))))
      (should (equal (alist-get 'files (+noema--agenda-protected-sources
                                       '((files . ["/private/var/example/work.noema" "/inactive/other.noema"])) nil))
                     ["/private/var/example/work.noema"])))))

(ert-deftest +noema-jupyter-signing-key-uses-csprng-when-device-read-is-rejected ()
  (cl-letf (((symbol-function 'insert-file-contents-literally)
             (lambda (&rest _) (signal 'file-error '("not a regular file"))))
            ((symbol-function 'random)
             (lambda (&rest _) (ert-fail "Kernel key must not use Emacs random"))))
    (let ((first (+noema-jupyter--signing-key))
          (second (+noema-jupyter--signing-key)))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" first))
      (should-not (equal first second)))))

(ert-deftest +noema-pane-remaps-text-commands-onto-the-page ()
  "Forwarded Emacs text commands act on the note, not the inert placeholder."
  (with-temp-buffer
    (+noema-keys-mode 1)
    ;; H-i keeps the ordinary Treemacs outline, which follows the pane's note.
    (should-not (command-remapping 'show-imenu))
    (dolist (entry '((+search/line-forward . noema-find)
                     (clipboard-kill-ring-save . noema-pane-copy)
                     (save-buffer . +noema/save)
                     (revert-buffer . noema-refresh-file)))
      (should (eq (command-remapping (car entry)) (cdr entry))))))

(ert-deftest +noema-pane-copy-accepts-a-keyboard-event ()
  "Cmd-C is a plain key event; the routed copy must not demand a mouse event."
  (let (sent)
    (cl-letf (((symbol-function 'noema--xwidget-buffer-p) (lambda (&optional _) t))
              ((symbol-function '+noema/command)
               (lambda (command &optional _detail) (push command sent))))
      (let ((last-command-event ?\M-c))
        (call-interactively #'noema-xwidget-copy nil [?\M-c])
        (call-interactively #'noema-pane-copy nil [?\M-c])))
    (should (equal sent '("copy" "copy")))))

(ert-deftest +noema-refresh-file-discards-only-when-asked ()
  "refresh-file never overwrites disk; C-u asks the page to drop its draft."
  (let (sent)
    (cl-letf (((symbol-function '+noema/command)
               (lambda (command &optional detail) (push (list command detail) sent))))
      (noema-refresh-file)
      (noema-refresh-file '(4)))
    (should (equal (reverse sent)
                   '(("refresh-file" nil)
                     ("refresh-file" ((value . "discard"))))))))

(ert-deftest +noema-pane-headings-skip-fenced-code ()
  "The heading jump lists real headings with their lines, not code comments."
  (let ((file (make-temp-file "noema-headings" nil ".md"
                              "# Top\n\n```sh\n# not a heading\n```\n## Child  ##\n")))
    (unwind-protect
        (should (equal (noema--pane-headings file)
                       '(("Top" . 1) ("  Child" . 6))))
      (delete-file file))))


(ert-deftest +noema-treemacs-heading-jump-moves-the-pane ()
  "A Treemacs heading click for a note in a Noema pane moves that pane."
  (let* ((file (make-temp-file "noema-outline" nil ".md" "# A\n\n## B\n"))
         (raw (let ((+noema--inhibit-redirect t)) (find-file-noselect file)))
         (pane (generate-new-buffer " *noema pane*"))
         sent)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer raw)
          (goto-char (point-min))
          (forward-line 2)
          (cl-letf (((symbol-function '+noema--buffer-for-file) (lambda (_) pane))
                    ((symbol-function 'noema--focus-xwidget-window) #'ignore)
                    ((symbol-function '+noema/command)
                     (lambda (command &optional detail) (push (list command detail) sent))))
            (+noema-treemacs-tag-visit-h raw))
          (should (equal sent '(("goto-line" ((value . 3))))))
          (should (eq (window-buffer (selected-window)) pane))
          (should-not (buffer-live-p raw)))
      (when (buffer-live-p raw) (kill-buffer raw))
      (kill-buffer pane)
      (delete-file file))))

(ert-deftest +noema-refresh-file-command-reloads-a-pane ()
  "The generic `refresh-file' reloads a Noema pane from disk."
  (require 'basic-toolkit)
  (let (called)
    (with-temp-buffer
      (+noema-keys-mode 1)
      (cl-letf (((symbol-function 'noema-refresh-file)
                 (lambda (&optional _) (interactive "P") (setq called t))))
        (refresh-file)))
    (should called)))

(ert-deftest +noema-bridge-visit-is-not-redirected-to-noema ()
  "A bridged send reads the note without reopening Noema."
  (require 'noema-context)
  (let* ((file (make-temp-file "noema-bridge" nil ".md" "# A\n\nbody\n"))
         (pane (generate-new-buffer " *noema pane*"))
         redirected references source-mode)
    (unwind-protect
        (with-current-buffer pane
          (setq-local +noema-buffer-file-name file)
          (cl-letf (((symbol-function '+noema/open-file) (lambda (f) (setq redirected f)))
                    ((symbol-function 'noema-context--send)
                     (lambda (&rest arguments)
                       (setq source-mode (with-current-buffer (find-buffer-visiting file)
                                           (derived-mode-p 'text-mode)))
                       (setq references (funcall (plist-get arguments :references)
                                                 (file-name-directory file) nil)))))
            (pcase-let ((`(,source ,begin . ,end)
                         (noema-md-bridge-region
                          file (list :line-start 1 :line-end 3))))
              (noema-md-bridge-run "agent" source begin end)))
          (should-not redirected)
          (should (equal (mapcar #'noema-context--reference-line references)
                         (list (file-name-nondirectory file))))
          ;; It was read as real Markdown, then dropped so the next open of
          ;; the note goes to Noema again.
          (should source-mode)
          (should-not (find-buffer-visiting file)))
      (when-let* ((source (find-buffer-visiting file))) (kill-buffer source))
      (kill-buffer pane)
      (delete-file file))))

(ert-deftest +noema-portal-task-wiki-and-send-use-the-current-client ()
  "Portal pages use the same task, Wiki and saved-selection command routes."
  (require 'noema-context)
  (let ((file (make-temp-file "noema-portal-commands" nil ".md" "# A\nbody\n"))
        sent opened)
    (unwind-protect
        (with-temp-buffer
          (setq-local major-mode 'portal-web-mode)
          (setq-local +noema--client-id "portal-note")
          (setq-local +noema-buffer-file-name file)
          (should (noema-md-bridge-pane-p))
          (cl-letf (((symbol-function '+noema--post)
                     (lambda (payload) (push payload sent)))
                    ((symbol-function '+noema--open-hosted-surface)
                     (lambda (payload) (setq opened payload))))
            (+noema/task-manager)
            (+noema/wiki-home)
            (noema-context-send-buffer))
          (should (equal (alist-get 'path opened) "/wiki"))
          (should (equal (alist-get 'client opened) "portal-note"))
          (should (equal (alist-get 'command (cadr sent)) "task-manager"))
          (should (equal (alist-get 'client (cadr sent)) "portal-note"))
          (should (equal (alist-get 'command (car sent)) "emacs-selection"))
          (should (equal (alist-get 'client (car sent)) "portal-note"))
          (let ((detail (alist-get 'detail (car sent))))
            (should (equal (alist-get 'action detail) "agent"))
            (should (equal (alist-get 'scope detail) "document"))
            (noema-md-bridge--settle (alist-get 'requestId detail))))
      (delete-file file))))

(ert-deftest +noema-portal-pages-return-from-buffer-list-and-reload-after-source-save ()
  (require 'noema-md-bridge)
  (let* ((file (make-temp-file "noema-portal-reload" nil ".md" "# A\n"))
         (pane (generate-new-buffer " *Noema Portal undisp*"))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer pane
            (setq-local major-mode 'portal-web-mode)
            (setq-local +noema--client-id "portal-reload")
            (setq-local +noema-buffer-file-name file))
          (should (memq pane (+noema--undisplayed-buffers)))
          (should (memq pane (noema-md-bridge--panes-for file)))
          (cl-letf (((symbol-function '+noema/command)
                     (lambda (command &optional detail)
                       (push (list command detail) sent))))
            (noema-md-bridge-notify-saved file))
          (should (equal sent `(("reload-if-clean" ((file . ,file)))))))
      (kill-buffer pane)
      (delete-file file))))

(ert-deftest +noema-legacy-task-manager-command-uses-page-route ()
  (require 'app/browser/config)
  (let (sent)
    (cl-letf (((symbol-function '+noema/task-manager)
               (lambda () (interactive) (setq sent t)))
              ((symbol-function '+xwidget--execute-script)
               (lambda (&rest _) (ert-fail "Task manager used xwidget script"))))
      (+xwidget/open-task-manager))
    (should sent)))

(ert-deftest +noema-bridge-prompts-get-the-keyboard ()
  "A bridged action releases the page's input and focuses its prompts."
  (require 'noema-md-bridge)
  (let* ((pane (generate-new-buffer " *noema pane*"))
         (source (generate-new-buffer " *noema source*"))
         released hooked)
    (unwind-protect
        (cl-letf (((symbol-function 'noema--release-xwidget-input-buffer)
                   (lambda (&optional buffer) (setq released buffer)))
                  ((symbol-function 'noema--select-emacs-window) #'ignore)
                  ((symbol-function 'noema-md-bridge-run)
                   (lambda (&rest _)
                     (setq hooked (memq #'noema--focus-minibuffer-if-active
                                        minibuffer-setup-hook))
                     (signal 'quit nil))))
          (noema-md-bridge--run-interactively pane "agent" source 1 1)
          (should (eq released pane))
          (should hooked))
      (kill-buffer pane)
      (kill-buffer source))))

(ert-deftest +noema-related-notes-uses-existing-agent-context ()
  "Related-note discovery sends a file reference through the Emacs agent path."
  (require 'noema-context)
  (with-temp-buffer
    (let (prompt references)
      (cl-letf (((symbol-function 'noema-context--region-references)
                 (lambda (_buffer begin end)
                   (should (= begin 1))
                   (should (= end 4))
                   (lambda (_root _session) '((file . "note.md")))))
                ((symbol-function 'noema-context--send)
                 (lambda (&rest args)
                   (setq prompt (plist-get args :prompt)
                         references (funcall (plist-get args :references) nil nil))
                   nil))
                ((symbol-function 'noema-md-bridge--drop-hidden) #'ignore))
        (noema-md-bridge-run "related" (current-buffer) 1 4))
      (should (string-match-p "semantic search" prompt))
      (should (equal references '((file . "note.md")))))))

(defmacro +noema-test--with-pane-window (&rest body)
  "Run BODY with `pane' shown in the only, selected window."
  (declare (indent 0))
  `(let ((pane (generate-new-buffer " noema-pane"))
         (other (generate-new-buffer "noema-result")))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer pane)
           (cl-letf (((symbol-function 'noema--xwidget-buffer-p)
                      (lambda (&optional buffer) (eq (or buffer (current-buffer)) pane)))
                     ((symbol-function 'noema--release-xwidget-input-buffer) #'ignore)
                     ((symbol-function 'noema--focus-xwidget-window) #'ignore))
             ,@body))
       (noema--forget-forwarded-command)
       (noema--stop-awaiting-forwarded-result)
       (kill-buffer pane)
       (kill-buffer other))))

(ert-deftest +noema-forwarded-command-focuses-what-it-opened ()
  "A command run from a Noema page leaves the keyboard in the window it opened."
  (+noema-test--with-pane-window
    (noema--follow-forwarded-command (selected-window))
    ;; The command shows its result beside the page without selecting it.
    (display-buffer other '(display-buffer-pop-up-window))
    (noema--after-forwarded-command)
    (should (eq (window-buffer (selected-window)) other))
    (should-not (memq #'noema--after-forwarded-command (default-value 'post-command-hook)))))

(ert-deftest +noema-forwarded-command-without-result-keeps-the-page ()
  "A command that opens nothing leaves selection where it was."
  (+noema-test--with-pane-window
    (let ((source (selected-window)))
      (noema--follow-forwarded-command source)
      (noema--after-forwarded-command)
      (should (eq (selected-window) source)))))

(ert-deftest +noema-forwarded-command-waits-for-a-prefix-argument ()
  "C-u before the command is not the command's result."
  (+noema-test--with-pane-window
    (noema--follow-forwarded-command (selected-window))
    (let ((prefix-arg '(4)))
      (noema--after-forwarded-command))
    (should (memq #'noema--after-forwarded-command (default-value 'post-command-hook)))
    (display-buffer other '(display-buffer-pop-up-window))
    (noema--after-forwarded-command)
    (should (eq (window-buffer (selected-window)) other))))

(ert-deftest +noema-host-owned-key-runs-where-emacs-is ()
  "An arrow WebKit got after Emacs took the keyboard stays in Emacs' window."
  (+noema-test--with-pane-window
    (let ((unread-command-events nil)
          (released nil))
      (display-buffer other '(display-buffer-pop-up-window))
      (select-window (get-buffer-window other))
      (cl-letf (((symbol-function 'noema--release-xwidget-input-buffer)
                 (lambda (&rest _) (setq released t))))
        (noema--run-emacs-key "<down>" "pane-client" t))
      (should (eq (window-buffer (selected-window)) other))
      (should-not released)
      (should (equal unread-command-events (listify-key-sequence (kbd "<down>")))))))

(ert-deftest +noema-releasing-a-pane-tells-its-page ()
  "Taking the keyboard from a pane also tells the page it no longer has it."
  (let ((pane (generate-new-buffer " noema-pane"))
        sent)
    (unwind-protect
        (with-current-buffer pane
          (setq major-mode 'xwidget-webkit-mode)
          (setq-local +noema--client-id "pane-client")
          (cl-letf (((symbol-function 'xwidget-webkit-edit-mode) #'ignore)
                    ((symbol-function '+noema/command)
                     (lambda (command &optional _) (push command sent))))
            (noema--release-xwidget-input-buffer pane))
          (should (equal sent '("host-owns-keyboard"))))
      (kill-buffer pane))))

(ert-deftest +noema-forwarded-command-without-result-returns-the-keyboard-to-the-page ()
  "M-q answered with n, or M-x cancelled: the page gets its keyboard back."
  (+noema-test--with-pane-window
    (let ((source (selected-window)) focused)
      (cl-letf (((symbol-function 'noema--focus-xwidget-window)
                 (lambda (window) (setq focused window))))
        (noema--follow-forwarded-command source)
        (noema--after-forwarded-command))
      (should (eq focused source))
      (should (memq #'noema--forwarded-result-appeared
                    (default-value 'window-buffer-change-functions))))))

(ert-deftest +noema-forwarded-command-follows-a-window-shown-late ()
  "A process that shows its buffer after the command returns still gets focus."
  (+noema-test--with-pane-window
    (noema--follow-forwarded-command (selected-window))
    (noema--after-forwarded-command)
    ;; The agent/terminal buffer appears from a process callback.
    (display-buffer other '(display-buffer-pop-up-window))
    (noema--forwarded-result-appeared)
    (should (eq (window-buffer (selected-window)) other))
    (should-not noema--forwarded-await)))

(ert-deftest +noema-late-window-never-pulls-back-someone-who-moved ()
  "Once the person selected another window, a late result does not steal it."
  (+noema-test--with-pane-window
    (let ((elsewhere (split-window)))
      (noema--follow-forwarded-command (selected-window))
      (noema--after-forwarded-command)
      (select-window elsewhere)
      (set-window-buffer elsewhere other)
      (noema--forwarded-result-appeared)
      (should (eq (selected-window) elsewhere))
      (should-not noema--forwarded-await))))

(ert-deftest +noema-recovery-keys-leave-meta-arrows-to-windmove ()
  "Cmd+Arrow moves between windows from a Noema pane as everywhere else."
  (dolist (key '("M-<left>" "M-<right>" "M-<up>" "M-<down>"))
    (should-not (lookup-key noema-xwidget-recovery-mode-map (kbd key))))
  (should (eq (lookup-key noema-xwidget-recovery-mode-map (kbd "<left>"))
              #'noema-xwidget-recover-key)))

(ert-deftest +noema-releasing-a-pane-without-client-still-tells-pages ()
  "A pane Emacs knows no client for gets the release as a broadcast."
  (let ((pane (generate-new-buffer " noema-pane"))
        sent)
    (unwind-protect
        (with-current-buffer pane
          (setq major-mode 'xwidget-webkit-mode)
          (cl-letf (((symbol-function 'xwidget-webkit-edit-mode) #'ignore)
                    ((symbol-function '+noema/command)
                     (lambda (command &optional _) (push command sent))))
            (noema--release-xwidget-input-buffer pane))
          (should (equal sent '("host-owns-keyboard"))))
      (kill-buffer pane))))

(ert-deftest +noema-surface-urls-carry-their-client ()
  "Non-file surfaces are addressed by the same client id Emacs registers."
  (should (equal (+noema--url-with-client "http://127.0.0.1:1/wiki" "noema")
                 "http://127.0.0.1:1/wiki?client=noema"))
  (should (equal (+noema--url-with-client "http://127.0.0.1:1/agenda?view=today" "noema")
                 "http://127.0.0.1:1/agenda?view=today&client=noema"))
  (should (equal (+noema--url-with-client "http://h/?client=x" "noema")
                 "http://h/?client=noema"))
  (should (equal (+noema--url-with-client "http://h/wiki#section" "noema")
                 "http://h/wiki?client=noema#section")))

(provide 'init-noema-tests)
;;; init-noema-tests.el ends here

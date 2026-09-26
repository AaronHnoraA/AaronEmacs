;;; init-lsp-remote-tests.el --- Logical LSP URI and remote-parity tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'init-lsp)
(require 'init-snippets)
(require 'init-python)
(require 'init-js2)
(require 'init-java)
(require 'lsp-mode)

(defun my/lsp-test-registration (method)
  "Return an lsp-mode registration object for METHOD."
  (if lsp-use-plists
      (list :method method)
    (let ((registration (make-hash-table :test #'equal)))
      (puthash "method" method registration)
      registration)))

(defun my/lsp-test-object (&rest pairs)
  "Return an LSP hash object initialized from keyword/value PAIRS."
  (let ((object (make-hash-table :test #'equal)))
    (while pairs
      (puthash (substring (symbol-name (pop pairs)) 1) (pop pairs) object))
  object))

(ert-deftest lsp-remote-company-timeout-falls-back-and-retries ()
  "A stalled remote CAPF does not block every subsequent completion."
  (with-temp-buffer
    (setq-local lsp-managed-mode t
                my/lsp-remote-change--eligible t)
    (let ((my/lsp-remote-completion-timeout 1.5)
          (my/lsp-remote-completion-retry-delay 3)
          (lsp-response-timeout 10)
          seen (calls 0))
      (should
       (null
        (my/lsp-remote-completion--candidates-a
         (lambda (&rest _)
           (setq seen lsp-response-timeout)
           (cl-incf calls)
           (error "Timeout while waiting for response.  Method: textDocument/completion")))))
      (should (= seen 1.5))
      (should (= calls 1))
      (should (numberp my/lsp-remote-completion--retry-at))
      (should (null (my/lsp-remote-completion--candidates-a
                     (lambda (&rest _) (cl-incf calls)))))
      (should (= calls 1))
      (setq my/lsp-remote-completion--retry-at (- (float-time) 1))
      (should (equal
               (my/lsp-remote-completion--candidates-a
                (lambda (&rest _) (cl-incf calls) '("print")))
               '("print")))
      (should-not my/lsp-remote-completion--retry-at)
      (setq-local my/lsp-remote-change--eligible nil)
      (should (equal
               (my/lsp-remote-completion--candidates-a
                (lambda (&rest _)
                  (setq seen lsp-response-timeout)
                  '("local")))
               '("local")))
      (should (= seen 10)))))

(ert-deftest lsp-remote-company-hard-deadline-survives-error-text-changes ()
  "A slow CAPF is bounded even if a newer LSP release changes its error."
  (with-temp-buffer
    (setq-local lsp-managed-mode t
                my/lsp-remote-change--eligible t)
    (let ((my/lsp-remote-completion-timeout 0.15)
          (lsp-response-timeout 10)
          (started (float-time)))
      (should-not
       (my/lsp-remote-completion--candidates-a
        (lambda (&rest _)
          (sit-for 1)
          '("late"))))
      (should (< (- (float-time) started) 0.8))
      (should (numberp my/lsp-remote-completion--retry-at)))))

(ert-deftest lsp-remote-company-recovery-retries-only-unchanged-input ()
  "A restored server may retry once only for the original visible edit."
  (let ((buffer (generate-new-buffer " *remote-company-retry*"))
        (calls 0))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "prin")
            (setq-local lsp-managed-mode t
                        company-mode t)
            (let ((context (list (selected-window)
                                 (buffer-chars-modified-tick) (point)
                                 num-nonmacro-input-events)))
              (cl-letf (((symbol-function 'company-idle-begin)
                         (lambda (&rest _) (cl-incf calls))))
                (my/lsp-remote-completion--retry-if-unchanged buffer context)
                (should (= calls 1))
                (let ((num-nonmacro-input-events
                       (1+ (nth 3 context))))
                  (my/lsp-remote-completion--retry-if-unchanged
                   buffer context))
                (should (= calls 1))
                (goto-char (point-min))
                (my/lsp-remote-completion--retry-if-unchanged buffer context)
                (should (= calls 1))
                (goto-char (point-max))
                (insert "t")
                (my/lsp-remote-completion--retry-if-unchanged buffer context)
                (should (= calls 1))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lsp-remote-completion-health-probe-restores-and-limits-restart ()
  "A stalled workspace gets one async recovery probe and bounded restart."
  (let* ((workspace (list 'remote-completion-workspace))
         (process (make-pipe-process
                   :name "lsp-completion-health-test" :noquery t))
         (buffer (generate-new-buffer " *lsp-completion-health-test*"))
         (my/lsp-remote-completion--restart-history
          (make-hash-table :test #'equal))
         callback token (restarts 0) canceled restart-source restart-reason)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local lsp-managed-mode t
                        my/lsp-remote-change--eligible t))
          (cl-letf (((symbol-function 'lsp-workspaces)
                     (lambda () (list workspace)))
                    ((symbol-function 'lsp--workspace-cmd-proc)
                     (lambda (_workspace) process))
                    ((symbol-function 'lsp--workspace-status)
                     (lambda (_workspace) 'initialized))
                    ((symbol-function 'lsp--text-document-position-params)
                     (lambda (&rest _) '(:textDocument (:uri "file:///a.py"))))
                    ((symbol-function 'lsp-request-async)
                     (lambda (_method _params fn &rest _keys)
                       (setq callback fn)))
                    ((symbol-function 'lsp-cancel-request-by-token)
                     (lambda (value) (push value canceled)))
                    ((symbol-function 'my/lsp-mode--workspace-key)
                     (lambda (_workspace)
                       '(my-python "/fs:test:/project/")))
                    ((symbol-function
                      'my/language-server--restart-workspace-from-source)
                     (lambda (_workspace source reason)
                       (setq restart-source source
                             restart-reason reason)
                       (cl-incf restarts))))
            (with-current-buffer buffer
              (my/lsp-remote-completion--record-timeout)
              (setq token (plist-get my/lsp-remote-completion--probe :token))
              (cancel-timer
               (plist-get my/lsp-remote-completion--probe :start-timer))
              (my/lsp-remote-completion--probe-start
               buffer workspace process token)
              (should callback)
              (should my/lsp-remote-completion--probe)
              (should-not
               (my/lsp-remote-completion--candidates-a
                (lambda (&rest _) (error "Blocking request escaped probe"))))
              (funcall callback nil)
              (should-not my/lsp-remote-completion--probe)
              (should-not my/lsp-remote-completion--retry-at)
              (setq-local my/lsp-remote-completion--probe
                          (list :workspace workspace :process process
                                :token (setq token (make-symbol "expired"))))
              (my/lsp-remote-completion--probe-finish
               buffer workspace process token 'expired)
              (should (= restarts 1))
              (should (eq restart-source buffer))
              (should (eq restart-reason 'completion-health-timeout))
              (should (= (length canceled) 1))
              (setq-local my/lsp-remote-completion--probe
                          (list :workspace workspace :process process
                                :token (setq token (make-symbol "again"))))
              (my/lsp-remote-completion--probe-finish
               buffer workspace process token 'expired)
              (should (= restarts 1))
              (my/lsp-remote-completion--managed-h)
              (should-not my/lsp-remote-completion--retry-at))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-process process))))

(ert-deftest lsp-remote-completion-health-defers-to-transport-recovery ()
  "A disconnected Remote owner retains control of its LSP generation."
  (let* ((workspace (list 'lsp-health-workspace))
         (owner (list 'remote-owner))
         (resource (list 'lsp-resource))
         (process (make-pipe-process
                   :name "lsp-completion-owner-test" :noquery t))
         (remote-workspaces (make-hash-table :test #'equal))
         (my/lsp-remote-completion--restart-history
          (make-hash-table :test #'equal))
         restarted)
    (unwind-protect
        (progn
          (puthash 'owner owner remote-workspaces)
          (cl-letf (((symbol-function 'lsp--workspace-cmd-proc)
                     (lambda (_workspace) process))
                    ((symbol-function 'lsp--workspace-status)
                     (lambda (_workspace) 'initialized))
                    ((symbol-function 'my/lsp-mode--workspace-key)
                     (lambda (_workspace)
                       '(my-python "/fs:test:/project/")))
                    ((symbol-function 'remote-workspace-resources)
                     (lambda (_owner) (list resource)))
                    ((symbol-function 'remote-workspace-resource-kind)
                     (lambda (_resource) 'lsp))
                    ((symbol-function 'remote-workspace-resource-value)
                     (lambda (_resource) workspace))
                    ((symbol-function 'remote-workspace-state)
                     (lambda (_owner) 'reconnecting))
                    ((symbol-function 'my/language-server--recover-lsp-resource)
                     (lambda (_resource) (setq restarted t)))
                    ((symbol-function
                      'my/language-server--restart-workspace-from-source)
                     (lambda (&rest _) (setq restarted t))))
            (my/lsp-remote-completion--maybe-restart workspace process)
            (should-not restarted)))
      (delete-process process))))

(ert-deftest lsp-remote-incremental-changes-flush-in-order-before-requests ()
  "Remote didChange batches keep ranges ordered and precede the next request."
  (let* ((workspace (list 'change-batch-workspace))
         (process (make-pipe-process :name "lsp-change-batch-test" :noquery t))
         (current process)
         new-process
         (sync-kind 2)
         (my/lsp-remote-change--queues (make-hash-table :test #'eq))
         (my/lsp-remote-change-batch-delay 60)
         fail-uri
         sent)
    (unwind-protect
        (with-temp-buffer
          (setq-local my/lsp-remote-change--eligible t)
          (cl-letf (((symbol-function 'lsp--workspace-proc)
                     (lambda (_workspace) current))
                    ((symbol-function 'lsp--workspace-sync-method)
                     (lambda (_workspace) sync-kind))
                    ((symbol-function 'lsp-notify)
                     (lambda (method params)
                       (when (equal fail-uri
                                    (plist-get
                                     (plist-get params :textDocument) :uri))
                         (error "Simulated transport refusal"))
                       (setq sent (append sent (list (cons method params)))))))
            (let ((lsp--cur-workspace workspace)
                  (fallback (lambda (method params)
                              (setq sent
                                    (append sent (list (cons method params)))))))
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didChange"
               '(:textDocument (:uri "file:///a.py" :version 1)
                 :contentChanges [(:text "a")]))
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didChange"
               '(:textDocument (:uri "file:///a.py" :version 2)
                 :contentChanges [(:text "b")]))
              (should-not sent)
              (should (= (plist-get
                          (gethash workspace my/lsp-remote-change--queues)
                          :count)
                         2))
              (my/lsp-remote-change--send-a
               (lambda (_message _process)
                 (setq sent (append sent '(request))))
               '(:method "textDocument/completion") process)
              (should (= (length sent) 2))
              (should (equal (caar sent) "textDocument/didChange"))
              (should (equal (plist-get (cdar sent) :textDocument)
                             '(:uri "file:///a.py" :version 2)))
              (should (equal (plist-get (cdar sent) :contentChanges)
                             [(:text "a") (:text "b")]))
              (should (eq (cadr sent) 'request))
              (should-not (gethash workspace my/lsp-remote-change--queues))
              (setq sent nil)
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didChange"
               '(:textDocument (:uri "file:///a.py" :version 3)
                 :contentChanges [(:text "c")]))
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didSave"
               '(:textDocument (:uri "file:///a.py")))
              (should (equal (mapcar #'car sent)
                             '("textDocument/didChange"
                               "textDocument/didSave")))
              (setq sent nil)
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didChange"
               '(:textDocument (:uri "file:///a.py" :version 4)
                 :contentChanges [(:text "d")]))
              (my/lsp-remote-change--request-a
               (lambda (&rest _arguments)
                 (setq sent (append sent '(request))))
               "textDocument/completion" nil)
              (should (equal (mapcar (lambda (item)
                                       (if (consp item) (car item) item))
                                     sent)
                             '("textDocument/didChange" request)))
              (setq sent nil)
              (dolist (entry '(("file:///a.py" 3 "c")
                              ("file:///b.py" 1 "x")
                              ("file:///a.py" 4 "d")))
                (my/lsp-remote-change--notify-a
                 fallback "textDocument/didChange"
                 (list :textDocument
                       (list :uri (nth 0 entry) :version (nth 1 entry))
                       :contentChanges
                       (vector (list :text (nth 2 entry))))))
              (my/lsp-remote-change--flush workspace)
              (should (equal (mapcar
                              (lambda (item)
                                (plist-get
                                 (plist-get (cdr item) :textDocument) :uri))
                              sent)
                             '("file:///a.py" "file:///b.py"
                               "file:///a.py")))
              (setq sent nil)
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didChange"
               '(:textDocument (:uri "file:///a.py" :version 5)
                 :contentChanges [(:text "e")]))
              (setq new-process (make-pipe-process
                                 :name "lsp-change-batch-new" :noquery t)
                    current new-process)
              (my/lsp-remote-change--flush workspace)
              (should-not sent)
              (should-not (gethash workspace my/lsp-remote-change--queues))
              (setq current process)
              (let ((my/lsp-remote-change-batch-max-events 2))
                (my/lsp-remote-change--notify-a
                 fallback "textDocument/didChange"
                 '(:textDocument (:uri "file:///a.py" :version 6)
                   :contentChanges [(:text "f")]))
                (my/lsp-remote-change--notify-a
                 fallback "textDocument/didChange"
                 '(:textDocument (:uri "file:///a.py" :version 7)
                   :contentChanges [(:text "g")]))
                (should (= (length sent) 1))
                (should (equal (plist-get (cdar sent) :contentChanges)
                               [(:text "f") (:text "g")]))
                (should-not
                 (gethash workspace my/lsp-remote-change--queues)))
              (setq sent nil sync-kind 1)
              (my/lsp-remote-change--notify-a
               fallback "textDocument/didChange"
               '(:textDocument (:uri "file:///a.py" :version 8)
                 :contentChanges [(:text "h")]))
              (should (= (length sent) 1))
              (should-not
               (gethash workspace my/lsp-remote-change--queues))
              (setq sent nil sync-kind 2)
              (dolist (entry '(("file:///a.py" 9 "i")
                              ("file:///b.py" 1 "j")
                              ("file:///a.py" 10 "k")))
                (my/lsp-remote-change--notify-a
                 fallback "textDocument/didChange"
                 (list :textDocument
                       (list :uri (nth 0 entry) :version (nth 1 entry))
                       :contentChanges (vector (list :text (nth 2 entry))))))
              (setq fail-uri "file:///b.py")
              (should-error
               (my/lsp-remote-change--send-a
                (lambda (_message _process)
                  (setq sent (append sent '(request))))
                '(:method "textDocument/completion") process))
              (should (equal (mapcar #'car sent)
                             '("textDocument/didChange")))
              (should (= (plist-get
                          (gethash workspace my/lsp-remote-change--queues)
                          :count)
                         2))
              (setq fail-uri nil)
              (my/lsp-remote-change--send-a
               (lambda (_message _process)
                 (setq sent (append sent '(request))))
               '(:method "textDocument/completion") process)
              (should (equal (mapcar (lambda (item)
                                       (if (consp item) (car item) item))
                                     sent)
                             '("textDocument/didChange"
                               "textDocument/didChange"
                               "textDocument/didChange" request)))
              (should (equal (mapcar (lambda (item)
                                       (plist-get
                                        (plist-get (cdr item) :textDocument)
                                        :uri))
                                     (butlast sent))
                             '("file:///a.py" "file:///b.py"
                               "file:///a.py")))
              (should-not
               (gethash workspace my/lsp-remote-change--queues)))))
      (my/lsp-remote-change--clear workspace)
      (when (processp new-process)
        (delete-process new-process))
      (delete-process process))))

(ert-deftest lsp-remote-completion-prewarm-is-async-and-generation-scoped ()
  "One remote workspace warms once per process without editing its buffer."
  (let* ((workspace (list 'prewarm-workspace))
         (my/lsp-completion--prewarm-state
          (make-hash-table :test #'eq))
         (first (make-pipe-process :name "lsp-prewarm-first" :noquery t))
         (second (make-pipe-process :name "lsp-prewarm-second" :noquery t))
         (current first)
         (buffer (generate-new-buffer " *lsp-prewarm-test*"))
         requests)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "value = 1\n")
            (setq-local major-mode 'python-mode
                        buffer-file-name
                        (expand-file-name "prewarm.py" temporary-file-directory)
                        lsp-managed-mode t
                        my/lsp-remote-change--eligible t)
            (goto-char (point-min)))
          (cl-letf (((symbol-function 'my/language-server--lsp-workspace-id)
                     (lambda (_workspace) 'my-python))
                    ((symbol-function 'lsp--workspace-cmd-proc)
                     (lambda (_workspace) current))
                    ((symbol-function 'lsp--workspace-status)
                     (lambda (_workspace) 'initialized))
                    ((symbol-function 'lsp--workspace-buffers)
                     (lambda (_workspace) (list buffer)))
                    ((symbol-function 'lsp--text-document-position-params)
                     (lambda (&rest _arguments)
                       (list :position (point))))
                    ((symbol-function 'lsp-request-async)
                     (lambda (method params callback &rest _keys)
                       (push (list method params) requests)
                       (funcall callback nil))))
            (my/lsp-completion--prewarm-schedule workspace)
            (let ((timer
                   (plist-get
                    (gethash workspace
                             my/lsp-completion--prewarm-state)
                    :timer)))
              (my/lsp-completion--prewarm-schedule workspace)
              (should (eq timer
                          (plist-get
                           (gethash workspace
                                    my/lsp-completion--prewarm-state)
                           :timer))))
            (with-current-buffer buffer
              (let ((old-point (point))
                    (old-tick (buffer-chars-modified-tick)))
                (my/lsp-completion--prewarm-dispatch
                 workspace first 0)
                (should (= old-point (point)))
                (should (= old-tick (buffer-chars-modified-tick)))))
            (should (equal (caar requests) "textDocument/completion"))
            (should (= (length requests) 1))
            (should (eq (plist-get
                         (gethash workspace
                                  my/lsp-completion--prewarm-state)
                         :state)
                        'ready))
            (setq current second)
            (my/lsp-completion--prewarm-schedule workspace)
            (my/lsp-completion--prewarm-finish
             workspace first 'ready)
            (should (eq (plist-get
                         (gethash workspace
                                  my/lsp-completion--prewarm-state)
                         :process)
                        second))
            (should (eq (plist-get
                         (gethash workspace
                                  my/lsp-completion--prewarm-state)
                         :state)
                        'scheduled))))
      (my/lsp-completion--prewarm-clear workspace)
      (delete-process first)
      (delete-process second)
      (kill-buffer buffer))))

(ert-deftest lsp-remote-company-waits-for-prewarm-without-losing-idle-popup ()
  "Automatic Company avoids a cold remote request and resumes safely."
  (let* ((workspace (list 'company-prewarm-workspace))
         (process (list 'company-prewarm-process))
         (my/lsp-completion--prewarm-state
          (make-hash-table :test #'eq))
         (buffer (generate-new-buffer " *python-company-prewarm*"))
         scheduled resumed)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "prin")
            (setq-local major-mode 'python-mode
                        lsp-managed-mode t
                        my/lsp-remote-change--eligible t
                        company-mode t
                        company-idle-delay 0.28)
            (goto-char (point-max)))
          (cl-letf (((symbol-function 'lsp-workspaces)
                     (lambda () (list workspace)))
                    ((symbol-function 'lsp--workspace-buffers)
                     (lambda (_workspace) (list buffer)))
                    ((symbol-function 'lsp--workspace-cmd-proc)
                     (lambda (_workspace) process))
                    ((symbol-function 'lsp--workspace-status)
                     (lambda (_workspace) 'initialized))
                    ((symbol-function 'my/language-server--lsp-workspace-id)
                     (lambda (_workspace) 'my-python))
                    ((symbol-function 'company--should-begin)
                     (lambda () t))
                    ((symbol-function 'company-idle-begin)
                     (lambda (&rest args) (push args resumed)))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat callback &rest args)
                       (push (cons callback args) scheduled))))
            (with-current-buffer buffer
              (my/lsp-completion--company-install-h))
            (puthash workspace (list :process process :state 'sent)
                     my/lsp-completion--prewarm-state)
            (with-current-buffer buffer
              (should-not
               (my/lsp-completion--company-idle-delay))
              (should my/lsp-completion--pending-company))
            (my/lsp-completion--prewarm-finish
             workspace process 'ready)
            (should (= (length scheduled) 1))
            (with-current-buffer buffer
              (should (= (my/lsp-completion--company-idle-delay)
                         my/lsp-completion-ready-idle-delay)))
            (let ((timer (pop scheduled)))
              (funcall (car timer) (nth 0 (cdr timer))
                       (nth 1 (cdr timer)) (nth 2 (cdr timer))
                       (nth 3 (cdr timer))))
            (should (= (length resumed) 1))
            (puthash workspace (list :process process :state 'sent)
                     my/lsp-completion--prewarm-state)
            (with-current-buffer buffer
              (should-not
               (my/lsp-completion--company-idle-delay))
              (goto-char (point-min)))
            (my/lsp-completion--prewarm-finish
             workspace process 'ready)
            (should-not scheduled)
            (with-current-buffer buffer
              (goto-char (point-max)))
            (puthash workspace (list :process process :state 'sent)
                     my/lsp-completion--prewarm-state)
            (with-current-buffer buffer
              (should-not
               (my/lsp-completion--company-idle-delay)))
            (my/lsp-completion--prewarm-finish
             workspace process 'failed)
            (should-not scheduled)
            (with-current-buffer buffer
              (should-not my/lsp-completion--pending-company)
              (should (= (my/lsp-completion--company-idle-delay)
                         0.28)))
            (puthash workspace (list :process process :state 'sent)
                     my/lsp-completion--prewarm-state)
            (with-current-buffer buffer
              (should-not
               (my/lsp-completion--company-idle-delay)))
            (my/lsp-completion--prewarm-clear workspace)
            (with-current-buffer buffer
              (should-not my/lsp-completion--pending-company))))
      (kill-buffer buffer))))

(ert-deftest lsp-remote-company-fast-idle-requires-current-ready-server ()
  "A fast timer must not follow a failed or replaced remote server."
  (let* ((workspace (list 'current-python-workspace))
         (process (list 'current-python-process))
         (my/lsp-completion--prewarm-state
          (make-hash-table :test #'eq))
         (my/lsp-completion-ready-idle-delay 0.12))
    (with-temp-buffer
      (setq-local lsp-managed-mode t
                  my/lsp-remote-change--eligible t
                  my/lsp-completion--normal-idle-delay 0.28)
      (cl-letf (((symbol-function 'lsp-workspaces)
                 (lambda () (list workspace)))
                ((symbol-function 'lsp--workspace-cmd-proc)
                 (lambda (_workspace) process))
                ((symbol-function 'lsp--workspace-status)
                 (lambda (_workspace) 'initialized))
                ((symbol-function 'my/language-server--lsp-workspace-id)
                 (lambda (_workspace) 'my-python)))
        (puthash workspace (list :process process :state 'ready)
                 my/lsp-completion--prewarm-state)
        (should (= (my/lsp-completion--company-idle-delay) 0.12))
        (let ((my/lsp-completion-ready-idle-delay nil))
          (should (= (my/lsp-completion--company-idle-delay) 0.28)))
        (setq-local my/lsp-completion--normal-idle-delay 0.08)
        (should (= (my/lsp-completion--company-idle-delay) 0.08))
        (setq-local my/lsp-completion--normal-idle-delay 0.28)
        (puthash workspace (list :process (list 'old-process) :state 'ready)
                 my/lsp-completion--prewarm-state)
        (should (= (my/lsp-completion--company-idle-delay) 0.28))
        (puthash workspace (list :process process :state 'failed)
                 my/lsp-completion--prewarm-state)
        (should (= (my/lsp-completion--company-idle-delay) 0.28))))))

(ert-deftest lsp-mode-local-logical-path-does-not-leak-fs-syntax ()
  (let ((default-directory "/fs:local:/tmp/"))
    (should
     (equal
      (lsp--path-to-uri "/fs:local:/tmp/A.java")
      "file:///tmp/A.java"))
    (should
     (equal
      (lsp--uri-to-path "file:///tmp/A.java")
      "/fs:local:/tmp/A.java"))))

(ert-deftest lsp-mode-session-folder-skips-unrelated-remote-stat-probes ()
  (let* ((folder "/ssh:box:/work/")
         (session
          (make-lsp-session
           :folders
           (append
            (cl-loop for index below 30
                     collect (format "/ssh:box:/other-%d/" index))
            (list folder))))
         (probes nil)
         (native-directory-p (symbol-function 'file-directory-p))
         (native-exists-p (symbol-function 'file-exists-p)))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (path)
                 (if (string-prefix-p "/ssh:" path)
                     (progn (push path probes) (equal path folder))
                   (funcall native-directory-p path))))
              ((symbol-function 'file-exists-p)
               (lambda (path)
                 (if (string-prefix-p "/ssh:" path)
                     (progn (push path probes) (equal path folder))
                   (funcall native-exists-p path)))))
      (should
       (equal
        (my/lsp-mode--find-session-folder-remote-a
         (lambda (&rest _) (ert-fail "Unexpected upstream search"))
         session "/ssh:box:/work/src/main.c")
        folder))
      (should (equal probes (list folder))))))

(ert-deftest lsp-mode-client-package-load-escapes-target-context ()
  (let ((default-directory "/fs:box:/work/")
        (remote-current-adapter-id "language-server")
        (remote-current-route 'target-route)
        (remote-current-workspace 'target-workspace)
        observed)
    (cl-letf (((symbol-function 'remote-client-process-environment)
               (lambda () '("CLIENT_ENV=1")))
              ((symbol-function 'remote-client-exec-path)
               (lambda () '("/client/bin")))
              ((symbol-function 'require)
               (lambda (feature &rest _)
                 (when (eq feature 'test-client-package)
                   (setq observed
                         (list default-directory process-environment
                               exec-path remote-current-adapter-id
                               remote-current-route remote-current-workspace)))
                 t)))
      (should (my/language-server--require-on-client
               'test-client-package)))
    (should
     (equal observed
            (list temporary-file-directory '("CLIENT_ENV=1")
                  '("/client/bin") nil nil nil)))))

(ert-deftest lsp-mode-external-client-feature-loads-on-client ()
  "A first Java client load must not inherit the target buffer directory."
  (with-temp-buffer
    (setq major-mode 'java-mode
          default-directory "/fs:box:/work/")
    (let ((my/lsp-mode-required-features
           '((java-mode . test-external-lsp-feature)))
          seen)
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest _arguments)
                   (when (eq feature 'test-external-lsp-feature)
                     (setq seen default-directory))
                   t)))
        (should (my/lsp-mode-supported-p)))
      (should (equal seen temporary-file-directory)))))

(ert-deftest lsp-mode-remote-uri-returns-to-current-logical-target ()
  (let ((default-directory "/fs:box:/work/"))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/work/src/Main.java")
       "file:///work/src/Main.java")
      "/fs:box:/work/src/Main.java"))))

(ert-deftest lsp-mode-uri-prefers-workspace-target-over-current-buffer ()
  "Async callbacks must not borrow an unrelated buffer's target."
  (let ((default-directory "/fs:local:/tmp/")
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/")))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/work/src/Main.java")
       "file:///work/src/Main.java")
      "/fs:box:/work/src/Main.java"))))

(ert-deftest lsp-mode-local-workspace-uses-the-same-uri-projection ()
  (let ((default-directory "/fs:box:/work/")
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/")))
    (should
     (equal
      (my/lsp-mode--uri-to-logical-a
       (lambda (_uri) "/tmp/project/Main.java")
       "file:///tmp/project/Main.java")
      "/fs:local:/tmp/project/Main.java"))))

(ert-deftest language-server-executable-lookup-uses-shared-adapter ()
  (let (adapter)
    (cl-letf (((symbol-function 'remote-executable-find)
               (lambda (_program &optional _context)
                 (setq adapter remote-current-adapter-id)
                 "/target/bin/server")))
      (should
       (equal
        (my/language-server-executable-find "server")
        "/target/bin/server")))
    (should (equal adapter "language-server"))))

(ert-deftest yasnippet-first-load-stays-on-client-in-logical-buffer ()
  "Local snippet libraries must not inherit a logical target directory."
  (with-temp-buffer
    (let ((default-directory "/fs:local:/tmp/project/")
          (process-environment '("TARGET=1"))
          (exec-path '("/target/bin"))
          (remote-current-adapter-id "language-server")
          (remote-current-workspace 'target-workspace)
          (features
           (delq 'yasnippet
                 (delq 'yasnippet-snippets (copy-sequence features))))
          loaded enabled)
      (cl-letf (((symbol-function 'remote-client-process-environment)
                 (lambda () '("CLIENT=1")))
                ((symbol-function 'remote-client-exec-path)
                 (lambda () '("/client/bin")))
                ((symbol-function 'my/yas--load-local-libraries)
                 (lambda ()
                   (setq loaded
                         (list default-directory process-environment
                               exec-path remote-current-adapter-id
                               remote-current-workspace))))
                ((symbol-function 'yas-minor-mode)
                 (lambda (arg)
                   (setq enabled (list arg default-directory)))))
        (my/yas--enable-now))
      (should (equal (nth 0 loaded) temporary-file-directory))
      (should (equal (nth 1 loaded) '("CLIENT=1")))
      (should (equal (nth 2 loaded) '("/client/bin")))
      (should-not (nth 3 loaded))
      (should-not (nth 4 loaded))
      (should (equal enabled '(1 "/fs:local:/tmp/project/"))))))

(ert-deftest yasnippet-cold-source-hook-defers-but-manual-command-enables ()
  "The initial source visit should avoid snippet loading until idle or use."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (let ((my/yas-enable-idle-delay 0.15)
          (features
           (delq 'yasnippet
                 (delq 'yasnippet-snippets (copy-sequence features))))
          timer
          calls)
      (cl-letf (((symbol-function 'my/yas--load-local-libraries)
                 (lambda () (push 'load calls)))
                ((symbol-function 'yas-minor-mode)
                 (lambda (_arg) (push 'enable calls)))
                ((symbol-function 'yas-insert-snippet)
                 (lambda () (interactive) (push 'insert calls))))
        (unwind-protect
            (progn
              (my/yas-enable-for-source-buffer)
              (setq timer my/yas--pending-enable)
              (should (timerp timer))
              (should-not calls)
              (my/yas-enable-for-source-buffer)
              (should (eq timer my/yas--pending-enable))
              (my/snippet-insert)
              (should (equal (reverse calls) '(load enable insert)))
              (should-not my/yas--pending-enable))
          (when (timerp timer)
            (cancel-timer timer)))))))

(ert-deftest yasnippet-idle-enable-ignores-obsolete-mode ()
  "An idle callback for a previous major mode cannot enable snippets."
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (let (enabled)
      (cl-letf (((symbol-function 'my/yas--enable-now)
                 (lambda () (setq enabled t))))
        (setq major-mode 'python-mode)
        (my/yas--enable-after-idle (current-buffer) 'c-mode)
        (should-not enabled)
        (my/yas--enable-after-idle (current-buffer) 'python-mode)
        (should enabled)))))

(ert-deftest language-server-auto-selector-is-target-neutral ()
  (dolist (directory
           '("/fs:local:/tmp/project/"
             "/fs:box:/work/project/"))
    (let ((default-directory directory)
          selected)
      (cl-letf
          (((symbol-function 'my/language-server-preferred-backend)
            (lambda () 'lsp-mode))
           ((symbol-function 'my/lsp-mode-ensure)
            (lambda () (setq selected 'lsp-mode))))
        (my/language-server--ensure-after-runtime))
      (should (eq selected 'lsp-mode)))))

(ert-deftest language-server-direnv-failure-does-not-disable-client ()
  (let ((started nil))
    (setq my/lsp-mode--waiting-for-direnv t)
    (cl-letf
        (((symbol-function 'my/language-server-preferred-backend)
          (lambda () 'lsp-mode))
         ((symbol-function 'my/lsp-mode-start-now)
          (lambda () (setq started t)))
         ((symbol-function 'message) #'ignore))
      (my/lsp-mode--direnv-ready nil '(error "broken envrc")))
    (should-not my/lsp-mode--waiting-for-direnv)
    (should started)))

(ert-deftest language-server-network-contact-routes-on-local-and-remote ()
  (dolist (directory
           '("/fs:local:/tmp/project/"
             "/fs:box:/work/project/"))
    (let ((default-directory directory)
          (remote-current-adapter-id "language-server")
          captured)
      (cl-letf
          (((symbol-function 'remote-open-network-stream)
            (lambda (name buffer host service &rest parameters)
              (setq captured
                    (list name buffer host service parameters))
              'routed-process)))
        (should
         (eq
          (my/language-server--open-network-stream-a
           (lambda (&rest _) (error "native path was used"))
           "server" nil "127.0.0.1" 2087 :type 'plain)
          'routed-process)))
      (should
       (equal
        (remote-context-target-id
         (plist-get (nth 4 captured) :remote-context))
        (remote-file-name-target directory)))
      (should
       (equal
        (plist-get (nth 4 captured) :remote-adapter)
        "language-server")))))

(ert-deftest language-server-shell-boundary-is-target-neutral ()
  ;; `my/language-server-apply-lsp-local-settings' also resolves any
  ;; project-local `:lsp-workspace' override; stub that lookup rather than
  ;; exercising real project/file-truename plumbing for a target this test
  ;; never links a route for.
  (cl-letf (((symbol-function 'my/language-server-project-workspace-configuration)
             (lambda () nil)))
    (dolist (directory
             '("/fs:local:/tmp/project/"
               "/fs:box:/work/project/"))
      (with-temp-buffer
        (let ((default-directory directory)
              (shell-file-name "/client/bin/zsh")
              (explicit-shell-file-name "/client/bin/zsh")
              (shell-command-switch "-c")
              (my/language-server-lsp-local-settings-hook nil))
          (my/language-server-apply-lsp-local-settings)
          ;; Shell placement belongs to the selected process backend.  Neither
          ;; a local nor a remote logical root may mutate client shell
          ;; globals.
          (should (equal shell-file-name "/client/bin/zsh"))
          (should (equal explicit-shell-file-name "/client/bin/zsh"))
          (should (equal shell-command-switch "-c")))))))

(ert-deftest python-language-server-command-is-selected-by-target-capability-only ()
  (cl-letf
      (((symbol-function 'my/language-server-executable-find)
        (lambda (program)
          (and (equal program "pylsp") "/target/bin/pylsp")))
       ((symbol-function 'my/lsp-python--provisioned-command)
        (lambda () nil)))
    (dolist (directory
             '("/fs:local:/tmp/project/"
               "/fs:box:/work/project/"))
      (let ((default-directory directory))
        (should
         (equal
          (my/python-language-server-command)
          '("/target/bin/pylsp")))))))

(ert-deftest js-project-bin-uses-the-environment-capsule-for-every-target ()
  (let (prepends)
    (cl-letf
        (((symbol-function 'locate-dominating-file)
          (lambda (directory _name) directory))
         ((symbol-function 'file-directory-p) (lambda (_file) t))
         ((symbol-function 'remote-environment-ensure)
          (lambda (&rest _arguments) 'base))
         ((symbol-function 'remote-environment-derive)
          (lambda (_environment _id &rest properties)
            (push (plist-get properties :path-prepend) prepends)
            'derived))
         ((symbol-function 'remote-environment-apply)
          (lambda (environment &optional _buffer) environment)))
      (dolist (directory
               '("/fs:local:/tmp/project/"
                 "/fs:box:/tmp/project/"))
        (with-temp-buffer
          (setq default-directory directory)
          (my/js-setup-project-node-bin))))
    (should
     (equal
      prepends
      '(("/tmp/project/node_modules/.bin")
        ("/tmp/project/node_modules/.bin"))))))

(ert-deftest java-missing-target-runtime-prevents-jdtls-selection ()
  "A client-local JDTLS bundle must not start without target-side Java."
  (with-temp-buffer
    (setq major-mode 'java-mode)
    (let ((my/language-server-toolchain--applied-profile nil))
      (cl-letf (((symbol-function 'my/language-server--require-on-client)
                 (lambda (_feature) t))
                ((symbol-function 'lsp--filter-clients)
                 (lambda (&rest _arguments)
                   (ert-fail "JDTLS client selection ran without Java"))))
        (should-not (my/language-server--external-feature-ready-p))
        (should-not (my/language-server-contact-available-p))))
    (let ((my/language-server-toolchain--applied-profile
           '(:executable "/opt/jdk/bin/java"))
          seen)
      (cl-letf (((symbol-function 'my/language-server-executable-find)
                 (lambda (program)
                   (setq seen program)
                   program)))
        (should (my/language-server--external-feature-ready-p))
        (should (equal seen "/opt/jdk/bin/java"))))))

(ert-deftest java-command-and-debug-channel-use-one-target-projection ()
  (should
   (equal
    (my/lsp-java--target-command-a
     (lambda ()
       '("/fs:box:/opt/jdtls/bin/jdtls"
         "-data" "/fs:box:/work/.cache/")))
    '("/opt/jdtls/bin/jdtls" "-data" "/work/.cache/")))
  (let ((my/java-debug--forwards (make-hash-table :test #'equal))
        calls)
    (cl-letf
        (((symbol-function 'remote-port-forward)
          (lambda (remote-endpoint &rest arguments)
            (push (list remote-endpoint arguments) calls)
            (remote-forward-create
             :handle nil
             :state 'open
             :remote-endpoint remote-endpoint
             :local-endpoint '(:host "127.0.0.1" :port 41000)))))
      (dolist (root
               '("/fs:local:/tmp/project/"
                 "/fs:box:/work/project/"))
        (should (= (my/java-debug--access-port root 5005) 41000))))
    (should (= (length calls) 2))))

(ert-deftest java-native-command-adds-per-root-workspace-and-config ()
  "In native-launcher mode, `-data'/`-configuration' must be appended.
`bin/jdtls' derives its own workspace directory from
`sha1(basename(cwd))' when `-data' is absent, so two Java projects that
merely share a directory basename would otherwise collide on one shared,
possibly half-imported JDTLS workspace."
  (require 'lsp-java)
  (let ((lsp-java-jdt-ls-prefer-native-command t)
        (lsp-java-workspace-dir "/fs:box:/work/.cache/jdtls-workspace/")
        (lsp-java-server-config-dir "/fs:box:/opt/jdtls/config_linux/"))
    (should
     (equal
      (my/lsp-java--target-command-a
       (lambda ()
         '("/fs:box:/opt/jdtls/bin/jdtls" "--jvm-arg=-Dlog.level=ALL")))
      '("/opt/jdtls/bin/jdtls" "--jvm-arg=-Dlog.level=ALL"
        "-data" "/work/.cache/jdtls-workspace/"
        "-configuration" "/opt/jdtls/config_linux/")))))

(ert-deftest java-native-launcher-uses-logical-executable-check ()
  (let ((lsp-java-jdt-ls-prefer-native-command t)
        (lsp-java-jdt-ls-command "jdtls")
        (lsp-java-server-install-dir "/fs:box:/opt/jdtls/")
        checked)
    (cl-letf (((symbol-function 'file-executable-p)
               (lambda (file)
                 (setq checked file)
                 t)))
      (should
       (equal
        (my/lsp-java--locate-server-command-a
         (lambda () (ert-fail "locate-file lost a valid target launcher")))
        "/fs:box:/opt/jdtls/bin/jdtls"))
      (should (equal checked "/fs:box:/opt/jdtls/bin/jdtls")))))

(ert-deftest java-non-native-command-does-not-add-data-flags ()
  "Jar-mode commands already carry their own `-data'/`-configuration';
this advice must not duplicate them when native mode is off."
  (require 'lsp-java)
  (let ((lsp-java-jdt-ls-prefer-native-command nil)
        (lsp-java-workspace-dir "/fs:box:/work/.cache/jdtls-workspace/"))
    (should
     (equal
      (my/lsp-java--target-command-a
       (lambda ()
         '("/fs:box:/opt/jdtls/bin/jdtls" "-data" "/fs:box:/work/.cache/")))
      '("/opt/jdtls/bin/jdtls" "-data" "/work/.cache/")))))

(ert-deftest java-buffers-select-only-one-jdtls-workspace-owner ()
  "One JDTLS client id serves every target now that
`lsp-auto-register-remote-clients' is nil, so a single root/folder wipe
must cover it without a `-tramp' counterpart."
  (require 'lsp-java)
  (let* ((clients (make-hash-table :test #'eq))
         (folders (make-hash-table :test #'eq))
         (session
          (make-lsp-session
           :folders nil
           :folders-blocklist nil
           :server-id->folders folders))
         (client (copy-lsp--client (gethash 'jdtls lsp-clients)))
         (lsp-clients clients)
         persisted)
    (puthash 'jdtls client clients)
    (puthash 'jdtls '("/old/root") folders)
    (with-temp-buffer
      (setq major-mode 'java-mode)
      (let ((lsp-enabled-clients nil))
        (cl-letf
            (((symbol-function 'lsp-session) (lambda () session))
             ((symbol-function 'lsp--persist-session)
              (lambda (_session) (setq persisted t))))
          (my/lsp-java--enforce-single-root))
        (should (equal lsp-enabled-clients '(jdtls)))))
    (should-not (lsp--client-multi-root client))
    (should-not (gethash 'jdtls folders))
    (should persisted)))

(ert-deftest lsp-workspace-is-one-recoverable-remote-workspace-resource ()
  (let ((remote-workspaces (make-hash-table :test #'equal))
        (remote-workspace--resource-counter 0)
        (lsp-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/")))
    (cl-letf
        (((symbol-function 'remote-workspace-track-route)
          (lambda (workspace &rest _arguments) workspace))
         ((symbol-function 'my/language-server--lsp-workspace-id)
          (lambda (_workspace) 'test-server)))
      (let ((lsp--cur-workspace lsp-workspace))
        (my/language-server-register-lsp-resource)
        (my/language-server-register-lsp-resource))
      (let* ((owner (car (hash-table-values remote-workspaces)))
             (resource
              (remote-workspace-find-resource
               owner 'lsp
               '(lsp-mode test-server "/fs:local:/tmp/project/"))))
        (should resource)
        (should (= (length (remote-workspace-resources owner)) 1))
        (should (eq (remote-workspace-resource-value resource)
                    lsp-workspace))))))

(ert-deftest dead-lsp-process-recovers-from-original-source-buffer ()
  "Transport recovery keeps the resource and opens a new LSP workspace."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (remote-workspace--resource-counter 0)
         (old (make-lsp--workspace :root "/fs:local:/tmp/project/"))
         (replacement
          (make-lsp--workspace :root "/fs:local:/tmp/project/"))
         (buffer (generate-new-buffer " *lsp-recovery-source*"))
         (other (generate-new-buffer " *lsp-recovery-sibling*"))
         restarted)
    (unwind-protect
        (progn
          (with-current-buffer other
            (setq buffer-file-name "/fs:local:/tmp/project/other.el"))
          (cl-letf
            (((symbol-function 'remote-workspace-track-route)
              (lambda (workspace &rest _arguments) workspace))
             ((symbol-function 'my/language-server--lsp-workspace-id)
              (lambda (_workspace) 'test-server))
             ((symbol-function 'lsp--workspace-buffers)
              (lambda (_workspace) (list buffer other)))
             ((symbol-function 'lsp--workspace-cmd-proc)
              (lambda (_workspace) nil))
             ((symbol-function 'lsp)
              (lambda (&optional _argument)
                (push (current-buffer) restarted)))
             ((symbol-function 'lsp-workspaces)
              (lambda () (list replacement))))
          (let ((lsp--cur-workspace old))
            (my/language-server-register-lsp-resource))
          (let* ((owner (car (hash-table-values remote-workspaces)))
                 (resource (remote-workspace-find-resource owner 'lsp)))
            (should resource)
            (setf (remote-workspace-state owner) 'reconnecting)
            (my/language-server--forget-resource-value old)
            (should (eq (remote-workspace-find-resource owner 'lsp)
                        resource))
            (remote-workspace-recover-resource owner resource)
            (should (equal restarted (list other buffer)))
            (should (eq (remote-workspace-resource-value resource)
                        replacement))
            (setf (remote-workspace-state owner) 'open)
            (my/language-server--forget-resource-value replacement)
            (should-not (remote-workspace-find-resource owner 'lsp)))))
      (kill-buffer buffer)
      (kill-buffer other))))

(ert-deftest lsp-crash-restart-defers-to-remote-transport-recovery ()
  "A transport reset must not start a competing LSP process."
  (let* ((owner (remote-workspace-create :state 'disconnected))
         (my/lsp-mode--restart-history (make-hash-table :test #'equal))
         (key '(test-server "/fs:local:/tmp/project/"))
         (calls 0))
    (setf (remote-workspace-resources owner)
          (list (remote-workspace-resource-create
                 :kind 'lsp :value 'old-workspace)))
    (cl-letf (((symbol-function 'my/lsp-mode--workspace-key)
               (lambda (_workspace) key))
              ((symbol-function 'remote-get-workspace)
               (lambda (_root) owner)))
      (my/lsp-mode--restart-with-circuit-breaker-a
       (lambda (_workspace) (cl-incf calls)) 'old-workspace)
      (should (= calls 0))
      (should-not (gethash key my/lsp-mode--restart-history))
      (setf (remote-workspace-state owner) 'reconnecting)
      (my/lsp-mode--restart-with-circuit-breaker-a
       (lambda (_workspace) (cl-incf calls)) 'old-workspace)
      (should (= calls 0))
      (setf (remote-workspace-state owner) 'failed)
      (my/lsp-mode--restart-with-circuit-breaker-a
       (lambda (_workspace) (cl-incf calls)) 'old-workspace)
      (should (= calls 0))
      (setf (remote-workspace-state owner) 'open)
      (my/lsp-mode--restart-with-circuit-breaker-a
       (lambda (_workspace) (cl-incf calls)) 'old-workspace)
      (should (= calls 1))
      (should (= (length (gethash key my/lsp-mode--restart-history)) 1)))))

(ert-deftest lsp-exit-reports-dead-pooled-transport-before-unregister ()
  "A dead transport keeps the LSP resource for Remote's own recovery."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (route (remote-route-create
                 :target-id "box" :link-id "ssh"
                 :link-plugin-id "tramp-rpc"))
         (value (make-lsp--workspace :root "/fs:box:/project/"))
         (resource (remote-workspace-resource-create
                    :kind 'lsp :value value :state 'open))
         (owner (remote-workspace-create
                 :state 'open :primary-route route
                 :resources (list resource)))
         (reports 0))
    (puthash 'owner owner remote-workspaces)
    (cl-letf (((symbol-function 'lsp--workspace-cmd-proc)
               (lambda (_workspace) nil))
              ((symbol-function 'remote-connection-cached-p)
               (lambda (_route) 'cached))
              ((symbol-function 'remote-connection--live-p)
               (lambda (&rest _) nil))
              ((symbol-function 'remote-report-route-failure)
               (lambda (actual-route error)
                 (should (eq actual-route route))
                 (should (eq (car error) 'remote-transport-error))
                 (cl-incf reports)
                 (setf (remote-workspace-state owner) 'disconnected))))
      (my/language-server-unregister-lsp-resource value))
    (should (= reports 1))
    (should (memq resource (remote-workspace-resources owner)))))

(ert-deftest lsp-exit-forgets-resource-when-transport-is-live ()
  "An ordinary language server exit must leave the transport healthy."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (route (remote-route-create
                 :target-id "box" :link-id "ssh"
                 :link-plugin-id "tramp-rpc"))
         (value (make-lsp--workspace :root "/fs:box:/project/"))
         (resource (remote-workspace-resource-create
                    :kind 'lsp :value value :state 'open))
         (owner (remote-workspace-create
                 :state 'open :primary-route route
                 :resources (list resource))))
    (puthash 'owner owner remote-workspaces)
    (cl-letf (((symbol-function 'lsp--workspace-cmd-proc)
               (lambda (_workspace) nil))
              ((symbol-function 'remote-connection-cached-p)
               (lambda (_route) 'cached))
              ((symbol-function 'remote-connection--live-p)
               (lambda (&rest _) t))
              ((symbol-function 'remote-report-route-failure)
               (lambda (&rest _) (ert-fail "Healthy transport reported dead"))))
      (my/language-server-unregister-lsp-resource value))
    (should (eq (remote-workspace-state owner) 'open))
    (should-not (remote-workspace-resources owner))))

(ert-deftest lsp-runtime-workspaces-own-distinct-remote-resources ()
  "Kernel identity separates handles without changing their Remote owner."
  (let* ((remote-workspaces (make-hash-table :test #'equal))
         (remote-workspace--resource-counter 0)
         (root "/fs:local:/tmp/project/")
         (first (make-lsp--workspace :root root))
         (second (make-lsp--workspace :root root)))
    (puthash my/language-server-runtime--workspace-metadata-key "kernel-one"
             (lsp--workspace-metadata first))
    (puthash my/language-server-runtime--workspace-metadata-key "kernel-two"
             (lsp--workspace-metadata second))
    (cl-letf
        (((symbol-function 'remote-workspace-track-route)
          (lambda (workspace &rest _arguments) workspace))
         ((symbol-function 'my/language-server--lsp-workspace-id)
          (lambda (_workspace) 'my-python)))
      (let ((lsp--cur-workspace first))
        (my/language-server-register-lsp-resource))
      (let ((lsp--cur-workspace second))
        (my/language-server-register-lsp-resource)))
    (let ((owner (car (hash-table-values remote-workspaces))))
      (should owner)
      (should (= (length (remote-workspace-resources owner)) 2))
      (should
       (remote-workspace-find-resource
        owner 'lsp
        '(lsp-mode my-python "/fs:local:/tmp/project/" "kernel-one")))
      (should
       (remote-workspace-find-resource
        owner 'lsp
        '(lsp-mode my-python "/fs:local:/tmp/project/" "kernel-two"))))))

(ert-deftest lsp-mode-target-only-root-uses-remote-watch-capability ()
  "Target-only roots keep watcher parity when Remote can own the watches."
  (let ((my/language-server-file-watch-policy 'auto)
        (lsp-enable-file-watchers t)
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/project/"))
        watcher-value
        called)
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) nil))
         ((symbol-function 'remote-routes)
          (lambda (&rest _)
            '(remote-watch-route))))
      (should
       (eq
        (my/lsp-mode--register-capability-via-remote-a
         (lambda (_registration)
           (setq called t
                 watcher-value lsp-enable-file-watchers)
           'registered)
         (my/lsp-test-registration
          "workspace/didChangeWatchedFiles"))
        'registered)))
    (should called)
    (should watcher-value)))

(ert-deftest lsp-mode-target-only-root-declines-watch-without-route ()
  (let ((my/language-server-file-watch-policy 'auto))
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) nil))
         ((symbol-function 'remote-routes)
          (lambda (&rest _) nil)))
      (should
       (my/language-server--skip-file-watch-p
        "/fs:box:/work/project/")))))

(ert-deftest lsp-mode-client-accessible-root-keeps-native-watchers ()
  (let ((my/language-server-file-watch-policy 'auto)
        (lsp-enable-file-watchers t)
        (lsp--cur-workspace
         (make-lsp--workspace :root "/fs:local:/tmp/project/"))
        watcher-value)
    (cl-letf
        (((symbol-function 'remote-client-file-name)
          (lambda (&rest _) "/tmp/project/")))
      (my/lsp-mode--register-capability-via-remote-a
       (lambda (_registration)
         (setq watcher-value lsp-enable-file-watchers))
       (my/lsp-test-registration
        "workspace/didChangeWatchedFiles")))
    (should watcher-value)))

(ert-deftest lsp-mode-loads-client-definitions-under-local-environment ()
  (let ((default-directory "/rpc:box:/tmp/project/")
        (lsp--client-packages-required nil)
        seen-directory seen-environment seen-exec-path)
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest _) t))
              ((symbol-function 'remote-client-process-environment)
               (lambda () '("HOME=/client")))
              ((symbol-function 'remote-client-exec-path)
               (lambda () '("/client/bin"))))
      (my/lsp-mode--require-client-packages-on-client-a
       (lambda ()
         (setq seen-directory default-directory
               seen-environment process-environment
               seen-exec-path exec-path))))
    (should (equal seen-directory temporary-file-directory))
    (should (equal seen-environment '("HOME=/client")))
    (should (equal seen-exec-path '("/client/bin")))))

(ert-deftest lsp-mode-skips-unneeded-client-package-load-on-startup ()
  "An already registered whitelist must not initialize all stock clients."
  (let ((lsp-enabled-clients '(my-clangd))
        (lsp-clients (make-hash-table :test #'eq))
        (lsp--client-packages-required nil)
        (my/lsp-mode--starting-client-selection t)
        (loads 0))
    (puthash 'my-clangd 'registered lsp-clients)
    (my/lsp-mode--require-client-packages-on-client-a
     (lambda () (cl-incf loads)))
    (should (= loads 0))
    (should-not lsp--client-packages-required)
    (let ((my/lsp-mode--starting-client-selection nil))
      (my/lsp-mode--require-client-packages-on-client-a
       (lambda () (cl-incf loads))))
    (should (= loads 1))
    (remhash 'my-clangd lsp-clients)
    (my/lsp-mode--require-client-packages-on-client-a
     (lambda () (cl-incf loads)))
    (should (= loads 2))))

(ert-deftest lsp-mode-target-watch-root-is-logical-and-workspace-owned ()
  (let (seen-directory seen-owner seen-adapter seen-metadata
        seen-flags seen-callback delivered)
    (cl-letf
        (((symbol-function 'my/language-server--canonical-root)
          (lambda (_directory) "/fs:box:/work/project/"))
         ((symbol-function 'remote-file-name-target)
          (lambda (_directory) "box"))
         ((symbol-function 'my/language-server--connect-workspace)
          (lambda (_root) 'owner))
         ((symbol-function 'remote-watch-tree)
          (lambda (directory flags callback)
            (setq seen-directory directory
                  seen-flags flags
                  seen-callback callback
                  seen-owner remote-file-watch-workspace
                  seen-adapter remote-current-adapter-id
                  seen-metadata remote-file-watch-metadata)
            'descriptor)))
      (let ((watch
             (my/lsp-mode--watch-root-via-remote-a
              (lambda (&rest _arguments)
                (ert-fail "A recursive watch must avoid the directory scan"))
              "/ssh:box:/work/project/"
              (lambda (event) (push event delivered))
              '("ignored-file") '("ignored-dir"))))
        (should (lsp-watch-p watch))
        (should (= (hash-table-count (lsp-watch-descriptors watch)) 1))
        (should (eq (gethash seen-directory
                             (lsp-watch-descriptors watch))
                    'descriptor))
        (funcall seen-callback '(descriptor changed "/fs:box:/work/project/a"))
        (funcall seen-callback
                 '(descriptor changed "/fs:box:/work/project/ignored-file"))
        (should (equal delivered
                       '((descriptor changed "/fs:box:/work/project/a"))))
        (cl-letf (((symbol-function 'file-directory-p)
                   (lambda (file)
                     (equal file "/fs:box:/work/project/new-dir")))
                  ((symbol-function 'directory-files-recursively)
                   (lambda (_directory _regexp &rest _)
                     '("/fs:box:/work/project/new-dir/a"
                       "/fs:box:/work/project/new-dir/ignored-file"))))
          (funcall seen-callback
                   '(descriptor created "/fs:box:/work/project/new-dir")))
        (should (equal (car delivered)
                       '(descriptor created
                                    "/fs:box:/work/project/new-dir/a")))))
    (should (equal seen-directory "/fs:box:/work/project/"))
    (should (equal seen-flags '(change)))
    (should (eq seen-owner 'owner))
    (should (equal seen-adapter "language-server"))
    (should (eq (plist-get seen-metadata :owner) 'lsp-mode))))

(ert-deftest lsp-mode-recursive-watch-unsupported-falls-back ()
  (let (fallback-directory)
    (cl-letf
        (((symbol-function 'my/language-server--canonical-root)
          (lambda (_directory) "/fs:box:/work/project/"))
         ((symbol-function 'remote-file-name-target)
          (lambda (_directory) "box"))
         ((symbol-function 'my/language-server--connect-workspace)
          (lambda (_root) 'owner))
         ((symbol-function 'remote-watch-tree)
          (lambda (&rest _)
            (signal 'remote-backend-unsupported '("No recursive watcher")))))
      (should
       (eq (my/lsp-mode--watch-root-via-remote-a
            (lambda (directory &rest _)
              (setq fallback-directory directory)
              'fallback)
            "/ssh:box:/work/project/" #'ignore nil nil)
           'fallback)))
    (should (equal fallback-directory "/fs:box:/work/project/"))))

(ert-deftest lsp-mode-bounded-shutdown-is-idempotent-and-keeps-source-buffer ()
  "Stopping a language server must never kill a user source buffer."
  (let* ((source (generate-new-buffer " *remote-java-source*"))
         (workspace
          (make-lsp--workspace
           :root "/fs:box:/work/project/"
           :buffers (list source)
           :shutdown-action nil))
         (shutdown-count 0)
         (killed-processes nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'lsp-workspace-shutdown)
              (lambda (_workspace)
                (cl-incf shutdown-count)
                ;; This is the strongest source-side operation performed by
                ;; upstream shutdown: remove managed-mode state in-place.
                (with-current-buffer source
                  (setq-local lsp-managed-mode nil))))
             ((symbol-function 'lsp-process-kill)
              (lambda (process)
                (push process killed-processes))))
          (should
           (my/lsp-mode-shutdown-workspace workspace 'test-shutdown))
          (should (buffer-live-p source))
          (should
           (my/lsp-mode-shutdown-workspace workspace 'repeated-shutdown))
          (should (buffer-live-p source))
          (should (= shutdown-count 1))
          (should-not killed-processes))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest lsp-mode-remote-shutdown-allows-route-latency ()
  (let ((lsp--cur-workspace
         (make-lsp--workspace :root "/fs:box:/work/project/"))
        (lsp-response-timeout 0.5)
        (my/lsp-mode-remote-shutdown-response-timeout 2)
        seen)
    (my/lsp-mode--remote-shutdown-timeout-a
     (lambda (_method _params &rest _keys)
       (setq seen lsp-response-timeout))
     "shutdown" nil)
    (should (= seen 2))))

;;; ── New in the single-backend migration ─────────────────────────────────────

(ert-deftest language-server-booster-wraps-resolved-command-per-target ()
  "The booster path must resolve on the same target that runs the server,
never the client, and must fail loudly rather than silently skip when
required and missing."
  (dolist (case '(("/fs:local:/tmp/project/" . "/local/bin/emacs-lsp-booster")
                  ("/fs:box:/work/project/" . "/box/bin/emacs-lsp-booster")))
    (let ((default-directory (car case))
          (my/language-server-booster-required t)
          seen-context)
      (cl-letf
          (((symbol-function 'lsp-resolve-final-command)
            (lambda (command _test) command))
           ((symbol-function 'remote-fs-file-name-p) (lambda (_path) t))
           ((symbol-function 'my/language-server-executable-find)
            (lambda (program)
              (setq seen-context default-directory)
              (and (equal program "emacs-lsp-booster") (cdr case)))))
        (should
         (equal
          (my/lsp-mode--resolve-logical-command-a
           #'lsp-resolve-final-command
           '("clangd" "--foo"))
          (list (cdr case) "--json-false-value" ":json-false" "--"
                "clangd" "--foo"))))
      (should (equal seen-context default-directory)))))

(ert-deftest language-server-booster-errors-when-required-and-missing ()
  (let ((my/language-server-booster-required t))
    (cl-letf
        (((symbol-function 'lsp-resolve-final-command)
          (lambda (command _test) command))
         ((symbol-function 'my/language-server-executable-find)
          (lambda (_program) nil)))
      (should-error
       (my/lsp-mode--resolve-logical-command-a
        #'lsp-resolve-final-command '("clangd"))))))

(ert-deftest language-server-booster-optional-falls-back-silently ()
  (let ((my/language-server-booster-required nil))
    (cl-letf
        (((symbol-function 'lsp-resolve-final-command)
          (lambda (command _test) command))
         ((symbol-function 'my/language-server-executable-find)
          (lambda (_program) nil)))
      (should
       (equal
        (my/lsp-mode--resolve-logical-command-a
         #'lsp-resolve-final-command '("clangd"))
        '("clangd"))))))

(ert-deftest language-server-executable-find-advice-is-scoped-to-adapter ()
  "Stock `lsp-clients-*' definitions call bare `executable-find'.  Inside
the language-server adapter's dynamic extent that must resolve through the
target; outside it, ordinary client-side lookups must be untouched."
  (cl-letf
      (((symbol-function 'remote-executable-find)
        (lambda (program &optional _context)
          (and (equal program "clangd") "/target/bin/clangd"))))
    (let ((remote-current-adapter-id "language-server"))
      (should
       (equal
        (my/language-server--executable-find-a
         (lambda (_program &optional _remote) nil)
         "clangd" nil)
        "/target/bin/clangd")))
    (let ((remote-current-adapter-id "exec"))
      (should-not
       (my/language-server--executable-find-a
        (lambda (_program &optional _remote) nil)
        "clangd" nil)))))

(ert-deftest language-server-executable-find-guard-prevents-remote-recursion ()
  "A local Remote provider may itself use `executable-find'."
  (let ((remote-current-adapter-id "language-server")
        (native-calls 0))
    (cl-labels ((native-find (_command &optional _remote)
                  (cl-incf native-calls)
                  "/native/bin/clangd"))
      (cl-letf (((symbol-function 'remote-executable-find)
                 (lambda (command &optional _context)
                   (my/language-server--executable-find-a
                    #'native-find command))))
        (should
         (equal
          (my/language-server--executable-find-a
           #'native-find "clangd")
          "/native/bin/clangd"))))
    (should (= native-calls 1))))

(ert-deftest language-server-start-reuses-executable-probes-per-context ()
  "Preflight and connection share results, including missing executables."
  (with-temp-buffer
    (let ((default-directory "/fs:box:/work/")
          (remote-current-adapter-id "language-server")
          (remote-current-workspace 'workspace-one)
          (remote-buffer-environment 'environment-one)
          (my/language-server-runtime-current nil)
          (my/language-server--lookup-cache-active t)
          (my/language-server--start-executable-cache
           (make-hash-table :test #'equal))
          (lookups 0))
      (cl-letf (((symbol-function 'remote-executable-find)
                 (lambda (program &optional _context)
                   (cl-incf lookups)
                   (and (equal program "clangd") "/target/bin/clangd"))))
        (should (equal (my/language-server-executable-find "clangd")
                       "/target/bin/clangd"))
        (should (equal (my/language-server--executable-find-a
                        #'ignore "clangd")
                       "/target/bin/clangd"))
        (should-not (my/language-server-executable-find "missing"))
        (should-not (my/language-server-executable-find "missing"))
        (should (= lookups 2))
        (setq remote-buffer-environment 'environment-two)
        (should (my/language-server-executable-find "clangd"))
        (should (= lookups 3))
        (setq remote-current-workspace 'workspace-two)
        (should (my/language-server-executable-find "clangd"))
        (should (= lookups 4))
        (let ((my/language-server--lookup-cache-active nil))
          (should (my/language-server-executable-find "clangd")))
        (should (= lookups 5))))))

(ert-deftest language-server-connection-clears-start-executable-cache ()
  "A failed connection must not leave executable results for a retry."
  (with-temp-buffer
    (let ((my/language-server--start-executable-cache
           (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'my/language-server--project-root-for-buffer)
                 (lambda () "/fs:box:/work/"))
                ((symbol-function 'my/language-server--connect-workspace)
                 (lambda (_root) 'workspace))
                ((symbol-function 'remote-workspace-context)
                 (lambda (_workspace) 'context))
                ((symbol-function 'remote-environment-ensure)
                 #'ignore))
        (should-error
         (my/lsp-mode--connect-via-remote-a
          (lambda () (error "connection failed"))))
        (should-not my/language-server--start-executable-cache)))))

(ert-deftest language-server-deep-configuration-merge-preserves-explicit-false ()
  (let* ((analysis
          (my/lsp-test-object
           :autoSearchPaths t
           :diagnosticMode "workspace"
           :nested (my/lsp-test-object :enabled t :level "strict")))
         (base (my/lsp-test-object :analysis analysis))
         (override
          '(:analysis
            (:diagnosticMode nil
             :nested (:enabled :json-false))))
         (merged (my/language-server--merge-values base override))
         (merged-analysis
          (my/language-server--mapping-ref merged :analysis 'missing))
         (nested
          (my/language-server--mapping-ref merged-analysis :nested 'missing)))
    (should (eq (my/language-server--mapping-ref
                 merged-analysis :autoSearchPaths 'missing)
                t))
    (should-not (my/language-server--mapping-ref
                 merged-analysis :diagnosticMode 'missing))
    (should (eq (my/language-server--mapping-ref nested :enabled 'missing)
                :json-false))
    (should (equal (my/language-server--mapping-ref nested :level 'missing)
                   "strict"))))

(ert-deftest language-server-workspace-configuration-is-section-and-owner-scoped ()
  (let* ((params
          (my/lsp-test-object
           :items
           (vector
            (my/lsp-test-object :section "python.analysis")
            (my/lsp-test-object :section "pylsp.plugins"))))
         (first
          (my/lsp-test-object
           :autoSearchPaths t :diagnosticMode "workspace"))
         (second '(:jedi (:enabled t :fuzzy nil)))
         (response (vector first second))
         (override
          '(:python (:analysis (:diagnosticMode nil))
            :pylsp (:plugins (:jedi (:enabled :json-false))))))
    (let ((lsp--cur-workspace 'owning-workspace))
      (cl-letf
          (((symbol-function
             'my/language-server--workspace-configuration-override)
            (lambda (workspace)
              (should (eq workspace 'owning-workspace))
              override)))
        (let* ((merged
                (my/language-server--workspace-configuration-response-a
                 (lambda (_params) response) params))
               (analysis (aref merged 0))
               (plugins (aref merged 1))
               (jedi (my/language-server--mapping-ref
                      plugins :jedi 'missing)))
          (should (eq (my/language-server--mapping-ref
                       analysis :autoSearchPaths 'missing)
                      t))
          (should-not (my/language-server--mapping-ref
                       analysis :diagnosticMode 'missing))
          (should (eq (my/language-server--mapping-ref
                       jedi :enabled 'missing)
                      :json-false))
          (should-not (my/language-server--mapping-ref
                       jedi :fuzzy 'missing)))))))

(ert-deftest language-server-reconfigure-does-not-flood-lsp-notifications ()
  (should-not (memq #'my/language-server--push-workspace-configuration-h
                    lsp-configure-hook))
  (let ((my/language-server--pushed-workspace-configurations
         (make-hash-table :test #'eq))
        (workspace 'first-generation)
        notifications)
    (with-temp-buffer
      (setq-local my/language-server--workspace-configuration
                  '(:python (:analysis (:diagnosticMode "openFilesOnly"))))
      (cl-letf (((symbol-function 'lsp-workspaces)
                 (lambda () (list workspace)))
                ((symbol-function 'lsp--set-configuration)
                 (lambda (configuration)
                   (push configuration notifications)
                   ;; A server registration can synchronously reconfigure the
                   ;; buffer while the notification is still being sent.
                   (my/language-server--push-workspace-configuration-h))))
        (my/language-server--push-workspace-configuration-h)
        (dotimes (_ 20)
          (my/language-server--push-workspace-configuration-h))
        (should (= (length notifications) 1))
        (setq-local my/language-server--workspace-configuration
                    '(:python (:analysis (:diagnosticMode "workspace"))))
        (my/language-server--push-workspace-configuration-h)
        (should (= (length notifications) 2))
        (setq workspace 'restarted-generation)
        (my/language-server--push-workspace-configuration-h)
        (should (= (length notifications) 3))))))

(ert-deftest language-server-custom-activation-cannot-leak-across-modes ()
  (let (client)
    (cl-letf (((symbol-function 'lsp-stdio-connection) #'identity)
              ((symbol-function 'make-lsp-client) (lambda (&rest args) args))
              ((symbol-function 'lsp-register-client)
               (lambda (value) (setq client value))))
      (my/register-language-server
       '(latex-mode) '("texlab")
       :server-id 'test-latex
       :activation-fn (lambda (&rest _) t)))
    (let ((activation (plist-get client :activation-fn)))
      (with-temp-buffer
        (setq major-mode 'c-mode)
        (should-not (funcall activation "file.c" 'c-mode)))
      (with-temp-buffer
        (setq major-mode 'latex-mode)
        (should (funcall activation "file.tex" 'latex-mode))))))

(ert-deftest language-server-announces-configuration-after-initialized ()
  "Every registered server is told its settings, as VS Code and Eglot do.
Pyright with workspace folders never pulls settings on its own and so never
analyzes anything without this notification."
  (let (client sent order)
    (cl-letf (((symbol-function 'lsp-stdio-connection) #'identity)
              ((symbol-function 'make-lsp-client) (lambda (&rest args) args))
              ((symbol-function 'lsp-register-client)
               (lambda (value) (setq client value)))
              ((symbol-function 'lsp--set-configuration)
               (lambda (settings)
                 (push (list lsp--cur-workspace settings) sent)
                 (push 'announce order))))
      (my/register-language-server
       '(python-mode) '("pyright-langserver" "--stdio")
       :server-id 'test-announce
       :initialized-fn (lambda (_workspace) (push 'custom order)))
      (let ((initialized (plist-get client :initialized-fn)))
        ;; Without an override the server still gets an (empty) object.
        (cl-letf (((symbol-function
                    'my/language-server--workspace-configuration-override)
                   (lambda (_) nil)))
          (funcall initialized 'workspace-a))
        (should (equal (caar sent) 'workspace-a))
        (should (hash-table-p (cadar sent)))
        (should (hash-table-empty-p (cadar sent)))
        ;; The caller's own hook runs after the announcement.
        (should (equal (reverse order) '(announce custom)))
        ;; With an override the workspace's own settings are sent and recorded,
        ;; so the dedupe in the push path does not resend them.
        (cl-letf (((symbol-function
                    'my/language-server--workspace-configuration-override)
                   (lambda (_) '(:python (:pythonPath "/venv/bin/python")))))
          (funcall initialized 'workspace-b))
        (should (equal (car sent)
                       '(workspace-b (:python (:pythonPath "/venv/bin/python")))))
        (should (equal (gethash 'workspace-b
                                my/language-server--pushed-workspace-configurations)
                       '(:python (:pythonPath "/venv/bin/python"))))))))

(ert-deftest language-server-logical-support-check-restores-client-slot ()
  (require 'lsp-java)
  (dolist (server-id '(my-clangd my-python jdtls))
    (let* ((client (gethash server-id lsp-clients))
           (original (lsp--client-remote? client)))
      (should client)
      (with-temp-buffer
        (setq buffer-file-name "/fs:box:/work/project/source")
        (should
         (my/lsp-mode--supports-logical-buffer-a
          (lambda (candidate)
            (lsp--client-remote? candidate))
          client)))
      (with-temp-buffer
        (setq buffer-file-name "/ssh:box:/work/project/source")
        (should
         (my/lsp-mode--supports-logical-buffer-a
          (lambda (candidate)
            (lsp--client-remote? candidate))
          client)))
      (should (eq (lsp--client-remote? client) original)))))

(ert-deftest language-server-stdio-connect-projects-tramp-root-through-remote ()
  (let* ((workspace
          (make-lsp--workspace :root "/ssh:box:/work/project/"))
         (owner
          (remote-workspace-create
           :id "box/work" :target-id "box"
           :root "/fs:box:/work/project/" :context 'remote-context))
         (connection
          (my/lsp-mode--stdio-connect-via-remote-a
           (list
            :connect
            (lambda (_filter _sentinel _name _environment-fn _workspace)
              (list
               default-directory
               lsp-use-workspace-root-for-server-default-directory
               remote-current-adapter-id
               remote-current-workspace)))))
         applied-context)
    (cl-letf (((symbol-function 'my/language-server--connect-workspace)
               (lambda (root)
                 (should (equal root "/fs:box:/work/project/"))
                 owner))
              ((symbol-function 'remote-environment-ensure)
               (lambda (context &rest _)
                 (setq applied-context context))))
      (should
       (equal
        (funcall (plist-get connection :connect)
                 #'ignore #'ignore "clangd" nil workspace)
        (list "/fs:box:/work/project/" nil "language-server" owner))))
    (should (eq applied-context 'remote-context))))

(ert-deftest language-server-core-languages-select-one-authoritative-client ()
  (with-temp-buffer
    (setq major-mode 'c-mode)
    (my/cpp-language-server-setup-h)
    (should (equal lsp-enabled-clients '(my-clangd))))
  (with-temp-buffer
    (setq major-mode 'python-mode)
    (cl-letf (((symbol-function
                'my/language-server-set-workspace-configuration)
               #'ignore))
      (my/python-language-server-setup-h))
    (should (equal lsp-enabled-clients '(my-python))))
  (should-not lsp-auto-register-remote-clients)
  (should (memq #'my/language-server-ensure-deferred prog-mode-hook)))

(ert-deftest language-server-missing-binary-is-quiet-only-for-auto-start ()
  (with-temp-buffer
    (setq major-mode 'python-mode)
    (let (messages started)
      (cl-letf (((symbol-function 'my/lsp-mode-supported-p) (lambda () t))
                ((symbol-function 'my/language-server-apply-process-environment)
                 #'ignore)
                ((symbol-function 'my/language-server-apply-lsp-local-settings)
                 #'ignore)
                ((symbol-function
                  'my/language-server-runtime-register-lsp-configuration)
                 #'ignore)
                ((symbol-function 'my/language-server-contact-available-p)
                 (lambda () nil))
                ((symbol-function 'lsp-deferred)
                 (lambda () (setq started t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format format-string arguments) messages))))
        (setq my/language-server--manual-start nil)
        (my/lsp-mode-start-now)
        (should-not messages)
        (setq my/language-server--manual-start t)
        (my/lsp-mode-start-now)
        (should (= (length messages) 1))
        (should-not started)))))

(ert-deftest language-server-coalesces-duplicate-deferred-starts ()
  "Repeated ensure hooks must not reapply a remote environment during startup."
  (with-temp-buffer
    (let ((my/lsp-mode--start-request nil)
          (my/language-server--manual-start nil)
          (remote-buffer-environment 'first-environment)
          (my/language-server-runtime-current 'first-runtime)
          (lsp-managed-mode nil)
          (applied 0)
          (started 0))
      (cl-letf (((symbol-function 'my/language-server-preferred-backend)
                 (lambda () 'lsp-mode))
                ((symbol-function 'my/direnv-update-environment-maybe)
                 (lambda (&rest _) nil))
                ((symbol-function 'my/lsp-mode-supported-p)
                 (lambda () t))
                ((symbol-function 'my/language-server-apply-process-environment)
                 (lambda () (cl-incf applied)))
                ((symbol-function 'my/language-server-apply-lsp-local-settings)
                 #'ignore)
                ((symbol-function
                  'my/language-server-runtime-register-lsp-configuration)
                 #'ignore)
                ((symbol-function 'my/language-server-contact-available-p)
                 (lambda () t))
                ((symbol-function 'my/language-server--load-client-ui)
                 #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda () (cl-incf started))))
        (my/lsp-mode-ensure)
        (my/lsp-mode-ensure)
        (should (= applied 1))
        (should (= started 1))
        (setq my/language-server--manual-start t)
        (my/lsp-mode-ensure)
        (should (= applied 2))
        (setq remote-buffer-environment 'second-environment)
        (my/lsp-mode-ensure)
        (should (= applied 3))
        (my/lsp-mode--clear-start-request-on-detach)
        (my/lsp-mode-ensure)
        (should (= applied 4))
        (should (= started 4))
        (setcar my/lsp-mode--start-request
                (- (float-time)
                   (1+ my/lsp-mode-start-request-coalesce-seconds)))
        (should-not (my/lsp-mode--start-request-active-p))))))

(ert-deftest language-server-failed-deferred-start-allows-retry ()
  (with-temp-buffer
    (let ((my/lsp-mode--start-request nil)
          (my/language-server--manual-start nil))
      (cl-letf (((symbol-function 'my/lsp-mode-supported-p)
                 (lambda () t))
                ((symbol-function 'my/language-server-apply-process-environment)
                 #'ignore)
                ((symbol-function 'my/language-server-apply-lsp-local-settings)
                 #'ignore)
                ((symbol-function
                  'my/language-server-runtime-register-lsp-configuration)
                 #'ignore)
                ((symbol-function 'my/language-server-contact-available-p)
                 (lambda () t))
                ((symbol-function 'my/language-server--load-client-ui)
                 #'ignore)
                ((symbol-function 'lsp-deferred)
                 (lambda () (error "start failed"))))
        (should-error (my/lsp-mode-start-now))
        (should-not my/lsp-mode--start-request)))))

(ert-deftest language-server-active-request-skips-runtime-reprepare ()
  (with-temp-buffer
    (let ((my/language-server-runtime-state 'unsupported)
          (my/language-server-runtime-current 'runtime)
          (remote-buffer-environment 'environment)
          (my/language-server--manual-start nil)
          (my/language-server--waiting-for-runtime nil)
          (prepared 0))
      (setq my/lsp-mode--start-request
            (list (float-time) 'runtime 'environment))
      (cl-letf (((symbol-function 'my/language-server-runtime-prepare)
                 (lambda (&rest _) (cl-incf prepared))))
        (my/language-server-ensure)
        (should (= prepared 0))
        (setq remote-buffer-environment 'changed-environment)
        (my/language-server-ensure)
        (should (= prepared 1))))))

(ert-deftest language-server-managed-folder-prewarm-is-idle-and-coalesced ()
  "Browsing an SSH workspace preloads the client only after one idle timer."
  (let ((my/language-server-folder-prewarm-idle-seconds 0.5)
        (my/language-server--folder-prewarm-timer nil)
        (my/language-server--folder-prewarmed nil)
        (owner (remote-workspace-create :target-id "test" :state 'open))
        (scheduled 0)
        (loaded nil)
        callback)
    (cl-letf (((symbol-function 'remote-workspace-for-path)
               (lambda (_path) owner))
              ((symbol-function 'run-with-idle-timer)
               (lambda (_seconds _repeat function)
                 (cl-incf scheduled)
                 (setq callback function)
                 'prewarm-timer))
              ((symbol-function 'my/language-server--require-on-client)
               (lambda (feature)
                 (push feature loaded)
                 t))
              ((symbol-function 'my/language-server--load-client-ui)
               (lambda () (push 'ui loaded))))
      (with-temp-buffer
        (setq default-directory "/tmp/")
        (my/language-server--folder-prewarm-schedule)
        (should (= scheduled 0))
        (setq default-directory "/fs:test:/tmp/")
        (my/language-server--folder-prewarm-schedule)
        (my/language-server--folder-prewarm-schedule)
        (should (= scheduled 1))
        (should-not loaded)
        (funcall callback)
        (should (equal loaded '(ui lsp-mode)))
        (should my/language-server--folder-prewarmed)
        (my/language-server--folder-prewarm-schedule)
        (should (= scheduled 1))))))

(ert-deftest language-server-local-folder-does-not-prewarm-client ()
  "A logical local Dired buffer keeps the native local resource policy."
  (let ((my/language-server-folder-prewarm-idle-seconds 0.5)
        (my/language-server--folder-prewarm-timer nil)
        (my/language-server--folder-prewarmed nil)
        (owner (remote-workspace-create :target-id "local" :state 'open))
        (scheduled 0))
    (cl-letf (((symbol-function 'remote-workspace-for-path)
               (lambda (_path) owner))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (cl-incf scheduled))))
      (with-temp-buffer
        (setq default-directory "/fs:local:/tmp/")
        (my/language-server--folder-prewarm-schedule)
        (should (= scheduled 0))))))

(ert-deftest language-server-folder-prewarm-failure-stays-retryable ()
  "A failed idle package load must not disable the later normal startup."
  (let ((my/language-server--folder-prewarm-timer 'pending)
        (my/language-server--folder-prewarmed nil)
        (failed t)
        (loaded 0))
    (cl-letf (((symbol-function 'my/language-server--require-on-client)
               (lambda (_feature)
                 (when failed (error "injected package load failure"))
                 t))
              ((symbol-function 'my/language-server--load-client-ui)
               (lambda () (cl-incf loaded)))
              ((symbol-function 'remote-log) #'ignore))
      (my/language-server--folder-prewarm-run)
      (should-not my/language-server--folder-prewarm-timer)
      (should-not my/language-server--folder-prewarmed)
      (setq failed nil)
      (my/language-server--folder-prewarm-run)
      (should my/language-server--folder-prewarmed)
      (should (= loaded 1)))))

(provide 'init-lsp-remote-tests)
;;; init-lsp-remote-tests.el ends here

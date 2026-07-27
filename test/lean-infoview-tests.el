;;; lean-infoview-tests.el --- Lean Infoview lifecycle tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'init-lean)
(require 'init-lean-infoview)

(ert-deftest lean-infoview-proxy-port-files-are-instance-isolated ()
  (let ((lean--proxy-port-files (make-hash-table :test #'equal))
        (lean--proxy-port-sequence 0)
        (root (file-name-as-directory user-emacs-directory)))
    (let ((first (lean--proxy-allocate-port-file root))
          (second (lean--proxy-allocate-port-file root)))
      (should-not (equal first second))
      (should (equal (lean--proxy-port-file root) second)))))

(ert-deftest lean-infoview-project-root-shares-eglot-proxy-identity ()
  (let* ((directory (make-temp-file "lean-infoview-root-" t))
         (default-directory (file-name-as-directory directory))
         (buffer-file-name (expand-file-name "Main.lean" directory))
         (toolchain (expand-file-name "lean-toolchain" directory))
         (lean--proxy-port-files (make-hash-table :test #'equal))
         (lean--proxy-port-sequence 0))
    (unwind-protect
        (progn
          (with-temp-file toolchain
            (insert "leanprover/lean4:stable\n"))
          (let* ((eglot-root (lean-project-root))
                 (port-file (lean--proxy-allocate-port-file eglot-root))
                 (infoview-root (lean--iv-project-root)))
            (should (equal infoview-root eglot-root))
            (should (lean--proxy-port-file-allocated-p infoview-root))
            (should (equal (lean--proxy-port-file infoview-root)
                           port-file))))
      (delete-directory directory t))))

(ert-deftest lean-infoview-remote-cache-keeps-logical-target-identity ()
  "Target HOME expansion must not turn a remote cache into a client path."
  (let (seen)
    (cl-letf (((symbol-function 'lean--proxy-bundle-fingerprint)
               (lambda () "0123456789abcdefdeadbeef"))
              ((symbol-function 'remote-file-name-target)
               (lambda (root)
                 (should (equal root "/fs:box:/work/project/"))
                 "box"))
              ((symbol-function 'remote-expand-file-name)
               (lambda (name directory target)
                 (setq seen (list name directory target))
                 "/fs:box:/home/me/.cache/emacs/lean-infoview/0123456789abcdef/")))
      (should
       (equal
        (lean--remote-proxy-directory "/fs:box:/work/project/")
        "/fs:box:/home/me/.cache/emacs/lean-infoview/0123456789abcdef/"))
      (should
       (equal seen
              '("~/.cache/emacs/lean-infoview/0123456789abcdef/"
                nil "box"))))))

(ert-deftest lean-infoview-node-always-resolves-on-the-client ()
  (let ((remote--buffer-base-process-environment '("PATH=/client/bin"))
        (remote--buffer-base-exec-path '("/client/bin"))
        seen-environment
        seen-exec-path)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program &optional _remote)
                 (should (equal program "node"))
                 (setq seen-environment process-environment
                       seen-exec-path exec-path)
                 "/client/bin/node")))
      (should
       (equal (lean--proxy-node-command "/fs:box:/work/")
              '("/client/bin/node")))
      (should (equal seen-environment '("PATH=/client/bin")))
      (should (equal seen-exec-path '("/client/bin"))))))

(ert-deftest lean-infoview-remote-contact-is-local-proxy-over-stdio-bridge ()
  (let ((remote-targets (copy-hash-table remote-targets))
        bridge-arguments)
    (remote-register-target "box" :trusted t)
    (remote-register-link "box" "native" "native")
    (cl-letf (((symbol-function 'lean-project-root)
               (lambda () "/fs:box:/work/"))
              ((symbol-function 'lean-root-dir-p)
               (lambda (_directory) t))
              ((symbol-function 'locate-dominating-file)
               (lambda (_file _predicate) "/fs:box:/work/"))
              ((symbol-function 'lean--proxy-available-p) (lambda () t))
              ((symbol-function 'lean--proxy-script-for-root)
               (lambda (_root) "/client/lean-proxy.mjs"))
              ((symbol-function 'lean--proxy-node-command)
               (lambda (_root) '("/client/node")))
              ((symbol-function 'lean--proxy-allocate-port-file)
               (lambda (_root) "/client/infoview.json"))
              ((symbol-function 'remote-file-name-target)
               (lambda (_root) "box"))
              ((symbol-function 'remote-client-file-name)
               (lambda (_root &optional _adapter) nil))
              ((symbol-function 'remote-local-bridge-command)
               (lambda (program &rest keys)
                 (setq bridge-arguments (cons program keys))
                 '("/usr/bin/ssh" "-T" "box" "remote-lake-command"))))
      (let ((default-directory "/fs:box:/work/"))
        (let* ((contact (lean--server-contact))
               (factory (plist-get (cdr contact) :process))
               process-arguments)
          (should (eq (car contact) 'eglot-lsp-server))
          (should (functionp factory))
          (cl-letf (((symbol-function 'remote-make-client-process)
                     (lambda (&rest plist)
                       (setq process-arguments plist)
                       'local-proxy-process)))
            (should (eq (funcall factory) 'local-proxy-process)))
          (should
           (equal
            (plist-get process-arguments :command)
            (list
             "/client/node" "/client/lean-proxy.mjs"
             "--root" temporary-file-directory
             "--port-file" "/client/infoview.json"
             "--" "/usr/bin/ssh" "-T" "box" "remote-lake-command")))
          (should
           (equal (plist-get process-arguments :connection-type) 'pipe)))
        (should
         (equal
          bridge-arguments
          '("lake" :args ("serve") :context "/fs:box:/work/"
            :adapter "language-server" :directory "/fs:box:/work/")))))))

(ert-deftest lean-infoview-manual-start-cancels-automatic-timer ()
  (with-temp-buffer
    (let ((buffer-file-name
           (expand-file-name "Main.lean" temporary-file-directory))
          (default-directory temporary-file-directory)
          (lean--eglot-waiting-for-environment nil)
          (lean-eglot-connect-timeout nil)
          (starts 0))
      (setq lean--eglot-start-timer
            (run-at-time 60 nil #'ignore))
      (unwind-protect
          (cl-letf (((symbol-function 'eglot-managed-p)
                     (lambda () nil))
                    ((symbol-function 'my/direnv-update-environment-maybe)
                     (lambda (&optional _path _callback) nil))
                    ((symbol-function 'my/eglot-start-now)
                     (lambda (&optional _interactive)
                       (cl-incf starts))))
            (lean--ensure-eglot)
            (should (= starts 1))
            (should-not lean--eglot-start-timer))
        (lean--cancel-eglot-start-timer)))))

(ert-deftest lean-eglot-recovers-a-stale-direnv-wait-latch ()
  (with-temp-buffer
    (let ((buffer-file-name
           (expand-file-name "Main.lean" temporary-file-directory))
          (default-directory temporary-file-directory)
          (direnv--active-root "/fs:box:/work/")
          (lean--eglot-waiting-for-environment t)
          (starts 0))
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
                ((symbol-function 'my/direnv-update-environment-maybe)
                 (lambda (&optional _path _callback) 'ready))
                ((symbol-function 'my/eglot-start-now)
                 (lambda (&optional _interactive) (cl-incf starts))))
        (lean--ensure-eglot)
        (should-not lean--eglot-waiting-for-environment)
        (should (= starts 1))))))

(ert-deftest lean-eglot-direnv-failure-falls-back-to-target-base-environment ()
  (with-temp-buffer
    (let ((lean--eglot-waiting-for-environment t)
          seen)
      (cl-letf (((symbol-function 'my/eglot-start-now)
                 (lambda (&optional interactive)
                   (setq seen interactive)))
                ((symbol-function 'message) #'ignore))
        (lean--eglot-direnv-ready
         nil '(remote-file-error "direnv unavailable"))
        (should-not lean--eglot-waiting-for-environment)
        (should seen)))))

(ert-deftest lean-infoview-rejects-a-dead-local-port-owner ()
  (let ((port-file
         (make-temp-file "lean-infoview-dead-owner-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file port-file
            (insert
             (json-encode
              '(:port 65511 :pid 2147483000))))
          (should-not (lean--iv-read-port port-file)))
      (when (file-exists-p port-file)
        (delete-file port-file)))))

(ert-deftest lean-infoview-accepts-a-live-local-port-owner ()
  (let ((port-file
         (make-temp-file "lean-infoview-live-owner-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file port-file
            (insert
             (json-encode
              `(:port 43123 :pid ,(emacs-pid)))))
          (should (= (lean--iv-read-port port-file) 43123)))
      (when (file-exists-p port-file)
        (delete-file port-file)))))

(ert-deftest lean-infoview-checks-local-owner-outside-remote-process-namespace ()
  (let ((default-directory "/fs:box:/work/")
        seen-directory)
    (cl-letf (((symbol-function 'process-attributes)
               (lambda (pid)
                 (should (= pid 43123))
                 (setq seen-directory default-directory)
                 '((comm . "node")))))
      (should
       (lean--iv-port-owner-live-p
        "/client/infoview.json" '(:pid 43123)))
      (should
       (equal seen-directory
              (file-name-as-directory temporary-file-directory))))))

(ert-deftest lean-infoview-xwidget-requires-a-graphical-frame ()
  (cl-letf (((symbol-function 'display-graphic-p)
             (lambda (&optional _frame) nil)))
    (should-not (lean-iv-xwidget-p))))

(ert-deftest lean-infoview-port-wait-follows-a-new-eglot-instance ()
  (let* ((pending (make-temp-name
                   (expand-file-name "lean-infoview-pending-" temporary-file-directory)))
         (active (make-temp-file "lean-infoview-active-" nil ".json"))
         (current pending)
         (lean-iv-port-wait-timeout 2)
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'lean--proxy-port-file)
                   (lambda (_root) current)))
          (lean--iv-wait-for-port
           user-emacs-directory
           (lambda (port) (setq result port)))
          (with-temp-file active
            (insert
             (json-encode
              `(:port 43124 :pid ,(emacs-pid)))))
          (setq current active)
          (let ((deadline (+ (float-time) 1.5)))
            (while (and (not result)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (= result 43124))
          (should-not lean--iv--port-wait-timer))
      (when (file-exists-p active)
        (delete-file active)))))

(ert-deftest lean-infoview-repeated-open-replaces-the-port-wait ()
  (with-temp-buffer
    (let ((lean-iv-port-wait-timeout 60)
          first second)
      (unwind-protect
          (cl-letf (((symbol-function 'lean--proxy-port-file)
                     (lambda (_root) "/nonexistent/infoview.json"))
                    ((symbol-function 'lean--iv-read-port)
                     (lambda (_file) nil)))
            (lean--iv-wait-for-port user-emacs-directory #'ignore)
            (setq first lean--iv--port-wait-timer)
            (should (timerp first))
            (lean--iv-wait-for-port user-emacs-directory #'ignore)
            (setq second lean--iv--port-wait-timer)
            (should (timerp second))
            (should-not (eq first second))
            (should-not (memq first timer-list)))
        (lean--iv-cancel-port-wait)))))

(ert-deftest lean-infoview-reconnect-uses-eglots-interactive-server-lookup ()
  (let (seen)
    (cl-letf (((symbol-function 'eglot-reconnect)
               (lambda (server &optional interactive)
                 (interactive (list 'current-server t))
                 (setq seen (list server interactive)))))
      (lean--iv-reconnect-eglot)
      (should (equal seen '(current-server t))))))

(provide 'lean-infoview-tests)
;;; lean-infoview-tests.el ends here

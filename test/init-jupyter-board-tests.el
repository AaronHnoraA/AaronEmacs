;;; init-jupyter-board-tests.el --- Jupyter Board tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-jupyter-board)

(ert-deftest my/jupyter-connection-maintenance-runs-on-owner-and-is-throttled ()
  (let ((my/jupyter-management--cleanup-times (make-hash-table :test #'equal))
        (launched 0) (completed 0))
    (cl-letf (((symbol-function 'my/jupyter-management-context) #'identity)
              ((symbol-function 'remote-exec-async)
               (lambda (program &rest args)
                 (cl-incf launched)
                 (should (equal program "python3"))
                 (should (equal (plist-get args :context) "lab"))
                 (should (equal (last (plist-get args :args) 3)
                                '("--runtime-dir" "/run/jupyter" "--apply")))
                 (funcall (plist-get args :callback) nil))))
      (dotimes (_ 2)
        (my/jupyter-management--prune-directory "lab" "/run/jupyter" (lambda () (cl-incf completed))))
      (should (= launched 1))
      (should (= completed 2)))))

(ert-deftest my/jupyter-board-detail-edits-original-json-and-rejects-conflicts ()
  (let* ((directory (make-temp-file "jupyter-detail-" t))
         (file (expand-file-name "kernel.json" directory))
         (my/jupyter-board-detail-buffer-name " *jupyter-detail-test*")
         (my/jupyter-board-buffer-name " *jupyter-no-board*")
         (my/noema--ready nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"argv\":[\"python\"],\"display_name\":\"Original\"}\n"))
          (cl-letf (((symbol-function 'my/jupyter-board--require-entry)
                     (lambda () (list :kind 'kernelspec :resource-dir directory :display-name "Original")))
                    ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
            (my/jupyter-board-describe))
          (with-current-buffer my/jupyter-board-detail-buffer-name
            (goto-char (point-min))
            (should-error (insert "changed header") :type 'text-read-only)
            (goto-char my/jupyter-detail--json-start)
            (search-forward "Original")
            (replace-match "Updated")
            (my/jupyter-detail-save)
            (should-not (buffer-modified-p))
            (should (equal (gethash "display_name" (json-parse-string
                                                     (with-temp-buffer (insert-file-contents file) (buffer-string))))
                           "Updated"))
            (goto-char my/jupyter-detail--json-start)
            (insert "invalid")
            (should-error (my/jupyter-detail-save) :type 'user-error)
            (delete-region my/jupyter-detail--json-start (+ 7 my/jupyter-detail--json-start))
            (with-temp-file file (insert "{\"argv\":[\"python\"],\"display_name\":\"External\"}\n"))
            (should-error (my/jupyter-detail-save) :type 'user-error)))
      (when-let* ((buffer (get-buffer my/jupyter-board-detail-buffer-name))) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest my/jupyter-repl-monads-preserve-lexical-state ()
  (require 'jupyter-repl)
  (should (= 7 (jupyter-run-with-state '(42)
                 (jupyter-bind (jupyter-push 7) (lambda (_) (jupyter-pop)))))))

(ert-deftest my/jupyter-detail-save-enables-project-metadata-for-legacy-launcher ()
  (let* ((directory (make-temp-file "jupyter-project-json-" t))
         (file (expand-file-name "kernel.json" directory))
         (my/noema--ready nil)
         (my/jupyter-board-buffer-name " *no-test-board*"))
    (unwind-protect
        (with-temp-buffer
          (my/jupyter-kernel-detail-mode)
          (setq my/jupyter-detail--file file
                my/jupyter-detail--original "{}")
          (with-temp-file file (insert "{}"))
          (setq my/jupyter-detail--json-start (copy-marker (point)))
          (insert "{\"argv\":[\"python\",\"-m\",\"remote_ikernel\",\"{connection_file}\"],\"display_name\":\"Audit\",\"metadata\":{\"aaron\":{\"project\":{\"root\":\"/work\"}}}}")
          (setq my/jupyter-detail--json-end (copy-marker (point) t))
          (my/jupyter-detail-save)
          (let ((argv (gethash "argv" (json-parse-string (buffer-string)))))
            (should (equal (last (append argv nil) 2) (list "--project-file" file))))
          (should (equal (buffer-string) (my/jupyter-detail--read-file file)))
          (my/jupyter-detail-save)
          (should (= 6 (length (gethash "argv" (json-parse-string (buffer-string)))))))
      (delete-directory directory t))))

(ert-deftest my/jupyter-contents-dired-skips-dirvish-process-probes ()
  (require 'init-dired)
  (cl-letf (((symbol-function 'remote-context) (lambda (_) 'context))
            ((symbol-function 'remote-routes) (lambda (&rest _) nil)))
    (should
     (equal (my/dirvish-file-only-remote-a
             (lambda (&rest _) (ert-fail "Dirvish would probe ls"))
             (lambda (directory &rest _) directory)
             "/fs:jupyter.4c6162:/")
            "/fs:jupyter.4c6162:/"))))

(defun my/jupyter-board-test--object (argv &optional metadata)
  "Return a minimal kernelspec object containing ARGV and METADATA."
  `((resource_dir . ,temporary-file-directory)
    (spec . ((argv . ,argv)
             (display_name . "Test Kernel")
             (language . "python")
             (metadata . ,metadata)))))

(ert-deftest my/jupyter-board-legacy-remote-defaults-to-core ()
  (let ((entry (my/jupyter-board--entry
                "rik_ssh_example_python"
                (my/jupyter-board-test--object
                 '("python" "-m" "remote_ikernel"
                   "--interface" "ssh" "--host" "example"
                   "--kernel_cmd" "python -m ipykernel -f {host_connection_file}"
                   "{connection_file}")))))
    (should (plist-get entry :remote))
    (should (equal (plist-get entry :group) "core"))
    (should (equal (plist-get entry :host) "example"))
    (should (equal (plist-get entry :interface) "ssh"))))

(ert-deftest my/jupyter-board-reads-temporary-metadata ()
  (let* ((metadata '((aaron . ((remote_kernel . ((group . "temporary")))))))
         (entry (my/jupyter-board--entry
                 "rik_ssh_example_python"
                 (my/jupyter-board-test--object
                  '("python" "-m" "remote_ikernel") metadata))))
    (should (equal (plist-get entry :group) "temporary"))))

(ert-deftest my/jupyter-board-normalizes-tunnel-hosts ()
  (should
   (equal
    (my/jupyter-board--remote-args-normalize
     '("--interface=ssh" "--tunnel-hosts=jump-a, jump-b" "--verbose"))
    '("--interface=ssh" "--tunnel-hosts" "jump-a" "jump-b" "--verbose"))))

(ert-deftest my/jupyter-board-parses-local-version ()
  (should
   (equal
    (my/jupyter-board--command-version
     "/usr/bin/printf" "Remote launcher (version 0.4.6+aaron.1).")
    "0.4.6+aaron.1")))

(ert-deftest my/jupyter-board-edit-args-prefer-namespaced-config ()
  (let* ((metadata
          '((aaron .
                   ((remote_kernel .
                                   ((group . "temporary")
                                    (config . ((interface . "ssh")
                                               (name . "Python")
                                               (kernel_cmd . "python -m ipykernel -f {connection_file}")
                                               (host . "example")
                                               (language . "python")
                                               (tunnel_hosts . ("jump"))))))))))
         (entry (my/jupyter-board--entry
                 "rik_ssh_example_python"
                 (my/jupyter-board-test--object
                  '("python" "-m" "remote_ikernel") metadata)))
         (args (my/jupyter-board--transient-config-args entry)))
    (should (member "--interface=ssh" args))
    (should (member "--name=Python" args))
    (should (member "--group=temporary" args))
    (should (member "--host=example" args))
    (should (member "--tunnel-hosts=jump" args))))

(ert-deftest my/jupyter-board-renders-remote-first-management-ui ()
  (let ((entries
         (list
          '(:name "rik_core" :display-name "Core" :language "python"
            :resource-dir "/tmp/core" :remote t :group "core" :interface "ssh")
          '(:name "rik_temp" :display-name "Temp" :language "python"
            :resource-dir "/tmp/temp" :remote t :group "temporary" :interface "ssh")
          '(:name "python3" :display-name "Python" :language "python"
            :resource-dir "/tmp/python" :remote nil))))
    (with-temp-buffer
      (my/jupyter-board-mode)
      (setq my/jupyter-board--target (remote-get-target "local"))
      (cl-letf (((symbol-function 'my/jupyter-management-discover-specs)
                 (lambda (_target callback) (funcall callback entries nil)))
                ((symbol-function 'my/jupyter-management-discover-connections)
                 (lambda (_target callback) (funcall callback nil nil)))
                ((symbol-function 'my/noema-api-call)
                 (lambda (_channel _args callback)
                   (funcall callback '((kernels . nil)) nil))))
        (my/jupyter-board-refresh))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Remote Kernel Manager" text))
        (should (string-match-p "Start Here" text))
        (should (string-match-p "Remote Kernels" text))
        (should (string-match-p "Open REPL" text))
        (should (string-match-p "Make temporary" text))
        (should (string-match-p "Keep profile" text))
        (should (string-match-p "Show kernels, connections & diagnostics" text))
        (should-not (string-match-p "Local / Target Kernels" text))
        (should (string-match-p "Core" text))
        (should (string-match-p "Temp" text))))))

(ert-deftest my/jupyter-board-renders-guided-empty-state ()
  (with-temp-buffer
    (my/jupyter-board-mode)
    (setq my/jupyter-board--target (remote-get-target "local")
          my/jupyter-board--entries nil
          my/jupyter-board--runtimes nil
          my/jupyter-board--connections nil)
    (my/jupyter-board--render)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "1 · Add" text))
      (should (string-match-p "No remote profile is configured yet" text))
      (should (string-match-p "Quick Add SSH" text)))))

(ert-deftest my/jupyter-board-action-buttons-carry-entry-context ()
  (let ((entry '(:id "spec:local:remote:test" :kind kernelspec
                 :name "rik_test" :display-name "Test" :language "python"
                 :remote t :group "core" :interface "ssh" :host "example"
                 :kernel-command "python3 -m ipykernel_launcher -f {host_connection_file}")))
    (with-temp-buffer
      (my/jupyter-board-mode)
      (let ((inhibit-read-only t))
        (my/jupyter-board--insert-entry entry))
      (goto-char (point-min))
      (search-forward "Open REPL")
      (should (equal (plist-get (my/jupyter-board--current-entry) :name)
                     "rik_test")))))

(ert-deftest my/jupyter-board-technical-view-is-explicit ()
  (with-temp-buffer
    (my/jupyter-board-mode)
    (setq my/jupyter-board--target (remote-get-target "local")
          my/jupyter-board--entries
          '((:id "local" :kind kernelspec :name "python3"
             :display-name "Python" :language "python" :resource-dir "/tmp/python")))
    (my/jupyter-board--render)
    (should-not (string-match-p "Local / Target Kernels"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
    (setq my/jupyter-board--show-advanced t)
    (my/jupyter-board--render)
    (should (string-match-p "Local / Target Kernels"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest my/jupyter-board-validates-remote-profile-connectivity-placeholder ()
  (should
   (my/jupyter-board--validate-remote-args
    '("--interface=ssh" "--host=user@example:2222" "--name=Python"
      "--kernel_cmd=python3 -m ipykernel_launcher -f {connection_file}")))
  (should-error
   (my/jupyter-board--validate-remote-args
    '("--interface=ssh" "--host=example" "--name=Python"
      "--kernel_cmd=python3 -m ipykernel_launcher"))
   :type 'user-error)
  (should-error
   (my/jupyter-board--validate-remote-args
    '("--interface=ssh" "--host=user at example" "--name=Python"
      "--kernel_cmd=python3 -m ipykernel_launcher -f {connection_file}"))
   :type 'user-error))

(ert-deftest my/jupyter-board-guided-edit-preserves-advanced-arguments ()
  (let* ((args '("--interface=ssh" "--host=old" "--name=Python"
                 "--kernel_cmd=python -m ipykernel -f {connection_file}"
                 "--tunnel-hosts=jump" "--verbose"))
         (updated (my/jupyter-board--replace-arg args "--host=" "new")))
    (should (member "--host=new" updated))
    (should-not (member "--host=old" updated))
    (should (member "--tunnel-hosts=jump" updated))
    (should (member "--verbose" updated))))

(ert-deftest my/jupyter-board-quick-add-builds-safe-saved-profile ()
  (let (saved-target saved-args)
    (with-temp-buffer
      (my/jupyter-board-mode)
      (setq my/jupyter-board--target (remote-get-target "local"))
      (cl-letf (((symbol-function 'my/jupyter-management-command)
                 (lambda (_target _kind) "/usr/bin/true"))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (cond
                    ((string-prefix-p "SSH host" prompt) "student@cluster:2222")
                    ((string-prefix-p "Profile name" prompt) "Course Python")
                    ((string-prefix-p "Remote working" prompt) "/srv/course")
                    ((string-prefix-p "Remote kernel" prompt)
                     "python3 -m ipykernel_launcher -f {connection_file}")
                    (t (ert-fail (format "Unexpected prompt: %s" prompt))))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "Saved (protected)"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'my/jupyter-board--save-remote-profile)
                 (lambda (target args &optional _origin)
                   (setq saved-target target saved-args args))))
        (my/jupyter-remote-quick-add)))
    (should (equal (my/jupyter-management-target-id saved-target) "local"))
    (should (member "--host=student@cluster:2222" saved-args))
    (should (member "--name=Course Python" saved-args))
    (should (member "--workdir=/srv/course" saved-args))
    (should (member "--group=core" saved-args))
    (should (member
             "--kernel_cmd=python3 -m ipykernel_launcher -f {connection_file}"
             saved-args))))

(ert-deftest my/jupyter-board-course-pytorch-preset-builds-remote-profile ()
  (let (called target args)
    (cl-letf (((symbol-function 'my/jupyter-board--start-target-command)
               (lambda (given-target _kind given-args &optional callback)
                 (setq target given-target args given-args called t)
                 (when callback (funcall callback "Added kernel ['rik_course']."))))
              ((symbol-function 'my/jupyter-board--remote-args-normalize)
               #'identity))
      (my/jupyter-board-add-course-pytorch
       :host "student@cluster:2222"
       :target (remote-get-target "local")))
    (should called)
    (should (equal (my/jupyter-management-target-id target) "local"))
    (should (member "--interface=ssh" args))
    (should (member "--host=student@cluster:2222" args))
    (should (member "--name=Python 3.13 PyTorch CUDA" args))
    (should (member "--workdir=/home/hc/Desktop/9444" args))
    (should (member "--group=core" args))
    (should (member
             "--kernel_cmd=/home/hc/Desktop/9444/.conda/bin/python -m ipykernel_launcher -f {connection_file}"
             args))))

(ert-deftest my/jupyter-board-course-pytorch-preset-requires-host ()
  (should-error (my/jupyter-board-add-course-pytorch :host "") :type 'user-error))

(ert-deftest my/jupyter-board-diagnoses-broken-absolute-launcher ()
  (let* ((entry (my/jupyter-board--entry
                 "broken"
                 (my/jupyter-board-test--object
                  '("/definitely/missing/jupyter-python" "-f" "{connection_file}"))))
         (health (plist-get entry :health)))
    (should (eq (plist-get health :status) 'error))
    (should (string-match-p "not executable" (plist-get health :detail)))))

(ert-deftest my/jupyter-board-validates-connection-payload ()
  (should
   (my/jupyter-management-connection-valid-p
    '((transport . "tcp") (ip . "127.0.0.1") (key . "secret")
      (shell_port . 1) (iopub_port . 2) (stdin_port . 3)
      (control_port . 4) (hb_port . 5))))
  (should-not
   (my/jupyter-management-connection-valid-p
    '((transport . "tcp") (ip . "127.0.0.1") (key . "secret")))))

(ert-deftest my/jupyter-board-repl-associates-the-source-buffer ()
  (require 'jupyter-repl)
  (let ((source (generate-new-buffer " *jupyter-board-source*")) call)
    (unwind-protect
        (cl-letf (((symbol-function 'jupyter-run-repl)
                   (lambda (&rest args)
                     (setq call (cons (current-buffer) args)))))
          (my/jupyter-management-run-repl
           '(:kind kernelspec :target-id "local" :name "python3") source)
          (should (eq (car call) source))
          (should (equal (cdr call) '("python3" nil t nil t))))
      (kill-buffer source))))

(ert-deftest my/jupyter-board-connection-associates-the-source-buffer ()
  (require 'jupyter-repl)
  (let ((source (generate-new-buffer " *jupyter-board-connect-source*")) call)
    (unwind-protect
        (cl-letf (((symbol-function 'jupyter-connect-repl)
                   (lambda (&rest args)
                     (setq call (cons (current-buffer) args)))))
          (my/jupyter-management-connect-repl
           '(:valid t :file "/tmp/kernel.json") source)
          (should (eq (car call) source))
          (should (equal (cdr call) '("/tmp/kernel.json" nil t nil t))))
      (kill-buffer source))))

(ert-deftest my/jupyter-board-discards-stale-refresh-results ()
  (let (spec-callbacks)
    (with-temp-buffer
      (my/jupyter-board-mode)
      (setq my/jupyter-board--target (remote-get-target "local"))
      (cl-letf (((symbol-function 'my/jupyter-management-discover-specs)
                 (lambda (_target callback) (push callback spec-callbacks)))
                ((symbol-function 'my/jupyter-management-discover-connections)
                 (lambda (_target callback) (funcall callback nil nil)))
                ((symbol-function 'my/noema-api-call)
                 (lambda (_channel _args callback)
                   (funcall callback '((kernels . nil)) nil))))
        (my/jupyter-board-refresh)
        (my/jupyter-board-refresh)
        (let ((newest (car spec-callbacks))
              (oldest (cadr spec-callbacks)))
          (funcall oldest '((:name "old" :kind kernelspec)) nil)
          (should-not my/jupyter-board--entries)
          (funcall newest '((:name "new" :kind kernelspec)) nil)
          (should (equal (plist-get (car my/jupyter-board--entries) :name)
                         "new")))))))

(ert-deftest my/jupyter-board-keeps-provider-cache-on-refresh-error ()
  (with-temp-buffer
    (my/jupyter-board-mode)
    (setq my/jupyter-board--target (remote-get-target "local")
          my/jupyter-board--entries '((:name "cached" :kind kernelspec)))
    (cl-letf (((symbol-function 'my/jupyter-management-discover-specs)
               (lambda (_target callback) (funcall callback nil "offline")))
              ((symbol-function 'my/jupyter-management-discover-connections)
               (lambda (_target callback) (funcall callback nil nil)))
              ((symbol-function 'my/noema-api-call)
               (lambda (_channel _args callback)
                 (funcall callback nil '((message . "offline"))))))
      (my/jupyter-board-refresh))
    (should (equal (plist-get (car my/jupyter-board--entries) :name)
                   "cached"))
    (should (equal (alist-get 'specs my/jupyter-board--errors) "offline"))))

(ert-deftest my/jupyter-board-deduplicates-noema-and-broker-runtime ()
  (let* ((broker '(:id "runtime:noema-broker:host-1" :kind runtime
                   :provider noema-broker :runtime-id "host-1"
                   :host-runtime-id "host-1" :target-id "local" :pid 42))
         (payload '((kernels . (((id . "node-1") (key . "key-1")
                                (kernel . "python3") (session . "default")
                                (hostRuntimeId . "host-1") (status . "idle"))))))
         (result (my/jupyter-board--normalize-noema-runtimes
                  payload (list broker) "local")))
    (should (= (length result) 1))
    (should (eq (plist-get (car result) :provider) 'noema))
    (should (= (plist-get (car result) :pid) 42))))

(ert-deftest my/jupyter-board-async-command-captures-output ()
  (let (output (calls 0))
    (unwind-protect
        (let ((process
               (my/jupyter-board--start-command
                "/usr/bin/printf" '("async-output")
                (lambda (value) (setq output value)
                  (cl-incf calls)))))
          ;; Completion intentionally runs on the event-loop turn after the
          ;; sentinel, so wait for the callback rather than only process life.
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (not output) (< (float-time) deadline))
              (accept-process-output process 0.05)
              (sit-for 0.01)))
          (should (equal output "async-output"))
          (should (= calls 1)))
      (when-let* ((buffer (get-buffer my/jupyter-board-log-buffer-name)))
        (kill-buffer buffer)))))

(provide 'init-jupyter-board-tests)
;;; init-jupyter-board-tests.el ends here

(ert-deftest my/jupyter-server-url-normalization-keeps-proxy-prefix ()
  (let ((parsed (my/noema-jupyter-server--parse-url
                 "https://lab.example/user/alice/lab/tree/a.ipynb?token=a%2Bb#x")))
    (should (equal (plist-get parsed :url) "https://lab.example/user/alice/"))
    (should (equal (plist-get parsed :token) "a+b")))
  (should (equal (plist-get (my/noema-jupyter-server--parse-url
                            "http://[::1]:8888/tree") :url)
                 "http://[::1]:8888/"))
  (dolist (url '("file:///tmp/test" "http://user:pass@example/" "http://example:70000/"))
    (should-error (my/noema-jupyter-server--parse-url url))))

(ert-deftest my/jupyter-server-save-keeps-credentials-out-of-persistent-config ()
  (let ((my/noema-jupyter-servers nil)
        (my/noema-jupyter-server--credentials (make-hash-table :test #'equal))
        saved)
    (cl-letf (((symbol-function 'config-set)
               (lambda (_ value &rest _) (setq saved value my/noema-jupyter-servers value)))
              ((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (let ((entry (my/noema-jupyter-server-save
                    '(:id "lab" :url "http://localhost:8888/lab?token=private"
                      :target "local" :auth token))))
        (should-not (string-match-p "private" (prin1-to-string saved)))
        (should (equal (my/noema-jupyter-server--secret entry) "private"))
        ;; Another route to the same host must never inherit the credential.
        (should-not (my/noema-jupyter-server--secret
                     (plist-put (copy-sequence entry) :target "another-target")))))))

(ert-deftest my/jupyter-server-edit-releases-old-route-and-credential ()
  (let* ((old '(:id "lab" :url "http://old.example/" :target "local" :auth token))
         (my/noema-jupyter-servers (list old))
         (my/noema-jupyter-server--credentials (make-hash-table :test #'equal))
         released)
    (puthash (my/noema-jupyter-server--credential-key old) "old-secret"
             my/noema-jupyter-server--credentials)
    (cl-letf (((symbol-function 'config-set)
               (lambda (_ value &rest _) (setq my/noema-jupyter-servers value)))
              ((symbol-function 'my/noema-jupyter-server--close-resource)
               (lambda (entry id reason) (push (list entry id reason) released))))
      (my/noema-jupyter-server-save
       '(:id "lab" :url "http://new.example/" :target "local" :auth token))
      (should (= (length my/noema-jupyter-servers) 1))
      (should (equal (caar released) old))
      (should (= (hash-table-count my/noema-jupyter-server--credentials) 0))
      (my/noema-jupyter-server-remove "lab")
      (should-not my/noema-jupyter-servers)
      (should (= (length released) 2)))))

(ert-deftest my/jupyter-server-catalog-redacts-legacy-url-tokens ()
  (let ((my/noema-jupyter-servers
         '((:id "lab" :url "https://lab.example/lab?token=legacy-secret"))))
    (cl-letf (((symbol-function 'my/noema-jupyter--defer) #'funcall))
      (let ((payload (my/noema-jupyter-server--list nil nil)))
        (should-not (string-match-p "legacy-secret" (prin1-to-string payload)))
        (should (equal (alist-get 'url (car (alist-get 'servers payload)))
                       "https://lab.example/"))))))

(ert-deftest my/jupyter-board-server-render-is-passive-and-redacts-secrets ()
  (let ((my/noema-jupyter-servers
         '((:id "lab" :name "Research" :url "https://lab.example/lab?token=private"
            :target "cluster" :auth token))))
    (with-temp-buffer
      (my/jupyter-board-mode)
      (setq my/jupyter-board--target (remote-get-target "local")
            my/jupyter-board--servers '(((id . "lab") (error . "secret-in-upstream-error"))))
      (cl-letf (((symbol-function 'my/noema-api-call)
                 (lambda (&rest _) (ert-fail "Rendering must not connect"))))
        (my/jupyter-board--render))
      (let ((text (buffer-string)))
        (should (string-match-p "Add Server URL" text))
        (should (string-match-p "Remote target cluster" text))
        (should (string-match-p "Unavailable" text))
        (should-not (string-match-p "private\\|secret-in-upstream-error" text))))))

(ert-deftest my/jupyter-board-server-check-ignores-out-of-order-responses ()
  (with-temp-buffer
    (my/jupyter-board-mode)
    (setq my/jupyter-board--target (remote-get-target "local"))
    (let (callbacks)
      (cl-letf (((symbol-function 'my/noema-api-call)
                 (lambda (_channel _args callback &rest _) (push callback callbacks))))
        (my/jupyter-board-server-check)
        (my/jupyter-board-server-check)
        (funcall (car callbacks) '((servers . (((id . "new"))))) nil)
        (funcall (cadr callbacks) '((servers . (((id . "old"))))) nil)
        (should (equal (alist-get 'id (car my/jupyter-board--servers)) "new"))
        (should-not my/jupyter-board--server-loading)))))

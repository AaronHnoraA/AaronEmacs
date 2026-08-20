;;; init-jupyter-board-tests.el --- Jupyter Board tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-jupyter-board)

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

(ert-deftest my/jupyter-board-renders-groups ()
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
        (should (string-match-p "Core Remote Kernels" text))
        (should (string-match-p "Temporary Remote Kernels" text))
        (should (string-match-p "Local / Target Kernels" text))
        (should (string-match-p "Core" text))
        (should (string-match-p "Temp" text))))))

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
          (while (and (not output) (process-live-p process))
            (accept-process-output process 0.1))
          (accept-process-output process 0.1)
          (should (equal output "async-output"))
          (should (= calls 1)))
      (when-let* ((buffer (get-buffer my/jupyter-board-log-buffer-name)))
        (kill-buffer buffer)))))

(provide 'init-jupyter-board-tests)
;;; init-jupyter-board-tests.el ends here

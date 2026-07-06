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
      (cl-letf (((symbol-function 'my/jupyter-board--load-entries)
                 (lambda () entries))
                ((symbol-function 'my/jupyter-board--command-version)
                 (lambda (&rest _args) "test"))
                ((symbol-function 'my/jupyter-board--module-source)
                 (lambda () "/tmp/remote_ikernel/__init__.py")))
        (my/jupyter-board-refresh))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Core Remote Kernels" text))
        (should (string-match-p "Temporary Remote Kernels" text))
        (should (string-match-p "Local / Other Kernels" text))
        (should (string-match-p "Core" text))
        (should (string-match-p "Temp" text))))))

(ert-deftest my/jupyter-board-async-command-captures-output ()
  (let (output)
    (unwind-protect
        (let ((process
               (my/jupyter-board--start-command
                "/usr/bin/printf" '("async-output")
                (lambda (value) (setq output value)))))
          (while (and (not output) (process-live-p process))
            (accept-process-output process 0.1))
          (accept-process-output process 0.1)
          (should (equal output "async-output")))
      (when-let* ((buffer (get-buffer my/jupyter-board-log-buffer-name)))
        (kill-buffer buffer)))))

(provide 'init-jupyter-board-tests)
;;; init-jupyter-board-tests.el ends here

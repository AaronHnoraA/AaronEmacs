;;; init-jupyter-board-tests.el --- Jupyter Board tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/jupyter/config)

(ert-deftest +jupyter-board-ssh-project-row-opens-remote-files ()
  "Configured SSH projects have direct project and filesystem entries."
  (let* ((shortcut '(:name "COMP9444" :target "aaron-pc"
                     :root "/home/aaron/Desktop/UNSW/COMP9444/"))
         (+jupyter-ssh-projects (list shortcut))
         opened files)
    (with-temp-buffer
      (cl-letf (((symbol-function '+jupyter/ssh-open-project)
                 (lambda (project) (setq opened project)))
                ((symbol-function 'dired)
                 (lambda (root) (setq files root))))
        (+jupyter-board--insert-ssh-projects)
        (goto-char (point-min))
        (should (search-forward "COMP9444" nil t))
        (should (search-forward "/fs:aaron-pc:/home/aaron/Desktop/UNSW/COMP9444/"
                                nil t))
        (goto-char (point-min))
        (search-forward "Open Project")
        (button-activate (button-at (1- (point))))
        (should (equal opened shortcut))
        (search-forward "Files")
        (button-activate (button-at (1- (point))))
        (should (equal files
                       "/fs:aaron-pc:/home/aaron/Desktop/UNSW/COMP9444/"))))))

(ert-deftest +jupyter-board-opens-local-resource-from-remote-view ()
  "Client-owned runtime files keep their machine when opened from a remote board."
  (let (visited context)
    (with-temp-buffer
      (setq default-directory "/fs:box:/work/")
      (cl-letf (((symbol-function '+jupyter-board--require-entry)
                 (lambda (&rest _)
                   '(:kind connection :file "/tmp/local-kernel.json")))
                ((symbol-function 'find-file)
                 (lambda (file &rest _)
                   (setq visited file context default-directory))))
        (+jupyter-board/open-resource))
      (should (equal default-directory "/fs:box:/work/")))
    (should (equal visited "/tmp/local-kernel.json"))
    (should-not (file-remote-p context))))

(ert-deftest +jupyter-connection-maintenance-runs-on-owner-and-is-throttled ()
  (let ((+jupyter-management--cleanup-times (make-hash-table :test #'equal))
        (launched 0) (completed 0))
    (cl-letf (((symbol-function '+jupyter-management-context) #'identity)
              ((symbol-function 'remote-exec-async)
               (lambda (program &rest args)
                 (cl-incf launched)
                 (should (equal program "python3"))
                 (should (equal (plist-get args :context) "lab"))
                 (should (equal (last (plist-get args :args) 3)
                                '("--runtime-dir" "/run/jupyter" "--apply")))
                 (funcall (plist-get args :callback)
                          (remote-exec-result-create :status 0 :stdout "" :stderr "")))))
      (dotimes (_ 2)
        (+jupyter-management--prune-directory
         "lab" "/run/jupyter"
         (lambda (&optional error)
           (should-not error)
           (cl-incf completed))))
      (should (= launched 1))
      (should (= completed 2))
      (+jupyter-management--prune-directory
       "lab" "/run/jupyter"
       (lambda (&optional error)
         (should-not error)
         (cl-incf completed)) t)
      (should (= launched 2))
      (should (= completed 3)))))

(ert-deftest +jupyter-connection-discovery-only-prunes-on-request ()
  "Board refresh may scan connection files without modifying the target."
  (let (maintain-requests replies)
    (cl-letf (((symbol-function '+jupyter-management-context)
               (lambda (_target) 'test-context))
              ((symbol-function '+jupyter-management-local-target-p)
               (lambda (_target) nil))
              ((symbol-function '+jupyter-target-command)
               (lambda (&rest _) '("jupyter")))
              ((symbol-function 'remote-exec-async)
               (lambda (_program &rest args)
                 (funcall (plist-get args :callback)
                          (remote-exec-result-create
                           :status 0 :stdout "/run/jupyter" :stderr ""))))
              ((symbol-function '+jupyter-management-logical-path)
               (lambda (_target directory) directory))
              ((symbol-function '+jupyter-management-scan-connections)
               (lambda (_directory) '(connection)))
              ((symbol-function '+jupyter-management--prune-directory)
               (lambda (_target _directory callback &optional force)
                 (push force maintain-requests)
                 (funcall callback nil))))
      (+jupyter-management-discover-connections
       "lab" (lambda (value error) (push (list value error) replies)))
      (should-not maintain-requests)
      (+jupyter-management-discover-connections
       "lab" (lambda (value error) (push (list value error) replies)) 'force)
      (should (equal maintain-requests '(t)))
      (should (equal replies '(((connection) nil) ((connection) nil)))))))

(ert-deftest +jupyter-connection-cleanup-failure-is-retryable ()
  "A failed cleanup must report its error and release the throttle."
  (let ((+jupyter-management--cleanup-times (make-hash-table :test #'equal))
        (launched 0)
        errors)
    (cl-letf (((symbol-function '+jupyter-management-context) #'identity)
              ((symbol-function '+jupyter-management-command)
               (lambda (_target _kind) "python3"))
              ((symbol-function 'remote-exec-async)
               (lambda (_program &rest args)
                 (cl-incf launched)
                 (funcall (plist-get args :callback)
                          (remote-exec-result-create
                           :status 1 :stdout "" :stderr "offline")))))
      (dotimes (_ 2)
        (+jupyter-management--prune-directory
         "lab" "/run/jupyter"
         (lambda (&optional error) (push error errors))))
      (should (= launched 2))
      (should (cl-every (lambda (error) (string-match-p "offline" error))
                        errors))
      (should-not (gethash '("lab" . "/run/jupyter")
                           +jupyter-management--cleanup-times)))))

(ert-deftest +jupyter-board-explicit-cleanup-refreshes-after-completion ()
  "The cleanup key is an explicit target operation with a visible refresh."
  (let (requested refreshed)
    (with-temp-buffer
      (+jupyter-board-mode)
      (setq +jupyter-board--target 'test-target)
      (+jupyter-board--setup-keys)
      (cl-letf (((symbol-function '+jupyter-management-discover-connections)
                 (lambda (target callback &optional maintain)
                   (setq requested (list target maintain))
                   (funcall callback nil nil)))
                ((symbol-function '+jupyter-board/refresh)
                 (lambda () (setq refreshed t))))
        (should (eq (key-binding (kbd "s"))
                    '+jupyter-board/clean-stale-connections))
        (+jupyter-board/clean-stale-connections))
      (should (equal requested '(test-target force)))
      (should refreshed))))

(ert-deftest +jupyter-board-detail-edits-original-json-and-rejects-conflicts ()
  (let* ((directory (make-temp-file "jupyter-detail-" t))
         (file (expand-file-name "kernel.json" directory))
         (+jupyter-board-detail-buffer-name " *jupyter-detail-test*")
         (+jupyter-board-buffer-name " *jupyter-no-board*")
         (+noema--ready nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"argv\":[\"python\"],\"display_name\":\"Original\"}\n"))
          (cl-letf (((symbol-function '+jupyter-board--require-entry)
                     (lambda () (list :kind 'kernelspec :resource-dir directory :display-name "Original")))
                    ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
            (+jupyter-board/describe))
          (with-current-buffer +jupyter-board-detail-buffer-name
            (goto-char (point-min))
            (should-error (insert "changed header") :type 'text-read-only)
            (goto-char +jupyter-detail--json-start)
            (search-forward "Original")
            (replace-match "Updated")
            (+jupyter/detail-save)
            (should-not (buffer-modified-p))
            (should (equal (gethash "display_name" (json-parse-string
                                                     (with-temp-buffer (insert-file-contents file) (buffer-string))))
                           "Updated"))
            (goto-char +jupyter-detail--json-start)
            (insert "invalid")
            (should-error (+jupyter/detail-save) :type 'user-error)
            (delete-region +jupyter-detail--json-start (+ 7 +jupyter-detail--json-start))
            (with-temp-file file (insert "{\"argv\":[\"python\"],\"display_name\":\"External\"}\n"))
            (should-error (+jupyter/detail-save) :type 'user-error)))
      (when-let* ((buffer (get-buffer +jupyter-board-detail-buffer-name))) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest +jupyter-repl-monads-preserve-lexical-state ()
  (require 'jupyter-repl)
  (should (= 7 (jupyter-run-with-state '(42)
                 (jupyter-bind (jupyter-push 7) (lambda (_) (jupyter-pop)))))))

(ert-deftest +jupyter-detail-save-enables-project-metadata-for-legacy-launcher ()
  (let* ((directory (make-temp-file "jupyter-project-json-" t))
         (file (expand-file-name "kernel.json" directory))
         (+noema--ready nil)
         (+jupyter-board-buffer-name " *no-test-board*"))
    (unwind-protect
        (with-temp-buffer
          (+jupyter-kernel-detail-mode)
          (setq +jupyter-detail--file file
                +jupyter-detail--original "{}")
          (with-temp-file file (insert "{}"))
          (setq +jupyter-detail--json-start (copy-marker (point)))
          (insert "{\"argv\":[\"python\",\"-m\",\"remote_ikernel\",\"{connection_file}\"],\"display_name\":\"Audit\",\"metadata\":{\"aaron\":{\"project\":{\"root\":\"/work\"}}}}")
          (setq +jupyter-detail--json-end (copy-marker (point) t))
          (+jupyter/detail-save)
          (let ((argv (gethash "argv" (json-parse-string (buffer-string)))))
            (should (equal (last (append argv nil) 2) (list "--project-file" file))))
          (should (equal (buffer-string) (+jupyter-detail--read-file file)))
          (+jupyter/detail-save)
          (should (= 6 (length (gethash "argv" (json-parse-string (buffer-string)))))))
      (delete-directory directory t))))

(ert-deftest +jupyter-contents-dired-skips-dirvish-process-probes ()
  (require 'emacs/dired/config)
  (cl-letf (((symbol-function 'remote-context) (lambda (_) 'context))
            ((symbol-function 'remote-routes) (lambda (&rest _) nil)))
    (should
     (equal (+dirvish-file-only-remote-a
             (lambda (&rest _) (ert-fail "Dirvish would probe ls"))
             (lambda (directory &rest _) directory)
             "/fs:jupyter.4c6162:/")
            "/fs:jupyter.4c6162:/"))))

(defun +jupyter-board-test--object (argv &optional metadata)
  "Return a minimal kernelspec object containing ARGV and METADATA."
  `((resource_dir . ,temporary-file-directory)
    (spec . ((argv . ,argv)
             (display_name . "Test Kernel")
             (language . "python")
             (metadata . ,metadata)))))

(ert-deftest +jupyter-board-legacy-remote-defaults-to-core ()
  (let ((entry (+jupyter-board--entry
                "rik_ssh_example_python"
                (+jupyter-board-test--object
                 '("python" "-m" "remote_ikernel"
                   "--interface" "ssh" "--host" "example"
                   "--kernel_cmd" "python -m ipykernel -f {host_connection_file}"
                   "{connection_file}")))))
    (should (plist-get entry :remote))
    (should (equal (plist-get entry :group) "core"))
    (should (equal (plist-get entry :host) "example"))
    (should (equal (plist-get entry :interface) "ssh"))))

(ert-deftest +jupyter-board-reads-temporary-metadata ()
  (let* ((metadata '((aaron . ((remote_kernel . ((group . "temporary")))))))
         (entry (+jupyter-board--entry
                 "rik_ssh_example_python"
                 (+jupyter-board-test--object
                  '("python" "-m" "remote_ikernel") metadata))))
    (should (equal (plist-get entry :group) "temporary"))))

(ert-deftest +jupyter-board-normalizes-tunnel-hosts ()
  (should
   (equal
    (+jupyter-board--remote-args-normalize
     '("--interface=ssh" "--tunnel-hosts=jump-a, jump-b" "--verbose"))
    '("--interface=ssh" "--tunnel-hosts" "jump-a" "jump-b" "--verbose"))))

(ert-deftest +jupyter-board-parses-local-version ()
  (should
   (equal
    (+jupyter-board--command-version
     "/usr/bin/printf" "Remote launcher (version 0.4.6+aaron.1).")
    "0.4.6+aaron.1")))

(ert-deftest +jupyter-board-edit-args-prefer-namespaced-config ()
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
         (entry (+jupyter-board--entry
                 "rik_ssh_example_python"
                 (+jupyter-board-test--object
                  '("python" "-m" "remote_ikernel") metadata)))
         (args (+jupyter-board--transient-config-args entry)))
    (should (member "--interface=ssh" args))
    (should (member "--name=Python" args))
    (should (member "--group=temporary" args))
    (should (member "--host=example" args))
    (should (member "--tunnel-hosts=jump" args))))

(ert-deftest +jupyter-board-renders-project-first-management-ui ()
  (let ((entries
         (list
          '(:name "rik_core" :display-name "Core" :language "python"
            :resource-dir "/tmp/core" :remote t :group "core" :interface "ssh")
          '(:name "rik_temp" :display-name "Temp" :language "python"
            :resource-dir "/tmp/temp" :remote t :group "temporary" :interface "ssh")
          '(:name "python3" :display-name "Python" :language "python"
            :resource-dir "/tmp/python" :remote nil))))
    (with-temp-buffer
      (+jupyter-board-mode)
      (setq +jupyter-board--target (remote-get-target "local"))
      (cl-letf (((symbol-function '+jupyter-management-discover-specs)
                 (lambda (_target callback) (funcall callback entries nil)))
                ((symbol-function '+jupyter-management-discover-connections)
                 (lambda (_target callback) (funcall callback nil nil)))
                ((symbol-function '+noema-api-call)
                 (lambda (_channel _args callback)
                   (funcall callback '((kernels . nil)) nil))))
        (+jupyter-board/refresh))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Jupyter Projects" text))
        (should (string-match-p "SSH Notebook Projects" text))
        (should (string-match-p "Open SSH Project" text))
        (should (string-match-p "Start Here" text))
        (should (string-match-p "Remote Kernels" text))
        (should (< (string-match "SSH Notebook Projects" text)
                   (string-match "Remote Kernels" text)))
        (should (string-match-p "Open REPL" text))
        (should (string-match-p "Make temporary" text))
        (should (string-match-p "Keep profile" text))
        (should (string-match-p "Show kernels, connections & diagnostics" text))
        (should-not (string-match-p "Local / Target Kernels" text))
        (should (string-match-p "Core" text))
        (should (string-match-p "Temp" text))))))

(ert-deftest +jupyter-board-renders-guided-empty-state ()
  (with-temp-buffer
    (+jupyter-board-mode)
    (setq +jupyter-board--target (remote-get-target "local")
          +jupyter-board--entries nil
          +jupyter-board--runtimes nil
          +jupyter-board--connections nil)
    (+jupyter-board--render)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "1 · Open" text))
      (should (string-match-p "Open SSH Project" text))
      (should (string-match-p "No remote profile is configured yet" text))
      (should (string-match-p "Quick Add SSH" text)))))

(ert-deftest +jupyter-board-shows-damaged-local-shutdown-record ()
  "Record parse failures stay inside the Board instead of opening warnings."
  (let* ((directory (make-temp-file "jupyter-board-bad-record-" t))
         (+noema-jupyter-pending-directory directory)
         (file (expand-file-name (concat (make-string 64 ?a) ".json")
                                 directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{oops"))
          (with-temp-buffer
            (+jupyter-board-mode)
            (setq +jupyter-board--target (remote-get-target "local"))
            (+jupyter-board--render)
            (should (string-match-p
                     "Saved Broker Record Problems"
                     (buffer-substring-no-properties
                      (point-min) (point-max))))))
      (delete-directory directory t))))

(ert-deftest +jupyter-board-action-buttons-carry-entry-context ()
  (let ((entry '(:id "spec:local:remote:test" :kind kernelspec
                 :name "rik_test" :display-name "Test" :language "python"
                 :remote t :group "core" :interface "ssh" :host "example"
                 :kernel-command "python3 -m ipykernel_launcher -f {host_connection_file}")))
    (with-temp-buffer
      (+jupyter-board-mode)
      (let ((inhibit-read-only t))
        (+jupyter-board--insert-entry entry))
      (goto-char (point-min))
      (search-forward "Open REPL")
      (should (equal (plist-get (+jupyter-board--current-entry) :name)
                     "rik_test")))))

(ert-deftest +jupyter-board-technical-view-is-explicit ()
  (with-temp-buffer
    (+jupyter-board-mode)
    (setq +jupyter-board--target (remote-get-target "local")
          +jupyter-board--entries
          '((:id "local" :kind kernelspec :name "python3"
             :display-name "Python" :language "python" :resource-dir "/tmp/python")))
    (+jupyter-board--render)
    (should-not (string-match-p "Local / Target Kernels"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
    (setq +jupyter-board--show-advanced t)
    (+jupyter-board--render)
    (should (string-match-p "Local / Target Kernels"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest +jupyter-board-validates-remote-profile-connectivity-placeholder ()
  (should
   (+jupyter-board--validate-remote-args
    '("--interface=ssh" "--host=user@example:2222" "--name=Python"
      "--kernel_cmd=python3 -m ipykernel_launcher -f {connection_file}")))
  (should-error
   (+jupyter-board--validate-remote-args
    '("--interface=ssh" "--host=example" "--name=Python"
      "--kernel_cmd=python3 -m ipykernel_launcher"))
   :type 'user-error)
  (should-error
   (+jupyter-board--validate-remote-args
    '("--interface=ssh" "--host=user at example" "--name=Python"
      "--kernel_cmd=python3 -m ipykernel_launcher -f {connection_file}"))
   :type 'user-error))

(ert-deftest +jupyter-board-guided-edit-preserves-advanced-arguments ()
  (let* ((args '("--interface=ssh" "--host=old" "--name=Python"
                 "--kernel_cmd=python -m ipykernel -f {connection_file}"
                 "--tunnel-hosts=jump" "--verbose"))
         (updated (+jupyter-board--replace-arg args "--host=" "new")))
    (should (member "--host=new" updated))
    (should-not (member "--host=old" updated))
    (should (member "--tunnel-hosts=jump" updated))
    (should (member "--verbose" updated))))

(ert-deftest +jupyter-board-quick-add-builds-safe-saved-profile ()
  (let (saved-target saved-args)
    (with-temp-buffer
      (+jupyter-board-mode)
      (setq +jupyter-board--target (remote-get-target "local"))
      (cl-letf (((symbol-function '+jupyter-management-command)
                 (lambda (_target _kind) "/usr/bin/true"))
                ((symbol-function '+jupyter-board--read-required)
                 (lambda (prompt _default _history)
                   (cond
                    ((string-prefix-p "SSH host" prompt) "student@cluster:2222")
                    ((string-prefix-p "Profile name" prompt) "Course Python")
                    ((string-prefix-p "Remote kernel" prompt)
                     "python3 -m ipykernel_launcher -f {connection_file}")
                    (t (ert-fail (format "Unexpected prompt: %s" prompt))))))
                ((symbol-function '+jupyter-board--read-workdir)
                 (lambda (_default) "/srv/course"))
                ((symbol-function '+jupyter-board--read-lifetime)
                 (lambda (_current-group) "Saved (protected)"))
                ((symbol-function '+jupyter-board--confirm-profile)
                 (lambda (&rest _) t))
                ((symbol-function '+jupyter-board--save-remote-profile)
                 (lambda (target args &optional _origin)
                   (setq saved-target target saved-args args))))
        (+jupyter/remote-quick-add)))
    (should (equal (+jupyter-management-target-id saved-target) "local"))
    (should (member "--host=student@cluster:2222" saved-args))
    (should (member "--name=Course Python" saved-args))
    (should (member "--workdir=/srv/course" saved-args))
    (should (member "--group=core" saved-args))
    (should (member
             "--kernel_cmd=python3 -m ipykernel_launcher -f {connection_file}"
             saved-args))))

(ert-deftest +jupyter-board-course-pytorch-preset-builds-remote-profile ()
  (let (called target args)
    (cl-letf (((symbol-function '+jupyter-board--start-target-command)
               (lambda (given-target _kind given-args &optional callback)
                 (setq target given-target args given-args called t)
                 (when callback (funcall callback "Added kernel ['rik_course']."))))
              ((symbol-function '+jupyter-board--remote-args-normalize)
               #'identity))
      (+jupyter-board/add-course-pytorch
       :host "student@cluster:2222"
       :target (remote-get-target "local")))
    (should called)
    (should (equal (+jupyter-management-target-id target) "local"))
    (should (member "--interface=ssh" args))
    (should (member "--host=student@cluster:2222" args))
    (should (member "--name=Python 3.13 PyTorch CUDA" args))
    (should (member "--workdir=/home/hc/Desktop/9444" args))
    (should (member "--group=core" args))
    (should (member
             "--kernel_cmd=/home/hc/Desktop/9444/.conda/bin/python -m ipykernel_launcher -f {connection_file}"
             args))))

(ert-deftest +jupyter-board-course-pytorch-preset-requires-host ()
  (should-error (+jupyter-board/add-course-pytorch :host "") :type 'user-error))

(ert-deftest +jupyter-board-diagnoses-broken-absolute-launcher ()
  (let* ((entry (+jupyter-board--entry
                 "broken"
                 (+jupyter-board-test--object
                  '("/definitely/missing/jupyter-python" "-f" "{connection_file}"))))
         (health (plist-get entry :health)))
    (should (eq (plist-get health :status) 'error))
    (should (string-match-p "not executable" (plist-get health :detail)))))

(ert-deftest +jupyter-board-validates-connection-payload ()
  (should
   (+jupyter-management-connection-valid-p
    '((transport . "tcp") (ip . "127.0.0.1") (key . "secret")
      (shell_port . 1) (iopub_port . 2) (stdin_port . 3)
      (control_port . 4) (hb_port . 5))))
  (should-not
   (+jupyter-management-connection-valid-p
    '((transport . "tcp") (ip . "127.0.0.1") (key . "secret")))))

(ert-deftest +jupyter-board-repl-associates-the-source-buffer ()
  (require 'jupyter-repl)
  (let ((source (generate-new-buffer " *jupyter-board-source*")) call)
    (unwind-protect
        (cl-letf (((symbol-function 'jupyter-run-repl)
                   (lambda (&rest args)
                     (setq call (cons (current-buffer) args)))))
          (+jupyter-management-run-repl
           '(:kind kernelspec :target-id "local" :name "python3") source)
          (should (eq (car call) source))
          (should (equal (cdr call) '("python3" nil t nil t))))
      (kill-buffer source))))

(ert-deftest +jupyter-board-connection-associates-the-source-buffer ()
  (require 'jupyter-repl)
  (let ((source (generate-new-buffer " *jupyter-board-connect-source*")) call)
    (unwind-protect
        (cl-letf (((symbol-function 'jupyter-connect-repl)
                   (lambda (&rest args)
                     (setq call (cons (current-buffer) args)))))
          (+jupyter-management-connect-repl
           '(:valid t :file "/tmp/kernel.json") source)
          (should (eq (car call) source))
          (should (equal (cdr call) '("/tmp/kernel.json" nil t nil t))))
      (kill-buffer source))))

(ert-deftest +jupyter-board-discards-stale-refresh-results ()
  (let (spec-callbacks)
    (with-temp-buffer
      (+jupyter-board-mode)
      (setq +jupyter-board--target (remote-get-target "local"))
      (cl-letf (((symbol-function '+jupyter-management-discover-specs)
                 (lambda (_target callback) (push callback spec-callbacks)))
                ((symbol-function '+jupyter-management-discover-connections)
                 (lambda (_target callback) (funcall callback nil nil)))
                ((symbol-function '+noema-api-call)
                 (lambda (_channel _args callback)
                   (funcall callback '((kernels . nil)) nil))))
        (+jupyter-board/refresh)
        (+jupyter-board/refresh)
        (let ((newest (car spec-callbacks))
              (oldest (cadr spec-callbacks)))
          (funcall oldest '((:name "old" :kind kernelspec)) nil)
          (should-not +jupyter-board--entries)
          (funcall newest '((:name "new" :kind kernelspec)) nil)
          (should (equal (plist-get (car +jupyter-board--entries) :name)
                         "new")))))))

(ert-deftest +jupyter-board-reopen-coalesces-pending-discovery ()
  "Opening the board again does not duplicate an unfinished provider scan."
  (let ((+jupyter-board-buffer-name "*Jupyter Board Reopen Test*")
        (spec-requests 0)
        spec-callback)
    (unwind-protect
        (cl-letf (((symbol-function '+jupyter-management-discover-specs)
                   (lambda (_target callback)
                     (cl-incf spec-requests)
                     (setq spec-callback callback)))
                  ((symbol-function '+jupyter-management-discover-connections)
                   (lambda (_target callback) (funcall callback nil nil)))
                  ((symbol-function '+noema-api-call)
                   (lambda (_channel _args callback) (funcall callback nil nil)))
                  ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
          (+jupyter/board)
          (+jupyter/board)
          (should (= spec-requests 1))
          (funcall spec-callback nil nil)
          (+jupyter/board)
          (should (= spec-requests 2)))
      (when-let* ((buffer (get-buffer +jupyter-board-buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest +jupyter-board-disconnect-invalidates-in-flight-result ()
  "An explicit target disconnect clears the board and rejects late replies."
  (let ((+jupyter-board-buffer-name "*Jupyter Board Disconnect Test*")
        spec-callback cleanup-callback)
    (unwind-protect
        (with-current-buffer (get-buffer-create +jupyter-board-buffer-name)
          (+jupyter-board-mode)
          (setq +jupyter-board--target (remote-get-target "local"))
          (cl-letf (((symbol-function '+jupyter-management-discover-specs)
                     (lambda (_target callback) (setq spec-callback callback)))
                    ((symbol-function '+jupyter-management-discover-connections)
                     (lambda (_target callback) (funcall callback nil nil)))
                    ((symbol-function '+noema-api-call)
                     (lambda (_channel _args callback) (funcall callback nil nil))))
            (+jupyter-board/refresh))
          (should +jupyter-board--loading)
          (cl-letf (((symbol-function '+jupyter-management-discover-connections)
                     (lambda (_target callback &optional _maintain)
                       (setq cleanup-callback callback))))
            (+jupyter-board/clean-stale-connections))
          (+jupyter-board--target-disconnected-h "local" 'user)
          (should-not +jupyter-board--loading)
          (should (equal (alist-get 'target +jupyter-board--errors)
                         "Disconnected; press g to refresh this target"))
          (funcall spec-callback '((:name "late")) nil)
          (should-not +jupyter-board--entries)
          (let ((generation +jupyter-board--refresh-generation))
            (funcall cleanup-callback nil nil)
            (should (= generation +jupyter-board--refresh-generation)))
          (should (string-match-p
                   "Disconnected; press g"
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (when-let* ((buffer (get-buffer +jupyter-board-buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest +jupyter-board-cleanup-does-not-refresh-another-target ()
  "A cleanup reply for the old target must not refresh a new selection."
  (let (cleanup-callback (refreshes 0))
    (with-temp-buffer
      (+jupyter-board-mode)
      (setq +jupyter-board--target "old")
      (cl-letf (((symbol-function '+jupyter-management-discover-connections)
                 (lambda (_target callback &optional _maintain)
                   (setq cleanup-callback callback)))
                ((symbol-function '+jupyter-board/refresh)
                 (lambda () (cl-incf refreshes))))
        (+jupyter-board/clean-stale-connections)
        (setq +jupyter-board--target "new")
        (funcall cleanup-callback nil nil)
        (should (= refreshes 0))))))

(ert-deftest +jupyter-board-keeps-provider-cache-on-refresh-error ()
  (with-temp-buffer
    (+jupyter-board-mode)
    (setq +jupyter-board--target (remote-get-target "local")
          +jupyter-board--entries '((:name "cached" :kind kernelspec)))
    (cl-letf (((symbol-function '+jupyter-management-discover-specs)
               (lambda (_target callback) (funcall callback nil "offline")))
              ((symbol-function '+jupyter-management-discover-connections)
               (lambda (_target callback) (funcall callback nil nil)))
              ((symbol-function '+noema-api-call)
               (lambda (_channel _args callback)
                 (funcall callback nil '((message . "offline"))))))
      (+jupyter-board/refresh))
    (should (equal (plist-get (car +jupyter-board--entries) :name)
                   "cached"))
    (should (equal (alist-get 'specs +jupyter-board--errors) "offline"))))

(ert-deftest +jupyter-board-deduplicates-noema-and-broker-runtime ()
  (let* ((broker '(:id "runtime:noema-broker:host-1" :kind runtime
                   :provider noema-broker :runtime-id "host-1"
                   :host-runtime-id "host-1" :target-id "local" :pid 42))
         (payload '((kernels . (((id . "node-1") (key . "key-1")
                                (kernel . "python3") (session . "default")
                                (hostRuntimeId . "host-1") (status . "idle"))))))
         (result (+jupyter-board--normalize-noema-runtimes
                  payload (list broker) "local")))
    (should (= (length result) 1))
    (should (eq (plist-get (car result) :provider) 'noema))
    (should (= (plist-get (car result) :pid) 42))))

(ert-deftest +jupyter-board-shows-unconfirmed-broker-shutdown ()
  "A stale Noema task status must not hide a failed target shutdown."
  (let* ((broker '(:id "runtime:noema-broker:host-1" :kind runtime
                   :provider noema-broker :runtime-id "host-1"
                   :host-runtime-id "host-1" :target-id "local"
                   :status shutdown-pending))
         (payload '((kernels . (((id . "node-1") (key . "key-1")
                                (hostRuntimeId . "host-1")
                                (status . "idle"))))))
         (result (+jupyter-board--normalize-noema-runtimes
                  payload (list broker) "local")))
    (should (eq (plist-get (car result) :status) 'shutdown-pending))))

(ert-deftest +jupyter-board-saved-shutdown-takes-one-actionable-row ()
  "A saved broker shutdown must not be hidden behind a stale Noema task."
  (let* ((broker '(:id "runtime:noema-broker:host-1" :kind runtime
                   :provider noema-broker :runtime-id "host-1"
                   :host-runtime-id "host-1" :target-id "local"
                   :status shutdown-pending :journal-pending t))
         (payload '((kernels . (((id . "node-1") (key . "key-1")
                                (hostRuntimeId . "host-1")
                                (status . "idle"))))))
         (result (+jupyter-board--normalize-noema-runtimes
                  payload (list broker) "local")))
    (should (= (length result) 1))
    (should (eq (car result) broker))
    (should (eq (plist-get (car result) :provider) 'noema-broker))))

(ert-deftest +jupyter-board-async-command-captures-output ()
  (let (output (calls 0))
    (unwind-protect
        (let ((process
               (+jupyter-board--start-command
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
      (when-let* ((buffer (get-buffer +jupyter-board-log-buffer-name)))
        (kill-buffer buffer)))))

(provide 'init-jupyter-board-tests)
;;; init-jupyter-board-tests.el ends here

(ert-deftest +jupyter-server-url-normalization-keeps-proxy-prefix ()
  (let ((parsed (+noema-jupyter-server--parse-url
                 "https://lab.example/user/alice/lab/tree/a.ipynb?token=a%2Bb#x")))
    (should (equal (plist-get parsed :url) "https://lab.example/user/alice/"))
    (should (equal (plist-get parsed :token) "a+b")))
  (should (equal (plist-get (+noema-jupyter-server--parse-url
                            "http://[::1]:8888/tree") :url)
                 "http://[::1]:8888/"))
  (dolist (url '("file:///tmp/test" "http://user:pass@example/" "http://example:70000/"))
    (should-error (+noema-jupyter-server--parse-url url))))

(ert-deftest +jupyter-server-save-keeps-credentials-out-of-persistent-config ()
  (let ((+noema-jupyter-servers nil)
        (+noema-jupyter-server--credentials (make-hash-table :test #'equal))
        saved)
    (cl-letf (((symbol-function 'config-set)
               (lambda (_ value &rest _) (setq saved value +noema-jupyter-servers value)))
              ((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (let ((entry (+noema-jupyter-server-save
                    '(:id "lab" :url "http://localhost:8888/lab?token=private"
                      :target "local" :auth token))))
        (should-not (string-match-p "private" (prin1-to-string saved)))
        (should (equal (+noema-jupyter-server--secret entry) "private"))
        ;; Another route to the same host must never inherit the credential.
        (should-not (+noema-jupyter-server--secret
                     (plist-put (copy-sequence entry) :target "another-target")))))))

(ert-deftest +jupyter-server-edit-releases-old-route-and-credential ()
  (let* ((old '(:id "lab" :url "http://old.example/" :target "local" :auth token))
         (+noema-jupyter-servers (list old))
         (+noema-jupyter-server--credentials (make-hash-table :test #'equal))
         released)
    (puthash (+noema-jupyter-server--credential-key old) "old-secret"
             +noema-jupyter-server--credentials)
    (cl-letf (((symbol-function 'config-set)
               (lambda (_ value &rest _) (setq +noema-jupyter-servers value)))
              ((symbol-function '+noema-jupyter-server--close-resource)
               (lambda (entry id reason) (push (list entry id reason) released))))
      (+noema-jupyter-server-save
       '(:id "lab" :url "http://new.example/" :target "local" :auth token))
      (should (= (length +noema-jupyter-servers) 1))
      (should (equal (caar released) old))
      (should (= (hash-table-count +noema-jupyter-server--credentials) 0))
      (+noema-jupyter-server-remove "lab")
      (should-not +noema-jupyter-servers)
      (should (= (length released) 2)))))

(ert-deftest +jupyter-server-catalog-redacts-legacy-url-tokens ()
  (let ((+noema-jupyter-servers
         '((:id "lab" :url "https://lab.example/lab?token=legacy-secret"))))
    (cl-letf (((symbol-function '+noema-jupyter--defer) #'funcall))
      (let ((payload (+noema-jupyter-server--list nil nil)))
        (should-not (string-match-p "legacy-secret" (prin1-to-string payload)))
        (should (equal (alist-get 'url (car (alist-get 'servers payload)))
                       "https://lab.example/"))))))

(ert-deftest +jupyter-board-server-render-is-passive-and-redacts-secrets ()
  (let ((+noema-jupyter-servers
         '((:id "lab" :name "Research" :url "https://lab.example/lab?token=private"
            :target "cluster" :auth token))))
    (with-temp-buffer
      (+jupyter-board-mode)
      (setq +jupyter-board--target (remote-get-target "local")
            +jupyter-board--servers '(((id . "lab") (error . "secret-in-upstream-error"))))
      (cl-letf (((symbol-function '+noema-api-call)
                 (lambda (&rest _) (ert-fail "Rendering must not connect"))))
        (+jupyter-board--render))
      (let ((text (buffer-string)))
        (should (string-match-p "Add Server URL" text))
        (should (string-match-p "Remote target cluster" text))
        (should (string-match-p "Unavailable" text))
        (should-not (string-match-p "private\\|secret-in-upstream-error" text))))))

(ert-deftest +jupyter-board-server-check-ignores-out-of-order-responses ()
  (with-temp-buffer
    (+jupyter-board-mode)
    (setq +jupyter-board--target (remote-get-target "local"))
    (let (callbacks)
      (cl-letf (((symbol-function '+noema-api-call)
                 (lambda (_channel _args callback &rest _) (push callback callbacks))))
        (+jupyter-board/server-check)
        (+jupyter-board/server-check)
        (funcall (car callbacks) '((servers . (((id . "new"))))) nil)
        (funcall (cadr callbacks) '((servers . (((id . "old"))))) nil)
        (should (equal (alist-get 'id (car +jupyter-board--servers)) "new"))
        (should-not +jupyter-board--server-loading)))))

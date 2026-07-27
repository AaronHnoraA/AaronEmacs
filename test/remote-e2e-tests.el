;;; remote-e2e-tests.el --- Opt-in real SSH remote tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run only through `make remote-e2e'.  The suite imports the normal SSH
;; configuration, chooses an Aaron-* target unless REMOTE_E2E_TARGET is set,
;; and confines all target mutations to a fresh /tmp directory.

;;; Code:

(require 'ert)
(require 'seq)
(require 'remote-framework)
(require 'remote-config)

(defun remote-e2e--enabled-p ()
  "Return non-nil when destructive temporary E2E checks are opted in."
  (equal (getenv "REMOTE_E2E") "1"))

(defun remote-e2e--target ()
  "Return the explicitly selected or first reachable Aaron-* target."
  (remote-config-load)
  (let* ((requested (getenv "REMOTE_E2E_TARGET"))
         (candidates
          (sort
           (seq-filter
            (lambda (target)
              (and
               (not (equal (remote-target-id target) "local"))
               (string-match-p
                "\\`Aaron-"
                (or (remote-target-label target) ""))
               (remote-pipelines-for-target
                (remote-target-id target))))
            (hash-table-values remote-targets))
           (lambda (left right)
             (string-lessp
              (remote-target-label left)
              (remote-target-label right)))))
         (selected
          (if requested
              (or
               (remote-get-target requested)
               (seq-find
                (lambda (target)
                  (equal (remote-target-label target) requested))
                candidates))
            (seq-find
             (lambda (target)
               (let ((default-directory temporary-file-directory))
                 (zerop
                  (call-process
                   "ssh" nil nil nil
                   "-T" "-o" "BatchMode=yes"
                   "-o" "ConnectTimeout=2"
                   "-o" "ConnectionAttempts=1"
                   (remote-target-label target)
                   "true"))))
             candidates))))
    (when selected
      (message "Remote E2E target: %s (%s)"
               (remote-target-label selected)
               (remote-target-id selected)))
    selected))

(ert-deftest remote-e2e-ssh-file-process-and-session-contract ()
  (unless (remote-e2e--enabled-p)
    (ert-skip "Set REMOTE_E2E=1 or run make remote-e2e"))
  (remote-fs-install)
  (let* ((target
          (or (remote-e2e--target)
              (ert-fail
               "No imported Aaron-* target; set REMOTE_E2E_TARGET")))
         (target-id (remote-target-id target))
         (bootstrap-context
          (remote-context
           (remote-make-file-name target-id "/tmp/")))
         (remote-directory
          (string-trim
           (remote-exec-output
            "mktemp"
            :args '("-d" "/tmp/emacs-remote-e2e.XXXXXX")
            :context bootstrap-context
            :adapter "exec"
            :check t)))
         (logical-directory
          (file-name-as-directory
           (remote-make-file-name target-id remote-directory)))
         (remote-file
          (expand-file-name "roundtrip.txt" logical-directory))
         (local-directory
          (make-temp-file "emacs-remote-e2e-" t))
         (local-file
          (expand-file-name "source.txt" local-directory))
         (payload
          (format "remote-e2e:%s:%s\n"
                  target-id (float-time))))
    (unwind-protect
        (progn
          (write-region payload nil local-file nil 'silent)
          (copy-file local-file remote-file)
          (should (file-exists-p remote-file))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents remote-file)
              (buffer-string))
            payload))
          (should
           (member remote-file
                   (directory-files logical-directory t
                                    "\\`roundtrip\\.txt\\'")))
          (let ((pwd
                 (string-trim
                  (remote-exec-output
                   "pwd" :context logical-directory
                   :adapter "exec" :check t))))
            (should
             (equal
              (directory-file-name pwd)
              (directory-file-name remote-directory))))
          (let* ((context (remote-context remote-file))
                 (first
                  (remote-session-warm
                   context "process" 'process-sync))
                 (second
                  (remote-session-warm
                   context "process" 'process-sync)))
            (should (eq first second))
            (should (> (remote-session-use-count second) 1)))
          (let (async-result)
            (let ((process
                   (remote-exec-async
                    "sh"
                    :args '("-c"
                            "printf async-stdout; printf async-stderr >&2")
                    :context logical-directory
                    :adapter "exec"
                    :callback
                    (lambda (result)
                      (setq async-result result)))))
              (while (process-live-p process)
                (accept-process-output process 0.1))
              (while (null async-result)
                (accept-process-output nil 0.05)))
            (should (zerop (remote-exec-result-status async-result)))
            (should
             (equal (remote-exec-result-stdout async-result)
                    "async-stdout"))
            (should
             (equal (remote-exec-result-stderr async-result)
                    "async-stderr")))
          ;; A routed listener remains an ordinary Emacs server process while
          ;; its advertised contact is the target-side dynamic SSH -R port.
          (when-let* ((python
                       (remote-executable-find
                        "python3" logical-directory)))
            (let (received listener)
              (unwind-protect
                  (progn
                    (setq listener
                          (remote-make-network-process
                           :name "remote-e2e-listener"
                           :server t
                           :host "127.0.0.1" :service t
                           :coding 'binary :noquery t
                           :filter
                           (lambda (_process string)
                             (setq received
                                   (concat received string)))
                           :remote-context
                           (remote-context logical-directory)))
                    (let ((port (process-contact listener :service)))
                      (should (integerp port))
                      (remote-exec
                       python
                       :args
                       (list
                        "-c"
                        (concat
                         "import socket;"
                         "s=socket.create_connection(('127.0.0.1',"
                         (number-to-string port)
                         "));s.sendall(b'reverse-ok');s.close()"))
                       :context logical-directory
                       :adapter "exec" :check t))
                    (let ((deadline (+ (float-time) 3)))
                      (while (and (not (equal received "reverse-ok"))
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.05)))
                    (should (equal received "reverse-ok")))
                (when listener
                  (remote-close-channel listener))))))
      (when (and remote-directory
                 (string-match-p
                  "\\`/tmp/emacs-remote-e2e\\.[[:alnum:]]+\\'"
                  remote-directory))
        (ignore-errors
          (remote-exec
           "rm" :args (list "-rf" remote-directory)
           :context bootstrap-context :adapter "exec")))
      (when (file-directory-p local-directory)
        (delete-directory local-directory t))
      (remote-session-clear t))))

(provide 'remote-e2e-tests)
;;; remote-e2e-tests.el ends here

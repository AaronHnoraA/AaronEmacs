;;; remote-live-target.el --- Disposable SSH target shared by live checks -*- lexical-binding: t; -*-

(require 'remote-config)
(remote-config-load)

(when-let* ((program (getenv "TERMINAL_REMOTE_SSH_PROGRAM")))
  (setq remote-client-ssh-program program
        exec-path (cons (file-name-directory program) exec-path))
  (setenv "PATH" (concat (file-name-directory program) path-separator (getenv "PATH"))))
(when (getenv "TERMINAL_REMOTE_TRACE")
  (advice-add 'process-file :before
              (lambda (program &rest _)
                (message "Live process-file: program=%s cwd=%s" program default-directory))))

(when-let* ((config (getenv "TERMINAL_REMOTE_SSH_CONFIG")))
  (remote-register-target "terminal-loopback" :trusted t :system "darwin"
                          :shell "/bin/zsh" :source 'terminal-test
                          :preferences '((pty . ("ssh-pty" "tramp-rpc" "tramp"))
                                         (default . ("tramp-rpc" "tramp"))))
  (remote-register-pipeline "terminal-loopback" "ssh" '("ssh-pty" "tramp-rpc" "tramp")
                            :config (list :host "terminal-loopback" :ssh-config-file config)))
(when-let* ((directory (getenv "TERMINAL_REMOTE_RPC_DIRECTORY")))
  (remote-require-on-client 'tramp-rpc-deploy)
  (setq tramp-rpc-deploy-remote-directory directory))
(when-let* ((binary (getenv "TERMINAL_REMOTE_RPC_BINARY")))
  (remote-require-on-client 'tramp-rpc-deploy)
  (setq tramp-rpc-deploy-never-deploy t tramp-rpc-deploy-remote-binary-path binary))

(provide 'remote-live-target)
;;; remote-live-target.el ends here

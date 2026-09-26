;;; remote-key-to-screen.el --- Interactive key-to-terminal probe -*- lexical-binding: t; -*-

;; Run through remote-key-to-screen.py, which supplies a real PTY and keys.
;; The remote source is visited but never saved.

(require 'init-lsp)
(require 'remote-config)
(require 'remote-framework)

(when (equal (getenv "REMOTE_KEY_SCREEN_FAST_IDLE") "0")
  (setq my/lsp-python-completion-ready-idle-delay nil))

(defvar my/remote-key-to-screen--buffer nil)
(defvar my/remote-key-to-screen--local-file nil)
(defvar my/remote-key-to-screen--local-directory nil)
(defvar my/remote-key-to-screen--count 0)
(defvar my/remote-key-to-screen--completion-requests 0)
(defvar my/remote-key-to-screen--dropped-completions 0)
(defvar my/remote-key-to-screen--popup-ready nil)
(defvar my/remote-key-to-screen--popup-tooltip nil)
(defconst my/remote-key-to-screen--popup-scenario
  (equal (getenv "REMOTE_KEY_SCREEN_SCENARIO") "python-completion"))
(defconst my/remote-key-to-screen--keys
  (if my/remote-key-to-screen--popup-scenario
      "prin"
    "αβγδεζηθικλμνξοπρστυφχψω"))

(defun my/remote-key-to-screen--report (status)
  "Write STATUS to the local ready file."
  (let ((file (getenv "REMOTE_KEY_SCREEN_READY")))
    (unless file (error "REMOTE_KEY_SCREEN_READY is missing"))
    (with-temp-buffer
      (insert status "\n")
      (write-region (point-min) (point-max) file nil 'silent))))

(defun my/remote-key-to-screen--cleanup ()
  "Discard the probe's unsaved edits and local temporary file."
  (when (buffer-live-p my/remote-key-to-screen--buffer)
    (with-current-buffer my/remote-key-to-screen--buffer
      (set-buffer-modified-p nil))
    (kill-buffer my/remote-key-to-screen--buffer))
  (when (and my/remote-key-to-screen--local-file
             (file-exists-p my/remote-key-to-screen--local-file))
    (delete-file my/remote-key-to-screen--local-file))
  (when (and my/remote-key-to-screen--local-directory
             (file-directory-p my/remote-key-to-screen--local-directory))
    (delete-directory my/remote-key-to-screen--local-directory t))
  (when (advice-member-p #'my/remote-key-to-screen--count-completion
                         'lsp-request-async)
    (advice-remove 'lsp-request-async
                   #'my/remote-key-to-screen--count-completion)))

(defun my/remote-key-to-screen--count-completion
    (original method &rest arguments)
  "Count completion METHOD calls while preserving ORIGINAL request."
  (when (equal method "textDocument/completion")
    (cl-incf my/remote-key-to-screen--completion-requests))
  (if (and (equal method "textDocument/completion")
           (equal (getenv "REMOTE_KEY_SCREEN_STALL_COMPLETION") "1")
           (= my/remote-key-to-screen--dropped-completions 0))
      (progn
        (cl-incf my/remote-key-to-screen--dropped-completions)
        nil)
    (apply original method arguments)))

(defun my/remote-key-to-screen--after-key ()
  "Exit after all expected terminal keys have been inserted."
  (when (and (characterp last-command-event)
             (string-match-p
              (regexp-quote (char-to-string last-command-event))
              my/remote-key-to-screen--keys))
    (setq my/remote-key-to-screen--count
          (1+ my/remote-key-to-screen--count))
    (when my/remote-key-to-screen--popup-scenario
      (my/remote-key-to-screen--report
       (format "KEY count=%d" my/remote-key-to-screen--count)))
    (when (= my/remote-key-to-screen--count
             (length my/remote-key-to-screen--keys))
      (if my/remote-key-to-screen--popup-scenario
          (run-at-time 0.02 nil #'my/remote-key-to-screen--poll-popup
                       (current-buffer) (+ (float-time) 8))
        (my/remote-key-to-screen--report
         (format "DONE completion_requests=%d dropped=%d"
                 my/remote-key-to-screen--completion-requests
                 my/remote-key-to-screen--dropped-completions))
        ;; Let the final redisplay reach the PTY before closing it.
        (run-at-time 0.5 nil #'my/remote-key-to-screen--finish)))))

(defun my/remote-key-to-screen--poll-popup (buffer deadline)
  "Watch BUFFER for Company's real `print' result until DEADLINE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((and (bound-and-true-p company-candidates)
             (stringp company-prefix)
             (string-suffix-p "prin" company-prefix)
             (or (eq company-backend 'company-capf)
                 (and (listp company-backend)
                      (memq 'company-capf company-backend)))
             (seq-some (lambda (candidate)
                         (and (stringp candidate)
                              (string= candidate "print")))
                       company-candidates))
        (setq my/remote-key-to-screen--popup-ready t
              my/remote-key-to-screen--popup-tooltip
              (and (fboundp 'company-tooltip-visible-p)
                   (company-tooltip-visible-p)))
        (redisplay t)
        (my/remote-key-to-screen--report
         (format "POPUP candidate=print backend=capf tooltip=%s completion_requests=%d"
                 (if my/remote-key-to-screen--popup-tooltip "yes" "no")
                 my/remote-key-to-screen--completion-requests))
        (run-at-time 0.5 nil #'my/remote-key-to-screen--finish))
       ((>= (float-time) deadline)
        (my/remote-key-to-screen--report
         (format "POPUP-TIMEOUT prefix=%S backend=%S candidates=%d completion_requests=%d retry-at=%S"
                 (and (boundp 'company-prefix) company-prefix)
                 (and (boundp 'company-backend) company-backend)
                 (if (boundp 'company-candidates)
                     (length company-candidates) 0)
                 my/remote-key-to-screen--completion-requests
                 my/lsp-remote-completion--retry-at))
        (run-at-time 0.1 nil #'kill-emacs 1))
       (t
        (run-at-time 0.02 nil #'my/remote-key-to-screen--poll-popup
                     buffer deadline))))))

(defun my/remote-key-to-screen--finish ()
  "Exit after recording any shutdown error for the PTY driver."
  (my/remote-key-to-screen--report
   (format "DONE completion_requests=%d dropped=%d exit_started=1 popup=%d tooltip=%d"
           my/remote-key-to-screen--completion-requests
           my/remote-key-to-screen--dropped-completions
           (if my/remote-key-to-screen--popup-ready 1 0)
           (if my/remote-key-to-screen--popup-tooltip 1 0)))
  (condition-case error-data
      (progn
        (kill-emacs 0)
        (my/remote-key-to-screen--report "EXIT-RETURNED"))
    (error
     (my/remote-key-to-screen--report
      (concat "EXIT-ERROR " (error-message-string error-data))))))

(defun my/remote-key-to-screen--run ()
  "Open a real Python LSP buffer and signal the PTY driver when ready."
  (condition-case error-data
      (progn
        (remote-config-load)
        (remote-fs-install)
        (when (equal (getenv "REMOTE_KEY_SCREEN_COPILOT") "0")
          (setq my/copilot-disable-on-remote t))
        (let* ((target (or (getenv "REMOTE_KEY_SCREEN_TARGET") "local"))
               (lsp-enabled
                (not (equal (getenv "REMOTE_KEY_SCREEN_LSP") "0")))
               (file (if (equal target "remote")
                         (or (getenv "REMOTE_KEY_SCREEN_FILE")
                             (error "REMOTE_KEY_SCREEN_FILE is missing"))
                       (setq my/remote-key-to-screen--local-directory
                             (make-temp-file "emacs-key-screen-" t))
                       (make-directory
                        (expand-file-name ".git"
                                          my/remote-key-to-screen--local-directory))
                       (setq my/remote-key-to-screen--local-file
                             (expand-file-name
                              "source.py"
                              my/remote-key-to-screen--local-directory))
                       (with-temp-buffer
                         (insert "name = 42\n")
                         (write-region (point-min) (point-max)
                                       my/remote-key-to-screen--local-file
                                       nil 'silent))
                       my/remote-key-to-screen--local-file))
               (visit-file
                (if (equal target "logical-local")
                    (remote-make-file-name "local" file)
                  file)))
          (unless lsp-enabled
            (setq my/language-server-disabled-modes
                  '(python-mode python-ts-mode)))
          (unless (file-readable-p visit-file)
            (error "Python source is not readable: %s" visit-file))
          (setq my/remote-key-to-screen--buffer
                (find-file-noselect visit-file))
          (switch-to-buffer my/remote-key-to-screen--buffer)
          (unless (derived-mode-p 'python-mode 'python-ts-mode)
            (error "Expected Python mode for %s" visit-file))
          (when lsp-enabled
            (setq-local lsp-auto-guess-root t
                        lsp-guess-root-without-session t
                        my/language-server--manual-start t)
            (my/language-server-ensure)
            (when (and (boundp 'lsp--buffer-deferred)
                       lsp--buffer-deferred
                       (fboundp 'lsp--init-if-visible))
              (lsp--init-if-visible))
            (let ((deadline (+ (float-time) 60))
                  workspace)
              (while (and (< (float-time) deadline)
                          (not (setq workspace
                                     (seq-find
                                      (lambda (candidate)
                                        (and (eq (lsp--workspace-status candidate)
                                                 'initialized)
                                             (eq (my/language-server--lsp-workspace-id
                                                  candidate)
                                                 'my-python)))
                                      (lsp-workspaces)))))
                (accept-process-output nil 0.05))
              (unless workspace (error "Python LSP initialization timed out"))
              (let ((deadline (+ (float-time) 8)))
                (while (and (< (float-time) deadline)
                            (not (eq
                                  'ready
                                  (plist-get
                                   (gethash workspace
                                            my/lsp-python-completion--prewarm-state)
                                   :state))))
                  (accept-process-output nil 0.05)))
            ;; Both /fs: and direct /rpc: visits must get the same bounded
            ;; completion and target-only breadcrumb policies.  A package
            ;; upgrade that detaches either spelling should fail this probe.
            (when (and (equal target "remote")
                       (not (ignore-errors
                              (remote-client-file-name visit-file))))
              (unless (bound-and-true-p my/lsp-remote-change--eligible)
                (error "Target-only LSP completion guard is inactive"))
              (unless (equal lsp-headerline-breadcrumb-segments
                             '(file symbols))
                (error "Target-only breadcrumb still probes project root")))))
          (unless lsp-enabled
            (when (bound-and-true-p lsp-managed-mode)
              (error "LSP unexpectedly managed the no-LSP probe"))
            (unless (bound-and-true-p breadcrumb-local-mode)
              (error "Generic breadcrumb is inactive in the no-LSP probe")))
          (when (and lsp-enabled
                     (or my/remote-key-to-screen--popup-scenario
                         (> (string-to-number
                             (or (getenv "REMOTE_KEY_SCREEN_INTERKEY_MS") "0"))
                            0)))
            (unless (bound-and-true-p company-mode)
              (error "Company is inactive in the completion probe"))
            (advice-add 'lsp-request-async :around
                        #'my/remote-key-to-screen--count-completion))
          (goto-char (point-max))
          (insert "\nkey_screen_value = ")
          (when (fboundp 'evil-insert-state) (evil-insert-state))
          (add-hook 'post-self-insert-hook
                    #'my/remote-key-to-screen--after-key nil t)
          (redisplay t)
          (my/remote-key-to-screen--report "READY")))
    (error
     (my/remote-key-to-screen--report
      (concat "ERROR " (error-message-string error-data)))
     (run-at-time 0.1 nil #'kill-emacs 1))))

(add-hook 'kill-emacs-hook #'my/remote-key-to-screen--cleanup)
(run-at-time 0 nil #'my/remote-key-to-screen--run)

(provide 'remote-key-to-screen)
;;; remote-key-to-screen.el ends here

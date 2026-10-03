;;; init-copilot-tests.el --- Copilot placement tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-copilot)

(ert-deftest my/copilot-remote-buffer-is-eligible-when-client-binary-is-enabled ()
  (let ((my/copilot-disable-on-remote nil)
        (default-directory "/fs:remote:/srv/project/")
        (buffer-read-only nil)
        (my/copilot-large-buffer-threshold nil))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest _args) t)))
      (should (my/copilot-buffer-eligible-p)))))

(ert-deftest my/copilot-remote-auto-start-waits-for-idle ()
  (let ((my/copilot-defer-on-remote t)
        (my/copilot-deferred-modes nil)
        (my/copilot-deferred-idle-delay 1.5)
        (my/copilot--auto-enable-timer nil)
        scheduled available mode-starts)
    (with-temp-buffer
      (setq default-directory "/fs:remote:/srv/project/")
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (delay repeat function buffer)
                   (setq scheduled (list delay repeat function buffer))
                   'scheduled))
                ((symbol-function 'my/copilot-available-p)
                 (lambda () (setq available t)))
                ((symbol-function 'copilot-mode)
                 (lambda (_arg) (setq mode-starts (1+ (or mode-starts 0))))))
        (my/copilot-auto-enable-h)
        (should-not available)
        (should-not mode-starts)
        (should (equal (car scheduled) 1.5))
        (should (eq (nth 2 scheduled) #'my/copilot--enable-buffer))
        (should (eq (nth 3 scheduled) (current-buffer)))
        (funcall (nth 2 scheduled) (nth 3 scheduled))
        (should available)
        (should (= mode-starts 1))))
    (setq available nil)
    (with-temp-buffer
      (setq default-directory "/tmp/")
      (let ((features (cons 'copilot features)))
        (cl-letf (((symbol-function 'file-remote-p)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'my/copilot-available-p)
                   (lambda () (setq available t)))
                  ((symbol-function 'copilot-mode)
                   (lambda (_arg) (setq mode-starts (1+ (or mode-starts 0))))))
          (my/copilot-auto-enable-h)
          (should available)
          (should (= mode-starts 2)))))))

(ert-deftest my/copilot-disconnect-stops-buffer-and-cancels-deferred-start ()
  (with-temp-buffer
    (let (mode-args)
      (setq-local copilot-mode t
                  my/copilot--auto-enable-timer
                  (run-with-idle-timer 30 nil #'my/copilot--enable-buffer
                                       (current-buffer)))
      (let ((timer my/copilot--auto-enable-timer))
        (cl-letf (((symbol-function 'copilot-mode)
                   (lambda (arg) (push arg mode-args)))
                  ((symbol-function 'my/copilot-available-p)
                   (lambda () (ert-fail "Disconnected buffer started Copilot")))
                  ((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _) (ert-fail "Disconnected buffer queued startup"))))
          (setq-local remote-buffer-disconnected-p t)
          (my/copilot--disconnect-buffer-h)
          (should (equal mode-args '(-1)))
          (should-not my/copilot--auto-enable-timer)
          (should-not (memq timer timer-idle-list))
          (should-not (my/copilot-buffer-eligible-p))
          (apply (timer--function timer) (timer--args timer))
          (my/copilot-auto-enable-h)
          (should (equal mode-args '(-1))))))))

(ert-deftest my/copilot-cold-local-auto-start-waits-for-short-idle ()
  "The first native source visit must not wait for Copilot startup."
  (with-temp-buffer
    (let ((features (delq 'copilot (copy-sequence features)))
          (my/copilot-cold-local-idle-delay 0.75)
          (my/copilot-deferred-modes nil)
          scheduled enabled)
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (delay repeat function buffer)
                   (setq scheduled (list delay repeat function buffer))
                   'scheduled))
                ((symbol-function 'my/copilot-available-p)
                 (lambda () t))
                ((symbol-function 'copilot-mode)
                 (lambda (_arg) (setq enabled t))))
        (my/copilot-auto-enable-h)
        (should (equal (car scheduled) 0.75))
        (should-not enabled)
        (funcall (nth 2 scheduled) (nth 3 scheduled))
        (should enabled)))))

(ert-deftest my/copilot-first-package-load-stays-on-client ()
  "Cold Copilot Lisp must load outside a target buffer's environment."
  (with-temp-buffer
    (let ((default-directory "/fs:remote:/srv/project/")
          (process-environment '("TARGET=1"))
          (exec-path '("/target/bin"))
          (remote-current-adapter-id "test-target")
          observed)
      (cl-letf (((symbol-function 'remote-client-process-environment)
                 (lambda () '("CLIENT=1")))
                ((symbol-function 'remote-client-exec-path)
                 (lambda () '("/client/bin"))))
        (my/copilot--call-on-client
         (lambda ()
           (setq observed
                 (list default-directory process-environment
                       exec-path remote-current-adapter-id)))))
      (should (equal observed
                     (list temporary-file-directory '("CLIENT=1")
                           '("/client/bin") nil))))))

(ert-deftest my/copilot-connection-spawns-through-client-placement ()
  (let ((default-directory "/fs:remote:/srv/project/")
        (my/copilot-server-max-heap-mb 1024)
        captured)
    (cl-letf
        (((symbol-function 'remote-client-process-environment)
          (lambda () '("PATH=/client/bin" "NODE_OPTIONS=--trace-warnings")))
         ((symbol-function 'remote-client-exec-path)
          (lambda () '("/client/bin")))
         ((symbol-function 'copilot--command)
          (lambda () '("/client/bin/copilot-language-server")))
         ((symbol-function 'remote-make-client-process)
          (lambda (&rest plist)
            (setq captured plist)
            'client-copilot-process)))
      (should
       (eq
        (my/copilot--make-client-process)
        'client-copilot-process))
      (should
       (equal
        (plist-get captured :command)
        '("/client/bin/copilot-language-server")))
      (should
       (equal
        (plist-get captured :remote-client-directory)
        temporary-file-directory))
      (should
       (equal
        (plist-get captured :remote-client-exec-path)
        '("/client/bin")))
      (should
       (member
        "NODE_OPTIONS=--trace-warnings --max-old-space-size=1024"
        (plist-get captured :remote-client-environment))))))

(ert-deftest my/copilot-exit-skips-sync-rpc-for-client-owned-process ()
  "An editor-owned Copilot child must not abort `kill-emacs' while stopping."
  (let ((process
         (make-pipe-process
          :name "copilot-exit-test" :command '("cat") :noquery t))
        (copilot--connection 'test-connection)
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'jsonrpc--process)
                   (lambda (_connection) process)))
          (should-not
           (my/copilot--nonblocking-exit-a
            (lambda (&rest _arguments) (setq called t))))
          (should-not called)
          (set-process-query-on-exit-flag process t)
          (my/copilot--nonblocking-exit-a
           (lambda (&rest _arguments) (setq called t)))
          (should called))
      (delete-process process)))
  (require 'copilot)
  (should (advice-member-p #'my/copilot--nonblocking-exit-a
                           'copilot--shutdown-server-at-exit)))

(ert-deftest my/copilot-server-executable-resolves-in-client-environment ()
  (let ((default-directory "/fs:remote:/srv/project/")
        seen)
    (cl-letf
        (((symbol-function 'remote-client-process-environment)
          (lambda () '("PATH=/client/bin")))
         ((symbol-function 'remote-client-exec-path)
          (lambda () '("/client/bin"))))
      (should
       (equal
        (my/copilot--client-server-executable-a
         (lambda ()
           (setq seen
                 (list
                  default-directory
                  process-environment
                  exec-path))
           "/client/bin/copilot-language-server"))
        "/client/bin/copilot-language-server"))
      (should (equal (car seen) temporary-file-directory))
      (should (equal (cadr seen) '("PATH=/client/bin")))
      (should (equal (caddr seen) '("/client/bin"))))))

(ert-deftest my/copilot-leaves-tab-to-company-and-indent ()
  "Copilot's overlay must not outrank the editor's normal TAB routing."
  (require 'copilot)
  (should-not (lookup-key copilot-completion-map (kbd "TAB")))
  (should-not (lookup-key copilot-completion-map (kbd "<tab>")))
  (should
   (eq (lookup-key copilot-completion-map (kbd "s-]"))
       #'my/forward-delimiter-or-copilot-dwim))
  (should
   (eq (lookup-key global-map (kbd "s-]"))
       #'my/forward-delimiter-or-copilot-dwim)))

(ert-deftest my/copilot-jump-labels-are-prefix-free ()
  (let ((labels (my/copilot--jump-labels 80)))
    (should (= (length labels) 80))
    (dolist (left labels)
      (dolist (right labels)
        (unless (equal left right)
          (should-not (string-prefix-p left right)))))))

(ert-deftest my/copilot-jump-accepts-the-labelled-prefix ()
  (let ((events (list ?s))
        rendered
        accepted)
    (cl-letf (((symbol-function 'copilot-current-completion)
               (lambda () "Alpha"))
              ((symbol-function 'copilot--get-overlay)
               (lambda () 'fake-overlay))
              ((symbol-function 'copilot--set-overlay-text)
               (lambda (_overlay text) (setq rendered text)))
              ((symbol-function 'copilot--overlay-visible)
               (lambda () t))
              ((symbol-function 'read-event)
               (lambda (&rest _args) (pop events)))
              ((symbol-function 'copilot-accept-completion)
               (lambda (transform)
                 (setq accepted (funcall transform "Alpha"))
                 t)))
      (should (my/copilot-accept-completion-jump))
      ;; Five targets receive a/s/d/f/g, so `s' selects through the second char.
      (should (equal accepted "Al"))
      (should (stringp rendered)))))

(provide 'init-copilot-tests)
;;; init-copilot-tests.el ends here

;;; init-copilot-tests.el --- Copilot placement tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tools/copilot/config)

(ert-deftest +copilot-remote-buffer-is-eligible-when-client-binary-is-enabled ()
  (let ((+copilot-disable-on-remote nil)
        (default-directory "/fs:remote:/srv/project/")
        (buffer-read-only nil)
        (+copilot-large-buffer-threshold nil))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (&rest _args) t)))
      (should (+copilot-buffer-eligible-p)))))

(ert-deftest +copilot-remote-auto-start-waits-for-idle ()
  (let ((+copilot-defer-on-remote t)
        (+copilot-deferred-modes nil)
        (+copilot-deferred-idle-delay 1.5)
        (+copilot--auto-enable-timer nil)
        scheduled available mode-starts)
    (with-temp-buffer
      (setq default-directory "/fs:remote:/srv/project/")
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (delay repeat function buffer)
                   (setq scheduled (list delay repeat function buffer))
                   'scheduled))
                ((symbol-function '+copilot-available-p)
                 (lambda () (setq available t)))
                ((symbol-function 'copilot-mode)
                 (lambda (_arg) (setq mode-starts (1+ (or mode-starts 0))))))
        (+copilot-auto-enable-h)
        (should-not available)
        (should-not mode-starts)
        (should (equal (car scheduled) 1.5))
        (should (eq (nth 2 scheduled) #'+copilot--enable-buffer))
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
                  ((symbol-function '+copilot-available-p)
                   (lambda () (setq available t)))
                  ((symbol-function 'copilot-mode)
                   (lambda (_arg) (setq mode-starts (1+ (or mode-starts 0))))))
          (+copilot-auto-enable-h)
          (should available)
          (should (= mode-starts 2)))))))

(ert-deftest +copilot-disconnect-stops-buffer-and-cancels-deferred-start ()
  (with-temp-buffer
    (let (mode-args)
      (setq-local copilot-mode t
                  +copilot--auto-enable-timer
                  (run-with-idle-timer 30 nil #'+copilot--enable-buffer
                                       (current-buffer)))
      (let ((timer +copilot--auto-enable-timer))
        (cl-letf (((symbol-function 'copilot-mode)
                   (lambda (arg) (push arg mode-args)))
                  ((symbol-function '+copilot-available-p)
                   (lambda () (ert-fail "Disconnected buffer started Copilot")))
                  ((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _) (ert-fail "Disconnected buffer queued startup"))))
          (setq-local remote-buffer-disconnected-p t)
          (+copilot--disconnect-buffer-h)
          (should (equal mode-args '(-1)))
          (should-not +copilot--auto-enable-timer)
          (should-not (memq timer timer-idle-list))
          (should-not (+copilot-buffer-eligible-p))
          (apply (timer--function timer) (timer--args timer))
          (+copilot-auto-enable-h)
          (should (equal mode-args '(-1))))))))

(ert-deftest +copilot-cold-local-auto-start-waits-for-short-idle ()
  "The first native source visit must not wait for Copilot startup."
  (with-temp-buffer
    (let ((features (delq 'copilot (copy-sequence features)))
          (+copilot-cold-local-idle-delay 0.75)
          (+copilot-deferred-modes nil)
          scheduled enabled)
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (delay repeat function buffer)
                   (setq scheduled (list delay repeat function buffer))
                   'scheduled))
                ((symbol-function '+copilot-available-p)
                 (lambda () t))
                ((symbol-function 'copilot-mode)
                 (lambda (_arg) (setq enabled t))))
        (+copilot-auto-enable-h)
        (should (equal (car scheduled) 0.75))
        (should-not enabled)
        (funcall (nth 2 scheduled) (nth 3 scheduled))
        (should enabled)))))

(ert-deftest +copilot-first-package-load-stays-on-client ()
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
        (+copilot--call-on-client
         (lambda ()
           (setq observed
                 (list default-directory process-environment
                       exec-path remote-current-adapter-id)))))
      (should (equal observed
                     (list temporary-file-directory '("CLIENT=1")
                           '("/client/bin") nil))))))

(ert-deftest +copilot-connection-spawns-through-client-placement ()
  (let ((default-directory "/fs:remote:/srv/project/")
        (+copilot-server-max-heap-mb 1024)
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
        (+copilot--make-client-process)
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

(ert-deftest +copilot-exit-skips-sync-rpc-for-client-owned-process ()
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
           (+copilot--nonblocking-exit-a
            (lambda (&rest _arguments) (setq called t))))
          (should-not called)
          (set-process-query-on-exit-flag process t)
          (+copilot--nonblocking-exit-a
           (lambda (&rest _arguments) (setq called t)))
          (should called))
      (delete-process process)))
  (require 'copilot)
  (should (advice-member-p #'+copilot--nonblocking-exit-a
                           'copilot--shutdown-server-at-exit)))

(ert-deftest +copilot-server-executable-resolves-in-client-environment ()
  (let ((default-directory "/fs:remote:/srv/project/")
        seen)
    (cl-letf
        (((symbol-function 'remote-client-process-environment)
          (lambda () '("PATH=/client/bin")))
         ((symbol-function 'remote-client-exec-path)
          (lambda () '("/client/bin"))))
      (should
       (equal
        (+copilot--client-server-executable-a
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

(ert-deftest +copilot-leaves-tab-to-company-and-indent ()
  "Copilot's overlay must not outrank the editor's normal TAB routing."
  (require 'copilot)
  (should-not (lookup-key copilot-completion-map (kbd "TAB")))
  (should-not (lookup-key copilot-completion-map (kbd "<tab>")))
  (should
   (eq (lookup-key copilot-completion-map (kbd "s-]"))
       #'+copilot/forward-delimiter-or-copilot-dwim))
  (should
   (eq (lookup-key global-map (kbd "s-]"))
       #'+copilot/forward-delimiter-or-copilot-dwim)))

(ert-deftest +copilot-jump-labels-are-prefix-free ()
  (let ((labels (+copilot--jump-labels 80)))
    (should (= (length labels) 80))
    (dolist (left labels)
      (dolist (right labels)
        (unless (equal left right)
          (should-not (string-prefix-p left right)))))))

(ert-deftest +copilot-jump-accepts-the-labelled-prefix ()
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
      (should (+copilot/accept-completion-jump))
      ;; Five targets receive a/s/d/f/g, so `s' selects through the second char.
      (should (equal accepted "Al"))
      (should (stringp rendered)))))

(provide 'init-copilot-tests)
;;; init-copilot-tests.el ends here

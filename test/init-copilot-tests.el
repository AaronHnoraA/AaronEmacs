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

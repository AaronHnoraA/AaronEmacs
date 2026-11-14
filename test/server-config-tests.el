;;; server-config-tests.el --- Emacs server reply contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'server)
(require 'cl-lib)

(ert-deftest server-config-reply-to-a-gone-client-is-quiet ()
  "A launcher's emacsclient times out and leaves; its late answer is not an error."
  (should (advice-member-p '+server-send-to-gone-client-a 'server-send-string))
  (let ((process (make-pipe-process :name "server-config-test" :noquery t)))
    (delete-process process)
    (should-not (condition-case nil (progn (server-send-string process "-print x\n") nil)
                  (error t)))))

(ert-deftest server-config-reply-to-a-live-client-is-sent ()
  (let (sent)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) (lambda (_ s) (setq sent s))))
      (server-send-string 'client "-print y\n")
      (should (equal sent "-print y\n")))))

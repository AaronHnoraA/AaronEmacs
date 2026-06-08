;;; init-aaronnote-tests.el --- Aaronnote bridge tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-aaronnote)

(ert-deftest my/aaronnote-run-emacs-key-queues-m-x ()
  (let ((buffer (generate-new-buffer "*aaronnote-test*"))
        (unread-command-events nil)
        (my/aaronnote--app-buffer nil)
        focused-frame
        scheduled-fn)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (setq my/aaronnote--app-buffer buffer)
          (cl-letf (((symbol-function 'select-frame-set-input-focus)
                     (lambda (frame) (setq focused-frame frame)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest _args)
                       (setq scheduled-fn function)
                       'mock-timer)))
            (my/aaronnote--run-emacs-key "M-x"))
          (should (equal unread-command-events
                         (listify-key-sequence (kbd "M-x"))))
          (should (eq (selected-window) (get-buffer-window buffer 'visible)))
          (should (eq focused-frame (selected-frame)))
          (should (eq scheduled-fn #'my/aaronnote--focus-minibuffer-if-active)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/aaronnote-run-emacs-key-queues-prefix-sequence ()
  (let ((unread-command-events nil)
        (my/aaronnote--app-buffer nil)
        scheduled-fn)
    (cl-letf (((symbol-function 'select-frame-set-input-focus)
               (lambda (_frame) nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest _args)
                 (setq scheduled-fn function)
                 'mock-timer)))
      (my/aaronnote--run-emacs-key "C-x C-f"))
    (should (equal unread-command-events
                   (listify-key-sequence (kbd "C-x C-f"))))
    (should (eq scheduled-fn #'my/aaronnote--focus-minibuffer-if-active))))

(provide 'init-aaronnote-tests)
;;; init-aaronnote-tests.el ends here

;;; remote-gui-company-popup.el --- Real GUI Python Company popup probe -*- lexical-binding: t; -*-

;; Load in a graphical Emacs frame after init.el, then evaluate
;; (my/remote-gui-company-popup-run "/rpc:host:/path/to/a.py").
;; The source file is visited and edited only in memory; it is never saved.

(require 'init-lsp)
(require 'remote-config)
(require 'remote-framework)
(require 'company)

(when (equal (getenv "REMOTE_GUI_COMPANY_FAST_IDLE") "0")
  (setq my/lsp-completion-ready-idle-delay nil))

(defun my/remote-gui-company-popup-run
    (file &optional request-rounds stall-first-completion automatic-input
          idle-delay)
  "Verify Company's `print' child frame while visiting Python FILE.
With REQUEST-ROUNDS, also time fresh server completion requests in this buffer.
With STALL-FIRST-COMPLETION, drop the first automatic completion reply.
With AUTOMATIC-INPUT, execute `prin' as editor commands and await idle Company.
IDLE-DELAY overrides the captured Python Company delay for this probe only."
  (unless (display-graphic-p)
    (error "A graphical frame is required"))
  (remote-config-load)
  (remote-fs-install)
  (unless (file-readable-p file)
    (error "Python source is not readable: %s" file))
  (let ((buffer (find-file-noselect file))
        result)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (unless (derived-mode-p 'python-mode 'python-ts-mode)
              (error "Expected Python mode for %s" file))
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
                          (not (and (bound-and-true-p lsp-managed-mode)
                                    (setq workspace
                                          (seq-find
                                           (lambda (candidate)
                                             (and (eq (lsp--workspace-status candidate)
                                                      'initialized)
                                                  (eq (my/language-server--lsp-workspace-id
                                                       candidate)
                                                      'my-python)))
                                           (lsp-workspaces))))))
                (accept-process-output nil 0.05))
              (unless workspace
                (error "Python LSP initialization timed out"))
              (let ((deadline (+ (float-time) 8)))
                (while (and (< (float-time) deadline)
                            (not (eq 'ready
                                     (plist-get
                                      (gethash
                                       workspace
                                       my/lsp-completion--prewarm-state)
                                      :state))))
                  (accept-process-output nil 0.05)))
              (unless (eq 'ready
                          (plist-get
                           (gethash
                            workspace
                            my/lsp-completion--prewarm-state)
                           :state))
                (error "Python completion prewarm did not become ready")))
            (when (and (numberp idle-delay) (> idle-delay 0))
              (setq-local my/lsp-completion--normal-idle-delay
                          idle-delay))
            (unless (and (bound-and-true-p company-mode)
                         (bound-and-true-p company-box-mode))
              (error "Company or company-box is inactive in the GUI buffer"))
            (goto-char (point-max))
            (insert (if automatic-input
                        "\nkey_screen_value = "
                      "\nkey_screen_value = prin"))
            (redisplay t)
            (let* ((started (float-time))
                   (window (selected-window))
                   (request-fn (symbol-function 'lsp-request-async))
                   (display-fn (symbol-function 'company-box--display))
                   request-ms box-ms ready-at dropped
                   request-start-at request-end-at box-start-at box-end-at
                   company-return-at)
              (cl-letf (((symbol-function 'lsp-request-async)
                         (lambda (method params callback &rest options)
                           (if (and stall-first-completion
                                    (not dropped)
                                    (equal method "textDocument/completion"))
                               (setq dropped t)
                             (if (equal method "textDocument/completion")
                               (let ((sent-at (float-time)))
                                 (unless request-start-at
                                   (setq request-start-at sent-at))
                                 (apply request-fn method params
                                        (lambda (&rest reply)
                                          (setq request-end-at (float-time))
                                          (push (* 1000
                                                   (- (float-time) sent-at))
                                                request-ms)
                                          (apply callback reply))
                                        options))
                               (apply request-fn method params callback
                                      options)))))
                        ((symbol-function 'company-box--display)
                         (lambda (&rest arguments)
                           (let ((render-at (float-time)))
                             (unless box-start-at
                               (setq box-start-at render-at))
                             (prog1 (apply display-fn arguments)
                               (setq box-end-at (float-time))
                               (push (* 1000
                                        (- (float-time) render-at))
                                     box-ms))))))
                (if automatic-input
                    (progn
                      (when (fboundp 'evil-insert-state)
                        (evil-insert-state))
                      (execute-kbd-macro "prin")
                      (let ((deadline (+ (float-time) 8)))
                        (while (and (< (float-time) deadline)
                                    (not (and
                                          (bound-and-true-p company-candidates)
                                          (or (eq company-backend 'company-capf)
                                              (and (listp company-backend)
                                                   (memq 'company-capf
                                                         company-backend)))
                                          (seq-some
                                           (lambda (candidate)
                                             (and (stringp candidate)
                                                  (string= candidate "print")))
                                           company-candidates)
                                          (let ((child
                                                 (company-box--get-frame)))
                                            (and child
                                                 (frame-visible-p child))))))
                          (sit-for 0.05)))
                      (setq company-return-at (float-time))
                      (redisplay t)
                      (setq ready-at (float-time)))
                  (company-idle-begin buffer window
                                      (buffer-chars-modified-tick) (point))
                  (setq company-return-at (float-time))
                  (redisplay t)
                  (setq ready-at (float-time))))
              (when (and stall-first-completion (not automatic-input))
                (unless dropped
                  (error "Fault injection missed the first completion request"))
                (let ((deadline (+ (float-time) 8)))
                  (while (and (< (float-time) deadline)
                              (not (and (bound-and-true-p company-candidates)
                                        (let ((child
                                               (company-box--get-frame)))
                                          (and child
                                               (frame-visible-p child))))))
                    (sit-for 0.05)))
                (setq ready-at (float-time)))
              (sit-for 0.05)
              (let* ((child (and (fboundp 'company-box--get-frame)
                                 (company-box--get-frame)))
                     (typed-p (and (>= (- (point) (point-min)) 4)
                                   (equal (buffer-substring-no-properties
                                           (- (point) 4) (point))
                                          "prin")))
                     (candidate-p (seq-some
                                   (lambda (candidate)
                                     (and (stringp candidate)
                                          (string= candidate "print")))
                                   company-candidates))
                     (popup-buffer (and child
                                        (window-buffer (frame-root-window child)))))
                (setq result
                      (list :file file
                            :typed-prin (and typed-p t)
                            :candidate-print (and candidate-p t)
                            :backend company-backend
                            :company-box-mode company-box-mode
                            :child-frame-visible
                            (and child (frame-visible-p child) t)
                            :child-frame-parent
                            (and child (eq (frame-parent child)
                                           (selected-frame)))
                            :popup-text-print
                            (and popup-buffer
                                 (with-current-buffer popup-buffer
                                   (save-excursion
                                     (goto-char (point-min))
                                     (search-forward "print" nil t)))
                                 t)
                            :candidate-ready-ms
                            (* 1000 (- ready-at started))
                            :automatic-input (and automatic-input t)
                            :configured-idle-delay
                            my/lsp-completion--normal-idle-delay
                            :effective-idle-delay
                            (if (functionp company-idle-delay)
                                (funcall company-idle-delay)
                              company-idle-delay)
                            :before-request-ms
                            (and request-start-at
                                 (* 1000 (- request-start-at started)))
                            :response-to-box-ms
                            (and request-end-at box-start-at
                                 (* 1000 (- box-start-at request-end-at)))
                            :after-box-ms
                            (and box-end-at
                                 (* 1000 (- ready-at box-end-at)))
                            :after-box-company-ms
                            (and box-end-at company-return-at
                                 (* 1000 (- company-return-at box-end-at)))
                            :redisplay-ms
                            (and company-return-at
                                 (* 1000 (- ready-at company-return-at)))
                            :dropped-first (and dropped t)
                            :lsp-response-ms (nreverse request-ms)
                            :box-render-ms (nreverse box-ms)))
                (unless (and (plist-get result :typed-prin)
                             (plist-get result :candidate-print)
                             (or (eq company-backend 'company-capf)
                                 (and (listp company-backend)
                                      (memq 'company-capf company-backend)))
                             (plist-get result :child-frame-visible)
                             (plist-get result :child-frame-parent)
                             (plist-get result :popup-text-print))
                  (setq result
                        (append
                         result
                         (list
                          :prewarm-state
                          (plist-get
                           (gethash
                            (car (lsp-workspaces))
                            my/lsp-completion--prewarm-state)
                           :state)
                          :lsp-managed lsp-managed-mode
                          :should-begin
                          (and (fboundp 'company--should-begin)
                               (company--should-begin))
                          :company-idle-delay company-idle-delay
                          :normal-idle-delay
                          my/lsp-completion--normal-idle-delay)))
                  (error "GUI Company popup did not show `print': %S" result))
                (when (and (integerp request-rounds)
                           (> request-rounds 0))
                  (let (samples)
                    (dotimes (_ request-rounds)
                      (let* ((params
                              (plist-put
                               (lsp--text-document-position-params)
                               :context
                               (lsp-completion--get-context nil nil)))
                             (requested-at (float-time))
                             (reply (lsp-request
                                     "textDocument/completion" params)))
                        (push (list :ms (* 1000
                                           (- (float-time) requested-at))
                                    :items (length
                                            (if (lsp-completion-list? reply)
                                                (lsp:completion-list-items reply)
                                              reply)))
                              samples)))
                    (setq result (plist-put result :server-requests
                                            (vconcat (nreverse samples))))))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (fboundp 'company-abort) (company-abort))
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    result))

(provide 'remote-gui-company-popup)
;;; remote-gui-company-popup.el ends here

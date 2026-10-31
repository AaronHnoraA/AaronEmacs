;;; agent-runtime-tests.el --- Shared runtime and UI contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/sessions)
(require 'tools/agent/context)
(require 'tools/agent/compose)
(require 'consult)

(defmacro agent-runtime-tests--buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (setq-local +agent-acp-session-root temporary-file-directory
                 +agent-acp-session-name "review" +agent-acp-session-agent "codex"
                 agent-shell--state (agent-shell--make-state :buffer (current-buffer)
                                                             :agent-config '((:identifier . codex))))
     (let ((+agent-session-action-functions nil) (+agent-session-id-functions nil)
           (+agent-session-menu-functions nil) (+agent-session-attention-functions nil))
       ,@body)))

(ert-deftest agent-runtime-c1-rename-does-not-load-a-domain-service ()
  (agent-runtime-tests--buffer
    (let ((+agent-session-forget-functions nil))
      (cl-letf (((symbol-function '+agent-acp--project-buffers) (lambda (&rest _) nil)))
        (+agent-session-rename (current-buffer) "renamed")
        (should (equal +agent-acp-session-name "renamed"))
        (should (equal shell-maker--buffer-name-override (buffer-name)))))))

(ert-deftest agent-runtime-c3-fork-requires-a-positive-capability ()
  (agent-runtime-tests--buffer
    (let (forked)
      (cl-letf (((symbol-function 'agent-shell-fork) (lambda () (interactive) (setq forked t))))
        (setf (map-elt agent-shell--state :supports-session-fork) :false)
        (should-error (+agent-session-fork (current-buffer)) :type 'user-error)
        (should-not forked)
        (setf (map-elt agent-shell--state :supports-session-fork) t)
        (+agent-session-fork (current-buffer))
        (should forked)))))

(ert-deftest agent-runtime-c10-domain-dispatch-is-an-optional-extension ()
  (agent-runtime-tests--buffer
    (let ((+agent-session-action-functions (list (lambda (operation _buffer &rest _) (eq operation 'restart))))
          restarted)
      (cl-letf (((symbol-function 'agent-shell-restart) (lambda (&rest _) (setq restarted t))))
        (+agent-session-restart (current-buffer))
        (should-not restarted)
        (let ((+agent-session-action-functions nil))
          (+agent-session-restart (current-buffer))
          (should restarted))))))

(ert-deftest agent-runtime-c3-dead-process-is-not-idle ()
  (agent-runtime-tests--buffer
    (let ((process (make-pipe-process :name "agent-runtime-test" :noquery t)))
      (delete-process process)
      (setf (map-elt agent-shell--state :client) (list (cons :process process)))
      (let ((snapshot (+agent-session-snapshot (current-buffer))))
        (should (eq (plist-get snapshot :process-state) 'dead))
        (should (equal (+agent-sessions--state snapshot) "dead"))))))

(ert-deftest agent-runtime-c10-ordinary-compose-reuses-gptel ()
  (with-temp-buffer
    (let ((+agent-editor-action-functions nil) sent)
      (cl-letf (((symbol-function 'gptel-send) (lambda (arg) (setq sent arg))))
        (+agent/compose-send 'explicit)
        (should (eq sent 'explicit))))))

(ert-deftest agent-runtime-c10-domain-editor-is-explicitly-adapted ()
  (let* (seen
         (+agent-editor-action-functions
          (list (lambda (operation &rest _) (push operation seen) t))))
    (cl-letf (((symbol-function 'gptel-rewrite) (lambda () (ert-fail "Domain editor handled the action"))))
      (+agent/compose-rewrite)
      (should (equal seen '(rewrite))))))

(ert-deftest agent-runtime-c5-cancelled-peek-restores-buffer-and-point ()
  (save-window-excursion
    (let ((original (generate-new-buffer " *agent-peek-original*"))
          (preview (generate-new-buffer " *agent-peek-preview*")) visited)
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) original)
            (with-current-buffer original (insert "one\ntwo\nthree\n"))
            (set-window-point (selected-window) 5)
            (cl-letf (((symbol-function '+agent-acp-sessions)
                       (lambda (&rest _) (list (list :buffer preview :name "review" :agent "codex" :root "/p/"))))
                      ((symbol-function '+agent-render-flush) #'ignore)
                      ((symbol-function '+agent-acp-show-buffer) (lambda (_) (setq visited t)))
                      ((symbol-function 'consult--read)
                       (lambda (choices &rest args)
                         (funcall (plist-get args :state) 'preview (car choices))
                         (should (eq (window-buffer) preview))
                         (should +agent-acp-previewing)
                         (signal 'quit nil))))
              (condition-case nil (+agent/peek) (quit nil)))
            (should (eq (window-buffer) original))
            (should (= (window-point) 5))
            (should-not visited))
        (kill-buffer original) (kill-buffer preview)))))

(ert-deftest agent-runtime-c5-confirmed-peek-restores-before-visiting ()
  (save-window-excursion
    (let ((original (window-buffer)) (preview (generate-new-buffer " *agent-peek-target*")) visited)
      (unwind-protect
          (cl-letf (((symbol-function '+agent-acp-sessions)
                     (lambda (&rest _) (list (list :buffer preview :name "review" :agent "codex" :root "/p/"))))
                    ((symbol-function '+agent-render-flush) #'ignore)
                    ((symbol-function '+agent-acp-show-buffer)
                     (lambda (buffer) (should (eq (window-buffer) original)) (setq visited buffer)))
                    ((symbol-function 'consult--read)
                     (lambda (choices &rest args)
                       (funcall (plist-get args :state) 'preview (car choices)) (car choices))))
            (+agent/peek)
            (should (eq visited preview)))
        (kill-buffer preview)))))


(ert-deftest agent-runtime-c6-reading-does-not-resolve-approval ()
  (agent-runtime-tests--buffer
    (setf (map-elt agent-shell--state :tool-calls)
          '(("tool" . ((:permission-request-id . 42)))))
    (setq +agent-acp-attention 'permission)
    (+agent-acp-clear-attention (current-buffer))
    (should-not +agent-acp-attention)
    (should (eq (plist-get (+agent-session-snapshot (current-buffer)) :attention-kind) 'permission))
    (should (equal (+agent-sessions--state (+agent-session-snapshot (current-buffer))) "approval"))))

(ert-deftest agent-runtime-c6-reading-does-not-erase-error-fact ()
  (agent-runtime-tests--buffer
    (setq +agent-acp-attention 'failed +agent-acp-last-error '((message . "provider failed")))
    (+agent-acp-clear-attention (current-buffer))
    (should-not +agent-acp-attention)
    (should (equal (plist-get (+agent-session-snapshot (current-buffer)) :last-error)
                   '((message . "provider failed"))))))

(ert-deftest agent-runtime-c3-fork-menu-uses-native-or-domain-capability ()
  (agent-runtime-tests--buffer
    (let ((+agent-session-operation-functions nil))
      (should-not (+agent-session-operation-supported-p 'fork (current-buffer)))
      (let ((+agent-session-operation-functions
             (list (lambda (operation _buffer) (eq operation 'fork)))))
        (should (+agent-session-operation-supported-p 'fork (current-buffer)))))))

(ert-deftest agent-runtime-c5-session-list-keys-survive-evil ()
  (require 'evil)
  (with-temp-buffer
    (+agent-sessions-mode)
    (evil-local-mode 1)
    (evil-change-to-initial-state)
    (should (eq evil-state 'emacs))
    (should (eq (key-binding "r") #'+agent-sessions-rename))
    (should (eq (key-binding "R") #'+agent-sessions-restart))
    (should (eq (key-binding "f") #'+agent-sessions-fork))
    (should (eq (key-binding "s") #'+agent-sessions-stop))))


(ert-deftest agent-runtime-c6-unread-request-is-not-a-pending-request ()
  (agent-runtime-tests--buffer
    (setq +agent-acp-attention 'permission)
    (let ((snapshot (+agent-session-snapshot (current-buffer))))
      (should (eq (plist-get snapshot :unread-kind) 'permission))
      (should-not (plist-get snapshot :attention-kind)))))

(provide 'agent-runtime-tests)

;;; command-shell-tests.el --- Contracts of the eshell command shell -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One test per contract in docs/design/command-shell.md.  Run with the full
;; configuration loaded (scripts/test-all).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'term/shell/eshell)
(require 'lang/cpp/config)
(require 'lang/go/config)

(defvar consult--buffer-display)
(defvar consult-buffer-filter)
(defvar consult-buffer-list-function)
(defvar consult-source-buffer)
(defvar consult-buffer-sources)

(defmacro command-shell-tests--with-dir (var &rest body)
  "Run BODY with VAR a fresh directory, killing the local eshell afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "command-shell-" t)))
         (kill-buffer-query-functions nil))
     (unwind-protect (progn ,@body)
       (when-let* ((buffer (get-buffer (+eshell-context-buffer-name ,var))))
         (kill-buffer buffer))
       (delete-directory ,var t))))

(defun command-shell-tests--wait (predicate &optional seconds)
  "Wait up to SECONDS (default 10) for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest command-shell-c1-buffer-is-per-context ()
  (command-shell-tests--with-dir dir
    (let ((sub (file-name-as-directory (expand-file-name "sub" dir))))
      (make-directory sub)
      (let ((a (+eshell-buffer dir))
            (b (+eshell-buffer sub)))
        (should (eq a b))
        (should (equal (file-truename (buffer-local-value 'default-directory b))
                       (file-truename sub)))))))

(ert-deftest command-shell-c1-named-tasks-have-separate-buffers ()
  "A task label gives automation an identifiable, independently queued buffer."
  (command-shell-tests--with-dir dir
    (let (first second result)
      (unwind-protect
          (progn
            (setq first (+eshell-run "*echo named" :directory dir
                                     :name "daily check"
                                     :callback (lambda (value) (setq result value)))
                  second (+eshell-buffer dir "build"))
            (should (equal (buffer-name first) "*eshell:local/daily-check*"))
            (should (equal (buffer-name second) "*eshell:local/build*"))
            (should-not (eq first second))
            (should (eq first (+eshell-buffer dir "daily check")))
            (should (command-shell-tests--wait (lambda () result)))
            (should (equal (plist-get result :name) "daily check"))
            (should (eql (plist-get result :status) 0)))
        (when (buffer-live-p first) (kill-buffer first))
        (when (buffer-live-p second) (kill-buffer second))))))

(ert-deftest command-shell-c9-generic-buffer-open-routes-to-float ()
  "Switching or visiting an Eshell buffer avoids replacing the current pane."
  (command-shell-tests--with-dir dir
    (let ((buffer (+eshell-buffer dir)) shown called)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _) t))
                ((symbol-function '+eshell--display)
                 (lambda (item) (setq shown item))))
        (should (eq (+eshell--switch-to-buffer-a
                     (lambda (&rest _) (setq called t)) buffer)
                    buffer))
        (should (eq shown buffer))
        (should-not called)
        (setq shown nil)
        (display-buffer buffer)
        (should (eq shown buffer))
        (setq shown nil)
        (cl-letf (((symbol-function 'ibuffer-current-buffer)
                   (lambda (&rest _) buffer)))
          (should (eq (+eshell--ibuffer-visit-a
                       (lambda (&rest _) (setq called t)))
                      buffer))
          (should (eq shown buffer))
          (should-not called))))))

(ert-deftest command-shell-c9-consult-searches-and-floats-eshell ()
  "A hidden Eshell appears in buffer search and placement keys do not split."
  (command-shell-tests--with-dir dir
    (let ((buffer (+eshell-buffer dir))
          (messages (get-buffer-create "*Messages*"))
          shown split selection)
      (should (memq buffer (+consult-buffer-list)))
      (should (memq messages (+consult-buffer-list)))
      (let ((consult--buffer-display #'switch-to-buffer)
            (consult-buffer-filter '("\\`\\*Messages\\*\\'")))
        (+consult-buffer-scoped-a
         (lambda (&optional _sources)
           (setq selection (list (memq messages
                                       (funcall consult-buffer-list-function))
                                 consult-buffer-filter))))
        (should (car selection))
        (should (equal (cadr selection)
                       '("\\`\\*Messages\\*\\'"))))
      (require 'consult)
      (let ((consult-buffer-list-function #'+consult-buffer-list))
        (should (cl-find messages
                         (funcall (plist-get consult-source-buffer :items))
                         :key #'cdr :test #'eq)))
      (let ((+consult-buffer-origin (selected-window))
            (+consult-buffer-placement 'right))
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&optional _) t))
                  ((symbol-function '+eshell--display)
                   (lambda (item) (setq shown item)))
                  ((symbol-function '+open-entry-in-split)
                   (lambda (&rest _) (setq split t))))
          (+consult-buffer-display buffer)
          (should (eq shown buffer))
          (should-not split))))))

(ert-deftest command-shell-c9-buffer-switcher-blacklists-implementation-buffers ()
  "The global switcher keeps useful buffers but omits Fundamental and hidden UI state."
  (require 'consult)
  (let ((fundamental (generate-new-buffer "*Temporary Fundamental Test*"))
        (hidden (generate-new-buffer " hidden-switch-test"))
        (events (generate-new-buffer "*Temporary JSONRPC Events Test*"))
        (compile-log (generate-new-buffer "*Temporary Elisp Compile Test*"))
        (useful (generate-new-buffer "*Useful Text Test*"))
        (original-sources consult-buffer-sources))
    (unwind-protect
        (progn
          (with-current-buffer fundamental (fundamental-mode))
          (with-current-buffer hidden (text-mode))
          (with-current-buffer events
            (setq major-mode 'jsonrpc-events-mode))
          (with-current-buffer compile-log
            (setq major-mode 'emacs-lisp-compilation-mode))
          (with-current-buffer useful (text-mode))
          (should-not (memq fundamental (+consult-buffer-list)))
          (should-not (memq hidden (+consult-buffer-list)))
          (should-not (memq events (+consult-buffer-list)))
          (should-not (memq compile-log (+consult-buffer-list)))
          (should (memq useful (+consult-buffer-list)))
          (should (memq (get-buffer "*Messages*") (+consult-buffer-list)))
          (let ((consult--buffer-display #'switch-to-buffer)
                (consult-buffer-sources '(consult-source-buffer
                                          consult-source-hidden-buffer
                                          consult-source-other-buffer)))
            (+consult-buffer-scoped-a
             (lambda (&optional _sources)
               (should (equal consult-buffer-sources
                              '(consult-source-buffer)))
               (should (eq consult-buffer-list-function
                           #'+consult-buffer-list)))))
          (should (equal consult-buffer-sources original-sources)))
      (mapc #'kill-buffer (list fundamental hidden events compile-log useful)))))

(ert-deftest command-shell-c9-exit-kills-last-perspective-eshell ()
  "Perspective's last-buffer protection must not make Eshell exit a no-op."
  (command-shell-tests--with-dir dir
    (let ((kill-buffer-query-functions
           (cons 'persp-maybe-kill-buffer kill-buffer-query-functions))
          buffer)
      (cl-letf (((symbol-function 'persp-maybe-kill-buffer)
                 (lambda () nil)))
        (setq buffer (+eshell-buffer dir "exit-test"))
        (unwind-protect
            (with-current-buffer buffer
              (should-not (memq 'persp-maybe-kill-buffer
                                kill-buffer-query-functions))
              (goto-char (point-max))
              (insert "exit")
              (eshell-send-input)
              (should-not (buffer-live-p buffer)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest command-shell-c10-build-api-reuses-project-session-and-gates-run ()
  "Builds share one named queue and && skips a run after failure."
  (command-shell-tests--with-dir dir
    (let (first second success failure)
      (unwind-protect
          (progn
            (setq first (+eshell-run-build
                         "*echo built" :run "*echo ran" :directory dir
                         :display nil
                         :callback (lambda (value) (setq success value))))
            (should (command-shell-tests--wait (lambda () success)))
            (should (eql (plist-get success :status) 0))
            (should (string-match-p (regexp-quote "built\nran")
                                    (plist-get success :output)))
            (should (string-match-p "build-"
                                    (buffer-name first)))
            (setq second (+eshell-run-build
                          "*false" :run "*echo must-not-run" :directory dir
                          :display nil
                          :callback (lambda (value) (setq failure value))))
            (should (eq first second))
            (should (command-shell-tests--wait (lambda () failure)))
            (should-not (eql (plist-get failure :status) 0))
            (should-not (string-match-p "must-not-run"
                                        (plist-get failure :output))))
        (when (buffer-live-p first) (kill-buffer first))))))

(ert-deftest command-shell-c10-cpp-build-chooses-make-or-single-file ()
  "C++ selects make when present and a quoted compiler command otherwise."
  (command-shell-tests--with-dir dir
    (let* ((file (expand-file-name "hello world.cc" dir))
           (makefile (expand-file-name "Makefile" dir))
           (default-directory dir)
           captured)
      (with-temp-file file (insert "int main() { return 0; }\n"))
      (with-temp-buffer
        (setq buffer-file-name file major-mode 'c++-mode
              default-directory dir)
        (cl-letf (((symbol-function '+eshell-run-build)
                   (lambda (command &rest keys)
                     (setq captured (cons command keys)))))
          (let ((+cpp-eshell-build-command nil)
                (+cpp-eshell-run-command nil)
                (+cpp-eshell-run-after-build nil))
            (+cpp/eshell-build t)
            (should (string-prefix-p "c++ " (car captured)))
            (should (string-match-p
                     (regexp-quote
                      (shell-quote-argument (file-name-nondirectory file)))
                     (car captured)))
            (should (equal (plist-get (cdr captured) :run)
                           (shell-quote-argument
                            (concat "./" (+eshell-build-output-file file)))))
            (with-temp-file makefile (insert "all:\n\t@true\n"))
            (setq captured nil)
            (+cpp/eshell-build)
            (should (equal (car captured) "make"))
            (should (equal (plist-get (cdr captured) :directory) dir))
            (should-not (plist-get (cdr captured) :run))
            (let ((+cpp-eshell-run-command "./demo"))
              (+cpp/eshell-build t)
              (should (equal (plist-get (cdr captured) :run) "./demo")))))))))

(ert-deftest command-shell-c10-go-build-chooses-module-and-run ()
  "Go uses its module's build and run commands in the shared task API."
  (command-shell-tests--with-dir dir
    (let* ((file (expand-file-name "main.go" dir))
           (module (expand-file-name "go.mod" dir))
           (default-directory dir)
           captured)
      (with-temp-file file (insert "package main\nfunc main() {}\n"))
      (with-temp-file module (insert "module example.test/demo\n\ngo 1.22\n"))
      (with-temp-buffer
        (setq buffer-file-name file major-mode 'go-ts-mode
              default-directory dir)
        (cl-letf (((symbol-function '+eshell-run-build)
                   (lambda (command &rest keys)
                     (setq captured (cons command keys)))))
          (let ((+go-eshell-build-command nil)
                (+go-eshell-run-command nil)
                (+go-eshell-run-after-build nil))
            (+go/eshell-build t)
            (should (equal (car captured) "go build ./..."))
            (should (equal (plist-get (cdr captured) :run) "go run ."))
            (with-temp-file (expand-file-name "Makefile" dir)
              (insert "all:\n\t@true\n"))
            (+go/eshell-build)
            (should (equal (car captured) "make"))
            (should-not (plist-get (cdr captured) :run))))))))

(ert-deftest command-shell-c10-language-modes-bind-direct-build-key ()
  "C/C++ and Go buffers reserve C-c C-c for their Eshell build entry."
  (dolist (entry '((c-mode-hook . +cpp/eshell-build)
                   (c++-mode-hook . +cpp/eshell-build)
                   (c-ts-mode-hook . +cpp/eshell-build)
                   (c++-ts-mode-hook . +cpp/eshell-build)
                   (go-mode-hook . +go/eshell-build)
                   (go-ts-mode-hook . +go/eshell-build)))
    (with-temp-buffer
      (run-hooks (car entry))
      (should (eq (key-binding (kbd "C-c C-c")) (cdr entry))))))

(ert-deftest command-shell-c10-cpp-standalone-build-and-run ()
  "A real standalone C++ file compiles and runs in the same Eshell task."
  (skip-unless (executable-find "c++"))
  (command-shell-tests--with-dir dir
    (let* ((file (expand-file-name "hello world.cc" dir))
           (binary (expand-file-name (+eshell-build-output-file file) dir))
           (default-directory dir)
           shell)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "#include <cstdio>\nint main() { std::puts(\"eshell-build-ok\"); }\n"))
            (with-temp-buffer
              (setq buffer-file-name file major-mode 'c++-mode
                    default-directory dir)
              (let ((+cpp-eshell-build-command nil)
                    (+cpp-eshell-run-command nil)
                    (+cpp-eshell-run-after-build nil))
                (setq shell (+cpp/eshell-build t))))
            (should (command-shell-tests--wait
                     (lambda () (and shell (not (+eshell-busy-p shell)))) 20))
            (with-current-buffer shell
              (should (eql eshell-last-command-status 0))
              (should (string-match-p "eshell-build-ok" (buffer-string)))))
        (when (buffer-live-p shell) (kill-buffer shell))
        (when (file-exists-p binary) (delete-file binary))))))

(ert-deftest command-shell-c10-go-standalone-build-and-run ()
  "A real standalone Go file builds and runs in the same Eshell task."
  (skip-unless (executable-find "go"))
  (command-shell-tests--with-dir dir
    (let* ((file (expand-file-name "main.go" dir))
           (binary (expand-file-name (+eshell-build-output-file file) dir))
           (default-directory dir)
           shell)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "package main\nimport \"fmt\"\nfunc main() { fmt.Println(\"go-build-ok\") }\n"))
            (with-temp-buffer
              (setq buffer-file-name file major-mode 'go-ts-mode
                    default-directory dir)
              (let ((+go-eshell-build-command nil)
                    (+go-eshell-run-command nil)
                    (+go-eshell-run-after-build nil))
                (setq shell (+go/eshell-build t))))
            (should (command-shell-tests--wait
                     (lambda () (and shell (not (+eshell-busy-p shell)))) 60))
            (with-current-buffer shell
              (should (eql eshell-last-command-status 0))
              (should (string-match-p "go-build-ok" (buffer-string)))))
        (when (buffer-live-p shell) (kill-buffer shell))
        (when (file-exists-p binary) (delete-file binary))))))

(ert-deftest command-shell-c9-selector-searches-switches-and-creates ()
  "The Eshell picker exposes named sessions and a separate new action."
  (command-shell-tests--with-dir dir
    (let ((default-directory dir)
          (named (+eshell-buffer dir "build"))
          created shown)
      (unwind-protect
          (progn
            (should (assoc "＋ 新建 Eshell" (+eshell--selector-candidates)))
            (should (cl-find-if (lambda (entry) (eq (cdr entry) named))
                                (+eshell--selector-candidates)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt candidates &rest _)
                         (car (cl-find-if (lambda (entry) (eq (cdr entry) named))
                                          candidates))))
                      ((symbol-function '+eshell--display)
                       (lambda (buffer) (setq shown buffer))))
              (should (eq (+eshell/toggle) named))
              (should (eq shown named))
              (setq created (+eshell/new dir))
              (should (equal (buffer-name created)
                             "*eshell:local/session-1*"))
              (should (equal (+eshell--next-session-name dir) "session-2"))))
        (when (buffer-live-p named) (kill-buffer named))
        (when (buffer-live-p created) (kill-buffer created))))))

(ert-deftest command-shell-c9-new-session-never-opens-a-parent-window ()
  "Creating a session does not invoke the stock Eshell display path."
  (command-shell-tests--with-dir dir
    (let (buffer shown)
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (&rest _)
                       (ert-fail "Eshell creation called pop-to-buffer")))
                    ((symbol-function '+eshell--display)
                     (lambda (item) (setq shown item))))
            (setq buffer (+eshell/new dir "window-safe"))
            (should (eq shown buffer))
            (should (equal (buffer-name buffer)
                           "*eshell:local/window-safe*")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest command-shell-c9-floating-repairs-a-legacy-pane ()
  "An Eshell left in a parent pane is withdrawn without changing the layout."
  (command-shell-tests--with-dir dir
    (save-window-excursion
      (let* ((buffer (+eshell-buffer dir))
             (window (selected-window))
             (count (length (window-list))))
        (set-window-buffer window buffer)
        (+eshell--vacate-ordinary-windows buffer)
        (should-not (eq (window-buffer window) buffer))
        (should (= count (length (window-list))))))))

(ert-deftest command-shell-c9-cmd-w-hides-only-the-float ()
  "Cmd-W hides a focused Eshell child frame and keeps its buffer alive."
  (command-shell-tests--with-dir dir
    (let ((buffer (+eshell-buffer dir)) hidden closed)
      (with-current-buffer buffer
        (should (eq (key-binding (kbd "M-w"))
                    #'+eshell/hide-or-close-window))
        (cl-letf (((symbol-function '+eshell--visible-floating-frame)
                   (lambda (_buffer) (selected-frame)))
                  ((symbol-function '+eshell--hide-floating)
                   (lambda (item) (setq hidden item)))
                  ((symbol-function '+windows/close-window)
                   (lambda () (setq closed t))))
          (+eshell/hide-or-close-window)
          (should (eq hidden buffer))
          (should-not closed)
          (should (buffer-live-p buffer)))))))

(ert-deftest command-shell-c1-first-prompt-shows-target-directory ()
  (command-shell-tests--with-dir dir
    (let ((buffer (+eshell-buffer dir)))
      (with-current-buffer buffer
        (should (equal (file-truename default-directory)
                       (file-truename dir)))
        (should (string-match-p
                 (regexp-quote (directory-file-name dir))
                 (car (last (split-string (buffer-string) "\n")))))
        (should (= (length (split-string (buffer-string) "\n" t)) 2))))))

(ert-deftest command-shell-c9-direct-eshell-uses-display-surface ()
  (command-shell-tests--with-dir dir
    (let (shown)
      (cl-letf (((symbol-function '+eshell--display)
                 (lambda (buffer) (setq shown buffer))))
        (let* ((default-directory dir)
               (buffer (eshell)))
          (should (eq shown buffer)))))))

(ert-deftest command-shell-c9-eshell-prefix-keeps-numbered-sessions ()
  "Direct Eshell keeps its numbered prefix behavior without opening a pane."
  (command-shell-tests--with-dir dir
    (let ((default-directory dir) numbered shown)
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (&rest _)
                       (ert-fail "Direct Eshell called pop-to-buffer")))
                    ((symbol-function '+eshell--display)
                     (lambda (item) (setq shown item))))
            (setq numbered (eshell 42))
            (should (eq shown numbered))
            (should (equal (buffer-name numbered) "*eshell*<42>")))
        (when (buffer-live-p numbered) (kill-buffer numbered))))))

(ert-deftest command-shell-c1-fs-local-parity ()
  (command-shell-tests--with-dir dir
    (let ((native (+eshell-run-sync "pwd" :directory dir :timeout 20))
          (fs (+eshell-run-sync "pwd" :directory (concat "/fs:local:" dir) :timeout 20)))
      (should (eq (plist-get native :buffer) (plist-get fs :buffer)))
      (should (eql (plist-get native :status) 0))
      (should (eql (plist-get fs :status) 0))
      (should (string-match-p (regexp-quote (directory-file-name (file-truename dir)))
                              (file-truename (string-trim (plist-get fs :output))))))))

(ert-deftest command-shell-c2-sync-returns-status-and-output ()
  (command-shell-tests--with-dir dir
    (let ((internal (+eshell-run-sync "echo hello" :directory dir :timeout 20))
          (external (+eshell-run-sync "*echo external" :directory dir :timeout 20))
          (failing (+eshell-run-sync "sh -c 'exit 3'" :directory dir :timeout 20)))
      (should (equal (string-trim (plist-get internal :output)) "hello"))
      (should (eql (plist-get internal :status) 0))
      (should (equal (string-trim (plist-get external :output)) "external"))
      (should (eql (plist-get failing :status) 3))
      (should (equal (plist-get failing :command) "sh -c 'exit 3'")))))

(ert-deftest command-shell-c3-commands-queue-in-order ()
  (command-shell-tests--with-dir dir
    (let (results)
      (dolist (word '("one" "two" "three"))
        (+eshell-run (format "*echo %s" word) :directory dir
                     :callback (lambda (r) (push r results))))
      (should (command-shell-tests--wait (lambda () (= (length results) 3)) 20))
      (should (equal (mapcar (lambda (r) (string-trim (plist-get r :output)))
                             (reverse results))
                     '("one" "two" "three"))))))

(ert-deftest command-shell-c3-queued-directory-waits-for-dispatch ()
  (command-shell-tests--with-dir dir
    (let* ((sub (file-name-as-directory (expand-file-name "sub" dir)))
           (buffer (+eshell-run "sleep 1" :directory dir))
           result)
      (make-directory sub)
      (should (command-shell-tests--wait
               (lambda () (buffer-local-value 'eshell-foreground-command buffer))))
      (+eshell-run "pwd" :directory sub
                   :callback (lambda (r) (setq result r)))
      (should (equal (file-truename (buffer-local-value 'default-directory buffer))
                     (file-truename dir)))
      (should (command-shell-tests--wait (lambda () result) 5))
      (should (equal (file-truename (string-trim (plist-get result :output)))
                     (file-truename (directory-file-name sub)))))))

(ert-deftest command-shell-c3-async-call-returns-before-dispatch ()
  (command-shell-tests--with-dir dir
    (let (result)
      (+eshell-run "echo later" :directory dir
                   :callback (lambda (r) (setq result r)))
      (should-not result)
      (should (command-shell-tests--wait (lambda () result) 5))
      (should (equal (string-trim (plist-get result :output)) "later")))))

(ert-deftest command-shell-c4-pending-input-is-restored ()
  (command-shell-tests--with-dir dir
    (let ((buffer (+eshell-buffer dir)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "half typed"))
      (+eshell-run-sync "echo x" :directory dir :timeout 20)
      (should (command-shell-tests--wait
               (lambda ()
                 (with-current-buffer buffer
                   (equal (buffer-substring-no-properties eshell-last-output-end
                                                          (point-max))
                          "half typed")))
               5)))))

(ert-deftest command-shell-c5-cancel-and-kill-resolve-callbacks ()
  (command-shell-tests--with-dir dir
    (let (running queued)
      (+eshell-run "sleep 30" :directory dir :callback (lambda (r) (setq running r)))
      (+eshell-run "echo later" :directory dir :callback (lambda (r) (setq queued r)))
      (let ((buffer (+eshell-buffer dir)))
        (should (command-shell-tests--wait
                 (lambda () (buffer-local-value 'eshell-foreground-command buffer))))
        (+eshell-cancel buffer)
        (should (eq (plist-get queued :reason) 'cancelled))
        (should (command-shell-tests--wait (lambda () running)))
        (should (eq (plist-get running :reason) 'cancelled))
        (should-not (plist-get running :status))))
    (let (killed)
      (+eshell-run "sleep 30" :directory dir :callback (lambda (r) (setq killed r)))
      (let ((buffer (+eshell-buffer dir)))
        (should (command-shell-tests--wait
                 (lambda () (buffer-local-value 'eshell-foreground-command buffer))))
        (kill-buffer buffer))
      (should (eq (plist-get killed :reason) 'killed)))))

(ert-deftest command-shell-c5-callback-error-does-not-stall-queue ()
  (command-shell-tests--with-dir dir
    (let (result)
      (+eshell-run "echo first" :directory dir
                   :callback (lambda (_r) (error "intentional callback failure")))
      (+eshell-run "echo second" :directory dir
                   :callback (lambda (r) (setq result r)))
      (should (command-shell-tests--wait (lambda () result) 5))
      (should (equal (string-trim (plist-get result :output)) "second")))))

(ert-deftest command-shell-c5-start-error-resolves-and-continues ()
  (command-shell-tests--with-dir dir
    (let* ((bad (file-name-as-directory (expand-file-name "bad" dir)))
           (sync (symbol-function '+eshell--sync-directory))
           failed next)
      (make-directory bad)
      (+eshell-run "sleep 0.2" :directory dir)
      (cl-letf (((symbol-function '+eshell--sync-directory)
                 (lambda (buffer directory)
                   (if (equal directory bad)
                       (error "simulated directory failure")
                     (funcall sync buffer directory)))))
        (+eshell-run "pwd" :directory bad
                     :callback (lambda (r) (setq failed r)))
        (+eshell-run "echo next" :directory dir
                     :callback (lambda (r) (setq next r)))
        (should (command-shell-tests--wait (lambda () next) 5)))
      (should (eq (plist-get failed :reason) 'error))
      (should (equal (string-trim (plist-get next :output)) "next")))))

(ert-deftest command-shell-c6-sync-timeout-interrupts ()
  (command-shell-tests--with-dir dir
    (let ((start (float-time)))
      (should-error (+eshell-run-sync "sleep 30" :directory dir :timeout 0.5)
                    :type '+eshell-timeout)
      (should (< (- (float-time) start) 5))
      (let ((buffer (+eshell-buffer dir)))
        (should (command-shell-tests--wait
                 (lambda () (not (+eshell-busy-p buffer)))))))))

(ert-deftest command-shell-c6-async-timeout-resolves-and-continues ()
  (command-shell-tests--with-dir dir
    (let (timed-out next)
      (+eshell-run "sleep 30" :directory dir :timeout 0.2
                   :callback (lambda (r) (setq timed-out r)))
      (+eshell-run "echo next" :directory dir
                   :callback (lambda (r) (setq next r)))
      (should (command-shell-tests--wait (lambda () timed-out) 5))
      (should (eq (plist-get timed-out :reason) 'timeout))
      (should-not (plist-get timed-out :status))
      (should (command-shell-tests--wait (lambda () next) 5))
      (should (equal (string-trim (plist-get next :output)) "next")))))

(ert-deftest command-shell-c8-output-and-queue-are-bounded ()
  (command-shell-tests--with-dir dir
    (let ((+eshell-max-output-chars 32)
          (+eshell-max-queued-commands 1)
          result)
      (+eshell-run (concat "echo " (make-string 80 ?x)) :directory dir
                   :callback (lambda (r) (setq result r)))
      (should (command-shell-tests--wait (lambda () result) 5))
      (should (plist-get result :truncated))
      (should (<= (length (plist-get result :output)) 32))
      (let ((buffer (+eshell-run "sleep 30" :directory dir)))
        (should (command-shell-tests--wait
                 (lambda () (buffer-local-value 'eshell-foreground-command buffer))))
        (+eshell-run "echo queued" :directory dir)
        (should-error (+eshell-run "echo overflow" :directory dir))
        (+eshell-cancel buffer)))))

(ert-deftest command-shell-c8-streaming-output-is-bounded ()
  (command-shell-tests--with-dir dir
    (let ((+eshell-max-output-chars 24)
          result)
      (+eshell-run
       "sh -c 'printf 12345678901234567890; sleep 0.05; printf abcdefghijklmnopqrst'"
       :directory dir :callback (lambda (r) (setq result r)))
      (should (command-shell-tests--wait (lambda () result) 5))
      (should (plist-get result :truncated))
      (should (<= (length (plist-get result :output)) 24))
      (should (string-suffix-p "abcdefghijklmnopqrst"
                               (string-trim (plist-get result :output)))))))

(ert-deftest command-shell-c6-sync-is-batch-only ()
  (let ((noninteractive nil))
    (should-error (+eshell-run-sync "echo forbidden"))))

(ert-deftest command-shell-c6-sync-timeout-keeps-other-commands ()
  (command-shell-tests--with-dir dir
    (let (first later)
      (+eshell-run "sleep 0.3" :directory dir
                   :callback (lambda (r) (setq first r)))
      (+eshell-run "echo later" :directory dir
                   :callback (lambda (r) (setq later r)))
      (should-error (+eshell-run-sync "echo timed-out"
                                          :directory dir :timeout 0.1)
                    :type '+eshell-timeout)
      (should (command-shell-tests--wait (lambda () later) 5))
      (should (eql (plist-get first :status) 0))
      (should (equal (string-trim (plist-get later :output)) "later")))))

(ert-deftest command-shell-c7-redraw-sequences-overwrite ()
  (with-temp-buffer
    (should (equal (+eshell-normalize-output "50%\e[1G60%") "50%\r60%"))
    (should (equal (+eshell-normalize-output "\e[2Kdone\e[0K") "done"))
    (should (equal (+eshell-normalize-output "\e[31mred\e[0m") "\e[31mred\e[0m"))
    (should (equal (+eshell-normalize-output "frame 1\r") "frame 1"))
    (should (equal (+eshell-normalize-output "frame 2") "\rframe 2"))))

(provide 'command-shell-tests)
;;; command-shell-tests.el ends here

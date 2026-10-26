;;; command-shell-tests.el --- Contracts of the eshell command shell -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; One test per contract in docs/design/command-shell.md.  Run with the full
;; configuration loaded (scripts/test-all).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'term/shell/eshell)

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

(ert-deftest command-shell-c1-fs-local-parity ()
  (command-shell-tests--with-dir dir
    (let ((native (+eshell-run-sync "pwd" :directory dir :timeout 20))
          (fs (+eshell-run-sync "pwd" :directory (concat "/fs:local:" dir) :timeout 20)))
      (should (eq (plist-get native :buffer) (plist-get fs :buffer)))
      (should (eql (plist-get native :status) 0))
      (should (eql (plist-get fs :status) 0))
      (should (string-match-p (regexp-quote (directory-file-name (file-truename dir)))
                              (file-truename (string-trim (plist-get fs :output))))))))

(ert-deftest command-shell-c1-remote-start-directory-has-no-tilde ()
  ;; `/fs:' names have no `~' spelling; the target's backend gives its home.
  (let (asked)
    (cl-letf (((symbol-function 'remote-expand-file-name)
               (lambda (name _dir target)
                 (setq asked (list name target))
                 (format "/fs:%s:/home/u/" target))))
      (should (equal (aaron-terminal-home-directory "/fs:aaron-pc:/tmp/")
                     "/fs:aaron-pc:/home/u/"))
      (should (equal asked '("~/" "aaron-pc"))))))

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
        (should-not (eql (plist-get running :status) 0))))
    (let (killed)
      (+eshell-run "sleep 30" :directory dir :callback (lambda (r) (setq killed r)))
      (let ((buffer (+eshell-buffer dir)))
        (should (command-shell-tests--wait
                 (lambda () (buffer-local-value 'eshell-foreground-command buffer))))
        (kill-buffer buffer))
      (should (eq (plist-get killed :reason) 'killed)))))

(ert-deftest command-shell-c6-sync-timeout-interrupts ()
  (command-shell-tests--with-dir dir
    (let ((start (float-time)))
      (should-error (+eshell-run-sync "sleep 30" :directory dir :timeout 0.5)
                    :type '+eshell-timeout)
      (should (< (- (float-time) start) 5))
      (let ((buffer (+eshell-buffer dir)))
        (should (command-shell-tests--wait
                 (lambda () (not (+eshell-busy-p buffer)))))))))

(ert-deftest command-shell-c7-redraw-sequences-overwrite ()
  (with-temp-buffer
    (should (equal (+eshell-normalize-output "50%\e[1G60%") "50%\r60%"))
    (should (equal (+eshell-normalize-output "\e[2Kdone\e[0K") "done"))
    (should (equal (+eshell-normalize-output "\e[31mred\e[0m") "\e[31mred\e[0m"))
    (should (equal (+eshell-normalize-output "frame 1\r") "frame 1"))
    (should (equal (+eshell-normalize-output "frame 2") "\rframe 2"))))

(provide 'command-shell-tests)
;;; command-shell-tests.el ends here

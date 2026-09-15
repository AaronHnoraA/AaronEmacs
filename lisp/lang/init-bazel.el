;;; init-bazel.el --- Target-aware Bazel workbench -*- lexical-binding: t; -*-

;;; Commentary:
;; Bazel mode, target discovery, and task execution through the Remote process
;; and project environment contracts.

;;; Code:

(require 'config)
(require 'init-funcs)
(require 'remote-fs)
(require 'remote-process)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function my/project-local-apply-env "init-project-local" (environment))
(declare-function my/project-local-env "init-project-local" (kind &optional root))
(declare-function my/direnv-update-environment-maybe "init-direnv" (&optional path))

(defvar my/direnv-subprocess-sync-inhibited)
(defvar my/task-last-command)
(defvar my/task-last-directory)
(defvar my/task-last-environment)
(defvar my/task-build-last-command)
(defvar my/task-build-last-directory)
(defvar my/task-build-last-environment)

(defgroup my/bazel nil
  "Bazel target discovery and execution."
  :group 'tools)

(config-defvar my/bazel-command-candidates nil
  "Target-side Bazel executables in preference order."
  :type '(repeat string)
  :group 'my/bazel)

(config-defvar my/bazel-cache-ttl nil
  "Seconds before a cached Bazel target list is refreshed."
  :type 'integer
  :group 'my/bazel)

(defconst my/bazel-cache-directory
  (expand-file-name "var/bazel/" user-emacs-directory)
  "Directory containing per-target Bazel query caches.")

(defconst my/bazel-workspace-markers
  '("MODULE.bazel" "WORKSPACE.bazel" "WORKSPACE")
  "Files which identify a Bazel workspace root.")

(defun my/bazel-workspace-root (&optional directory)
  "Return the Bazel workspace containing DIRECTORY."
  (let ((directory (or directory default-directory)))
    (or (seq-some (lambda (marker)
                    (locate-dominating-file directory marker))
                  my/bazel-workspace-markers)
        (user-error "No Bazel MODULE or WORKSPACE found"))))

(defun my/bazel--context (&optional root)
  "Return the Remote context for ROOT or the current Bazel workspace."
  (remote-context (or root (my/bazel-workspace-root))))

(defun my/bazel-executable (&optional context)
  "Return the first configured Bazel executable available on CONTEXT."
  (let ((context (or context (my/bazel--context))))
    (or (seq-some (lambda (program)
                    (remote-executable-find program context))
                  my/bazel-command-candidates)
        (user-error "Neither bazelisk nor bazel is available on this target"))))

(defun my/bazel--cache-file (root context)
  "Return the target-list cache file for ROOT and CONTEXT."
  (make-directory my/bazel-cache-directory t)
  (expand-file-name
   (concat (secure-hash
            'sha256
            (format "%s\0%s" (remote-context-target-id context) root))
           ".eld")
   my/bazel-cache-directory))

(defun my/bazel--cache-read (file)
  "Return a fresh target list from FILE, or nil."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((value (read (current-buffer))))
            (when (and (listp value)
                       (numberp (plist-get value :time))
                       (< (- (float-time) (plist-get value :time))
                          my/bazel-cache-ttl))
              (plist-get value :targets))))
      (error nil))))

(defun my/bazel--cache-write (file targets)
  "Persist TARGETS in FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 (list :time (float-time) :targets targets) (current-buffer)))))

(defun my/bazel-invalidate-cache (&optional directory)
  "Delete the Bazel target cache associated with DIRECTORY."
  (interactive)
  (condition-case nil
      (let* ((root (my/bazel-workspace-root directory))
             (context (my/bazel--context root))
             (file (my/bazel--cache-file root context)))
        (when (file-exists-p file)
          (delete-file file)))
    (user-error nil)))

(defun my/bazel-invalidate-cache-after-save-h ()
  "Invalidate target discovery after saving a Bazel definition file."
  (when (and buffer-file-name
             (or (member (file-name-nondirectory buffer-file-name)
                         '("BUILD" "BUILD.bazel" "MODULE.bazel"
                           "WORKSPACE" "WORKSPACE.bazel"))
                 (string-suffix-p ".bzl" buffer-file-name)))
    (my/bazel-invalidate-cache default-directory)))

(defun my/bazel--parse-targets (output)
  "Parse Bazel label OUTPUT into a stable target list."
  (sort (delete-dups (split-string output "\n" t "[[:space:]]+"))
        #'string-lessp))

(defun my/bazel-with-targets (callback &optional refresh)
  "Call CALLBACK with ROOT, CONTEXT, EXECUTABLE, and discovered targets.
With REFRESH, bypass the persistent cache."
  (let* ((root (file-name-as-directory (my/bazel-workspace-root)))
         (context (my/bazel--context root))
         (executable (my/bazel-executable context))
         (cache-file (my/bazel--cache-file root context))
         (cached (and (not refresh) (my/bazel--cache-read cache-file))))
    (if cached
        (funcall callback root context executable cached)
      (message "Querying Bazel targets…")
      (remote-exec-async
       executable
       :args '("query" "//..." "--output=label")
       :context context :filesystem-effects 'none :name "bazel-query"
       :callback
       (lambda (result)
         (if (zerop (remote-exec-result-status result))
             (let ((targets (my/bazel--parse-targets
                             (remote-exec-result-stdout result))))
               (my/bazel--cache-write cache-file targets)
               (funcall callback root context executable targets))
           (message "Bazel query failed: %s"
                    (string-trim (remote-exec-result-stderr result)))))))))

(defun my/bazel--read-target (targets &optional prompt)
  "Read one label from TARGETS."
  (unless targets
    (user-error "No Bazel targets found"))
  (completing-read (or prompt "Bazel target: ") targets nil t))

(defun my/bazel--project-environment (kind root)
  "Return KIND project environment for ROOT when the project layer is loaded."
  (when (fboundp 'my/project-local-env)
    (my/project-local-env kind root)))

(defun my/bazel--run-command (subcommand root executable target)
  "Run Bazel SUBCOMMAND for TARGET from ROOT with EXECUTABLE."
  (let* ((kind (if (equal subcommand "build") 'build 'task))
         (environment (my/bazel--project-environment kind root))
         (command (format "%s %s %s"
                          (shell-quote-argument executable)
                          subcommand
                          (shell-quote-argument target))))
    (when (fboundp 'my/direnv-update-environment-maybe)
      (my/direnv-update-environment-maybe root))
    (if (eq kind 'build)
        (setq my/task-build-last-command command
              my/task-build-last-directory root
              my/task-build-last-environment environment)
      (setq my/task-last-command command
            my/task-last-directory root
            my/task-last-environment environment))
    (let ((default-directory root)
          (process-environment
           (if (fboundp 'my/project-local-apply-env)
               (my/project-local-apply-env environment)
             process-environment))
          (compilation-buffer-name-function
           (lambda (_) (format "*bazel %s*" subcommand)))
          (my/direnv-subprocess-sync-inhibited t))
      (compile command))))

(defun my/bazel-run-target (subcommand &optional refresh)
  "Choose a target and run Bazel SUBCOMMAND.
With REFRESH, refresh target discovery first."
  (let ((origin (current-buffer)))
    (my/bazel-with-targets
     (lambda (root _context executable targets)
       (when (buffer-live-p origin)
         (with-current-buffer origin
           (my/bazel--run-command
            subcommand root executable
            (my/bazel--read-target targets
                                   (format "Bazel %s target: " subcommand))))))
     refresh)))

(defun my/bazel-build (&optional refresh)
  "Choose and build a Bazel target."
  (interactive "P")
  (my/bazel-run-target "build" refresh))

(defun my/bazel-test (&optional refresh)
  "Choose and test a Bazel target."
  (interactive "P")
  (my/bazel-run-target "test" refresh))

(defun my/bazel-run (&optional refresh)
  "Choose and run a Bazel target."
  (interactive "P")
  (my/bazel-run-target "run" refresh))

(defun my/bazel-insert-target (&optional refresh)
  "Choose and insert a Bazel target label."
  (interactive "P")
  (let ((origin (current-buffer))
        (marker (copy-marker (point) t)))
    (my/bazel-with-targets
     (lambda (_root _context _executable targets)
       (unwind-protect
           (when (and (buffer-live-p origin) (marker-buffer marker))
             (let ((target (my/bazel--read-target targets)))
               (with-current-buffer origin
                 (goto-char marker)
                 (insert target))))
         (set-marker marker nil)))
     refresh)))

(defun my/bazel-jump-to-target (&optional refresh)
  "Choose a Bazel target and jump to its definition."
  (interactive "P")
  (my/bazel-with-targets
   (lambda (root context executable targets)
     (let ((target (my/bazel--read-target targets)))
       (remote-exec-async
        executable :args (list "query" target "--output=location")
        :context context :filesystem-effects 'none :name "bazel-location"
        :callback
        (lambda (result)
          (let ((output (remote-exec-result-stdout result)))
            (if (and (zerop (remote-exec-result-status result))
                     (string-match
                      "\\`\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):" output))
                (let ((file (remote-expand-file-name
                             (match-string 1 output) root context))
                      (line (string-to-number (match-string 2 output)))
                      (column (string-to-number (match-string 3 output))))
                  (find-file file)
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column (1- column)))
              (message "Bazel location query failed: %s"
                       (string-trim
                        (if (string-empty-p (remote-exec-result-stderr result))
                            output
                          (remote-exec-result-stderr result))))))))))
   refresh))

(defun my/bazel-open-info-directory (key)
  "Open the directory returned by `bazel info KEY'."
  (let* ((root (file-name-as-directory (my/bazel-workspace-root)))
         (context (my/bazel--context root))
         (executable (my/bazel-executable context)))
    (remote-exec-async
     executable :args (list "info" key)
     :context context :filesystem-effects 'none :name "bazel-info"
     :callback
     (lambda (result)
       (if (zerop (remote-exec-result-status result))
           (dired (remote-expand-file-name
                   (string-trim (remote-exec-result-stdout result))
                   root context))
         (message "bazel info failed: %s"
                  (string-trim (remote-exec-result-stderr result))))))))

(defun my/bazel-open-bin ()
  "Open the current workspace's Bazel binary output directory."
  (interactive)
  (my/bazel-open-info-directory "bazel-bin"))

(defun my/bazel-open-out ()
  "Open the current workspace's Bazel output directory."
  (interactive)
  (my/bazel-open-info-directory "output_base"))

(defun my/bazel-refresh-targets ()
  "Refresh cached Bazel targets for the current workspace."
  (interactive)
  (my/bazel-with-targets
   (lambda (_root _context _executable targets)
     (message "Cached %d Bazel targets" (length targets)))
   t))

(transient-define-prefix my/bazel-dispatch ()
  "Bazel target workbench."
  [["Run"
    ("b" "build" my/bazel-build)
    ("t" "test" my/bazel-test)
    ("r" "run" my/bazel-run)]
   ["Targets"
    ("i" "insert label" my/bazel-insert-target)
    ("j" "jump to definition" my/bazel-jump-to-target)
    ("R" "refresh query" my/bazel-refresh-targets)]
   ["Outputs"
    ("B" "open bazel-bin" my/bazel-open-bin)
    ("O" "open output base" my/bazel-open-out)]])

(defun my/bazel-mode-setup-h ()
  "Configure Bazel editing for the current logical target."
  (let ((context (remote-context default-directory)))
    (setq-local bazel-buildifier-before-save
                (and (remote-executable-find "buildifier" context) t))))

(use-package bazel
  :ensure t
  :commands (bazel-build bazel-run bazel-test bazel-coverage)
  :mode (("/WORKSPACE\\'" . bazel-workspace-mode)
         ("/WORKSPACE\\.bazel\\'" . bazel-workspace-mode)
         ("/MODULE\\.bazel\\'" . bazel-workspace-mode)
         ("/BUILD\\'" . bazel-mode)
         ("/BUILD\\.bazel\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode))
  :hook ((bazel-mode bazel-workspace-mode) . my/bazel-mode-setup-h)
  :config
  (add-hook 'after-save-hook #'my/bazel-invalidate-cache-after-save-h))

(provide 'init-bazel)
;;; init-bazel.el ends here

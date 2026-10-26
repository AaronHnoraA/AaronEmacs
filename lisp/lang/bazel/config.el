;;; lang/bazel/config.el --- Target-aware Bazel workbench -*- lexical-binding: t; -*-

;;; Commentary:
;; Bazel mode, target discovery, and task execution through the Remote process
;; and project environment contracts.

;;; Code:

(require 'config)
(require 'aaron-funcs)
(require 'tools/output/config)
(require 'remote-fs)
(require 'remote-process)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function +project-local-apply-env "tools/project-local/config" (environment))
(declare-function +project-local-env "tools/project-local/config" (kind &optional root))
(declare-function +direnv-update-environment-maybe "tools/direnv/config" (&optional path))

(defvar +direnv-subprocess-sync-inhibited)
(defgroup +bazel nil
  "Bazel target discovery and execution."
  :group 'tools)

(config-defvar +bazel-command-candidates nil
  "Target-side Bazel executables in preference order."
  :type '(repeat string)
  :group '+bazel)

(config-defvar +bazel-cache-ttl nil
  "Seconds before a cached Bazel target list is refreshed."
  :type 'integer
  :group '+bazel)

(defconst +bazel-cache-directory
  (expand-file-name "var/bazel/" user-emacs-directory)
  "Directory containing per-target Bazel query caches.")

(defconst +bazel-workspace-markers
  '("MODULE.bazel" "WORKSPACE.bazel" "WORKSPACE")
  "Files which identify a Bazel workspace root.")

(defun +bazel-workspace-root (&optional directory)
  "Return the Bazel workspace containing DIRECTORY."
  (let ((directory (or directory default-directory)))
    (or (seq-some (lambda (marker)
                    (locate-dominating-file directory marker))
                  +bazel-workspace-markers)
        (user-error "No Bazel MODULE or WORKSPACE found"))))

(defun +bazel--context (&optional root)
  "Return the Remote context for ROOT or the current Bazel workspace."
  (remote-context (or root (+bazel-workspace-root))))

(defun +bazel-executable (&optional context)
  "Return the first configured Bazel executable available on CONTEXT."
  (let ((context (or context (+bazel--context))))
    (or (seq-some (lambda (program)
                    (remote-executable-find program context))
                  +bazel-command-candidates)
        (user-error "Neither bazelisk nor bazel is available on this target"))))

(defun +bazel--cache-file (root context)
  "Return the target-list cache file for ROOT and CONTEXT."
  (make-directory +bazel-cache-directory t)
  (expand-file-name
   (concat (secure-hash
            'sha256
            (format "%s\0%s" (remote-context-target-id context) root))
           ".eld")
   +bazel-cache-directory))

(defun +bazel--cache-read (file)
  "Return a fresh target list from FILE, or nil."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((value (read (current-buffer))))
            (when (and (listp value)
                       (numberp (plist-get value :time))
                       (< (- (float-time) (plist-get value :time))
                          +bazel-cache-ttl))
              (plist-get value :targets))))
      (error nil))))

(defun +bazel--cache-write (file targets)
  "Persist TARGETS in FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 (list :time (float-time) :targets targets) (current-buffer)))))

(defun +bazel/invalidate-cache (&optional directory)
  "Delete the Bazel target cache associated with DIRECTORY."
  (interactive)
  (condition-case nil
      (let* ((root (+bazel-workspace-root directory))
             (context (+bazel--context root))
             (file (+bazel--cache-file root context)))
        (when (file-exists-p file)
          (delete-file file)))
    (user-error nil)))

(defun +bazel-invalidate-cache-after-save-h ()
  "Invalidate target discovery after saving a Bazel definition file."
  (when (and buffer-file-name
             (or (member (file-name-nondirectory buffer-file-name)
                         '("BUILD" "BUILD.bazel" "MODULE.bazel"
                           "WORKSPACE" "WORKSPACE.bazel"))
                 (string-suffix-p ".bzl" buffer-file-name)))
    (+bazel/invalidate-cache default-directory)))

(defun +bazel--parse-targets (output)
  "Parse Bazel label OUTPUT into a stable target list."
  (sort (delete-dups (split-string output "\n" t "[[:space:]]+"))
        #'string-lessp))

(defun +bazel-with-targets (callback &optional refresh on-error)
  "Call CALLBACK with ROOT, CONTEXT, EXECUTABLE, and discovered targets.
With REFRESH, bypass the persistent cache.  Call ON-ERROR on query failure."
  (let* ((root (file-name-as-directory (+bazel-workspace-root)))
         (context (+bazel--context root))
         (executable (+bazel-executable context))
         (cache-file (+bazel--cache-file root context))
         (cached (and (not refresh) (+bazel--cache-read cache-file))))
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
             (let ((targets (+bazel--parse-targets
                             (remote-exec-result-stdout result))))
               (if targets
                   (progn
                     (condition-case err
                         (+bazel--cache-write cache-file targets)
                       (error
                        (message "Bazel target cache write failed: %s"
                                 (error-message-string err))))
                     (funcall callback root context executable targets))
                 (when on-error
                   (funcall on-error result))
                 (message "No Bazel targets found in %s" root)))
           (when on-error
             (funcall on-error result))
           (message "Bazel query failed (%s): %s"
                    (remote-exec-result-status result)
                    (let ((detail (string-trim
                                   (remote-exec-result-stderr result))))
                      (if (string-empty-p detail) "no error output" detail)))))))))

(defun +bazel--read-target (targets &optional prompt)
  "Read one label from TARGETS."
  (unless targets
    (user-error "No Bazel targets found"))
  (completing-read (or prompt "Bazel target: ") targets nil t))

(defun +bazel--project-environment (kind root)
  "Return KIND project environment for ROOT when the project layer is loaded."
  (when (fboundp '+project-local-env)
    (+project-local-env kind root)))

(defun +bazel--run-command (subcommand root executable target)
  "Run Bazel SUBCOMMAND for TARGET from ROOT with EXECUTABLE."
  (let* ((kind (if (equal subcommand "build") 'build 'task))
         (environment (+bazel--project-environment kind root))
         (command (format "%s %s %s"
                          (shell-quote-argument executable)
                          subcommand
                          (shell-quote-argument target))))
    (when (fboundp '+direnv-update-environment-maybe)
      (+direnv-update-environment-maybe root))
    (let ((default-directory root)
          (process-environment
           (if (fboundp '+project-local-apply-env)
               (+project-local-apply-env environment)
             process-environment))
          (+direnv-subprocess-sync-inhibited t))
      (prog1 (+output-compile command (format "bazel %s" subcommand)
                                environment)
        (+output-record-run kind command root environment)))))

(defun +bazel-run-target (subcommand &optional refresh)
  "Choose a target and run Bazel SUBCOMMAND.
With REFRESH, refresh target discovery first."
  (let ((origin (current-buffer))
        (origin-frame (selected-frame)))
    (+bazel-with-targets
     (lambda (root _context executable targets)
       (when (and (buffer-live-p origin) (frame-live-p origin-frame))
         (with-selected-frame origin-frame
           (with-current-buffer origin
             (+bazel--run-command
              subcommand root executable
              (+bazel--read-target targets
                                     (format "Bazel %s target: " subcommand)))))))
     refresh)))

(defun bazel-target-build (&optional refresh)
  "Choose and build a Bazel target."
  (interactive "P")
  (+bazel-run-target "build" refresh))

(defun bazel-target-test (&optional refresh)
  "Choose and test a Bazel target."
  (interactive "P")
  (+bazel-run-target "test" refresh))

(defun bazel-target-run (&optional refresh)
  "Choose and run a Bazel target."
  (interactive "P")
  (+bazel-run-target "run" refresh))

(defun bazel-target-insert (&optional refresh)
  "Choose and insert a Bazel target label."
  (interactive "P")
  (let ((origin (current-buffer))
        (origin-frame (selected-frame))
        (marker (copy-marker (point) t)))
    (condition-case err
        (+bazel-with-targets
         (lambda (_root _context _executable targets)
           (unwind-protect
               (when (and (buffer-live-p origin)
                          (frame-live-p origin-frame)
                          (marker-buffer marker))
                 (with-selected-frame origin-frame
                   (with-current-buffer origin
                     (let ((target (+bazel--read-target targets)))
                       (goto-char marker)
                       (insert target)))))
             (set-marker marker nil)))
         refresh
         (lambda (_result) (set-marker marker nil)))
      (error
       (set-marker marker nil)
       (signal (car err) (cdr err))))))

(defun bazel-target-jump (&optional refresh)
  "Choose a Bazel target and jump to its definition."
  (interactive "P")
  (let ((origin (current-buffer))
        (origin-frame (selected-frame)))
    (+bazel-with-targets
     (lambda (root context executable targets)
       (when (and (buffer-live-p origin) (frame-live-p origin-frame))
         (with-selected-frame origin-frame
           (with-current-buffer origin
             (let ((target (+bazel--read-target targets)))
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
                          (when (and (buffer-live-p origin)
                                     (frame-live-p origin-frame))
                            (with-selected-frame origin-frame
                              (find-file file)
                              (goto-char (point-min))
                              (forward-line (1- line))
                              (move-to-column (1- column)))))
                      (message "Bazel location query failed: %s"
                               (string-trim
                                (if (string-empty-p (remote-exec-result-stderr result))
                                    output
                                  (remote-exec-result-stderr result)))))))))))))
     refresh)))

(defun +bazel-open-info-directory (key)
  "Open the directory returned by `bazel info KEY'."
  (let* ((origin (current-buffer))
         (origin-frame (selected-frame))
         (root (file-name-as-directory (+bazel-workspace-root)))
         (context (+bazel--context root))
         (executable (+bazel-executable context)))
    (remote-exec-async
     executable :args (list "info" key)
     :context context :filesystem-effects 'none :name "bazel-info"
     :callback
     (lambda (result)
       (if (zerop (remote-exec-result-status result))
           (when (and (buffer-live-p origin) (frame-live-p origin-frame))
             (with-selected-frame origin-frame
               (dired (remote-expand-file-name
                       (string-trim (remote-exec-result-stdout result))
                       root context))))
         (message "bazel info failed: %s"
                  (string-trim (remote-exec-result-stderr result))))))))

(defun bazel-output-bin ()
  "Open the current workspace's Bazel binary output directory."
  (interactive)
  (+bazel-open-info-directory "bazel-bin"))

(defun bazel-output-base ()
  "Open the current workspace's Bazel output directory."
  (interactive)
  (+bazel-open-info-directory "output_base"))

(defun bazel-target-refresh ()
  "Refresh cached Bazel targets for the current workspace."
  (interactive)
  (+bazel-with-targets
   (lambda (_root _context _executable targets)
     (message "Cached %d Bazel targets" (length targets)))
   t))

(transient-define-prefix bazel-target-menu ()
  "Bazel target workbench."
  [["Run"
    ("b" "build" bazel-target-build)
    ("t" "test" bazel-target-test)
    ("r" "run" bazel-target-run)]
   ["Targets"
    ("i" "insert label" bazel-target-insert)
    ("j" "jump to definition" bazel-target-jump)
    ("R" "refresh query" bazel-target-refresh)]
   ["Outputs"
    ("B" "open bazel-bin" bazel-output-bin)
    ("O" "open output base" bazel-output-base)]])

(defun +bazel-mode-setup-h ()
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
  :hook ((bazel-mode bazel-workspace-mode) . +bazel-mode-setup-h)
  :config
  (add-hook 'after-save-hook #'+bazel-invalidate-cache-after-save-h))

(provide 'lang/bazel/config)
;;; lang/bazel/config.el ends here

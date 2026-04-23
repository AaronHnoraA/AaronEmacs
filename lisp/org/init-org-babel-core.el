;;; init-org-babel-core.el --- Org Babel configuration centered on ob-jupyter -*- lexical-binding: t -*-

;;; Commentary:
;; Keep local-only Org Babel languages minimal and route supported compute
;; languages through Jupyter.  The old native and remote execution helpers are
;; intentionally removed so Python and Sage depend on `ob-jupyter'.
;;
;; See docs/jupyter-workflow.org for the current operational model.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-src)
(require 'ob)
(require 'ob-core)
(require 'init-jupyter)
(require 'ob-jupyter)

(declare-function jupyter-canonicalize-language-string "jupyter-kernelspec" (language))
(declare-function jupyter-kernelspec-name "jupyter-kernelspec" (spec))
(declare-function jupyter-kernelspec-plist "jupyter-kernelspec" (spec))
(declare-function jupyter-kernelspecs "jupyter-kernelspec" (&optional directory refresh))
(declare-function org-babel-jupyter-aliases-from-kernelspecs "ob-jupyter" (&optional refresh specs))
(declare-function org-babel-jupyter-language-p "ob-jupyter" (lang))
(declare-function org-babel-jupyter-make-language-alias "ob-jupyter" (kernel lang))
(declare-function org-babel-jupyter-override-src-block "ob-jupyter" (lang))
(declare-function my/jupyter-default-session-for-language "init-jupyter" (language fallback))
(declare-function my/jupyter-preferred-kernels-for-language "init-jupyter" (language fallback))

(defconst my/org-babel-config-root
  (file-name-as-directory
   (expand-file-name
    (or (and (or load-file-name buffer-file-name)
             (locate-dominating-file (or load-file-name buffer-file-name) "init.el"))
        user-emacs-directory)))
  "Root directory of this Emacs configuration.")

(defconst my/org-babel-jupyter-native-backends
  '(("python"
     :kernel-language "python"
     :preferred-kernels ("python3")
     :session "python"
     :mode python
     :extension "py")
    ("sage"
     :kernel-language "sage"
     :preferred-kernels ("sagemath-10.8")
     :session "sage"
     :mode python
     :extension "sage")
    ("sagemath"
     :kernel-language "sage"
     :preferred-kernels ("sagemath-10.8")
     :session "sagemath"
     :mode python
     :extension "sage")
    ("maple"
     :kernel-language "maple"
     :preferred-kernels ("maple")
     :session "maple"
     :mode fundamental
     :extension "mpl"))
  "Native Org source block languages routed through Jupyter.")

(defconst my/org-babel-native-src-lang-modes
  '(("C++" . c++)
    ("cpp" . c++)
    ("bash" . shell)
    ("sh" . shell)
    ("zsh" . shell)
    ("dot" . graphviz-dot)
    ("ditaa" . picture)
    ("rust" . rust))
  "Extra `org-src-lang-modes' mappings for native Babel languages.")

(cl-defstruct (my/org-babel-async-job
               (:constructor my/org-babel-async-job-create))
  id
  buffer
  marker
  hash
  language
  started-at
  process
  source-file
  runner-file
  output-file
  log-buffer)

(defgroup my/org-babel nil
  "Org Babel execution helpers."
  :group 'org)

(defcustom my/org-ditaa-jar-candidates
  '("/opt/homebrew/opt/ditaa/libexec"
    "/usr/local/opt/ditaa/libexec"
    "~/.local/share/ditaa"
    "~/bin")
  "Candidate directories used to locate the system ditaa jar for Org Babel."
  :type '(repeat directory)
  :group 'my/org-babel)

(defcustom my/org-dot-command-candidates
  '("/opt/homebrew/bin/dot"
    "/usr/local/bin/dot")
  "Candidate absolute paths used to locate the system Graphviz dot binary."
  :type '(repeat file)
  :group 'my/org-babel)

(defcustom my/org-babel-async-emacs-program
  (expand-file-name invocation-name invocation-directory)
  "Emacs executable used to run asynchronous Org Babel jobs."
  :type 'file
  :group 'my/org-babel)

(defvar my/org-babel-in-async-child nil
  "Non-nil when this Emacs process is executing an async Babel child job.")

(defvar my/org-babel-async-jobs nil
  "List of active asynchronous Org Babel jobs.")

(defvar my/org-babel-async-job-counter 0
  "Monotonic counter used for Org Babel async job ids.")

(defun my/org-babel--find-dot-command ()
  "Return the first usable system Graphviz dot binary, or nil."
  (or (executable-find "dot")
      (cl-find-if #'file-exists-p
                  (mapcar #'expand-file-name my/org-dot-command-candidates))))

(defun my/org-babel--find-ditaa-jar ()
  "Return the first usable system ditaa jar path, or nil when none is found."
  (or
   (cl-find-if
    #'identity
    (mapcar
     (lambda (dir)
       (let ((path (expand-file-name dir)))
         (when (file-directory-p path)
           (car (sort (directory-files-recursively path "ditaa.*\\.jar\\'")
                      #'string>)))))
     my/org-ditaa-jar-candidates))
   (let ((brew-cellar
          (cl-find-if #'file-directory-p
                      '("/opt/homebrew/Cellar/ditaa"
                        "/usr/local/Cellar/ditaa"))))
     (when brew-cellar
       (car (sort (directory-files-recursively brew-cellar "ditaa.*\\.jar\\'")
                  #'string>))))))

(defun my/org-babel--ensure-output-directory (&optional info)
  "Create the parent directory for the current block's `:file' target.
INFO defaults to the source block at point."
  (let* ((info (or info (org-babel-get-src-block-info)))
         (params (nth 2 info))
         (target (cdr (assq :file params))))
    (when (and (stringp target)
               (not (string-empty-p target)))
      (let ((directory (file-name-directory
                        (expand-file-name target default-directory))))
        (when (and directory (not (file-directory-p directory)))
          (make-directory directory t))))))

(defun my/org-babel--prepare-src-block (info)
  "Normalize execution parameters for source block INFO."
  (let* ((language (car info))
         (params (copy-tree (nth 2 info))))
    (when (and (stringp language)
               (string= (downcase language) "dot")
               (not (assq :cmd params)))
      (when-let* ((dot-command (my/org-babel--find-dot-command)))
        (push (cons :cmd dot-command) params)))
    (setf (nth 2 info) params)
    (my/org-babel--ensure-output-directory info)
    info))

(defun my/org-babel--normalize-boolish (value)
  "Normalize VALUE into a lowercase string."
  (downcase (string-trim (format "%s" (or value "")))))

(defun my/org-babel--yes-p (value)
  "Return non-nil when VALUE means yes."
  (member (my/org-babel--normalize-boolish value)
          '("yes" "y" "t" "true" "1")))

(defun my/org-babel--sessionless-p (params)
  "Return non-nil when PARAMS describe a non-session block."
  (let ((session (cdr (assq :session params))))
    (member (my/org-babel--normalize-boolish session)
            '("" "none" "nil" "no" "false"))))

(defun my/org-babel--jupyter-language-p (language)
  "Return non-nil when LANGUAGE is routed through `ob-jupyter'."
  (and (featurep 'ob-jupyter)
       (org-babel-jupyter-language-p (format "%s" language))))

(defun my/org-babel--current-src-block-element ()
  "Return the current source block element."
  (let ((head (org-babel-where-is-src-block-head)))
    (unless head
      (user-error "Point is not in an Org source block"))
    (save-excursion
      (goto-char head)
      (let ((element (org-element-at-point)))
        (unless (eq (org-element-type element) 'src-block)
          (user-error "Current Org element is not a source block"))
        element))))

(defun my/org-babel--block-hash (begin end)
  "Return a stable hash for the source block between BEGIN and END."
  (md5 (buffer-substring-no-properties begin end)))

(defun my/org-babel--current-src-block-hash ()
  "Return the hash of the current source block."
  (let ((element (my/org-babel--current-src-block-element)))
    (my/org-babel--block-hash
     (org-element-property :begin element)
     (org-element-property :end element))))

(defun my/org-babel--result-block-text (&optional info)
  "Return the literal result block text for INFO, or an empty string."
  (let ((result-beg (org-babel-where-is-src-block-result nil info)))
    (if result-beg
        (save-excursion
          (goto-char result-beg)
          (buffer-substring-no-properties result-beg (org-babel-result-end)))
      "")))

(defun my/org-babel--format-status-result (text)
  "Return TEXT as a simple `#+RESULTS:' block."
  (concat "#+RESULTS:\n"
          (mapconcat (lambda (line) (format ": %s" line))
                     (split-string (string-trim-right text) "\n")
                     "\n")
          "\n"))

(defun my/org-babel--replace-result-block (raw-text)
  "Replace the current source block result with RAW-TEXT."
  (let* ((element (my/org-babel--current-src-block-element))
         (insert-pos (org-element-property :end element)))
    (save-excursion
      (goto-char (org-element-property :begin element))
      (org-babel-remove-result)
      (goto-char insert-pos)
      (when (and raw-text (not (string-empty-p raw-text)))
        (insert raw-text)
        (unless (string-suffix-p "\n" raw-text)
          (insert "\n"))))
    (org-redisplay-inline-images)))

(defun my/org-babel--write-sexp-file (file data)
  "Write DATA into FILE as a printed Lisp expression."
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 data (current-buffer)))))

(defun my/org-babel--read-sexp-file (file)
  "Read and return one Lisp expression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun my/org-babel-async-batch-run (org-file point default-dir output-file)
  "Execute the source block at POINT in ORG-FILE and write results to OUTPUT-FILE.
DEFAULT-DIR becomes the buffer-local `default-directory' before execution."
  (let ((my/org-babel-in-async-child t))
    (condition-case err
        (let ((buffer (find-file-noselect org-file)))
          (with-current-buffer buffer
            (org-mode)
            (setq default-directory default-dir)
            (goto-char (min point (point-max)))
            (org-babel-execute-src-block)
            (goto-char (min point (point-max)))
            (let ((info (org-babel-get-src-block-info)))
              (my/org-babel--write-sexp-file
               output-file
               `(:ok t :result ,(my/org-babel--result-block-text info))))))
      (error
       (let ((backtrace-text
              (with-temp-buffer
                (let ((standard-output (current-buffer)))
                  (backtrace))
                (buffer-string))))
         (my/org-babel--write-sexp-file
          output-file
          `(:ok nil
                :error ,(error-message-string err)
                :debug ,(concat (prin1-to-string err) "\n" backtrace-text))))
       (kill-emacs 1)))))

(defun my/org-babel--job-buffer-live-p (job)
  "Return non-nil when JOB still has a live buffer and marker."
  (and (buffer-live-p (my/org-babel-async-job-buffer job))
       (marker-buffer (my/org-babel-async-job-marker job))))

(defun my/org-babel--job-find-by-position (buffer position)
  "Return the active async job in BUFFER at source block POSITION."
  (cl-find-if
   (lambda (job)
     (and (eq buffer (my/org-babel-async-job-buffer job))
          (marker-buffer (my/org-babel-async-job-marker job))
          (= position (marker-position (my/org-babel-async-job-marker job)))))
   my/org-babel-async-jobs))

(defun my/org-babel--job-cleanup (job)
  "Remove JOB from registries and delete its temp files."
  (setq my/org-babel-async-jobs (delq job my/org-babel-async-jobs))
  (when (buffer-live-p (my/org-babel-async-job-log-buffer job))
    (kill-buffer (my/org-babel-async-job-log-buffer job)))
  (dolist (file (list (my/org-babel-async-job-source-file job)
                      (my/org-babel-async-job-runner-file job)
                      (my/org-babel-async-job-output-file job)))
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file))))
  (when (markerp (my/org-babel-async-job-marker job))
    (set-marker (my/org-babel-async-job-marker job) nil)))

(defun my/org-babel--cleanup-startup-resources (process log-buffer marker files)
  "Clean resources allocated while starting an async Babel job."
  (when (processp process)
    (ignore-errors (set-process-sentinel process #'ignore))
    (when (process-live-p process)
      (ignore-errors (delete-process process))))
  (when (buffer-live-p log-buffer)
    (kill-buffer log-buffer))
  (when (markerp marker)
    (set-marker marker nil))
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file)))))

(defun my/org-babel--job-update-buffer (job raw-text)
  "Replace JOB result in its source buffer with RAW-TEXT."
  (when (my/org-babel--job-buffer-live-p job)
    (with-current-buffer (my/org-babel-async-job-buffer job)
      (save-excursion
        (goto-char (my/org-babel-async-job-marker job))
        (if (not (org-in-src-block-p))
            (message "Org Babel async job %s finished, but the block no longer exists"
                     (my/org-babel-async-job-id job))
          (let ((current-hash (my/org-babel--current-src-block-hash)))
            (if (not (string= current-hash (my/org-babel-async-job-hash job)))
                (message "Org Babel async job %s finished, but the source block changed; results not inserted"
                         (my/org-babel-async-job-id job))
              (my/org-babel--replace-result-block raw-text))))))))

(defun my/org-babel--job-failure-text (job details)
  "Return a formatted failure result block for JOB using DETAILS."
  (my/org-babel--format-status-result
   (format "FAILED job %s\n%s"
           (my/org-babel-async-job-id job)
           (string-trim-right details))))

(defun my/org-babel--async-sentinel (process _event)
  "Handle async Org Babel PROCESS completion."
  (let ((job (process-get process 'my/org-babel-job)))
    (when (and job
               (memq (process-status process) '(exit signal)))
      (unwind-protect
          (cond
           ((process-get process 'my/org-babel-cancelled)
            (my/org-babel--job-update-buffer
             job
             (my/org-babel--format-status-result
              (format "CANCELLED job %s" (my/org-babel-async-job-id job)))))
           ((file-exists-p (my/org-babel-async-job-output-file job))
            (let* ((payload (my/org-babel--read-sexp-file
                             (my/org-babel-async-job-output-file job)))
                   (ok (plist-get payload :ok))
                   (result (plist-get payload :result))
                   (error-text (or (and (plist-get payload :error)
                                        (format "%s\n%s"
                                                (plist-get payload :error)
                                                (or (plist-get payload :debug) "")))
                                   (plist-get payload :debug)
                                   "Async Babel job failed")))
              (if ok
                  (progn
                    (my/org-babel--job-update-buffer job result)
                    (message "Org Babel async job %s finished"
                             (my/org-babel-async-job-id job)))
                (my/org-babel--job-update-buffer
                 job (my/org-babel--job-failure-text job error-text))
                (message "Org Babel async job %s failed: %s"
                         (my/org-babel-async-job-id job) error-text))))
           (t
            (let ((details (if (buffer-live-p (my/org-babel-async-job-log-buffer job))
                               (with-current-buffer (my/org-babel-async-job-log-buffer job)
                                 (string-trim-right (buffer-string)))
                             (format "Process exited with code %s"
                                     (process-exit-status process)))))
              (my/org-babel--job-update-buffer
               job (my/org-babel--job-failure-text job details))
              (message "Org Babel async job %s failed"
                       (my/org-babel-async-job-id job)))))
        (my/org-babel--job-cleanup job)))))

(defun my/org-babel--async-eval-form (source-file point default-dir output-file)
  "Return the batch eval form for SOURCE-FILE at POINT."
  (prin1-to-string
   `(progn
      (setq my/org-babel-in-async-child t)
      (require 'package)
      (package-initialize)
      (require 'use-package)
      (load-file ,(expand-file-name "lisp/init-package-utils.el" my/org-babel-config-root))
      (load-file ,(expand-file-name "lisp/init-org-babel.el" my/org-babel-config-root))
      (my/org-babel-async-batch-run
       ,source-file
       ,point
       ,default-dir
       ,output-file))))

(defun my/org-babel--write-runner-file (file source-file point default-dir output-file)
  "Write an async runner FILE for SOURCE-FILE at POINT."
  (with-temp-file file
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (insert (my/org-babel--async-eval-form source-file point default-dir output-file))
    (insert "\n")))

(defun my/org-babel--start-async-job (&optional info)
  "Start an async Babel job for the current source block.
INFO is the result of `org-babel-get-src-block-info'."
  (let* ((info (or info (org-babel-get-src-block-info)))
         (params (nth 2 info))
         (language (car info))
         (element (my/org-babel--current-src-block-element))
         (begin (org-element-property :begin element))
         (end (org-element-property :end element))
         (hash (my/org-babel--block-hash begin end))
         (buffer (current-buffer))
         (default-dir (expand-file-name default-directory))
         (job-id (format "ob-%04d" (cl-incf my/org-babel-async-job-counter)))
         (source-file (make-temp-file "org-babel-async-" nil ".org"))
         (runner-file (make-temp-file "org-babel-async-runner-" nil ".el"))
         (output-file (let ((file (make-temp-file "org-babel-async-" nil ".el")))
                        (delete-file file)
                        file))
         (log-buffer (generate-new-buffer (format " *org-babel-async-%s*" job-id)))
         (marker (copy-marker begin))
         (existing (my/org-babel--job-find-by-position buffer begin))
         (source-text (save-restriction
                        (widen)
                        (buffer-substring-no-properties (point-min) (point-max))))
         process job started)
    (condition-case err
        (progn
          (unless (my/org-babel--sessionless-p params)
            (user-error "Async Org Babel currently supports only :session none"))
          (when existing
            (my/org-babel-cancel-async-job existing))
          (with-temp-file source-file
            (insert source-text))
          (my/org-babel--write-runner-file runner-file source-file begin default-dir output-file)
          (let ((default-directory default-dir))
            (setq process
                  (make-process
                   :name (format "org-babel-async-%s" job-id)
                   :buffer log-buffer
                   :noquery t
                   :sentinel #'my/org-babel--async-sentinel
                   :command (list my/org-babel-async-emacs-program
                                  "--batch" "-Q"
                                  "-L" (expand-file-name "lisp" my/org-babel-config-root)
                                  "-L" (expand-file-name "lisp/lang" my/org-babel-config-root)
                                  "-l" runner-file))))
          (setq job (my/org-babel-async-job-create
                     :id job-id
                     :buffer buffer
                     :marker marker
                     :hash hash
                     :language language
                     :started-at (current-time)
                     :process process
                     :source-file source-file
                     :runner-file runner-file
                     :output-file output-file
                     :log-buffer log-buffer))
          (process-put process 'my/org-babel-job job)
          (push job my/org-babel-async-jobs)
          (my/org-babel--replace-result-block
           (my/org-babel--format-status-result
            (format "RUNNING job %s (%s)\nUse C-c C-v C-k to cancel"
                    job-id language)))
          (setq started t)
          (message "Started Org Babel async job %s for %s" job-id language)
          job)
      (error
       (unless started
         (setq my/org-babel-async-jobs (delq job my/org-babel-async-jobs))
         (my/org-babel--cleanup-startup-resources
          process log-buffer marker
          (list source-file runner-file output-file)))
       (signal (car err) (cdr err))))))

(defun my/org-babel-cancel-async-job (job)
  "Cancel Org Babel async JOB."
  (let ((process (my/org-babel-async-job-process job)))
    (when (process-live-p process)
      (process-put process 'my/org-babel-cancelled t)
      (kill-process process))
    (message "Cancelled Org Babel async job %s"
             (my/org-babel-async-job-id job))))

(defun my/org-babel-cancel-current-async-job ()
  "Cancel the async Org Babel job for the current source block."
  (interactive)
  (let* ((begin (save-excursion
                  (goto-char (org-babel-where-is-src-block-head))
                  (point)))
         (job (my/org-babel--job-find-by-position (current-buffer) begin)))
    (unless job
      (user-error "No running Org Babel async job for the current block"))
    (my/org-babel-cancel-async-job job)))

(defun my/org-babel-cancel-all-async-jobs ()
  "Cancel all running Org Babel async jobs."
  (interactive)
  (dolist (job (copy-sequence my/org-babel-async-jobs))
    (my/org-babel-cancel-async-job job)))

(defun my/org-babel-execute-src-block-async ()
  "Execute the current Org source block asynchronously."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (my/org-babel--prepare-src-block info)
    (when (my/org-babel--jupyter-language-p (car info))
      (user-error "Jupyter blocks use `:async yes' directly"))
    (my/org-babel--start-async-job info)))

(defun my/org-babel-execute-src-block-a
    (orig-fn &optional arg info params executor-type)
  "Run `org-babel-execute-src-block' asynchronously when `:async yes'."
  (let* ((info (copy-tree (or info (org-babel-get-src-block-info))))
         (_ (cl-callf org-babel-merge-params (nth 2 info) params))
         (_ (my/org-babel--prepare-src-block info))
         (params (nth 2 info))
         (language (car info)))
    (if (or my/org-babel-in-async-child
            (not (called-interactively-p 'interactive)))
        (funcall orig-fn arg info nil executor-type)
      (if (and (my/org-babel--yes-p (cdr (assq :async params)))
               (my/org-babel--sessionless-p params)
               (not (my/org-babel--jupyter-language-p language)))
          (my/org-babel--start-async-job info)
        (funcall orig-fn arg info nil executor-type)))))

(advice-add 'org-babel-execute-src-block :around #'my/org-babel-execute-src-block-a)

(defun my/org-babel-jupyter--normalize-language (language)
  "Normalize a kernelspec LANGUAGE into the Jupyter alias language."
  (let ((language (jupyter-canonicalize-language-string (format "%s" language))))
    (or (cadr (assoc language org-babel-jupyter-language-aliases))
        language)))

(defun my/org-babel-jupyter--available-kernels ()
  "Return an alist mapping kernel languages to available kernel names."
  (let ((default-directory user-emacs-directory)
        available)
    (dolist (spec (or (with-demoted-errors "Error retrieving kernelspecs: %S"
                        (jupyter-kernelspecs user-emacs-directory))
                      nil))
      (let* ((kernel (jupyter-kernelspec-name spec))
             (lang (my/org-babel-jupyter--normalize-language
                    (plist-get (jupyter-kernelspec-plist spec) :language)))
             (slot (assoc lang available)))
        (when (and (stringp kernel) (stringp lang))
          (if slot
              (cl-pushnew kernel (cdr slot) :test #'equal)
            (push (cons lang (list kernel)) available)))))
    (nreverse available)))

(defun my/org-babel-jupyter--pick-kernel (kernels preferred-kernels)
  "Choose a kernel from KERNELS, preferring PREFERRED-KERNELS."
  (or (cl-find-if (lambda (kernel)
                    (member kernel kernels))
                  preferred-kernels)
      (cl-find-if (lambda (kernel)
                    (not (string-match-p "\\`rik_ssh_" kernel)))
                  kernels)
      (car kernels)))

(defun my/org-babel-jupyter--register-language-metadata (entry)
  "Register `org-src' and tangle metadata from Jupyter ENTRY."
  (let* ((lang (car entry))
         (plist (cdr entry))
         (mode (plist-get plist :mode))
         (extension (plist-get plist :extension)))
    (when mode
      (setf (alist-get lang org-src-lang-modes nil nil #'equal) mode))
    (when extension
      (setf (alist-get lang org-babel-tangle-lang-exts nil nil #'equal) extension))))

(use-package ob-rust
  :ensure t
  :defer t)

(defun my/org-babel-jupyter--defaults-symbol (lang)
  "Return the default-header-args symbol for Jupyter LANG."
  (intern (format "org-babel-default-header-args:jupyter-%s" lang)))

(defun my/org-babel-jupyter--set-defaults (lang kernel session)
  "Set the Jupyter defaults for LANG to KERNEL and SESSION."
  (let* ((var (my/org-babel-jupyter--defaults-symbol lang))
         (defaults (if (boundp var)
                       (copy-tree (symbol-value var))
                     (copy-tree org-babel-default-header-args:jupyter))))
    (setf (alist-get :kernel defaults nil nil #'eq) kernel)
    (setf (alist-get :session defaults nil nil #'eq) session)
    (setf (alist-get :async defaults nil nil #'eq) "yes")
    (set var defaults)))

(defun my/org-babel-jupyter-set-language-session (lang session &optional async)
  "Update LANG's Jupyter defaults so new blocks use SESSION."
  (let* ((var (my/org-babel-jupyter--defaults-symbol lang))
         (defaults (if (boundp var)
                       (copy-tree (symbol-value var))
                     (copy-tree org-babel-default-header-args:jupyter))))
    (setf (alist-get :session defaults nil nil #'eq) session)
    (when async
      (setf (alist-get :async defaults nil nil #'eq) async))
    (set var defaults)))

(defun my/org-babel-jupyter--configure-language (entry available-kernels)
  "Route Jupyter ENTRY through AVAILABLE-KERNELS when possible."
  (let* ((lang (car entry))
         (plist (cdr entry))
         (kernel-language (or (plist-get plist :kernel-language) lang))
         (kernels (cdr (assoc kernel-language available-kernels)))
         (preferred-kernels (my/jupyter-preferred-kernels-for-language
                             lang
                             (plist-get plist :preferred-kernels)))
         (kernel (and kernels
                      (my/org-babel-jupyter--pick-kernel
                       kernels
                       preferred-kernels)))
         (session (my/jupyter-default-session-for-language
                   lang
                   (or (plist-get plist :session) lang))))
    (when kernel
      (org-babel-jupyter-make-language-alias kernel lang)
      (my/org-babel-jupyter--set-defaults lang kernel session)
      (org-babel-jupyter-override-src-block lang)
      lang)))

(defun my/org-babel-configure-jupyter-backends ()
  "Configure native Org Babel languages to execute via Jupyter."
  (let ((default-directory user-emacs-directory)
        (available-kernels (my/org-babel-jupyter--available-kernels)))
    (dolist (entry my/org-babel-jupyter-native-backends)
      (my/org-babel-jupyter--register-language-metadata entry))
    (org-babel-jupyter-aliases-from-kernelspecs)
    (dolist (entry my/org-babel-jupyter-native-backends)
      (my/org-babel-jupyter--configure-language entry available-kernels))))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

(dolist (pair my/org-babel-native-src-lang-modes)
  (setf (alist-get (car pair) org-src-lang-modes nil nil #'equal)
        (cdr pair)))

(setq org-babel-load-languages
      (cl-remove-duplicates
       (append
        '((dot . t)
          (ditaa . t)
          (emacs-lisp . t)
          (C . t)
          (shell . t)
          (rust . t))
        (cl-remove-if
         (lambda (pair)
           (memq (car pair)
                 '(C C++ cpp shell bash sh zsh rust
                   python sage sagemath maple)))
         org-babel-load-languages))
       :key #'car))

(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

(setf (alist-get :cmdline org-babel-default-header-args:dot nil nil #'eq)
      "-Tsvg")
(setf (alist-get :results org-babel-default-header-args:ditaa nil nil #'eq)
      "file graphics replace")
(setf (alist-get :exports org-babel-default-header-args:ditaa nil nil #'eq)
      "results")

(when-let* ((ditaa-jar (my/org-babel--find-ditaa-jar)))
  (setq org-ditaa-jar-path ditaa-jar))

(my/org-babel-configure-jupyter-backends)

(unless (assoc 'async org-babel-common-header-args-w-values)
  (add-to-list 'org-babel-common-header-args-w-values '(async (yes no))))

(provide 'init-org-babel-core)
;;; init-org-babel-core.el ends here

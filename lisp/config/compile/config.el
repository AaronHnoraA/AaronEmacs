;;; config/compile/config.el --- Compile helpers for the local Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'bytecomp)
(require 'subr-x)
(require 'transient)

(defvar package-user-dir)

(declare-function aaron/show-warnings-buffer "aaron-utils" ())
(declare-function aaron-open-client-document "aaron-open" (file &optional view))
(declare-function +health/startup-check "config/health/config" ())
(declare-function +health/byte-compile-check "config/health/config" ())
(declare-function +health/native-compile-check "config/health/config" ())

(defgroup +compile nil
  "Compilation helpers for the local Emacs config."
  :group 'convenience)

(defconst +compile-config-root
  (file-name-as-directory (expand-file-name user-emacs-directory))
  "Root directory of this Emacs config.")

(defmacro +compile--on-client (&rest body)
  "Run BODY with this Emacs installation's file and process context."
  (declare (indent 0) (debug t))
  `(let ((default-directory +compile-config-root)
         (process-environment (default-value 'process-environment))
         (exec-path (default-value 'exec-path)))
     (with-temp-buffer
       (setq default-directory +compile-config-root)
       ,@body)))

(defconst +lisp-dir
  (file-name-as-directory (expand-file-name "lisp" +compile-config-root))
  "Directory containing the main Emacs Lisp config modules.")

(defconst +native-comp-cache-dir
  (file-name-as-directory
   (expand-file-name "var/eln-cache" +compile-config-root))
  "Dedicated native compilation cache directory for this config.")

(defconst +compile-board-buffer-name "*Compile Board*"
  "Buffer name used by the compile management board.")

(config-defvar +compile-target-files nil
  "Top-level Emacs Lisp files managed by the compile helpers."
  :type '(repeat string)
  :group '+compile)

(config-defvar +compile-target-directories nil
  "Directories managed by the compile helpers."
  :type '(repeat string)
  :group '+compile)

(config-defvar +compile-third-party-directories nil
  "Extra directories under the config root included by full build commands."
  :type '(repeat string)
  :group '+compile)

(config-defvar +package-enable-native-compile nil
  "If non-nil, let package installation native-compile packages when supported."
  :type 'boolean
  :group '+compile)

(config-defvar +native-comp-enable-jit nil
  "If non-nil, allow JIT native compilation when Emacs supports it."
  :type 'boolean
  :group '+compile)

(config-defvar +native-comp-enable-deferred nil
  "If non-nil, allow deferred native compilation when Emacs supports it."
  :type 'boolean
  :group '+compile)

(config-defvar +native-comp-pop-log nil
  "If non-nil, display the native compilation log buffer after queueing jobs."
  :type 'boolean
  :group '+compile)

(config-defvar +native-comp-async-report-policy nil
  "Value assigned to `native-comp-async-report-warnings-errors' when available."
  :type '(choice (const :tag "Default (nil)" nil) (const :tag "Silent (recommended)" silent) (const :tag "Verbose (t)" t))
  :group '+compile)

(config-defvar +native-comp-verbose nil
  "Value assigned to `native-comp-verbose' when available."
  :type 'integer
  :group '+compile)

(config-defvar +native-comp-speed nil
  "Optimization level used by native compilation when `comp-speed' exists."
  :type 'integer
  :group '+compile)

(config-defvar +native-comp-warning-on-missing-source nil
  "Value assigned to `native-comp-warning-on-missing-source' when available."
  :type 'boolean
  :group '+compile)

(config-defvar +compile-auto-native-on-save nil
  "If non-nil, queue native compilation after saving local Emacs Lisp config files."
  :type 'boolean
  :group '+compile)

(defvar +native-comp--progress-timer nil
  "Timer used to report native compilation progress.")

(defvar +native-comp--progress-start-time nil
  "When native compilation progress reporting started.")

(define-derived-mode +compile-board-mode aaron-ui-board-mode "Compile-Board"
  "Major mode for the compile management board.")

(defun +compile--root-file-paths ()
  "Return existing compile target files as absolute paths."
  (seq-filter
   #'file-exists-p
   (mapcar (lambda (file)
             (expand-file-name file +compile-config-root))
           +compile-target-files)))

(defun +compile--directory-paths ()
  "Return existing compile target directories as absolute paths."
  (seq-filter
   #'file-directory-p
   (mapcar (lambda (dir)
             (file-name-as-directory
              (expand-file-name dir +compile-config-root)))
           +compile-target-directories)))

(defun +compile--target-el-files ()
  "Return all managed Emacs Lisp source files."
  (delete-dups
   (append
    (+compile--root-file-paths)
    (apply #'append
           (mapcar (lambda (dir)
                     (directory-files-recursively dir "\\.el\\'"))
                   (+compile--directory-paths))))))

(defun +compile--third-party-directory-paths ()
  "Return third-party directories included by full build commands."
  (delete-dups
   (append
    (when (and (boundp 'package-user-dir)
               package-user-dir
               (file-directory-p package-user-dir))
      (list (file-name-as-directory (expand-file-name package-user-dir))))
    (seq-filter
     #'file-directory-p
     (mapcar (lambda (dir)
               (file-name-as-directory
                (expand-file-name dir +compile-config-root)))
             +compile-third-party-directories)))))

(defun +compile--elpa-recurse-dir-p (dir)
  "Return non-nil when DIR should be recursed into during ELPA compilation.
Skips test suites and example directories that ship with some packages."
  (let ((base (file-name-nondirectory (directory-file-name dir))))
    (not (string-match-p (rx bos (or "test" "tests" "examples" "example") eos) base))))

(defun +compile--all-el-files ()
  "Return managed and third-party Emacs Lisp source files."
  (delete-dups
   (append
    (+compile--target-el-files)
    (apply #'append
           (mapcar (lambda (dir)
                     (directory-files-recursively
                      dir "\\.el\\'"
                      nil nil #'+compile--elpa-recurse-dir-p))
                   (+compile--third-party-directory-paths))))))

(defun +compile--path-in-config-p (path)
  "Return non-nil when PATH is inside the current Emacs config."
  (and (string-prefix-p +compile-config-root (expand-file-name path))
       (+compile--on-client
         (string-prefix-p +compile-config-root
                          (file-truename (expand-file-name path))))))

(defun +ensure-lisp-dir ()
  "Ensure `+lisp-dir' exists."
  (unless (file-directory-p +lisp-dir)
    (user-error "Directory does not exist: %s" +lisp-dir))
  +lisp-dir)

(defun +compile--byte-dest-file (file)
  "Return the byte-compiled destination for FILE."
  (byte-compile-dest-file file))

(defun +compile--current-managed-file ()
  "Return the current buffer file if it is a managed Emacs Lisp file."
  (when (and buffer-file-name
             (derived-mode-p 'emacs-lisp-mode)
             (+compile--path-in-config-p buffer-file-name))
    (expand-file-name buffer-file-name)))

(defun +compile--refresh-board-if-visible ()
  "Refresh the compile board if it is currently visible."
  (when-let* ((buffer (get-buffer +compile-board-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p '+compile-board-mode)
          (+compile-board/refresh))))))

(defun +compile/apply-runtime-settings ()
  "Apply compile-related runtime knobs for this Emacs session."
  (interactive)
  (when (boundp 'package-native-compile)
    (setq package-native-compile +package-enable-native-compile))
  (when (require 'comp nil t)
    (when (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation +native-comp-enable-jit))
    (when (boundp 'native-comp-deferred-compilation)
      (setq native-comp-deferred-compilation +native-comp-enable-deferred))
    (when (boundp 'native-comp-async-report-warnings-errors)
      (setq native-comp-async-report-warnings-errors
            +native-comp-async-report-policy))
    (when (boundp 'native-comp-verbose)
      (setq native-comp-verbose +native-comp-verbose))
    (when (boundp 'native-comp-warning-on-missing-source)
      (setq native-comp-warning-on-missing-source
            +native-comp-warning-on-missing-source))
    (when (boundp 'comp-speed)
      (setq comp-speed +native-comp-speed))))

(+compile/apply-runtime-settings)

(defun +native-comp-available-p ()
  "Return non-nil if this Emacs supports native compilation."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)
       (fboundp 'native-compile-async)))

(defun +native-comp--log-buffer ()
  "Return an existing native compilation log buffer if any."
  (or (get-buffer "*Async-native-compile-log*")
      (get-buffer "*Native-compile-Log*")
      (get-buffer "*Native-compile-Log*<2>")))

(defun +native-comp/open-log ()
  "Open the native compilation log buffer if it exists."
  (interactive)
  (if-let* ((buffer (+native-comp--log-buffer)))
      (pop-to-buffer buffer)
    (message "No native compilation log buffer yet")))

(defun +native-comp--queue-size ()
  "Return total async native compilation queue size."
  (let ((active (if (boundp 'comp-async-compilations)
                    (if (hash-table-p comp-async-compilations)
                        (hash-table-count comp-async-compilations)
                      (length comp-async-compilations))
                  0))
        (queued (if (boundp 'comp-files-queue)
                    (if (hash-table-p comp-files-queue)
                        (hash-table-count comp-files-queue)
                      (length comp-files-queue))
                  0)))
    (+ active queued)))

(defun +native-comp--start-progress (label)
  "Start minibuffer progress reporting for native compilation of LABEL."
  (setq +native-comp--progress-start-time (float-time))
  (when (timerp +native-comp--progress-timer)
    (cancel-timer +native-comp--progress-timer))
  (setq +native-comp--progress-timer
        (run-with-timer
         1 1
         (lambda ()
           (let ((jobs (+native-comp--queue-size)))
             (if (<= jobs 0)
                 (progn
                   (when (timerp +native-comp--progress-timer)
                     (cancel-timer +native-comp--progress-timer))
                   (setq +native-comp--progress-timer nil)
                   (message "Native compilation completed for %s in %.1fs"
                            label
                            (- (float-time)
                               (or +native-comp--progress-start-time
                                   (float-time)))))
               (message "Native compilation for %s in progress (%d jobs left)"
                        label jobs)))))))

(defun +native-comp--maybe-show-log ()
  "Show the native compilation log buffer a moment later if requested."
  (when +native-comp-pop-log
    (run-with-timer
     0.8 nil
     (lambda ()
       (when-let* ((buffer (+native-comp--log-buffer)))
         (display-buffer buffer))))))

(defun +native-comp--el-to-eln-file (file)
  "Return the `.eln' path corresponding to FILE, or nil."
  (when (and (require 'comp nil t)
             (file-exists-p file))
    (ignore-errors
      (cond
       ((fboundp 'comp-el-to-eln-file)
        (comp-el-to-eln-file file))
       ((fboundp 'comp-el-to-eln-filename)
        (comp-el-to-eln-filename file))))))

(defun +native-comp--delete-config-artifacts ()
  "Delete native compilation artifacts for the managed config files."
  (let ((deleted 0))
    (dolist (file (+compile--target-el-files))
      (when-let* ((eln-file (+native-comp--el-to-eln-file file))
                  ((file-exists-p eln-file)))
        (delete-file eln-file)
        (cl-incf deleted)))
    deleted))

(defun +native-comp/reset-cache ()
  "Delete and recreate the dedicated native compilation cache directory."
  (interactive)
  (+compile--on-client
    (let ((dir +native-comp-cache-dir))
      (when (file-directory-p dir)
        (delete-directory dir t))
      (make-directory dir t)
      (message "Reset native compilation cache: %s"
               (abbreviate-file-name dir))
      (+compile--refresh-board-if-visible))))

(defun +compile--count-existing-elc-files ()
  "Return the number of managed byte-compiled files that currently exist."
  (cl-count-if #'file-exists-p
               (mapcar #'+compile--byte-dest-file
                       (+compile--target-el-files))))

(defun +compile--count-existing-config-eln-files ()
  "Return the number of managed native-compiled files that currently exist."
  (cl-count-if #'file-exists-p
               (delq nil
                     (mapcar #'+native-comp--el-to-eln-file
                             (+compile--target-el-files)))))

(defun +compile--count-cache-eln-files ()
  "Return the number of `.eln' files in the dedicated native cache."
  (if (file-directory-p +native-comp-cache-dir)
      (length (directory-files-recursively +native-comp-cache-dir "\\.eln\\'"))
    0))

(defun +compile--maybe-byte-compile-file (file force)
  "Byte-compile FILE when needed.  FORCE recompiles unconditionally."
  (let ((dest (+compile--byte-dest-file file)))
    (when (or force
              (not (file-exists-p dest))
              (file-newer-than-file-p file dest))
      (byte-compile-file file)
      t)))

(defun +compile/byte-compile-current-file (&optional force)
  "Byte-compile the current managed Emacs Lisp file.
With prefix arg FORCE, recompile unconditionally."
  (interactive "P")
  (let ((file (or (+compile--current-managed-file)
                  (user-error "Current buffer is not a managed Emacs Lisp file"))))
    (save-buffer)
    (+compile--on-client
      (if (+compile--maybe-byte-compile-file file force)
          (message "Byte-compiled %s" (file-name-nondirectory file))
        (message "Byte-compiled file is already up to date: %s"
                 (file-name-nondirectory file))))
    (+compile--refresh-board-if-visible)))

(defun +compile/byte-recompile-lisp-dir (&optional force)
  "Byte-recompile `+lisp-dir' recursively.
With prefix arg FORCE, recompile all files."
  (interactive "P")
  (+compile--on-client
    (let ((base (+ensure-lisp-dir)))
      (byte-recompile-directory base 0 force)
      (message "Byte-recompiled %s (force=%s)"
               (abbreviate-file-name base)
               (and force t))
      (+compile--refresh-board-if-visible))))

(defun +compile/byte-compile-config (&optional force)
  "Byte-compile the managed Emacs config.
With prefix arg FORCE, recompile everything unconditionally."
  (interactive "P")
  (+compile--on-client
    (let ((compiled 0))
      (dolist (file (+compile--root-file-paths))
        (when (+compile--maybe-byte-compile-file file force)
          (cl-incf compiled)))
      (dolist (dir (+compile--directory-paths))
        (byte-recompile-directory dir 0 force))
      (message "Byte-compile finished for config (top-level compiled=%d, force=%s)"
               compiled
               (and force t))
      (+compile--refresh-board-if-visible))))

(defun +compile/byte-compile-all (&optional force)
  "Byte-compile the local config and third-party Elisp directories.
With prefix arg FORCE, recompile everything unconditionally."
  (interactive "P")
  (+compile/byte-compile-config force)
  (+compile--on-client
    (dolist (dir (+compile--third-party-directory-paths))
      (byte-recompile-directory dir 0 force)))
  (message "Byte-compile finished for config + third-party Elisp (force=%s)"
           (and force t))
  (+compile--refresh-board-if-visible))

(defun +compile/native-compile-current-file (&optional force)
  "Queue native compilation for the current managed Emacs Lisp file.
With prefix arg FORCE, delete the file's current `.eln' first."
  (interactive "P")
  (unless (+native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (let ((file (or (+compile--current-managed-file)
                  (user-error "Current buffer is not a managed Emacs Lisp file"))))
    (save-buffer)
    (+compile--on-client
      (+compile/apply-runtime-settings)
      (when force
        (when-let* ((eln-file (+native-comp--el-to-eln-file file))
                    ((file-exists-p eln-file)))
          (delete-file eln-file)))
      (native-compile-async file))
    (+native-comp--maybe-show-log)
    (+native-comp--start-progress (file-name-nondirectory file))
    (message "Queued native compilation for %s" (file-name-nondirectory file))
    (+compile--refresh-board-if-visible)))

(defun +compile/native-compile-lisp-dir (&optional force)
  "Queue native compilation for `+lisp-dir' recursively.
With prefix arg FORCE, delete existing managed `.eln' artifacts first."
  (interactive "P")
  (unless (+native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (+compile--on-client
    (let ((base (+ensure-lisp-dir)))
      (+compile/apply-runtime-settings)
      (when force
        (message "Deleted %d managed native artifacts before recompiling"
                 (+native-comp--delete-config-artifacts)))
      (native-compile-async base t)
      (+native-comp--maybe-show-log)
      (+native-comp--start-progress (abbreviate-file-name base))
      (message "Queued native compilation for %s" (abbreviate-file-name base))
      (+compile--refresh-board-if-visible))))

(defun +compile/native-compile-config (&optional force)
  "Queue native compilation for the managed Emacs config.
With prefix arg FORCE, delete managed `.eln' artifacts first."
  (interactive "P")
  (unless (+native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (+compile--on-client
    (+compile/apply-runtime-settings)
    (when force
      (message "Deleted %d managed native artifacts before recompiling"
               (+native-comp--delete-config-artifacts)))
    (dolist (file (+compile--root-file-paths))
      (native-compile-async file))
    (dolist (dir (+compile--directory-paths))
      (native-compile-async dir t)))
  (+native-comp--maybe-show-log)
  (+native-comp--start-progress "config")
  (message "Queued native compilation for the local Emacs config")
  (+compile--refresh-board-if-visible))

(defun +compile/native-compile-all (&optional force)
  "Synchronously native-compile the local config and third-party Elisp.
With prefix arg FORCE, delete the dedicated ELN cache first."
  (interactive "P")
  (unless (+native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (+compile--on-client
    (+compile/apply-runtime-settings)
    (when force
      (+native-comp/reset-cache))
    (let ((compiled 0) (skipped 0))
      (dolist (file (+compile--all-el-files))
        (condition-case err
            (progn (native-compile file)
                   (cl-incf compiled))
          (error
           (cl-incf skipped)
           (message "Native-compile skipped %s: %s"
                    (file-relative-name file user-emacs-directory)
                    (error-message-string err)))))
      (message "Native-compile finished (compiled=%d, skipped=%d, force=%s)"
               compiled skipped (and force t))))
  (+compile--refresh-board-if-visible))

(defun +compile/build-all (&optional force)
  "Run full byte + native compilation for config and third-party Elisp."
  (interactive "P")
  (+compile/byte-compile-all force)
  (+compile/native-compile-all force))

(defun +compile/clean-byte-artifacts ()
  "Delete managed byte-compiled files."
  (interactive)
  (+compile--on-client
    (let ((deleted 0))
      (dolist (file (+compile--target-el-files))
        (let ((elc-file (+compile--byte-dest-file file)))
          (when (file-exists-p elc-file)
            (delete-file elc-file)
            (cl-incf deleted))))
      (message "Deleted %d managed .elc files" deleted)
      (+compile--refresh-board-if-visible))))

(defun +compile/clean-native-artifacts ()
  "Delete managed native-compiled files for this config."
  (interactive)
  (+compile--on-client
    (message "Deleted %d managed .eln files"
             (+native-comp--delete-config-artifacts)))
  (+compile--refresh-board-if-visible))

(defun +compile/clean-all-artifacts ()
  "Delete managed byte-compiled and native-compiled artifacts."
  (interactive)
  (+compile/clean-byte-artifacts)
  (+compile/clean-native-artifacts)
  (message "Deleted managed .elc and .eln artifacts")
  (+compile--refresh-board-if-visible))

(defun +compile-auto-native-on-save ()
  "Queue native compilation when saving managed Emacs Lisp config files."
  (when (and +compile-auto-native-on-save
             (+native-comp-available-p)
             (+compile--current-managed-file))
    (let ((file buffer-file-name))
      (+compile--on-client
        (native-compile-async file)))
    (message "Auto-queued native compilation for %s"
             (file-name-nondirectory buffer-file-name))
    (+compile--refresh-board-if-visible)))

(add-hook 'after-save-hook #'+compile-auto-native-on-save)

(defun +compile/toggle-auto-native-on-save ()
  "Toggle `+compile-auto-native-on-save'."
  (interactive)
  (setq +compile-auto-native-on-save
        (not +compile-auto-native-on-save))
  (message "Auto native compile on save %s"
           (if +compile-auto-native-on-save "enabled" "disabled"))
  (+compile--refresh-board-if-visible))

(defun +compile/open-docs ()
  "Open the maintenance documentation for compile and cleanup workflows."
  (interactive)
  (aaron-open-client-document
   (expand-file-name "docs/ops/maintenance.md" +compile-config-root)))

(defun +compile-board--insert-button (label action help)
  "Insert a text button with LABEL, ACTION, and HELP."
  (insert-text-button
   label
   'action action
   'follow-link t
   'help-echo help))

(defun +compile-board--insert-openable-path (path)
  "Insert PATH as a clickable button."
  (aaron-ui-board-insert-openable-path path))

(defun +compile-board/refresh ()
  "Refresh the compile management board."
  (interactive)
  (unless (derived-mode-p '+compile-board-mode)
    (user-error "Not in the Compile Board"))
  (let ((inhibit-read-only t)
        (target-files (+compile--target-el-files))
        (native-available (+native-comp-available-p)))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Compile Board"
        :icon 'compile
        :stats (list
                (cons (format "%d .el" (length target-files)) nil)
                (cons (format "%d .elc" (+compile--count-existing-elc-files)) nil)
                (cons (format "%d .eln" (+compile--count-existing-config-eln-files)) nil))
        :actions '((:label "Dispatch" :command config-compile-menu :primary t :help "Transient menu")
                   (:label "Docs"     :command +compile/open-docs             :help "Maintenance docs")))

       ;; Overview
       (aaron-ui-board-insert-section "Overview")
       (aaron-ui-board-insert-field "Emacs" emacs-version)
       (insert "   " (propertize (format "%-16s" "Config root") 'face 'aaron-ui-board-meta))
       (+compile-board--insert-openable-path +compile-config-root)
       (insert "\n")
       (insert "   " (propertize (format "%-16s" "ELN cache") 'face 'aaron-ui-board-meta))
       (+compile-board--insert-openable-path +native-comp-cache-dir)
       (insert "\n\n")

       ;; Native Compile
       (aaron-ui-board-insert-section
        "Native Compile" nil
        (if native-available 'success 'warning))
       (aaron-ui-board-insert-field
        "Available" (if native-available "yes" "no")
        (if native-available 'aaron-ui-board-good 'aaron-ui-board-warn))
       (aaron-ui-board-insert-field
        "Package compile"
        (if (and (boundp 'package-native-compile) package-native-compile) "enabled" "disabled"))
       (aaron-ui-board-insert-field
        "JIT"
        (if (and (boundp 'native-comp-jit-compilation) native-comp-jit-compilation)
            "enabled" "disabled"))
       (aaron-ui-board-insert-field
        "Warning policy"
        (if (boundp 'native-comp-async-report-warnings-errors)
            (format "%S" native-comp-async-report-warnings-errors)
          (format "%S" +native-comp-async-report-policy)))
       (aaron-ui-board-insert-field
        "comp-speed"
        (if (boundp 'comp-speed)
            (number-to-string comp-speed)
          (number-to-string +native-comp-speed)))
       (aaron-ui-board-insert-field "Queue size" (number-to-string (+native-comp--queue-size)))
       (aaron-ui-board-insert-field
        "Cached .eln" (number-to-string (+compile--count-cache-eln-files)))
       (insert "\n")

       ;; Automation
       (aaron-ui-board-insert-section "Automation")
       (aaron-ui-board-insert-field
        "Auto native on save"
        (if +compile-auto-native-on-save "enabled" "disabled")
        (if +compile-auto-native-on-save 'aaron-ui-board-good 'aaron-ui-board-meta))
       (insert "\n")

       ;; Actions
       (aaron-ui-board-insert-section "Actions")
       (insert "   ")
       (aaron-ui-board-insert-actions
        '((:label "Byte Config"    :command +compile/byte-compile-config         :primary t :help "Byte-compile config")
          (:label "Native Config"  :command +compile/native-compile-config       :help "Native-compile config")
          (:label "Byte Current"   :command +compile/byte-compile-current-file   :help "Byte-compile current file")
          (:label "Native Current" :command +compile/native-compile-current-file :help "Native-compile current file")))
       (insert "\n   ")
       (aaron-ui-board-insert-actions
        '((:label "Clean .elc"     :command +compile/clean-byte-artifacts   :help "Delete .elc files")
          (:label "Clean .eln"     :command +compile/clean-native-artifacts :help "Delete .eln files")
          (:label "Reset ELN"      :command +native-comp/reset-cache        :help "Reset ELN cache")
          (:label "Native Log"     :command +native-comp/open-log           :help "Open native log")
          (:label "Auto Native"    :command +compile/toggle-auto-native-on-save
                  :help "Toggle auto native on save")))
       (insert "\n\n")

       ;; Health hint
       (aaron-ui-board-insert-section "Health")
       (aaron-ui-board-insert-key-hints
        "Keys: g refresh  b byte  n native  f byte-file  F native-file  c clean .elc  C clean .eln  s smoke  ? dispatch  q quit")))))

(defun +compile/board ()
  "Open the compile management board."
  (interactive)
  (let ((buffer (get-buffer-create +compile-board-buffer-name)))
    (with-current-buffer buffer
      (+compile-board-mode)
      (setq-local default-directory +compile-config-root
                  process-environment (default-value 'process-environment)
                  exec-path (default-value 'exec-path))
      (aaron-ui-board-set-header "Compile Board" 'compile)
      (setq-local aaron-ui-board-refresh-function #'+compile-board/refresh)
      (let ((map (copy-keymap special-mode-map)))
        (use-local-map map)
        (local-set-key (kbd "g") #'+compile-board/refresh)
        (local-set-key (kbd "b") #'+compile/byte-compile-config)
        (local-set-key (kbd "B") (lambda () (interactive) (+compile/byte-compile-config t)))
        (local-set-key (kbd "n") #'+compile/native-compile-config)
        (local-set-key (kbd "N") (lambda () (interactive) (+compile/native-compile-config t)))
        (local-set-key (kbd "f") #'+compile/byte-compile-current-file)
        (local-set-key (kbd "F") #'+compile/native-compile-current-file)
        (local-set-key (kbd "c") #'+compile/clean-byte-artifacts)
        (local-set-key (kbd "C") #'+compile/clean-native-artifacts)
        (local-set-key (kbd "X") #'+native-comp/reset-cache)
        (local-set-key (kbd "l") #'+native-comp/open-log)
        (local-set-key (kbd "o") #'+compile/open-docs)
        (local-set-key (kbd "t") #'+compile/toggle-auto-native-on-save)
        (local-set-key (kbd "?") #'config-compile-menu)
        (local-set-key (kbd "s")
                       (lambda ()
                         (interactive)
                         (if (fboundp '+health/startup-check)
                             (+health/startup-check)
                           (user-error "Health helpers are not loaded yet"))))
        (local-set-key (kbd "y")
                       (lambda ()
                         (interactive)
                         (if (fboundp '+health/byte-compile-check)
                             (+health/byte-compile-check)
                           (user-error "Health helpers are not loaded yet"))))
        (local-set-key (kbd "Y")
                       (lambda ()
                         (interactive)
                         (if (fboundp '+health/native-compile-check)
                             (+health/native-compile-check)
                           (user-error "Health helpers are not loaded yet")))))
      (+compile-board/refresh))
    (pop-to-buffer buffer)))

(transient-define-prefix config-compile-menu ()
  "Compile and cleanup workflow for the local Emacs config."
  [["Board"
    ("b" "open board" +compile/board)
    ("o" "maintenance docs" +compile/open-docs)]
   ["Compile"
    ("e" "byte config" +compile/byte-compile-config)
    ("E" "force byte config" (lambda () (interactive) (+compile/byte-compile-config t)))
    ("n" "native config" +compile/native-compile-config)
    ("N" "force native config" (lambda () (interactive) (+compile/native-compile-config t)))
    ("f" "byte current file" +compile/byte-compile-current-file)
    ("F" "native current file" +compile/native-compile-current-file)]
   ["Clean"
    ("c" "clean .elc" +compile/clean-byte-artifacts)
    ("C" "clean config .eln" +compile/clean-native-artifacts)
    ("X" "reset eln cache" +native-comp/reset-cache)
    ("a" "clean all managed artifacts" +compile/clean-all-artifacts)]
   ["Health"
    ("s" "startup smoke"
     (lambda ()
       (interactive)
       (if (fboundp '+health/startup-check)
           (+health/startup-check)
         (user-error "Health helpers are not loaded yet"))))
    ("y" "byte smoke"
     (lambda ()
       (interactive)
       (if (fboundp '+health/byte-compile-check)
           (+health/byte-compile-check)
         (user-error "Health helpers are not loaded yet"))))
    ("Y" "native smoke"
     (lambda ()
       (interactive)
       (if (fboundp '+health/native-compile-check)
           (+health/native-compile-check)
         (user-error "Health helpers are not loaded yet"))))
    ("l" "native log" +native-comp/open-log)
    ("t" "toggle auto native on save" +compile/toggle-auto-native-on-save)]])

(defalias 'config-compile #'+compile/board
  "Open the Emacs configuration compile board.")
(define-leader-key!
  "cb" '(:def +compile/board :which-key "compile board")
  "cB" '(:def config-compile-menu :which-key "compile menu"))

;; Popup rule for this module's buffers (see ui/popup).
(defvar +display-rules-bottom-size)
(set-popup-rule! 'compilation-mode :select t :align 'below :size +display-rules-bottom-size)

;; Boards start in Emacs state.
(set-evil-initial-state! '+compile-board-mode 'emacs)

(provide 'config/compile/config)
;;; config/compile/config.el ends here

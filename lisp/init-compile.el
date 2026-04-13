;;; init-compile.el --- Compile helpers for the local Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'subr-x)
(require 'transient)

(defvar package-user-dir)

(declare-function my/evil-global-leader-set "init-funcs" (key def desc))
(declare-function my/show-warnings-buffer "init-utils" ())
(declare-function my/health-startup-check "init-health" ())
(declare-function my/health-byte-compile-check "init-health" ())
(declare-function my/health-native-compile-check "init-health" ())

(defgroup my/compile nil
  "Compilation helpers for the local Emacs config."
  :group 'convenience)

(defconst my/compile-config-root
  (file-name-as-directory (expand-file-name user-emacs-directory))
  "Root directory of this Emacs config.")

(defconst my/lisp-dir
  (file-name-as-directory (expand-file-name "lisp" my/compile-config-root))
  "Directory containing the main Emacs Lisp config modules.")

(defconst my/native-comp-cache-dir
  (file-name-as-directory
   (expand-file-name "var/eln-cache" my/compile-config-root))
  "Dedicated native compilation cache directory for this config.")

(defconst my/compile-board-buffer-name "*Compile Board*"
  "Buffer name used by the compile management board.")

(defcustom my/compile-target-files
  '("early-init.el" "init.el" "bootstrap.el")
  "Top-level Emacs Lisp files managed by the compile helpers."
  :type '(repeat string)
  :group 'my/compile)

(defcustom my/compile-target-directories
  '("lisp")
  "Directories managed by the compile helpers."
  :type '(repeat string)
  :group 'my/compile)

(defcustom my/compile-third-party-directories
  '("site-lisp")
  "Extra directories under the config root included by full build commands."
  :type '(repeat string)
  :group 'my/compile)

(defcustom my/package-enable-native-compile t
  "If non-nil, let package installation native-compile packages when supported."
  :type 'boolean
  :group 'my/compile)

(defcustom my/native-comp-enable-jit t
  "If non-nil, allow JIT native compilation when Emacs supports it."
  :type 'boolean
  :group 'my/compile)

(defcustom my/native-comp-enable-deferred t
  "If non-nil, allow deferred native compilation when Emacs supports it."
  :type 'boolean
  :group 'my/compile)

(defcustom my/native-comp-pop-log nil
  "If non-nil, display the native compilation log buffer after queueing jobs."
  :type 'boolean
  :group 'my/compile)

(defcustom my/native-comp-async-report-policy 'silent
  "Value assigned to `native-comp-async-report-warnings-errors' when available."
  :type '(choice (const :tag "Default (nil)" nil)
                 (const :tag "Silent (recommended)" silent)
                 (const :tag "Verbose (t)" t))
  :group 'my/compile)

(defcustom my/native-comp-verbose 0
  "Value assigned to `native-comp-verbose' when available."
  :type 'integer
  :group 'my/compile)

(defcustom my/native-comp-speed 2
  "Optimization level used by native compilation when `comp-speed' exists."
  :type 'integer
  :group 'my/compile)

(defcustom my/native-comp-warning-on-missing-source nil
  "Value assigned to `native-comp-warning-on-missing-source' when available."
  :type 'boolean
  :group 'my/compile)

(defcustom my/compile-auto-native-on-save nil
  "If non-nil, queue native compilation after saving local Emacs Lisp config files."
  :type 'boolean
  :group 'my/compile)

(defvar my/native-comp--progress-timer nil
  "Timer used to report native compilation progress.")

(defvar my/native-comp--progress-start-time nil
  "When native compilation progress reporting started.")

(define-derived-mode my/compile-board-mode special-mode "Compile-Board"
  "Major mode for the compile management board.")

(defun my/compile--root-file-paths ()
  "Return existing compile target files as absolute paths."
  (seq-filter
   #'file-exists-p
   (mapcar (lambda (file)
             (expand-file-name file my/compile-config-root))
           my/compile-target-files)))

(defun my/compile--directory-paths ()
  "Return existing compile target directories as absolute paths."
  (seq-filter
   #'file-directory-p
   (mapcar (lambda (dir)
             (file-name-as-directory
              (expand-file-name dir my/compile-config-root)))
           my/compile-target-directories)))

(defun my/compile--target-el-files ()
  "Return all managed Emacs Lisp source files."
  (delete-dups
   (append
    (my/compile--root-file-paths)
    (apply #'append
           (mapcar (lambda (dir)
                     (directory-files-recursively dir "\\.el\\'"))
                   (my/compile--directory-paths))))))

(defun my/compile--third-party-directory-paths ()
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
                (expand-file-name dir my/compile-config-root)))
             my/compile-third-party-directories)))))

(defun my/compile--all-el-files ()
  "Return managed and third-party Emacs Lisp source files."
  (delete-dups
   (append
    (my/compile--target-el-files)
    (apply #'append
           (mapcar (lambda (dir)
                     (directory-files-recursively dir "\\.el\\'"))
                   (my/compile--third-party-directory-paths))))))

(defun my/compile--path-in-config-p (path)
  "Return non-nil when PATH is inside the current Emacs config."
  (string-prefix-p my/compile-config-root
                   (file-truename (expand-file-name path))))

(defun my/ensure-lisp-dir ()
  "Ensure `my/lisp-dir' exists."
  (unless (file-directory-p my/lisp-dir)
    (user-error "Directory does not exist: %s" my/lisp-dir))
  my/lisp-dir)

(defun my/compile--byte-dest-file (file)
  "Return the byte-compiled destination for FILE."
  (byte-compile-dest-file file))

(defun my/compile--current-managed-file ()
  "Return the current buffer file if it is a managed Emacs Lisp file."
  (when (and buffer-file-name
             (derived-mode-p 'emacs-lisp-mode)
             (my/compile--path-in-config-p buffer-file-name))
    (expand-file-name buffer-file-name)))

(defun my/compile--refresh-board-if-visible ()
  "Refresh the compile board if it is currently visible."
  (when-let* ((buffer (get-buffer my/compile-board-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'my/compile-board-mode)
          (my/compile-board-refresh))))))

(defun my/compile-apply-runtime-settings ()
  "Apply compile-related runtime knobs for this Emacs session."
  (interactive)
  (when (boundp 'package-native-compile)
    (setq package-native-compile my/package-enable-native-compile))
  (when (require 'comp nil t)
    (when (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation my/native-comp-enable-jit))
    (when (boundp 'native-comp-deferred-compilation)
      (setq native-comp-deferred-compilation my/native-comp-enable-deferred))
    (when (boundp 'native-comp-async-report-warnings-errors)
      (setq native-comp-async-report-warnings-errors
            my/native-comp-async-report-policy))
    (when (boundp 'native-comp-verbose)
      (setq native-comp-verbose my/native-comp-verbose))
    (when (boundp 'native-comp-warning-on-missing-source)
      (setq native-comp-warning-on-missing-source
            my/native-comp-warning-on-missing-source))
    (when (boundp 'comp-speed)
      (setq comp-speed my/native-comp-speed))))

(my/compile-apply-runtime-settings)

(defun my/native-comp-available-p ()
  "Return non-nil if this Emacs supports native compilation."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)
       (fboundp 'native-compile-async)))

(defun my/native-comp--log-buffer ()
  "Return an existing native compilation log buffer if any."
  (or (get-buffer "*Async-native-compile-log*")
      (get-buffer "*Native-compile-Log*")
      (get-buffer "*Native-compile-Log*<2>")))

(defun my/native-comp-open-log ()
  "Open the native compilation log buffer if it exists."
  (interactive)
  (if-let* ((buffer (my/native-comp--log-buffer)))
      (pop-to-buffer buffer)
    (message "No native compilation log buffer yet")))

(defun my/native-comp--queue-size ()
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

(defun my/native-comp--start-progress (label)
  "Start minibuffer progress reporting for native compilation of LABEL."
  (setq my/native-comp--progress-start-time (float-time))
  (when (timerp my/native-comp--progress-timer)
    (cancel-timer my/native-comp--progress-timer))
  (setq my/native-comp--progress-timer
        (run-with-timer
         1 1
         (lambda ()
           (let ((jobs (my/native-comp--queue-size)))
             (if (<= jobs 0)
                 (progn
                   (when (timerp my/native-comp--progress-timer)
                     (cancel-timer my/native-comp--progress-timer))
                   (setq my/native-comp--progress-timer nil)
                   (message "Native compilation completed for %s in %.1fs"
                            label
                            (- (float-time)
                               (or my/native-comp--progress-start-time
                                   (float-time)))))
               (message "Native compilation for %s in progress (%d jobs left)"
                        label jobs)))))))

(defun my/native-comp--maybe-show-log ()
  "Show the native compilation log buffer a moment later if requested."
  (when my/native-comp-pop-log
    (run-with-timer
     0.8 nil
     (lambda ()
       (when-let* ((buffer (my/native-comp--log-buffer)))
         (display-buffer buffer))))))

(defun my/native-comp--el-to-eln-file (file)
  "Return the `.eln' path corresponding to FILE, or nil."
  (when (and (require 'comp nil t)
             (file-exists-p file))
    (ignore-errors
      (cond
       ((fboundp 'comp-el-to-eln-file)
        (comp-el-to-eln-file file))
       ((fboundp 'comp-el-to-eln-filename)
        (comp-el-to-eln-filename file))))))

(defun my/native-comp--delete-config-artifacts ()
  "Delete native compilation artifacts for the managed config files."
  (let ((deleted 0))
    (dolist (file (my/compile--target-el-files))
      (when-let* ((eln-file (my/native-comp--el-to-eln-file file))
                  ((file-exists-p eln-file)))
        (delete-file eln-file)
        (cl-incf deleted)))
    deleted))

(defun my/native-comp-reset-cache ()
  "Delete and recreate the dedicated native compilation cache directory."
  (interactive)
  (let ((dir my/native-comp-cache-dir))
    (when (file-directory-p dir)
      (delete-directory dir t))
    (make-directory dir t)
    (message "Reset native compilation cache: %s"
             (abbreviate-file-name dir))
    (my/compile--refresh-board-if-visible)))

(defun my/compile--count-existing-elc-files ()
  "Return the number of managed byte-compiled files that currently exist."
  (cl-count-if #'file-exists-p
               (mapcar #'my/compile--byte-dest-file
                       (my/compile--target-el-files))))

(defun my/compile--count-existing-config-eln-files ()
  "Return the number of managed native-compiled files that currently exist."
  (cl-count-if #'file-exists-p
               (delq nil
                     (mapcar #'my/native-comp--el-to-eln-file
                             (my/compile--target-el-files)))))

(defun my/compile--count-cache-eln-files ()
  "Return the number of `.eln' files in the dedicated native cache."
  (if (file-directory-p my/native-comp-cache-dir)
      (length (directory-files-recursively my/native-comp-cache-dir "\\.eln\\'"))
    0))

(defun my/compile--maybe-byte-compile-file (file force)
  "Byte-compile FILE when needed.  FORCE recompiles unconditionally."
  (let ((dest (my/compile--byte-dest-file file)))
    (when (or force
              (not (file-exists-p dest))
              (file-newer-than-file-p file dest))
      (byte-compile-file file)
      t)))

(defun my/byte-compile-current-file (&optional force)
  "Byte-compile the current managed Emacs Lisp file.
With prefix arg FORCE, recompile unconditionally."
  (interactive "P")
  (let ((file (or (my/compile--current-managed-file)
                  (user-error "Current buffer is not a managed Emacs Lisp file"))))
    (save-buffer)
    (if (my/compile--maybe-byte-compile-file file force)
        (message "Byte-compiled %s" (file-name-nondirectory file))
      (message "Byte-compiled file is already up to date: %s"
               (file-name-nondirectory file)))
    (my/compile--refresh-board-if-visible)))

(defun my/byte-recompile-lisp-dir (&optional force)
  "Byte-recompile `my/lisp-dir' recursively.
With prefix arg FORCE, recompile all files."
  (interactive "P")
  (let ((base (my/ensure-lisp-dir)))
    (byte-recompile-directory base 0 force)
    (message "Byte-recompiled %s (force=%s)"
             (abbreviate-file-name base)
             (and force t))
    (my/compile--refresh-board-if-visible)))

(defun my/byte-compile-config (&optional force)
  "Byte-compile the managed Emacs config.
With prefix arg FORCE, recompile everything unconditionally."
  (interactive "P")
  (let ((compiled 0))
    (dolist (file (my/compile--root-file-paths))
      (when (my/compile--maybe-byte-compile-file file force)
        (cl-incf compiled)))
    (dolist (dir (my/compile--directory-paths))
      (byte-recompile-directory dir 0 force))
    (message "Byte-compile finished for config (top-level compiled=%d, force=%s)"
             compiled
             (and force t))
    (my/compile--refresh-board-if-visible)))

(defun my/byte-compile-all (&optional force)
  "Byte-compile the local config and third-party Elisp directories.
With prefix arg FORCE, recompile everything unconditionally."
  (interactive "P")
  (my/byte-compile-config force)
  (dolist (dir (my/compile--third-party-directory-paths))
    (byte-recompile-directory dir 0 force))
  (message "Byte-compile finished for config + third-party Elisp (force=%s)"
           (and force t))
  (my/compile--refresh-board-if-visible))

(defun my/native-compile-current-file (&optional force)
  "Queue native compilation for the current managed Emacs Lisp file.
With prefix arg FORCE, delete the file's current `.eln' first."
  (interactive "P")
  (unless (my/native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (let ((file (or (my/compile--current-managed-file)
                  (user-error "Current buffer is not a managed Emacs Lisp file"))))
    (save-buffer)
    (my/compile-apply-runtime-settings)
    (when force
      (when-let* ((eln-file (my/native-comp--el-to-eln-file file))
                  ((file-exists-p eln-file)))
        (delete-file eln-file)))
    (native-compile-async file)
    (my/native-comp--maybe-show-log)
    (my/native-comp--start-progress (file-name-nondirectory file))
    (message "Queued native compilation for %s" (file-name-nondirectory file))
    (my/compile--refresh-board-if-visible)))

(defun my/native-compile-lisp-dir (&optional force)
  "Queue native compilation for `my/lisp-dir' recursively.
With prefix arg FORCE, delete existing managed `.eln' artifacts first."
  (interactive "P")
  (unless (my/native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (let ((base (my/ensure-lisp-dir)))
    (my/compile-apply-runtime-settings)
    (when force
      (message "Deleted %d managed native artifacts before recompiling"
               (my/native-comp--delete-config-artifacts)))
    (native-compile-async base t)
    (my/native-comp--maybe-show-log)
    (my/native-comp--start-progress (abbreviate-file-name base))
    (message "Queued native compilation for %s" (abbreviate-file-name base))
    (my/compile--refresh-board-if-visible)))

(defun my/native-compile-config (&optional force)
  "Queue native compilation for the managed Emacs config.
With prefix arg FORCE, delete managed `.eln' artifacts first."
  (interactive "P")
  (unless (my/native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (my/compile-apply-runtime-settings)
  (when force
    (message "Deleted %d managed native artifacts before recompiling"
             (my/native-comp--delete-config-artifacts)))
  (dolist (file (my/compile--root-file-paths))
    (native-compile-async file))
  (dolist (dir (my/compile--directory-paths))
    (native-compile-async dir t))
  (my/native-comp--maybe-show-log)
  (my/native-comp--start-progress "config")
  (message "Queued native compilation for the local Emacs config")
  (my/compile--refresh-board-if-visible))

(defun my/native-compile-all (&optional force)
  "Synchronously native-compile the local config and third-party Elisp.
With prefix arg FORCE, delete the dedicated ELN cache first."
  (interactive "P")
  (unless (my/native-comp-available-p)
    (user-error "Native compilation is not available in this Emacs"))
  (my/compile-apply-runtime-settings)
  (when force
    (my/native-comp-reset-cache))
  (let ((compiled 0))
    (dolist (file (my/compile--all-el-files))
      (native-compile file)
      (cl-incf compiled))
    (message "Native-compile finished for config + third-party Elisp (files=%d, force=%s)"
             compiled
             (and force t)))
  (my/compile--refresh-board-if-visible))

(defun my/build-all (&optional force)
  "Run full byte + native compilation for config and third-party Elisp."
  (interactive "P")
  (my/byte-compile-all force)
  (my/native-compile-all force))

(defun my/compile-clean-byte-artifacts ()
  "Delete managed byte-compiled files."
  (interactive)
  (let ((deleted 0))
    (dolist (file (my/compile--target-el-files))
      (let ((elc-file (my/compile--byte-dest-file file)))
        (when (file-exists-p elc-file)
          (delete-file elc-file)
          (cl-incf deleted))))
    (message "Deleted %d managed .elc files" deleted)
    (my/compile--refresh-board-if-visible)))

(defun my/compile-clean-native-artifacts ()
  "Delete managed native-compiled files for this config."
  (interactive)
  (message "Deleted %d managed .eln files"
           (my/native-comp--delete-config-artifacts))
  (my/compile--refresh-board-if-visible))

(defun my/compile-clean-all-artifacts ()
  "Delete managed byte-compiled and native-compiled artifacts."
  (interactive)
  (my/compile-clean-byte-artifacts)
  (my/compile-clean-native-artifacts)
  (message "Deleted managed .elc and .eln artifacts")
  (my/compile--refresh-board-if-visible))

(defun my/compile-auto-native-on-save ()
  "Queue native compilation when saving managed Emacs Lisp config files."
  (when (and my/compile-auto-native-on-save
             (my/native-comp-available-p)
             (my/compile--current-managed-file))
    (native-compile-async buffer-file-name)
    (message "Auto-queued native compilation for %s"
             (file-name-nondirectory buffer-file-name))
    (my/compile--refresh-board-if-visible)))

(add-hook 'after-save-hook #'my/compile-auto-native-on-save)

(defun my/compile-toggle-auto-native-on-save ()
  "Toggle `my/compile-auto-native-on-save'."
  (interactive)
  (setq my/compile-auto-native-on-save
        (not my/compile-auto-native-on-save))
  (message "Auto native compile on save %s"
           (if my/compile-auto-native-on-save "enabled" "disabled"))
  (my/compile--refresh-board-if-visible))

(defun my/compile-open-docs ()
  "Open the maintenance documentation for compile and cleanup workflows."
  (interactive)
  (find-file (expand-file-name "docs/maintenance.md" my/compile-config-root)))

(defun my/compile-board--insert-button (label action help)
  "Insert a text button with LABEL, ACTION, and HELP."
  (insert-text-button
   label
   'action action
   'follow-link t
   'help-echo help))

(defun my/compile-board--insert-openable-path (path)
  "Insert PATH as a button."
  (my/compile-board--insert-button
   (abbreviate-file-name (expand-file-name path))
   (lambda (_button)
     (find-file path))
   "Open this path"))

(defun my/compile-board-refresh ()
  "Refresh the compile management board."
  (interactive)
  (let ((inhibit-read-only t)
        (target-files (my/compile--target-el-files))
        (native-available (my/native-comp-available-p)))
    (erase-buffer)
    (insert "Compile Board\n")
    (insert "=============\n\n")
    (insert "Keys: g refresh, b byte config, B force byte, n native config, N force native, f byte file, F native file, c clean .elc, C clean config .eln, X reset eln cache, l native log, s startup smoke, y byte smoke, Y native smoke, t toggle auto native on save, o docs, ? dispatch, q quit\n\n")

    (insert "Overview\n")
    (insert "--------\n")
    (insert (format "%-24s %s\n" "Emacs" emacs-version))
    (insert (format "%-24s " "Config root"))
    (my/compile-board--insert-openable-path my/compile-config-root)
    (insert "\n")
    (insert (format "%-24s %d\n" "Managed .el files" (length target-files)))
    (insert (format "%-24s %d\n" "Existing .elc files"
                    (my/compile--count-existing-elc-files)))
    (insert (format "%-24s %d\n\n" "Managed .eln files"
                    (my/compile--count-existing-config-eln-files)))

    (insert "Native Compile\n")
    (insert "--------------\n")
    (insert (format "%-24s %s\n" "Available" (if native-available "yes" "no")))
    (insert (format "%-24s %s\n" "Package native compile"
                    (if (and (boundp 'package-native-compile)
                             package-native-compile)
                        "enabled"
                      "disabled")))
    (insert (format "%-24s %s\n" "JIT"
                    (if (and (boundp 'native-comp-jit-compilation)
                             native-comp-jit-compilation)
                        "enabled"
                      "disabled")))
    (insert (format "%-24s %s\n" "Deferred"
                    (if (and (boundp 'native-comp-deferred-compilation)
                             native-comp-deferred-compilation)
                        "enabled"
                      "disabled")))
    (insert (format "%-24s %s\n" "Warning policy"
                    (if (boundp 'native-comp-async-report-warnings-errors)
                        (format "%S" native-comp-async-report-warnings-errors)
                      (format "%S" my/native-comp-async-report-policy))))
    (insert (format "%-24s %s\n" "Missing source warning"
                    (if (and (boundp 'native-comp-warning-on-missing-source)
                             native-comp-warning-on-missing-source)
                        "enabled"
                      "disabled")))
    (insert (format "%-24s %s\n" "comp-speed"
                    (if (boundp 'comp-speed)
                        (number-to-string comp-speed)
                      (number-to-string my/native-comp-speed))))
    (insert (format "%-24s %d\n" "Queue size" (my/native-comp--queue-size)))
    (insert (format "%-24s " "ELN cache"))
    (my/compile-board--insert-openable-path my/native-comp-cache-dir)
    (insert "\n")
    (insert (format "%-24s %d\n\n" "Cached .eln files"
                    (my/compile--count-cache-eln-files)))

    (insert "Automation\n")
    (insert "----------\n")
    (insert (format "%-24s %s\n\n" "Auto native on save"
                    (if my/compile-auto-native-on-save "enabled" "disabled")))

    (insert "Actions\n")
    (insert "-------\n")
    (my/compile-board--insert-button
     "[byte config]"
     (lambda (_button) (my/byte-compile-config))
     "Byte-compile the managed config")
    (insert " ")
    (my/compile-board--insert-button
     "[force byte]"
     (lambda (_button) (my/byte-compile-config t))
     "Force byte-compilation for the managed config")
    (insert " ")
    (my/compile-board--insert-button
     "[native config]"
     (lambda (_button) (my/native-compile-config))
     "Queue native compilation for the managed config")
    (insert " ")
    (my/compile-board--insert-button
     "[force native]"
     (lambda (_button) (my/native-compile-config t))
     "Force native compilation for the managed config")
    (insert "\n")
    (my/compile-board--insert-button
     "[byte current]"
     (lambda (_button) (call-interactively #'my/byte-compile-current-file))
     "Byte-compile the current file")
    (insert " ")
    (my/compile-board--insert-button
     "[native current]"
     (lambda (_button) (call-interactively #'my/native-compile-current-file))
     "Queue native compilation for the current file")
    (insert " ")
    (my/compile-board--insert-button
     "[clean .elc]"
     (lambda (_button) (my/compile-clean-byte-artifacts))
     "Delete managed .elc files")
    (insert " ")
    (my/compile-board--insert-button
     "[clean config .eln]"
     (lambda (_button) (my/compile-clean-native-artifacts))
     "Delete managed .eln files")
    (insert " ")
    (my/compile-board--insert-button
     "[reset eln cache]"
     (lambda (_button) (my/native-comp-reset-cache))
     "Delete the dedicated local ELN cache")
    (insert "\n")
    (my/compile-board--insert-button
     "[native log]"
     (lambda (_button) (my/native-comp-open-log))
     "Open the native compilation log")
    (insert " ")
    (my/compile-board--insert-button
     "[warnings]"
     (lambda (_button)
       (if (fboundp 'my/show-warnings-buffer)
           (my/show-warnings-buffer)
         (user-error "Warnings buffer helper is not loaded yet")))
     "Open the warnings buffer")
    (insert " ")
    (my/compile-board--insert-button
     "[toggle auto native]"
     (lambda (_button) (my/compile-toggle-auto-native-on-save))
     "Toggle native compilation on save for this config")
    (insert " ")
    (my/compile-board--insert-button
     "[docs]"
     (lambda (_button) (my/compile-open-docs))
     "Open maintenance documentation")
    (insert "\n\n")

    (insert "Health\n")
    (insert "------\n")
    (insert "Use `s`, `y`, `Y` or the dispatch menu for startup and compile smoke checks.\n")
    (goto-char (point-min))))

(defun my/compile-board ()
  "Open the compile management board."
  (interactive)
  (let ((buffer (get-buffer-create my/compile-board-buffer-name)))
    (with-current-buffer buffer
      (my/compile-board-mode)
      (let ((map (copy-keymap special-mode-map)))
        (use-local-map map)
        (local-set-key (kbd "g") #'my/compile-board-refresh)
        (local-set-key (kbd "b") #'my/byte-compile-config)
        (local-set-key (kbd "B") (lambda () (interactive) (my/byte-compile-config t)))
        (local-set-key (kbd "n") #'my/native-compile-config)
        (local-set-key (kbd "N") (lambda () (interactive) (my/native-compile-config t)))
        (local-set-key (kbd "f") #'my/byte-compile-current-file)
        (local-set-key (kbd "F") #'my/native-compile-current-file)
        (local-set-key (kbd "c") #'my/compile-clean-byte-artifacts)
        (local-set-key (kbd "C") #'my/compile-clean-native-artifacts)
        (local-set-key (kbd "X") #'my/native-comp-reset-cache)
        (local-set-key (kbd "l") #'my/native-comp-open-log)
        (local-set-key (kbd "o") #'my/compile-open-docs)
        (local-set-key (kbd "t") #'my/compile-toggle-auto-native-on-save)
        (local-set-key (kbd "?") #'my/compile-dispatch)
        (local-set-key (kbd "s")
                       (lambda ()
                         (interactive)
                         (if (fboundp 'my/health-startup-check)
                             (my/health-startup-check)
                           (user-error "Health helpers are not loaded yet"))))
        (local-set-key (kbd "y")
                       (lambda ()
                         (interactive)
                         (if (fboundp 'my/health-byte-compile-check)
                             (my/health-byte-compile-check)
                           (user-error "Health helpers are not loaded yet"))))
        (local-set-key (kbd "Y")
                       (lambda ()
                         (interactive)
                         (if (fboundp 'my/health-native-compile-check)
                             (my/health-native-compile-check)
                           (user-error "Health helpers are not loaded yet")))))
      (my/compile-board-refresh))
    (pop-to-buffer buffer)))

(transient-define-prefix my/compile-dispatch ()
  "Compile and cleanup workflow for the local Emacs config."
  [["Board"
    ("b" "open board" my/compile-board)
    ("o" "maintenance docs" my/compile-open-docs)]
   ["Compile"
    ("e" "byte config" my/byte-compile-config)
    ("E" "force byte config" (lambda () (interactive) (my/byte-compile-config t)))
    ("n" "native config" my/native-compile-config)
    ("N" "force native config" (lambda () (interactive) (my/native-compile-config t)))
    ("f" "byte current file" my/byte-compile-current-file)
    ("F" "native current file" my/native-compile-current-file)]
   ["Clean"
    ("c" "clean .elc" my/compile-clean-byte-artifacts)
    ("C" "clean config .eln" my/compile-clean-native-artifacts)
    ("X" "reset eln cache" my/native-comp-reset-cache)
    ("a" "clean all managed artifacts" my/compile-clean-all-artifacts)]
   ["Health"
    ("s" "startup smoke"
     (lambda ()
       (interactive)
       (if (fboundp 'my/health-startup-check)
           (my/health-startup-check)
         (user-error "Health helpers are not loaded yet"))))
    ("y" "byte smoke"
     (lambda ()
       (interactive)
       (if (fboundp 'my/health-byte-compile-check)
           (my/health-byte-compile-check)
         (user-error "Health helpers are not loaded yet"))))
    ("Y" "native smoke"
     (lambda ()
       (interactive)
       (if (fboundp 'my/health-native-compile-check)
           (my/health-native-compile-check)
         (user-error "Health helpers are not loaded yet"))))
    ("l" "native log" my/native-comp-open-log)
    ("t" "toggle auto native on save" my/compile-toggle-auto-native-on-save)]])

(when (fboundp 'my/evil-global-leader-set)
  (my/evil-global-leader-set "cb" #'my/compile-board "compile board")
  (my/evil-global-leader-set "c?" #'my/compile-dispatch "compile menu"))

(provide 'init-compile)
;;; init-compile.el ends here

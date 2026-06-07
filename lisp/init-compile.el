;;; init-compile.el --- Compile helpers for the local Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'bytecomp)
(require 'subr-x)
(require 'transient)

(defvar package-user-dir)

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

(define-derived-mode my/compile-board-mode aaron-ui-board-mode "Compile-Board"
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

(defun my/compile--elpa-recurse-dir-p (dir)
  "Return non-nil when DIR should be recursed into during ELPA compilation.
Skips test suites and example directories that ship with some packages."
  (let ((base (file-name-nondirectory (directory-file-name dir))))
    (not (string-match-p (rx bos (or "test" "tests" "examples" "example") eos) base))))

(defun my/compile--all-el-files ()
  "Return managed and third-party Emacs Lisp source files."
  (delete-dups
   (append
    (my/compile--target-el-files)
    (apply #'append
           (mapcar (lambda (dir)
                     (directory-files-recursively
                      dir "\\.el\\'"
                      nil nil #'my/compile--elpa-recurse-dir-p))
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
  (let ((compiled 0) (skipped 0))
    (dolist (file (my/compile--all-el-files))
      (condition-case err
          (progn (native-compile file)
                 (cl-incf compiled))
        (error
         (cl-incf skipped)
         (message "Native-compile skipped %s: %s"
                  (file-relative-name file user-emacs-directory)
                  (error-message-string err)))))
    (message "Native-compile finished (compiled=%d, skipped=%d, force=%s)"
             compiled skipped (and force t)))
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
  "Insert PATH as a clickable button."
  (aaron-ui-board-insert-openable-path path))

(defun my/compile-board-refresh ()
  "Refresh the compile management board."
  (interactive)
  (let ((inhibit-read-only t)
        (target-files (my/compile--target-el-files))
        (native-available (my/native-comp-available-p)))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Compile Board"
        :icon 'compile
        :stats (list
                (cons (format "%d .el" (length target-files)) nil)
                (cons (format "%d .elc" (my/compile--count-existing-elc-files)) nil)
                (cons (format "%d .eln" (my/compile--count-existing-config-eln-files)) nil))
        :actions '((:label "Dispatch" :command my/compile-dispatch :primary t :help "Transient menu")
                   (:label "Docs"     :command my/compile-open-docs             :help "Maintenance docs")))

       ;; Overview
       (aaron-ui-board-insert-section "Overview")
       (aaron-ui-board-insert-field "Emacs" emacs-version)
       (insert "   " (propertize (format "%-16s" "Config root") 'face 'aaron-ui-board-meta))
       (my/compile-board--insert-openable-path my/compile-config-root)
       (insert "\n")
       (insert "   " (propertize (format "%-16s" "ELN cache") 'face 'aaron-ui-board-meta))
       (my/compile-board--insert-openable-path my/native-comp-cache-dir)
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
          (format "%S" my/native-comp-async-report-policy)))
       (aaron-ui-board-insert-field
        "comp-speed"
        (if (boundp 'comp-speed)
            (number-to-string comp-speed)
          (number-to-string my/native-comp-speed)))
       (aaron-ui-board-insert-field "Queue size" (number-to-string (my/native-comp--queue-size)))
       (aaron-ui-board-insert-field
        "Cached .eln" (number-to-string (my/compile--count-cache-eln-files)))
       (insert "\n")

       ;; Automation
       (aaron-ui-board-insert-section "Automation")
       (aaron-ui-board-insert-field
        "Auto native on save"
        (if my/compile-auto-native-on-save "enabled" "disabled")
        (if my/compile-auto-native-on-save 'aaron-ui-board-good 'aaron-ui-board-meta))
       (insert "\n")

       ;; Actions
       (aaron-ui-board-insert-section "Actions")
       (insert "   ")
       (aaron-ui-board-insert-actions
        '((:label "Byte Config"    :command my/byte-compile-config         :primary t :help "Byte-compile config")
          (:label "Native Config"  :command my/native-compile-config       :help "Native-compile config")
          (:label "Byte Current"   :command my/byte-compile-current-file   :help "Byte-compile current file")
          (:label "Native Current" :command my/native-compile-current-file :help "Native-compile current file")))
       (insert "\n   ")
       (aaron-ui-board-insert-actions
        '((:label "Clean .elc"     :command my/compile-clean-byte-artifacts   :help "Delete .elc files")
          (:label "Clean .eln"     :command my/compile-clean-native-artifacts :help "Delete .eln files")
          (:label "Reset ELN"      :command my/native-comp-reset-cache        :help "Reset ELN cache")
          (:label "Native Log"     :command my/native-comp-open-log           :help "Open native log")
          (:label "Auto Native"    :command my/compile-toggle-auto-native-on-save
                  :help "Toggle auto native on save")))
       (insert "\n\n")

       ;; Health hint
       (aaron-ui-board-insert-section "Health")
       (aaron-ui-board-insert-key-hints
        "Keys: g refresh  b byte  n native  f byte-file  F native-file  c clean .elc  C clean .eln  s smoke  ? dispatch  q quit")))))

(defun my/compile-board ()
  "Open the compile management board."
  (interactive)
  (let ((buffer (get-buffer-create my/compile-board-buffer-name)))
    (with-current-buffer buffer
      (my/compile-board-mode)
      (aaron-ui-board-set-header "Compile Board" 'compile)
      (setq-local aaron-ui-board-refresh-function #'my/compile-board-refresh)
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

(my/leader!
  "cb" '(:def my/compile-board :which-key "compile board")
  "c?" '(:def my/compile-dispatch :which-key "compile menu"))

(provide 'init-compile)
;;; init-compile.el ends here

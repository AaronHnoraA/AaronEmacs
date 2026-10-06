;;; init-doom-extra.el --- Doom-inspired foundation extras -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'init-funcs)

(declare-function editorconfig-find-current-editorconfig "editorconfig")
(declare-function ediff-current-file "ediff" ())
(declare-function link-hint-copy-link "link-hint" ())
(declare-function link-hint-open-link "link-hint" ())
(declare-function undo-tree-save-history "undo-tree" (&optional filename overwrite))
(declare-function material-icon-ibuffer-icons-mode "material-icon-ibuffer" (&optional arg))
(defvar material-icon-ibuffer-formats)

(defvar vertico-map)

(defgroup my/doom-extra nil
  "Doom-inspired UI and editing enhancements."
  :group 'convenience)

(defconst my/dtrt-indent-excluded-modes
  '(pascal-mode
    so-long-mode
    emacs-lisp-mode
    org-mode)
  "Major modes where indentation should not be guessed automatically.")

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info nil)
  (auto-revert-interval 5)
  (auto-revert-remote-files nil)
  (auto-revert-stop-on-user-input t)
  (auto-revert-use-notify t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1)

  (declare-function remote-file-watch-cost "remote-fs"
                    (file-name &optional adapter))
  (declare-function remote-file-operation-cost "remote-fs"
                    (file-name &optional adapter))

  (defun my/auto-revert-follow-pushed-changes-h ()
    "Let auto-revert follow this file when its route pushes change events.
`auto-revert-remote-files' stays nil globally because a polled or
process-per-watch route would stat or spawn per buffer.  When the default
exclusion would skip this buffer but the Remote route delivers notifications
over a stream it already owns (VS Code's watcher model), opt this buffer in:
external edits then revert it without any polling."
    (when (and buffer-file-name
               auto-revert-use-notify
               (fboundp 'remote-file-watch-cost)
               (string-match-p auto-revert-notify-exclude-dir-regexp
                               (expand-file-name default-directory))
               (eq (ignore-errors (remote-file-watch-cost buffer-file-name))
                   'push))
      (setq-local auto-revert-remote-files t)
      (setq-local auto-revert-notify-exclude-dir-regexp
                  (concat mounted-file-systems))))

  ;; Run before `auto-revert--global-adopt-current-buffer' adds the watch.
  (add-hook 'find-file-hook #'my/auto-revert-follow-pushed-changes-h -90)

  (defun my/auto-revert-watch-open-file-h ()
    "Start file notifications as soon as a visited file is adopted.
Without this, Auto-Revert waits until its next timer tick to install
the watch, so an agent's first edit just after opening a file may be
noticed several seconds late.  The normal Auto-Revert handler still
decides whether the buffer is safe to reload."
    (when (and buffer-file-name
               (bound-and-true-p auto-revert--global-mode)
               (not (bound-and-true-p global-auto-revert-ignore-buffer))
               auto-revert-use-notify
               ;; Match Auto-Revert's own remote gate: only push-capable
               ;; routes opted in by the earlier hook may install a watch.
               (or auto-revert-remote-files
                   (not (file-remote-p default-directory)))
               (not auto-revert-notify-watch-descriptor)
               (not (string-match-p auto-revert-notify-exclude-dir-regexp
                                    (expand-file-name default-directory))))
      ;; Register only the watch here.  The file was just read by `find-file',
      ;; so running `auto-revert-buffer' would add an unnecessary stat (and a
      ;; remote round trip) on every visit.
      (auto-revert-notify-add-watch)))

  ;; The built-in adoption hook runs at depth zero on `find-file-hook'.
  (add-hook 'find-file-hook #'my/auto-revert-watch-open-file-h 90)

  (defvar my/auto-revert--warned-modified-files (make-hash-table :test 'equal)
    "Files already reported as changed on disk while modified in Emacs.")

  (defvar my/auto-revert--focus-check-timer nil
    "Idle timer used to debounce stale buffer checks after focus changes.")

  (defvar my/auto-revert--scan-timer nil
    "Timer used to detect modified buffers whose files changed on disk.")

  (defvar my/auto-revert--agent-turn-check-timer nil
    "One-shot check of open files after an ACP agent turn finishes.")

  (defvar-local my/auto-revert--show-check-timer nil
    "One-shot disk check after a round-trip file buffer becomes visible.")

  (defvar-local my/auto-revert-agent-refresh-function nil
    "Optional BUFFER refresh function used after an ACP agent turn.
Modes with their own disk merge rules can set this buffer-locally.  When nil,
ordinary files use the safe auto-revert check.")

  (defvar my/auto-revert-agent-refresh-functions nil
    "Hook for non-buffer surfaces to refresh after an ACP turn.
Each function runs once after the bounded buffer refresh pass.")

  (defvar my/auto-revert-recent-buffer-limit 12
    "Maximum recent file buffers checked after focus changes.")

  (defun my/auto-revert--candidate-buffers ()
    "Return visible and recent file buffers worth checking."
    (let ((seen nil)
          (recent-count 0))
      (dolist (frame (frame-list))
        (when (eq (frame-visible-p frame) t)
          (dolist (window (window-list frame 'no-minibuf))
            (let ((buffer (window-buffer window)))
              (when (and (buffer-live-p buffer)
                         (not (memq buffer seen)))
                (push buffer seen))))))
      (dolist (buffer (buffer-list))
        (when (and (< recent-count my/auto-revert-recent-buffer-limit)
                   (not (memq buffer seen)))
          (with-current-buffer buffer
            (when buffer-file-name
              (setq recent-count (1+ recent-count))
              (push buffer seen)))))
      (nreverse seen)))

  (defun my/auto-revert--refresh-review-state (buffer)
    "Refresh VC and diff indicators for BUFFER after external changes."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when buffer-file-name
          (when (fboundp 'vc-refresh-state)
            (ignore-errors (vc-refresh-state)))
          (when (bound-and-true-p diff-hl-mode)
            (ignore-errors (diff-hl-update)))))))

  (defun my/auto-revert--file-check-allowed-p (allow-round-trip)
    "Whether the current file may be checked at this event boundary.
Multiplexed routes are cheap enough for a focus check.  A route that pays a
shell round trip is checked only after an ACP turn or an explicit command.
An explicitly disconnected target must never be reopened by either path."
    (and (not (bound-and-true-p remote-buffer-disconnected-p))
         (let ((cost (if (fboundp 'remote-file-operation-cost)
                         (or (ignore-errors
                               (remote-file-operation-cost buffer-file-name))
                             'round-trip)
                       'batched)))
           (or (eq cost 'batched)
               (and allow-round-trip (eq cost 'round-trip))))))

  (defun my/auto-revert--modified-stale-file-buffer-p (&optional buffer allow-round-trip)
    "Return non-nil when BUFFER has local edits and its file changed on disk."
    (when (buffer-live-p (or buffer (current-buffer)))
      (with-current-buffer (or buffer (current-buffer))
        (and buffer-file-name
             ;; Buffers that opt out of auto-revert own their disk sync.
             (not (bound-and-true-p global-auto-revert-ignore-buffer))
             (buffer-modified-p)
             (not buffer-read-only)
             (my/auto-revert--file-check-allowed-p allow-round-trip)
             ;; Unchanged files need only one metadata check.  On push-backed
             ;; routes this may be answered from the watch without any I/O.
             (not (verify-visited-file-modtime (current-buffer)))
             (file-exists-p buffer-file-name)))))

  (defun my/auto-revert--unmodified-stale-file-buffer-p (&optional buffer allow-round-trip)
    "Return non-nil when BUFFER can be safely refreshed from disk."
    (when (buffer-live-p (or buffer (current-buffer)))
      (with-current-buffer (or buffer (current-buffer))
        (and buffer-file-name
             (not (bound-and-true-p global-auto-revert-ignore-buffer))
             (not (buffer-modified-p))
             (my/auto-revert--file-check-allowed-p allow-round-trip)
             (not (verify-visited-file-modtime (current-buffer)))
             (file-exists-p buffer-file-name)))))

  (defun my/auto-revert--preserve-undo-state (&optional buffer)
    "Record BUFFER's local edits before an intentional revert."
    (when (my/auto-revert--modified-stale-file-buffer-p buffer t)
      (with-current-buffer (or buffer (current-buffer))
        (undo-boundary)
        (when (and (bound-and-true-p undo-tree-mode)
                   (fboundp 'undo-tree-save-history))
          (ignore-errors (undo-tree-save-history nil t)))
        (undo-boundary))))

  (defun my/auto-revert-after-revert-h ()
    "Refresh audit UI after `auto-revert-mode' reloads a file."
    (when buffer-file-name
      (remhash (expand-file-name buffer-file-name)
               my/auto-revert--warned-modified-files)
      (run-with-idle-timer
       0.05 nil #'my/auto-revert--refresh-review-state (current-buffer))))

  (defun my/auto-revert--warn-modified-stale-buffer (buffer &optional allow-round-trip)
    "Warn once if BUFFER has unsaved edits against a changed file."
    (with-current-buffer buffer
      (when (my/auto-revert--modified-stale-file-buffer-p buffer allow-round-trip)
        (let ((file (expand-file-name buffer-file-name)))
          (unless (gethash file my/auto-revert--warned-modified-files)
            (puthash file t my/auto-revert--warned-modified-files)
            (message
             "File changed on disk while buffer has unsaved edits: %s; M-x my/auto-revert-resolve-current-buffer"
             (abbreviate-file-name file)))))))

  (defun my/auto-revert-warn-modified-stale-buffers-h ()
    "Warn when a modified buffer's file also changed on disk."
    (dolist (buffer (my/auto-revert--candidate-buffers))
      (my/auto-revert--warn-modified-stale-buffer buffer)))

  (defun my/auto-revert--refresh-buffer-if-safe (buffer &optional allow-round-trip)
    "Refresh BUFFER when it is an unmodified stale file buffer."
    (when (my/auto-revert--unmodified-stale-file-buffer-p buffer allow-round-trip)
      (with-current-buffer buffer
        (revert-buffer :ignore-auto :noconfirm :preserve-modes))))

  (defun my/auto-revert-refresh-visible-stale-buffers-h ()
    "Refresh visible and recent unmodified file buffers changed on disk."
    (dolist (buffer (my/auto-revert--candidate-buffers))
      (my/auto-revert--refresh-buffer-if-safe buffer)))

  (defun my/auto-revert-check-stale-buffers-h ()
    "Refresh safe buffers and warn about modified stale buffers."
    (dolist (buffer (my/auto-revert--candidate-buffers))
      (my/auto-revert--refresh-buffer-if-safe buffer)
      (my/auto-revert--warn-modified-stale-buffer buffer)))

  (defun my/auto-revert-agent-refresh-buffer (buffer)
    "Refresh BUFFER after an ACP turn using its registered disk policy."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (bound-and-true-p remote-buffer-disconnected-p)
          (if (functionp my/auto-revert-agent-refresh-function)
              (funcall my/auto-revert-agent-refresh-function buffer)
            (my/auto-revert--refresh-buffer-if-safe buffer t)
            (my/auto-revert--warn-modified-stale-buffer buffer t))))))

  (defun my/auto-revert-agent-turn-complete-h (_buffer _event)
    "Check visible and recent files once after an ACP turn completes."
    (unless (timerp my/auto-revert--agent-turn-check-timer)
      (setq my/auto-revert--agent-turn-check-timer
            (run-at-time
             0 nil
             (lambda ()
               (setq my/auto-revert--agent-turn-check-timer nil)
               (dolist (candidate (my/auto-revert--candidate-buffers))
                 (condition-case err
                     (my/auto-revert-agent-refresh-buffer candidate)
                   (error
                    (display-warning 'autorevert
                                     (format "Agent refresh failed for %s: %s"
                                             (buffer-name candidate)
                                             (error-message-string err))
                                     :warning))))
               (run-hooks 'my/auto-revert-agent-refresh-functions))))))

  (add-hook 'noema-agent-acp-turn-complete-functions
            #'my/auto-revert-agent-turn-complete-h)

  (defun my/auto-revert-check-shown-round-trip-file-h (window)
    "Check a costly file once when WINDOW displays it.
This covers a remote buffer older than the recent-buffer limit without
stat-ing every hidden remote file after each agent turn."
    (when (window-live-p window)
      (let ((buffer (window-buffer window)))
        (with-current-buffer buffer
          (when (and buffer-file-name
                     (not (bound-and-true-p remote-buffer-disconnected-p))
                     (fboundp 'remote-file-operation-cost)
                     (eq (ignore-errors
                           (remote-file-operation-cost buffer-file-name))
                         'round-trip)
                     (not (timerp my/auto-revert--show-check-timer)))
            (setq-local my/auto-revert--show-check-timer
                        (run-at-time
                         0 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq-local my/auto-revert--show-check-timer nil)
                               (when (and (get-buffer-window buffer t)
                                          (not (bound-and-true-p remote-buffer-disconnected-p)))
                                 (condition-case err
                                     (my/auto-revert-agent-refresh-buffer buffer)
                                   (error
                                    (display-warning
                                     'autorevert
                                     (format "Shown file refresh failed for %s: %s"
                                             (buffer-name buffer)
                                             (error-message-string err))
                                     :warning))))))))))))))

  (add-hook 'window-buffer-change-functions
            #'my/auto-revert-check-shown-round-trip-file-h)

  (defun my/auto-revert-schedule-stale-check-h ()
    "Schedule a debounced stale buffer check after focus changes."
    (when (timerp my/auto-revert--focus-check-timer)
      (cancel-timer my/auto-revert--focus-check-timer))
    (setq my/auto-revert--focus-check-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (ignore-errors
               (my/auto-revert-check-stale-buffers-h))))))

  (defun my/auto-revert-resolve-current-buffer ()
    "Resolve a modified buffer whose file changed on disk."
    (interactive)
    (unless (my/auto-revert--modified-stale-file-buffer-p (current-buffer) t)
      (user-error "Current buffer does not have unsaved edits against a changed file"))
    (pcase (completing-read
            "Resolve changed file: "
            '("merge with ediff" "accept disk version" "keep local and overwrite disk")
            nil t)
      ("merge with ediff"
       (my/auto-revert--preserve-undo-state (current-buffer))
       (require 'ediff)
       (ediff-current-file))
      ("accept disk version"
       (when (yes-or-no-p "Discard local edits and reload disk version? ")
         (my/auto-revert--preserve-undo-state (current-buffer))
         (revert-buffer :ignore-auto :noconfirm :preserve-modes)))
      ("keep local and overwrite disk"
       (when (yes-or-no-p "Overwrite disk with current buffer contents? ")
         (set-visited-file-modtime)
         (save-buffer)
         (when buffer-file-name
           (remhash (expand-file-name buffer-file-name)
                    my/auto-revert--warned-modified-files))))))

  (defun my/auto-revert-ask-user-about-supersession-threat-a (orig-fun filename)
    "Use the local stale-buffer resolver instead of Emacs' raw save prompt."
    (if (or noninteractive
            (not (my/auto-revert--modified-stale-file-buffer-p (current-buffer) t)))
        (funcall orig-fun filename)
      (pcase (completing-read
              (format "File changed on disk: %s "
                      (abbreviate-file-name filename))
              '("merge with ediff" "accept disk version" "keep local and overwrite disk")
              nil t)
        ("merge with ediff"
         (my/auto-revert--preserve-undo-state (current-buffer))
         (require 'ediff)
         (ediff-current-file)
         (signal 'file-supersession (list filename)))
        ("accept disk version"
         (when (yes-or-no-p "Discard local edits and reload disk version? ")
           (my/auto-revert--preserve-undo-state (current-buffer))
           (revert-buffer :ignore-auto :noconfirm :preserve-modes))
         (signal 'file-supersession (list filename)))
        ("keep local and overwrite disk"
         (when (yes-or-no-p "Overwrite disk with current buffer contents? ")
           (set-visited-file-modtime)
           (when buffer-file-name
             (remhash (expand-file-name buffer-file-name)
                      my/auto-revert--warned-modified-files)))))))

  (add-hook 'before-revert-hook #'my/auto-revert--preserve-undo-state)
  (add-hook 'after-revert-hook #'my/auto-revert-after-revert-h)
  (advice-remove 'ask-user-about-supersession-threat
                 #'my/auto-revert-ask-user-about-supersession-threat-a)
  (advice-add 'ask-user-about-supersession-threat
              :around #'my/auto-revert-ask-user-about-supersession-threat-a)
  (when (timerp my/auto-revert--scan-timer)
    (cancel-timer my/auto-revert--scan-timer)
    (setq my/auto-revert--scan-timer nil))
  (remove-function after-focus-change-function
                   #'my/auto-revert-schedule-stale-check-h)
  (add-function :after after-focus-change-function
                #'my/auto-revert-schedule-stale-check-h)
  (add-hook 'after-save-hook
            (lambda ()
              (when buffer-file-name
                (remhash (expand-file-name buffer-file-name)
                         my/auto-revert--warned-modified-files)
                (when (and (not (bound-and-true-p global-auto-revert-ignore-buffer))
                           (my/auto-revert--file-check-allowed-p nil))
                  (my/auto-revert-schedule-stale-check-h))))))

(with-eval-after-load 'saveplace
  (define-advice save-place-find-file-hook (:after (&rest _) my/recenter-after-save-place)
    "Recenter after jumping to a remembered place."
    (when buffer-file-name
      (ignore-errors (recenter))))
  (define-advice save-place-to-alist (:around (fn &rest args) my/inhibit-save-place-in-long-files)
    "Skip save-place bookkeeping for very large buffers."
    (unless (bound-and-true-p so-long-minor-mode)
      (apply fn args))))

(use-package editorconfig
  :ensure nil
  :hook (after-init . editorconfig-mode)
  :config
  (when (boundp 'editorconfig-trim-whitespaces-mode)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))
  ;; Built-in/editorconfig variants differ across Emacs releases.
  (when (boundp 'editorconfig-exclude-regexps)
    (add-to-list 'editorconfig-exclude-regexps
                 "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))
  (add-hook 'editorconfig-after-apply-functions
            (lambda (props)
              (when (and (bound-and-true-p dtrt-indent-mode)
                         (or (gethash 'indent_style props)
                             (gethash 'indent_size props)))
                (dtrt-indent-mode -1))
              (when (and (gethash 'indent_size props)
                         (derived-mode-p 'org-mode)
                         (not (fboundp 'org--set-tab-width)))
                (setq tab-width 8))
              (when (bound-and-true-p whitespace-mode)
                (whitespace-mode -1)
                (whitespace-mode 1)))))

(use-package dtrt-indent
  :ensure t
  :defer 2
  :config
  (defun my/dtrt-indent-enable-maybe ()
    "Enable `dtrt-indent-mode' when the current buffer looks suitable."
    (unless (or buffer-read-only
                (bound-and-true-p so-long-minor-mode)
                (member major-mode my/dtrt-indent-excluded-modes)
                (and (boundp 'buffer-file-name) (not buffer-file-name))
                (and (fboundp 'project-current)
                     (project-current nil default-directory)
                     (locate-dominating-file default-directory ".editorconfig")))
      (let ((inhibit-message t))
        (dtrt-indent-mode 1))))

  (add-hook 'change-major-mode-after-body-hook #'my/dtrt-indent-enable-maybe)
  (add-hook 'read-only-mode-hook #'my/dtrt-indent-enable-maybe)
  (setq dtrt-indent-run-after-smie t
        dtrt-indent-max-lines 2000)
  (when (boundp 'dtrt-indent-hook-generic-mapping-list)
    (dolist (var (get 'tab-width 'indent-vars))
      (cl-callf2 rassq-delete-all var dtrt-indent-hook-generic-mapping-list))))

(use-package whitespace
  :ensure nil
  :config
  (defun my/whitespace-highlight-incorrect-indentation-h ()
    "Highlight indentation inconsistent with the current tab policy."
    (unless (or (eq major-mode 'fundamental-mode)
                (not (derived-mode-p 'prog-mode 'conf-mode))
                (bound-and-true-p global-whitespace-mode)
                (null buffer-file-name)
                buffer-read-only
                (my/remote-buffer-p)
                (my/buffer-large-p nil (* 512 1024))
                (bound-and-true-p so-long-minor-mode))
      (setq-local whitespace-style
                  (cl-union (if indent-tabs-mode
                                '(face indentation)
                              '(face tabs tab-mark))
                            '(trailing)))
      (whitespace-mode 1)))

  (add-hook 'after-change-major-mode-hook #'my/whitespace-highlight-incorrect-indentation-h))

(use-package consult-dir
  :ensure t
  :after consult
  :bind (("C-x C-d" . consult-dir)))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-x C-d") #'consult-dir)
  (define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file))

(use-package rainbow-mode
  :ensure t
  :hook ((css-mode
          css-ts-mode
          html-mode
          html-ts-mode
          mhtml-mode
          nxml-mode
          web-mode
          emacs-lisp-mode) . rainbow-mode))

(use-package link-hint
  :ensure t
  :bind (("M-o" . link-hint-open-link))
  :commands (link-hint-open-link
             link-hint-copy-link))

(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

(use-package ibuffer-vc
  :ensure t
  :after ibuffer)

(use-package ibuffer-projectile
  :ensure t
  :after (ibuffer projectile)
  :commands ibuffer-projectile-set-filter-groups)

(defconst my/ibuffer-base-formats
  '((mark modified read-only locked
          " " (name 30 30 :left :elide)
          " " (size 9 -1 :right)
          " " (mode 18 18 :left :elide)
          " " (vc-status 10 10 :left)
          " " my-filename-and-process)
    (mark " " (name 30 -1) " " filename))
  "Aaron's compact Ibuffer columns without graphical icons.")

(defconst my/ibuffer-icon-formats
  '((mark modified read-only locked
          " " (icon 2 2)
          (name 28 28 :left :elide)
          " " (size 9 -1 :right)
          " " (mode 18 18 :left :elide)
          " " (vc-status 10 10 :left)
          " " my-filename-and-process)
    (mark " " (icon 2 2) (name 28 -1) " " filename))
  "Aaron's Ibuffer columns with Material SVG file icons.")

(defun my/ibuffer-material-icons-setup ()
  "Use Material SVG icons without replacing Aaron's custom columns."
  (if (and (display-graphic-p)
           (image-type-available-p 'svg)
           (require 'material-icon-ibuffer nil t))
      (progn
        (setq material-icon-ibuffer-formats my/ibuffer-icon-formats)
        (material-icon-ibuffer-icons-mode 1))
    (setq-local ibuffer-formats my/ibuffer-base-formats)))

(add-hook 'ibuffer-mode-hook #'my/ibuffer-material-icons-setup)

(with-eval-after-load 'ibuffer
  (define-ibuffer-column vc-status
    (:name "VC" :inline t)
    (if-let* ((buffer-file-name (buffer-local-value 'buffer-file-name buffer))
              (backend (vc-backend buffer-file-name)))
        (symbol-name backend)
      ""))

  (define-ibuffer-column my-filename-and-process
    (:name "Filename/Process" :inline t)
    (cond
     ((and (fboundp 'my/ibuffer-noema-file-name)
           (my/ibuffer-noema-file-name buffer))
      (abbreviate-file-name (my/ibuffer-noema-file-name buffer)))
     ((buffer-local-value 'buffer-file-name buffer)
      (abbreviate-file-name (buffer-local-value 'buffer-file-name buffer)))
     ((get-buffer-process buffer)
      (format "(%s)" (process-name (get-buffer-process buffer))))
     (t "")))

  (setq ibuffer-formats my/ibuffer-base-formats)

  (define-key ibuffer-mode-map (kbd "V") #'ibuffer-vc-set-filter-groups-by-vc-root)
  (define-key ibuffer-mode-map (kbd "P") #'ibuffer-projectile-set-filter-groups))

(defun my/sudo-file-path (file)
  "Return a TRAMP sudo path for FILE."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let* ((user (file-remote-p file 'user)))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun my/sudo-find-file (file)
  "Open FILE as root."
  (interactive (list (read-file-name "Open file as root: ")))
  (find-file (my/sudo-file-path (expand-file-name file))))

(defun my/sudo-this-file ()
  "Reopen the current file as root."
  (interactive)
  (my/sudo-find-file
   (or (buffer-file-name (buffer-base-buffer))
       (when (derived-mode-p 'dired-mode 'wdired-mode)
         default-directory)
       (user-error "Current buffer isn't visiting a file"))))

(defun my/sudo-save-buffer ()
  "Write the current buffer as root, then reload the original file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer isn't visiting a file"))
  (let ((source (current-buffer))
        (sudo-file (my/sudo-file-path buffer-file-name)))
    (with-current-buffer (find-file-noselect sudo-file)
      (erase-buffer)
      (insert-buffer-substring source)
      (save-buffer)
      (kill-buffer))
    (revert-buffer t t)
    (message "Saved %s as root" buffer-file-name)))

(my/leader!
  "f d" '(:def consult-dir :which-key "consult dir")
  "f u" '(:def my/sudo-find-file :which-key "sudo find file")
  "f U" '(:def my/sudo-this-file :which-key "sudo this file")
  "f S" '(:def my/sudo-save-buffer :which-key "sudo save")
  "f t" '(:def my/template-switch :which-key "switch template")
  "f T" '(:def my/template-debug :which-key "debug template")
  "o o" '(:def link-hint-open-link :which-key "open link")
  "o O" '(:def link-hint-copy-link :which-key "copy link")
  "p c" '(:def editorconfig-find-current-editorconfig :which-key "editorconfig")
  "q r" '(:def restart-emacs :which-key "restart emacs"))

(provide 'init-doom-extra)
;;; init-doom-extra.el ends here

;;; term/portal/config.el --- Native Ghostty terminal entry points -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Load Portal's terminal feature on demand; web views use the same native core.

;;; Code:

(require 'aaron-packages)
(require 'general)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-terminal)
(require 'remote-workspace)

(declare-function portal-terminal "portal-terminal" (&optional new))
(declare-function portal-terminal-new "portal-terminal" ())
(declare-function portal-terminal-buffers "portal-terminal" (&optional project quick))
(declare-function portal-terminal-focus "portal-terminal" ())
(declare-function portal-terminal-start "portal-terminal"
                  (directory program arguments &optional name))
(declare-function portal--terminal-paste "portal-terminal" (target text &optional enter))
(declare-function portal--terminal-text "portal-terminal" (target))
(declare-function portal--inspect "portal-view" (target))
(declare-function portal-terminal-bind-key "portal-terminal-bindings" (map key command))
(declare-function portal-terminal-refresh-bindings "portal-terminal-bindings" ())
(declare-function portal-input-focus "portal-input" ())
(declare-function portal-input--action "portal-input" (action &optional target serial))
(declare-function portal-view--target-buffer "portal-view" (target))
(defvar portal-input--resume)
(defvar portal-input--origin)
(declare-function +read-ssh-host "term/shell/config" ())
(declare-function aaron-terminal-normalize-directory "aaron-funcs" (directory))
(declare-function remote-client-file-name "remote-fs" (file))
(declare-function remote-canonicalize-emacs-file-name
                  "remote-fs" (file-name &optional directory))
(declare-function +background-make-frame "config/background/config" (&optional parameters))
(declare-function +tab-line-all-terminal-buffers "ui/tabbar/config" ())
(declare-function +portal-popup--live-buffers "term/portal/popup" ())
(declare-function +portal-popup--normal-windows "term/portal/popup" ())
(declare-function +portal-popup/select-buffer "term/portal/popup" (buffer))
(declare-function +portal-popup-move-buffer-to-window
                  "term/portal/popup" (buffer target))
(declare-function +window-dashboard-buffer "ui/windows/config" ())
(declare-function +windows/close-window "ui/windows/config" ())
(declare-function +background/hide "config/background/config" (&optional all))
(declare-function vertico-exit "vertico" ())
(defvar vertico--input)
(defvar vertico-sort-override-function)

(defconst +portal-package-url "https://github.com/xiaoxinghu/portal.git"
  "Portal package repository.")

(defconst +portal-package-revision
  "0fbb934bcf9ba00b16b984e370be8f08c44ec560"
  "Portal revision checked for this configuration.")

(aaron-package-register-vc
 'portal `(:url ,+portal-package-url :rev ,+portal-package-revision))

(defvar-local +portal-target-directory nil
  "Remote directory reached by SSH inside a local Portal terminal.")

(defvar +portal-floating-frame-size '(110 . 32)
  "Columns and lines of a standalone Portal terminal frame.")

(defvar +portal--last-raycast-frame nil
  "Newest floating terminal frame launched by Raycast.")

(defvar +portal-wheel-scroll-lines 5
  "Lines scrolled per wheel event in a Portal terminal.")

(defun +portal--translate-command-key (original character modifiers)
  "Translate macOS Command from Portal's Super event to Emacs Meta."
  (funcall original character
           (if (and (eq system-type 'darwin)
                    (eq mac-command-modifier 'meta)
                    (/= 0 (logand modifiers 8)))
               (logior 4 (logand modifiers (lognot 8)))
             modifiers)))

(defun +portal-windmove-boundary-a (original &rest args)
  "Keep a native Portal view focused when Windmove reaches a boundary."
  (let ((origin (selected-window))
        (portal-buffer (and (bound-and-true-p portal-input-mode)
                            (bound-and-true-p portal-view--target)
                            (current-buffer))))
    (condition-case err
        (apply original args)
      (user-error
       (if (or (bound-and-true-p portal-input--resume)
               (and portal-buffer
                    (eq origin (selected-window))
                    (eq portal-buffer (window-buffer origin))
                    (or (not +portal--explicitly-unfocused)
                        (portal-input--action "focused"))))
           (progn
             (message "%s" (error-message-string err))
             (when (and portal-buffer
                        (not (bound-and-true-p portal-input--resume)))
               (+portal--focus-selected-window (selected-frame) t)))
         (signal (car err) (cdr err)))))))

(with-eval-after-load 'windmove
  (unless (advice-member-p #'+portal-windmove-boundary-a
                           'windmove-do-window-select)
    (advice-add 'windmove-do-window-select :around
                #'+portal-windmove-boundary-a)))

(with-eval-after-load 'portal-input
  (unless (advice-member-p #'+portal--translate-command-key 'portal-input--event)
    (advice-add 'portal-input--event :around
                #'+portal--translate-command-key))
  (unless (advice-member-p #'+portal--record-explicit-unfocus-a
                           'portal-input-unfocus)
    (advice-add 'portal-input-unfocus :around
                #'+portal--record-explicit-unfocus-a)))

(defvar +portal--focusing-selected-window nil
  "Non-nil while synchronizing keyboard focus after a window event.")
(defvar +portal--selected-pane-state nil
  "Last selected frame, window, and buffer seen by focus synchronization.")
(defvar +portal--selected-view-state nil
  "Last selected Portal view, window, and native placement.")
(defvar +portal-focus-gained-hook nil
  "Hook run in a Portal buffer after its native view gains keyboard focus.")
(defvar-local +portal--explicitly-unfocused nil
  "Non-nil after the user explicitly returns a Portal view to Emacs.")

(defun +portal--record-explicit-unfocus-a (original &rest args)
  "Remember an explicit Portal escape before calling ORIGINAL with ARGS."
  (when (bound-and-true-p portal-input-mode)
    (setq-local +portal--explicitly-unfocused t))
  (apply original args))

(defconst +portal-buffer-kept-modes
  '(portal-input-mode +noema-keys-mode)
  "Buffer-local modes used by Portal or Noema page controls.")

(defun +portal--quiet-buffer ()
  "Keep editor plugins from running inside a Portal surface buffer."
  (when (bound-and-true-p portal-input-mode)
    (dolist (mode minor-mode-list)
      (when (and (local-variable-p mode)
                 (boundp mode)
                 (symbol-value mode)
                 (fboundp mode)
                 (not (memq mode +portal-buffer-kept-modes)))
        (funcall mode -1)))
    (when (fboundp 'font-lock-mode) (font-lock-mode -1))
    (setq-local completion-at-point-functions nil
                eldoc-documentation-functions nil)))

(defun +portal--keep-emacs-after-minibuffer-h ()
  "Leave Emacs the keyboard after a handed-off command reads the minibuffer.
A chord typed in a view that opens a prompt, as `M-x' does, starts work in
Emacs, so the view does not take the keyboard back when it finishes."
  (when (and (bound-and-true-p portal-input--resume)
             (bound-and-true-p portal-input--origin))
    (setq portal-input--resume nil)
    (when-let* ((buffer (portal-view--target-buffer portal-input--origin)))
      (with-current-buffer buffer
        (setq-local +portal--explicitly-unfocused t)))))

(add-hook 'minibuffer-setup-hook #'+portal--keep-emacs-after-minibuffer-h)

(defun +portal--focus-selected-window (frame &optional selection-changed)
  "Match native keyboard focus to FRAME's selected Emacs window.
SELECTION-CHANGED also handles a frame activation.  Leave a view's escape
alone until selection, buffer, or native placement actually changes; once
Emacs holds the keyboard explicitly, only moving to another pane or buffer
and back gives it to the view again."
  (when (and (not +portal--focusing-selected-window)
             (eq frame (selected-frame))
             (not (active-minibuffer-window)))
    (let* ((window (frame-selected-window frame))
           (buffer (window-buffer window))
           (pane (list frame window buffer))
           (pane-changed (not (equal pane +portal--selected-pane-state)))
           (previous-target (nth 2 +portal--selected-view-state)))
      (setq +portal--selected-pane-state pane)
      (with-current-buffer buffer
        (let* ((target (and (featurep 'portal-input)
                            (bound-and-true-p portal-input-mode)
                            (bound-and-true-p portal-view--target)))
               (view (and target
                          (list frame window target portal-view--geometry)))
               (view-changed (not (equal view +portal--selected-view-state))))
          (when (and (or selection-changed pane-changed view-changed)
                     ;; Leaving the minibuffer reselects the same pane.
                     (not (and target +portal--explicitly-unfocused
                               (not pane-changed))))
            (setq +portal--selected-view-state view)
            (let ((+portal--focusing-selected-window t))
              (when (and previous-target
                         (not (equal previous-target target))
                         (portal-input--action "focused" previous-target))
                (portal-input--action "unfocus" previous-target)
                (when (null target)
                  (select-frame-set-input-focus frame)))
              ;; Native `focused' only means first responder within its own
              ;; NSWindow.  A different frame can be key while it is true.
              (when (and target
                         (or selection-changed pane-changed
                             (not (portal-input--action "focused" target))))
                (portal-input-focus)
                (when (portal-input--action "focused" target)
                  (setq-local +portal--explicitly-unfocused nil)
                  (run-hooks '+portal-focus-gained-hook))))))))))

(defun +portal--focus-after-selection-change (frame)
  "Synchronize Portal focus after FRAME selects a different pane."
  (+portal--focus-selected-window frame t))

(defun +portal--focus-after-command ()
  "Catch selection and buffer changes made together by one Emacs command."
  (let* ((frame (selected-frame))
         (window (frame-selected-window frame))
         (pane +portal--selected-pane-state))
    (unless (and (eq frame (nth 0 pane))
                 (eq window (nth 1 pane))
                 (eq (window-buffer window) (nth 2 pane)))
      (+portal--focus-selected-window frame))))

(add-hook 'window-selection-change-functions
          #'+portal--focus-after-selection-change 90)
(add-hook 'window-state-change-functions
          #'+portal--focus-selected-window 90)
(add-hook 'post-command-hook #'+portal--focus-after-command 90)
(defun +portal--ensure ()
  "Load only Portal's terminal feature in a supported graphical Emacs."
  (unless (and (eq system-type 'darwin)
               (version<= "31.1" emacs-version)
               (display-graphic-p))
    (user-error "Portal needs graphical macOS Emacs 31.1 or newer"))
  (aaron-package-ensure-vc 'portal +portal-package-url
                        +portal-package-revision)
  (require 'portal-terminal))

(defun +portal-workspace-context (&optional directory)
  "Return the Remote workspace context for DIRECTORY."
  (let* ((directory (remote-canonicalize-emacs-file-name
                     (or (and directory
                              (aaron-terminal-normalize-directory directory))
                         default-directory)))
         (context (remote-context directory)))
    (if (or (remote-context-workspace-id context)
            (not (eq (remote-file-operation-cost directory) 'batched)))
        context
      (let ((default-directory directory))
        (if-let* ((project (ignore-errors (project-current nil directory)))
                  (root (project-root project)))
            (remote-context root)
          context)))))

(defun +portal-workspace-id (&optional directory)
  "Return the workspace ID for DIRECTORY."
  (remote-workspace-context-id (+portal-workspace-context directory)))

(defun +portal--ssh-arguments (directory &optional command)
  "Return SSH arguments to open DIRECTORY and optional COMMAND."
  (let* ((method (file-remote-p directory 'method))
         (host (file-remote-p directory 'host))
         (user (file-remote-p directory 'user))
         (port (file-remote-p directory 'port))
         (path (file-remote-p directory 'localname))
         (destination (if user (format "%s@%s" user host) host))
         (script (concat
                  (when (and path (not (string-empty-p path)))
                    (format "cd %s && " (shell-quote-argument path)))
                  (if command
                      (format "exec %s" command)
                    "exec \"${SHELL:-/bin/sh}\" -l"))))
    (unless (and (member method '("ssh" "scp")) host)
      (user-error "This target has no SSH address for Portal"))
    (append (when port (list "-p" port))
            (list "-t" destination script))))

(cl-defun +portal-create (&key directory name command (display t))
  "Create a Portal terminal in DIRECTORY with NAME and optional COMMAND.
COMMAND is a shell string or a list of program and argument strings.
When DISPLAY is nil, restore the previous window layout after creation.
SSH directories run SSH inside Portal; other remote targets use the routed
terminal frontend."
  (let* ((directory (file-name-as-directory
                     (or (and directory
                              (aaron-terminal-normalize-directory directory))
                         default-directory)))
         (physical (if (fboundp 'remote-client-file-name)
                       (or (remote-client-file-name directory) directory)
                     directory))
         (method (file-remote-p physical 'method))
         (configuration (unless display (current-window-configuration)))
         buffer)
    (unwind-protect
        (setq buffer
              (cond
               ((and method (not (member method '("ssh" "scp"))))
                (remote-terminal-buffer
                 (remote-terminal-open (+portal-workspace-context directory)
                                       :name name :display display)))
               (t
                (+portal--ensure)
                (let ((default-directory (if method
                                             (expand-file-name "~/")
                                           physical)))
                  (cond
                   (method
                    (let ((result
                           (portal-terminal-start
                            default-directory "ssh"
                            (+portal--ssh-arguments
                             physical
                             (cond ((stringp command) command)
                                   ((consp command)
                                    (mapconcat #'shell-quote-argument command " "))))
                            name)))
                      (with-current-buffer result
                        (setq-local +portal-target-directory directory))
                      result))
                   (command
                    (let ((argv (if (stringp command)
                                    (list "/bin/zsh" "-lc" command)
                                  command)))
                      (portal-terminal-start default-directory
                                             (car argv) (cdr argv) name)))
                   (t
                    (let ((result (portal-terminal-new)))
                      (when name
                        (with-current-buffer result (rename-buffer name t)))
                      result)))))))
      (when configuration
        (set-window-configuration configuration)))
    (when (and display (buffer-live-p buffer)
               (not (eq (current-buffer) buffer)))
      (pop-to-buffer buffer))
    buffer))

(defun +portal/terminal ()
  "Open a native Ghostty terminal in the selected Emacs window.
Portal needs graphical macOS Emacs 31.1 or newer.  An SSH workspace uses
the native Portal view with SSH running inside it."
  (interactive)
  (+portal-create :directory default-directory))

(defalias 'terminal #'portal-terminal
  "Open a native Ghostty terminal in the selected Emacs window.")

(defvar +terminal-selector-placement 'current
  "Window placement requested from the terminal minibuffer.")

(defvar +terminal-selector-active nil
  "Non-nil while the terminal choice minibuffer is active.")

(defun +terminal-selector-exit (placement)
  "Accept the highlighted terminal with PLACEMENT."
  (setq +terminal-selector-placement placement)
  (if (bound-and-true-p vertico--input)
      (vertico-exit)
    (exit-minibuffer)))

(defun +terminal-selector-setup ()
  "Add placement keys to this terminal selection minibuffer."
  (let ((map (copy-keymap (current-local-map))))
    (dolist (entry '(("S-RET" . right)
                     ("S-<return>" . right)
                     ("M-RET" . below)
                     ("M-<return>" . below)
                     ("s-RET" . below)
                     ("s-<return>" . below)
                     ("M-S-RET" . frame)
                     ("M-S-<return>" . frame)))
      (let ((placement (cdr entry)))
        (keymap-set map (car entry)
                    (lambda () (interactive)
                      (+terminal-selector-exit placement)))))
    (use-local-map map)))

(defun +terminal-selector-candidates (&optional exclude)
  "Return ordinary and popup terminal choices, omitting EXCLUDE."
  (append
   (mapcar (lambda (buffer)
             (cons (format "终端 · %s" (buffer-name buffer)) buffer))
           (seq-remove (lambda (buffer) (eq buffer exclude))
                       (+tab-line-all-terminal-buffers)))
   (mapcar (lambda (buffer)
             (cons (format "弹窗 · %s" (buffer-name buffer)) buffer))
           (seq-filter
            (lambda (buffer)
              (and (not (eq buffer exclude))
                   (with-current-buffer buffer
                     (or (derived-mode-p 'portal-terminal-mode)
                         (bound-and-true-p remote-terminal-instance)))))
            (+portal-popup--live-buffers)))
   (list (cons "＋ 新建终端" :new))))

(defun +portal-vacate-window (window buffer)
  "Remove Portal BUFFER from WINDOW without displaying another Portal view."
  (when (and (window-live-p window) (eq (window-buffer window) buffer))
    (let ((previous
           (seq-find
            (lambda (candidate)
              (and (buffer-live-p candidate)
                   (not (eq candidate buffer))
                   (with-current-buffer candidate
                     (not (or (derived-mode-p 'portal-terminal-mode)
                              (bound-and-true-p remote-terminal-instance)
                              (bound-and-true-p portal-input-mode)
                              (bound-and-true-p portal-view--target))))
                   (not (get-buffer-window candidate t))))
            (mapcar #'car (window-prev-buffers window)))))
      (set-window-buffer window (or previous (+window-dashboard-buffer))))))

(defun +portal-move-buffer-to-window (buffer target)
  "Show Portal BUFFER only in TARGET, vacating every other window first."
  (unless (window-live-p target)
    (user-error "Target window was closed"))
  (dolist (window (get-buffer-window-list buffer nil t))
    (unless (eq window target)
      (+portal-vacate-window window buffer)))
  (set-window-buffer target buffer)
  (select-window target)
  buffer)

(defvar +portal--enforcing-single-window nil
  "Non-nil while vacating duplicate windows for a Portal view.")

(defun +portal-sync-single-window-a (original buffer)
  "Keep Portal BUFFER in one window before ORIGINAL places its native view."
  (unless +portal--enforcing-single-window
    (let ((+portal--enforcing-single-window t))
      (let* ((windows (get-buffer-window-list buffer nil t))
             (selected (if (eq (selected-window) (active-minibuffer-window))
                           (minibuffer-selected-window)
                         (selected-window)))
             (previous (and (buffer-live-p buffer)
                            (buffer-local-value 'portal-view--window buffer)))
             (target (cond ((memq selected windows) selected)
                           ((memq previous windows) previous)
                           (t (car windows)))))
        (dolist (window windows)
          (unless (eq window target)
            (+portal-vacate-window window buffer)))
        ;; A temporary buffer (for example Ibuffer) can hide the native layer
        ;; while Portal still remembers the same geometry.  Its ordinary sync
        ;; would then skip placement and leave a blank terminal pane.
        (when (and target
                   (frame-visible-p (window-frame target))
                   (fboundp 'portal--inspect))
          (with-current-buffer buffer
            (when (and (bound-and-true-p portal-view--target)
                       portal-view--geometry)
              (let ((state (ignore-errors
                             (portal--inspect portal-view--target))))
                (when (and state
                           (plist-member state :visible)
                           (not (plist-get state :visible)))
                  (setq portal-view--geometry nil)))))))
      (funcall original buffer))))

(defun +portal-close-terminal-buffer-a (original &rest args)
  "Remove an empty terminal BUFFER after ORIGINAL closes its native view."
  (let ((buffer (current-buffer))
        (terminal (derived-mode-p 'portal-terminal-mode)))
    (prog1 (apply original args)
      ;; `portal-view-close' also runs inside `kill-buffer-hook'.  Defer the
      ;; cleanup so that an already running buffer kill can finish first.
      (when terminal
        (run-at-time
         0 nil
         (lambda (candidate)
           (when (and (buffer-live-p candidate)
                      (with-current-buffer candidate
                        (and (derived-mode-p 'portal-terminal-mode)
                             (not (bound-and-true-p portal-view--target)))))
             (kill-buffer candidate)))
         buffer)))))

(with-eval-after-load 'portal-view
  (unless (advice-member-p #'+portal-sync-single-window-a
                           'portal-view--sync-buffer)
    (advice-add 'portal-view--sync-buffer :around
                #'+portal-sync-single-window-a))
  (unless (advice-member-p #'+portal-close-terminal-buffer-a
                           'portal-view-close)
    (advice-add 'portal-view-close :around
                #'+portal-close-terminal-buffer-a)))

(defun +portal-frame-only-shows-buffer-p (frame buffer)
  "Return non-nil if BUFFER occupies every ordinary window of FRAME."
  (and (frame-live-p frame)
       (get-buffer-window buffer frame)
       (cl-every (lambda (window) (eq (window-buffer window) buffer))
                 (window-list frame 'no-minibuf))))

(defun +terminal-selector-move-to-new-frame (buffer)
  "Move terminal BUFFER to a new frame without leaving it visible elsewhere."
  (let* ((owners (seq-filter
                  (lambda (frame)
                    (eq (frame-parameter frame '+portal/terminal) buffer))
                  (frame-list)))
         (close-owners
          (seq-filter (lambda (frame)
                        (+portal-frame-only-shows-buffer-p frame buffer))
                      owners))
         (frame (if (fboundp '+background-make-frame)
                    (+background-make-frame)
                  (make-frame)))
         (target (frame-selected-window frame))
         completed)
    (unwind-protect
        (progn
          (if (buffer-local-value '+portal-popup-instance-p buffer)
              (+portal-popup-move-buffer-to-window buffer target)
            (+portal-move-buffer-to-window buffer target))
          (dolist (owner owners)
            (set-frame-parameter owner '+portal/terminal nil))
          (dolist (owner close-owners)
            (when (frame-live-p owner)
              (delete-frame owner t)))
          (set-frame-parameter frame '+portal/terminal buffer)
          (select-frame frame)
          (select-frame-set-input-focus frame)
          (setq completed t)
          buffer)
      (unless completed
        (when (frame-live-p frame)
          (delete-frame frame t))))))

(defun +portal/terminal-selector ()
  "Switch to a terminal or create one, choosing the window with Return.
Return uses this window, Shift-Return splits right, and Command-Return
splits below.  Command-Shift-Return opens a new frame.  The active terminal
is omitted from the choices."
  (interactive)
  (let* ((source (current-buffer))
         (source-directory default-directory)
         (popup (bound-and-true-p +portal-popup-instance-p))
         (origin (if popup
                     (car (+portal-popup--normal-windows))
                   (selected-window)))
         (candidates (+terminal-selector-candidates
                      (and (or popup (derived-mode-p 'portal-terminal-mode))
                           source)))
         (+terminal-selector-active t)
         (+terminal-selector-placement 'current)
         (vertico-sort-override-function #'identity)
         (choice (minibuffer-with-setup-hook #'+terminal-selector-setup
                   (completing-read
                    "终端 (RET 当前 / S-RET 右侧 / Cmd-RET 下方 / Cmd-S-RET 新窗口): "
                    candidates nil t)))
         (selected (cdr (assoc choice candidates))))
    (unless selected
      (user-error "No terminal selected"))
    (cond
     ((eq +terminal-selector-placement 'frame)
      (if (eq selected :new)
          (+portal-open-external source-directory)
        (+terminal-selector-move-to-new-frame selected)))
     ((and (bufferp selected)
           (buffer-local-value '+portal-popup-instance-p selected))
      (+portal-popup/select-buffer selected))
     (t
      (unless origin
        (user-error "No ordinary window for a terminal"))
      (let ((target (pcase +terminal-selector-placement
                      ('right (split-window origin nil 'right))
                      ('below (split-window origin nil 'below))
                      (_ origin))))
        (select-window target)
        (if (eq selected :new)
            (let ((default-directory source-directory))
              (portal-terminal-new))
          (+portal-move-buffer-to-window selected target)
          (when (derived-mode-p 'portal-terminal-mode)
            (portal-terminal-focus))))))))

(defalias 'terminal-switcher #'+portal/terminal-selector
  "Choose an existing terminal or create a new one.")

(general-define-key :keymaps 'global "M-t" #'+portal/terminal-selector)

(defun +portal-ordinary-terminal-tabs-h ()
  "Keep ordinary Portal sessions in one tab line after configuration reload."
  (when (equal header-line-format '(:eval (+tab-line-format)))
    (kill-local-variable 'header-line-format))
  (unless tab-line-format
    (setq-local tab-line-format '(:eval (tab-line-format)))))

(defun +portal-open-in-directory (name directory)
  "Open or reuse a terminal named NAME in DIRECTORY."
  (let ((buffer (get-buffer name)))
    (if (and buffer
             (equal (or (buffer-local-value '+portal-target-directory buffer)
                        (buffer-local-value 'default-directory buffer))
                    directory))
        (progn (pop-to-buffer buffer) buffer)
      (+portal-create :directory directory :name name))))

(defun +portal/named (name)
  "Create or switch to a Portal terminal named NAME."
  (interactive "sPortal name: ")
  (+portal-open-in-directory (format "*portal:%s*" name)
                               default-directory))

(defun +portal/ssh (host)
  "Open a native Portal terminal over SSH to HOST."
  (interactive (list (+read-ssh-host)))
  (+portal-open-in-directory (format "*portal:ssh:%s*" host)
                               (format "/ssh:%s:~/" host)))

(defun +portal-open-external (&optional path command)
  "Open PATH or COMMAND in a Portal terminal in a new Emacs frame."
  (let* ((frame (when (display-graphic-p)
                  (if (fboundp '+background-make-frame)
                      (+background-make-frame)
                    (make-frame))))
         (path (and path (expand-file-name path)))
         (directory (if path
                        (if (file-directory-p path)
                            path (file-name-directory path))
                      (expand-file-name "~/")))
         (program (or command
                      (and path (not (file-directory-p path))
                           (if (file-executable-p path)
                               (list path)
                             (list "/bin/zsh" path)))))
         buffer)
    (when frame (select-frame frame))
    (setq buffer (+portal-create :directory directory :command program))
    (when frame
      (set-frame-parameter frame '+portal/terminal buffer))
    buffer))

(defun +portal-terminal-kill-h ()
  "Close the owning frame or vacate windows before this terminal exits."
  (save-current-buffer
    (let ((buffer (current-buffer)))
      (unless (bound-and-true-p +portal-popup-instance-p)
        (let* ((windows (get-buffer-window-list buffer nil t))
               (owners (seq-filter
                        (lambda (frame)
                          (eq (frame-parameter frame '+portal/terminal)
                              buffer))
                        (frame-list)))
               (close-owners
                (seq-filter
                 (lambda (frame)
                   (+portal-frame-only-shows-buffer-p frame buffer))
                 owners)))
          ;; A frame may still carry ownership after the terminal was hidden.
          ;; Clear it regardless of whether that frame currently shows BUFFER.
          (dolist (frame owners)
            (set-frame-parameter frame '+portal/terminal nil))
          (dolist (window windows)
            (+portal-vacate-window window buffer))
          (dolist (frame close-owners)
            (when (frame-live-p frame)
              (if (> (length (frame-list)) 1)
                  (delete-frame frame t)
                (when (fboundp '+background/hide)
                  (+background/hide t)))))
          ;; Frame deletion and `kill-buffer' can both change the selected
          ;; pane after their hooks return.  Re-place surviving native views
          ;; after that transaction, so another terminal does not stay blank.
          (when (fboundp 'portal-view-sync)
            (run-at-time 0 nil #'portal-view-sync)))))))

(defun +portal-clear-stale-frame-owner-h (window &rest _)
  "Clear a floating terminal owner when its FRAME no longer displays it."
  (when (window-live-p window)
    (let* ((frame (window-frame window))
           (owner (frame-parameter frame '+portal/terminal)))
      (when (and owner
                 (not (and (buffer-live-p owner)
                           (get-buffer-window owner frame))))
        (set-frame-parameter frame '+portal/terminal nil)))))

(add-hook 'window-buffer-change-functions
          #'+portal-clear-stale-frame-owner-h)

(dolist (frame (frame-list))
  (+portal-clear-stale-frame-owner-h (frame-selected-window frame)))

;; Portal already hides a native view when its frame is deleted.  Removing
;; this older hook also fixes a running Emacs after reloading the config:
;; closing a frame must leave its terminal session in the buffer list.
(remove-hook 'delete-frame-functions #'+portal--delete-frame-terminal)

(defun +portal/open-new ()
  "Open a fresh Portal terminal in a centered floating frame."
  (interactive)
  (let* ((buffer (+portal-open-external))
         (frame (selected-frame)))
    (delete-other-windows)
    (set-frame-size frame (car +portal-floating-frame-size)
                    (cdr +portal-floating-frame-size))
    (pcase-let ((`(,x ,y ,w ,h) (frame-monitor-workarea frame)))
      (set-frame-position frame
                          (+ x (max 0 (/ (- w (frame-outer-width frame)) 2)))
                          (+ y (max 0 (/ (- h (frame-outer-height frame)) 2)))))
    ;; Raycast closes its window after Emacs has returned.  Claim the new
    ;; frame once then, never an older frame from a rapid earlier invocation.
    (setq +portal--last-raycast-frame frame)
    (run-at-time
     0.4 nil
     (lambda (target expected-buffer)
       (when (and (frame-live-p target)
                  (eq target +portal--last-raycast-frame)
                  (eq target (selected-frame))
                  (eq expected-buffer
                      (window-buffer (frame-selected-window target))))
         (select-frame-set-input-focus target)))
     frame buffer)
    buffer))

(defun +portal--terminal-target ()
  "Return the current native terminal target or signal a user error."
  (or (and (derived-mode-p 'portal-terminal-mode)
           (boundp 'portal-view--target)
           portal-view--target)
      (user-error "Select a Portal terminal")))

(defun +portal-send-text (text)
  "Send TEXT to the selected Portal terminal."
  (portal--terminal-paste (+portal--terminal-target) text))

(defun +portal--send-dropped-paths (files)
  "Paste the dropped FILES into the selected Portal terminal."
  (+portal-send-text (+macos-drop-text files)))

(set-file-drop-handler! 'portal-terminal-mode #'+portal--send-dropped-paths)

(defun +portal/vim-write ()
  "Send Vim's write command to the selected Portal terminal."
  (interactive)
  (+portal-send-text "\e:w\r"))

(defun +portal/paste ()
  "Paste the most recent Emacs clipboard entry into Portal."
  (interactive)
  (+portal-send-text (current-kill 0 t)))

(defun +portal/scrollback ()
  "Show the selected terminal's scrollback in a searchable Emacs buffer."
  (interactive)
  (let ((text (portal--terminal-text (+portal--terminal-target)))
        (name (format "*Portal scrollback: %s*" (buffer-name))))
    (with-current-buffer (get-buffer-create name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (special-mode)))
    (pop-to-buffer name)))

(defun +portal-copy-match (regexp label)
  "Copy a REGEXP match from Portal scrollback, prompted by LABEL."
  (let* ((text (portal--terminal-text (+portal--terminal-target)))
         (matches nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string-no-properties 1) matches)))
    (unless matches (user-error "No %s in this terminal" label))
    (let ((chosen (completing-read
                   (format "%s: " label)
                   (delete-dups (nreverse matches)) nil t)))
      (kill-new chosen)
      chosen)))

(defun +portal/hint-path ()
  "Copy a path from Portal output."
  (interactive)
  (+portal-copy-match
   "\\(\\(?:~/\\|\\.\\.?/\\|/\\)[^[:space:]\"'<>;]+\\)" "Path"))

(defun +portal/hint-hash ()
  "Copy a hexadecimal hash from Portal output."
  (interactive)
  (+portal-copy-match "\\b\\([[:xdigit:]]\\{7,40\\}\\)\\b" "Hash"))

(defun +portal/hint-key ()
  "Copy a KEY value from Portal output."
  (interactive)
  (+portal-copy-match "KEY: +\\([^[:space:];]+\\);" "Key"))

(defun +portal-install-keys ()
  "Install Portal terminal shortcuts."
  (require 'portal-terminal-bindings)
  (dolist (entry '(("M-t" . +portal/terminal-selector)
                   ("M-S-f" . +portal/scrollback)
                   ("C-M-f" . toggle-frame-fullscreen)
                   ("M-S-h" . windmove-left)
                   ("M-S-j" . windmove-down)
                   ("M-S-k" . windmove-up)
                   ("M-S-l" . windmove-right)
                   ("M-S-z" . +windows/toggle-delete-other-windows)
                   ("M-S-o" . toggle-one-window)
                   ("M-s" . +portal/vim-write)
                   ("M-v" . +portal/paste)
                   ("C-S-p" . +portal/hint-path)
                   ("C-S-h" . +portal/hint-hash)
                   ("C-S-d" . +portal/hint-key)
                   ("C-c C-e" . +portal-popup/toggle)
                   ("C-c E" . +portal-popup/cycle)
                   ("C-c M-e" . +portal/toggle-fixed)
                   ("M-`" . +portal-popup/toggle)))
    (keymap-set portal-terminal-mode-map (car entry) (cdr entry)))
  ;; Native bindings claim keys before Ghostty or the macOS window menu.
  ;; Command uses Meta in this Emacs, so use the same commands as other buffers.
  (dolist (entry `(("C-x" . ,ctl-x-map)
                   ("s-t" . +portal/terminal-selector)
                   ("C-c e" . +portal-popup/toggle)
                   ("C-c C-e" . +portal-popup/toggle)
                   ("C-c E" . +portal-popup/cycle)
                   ("C-c M-e" . +portal/toggle-fixed)
                   ("s-w" . +windows/close-window)
                   ("s-W" . persp-kill-buffer*)
                   ("s-x" . execute-extended-command)))
    (keymap-set portal-terminal-native-map (car entry) (cdr entry)))
  (portal-terminal-refresh-bindings)
  (dotimes (index 9)
    (let ((number (1+ index)))
      (keymap-set portal-terminal-mode-map
                  (format "M-S-%d" number)
                  (lambda () (interactive)
                    (+portal-popup/select-index number))))))

(with-eval-after-load 'portal-input
  (customize-set-variable 'portal-input-mode-line-tint nil)
  (add-hook 'portal-input-mode-hook #'+portal--quiet-buffer 95))

(with-eval-after-load 'portal-terminal
  (setq portal-terminal-auto-close-on-exit t)
  (add-hook 'portal-terminal-mode-hook
            (lambda () (add-hook 'kill-buffer-hook
                            #'+portal-terminal-kill-h nil t)))
  (dolist (buffer (portal-terminal-buffers))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'+portal-terminal-kill-h nil t)))
  (add-hook 'portal-terminal-mode-hook #'+portal-ordinary-terminal-tabs-h)
  (dolist (buffer (portal-terminal-buffers))
    (when (+tab-line-ordinary-terminal-p buffer)
      (with-current-buffer buffer
        (+portal-ordinary-terminal-tabs-h))))
  (add-hook 'portal-terminal-mode-hook
            (lambda () (setq-local mode-line-process nil)))
  (+portal-install-keys))

(provide 'term/portal/config)
;;; term/portal/config.el ends here

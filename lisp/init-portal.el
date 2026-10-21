;;; init-portal.el --- Native Ghostty terminal entry points -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Load Portal's terminal feature on demand; web views use the same native core.

;;; Code:

(require 'init-package-utils)
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
(declare-function portal-terminal-bind-key "portal-terminal-bindings" (map key command))
(declare-function portal-terminal-refresh-bindings "portal-terminal-bindings" ())
(declare-function portal-input-focus "portal-input" ())
(declare-function portal-input--action "portal-input" (action &optional target serial))
(declare-function my/read-ssh-host "init-shell" ())
(declare-function my/terminal-normalize-directory "init-funcs" (directory))
(declare-function remote-client-file-name "remote-fs" (file))
(declare-function remote-canonicalize-emacs-file-name
                  "remote-fs" (file-name &optional directory))
(declare-function my/background-make-frame "init-background" (&optional parameters))
(declare-function my/tab-line-all-terminal-buffers "init-tabbar" ())
(declare-function my/portal-popup--live-buffers "init-portal-popup" ())
(declare-function my/portal-popup--normal-windows "init-portal-popup" ())
(declare-function my/portal-popup-select-buffer "init-portal-popup" (buffer))
(declare-function vertico-exit "vertico" ())
(defvar vertico--input)
(defvar vertico-sort-override-function)

(defconst my/portal-package-url "https://github.com/xiaoxinghu/portal.git"
  "Portal package repository.")

(defconst my/portal-package-revision
  "0fbb934bcf9ba00b16b984e370be8f08c44ec560"
  "Portal revision checked for this configuration.")

(my/package-register-vc
 'portal `(:url ,my/portal-package-url :rev ,my/portal-package-revision))

(defvar-local my/portal-target-directory nil
  "Remote directory reached by SSH inside a local Portal terminal.")

(defvar my/portal-floating-frame-size '(110 . 32)
  "Columns and lines of a standalone Portal terminal frame.")

(defvar my/portal-wheel-scroll-lines 5
  "Lines scrolled per wheel event in a Portal terminal.")

(defun my/portal--translate-command-key (original character modifiers)
  "Translate macOS Command from Portal's Super event to Emacs Meta."
  (funcall original character
           (if (and (eq system-type 'darwin)
                    (eq mac-command-modifier 'meta)
                    (/= 0 (logand modifiers 8)))
               (logior 4 (logand modifiers (lognot 8)))
             modifiers)))

(defun my/portal-windmove-boundary-a (original &rest args)
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
                    (or (not my/portal--explicitly-unfocused)
                        (portal-input--action "focused"))))
           (progn
             (message "%s" (error-message-string err))
             (when (and portal-buffer
                        (not (bound-and-true-p portal-input--resume)))
               (my/portal--focus-selected-window (selected-frame) t)))
         (signal (car err) (cdr err)))))))

(with-eval-after-load 'windmove
  (unless (advice-member-p #'my/portal-windmove-boundary-a
                           'windmove-do-window-select)
    (advice-add 'windmove-do-window-select :around
                #'my/portal-windmove-boundary-a)))

(with-eval-after-load 'portal-input
  (unless (advice-member-p #'my/portal--translate-command-key 'portal-input--event)
    (advice-add 'portal-input--event :around
                #'my/portal--translate-command-key))
  (unless (advice-member-p #'my/portal--record-explicit-unfocus-a
                           'portal-input-unfocus)
    (advice-add 'portal-input-unfocus :around
                #'my/portal--record-explicit-unfocus-a)))

(defvar my/portal--focusing-selected-window nil
  "Non-nil while synchronizing keyboard focus after a window event.")
(defvar my/portal--selected-pane-state nil
  "Last selected frame, window, and buffer seen by focus synchronization.")
(defvar my/portal--selected-view-state nil
  "Last selected Portal view, window, and native placement.")
(defvar my/portal-focus-gained-hook nil
  "Hook run in a Portal buffer after its native view gains keyboard focus.")
(defvar-local my/portal--explicitly-unfocused nil
  "Non-nil after the user explicitly returns a Portal view to Emacs.")

(defun my/portal--record-explicit-unfocus-a (original &rest args)
  "Remember an explicit Portal escape before calling ORIGINAL with ARGS."
  (when (bound-and-true-p portal-input-mode)
    (setq-local my/portal--explicitly-unfocused t))
  (apply original args))

(defconst my/portal-buffer-kept-modes
  '(portal-input-mode my/noema-keys-mode)
  "Buffer-local modes used by Portal or Noema page controls.")

(defun my/portal--quiet-buffer ()
  "Keep editor plugins from running inside a Portal surface buffer."
  (when (bound-and-true-p portal-input-mode)
    (dolist (mode minor-mode-list)
      (when (and (local-variable-p mode)
                 (boundp mode)
                 (symbol-value mode)
                 (fboundp mode)
                 (not (memq mode my/portal-buffer-kept-modes)))
        (funcall mode -1)))
    (when (fboundp 'font-lock-mode) (font-lock-mode -1))
    (setq-local completion-at-point-functions nil
                eldoc-documentation-functions nil)))

(defun my/portal--focus-selected-window (frame &optional selection-changed)
  "Match native keyboard focus to FRAME's selected Emacs window.
SELECTION-CHANGED also handles a frame activation.  Leave a view's explicit
escape alone until selection, buffer, or native placement actually changes."
  (when (and (not my/portal--focusing-selected-window)
             (eq frame (selected-frame))
             (not (active-minibuffer-window)))
    (let* ((window (frame-selected-window frame))
           (buffer (window-buffer window))
           (pane (list frame window buffer))
           (pane-changed (not (equal pane my/portal--selected-pane-state)))
           (previous-target (nth 2 my/portal--selected-view-state)))
      (setq my/portal--selected-pane-state pane)
      (with-current-buffer buffer
        (let* ((target (and (featurep 'portal-input)
                            (bound-and-true-p portal-input-mode)
                            (bound-and-true-p portal-view--target)))
               (view (and target
                          (list frame window target portal-view--geometry)))
               (view-changed (not (equal view my/portal--selected-view-state))))
          (when (or selection-changed pane-changed view-changed)
            (setq my/portal--selected-view-state view)
            (let ((my/portal--focusing-selected-window t))
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
                  (setq-local my/portal--explicitly-unfocused nil)
                  (run-hooks 'my/portal-focus-gained-hook))))))))))

(defun my/portal--focus-after-selection-change (frame)
  "Synchronize Portal focus after FRAME selects a different pane."
  (my/portal--focus-selected-window frame t))

(defun my/portal--focus-after-command ()
  "Catch selection and buffer changes made together by one Emacs command."
  (let* ((frame (selected-frame))
         (window (frame-selected-window frame))
         (pane my/portal--selected-pane-state))
    (unless (and (eq frame (nth 0 pane))
                 (eq window (nth 1 pane))
                 (eq (window-buffer window) (nth 2 pane)))
      (my/portal--focus-selected-window frame))))

(add-hook 'window-selection-change-functions
          #'my/portal--focus-after-selection-change 90)
(add-hook 'window-state-change-functions
          #'my/portal--focus-selected-window 90)
(add-hook 'post-command-hook #'my/portal--focus-after-command 90)

(defun my/portal--ensure ()
  "Load only Portal's terminal feature in a supported graphical Emacs."
  (unless (and (eq system-type 'darwin)
               (version<= "31.1" emacs-version)
               (display-graphic-p))
    (user-error "Portal needs graphical macOS Emacs 31.1 or newer"))
  (my/package-ensure-vc 'portal my/portal-package-url
                        my/portal-package-revision)
  (require 'portal-terminal))

(defun my/portal-workspace-context (&optional directory)
  "Return the Remote workspace context for DIRECTORY."
  (let* ((directory (remote-canonicalize-emacs-file-name
                     (or (and directory
                              (my/terminal-normalize-directory directory))
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

(defun my/portal-workspace-id (&optional directory)
  "Return the workspace ID for DIRECTORY."
  (remote-workspace-context-id (my/portal-workspace-context directory)))

(defun my/portal--ssh-arguments (directory &optional command)
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

(cl-defun my/portal-create (&key directory name command (display t))
  "Create a Portal terminal in DIRECTORY with NAME and optional COMMAND.
COMMAND is a shell string or a list of program and argument strings.
When DISPLAY is nil, restore the previous window layout after creation.
SSH directories run SSH inside Portal; other remote targets use the routed
terminal frontend."
  (let* ((directory (file-name-as-directory
                     (or (and directory
                              (my/terminal-normalize-directory directory))
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
                 (remote-terminal-open (my/portal-workspace-context directory)
                                       :name name :display display)))
               (t
                (my/portal--ensure)
                (let ((default-directory (if method
                                             (expand-file-name "~/")
                                           physical)))
                  (cond
                   (method
                    (let ((result
                           (portal-terminal-start
                            default-directory "ssh"
                            (my/portal--ssh-arguments
                             physical
                             (cond ((stringp command) command)
                                   ((consp command)
                                    (mapconcat #'shell-quote-argument command " "))))
                            name)))
                      (with-current-buffer result
                        (setq-local my/portal-target-directory directory))
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

(defun my/portal-terminal ()
  "Open a native Ghostty terminal in the selected Emacs window.
Portal needs graphical macOS Emacs 31.1 or newer.  An SSH workspace uses
the native Portal view with SSH running inside it."
  (interactive)
  (my/portal-create :directory default-directory))

(defalias 'terminal #'portal-terminal
  "Open a native Ghostty terminal in the selected Emacs window.")

(defvar my/terminal-selector-placement 'current
  "Window placement requested from the terminal minibuffer.")

(defvar my/terminal-selector-active nil
  "Non-nil while the terminal choice minibuffer is active.")

(defun my/terminal-selector-exit (placement)
  "Accept the highlighted terminal with PLACEMENT."
  (setq my/terminal-selector-placement placement)
  (if (bound-and-true-p vertico--input)
      (vertico-exit)
    (exit-minibuffer)))

(defun my/terminal-selector-setup ()
  "Add placement keys to this terminal selection minibuffer."
  (let ((map (copy-keymap (current-local-map))))
    (dolist (entry '(("S-RET" . right)
                     ("S-<return>" . right)
                     ("M-RET" . below)
                     ("M-<return>" . below)
                     ("s-RET" . below)
                     ("s-<return>" . below)))
      (let ((placement (cdr entry)))
        (keymap-set map (car entry)
                    (lambda () (interactive)
                      (my/terminal-selector-exit placement)))))
    (use-local-map map)))

(defun my/terminal-selector-candidates (&optional exclude)
  "Return ordinary and popup terminal choices, omitting EXCLUDE."
  (append
   (mapcar (lambda (buffer)
             (cons (format "终端 · %s" (buffer-name buffer)) buffer))
           (seq-remove (lambda (buffer) (eq buffer exclude))
                       (my/tab-line-all-terminal-buffers)))
   (mapcar (lambda (buffer)
             (cons (format "弹窗 · %s" (buffer-name buffer)) buffer))
           (seq-filter
            (lambda (buffer)
              (and (not (eq buffer exclude))
                   (with-current-buffer buffer
                     (or (derived-mode-p 'portal-terminal-mode)
                         (bound-and-true-p remote-terminal-instance)))))
            (my/portal-popup--live-buffers)))
   (list (cons "＋ 新建终端" :new))))

(defun my/terminal-selector ()
  "Switch to a terminal or create one, choosing the window with Return.
Return uses this window, Shift-Return splits right, and Command-Return
splits below.  The active terminal is omitted from the choices."
  (interactive)
  (let* ((source (current-buffer))
         (source-directory default-directory)
         (popup (bound-and-true-p my/portal-popup-instance-p))
         (origin (if popup
                     (or (car (my/portal-popup--normal-windows))
                         (user-error "No ordinary window for a terminal"))
                   (selected-window)))
         (candidates (my/terminal-selector-candidates
                      (and (or popup (derived-mode-p 'portal-terminal-mode))
                           source)))
         (my/terminal-selector-active t)
         (my/terminal-selector-placement 'current)
         (vertico-sort-override-function #'identity)
         (choice (minibuffer-with-setup-hook #'my/terminal-selector-setup
                   (completing-read
                    "终端 (RET 当前 / S-RET 右侧 / Cmd-RET 下方): "
                    candidates nil t)))
         (selected (cdr (assoc choice candidates))))
    (unless selected
      (user-error "No terminal selected"))
    (if (and (bufferp selected)
             (buffer-local-value 'my/portal-popup-instance-p selected))
        (my/portal-popup-select-buffer selected)
      (let ((target (pcase my/terminal-selector-placement
                      ('right (split-window origin nil 'right))
                      ('below (split-window origin nil 'below))
                      (_ origin))))
        (select-window target)
        (if (eq selected :new)
            (let ((default-directory source-directory))
              (portal-terminal-new))
          (switch-to-buffer selected)
          (when (derived-mode-p 'portal-terminal-mode)
            (portal-terminal-focus)))))))

(defalias 'terminal-switcher #'my/terminal-selector
  "Choose an existing terminal or create a new one.")

(general-define-key :keymaps 'global "M-t" #'my/terminal-selector)

(defun my/portal-ordinary-terminal-tabs-h ()
  "Keep ordinary Portal sessions in one tab line after configuration reload."
  (when (equal header-line-format '(:eval (my/tab-line-format)))
    (kill-local-variable 'header-line-format))
  (unless tab-line-format
    (setq-local tab-line-format '(:eval (tab-line-format)))))

(defun my/portal-open-in-directory (name directory)
  "Open or reuse a terminal named NAME in DIRECTORY."
  (let ((buffer (get-buffer name)))
    (if (and buffer
             (equal (or (buffer-local-value 'my/portal-target-directory buffer)
                        (buffer-local-value 'default-directory buffer))
                    directory))
        (progn (pop-to-buffer buffer) buffer)
      (my/portal-create :directory directory :name name))))

(defun my/portal-named (name)
  "Create or switch to a Portal terminal named NAME."
  (interactive "sPortal name: ")
  (my/portal-open-in-directory (format "*portal:%s*" name)
                               default-directory))

(defun my/portal-ssh (host)
  "Open a native Portal terminal over SSH to HOST."
  (interactive (list (my/read-ssh-host)))
  (my/portal-open-in-directory (format "*portal:ssh:%s*" host)
                               (format "/ssh:%s:~/" host)))

(defun my/portal-open-external (&optional path command)
  "Open PATH or COMMAND in a Portal terminal in a new Emacs frame."
  (let* ((frame (when (display-graphic-p)
                  (if (fboundp 'my/background-make-frame)
                      (my/background-make-frame '((no-focus-on-map . t)))
                    (make-frame '((no-focus-on-map . t))))))
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
    ;; Select the new frame for Portal's window placement without activating
    ;; its NSWindow while the external launcher still owns the keyboard.
    (when frame (select-frame frame))
    (setq buffer (my/portal-create :directory directory :command program))
    (when frame
      (set-frame-parameter frame 'my/portal-terminal buffer)
      (set-frame-parameter frame 'alpha 87)
      (when (null program)
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (when (and (frame-live-p frame)
                                 (seq-some (lambda (other)
                                             (and (not (eq other frame))
                                                  (frame-live-p other)))
                                           (frame-list))
                                 (eq (frame-parameter frame 'my/portal-terminal)
                                     (current-buffer)))
                        (set-frame-parameter frame 'my/portal-terminal nil)
                        (delete-frame frame t)))
                    nil t)))
      ;; Raycast closes its own window after the action completes.  Activating
      ;; the frame before then lets macOS restore the old terminal on close.
      (run-at-time 0.4 nil
                   (lambda (target)
                     (when (frame-live-p target)
                       (set-frame-parameter target 'no-focus-on-map nil)
                       (select-frame-set-input-focus target)))
                   frame))
    buffer))

(defun my/portal--delete-frame-terminal (frame)
  "End the standalone terminal owned by FRAME when FRAME closes."
  (when-let* ((buffer (frame-parameter frame 'my/portal-terminal))
              ((buffer-live-p buffer)))
    (set-frame-parameter frame 'my/portal-terminal nil)
    (unless (seq-some (lambda (window)
                        (not (eq (window-frame window) frame)))
                      (get-buffer-window-list buffer nil t))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(add-hook 'delete-frame-functions #'my/portal--delete-frame-terminal)

(defun my/portal-open-new ()
  "Open a fresh Portal terminal in a centered floating frame."
  (interactive)
  (let* ((buffer (my/portal-open-external))
         (frame (selected-frame)))
    (delete-other-windows)
    (set-frame-size frame (car my/portal-floating-frame-size)
                    (cdr my/portal-floating-frame-size))
    (pcase-let ((`(,x ,y ,w ,h) (frame-monitor-workarea frame)))
      (set-frame-position frame
                          (+ x (max 0 (/ (- w (frame-outer-width frame)) 2)))
                          (+ y (max 0 (/ (- h (frame-outer-height frame)) 2)))))
    buffer))

(defun my/portal--terminal-target ()
  "Return the current native terminal target or signal a user error."
  (or (and (derived-mode-p 'portal-terminal-mode)
           (boundp 'portal-view--target)
           portal-view--target)
      (user-error "Select a Portal terminal")))

(defun my/portal-send-text (text)
  "Send TEXT to the selected Portal terminal."
  (portal--terminal-paste (my/portal--terminal-target) text))

(defun my/portal-vim-write ()
  "Send Vim's write command to the selected Portal terminal."
  (interactive)
  (my/portal-send-text "\e:w\r"))

(defun my/portal-paste ()
  "Paste the most recent Emacs clipboard entry into Portal."
  (interactive)
  (my/portal-send-text (current-kill 0 t)))

(defun my/portal-scrollback ()
  "Show the selected terminal's scrollback in a searchable Emacs buffer."
  (interactive)
  (let ((text (portal--terminal-text (my/portal--terminal-target)))
        (name (format "*Portal scrollback: %s*" (buffer-name))))
    (with-current-buffer (get-buffer-create name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (special-mode)))
    (pop-to-buffer name)))

(defun my/portal-copy-match (regexp label)
  "Copy a REGEXP match from Portal scrollback, prompted by LABEL."
  (let* ((text (portal--terminal-text (my/portal--terminal-target)))
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

(defun my/portal-hint-path ()
  "Copy a path from Portal output."
  (interactive)
  (my/portal-copy-match
   "\\(\\(?:~/\\|\\.\\.?/\\|/\\)[^[:space:]\"'<>;]+\\)" "Path"))

(defun my/portal-hint-hash ()
  "Copy a hexadecimal hash from Portal output."
  (interactive)
  (my/portal-copy-match "\\b\\([[:xdigit:]]\\{7,40\\}\\)\\b" "Hash"))

(defun my/portal-hint-key ()
  "Copy a KEY value from Portal output."
  (interactive)
  (my/portal-copy-match "KEY: +\\([^[:space:];]+\\);" "Key"))

(defun my/portal-install-keys ()
  "Install Portal terminal shortcuts."
  (require 'portal-terminal-bindings)
  (dolist (entry '(("M-t" . my/terminal-selector)
                   ("M-S-f" . my/portal-scrollback)
                   ("C-M-f" . toggle-frame-fullscreen)
                   ("M-S-h" . windmove-left)
                   ("M-S-j" . windmove-down)
                   ("M-S-k" . windmove-up)
                   ("M-S-l" . windmove-right)
                   ("M-S-z" . my/toggle-delete-other-windows)
                   ("M-S-o" . toggle-one-window)
                   ("M-s" . my/portal-vim-write)
                   ("M-v" . my/portal-paste)
                   ("C-S-p" . my/portal-hint-path)
                   ("C-S-h" . my/portal-hint-hash)
                   ("C-S-d" . my/portal-hint-key)
                   ("C-c C-e" . my/portal-popup-toggle)
                   ("C-c E" . my/portal-popup-cycle)
                   ("C-c M-e" . my/portal-toggle-fixed)
                   ("M-`" . my/portal-popup-toggle)))
    (keymap-set portal-terminal-mode-map (car entry) (cdr entry)))
  ;; Native bindings claim keys before Ghostty or the macOS window menu.
  ;; Command uses Meta in this Emacs, so use the same commands as other buffers.
  (dolist (entry `(("C-x" . ,ctl-x-map)
                   ("s-t" . my/terminal-selector)
                   ("C-c e" . my/portal-popup-toggle)
                   ("C-c C-e" . my/portal-popup-toggle)
                   ("C-c E" . my/portal-popup-cycle)
                   ("C-c M-e" . my/portal-toggle-fixed)
                   ("s-w" . delete-window)
                   ("s-W" . persp-kill-buffer*)
                   ("s-x" . execute-extended-command)))
    (keymap-set portal-terminal-native-map (car entry) (cdr entry)))
  (portal-terminal-refresh-bindings)
  (dotimes (index 9)
    (let ((number (1+ index)))
      (keymap-set portal-terminal-mode-map
                  (format "M-S-%d" number)
                  (lambda () (interactive)
                    (my/portal-popup-select-index number))))))

(with-eval-after-load 'portal-input
  (customize-set-variable 'portal-input-mode-line-tint nil)
  (add-hook 'portal-input-mode-hook #'my/portal--quiet-buffer 95))

(with-eval-after-load 'portal-terminal
  (setq portal-terminal-auto-close-on-exit t)
  (add-hook 'portal-terminal-mode-hook #'my/portal-ordinary-terminal-tabs-h)
  (dolist (buffer (portal-terminal-buffers))
    (when (my/tab-line-ordinary-terminal-p buffer)
      (with-current-buffer buffer
        (my/portal-ordinary-terminal-tabs-h))))
  (add-hook 'portal-terminal-mode-hook
            (lambda () (setq-local mode-line-process nil)))
  (my/portal-install-keys))

(provide 'init-portal)
;;; init-portal.el ends here

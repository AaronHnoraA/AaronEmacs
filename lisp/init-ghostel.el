;;; init-ghostel.el --- Routed Ghostel terminals -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-terminal)
(require 'remote-workspace)

(declare-function my/terminal-cd-command "init-funcs" (directory))
(declare-function my/terminal-normalize-directory "init-funcs" (directory))
(declare-function my/ghostel-create-hidden "init-utils" (name))
(declare-function my/ghostel-wrap "init-utils" (command &rest args))
(declare-function my/ghostel-popup-display-buffer "init-ghostel-popup" (buffer))
(declare-function my/ghostel-popup-select-index "init-ghostel-popup" (index))
(declare-function ghostel-create "ghostel" (&optional name display identity))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-readonly-exit "ghostel" ())
(declare-function ghostel-readonly-copy "ghostel" ())
(declare-function ghostel-yank "ghostel" (&optional arg))
(declare-function ghostel-sync-theme "ghostel" ())

(declare-function my/global-popup--workarea "init-global-popup" ())
(defvar ghostel-shell)
(defvar ghostel-tramp-shells)
(defvar ghostel-semi-char-mode-map)
(defvar ghostel-mode-map)
(defvar ghostel-char-mode-map)
(defvar ghostel-readonly-mode-map)
(defvar ghostel--input-mode)
(defvar remote-terminal-instance)
(defvar remote-environment-inhibit)

(defvar my/ghostel-startup-send-delay 0.05
  "Delay between attempts to send a command to a new Ghostel shell.")

(defvar my/ghostel-startup-send-retries 20
  "Maximum attempts to send a command to a new Ghostel shell.")

(defvar my/ghostel-wheel-scroll-lines 5
  "Number of lines scrolled by a Ghostel mouse wheel event.")

(defconst my/ghostel-kitty-palette
  '((ghostel-color-black . "#45475A")
    (ghostel-color-red . "#F38BA8")
    (ghostel-color-green . "#A6E3A1")
    (ghostel-color-yellow . "#F9E2AF")
    (ghostel-color-blue . "#89B4FA")
    (ghostel-color-magenta . "#F5C2E7")
    (ghostel-color-cyan . "#94E2D5")
    (ghostel-color-white . "#BAC2DE")
    (ghostel-color-bright-black . "#585B70")
    (ghostel-color-bright-red . "#F38BA8")
    (ghostel-color-bright-green . "#A6E3A1")
    (ghostel-color-bright-yellow . "#F9E2AF")
    (ghostel-color-bright-blue . "#89B4FA")
    (ghostel-color-bright-magenta . "#F5C2E7")
    (ghostel-color-bright-cyan . "#94E2D5")
    (ghostel-color-bright-white . "#A6ADC8"))
  "The ANSI colors from the previous Kitty Catppuccin Mocha theme.")

(defun my/ghostel-apply-kitty-appearance ()
  "Apply the former Kitty terminal font and ANSI colors to Ghostel."
  (let ((font (if (and (display-graphic-p)
                       (member "FiraCode Nerd Font Mono" (font-family-list)))
                  "FiraCode Nerd Font Mono"
                "Fira Code")))
    (set-face-attribute 'ghostel-default nil
                        :family font :height 200
                        :foreground "#CDD6F4" :background "#1E1E2E")
    (dolist (entry my/ghostel-kitty-palette)
      (set-face-attribute (car entry) nil :foreground (cdr entry)))
    (set-face-attribute 'ghostel-fake-cursor-box nil
                        :foreground "#1E1E2E" :background "#F5E0DC")
    (ghostel-sync-theme)))

(defun my/ghostel-send-command (buffer command &optional retries)
  "Send COMMAND and a newline to Ghostel BUFFER once its process is live."
  (when (and (buffer-live-p buffer)
             (stringp command)
             (not (string-empty-p command)))
    (let ((remaining (or retries my/ghostel-startup-send-retries)))
      (if-let* ((process (get-buffer-process buffer))
                ((process-live-p process)))
          (with-current-buffer buffer
            (ghostel-send-string (concat command "\n")))
        (when (> remaining 0)
          (run-at-time my/ghostel-startup-send-delay nil
                       #'my/ghostel-send-command buffer command
                       (1- remaining)))))))

(defun my/ghostel-workspace-context (&optional directory)
  "Return the remote workspace context for DIRECTORY."
  (let* ((directory (remote-canonicalize-file-name
                     (or (my/terminal-normalize-directory directory)
                         default-directory)))
         (context (remote-context directory)))
    (if (or (remote-context-workspace-id context)
            (not (equal (remote-context-target-id context) "local")))
        context
      (let ((default-directory directory))
        (if-let* ((project (ignore-errors
                            (project-current nil directory)))
                  (root (project-root project)))
            (remote-context (remote-canonicalize-file-name root directory))
          context)))))

(defun my/ghostel-workspace-id (&optional directory)
  "Return the stable remote workspace ID for DIRECTORY."
  (remote-workspace-context-id
   (my/ghostel-workspace-context directory)))

(defun my/ghostel--routed-launch-a (orig-fn &rest args)
  "Create a Ghostel terminal through the routed workspace around ORIG-FN."
  (let* ((target-directory
          (remote-canonicalize-file-name
           (or (my/terminal-normalize-directory default-directory)
               default-directory)))
         (context (my/ghostel-workspace-context target-directory))
         (workspace (remote-workspace-open
                     context :connect nil
                     :load-environment
                     (equal (remote-context-target-id context) "local")))
         (workspace-id (remote-workspace-context-id context))
         (shell-argv (remote-terminal-command
                      workspace "default"
                      (not (equal (remote-context-target-id context) "local"))))
         (physical-directory (or (remote-client-file-name target-directory)
                                 target-directory))
         (method (file-remote-p physical-directory 'method))
         (ghostel-shell shell-argv)
         (ghostel-tramp-shells
          (if method
              (cons (append (list method (car shell-argv) nil)
                            (cdr shell-argv))
                    ghostel-tramp-shells)
            ghostel-tramp-shells))
         (default-directory physical-directory)
         (remote-environment-inhibit
          (and (not (equal (remote-context-target-id context) "local"))
               (not (remote-workspace-environment workspace))))
         (buffer (apply orig-fn args))
         (process (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (unless (and (processp process) (process-live-p process))
      (error "Ghostel did not create a live terminal process"))
    (when-let* ((existing (buffer-local-value 'remote-terminal-instance buffer))
                ((not (equal (remote-terminal-workspace-id existing)
                             workspace-id))))
      (error "Ghostel %s belongs to %s, not %s"
             (buffer-name buffer)
             (remote-terminal-workspace-id existing)
             workspace-id))
    (remote-terminal-adopt
     workspace buffer :process process :name (buffer-name buffer)
     :profile "default"
     :metadata (list :frontend 'ghostel
                     :directory target-directory
                     :restart-function #'my/ghostel--restart-terminal))
    (when-let* ((environment (remote-workspace-environment workspace)))
      (remote-environment-apply environment buffer))
    (with-current-buffer buffer
      (setq-local default-directory target-directory))
    buffer))

(defun my/ghostel--restart-terminal (terminal _workspace)
  "Recreate Ghostel TERMINAL after an explicit remote restart."
  (let* ((metadata (remote-terminal-metadata terminal))
         (default-directory (plist-get metadata :directory))
         (buffer (my/ghostel-create-hidden (remote-terminal-name terminal))))
    (when (and (plist-get metadata :popup)
               (fboundp 'my/ghostel-popup-display-buffer))
      (my/ghostel-popup-display-buffer buffer))
    buffer))

(defun my/ghostel--send-cd (buffer directory)
  "Send a directory change to Ghostel BUFFER."
  (when-let* ((directory (my/terminal-normalize-directory directory))
              (command (my/terminal-cd-command directory)))
    (with-current-buffer buffer
      (setq-local default-directory directory))
    (my/ghostel-send-command buffer command)))

(defun my/ghostel--open-in-directory (buffer-name directory)
  "Open or switch to BUFFER-NAME in DIRECTORY."
  (if-let* ((buffer (get-buffer buffer-name)))
      (progn
        (my/ghostel--send-cd buffer directory)
        buffer)
    (let ((default-directory directory))
      (my/ghostel-create-hidden buffer-name))))

(defun my/ghostel-named (name)
  "Create or switch to a named Ghostel buffer."
  (interactive "sGhostel name: ")
  (let ((name (format "*ghostel:%s*" name)))
    (pop-to-buffer
     (or (get-buffer name)
         (my/ghostel-create-hidden name)))))

(defun my/ghostel-open-external (&optional path command)
  "Open a Ghostel terminal for PATH or COMMAND from macOS.
Directories become the terminal's working directory.  Files and commands run
as programs in a terminal that keeps their output visible after exit."
  (when (display-graphic-p)
    (select-frame-set-input-focus (make-frame)))
  (let* ((path (and path (expand-file-name path)))
         (directory (if path
                        (if (file-directory-p path) path (file-name-directory path))
                      (expand-file-name "~")))
         (default-directory (file-name-as-directory directory))
         (buffer
          (if command
              (my/ghostel-wrap command :directory directory)
            (if (and path (not (file-directory-p path)))
              (my/ghostel-wrap
               (if (file-executable-p path)
                   (list path)
                 (list "/bin/zsh" path))
               :directory directory
               :buffer-name (format "*ghostel:%s*" (file-name-nondirectory path)))
              (let ((buffer (my/ghostel-create-hidden
                             (generate-new-buffer-name "*ghostel:macOS*"))))
                (pop-to-buffer buffer)
                buffer)))))
    (with-current-buffer buffer
      (setq-local ghostel-kill-buffer-on-exit nil))
    (when (display-graphic-p)
      (set-frame-parameter nil 'alpha 87)
      (select-frame-set-input-focus (selected-frame)))
    buffer))

(defvar my/ghostel-floating-frame-size '(110 . 32)
  "Columns and lines of the frame opened by `my/ghostel-open-new'.")

(defun my/ghostel-open-new ()
  "Open a new Ghostel terminal alone in a floating frame.
Every call is a new instance, centred on the monitor under the pointer.
Exiting the shell kills the buffer and closes that frame."
  (interactive)
  (let* ((buffer (my/ghostel-open-external))
         (frame (selected-frame))
         (window (get-buffer-window buffer frame)))
    (when window
      (delete-other-windows window))
    ;; The frame exists for this terminal, so both go when the shell exits.
    (with-current-buffer buffer
      (setq-local ghostel-kill-buffer-on-exit t)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (and (frame-live-p frame)
                             (one-window-p t frame))
                    (delete-frame frame t)))
                nil t))
    (set-frame-size frame
                    (car my/ghostel-floating-frame-size)
                    (cdr my/ghostel-floating-frame-size))
    (pcase-let ((`(,x ,y ,w ,h) (if (fboundp 'my/global-popup--workarea)
                                    (my/global-popup--workarea)
                                  (frame-monitor-workarea frame))))
      (set-frame-position frame
                          (+ x (max 0 (/ (- w (frame-outer-width frame)) 2)))
                          (+ y (max 0 (/ (- h (frame-outer-height frame)) 2)))))
    buffer))

(defun my/ghostel--ssh-target-directory (host)
  "Return the preferred TRAMP directory for SSH HOST."
  (let* ((directory (my/terminal-normalize-directory default-directory))
         (method (and directory (file-remote-p directory 'method)))
         (remote-host (and directory (file-remote-p directory 'host))))
    (if (and method remote-host (string= host remote-host)
             (or (string-prefix-p "ssh" method)
                 (string= method "scp")))
        directory
      (format "/ssh:%s:~/" host))))

(defun my/ghostel-ssh (host)
  "Open a Ghostel terminal on SSH HOST."
  (interactive (list (my/read-ssh-host)))
  (let ((name (format "*ghostel:ssh:%s*" host))
        (directory (my/ghostel--ssh-target-directory host)))
    (pop-to-buffer
     (my/ghostel--open-in-directory name directory))))

(defun my/ghostel-copy-dwim ()
  "Copy the active region or enter Ghostel copy mode."
  (interactive)
  (if (region-active-p)
      (progn (clipboard-kill-ring-save (region-beginning) (region-end))
             (deactivate-mark)
             (when (eq ghostel--input-mode 'copy)
               (ghostel-readonly-exit)))
    (ghostel-copy-mode)))

(defun my/ghostel-vim-write ()
  "Send the former Kitty Command-S write sequence to a terminal TUI."
  (interactive)
  (ghostel-send-string "\e:w\r"))

(defun my/ghostel-copy-match (regexp label)
  "Copy a REGEXP match in the terminal scrollback, prompted by LABEL."
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string-no-properties 1) matches)))
    (unless matches
      (user-error "No %s in this terminal" label))
    (let ((chosen (completing-read
                   (format "%s: " label)
                   (delete-dups (nreverse matches)) nil t)))
      (kill-new chosen)
      (message "Copied %s" chosen)
      chosen)))

(defun my/ghostel-hint-path ()
  "Copy a path from Ghostel output, like Kitty's path hints."
  (interactive)
  (my/ghostel-copy-match
   "\\(\\(?:~/\\|\\.\\.?/\\|/\\)[^[:space:]\"'<>;]+\\)" "Path"))

(defun my/ghostel-hint-hash ()
  "Copy a hex hash from Ghostel output, like Kitty's hash hints."
  (interactive)
  (my/ghostel-copy-match "\\b\\([[:xdigit:]]\\{7,40\\}\\)\\b" "Hash"))

(defun my/ghostel-hint-key ()
  "Copy a KEY value from Ghostel output, like the former Kitty helper."
  (interactive)
  (my/ghostel-copy-match "KEY: +\\([^[:space:];]+\\);" "Key"))

(defun my/ghostel-wheel-scroll-up (event)
  "Scroll to older Ghostel output from mouse EVENT."
  (interactive "e")
  (when-let* ((window (posn-window (event-start event)))
              ((window-live-p window)))
    (with-selected-window window
      (scroll-down-command my/ghostel-wheel-scroll-lines))))

(defun my/ghostel-wheel-scroll-down (event)
  "Scroll to newer Ghostel output from mouse EVENT."
  (interactive "e")
  (when-let* ((window (posn-window (event-start event)))
              ((window-live-p window)))
    (with-selected-window window
      (scroll-up-command my/ghostel-wheel-scroll-lines))))

(defun my/ghostel-install-keys ()
  "Install former terminal shortcuts on Ghostel's input maps."
  ;; Char mode forwards C-c directly; reserve a prefix for popup controls
  ;; and retain C-c C-c as the explicit terminal interrupt.
  (keymap-set ghostel-char-mode-map "C-c" (make-sparse-keymap))
  (keymap-set ghostel-char-mode-map "C-c C-c" #'ghostel-send-C-c)
  (dolist (map (list ghostel-mode-map ghostel-semi-char-mode-map
                     ghostel-char-mode-map))
    (keymap-set map "C-c C-e" #'ghostel-toggle)
    (keymap-set map "C-c E" #'my/ghostel-popup-cycle)
    (keymap-set map "C-c M-e" #'my/ghostel-toggle-fixed)
    (keymap-set map "M-`" #'ghostel-toggle)
    (keymap-set map "M-t" #'my/ghostel-popup-new)
    (keymap-set map "M-S-f" #'ghostel-copy-mode)
    (keymap-set map "C-M-f" #'toggle-frame-fullscreen)
    (keymap-set map "M-S-h" #'windmove-left)
    (keymap-set map "M-S-j" #'windmove-down)
    (keymap-set map "M-S-k" #'windmove-up)
    (keymap-set map "M-S-l" #'windmove-right)
    (keymap-set map "M-S-z" #'my/toggle-delete-other-windows)
    (keymap-set map "M-S-o" #'toggle-one-window)
    (keymap-set map "M-s" #'my/ghostel-vim-write)
    (keymap-set map "C-S-p" #'my/ghostel-hint-path)
    (keymap-set map "C-S-h" #'my/ghostel-hint-hash)
    (keymap-set map "C-S-d" #'my/ghostel-hint-key)
    (cl-loop for index from 1 to 9
             for shifted across "!@#$%^&*("
             do (let* ((tab-index index)
                       (command
                       (lambda () (interactive)
                         (my/ghostel-popup-select-index tab-index))))
                  (keymap-set map (format "M-S-%d" index) command)
                  (keymap-set map (format "M-%c" shifted) command))))
  (keymap-set ghostel-semi-char-mode-map "M-c" #'my/ghostel-copy-dwim)
  (keymap-set ghostel-semi-char-mode-map "M-y" #'ghostel-copy-mode)
  (keymap-set ghostel-semi-char-mode-map "M-v" #'ghostel-yank)
  (keymap-set ghostel-readonly-mode-map "M-c" #'my/ghostel-copy-dwim)
  (keymap-set ghostel-readonly-mode-map "M-v" #'ghostel-yank)
  (keymap-set ghostel-readonly-mode-map "<wheel-up>" #'my/ghostel-wheel-scroll-up)
  (keymap-set ghostel-readonly-mode-map "<wheel-down>" #'my/ghostel-wheel-scroll-down)
  ;; Ghostel already forwards wheel events to TUI programs when they ask for
  ;; mouse tracking.  Preserve Emacs scrolling through its copy mode instead.
  (keymap-set ghostel-readonly-mode-map "M-y" #'ghostel-copy-mode))

(use-package ghostel
  :ensure t
  :commands (ghostel ghostel-project ghostel-create)
  :init
  (setq ghostel-module-directory
        (expand-file-name "var/ghostel-module/" user-emacs-directory)
        ghostel-module-auto-install 'download)
  :hook (ghostel-mode . shell-mode-common-init)
  :config
  (advice-remove 'ghostel-create #'my/ghostel--routed-launch-a)
  (advice-add 'ghostel-create :around #'my/ghostel--routed-launch-a)
  (my/ghostel-apply-kitty-appearance)
  (my/ghostel-install-keys))

(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(provide 'init-ghostel)
;;; init-ghostel.el ends here

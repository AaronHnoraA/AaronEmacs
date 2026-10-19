;;; init-ghostel-popup.el --- Popup ghostel pool -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-ui)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'easymenu)

(declare-function my/terminal-normalize-directory "init-funcs" (directory))
(declare-function my/ghostel-send-command "init-ghostel" (buffer command &optional retries))
(declare-function my/ghostel-create-hidden "init-utils" (name))
(declare-function my/ghostel-require "init-utils" ())
(declare-function my/ghostel-workspace-id "init-ghostel" (&optional directory))
(declare-function remote-terminal-p "remote-terminal" (object))
(declare-function remote-terminal-put-metadata "remote-terminal"
                  (terminal property value))
(declare-function remote-terminal-workspace-id "remote-terminal" (terminal))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function project-root "project" (project))
(declare-function my/project-current-root "init-project")
(declare-function noema-agent-acp-config-for "noema-agent-acp" (agent))
(declare-function noema-agent-acp-start "noema-agent-acp" (&rest args))
(declare-function noema-agent-acp-adopt "noema-agent-acp" (buffer &rest args))
(declare-function noema-agent-acp-tabs-mode "noema-agent-acp" (&optional arg))
(defvar noema-agent-acp-display-buffer-function)

(defvar ghostel-kill-buffer-on-exit)
(defvar remote-terminal-instance)

(defgroup my/ghostel-popup nil
  "Popup ghostel pool helpers."
  :group 'term
  :prefix "my/ghostel-popup-")

(config-defvar my/ghostel-popup-window-height nil
  "Height of the popup ghostel side window."
  :type 'number
  :group 'my/ghostel-popup)

(config-defvar my/project-popup-ghostel-apps nil
  "Preset terminal apps launched in a fresh popup ghostel at project root."
  :type '(alist :key-type string :value-type string)
  :group 'my/ghostel-popup)

(defvar my/ghostel-popup-buffers nil
  "Popup ghostel buffers in creation order.")

(defvar my/ghostel-popup-current-buffer nil
  "Current popup ghostel buffer.")

(defvar my/ghostel-popup-origin-buffer nil
  "Last focused work buffer used to place new popup terminals and agents.")

(defvar my/ghostel-popup-last-height nil
  "Last popup ghostel height ratio recorded from a visible window.")

(defvar my/ghostel-popup--displaying nil
  "Dynamically non-nil while a popup window is being shown and selected.")

(defvar-local my/ghostel-popup-fixed nil
  "Whether this popup ghostel buffer should stay visible after focus changes.")

(defvar-local my/ghostel-popup-instance-p nil
  "Whether the current buffer belongs to the popup ghostel pool.")

(defvar-local my/ghostel-popup-kind 'terminal
  "Semantic kind for the current popup buffer.")

(defvar-local my/ghostel-popup-title nil
  "Optional title shown in the popup header for the current buffer.")

(defvar-local my/ghostel-popup-workspace-id nil
  "Stable remote workspace ID which owns the current popup buffer.")

(defvar-local my/ghostel-popup--saved-ui nil
  "Window-line values to restore when this buffer leaves the popup pool.")

(defface my/ghostel-popup-tab-current
  `((t (:inherit mode-line-buffer-id
        :foreground ,(aaron-ui-color 'fg-strong)
        :background ,(aaron-ui-color 'bg-elevated)
        :box (:line-width (1 . -1)
              :color ,(aaron-ui-color 'border-popup-separator)))))
  "Face used for the active popup ghostel tab."
  :group 'my/ghostel-popup)

(defface my/ghostel-popup-tab
  `((t (:inherit mode-line
        :foreground ,(aaron-ui-color 'fg-muted)
        :background ,(aaron-ui-color 'bg-base))))
  "Face used for inactive popup ghostel tabs."
  :group 'my/ghostel-popup)

(defface my/ghostel-popup-separator
  `((t (:inherit mode-line
        :foreground ,(aaron-ui-color 'bg-popup-separator)
        :background ,(aaron-ui-color 'bg-popup-separator)
        :box nil
        :overline ,(aaron-ui-color 'border-popup-separator)
        :underline nil
        :height 0.24)))
  "Face used for the popup ghostel separator bar."
  :group 'my/ghostel-popup)

(defun my/ghostel-popup--separator-line ()
  "Return a full-width separator line for popup ghostel windows."
  (propertize
   " "
   'face 'my/ghostel-popup-separator
   'display '(space :align-to right)))

(defun my/ghostel-popup--kind-label (&optional buffer)
  "Return a short semantic label for popup BUFFER."
  (let ((kind (if buffer
                  (buffer-local-value 'my/ghostel-popup-kind buffer)
                my/ghostel-popup-kind)))
    (pcase kind
      ('ai-claude "cc")
      ('ai-codex "codex")
      ('ai-opencode "opencode")
      ('agent "agent")
      ('buffer "buf")
      (_ "term"))))

(defun my/ghostel-popup--tab-title (buffer)
  "Return the display title for popup BUFFER."
  (or (buffer-local-value 'my/ghostel-popup-title buffer)
      (buffer-name buffer)))

(defun my/ghostel-popup--location-label (buffer)
  "Return BUFFER's project and target label without contacting its host."
  (let* ((directory (buffer-local-value 'default-directory buffer))
         (workspace (buffer-local-value 'my/ghostel-popup-workspace-id buffer))
         (target (and (stringp workspace)
                      (string-match "\\`\\([^@]+\\)@" workspace)
                      (match-string 1 workspace)))
         (name (file-name-nondirectory (directory-file-name directory))))
    (concat (if (string-empty-p name) "/" name)
            (if (and target (not (equal target "local")))
                (concat "@" target)
              ""))))

(defun my/ghostel-popup-select-buffer (buffer)
  "Select popup BUFFER in the shared popup window."
  (interactive
   (list (my/ghostel-popup--read-buffer "Popup tab: ")))
  (unless (my/ghostel-popup--buffer-p buffer)
    (user-error "Not a popup ghostel buffer: %s" buffer))
  (select-window (my/ghostel-popup--show-buffer buffer))
  buffer)

(defun my/ghostel-popup-select-index (index)
  "Switch to popup terminal tab INDEX in the global popup pool."
  (interactive "nPopup tab number: ")
  (let ((buffer (nth (1- index) (my/ghostel-popup--live-buffers))))
    (unless buffer
      (user-error "No popup terminal tab %d" index))
    (my/ghostel-popup-select-buffer buffer)))

(defun my/ghostel-popup--tab-segment (buffer index current)
  "Return a clickable tab segment for BUFFER at INDEX.
CURRENT is the currently displayed popup buffer."
  (let* ((selected (eq buffer current))
         (face (if selected 'my/ghostel-popup-tab-current 'my/ghostel-popup-tab))
         (title (my/ghostel-popup--tab-title buffer))
         (label (format " %d:%s %s · %s "
                        index
                        (my/ghostel-popup--kind-label buffer)
                        (file-name-nondirectory
                         (directory-file-name title))
                        (my/ghostel-popup--location-label buffer))))
    (propertize
     label
     'face face
     'mouse-face 'mode-line-highlight
     'help-echo (format "Switch to %s (%s)"
                        (buffer-name buffer)
                        (buffer-local-value 'default-directory buffer))
     'local-map (let ((map (make-sparse-keymap)))
                  (define-key map [header-line mouse-3] #'my/ghostel-popup-menu)
                  (define-key map [tab-line mouse-3] #'my/ghostel-popup-menu)
                  (define-key map [tab-line mouse-1]
                              (lambda () (interactive) (my/ghostel-popup-select-buffer buffer)))
                  (define-key map [header-line mouse-1]
                              (lambda ()
                                (interactive)
                                (my/ghostel-popup-select-buffer buffer)))
                  map))))

(defun my/ghostel-popup-new ()
  "Create a popup ghostel in the focused work buffer's directory."
  (interactive)
  (let ((directory (buffer-local-value
                    'default-directory (my/ghostel-popup--context-buffer))))
    (select-window
     (my/ghostel-popup--show-buffer
      (my/ghostel-popup--create-buffer directory)))))

(defun my/ghostel-popup--agent-menu-items ()
  "Return agent commands without loading agent-shell or starting processes."
  '(["Claude" my/ghostel-popup-agent-claude t]
    ["Codex" my/ghostel-popup-agent-codex t]
    ["OpenCode" my/ghostel-popup-agent-opencode t]))

(defun my/ghostel-popup-menu (&optional event)
  "Open the popup launcher menu at mouse EVENT, or at point."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (popup-menu
   (easy-menu-create-menu
    "New popup"
    (append '(["Terminal" my/ghostel-popup-new t]
              "---"
              ["Move buffer into popup" my/ghostel-popup-move-buffer-in t]
              ["Move popup buffer to window…" my/ghostel-popup-move-buffer-out
               (my/ghostel-popup--live-buffers)]
              "---")
            (list (cons "Applications"
                        (mapcar (lambda (app)
                                  (vector (car app)
                                          `(lambda () (interactive) (my/ghostel-popup-app ,(car app))) t))
                                my/project-popup-ghostel-apps)))
            (list (cons "Agent" (my/ghostel-popup--agent-menu-items))))) event))

(defun my/ghostel-popup-agent-menu (&optional event)
  "Choose an agent-shell session to open in the shared popup."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (popup-menu (easy-menu-create-menu "Agent" (my/ghostel-popup--agent-menu-items)) event))

(defun my/agent-shell-launch-context ()
  "Return (NOEMA-ROOT . DIRECTORY) for the focused work buffer."
  (with-current-buffer (my/ghostel-popup--context-buffer)
    (let* ((noema-root (and (require 'noema-research nil t)
                            (noema-project-root default-directory)))
           (directory
            (file-name-as-directory
             (or (and noema-root (noema-project-workspace noema-root))
                 (and (fboundp 'my/project-current-root)
                      (my/project-current-root))
                 default-directory))))
      (cons noema-root directory))))

(defun my/ghostel-popup-agent (agent)
  "Start AGENT through the Noema agent-shell boundary, never through ghostel."
  (interactive (list (intern (completing-read "Popup agent: " '("claude" "codex" "opencode") nil t))))
  (unless (memq agent '(claude codex opencode)) (user-error "Unsupported popup agent: %s" agent))
  (require 'noema-agent-acp)
  (pcase-let* ((`(,noema-root . ,directory) (my/agent-shell-launch-context)))
    (let* ((workspace (my/ghostel-popup--requested-workspace-id directory))
           (config (or (noema-agent-acp-config-for agent)
                       (user-error "No agent-shell configuration for %s" agent)))
           ;; Protect the existing temporary popup while ACP initializes.
           (my/ghostel-popup--displaying t)
           (buffer (noema-agent-acp-start :config config :directory directory
                                          :focus nil :origin 'popup)))
      (noema-agent-acp-adopt buffer :agent agent :origin 'popup
                             :root noema-root)
      (with-current-buffer buffer
        (setq-local my/ghostel-popup-kind (intern (format "ai-%s" agent))
                    my/ghostel-popup-title (capitalize (symbol-name agent))
                    my/ghostel-popup-workspace-id workspace
                    noema-agent-acp-display-buffer-function
                    #'my/ghostel-popup-display-buffer)
        (noema-agent-acp-tabs-mode -1)
        (my/ghostel-popup-agent-keys-mode 1))
      (my/ghostel-popup-display-buffer buffer))))

(defalias 'my/agent-shell #'my/ghostel-popup-agent
  "Start a managed agent in the global popup from the focused project.")

(defun my/ghostel-popup-agent-claude ()
  "Open Claude in the popup using agent-shell."
  (interactive) (my/ghostel-popup-agent 'claude))
(defun my/ghostel-popup-agent-codex ()
  "Open Codex in the popup using agent-shell."
  (interactive) (my/ghostel-popup-agent 'codex))
(defun my/ghostel-popup-agent-opencode ()
  "Open OpenCode in the popup using agent-shell."
  (interactive) (my/ghostel-popup-agent 'opencode))

(defvar my/ghostel-popup-agent-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'ghostel-toggle)
    (define-key map (kbd "C-c E") #'my/ghostel-popup-cycle)
    (define-key map (kbd "C-c M-e") #'my/ghostel-toggle-fixed)
    map))

(define-minor-mode my/ghostel-popup-agent-keys-mode
  "Share popup folding, cycling and pinning keys in an agent-shell buffer."
  :lighter nil :keymap my/ghostel-popup-agent-keys-mode-map)

(defun my/ghostel-popup--new-tab-segment ()
  "Return a clickable segment that creates a new popup ghostel."
  (propertize
   " +term "
   'face 'my/ghostel-popup-tab
   'mouse-face 'mode-line-highlight
   'help-echo "Left click: terminal; right click: Applications / Agent menu"
   'local-map (let ((map (make-sparse-keymap)))
                (define-key map [header-line mouse-3] #'my/ghostel-popup-menu)
                (define-key map [tab-line mouse-3] #'my/ghostel-popup-menu)
                (define-key map [tab-line mouse-1] #'my/ghostel-popup-new)
                (define-key map [header-line mouse-1]
                            (lambda ()
                              (interactive)
                              (my/ghostel-popup-new)))
                map)))

(defun my/ghostel-popup--agent-tab-segment ()
  "Return the agent launcher; constructing it performs no IO."
  (propertize " +Agent " 'face 'my/ghostel-popup-tab 'mouse-face 'mode-line-highlight
              'help-echo "Claude / Codex / OpenCode via agent-shell"
              'local-map (let ((map (make-sparse-keymap)))
                           (dolist (key '([header-line mouse-1] [header-line mouse-3]
                                          [tab-line mouse-1] [tab-line mouse-3]))
                             (define-key map key #'my/ghostel-popup-agent-menu))
                           map)))

(defun my/ghostel-popup--tab-line ()
  "Return the popup ghostel tab strip for the header line."
  (let* ((buffers (my/ghostel-popup--live-buffers))
         (current (current-buffer))
         (tabs (cl-loop for buffer in buffers
                        for index from 1
                        collect (my/ghostel-popup--tab-segment
                                 buffer index current))))
    (append
     (list " ")
     tabs
     (list " "
           (my/ghostel-popup--new-tab-segment)
           (my/ghostel-popup--agent-tab-segment)
           " "
           (propertize "C-c C-e toggle  C-c E next  C-c M-e pin"
                       'face 'shadow)))))

(defun my/ghostel-popup-apply-ui (buffer)
  "Apply local popup terminal UI to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (derived-mode-p 'ghostel-mode)
          (progn
            (setq-local mode-line-format '((:eval (my/ghostel-popup--separator-line))))
            (setq-local header-line-format '(:eval (my/ghostel-popup--tab-line))))
        ;; Native agent headers and ordinary buffer headers stay intact.
        (setq-local tab-line-format '(:eval (my/ghostel-popup--tab-line))
                    tab-line-exclude t))
      (when (or (derived-mode-p 'ghostel-mode)
                (derived-mode-p 'agent-shell-mode))
        (setq-local fringes-outside-margins nil)
        (setq-local left-margin-width 0)
        (setq-local right-margin-width 0)))))

(defun my/ghostel-popup--save-ui ()
  "Save the current buffer's window-line values before popup adoption."
  (unless my/ghostel-popup--saved-ui
    (setq-local my/ghostel-popup--saved-ui
                (mapcar (lambda (variable)
                          (list variable (local-variable-p variable)
                                (symbol-value variable)))
                        '(mode-line-format header-line-format
                          tab-line-format tab-line-exclude
                          fringes-outside-margins left-margin-width
                          right-margin-width)))))

(defun my/ghostel-popup--restore-ui ()
  "Restore the current buffer's window-line values after popup release."
  (if my/ghostel-popup--saved-ui
      (dolist (entry my/ghostel-popup--saved-ui)
        (if (cadr entry)
            (set (make-local-variable (car entry)) (caddr entry))
          (kill-local-variable (car entry))))
    ;; A buffer from an Emacs session that loaded the old popup UI has no
    ;; snapshot.  Remove only the chrome that old code installed.
    (if (derived-mode-p 'ghostel-mode)
        (progn
          (kill-local-variable 'mode-line-format)
          (kill-local-variable 'header-line-format))
      (kill-local-variable 'tab-line-format)
      (kill-local-variable 'tab-line-exclude)))
  (setq-local my/ghostel-popup--saved-ui nil))

(defun my/ghostel-popup--buffer-p (buffer)
  "Return non-nil when BUFFER is a live popup ghostel buffer."
  (and (buffer-live-p buffer)
       (buffer-local-value 'my/ghostel-popup-instance-p buffer)))

(defun my/ghostel-popup--live-buffers ()
  "Return every live popup buffer in creation order, pruning dead entries."
  (setq my/ghostel-popup-buffers
        (cl-remove-if-not #'my/ghostel-popup--buffer-p my/ghostel-popup-buffers)))

(defun my/ghostel-popup--context-buffer ()
  "Return the focused work buffer, even while the popup has focus."
  (let ((selected (window-buffer (selected-window))))
    (if (or (minibufferp selected)
            (my/ghostel-popup--buffer-p selected))
        (or (and (buffer-live-p my/ghostel-popup-origin-buffer)
                 my/ghostel-popup-origin-buffer)
            (and (minibufferp selected)
                 (window-buffer (minibuffer-selected-window)))
            selected)
      selected)))

(defun my/ghostel-popup--remember-origin (&rest _)
  "Remember the focused work buffer for popup launchers."
  (let ((buffer (window-buffer (selected-window))))
    (unless (or (minibufferp buffer)
                (my/ghostel-popup--buffer-p buffer))
      (setq my/ghostel-popup-origin-buffer buffer))))

(defun my/ghostel-popup--requested-workspace-id (&optional directory)
  "Return the popup workspace ID implied by DIRECTORY or current buffer."
  (or (and (null directory)
           (bound-and-true-p my/ghostel-popup-instance-p)
           my/ghostel-popup-workspace-id)
      (my/ghostel-workspace-id (or directory default-directory))))

(defun my/ghostel-popup--current-buffer (&optional create directory)
  "Return the current popup ghostel buffer.
When CREATE is non-nil, create one if the global pool is empty.
DIRECTORY is used only when creating a buffer."
  (let ((buffers (my/ghostel-popup--live-buffers)))
    (unless (my/ghostel-popup--buffer-p my/ghostel-popup-current-buffer)
      (setq my/ghostel-popup-current-buffer (car buffers)))
    (or my/ghostel-popup-current-buffer
        (when create
          (my/ghostel-popup--create-buffer
           (or directory default-directory))))))

(defun my/ghostel-popup--buffer-name (index)
  "Return the default popup ghostel buffer name for INDEX."
  (if (= index 1)
      "*ghostel-popup*"
    (format "*ghostel-popup:%d*" index)))

(defun my/ghostel-popup--next-buffer-name ()
  "Return the next available popup ghostel buffer name."
  (let ((index 1)
        name)
    (while
        (progn
          (setq name (my/ghostel-popup--buffer-name index))
          (setq index (1+ index))
          (get-buffer name)))
    name))

(defun my/ghostel-popup--window (&optional buffer)
  "Return the popup ghostel window on the selected frame.
When BUFFER is non-nil, return the window displaying BUFFER."
  (catch 'window
    (dolist (window (window-list (selected-frame) 'no-minibuf))
      (when (and (window-parameter window 'my-ghostel-popup)
                 (or (null buffer)
                     (eq (window-buffer window) buffer)))
        (throw 'window window)))))

(defun my/ghostel-popup--effective-window-height ()
  "Return the height to use for newly shown popup ghostel windows."
  (if (and (numberp my/ghostel-popup-last-height)
           (> my/ghostel-popup-last-height 0)
           (<= my/ghostel-popup-last-height 1))
      my/ghostel-popup-last-height
    my/ghostel-popup-window-height))

(defun my/ghostel-popup--record-window-height (&optional window)
  "Record WINDOW's current height for future popup ghostel toggles."
  (when-let* ((window (or window (my/ghostel-popup--window)))
              (root-window (frame-root-window (window-frame window)))
              (root-height (window-total-height root-window))
              ((> root-height 0)))
    (setq my/ghostel-popup-last-height
          (/ (float (window-total-height window))
             (float root-height)))))

(defun my/ghostel-popup--show-buffer (buffer)
  "Show popup ghostel BUFFER in the shared side window."
  (let ((my/ghostel-popup--displaying t))
    (my/ghostel-popup--remember-origin)
    (setq my/ghostel-popup-current-buffer buffer)
    (let ((window (or (my/ghostel-popup--window)
                      (display-buffer-in-side-window
                       buffer
                       `((side . top)
                         (slot . 1)
                         (window-height . ,(my/ghostel-popup--effective-window-height)))))))
      (set-window-buffer window buffer)
      (set-window-parameter window 'my-ghostel-popup t)
      (set-window-parameter
       window 'my-ghostel-fixed
       (buffer-local-value 'my/ghostel-popup-fixed buffer))
      (set-window-parameter window 'no-delete-other-windows t)
      (window-preserve-size window nil t)
      ;; Selecting inside the protected phase prevents
      ;; `buffer-list-update-hook' from observing a newly created popup as an
      ;; already-unfocused temporary window.
      (select-window window)
      window)))

(defun my/ghostel-popup-display-buffer (buffer)
  "Display BUFFER using the shared popup ghostel window logic."
  (unless (buffer-live-p buffer)
    (user-error "Dead buffer: %s" buffer))
  (with-current-buffer buffer
    (unless my/ghostel-popup-instance-p
      (my/ghostel-popup--save-ui))
    (setq-local my/ghostel-popup-instance-p t)
    (unless (or (derived-mode-p 'ghostel-mode)
                (derived-mode-p 'agent-shell-mode))
      (setq-local my/ghostel-popup-kind 'buffer))
    (unless my/ghostel-popup-workspace-id
      (setq-local my/ghostel-popup-workspace-id
                  (my/ghostel-workspace-id default-directory)))
    (unless (boundp 'my/ghostel-popup-fixed)
      (setq-local my/ghostel-popup-fixed nil))
    (when (and (boundp 'remote-terminal-instance)
               (remote-terminal-p remote-terminal-instance))
      (remote-terminal-put-metadata
       remote-terminal-instance :popup t))
    (when (derived-mode-p 'agent-shell-mode)
      (when (eq my/ghostel-popup-kind 'terminal)
        (let ((agent (and (boundp 'noema-agent-acp-session-agent)
                          noema-agent-acp-session-agent)))
          (setq-local my/ghostel-popup-kind
                      (if agent (intern (format "ai-%s" agent)) 'agent)
                      my/ghostel-popup-title
                      (and agent (capitalize (format "%s" agent))))))
      (when (fboundp 'noema-agent-acp-tabs-mode)
        (noema-agent-acp-tabs-mode -1))
      (setq-local noema-agent-acp-display-buffer-function
                  #'my/ghostel-popup-display-buffer)
      (my/ghostel-popup-agent-keys-mode 1))
    (my/ghostel-popup-apply-ui buffer)
    (add-hook 'kill-buffer-hook #'my/ghostel-popup--on-kill nil t))
  (unless (memq buffer (my/ghostel-popup--live-buffers))
    (setq my/ghostel-popup-buffers
          (append (my/ghostel-popup--live-buffers) (list buffer))))
  (select-window (my/ghostel-popup--show-buffer buffer))
  buffer)

(defun my/ghostel-popup--normal-windows ()
  "Return ordinary windows on the selected frame."
  (seq-remove (lambda (window)
                (or (window-minibuffer-p window)
                    (window-parameter window 'my-ghostel-popup)))
              (window-list (selected-frame) 'nomini)))

(defun my/ghostel-popup--choose-normal-window (prompt)
  "Choose a non-popup window with PROMPT, using ace-window when needed."
  (let ((windows (my/ghostel-popup--normal-windows)))
    (unless windows
      (user-error "No ordinary window is available"))
    (if (= (length windows) 1)
        (car windows)
      (require 'ace-window)
      (let ((original (symbol-function 'aw-window-list))
            (aw-scope 'frame))
        (cl-letf (((symbol-function 'aw-window-list)
                   (lambda ()
                     (seq-filter (lambda (window) (memq window windows))
                                 (funcall original)))))
          (let ((window (aw-select prompt)))
            (unless (memq window windows)
              (user-error "No window selected"))
            window))))))

(defun my/ghostel-popup-move-buffer-in ()
  "Move the current ordinary buffer into the shared popup.
From a popup tab, use ace-window to choose the ordinary source window."
  (interactive)
  (let* ((my/ghostel-popup--displaying t)
         (source (if (window-parameter (selected-window) 'my-ghostel-popup)
                     (my/ghostel-popup--choose-normal-window
                      "Move buffer into popup")
                   (selected-window)))
         (buffer (window-buffer source)))
    (when (window-dedicated-p source)
      (user-error "Cannot move a buffer from a dedicated window"))
    (with-selected-window source
      (switch-to-prev-buffer source 'bury))
    (my/ghostel-popup-display-buffer buffer)))

(defun my/ghostel-popup--release-buffer (buffer target)
  "Remove BUFFER from the popup pool and send future displays to TARGET."
  (with-current-buffer buffer
    (setq-local my/ghostel-popup-instance-p nil)
    (remove-hook 'kill-buffer-hook #'my/ghostel-popup--on-kill t)
    (my/ghostel-popup--restore-ui)
    (when (and (boundp 'remote-terminal-instance)
               (remote-terminal-p remote-terminal-instance))
      (remote-terminal-put-metadata remote-terminal-instance :popup nil))
    (when (derived-mode-p 'agent-shell-mode)
      (my/ghostel-popup-agent-keys-mode -1)
      (when (equal tab-line-format '(:eval (noema-agent-acp--tab-line)))
        (kill-local-variable 'tab-line-format))
      (setq-local noema-agent-acp-display-buffer-function
                  (lambda (session)
                    (when (window-live-p target)
                      (select-window target))
                    (switch-to-buffer session)))))
  (setq my/ghostel-popup-buffers (delq buffer my/ghostel-popup-buffers)))

(defun my/ghostel-popup-move-buffer-out ()
  "Move the current popup buffer into an ordinary window chosen with ace-window."
  (interactive)
  (let* ((popup (my/ghostel-popup--window))
         (buffer (if popup (window-buffer popup)
                   (my/ghostel-popup--current-buffer))))
    (unless (my/ghostel-popup--buffer-p buffer)
      (user-error "No popup buffer is available"))
    (let* ((my/ghostel-popup--displaying t)
           (target (my/ghostel-popup--choose-normal-window
                    "Move popup buffer to window"))
           (next (seq-find (lambda (entry) (not (eq entry buffer)))
                           (my/ghostel-popup--live-buffers))))
      (my/ghostel-popup--release-buffer buffer target)
      (when (eq my/ghostel-popup-current-buffer buffer)
        (setq my/ghostel-popup-current-buffer next))
      (set-window-buffer target buffer)
      (when (and popup (eq (window-buffer popup) buffer))
        (if next
            (progn
              (set-window-buffer popup next)
              (set-window-parameter
               popup 'my-ghostel-fixed
               (buffer-local-value 'my/ghostel-popup-fixed next)))
          (my/ghostel-hide-popup)))
      (select-window target)
      (let ((my/ghostel-popup--displaying nil))
        (my/ghostel-popup--auto-hide))
      buffer)))

(defun my/ghostel-popup--on-kill ()
  "Clean up popup ghostel state when the current buffer is killed."
  (let* ((buffer (current-buffer))
         (buffers (my/ghostel-popup--live-buffers))
         (tail (member buffer buffers))
         (next (or (cadr tail)
                   (seq-find (lambda (entry) (not (eq entry buffer))) buffers)))
         (window (my/ghostel-popup--window buffer)))
    (when window
      (my/ghostel-popup--record-window-height window))
    (setq my/ghostel-popup-buffers (delq buffer my/ghostel-popup-buffers))
    (when (eq my/ghostel-popup-current-buffer buffer)
      (setq my/ghostel-popup-current-buffer
            (and next
                 (not (eq next buffer))
                 next)))
    (when window
      (ignore-errors (delete-window window)))))

(defun my/ghostel-popup--create-buffer (&optional directory buffer-name)
  "Create and register a new popup ghostel buffer.
Use DIRECTORY as the initial terminal directory when non-nil.
Use BUFFER-NAME when non-nil."
  (my/ghostel-require)
  (let* ((default-directory (or (my/terminal-normalize-directory directory)
                                default-directory))
         (target-name (generate-new-buffer-name
                       (or buffer-name
                           (my/ghostel-popup--next-buffer-name)))))
    (with-current-buffer (my/ghostel-create-hidden target-name)
      (my/ghostel-popup--save-ui)
      (setq-local my/ghostel-popup-instance-p t)
      (setq-local my/ghostel-popup-fixed nil)
      (setq-local
       my/ghostel-popup-workspace-id
       (if (and (boundp 'remote-terminal-instance)
                remote-terminal-instance)
           (remote-terminal-workspace-id remote-terminal-instance)
         (my/ghostel-workspace-id default-directory)))
      (when (and (boundp 'remote-terminal-instance)
                 (remote-terminal-p remote-terminal-instance))
        (remote-terminal-put-metadata
         remote-terminal-instance :popup t))
      (my/ghostel-popup-apply-ui (current-buffer))
      (add-hook 'kill-buffer-hook #'my/ghostel-popup--on-kill nil t)
      (setq my/ghostel-popup-buffers
            (append (my/ghostel-popup--live-buffers) (list (current-buffer))))
      (setq my/ghostel-popup-current-buffer (current-buffer))
      (current-buffer))))

(defun my/ghostel-popup--project-root ()
  "Return the current project root.
Signal a user error when outside a project."
  (with-current-buffer (my/ghostel-popup--context-buffer)
    (or (and (fboundp 'my/project-current-root)
             (my/project-current-root))
        (when (fboundp 'project-current)
          (when-let* ((project (project-current nil default-directory)))
            (project-root project)))
        (user-error "Not inside a project"))))

(defun my/ghostel-popup--project-name (project-root)
  "Return a short project name for PROJECT-ROOT."
  (file-name-nondirectory
   (directory-file-name
    (file-name-as-directory
     (expand-file-name project-root)))))

(defun my/ghostel-popup-app (app &optional directory)
  "Run configured terminal APP in a fresh popup at DIRECTORY or here."
  (interactive
   (list
    (completing-read "Project terminal app: "
                     (mapcar #'car my/project-popup-ghostel-apps)
                     nil t)))
  (let* ((project-root (file-name-as-directory
                        (expand-file-name
                         (or directory
                             (buffer-local-value
                              'default-directory
                              (my/ghostel-popup--context-buffer))))))
         (command (or (cdr (assoc app my/project-popup-ghostel-apps))
                      (user-error "Unknown project terminal app: %s" app)))
         (buffer-name (format "*ghostel-popup:%s:%s*"
                              app
                              (my/ghostel-popup--project-name project-root)))
         (buffer (my/ghostel-popup--create-buffer project-root buffer-name)))
    (with-current-buffer buffer
      (setq-local ghostel-kill-buffer-on-exit t))
    (my/ghostel-send-command buffer (format "clear; %s; exit" command))
    (select-window (my/ghostel-popup--show-buffer buffer))
    buffer))

(defun my/project-popup-ghostel-app (app)
  "Run APP in a fresh popup ghostel rooted at the current project."
  (interactive (list (completing-read "Project terminal app: "
                                     (mapcar #'car my/project-popup-ghostel-apps) nil t)))
  (my/ghostel-popup-app app (my/ghostel-popup--project-root)))

(defun my/ghostel-popup--next-buffer ()
  "Return the next popup ghostel buffer in creation order."
  (let* ((buffers (my/ghostel-popup--live-buffers))
         (current (my/ghostel-popup--current-buffer)))
    (cond
     ((null buffers)
      nil)
     ((null current)
      (car buffers))
     (t
      (or (cadr (member current buffers))
          (car buffers))))))

(defun my/ghostel-popup--read-buffer (prompt)
  "Read a popup ghostel buffer with PROMPT."
  (let* ((buffers (my/ghostel-popup--live-buffers))
         (current (my/ghostel-popup--current-buffer))
         (default (and current (buffer-name current))))
    (unless buffers
      (user-error "No popup ghostel buffers"))
    (get-buffer
     (completing-read prompt
                      (mapcar #'buffer-name buffers)
                      nil t nil nil default))))

(defun my/ghostel-hide-popup ()
  "Hide the current popup ghostel window."
  (interactive)
  (when-let* ((window (my/ghostel-popup--window)))
    (my/ghostel-popup--record-window-height window)
    (ignore-errors (delete-window window))))

(defun my/ghostel-show-popup ()
  "Show the current popup ghostel buffer."
  (interactive)
  (my/ghostel-popup--show-buffer (my/ghostel-popup--current-buffer t)))

(defun my/ghostel-popup-cycle (arg)
  "Cycle popup ghostel buffers.
With prefix ARG, create a new popup ghostel and switch to it."
  (interactive "P")
  (let ((buffer (if arg
                    (my/ghostel-popup--create-buffer
                     (buffer-local-value
                      'default-directory (my/ghostel-popup--context-buffer)))
                  (or (my/ghostel-popup--next-buffer)
                      (my/ghostel-popup--create-buffer
                       (buffer-local-value
                        'default-directory (my/ghostel-popup--context-buffer)))))))
    (select-window (my/ghostel-popup--show-buffer buffer))
    buffer))

(defun my/ghostel-popup-kill (buffer)
  "Kill popup ghostel BUFFER."
  (interactive
   (list (if current-prefix-arg
             (my/ghostel-popup--read-buffer "Kill popup ghostel: ")
           (or (my/ghostel-popup--current-buffer)
               (user-error "No popup ghostel buffers")))))
  (kill-buffer buffer))

(defun my/ghostel-popup-rename (buffer name)
  "Rename popup ghostel BUFFER to NAME."
  (interactive
   (let* ((buffer (if current-prefix-arg
                      (my/ghostel-popup--read-buffer "Rename popup ghostel: ")
                    (or (my/ghostel-popup--current-buffer)
                        (user-error "No popup ghostel buffers"))))
          (current-name (buffer-name buffer)))
     (list buffer
           (read-string "New popup ghostel name: " current-name nil current-name))))
  (when-let* ((existing (get-buffer name)))
    (unless (eq existing buffer)
      (user-error "Buffer %s already exists" name)))
  (with-current-buffer buffer
    (rename-buffer name))
  buffer)

(defun my/ghostel-toggle-fixed ()
  "Toggle fixed mode for the current popup ghostel buffer."
  (interactive)
  (let* ((buffer (my/ghostel-popup--current-buffer t))
         (fixed (with-current-buffer buffer
                  (setq-local my/ghostel-popup-fixed (not my/ghostel-popup-fixed))
                  my/ghostel-popup-fixed))
         (window (my/ghostel-popup--show-buffer buffer)))
    (set-window-parameter window 'my-ghostel-fixed fixed)
    (select-window window)
    (message "Popup ghostel %s %s"
             (buffer-name buffer)
             (if fixed "fixed" "temporary"))))

(defun my/ghostel-popup--auto-hide (&rest _)
  "Hide the popup ghostel window when a temporary instance loses focus."
  (unless my/ghostel-popup--displaying
    (when-let* (((or my/ghostel-popup-current-buffer
                   my/ghostel-popup-buffers))
                (window (my/ghostel-popup--window)))
      (let ((buffer (window-buffer window)))
        (unless (or (active-minibuffer-window)
                    (eq (selected-window) window)
                    (buffer-local-value 'my/ghostel-popup-fixed buffer))
          (my/ghostel-hide-popup))))))

(defun ghostel-toggle ()
  "Toggle the current popup ghostel buffer."
  (interactive)
  (let* ((buffer (my/ghostel-popup--current-buffer t))
         (window (my/ghostel-popup--window)))
    (cond
     ((and window
           (eq (window-buffer window) buffer)
           (eq (selected-window) window))
      (my/ghostel-hide-popup))
     ((and window
           (eq (window-buffer window) buffer))
      (select-window window))
     (t
      (select-window (my/ghostel-popup--show-buffer buffer))))))

(global-set-key (kbd "M-`") #'ghostel-toggle)
(global-set-key (kbd "C-c e") #'ghostel-toggle)
(global-set-key (kbd "C-c C-e") #'ghostel-toggle)
(global-set-key (kbd "C-c E") #'my/ghostel-popup-cycle)
(global-set-key (kbd "C-c M-E") #'my/ghostel-popup-new)
(global-set-key (kbd "C-c M-e") #'my/ghostel-toggle-fixed)
(global-set-key (kbd "C-c w p") #'my/ghostel-popup-move-buffer-in)
(global-set-key (kbd "C-c w P") #'my/ghostel-popup-move-buffer-out)

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/ghostel-popup-last-height))

(add-hook 'window-selection-change-functions #'my/ghostel-popup--auto-hide)
(add-hook 'window-selection-change-functions #'my/ghostel-popup--remember-origin)
(add-hook 'buffer-list-update-hook #'my/ghostel-popup--auto-hide)

;; Reloading this library also updates agents already in the popup pool.
(dolist (buffer (my/ghostel-popup--live-buffers))
  (when (with-current-buffer buffer (derived-mode-p 'agent-shell-mode))
    (my/ghostel-popup-apply-ui buffer)))

(provide 'init-ghostel-popup)
;;; init-ghostel-popup.el ends here

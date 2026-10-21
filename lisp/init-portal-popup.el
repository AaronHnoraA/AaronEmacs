;;; init-portal-popup.el --- Popup portal pool -*- lexical-binding: t -*-

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
(declare-function my/portal-create "init-portal" (&rest args))
(declare-function my/portal-workspace-id "init-portal" (&optional directory))
(declare-function remote-terminal-p "remote-terminal" (object))
(declare-function remote-terminal-put-metadata "remote-terminal"
                  (terminal property value))
(declare-function remote-terminal-workspace-id "remote-terminal" (terminal))
(declare-function project-root "project" (project))
(declare-function my/project-current-root "init-project")
(declare-function noema-agent-acp-config-for "noema-agent-acp" (agent))
(declare-function noema-agent-acp-start "noema-agent-acp" (&rest args))
(declare-function noema-agent-acp-adopt "noema-agent-acp" (buffer &rest args))
(declare-function noema-agent-acp-tabs-mode "noema-agent-acp" (&optional arg))
(defvar noema-agent-acp-display-buffer-function)

(defvar remote-terminal-instance)

(defgroup my/portal-popup nil
  "Popup portal pool helpers."
  :group 'term
  :prefix "my/portal-popup-")

(config-defvar my/portal-popup-window-height nil
  "Height of the popup portal side window."
  :type 'number
  :group 'my/portal-popup)

(config-defvar my/project-popup-portal-apps nil
  "Preset terminal apps launched in a fresh popup portal at project root."
  :type '(alist :key-type string :value-type string)
  :group 'my/portal-popup)

(defvar my/portal-popup-buffers nil
  "Popup portal buffers in creation order.")

(defvar my/portal-popup-current-buffer nil
  "Current popup portal buffer.")

(defvar my/portal-popup-origin-buffer nil
  "Last focused work buffer used to place new popup terminals and agents.")

(defvar my/portal-popup-last-height nil
  "Last popup portal height ratio recorded from a visible window.")

(defvar my/portal-popup--displaying nil
  "Dynamically non-nil while a popup window is being shown and selected.")

(defvar-local my/portal-popup-fixed nil
  "Whether this popup portal buffer should stay visible after focus changes.")

(defvar-local my/portal-popup-instance-p nil
  "Whether the current buffer belongs to the popup portal pool.")

(defvar-local my/portal-popup-kind 'terminal
  "Semantic kind for the current popup buffer.")

(defvar-local my/portal-popup-title nil
  "Optional title shown in the popup header for the current buffer.")

(defvar-local my/portal-popup-workspace-id nil
  "Stable remote workspace ID which owns the current popup buffer.")

(defvar-local my/portal-popup--saved-ui nil
  "Window-line values to restore when this buffer leaves the popup pool.")

(defface my/portal-popup-tab-current
  `((t (:inherit mode-line-buffer-id
        :foreground ,(aaron-ui-color 'fg-strong)
        :background ,(aaron-ui-color 'bg-elevated)
        :box (:line-width (1 . -1)
              :color ,(aaron-ui-color 'border-popup-separator)))))
  "Face used for the active popup portal tab."
  :group 'my/portal-popup)

(defface my/portal-popup-tab
  `((t (:inherit mode-line
        :foreground ,(aaron-ui-color 'fg-muted)
        :background ,(aaron-ui-color 'bg-base))))
  "Face used for inactive popup portal tabs."
  :group 'my/portal-popup)

(defface my/portal-popup-separator
  `((t (:inherit mode-line
        :foreground ,(aaron-ui-color 'bg-popup-separator)
        :background ,(aaron-ui-color 'bg-popup-separator)
        :box nil
        :overline ,(aaron-ui-color 'border-popup-separator)
        :underline nil
        :height 0.24)))
  "Face used for the popup portal separator bar."
  :group 'my/portal-popup)

(defun my/portal-popup--separator-line ()
  "Return a full-width separator line for popup portal windows."
  (propertize
   " "
   'face 'my/portal-popup-separator
   'display '(space :align-to right)))

(defun my/portal-popup--kind-label (&optional buffer)
  "Return a short semantic label for popup BUFFER."
  (let ((kind (if buffer
                  (buffer-local-value 'my/portal-popup-kind buffer)
                my/portal-popup-kind)))
    (pcase kind
      ('ai-claude "cc")
      ('ai-codex "codex")
      ('ai-opencode "opencode")
      ('agent "agent")
      ('buffer "buf")
      (_ "term"))))

(defun my/portal-popup--tab-title (buffer)
  "Return the display title for popup BUFFER."
  (or (buffer-local-value 'my/portal-popup-title buffer)
      (buffer-name buffer)))

(defun my/portal-popup--location-label (buffer)
  "Return BUFFER's project and target label without contacting its host."
  (let* ((directory (buffer-local-value 'default-directory buffer))
         (workspace (buffer-local-value 'my/portal-popup-workspace-id buffer))
         (target (and (stringp workspace)
                      (string-match "\\`\\([^@]+\\)@" workspace)
                      (match-string 1 workspace)))
         (name (file-name-nondirectory (directory-file-name directory))))
    (concat (if (string-empty-p name) "/" name)
            (if (and target (not (equal target "local")))
                (concat "@" target)
              ""))))

(defun my/portal-popup-select-buffer (buffer)
  "Select popup BUFFER in the shared popup window."
  (interactive
   (list (my/portal-popup--read-buffer "Popup tab: ")))
  (unless (my/portal-popup--buffer-p buffer)
    (user-error "Not a popup portal buffer: %s" buffer))
  (select-window (my/portal-popup--show-buffer buffer))
  buffer)

(defun my/portal-popup-select-index (index)
  "Switch to popup terminal tab INDEX in the global popup pool."
  (interactive "nPopup tab number: ")
  (let ((buffer (nth (1- index) (my/portal-popup--live-buffers))))
    (unless buffer
      (user-error "No popup terminal tab %d" index))
    (my/portal-popup-select-buffer buffer)))

(defun my/portal-popup--tab-segment (buffer index current)
  "Return a clickable tab segment for BUFFER at INDEX.
CURRENT is the currently displayed popup buffer."
  (let* ((selected (eq buffer current))
         (face (if selected 'my/portal-popup-tab-current 'my/portal-popup-tab))
         (title (my/portal-popup--tab-title buffer))
         (label (format " %d:%s %s · %s "
                        index
                        (my/portal-popup--kind-label buffer)
                        (file-name-nondirectory
                         (directory-file-name title))
                        (my/portal-popup--location-label buffer))))
    (propertize
     label
     'face face
     'mouse-face 'mode-line-highlight
     'help-echo (format "Switch to %s (%s)"
                        (buffer-name buffer)
                        (buffer-local-value 'default-directory buffer))
     'local-map (let ((map (make-sparse-keymap)))
                  (dolist (key '([header-line mouse-3] [tab-line mouse-3]))
                    (define-key map key
                                (lambda (event)
                                  (interactive "e")
                                  (my/portal-popup-tab-menu buffer event))))
                  (define-key map [tab-line mouse-1]
                              (lambda () (interactive) (my/portal-popup-select-buffer buffer)))
                  (define-key map [header-line mouse-1]
                              (lambda ()
                                (interactive)
                                (my/portal-popup-select-buffer buffer)))
                  map))))

(defun my/portal-popup-move-tab (buffer position)
  "Move popup BUFFER to one-based POSITION in the popup tab strip."
  (let* ((buffers (my/portal-popup--live-buffers))
         (without (delq buffer (copy-sequence buffers))))
    (unless (memq buffer buffers)
      (user-error "Popup tab is no longer available"))
    (setq my/portal-popup-buffers
          (append (seq-take without (1- position))
                  (list buffer)
                  (seq-drop without (1- position))))
    (force-mode-line-update t)))

(defun my/portal-popup-tab-menu (buffer &optional event)
  "Show actions for popup BUFFER at mouse EVENT."
  (let* ((buffers (my/portal-popup--live-buffers))
         (index (cl-position buffer buffers :test #'eq)))
    (unless index
      (user-error "Popup tab is no longer available"))
    (popup-menu
     (easy-menu-create-menu
      (format "Popup: %s" (buffer-name buffer))
      (append
       (list
        (vector "切换到此标签"
                `(lambda () (interactive)
                   (my/portal-popup-select-buffer ,buffer)) t)
        (vector "重命名"
                `(lambda () (interactive)
                   (my/portal-popup-rename
                    ,buffer
                    (read-string "New popup name: " (buffer-name ,buffer)))) t)
        (vector "关闭"
                `(lambda () (interactive)
                   (my/portal-popup-kill ,buffer)) t)
        "---"
        (vector "左移"
                `(lambda () (interactive)
                   (my/portal-popup-move-tab ,buffer ,index))
                (> index 0))
        (vector "右移"
                `(lambda () (interactive)
                   (my/portal-popup-move-tab ,buffer ,(+ index 2)))
                (< index (1- (length buffers)))))
       (list
        (cons "移动到位置"
              (cl-loop for position from 1 to (length buffers)
                       collect
                       (vector (format "第 %d 个" position)
                               `(lambda () (interactive)
                                  (my/portal-popup-move-tab ,buffer ,position))
                               (/= position (1+ index))))))))
     event)))

(defun my/portal-popup-new ()
  "Create a popup portal in the focused work buffer's directory."
  (interactive)
  (let ((directory (buffer-local-value
                    'default-directory (my/portal-popup--context-buffer))))
    (select-window
     (my/portal-popup--show-buffer
      (my/portal-popup--create-buffer directory)))))

(defun my/portal-popup--agent-menu-items ()
  "Return agent commands without loading agent-shell or starting processes."
  '(["Claude" my/portal-popup-agent-claude t]
    ["Codex" my/portal-popup-agent-codex t]
    ["OpenCode" my/portal-popup-agent-opencode t]))

(defun my/portal-popup-menu (&optional event)
  "Open the popup launcher menu at mouse EVENT, or at point."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (popup-menu
   (easy-menu-create-menu
    "New popup"
    (append '(["Terminal" my/portal-popup-new t]
              "---"
              ["Move buffer into popup" my/portal-popup-move-buffer-in t]
              ["Move popup buffer to window…" my/portal-popup-move-buffer-out
               (my/portal-popup--live-buffers)]
              "---")
            (list (cons "Applications"
                        (mapcar (lambda (app)
                                  (vector (car app)
                                          `(lambda () (interactive) (my/portal-popup-app ,(car app))) t))
                                my/project-popup-portal-apps)))
            (list (cons "Agent" (my/portal-popup--agent-menu-items))))) event))

(defun my/portal-popup-agent-menu (&optional event)
  "Choose an agent-shell session to open in the shared popup."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (popup-menu (easy-menu-create-menu "Agent" (my/portal-popup--agent-menu-items)) event))

(defun my/agent-shell-launch-context ()
  "Return (NOEMA-ROOT . DIRECTORY) for the focused work buffer."
  (with-current-buffer (my/portal-popup--context-buffer)
    (let* ((noema-root (and (require 'noema-research nil t)
                            (noema-project-root default-directory)))
           (directory
            (file-name-as-directory
             (or (and noema-root (noema-project-workspace noema-root))
                 (and (fboundp 'my/project-current-root)
                      (my/project-current-root))
                 default-directory))))
      (cons noema-root directory))))

(defun my/portal-popup-agent (agent)
  "Start AGENT through the Noema agent-shell boundary, never through portal."
  (interactive (list (intern (completing-read "Popup agent: " '("claude" "codex" "opencode") nil t))))
  (unless (memq agent '(claude codex opencode)) (user-error "Unsupported popup agent: %s" agent))
  (require 'noema-agent-acp)
  (pcase-let* ((`(,noema-root . ,directory) (my/agent-shell-launch-context)))
    (let* ((workspace (my/portal-popup--requested-workspace-id directory))
           (config (or (noema-agent-acp-config-for agent)
                       (user-error "No agent-shell configuration for %s" agent)))
           ;; Protect the existing temporary popup while ACP initializes.
           (my/portal-popup--displaying t)
           (buffer (noema-agent-acp-start :config config :directory directory
                                          :focus nil :origin 'popup)))
      (noema-agent-acp-adopt buffer :agent agent :origin 'popup
                             :root noema-root)
      (with-current-buffer buffer
        (setq-local my/portal-popup-kind (intern (format "ai-%s" agent))
                    my/portal-popup-title (capitalize (symbol-name agent))
                    my/portal-popup-workspace-id workspace
                    noema-agent-acp-display-buffer-function
                    #'my/portal-popup-display-buffer)
        (noema-agent-acp-tabs-mode -1)
        (my/portal-popup-agent-keys-mode 1))
      (my/portal-popup-display-buffer buffer))))

(defalias 'my/agent-shell #'my/portal-popup-agent
  "Start a managed agent in the global popup from the focused project.")

(defun my/portal-popup-agent-claude ()
  "Open Claude in the popup using agent-shell."
  (interactive) (my/portal-popup-agent 'claude))
(defun my/portal-popup-agent-codex ()
  "Open Codex in the popup using agent-shell."
  (interactive) (my/portal-popup-agent 'codex))
(defun my/portal-popup-agent-opencode ()
  "Open OpenCode in the popup using agent-shell."
  (interactive) (my/portal-popup-agent 'opencode))

(defvar my/portal-popup-agent-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'my/portal-popup-toggle)
    (define-key map (kbd "C-c E") #'my/portal-popup-cycle)
    (define-key map (kbd "C-c M-e") #'my/portal-toggle-fixed)
    map))

(define-minor-mode my/portal-popup-agent-keys-mode
  "Share popup folding, cycling and pinning keys in an agent-shell buffer."
  :lighter nil :keymap my/portal-popup-agent-keys-mode-map)

(defun my/portal-popup--new-tab-segment ()
  "Return a clickable segment that creates a new popup portal."
  (propertize
   " +term "
   'face 'my/portal-popup-tab
   'mouse-face 'mode-line-highlight
   'help-echo "Left click: terminal; right click: Applications / Agent menu"
   'local-map (let ((map (make-sparse-keymap)))
                (define-key map [header-line mouse-3] #'my/portal-popup-menu)
                (define-key map [tab-line mouse-3] #'my/portal-popup-menu)
                (define-key map [tab-line mouse-1] #'my/portal-popup-new)
                (define-key map [header-line mouse-1]
                            (lambda ()
                              (interactive)
                              (my/portal-popup-new)))
                map)))

(defun my/portal-popup--agent-tab-segment ()
  "Return the agent launcher; constructing it performs no IO."
  (propertize " +Agent " 'face 'my/portal-popup-tab 'mouse-face 'mode-line-highlight
              'help-echo "Claude / Codex / OpenCode via agent-shell"
              'local-map (let ((map (make-sparse-keymap)))
                           (dolist (key '([header-line mouse-1] [header-line mouse-3]
                                          [tab-line mouse-1] [tab-line mouse-3]))
                             (define-key map key #'my/portal-popup-agent-menu))
                           map)))

(defun my/portal-popup--tab-line ()
  "Return the popup portal tab strip for the header line."
  (let* ((buffers (my/portal-popup--live-buffers))
         (current (current-buffer))
         (tabs (cl-loop for buffer in buffers
                        for index from 1
                        collect (my/portal-popup--tab-segment
                                 buffer index current))))
    (append
     (list " ")
     tabs
     (list " "
           (my/portal-popup--new-tab-segment)
           (my/portal-popup--agent-tab-segment)
           " "
           (propertize "C-c C-e toggle  C-c E next  C-c M-e pin"
                       'face 'shadow)))))

(defun my/portal-popup-apply-ui (buffer)
  "Apply local popup terminal UI to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (derived-mode-p 'portal-terminal-mode)
          (progn
            (setq-local header-line-format '(:eval (my/portal-popup--tab-line))))
        ;; Native agent headers and ordinary buffer headers stay intact.
        (setq-local tab-line-format '(:eval (my/portal-popup--tab-line))
                    tab-line-exclude t))
      (when (or (derived-mode-p 'portal-terminal-mode)
                (derived-mode-p 'agent-shell-mode))
        (setq-local fringes-outside-margins nil)
        (setq-local left-margin-width 0)
        (setq-local right-margin-width 0)))))

(defun my/portal-popup--save-ui ()
  "Save the current buffer's window-line values before popup adoption."
  (unless my/portal-popup--saved-ui
    (setq-local my/portal-popup--saved-ui
                (mapcar (lambda (variable)
                          (list variable (local-variable-p variable)
                                (symbol-value variable)))
                        '(mode-line-format header-line-format
                          tab-line-format tab-line-exclude
                          fringes-outside-margins left-margin-width
                          right-margin-width)))))

(defun my/portal-popup--restore-ui ()
  "Restore the current buffer's window-line values after popup release."
  (if my/portal-popup--saved-ui
      (dolist (entry my/portal-popup--saved-ui)
        (if (cadr entry)
            (set (make-local-variable (car entry)) (caddr entry))
          (kill-local-variable (car entry))))
    ;; A buffer from an Emacs session that loaded the old popup UI has no
    ;; snapshot.  Remove only the chrome that old code installed.
    (if (derived-mode-p 'portal-terminal-mode)
        (progn
          (kill-local-variable 'mode-line-format)
          (kill-local-variable 'header-line-format))
      (kill-local-variable 'tab-line-format)
      (kill-local-variable 'tab-line-exclude)))
  (setq-local my/portal-popup--saved-ui nil))

(defun my/portal-popup--buffer-p (buffer)
  "Return non-nil when BUFFER is a live popup portal buffer."
  (and (buffer-live-p buffer)
       (buffer-local-value 'my/portal-popup-instance-p buffer)))

(defun my/portal-popup--live-buffers ()
  "Return every live popup buffer in creation order, pruning dead entries."
  (setq my/portal-popup-buffers
        (cl-remove-if-not #'my/portal-popup--buffer-p my/portal-popup-buffers)))

(defun my/portal-popup--context-buffer ()
  "Return the focused work buffer, even while the popup has focus."
  (let ((selected (window-buffer (selected-window))))
    (if (or (minibufferp selected)
            (my/portal-popup--buffer-p selected))
        (or (and (buffer-live-p my/portal-popup-origin-buffer)
                 my/portal-popup-origin-buffer)
            (and (minibufferp selected)
                 (window-buffer (minibuffer-selected-window)))
            selected)
      selected)))

(defun my/portal-popup--remember-origin (&rest _)
  "Remember the focused work buffer for popup launchers."
  (let ((buffer (window-buffer (selected-window))))
    (unless (or (minibufferp buffer)
                (my/portal-popup--buffer-p buffer))
      (setq my/portal-popup-origin-buffer buffer))))

(defun my/portal-popup--requested-workspace-id (&optional directory)
  "Return the popup workspace ID implied by DIRECTORY or current buffer."
  (or (and (null directory)
           (bound-and-true-p my/portal-popup-instance-p)
           my/portal-popup-workspace-id)
      (my/portal-workspace-id (or directory default-directory))))

(defun my/portal-popup--current-buffer (&optional create directory)
  "Return the current popup portal buffer.
When CREATE is non-nil, create one if the global pool is empty.
DIRECTORY is used only when creating a buffer."
  (let ((buffers (my/portal-popup--live-buffers)))
    (unless (my/portal-popup--buffer-p my/portal-popup-current-buffer)
      (setq my/portal-popup-current-buffer (car buffers)))
    (or my/portal-popup-current-buffer
        (when create
          (my/portal-popup--create-buffer
           (or directory default-directory))))))

(defun my/portal-popup--buffer-name (index)
  "Return the default popup portal buffer name for INDEX."
  (if (= index 1)
      "*portal-popup*"
    (format "*portal-popup:%d*" index)))

(defun my/portal-popup--next-buffer-name ()
  "Return the next available popup portal buffer name."
  (let ((index 1)
        name)
    (while
        (progn
          (setq name (my/portal-popup--buffer-name index))
          (setq index (1+ index))
          (get-buffer name)))
    name))

(defun my/portal-popup--windows (&optional buffer)
  "Return popup windows, across frames for BUFFER or on the selected frame."
  (seq-filter
   (lambda (window) (window-parameter window 'my-portal-popup))
   (if buffer
       (get-buffer-window-list buffer nil t)
     (window-list (selected-frame) 'no-minibuf))))

(defun my/portal-popup--window (&optional buffer)
  "Return a popup window on this frame, or one showing BUFFER in any frame."
  (car (my/portal-popup--windows buffer)))

(defun my/portal-popup--effective-window-height ()
  "Return the height to use for newly shown popup portal windows."
  (if (and (numberp my/portal-popup-last-height)
           (> my/portal-popup-last-height 0)
           (<= my/portal-popup-last-height 1))
      my/portal-popup-last-height
    my/portal-popup-window-height))

(defun my/portal-popup--record-window-height (&optional window)
  "Record WINDOW's current height for future popup portal toggles."
  (when-let* ((window (or window (my/portal-popup--window)))
              (root-window (frame-root-window (window-frame window)))
              (root-height (window-total-height root-window))
              ((> root-height 0)))
    (setq my/portal-popup-last-height
          (/ (float (window-total-height window))
             (float root-height)))))

(defun my/portal-popup--show-buffer (buffer)
  "Show popup portal BUFFER in the shared side window."
  (let ((my/portal-popup--displaying t))
    (my/portal-popup--remember-origin)
    (setq my/portal-popup-current-buffer buffer)
    (let ((window (or (my/portal-popup--window)
                      (display-buffer-in-side-window
                       buffer
                       `((side . top)
                         (slot . 1)
                         (window-height . ,(my/portal-popup--effective-window-height)))))))
      (set-window-buffer window buffer)
      (set-window-parameter window 'my-portal-popup t)
      (set-window-parameter
       window 'my-portal-fixed
       (buffer-local-value 'my/portal-popup-fixed buffer))
      (set-window-parameter window 'no-delete-other-windows t)
      (window-preserve-size window nil t)
      ;; Selecting inside the protected phase prevents
      ;; `buffer-list-update-hook' from observing a newly created popup as an
      ;; already-unfocused temporary window.
      (select-window window)
      window)))

(defun my/portal-popup-display-buffer (buffer)
  "Display BUFFER using the shared popup portal window logic."
  (unless (buffer-live-p buffer)
    (user-error "Dead buffer: %s" buffer))
  (with-current-buffer buffer
    (unless my/portal-popup-instance-p
      (my/portal-popup--save-ui))
    (setq-local my/portal-popup-instance-p t)
    (unless (or (derived-mode-p 'portal-terminal-mode)
                (derived-mode-p 'agent-shell-mode))
      (setq-local my/portal-popup-kind 'buffer))
    (unless my/portal-popup-workspace-id
      (setq-local my/portal-popup-workspace-id
                  (my/portal-workspace-id default-directory)))
    (unless (boundp 'my/portal-popup-fixed)
      (setq-local my/portal-popup-fixed nil))
    (when (and (boundp 'remote-terminal-instance)
               (remote-terminal-p remote-terminal-instance))
      (remote-terminal-put-metadata
       remote-terminal-instance :popup t))
    (when (derived-mode-p 'agent-shell-mode)
      (when (eq my/portal-popup-kind 'terminal)
        (let ((agent (and (boundp 'noema-agent-acp-session-agent)
                          noema-agent-acp-session-agent)))
          (setq-local my/portal-popup-kind
                      (if agent (intern (format "ai-%s" agent)) 'agent)
                      my/portal-popup-title
                      (and agent (capitalize (format "%s" agent))))))
      (when (fboundp 'noema-agent-acp-tabs-mode)
        (noema-agent-acp-tabs-mode -1))
      (setq-local noema-agent-acp-display-buffer-function
                  #'my/portal-popup-display-buffer)
      (my/portal-popup-agent-keys-mode 1))
    (my/portal-popup-apply-ui buffer)
    (add-hook 'kill-buffer-hook #'my/portal-popup--on-kill 95 t))
  (unless (memq buffer (my/portal-popup--live-buffers))
    (setq my/portal-popup-buffers
          (append (my/portal-popup--live-buffers) (list buffer))))
  (select-window (my/portal-popup--show-buffer buffer))
  buffer)

(defun my/portal-popup--normal-windows ()
  "Return ordinary windows on the selected frame."
  (seq-remove (lambda (window)
                (or (window-minibuffer-p window)
                    (window-parameter window 'my-portal-popup)))
              (window-list (selected-frame) 'nomini)))

(defun my/portal-popup--choose-normal-window (prompt)
  "Choose a non-popup window with PROMPT, using ace-window when needed."
  (let ((windows (my/portal-popup--normal-windows)))
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

(defun my/portal-popup-move-buffer-in ()
  "Move the current ordinary buffer into the shared popup.
From a popup tab, use ace-window to choose the ordinary source window."
  (interactive)
  (let* ((my/portal-popup--displaying t)
         (source (if (window-parameter (selected-window) 'my-portal-popup)
                     (my/portal-popup--choose-normal-window
                      "Move buffer into popup")
                   (selected-window)))
         (buffer (window-buffer source)))
    (when (window-dedicated-p source)
      (user-error "Cannot move a buffer from a dedicated window"))
    (with-selected-window source
      (switch-to-prev-buffer source 'bury))
    (my/portal-popup-display-buffer buffer)))

(defun my/portal-popup--release-buffer (buffer target)
  "Remove BUFFER from the popup pool and send future displays to TARGET."
  (with-current-buffer buffer
    (setq-local my/portal-popup-instance-p nil)
    (remove-hook 'kill-buffer-hook #'my/portal-popup--on-kill t)
    (my/portal-popup--restore-ui)
    (when (and (boundp 'remote-terminal-instance)
               (remote-terminal-p remote-terminal-instance))
      (remote-terminal-put-metadata remote-terminal-instance :popup nil))
    (when (derived-mode-p 'agent-shell-mode)
      (my/portal-popup-agent-keys-mode -1)
      (when (equal tab-line-format '(:eval (noema-agent-acp--tab-line)))
        (kill-local-variable 'tab-line-format))
      (setq-local noema-agent-acp-display-buffer-function
                  (lambda (session)
                    (when (window-live-p target)
                      (select-window target))
                    (switch-to-buffer session)))))
  (setq my/portal-popup-buffers (delq buffer my/portal-popup-buffers)))

(defun my/portal-popup-move-buffer-out ()
  "Move the current popup buffer into an ordinary window chosen with ace-window."
  (interactive)
  (let* ((popup (my/portal-popup--window))
         (buffer (if popup (window-buffer popup)
                   (my/portal-popup--current-buffer))))
    (unless (my/portal-popup--buffer-p buffer)
      (user-error "No popup buffer is available"))
    (let* ((my/portal-popup--displaying t)
           (target (my/portal-popup--choose-normal-window
                    "Move popup buffer to window"))
           (next (seq-find (lambda (entry) (not (eq entry buffer)))
                           (my/portal-popup--live-buffers))))
      (my/portal-popup--release-buffer buffer target)
      (when (eq my/portal-popup-current-buffer buffer)
        (setq my/portal-popup-current-buffer next))
      (set-window-buffer target buffer)
      (when (and popup (eq (window-buffer popup) buffer))
        (if next
            (progn
              (set-window-buffer popup next)
              (set-window-parameter
               popup 'my-portal-fixed
               (buffer-local-value 'my/portal-popup-fixed next)))
          (my/portal-hide-popup)))
      (select-window target)
      (let ((my/portal-popup--displaying nil))
        (my/portal-popup--auto-hide))
      buffer)))

(defun my/portal-popup--on-kill ()
  "Clean up popup portal state when the current buffer is killed."
  (let* ((buffer (current-buffer))
         (buffers (my/portal-popup--live-buffers))
         (tail (member buffer buffers))
         (next (or (cadr tail)
                   (seq-find (lambda (entry) (not (eq entry buffer))) buffers)))
         (windows (my/portal-popup--windows buffer)))
    (dolist (window windows)
      (my/portal-popup--record-window-height window))
    (setq my/portal-popup-buffers (delq buffer my/portal-popup-buffers))
    (when (eq my/portal-popup-current-buffer buffer)
      (setq my/portal-popup-current-buffer
            (and next
                 (not (eq next buffer))
                 next)))
    (dolist (window windows)
      (when (window-live-p window)
        (ignore-errors (delete-window window))))))

(defun my/portal-popup--create-buffer (&optional directory buffer-name)
  "Create and register a new popup portal buffer.
Use DIRECTORY as the initial terminal directory when non-nil.
Use BUFFER-NAME when non-nil."
  (let* ((default-directory (or (my/terminal-normalize-directory directory)
                                default-directory))
         (target-name (generate-new-buffer-name
                       (or buffer-name
                           (my/portal-popup--next-buffer-name)))))
    (with-current-buffer (my/portal-create :directory default-directory
                                           :name target-name :display nil)
      (my/portal-popup--save-ui)
      (setq-local my/portal-popup-instance-p t)
      (setq-local my/portal-popup-fixed nil)
      (setq-local
       my/portal-popup-workspace-id
       (if (and (boundp 'remote-terminal-instance)
                remote-terminal-instance)
           (remote-terminal-workspace-id remote-terminal-instance)
         (my/portal-workspace-id default-directory)))
      (when (and (boundp 'remote-terminal-instance)
                 (remote-terminal-p remote-terminal-instance))
        (remote-terminal-put-metadata
         remote-terminal-instance :popup t))
      (my/portal-popup-apply-ui (current-buffer))
      (add-hook 'kill-buffer-hook #'my/portal-popup--on-kill 95 t)
      (setq my/portal-popup-buffers
            (append (my/portal-popup--live-buffers) (list (current-buffer))))
      (setq my/portal-popup-current-buffer (current-buffer))
      (current-buffer))))

(defun my/portal-popup--project-root ()
  "Return the current project root.
Signal a user error when outside a project."
  (with-current-buffer (my/portal-popup--context-buffer)
    (or (and (fboundp 'my/project-current-root)
             (my/project-current-root))
        (when (fboundp 'project-current)
          (when-let* ((project (project-current nil default-directory)))
            (project-root project)))
        (user-error "Not inside a project"))))

(defun my/portal-popup--project-name (project-root)
  "Return a short project name for PROJECT-ROOT."
  (file-name-nondirectory
   (directory-file-name
    (file-name-as-directory
     (expand-file-name project-root)))))

(defun my/portal-popup-app (app &optional directory)
  "Run configured terminal APP in a fresh popup at DIRECTORY or here."
  (interactive
   (list
    (completing-read "Project terminal app: "
                     (mapcar #'car my/project-popup-portal-apps)
                     nil t)))
  (let* ((project-root (file-name-as-directory
                        (expand-file-name
                         (or directory
                             (buffer-local-value
                              'default-directory
                              (my/portal-popup--context-buffer))))))
         (command (or (cdr (assoc app my/project-popup-portal-apps))
                      (user-error "Unknown project terminal app: %s" app)))
         (buffer-name (format "*portal-popup:%s:%s*"
                              app
                              (my/portal-popup--project-name project-root)))
         (buffer (my/portal-create :directory project-root
                                   :name buffer-name :command command
                                   :display nil)))
    (my/portal-popup-display-buffer buffer)
    (select-window (my/portal-popup--show-buffer buffer))
    buffer))

(defun my/project-popup-portal-app (app)
  "Run APP in a fresh popup portal rooted at the current project."
  (interactive (list (completing-read "Project terminal app: "
                                     (mapcar #'car my/project-popup-portal-apps) nil t)))
  (my/portal-popup-app app (my/portal-popup--project-root)))

(defun my/portal-popup--next-buffer ()
  "Return the next popup portal buffer in creation order."
  (let* ((buffers (my/portal-popup--live-buffers))
         (current (my/portal-popup--current-buffer)))
    (cond
     ((null buffers)
      nil)
     ((null current)
      (car buffers))
     (t
      (or (cadr (member current buffers))
          (car buffers))))))

(defun my/portal-popup--read-buffer (prompt)
  "Read a popup portal buffer with PROMPT."
  (let* ((buffers (my/portal-popup--live-buffers))
         (current (my/portal-popup--current-buffer))
         (default (and current (buffer-name current))))
    (unless buffers
      (user-error "No popup portal buffers"))
    (get-buffer
     (completing-read prompt
                      (mapcar #'buffer-name buffers)
                      nil t nil nil default))))

(defun my/portal-hide-popup ()
  "Hide the current popup portal window."
  (interactive)
  (when-let* ((window (my/portal-popup--window)))
    (my/portal-popup--record-window-height window)
    (ignore-errors (delete-window window))))

(defun my/portal-show-popup ()
  "Show the current popup portal buffer."
  (interactive)
  (my/portal-popup--show-buffer (my/portal-popup--current-buffer t)))

(defun my/portal-popup-cycle (arg)
  "Cycle popup portal buffers.
With prefix ARG, create a new popup portal and switch to it."
  (interactive "P")
  (let ((buffer (if arg
                    (my/portal-popup--create-buffer
                     (buffer-local-value
                      'default-directory (my/portal-popup--context-buffer)))
                  (or (my/portal-popup--next-buffer)
                      (my/portal-popup--create-buffer
                       (buffer-local-value
                        'default-directory (my/portal-popup--context-buffer)))))))
    (select-window (my/portal-popup--show-buffer buffer))
    buffer))

(defun my/portal-popup-kill (buffer)
  "Kill popup portal BUFFER."
  (interactive
   (list (if current-prefix-arg
             (my/portal-popup--read-buffer "Kill popup portal: ")
           (or (my/portal-popup--current-buffer)
               (user-error "No popup portal buffers")))))
  (kill-buffer buffer))

(defun my/portal-popup-rename (buffer name)
  "Rename popup portal BUFFER to NAME."
  (interactive
   (let* ((buffer (if current-prefix-arg
                      (my/portal-popup--read-buffer "Rename popup portal: ")
                    (or (my/portal-popup--current-buffer)
                        (user-error "No popup portal buffers"))))
          (current-name (buffer-name buffer)))
     (list buffer
           (read-string "New popup portal name: " current-name nil current-name))))
  (when-let* ((existing (get-buffer name)))
    (unless (eq existing buffer)
      (user-error "Buffer %s already exists" name)))
  (with-current-buffer buffer
    (rename-buffer name))
  buffer)

(defun my/portal-toggle-fixed ()
  "Toggle fixed mode for the current popup portal buffer."
  (interactive)
  (let* ((buffer (my/portal-popup--current-buffer t))
         (fixed (with-current-buffer buffer
                  (setq-local my/portal-popup-fixed (not my/portal-popup-fixed))
                  my/portal-popup-fixed))
         (window (my/portal-popup--show-buffer buffer)))
    (set-window-parameter window 'my-portal-fixed fixed)
    (select-window window)
    (message "Popup portal %s %s"
             (buffer-name buffer)
             (if fixed "fixed" "temporary"))))

(defun my/portal-popup--auto-hide (&rest _)
  "Hide the popup portal window when a temporary instance loses focus."
  (unless my/portal-popup--displaying
    (when-let* (((or my/portal-popup-current-buffer
                   my/portal-popup-buffers))
                (window (my/portal-popup--window)))
      (let ((buffer (window-buffer window)))
        (unless (or (active-minibuffer-window)
                    (eq (selected-window) window)
                    (buffer-local-value 'my/portal-popup-fixed buffer))
          (my/portal-hide-popup))))))

(defun my/portal-popup-toggle ()
  "Toggle the current popup portal buffer."
  (interactive)
  (let* ((buffer (my/portal-popup--current-buffer t))
         (window (my/portal-popup--window)))
    (cond
     ((and window
           (eq (window-buffer window) buffer)
           (eq (selected-window) window))
      (my/portal-hide-popup))
     ((and window
           (eq (window-buffer window) buffer))
      (select-window window))
     (t
      (select-window (my/portal-popup--show-buffer buffer))))))

(global-set-key (kbd "M-`") #'my/portal-popup-toggle)
(global-set-key (kbd "C-c e") #'my/portal-popup-toggle)
(global-set-key (kbd "C-c C-e") #'my/portal-popup-toggle)
(global-set-key (kbd "C-c E") #'my/portal-popup-cycle)
(global-set-key (kbd "C-c M-E") #'my/portal-popup-new)
(global-set-key (kbd "C-c M-e") #'my/portal-toggle-fixed)
(global-set-key (kbd "C-c w p") #'my/portal-popup-move-buffer-in)
(global-set-key (kbd "C-c w P") #'my/portal-popup-move-buffer-out)

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/portal-popup-last-height))

(add-hook 'window-selection-change-functions #'my/portal-popup--auto-hide)
(add-hook 'window-selection-change-functions #'my/portal-popup--remember-origin)
(add-hook 'buffer-list-update-hook #'my/portal-popup--auto-hide)

;; Reloading this library also updates agents already in the popup pool.
(dolist (buffer (my/portal-popup--live-buffers))
  (when (with-current-buffer buffer (derived-mode-p 'agent-shell-mode))
    (my/portal-popup-apply-ui buffer)))

(provide 'init-portal-popup)
;;; init-portal-popup.el ends here

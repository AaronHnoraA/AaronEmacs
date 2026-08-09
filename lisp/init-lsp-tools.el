;;; init-lsp-tools.el --- Language server dashboard and diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Display, maintenance, and debugging helpers for the language-server stack.
;; This mirrors the split used by the Jupyter workflow: keep the core routing in
;; `init-lsp.el', keep the backend-agnostic operations in `init-lsp-ops.el',
;; and put the display / doctor / dispatch surface here.

;;; Code:

(require 'aaron-ui-board)
(require 'cl-lib)
(require 'easymenu)
(require 'init-funcs)
(require 'pp)
(require 'subr-x)
(require 'transient)

(defconst my/language-server-tools--module-file
  (or load-file-name buffer-file-name)
  "Absolute path of `init-lsp-tools.el' at load time.")

(defconst my/language-server-manager-buffer-name "*Language Server Hub*"
  "Buffer name used by the language-server dashboard.")

(defconst my/language-server-doctor-buffer-name "*Language Server Doctor*"
  "Buffer name used by the language-server doctor report.")

(defconst my/language-server-doc-file
  (expand-file-name
   "../docs/lsp-workflow.org"
   (file-name-directory my/language-server-tools--module-file))
  "Primary language-server workflow document.")

(defvar my/language-server-manager-extra-section-functions nil
  "Functions used to append extra sections to the language-server hub.")

(defvar my/language-server-manager-setup-functions nil
  "Functions run after the language-server hub keymap is initialized.")

(defvar-local my/language-server-manager-source-buffer nil
  "Source buffer used by the current language-server view.")

(defvar my/language-server-manager-entry-mouse-map nil
  "Mouse keymap installed on row entries in language-server views.")

(defvar company-mode)
(defvar breadcrumb-local-mode)
(defvar eglot-autoreconnect)
(defvar eglot-autoshutdown)
(defvar eglot-events-buffer-size)
(defvar eglot-workspace-configuration)
(defvar flymake-mode)
(defvar flymake-no-changes-timeout)
(defvar lsp-inlay-hint-enable)
(defvar lsp-log-io)

(declare-function find-file "files" (filename &optional wildcards))
(declare-function locate-library "find-func" (library &optional nosuffix path interactive-call))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function my/current-language-server-backend "init-lsp")
(declare-function my/language-server-code-actions "init-lsp")
(declare-function my/language-server-eglot-program-entries "init-lsp")
(declare-function my/language-server-ensure "init-lsp")
(declare-function my/language-server-format-buffer "init-lsp")
(declare-function my/language-server-lsp-mode-preference-entries "init-lsp")
(declare-function my/language-server-project-backend-override "init-lsp")
(declare-function my/language-server-rename "init-lsp")
(declare-function my/language-server-organize-imports "init-lsp-ops")
(declare-function my/language-server-restart "init-lsp-ops")
(declare-function my/language-server-shutdown "init-lsp-ops")
(declare-function my/language-server-open-log "init-lsp-ops")
(declare-function my/language-server-describe-session "init-lsp-ops")
(declare-function my/language-server-show-workspace-configuration "init-lsp-ops")
(declare-function my/language-server-current-toolchain-profile "init-lsp-toolchain" (&optional buffer))
(declare-function my/language-server-toolchain-description "init-lsp-toolchain" (&optional buffer))
(declare-function my/language-server-select-toolchain "init-lsp-toolchain")
(declare-function my/language-server-reset-toolchain "init-lsp-toolchain")
(declare-function my/language-server-refresh-toolchains "init-lsp-toolchain")
(declare-function my/problems-buffer "init-problems")
(declare-function my/problems-project "init-problems")
(declare-function my/diagnostics-buffer-ui "init-diagnostics-ui")
(declare-function my/diagnostics-project-ui "init-diagnostics-ui")
(declare-function my/diagnostics-dispatch "init-diagnostics-extra")
(declare-function eglot-inlay-hints-mode "eglot" (&optional arg))
(declare-function lsp-inlay-hints-mode "lsp-mode" (&optional arg))

(define-derived-mode my/language-server-manager-mode aaron-ui-board-mode "Lang-Server-Hub"
  "Major mode for the language-server dashboard.")

(define-derived-mode my/language-server-doctor-mode aaron-ui-board-mode "Lang-Server-Doctor"
  "Major mode for the language-server doctor report.")

(defun my/language-server-manager--assert-view-buffer ()
  "Signal unless the current buffer is the language-server Hub."
  (unless (derived-mode-p 'my/language-server-manager-mode)
    (user-error "Refusing to render Language Server Hub into %s" (buffer-name))))

(defun my/language-server-doctor--assert-view-buffer ()
  "Signal unless the current buffer is the language-server Doctor."
  (unless (derived-mode-p 'my/language-server-doctor-mode)
    (user-error "Refusing to render Language Server Doctor into %s" (buffer-name))))

(defun my/language-server--source-buffer ()
  "Return the source buffer associated with the current view."
  (if (and (boundp 'my/language-server-manager-source-buffer)
           (buffer-live-p my/language-server-manager-source-buffer))
      my/language-server-manager-source-buffer
    (current-buffer)))

(defun my/language-server--source-buffer-or-error ()
  "Return the current source buffer or signal a user error."
  (let ((buffer (my/language-server--source-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "No live source buffer is associated with this view"))
    buffer))

(defun my/language-server--clear-source-buffer-references-h ()
  "Clear manager views that point at the buffer being killed."
  (let ((source (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (or (derived-mode-p 'my/language-server-manager-mode)
                         (derived-mode-p 'my/language-server-doctor-mode))
                     (eq my/language-server-manager-source-buffer source))
            (setq-local my/language-server-manager-source-buffer nil)))))))

(defun my/language-server--watch-source-buffer (source)
  "Install source cleanup for language-server manager views."
  (when (buffer-live-p source)
    (with-current-buffer source
      (add-hook 'kill-buffer-hook
                #'my/language-server--clear-source-buffer-references-h nil t))))

(defun my/language-server--view-refresh (buffer)
  "Refresh language-server BUFFER when it is a managed view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'my/language-server-manager-mode)
        (my/language-server-manager-refresh))
       ((derived-mode-p 'my/language-server-doctor-mode)
        (my/language-server-doctor-refresh))))))

(defun my/language-server--call-in-source-buffer (command &optional refresh)
  "Call COMMAND in the associated source buffer.
When REFRESH is non-nil, refresh the current hub/doctor view afterwards."
  (let ((source (my/language-server--source-buffer-or-error))
        (view (current-buffer)))
    (with-current-buffer source
      (call-interactively command))
    (when refresh
      (my/language-server--view-refresh view))))

(defun my/language-server--mode-list (modes)
  "Normalize MODES into a list of major-mode symbols."
  (cond
   ((null modes) nil)
   ((symbolp modes) (list modes))
   ((listp modes) modes)
   (t nil)))

(defun my/language-server--format-mode-list (modes)
  "Format MODES as a comma-separated string."
  (string-join (mapcar #'symbol-name (my/language-server--mode-list modes)) ", "))

(defun my/language-server--library-path (library)
  "Return the path of LIBRARY when it can be located."
  (ignore-errors
    (locate-library library)))

(defun my/language-server--feature-library-path (feature)
  "Return FEATURE's loadable library path when possible."
  (when feature
    (my/language-server--library-path (symbol-name feature))))

(defun my/language-server--feature-status (feature)
  "Return a short status string for FEATURE."
  (cond
   ((null feature) "ready")
   ((featurep feature) "loaded")
   ((my/language-server--feature-library-path feature) "available")
   (t "missing")))

(defun my/language-server--project-root (&optional buffer)
  "Return the project root for BUFFER, or nil when unavailable."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((project (project-current nil default-directory)))
      (expand-file-name (project-root project)))))

(defun my/language-server--current-lsp-preference-entry (&optional buffer)
  "Return the explicit `lsp-mode' route matching BUFFER, if any."
  (with-current-buffer (or buffer (current-buffer))
    (cl-find-if
     (lambda (entry)
       (derived-mode-p (plist-get entry :mode)))
     (my/language-server-lsp-mode-preference-entries))))

(defun my/language-server--eglot-entry-matches-buffer-p (entry &optional buffer)
  "Return non-nil when ENTRY matches BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((modes (my/language-server--mode-list (plist-get entry :modes))))
      (and modes
           (apply #'derived-mode-p modes)))))

(defun my/language-server--current-eglot-entry (&optional buffer)
  "Return the custom Eglot mapping matching BUFFER, if any."
  (with-current-buffer (or buffer (current-buffer))
    (cl-find-if #'my/language-server--eglot-entry-matches-buffer-p
                (my/language-server-eglot-program-entries))))

(defun my/language-server--current-policy (&optional buffer)
  "Return a short description of the expected backend policy for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((active (and (fboundp 'my/current-language-server-backend)
                       (my/current-language-server-backend)))
          (override (and (fboundp 'my/language-server-project-backend-override)
                         (my/language-server-project-backend-override))))
      (cond
       ((eq override 'disabled)
        "disabled (project-local override)")
       ((eq override 'lsp-mode)
        (if (eq active 'lsp-mode)
            "lsp-mode (active project-local override)"
          "lsp-mode (project-local override)"))
       ((eq override 'eglot)
        (if (eq active 'eglot)
            "eglot (active project-local override)"
          "eglot (project-local override)"))
       ((my/language-server--current-lsp-preference-entry)
        (if (eq active 'lsp-mode)
            "lsp-mode (active explicit route)"
          "lsp-mode (explicit route)"))
       ((my/language-server--current-eglot-entry)
        (if (eq active 'eglot)
            "eglot (active custom route)"
          "eglot (custom route)"))
       ((eq active 'eglot) "eglot (active)")
       ((derived-mode-p 'prog-mode) "eglot (prog-mode default)")
       (t "manual / unknown")))))

(defun my/language-server--source-status (&optional source)
  "Collect display status for SOURCE without inserting into that buffer."
  (let ((source (or source (my/language-server--source-buffer))))
    (when (buffer-live-p source)
      (with-current-buffer source
        (let* ((lsp-entry (my/language-server--current-lsp-preference-entry source))
               (eglot-entry (my/language-server--current-eglot-entry source))
               (feature (and lsp-entry (plist-get lsp-entry :feature)))
               (active (and (fboundp 'my/current-language-server-backend)
                            (my/current-language-server-backend)))
               (runtime (and (boundp 'my/language-server-runtime-current)
                             my/language-server-runtime-current))
               (project-root (my/language-server--project-root source))
               (workspace-set (local-variable-p 'eglot-workspace-configuration source))
               (workspace (and workspace-set eglot-workspace-configuration)))
          (list :buffer (buffer-name source)
                :file (or (and buffer-file-name
                               (abbreviate-file-name buffer-file-name))
                          "-")
                :major-mode major-mode
                :default-directory (abbreviate-file-name default-directory)
                :project-root (if project-root
                                  (abbreviate-file-name project-root)
                                "-")
                :policy (my/language-server--current-policy source)
                :active-backend (or active "-")
                :required-feature (or feature "-")
                :feature-status (my/language-server--feature-status feature)
                :eglot-match (if eglot-entry
                                  (or (plist-get eglot-entry :label)
                                      (my/language-server--format-mode-list
                                       (plist-get eglot-entry :modes)))
                                "-")
                :flymake (if (bound-and-true-p flymake-mode) "on" "off")
                :company (if (bound-and-true-p company-mode) "on" "off")
                :breadcrumb (if (bound-and-true-p breadcrumb-local-mode) "on" "off")
                :workspace-set workspace-set
                :toolchain (and (fboundp 'my/language-server-toolchain-description)
                                (my/language-server-toolchain-description source))
                :toolchain-id
                (and (fboundp 'my/language-server-current-toolchain-profile)
                     (plist-get (my/language-server-current-toolchain-profile source) :id))
                :runtime-state
                (if (boundp 'my/language-server-runtime-state)
                    my/language-server-runtime-state
                  'unavailable)
                :runtime
                (and (fboundp 'my/language-server-runtime-description)
                     (my/language-server-runtime-description source))
                :runtime-id
                (and runtime (my/language-server-runtime-id runtime))
                :runtime-provider
                (and runtime (my/language-server-runtime-provider runtime))
                :runtime-target
                (and runtime
                     (plist-get (my/language-server-runtime-metadata runtime)
                                :target))
                :runtime-fallback
                (and (boundp 'my/language-server-runtime-error)
                     my/language-server-runtime-error)
                :workspace (and workspace
                                (string-trim-right
                                 (pp-to-string workspace)))))))))

(defun my/language-server--openable-path (path)
  "Return PATH abbreviated for display."
  (abbreviate-file-name (expand-file-name path)))

(defun my/language-server--insert-button (label action help)
  "Insert a text button with LABEL, ACTION, and HELP."
  (insert-text-button
   label
   'action action
   'follow-link t
   'help-echo help))

(defun my/language-server--insert-openable-path (path)
  "Insert PATH as a button that opens it."
  (aaron-ui-board-insert-openable-path path (my/language-server--openable-path path)))

(defun my/language-server--entry-source (entry)
  "Return ENTRY's source file path, if any."
  (plist-get entry :source))

(defun my/language-server-manager--current-entry ()
  "Return the manager entry at point, if any."
  (or (get-text-property (point) 'my/language-server-entry)
      (get-text-property (line-beginning-position) 'my/language-server-entry)))

(defun my/language-server-manager--set-entry-properties (start end entry)
  "Mark the region between START and END with manager ENTRY."
  (add-text-properties start end
                       `(my/language-server-entry ,entry
                                                  local-map ,my/language-server-manager-entry-mouse-map
                                                  keymap ,my/language-server-manager-entry-mouse-map
                                                  mouse-face highlight
                                                  help-echo "RET/mouse-1: context action, mouse-3: menu")))

(defun my/language-server-manager-open-source ()
  "Open the source file referenced by the entry at point."
  (interactive)
  (let* ((entry (my/language-server-manager--current-entry))
         (source (and entry (my/language-server--entry-source entry))))
    (unless source
      (user-error "No source file is attached to the entry at point"))
    (find-file source)))

(defun my/language-server-manager-context-action ()
  "Run the default action for the entry at point."
  (interactive)
  (if-let* ((entry (my/language-server-manager--current-entry))
            (source (my/language-server--entry-source entry)))
      (find-file source)
    (my/language-server-manager-ensure)))

(defun my/language-server-manager--mouse-set-point (event)
  "Move point to the language-server view row clicked by EVENT."
  (let* ((posn (and event (event-start event)))
         (window (and posn (posn-window posn)))
         (point (and posn (posn-point posn))))
    (when (window-live-p window)
      (select-window window))
    (when (integer-or-marker-p point)
      (goto-char point))))

(defun my/language-server-manager-entry-mouse-action (event)
  "Run the default row action for the language-server entry under EVENT."
  (interactive "e")
  (my/language-server-manager--mouse-set-point event)
  (my/language-server-manager-context-action))

(defun my/language-server-manager--menu-item (label command &optional enabled)
  "Return a popup menu item for LABEL and COMMAND."
  (vector label command (and (fboundp command) enabled)))

(defun my/language-server-manager--entry-menu-items (entry)
  "Return right-click menu items for language-server ENTRY."
  (let ((source (and entry (my/language-server--entry-source entry))))
    (list
     (my/language-server-manager--menu-item
      "Context action" 'my/language-server-manager-context-action t)
     (my/language-server-manager--menu-item
      "Open entry source" 'my/language-server-manager-open-source source)
     (list
      "Current Buffer"
      (my/language-server-manager--menu-item
       "Ensure / connect" 'my/language-server-manager-ensure t)
      (my/language-server-manager--menu-item
       "Restart" 'my/language-server-manager-restart t)
      (my/language-server-manager--menu-item
       "Shutdown" 'my/language-server-manager-shutdown t)
      (my/language-server-manager--menu-item
       "Open log" 'my/language-server-manager-open-log t)
      (my/language-server-manager--menu-item
       "Describe session" 'my/language-server-manager-describe-session t)
      (my/language-server-manager--menu-item
       "Workspace config" 'my/language-server-manager-show-workspace-configuration t))
     (list
      "Edits / Diagnostics"
      (my/language-server-manager--menu-item
       "Code actions" 'my/language-server-manager-code-actions t)
      (my/language-server-manager--menu-item
       "Organize imports" 'my/language-server-manager-organize-imports t)
      (my/language-server-manager--menu-item
       "Format buffer" 'my/language-server-manager-format-buffer t)
      (my/language-server-manager--menu-item
       "Rename symbol" 'my/language-server-manager-rename t)
      (my/language-server-manager--menu-item
       "Buffer problems" 'my/language-server-manager-problems-buffer t)
      (my/language-server-manager--menu-item
       "Project problems" 'my/language-server-manager-problems-project t))
     (list
      "Views"
      (my/language-server-manager--menu-item
       "Refresh" 'my/language-server-manager-refresh t)
      (my/language-server-manager--menu-item
       "Hub" 'my/language-server-manager t)
      (my/language-server-manager--menu-item
       "Doctor" 'my/language-server-doctor t)
      (my/language-server-manager--menu-item
       "Dispatch" 'my/language-server-dispatch t)
      (my/language-server-manager--menu-item
       "Docs" 'my/language-server-manager-open-docs t)))))

(defun my/language-server-manager--view-menu-items ()
  "Return right-click menu items for language-server Hub/Doctor views."
  (let ((refresh-command
         (if (derived-mode-p 'my/language-server-doctor-mode)
             'my/language-server-doctor-refresh
           'my/language-server-manager-refresh)))
    (list
     (list
      "Views"
      (my/language-server-manager--menu-item
       "Refresh" refresh-command t)
      (my/language-server-manager--menu-item
       "Hub" 'my/language-server-manager t)
      (my/language-server-manager--menu-item
       "Doctor" 'my/language-server-doctor t)
      (my/language-server-manager--menu-item
       "Dispatch" 'my/language-server-dispatch t)
      (my/language-server-manager--menu-item
       "Docs" 'my/language-server-manager-open-docs t))
     (list
      "Lifecycle"
      (my/language-server-manager--menu-item
       "Ensure / connect" 'my/language-server-manager-ensure t)
      (my/language-server-manager--menu-item
       "Restart" 'my/language-server-manager-restart t)
      (my/language-server-manager--menu-item
       "Shutdown" 'my/language-server-manager-shutdown t)
      (my/language-server-manager--menu-item
       "Open log" 'my/language-server-manager-open-log t)
      (my/language-server-manager--menu-item
       "Describe session" 'my/language-server-manager-describe-session t))
     (list
      "Edits"
      (my/language-server-manager--menu-item
       "Code actions" 'my/language-server-manager-code-actions t)
      (my/language-server-manager--menu-item
       "Organize imports" 'my/language-server-manager-organize-imports t)
      (my/language-server-manager--menu-item
       "Format buffer" 'my/language-server-manager-format-buffer t)
      (my/language-server-manager--menu-item
       "Rename symbol" 'my/language-server-manager-rename t))
     (list
      "Diagnostics"
      (my/language-server-manager--menu-item
       "Buffer problems" 'my/language-server-manager-problems-buffer t)
      (my/language-server-manager--menu-item
       "Project problems" 'my/language-server-manager-problems-project t)
      (my/language-server-manager--menu-item
       "Buffer diagnostics UI" 'my/language-server-manager-diagnostics-buffer-ui t)
      (my/language-server-manager--menu-item
       "Project diagnostics UI" 'my/language-server-manager-diagnostics-project-ui t)
      (my/language-server-manager--menu-item
       "Diagnostics menu" 'my/language-server-manager-diagnostics-menu t)))))

(defun my/language-server-manager-popup-menu (event)
  "Show a right-click menu for language-server Hub/Doctor EVENT."
  (interactive "e")
  (my/language-server-manager--mouse-set-point event)
  (let* ((entry (my/language-server-manager--current-entry))
         (title (if entry "Language Server Entry" "Language Server"))
         (items (if entry
                    (my/language-server-manager--entry-menu-items entry)
                  (my/language-server-manager--view-menu-items))))
    (popup-menu (easy-menu-create-menu title items) event)))

(setq my/language-server-manager-entry-mouse-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] #'my/language-server-manager-entry-mouse-action)
        (define-key map [mouse-3] #'my/language-server-manager-popup-menu)
        map))

(defun my/language-server-manager-open-docs ()
  "Open the dedicated language-server workflow document."
  (interactive)
  (find-file my/language-server-doc-file))

(defun my/language-server--executable-report (name)
  "Return a plist describing executable NAME."
  (let ((path (executable-find name)))
    (list :name name
          :path path
          :ok (and path t))))

(defun my/language-server--executable-summary (names)
  "Return a compact summary string for executable NAMES."
  (if names
      (string-join
       (mapcar
        (lambda (name)
          (format "%s=%s"
                  name
                  (if (executable-find name) "ok" "missing")))
        names)
       ", ")
    "-"))

(defun my/language-server--value-or-unset (symbol)
  "Return SYMBOL's value, or the string \"unset\" when it is unbound."
  (if (boundp symbol)
      (symbol-value symbol)
    "unset"))

(defun my/language-server--enabled-value (symbol)
  "Return non-nil when SYMBOL is bound and its value is non-nil."
  (and (boundp symbol)
       (symbol-value symbol)))

(defun my/language-server--toggle-default (symbol)
  "Toggle SYMBOL's default value and return the new value."
  (let ((value (not (my/language-server--enabled-value symbol))))
    (set-default symbol value)
    value))

(defun my/language-server--read-number-choice (name symbol choices fallback)
  "Read a numeric value for SYMBOL named NAME from CHOICES."
  (let* ((current (my/language-server--value-or-unset symbol))
         (default (if (numberp current)
                      (format "%s" current)
                    fallback)))
    (string-to-number
     (completing-read
      (format "%s (current %s): " name current)
      choices nil t nil nil default))))

(defun my/language-server--runtime-knob-entries ()
  "Return important runtime knob/value pairs."
  `(("read-process-output-max" . ,(my/language-server--value-or-unset
                                   'read-process-output-max))
    ("eglot-autoshutdown" . ,(my/language-server--value-or-unset
                              'eglot-autoshutdown))
    ("eglot-autoreconnect" . ,(my/language-server--value-or-unset
                               'eglot-autoreconnect))
    ("eglot-events-buffer-size" . ,(my/language-server--value-or-unset
                                    'eglot-events-buffer-size))
    ("lsp-log-io" . ,(my/language-server--value-or-unset 'lsp-log-io))
    ("lsp-inlay-hint-enable" . ,(my/language-server--value-or-unset
                                 'lsp-inlay-hint-enable))
    ("flymake-no-changes-timeout" . ,(my/language-server--value-or-unset
                                      'flymake-no-changes-timeout))))

(defun my/language-server--maybe-refresh-current-view ()
  "Refresh the current view when it is a hub or doctor buffer."
  (cond
   ((derived-mode-p 'my/language-server-manager-mode)
    (my/language-server-manager-refresh))
   ((derived-mode-p 'my/language-server-doctor-mode)
    (my/language-server-doctor-refresh))))

(defun my/language-server--sync-source-buffer-inlay-hints ()
  "Apply the current inlay-hint preference to the source buffer."
  (let ((source (my/language-server--source-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (pcase (and (fboundp 'my/current-language-server-backend)
                    (my/current-language-server-backend))
          ('eglot
           (when (fboundp 'eglot-inlay-hints-mode)
             (eglot-inlay-hints-mode
              (if (my/language-server--enabled-value 'lsp-inlay-hint-enable)
                  1
                -1))))
          ('lsp-mode
           (when (fboundp 'lsp-inlay-hints-mode)
             (lsp-inlay-hints-mode
              (if (my/language-server--enabled-value 'lsp-inlay-hint-enable)
                  1
                -1)))))))))

(defun my/language-server-toggle-eglot-autoreconnect ()
  "Toggle `eglot-autoreconnect' for the current Emacs session."
  (interactive)
  (message "eglot-autoreconnect: %s"
           (my/language-server--toggle-default 'eglot-autoreconnect))
  (my/language-server--maybe-refresh-current-view))

(defun my/language-server-toggle-eglot-autoshutdown ()
  "Toggle `eglot-autoshutdown' for the current Emacs session."
  (interactive)
  (message "eglot-autoshutdown: %s"
           (my/language-server--toggle-default 'eglot-autoshutdown))
  (my/language-server--maybe-refresh-current-view))

(defun my/language-server-toggle-lsp-log-io ()
  "Toggle `lsp-log-io' for the current Emacs session."
  (interactive)
  (message "lsp-log-io: %s"
           (my/language-server--toggle-default 'lsp-log-io))
  (my/language-server--maybe-refresh-current-view))

(defun my/language-server-toggle-inlay-hints ()
  "Toggle `lsp-inlay-hint-enable' and sync the source buffer."
  (interactive)
  (my/language-server--toggle-default 'lsp-inlay-hint-enable)
  (my/language-server--sync-source-buffer-inlay-hints)
  (message "lsp-inlay-hint-enable: %s" lsp-inlay-hint-enable)
  (my/language-server--maybe-refresh-current-view))

(defun my/language-server-set-read-process-output-max (value)
  "Set `read-process-output-max' to VALUE for the current session."
  (interactive
   (list
    (my/language-server--read-number-choice
     "read-process-output-max"
     'read-process-output-max
     '("65536" "262144" "524288" "1048576" "2097152")
     "1048576")))
  (setq-default read-process-output-max value)
  (message "read-process-output-max: %s" read-process-output-max)
  (my/language-server--maybe-refresh-current-view))

(defun my/language-server-set-eglot-events-buffer-size (value)
  "Set `eglot-events-buffer-size' to VALUE for the current session."
  (interactive
   (list
    (my/language-server--read-number-choice
     "eglot-events-buffer-size"
     'eglot-events-buffer-size
     '("0" "20000" "100000" "200000" "1000000")
     "200000")))
  (setq-default eglot-events-buffer-size value)
  (message "eglot-events-buffer-size: %s" eglot-events-buffer-size)
  (my/language-server--maybe-refresh-current-view))

(defun my/language-server-manager-ensure ()
  "Ensure the preferred language-server backend for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-ensure t))

(defun my/language-server-manager-restart ()
  "Restart the language server for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-restart t))

(defun my/language-server-manager-shutdown ()
  "Shutdown the language server for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-shutdown t))

(defun my/language-server-manager-open-log ()
  "Open the language-server log buffer for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-open-log))

(defun my/language-server-manager-describe-session ()
  "Describe the active language-server session for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-describe-session))

(defun my/language-server-manager-show-workspace-configuration ()
  "Show the active workspace configuration for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer
   #'my/language-server-show-workspace-configuration))

(defun my/language-server-manager-select-toolchain ()
  "Select a toolchain for the associated source project."
  (interactive)
  (my/language-server--call-in-source-buffer
   #'my/language-server-select-toolchain t))

(defun my/language-server-manager-reset-toolchain ()
  "Reset the associated source project's session toolchain."
  (interactive)
  (my/language-server--call-in-source-buffer
   #'my/language-server-reset-toolchain t))

(defun my/language-server-manager-refresh-toolchains ()
  "Refresh discovered toolchains for the associated source project."
  (interactive)
  (my/language-server--call-in-source-buffer
   #'my/language-server-refresh-toolchains t))

(defun my/language-server-manager-organize-imports ()
  "Organize imports for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-organize-imports t))

(defun my/language-server-manager-code-actions ()
  "Run code actions for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-code-actions t))

(defun my/language-server-manager-format-buffer ()
  "Format the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-format-buffer t))

(defun my/language-server-manager-rename ()
  "Rename the symbol at point in the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/language-server-rename t))

(defun my/language-server-manager-problems-buffer ()
  "Open the buffer-local problems view for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/problems-buffer))

(defun my/language-server-manager-problems-project ()
  "Open the project-wide problems view for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/problems-project))

(defun my/language-server-manager-diagnostics-buffer-ui ()
  "Open the diagnostics UI for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/diagnostics-buffer-ui))

(defun my/language-server-manager-diagnostics-project-ui ()
  "Open the project diagnostics UI for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/diagnostics-project-ui))

(defun my/language-server-manager-diagnostics-menu ()
  "Open the diagnostics dispatch menu for the source buffer."
  (interactive)
  (my/language-server--call-in-source-buffer #'my/diagnostics-dispatch))

(defun my/language-server-manager-insert-maintenance-section ()
  "Insert the top-level maintenance section."
  (my/language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Maintenance")
  (aaron-ui-board-insert-field
   "lsp-mode routes"
   (number-to-string (length (my/language-server-lsp-mode-preference-entries))))
  (aaron-ui-board-insert-field
   "eglot mappings"
   (number-to-string (length (my/language-server-eglot-program-entries))))
  (when (boundp 'my/language-server-toolchain-providers)
    (aaron-ui-board-insert-field
     "toolchain providers"
     (number-to-string (length my/language-server-toolchain-providers))))
  (insert "   ")
  (aaron-ui-board-insert-actions
   '((:label "Doctor"    :command my/language-server-doctor    :help "Doctor report")
     (:label "Dispatch"  :command my/language-server-dispatch  :help "Transient menu")
     (:label "Docs"      :command my/language-server-manager-open-docs :help "Workflow docs")
     (:label "Problems"  :command my/language-server-manager-problems-project :help "Project problems")))
  (insert "\n\n"))

(defun my/language-server-manager--insert-settings-section ()
  "Insert quick runtime settings for the language-server stack."
  (my/language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Quick Settings")
  (aaron-ui-board-insert-field
   "eglot-autoreconnect"
   (format "%s" (my/language-server--value-or-unset 'eglot-autoreconnect)))
  (aaron-ui-board-insert-field
   "eglot-autoshutdown"
   (format "%s" (my/language-server--value-or-unset 'eglot-autoshutdown)))
  (aaron-ui-board-insert-field
   "lsp-log-io"
   (format "%s" (my/language-server--value-or-unset 'lsp-log-io)))
  (aaron-ui-board-insert-field
   "lsp-inlay-hints"
   (format "%s" (my/language-server--value-or-unset 'lsp-inlay-hint-enable)))
  (aaron-ui-board-insert-field
   "read-process-output-max"
   (format "%s" (my/language-server--value-or-unset 'read-process-output-max)))
  (aaron-ui-board-insert-field
   "eglot-events-buffer"
   (format "%s" (my/language-server--value-or-unset 'eglot-events-buffer-size)))
  (insert "   ")
  (aaron-ui-board-insert-actions
   '((:label "Autoreconnect" :command my/language-server-toggle-eglot-autoreconnect
              :help "Toggle eglot autoreconnect")
     (:label "Autoshutdown"  :command my/language-server-toggle-eglot-autoshutdown
              :help "Toggle eglot autoshutdown")
     (:label "Log IO"        :command my/language-server-toggle-lsp-log-io
              :help "Toggle lsp-mode wire logging")
     (:label "Inlay Hints"   :command my/language-server-toggle-inlay-hints
              :help "Toggle inlay hints")))
  (insert " ")
  (aaron-ui-board-insert-action
   "Output Max" #'my/language-server-set-read-process-output-max "Set read-process-output-max")
  (insert " ")
  (aaron-ui-board-insert-action
   "Events Buf" #'my/language-server-set-eglot-events-buffer-size "Set eglot-events-buffer-size")
  (insert "\n\n"))

(defun my/language-server-manager--insert-current-buffer-section ()
  "Insert the current-buffer status section."
  (my/language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Current Buffer")
  (if-let* ((status (my/language-server--source-status)))
      (progn
        (aaron-ui-board-insert-field "buffer"      (plist-get status :buffer))
        (aaron-ui-board-insert-field "file"        (plist-get status :file))
        (aaron-ui-board-insert-field "major mode"  (format "%s" (plist-get status :major-mode)))
        (aaron-ui-board-insert-field "directory"   (plist-get status :default-directory))
        (aaron-ui-board-insert-field "project root" (plist-get status :project-root))
        (aaron-ui-board-insert-field "route policy" (plist-get status :policy))
        (aaron-ui-board-insert-field "active backend" (format "%s" (plist-get status :active-backend)))
        (aaron-ui-board-insert-field
         "toolchain"
         (format "%s%s"
                 (or (plist-get status :toolchain) "automatic / PATH")
                 (if-let* ((id (plist-get status :toolchain-id)))
                     (format "  [%s]" id)
                   "")))
        (aaron-ui-board-insert-field
         "runtime context"
         (format "%s  [state=%s%s%s]"
                 (or (plist-get status :runtime) "project default")
                 (plist-get status :runtime-state)
                 (if-let* ((provider (plist-get status :runtime-provider)))
                     (format ", provider=%s" provider) "")
                 (if-let* ((target (plist-get status :runtime-target)))
                     (format ", target=%s" target) "")))
        (when-let* ((fallback (plist-get status :runtime-fallback)))
          (aaron-ui-board-insert-field "runtime fallback" fallback
                                       'aaron-ui-board-warn))
        (aaron-ui-board-insert-field
         "lsp feature"
         (format "%s (%s)" (plist-get status :required-feature) (plist-get status :feature-status)))
        (aaron-ui-board-insert-field "eglot mapping" (plist-get status :eglot-match))
        (aaron-ui-board-insert-field
         "flymake/company"
         (format "%s / %s / %s"
                 (plist-get status :flymake)
                 (plist-get status :company)
                 (plist-get status :breadcrumb)))
        (aaron-ui-board-insert-field
         "workspace config"
         (if (plist-get status :workspace-set) "set" "unset"))
        (when-let* ((workspace (plist-get status :workspace)))
          (insert "   "
                  (propertize workspace 'face 'aaron-ui-board-path)
                  "\n"))
        (insert "   ")
        (aaron-ui-board-insert-actions
         '((:label "Ensure"    :command my/language-server-manager-ensure    :primary t
                   :help "Ensure backend")
           (:label "Restart"   :command my/language-server-manager-restart   :help "Restart server")
           (:label "Shutdown"  :command my/language-server-manager-shutdown  :help "Shutdown server")
           (:label "Log"       :command my/language-server-manager-open-log  :help "Open log")
           (:label "Session"   :command my/language-server-manager-describe-session :help "Session info")
           (:label "Config"    :command my/language-server-manager-show-workspace-configuration
                   :help "Workspace config")
           (:label "Toolchain" :command my/language-server-manager-select-toolchain
                   :help "Select project toolchain")
           (:label "Reset TC"  :command my/language-server-manager-reset-toolchain
                   :help "Reset session toolchain")
           (:label "Actions"   :command my/language-server-manager-code-actions   :help "Code actions")
           (:label "Format"    :command my/language-server-manager-format-buffer  :help "Format buffer")
           (:label "Rename"    :command my/language-server-manager-rename         :help "Rename symbol")))
        (insert "\n\n"))
    (aaron-ui-board-insert-empty "No source buffer.")))

(defun my/language-server-manager--insert-routing-section ()
  "Insert explicit `lsp-mode' routing overrides."
  (my/language-server-manager--assert-view-buffer)
  (let ((entries (my/language-server-lsp-mode-preference-entries)))
    (aaron-ui-board-insert-section "Explicit lsp-mode Routes" (length entries))
    (if entries
        (dolist (entry entries)
          (let* ((mode (plist-get entry :mode))
                 (feature (plist-get entry :feature))
                 (source (plist-get entry :source))
                 (note (plist-get entry :note))
                 (status (my/language-server--feature-status feature))
                 (current (with-current-buffer (my/language-server--source-buffer)
                            (derived-mode-p mode)))
                 (start (point)))
            (aaron-ui-board-insert-field
             (symbol-name mode)
             (format "lsp-mode  feature=%s  %s%s"
                     (or feature "-") status
                     (if current "  ●current" ""))
             (if current 'aaron-ui-board-badge-info nil))
            (when source
              (insert "   " (propertize "source  " 'face 'aaron-ui-board-meta))
              (my/language-server--insert-openable-path source)
              (insert "\n"))
            (when note
              (insert "   " (propertize note 'face 'aaron-ui-board-detail) "\n"))
            (when source
              (insert "   ")
              (aaron-ui-board-insert-action
               "Open source" (lambda () (find-file source)) "Open the file defining this route")
              (insert "\n"))
            (insert "\n")
            (my/language-server-manager--set-entry-properties
             start (point)
             (list :kind 'route :source source :mode mode))))
      (aaron-ui-board-insert-empty "No explicit lsp-mode overrides are registered."))))

(defun my/language-server-manager--insert-eglot-section ()
  "Insert locally registered Eglot server mappings."
  (my/language-server-manager--assert-view-buffer)
  (let ((entries (my/language-server-eglot-program-entries)))
    (aaron-ui-board-insert-section "Custom Eglot Server Mappings" (length entries))
    (if entries
        (dolist (entry entries)
          (let* ((modes (plist-get entry :modes))
                 (label (or (plist-get entry :label)
                            (format "%s" (plist-get entry :program))))
                 (executables (plist-get entry :executables))
                 (source (plist-get entry :source))
                 (note (plist-get entry :note))
                 (current (my/language-server--eglot-entry-matches-buffer-p
                           entry
                           (my/language-server--source-buffer)))
                 (start (point)))
            (aaron-ui-board-insert-field
             (my/language-server--format-mode-list modes)
             (concat label (if current "  ●current" ""))
             (if current 'aaron-ui-board-badge-info nil))
            (aaron-ui-board-insert-field
             "executables"
             (my/language-server--executable-summary executables))
            (when source
              (insert "   " (propertize "source  " 'face 'aaron-ui-board-meta))
              (my/language-server--insert-openable-path source)
              (insert "\n"))
            (when note
              (insert "   " (propertize note 'face 'aaron-ui-board-detail) "\n"))
            (when source
              (insert "   ")
              (aaron-ui-board-insert-action
               "Open source" (lambda () (find-file source)) "Open the file defining this mapping")
              (insert "\n"))
            (insert "\n")
            (my/language-server-manager--set-entry-properties
             start (point)
             (list :kind 'eglot :source source :modes modes))))
      (aaron-ui-board-insert-empty "No custom Eglot mappings are registered."))))

(defun my/language-server-manager--insert-runtime-knobs ()
  "Insert the runtime knobs section."
  (my/language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Runtime Knobs")
  (dolist (entry (my/language-server--runtime-knob-entries))
    (aaron-ui-board-insert-field (car entry) (format "%s" (or (cdr entry) "-"))))
  (insert "\n"))

(defun my/language-server-manager-refresh ()
  "Refresh the language-server dashboard."
  (interactive)
  (my/language-server-manager--assert-view-buffer)
  (let ((inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Language Server Hub"
        :icon 'server
        :actions '((:label "Doctor"   :command my/language-server-doctor   :help "Doctor report")
                   (:label "Dispatch" :command my/language-server-dispatch :help "Transient menu" :primary t)
                   (:label "Docs"     :command my/language-server-manager-open-docs :help "Workflow docs")))
       (my/language-server-manager-insert-maintenance-section)
       (run-hooks 'my/language-server-manager-extra-section-functions)
       (my/language-server-manager--insert-current-buffer-section)
       (my/language-server-manager--insert-settings-section)
       (my/language-server-manager--insert-routing-section)
       (my/language-server-manager--insert-eglot-section)
       (my/language-server-manager--insert-runtime-knobs)
       (aaron-ui-board-insert-key-hints
        "Keys: g refresh  i toolchain  x reset-toolchain  v rescan-toolchains  e ensure  r restart  k shutdown  l log  s session  c config  o imports  a actions  f format  R rename  p problems  D doctor  q quit")))))

(defun my/language-server--doctor-insert-libraries ()
  "Insert the library availability section."
  (my/language-server-doctor--assert-view-buffer)
  (aaron-ui-board-insert-section "Libraries")
  (dolist (library '("eglot" "lsp-mode" "company" "company-box"
                     "company-prescient" "flymake-diagnostic-at-point"
                     "eldoc-box" "breadcrumb" "dape"))
    (let ((path (my/language-server--library-path library)))
      (if path
          (progn
            (insert "   "
                    (propertize (format "%-24s" library) 'face 'aaron-ui-board-meta))
            (my/language-server--insert-openable-path path)
            (insert "\n"))
        (aaron-ui-board-insert-field library "MISSING" 'aaron-ui-board-bad))))
  (insert "\n"))

(defun my/language-server--doctor-insert-executables ()
  "Insert the executable availability section."
  (my/language-server-doctor--assert-view-buffer)
  (let ((names (delete-dups
                (apply #'append
                       (delq nil
                             (mapcar (lambda (entry)
                                       (copy-sequence
                                        (plist-get entry :executables)))
                                     (my/language-server-eglot-program-entries)))))))
    (aaron-ui-board-insert-section "Executables" (length names))
    (if names
        (dolist (name names)
          (let ((report (my/language-server--executable-report name)))
            (if-let* ((path (plist-get report :path)))
                (progn
                  (insert "   "
                          (propertize (format "%-24s" (plist-get report :name))
                                      'face 'aaron-ui-board-meta))
                  (my/language-server--insert-openable-path path)
                  (insert "\n"))
              (aaron-ui-board-insert-field name "MISSING" 'aaron-ui-board-bad))))
      (aaron-ui-board-insert-empty "No custom server executables are registered."))
    (insert "\n")))

(defun my/language-server--doctor-insert-current-buffer (source)
  "Insert a current-buffer report for SOURCE."
  (my/language-server-doctor--assert-view-buffer)
  (aaron-ui-board-insert-section "Current Buffer")
  (if-let* ((status (my/language-server--source-status source)))
      (progn
        (aaron-ui-board-insert-field "buffer"      (plist-get status :buffer))
        (aaron-ui-board-insert-field "file"        (plist-get status :file))
        (aaron-ui-board-insert-field "major mode"  (format "%s" (plist-get status :major-mode)))
        (aaron-ui-board-insert-field "directory"   (plist-get status :default-directory))
        (aaron-ui-board-insert-field "project root" (plist-get status :project-root))
        (aaron-ui-board-insert-field "route policy" (plist-get status :policy))
        (aaron-ui-board-insert-field "active backend" (format "%s" (plist-get status :active-backend)))
        (aaron-ui-board-insert-field
         "runtime context"
         (format "%s  [state=%s%s%s]"
                 (or (plist-get status :runtime) "project default")
                 (plist-get status :runtime-state)
                 (if-let* ((provider (plist-get status :runtime-provider)))
                     (format ", provider=%s" provider) "")
                 (if-let* ((target (plist-get status :runtime-target)))
                     (format ", target=%s" target) "")))
        (when-let* ((fallback (plist-get status :runtime-fallback)))
          (aaron-ui-board-insert-field "runtime fallback" fallback
                                       'aaron-ui-board-warn))
        (aaron-ui-board-insert-field
         "lsp feature"
         (format "%s (%s)" (plist-get status :required-feature) (plist-get status :feature-status)))
        (aaron-ui-board-insert-field "eglot mapping"   (plist-get status :eglot-match))
        (aaron-ui-board-insert-field
         "flymake/company"
         (format "%s / %s / %s"
                 (plist-get status :flymake)
                 (plist-get status :company)
                 (plist-get status :breadcrumb)))
        (aaron-ui-board-insert-field
         "workspace config"
         (if (plist-get status :workspace-set) "set" "unset"))
        (when-let* ((workspace (plist-get status :workspace)))
          (insert "   "
                  (propertize workspace 'face 'aaron-ui-board-path)
                  "\n")))
    (aaron-ui-board-insert-empty "No source buffer."))
  (insert "\n"))

(defun my/language-server--doctor-insert-routing ()
  "Insert routing and mapping summaries."
  (my/language-server-doctor--assert-view-buffer)
  (let ((lsp-entries (my/language-server-lsp-mode-preference-entries))
        (eglot-entries (my/language-server-eglot-program-entries)))
    (aaron-ui-board-insert-section
     "Routing Summary"
     (+ (length lsp-entries) (length eglot-entries)))
    (aaron-ui-board-insert-field "lsp-mode routes"  (number-to-string (length lsp-entries)))
    (dolist (entry lsp-entries)
      (insert "   "
              (propertize (format "  %-18s " (plist-get entry :mode)) 'face 'aaron-ui-board-detail)
              (propertize (format "feature=%-12s status=%s"
                                  (or (plist-get entry :feature) "-")
                                  (my/language-server--feature-status (plist-get entry :feature)))
                          'face 'aaron-ui-board-meta)
              "\n"))
    (aaron-ui-board-insert-field "eglot mappings" (number-to-string (length eglot-entries)))
    (dolist (entry eglot-entries)
      (insert "   "
              (propertize (format "  %-36s " (my/language-server--format-mode-list
                                              (plist-get entry :modes)))
                          'face 'aaron-ui-board-detail)
              (propertize (format "executables=%s"
                                  (my/language-server--executable-summary
                                   (plist-get entry :executables)))
                          'face 'aaron-ui-board-meta)
              "\n"))
    (insert "\n")))

(defun my/language-server--doctor-insert-runtime-knobs ()
  "Insert runtime knob values."
  (my/language-server-doctor--assert-view-buffer)
  (aaron-ui-board-insert-section "Runtime Knobs")
  (dolist (entry (my/language-server--runtime-knob-entries))
    (aaron-ui-board-insert-field (car entry) (format "%s" (or (cdr entry) "-"))))
  (insert "\n"))

(defun my/language-server-doctor-refresh ()
  "Refresh the current language-server doctor buffer."
  (interactive)
  (my/language-server-doctor--assert-view-buffer)
  (let ((source (my/language-server--source-buffer))
        (inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Language Server Doctor"
        :icon 'diagnostics
        :actions '((:label "Hub"      :command my/language-server-manager  :help "Open Hub"  :primary t)
                   (:label "Dispatch" :command my/language-server-dispatch :help "Transient menu")))
       (my/language-server--doctor-insert-libraries)
       (my/language-server--doctor-insert-executables)
       (my/language-server--doctor-insert-current-buffer source)
       (my/language-server--doctor-insert-routing)
       (my/language-server--doctor-insert-runtime-knobs)
       (aaron-ui-board-insert-key-hints
        "Keys: g refresh  h hub  e ensure  r restart  k shutdown  l log  s session  D doctor  q quit")))))

(defun my/language-server-doctor ()
  "Open a doctor report for the language-server stack."
  (interactive)
  (let ((buffer (get-buffer-create my/language-server-doctor-buffer-name))
        (source (my/language-server--source-buffer)))
    (with-current-buffer buffer
      (my/language-server-doctor-mode)
      (aaron-ui-board-set-header "Language Server Doctor" 'diagnostics)
      (setq-local my/language-server-manager-source-buffer source)
      (setq-local aaron-ui-board-refresh-function #'my/language-server-doctor-refresh)
      (my/language-server--watch-source-buffer source)
      (let ((inhibit-read-only t))
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "g") #'my/language-server-doctor-refresh)
        (local-set-key (kbd "h") #'my/language-server-manager)
        (local-set-key (kbd "e") #'my/language-server-manager-ensure)
        (local-set-key (kbd "r") #'my/language-server-manager-restart)
        (local-set-key (kbd "k") #'my/language-server-manager-shutdown)
        (local-set-key (kbd "l") #'my/language-server-manager-open-log)
        (local-set-key (kbd "s") #'my/language-server-manager-describe-session)
        (local-set-key (kbd "c") #'my/language-server-manager-show-workspace-configuration)
        (local-set-key (kbd "D") #'my/language-server-doctor)
        (local-set-key (kbd "A") #'my/language-server-toggle-eglot-autoreconnect)
        (local-set-key (kbd "S") #'my/language-server-toggle-eglot-autoshutdown)
        (local-set-key (kbd "L") #'my/language-server-toggle-lsp-log-io)
        (local-set-key (kbd "I") #'my/language-server-toggle-inlay-hints)
        (local-set-key (kbd "M") #'my/language-server-set-read-process-output-max)
        (local-set-key (kbd "E") #'my/language-server-set-eglot-events-buffer-size)
        (local-set-key (kbd "O") #'my/language-server-manager-open-docs)
        (local-set-key (kbd "?") #'my/language-server-dispatch)
        (local-set-key [mouse-3] #'my/language-server-manager-popup-menu)
        (my/language-server-doctor-refresh)))
    (pop-to-buffer buffer)))

(defun my/language-server-manager-setup-keys ()
  "Install local keybindings for the language-server hub."
  (local-set-key (kbd "g") #'my/language-server-manager-refresh)
  (local-set-key (kbd "e") #'my/language-server-manager-ensure)
  (local-set-key (kbd "r") #'my/language-server-manager-restart)
  (local-set-key (kbd "k") #'my/language-server-manager-shutdown)
  (local-set-key (kbd "l") #'my/language-server-manager-open-log)
  (local-set-key (kbd "s") #'my/language-server-manager-describe-session)
  (local-set-key (kbd "c") #'my/language-server-manager-show-workspace-configuration)
  (local-set-key (kbd "i") #'my/language-server-manager-select-toolchain)
  (local-set-key (kbd "x") #'my/language-server-manager-reset-toolchain)
  (local-set-key (kbd "v") #'my/language-server-manager-refresh-toolchains)
  (local-set-key (kbd "o") #'my/language-server-manager-organize-imports)
  (local-set-key (kbd "a") #'my/language-server-manager-code-actions)
  (local-set-key (kbd "f") #'my/language-server-manager-format-buffer)
  (local-set-key (kbd "R") #'my/language-server-manager-rename)
  (local-set-key (kbd "p") #'my/language-server-manager-problems-buffer)
  (local-set-key (kbd "P") #'my/language-server-manager-problems-project)
  (local-set-key (kbd "d") #'my/language-server-manager-diagnostics-buffer-ui)
  (local-set-key (kbd "T") #'my/language-server-manager-diagnostics-project-ui)
  (local-set-key (kbd "m") #'my/language-server-manager-diagnostics-menu)
  (local-set-key (kbd "D") #'my/language-server-doctor)
  (local-set-key (kbd "A") #'my/language-server-toggle-eglot-autoreconnect)
  (local-set-key (kbd "S") #'my/language-server-toggle-eglot-autoshutdown)
  (local-set-key (kbd "L") #'my/language-server-toggle-lsp-log-io)
  (local-set-key (kbd "I") #'my/language-server-toggle-inlay-hints)
  (local-set-key (kbd "M") #'my/language-server-set-read-process-output-max)
  (local-set-key (kbd "E") #'my/language-server-set-eglot-events-buffer-size)
  (local-set-key (kbd "O") #'my/language-server-manager-open-docs)
  (local-set-key (kbd "?") #'my/language-server-dispatch)
  (local-set-key (kbd "RET") #'my/language-server-manager-context-action)
  (local-set-key [mouse-3] #'my/language-server-manager-popup-menu))

(defun my/language-server-manager ()
  "Open the language-server dashboard."
  (interactive)
  (let ((buffer (get-buffer-create my/language-server-manager-buffer-name))
        (source (my/language-server--source-buffer)))
    (with-current-buffer buffer
      (my/language-server-manager-mode)
      (aaron-ui-board-set-header "Language Server Hub" 'server)
      (setq-local my/language-server-manager-source-buffer source)
      (setq-local aaron-ui-board-refresh-function #'my/language-server-manager-refresh)
      (my/language-server--watch-source-buffer source)
      (let ((inhibit-read-only t))
        (use-local-map (copy-keymap special-mode-map))
        (my/language-server-manager-setup-keys)
        (run-hooks 'my/language-server-manager-setup-functions)
        (my/language-server-manager-refresh)))
    (pop-to-buffer buffer)))

(transient-define-prefix my/language-server-dispatch ()
  "Language-server command surface."
  [["Views"
    ("h" "hub" my/language-server-manager)
    ("D" "doctor" my/language-server-doctor)
    ("O" "docs" my/language-server-manager-open-docs)]
   ["Lifecycle"
    ("e" "ensure" my/language-server-manager-ensure)
    ("r" "restart" my/language-server-manager-restart)
    ("k" "shutdown" my/language-server-manager-shutdown)
    ("l" "log" my/language-server-manager-open-log)
    ("s" "session" my/language-server-manager-describe-session)
    ("c" "config" my/language-server-manager-show-workspace-configuration)]
   ["Toolchain"
    ("i" "select" my/language-server-manager-select-toolchain)
    ("x" "reset" my/language-server-manager-reset-toolchain)
    ("v" "rescan" my/language-server-manager-refresh-toolchains)]
   ["Edits"
    ("a" "code actions" my/language-server-manager-code-actions)
    ("o" "organize imports" my/language-server-manager-organize-imports)
    ("f" "format" my/language-server-manager-format-buffer)
    ("R" "rename" my/language-server-manager-rename)]
   ["Tuning"
    ("A" "toggle autoreconnect" my/language-server-toggle-eglot-autoreconnect)
    ("S" "toggle autoshutdown" my/language-server-toggle-eglot-autoshutdown)
    ("L" "toggle log-io" my/language-server-toggle-lsp-log-io)
    ("I" "toggle inlay hints" my/language-server-toggle-inlay-hints)
    ("M" "set output max" my/language-server-set-read-process-output-max)
    ("E" "set events buffer" my/language-server-set-eglot-events-buffer-size)]
   ["Diagnostics"
    ("p" "buffer problems" my/language-server-manager-problems-buffer)
    ("P" "project problems" my/language-server-manager-problems-project)
    ("d" "buffer diagnostics ui" my/language-server-manager-diagnostics-buffer-ui)
    ("T" "project diagnostics ui" my/language-server-manager-diagnostics-project-ui)
    ("m" "diagnostics menu" my/language-server-manager-diagnostics-menu)]])

(defalias 'my/language-server-ops-dispatch #'my/language-server-dispatch)

(my/leader!
  "c L" '(:def my/language-server-dispatch :which-key "language server"))

(provide 'init-lsp-tools)
;;; init-lsp-tools.el ends here

;;; init-lsp-tools.el --- Language server dashboard and diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Display, maintenance, and debugging helpers for the language-server stack.
;; This mirrors the split used by the Jupyter workflow: keep the core routing in
;; `init-lsp.el', keep the backend-agnostic operations in `init-lsp-ops.el',
;; and put the display / doctor / dispatch surface here.

;;; Code:

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
(declare-function my/problems-buffer "init-problems")
(declare-function my/problems-project "init-problems")
(declare-function my/diagnostics-buffer-ui "init-diagnostics-ui")
(declare-function my/diagnostics-project-ui "init-diagnostics-ui")
(declare-function my/diagnostics-dispatch "init-diagnostics-extra")
(declare-function eglot-inlay-hints-mode "eglot" (&optional arg))
(declare-function lsp-inlay-hints-mode "lsp-mode" (&optional arg))

(define-derived-mode my/language-server-manager-mode special-mode "Lang-Server-Hub"
  "Major mode for the language-server dashboard.")

(define-derived-mode my/language-server-doctor-mode special-mode "Lang-Server-Doctor"
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
  (insert-text-button
   (my/language-server--openable-path path)
   'follow-link t
   'help-echo "Open this path"
   'action (lambda (_button)
             (find-file path))))

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
  (insert "Maintenance\n")
  (insert "-----------\n")
  (insert (format "explicit lsp-mode routes: %d\n"
                  (length (my/language-server-lsp-mode-preference-entries))))
  (insert (format "custom eglot mappings: %d\n"
                  (length (my/language-server-eglot-program-entries))))
  (insert "actions: ")
  (my/language-server--insert-button
   "[doctor]"
   (lambda (_button) (my/language-server-doctor))
   "Open the language-server doctor report")
  (insert " ")
  (my/language-server--insert-button
   "[dispatch]"
   (lambda (_button) (my/language-server-dispatch))
   "Open the transient dispatch menu")
  (insert " ")
  (my/language-server--insert-button
   "[docs]"
   (lambda (_button) (my/language-server-manager-open-docs))
   "Open the workflow documentation")
  (insert " ")
  (my/language-server--insert-button
   "[project problems]"
   (lambda (_button) (my/language-server-manager-problems-project))
   "Open the project problems view")
  (insert "\n\n"))

(defun my/language-server-manager--insert-settings-section ()
  "Insert quick runtime settings for the language-server stack."
  (my/language-server-manager--assert-view-buffer)
  (insert "Quick Settings\n")
  (insert "--------------\n")
  (insert "These toggles only affect the current Emacs session.  If one of them proves useful, move it back into the corresponding init file.\n")
  (insert (format "eglot-autoreconnect: %s\n"
                  (my/language-server--value-or-unset 'eglot-autoreconnect)))
  (insert (format "eglot-autoshutdown: %s\n"
                  (my/language-server--value-or-unset 'eglot-autoshutdown)))
  (insert (format "lsp-log-io: %s\n"
                  (my/language-server--value-or-unset 'lsp-log-io)))
  (insert (format "lsp-inlay-hint-enable: %s\n"
                  (my/language-server--value-or-unset 'lsp-inlay-hint-enable)))
  (insert (format "read-process-output-max: %s\n"
                  (my/language-server--value-or-unset 'read-process-output-max)))
  (insert (format "eglot-events-buffer-size: %s\n"
                  (my/language-server--value-or-unset 'eglot-events-buffer-size)))
  (insert "actions: ")
  (my/language-server--insert-button
   "[toggle autoreconnect]"
   (lambda (_button) (my/language-server-toggle-eglot-autoreconnect))
   "Toggle eglot autoreconnect")
  (insert " ")
  (my/language-server--insert-button
   "[toggle autoshutdown]"
   (lambda (_button) (my/language-server-toggle-eglot-autoshutdown))
   "Toggle eglot autoshutdown")
  (insert " ")
  (my/language-server--insert-button
   "[toggle log-io]"
   (lambda (_button) (my/language-server-toggle-lsp-log-io))
   "Toggle lsp-mode wire logging")
  (insert " ")
  (my/language-server--insert-button
   "[toggle inlay hints]"
   (lambda (_button) (my/language-server-toggle-inlay-hints))
   "Toggle inlay hints for the current session")
  (insert " ")
  (my/language-server--insert-button
   "[set output max]"
   (lambda (_button) (call-interactively #'my/language-server-set-read-process-output-max))
   "Set read-process-output-max")
  (insert " ")
  (my/language-server--insert-button
   "[set events buffer]"
   (lambda (_button) (call-interactively #'my/language-server-set-eglot-events-buffer-size))
   "Set eglot-events-buffer-size")
  (insert "\n\n"))

(defun my/language-server-manager--insert-current-buffer-section ()
  "Insert the current-buffer status section."
  (my/language-server-manager--assert-view-buffer)
  (insert "Current Buffer\n")
  (insert "--------------\n")
  (if-let* ((status (my/language-server--source-status)))
      (progn
        (insert (format "buffer: %s\n" (plist-get status :buffer)))
        (insert (format "file: %s\n" (plist-get status :file)))
        (insert (format "major mode: %s\n" (plist-get status :major-mode)))
        (insert (format "default-directory: %s\n"
                        (plist-get status :default-directory)))
        (insert (format "project root: %s\n" (plist-get status :project-root)))
        (insert (format "route policy: %s\n" (plist-get status :policy)))
        (insert (format "active backend: %s\n"
                        (plist-get status :active-backend)))
        (insert (format "required lsp feature: %s (%s)\n"
                        (plist-get status :required-feature)
                        (plist-get status :feature-status)))
        (insert (format "matching custom eglot mapping: %s\n"
                        (plist-get status :eglot-match)))
        (insert (format "flymake/company/breadcrumb: %s / %s / %s\n"
                        (plist-get status :flymake)
                        (plist-get status :company)
                        (plist-get status :breadcrumb)))
        (insert (format "local eglot workspace config: %s\n"
                        (if (plist-get status :workspace-set)
                            "set"
                          "unset")))
        (when-let* ((workspace (plist-get status :workspace)))
          (insert (format "  %s\n" workspace)))
        (insert "actions: ")
        (my/language-server--insert-button
         "[ensure]"
         (lambda (_button) (my/language-server-manager-ensure))
         "Ensure the preferred backend for the source buffer")
        (insert " ")
        (my/language-server--insert-button
         "[restart]"
         (lambda (_button) (my/language-server-manager-restart))
         "Restart the active language server")
        (insert " ")
        (my/language-server--insert-button
         "[shutdown]"
         (lambda (_button) (my/language-server-manager-shutdown))
         "Shutdown the active language server")
        (insert " ")
        (my/language-server--insert-button
         "[log]"
         (lambda (_button) (my/language-server-manager-open-log))
         "Open the active backend log")
        (insert " ")
        (my/language-server--insert-button
         "[session]"
         (lambda (_button) (my/language-server-manager-describe-session))
         "Describe the current language-server session")
        (insert " ")
        (my/language-server--insert-button
         "[config]"
         (lambda (_button) (my/language-server-manager-show-workspace-configuration))
         "Show backend workspace configuration")
        (insert " ")
        (my/language-server--insert-button
         "[actions]"
         (lambda (_button) (my/language-server-manager-code-actions))
         "Run code actions")
        (insert " ")
        (my/language-server--insert-button
         "[format]"
         (lambda (_button) (my/language-server-manager-format-buffer))
         "Format the current buffer")
        (insert " ")
        (my/language-server--insert-button
         "[rename]"
         (lambda (_button) (my/language-server-manager-rename))
         "Rename the symbol at point")
        (insert "\n\n"))
    (insert "No source buffer.\n\n")))

(defun my/language-server-manager--insert-routing-section ()
  "Insert explicit `lsp-mode' routing overrides."
  (my/language-server-manager--assert-view-buffer)
  (insert "Explicit lsp-mode Routes\n")
  (insert "------------------------\n")
  (insert "Default behavior: `prog-mode' buffers prefer Eglot unless a mode is explicitly routed to `lsp-mode`.\n\n")
  (let ((entries (my/language-server-lsp-mode-preference-entries)))
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
            (insert (format "%-18s backend=lsp-mode feature=%-12s status=%s%s\n"
                            mode
                            (or feature "-")
                            status
                            (if current " current=yes" "")))
            (insert "  source: ")
            (if source
                (my/language-server--insert-openable-path source)
              (insert "-"))
            (insert "\n")
            (when note
              (insert (format "  note: %s\n" note)))
            (insert "  actions: ")
            (if source
                (my/language-server--insert-button
                 "[source]"
                 (lambda (_button) (find-file source))
                 "Open the file defining this route")
              (insert "-"))
            (insert "\n\n")
            (my/language-server-manager--set-entry-properties
             start (point)
             (list :kind 'route
                   :source source
                   :mode mode))))
      (insert "No explicit lsp-mode overrides are registered.\n\n"))))

(defun my/language-server-manager--insert-eglot-section ()
  "Insert locally registered Eglot server mappings."
  (my/language-server-manager--assert-view-buffer)
  (insert "Custom Eglot Server Mappings\n")
  (insert "----------------------------\n")
  (insert "Built-in Eglot mappings still apply.  This section only lists local additions/overrides registered through `my/register-eglot-server-program`.\n\n")
  (let ((entries (my/language-server-eglot-program-entries)))
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
            (insert (format "%s%s\n"
                            (my/language-server--format-mode-list modes)
                            (if current "  current=yes" "")))
            (insert (format "  server: %s\n" label))
            (insert (format "  executables: %s\n"
                            (my/language-server--executable-summary executables)))
            (insert "  source: ")
            (if source
                (my/language-server--insert-openable-path source)
              (insert "-"))
            (insert "\n")
            (when note
              (insert (format "  note: %s\n" note)))
            (insert "  actions: ")
            (if source
                (my/language-server--insert-button
                 "[source]"
                 (lambda (_button) (find-file source))
                 "Open the file defining this mapping")
              (insert "-"))
            (insert "\n\n")
            (my/language-server-manager--set-entry-properties
             start (point)
             (list :kind 'eglot
                   :source source
                   :modes modes))))
      (insert "No custom Eglot mappings are registered.\n\n"))))

(defun my/language-server-manager--insert-runtime-knobs ()
  "Insert the runtime knobs section."
  (my/language-server-manager--assert-view-buffer)
  (insert "Runtime Knobs\n")
  (insert "-------------\n")
  (dolist (entry (my/language-server--runtime-knob-entries))
    (insert (format "%-28s %s\n"
                    (car entry)
                    (or (cdr entry) "-"))))
  (insert "\n"))

(defun my/language-server-manager-refresh ()
  "Refresh the language-server dashboard."
  (interactive)
  (my/language-server-manager--assert-view-buffer)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Language Server Hub\n")
    (insert "===================\n\n")
    (insert "Keys: g refresh, e ensure, r restart, k shutdown, l log, s session, c config, o organize imports, a code actions, f format, R rename, p problems, P project problems, d diagnostics ui, m diagnostics menu, A autoreconnect, S autoshutdown, L log-io, I inlay hints, M output max, E events buffer, D doctor, O docs, ? dispatch, RET context action, q quit\n\n")
    (my/language-server-manager-insert-maintenance-section)
    (run-hooks 'my/language-server-manager-extra-section-functions)
    (my/language-server-manager--insert-current-buffer-section)
    (my/language-server-manager--insert-settings-section)
    (my/language-server-manager--insert-routing-section)
    (my/language-server-manager--insert-eglot-section)
    (my/language-server-manager--insert-runtime-knobs)
    (goto-char (point-min))))

(defun my/language-server--doctor-insert-libraries ()
  "Insert the library availability section."
  (my/language-server-doctor--assert-view-buffer)
  (insert "Libraries\n")
  (insert "---------\n")
  (dolist (library '("eglot"
                     "lsp-mode"
                     "company"
                     "company-box"
                     "company-prescient"
                     "flymake-diagnostic-at-point"
                     "eldoc-box"
                     "breadcrumb"
                     "dape"))
    (let ((path (my/language-server--library-path library)))
      (insert (format "%-28s " library))
      (if path
          (my/language-server--insert-openable-path path)
        (insert "MISSING"))
      (insert "\n")))
  (insert "\n"))

(defun my/language-server--doctor-insert-executables ()
  "Insert the executable availability section."
  (my/language-server-doctor--assert-view-buffer)
  (insert "Executables\n")
  (insert "-----------\n")
  (let ((names (delete-dups
                (apply #'append
                       (delq nil
                             (mapcar (lambda (entry)
                                       (copy-sequence
                                        (plist-get entry :executables)))
                                     (my/language-server-eglot-program-entries)))))))
    (if names
        (dolist (name names)
          (let ((report (my/language-server--executable-report name)))
            (insert (format "%-28s " (plist-get report :name)))
            (if-let* ((path (plist-get report :path)))
                (my/language-server--insert-openable-path path)
              (insert "MISSING"))
            (insert "\n")))
      (insert "No custom server executables are registered.\n"))
    (insert "\n")))

(defun my/language-server--doctor-insert-current-buffer (source)
  "Insert a current-buffer report for SOURCE."
  (my/language-server-doctor--assert-view-buffer)
  (insert "Current Buffer\n")
  (insert "--------------\n")
  (if-let* ((status (my/language-server--source-status source)))
      (progn
        (insert (format "buffer: %s\n" (plist-get status :buffer)))
        (insert (format "file: %s\n" (plist-get status :file)))
        (insert (format "major mode: %s\n" (plist-get status :major-mode)))
        (insert (format "default-directory: %s\n"
                        (plist-get status :default-directory)))
        (insert (format "project root: %s\n" (plist-get status :project-root)))
        (insert (format "route policy: %s\n" (plist-get status :policy)))
        (insert (format "active backend: %s\n"
                        (plist-get status :active-backend)))
        (insert (format "required lsp feature: %s (%s)\n"
                        (plist-get status :required-feature)
                        (plist-get status :feature-status)))
        (insert (format "matching custom eglot mapping: %s\n"
                        (plist-get status :eglot-match)))
        (insert (format "flymake/company/breadcrumb: %s / %s / %s\n"
                        (plist-get status :flymake)
                        (plist-get status :company)
                        (plist-get status :breadcrumb)))
        (insert (format "local eglot workspace config: %s\n"
                        (if (plist-get status :workspace-set)
                            "set"
                          "unset")))
        (when-let* ((workspace (plist-get status :workspace)))
          (insert workspace)
          (insert "\n")))
    (insert "No source buffer.\n"))
  (insert "\n"))

(defun my/language-server--doctor-insert-routing ()
  "Insert routing and mapping summaries."
  (my/language-server-doctor--assert-view-buffer)
  (insert "Routing Summary\n")
  (insert "---------------\n")
  (insert (format "explicit lsp-mode routes: %d\n"
                  (length (my/language-server-lsp-mode-preference-entries))))
  (dolist (entry (my/language-server-lsp-mode-preference-entries))
    (insert (format "  %-18s feature=%-12s status=%s\n"
                    (plist-get entry :mode)
                    (or (plist-get entry :feature) "-")
                    (my/language-server--feature-status
                     (plist-get entry :feature)))))
  (insert (format "custom eglot mappings: %d\n"
                  (length (my/language-server-eglot-program-entries))))
  (dolist (entry (my/language-server-eglot-program-entries))
    (insert (format "  %-36s executables=%s\n"
                    (my/language-server--format-mode-list (plist-get entry :modes))
                    (my/language-server--executable-summary
                     (plist-get entry :executables)))))
  (insert "\n"))

(defun my/language-server--doctor-insert-runtime-knobs ()
  "Insert runtime knob values."
  (my/language-server-doctor--assert-view-buffer)
  (insert "Runtime Knobs\n")
  (insert "-------------\n")
  (dolist (entry (my/language-server--runtime-knob-entries))
    (insert (format "%-28s %s\n"
                    (car entry)
                    (or (cdr entry) "-"))))
  (insert "\n"))

(defun my/language-server-doctor-refresh ()
  "Refresh the current language-server doctor buffer."
  (interactive)
  (my/language-server-doctor--assert-view-buffer)
  (let ((source (my/language-server--source-buffer))
        (inhibit-read-only t))
    (erase-buffer)
    (insert "Language Server Doctor\n")
    (insert "======================\n\n")
    (my/language-server--doctor-insert-libraries)
    (my/language-server--doctor-insert-executables)
    (my/language-server--doctor-insert-current-buffer source)
    (my/language-server--doctor-insert-routing)
    (my/language-server--doctor-insert-runtime-knobs)
    (goto-char (point-min))))

(defun my/language-server-doctor ()
  "Open a doctor report for the language-server stack."
  (interactive)
  (let ((buffer (get-buffer-create my/language-server-doctor-buffer-name))
        (source (my/language-server--source-buffer)))
    (with-current-buffer buffer
      (my/language-server-doctor-mode)
      (setq-local my/language-server-manager-source-buffer source)
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
      (setq-local my/language-server-manager-source-buffer source)
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

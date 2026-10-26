;;; tools/lsp-hub/config.el --- Language server dashboard and diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Display, maintenance, and debugging helpers for the language-server stack.
;; This mirrors the split used by the Jupyter workflow: keep the core routing in
;; `tools/lsp/config.el', keep the backend-agnostic operations in `tools/lsp-ops/config.el',
;; and put the display / doctor / dispatch surface here.

;;; Code:

(require 'aaron-ui-board)
(require 'backtrace)
(require 'cl-lib)
(require 'easymenu)
(require 'aaron-funcs)
(require 'aaron-open)
(require 'pp)
(require 'subr-x)
(require 'transient)

(declare-function aaron-open-client-document "aaron-open" (file &optional view))

(defconst +language-server-tools--module-file
  (or load-file-name buffer-file-name)
  "Absolute path of `tools/lsp-hub/config.el' at load time.")

(defconst +language-server-manager-buffer-name "*Language Server Hub*"
  "Buffer name used by the language-server dashboard.")

(defconst +language-server-doctor-buffer-name "*Language Server Doctor*"
  "Buffer name used by the language-server doctor report.")

(defconst +language-server-doc-file
  (expand-file-name
   "../../../docs/design/lsp-workflow.md"
   (file-name-directory +language-server-tools--module-file))
  "Primary language-server workflow document.")

(defvar +language-server-manager-extra-section-functions nil
  "Functions used to append extra sections to the language-server hub.")

(defvar +language-server-manager-setup-functions nil
  "Functions run after the language-server hub keymap is initialized.")

(defvar-local +language-server-manager-source-buffer nil
  "Source buffer used by the current language-server view.")

(defvar +language-server-manager-entry-mouse-map nil
  "Mouse keymap installed on row entries in language-server views.")

(defvar company-mode)
(defvar breadcrumb-local-mode)
(defvar lsp-keep-workspace-alive)
(defvar lsp-log-max)
(defvar lsp-restart)
(defvar +language-server--workspace-configuration)
(defvar flymake-mode)
(defvar flymake-no-changes-timeout)
(defvar lsp-inlay-hint-enable)
(defvar lsp-log-io)

(declare-function find-file "files" (filename &optional wildcards))
(declare-function locate-library "find-func" (library &optional nosuffix path interactive-call))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +language-server/code-actions "tools/lsp/config")
(declare-function +language-server-program-entries "tools/lsp/config")
(declare-function +language-server/ensure "tools/lsp/config")
(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function +language-server/format-buffer "tools/lsp/config")
(declare-function +language-server-project-backend-override "tools/lsp/config")
(declare-function +language-server/rename "tools/lsp/config")
(declare-function +language-server/organize-imports "tools/lsp-ops/config")
(declare-function +language-server/restart "tools/lsp-ops/config")
(declare-function +language-server/shutdown "tools/lsp-ops/config")
(declare-function +language-server/open-log "tools/lsp-ops/config")
(declare-function +language-server/describe-session "tools/lsp-ops/config")
(declare-function +language-server/show-workspace-configuration "tools/lsp-ops/config")
(declare-function +language-server-current-toolchain-profile "tools/lsp/toolchain" (&optional buffer))
(declare-function +language-server-toolchain-description "tools/lsp/toolchain" (&optional buffer))
(declare-function +language-server/select-toolchain "tools/lsp/toolchain")
(declare-function +language-server/reset-toolchain "tools/lsp/toolchain")
(declare-function +language-server/refresh-toolchains "tools/lsp/toolchain")
(declare-function +problems/buffer "checkers/problems/config")
(declare-function +problems/project "checkers/problems/config")
(declare-function +diagnostics/buffer-ui "checkers/diagnostics/config")
(declare-function +diagnostics/project-ui "checkers/diagnostics/config")
(declare-function diagnostics-menu "checkers/diagnostics-extra/config")
(declare-function lsp-inlay-hints-mode "lsp-mode" (&optional arg))

(define-derived-mode +language-server-manager-mode aaron-ui-board-mode "Lang-Server-Hub"
  "Major mode for the language-server dashboard.")

(define-derived-mode +language-server-doctor-mode aaron-ui-board-mode "Lang-Server-Doctor"
  "Major mode for the language-server doctor report.")

(defun +language-server-manager--assert-view-buffer ()
  "Signal unless the current buffer is the language-server Hub."
  (unless (derived-mode-p '+language-server-manager-mode)
    (user-error "Refusing to render Language Server Hub into %s" (buffer-name))))

(defun +language-server-doctor--assert-view-buffer ()
  "Signal unless the current buffer is the language-server Doctor."
  (unless (derived-mode-p '+language-server-doctor-mode)
    (user-error "Refusing to render Language Server Doctor into %s" (buffer-name))))

(defun +language-server--source-buffer ()
  "Return the source buffer associated with the current view."
  (if (and (boundp '+language-server-manager-source-buffer)
           (buffer-live-p +language-server-manager-source-buffer))
      +language-server-manager-source-buffer
    (current-buffer)))

(defun +language-server--source-buffer-or-error ()
  "Return the current source buffer or signal a user error."
  (let ((buffer (+language-server--source-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "No live source buffer is associated with this view"))
    buffer))

(defun +language-server--clear-source-buffer-references-h ()
  "Clear manager views that point at the buffer being killed."
  (let ((source (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (or (derived-mode-p '+language-server-manager-mode)
                         (derived-mode-p '+language-server-doctor-mode))
                     (eq +language-server-manager-source-buffer source))
            (setq-local +language-server-manager-source-buffer nil)))))))

(defun +language-server--watch-source-buffer (source)
  "Install source cleanup for language-server manager views."
  (when (buffer-live-p source)
    (with-current-buffer source
      (add-hook 'kill-buffer-hook
                #'+language-server--clear-source-buffer-references-h nil t))))

(defun +language-server--view-refresh (buffer)
  "Refresh language-server BUFFER when it is a managed view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((derived-mode-p '+language-server-manager-mode)
        (+language-server/manager-refresh))
       ((derived-mode-p '+language-server-doctor-mode)
        (+language-server/doctor-refresh))))))

(defun +language-server--call-in-source-buffer (command &optional refresh)
  "Call COMMAND in the associated source buffer.
When REFRESH is non-nil, refresh the current hub/doctor view afterwards."
  (let ((source (+language-server--source-buffer-or-error))
        (view (current-buffer)))
    (with-current-buffer source
      (call-interactively command))
    (when refresh
      (+language-server--view-refresh view))))

(defun +language-server--mode-list (modes)
  "Normalize MODES into a list of major-mode symbols."
  (cond
   ((null modes) nil)
   ((symbolp modes) (list modes))
   ((listp modes) modes)
   (t nil)))

(defun +language-server--format-mode-list (modes)
  "Format MODES as a comma-separated string."
  (string-join (mapcar #'symbol-name (+language-server--mode-list modes)) ", "))

(defun +language-server--library-path (library)
  "Return the path of LIBRARY when it can be located."
  (ignore-errors
    (locate-library library)))

(defun +language-server--feature-library-path (feature)
  "Return FEATURE's loadable library path when possible."
  (when feature
    (+language-server--library-path (symbol-name feature))))

(defun +language-server--feature-status (feature)
  "Return a short status string for FEATURE."
  (cond
   ((null feature) "ready")
   ((featurep feature) "loaded")
   ((+language-server--feature-library-path feature) "available")
   (t "missing")))

(defun +language-server--project-root (&optional buffer)
  "Return the project root for BUFFER, or nil when unavailable."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((project (project-current nil default-directory)))
      (expand-file-name (project-root project)))))

(defun +language-server--entry-matches-buffer-p (entry &optional buffer)
  "Return non-nil when ENTRY matches BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((modes (+language-server--mode-list (plist-get entry :modes))))
      (and modes
           (apply #'derived-mode-p modes)))))

(defun +language-server--current-entry (&optional buffer)
  "Return the registered language server matching BUFFER, if any."
  (with-current-buffer (or buffer (current-buffer))
    (cl-find-if #'+language-server--entry-matches-buffer-p
                (+language-server-program-entries))))

(defun +language-server--current-policy (&optional buffer)
  "Return a short description of the expected backend policy for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((active (and (fboundp '+current-language-server-backend)
                       (+current-language-server-backend)))
          (override (and (fboundp '+language-server-project-backend-override)
                         (+language-server-project-backend-override))))
      (cond
       ((eq override 'disabled)
        "disabled (project-local override)")
       ((eq override 'lsp-mode)
        (if (eq active 'lsp-mode)
            "lsp-mode (active project-local override)"
          "lsp-mode (project-local override)"))
       ((+language-server--current-entry)
        (if (eq active 'lsp-mode)
            "lsp-mode (active registered route)"
          "lsp-mode (registered route)"))
       ((eq active 'lsp-mode) "lsp-mode (active)")
       ((derived-mode-p 'prog-mode) "lsp-mode (prog-mode default)")
       (t "manual / unknown")))))

(defun +language-server--source-status (&optional source)
  "Collect display status for SOURCE without inserting into that buffer."
  (let ((source (or source (+language-server--source-buffer))))
    (when (buffer-live-p source)
      (with-current-buffer source
        (let* ((entry (+language-server--current-entry source))
               (feature (and entry (plist-get entry :feature)))
               (active (and (fboundp '+current-language-server-backend)
                            (+current-language-server-backend)))
               (runtime (and (boundp '+language-server-runtime-current)
                             +language-server-runtime-current))
               (project-root (+language-server--project-root source))
               (workspace-set
                (local-variable-p '+language-server--workspace-configuration source))
               (workspace (and workspace-set
                               +language-server--workspace-configuration)))
          (list :buffer (buffer-name source)
                :file (or (and buffer-file-name
                               (abbreviate-file-name buffer-file-name))
                          "-")
                :major-mode major-mode
                :default-directory (abbreviate-file-name default-directory)
                :project-root (if project-root
                                  (abbreviate-file-name project-root)
                                "-")
                :policy (+language-server--current-policy source)
                :active-backend (or active "-")
                :required-feature (or feature "-")
                :feature-status (+language-server--feature-status feature)
                :server-match (if entry
                                  (or (plist-get entry :label)
                                      (+language-server--format-mode-list
                                       (plist-get entry :modes)))
                                "-")
                :flymake (if (bound-and-true-p flymake-mode) "on" "off")
                :company (if (bound-and-true-p company-mode) "on" "off")
                :breadcrumb (if (bound-and-true-p breadcrumb-local-mode) "on" "off")
                :workspace-set workspace-set
                :toolchain (and (fboundp '+language-server-toolchain-description)
                                (+language-server-toolchain-description source))
                :toolchain-id
                (and (fboundp '+language-server-current-toolchain-profile)
                     (plist-get (+language-server-current-toolchain-profile source) :id))
                :runtime-state
                (if (boundp '+language-server-runtime-state)
                    +language-server-runtime-state
                  'unavailable)
                :runtime
                (and (fboundp '+language-server-runtime-description)
                     (+language-server-runtime-description source))
                :runtime-id
                (and runtime (+language-server-runtime-id runtime))
                :runtime-provider
                (and runtime (+language-server-runtime-provider runtime))
                :runtime-target
                (and runtime
                     (plist-get (+language-server-runtime-metadata runtime)
                                :target))
                :runtime-fallback
                (and (boundp '+language-server-runtime-error)
                     (+language-server-runtime-fallback-text
                      +language-server-runtime-error))
                :workspace (and workspace
                                (string-trim-right
                                 (pp-to-string workspace)))))))))

(defun +language-server--openable-path (path)
  "Return PATH abbreviated for display."
  (abbreviate-file-name (aaron-open-resolve-file-name path)))

(defun +language-server--insert-button (label action help)
  "Insert a text button with LABEL, ACTION, and HELP."
  (insert-text-button
   label
   'action action
   'follow-link t
   'help-echo help))

(defun +language-server--insert-openable-path (path)
  "Insert PATH as a button that opens it."
  (aaron-ui-board-insert-openable-path path (+language-server--openable-path path)))

(defun +language-server--entry-source (entry)
  "Return ENTRY's source file path, if any."
  (plist-get entry :source))

(defun +language-server-manager--current-entry ()
  "Return the manager entry at point, if any."
  (or (get-text-property (point) '+language-server-entry)
      (get-text-property (line-beginning-position) '+language-server-entry)))

(defun +language-server-manager--set-entry-properties (start end entry)
  "Mark the region between START and END with manager ENTRY."
  (add-text-properties start end
                       `(+language-server-entry ,entry
                                                  local-map ,+language-server-manager-entry-mouse-map
                                                  keymap ,+language-server-manager-entry-mouse-map
                                                  mouse-face highlight
                                                  help-echo "RET/mouse-1: context action, mouse-3: menu")))

(defun +language-server/manager-open-source ()
  "Open the source file referenced by the entry at point."
  (interactive)
  (let* ((entry (+language-server-manager--current-entry))
         (source (and entry (+language-server--entry-source entry))))
    (unless source
      (user-error "No source file is attached to the entry at point"))
    (aaron-open-visit-file (aaron-open-resolve-file-name source))))

(defun +language-server/manager-context-action ()
  "Run the default action for the entry at point."
  (interactive)
  (if-let* ((entry (+language-server-manager--current-entry))
            (source (+language-server--entry-source entry)))
      (aaron-open-visit-file (aaron-open-resolve-file-name source))
    (+language-server/manager-ensure)))

(defun +language-server-manager--mouse-set-point (event)
  "Move point to the language-server view row clicked by EVENT."
  (let* ((posn (and event (event-start event)))
         (window (and posn (posn-window posn)))
         (point (and posn (posn-point posn))))
    (when (window-live-p window)
      (select-window window))
    (when (integer-or-marker-p point)
      (goto-char point))))

(defun +language-server/manager-entry-mouse-action (event)
  "Run the default row action for the language-server entry under EVENT."
  (interactive "e")
  (+language-server-manager--mouse-set-point event)
  (+language-server/manager-context-action))

(defun +language-server-manager--menu-item (label command &optional enabled)
  "Return a popup menu item for LABEL and COMMAND."
  (vector label command (and (fboundp command) enabled)))

(defun +language-server-manager--entry-menu-items (entry)
  "Return right-click menu items for language-server ENTRY."
  (let ((source (and entry (+language-server--entry-source entry))))
    (list
     (+language-server-manager--menu-item
      "Context action" '+language-server/manager-context-action t)
     (+language-server-manager--menu-item
      "Open entry source" '+language-server/manager-open-source source)
     (list
      "Current Buffer"
      (+language-server-manager--menu-item
       "Ensure / connect" '+language-server/manager-ensure t)
      (+language-server-manager--menu-item
       "Restart" '+language-server/manager-restart t)
      (+language-server-manager--menu-item
       "Shutdown" '+language-server/manager-shutdown t)
      (+language-server-manager--menu-item
       "Open log" '+language-server/manager-open-log t)
      (+language-server-manager--menu-item
       "Describe session" '+language-server/manager-describe-session t)
      (+language-server-manager--menu-item
       "Workspace config" '+language-server/manager-show-workspace-configuration t))
     (list
      "Edits / Diagnostics"
      (+language-server-manager--menu-item
       "Code actions" '+language-server/manager-code-actions t)
      (+language-server-manager--menu-item
       "Organize imports" '+language-server/manager-organize-imports t)
      (+language-server-manager--menu-item
       "Format buffer" '+language-server/manager-format-buffer t)
      (+language-server-manager--menu-item
       "Rename symbol" '+language-server/manager-rename t)
      (+language-server-manager--menu-item
       "Buffer problems" '+language-server/manager-problems-buffer t)
      (+language-server-manager--menu-item
       "Project problems" '+language-server/manager-problems-project t))
     (list
      "Views"
      (+language-server-manager--menu-item
       "Refresh" '+language-server/manager-refresh t)
      (+language-server-manager--menu-item
       "Hub" '+language-server/manager t)
      (+language-server-manager--menu-item
       "Doctor" '+language-server/doctor t)
      (+language-server-manager--menu-item
       "Dispatch" 'language-server-menu t)
      (+language-server-manager--menu-item
       "Docs" '+language-server/manager-open-docs t)))))

(defun +language-server-manager--view-menu-items ()
  "Return right-click menu items for language-server Hub/Doctor views."
  (let ((refresh-command
         (if (derived-mode-p '+language-server-doctor-mode)
             '+language-server/doctor-refresh
           '+language-server/manager-refresh)))
    (list
     (list
      "Views"
      (+language-server-manager--menu-item
       "Refresh" refresh-command t)
      (+language-server-manager--menu-item
       "Hub" '+language-server/manager t)
      (+language-server-manager--menu-item
       "Doctor" '+language-server/doctor t)
      (+language-server-manager--menu-item
       "Dispatch" 'language-server-menu t)
      (+language-server-manager--menu-item
       "Docs" '+language-server/manager-open-docs t))
     (list
      "Lifecycle"
      (+language-server-manager--menu-item
       "Ensure / connect" '+language-server/manager-ensure t)
      (+language-server-manager--menu-item
       "Restart" '+language-server/manager-restart t)
      (+language-server-manager--menu-item
       "Shutdown" '+language-server/manager-shutdown t)
      (+language-server-manager--menu-item
       "Open log" '+language-server/manager-open-log t)
      (+language-server-manager--menu-item
       "Describe session" '+language-server/manager-describe-session t))
     (list
      "Edits"
      (+language-server-manager--menu-item
       "Code actions" '+language-server/manager-code-actions t)
      (+language-server-manager--menu-item
       "Organize imports" '+language-server/manager-organize-imports t)
      (+language-server-manager--menu-item
       "Format buffer" '+language-server/manager-format-buffer t)
      (+language-server-manager--menu-item
       "Rename symbol" '+language-server/manager-rename t))
     (list
      "Diagnostics"
      (+language-server-manager--menu-item
       "Buffer problems" '+language-server/manager-problems-buffer t)
      (+language-server-manager--menu-item
       "Project problems" '+language-server/manager-problems-project t)
      (+language-server-manager--menu-item
       "Buffer diagnostics UI" '+language-server/manager-diagnostics-buffer-ui t)
      (+language-server-manager--menu-item
       "Project diagnostics UI" '+language-server/manager-diagnostics-project-ui t)
      (+language-server-manager--menu-item
       "Diagnostics menu" '+language-server/manager-diagnostics-menu t)))))

(defun +language-server/manager-popup-menu (event)
  "Show a right-click menu for language-server Hub/Doctor EVENT."
  (interactive "e")
  (+language-server-manager--mouse-set-point event)
  (let* ((entry (+language-server-manager--current-entry))
         (title (if entry "Language Server Entry" "Language Server"))
         (items (if entry
                    (+language-server-manager--entry-menu-items entry)
                  (+language-server-manager--view-menu-items))))
    (popup-menu (easy-menu-create-menu title items) event)))

(setq +language-server-manager-entry-mouse-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] #'+language-server/manager-entry-mouse-action)
        (define-key map [mouse-3] #'+language-server/manager-popup-menu)
        map))

(defun +language-server/manager-open-docs ()
  "Open the dedicated language-server workflow document."
  (interactive)
  (aaron-open-client-document +language-server-doc-file))

(defun +language-server--executable-lookup (name)
  "Resolve NAME in the associated source buffer's tooling environment.
Doctor and Hub views run in a dedicated buffer, so this must switch to
`+language-server--source-buffer' before resolving: the server it is
reporting on runs on that buffer's target, not on the client machine."
  (with-current-buffer (+language-server--source-buffer)
    (+language-server-executable-find name)))

(defun +language-server--executable-report (name)
  "Return a plist describing executable NAME on the source buffer's target."
  (let ((path (+language-server--executable-lookup name)))
    (list :name name
          :path path
          :ok (and path t))))

(defun +language-server--executable-summary (names)
  "Return a compact summary string for executable NAMES."
  (if names
      (string-join
       (mapcar
        (lambda (name)
          (format "%s=%s"
                  name
                  (if (+language-server--executable-lookup name) "ok" "missing")))
        names)
       ", ")
    "-"))

(defun +language-server--value-or-unset (symbol)
  "Return SYMBOL's value, or the string \"unset\" when it is unbound."
  (if (boundp symbol)
      (symbol-value symbol)
    "unset"))

(defun +language-server--enabled-value (symbol)
  "Return non-nil when SYMBOL is bound and its value is non-nil."
  (and (boundp symbol)
       (symbol-value symbol)))

(defun +language-server--toggle-default (symbol)
  "Toggle SYMBOL's default value and return the new value."
  (let ((value (not (+language-server--enabled-value symbol))))
    (set-default symbol value)
    value))

(defun +language-server--read-number-choice (name symbol choices fallback)
  "Read a numeric value for SYMBOL named NAME from CHOICES."
  (let* ((current (+language-server--value-or-unset symbol))
         (default (if (numberp current)
                      (format "%s" current)
                    fallback)))
    (string-to-number
     (completing-read
      (format "%s (current %s): " name current)
      choices nil t nil nil default))))

(defun +language-server--runtime-knob-entries ()
  "Return important runtime knob/value pairs."
  `(("read-process-output-max" . ,(+language-server--value-or-unset
                                   'read-process-output-max))
    ("lsp-keep-workspace-alive" . ,(+language-server--value-or-unset
                                    'lsp-keep-workspace-alive))
    ("lsp-restart" . ,(+language-server--value-or-unset 'lsp-restart))
    ("lsp-log-max" . ,(+language-server--value-or-unset 'lsp-log-max))
    ("lsp-log-io" . ,(+language-server--value-or-unset 'lsp-log-io))
    ("lsp-inlay-hint-enable" . ,(+language-server--value-or-unset
                                 'lsp-inlay-hint-enable))
    ("flymake-no-changes-timeout" . ,(+language-server--value-or-unset
                                      'flymake-no-changes-timeout))))

(defun +language-server--maybe-refresh-current-view ()
  "Refresh the current view when it is a hub or doctor buffer."
  (cond
   ((derived-mode-p '+language-server-manager-mode)
    (+language-server/manager-refresh))
   ((derived-mode-p '+language-server-doctor-mode)
    (+language-server/doctor-refresh))))

(defun +language-server--sync-source-buffer-inlay-hints ()
  "Apply the current inlay-hint preference to the source buffer."
  (let ((source (+language-server--source-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (and (fboundp '+current-language-server-backend)
                   (+current-language-server-backend)
                   (fboundp 'lsp-inlay-hints-mode))
          (lsp-inlay-hints-mode
           (if (+language-server--enabled-value 'lsp-inlay-hint-enable)
               1
             -1)))))))

(defun +language-server/cycle-restart-policy ()
  "Cycle `lsp-restart' between its three supported policies."
  (interactive)
  (setq-default lsp-restart
                (pcase (default-value 'lsp-restart)
                  ('interactive 'auto-restart)
                  ('auto-restart 'ignore)
                  (_ 'interactive)))
  (when (fboundp 'config-set)
    (ignore-errors (config-set 'lsp-restart (default-value 'lsp-restart))))
  (message "lsp-restart: %s" (default-value 'lsp-restart))
  (+language-server--maybe-refresh-current-view))

(defun +language-server/toggle-keep-workspace-alive ()
  "Toggle `lsp-keep-workspace-alive' for the current Emacs session."
  (interactive)
  (message "lsp-keep-workspace-alive: %s"
           (+language-server--toggle-default 'lsp-keep-workspace-alive))
  (+language-server--maybe-refresh-current-view))

(defun +language-server/toggle-lsp-log-io ()
  "Toggle `lsp-log-io' for the current Emacs session."
  (interactive)
  (message "lsp-log-io: %s"
           (+language-server--toggle-default 'lsp-log-io))
  (+language-server--maybe-refresh-current-view))

(defun +language-server/toggle-inlay-hints ()
  "Toggle `lsp-inlay-hint-enable' and sync the source buffer."
  (interactive)
  (+language-server--toggle-default 'lsp-inlay-hint-enable)
  (+language-server--sync-source-buffer-inlay-hints)
  (message "lsp-inlay-hint-enable: %s" lsp-inlay-hint-enable)
  (+language-server--maybe-refresh-current-view))

(defun +language-server/set-read-process-output-max (value)
  "Set `read-process-output-max' to VALUE for the current session."
  (interactive
   (list
    (+language-server--read-number-choice
     "read-process-output-max"
     'read-process-output-max
     '("65536" "262144" "524288" "1048576" "2097152")
     "1048576")))
  (setq-default read-process-output-max value)
  (message "read-process-output-max: %s" read-process-output-max)
  (+language-server--maybe-refresh-current-view))

(defun +language-server/set-log-max (value)
  "Set `lsp-log-max' to VALUE for the current session."
  (interactive
   (list
    (+language-server--read-number-choice
     "lsp-log-max"
     'lsp-log-max
     '("0" "2000" "10000" "50000" "200000")
     "10000")))
  (setq-default lsp-log-max value)
  (message "lsp-log-max: %s" lsp-log-max)
  (+language-server--maybe-refresh-current-view))

(defun +language-server/manager-ensure ()
  "Ensure the preferred language-server backend for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/ensure t))

(defun +language-server/manager-restart ()
  "Restart the language server for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/restart t))

(defun +language-server/manager-shutdown ()
  "Shutdown the language server for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/shutdown t))

(defun +language-server/manager-open-log ()
  "Open the language-server log buffer for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/open-log))

(defun +language-server/manager-describe-session ()
  "Describe the active language-server session for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/describe-session))

(defun +language-server/manager-show-workspace-configuration ()
  "Show the active workspace configuration for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer
   #'+language-server/show-workspace-configuration))

(defun +language-server/manager-select-toolchain ()
  "Select a toolchain for the associated source project."
  (interactive)
  (+language-server--call-in-source-buffer
   #'+language-server/select-toolchain t))

(defun +language-server/manager-reset-toolchain ()
  "Reset the associated source project's session toolchain."
  (interactive)
  (+language-server--call-in-source-buffer
   #'+language-server/reset-toolchain t))

(defun +language-server/manager-refresh-toolchains ()
  "Refresh discovered toolchains for the associated source project."
  (interactive)
  (+language-server--call-in-source-buffer
   #'+language-server/refresh-toolchains t))

(defun +language-server/manager-organize-imports ()
  "Organize imports for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/organize-imports t))

(defun +language-server/manager-code-actions ()
  "Run code actions for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/code-actions t))

(defun +language-server/manager-format-buffer ()
  "Format the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/format-buffer t))

(defun +language-server/manager-rename ()
  "Rename the symbol at point in the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+language-server/rename t))

(defun +language-server/manager-problems-buffer ()
  "Open the buffer-local problems view for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+problems/buffer))

(defun +language-server/manager-problems-project ()
  "Open the project-wide problems view for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+problems/project))

(defun +language-server/manager-diagnostics-buffer-ui ()
  "Open the diagnostics UI for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+diagnostics/buffer-ui))

(defun +language-server/manager-diagnostics-project-ui ()
  "Open Flymake's project diagnostics list for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'+diagnostics/project-ui))

(defun +language-server/manager-diagnostics-menu ()
  "Open the diagnostics dispatch menu for the source buffer."
  (interactive)
  (+language-server--call-in-source-buffer #'diagnostics-menu))

(defun +language-server-manager-insert-maintenance-section ()
  "Insert the top-level maintenance section."
  (+language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Maintenance")
  (aaron-ui-board-insert-field
   "registered servers"
   (number-to-string (length (+language-server-program-entries))))
  (when (boundp '+language-server-toolchain-providers)
    (aaron-ui-board-insert-field
     "toolchain providers"
     (number-to-string (length +language-server-toolchain-providers))))
  (insert "   ")
  (aaron-ui-board-insert-actions
   '((:label "Doctor"    :command +language-server/doctor    :help "Doctor report")
     (:label "Dispatch"  :command language-server-menu  :help "Transient menu")
     (:label "Docs"      :command +language-server/manager-open-docs :help "Workflow docs")
     (:label "Problems"  :command +language-server/manager-problems-project :help "Project problems")))
  (insert "\n\n"))

(defun +language-server-manager--insert-settings-section ()
  "Insert quick runtime settings for the language-server stack."
  (+language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Quick Settings")
  (aaron-ui-board-insert-field
   "lsp-restart"
   (format "%s" (+language-server--value-or-unset 'lsp-restart)))
  (aaron-ui-board-insert-field
   "lsp-keep-workspace-alive"
   (format "%s" (+language-server--value-or-unset 'lsp-keep-workspace-alive)))
  (aaron-ui-board-insert-field
   "lsp-log-io"
   (format "%s" (+language-server--value-or-unset 'lsp-log-io)))
  (aaron-ui-board-insert-field
   "lsp-inlay-hints"
   (format "%s" (+language-server--value-or-unset 'lsp-inlay-hint-enable)))
  (aaron-ui-board-insert-field
   "read-process-output-max"
   (format "%s" (+language-server--value-or-unset 'read-process-output-max)))
  (aaron-ui-board-insert-field
   "lsp-log-max"
   (format "%s" (+language-server--value-or-unset 'lsp-log-max)))
  (insert "   ")
  (aaron-ui-board-insert-actions
   '((:label "Restart Policy" :command +language-server/cycle-restart-policy
              :help "Cycle lsp-restart policy")
     (:label "Keep Alive"    :command +language-server/toggle-keep-workspace-alive
              :help "Toggle lsp-keep-workspace-alive")
     (:label "Log IO"        :command +language-server/toggle-lsp-log-io
              :help "Toggle lsp-mode wire logging")
     (:label "Inlay Hints"   :command +language-server/toggle-inlay-hints
              :help "Toggle inlay hints")))
  (insert " ")
  (aaron-ui-board-insert-action
   "Output Max" #'+language-server/set-read-process-output-max "Set read-process-output-max")
  (insert " ")
  (aaron-ui-board-insert-action
   "Log Max" #'+language-server/set-log-max "Set lsp-log-max")
  (insert "\n\n"))

(defun +language-server-manager--insert-current-buffer-section ()
  "Insert the current-buffer status section."
  (+language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Current Buffer")
  (if-let* ((status (+language-server--source-status)))
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
        (aaron-ui-board-insert-field "server" (plist-get status :server-match))
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
         '((:label "Ensure"    :command +language-server/manager-ensure    :primary t
                   :help "Ensure backend")
           (:label "Restart"   :command +language-server/manager-restart   :help "Restart server")
           (:label "Shutdown"  :command +language-server/manager-shutdown  :help "Shutdown server")
           (:label "Log"       :command +language-server/manager-open-log  :help "Open log")
           (:label "Session"   :command +language-server/manager-describe-session :help "Session info")
           (:label "Config"    :command +language-server/manager-show-workspace-configuration
                   :help "Workspace config")
           (:label "Toolchain" :command +language-server/manager-select-toolchain
                   :help "Select project toolchain")
           (:label "Reset TC"  :command +language-server/manager-reset-toolchain
                   :help "Reset session toolchain")
           (:label "Actions"   :command +language-server/manager-code-actions   :help "Code actions")
           (:label "Format"    :command +language-server/manager-format-buffer  :help "Format buffer")
           (:label "Rename"    :command +language-server/manager-rename         :help "Rename symbol")))
        (insert "\n\n"))
    (aaron-ui-board-insert-empty "No source buffer.")))

(defun +language-server-manager--insert-routing-section ()
  "Insert the language servers registered by `set-language-server!'."
  (+language-server-manager--assert-view-buffer)
  (let ((entries (+language-server-program-entries)))
    (aaron-ui-board-insert-section "Registered Language Servers" (length entries))
    (if entries
        (dolist (entry entries)
          (let* ((modes (plist-get entry :modes))
                 (label (or (plist-get entry :label)
                            (format "%s" (plist-get entry :server-id))))
                 (feature (plist-get entry :feature))
                 (executables (plist-get entry :executables))
                 (placement (plist-get entry :placement))
                 (source (plist-get entry :source))
                 (note (plist-get entry :note))
                 (current (+language-server--entry-matches-buffer-p
                           entry
                           (+language-server--source-buffer)))
                 (start (point)))
            (aaron-ui-board-insert-field
             (+language-server--format-mode-list modes)
             (concat label (if current "  ●current" ""))
             (if current 'aaron-ui-board-badge-info nil))
            (aaron-ui-board-insert-field
             "placement" (format "%s" (or placement 'target)))
            (when feature
              (aaron-ui-board-insert-field
               "feature"
               (format "%s  %s" feature
                       (+language-server--feature-status feature))))
            (aaron-ui-board-insert-field
             "executables"
             (+language-server--executable-summary executables))
            (when source
              (insert "   " (propertize "source  " 'face 'aaron-ui-board-meta))
              (+language-server--insert-openable-path source)
              (insert "\n"))
            (when note
              (insert "   " (propertize note 'face 'aaron-ui-board-detail) "\n"))
            (when source
              (insert "   ")
              (aaron-ui-board-insert-action
               "Open source"
               (lambda ()
                 (aaron-open-visit-file (aaron-open-resolve-file-name source)))
               "Open the file registering this server")
              (insert "\n"))
            (insert "\n")
            (+language-server-manager--set-entry-properties
             start (point)
             (list :kind 'server :source source :modes modes))))
      (aaron-ui-board-insert-empty "No language servers are registered."))))

(defun +language-server-manager--insert-runtime-knobs ()
  "Insert the runtime knobs section."
  (+language-server-manager--assert-view-buffer)
  (aaron-ui-board-insert-section "Runtime Knobs")
  (dolist (entry (+language-server--runtime-knob-entries))
    (aaron-ui-board-insert-field (car entry) (format "%s" (or (cdr entry) "-"))))
  (insert "\n"))

(defun +language-server/manager-refresh ()
  "Refresh the language-server dashboard."
  (interactive)
  (+language-server-manager--assert-view-buffer)
  (let ((inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Language Server Hub"
        :icon 'server
        :actions '((:label "Doctor"   :command +language-server/doctor   :help "Doctor report")
                   (:label "Dispatch" :command language-server-menu :help "Transient menu" :primary t)
                   (:label "Docs"     :command +language-server/manager-open-docs :help "Workflow docs")))
       (+language-server-manager-insert-maintenance-section)
       (run-hooks '+language-server-manager-extra-section-functions)
       (+language-server-manager--insert-current-buffer-section)
       (+language-server-manager--insert-settings-section)
       (+language-server-manager--insert-routing-section)
       (+language-server-manager--insert-runtime-knobs)
       (aaron-ui-board-insert-key-hints
        "Keys: g refresh  i toolchain  x reset-toolchain  v rescan-toolchains  e ensure  r restart  k shutdown  l log  s session  c config  o imports  a actions  f format  R rename  p problems  D doctor  q quit")))))

(defun +language-server--doctor-insert-libraries ()
  "Insert the library availability section."
  (+language-server-doctor--assert-view-buffer)
  (aaron-ui-board-insert-section "Libraries")
  (dolist (library '("lsp-mode" "lsp-ui" "company" "company-box"
                     "company-prescient" "flymake-diagnostic-at-point"
                     "sideline" "sideline-lsp"
                     "breadcrumb" "dape"))
    (let ((path (+language-server--library-path library)))
      (if path
          (progn
            (insert "   "
                    (propertize (format "%-24s" library) 'face 'aaron-ui-board-meta))
            (+language-server--insert-openable-path path)
            (insert "\n"))
        (aaron-ui-board-insert-field library "MISSING" 'aaron-ui-board-bad))))
  (insert "\n"))

(defun +language-server--doctor-insert-executables ()
  "Insert the executable availability section."
  (+language-server-doctor--assert-view-buffer)
  (let ((names (delete-dups
                (apply #'append
                       (delq nil
                             (mapcar (lambda (entry)
                                       (copy-sequence
                                        (plist-get entry :executables)))
                                     (+language-server-program-entries)))))))
    (aaron-ui-board-insert-section "Executables" (length names))
    (if names
        (dolist (name names)
          (let ((report (+language-server--executable-report name)))
            (if-let* ((path (plist-get report :path)))
                (progn
                  (insert "   "
                          (propertize (format "%-24s" (plist-get report :name))
                                      'face 'aaron-ui-board-meta))
                  (+language-server--insert-openable-path path)
                  (insert "\n"))
              (aaron-ui-board-insert-field name "MISSING" 'aaron-ui-board-bad))))
      (aaron-ui-board-insert-empty "No custom server executables are registered."))
    (insert "\n")))

(defun +language-server--doctor-insert-current-buffer (source)
  "Insert a current-buffer report for SOURCE."
  (+language-server-doctor--assert-view-buffer)
  (aaron-ui-board-insert-section "Current Buffer")
  (if-let* ((status (+language-server--source-status source)))
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
        (aaron-ui-board-insert-field "server"          (plist-get status :server-match))
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

(defun +language-server--doctor-insert-routing ()
  "Insert the registered language-server summary."
  (+language-server-doctor--assert-view-buffer)
  (let ((entries (+language-server-program-entries)))
    (aaron-ui-board-insert-section "Routing Summary" (length entries))
    (aaron-ui-board-insert-field
     "registered servers" (number-to-string (length entries)))
    (dolist (entry entries)
      (insert "   "
              (propertize (format "  %-36s " (+language-server--format-mode-list
                                              (plist-get entry :modes)))
                          'face 'aaron-ui-board-detail)
              (propertize (format "placement=%-8s feature=%-12s executables=%s"
                                  (or (plist-get entry :placement) 'target)
                                  (or (plist-get entry :feature) "-")
                                  (+language-server--executable-summary
                                   (plist-get entry :executables)))
                          'face 'aaron-ui-board-meta)
              "\n"))
    (insert "\n")))

(defun +language-server--doctor-insert-runtime-knobs ()
  "Insert runtime knob values."
  (+language-server-doctor--assert-view-buffer)
  (aaron-ui-board-insert-section "Runtime Knobs")
  (dolist (entry (+language-server--runtime-knob-entries))
    (aaron-ui-board-insert-field (car entry) (format "%s" (or (cdr entry) "-"))))
  (insert "\n"))

(defun +language-server/doctor-refresh ()
  "Refresh the current language-server doctor buffer."
  (interactive)
  (+language-server-doctor--assert-view-buffer)
  (let ((source (+language-server--source-buffer))
        (inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Language Server Doctor"
        :icon 'diagnostics
        :actions '((:label "Hub"      :command +language-server/manager  :help "Open Hub"  :primary t)
                   (:label "Dispatch" :command language-server-menu :help "Transient menu")))
       (+language-server--doctor-insert-libraries)
       (+language-server--doctor-insert-executables)
       (+language-server--doctor-insert-current-buffer source)
       (+language-server--doctor-insert-routing)
       (+language-server--doctor-insert-runtime-knobs)
       (aaron-ui-board-insert-key-hints
        "Keys: g refresh  h hub  e ensure  r restart  k shutdown  l log  s session  D doctor  q quit")))))

(defun +language-server/doctor ()
  "Open a doctor report for the language-server stack."
  (interactive)
  (let ((buffer (get-buffer-create +language-server-doctor-buffer-name))
        (source (+language-server--source-buffer)))
    (with-current-buffer buffer
      (+language-server-doctor-mode)
      (aaron-ui-board-set-header "Language Server Doctor" 'diagnostics)
      (setq-local +language-server-manager-source-buffer source)
      (setq-local aaron-ui-board-refresh-function #'+language-server/doctor-refresh)
      (+language-server--watch-source-buffer source)
      (let ((inhibit-read-only t))
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "g") #'+language-server/doctor-refresh)
        (local-set-key (kbd "h") #'+language-server/manager)
        (local-set-key (kbd "e") #'+language-server/manager-ensure)
        (local-set-key (kbd "r") #'+language-server/manager-restart)
        (local-set-key (kbd "k") #'+language-server/manager-shutdown)
        (local-set-key (kbd "l") #'+language-server/manager-open-log)
        (local-set-key (kbd "s") #'+language-server/manager-describe-session)
        (local-set-key (kbd "c") #'+language-server/manager-show-workspace-configuration)
        (local-set-key (kbd "D") #'+language-server/doctor)
        (local-set-key (kbd "A") #'+language-server/cycle-restart-policy)
        (local-set-key (kbd "S") #'+language-server/toggle-keep-workspace-alive)
        (local-set-key (kbd "L") #'+language-server/toggle-lsp-log-io)
        (local-set-key (kbd "I") #'+language-server/toggle-inlay-hints)
        (local-set-key (kbd "M") #'+language-server/set-read-process-output-max)
        (local-set-key (kbd "E") #'+language-server/set-log-max)
        (local-set-key (kbd "O") #'+language-server/manager-open-docs)
        (local-set-key (kbd "?") #'language-server-menu)
        (local-set-key [mouse-3] #'+language-server/manager-popup-menu)
        (+language-server/doctor-refresh)))
    (pop-to-buffer buffer)))

(defun +language-server-manager-setup-keys ()
  "Install local keybindings for the language-server hub."
  (local-set-key (kbd "g") #'+language-server/manager-refresh)
  (local-set-key (kbd "e") #'+language-server/manager-ensure)
  (local-set-key (kbd "r") #'+language-server/manager-restart)
  (local-set-key (kbd "k") #'+language-server/manager-shutdown)
  (local-set-key (kbd "l") #'+language-server/manager-open-log)
  (local-set-key (kbd "s") #'+language-server/manager-describe-session)
  (local-set-key (kbd "c") #'+language-server/manager-show-workspace-configuration)
  (local-set-key (kbd "i") #'+language-server/manager-select-toolchain)
  (local-set-key (kbd "x") #'+language-server/manager-reset-toolchain)
  (local-set-key (kbd "v") #'+language-server/manager-refresh-toolchains)
  (local-set-key (kbd "o") #'+language-server/manager-organize-imports)
  (local-set-key (kbd "a") #'+language-server/manager-code-actions)
  (local-set-key (kbd "f") #'+language-server/manager-format-buffer)
  (local-set-key (kbd "R") #'+language-server/manager-rename)
  (local-set-key (kbd "p") #'+language-server/manager-problems-buffer)
  (local-set-key (kbd "P") #'+language-server/manager-problems-project)
  (local-set-key (kbd "d") #'+language-server/manager-diagnostics-buffer-ui)
  (local-set-key (kbd "T") #'+language-server/manager-diagnostics-project-ui)
  (local-set-key (kbd "m") #'+language-server/manager-diagnostics-menu)
  (local-set-key (kbd "D") #'+language-server/doctor)
  (local-set-key (kbd "A") #'+language-server/cycle-restart-policy)
  (local-set-key (kbd "S") #'+language-server/toggle-keep-workspace-alive)
  (local-set-key (kbd "L") #'+language-server/toggle-lsp-log-io)
  (local-set-key (kbd "I") #'+language-server/toggle-inlay-hints)
  (local-set-key (kbd "M") #'+language-server/set-read-process-output-max)
  (local-set-key (kbd "E") #'+language-server/set-log-max)
  (local-set-key (kbd "O") #'+language-server/manager-open-docs)
  (local-set-key (kbd "?") #'language-server-menu)
  (local-set-key (kbd "RET") #'+language-server/manager-context-action)
  (local-set-key [mouse-3] #'+language-server/manager-popup-menu))

(defun +language-server/manager ()
  "Open the language-server dashboard."
  (interactive)
  (let ((buffer (get-buffer-create +language-server-manager-buffer-name))
        (source (+language-server--source-buffer)))
    (with-current-buffer buffer
      (+language-server-manager-mode)
      (aaron-ui-board-set-header "Language Server Hub" 'server)
      (setq-local +language-server-manager-source-buffer source)
      (setq-local aaron-ui-board-refresh-function #'+language-server/manager-refresh)
      (+language-server--watch-source-buffer source)
      (let ((inhibit-read-only t))
        (use-local-map (copy-keymap special-mode-map))
        (+language-server-manager-setup-keys)
        (run-hooks '+language-server-manager-setup-functions)
        (+language-server/manager-refresh)))
    (pop-to-buffer buffer)))

(transient-define-prefix language-server-menu ()
  "Language-server command surface."
  [["Views"
    ("h" "hub" +language-server/manager)
    ("D" "doctor" +language-server/doctor)
    ("O" "docs" +language-server/manager-open-docs)]
   ["Lifecycle"
    ("e" "ensure" +language-server/manager-ensure)
    ("r" "restart" +language-server/manager-restart)
    ("k" "shutdown" +language-server/manager-shutdown)
    ("l" "log" +language-server/manager-open-log)
    ("s" "session" +language-server/manager-describe-session)
    ("c" "config" +language-server/manager-show-workspace-configuration)]
   ["Toolchain"
    ("i" "select" +language-server/manager-select-toolchain)
    ("x" "reset" +language-server/manager-reset-toolchain)
    ("v" "rescan" +language-server/manager-refresh-toolchains)]
   ["Edits"
    ("a" "code actions" +language-server/manager-code-actions)
    ("o" "organize imports" +language-server/manager-organize-imports)
    ("f" "format" +language-server/manager-format-buffer)
    ("R" "rename" +language-server/manager-rename)]
   ["Tuning"
    ("A" "cycle restart policy" +language-server/cycle-restart-policy)
    ("S" "toggle keep-alive" +language-server/toggle-keep-workspace-alive)
    ("L" "toggle log-io" +language-server/toggle-lsp-log-io)
    ("I" "toggle inlay hints" +language-server/toggle-inlay-hints)
    ("M" "set output max" +language-server/set-read-process-output-max)
    ("E" "set log max" +language-server/set-log-max)]
   ["Diagnostics"
    ("p" "buffer problems" +language-server/manager-problems-buffer)
    ("P" "project problems" +language-server/manager-problems-project)
    ("d" "buffer diagnostics ui" +language-server/manager-diagnostics-buffer-ui)
    ("T" "Flymake project list" +language-server/manager-diagnostics-project-ui)
    ("m" "diagnostics menu" +language-server/manager-diagnostics-menu)
    ("x" "message handler errors" +language-server/message-errors)]])

;;; Message handler failures

(defconst +language-server-message-errors-buffer-name
  "*Language Server Message Errors*"
  "Buffer holding backtraces of failed server-message handlers.")

(defvar +language-server-message-errors-max 20
  "Most recent server-message handler failures kept with their backtraces.")

(defvar +language-server--message-error-count 0
  "Failures recorded in the current buffer generation.")

(defun +language-server--record-message-error (&rest arguments)
  "Record the backtrace of a failed server-message handler, then return.
Installed as `debugger' only inside `lsp--parser-on-message', whose
`with-demoted-errors' reports nothing but the error object.  Returning lets
that form handle the error exactly as before.  ARGUMENTS are the debugger's."
  (let ((trace (backtrace-to-string))
        (buffer (get-buffer-create
                 +language-server-message-errors-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "\f\n%F %T  ")
                (format "%S\n" (cadr arguments))
                trace "\n")
        ;; Keep only the most recent failures; each starts with a page break.
        (goto-char (point-max))
        (when (search-backward "\f" nil t
                               (1+ +language-server-message-errors-max))
          (delete-region (point-min) (point))))
      (setq buffer-read-only t))
    (cl-incf +language-server--message-error-count)
    nil))

(defun +language-server--parser-on-message-a (function &rest arguments)
  "Call FUNCTION with ARGUMENTS, keeping a backtrace of handler failures.
`debug-on-error' only reaches handlers declared with `debug', which is the
`with-demoted-errors' boundary itself; errors that inner code catches with
`ignore-errors' or `condition-case' are not recorded."
  (let ((debug-on-error t)
        (debugger #'+language-server--record-message-error))
    (apply function arguments)))

(with-eval-after-load 'lsp-mode
  (when (fboundp 'lsp--parser-on-message)
    (advice-add 'lsp--parser-on-message
                :around #'+language-server--parser-on-message-a)))

(defun +language-server/message-errors ()
  "Show backtraces of server messages whose handlers failed.
These are the \"Error processing message\" lines in *Messages*."
  (interactive)
  (if-let* ((buffer (get-buffer +language-server-message-errors-buffer-name)))
      (pop-to-buffer buffer)
    (message "No language-server message handler has failed")))

(define-leader-key!
  "c s" '(:def language-server-menu :which-key "language server"))

;; Boards start in Emacs state.
(set-evil-initial-state! '(+language-server-manager-mode +language-server-doctor-mode) 'emacs)

(provide 'tools/lsp-hub/config)
;;; tools/lsp-hub/config.el ends here

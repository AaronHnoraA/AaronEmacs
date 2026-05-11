;;; init-typst.el --- Typst editing and LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Typst buffers use `typst-ts-mode' with Eglot and tinymist.  Keep a small
;; local fallback mode for manually opened buffers, but `.typ' files default to
;; the package mode.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'compile)
(require 'cl-lib)
(require 'json)
(require 'websocket)

(declare-function my/eglot-ensure-unless-lsp-mode "init-lsp")
(declare-function my/eglot-set-workspace-configuration "init-lsp" (configuration))
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))
(declare-function eglot-current-server "eglot" ())
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-signal-didChangeConfiguration "eglot" (server))
(declare-function company-mode "company" (&optional arg))
(declare-function previewer-workbench "previewer" ())
(declare-function websocket-open "websocket" (url &rest args))
(declare-function websocket-close "websocket" (websocket))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-frame-text "websocket" (frame))
(autoload 'previewer-workbench "previewer" nil t)
(defvar company-backends)
(defvar company-idle-delay)
(defvar company-minimum-prefix-length)
(defvar previewer-external-url-function nil)
(defvar previewer-external-start-function nil)
(defvar previewer-external-browser-backend nil)
(defvar previewer-external-open-delay nil)
(defvar treesit-font-lock-level)

(defvar-local my/typst-preview--start-timer nil
  "Retry timer used while waiting for Tinymist Eglot startup.")
(defvar-local my/typst-preview--process nil
  "Tinymist preview process for the current Typst buffer.")
(defvar-local my/typst-preview--process-file nil
  "File currently served by `my/typst-preview--process'.")
(defvar-local my/typst-preview--process-args nil
  "Arguments currently used by `my/typst-preview--process'.")
(defvar-local my/typst-preview--control-host nil
  "Control-plane host reported by the current Tinymist preview process.")
(defvar-local my/typst-preview--socket nil
  "Websocket connected to Tinymist's control-plane server.")
(defvar-local my/typst-preview--sync-timer nil
  "Idle timer used to sync unsaved Typst buffer contents to Tinymist.")

(defgroup my/typst nil
  "Typst editing and language-server integration."
  :group 'languages)

(defcustom my/typst-lsp-executable "tinymist"
  "Executable used for Typst language-server support."
  :type 'string
  :group 'my/typst)

(defcustom my/typst-lsp-extra-font-paths nil
  "Extra font paths passed to `tinymist lsp --font-path'."
  :type '(repeat directory)
  :group 'my/typst)

(defcustom my/typst-preview-partial-rendering t
  "When non-nil, enable partial rendering in `tinymist preview'."
  :type 'boolean
  :group 'my/typst)

(defcustom my/typst-preview-invert-colors "never"
  "Color inversion policy passed to `tinymist preview --invert-colors'."
  :type '(choice (const "never")
                 (const "auto")
                 (const "always")
                 string)
  :group 'my/typst)

(defcustom my/typst-preview-math-font "GFS Neohellenic Math"
  "Preferred math font family used by Typst preview tools."
  :type 'string
  :group 'my/typst)

(defvar my/typst-preview--math-font-line-cache nil)

(defvaralias 'my/org-typst-preview-math-font 'my/typst-preview-math-font)
(defvaralias 'my/latex-preview-math-font 'my/typst-preview-math-font)

(defun my/typst-preview--preferred-math-font-file ()
  "Return the first preferred Typst math-font file available on this machine."
  (let ((candidates
         (list (expand-file-name "~/Library/Fonts/GFSNeohellenicMath.otf")
               (expand-file-name "~/Library/Fonts/STIXTwoMath-Regular.ttf")
               (expand-file-name "~/Library/Fonts/LibertinusMath-Regular.otf")
               "/System/Library/Fonts/Supplemental/STIXTwoMath.otf")))
    (or
     (catch 'font
       (dolist (candidate candidates)
         (when (file-exists-p candidate)
           (throw 'font candidate))))
     (when (executable-find "kpsewhich")
       (let ((lm-math
              (string-trim
               (shell-command-to-string "kpsewhich latinmodern-math.otf 2>/dev/null"))))
         (unless (string-empty-p lm-math)
           lm-math))))))

(defalias 'my/org-typst-preview--preferred-math-font-file
  #'my/typst-preview--preferred-math-font-file)
(defalias 'my/latex-preview--preferred-math-font-file
  #'my/typst-preview--preferred-math-font-file)

(defun my/typst-preview-math-font-line ()
  "Return the `\\setmathfont' line used by LaTeX export fallback snippets."
  (or my/typst-preview--math-font-line-cache
      (setq my/typst-preview--math-font-line-cache
            (if-let* ((font-file (my/typst-preview--preferred-math-font-file)))
                (format "\\setmathfont[Path=%s]{%s}"
                        (file-name-as-directory (file-name-directory font-file))
                        (file-name-nondirectory font-file))
              (format "\\setmathfont{%s}" my/typst-preview-math-font)))))

(defalias 'my/org-typst-preview-math-font-line
  #'my/typst-preview-math-font-line)
(defalias 'my/latex-preview-math-font-line
  #'my/typst-preview-math-font-line)

(defcustom my/typst-preview-host "127.0.0.1:23635"
  "Host and port used for Tinymist's official web preview."
  :type 'string
  :group 'my/typst)

(defcustom my/typst-preview-control-host "127.0.0.1:0"
  "Host and port used for Tinymist's control-plane websocket.
Use port 0 to let Tinymist choose a free port."
  :type 'string
  :group 'my/typst)

(defcustom my/typst-company-idle-delay 0.18
  "Idle seconds before Company requests Typst LSP completion."
  :type '(choice (const :tag "Disable idle popup" nil) number)
  :group 'my/typst)

(defcustom my/typst-preview-sync-delay 0.15
  "Idle seconds before syncing unsaved Typst buffer contents to Tinymist."
  :type 'number
  :group 'my/typst)

(defcustom my/typst-preview-center-source t
  "When non-nil, recenter source buffers after preview click synchronization."
  :type 'boolean
  :group 'my/typst)

(defcustom my/typst-preview-pop-log-on-error nil
  "When non-nil, display the Tinymist preview log after preview errors."
  :type 'boolean
  :group 'my/typst)

(defvar my/typst-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for the local Typst fallback mode.")

(defvar my/typst-font-lock-keywords
  '(("^=+\\s-+\\(.+\\)$" 1 font-lock-function-name-face)
    ("@[[:alnum:]_:-]+" . font-lock-constant-face)
    ("#\\([[:alpha:]_][[:alnum:]_-]*\\)" 1 font-lock-keyword-face)
    ("\\$\\([^$\n]+\\)\\$" 1 font-lock-variable-name-face))
  "Basic highlighting for the local Typst fallback mode.")

;;;###autoload
(define-derived-mode my/typst-mode prog-mode "Typst"
  "A lightweight fallback mode for Typst files."
  :syntax-table my/typst-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(my/typst-font-lock-keywords)))

(defun my/typst--candidate-executable-paths (program)
  "Return common absolute install locations for PROGRAM."
  (list (format "/opt/homebrew/bin/%s" program)
        (format "/usr/local/bin/%s" program)
        (expand-file-name (format "~/.nix-profile/bin/%s" program))
        (format "/etc/profiles/per-user/%s/bin/%s"
                (user-login-name)
                program)
        (format "/run/current-system/sw/bin/%s" program)))

(defun my/typst--find-executable (program)
  "Return PROGRAM's executable path, including GUI Emacs fallback paths."
  (or (executable-find program)
      (seq-find #'file-executable-p
                (my/typst--candidate-executable-paths program))
      (let* ((shell (or (getenv "SHELL") shell-file-name))
             (path
              (when (and shell (file-executable-p shell))
                (string-trim
                 (with-temp-buffer
                   (when (zerop
                          (call-process
                           shell nil t nil "-lc"
                           (format "command -v %s 2>/dev/null"
                                   (shell-quote-argument program))))
                     (buffer-string)))))))
        (and (stringp path)
             (not (string-empty-p path))
             path))))

(defun my/typst-tinymist-command ()
  "Return the tinymist executable path, or nil when unavailable."
  (my/typst--find-executable my/typst-lsp-executable))

(defun my/typst-lsp-font-paths ()
  "Return font paths that should be visible to tinymist."
  (delete-dups
   (delq nil
         (append
          (copy-sequence my/typst-lsp-extra-font-paths)
          (list
           (when (fboundp 'my/org-typst-preview--preferred-math-font-file)
             (when-let* ((font-file (my/org-typst-preview--preferred-math-font-file)))
               (file-name-directory font-file))))))))

(defun my/typst-eglot-server-command ()
  "Return the Eglot server command for tinymist."
  (let ((command (or (my/typst-tinymist-command)
                     my/typst-lsp-executable))
        (font-paths (seq-filter #'file-directory-p
                                (my/typst-lsp-font-paths))))
    (append (list command "lsp")
            (when font-paths
              (list "--font-path"
                    (mapconcat #'identity font-paths path-separator))))))

(defun my/typst-preview-args ()
  "Return Tinymist CLI preview launch arguments."
  (append
	   (list "preview"
	         "--no-open"
	         "--data-plane-host"
	         my/typst-preview-host
	         "--control-plane-host"
	         my/typst-preview-control-host
	         (format "--invert-colors=%s" my/typst-preview-invert-colors))
   (when my/typst-preview-partial-rendering
     (list "--partial-rendering=true"))
   (when-let* ((font-paths (seq-filter #'file-directory-p
                                       (my/typst-lsp-font-paths))))
     (list "--font-path"
           (mapconcat #'identity font-paths path-separator)))))

(defun my/typst-preview-url ()
  "Return the xwidget URL for Tinymist's official web preview."
  (format "http://%s" my/typst-preview-host))

(defun my/typst-previewer-setup ()
  "Route Previewer to Tinymist's official web preview for this buffer."
  (setq-local previewer-external-url-function #'my/typst-preview-url)
  (setq-local previewer-external-start-function #'my/typst-start-default-preview)
  (setq-local previewer-external-browser-backend 'system)
  (setq-local previewer-external-open-delay 0.8))

(defun my/typst-eglot-workspace-configuration ()
  "Return workspace configuration for Tinymist."
  `(:tinymist
    (:preview
     (:invertColors ,my/typst-preview-invert-colors
      :browsing
      (:args ,(vconcat
	               (list "--no-open"
	                     (format "--data-plane-host=%s" my/typst-preview-host)
	                     (format "--control-plane-host=%s"
	                             my/typst-preview-control-host)
	                     (format "--invert-colors=%s"
	                             my/typst-preview-invert-colors))))))))

(defun my/typst-eglot-setup ()
  "Configure and start Eglot for Typst buffers when tinymist is available."
  (when (fboundp 'my/eglot-set-workspace-configuration)
    (my/eglot-set-workspace-configuration
     (my/typst-eglot-workspace-configuration)))
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (eglot-signal-didChangeConfiguration (eglot-current-server)))
  (my/typst-eglot-ensure))

(defun my/typst-eglot-available-p ()
  "Return non-nil when tinymist is available."
  (and (my/typst-tinymist-command) t))

(defun my/typst-eglot-ensure ()
  "Start Eglot for Typst buffers when tinymist is available."
  (when (my/typst-eglot-available-p)
    (my/eglot-ensure-unless-lsp-mode)))

(defun my/typst-company-setup ()
  "Enable basic coding completion for Typst buffers."
  (when (fboundp 'company-mode)
    (company-mode 1))
  (setq-local company-backends
              '((company-capf
                 company-files
                 :with company-yasnippet)
                company-dabbrev-code))
  (setq-local company-idle-delay my/typst-company-idle-delay)
  (setq-local company-minimum-prefix-length 1))

(defun my/typst--cancel-preview-start-timer ()
  "Cancel a pending Tinymist preview start retry."
  (when (timerp my/typst-preview--start-timer)
    (cancel-timer my/typst-preview--start-timer))
  (setq my/typst-preview--start-timer nil))

(defun my/typst-preview--cancel-sync-timer ()
  "Cancel a pending preview memory-sync timer."
  (when (timerp my/typst-preview--sync-timer)
    (cancel-timer my/typst-preview--sync-timer))
  (setq my/typst-preview--sync-timer nil))

(defun my/typst-preview--active-p ()
  "Return non-nil when this buffer owns a live Tinymist preview process."
  (and (process-live-p my/typst-preview--process)
       buffer-file-name
       (equal my/typst-preview--process-file
              (file-truename buffer-file-name))))

(defun my/typst-preview--buffer-string ()
  "Return the current buffer contents without narrowing."
  (save-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my/typst-preview-sync-memory (&optional buffer)
  "Send BUFFER's unsaved contents to Tinymist's preview memory store."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq my/typst-preview--sync-timer nil)
        (when (and (my/typst-preview--active-p)
                   my/typst-preview--socket
                   buffer-file-name)
          (condition-case err
              (websocket-send-text
               my/typst-preview--socket
               (json-encode
                `(("event" . "updateMemoryFiles")
                  ("files" (,(file-truename buffer-file-name)
                            . ,(my/typst-preview--buffer-string))))))
            (error
             (message "Tinymist preview memory sync failed: %s"
                      (error-message-string err)))))))))

(defun my/typst-preview-after-change (&rest _)
  "Schedule an in-memory sync so Tinymist sees unsaved buffer edits."
  (when (and (my/typst-preview--active-p)
             my/typst-preview--socket)
    (my/typst-preview--cancel-sync-timer)
    (setq my/typst-preview--sync-timer
          (run-with-idle-timer
           my/typst-preview-sync-delay nil
           #'my/typst-preview-sync-memory
           (current-buffer)))))

(defun my/typst-preview--goto-file-position (file-name position)
  "Open FILE-NAME and move point to Tinymist zero-based POSITION."
  (when (and (stringp file-name)
             (vectorp position)
             (>= (length position) 2))
    (let ((buffer (or (get-file-buffer file-name)
                      (find-file-noselect file-name))))
      (pop-to-buffer buffer)
      (goto-char (point-min))
      (forward-line (aref position 0))
      (ignore-errors
        (forward-char (aref position 1)))
      (when my/typst-preview-center-source
        (recenter-top-bottom)))))

(defun my/typst-preview--handle-control-message (source-buffer _socket frame)
  "Handle a Tinymist control-plane websocket FRAME for SOURCE-BUFFER."
  (condition-case err
      (let* ((message (json-parse-string (websocket-frame-text frame)))
             (event (gethash "event" message)))
        (pcase event
          ("editorScrollTo"
           (my/typst-preview--goto-file-position
            (gethash "filepath" message)
            (gethash "start" message)))
          ("syncEditorChanges"
           (when (buffer-live-p source-buffer)
             (my/typst-preview-sync-memory source-buffer)))
          ("compileStatus"
           (when (string= "CompileError" (gethash "kind" message))
             (when (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 (message "Tinymist preview compile error; see %s"
                          (buffer-name (my/typst-preview-log-buffer)))))))))
    (error
     (message "Tinymist preview control message failed: %s"
              (error-message-string err)))))

(defun my/typst-preview--connect-control (source-buffer host)
  "Connect SOURCE-BUFFER to Tinymist control-plane HOST."
  (when (and (buffer-live-p source-buffer)
             (stringp host)
             (not (string-empty-p host)))
    (with-current-buffer source-buffer
      (unless my/typst-preview--socket
        (setq my/typst-preview--control-host host)
        (setq my/typst-preview--socket
              (websocket-open
               (concat "ws://" host)
               :custom-header-alist '(("origin" . "vscode-webview://emacs"))
               :on-message
               (lambda (socket frame)
                 (my/typst-preview--handle-control-message
                  source-buffer socket frame))
               :on-close
               (lambda (_websocket)
                 (when (buffer-live-p source-buffer)
                   (with-current-buffer source-buffer
                     (setq my/typst-preview--socket nil
                           my/typst-preview--control-host nil))))))
        (my/typst-preview-sync-memory source-buffer)))))

(defun my/typst-preview--maybe-connect-control (source-buffer output)
  "Parse Tinymist OUTPUT and connect SOURCE-BUFFER to control-plane if ready."
  (when (and (buffer-live-p source-buffer)
             (string-match "Control panel server listening on: \\(.+\\)" output))
    (my/typst-preview--connect-control
     source-buffer
     (string-trim (match-string 1 output)))))

(defun my/typst-preview-send-position ()
  "Ask Tinymist's preview pane to scroll to point in the current Typst buffer."
  (interactive)
  (unless (and (my/typst-preview--active-p)
               my/typst-preview--socket)
    (user-error "Tinymist preview is not connected"))
  (websocket-send-text
   my/typst-preview--socket
   (json-encode
    `(("event" . "panelScrollTo")
      ("filepath" . ,(file-truename buffer-file-name))
      ("line" . ,(1- (line-number-at-pos)))
      ("character" . ,(max 1 (current-column)))))))

(defun my/typst-preview-log-buffer ()
  "Return the Tinymist preview log buffer for the current Typst buffer."
  (get-buffer-create
   (format "*tinymist-preview:%s*"
           (or (and buffer-file-name
                    (file-name-nondirectory buffer-file-name))
               (buffer-name)))))

(defun my/typst-preview-show-log ()
  "Show the Tinymist preview log for the current Typst buffer."
  (interactive)
  (display-buffer (my/typst-preview-log-buffer)))

(defun my/typst-stop-preview ()
  "Stop the current buffer's Tinymist preview process."
  (interactive)
  (my/typst--cancel-preview-start-timer)
  (my/typst-preview--cancel-sync-timer)
  (when my/typst-preview--socket
    (condition-case nil
        (websocket-close my/typst-preview--socket)
      (error nil)))
  (when (process-live-p my/typst-preview--process)
    (delete-process my/typst-preview--process))
  (setq my/typst-preview--process nil
        my/typst-preview--process-file nil
        my/typst-preview--process-args nil
        my/typst-preview--control-host nil
        my/typst-preview--socket nil))

(defun my/typst-preview-cleanup ()
  "Clean Typst preview resources owned by the current buffer."
  (my/typst-stop-preview))

(defun my/typst-preview-buffer-setup ()
  "Install buffer-local hooks used by Typst preview."
  (add-hook 'after-change-functions #'my/typst-preview-after-change nil t)
  (add-hook 'kill-buffer-hook #'my/typst-preview-cleanup nil t))

(defun my/typst-start-default-preview ()
  "Start Tinymist's official preview server for the current buffer."
  (unless (my/typst-tinymist-command)
    (user-error "Cannot find tinymist"))
  (unless buffer-file-name
    (user-error "Save this Typst buffer before starting preview"))
  (when (buffer-modified-p)
    (save-buffer))
  (my/typst-eglot-setup)
  (let ((file (file-truename buffer-file-name)))
    (let* ((command (my/typst-tinymist-command))
           (args (append (my/typst-preview-args) (list file))))
      (unless (and (process-live-p my/typst-preview--process)
                   (equal my/typst-preview--process-file file)
                   (equal my/typst-preview--process-args args))
        (my/typst-stop-preview)
        (let* ((command command)
             (args args)
             (source-buffer (current-buffer))
             (log-buffer (my/typst-preview-log-buffer)))
        (with-current-buffer log-buffer
          (erase-buffer)
          (insert (format "$ %s %s\n\n"
                          command
                          (mapconcat #'shell-quote-argument args " "))))
        (let ((error-shown nil))
          (setq my/typst-preview--process
                (make-process
                 :name "tinymist-preview"
                 :buffer log-buffer
                 :command (cons command args)
                 :noquery t
                 :connection-type 'pipe
                 :filter
                 (lambda (process output)
                   (when-let* ((buffer (process-buffer process))
                               ((buffer-live-p buffer)))
	                     (with-current-buffer buffer
	                       (goto-char (point-max))
	                       (insert output)
		               (my/typst-preview--maybe-connect-control
		                source-buffer (buffer-string)))
	                     (when (and (not error-shown)
                                (string-match-p
                                 "\\(?:compilation failed\\|^error:\\)"
                                 output))
                       (setq error-shown t)
                       (message "Tinymist preview has errors; see %s"
                                (buffer-name buffer))
                       (when my/typst-preview-pop-log-on-error
                         (display-buffer buffer)))))
                 :sentinel
                 (lambda (process event)
                   (unless (process-live-p process)
                     (when (buffer-live-p source-buffer)
                       (with-current-buffer source-buffer
                         (when (eq process my/typst-preview--process)
                           (setq my/typst-preview--process nil
                                 my/typst-preview--process-file nil
                                 my/typst-preview--control-host nil))))
                     (message "Tinymist preview %s; see %s"
                              (string-trim event)
                             (buffer-name (process-buffer process))))))))
        (setq my/typst-preview--process-file file
              my/typst-preview--process-args args))))))

(defun my/typst-preview ()
  "Start Tinymist's official preview and open it with the system browser."
  (interactive)
  (my/typst-previewer-setup)
  (previewer-workbench)
  (message "Started Tinymist official previewer"))

(defun my/typst-ts-pretty-setup ()
  "Keep Typst buffers on `typst-ts-mode' native math script rendering."
  (setq-local treesit-font-lock-level
              (max 3 (if (integerp treesit-font-lock-level)
                         treesit-font-lock-level
                       0))))

(use-package typst-ts-mode
  :ensure t
  :mode ("\\.typ\\'" . typst-ts-mode)
  :bind (:map typst-ts-mode-map
              ("C-c C-p" . my/typst-preview)
              ("C-c C-j" . my/typst-preview-send-position))
  :hook ((typst-ts-mode . my/typst-eglot-setup)
         (typst-ts-mode . my/typst-company-setup)
         (typst-ts-mode . my/typst-previewer-setup)
         (typst-ts-mode . my/typst-preview-buffer-setup)
         (typst-ts-mode . my/typst-ts-pretty-setup)
         (typst-ts-mode . flymake-mode)))

(dolist (hook '(typst-mode-hook my/typst-mode-hook))
  (add-hook hook #'my/typst-eglot-setup)
  (add-hook hook #'my/typst-company-setup)
  (add-hook hook #'my/typst-previewer-setup)
  (add-hook hook #'my/typst-preview-buffer-setup)
  (add-hook hook #'flymake-mode))

(with-eval-after-load 'typst-mode
  (when (boundp 'typst-mode-map)
    (define-key typst-mode-map (kbd "C-c C-p") #'my/typst-preview)))

(define-key my/typst-mode-map (kbd "C-c C-p") #'my/typst-preview)
(define-key my/typst-mode-map (kbd "C-c C-j") #'my/typst-preview-send-position)

(use-package eglot
  :ensure nil
  :defer t)

(with-eval-after-load 'eglot
  (cl-defmethod eglot-register-capability
    (_server (method (eql workspace/didChangeConfiguration)) _id &rest _params)
    nil)
  (cl-defmethod eglot-unregister-capability
    (_server (method (eql workspace/didChangeConfiguration)) _id &rest _params)
    nil)
  (when (fboundp 'my/register-eglot-server-program)
    (my/register-eglot-server-program
     '(typst-ts-mode typst-mode my/typst-mode)
     (my/typst-eglot-server-command)
     :label "tinymist"
     :executables (list my/typst-lsp-executable)
     :note "Typst buffers use tinymist through Eglot.")))

(provide 'init-typst)
;;; init-typst.el ends here

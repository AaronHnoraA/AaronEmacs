;;; aaron-neopyter.el --- Emacs client for the Neopyter JupyterLab extension -*- lexical-binding: t -*-

;;; Commentary:
;; aaron-neopyter is an Emacs-side Neopyter-compatible client.  It bridges
;; Jupytext percent-format scripts (*.ju.py, *.ju.r) to JupyterLab via the
;; official Neopyter JupyterLab extension (pip install neopyter).
;;
;; Architecture:
;;   - Emacs runs a WebSocket server (direct mode, default 127.0.0.1:9001)
;;   - The extension connects to Emacs as a WebSocket client
;;   - Communication is msgpack-rpc with base64-encoded text frames
;;   - Emacs is source of truth; JupyterLab is the preview/execution frontend
;;
;; See docs/neopyter-protocol-notes.md for protocol details.
;;
;; Quick start:
;;   1. pip install neopyter
;;   2. In JupyterLab: Settings → Neopyter → Mode=direct, IP=127.0.0.1, Port=9001
;;   3. Open a *.ju.py file in Emacs
;;   4. M-x aaron-neopyter-connect
;;   5. M-x aaron-neopyter-open-notebook

;;; Code:

(require 'cl-lib)
(require 'aaron-neopyter-rpc)
(require 'aaron-neopyter-parser)
(require 'aaron-neopyter-sync)
(require 'aaron-neopyter-jupyter)
(require 'aaron-neopyter-ui)
(require 'aaron-neopyter-commands)

;;; Customization

(defgroup aaron-neopyter nil
  "Emacs client for the Neopyter JupyterLab extension."
  :group 'tools
  :prefix "aaron-neopyter-")

(defcustom aaron-neopyter-remote-address "127.0.0.1:9001"
  "Address the Neopyter WebSocket server listens on (HOST:PORT)."
  :type 'string
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-mode-type 'direct
  "Connection mode: `direct' (Emacs is WS server) or `proxy' (future)."
  :type '(choice (const direct) (const proxy))
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-file-patterns '("*.ju.py" "*.ju.r")
  "Glob patterns for files that auto-enable `aaron-neopyter-mode'."
  :type '(repeat string)
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-auto-connect t
  "Start the server automatically when `aaron-neopyter-mode' first activates."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-auto-attach t
  "Automatically open and sync the notebook when the extension connects."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-auto-create-ipynb t
  "Create the paired .ipynb if it does not exist in JupyterLab."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-auto-activate-file t
  "Activate the notebook in JupyterLab when attaching."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-follow-point nil
  "Whether point movement automatically activates the matching JupyterLab cell.
When nil, use `aaron-neopyter-sync-point' to sync point manually."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-sync-debounce 0.35
  "Seconds to wait after the last buffer change before syncing."
  :type 'float
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-cursor-debounce 0.08
  "Seconds to wait after point movement before sending activateCell."
  :type 'float
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-scroll-enable t
  "Whether to scroll JupyterLab to the active cell when cursor moves."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-scroll-align "center"
  "Alignment when scrolling to a cell: \"auto\" | \"start\" | \"end\" | \"center\"."
  :type '(choice (const "auto") (const "start") (const "end") (const "center"))
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-partial-sync nil
  "Use partial sync (experimental, default off).  Full sync is always safe."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-debug nil
  "Enable verbose RPC logging to the Neopyter log buffer."
  :type 'boolean
  :group 'aaron-neopyter)

(defcustom aaron-neopyter-jupyter-root nil
  "Absolute directory from which JupyterLab was launched (its notebook-dir).
JupyterLab's content API treats all notebook paths as relative to this root.
Set this to the directory you pass to `jupyter lab --notebook-dir' or where
you run `jupyter lab' from, e.g.:

  (setq aaron-neopyter-jupyter-root \"/Users/hc/Documents/AaronNote\")

When nil, paths are sent as-is (works only if Emacs CWD = JupyterLab root).
You can also run `M-x aaron-neopyter-detect-jupyter-root' to auto-detect it
from a currently-open notebook."
  :type '(choice (const :tag "Auto (no root strip)" nil) directory)
  :group 'aaron-neopyter)

;;; Buffer-local tracking

(defvar-local aaron-neopyter--co-enabled-jupytext nil
  "Non-nil when this mode co-activated `jupytext-mode' and should co-disable it.")

;;; Global connection

(defvar aaron-neopyter--connection nil
  "The active `aaron-neopyter--conn', or nil if not started.")

(defun aaron-neopyter--parse-address (addr)
  "Parse ADDR \"host:port\" into (host . port) cons."
  (if (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" addr)
      (cons (match-string 1 addr) (string-to-number (match-string 2 addr)))
    (error "Invalid address: %s" addr)))

(defun aaron-neopyter--start-server ()
  "Start the WebSocket server if not already running."
  (if (and aaron-neopyter--connection
           (aaron-neopyter--conn-server aaron-neopyter--connection))
      (message "Neopyter: server already running at %s"
               aaron-neopyter-remote-address)
    (let* ((addr (aaron-neopyter--parse-address aaron-neopyter-remote-address))
           (host (car addr))
           (port (cdr addr)))
      (setq aaron-neopyter--connection
            (aaron-neopyter-rpc-start-server
             host port
             ;; on-connect: attach all live neopyter-mode buffers
             #'aaron-neopyter--on-extension-connect
             ;; on-disconnect: update UI
             #'aaron-neopyter--on-extension-disconnect))
      (aaron-neopyter-ui-refresh))))

(defun aaron-neopyter--stop-server ()
  "Stop the WebSocket server and clean up."
  (when aaron-neopyter--connection
    (aaron-neopyter-rpc-stop-server aaron-neopyter--connection)
    (setq aaron-neopyter--connection nil)
    (aaron-neopyter-ui-refresh)
    (message "Neopyter: server stopped")))

(defun aaron-neopyter--on-extension-connect ()
  "Called when the JupyterLab extension connects."
  (aaron-neopyter-ui-refresh)
  (when aaron-neopyter-auto-attach
    ;; Attach every live buffer with aaron-neopyter-mode active
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'aaron-neopyter-mode buf)
        (with-current-buffer buf
          (aaron-neopyter--attach-current-buffer))))))

(defun aaron-neopyter--on-extension-disconnect ()
  "Called when the JupyterLab extension disconnects."
  (aaron-neopyter-ui-refresh)
  ;; Reset last-cell-idx so re-connect triggers a fresh cursor send
  (dolist (buf (buffer-list))
    (let ((session (buffer-local-value 'aaron-neopyter--session buf)))
      (when session
        (setf (aaron-neopyter--session-last-cell-idx session) -1)))))

(defun aaron-neopyter--attach-current-buffer ()
  "Initialize session and attach the current buffer to the notebook."
  (condition-case err
      (progn
        (aaron-neopyter-sync-init-session)
        (let ((conn aaron-neopyter--connection))
          (when (aaron-neopyter-rpc-connected-p conn)
            (aaron-neopyter-sync-attach conn))))
    (error
     (message "Neopyter: attach error in %s: %s"
              (buffer-name) (error-message-string err)))))

;;; Buffer hook handlers (installed by the minor mode)

(defconst aaron-neopyter--evil-nav-commands
  '(evil-backward-char         ; h
    evil-forward-char          ; l
    evil-next-line             ; j
    evil-previous-line         ; k
    evil-next-visual-line      ; gj
    evil-previous-visual-line  ; gk
    evil-beginning-of-line     ; 0 / ^
    evil-end-of-line           ; $
    evil-forward-word-begin    ; w
    evil-backward-word-begin   ; b
    evil-forward-word-end      ; e
    evil-goto-first-line       ; gg
    evil-goto-line             ; G
    evil-scroll-up             ; C-u
    evil-scroll-down           ; C-d
    evil-scroll-page-up        ; C-b
    evil-scroll-page-down)     ; C-f
  "Evil commands that are pure navigation and must not trigger a Neopyter sync.")

(defun aaron-neopyter--evil-nav-p ()
  "Return non-nil when the last command was an Evil pure-navigation motion."
  (and (bound-and-true-p evil-local-mode)
       (memq this-command aaron-neopyter--evil-nav-commands)))

(defun aaron-neopyter--after-change-h (_beg _end _len)
  "Hook: schedule sync after a buffer change.
Skipped when Evil is in a non-editing state (normal/motion/visual)
to prevent stray overlay or undo-record side effects from triggering
spurious fullSync RPCs."
  (when (and aaron-neopyter--session
             (aaron-neopyter-rpc-connected-p aaron-neopyter--connection)
             ;; Only sync from states that actually edit text
             (not (and (bound-and-true-p evil-local-mode)
                       (boundp 'evil-state)
                       (memq evil-state '(normal motion visual operator)))))
    (aaron-neopyter-sync-schedule aaron-neopyter--connection
                                  aaron-neopyter-sync-debounce)))

(defun aaron-neopyter--post-command-h ()
  "Hook: schedule cursor sync after a command.
Pure Evil navigation keystrokes (hjkl etc.) are excluded from
immediately scheduling activateCell/scrollToItem — the user is
still in transit and rapid RPC calls can cause JupyterLab to
process its own keybindings unexpectedly."
  (when (and aaron-neopyter--session
             (aaron-neopyter--session-follow-point aaron-neopyter--session)
             (aaron-neopyter-rpc-connected-p aaron-neopyter--connection)
             (not (aaron-neopyter--evil-nav-p)))
    (aaron-neopyter-cursor-schedule aaron-neopyter--connection
                                    aaron-neopyter-cursor-debounce)))

(defun aaron-neopyter--after-save-h ()
  "Hook: trigger a sync on save (belt-and-suspenders for debounce)."
  (when (and aaron-neopyter--session
             (aaron-neopyter-rpc-connected-p aaron-neopyter--connection))
    ;; Small delay to let jupytext-mode's own save hook run first
    (run-with-timer 0.1 nil
                    (lambda ()
                      (when (buffer-live-p (current-buffer))
                        (with-current-buffer (current-buffer)
                          (aaron-neopyter-sync-now aaron-neopyter--connection)))))))

(defun aaron-neopyter--kill-buffer-h ()
  "Hook: clean up session timers when the buffer is killed."
  (aaron-neopyter-sync-teardown-session))

;;; Minor mode keymap

(defvar aaron-neopyter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'aaron-neopyter-run-cell)
    (define-key map (kbd "C-c C-a") #'aaron-neopyter-run-all-above)
    (define-key map (kbd "C-c C-b") #'aaron-neopyter-run-all-below)
    (define-key map (kbd "C-c C-r") #'aaron-neopyter-restart-kernel)
    (define-key map (kbd "C-c C-s") #'aaron-neopyter-sync-current)
    (define-key map (kbd "C-c C-o") #'aaron-neopyter-open-notebook)
    (define-key map (kbd "C-c C-l") #'aaron-neopyter-toggle-follow-point)
    (define-key map (kbd "M-RET") #'aaron-neopyter-sync-point)
    map)
  "Keymap for `aaron-neopyter-mode'.")

;;; Minor mode

;;;###autoload
(define-minor-mode aaron-neopyter-mode
  "Minor mode to sync *.ju.py buffers with JupyterLab via the Neopyter extension.

\\{aaron-neopyter-mode-map}"
  :lighter (:eval (aaron-neopyter-ui-lighter))
  :keymap aaron-neopyter-mode-map
  (if aaron-neopyter-mode
      (aaron-neopyter--mode-enable)
    (aaron-neopyter--mode-disable)))

(defun aaron-neopyter--mode-enable ()
  "Set up hooks and start the server if needed."
  (add-hook 'after-change-functions #'aaron-neopyter--after-change-h nil t)
  (add-hook 'post-command-hook      #'aaron-neopyter--post-command-h nil t)
  (add-hook 'after-save-hook        #'aaron-neopyter--after-save-h   nil t)
  (add-hook 'kill-buffer-hook       #'aaron-neopyter--kill-buffer-h  nil t)
  ;; Co-enable jupytext-mode for the save-to-disk fallback (jupytext --update).
  ;; When Neopyter is disconnected the notebook on disk stays in sync via jupytext.
  (when (and (fboundp 'jupytext-mode)
             (not (bound-and-true-p jupytext-mode)))
    (condition-case nil
        (progn (jupytext-mode 1)
               (setq aaron-neopyter--co-enabled-jupytext t))
      (error nil)))
  ;; Start global server on first activation if requested
  (when (and aaron-neopyter-auto-connect
             (not (and aaron-neopyter--connection
                       (aaron-neopyter--conn-server aaron-neopyter--connection))))
    (aaron-neopyter--start-server))
  ;; If already connected, attach immediately
  (when (and aaron-neopyter-auto-attach
             (aaron-neopyter-rpc-connected-p aaron-neopyter--connection))
    (aaron-neopyter--attach-current-buffer)))

(defun aaron-neopyter--mode-disable ()
  "Remove hooks and tear down session."
  (remove-hook 'after-change-functions #'aaron-neopyter--after-change-h t)
  (remove-hook 'post-command-hook      #'aaron-neopyter--post-command-h t)
  (remove-hook 'after-save-hook        #'aaron-neopyter--after-save-h   t)
  (remove-hook 'kill-buffer-hook       #'aaron-neopyter--kill-buffer-h  t)
  ;; Co-disable jupytext-mode if we were the ones who activated it
  (when (and aaron-neopyter--co-enabled-jupytext
             (fboundp 'jupytext-mode)
             (bound-and-true-p jupytext-mode))
    (condition-case nil (jupytext-mode -1) (error nil))
    (setq aaron-neopyter--co-enabled-jupytext nil))
  (aaron-neopyter-sync-teardown-session))

;;; Health check

(defun aaron-neopyter-health-check ()
  "Run a quick echo/version health check against the connected extension."
  (interactive)
  (if (not (aaron-neopyter-rpc-connected-p aaron-neopyter--connection))
      (message "Neopyter: not connected")
    (let ((conn aaron-neopyter--connection))
      (aaron-neopyter-jupyter-get-version
       conn
       (lambda (version err)
         (if err
             (message "Neopyter health: ERROR %s" err)
           (message "Neopyter health: OK — extension version %s" version))))
      (aaron-neopyter-jupyter-echo
       conn "ping"
       (lambda (result err)
         (if err
             (message "Neopyter echo: ERROR %s" err)
           (message "Neopyter echo: %s" result)))))))

(provide 'aaron-neopyter)
;;; aaron-neopyter.el ends here

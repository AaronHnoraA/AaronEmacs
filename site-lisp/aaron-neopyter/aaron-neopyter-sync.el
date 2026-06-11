;;; aaron-neopyter-sync.el --- Buffer-to-notebook sync and session state -*- lexical-binding: t -*-

;;; Commentary:
;; Manages per-buffer session state and drives the sync+cursor pipeline:
;;
;;   after-change-functions → schedule debounced full-sync
;;   post-command-hook      → schedule debounced cursor follow
;;
;; Emacs is the source of truth.  The notebook receives the current buffer
;; content via fullSync.  Cursor position is sent via activateCell +
;; scrollToItem only when the active cell index changes.

;;; Code:

(require 'cl-lib)
(require 'aaron-neopyter-parser)
(require 'aaron-neopyter-jupyter)

(declare-function my/jupytext--canonical-script-file "init-jupyter-core")
(declare-function my/jupytext--ensure-pair           "init-jupyter-core")

;; Forward declaration; the real defcustom lives in aaron-neopyter.el.
;; This lets sync.el use it without a circular-require.
(defvar aaron-neopyter-jupyter-root nil
  "Absolute directory JupyterLab was launched from (its notebook-dir / root-dir).
All paths sent over RPC must be relative to this root.
Set to e.g. \"/Users/hc/Documents/AaronNote\" in your init file or via
M-x customize-option aaron-neopyter-jupyter-root.")

;;; Session struct (one per buffer)

(cl-defstruct (aaron-neopyter--session
               (:constructor aaron-neopyter--make-session)
               (:copier nil))
  "Per-buffer sync session."
  notebook-path   ; string: absolute path to paired .ipynb
  (version 0)     ; integer: monotonic edit counter
  cell-cache      ; list of aaron-neopyter-cell (last parse result)
  (last-cell-idx -1) ; integer: last activated cell index (-1 = unknown)
  (follow-point t)   ; boolean
  sync-timer         ; timer or nil
  cursor-timer)      ; timer or nil

(defvar-local aaron-neopyter--session nil
  "The `aaron-neopyter--session' for this buffer, or nil.")

;;; Notebook path resolution

(defun aaron-neopyter-sync--own-mapper (file)
  "Return the paired .ipynb path for FILE using the built-in rule.
Strips a .ju. infix and replaces the extension with .ipynb."
  (let* ((without-ju (replace-regexp-in-string "\\.ju\\.\\([^.]+\\)\\'" ".\\1" file))
         (sans-ext   (file-name-sans-extension without-ju)))
    (concat sans-ext ".ipynb")))

(defun aaron-neopyter-sync--rpc-path (abs-path)
  "Convert ABS-PATH to a JupyterLab-relative path for use in RPC calls.
JupyterLab's content API treats all paths as relative to the directory from
which `jupyter lab' was launched.  When `aaron-neopyter-jupyter-root' is set
and ABS-PATH is under that root, the root prefix is stripped.  Otherwise
ABS-PATH is returned unchanged (which works when both Emacs and JupyterLab
share the same CWD / when no root is configured)."
  (let ((root (and (boundp 'aaron-neopyter-jupyter-root)
                   aaron-neopyter-jupyter-root)))
    (if (and root (stringp root) (not (string-empty-p root)))
        (let ((root-dir (file-name-as-directory (expand-file-name root))))
          (if (string-prefix-p root-dir abs-path)
              (substring abs-path (length root-dir))
            abs-path))
      abs-path)))

(defun aaron-neopyter-sync--notebook-path ()
  "Return the absolute .ipynb path paired with the current buffer.
Uses, in order: (1) a manually-registered jupytext pair (if the
stored path is already clean, i.e. not a .ju.IPYNB mis-naming),
then (2) our own .ju.-infix mapper."
  (or
   ;; Honour an explicitly registered pair, but guard against the stale
   ;; .ju.ipynb path that old versions of my/jupytext--default-notebook-file
   ;; could produce for .ju.* source files.
   (and (boundp 'my/jupytext-notebook-file)
        my/jupytext-notebook-file
        (not (string-match-p "\\.ju\\.[^./]+\\'" my/jupytext-notebook-file))
        (expand-file-name my/jupytext-notebook-file))
   ;; Own mapper: strip .ju. infix, swap extension to .ipynb
   (and buffer-file-name
        (aaron-neopyter-sync--own-mapper
         (expand-file-name buffer-file-name)))))

;;; Session lifecycle

(defun aaron-neopyter-sync-init-session ()
  "Initialize or reinitialize the session for the current buffer.
The notebook-path stored in the session is always the JupyterLab-relative path
(see `aaron-neopyter-sync--rpc-path').  Return the session struct."
  (let* ((abs-path (aaron-neopyter-sync--notebook-path))
         (rpc-path (and abs-path (aaron-neopyter-sync--rpc-path abs-path))))
    (unless rpc-path
      (user-error "Cannot determine notebook path for buffer %s" (buffer-name)))
    (let ((session (or aaron-neopyter--session
                       (aaron-neopyter--make-session))))
      (setf (aaron-neopyter--session-notebook-path session) rpc-path)
      (setq aaron-neopyter--session session)
      session)))

(defun aaron-neopyter-sync-teardown-session ()
  "Cancel timers and clear the session for the current buffer."
  (when aaron-neopyter--session
    (let ((s aaron-neopyter--session))
      (when (timerp (aaron-neopyter--session-sync-timer s))
        (cancel-timer (aaron-neopyter--session-sync-timer s)))
      (when (timerp (aaron-neopyter--session-cursor-timer s))
        (cancel-timer (aaron-neopyter--session-cursor-timer s)))
      (setf (aaron-neopyter--session-sync-timer   s) nil)
      (setf (aaron-neopyter--session-cursor-timer s) nil)))
  (setq aaron-neopyter--session nil))

;;; Full sync

(defun aaron-neopyter-sync-now (conn)
  "Parse the current buffer and send fullSync to JupyterLab via CONN.
Updates the cell cache.  Call from a timer or interactively."
  (when (and aaron-neopyter--session
             (buffer-live-p (current-buffer)))
    (let* ((session  aaron-neopyter--session)
           (nb-path  (aaron-neopyter--session-notebook-path session))
           (cells    (aaron-neopyter-parse-buffer)))
      (setf (aaron-neopyter--session-cell-cache session) cells)
      (cl-incf (aaron-neopyter--session-version session))
      (when nb-path
        (let ((rpc-cells (aaron-neopyter-cells-to-rpc cells)))
          (aaron-neopyter-jupyter-full-sync
           conn nb-path rpc-cells
           (lambda (_r err)
             (when err
               (message "[neopyter-sync] fullSync error: %s" err)))))))))

(defun aaron-neopyter-sync-schedule (conn delay)
  "Schedule a debounced fullSync for the current buffer via CONN after DELAY seconds."
  (when aaron-neopyter--session
    (let ((s aaron-neopyter--session)
          (buf (current-buffer)))
      (when (timerp (aaron-neopyter--session-sync-timer s))
        (cancel-timer (aaron-neopyter--session-sync-timer s)))
      (setf (aaron-neopyter--session-sync-timer s)
            (run-with-timer
             delay nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (aaron-neopyter-sync-now conn)))))))))

;;; Cursor / active cell follow

(defun aaron-neopyter-cursor-now (conn)
  "Send activateCell + scrollToItem to JupyterLab based on current point.
Only sends if the cell index actually changed."
  (when (and aaron-neopyter--session
             (buffer-live-p (current-buffer))
             (aaron-neopyter--session-follow-point aaron-neopyter--session))
    (let* ((session  aaron-neopyter--session)
           (nb-path  (aaron-neopyter--session-notebook-path session))
           (cells    (or (aaron-neopyter--session-cell-cache session)
                         (aaron-neopyter-parse-buffer)))
           (idx      (aaron-neopyter-cell-index-at-pos (point) cells)))
      (when (and nb-path (not (= idx (aaron-neopyter--session-last-cell-idx session))))
        (setf (aaron-neopyter--session-last-cell-idx session) idx)
        (aaron-neopyter-jupyter-activate-cell conn nb-path idx)
        (aaron-neopyter-jupyter-scroll-to-cell conn nb-path idx "center" 0.0)))))

(defun aaron-neopyter-cursor-schedule (conn delay)
  "Schedule a debounced cursor sync for the current buffer via CONN after DELAY seconds."
  (when aaron-neopyter--session
    (let ((s aaron-neopyter--session)
          (buf (current-buffer)))
      (when (timerp (aaron-neopyter--session-cursor-timer s))
        (cancel-timer (aaron-neopyter--session-cursor-timer s)))
      (setf (aaron-neopyter--session-cursor-timer s)
            (run-with-timer
             delay nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (aaron-neopyter-cursor-now conn)))))))))

;;; Auto-attach: open notebook + initial sync

(defun aaron-neopyter-sync-attach (conn &optional callback)
  "Open or create the paired notebook, then perform initial fullSync.
CALLBACK receives (nil error) when done."
  (when (and aaron-neopyter--session conn)
    (let* ((session aaron-neopyter--session)
           (path    (aaron-neopyter--session-notebook-path session))
           (buf     (current-buffer)))
      (aaron-neopyter-jupyter-open-or-create
       conn path
       (lambda (_r err)
         (if err
             (progn
               (message "[neopyter-sync] open-or-create failed: %s" err)
               (when callback (funcall callback nil err)))
           ;; Now do initial sync
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (aaron-neopyter-sync-now conn)
               ;; Activate the cell at point
               (aaron-neopyter-cursor-now conn)
               (when callback (funcall callback nil nil))))))))))

(provide 'aaron-neopyter-sync)
;;; aaron-neopyter-sync.el ends here

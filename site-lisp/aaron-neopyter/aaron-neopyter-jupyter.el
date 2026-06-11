;;; aaron-neopyter-jupyter.el --- High-level JupyterLab RPC wrappers -*- lexical-binding: t -*-

;;; Commentary:
;; One function per confirmed Neopyter RPC method.
;;
;; IMPORTANT — Notebook method path convention (from TypeScript src/index.ts):
;;   Every notebook/cell-level method takes PATH as its FIRST argument.
;;   The extension's `getNotebookModel(path)` looks up the active panel by path.
;;   Docmanager methods (isFileExist, openOrReveal, activateNotebook, …) also
;;   take path but as their semantic first argument and were already correct.
;;
;; Functions that need PATH:
;;   fullSync, partialSync, activateCell, scrollToItem, setMode, getCellNum,
;;   setCellNum, getCell, insertCell, deleteCell, setCellSource, setCellType,
;;   save, runSelectedCell, runAllAbove, runAllBelow, runAll,
;;   restartKernel, restartRunAll
;;
;; Functions that do NOT need PATH (utility/docmanager):
;;   getVersion, echo, executeCommand, isFileExist, isFileOpen,
;;   openFile, openOrReveal, activateNotebook, createNew, getCurrentNotebook,
;;   closeFile, selectAbove, selectBelow

;;; Code:

(require 'cl-lib)
(require 'aaron-neopyter-rpc)

(defun aaron-neopyter-jupyter--cb (label)
  "Return a default callback that logs errors for LABEL."
  (lambda (_result err)
    (when err
      (message "[neopyter] %s error: %s" label err))))

;;; ─── Version / health ──────────────────────────────────────────────────────

(defun aaron-neopyter-jupyter-get-version (conn callback)
  "Call getVersion on CONN; invoke CALLBACK with (version error)."
  (aaron-neopyter-rpc-request conn "getVersion" (vector) callback))

(defun aaron-neopyter-jupyter-echo (conn msg callback)
  "Call echo MSG on CONN; invoke CALLBACK with (result error)."
  (aaron-neopyter-rpc-request conn "echo" (vector msg) callback))

;;; ─── File / docmanager (no notebook-path prefix needed) ────────────────────

(defun aaron-neopyter-jupyter-is-file-exist (conn path callback)
  "Call isFileExist for PATH on CONN; CALLBACK receives (bool error)."
  (aaron-neopyter-rpc-request conn "isFileExist" (vector path) callback))

(defun aaron-neopyter-jupyter-is-file-open (conn path callback)
  "Call isFileOpen for PATH on CONN; CALLBACK receives (bool error)."
  (aaron-neopyter-rpc-request conn "isFileOpen" (vector path) callback))

(defun aaron-neopyter-jupyter-open-file (conn path &optional callback)
  "Open PATH in JupyterLab via CONN."
  (aaron-neopyter-rpc-request conn "openFile" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "openFile"))))

(defun aaron-neopyter-jupyter-open-or-reveal (conn path &optional callback)
  "Open or reveal PATH in JupyterLab via CONN."
  (aaron-neopyter-rpc-request conn "openOrReveal" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "openOrReveal"))))

(defun aaron-neopyter-jupyter-activate-notebook (conn path &optional callback)
  "Activate the notebook at PATH in JupyterLab via CONN."
  (aaron-neopyter-rpc-request conn "activateNotebook" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "activateNotebook"))))

(defun aaron-neopyter-jupyter-create-new (conn path &optional widget kernel callback)
  "Create a new notebook at PATH with WIDGET and KERNEL via CONN."
  (aaron-neopyter-rpc-request conn "createNew"
                              (vector path (or widget "Notebook") (or kernel ""))
                              (or callback (aaron-neopyter-jupyter--cb "createNew"))))

(defun aaron-neopyter-jupyter-open-or-create (conn path &optional callback)
  "Open PATH if it exists, otherwise create it, then activate.
CALLBACK receives (nil error) when the notebook is open and active."
  (aaron-neopyter-jupyter-is-file-exist
   conn path
   (lambda (exists err)
     (if err
         (when callback (funcall callback nil err))
       (if exists
           (aaron-neopyter-jupyter-open-or-reveal
            conn path
            (lambda (_r err2)
              (if err2
                  (when callback (funcall callback nil err2))
                (aaron-neopyter-jupyter-activate-notebook conn path callback))))
         (aaron-neopyter-jupyter-create-new
          conn path nil nil
          (lambda (_r err2)
            (if err2
                (when callback (funcall callback nil err2))
              (run-with-timer
               0.5 nil
               (lambda ()
                 (aaron-neopyter-jupyter-activate-notebook conn path callback)))))))))))

(defun aaron-neopyter-jupyter-get-current-notebook (conn callback)
  "Get the currently active notebook path; CALLBACK receives (path error)."
  (aaron-neopyter-rpc-request conn "getCurrentNotebook" (vector) callback))

(defun aaron-neopyter-jupyter-close-file (conn path &optional callback)
  "Close the file at PATH in JupyterLab via CONN."
  (aaron-neopyter-rpc-request conn "closeFile" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "closeFile"))))

;;; ─── Notebook methods (all take PATH as first arg) ─────────────────────────

(defun aaron-neopyter-jupyter-save (conn path &optional callback)
  "Save the notebook at PATH via CONN."
  (aaron-neopyter-rpc-request conn "save" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "save"))))

(defun aaron-neopyter-jupyter-activate-cell (conn path idx &optional callback)
  "Activate cell IDX (0-based) in the notebook at PATH via CONN."
  (aaron-neopyter-rpc-request conn "activateCell" (vector path idx)
                              (or callback (aaron-neopyter-jupyter--cb "activateCell"))))

(defun aaron-neopyter-jupyter-scroll-to-cell (conn path idx &optional align margin callback)
  "Scroll the notebook at PATH to cell IDX via CONN.
ALIGN: \"auto\"|\"start\"|\"end\"|\"center\" (default \"center\").
MARGIN: float (default 0.0)."
  (aaron-neopyter-rpc-request conn "scrollToItem"
                              (vector path idx (or align "center") (or margin 0.0))
                              (or callback (aaron-neopyter-jupyter--cb "scrollToItem"))))

(defun aaron-neopyter-jupyter-set-mode (conn path mode &optional callback)
  "Set the notebook at PATH to MODE (\"command\" or \"edit\") via CONN."
  (aaron-neopyter-rpc-request conn "setMode" (vector path mode)
                              (or callback (aaron-neopyter-jupyter--cb "setMode"))))

(defun aaron-neopyter-jupyter-get-cell-num (conn path callback)
  "Get the number of cells in the notebook at PATH; CALLBACK receives (n error)."
  (aaron-neopyter-rpc-request conn "getCellNum" (vector path) callback))

(defun aaron-neopyter-jupyter-full-sync (conn path cells &optional callback)
  "Full sync: replace all cells in the notebook at PATH with CELLS via CONN.
CELLS is a vector of alists with keys \"source\" and \"cell_type\"."
  (aaron-neopyter-rpc-request conn "fullSync" (vector path cells)
                              (or callback (aaron-neopyter-jupyter--cb "fullSync"))))

(defun aaron-neopyter-jupyter-partial-sync (conn path start-idx end-idx cells &optional callback)
  "Partial sync: replace cells from START-IDX to END-IDX in PATH with CELLS."
  (aaron-neopyter-rpc-request conn "partialSync" (vector path start-idx end-idx cells)
                              (or callback (aaron-neopyter-jupyter--cb "partialSync"))))

(defun aaron-neopyter-jupyter-run-cell (conn path &optional callback)
  "Run the selected cell in the notebook at PATH via CONN."
  (aaron-neopyter-rpc-request conn "runSelectedCell" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "runSelectedCell"))))

(defun aaron-neopyter-jupyter-run-all-above (conn path &optional callback)
  "Run all cells above the current one in PATH via CONN."
  (aaron-neopyter-rpc-request conn "runAllAbove" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "runAllAbove"))))

(defun aaron-neopyter-jupyter-run-all-below (conn path &optional callback)
  "Run the current cell and all below in PATH via CONN."
  (aaron-neopyter-rpc-request conn "runAllBelow" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "runAllBelow"))))

(defun aaron-neopyter-jupyter-run-all (conn path &optional callback)
  "Run all cells in the notebook at PATH via CONN."
  (aaron-neopyter-rpc-request conn "runAll" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "runAll"))))

(defun aaron-neopyter-jupyter-restart-kernel (conn path &optional callback)
  "Restart the kernel for the notebook at PATH via CONN."
  (aaron-neopyter-rpc-request conn "restartKernel" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "restartKernel"))))

(defun aaron-neopyter-jupyter-restart-run-all (conn path &optional callback)
  "Restart the kernel and run all cells for the notebook at PATH via CONN."
  (aaron-neopyter-rpc-request conn "restartRunAll" (vector path)
                              (or callback (aaron-neopyter-jupyter--cb "restartRunAll"))))

;;; ─── JupyterLab commands (no path) ─────────────────────────────────────────

(defun aaron-neopyter-jupyter-execute-command (conn command &optional args callback)
  "Execute a JupyterLab command string COMMAND with optional ARGS via CONN."
  (if args
      (aaron-neopyter-rpc-request conn "executeCommand" (vector command args)
                                  (or callback (aaron-neopyter-jupyter--cb command)))
    (aaron-neopyter-rpc-request conn "executeCommand" (vector command)
                                (or callback (aaron-neopyter-jupyter--cb command)))))

(provide 'aaron-neopyter-jupyter)
;;; aaron-neopyter-jupyter.el ends here

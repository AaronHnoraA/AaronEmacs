;;; tools/agent/presence.el --- Collaborators shown in Treemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Treemacs marks the files collaborators work on (C69): a file bound to
;; collaborators shows " ◆N" and its pending review items " ✎M", a file an
;; agent only touched shows " ◇", a folder shows what is below it, and a file
;; an agent is working on now takes the working face.  The marks come from the
;; same session snapshots and collaboration records as the topology, through
;; Treemacs' own annotation API under the source `agent-collab'.  State events
;; schedule one refresh; nothing polls.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tools/agent/acp)
(require 'tools/agent/binding)

(declare-function treemacs-set-annotation-suffix "treemacs-annotations" (path suffix source))
(declare-function treemacs-set-annotation-face "treemacs-annotations" (path face source))
(declare-function treemacs-clear-annotation-suffixes "treemacs-annotations" (source))
(declare-function treemacs-clear-annotation-faces "treemacs-annotations" (source))
(declare-function treemacs-apply-annotations-in-buffer "treemacs-annotations" (buffer))
(declare-function treemacs-get-local-buffer "treemacs-scope" ())
(declare-function +agent-topology-file-counts "tools/agent/topology" (file))
(declare-function +agent-topology--sessions "tools/agent/topology" ())
(defvar +agent-collab-changed-functions)

(defface +agent-presence-working '((t :inherit font-lock-function-name-face :weight bold))
  "Treemacs face of a file a collaborator is working on now."
  :group '+agent-session)

(defvar +agent-presence--timer nil "Pending coalesced Treemacs refresh.")

(defun +agent-presence--path (file)
  "Return FILE as Treemacs names it: a client path without a trailing slash."
  (directory-file-name
   (or (and (fboundp 'remote-client-file-name) (ignore-errors (remote-client-file-name file)))
       file)))

(defun +agent-presence-entries (sessions counts)
  "Return ((PATH . PLIST) ...) of the files and folders SESSIONS work on.
PLIST has :collaborators, :pending, :working and :touched, and :folder for an
aggregated folder.  COUNTS is called with a file and returns (:pending)."
  (let ((table (make-hash-table :test #'equal)) order)
    (cl-flet ((entry (path)
                (or (gethash path table)
                    (progn (push path order)
                           (puthash path (list :collaborators 0 :pending 0) table))))
              (store (path plist) (puthash path plist table)))
      (dolist (session sessions)
        (unless (eq (plist-get session :kind) 'pi)
          (let ((working (memq (plist-get session :turn-state) '(running stopping))))
            (when-let* ((file (plist-get session :file)))
              (let* ((path (+agent-presence--path file))
                     (plist (entry path)))
                (setq plist (plist-put plist :collaborators (1+ (plist-get plist :collaborators))))
                (when working (setq plist (plist-put plist :working t)))
                (store path plist)))
            (dolist (file (plist-get session :touched))
              (let ((path (+agent-presence--path file)))
                (store path (plist-put (entry path) :touched t)))))))
      ;; Pending review items of every marked file.
      (dolist (path (reverse order))
        (when-let* ((pending (plist-get (funcall counts path) :pending)))
          (store path (plist-put (gethash path table) :pending pending))))
      ;; Folders show what is below them, up to the file's project root; each
      ;; file counts once, whatever number of sessions share that root.
      (let ((roots (delete-dups (delq nil (mapcar (lambda (session)
                                                    (and (plist-get session :root)
                                                         (+agent-presence--path (plist-get session :root))))
                                                  sessions)))))
        (dolist (path (reverse order))
          (let* ((plist (gethash path table))
                 (root (seq-find (lambda (root) (string-prefix-p (file-name-as-directory root) path))
                                 roots)))
            (when root
              (let ((folder (directory-file-name (file-name-directory path))))
                (while (and (string-prefix-p (file-name-as-directory root) (file-name-as-directory folder))
                            (not (equal folder root)))
                  (let ((summary (entry folder)))
                    (setq summary (plist-put summary :folder t))
                    (setq summary (plist-put summary :collaborators
                                             (+ (plist-get summary :collaborators)
                                                (plist-get plist :collaborators))))
                    (setq summary (plist-put summary :pending
                                             (+ (plist-get summary :pending)
                                                (or (plist-get plist :pending) 0))))
                    (when (plist-get plist :touched) (setq summary (plist-put summary :touched t)))
                    (when (plist-get plist :working) (setq summary (plist-put summary :working t)))
                    (store folder summary))
                  (setq folder (directory-file-name (file-name-directory folder)))))))))
      (mapcar (lambda (path) (cons path (gethash path table))) (nreverse order)))))

(defun +agent-presence--suffix (plist)
  "Return the annotation suffix of PLIST, or nil when it marks nothing."
  (let ((collaborators (or (plist-get plist :collaborators) 0))
        (pending (or (plist-get plist :pending) 0)))
    (cond ((> collaborators 0)
           (concat (format " ◆%d" collaborators) (if (> pending 0) (format " ✎%d" pending) "")))
          ((> pending 0) (format " ✎%d" pending))
          ((plist-get plist :touched) " ◇"))))

(defun +agent-presence--apply-visible ()
  "Repaint the annotations of the visible Treemacs buffer."
  (when-let* ((buffer (ignore-errors (treemacs-get-local-buffer))))
    (treemacs-apply-annotations-in-buffer buffer)))

(defun +agent-presence-apply (entries)
  "Replace the `agent-collab' Treemacs annotations with ENTRIES."
  (treemacs-clear-annotation-suffixes 'agent-collab)
  (treemacs-clear-annotation-faces 'agent-collab)
  (dolist (entry entries)
    (when-let* ((suffix (+agent-presence--suffix (cdr entry))))
      (treemacs-set-annotation-suffix (car entry) suffix 'agent-collab))
    (when (and (plist-get (cdr entry) :working) (not (plist-get (cdr entry) :folder)))
      (treemacs-set-annotation-face (car entry) '+agent-presence-working 'agent-collab)))
  (+agent-presence--apply-visible))

(defun +agent-presence-refresh ()
  "Recompute and repaint the collaborator marks in Treemacs."
  (interactive)
  (setq +agent-presence--timer nil)
  (when (featurep 'treemacs)
    (require 'tools/agent/topology)
    (+agent-presence-apply
     (+agent-presence-entries (+agent-topology--sessions) #'+agent-topology-file-counts))))

(defun +agent-presence--changed-h (&rest _)
  "Schedule one Treemacs refresh after a burst of state events."
  (unless +agent-presence--timer
    (setq +agent-presence--timer (run-with-idle-timer 1 nil #'+agent-presence-refresh))))

(add-hook '+agent-acp-changed-functions #'+agent-presence--changed-h)
(add-hook '+agent-collab-changed-functions #'+agent-presence--changed-h)

(provide 'tools/agent/presence)
;;; presence.el ends here

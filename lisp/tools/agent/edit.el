;;; tools/agent/edit.el --- Agents change text files through Emacs (C74) -*- lexical-binding: t; -*-

;;; Commentary:
;; `agent:file:edit' (the Noema host) resolves an agent's edit forms against
;; the text Emacs holds and asks here to apply them.  The text is the visiting
;; buffer's when there is one, so the person's unsaved edits are what the agent
;; edits, and they are not saved for the person; a clean buffer is saved, and
;; a file no buffer visits is written directly.  A version (the text's hash)
;; and each edit's old text guard the apply: anything that moved meanwhile is a
;; conflict the host retries on the newer text.  A change the host refuses as
;; too large or too contested reaches the person here, on the asking session.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function +agent-acp-session-buffer-for-token "tools/agent/acp" (token))
(declare-function +agent-acp--note-attention "tools/agent/acp" (buffer reason))
(declare-function +agent-acp-show-buffer "tools/agent/acp" (buffer &rest args))
(declare-function aaron-notify "aaron-notify" (title &rest args))
(defvar +agent-acp-session-name)

(defun +agent-edit--text (file)
  "Return (TEXT . BUFFER) for FILE: its visiting buffer's text, or the file's."
  (if-let* ((buffer (find-buffer-visiting file)))
      (cons (with-current-buffer buffer (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
            buffer)
    (cons (with-temp-buffer
            (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
            (buffer-string))
          nil)))

(defun +agent-edit--version (text)
  "Return the version of TEXT."
  (secure-hash 'sha1 text))

(defun +agent-edit--apply (edits)
  "Apply EDITS, (FROM TO OLD NEW) in characters, to the current buffer.
Return nil, changing nothing, when an old text is not where it was."
  (let ((sorted (sort (copy-sequence edits) (lambda (a b) (> (car a) (car b))))))
    (when (cl-every (lambda (edit)
                      (pcase-let ((`(,from ,to ,old ,_) edit))
                        (and (<= (point-min) (1+ from)) (<= (1+ to) (1+ (buffer-size)))
                             (equal (buffer-substring-no-properties (1+ from) (1+ to)) old))))
                    sorted)
      (save-excursion
        (dolist (edit sorted t)
          (pcase-let ((`(,from ,to ,_ ,new) edit))
            (goto-char (1+ from))
            (delete-region (1+ from) (1+ to))
            (insert new)))))))

(defun +agent-edit--edits (value)
  "Return the edits of VALUE as (FROM TO OLD NEW) lists."
  (mapcar (lambda (edit)
            (list (alist-get 'from edit) (alist-get 'to edit)
                  (or (alist-get 'oldText edit) "") (or (alist-get 'newText edit) "")))
          (append value nil)))

(defun +agent-edit-file-api (params)
  "Read or change a text file for the Noema host's agent:file:edit (C74)."
  (let* ((file (expand-file-name (alist-get 'file params)))
         (action (alist-get 'action params)))
    (unless (file-exists-p file) (error "No such file: %s" file))
    (pcase-let ((`(,text . ,buffer) (+agent-edit--text file)))
      (pcase action
        ("read" `((ok . t) (content . ,text) (version . ,(+agent-edit--version text))
                  (open . ,(if buffer t :json-false))))
        ("apply"
         (if (not (equal (alist-get 'version params) (+agent-edit--version text)))
             '((ok . :json-false) (conflict . t))
           (let ((edits (+agent-edit--edits (alist-get 'edits params))))
             (if buffer
                 (with-current-buffer buffer
                   (let ((clean (not (buffer-modified-p))))
                     (save-restriction
                       (widen)
                       (if (not (+agent-edit--apply edits))
                           '((ok . :json-false) (conflict . t))
                         ;; A clean buffer is saved; unsaved edits of the
                         ;; person's stay theirs to save.
                         (when clean (let ((save-silently t)) (basic-save-buffer)))
                         `((ok . t) (saved . ,(if clean t :json-false)) (buffer . ,(buffer-name)))))))
               (with-temp-buffer
                 (insert text)
                 (if (not (+agent-edit--apply edits))
                     '((ok . :json-false) (conflict . t))
                   (let ((coding-system-for-write 'utf-8-unix))
                     (write-region nil nil file nil 'silent))
                   '((ok . t) (saved . t))))))))
        (_ (error "Unknown file action: %s" action))))))

(defun +agent-edit-needs-person (payload)
  "Tell the person that the session in PAYLOAD needs them, and why (C74)."
  (let* ((buffer (+agent-acp-session-buffer-for-token (alist-get 'session payload)))
         (name (if buffer (buffer-local-value '+agent-acp-session-name buffer) "An agent"))
         (file (alist-get 'file payload)))
    (when buffer (+agent-acp--note-attention buffer 'input))
    (aaron-notify (format "%s needs you%s" name (if file (format " on %s" (file-name-nondirectory file)) ""))
                  :body (alist-get 'reason payload) :level 'warning :source "agent"
                  :action (when buffer (lambda () (+agent-acp-show-buffer buffer))))))

(provide 'tools/agent/edit)
;;; edit.el ends here

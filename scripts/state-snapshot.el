;;; state-snapshot.el --- Startup state snapshots for behaviour checks -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Records what a full startup leaves behind, one fact per line, so that a
;; change meant to keep behaviour can prove it: take a baseline before the
;; change, a snapshot after it, and compare.  See `make state-baseline' and
;; `make state-check' in docs/ops/maintenance.md.
;;
;; Recorded:
;;   FEATURE  every loaded feature
;;   HOOK     every hook variable's global functions, in order
;;   ADVICE   every advised function and its advice
;;   KEY      every binding in every keymap held by a variable
;;   VAR      every customizable variable's global value
;;   TIMER    every pending timer's function
;;
;; Lambdas and byte-code show as placeholders, so a change inside an
;; anonymous function is not seen; named functions are.  Values that depend
;; on the machine rather than on the configuration (host names) are left out
;; of comparisons through `aaron-state-ignore-regexps'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar aaron-state-ignore-regexps
  '("^VAR \\(?:tramp-default-host\\|tramp-default-host-alist\\|tramp-local-host-regexp\\|tramp-default-method-alist\\|connection-local-criteria-alist\\) ")
  "Snapshot lines that depend on the machine and are not compared.")

(defun aaron-state--fn-name (fn)
  "Return a stable label for function FN."
  (cond ((symbolp fn) (symbol-name fn))
        ((byte-code-function-p fn) "<bytecode>")
        ((or (functionp fn) (consp fn)) "<lambda>")
        (t (format "<%s>" (type-of fn)))))

(defun aaron-state--keymap-lines (map prefix depth)
  "Return binding lines of MAP under PREFIX, at most a few levels deep."
  (let (acc)
    (when (< depth 6)
      (map-keymap
       (lambda (ev def)
         (let ((key (string-trim (concat prefix " " (or (ignore-errors
                                                          (key-description (vector ev)))
                                                        "?")))))
           (cond ((and (symbolp def) (boundp def) (keymapp (symbol-value def)))
                  (push (format "%s => map:%s" key def) acc))
                 ((keymapp def)
                  (setq acc (nconc (aaron-state--keymap-lines def key (1+ depth)) acc)))
                 (def (push (format "%s => %s" key (aaron-state--fn-name def)) acc)))))
       map))
    acc))

(defun aaron-state-snapshot-lines ()
  "Return the current session's state as a sorted list of lines."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        lines)
    (dolist (f features)
      (push (format "FEATURE %s" f) lines))
    (mapatoms
     (lambda (s)
       (when (and (boundp s) (not (keywordp s)))
         (let ((name (symbol-name s))
               (val (ignore-errors (default-value s))))
           (when (and (string-match-p "\\(-hook\\|-hooks\\|-functions\\)\\'" name)
                      (listp val))
             (push (format "HOOK %s %S" name (mapcar #'aaron-state--fn-name val)) lines))
           (when (and (keymapp val) (not (symbolp val)))
             (dolist (l (aaron-state--keymap-lines val "" 0))
               (push (format "KEY %s %s" name l) lines)))
           (when (custom-variable-p s)
             (let ((print-length 30) (print-level 4))
               (push (format "VAR %s %s" name
                             (replace-regexp-in-string
                              "#<[^>]*>" "#<obj>" (prin1-to-string val)))
                     lines)))))
       (when (and (fboundp s) (advice--p (advice--symbol-function s)))
         (let (advs)
           (advice-mapc (lambda (f _props) (push (aaron-state--fn-name f) advs)) s)
           (push (format "ADVICE %s %S" s (sort advs #'string<)) lines)))))
    (dolist (timer (append timer-list timer-idle-list))
      (push (format "TIMER %s%s" (aaron-state--fn-name (timer--function timer))
                    (if (timer--idle-delay timer) " idle" ""))
            lines))
    (sort lines #'string<)))

(defun aaron-state-snapshot-write (file)
  "Write the current session's state to FILE."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (let ((coding-system-for-write 'utf-8-emacs-unix)
        (lines (aaron-state-snapshot-lines)))
    (with-temp-file file
      (insert (string-join lines "\n") "\n"))
    (message "state snapshot: %d lines -> %s" (length lines) file)))

(defun aaron-state--read (file)
  "Return the lines of snapshot FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-emacs-unix))
      (insert-file-contents file))
    (split-string (buffer-string) "\n" t)))

(defun aaron-state--rename (lines renames)
  "Apply RENAMES, an alist of (OLD . NEW) strings, to symbol names in LINES."
  (if (null renames)
      lines
    (let ((re (concat "\\_<" (regexp-opt (mapcar #'car renames)) "\\_>")))
      (mapcar (lambda (line)
                (replace-regexp-in-string
                 re (lambda (m) (cdr (assoc m renames))) line t t))
              lines))))

(defun aaron-state-compare (baseline current &optional renames-file)
  "Compare snapshot files BASELINE and CURRENT; return (ONLY-BEFORE . ONLY-AFTER).
RENAMES-FILE, when given, holds an alist of (OLD . NEW) symbol names
applied to BASELINE first, for changes that rename on purpose."
  (let* ((renames (and renames-file
                       (with-temp-buffer
                         (insert-file-contents renames-file)
                         (read (current-buffer)))))
         (ignored (lambda (l) (seq-some (lambda (re) (string-match-p re l))
                                        aaron-state-ignore-regexps)))
         (a (seq-remove ignored (aaron-state--rename (aaron-state--read baseline) renames)))
         (b (seq-remove ignored (aaron-state--read current)))
         (count (make-hash-table :test 'equal))
         before after)
    (dolist (l a) (cl-incf (gethash l count 0)))
    (dolist (l b) (cl-decf (gethash l count 0)))
    (maphash (lambda (l n)
               (cond ((> n 0) (dotimes (_ n) (push l before)))
                     ((< n 0) (dotimes (_ (- n)) (push l after)))))
             count)
    (cons (sort before #'string<) (sort after #'string<))))

(defun aaron-state-compare-and-exit (baseline current &optional renames-file)
  "Print the difference between BASELINE and CURRENT, then exit.
Exit status is 0 when the snapshots match, 1 otherwise."
  (let* ((diff (aaron-state-compare baseline current renames-file))
         (before (car diff))
         (after (cdr diff)))
    (dolist (l before) (princ (format "- %s\n" l)))
    (dolist (l after) (princ (format "+ %s\n" l)))
    (princ (format "%d line(s) only before, %d only after\n" (length before) (length after)))
    (kill-emacs (if (or before after) 1 0))))

(provide 'state-snapshot)
;;; state-snapshot.el ends here

;;; checkers/problems/config.el --- Diagnostics pickers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'aaron-funcs)
(require 'seq)

(declare-function consult--jump-state "consult" ())
(declare-function consult--lookup-candidate "consult" (selected candidates &optional no-error))
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--type-group "consult" (types))
(declare-function consult--type-narrow "consult" (types))
(declare-function consult-flymake--candidates "consult-flymake" (diags))
(declare-function flymake--lookup-type-property "flymake" (type property &optional default))
(declare-function flymake--project-diagnostics "flymake" (&optional project))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-make-diagnostic "flymake" (locus beg end type info &optional data overlay-properties))
(declare-function flymake-show-project-diagnostics "flymake" ())
(declare-function lsp-diagnostics "lsp-mode" (&optional current-workspace?))
(declare-function remote-canonicalize-file-name "remote-fs" (file-name &optional directory))
(declare-function remote-client-file-name "remote-fs" (file-name &optional adapter))

(defconst +problems--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?n . "Note"))
  "Narrow keys used by the diagnostics picker.")

(defun +problems--severity-label (severity)
  "Return a display label for SEVERITY."
  (pcase severity
    (:error "Errors")
    (:warning "Warnings")
    (:note "Notes")
    (_ "Diagnostics")))

(defun +problems--severity-category (diag)
  "Return DIAG severity as `error', `warning', or `note'."
  (pcase (flymake--lookup-type-property
          (flymake-diagnostic-type diag)
          'flymake-category)
    ('flymake-error 'error)
    ('flymake-warning 'warning)
    (_ 'note)))

(defun +problems--filter-diags (diags severity)
  "Return DIAGS filtered by SEVERITY when non-nil."
  (let ((category (pcase severity
                    (:error 'error)
                    (:warning 'warning)
                    (:note 'note)
                    (_ nil))))
    (if category
        (seq-filter (lambda (diag)
                      (eq (+problems--severity-category diag) category))
                    diags)
      diags)))

(defun +problems--collect (scope)
  "Collect diagnostics for SCOPE."
  (pcase scope
    ('project
     (when-let* ((project (project-current nil default-directory)))
       (if (fboundp 'flymake--project-diagnostics)
           (flymake--project-diagnostics project)
         (flymake-diagnostics))))
    (_
     (unless (bound-and-true-p flymake-mode)
       (user-error "Flymake is not active in the current buffer"))
     (flymake-diagnostics))))

(defvar +problems--lsp-list-only-owned nil
  "Alist of (ROOT . LOCI) previously written into
`flymake-list-only-diagnostics' by
`+problems--sync-lsp-list-only-diagnostics'.  Tracking what this
function itself wrote lets it retract entries whose lsp-mode
diagnostics have since cleared, instead of leaking them forever in a
global variable.")

(defun +problems--lsp-severity-to-flymake (severity)
  "Return the Flymake diagnostic type for LSP protocol SEVERITY.
An absent severity means `Error\=' per the LSP specification."
  (pcase severity
    (1 :error)
    (2 :warning)
    ((or 3 4) :note)
    (_ :error)))

(defun +problems--lsp-diagnostic-to-flymake (locus diag)
  "Convert raw LSP protocol DIAG into a file-locus Flymake diagnostic.
LOCUS is the file name the diagnostic is attached to.  Uses the
`(LINE . COL)\=' file-locus convention Flymake expects for diagnostics on
files with no live buffer, so lsp-mode's entries render correctly in
`flymake-show-project-diagnostics\='."
  (let* ((range (plist-get diag :range))
         (start (plist-get range :start))
         (line (1+ (or (plist-get start :line) 0)))
         (char (1+ (or (plist-get start :character) 0))))
    (flymake-make-diagnostic
     locus (cons line char) nil
     (+problems--lsp-severity-to-flymake (plist-get diag :severity))
     (list (plist-get diag :source) (plist-get diag :code)
           (plist-get diag :message)))))

(defun +problems--sync-lsp-list-only-diagnostics (root)
  "Bridge lsp-mode's workspace diagnostics under ROOT into Flymake.
lsp-mode only reports diagnostics through its Flymake backend for the
current buffer (`lsp-diagnostics--flymake-update-diagnostics' keys off
`buffer-file-name'), so `flymake--project-diagnostics' never learns
about JDTLS/lsp-mode errors in files that are not visited.  This
populates the same public `flymake-list-only-diagnostics' extension
point Flymake itself uses for diagnostics on unopened files, for
whichever files have no live buffer."
  (when (fboundp 'lsp-diagnostics)
    (let* ((canonical-root
            (ignore-errors (remote-canonicalize-emacs-file-name root)))
           (previously (and canonical-root
                            (cdr (assoc canonical-root
                                       +problems--lsp-list-only-owned))))
           (open-files (make-hash-table :test #'equal))
           (fresh nil))
      (when canonical-root
        (dolist (buffer (buffer-list))
          (when-let* ((file (buffer-local-value 'buffer-file-name buffer))
                      (canonical
                       (ignore-errors
                         (remote-canonicalize-emacs-file-name file))))
            (puthash canonical t open-files)))
        (maphash
         (lambda (file diags)
           (let* ((canonical-file
                   (and (stringp file)
                        (ignore-errors
                          (remote-canonicalize-emacs-file-name file))))
                  (client-file
                   (and canonical-file
                        (ignore-errors
                          (remote-client-file-name canonical-file)))))
             (when (and diags canonical-file
                        (not (gethash canonical-file open-files))
                        (string-prefix-p
                         (file-name-as-directory canonical-root)
                         canonical-file))
               (let ((locus (or client-file canonical-file)))
                 (push locus fresh)
                 (setf (alist-get locus flymake-list-only-diagnostics nil nil #'equal)
                       (mapcar (lambda (diag)
                                 (+problems--lsp-diagnostic-to-flymake locus diag))
                               diags))))))
         (lsp-diagnostics t))
        (dolist (stale (seq-difference previously fresh))
          (setf (alist-get stale flymake-list-only-diagnostics nil 'remove #'equal)
                nil))
        (setf (alist-get canonical-root +problems--lsp-list-only-owned nil nil #'equal)
              fresh)))))

(defun +problems/project-full ()
  "Show all project diagnostics, including files that are not open.
`+problems/project' uses the buffer-centric `consult-flymake' picker,
which silently drops any diagnostic whose locus is not a live buffer
\(see `consult-flymake--candidates''s `buffer-live-p' check\), so it can
never show a cross-file error in a file nobody has visited yet.  This
instead renders Emacs's native `flymake-project-diagnostics-mode'
listing, which understands file-locus diagnostics directly, after
bridging in whatever lsp-mode currently knows for this project."
  (interactive)
  (when-let* ((project (project-current nil default-directory)))
    (+problems--sync-lsp-list-only-diagnostics (project-root project)))
  (flymake-show-project-diagnostics))

(defun +problems--read (scope &optional severity)
  "Read a diagnostic from SCOPE, optionally filtered by SEVERITY."
  (require 'consult)
  (require 'consult-flymake)
  (consult--read
   (consult-flymake--candidates
    (+problems--filter-diags
     (+problems--collect scope)
     severity))
   :prompt (format "%s %s: "
                   (pcase scope
                     ('project "Project")
                     (_ "Buffer"))
                   (downcase (+problems--severity-label severity)))
   :category 'consult-flymake-error
   :history t
   :require-match t
   :sort nil
   :group (consult--type-group +problems--narrow)
   :narrow (consult--type-narrow +problems--narrow)
   :lookup #'consult--lookup-candidate
   :state (consult--jump-state)))

(defun +problems/buffer ()
  "Show diagnostics for the current buffer with preview."
  (interactive)
  (+problems--read 'buffer))

(defun +problems/project ()
  "Show diagnostics for the current project with preview."
  (interactive)
  (+problems--read 'project))

(defun +problems/buffer-errors ()
  "Show current-buffer errors with preview."
  (interactive)
  (+problems--read 'buffer :error))

(defun +problems/buffer-warnings ()
  "Show current-buffer warnings with preview."
  (interactive)
  (+problems--read 'buffer :warning))

(defun +problems/buffer-notes ()
  "Show current-buffer notes with preview."
  (interactive)
  (+problems--read 'buffer :note))

(defun +problems/project-errors ()
  "Show project errors with preview."
  (interactive)
  (+problems--read 'project :error))

(defun +problems/project-warnings ()
  "Show project warnings with preview."
  (interactive)
  (+problems--read 'project :warning))

(defun +problems/project-notes ()
  "Show project notes with preview."
  (interactive)
  (+problems--read 'project :note))

(aaron-leader!
  "c !" '(:def +problems/buffer :which-key "buffer problems"))

(provide 'checkers/problems/config)
;;; checkers/problems/config.el ends here

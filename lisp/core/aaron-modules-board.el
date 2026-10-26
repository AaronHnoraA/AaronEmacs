;;; aaron-modules-board.el --- Module board and module descriptions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The supervision surface of the module system:
;;
;;   `aaron/modules'          every module by category with state, load
;;                            time, what it installed (hooks, advice,
;;                            timers, keys), its settings and its findings
;;   `aaron/describe-module'  one module in full, with source links
;;   `aaron/doctor'           run the doctor and show the board
;;
;; It reads the registry and ledger in `aaron-modules', the doctor in
;; `aaron-doctor', and the `config' registry for the settings a module owns.

;;; Code:

(require 'aaron-modules)
(require 'aaron-doctor)
(require 'aaron-ui-board)

(defvar config--registry)

;;; Facts about a module

(defun aaron-modules-board--source-names (module)
  "Return the file names of MODULE's sources."
  (mapcar #'file-name-nondirectory (aaron-module-source-files module)))

(defun aaron-modules-board--settings (module)
  "Return the `config' setting names registered from MODULE's files."
  (when (boundp 'config--registry)
    (let ((files (aaron-modules-board--source-names module))
          names)
      (maphash (lambda (name entry)
                 (when (member (plist-get entry :source) files)
                   (push name names)))
               config--registry)
      (sort names (lambda (a b) (string< (format "%s" a) (format "%s" b)))))))

(defun aaron-modules-board--live-timers (module)
  "Return MODULE's recorded timers that are still scheduled."
  (seq-filter (lambda (timer) (or (memq timer timer-list) (memq timer timer-idle-list)))
              (aaron-module-ledger (aaron-module-key module) 'timers)))

(defun aaron-modules-board--state-badge (module)
  "Return (LABEL . TONE) for MODULE's state."
  (pcase (aaron-module-state module)
    ('loaded '("loaded" . success))
    ('failed '("failed" . danger))
    ('waiting '("waiting" . warning))
    ('autoload '("autoload" . info))
    (_ '("declared" . muted))))

(defun aaron-modules-board--findings (key)
  "Return the number of doctor findings for KEY, or nil if not diagnosed."
  (when-let* ((r (gethash key aaron-doctor-results)))
    (+ (length (plist-get r :errors)) (length (plist-get r :warnings))
       (length (plist-get r :violations)) (length (plist-get r :conflicts)))))

(defun aaron-modules-board--unowned-files ()
  "Return Lisp files under the module load path that no module owns."
  (let ((owned (make-hash-table :test 'equal)))
    (maphash (lambda (_key module)
               (dolist (f (aaron-module-source-files module))
                 (puthash (expand-file-name f) t owned)))
             aaron-modules)
    (cl-loop for root in aaron-module-load-path
             append (cl-loop for f in (directory-files-recursively root "\\.el\\'")
                             unless (or (gethash (expand-file-name f) owned)
                                        (string-match-p "/\\.elsa/\\|/core/\\|/init-modules\\.el\\'" f))
                             collect f))))

(defun aaron-modules-board--keymap-name (keymap)
  "Return the name of a variable holding KEYMAP, or a placeholder."
  (or (cl-loop for name in '(global-map esc-map ctl-x-map mode-specific-map)
               if (eq (symbol-value name) keymap) return (symbol-name name))
      (catch 'found
        (mapatoms (lambda (s)
                    (when (and (boundp s) (eq (symbol-value s) keymap))
                      (throw 'found (symbol-name s))))))
      "<keymap>"))

;;; Board

(defun aaron-modules-board--insert-module (key)
  "Insert the board row for module KEY."
  (let* ((module (aaron-module-get key))
         (badge (aaron-modules-board--state-badge module))
         (time (aaron-module-load-time module))
         (findings (aaron-modules-board--findings key))
         (count (lambda (kind) (length (aaron-module-ledger key kind)))))
    (aaron-ui-board-insert-row
     :id key
     :icon (if (aaron-module-directory-p module) 'directory 'package)
     :badge (car badge) :badge-tone (cdr badge)
     :title (symbol-name (cdr key))
     :meta (string-join
            (delq nil
                  (list (and time (format "%.0fms" (* 1000 time)))
                        (format "%d hooks · %d advice · %d timers · %d keys"
                                (funcall count 'hooks) (funcall count 'advice)
                                (funcall count 'timers) (funcall count 'keys))
                        (and (aaron-module-flags module)
                             (mapconcat #'symbol-name (aaron-module-flags module) " "))
                        (and findings (> findings 0) (format "%d findings" findings))
                        (and (aaron-module-doctor module) "doctor")))
            "  ")
     :detail (aaron-module-error module)
     :action (lambda (_) (aaron/describe-module key))
     :help "RET: describe module")))

(defun aaron-modules-board--render ()
  "Render the module board into the current buffer."
  (let* ((keys (aaron-module-list))
         (states (mapcar (lambda (k) (aaron-module-state (aaron-module-get k))) keys))
         (failed (cl-count 'failed states))
         (waiting (cl-count 'waiting states))
         (summary (aaron-doctor-summary))
         (unowned (aaron-modules-board--unowned-files))
         groups)
    (dolist (k keys) (cl-pushnew (car k) groups))
    (aaron-ui-board-insert-page-header
     "Modules"
     :icon 'management
     :subtitle (format "%d modules in %d categories, loaded in %s"
                       (length keys) (length groups)
                       (if aaron-modules-init-time
                           (format "%.2fs" aaron-modules-init-time) "-"))
     :stats (delq nil
                  (list (cons (format "%d loaded" (cl-count 'loaded states)) 'success)
                        (and (> waiting 0) (cons (format "%d waiting" waiting) 'warning))
                        (and (> failed 0) (cons (format "%d failed" failed) 'danger))
                        (and aaron-module-key-conflicts
                             (cons (format "%d key conflicts" (length aaron-module-key-conflicts))
                                   'warning))
                        (and (> (plist-get summary :modules) 0)
                             (cons (format "%d policy hits" (plist-get summary :violations))
                                   (if (> (plist-get summary :violations) 0) 'warning 'success)))))
     :actions '((:label "Doctor" :command aaron/doctor :help "Diagnose every module" :primary t)
                (:label "Load module" :command aaron/load-module
                 :help "Retry a failed module or start a waiting one")
                (:label "Health" :command config-health :help "Open Config Health")))
    (insert "\n")
    (dolist (group (nreverse groups))
      (let ((members (seq-filter (lambda (k) (eq (car k) group)) keys)))
        (aaron-ui-board-insert-section (substring (symbol-name group) 1) (length members))
        (dolist (k members) (aaron-modules-board--insert-module k))
        (insert "\n")))
    (when aaron-module-key-conflicts
      (aaron-ui-board-insert-section "Key conflicts" (length aaron-module-key-conflicts) 'warning)
      (dolist (c aaron-module-key-conflicts)
        (aaron-ui-board-insert-row
         :icon 'warning
         :title (format "%s  %s" (aaron-modules-board--keymap-name (plist-get c :keymap))
                        (plist-get c :key))
         :meta (format "%s (%s) → %s (%s)"
                       (plist-get c :old) (cdr (plist-get c :old-module))
                       (plist-get c :new) (cdr (plist-get c :new-module)))
         :action (let ((key (plist-get c :new-module)))
                   (lambda (_) (aaron/describe-module key)))))
      (insert "\n"))
    (when unowned
      (aaron-ui-board-insert-section "Files no module owns" (length unowned) 'muted)
      (dolist (f unowned)
        (aaron-ui-board-insert-row
         :icon 'path
         :title (file-relative-name f user-emacs-directory)
         :action (lambda (_) (find-file f)))))))

(define-derived-mode aaron-modules-board-mode aaron-ui-board-mode "Modules"
  "Board of the configuration's modules.")

;;;###autoload
(defun aaron/modules (&optional no-select)
  "Show every module with its state, load time, footprint and findings.
With NO-SELECT, refresh without selecting the board."
  (interactive)
  (let ((buffer (get-buffer-create "*Modules*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'aaron-modules-board-mode)
        (aaron-modules-board-mode))
      (aaron-ui-board-set-header "Modules" 'management)
      (setq-local aaron-ui-board-refresh-function
                  (lambda () (aaron/modules t)))
      (aaron-ui-board-render #'aaron-modules-board--render))
    (unless no-select (pop-to-buffer buffer))
    buffer))

;;;###autoload
(defun aaron/doctor ()
  "Diagnose every module, then show the module board."
  (interactive)
  (let ((n (aaron-doctor-run)))
    (aaron/modules)
    (message "Doctor: %d module(s) with findings" n)))

;;; Describe

(defun aaron-modules-board--insert-list (title items fn)
  "Insert section TITLE listing ITEMS, each rendered as a row by FN."
  (when items
    (aaron-ui-board-insert-section title (length items))
    (dolist (item items) (funcall fn item))
    (insert "\n")))

(defun aaron-modules-board--fn-label (fn)
  "Return a short label for function FN."
  (if (symbolp fn) (symbol-name fn) "<lambda>"))

(defun aaron-modules-board--describe-render (key)
  "Render the description of module KEY."
  (let* ((module (aaron-module-get key))
         (badge (aaron-modules-board--state-badge module))
         (doctor (gethash key aaron-doctor-results)))
    (aaron-ui-board-insert-page-header
     (format "%s %s" (car key) (cdr key))
     :icon (if (aaron-module-directory-p module) 'directory 'package)
     :subtitle (if (aaron-module-directory-p module)
                   (abbreviate-file-name (aaron-module-path module))
                 (format "feature %s" (aaron-module-feature module)))
     :stats (list badge)
     :actions (delq nil
                    (list (list :label "Diagnose" :help "Run this module's doctor"
                                :command (lambda () (aaron-doctor-module key)
                                           (aaron/describe-module key)))
                          (and (aaron-module-doctor module)
                               (list :label "Own doctor" :command (aaron-module-doctor module)
                                     :help (format "%s" (aaron-module-doctor module))))
                          (and (memq (aaron-module-state module) '(failed waiting))
                               (list :label "Load now" :help "Load this module now"
                                     :command (lambda () (aaron/load-module key)
                                                (aaron/describe-module key)))))))
    (insert "\n")
    (aaron-ui-board-insert-section "Module")
    (aaron-ui-board-insert-field "Load time"
                                 (if-let* ((time (aaron-module-load-time module)))
                                     (format "%.1fms" (* 1000 time))
                                   "-"))
    (aaron-ui-board-insert-field "Depth" (format "%S" (aaron-module-depth module)))
    (aaron-ui-board-insert-field "Flags" (format "%s" (or (aaron-module-flags module) "-")))
    (when (aaron-module-after module)
      (aaron-ui-board-insert-field "After" (format "%s" (aaron-module-after module))))
    (when (aaron-module-autoload module)
      (aaron-ui-board-insert-field "Autoloads" (format "%s" (aaron-module-autoload module))))
    (when (aaron-module-error module)
      (aaron-ui-board-insert-field "Error" (aaron-module-error module) 'error))
    (insert "\n")
    (aaron-modules-board--insert-list
     "Sources" (aaron-module-source-files module)
     (lambda (f) (aaron-ui-board-insert-row :icon 'path :title (abbreviate-file-name f)
                                            :action (lambda (_) (find-file f)))))
    (aaron-modules-board--insert-list
     "Settings" (aaron-modules-board--settings module)
     (lambda (name) (aaron-ui-board-insert-row
                     :icon 'gear :title (format "%s" name)
                     :meta (format "%S" (and (boundp name) (symbol-value name)))
                     :action (lambda (_) (describe-variable name)))))
    (when doctor
      (aaron-modules-board--insert-list
       "Doctor" (append (mapcar (lambda (e) (cons 'danger e)) (plist-get doctor :errors))
                        (mapcar (lambda (w) (cons 'warning w)) (plist-get doctor :warnings))
                        (mapcar (lambda (n) (cons 'muted n)) (plist-get doctor :notes)))
       (lambda (f) (aaron-ui-board-insert-row :icon 'diagnostics :badge-tone (car f)
                                              :badge (symbol-name (car f)) :title (cdr f))))
      (aaron-modules-board--insert-list
       "Policy" (plist-get doctor :violations)
       (pcase-lambda (`(,id ,file ,line ,text))
         (aaron-ui-board-insert-row
          :icon 'warning :badge (symbol-name id) :badge-tone 'warning
          :title (format "%s:%d" (file-name-nondirectory file) line)
          :detail text
          :action (lambda (_) (find-file file) (goto-char (point-min))
                    (forward-line (1- line)))))))
    (aaron-modules-board--insert-list
     "Key conflicts" (seq-filter (lambda (c) (or (equal (plist-get c :new-module) key)
                                                 (equal (plist-get c :old-module) key)))
                                 aaron-module-key-conflicts)
     (lambda (c) (aaron-ui-board-insert-row
                  :icon 'warning
                  :title (format "%s  %s" (aaron-modules-board--keymap-name (plist-get c :keymap))
                                 (plist-get c :key))
                  :meta (format "%s (%s) → %s (%s)"
                                (plist-get c :old) (cdr (plist-get c :old-module))
                                (plist-get c :new) (cdr (plist-get c :new-module))))))
    (aaron-modules-board--insert-list
     "Hooks" (aaron-module-ledger key 'hooks)
     (lambda (h) (aaron-ui-board-insert-row
                  :icon 'section :title (format "%s" (car h))
                  :meta (aaron-modules-board--fn-label (cdr h)))))
    (aaron-modules-board--insert-list
     "Advice" (aaron-module-ledger key 'advice)
     (lambda (a) (aaron-ui-board-insert-row
                  :icon 'section :title (format "%s" (nth 0 a))
                  :meta (format "%s %s" (nth 1 a) (aaron-modules-board--fn-label (nth 2 a))))))
    (aaron-modules-board--insert-list
     "Live timers" (aaron-modules-board--live-timers module)
     (lambda (timer) (aaron-ui-board-insert-row
                      :icon 'clock
                      :title (aaron-modules-board--fn-label (timer--function timer))
                      :meta (if (timer--idle-delay timer) "idle" "")
                      :action (lambda (_) (when (yes-or-no-p "Cancel this timer? ")
                                            (cancel-timer timer)
                                            (aaron/describe-module key))))))
    (aaron-modules-board--insert-list
     "Keys" (aaron-module-ledger key 'keys)
     (pcase-lambda (`(,map ,desc ,def))
       (aaron-ui-board-insert-row
        :icon 'tag :title desc
        :meta (format "%s  %s" (aaron-modules-board--fn-label def)
                      (aaron-modules-board--keymap-name map)))))))

;;;###autoload
(defun aaron/describe-module (key)
  "Describe module KEY: state, sources, settings, findings and footprint."
  (interactive (list (aaron-module-read "Describe module: ")))
  (let ((buffer (get-buffer-create (format "*Module %s %s*" (car key) (cdr key)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'aaron-modules-board-mode)
        (aaron-modules-board-mode))
      (aaron-ui-board-set-header (format "%s %s" (car key) (cdr key)) 'package)
      (setq-local aaron-ui-board-refresh-function
                  (lambda () (aaron/describe-module key)))
      (aaron-ui-board-render (lambda () (aaron-modules-board--describe-render key))))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defalias 'module-board #'aaron/modules)
;;;###autoload
(defalias 'module-doctor #'aaron/doctor)

(provide 'aaron-modules-board)
;;; aaron-modules-board.el ends here

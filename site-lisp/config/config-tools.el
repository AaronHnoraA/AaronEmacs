;;; config-tools.el --- Management board for the config registry -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A `tabulated-list' board to browse, edit, reset, reload and persist every
;; registered configuration item, plus a `transient' dispatch for global
;; actions.  Modeled on the repo's `git-board' and `*-dispatch' idioms.
;;
;; Entry points:
;;   M-x config-board        open the board
;;   M-x config-dispatch     transient menu (SPC h c)

;;; Code:

(require 'config)
(require 'tabulated-list)
(require 'transient)

(declare-function evil-set-initial-state "evil" (mode state))
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))

(defvar config-board-buffer-name "*Config*"
  "Buffer name for the configuration board.")

(defvar-local config--group-filter nil
  "When non-nil, the board only shows items in this group symbol.")

;;; Rendering ----------------------------------------------------------------

(defun config--truncate (string width)
  "Return STRING truncated to WIDTH characters."
  (if (> (length string) width)
      (concat (substring string 0 (max 0 (- width 1))) "…")
    string))

(defun config--value-string (entry)
  "Return a display string for ENTRY's current value."
  (pcase (plist-get entry :kind)
    ('file
     (if (file-exists-p (plist-get entry :path)) "exists" "missing"))
    ('hook
     (let* ((hook (plist-get entry :hook))
            (cands (plist-get entry :candidates))
            (active (cl-count-if (lambda (c) (config-hook-member-p hook (car c)))
                                 cands)))
       (format "%d/%d on" active (length cands))))
    (_ (config--truncate (prin1-to-string (config-get (plist-get entry :name)))
                         22))))

(defun config--doc-string (entry)
  "Return a static hint string for ENTRY: doc > choices > empty."
  (pcase (plist-get entry :kind)
    ('hook (let ((cands (plist-get entry :candidates)))
             (config--truncate
              (mapconcat (lambda (c) (symbol-name (car c))) cands " ")
              22)))
    ('file (or (plist-get entry :doc) ""))
    (_ (or (and-let* ((d (plist-get entry :doc))) (config--truncate d 22))
           (and-let* ((cs (plist-get entry :choices)))
             (config--truncate (format "%S" cs) 22))
           ""))))

(defun config--type-string (entry)
  "Return a display string for ENTRY's type/kind."
  (pcase (plist-get entry :kind)
    ('file "file")
    ('hook "hook")
    (_ (format "%s" (or (plist-get entry :type) 'sexp)))))

(defun config--sorted-entries ()
  "Return registered entries, filtered and sorted by group then name."
  (let ((items (config-list config--group-filter)))
    (sort items
          (lambda (a b)
            (let ((ga (symbol-name (or (plist-get a :group) 'zzz)))
                  (gb (symbol-name (or (plist-get b :group) 'zzz))))
              (if (string= ga gb)
                  (string< (symbol-name (plist-get a :name))
                           (symbol-name (plist-get b :name)))
                (string< ga gb)))))))

(defun config--list-entries ()
  "Build `tabulated-list-entries' from the registry."
  (mapcar
   (lambda (entry)
     (let ((name (plist-get entry :name)))
       (list name
             (vector (symbol-name (or (plist-get entry :group) 'misc))
                     (symbol-name name)
                     (config--type-string entry)
                     (config--value-string entry)
                     (config--doc-string entry)
                     (or (plist-get entry :source) "")))))
   (config--sorted-entries)))

(define-derived-mode config-board-mode tabulated-list-mode "Config"
  "Major mode for the configuration board."
  (setq tabulated-list-format [("Group" 12 t)
                               ("Name" 30 t)
                               ("Type" 9 t)
                               ("Value" 24 nil)
                               ("Doc" 22 nil)
                               ("Source" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Group" nil))
  (tabulated-list-init-header))

(defun config-board-refresh ()
  "Refresh the configuration board."
  (interactive)
  (when (derived-mode-p 'config-board-mode)
    (setq tabulated-list-entries (config--list-entries))
    (tabulated-list-print t)))

;;;###autoload
(defun config-board ()
  "Open the configuration management board."
  (interactive)
  (let ((buffer (get-buffer-create config-board-buffer-name)))
    (with-current-buffer buffer
      (config-board-mode)
      (config-board-refresh))
    (switch-to-buffer buffer)))

;;; Item helpers -------------------------------------------------------------

(defun config--entry-at-point ()
  "Return (NAME . ENTRY) for the row at point, or signal an error."
  (let* ((name (tabulated-list-get-id))
         (entry (and name (config--entry name))))
    (unless entry (user-error "No configuration item on this line"))
    (cons name entry)))

(defun config--read-choice (entry current)
  "Read a value for ENTRY among its `:choices', defaulting to CURRENT."
  (let* ((choices (or (plist-get entry :choices) (list current)))
         (sel (completing-read
               (format "%s: " (plist-get entry :name))
               (mapcar (lambda (c) (format "%S" c)) choices)
               nil nil)))
    (car (read-from-string sel))))

(defun config--edit-variable (entry name)
  "Prompt for and set a new value for variable item NAME using ENTRY."
  (let ((type (plist-get entry :type))
        (current (config-get name))
        (choices (plist-get entry :choices)))
    (config-set
     name
     (pcase type
       ('boolean (not current))
       ('integer (read-number (format "%s: " name) (and (integerp current) current)))
       ('number (read-number (format "%s: " name) (and (numberp current) current)))
       ('string (read-string (format "%s: " name) (and (stringp current) current)))
       ((or 'function 'face) (config--read-choice entry current))
       (_ (if choices
              (config--read-choice entry current)
            (car (read-from-string
                  (read-string (format "%s (sexp): " name)
                               (prin1-to-string current))))))))))

(defun config--toggle-hook (entry)
  "Toggle a candidate on the hook described by ENTRY."
  (let* ((hook (plist-get entry :hook))
         (cands (plist-get entry :candidates))
         (table (mapcar
                 (lambda (c)
                   (cons (format "%s %s"
                                 (if (config-hook-member-p hook (car c)) "[x]" "[ ]")
                                 (cdr c))
                         (car c)))
                 cands))
         (sel (completing-read "Toggle on hook: " table nil t))
         (fn (cdr (assoc sel table))))
    (when fn (config-hook-toggle hook fn))))

;;; Commands -----------------------------------------------------------------

(defun config-edit ()
  "Edit the configuration item at point (dispatch by kind)."
  (interactive)
  (pcase-let ((`(,name . ,entry) (config--entry-at-point)))
    (pcase (plist-get entry :kind)
      ('file (config-file-open name))
      ('hook (config--toggle-hook entry))
      (_ (config--edit-variable entry name))))
  (config-board-refresh))

(defun config-toggle ()
  "Toggle the boolean or hook item at point."
  (interactive)
  (pcase-let ((`(,name . ,entry) (config--entry-at-point)))
    (pcase (plist-get entry :kind)
      ('hook (config--toggle-hook entry))
      ('variable
       (if (eq (plist-get entry :type) 'boolean)
           (config-set name (not (config-get name)))
         (user-error "%s is not a boolean" name)))
      (_ (user-error "Cannot toggle %s" name))))
  (config-board-refresh))

(defun config-reset-at-point ()
  "Remove the stored override for the variable item at point.
The live value is not changed; the override is removed from the store."
  (interactive)
  (pcase-let ((`(,name . ,entry) (config--entry-at-point)))
    (unless (eq (plist-get entry :kind) 'variable)
      (user-error "Only variable items have stored overrides"))
    (config-reset name))
  (config-board-refresh))

(defun config-reload-file ()
  "Reload the file item at point."
  (interactive)
  (pcase-let ((`(,name . ,entry) (config--entry-at-point)))
    (unless (eq (plist-get entry :kind) 'file)
      (user-error "%s is not a file item" name))
    (config-file-reload name))
  (config-board-refresh))

(defun config-open-file ()
  "Open the file item at point for editing."
  (interactive)
  (pcase-let ((`(,name . ,_entry) (config--entry-at-point)))
    (config-file-open name)))

(defun config-filter-group ()
  "Filter the board by a group; empty input clears the filter."
  (interactive)
  (let* ((groups (delete-dups
                  (delq nil (mapcar (lambda (e) (plist-get e :group))
                                    (config-list)))))
         (sel (completing-read "Group (empty = all): "
                               (mapcar #'symbol-name groups) nil nil)))
    (setq config--group-filter
          (and (not (string-empty-p sel)) (intern sel)))
    (config-board-refresh)))

(defun config-save ()
  "Force-write the override store to disk."
  (interactive)
  (config--persist)
  (message "config: saved %s" config-store-file))

(defun config-reset-all ()
  "Drop every stored override (live values stay as-is; store becomes empty)."
  (interactive)
  (when (yes-or-no-p "Remove ALL stored config overrides? ")
    (setq config--overrides nil)
    (config--persist)
    (when (get-buffer config-board-buffer-name)
      (with-current-buffer config-board-buffer-name
        (config-board-refresh)))
    (message "config: all overrides cleared")))

(defun config-open-store ()
  "Open the override store file."
  (interactive)
  (find-file config-store-file))

;;; Keymap -------------------------------------------------------------------

(let ((map config-board-mode-map))
  (define-key map (kbd "RET") #'config-edit)
  (define-key map (kbd "e") #'config-edit)
  (define-key map (kbd "t") #'config-toggle)
  (define-key map (kbd "d") #'config-reset-at-point)
  (define-key map (kbd "r") #'config-reload-file)
  (define-key map (kbd "o") #'config-open-file)
  (define-key map (kbd "f") #'config-filter-group)
  (define-key map (kbd "s") #'config-save)
  (define-key map (kbd "g") #'config-board-refresh)
  (define-key map (kbd "q") #'quit-window))

(with-eval-after-load 'evil
  (evil-set-initial-state 'config-board-mode 'normal)
  (evil-define-key* 'normal config-board-mode-map
    (kbd "RET") #'config-edit
    (kbd "<return>") #'config-edit
    (kbd "e") #'config-edit
    (kbd "t") #'config-toggle
    (kbd "d") #'config-reset-at-point
    (kbd "r") #'config-reload-file
    (kbd "o") #'config-open-file
    (kbd "f") #'config-filter-group
    (kbd "s") #'config-save
    (kbd "g") #'config-board-refresh
    (kbd "q") #'quit-window))

;;; Transient dispatch -------------------------------------------------------

(transient-define-prefix config-dispatch ()
  "Configuration management workflow."
  [["Board"
    ("c" "open board" config-board)
    ("f" "filter group" config-filter-group)]
   ["Store"
    ("s" "save store" config-save)
    ("R" "refresh stores" config-refresh-store-files)
    ("o" "open store file" config-open-store)
    ("D" "clear all overrides" config-reset-all)]])

(my/leader!
  "h c" '(:def config-dispatch :which-key "config"))

(provide 'config-tools)
;;; config-tools.el ends here

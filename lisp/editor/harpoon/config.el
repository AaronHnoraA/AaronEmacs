;;; editor/harpoon/config.el --- Quick project file marks -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)
(require 'aaron-open)
(require 'remote-fs)

(declare-function +project-current-root "tools/project/config")

(defgroup +harpoon nil
  "Quick file marks."
  :group 'tools)

(config-defvar +harpoon-slot-count nil
  "Number of per-project quick-jump slots."
  :type 'integer
  :group '+harpoon)

(defconst +harpoon-state-file
  (expand-file-name "harpoon.el" (expand-file-name "var" user-emacs-directory)))

(defvar +harpoon-state nil
  "Alist mapping project roots to quick-mark slots.")

(defun +harpoon--identity (path)
  "Return PATH's logical identity, including its machine."
  (remote-canonicalize-emacs-file-name path))

(defun +harpoon--root-key (root)
  "Return the target-qualified key for ROOT."
  (file-name-as-directory (+harpoon--identity root)))

(defun +harpoon-load-state ()
  "Load persisted harpoon state."
  (when (file-readable-p +harpoon-state-file)
    (with-temp-buffer
      (insert-file-contents +harpoon-state-file)
      (setq +harpoon-state
            (mapcar (lambda (entry)
                      (cons (+harpoon--root-key (car entry))
                            (mapcar (lambda (file)
                                      (and file (+harpoon--identity file)))
                                    (cdr entry))))
                    (read (current-buffer)))))))

(defun +harpoon-save-state ()
  "Persist harpoon state to disk."
  (make-directory (file-name-directory +harpoon-state-file) t)
  (with-temp-file +harpoon-state-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 +harpoon-state (current-buffer)))))

(defun +harpoon-project-root ()
  "Return the current project root or `default-directory'."
  (or (and (fboundp '+project-current-root)
           (+project-current-root))
      default-directory))

(defun +harpoon--slots (&optional root)
  "Return slots list for ROOT."
  (let ((root (+harpoon--root-key (or root (+harpoon-project-root)))))
    (or (alist-get root +harpoon-state nil nil #'equal)
        (make-list +harpoon-slot-count nil))))

(defun +harpoon--set-slots (slots &optional root)
  "Store SLOTS for ROOT."
  (let ((root (+harpoon--root-key (or root (+harpoon-project-root)))))
    (setf (alist-get root +harpoon-state nil nil #'equal)
          (mapcar (lambda (file) (and file (+harpoon--identity file)))
                  slots))
    (+harpoon-save-state)))

(defun +harpoon/mark-current-file (&optional index)
  "Mark current file in the first empty slot or at INDEX."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((slots (copy-sequence (+harpoon--slots)))
         (index (or index
                    (cl-position nil slots :test #'equal)
                    0)))
    (setf (nth index slots) (+harpoon--identity buffer-file-name))
    (+harpoon--set-slots slots)
    (message "Marked %s in slot %d" (buffer-name) (1+ index))))

(defun +harpoon/unmark-current-file ()
  "Remove the current file from harpoon slots."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((file (+harpoon--identity buffer-file-name))
         (slots (mapcar (lambda (slot) (unless (equal slot file) slot))
                        (+harpoon--slots))))
    (+harpoon--set-slots slots)
    (message "Removed %s from harpoon slots" (file-name-nondirectory file))))

(defun +harpoon/jump (index)
  "Jump to harpoon slot INDEX."
  (interactive "nHarpoon slot: ")
  (let* ((slots (+harpoon--slots))
         (file (nth (1- index) slots)))
    (unless file
      (user-error "Harpoon slot %d is empty" index))
    (aaron-open-visit-file (or (remote-client-file-name file) file))))

(defun +harpoon/quick-menu ()
  "Select a marked file from the current project's slots."
  (interactive)
  (let* ((slots (+harpoon--slots))
         (candidates
          (cl-loop for file in slots
                   for idx from 1
                   when file
                   collect (cons (format "%d: %s" idx (abbreviate-file-name file)) file))))
    (unless candidates
      (user-error "No harpoon marks for this project"))
    (let ((file (cdr (assoc (completing-read "Harpoon: "
                                            (mapcar #'car candidates) nil t)
                            candidates))))
      (aaron-open-visit-file (or (remote-client-file-name file) file)))))

(+harpoon-load-state)

(aaron-leader!
  "j"   '(:ignore t :which-key "jump")
  "j m" '(:def +harpoon/mark-current-file :which-key "mark file")
  "j d" '(:def +harpoon/unmark-current-file :which-key "unmark file")
  "j j" '(:def +harpoon/quick-menu :which-key "harpoon menu")
  "j 1" '(:def (lambda () (interactive) (+harpoon/jump 1)) :which-key "slot 1")
  "j 2" '(:def (lambda () (interactive) (+harpoon/jump 2)) :which-key "slot 2")
  "j 3" '(:def (lambda () (interactive) (+harpoon/jump 3)) :which-key "slot 3")
  "j 4" '(:def (lambda () (interactive) (+harpoon/jump 4)) :which-key "slot 4"))

(provide 'editor/harpoon/config)
;;; editor/harpoon/config.el ends here

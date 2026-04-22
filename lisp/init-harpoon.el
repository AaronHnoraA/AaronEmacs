;;; init-harpoon.el --- Quick project file marks -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)

(declare-function my/project-current-root "init-project")

(defgroup my/harpoon nil
  "Quick file marks."
  :group 'tools)

(defcustom my/harpoon-slot-count 4
  "Number of per-project quick-jump slots."
  :type 'integer
  :group 'my/harpoon)

(defconst my/harpoon-state-file
  (expand-file-name "harpoon.el" (expand-file-name "var" user-emacs-directory)))

(defvar my/harpoon-state nil
  "Alist mapping project roots to quick-mark slots.")

(defun my/harpoon-load-state ()
  "Load persisted harpoon state."
  (when (file-readable-p my/harpoon-state-file)
    (with-temp-buffer
      (insert-file-contents my/harpoon-state-file)
      (setq my/harpoon-state (read (current-buffer))))))

(defun my/harpoon-save-state ()
  "Persist harpoon state to disk."
  (make-directory (file-name-directory my/harpoon-state-file) t)
  (with-temp-file my/harpoon-state-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 my/harpoon-state (current-buffer)))))

(defun my/harpoon-project-root ()
  "Return the current project root or `default-directory'."
  (or (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      default-directory))

(defun my/harpoon--slots (&optional root)
  "Return slots list for ROOT."
  (let ((root (file-name-as-directory (expand-file-name (or root (my/harpoon-project-root))))))
    (or (alist-get root my/harpoon-state nil nil #'equal)
        (make-list my/harpoon-slot-count nil))))

(defun my/harpoon--set-slots (slots &optional root)
  "Store SLOTS for ROOT."
  (let ((root (file-name-as-directory (expand-file-name (or root (my/harpoon-project-root))))))
    (setf (alist-get root my/harpoon-state nil nil #'equal) slots)
    (my/harpoon-save-state)))

(defun my/harpoon-mark-current-file (&optional index)
  "Mark current file in the first empty slot or at INDEX."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((slots (copy-sequence (my/harpoon--slots)))
         (index (or index
                    (cl-position nil slots :test #'equal)
                    0)))
    (setf (nth index slots) (expand-file-name buffer-file-name))
    (my/harpoon--set-slots slots)
    (message "Marked %s in slot %d" (buffer-name) (1+ index))))

(defun my/harpoon-unmark-current-file ()
  "Remove the current file from harpoon slots."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((file (expand-file-name buffer-file-name))
         (slots (mapcar (lambda (slot) (unless (equal slot file) slot))
                        (my/harpoon--slots))))
    (my/harpoon--set-slots slots)
    (message "Removed %s from harpoon slots" (file-name-nondirectory file))))

(defun my/harpoon-jump (index)
  "Jump to harpoon slot INDEX."
  (interactive "nHarpoon slot: ")
  (let* ((slots (my/harpoon--slots))
         (file (nth (1- index) slots)))
    (unless file
      (user-error "Harpoon slot %d is empty" index))
    (find-file file)))

(defun my/harpoon-quick-menu ()
  "Select a marked file from the current project's slots."
  (interactive)
  (let* ((slots (my/harpoon--slots))
         (candidates
          (cl-loop for file in slots
                   for idx from 1
                   when file
                   collect (cons (format "%d: %s" idx (abbreviate-file-name file)) file))))
    (unless candidates
      (user-error "No harpoon marks for this project"))
    (find-file (cdr (assoc (completing-read "Harpoon: "
                                            (mapcar #'car candidates) nil t)
                           candidates)))))

(my/harpoon-load-state)

(my/leader!
  "j"   '(:ignore t :which-key "jump")
  "j m" '(:def my/harpoon-mark-current-file :which-key "mark file")
  "j d" '(:def my/harpoon-unmark-current-file :which-key "unmark file")
  "j j" '(:def my/harpoon-quick-menu :which-key "harpoon menu")
  "j 1" '(:def (lambda () (interactive) (my/harpoon-jump 1)) :which-key "slot 1")
  "j 2" '(:def (lambda () (interactive) (my/harpoon-jump 2)) :which-key "slot 2")
  "j 3" '(:def (lambda () (interactive) (my/harpoon-jump 3)) :which-key "slot 3")
  "j 4" '(:def (lambda () (interactive) (my/harpoon-jump 4)) :which-key "slot 4"))

(provide 'init-harpoon)
;;; init-harpoon.el ends here

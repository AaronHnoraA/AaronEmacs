;;; remote-board.el --- Target, route, and health observability -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The board displays logical targets.  Link implementations are shown as
;; route state, never as duplicate target entries.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-config)
(require 'remote-doctor)

(defvar remote-target-history nil)

(defun remote-target-list ()
  "Return registered targets sorted by label."
  (let (targets)
    (maphash (lambda (_id target) (push target targets)) remote-targets)
    (sort targets
          (lambda (left right)
            (string-lessp
             (remote-target-label left)
             (remote-target-label right))))))

(defun remote-read-target (&optional prompt omit-local)
  "Read and return a target.
PROMPT customizes the minibuffer prompt.  OMIT-LOCAL excludes `local'."
  (let (table)
    (dolist (target (remote-target-list))
      (unless (and omit-local
                   (equal (remote-target-id target) "local"))
        (push (cons
               (format "%-24s %s"
                       (remote-target-label target)
                       (remote-target-id target))
               target)
              table)))
    (cdr
     (assoc
      (completing-read
       (or prompt "Target: ") table nil t nil 'remote-target-history)
      table))))

(defun remote--route-label (target capability adapter)
  "Return compact route label for TARGET, CAPABILITY, and ADAPTER."
  (let* ((file-name
          (remote-make-file-name (remote-target-id target) "/"))
         (context (remote-context file-name))
         (route (car (remote-routes adapter capability context))))
    (if route
        (let* ((link (remote-route-link route))
               (health
                (or (remote-route-backend-health route)
                    (remote-link-health link capability))))
          (format "%s:%s%s"
                  (remote-route-link-plugin-id route)
                  (remote-link-short-id link)
                  (if (eq (plist-get health :status) 'failed)
                      " !"
                    "")))
      "unavailable")))

(defun remote-board--entries ()
  "Return `tabulated-list-entries' for the current registry."
  (mapcar
   (lambda (target)
     (let ((id (remote-target-id target)))
       (list
        id
        (vector
         (remote-target-label target)
         id
         (number-to-string (length (remote-links-for-target id)))
         (remote--route-label target 'file-read "emacs-file")
         (remote--route-label target 'process-async "process")
         (if (remote-target-trusted target) "trusted" "untrusted")))))
   (remote-target-list)))

(defun remote-board-refresh ()
  "Reload configuration and refresh the board."
  (interactive)
  (remote-config-reload)
  (when (derived-mode-p 'remote-board-mode)
    (setq tabulated-list-entries (remote-board--entries))
    (tabulated-list-print t)))

(defun remote-board-target-at-point ()
  "Return target represented by the current row."
  (or (remote-get-target (tabulated-list-get-id))
      (user-error "No target on this row")))

(defun remote-open-target (&optional target)
  "Open TARGET's default logical directory."
  (interactive
   (list (unless (derived-mode-p 'remote-board-mode)
           (remote-read-target "Open target: "))))
  (find-file (remote-target-file-name
              (or target (remote-board-target-at-point)))))

(defun remote-copy-target-uri (&optional target)
  "Copy TARGET's default fs URI."
  (interactive
   (list (unless (derived-mode-p 'remote-board-mode)
           (remote-read-target "Copy target URI: "))))
  (let ((uri
         (remote-file-name-to-uri
          (remote-target-file-name
           (or target (remote-board-target-at-point))))))
    (kill-new uri)
    (message "Copied %s" uri)))

(defun remote-edit-config ()
  "Visit `remote-config-file'."
  (interactive)
  (find-file remote-config-file))

(defun remote-route-log-buffer ()
  "Display recent route decisions and failures."
  (interactive)
  (let ((buffer (get-buffer-create "*Remote Routes*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (dolist (entry remote-route-log)
          (insert
           (format-time-string
            "%Y-%m-%d %H:%M:%S "
            (plist-get entry :time)))
          (insert (format "%S\n" (cdr (cdr entry)))))))
    (pop-to-buffer buffer)))

(defun remote-board-doctor (&optional target)
  "Run `remote-doctor' for TARGET or the target at point."
  (interactive)
  (remote-doctor
   (or target
       (and (derived-mode-p 'remote-board-mode)
            (remote-board-target-at-point)))
   current-prefix-arg))

(defvar-keymap remote-board-mode-map
  :parent tabulated-list-mode-map
  "RET" #'remote-open-target
  "o" #'remote-open-target
  "w" #'remote-copy-target-uri
  "e" #'remote-edit-config
  "l" #'remote-route-log-buffer
  "d" #'remote-board-doctor
  "g" #'remote-board-refresh)

(define-derived-mode remote-board-mode tabulated-list-mode "Remote"
  "Mode for logical targets and resolved route health."
  (setq tabulated-list-format
        [("Target" 26 t)
         ("ID" 22 t)
         ("Links" 7 nil)
         ("Files" 28 nil)
         ("Processes" 28 nil)
         ("Trust" 10 nil)])
  (setq tabulated-list-padding 2
        tabulated-list-sort-key (cons "Target" nil)
        tabulated-list-entries (remote-board--entries))
  (tabulated-list-init-header))

(defun remote-board ()
  "Open the logical target and route board."
  (interactive)
  (let ((buffer (get-buffer-create "*Remote*")))
    (with-current-buffer buffer
      (remote-board-mode)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(provide 'remote-board)
;;; remote-board.el ends here

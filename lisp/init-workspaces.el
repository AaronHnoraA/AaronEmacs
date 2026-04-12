;;; init-workspaces.el --- Doom-style workspace helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'init-funcs)
(require 'transient)

(defvar persp-mode)
(defvar persp-selected-face)

(declare-function persp-current-name "perspective")
(declare-function persp-kill "perspective" (name))
(declare-function persp-names "perspective")
(declare-function persp-rename "perspective" (name))
(declare-function persp-switch "perspective" (name &optional norecord))
(declare-function persp-switch-last "perspective")

(defface my/workspace-current-face
  '((t (:inherit mode-line-emphasis :weight bold)))
  "Face used for the current workspace in echo-area displays."
  :group 'convenience)

(defface my/workspace-inactive-face
  '((t (:inherit shadow)))
  "Face used for inactive workspaces in echo-area displays."
  :group 'convenience)

(defun my/workspace--ensure-enabled ()
  "Raise a user error when `persp-mode' is unavailable."
  (unless (bound-and-true-p persp-mode)
    (user-error "Perspective workspaces are not enabled")))

(defun my/workspace-list-names ()
  "Return the current list of workspace names."
  (my/workspace--ensure-enabled)
  (or (persp-names) nil))

(defun my/workspace-current-name ()
  "Return the active workspace name."
  (my/workspace--ensure-enabled)
  (persp-current-name))

(defun my/workspace--generate-name ()
  "Generate a new anonymous workspace name."
  (let ((index 1)
        (names (my/workspace-list-names))
        candidate)
    (while
        (progn
          (setq candidate (format "#%d" index))
          (setq index (1+ index))
          (member candidate names)))
    candidate))

(defun my/workspace--segment (index name current)
  "Build an echo-area segment for workspace INDEX and NAME.
CURRENT is the active workspace name."
  (let ((face (if (equal name current)
                  'my/workspace-current-face
                'my/workspace-inactive-face)))
    (propertize
     (format "%d:%s" (1+ index) (if (equal name current)
                                    (format "[%s]" name)
                                  name))
     'face face)))

(defun my/workspace-display ()
  "Display the current workspaces in the echo area."
  (interactive)
  (let* ((names (my/workspace-list-names))
         (current (my/workspace-current-name))
         (segments (seq-map-indexed
                    (lambda (name index)
                      (my/workspace--segment index name current))
                    names)))
    (message "Workspaces: %s" (string-join segments "  "))))

(defun my/workspace-switch-to (name)
  "Switch to workspace NAME."
  (interactive
   (list (completing-read "Switch to workspace: "
                          (my/workspace-list-names)
                          nil t nil nil
                          (my/workspace-current-name))))
  (persp-switch name)
  (my/workspace-display))

(defun my/workspace-new (&optional name)
  "Create a new workspace named NAME.
When NAME is nil, generate an anonymous workspace name."
  (interactive)
  (my/workspace--ensure-enabled)
  (let ((name (or name (my/workspace--generate-name))))
    (when (member name (my/workspace-list-names))
      (user-error "Workspace %s already exists" name))
    (persp-switch name)
    (my/workspace-display)))

(defun my/workspace-new-named (name)
  "Create a new named workspace NAME."
  (interactive "sWorkspace name: ")
  (my/workspace-new name))

(defun my/workspace-rename (name)
  "Rename the current workspace to NAME."
  (interactive "sRename workspace to: ")
  (my/workspace--ensure-enabled)
  (persp-rename name)
  (my/workspace-display))

(defun my/workspace-kill (&optional name)
  "Kill workspace NAME.
When NAME is nil, kill the current workspace."
  (interactive)
  (my/workspace--ensure-enabled)
  (when (<= (length (my/workspace-list-names)) 1)
    (user-error "Can't delete the last workspace"))
  (persp-kill (or name (my/workspace-current-name)))
  (my/workspace-display))

(defun my/workspace-other ()
  "Switch to the previously active workspace."
  (interactive)
  (my/workspace--ensure-enabled)
  (persp-switch-last)
  (my/workspace-display))

(defun my/workspace-cycle (step)
  "Move STEP workspaces right or left."
  (my/workspace--ensure-enabled)
  (let* ((names (my/workspace-list-names))
         (count (length names))
         (current (my/workspace-current-name))
         (index (cl-position current names :test #'equal)))
    (when (<= count 1)
      (user-error "No other workspaces"))
    (my/workspace-switch-to
     (nth (mod (+ index step) count) names))))

(defun my/workspace-switch-left (&optional count)
  "Switch COUNT workspaces to the left."
  (interactive "p")
  (my/workspace-cycle (- (or count 1))))

(defun my/workspace-switch-right (&optional count)
  "Switch COUNT workspaces to the right."
  (interactive "p")
  (my/workspace-cycle (or count 1)))

(defun my/workspace-switch-to-final ()
  "Switch to the final workspace in the current list."
  (interactive)
  (my/workspace-switch-to (car (last (my/workspace-list-names)))))

(dotimes (i 9)
  (defalias (intern (format "my/workspace-switch-to-%d" i))
    (lambda ()
      (interactive)
      (my/workspace-switch-to
       (or (nth i (my/workspace-list-names))
           (user-error "No workspace at slot %d" (1+ i)))))))

(transient-define-prefix my/workspace-dispatch ()
  "Workspace operations."
  [["Workspace"
    ("TAB" "display" my/workspace-display :transient t)
    ("." "switch" my/workspace-switch-to :transient transient--do-exit)
    ("`" "other" my/workspace-other :transient transient--do-exit)
    ("n" "new" my/workspace-new :transient transient--do-exit)
    ("N" "new named" my/workspace-new-named :transient transient--do-exit)
    ("r" "rename" my/workspace-rename :transient transient--do-exit)
    ("d" "delete" my/workspace-kill :transient transient--do-exit)
    ("[" "left" my/workspace-switch-left :transient transient--do-exit)
    ("]" "right" my/workspace-switch-right :transient transient--do-exit)]])

(with-eval-after-load 'evil
  (define-prefix-command 'my/workspace-prefix-map)
  (evil-define-key* 'normal 'global (kbd "<leader>TAB") my/workspace-prefix-map)
  (evil-define-key* 'normal 'global (kbd "<leader><tab>") my/workspace-prefix-map)
  (define-key my/workspace-prefix-map (kbd "TAB") #'my/workspace-display)
  (define-key my/workspace-prefix-map (kbd "<tab>") #'my/workspace-display)
  (define-key my/workspace-prefix-map (kbd ".") #'my/workspace-switch-to)
  (define-key my/workspace-prefix-map (kbd "`") #'my/workspace-other)
  (define-key my/workspace-prefix-map (kbd "n") #'my/workspace-new)
  (define-key my/workspace-prefix-map (kbd "N") #'my/workspace-new-named)
  (define-key my/workspace-prefix-map (kbd "r") #'my/workspace-rename)
  (define-key my/workspace-prefix-map (kbd "d") #'my/workspace-kill)
  (define-key my/workspace-prefix-map (kbd "[") #'my/workspace-switch-left)
  (define-key my/workspace-prefix-map (kbd "]") #'my/workspace-switch-right)
  (define-key my/workspace-prefix-map (kbd "0") #'my/workspace-switch-to-final)
  (dotimes (i 9)
    (define-key my/workspace-prefix-map
                (kbd (number-to-string (1+ i)))
                (intern (format "my/workspace-switch-to-%d" i))))
  (evil-define-key* 'normal 'global (kbd "[w") #'my/workspace-switch-left)
  (evil-define-key* 'normal 'global (kbd "]w") #'my/workspace-switch-right))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "SPC TAB" "workspace"
    "SPC <tab>" "workspace"
    "SPC TAB TAB" "display"
    "SPC <tab> <tab>" "display"
    "SPC TAB ." "switch"
    "SPC TAB `" "other"
    "SPC TAB n" "new"
    "SPC TAB N" "new named"
    "SPC TAB r" "rename"
    "SPC TAB d" "delete"
    "SPC TAB [" "left"
    "SPC TAB ]" "right"
    "SPC TAB 0" "final"))

(dotimes (i 9)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      (format "SPC TAB %d" (1+ i))
      (format "slot %d" (1+ i)))))

(provide 'init-workspaces)
;;; init-workspaces.el ends here

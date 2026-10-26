;;; ui/workspaces/config.el --- Doom-style workspace helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'aaron-funcs)
(require 'transient)

(defvar persp-mode)
(defvar persp-selected-face)

(declare-function persp-current-name "perspective")
(declare-function persp-kill "perspective" (name))
(declare-function persp-names "perspective")
(declare-function persp-rename "perspective" (name))
(declare-function persp-valid-name-p "perspective" (name))
(declare-function persp-switch-to-buffer* "perspective" (&optional buffer-or-name))
(declare-function persp-switch "perspective" (name &optional norecord))
(declare-function persp-switch-last "perspective")
(declare-function switch-to-buffer "window" (buffer-or-name &optional norecord force-same-window))
(declare-function tab-bar--current-tab "tab-bar")
(declare-function tab-bar-new-tab "tab-bar" (&optional arg))
(declare-function tab-bar-close-tab-by-name "tab-bar" (name))
(declare-function tab-bar-rename-tab "tab-bar" (name))
(declare-function tab-bar-tabs "tab-bar" ())

(defface +workspace-current-face
  '((t (:inherit mode-line-emphasis :weight bold)))
  "Face used for the current workspace in echo-area displays."
  :group 'convenience)

(defface +workspace-inactive-face
  '((t (:inherit shadow)))
  "Face used for inactive workspaces in echo-area displays."
  :group 'convenience)

(defvar +tab-workspace-sync-in-progress nil
  "Prevent recursive synchronization between `tab-bar' and workspaces.")

(defun +workspace--ensure-enabled ()
  "Raise a user error when `persp-mode' is unavailable."
  (unless (bound-and-true-p persp-mode)
    (user-error "Perspective workspaces are not enabled")))

(defun +workspace-list-names ()
  "Return the current list of workspace names."
  (+workspace--ensure-enabled)
  (or (persp-names) nil))

(defun +workspace-current-name ()
  "Return the active workspace name."
  (+workspace--ensure-enabled)
  (persp-current-name))

(defun +tab-list-names ()
  "Return the current visible tab names."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (tab-bar-tabs)))

(defun +tab-current-name ()
  "Return the current tab name."
  (alist-get 'name (tab-bar--current-tab)))

(defun +tab-count ()
  "Return the current number of tabs."
  (length (tab-bar-tabs)))

(defun +tab-switch-to-current-workspace ()
  "Rename the current tab so it matches the active workspace."
  (when (and (bound-and-true-p tab-bar-mode)
             (bound-and-true-p persp-mode)
             (not +tab-workspace-sync-in-progress))
    (let ((name (+workspace-current-name)))
      (unless (equal (+tab-current-name) name)
        (let ((+tab-workspace-sync-in-progress t))
          (tab-bar-rename-tab name))))))

(defun +tab-switch-to-matching-workspace (&optional _previous tab)
  "Switch the active workspace to the name of TAB.
When TAB is nil, use the currently selected tab."
  (when (and (bound-and-true-p persp-mode)
             (not +tab-workspace-sync-in-progress))
    (let* ((tab (or tab (tab-bar--current-tab)))
           (name (alist-get 'name tab)))
      (when (and (stringp name)
                 (member name (+workspace-list-names))
                 (not (equal name (+workspace-current-name))))
        (let ((+tab-workspace-sync-in-progress t))
          (persp-switch name))))))

(defun +workspace--generate-name ()
  "Generate a name unused by workspaces and visible tabs."
  (let ((index 1)
        (names (append (+workspace-list-names)
                       (when (bound-and-true-p tab-bar-mode)
                         (+tab-list-names))))
        candidate)
    (while
        (progn
          (setq candidate (format "#%d" index))
          (setq index (1+ index))
          (member candidate names)))
    candidate))

(defun +workspace--segment (index name current)
  "Build an echo-area segment for workspace INDEX and NAME.
CURRENT is the active workspace name."
  (let ((face (if (equal name current)
                  '+workspace-current-face
                '+workspace-inactive-face)))
    (propertize
     (format "%d:%s" (1+ index) (if (equal name current)
                                    (format "[%s]" name)
                                  name))
     'face face)))

(defun +workspace/display ()
  "Display the current workspaces in the echo area."
  (interactive)
  (let* ((names (+workspace-list-names))
         (current (+workspace-current-name))
         (segments (seq-map-indexed
                    (lambda (name index)
                      (+workspace--segment index name current))
                    names)))
    (message "Workspaces: %s" (string-join segments "  "))))

(defun +workspace/switch-to (name)
  "Switch to workspace NAME."
  (interactive
   (list (completing-read "Switch to workspace: "
                          (+workspace-list-names)
                          nil t nil nil
                          (+workspace-current-name))))
  (persp-switch name)
  (+tab-switch-to-current-workspace)
  (+workspace/display))

(defun +workspace/switch-buffer ()
  "Switch to a buffer inside the current workspace."
  (interactive)
  (+workspace--ensure-enabled)
  (call-interactively
   (if (fboundp 'persp-switch-to-buffer*)
       #'persp-switch-to-buffer*
     #'switch-to-buffer)))

(defun +workspace/new (&optional name)
  "Create a new workspace named NAME.
When NAME is nil, generate an anonymous workspace name."
  (interactive)
  (+workspace--ensure-enabled)
  (let ((name (or name (+workspace--generate-name))))
    (unless (persp-valid-name-p name)
      (user-error "Choose a nonempty workspace name"))
    (when (member name (+workspace-list-names))
      (user-error "Workspace %s already exists" name))
    (when (and (bound-and-true-p tab-bar-mode)
               (member name (+tab-list-names))
               (not (equal name (+tab-current-name))))
      (user-error "Another tab already uses the name %s" name))
    (persp-switch name)
    (+tab-switch-to-current-workspace)
    (+workspace/display)))

(defun +workspace/new-named (name)
  "Create a new named workspace NAME."
  (interactive "sWorkspace name: ")
  (+workspace/new name))

(defun +workspace/rename (name)
  "Rename the current workspace to NAME."
  (interactive "sRename workspace to: ")
  (+workspace--ensure-enabled)
  (persp-rename name)
  (+tab-switch-to-current-workspace)
  (+workspace/display))

(defun +workspace/kill (&optional name)
  "Kill workspace NAME.
When NAME is nil, kill the current workspace.  Close tabs with that name."
  (interactive)
  (+workspace--ensure-enabled)
  (when (<= (length (+workspace-list-names)) 1)
    (user-error "Can't delete the last workspace"))
  (let ((name (or name (+workspace-current-name))))
    (unless (member name (+workspace-list-names))
      (user-error "No workspace named %s" name))
    ;; Perspective activation otherwise renames the tab before it can be
    ;; closed.  The sole tab has to be reused for the surviving workspace.
    (let ((+tab-workspace-sync-in-progress t))
      (when (bound-and-true-p tab-bar-mode)
        (while (and (> (+tab-count) 1)
                    (member name (+tab-list-names)))
          (let ((old-count (+tab-count)))
            (tab-bar-close-tab-by-name name)
            (when (= old-count (+tab-count))
              (user-error "Tab %s could not be closed; workspace kept" name)))))
      (persp-kill name)))
  (+tab-switch-to-current-workspace)
  (+workspace/display))

(defun +workspace/other ()
  "Switch to the previously active workspace."
  (interactive)
  (+workspace--ensure-enabled)
  (persp-switch-last)
  (+tab-switch-to-current-workspace)
  (+workspace/display))

(defun +workspaces/tab-new (&optional name)
  "Create a new top-level tab and matching workspace named NAME."
  (interactive)
  (+workspace--ensure-enabled)
  (let ((name (or name (+workspace--generate-name))))
    (unless (persp-valid-name-p name)
      (user-error "Choose a nonempty tab/workspace name"))
    (when (or (member name (+tab-list-names))
              (member name (+workspace-list-names)))
      (user-error "Tab or workspace %s already exists" name))
    (let ((+tab-workspace-sync-in-progress t))
      (tab-bar-new-tab)
      (persp-switch name)
      (tab-bar-rename-tab name))
    (+workspace/display)))

(defun +workspaces/tab-new-named (name)
  "Create a new top-level tab and matching workspace NAME."
  (interactive "sNew tab/workspace name: ")
  (+workspaces/tab-new name))

(defun +workspaces/tab-rename (name)
  "Rename the current tab and current workspace to NAME."
  (interactive "sRename tab/workspace to: ")
  (+workspace--ensure-enabled)
  (when (and (member name (+tab-list-names))
             (not (equal name (+tab-current-name))))
    (user-error "Tab %s already exists" name))
  (let ((+tab-workspace-sync-in-progress t))
    (persp-rename name)
    (tab-bar-rename-tab name))
  (+workspace/display))

(defun +workspace-cycle (step)
  "Move STEP workspaces right or left."
  (+workspace--ensure-enabled)
  (let* ((names (+workspace-list-names))
         (count (length names))
         (current (+workspace-current-name))
         (index (cl-position current names :test #'equal)))
    (when (<= count 1)
      (user-error "No other workspaces"))
    (+workspace/switch-to
     (nth (mod (+ index step) count) names))))

(defun +workspace/switch-left (&optional count)
  "Switch COUNT workspaces to the left."
  (interactive "p")
  (+workspace-cycle (- (or count 1))))

(defun +workspace/switch-right (&optional count)
  "Switch COUNT workspaces to the right."
  (interactive "p")
  (+workspace-cycle (or count 1)))

(defun +workspace/switch-to-final ()
  "Switch to the final workspace in the current list."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list-names)))))

(dotimes (i 9)
  (defalias (intern (format "+workspace/switch-to-%d" i))
    (lambda ()
      (interactive)
      (+workspace/switch-to
       (or (nth i (+workspace-list-names))
           (user-error "No workspace at slot %d" (1+ i)))))))

(transient-define-prefix workspace-menu ()
  "Workspace operations."
  [["Workspace"
    ("TAB" "display" +workspace/display :transient t)
    ("." "switch" +workspace/switch-to :transient transient--do-exit)
    ("b" "buffer" +workspace/switch-buffer :transient transient--do-exit)
    ("`" "other" +workspace/other :transient transient--do-exit)
    ("n" "new" +workspace/new :transient transient--do-exit)
    ("N" "new named" +workspace/new-named :transient transient--do-exit)
    ("r" "rename" +workspace/rename :transient transient--do-exit)
    ("d" "delete" +workspace/kill :transient transient--do-exit)
    ("[" "left" +workspace/switch-left :transient transient--do-exit)
    ("]" "right" +workspace/switch-right :transient transient--do-exit)]])

(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-tab-post-select-functions #'+tab-switch-to-matching-workspace)
  (add-hook 'persp-activated-functions #'+tab-switch-to-current-workspace)
  (when (bound-and-true-p persp-mode)
    (+tab-switch-to-current-workspace)))

(with-eval-after-load 'evil
  (define-prefix-command '+workspace-prefix-map)
  (evil-define-key* 'normal 'global (kbd "<leader>TAB") +workspace-prefix-map)
  (evil-define-key* 'normal 'global (kbd "<leader><tab>") +workspace-prefix-map)
  (define-key +workspace-prefix-map (kbd "TAB") #'+workspace/display)
  (define-key +workspace-prefix-map (kbd "<tab>") #'+workspace/display)
  (define-key +workspace-prefix-map (kbd ".") #'+workspace/switch-to)
  (define-key +workspace-prefix-map (kbd "b") #'+workspace/switch-buffer)
  (define-key +workspace-prefix-map (kbd "`") #'+workspace/other)
  (define-key +workspace-prefix-map (kbd "n") #'+workspace/new)
  (define-key +workspace-prefix-map (kbd "N") #'+workspace/new-named)
  (define-key +workspace-prefix-map (kbd "r") #'+workspace/rename)
  (define-key +workspace-prefix-map (kbd "d") #'+workspace/kill)
  (define-key +workspace-prefix-map (kbd "[") #'+workspace/switch-left)
  (define-key +workspace-prefix-map (kbd "]") #'+workspace/switch-right)
  (define-key +workspace-prefix-map (kbd "0") #'+workspace/switch-to-final)
  (dotimes (i 9)
    (define-key +workspace-prefix-map
                (kbd (number-to-string (1+ i)))
                (intern (format "+workspace/switch-to-%d" i))))
  (evil-define-key* 'normal 'global (kbd "[w") #'+workspace/switch-left)
  (evil-define-key* 'normal 'global (kbd "]w") #'+workspace/switch-right))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "SPC TAB" "workspace"
    "SPC <tab>" "workspace"
    "SPC TAB TAB" "display"
    "SPC <tab> <tab>" "display"
    "SPC TAB ." "switch"
    "SPC TAB b" "buffer"
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

(provide 'ui/workspaces/config)
;;; ui/workspaces/config.el ends here

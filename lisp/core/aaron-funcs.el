;;; core/aaron-funcs.el --- core functions and macros -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(require 'config)

(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(require 'general)

(general-create-definer aaron-leader!
  :states 'normal
  :keymaps 'global
  :prefix "<leader>")

(general-create-definer aaron-local-leader!
  :states 'normal
  :prefix "<localleader>")

(declare-function yas-active-snippets "yasnippet" (&optional beg end))
(declare-function yas-current-field "yasnippet" ())
(declare-function yas-next-field "yasnippet" (&optional arg))
(declare-function yas-prev-field "yasnippet" (&optional arg))

(defgroup aaron-editor-experience nil
  "Shared helpers for editor ergonomics."
  :group 'convenience)

(config-defvar aaron-large-buffer-size nil
  "Default threshold in bytes for treating a buffer as large."
  :type 'integer)

(defun aaron-terminal-normalize-directory (directory)
  "Return DIRECTORY as an absolute directory name on its own machine."
  (when directory
    (require 'remote-fs)
    (file-name-as-directory
     (remote-resolve-emacs-file-name directory))))

(defun aaron-terminal-home-directory (&optional directory)
  "Return the home directory for DIRECTORY's local or remote context."
  (when-let* ((directory (aaron-terminal-normalize-directory
                          (or directory default-directory))))
    (if-let* ((remote-prefix (file-remote-p directory))
              (_ (not (and (remote-fs-file-name-p directory)
                           (equal (remote-file-name-target directory) "local")))))
        (concat remote-prefix "~/")
      (let ((default-directory temporary-file-directory))
        (file-name-as-directory (expand-file-name "~"))))))

(defun aaron-terminal-shell-directory (directory)
  "Return DIRECTORY in the form expected by an interactive shell."
  (when-let* ((directory (aaron-terminal-normalize-directory directory)))
    (if (file-remote-p directory)
        (file-name-as-directory (file-local-name directory))
      directory)))

(defun aaron-terminal-cd-command (directory)
  "Return a shell command that changes to DIRECTORY."
  (when-let* ((shell-directory (aaron-terminal-shell-directory directory)))
    (format "cd %s"
            (shell-quote-argument
             (directory-file-name shell-directory)))))

(defun aaron-buffer-large-p (&optional buffer threshold)
  "Return non-nil if BUFFER exceeds THRESHOLD bytes.
BUFFER defaults to the current buffer.  THRESHOLD defaults to
`aaron-large-buffer-size'."
  (with-current-buffer (or buffer (current-buffer))
    (> (buffer-size) (or threshold aaron-large-buffer-size))))

(defun aaron-remote-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is remote.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (file-remote-p (or buffer-file-name default-directory))))

(defun aaron-rich-ui-buffer-p (&optional buffer threshold)
  "Return non-nil when BUFFER is suitable for expensive UI niceties.
Power-first preference: only require a graphical frame.
BUFFER and THRESHOLD are accepted for compatibility."
  (ignore threshold)
  (with-current-buffer (or buffer (current-buffer))
    (display-graphic-p)))

(defmacro shut-up! (func)
  "Silence FUNC."
  `(advice-add ,func :around
               (defun ,(intern (format "shut-up-%s" func)) (f &rest args)
                 (let ((inhibit-message t))
                   (ignore-errors (apply f args))))))

(defun +rename-current-file (newname)
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name newname)
  (rename-buffer newname))

(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-current-buffer)
    (delete-file file)))

(defun +copy-current-file (new-path &optional overwrite-p)
  "Copy current buffer's file to `NEW-PATH'.
If `OVERWRITE-P', overwrite the destination file without
confirmation."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (list (read-file-name "Copy file to: ")
           current-prefix-arg)))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) t)
    (copy-file old-path new-path (or overwrite-p 1))))

(defun +copy-current-filename (file)
  "Copy the full path to the current FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (kill-new file)
  (message "Copying '%s' to clipboard" file))

(defun +copy-current-buffer-name ()
  "Copy the name of current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "Copying '%s' to clipboard" (buffer-name)))

(defun +transient-tab-bar-history ()
  "Transient map of command `tab-bar-history'."
  (interactive)
  (let ((echo-keystrokes nil))
    (tab-bar-history-back)
    (message "tab-bar-history: [u]back [r]forward")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "u" #'tab-bar-history-back)
       (define-key map "r" #'tab-bar-history-forward)
       map)
     t)))

(defun +switch-to-compilation ()
  "Switch to the \"*compilation*\" buffer."
  (interactive)
  (when-let* ((buf (get-buffer "*compilation*")))
    (pop-to-buffer buf)))

(defun aaron-delimiter--org-script-range-at-point ()
  "Return (OPEN . CLOSE) for the ^{...} or _{...} surrounding point, or nil.
OPEN is the buffer position of `{'; CLOSE is the position just past `}'.
Works even when org-appear hides the braces, because overlays do not
alter buffer positions."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((origin (point))
            found)
        (while (and (not found)
                    (search-backward "{" nil t))
          (when (memq (char-before) '(?^ ?_))
            (let* ((open (point))
                   (close (save-excursion
                            (goto-char (1+ open))
                            (search-forward "}" nil t))))
              (when (and close (> origin open) (<= origin close))
                (setq found (cons open close))))))
        found))))

(defun aaron-delimiter--char-syntax-at (pos)
  "Return syntax class for the character at POS, or nil."
  (when-let* ((char (char-after pos)))
    (char-syntax char)))

(defun aaron-delimiter--opening-pos-at (pos)
  "Return POS when it is on an opening delimiter, otherwise nil."
  (when (eq (aaron-delimiter--char-syntax-at pos) ?\()
    pos))

(defun aaron-delimiter--closing-pos-at (pos)
  "Return POS when it is on a closing delimiter, otherwise nil."
  (when (eq (aaron-delimiter--char-syntax-at pos) ?\))
    pos))

(defun aaron-delimiter--containing-open-pos (&optional pos)
  "Return opening delimiter position surrounding POS or point."
  (let* ((pos (or pos (point)))
         (state (syntax-ppss pos)))
    (or (nth 1 state)
        (when (nth 3 state)
          (nth 8 state)))))

(defun aaron-delimiter--forward-target ()
  "Return forward jump target for delimiter DWIM commands."
  (or
   (when-let* ((open (aaron-delimiter--opening-pos-at (point))))
     (ignore-errors (scan-sexps open 1)))
   (when-let* ((close (aaron-delimiter--closing-pos-at (point))))
     (1+ close))
   (when-let* ((open-before (and (> (point) (point-min))
                                 (aaron-delimiter--opening-pos-at (1- (point))))))
     (ignore-errors (scan-sexps open-before 1)))
   (when-let* ((container (aaron-delimiter--containing-open-pos)))
     (ignore-errors (scan-sexps container 1)))
   (save-excursion
     (let ((line-end (line-end-position)))
       (when (re-search-forward "[])}]" line-end t)
         (match-end 0))))))

(defun aaron-snippet-active-p ()
  "Return non-nil when a live yasnippet field is active at point."
  (and (bound-and-true-p yas-minor-mode)
       (fboundp 'yas-active-snippets)
       (fboundp 'yas-current-field)
       (yas-active-snippets)
       (yas-current-field)))

(defun aaron/snippet-next-field-dwim ()
  "Advance the active snippet field when possible."
  (interactive)
  (unless (aaron-snippet-active-p)
    (user-error "No active snippet field"))
  (yas-next-field))

(defun aaron/snippet-previous-field-dwim ()
  "Move to the previous active snippet field when possible."
  (interactive)
  (unless (aaron-snippet-active-p)
    (user-error "No active snippet field"))
  (yas-prev-field))

(defun aaron/forward-delimiter-dwim ()
  "Jump forward across the current delimiter context.
Prefer structural navigation for Org scripts, lists and strings, then
fall back to a same-line closer search."
  (interactive)
  (if-let* ((range (aaron-delimiter--org-script-range-at-point)))
      ;; cdr is already one past `}' (search-forward return value)
      (goto-char (cdr range))
    (if-let* ((target (aaron-delimiter--forward-target)))
        (goto-char target)
      (user-error "No forward closing delimiter found"))))

(defun aaron-delimiter--backward-open-target ()
  "Return backward jump target for delimiter DWIM commands."
  (let* ((point (point))
         (open-before (and (> point (point-min))
                           (aaron-delimiter--opening-pos-at (1- point))))
         (close-before (and (> point (point-min))
                            (aaron-delimiter--closing-pos-at (1- point)))))
    (or
     (when open-before
       (when-let* ((outer (aaron-delimiter--containing-open-pos open-before)))
         (unless (= outer open-before)
           (1+ outer))))
     (when-let* ((close (aaron-delimiter--closing-pos-at point)))
       (when-let* ((open (ignore-errors (scan-sexps (1+ close) -1))))
         (1+ open)))
     (when close-before
       (when-let* ((open (ignore-errors (scan-sexps point -1))))
         (1+ open)))
     (when-let* ((container (aaron-delimiter--containing-open-pos)))
       (1+ container))
     (save-excursion
       (when (re-search-backward "[][({]" nil t)
         (1+ (point)))))))

(defun aaron/backward-delimiter-dwim ()
  "Jump backward to the current delimiter's inner start.
When already at an inner start, try to escape to the parent delimiter."
  (interactive)
  (if-let* ((range (aaron-delimiter--org-script-range-at-point))
            (inner-start (1+ (car range))))
      (if (= (point) inner-start)
          ;; Already at the content start — escape outward past ^ or _
          (if-let* ((outer (aaron-delimiter--backward-open-target)))
              (goto-char outer)
            (goto-char (max (point-min) (1- (car range)))))
        (goto-char inner-start))
    (if-let* ((target (aaron-delimiter--backward-open-target)))
        (goto-char target)
      (user-error "No backward opening delimiter found"))))

(defun aaron/backward-delimiter-or-snippet-dwim ()
  "Prefer snippet field retreat, then language-specific or delimiter backward jump."
  (interactive)
  (if (aaron-snippet-active-p)
      (aaron/snippet-previous-field-dwim)
    (aaron/jump-backward-dwim)))

;;; ── Per-language jump registry ───────────────────────────────────────────────

(defvar aaron-jump-handler-alist nil
  "Alist of (MODES . PLIST) for per-language jump handlers.
MODES is a major-mode symbol or list of symbols.  PLIST keys: :forward FN,
:backward FN.  Registered via `aaron-register-jump-handler'.")

(defun aaron-register-jump-handler (modes &rest props)
  "Register :forward / :backward jump functions for MODES.
MODES may be a single major-mode symbol or a list.  Replaces any prior
registration whose MODES list is `equal' to the new one."
  (let ((modes (if (listp modes) modes (list modes))))
    (setq aaron-jump-handler-alist
          (cons (cons modes props)
                (cl-remove-if
                 (lambda (entry) (equal (car entry) modes))
                 aaron-jump-handler-alist)))))

(defun aaron-jump--handler (key)
  "Return the :forward or :backward handler for the current buffer, or nil."
  (cl-loop for (modes . plist) in aaron-jump-handler-alist
           when (apply #'derived-mode-p modes)
           return (plist-get plist key)))

(defun aaron/jump-forward-dwim ()
  "Language-specific forward jump, else structural delimiter jump."
  (interactive)
  (if-let* ((fn (aaron-jump--handler :forward)))
      (funcall fn)
    (aaron/forward-delimiter-dwim)))

(defun aaron/jump-backward-dwim ()
  "Language-specific backward jump, else structural delimiter jump."
  (interactive)
  (if-let* ((fn (aaron-jump--handler :backward)))
      (funcall fn)
    (aaron/backward-delimiter-dwim)))

(defun aaron-evil-define-key (state keymap key command)
  "Bind KEY to COMMAND for Evil STATE in KEYMAP.
Use vendored `general.el' for symbolic keymaps and Evil's native
binding helper for live keymap objects."
  (with-eval-after-load 'evil
    (if (symbolp keymap)
        (general-define-key
         :states state
         :keymaps keymap
         key command)
      (evil-define-key* state keymap (kbd key) command))))

(defun aaron-evil-global-leader-set (key command &optional replacement)
  "Bind COMMAND to `SPC KEY' in Evil normal state.
When REPLACEMENT is non-nil, register it with Which-Key."
  (aaron-evil-define-key 'normal 'global (concat "<leader>" key) command)
  (when replacement
    (aaron-leader-key-label key replacement)))

(defun aaron-leader-key-label (key replacement)
  "Register REPLACEMENT for `SPC KEY' in Which-Key."
  (when replacement
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements (concat "SPC " key) replacement))))

(defvar aaron-escape-hook nil
  "Hook run by `aaron/escape'.
If any function returns non-nil, later hooks are skipped.")

(defvar aaron-global-quit-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-q") #'aaron/confirm-quit-emacs)
    map)
  "High-priority keymap for global quit bindings.")

(defun aaron/escape (&optional interactive)
  "Run `aaron-escape-hook', then fall back to `keyboard-quit'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond
     ((minibuffer-window-active-p (minibuffer-window))
      (when interactive
        (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))
     ((run-hook-with-args-until-success 'aaron-escape-hook))
     ((or defining-kbd-macro executing-kbd-macro) nil)
     (t
      (unwind-protect
          (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit)))))))

(defun aaron/confirm-quit-emacs ()
  "Quit Emacs after asking for confirmation."
  (interactive)
  (when (yes-or-no-p "Quit Emacs? ")
    (save-buffers-kill-terminal)))

(define-minor-mode aaron-global-quit-key-mode
  "Keep global quit bindings available above local keymaps."
  :init-value t
  :global t
  :lighter nil
  :keymap aaron-global-quit-key-mode-map)

;; A GUI Escape key is the distinct `<escape>' event, which is globally
;; unbound in Emacs 31.  Remapping `keyboard-quit' alone therefore leaves
;; Evil-disabled buffers able to leak Escape to macOS native fullscreen.
;; Keep this in the global map so minibuffer, isearch, terminal, and browser
;; maps can still provide their more specific local Escape behavior.
(general-define-key
 [remap keyboard-quit] #'aaron/escape
 "<escape>" #'aaron/escape
 "M-s M-s" #'save-buffer)

(aaron-global-quit-key-mode 1)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'aaron/escape))

(provide 'aaron-funcs)
;;; core/aaron-funcs.el ends here

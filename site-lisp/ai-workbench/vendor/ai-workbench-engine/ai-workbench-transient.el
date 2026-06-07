;;; ai-workbench-transient.el --- Transient menu for ai-workbench-engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'ai-workbench-engine)
(require 'transient)

(declare-function ediff-regions-internal "ediff")
(declare-function ediff-make-cloned-buffer "ediff-utils")
(declare-function org-escape-code-in-string "org-src")
(declare-function ai-workbench--vterm-delete "ai-workbench-integrations")


;; * Helper functions and vars

(defvar-local ai-workbench--rewrite-overlays nil
  "List of active rewrite overlays in the buffer.")

(defun ai-workbench--rewrite-sanitize-overlays ()
  "Ensure ai-workbench-engine's rewrite overlays in buffer are consistent."
  (setq ai-workbench--rewrite-overlays
        (cl-delete-if-not #'overlay-buffer
                          ai-workbench--rewrite-overlays)))

(defvar ai-workbench--set-buffer-locally nil
  "Set model parameters from `ai-workbench-menu' buffer-locally.

Affects the system message too.")

(defun ai-workbench--set-with-scope (sym value &optional scope)
  "Set SYM's symbol value to VALUE with SCOPE.

If SCOPE is t, set it buffer-locally.
If SCOPE is 1, reset it after the next ai-workbench-engine request.  (oneshot)
Otherwise, clear any buffer-local value and set its default
global value."
  (pcase scope
    (1 (unless (get sym 'ai-workbench-history)
         (put sym 'ai-workbench-history (symbol-value sym))
         (letrec ((restore-value
                   (lambda ()
                     (remove-hook 'ai-workbench-post-request-hook restore-value)
                     (run-at-time         ; Required to work around let bindings
                      0 nil (lambda (s)        ; otherwise this change is overwritten!
                              (set s (get s 'ai-workbench-history))
                              (put s 'ai-workbench-history nil))
                      sym))))
           (add-hook 'ai-workbench-post-request-hook restore-value)))
       (set sym value))
    ('t (set (make-local-variable sym) value))
    (_ (kill-local-variable sym)
       (set sym value))))

(defun ai-workbench--preset-mismatch-p (name)
  "Check if ai-workbench-engine preset with NAME is in effect.

This is intended to be fast but imperfect.  See
`ai-workbench--preset-mismatch-value' for more granular checking."
  (let ((elm (or (ai-workbench-get-preset name)
                 (ai-workbench-get-preset (intern-soft name))))
        key val)
    (catch 'mismatch
      (while elm
        (setq key (pop elm) val (pop elm))
        (cond
         ((memq key '(:description :parents)) 'nil)
         ((eq key :system)
          (or (equal ai-workbench-system-prompt val)
              (functionp val)    ; Ignore functions, modify-specs for speed here
              (and (consp val) (keywordp (car val)))
              (and-let* (((symbolp val))
                         (p (assq val ai-workbench-directives)))
                (equal ai-workbench-system-prompt (cdr p)))
              (throw 'mismatch t)))
         ((eq key :backend)
          (or (if (stringp val)
                  (equal (ai-workbench-backend-name ai-workbench-backend) val)
                (eq ai-workbench-backend val))
              (throw 'mismatch t)))
         ((eq key :tools)
          (setq val (cl-loop ; Check against tool names, not tools (faster with sorting)
                     for tool in (ensure-list (ai-workbench--modify-value ai-workbench-llm-tools val))
                     for tool-name = (or (and (stringp tool) tool)
                                         (ignore-errors (ai-workbench-tool-name tool)))
                     if (not (member tool-name uniq-tool-names))
                     collect tool-name into uniq-tool-names
                     finally return uniq-tool-names))
          (or (equal (sort val #'string-lessp) ;preset tools same as ai-workbench-llm-tools?
                     (sort (mapcar #'ai-workbench-tool-name ai-workbench-llm-tools)
                           #'string-lessp))
              (throw 'mismatch t)))
         (t (let* ((suffix (substring
                            (if (symbolp key) (symbol-name key) key) 1))
                   (sym (or (intern-soft (concat "ai-workbench-" suffix))
                            (intern-soft (concat "ai-workbench--" suffix)))))
              ;; FIXME(modify-list): Fix for values specified with a spec, like :eval
              (or (null sym)
                  (and (boundp sym) (equal (eval sym) val))
                  (throw 'mismatch t)))))))))

(defun ai-workbench--get-directive (args)
  "Find the additional directive in the transient ARGS.

Meant to be called when `ai-workbench-menu' is active."
  (cl-some (lambda (s) (and (stringp s) (string-prefix-p ":" s)
                       (substring s 1)))
                  args))

(defun ai-workbench--instructions-make-overlay (text &optional ov)
  "Make or move overlay OV with TEXT."
  (save-excursion
    ;; Move point to overlay position
    (cond
     ((use-region-p)
      (if (pos-visible-in-window-p (region-beginning))
          (goto-char (region-beginning))))
     ((ai-workbench--in-response-p)
      (ai-workbench-beginning-of-response)
      (skip-chars-forward "\n \t"))
     (t (text-property-search-backward 'ai-workbench-engine 'response)
        (skip-chars-forward "\n \t")))
    ;; Make overlay
    (if (and ov (overlayp ov))
        (move-overlay ov (point) (point) (current-buffer))
      (setq ov (make-overlay (point) (point) nil t)))
    (overlay-put ov 'before-string nil)
    ;; (unless (or (bobp) (eq (char-before) "\n"))
    ;;   (overlay-put ov 'before-string (propertize "\n" 'font-lock-face 'shadow)))
    (overlay-put ov 'category 'ai-workbench-engine)
    (overlay-put
     ov 'after-string
     (concat (propertize (concat "DIRECTIVE: " text)
                         'font-lock-face '(:inherit shadow :weight bold  :box t))
      "\n"))
    ov))

(defconst ai-workbench--read-with-prefix-help
  (concat
   (propertize "TAB" 'face 'help-key-binding)
   (propertize ": expand, " 'face 'default)
   (propertize "M-n" 'face 'help-key-binding)
   (propertize "/" 'face 'default)
   (propertize "M-p" 'face 'help-key-binding)
   (propertize ": next/previous" 'face 'default))
  "Help string (TODO).")

(defun ai-workbench--read-with-prefix (prefix)
  "Show string PREFIX in the minibuffer after the minibuffer prompt.

PREFIX is shown in an overlay.  Repeated calls to this function
will toggle its visibility state."
  (unless (minibufferp)
    (user-error "This command is intended to be used in the minibuffer"))
  (let* ((update
         (lambda (ov s)
           (overlay-put
            ov 'after-string
            (and s (concat (propertize (concat "\n" s "\n") 'face 'shadow)
                           (make-separator-line))))))
         (max-width (- (window-width) (minibuffer-prompt-end)))
         (max (or max-mini-window-height 0.4))
         (max-height (- (or (and (natnump max) max)
                            (floor (* max (frame-height))))
                        5)))
    (if (and prefix (not (string-empty-p prefix)) (> max-height 1))
        (progn
          (unless visual-line-mode (visual-line-mode 1))
          (goto-char (minibuffer-prompt-end))
          (pcase-let ((`(,prop . ,ov)
                       (get-char-property-and-overlay
                        (point-min) 'ai-workbench-engine)))
            (unless ov
              (setq ov (make-overlay
                        (point-min) (minibuffer-prompt-end) nil t)))
            (pcase prop
              ('partial
               (if (> (length prefix) max-width)
                   (progn
                     (overlay-put ov 'ai-workbench-engine 'prefix)
                     (let ((disp-size
                            (cl-loop for char across prefix
                                     for idx upfrom 0
                                     with n = 0 with max-length = (* max-height max-width)
                                     if (eq char ?\n) do (cl-incf n)
                                     if (> n max-height) return idx
                                     if (> idx max-length)
                                     return idx
                                     finally return nil)))
                       (funcall update ov
                                (if disp-size
                                    (truncate-string-to-width
                                     prefix disp-size  nil nil 'ellipsis)
                                  prefix))))
                 (overlay-put ov 'ai-workbench-engine 'hide)
                 (funcall update ov nil)))
              ('prefix (overlay-put ov 'ai-workbench-engine 'hide)
                       (funcall update ov nil))
              (_ (overlay-put ov 'ai-workbench-engine 'partial)
                 (funcall update ov (truncate-string-to-width
                                     prefix max-width nil nil
                                     'ellipsis))))))
      (when-let* ((prop-ov (get-char-property-and-overlay (point-min) 'ai-workbench-engine)))
        (when (overlayp (cdr prop-ov)) (delete-overlay (cdr prop-ov)))))))

(defvar ai-workbench--minibuffer-prompt-history nil
  "History of prompts read from the minibuffer by ai-workbench-engine.")

(defun ai-workbench--read-minibuffer-prompt (&optional read-prompt)
  "Read a user prompt from the minibuffer.

Prompt with READ-PROMPT if supplied.  Return a cons cell of the buffer
region (if included) and the provided instructions."
  (let* ((include-region (use-region-p))
         (cb (current-buffer))
         (get-region (lambda () (with-current-buffer cb
                             (and include-region
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))))))
         (cycle-prefix (lambda () (interactive)
                         (let ((p (point)))
                           (ai-workbench--read-with-prefix (funcall get-region))
                           (goto-char p))))
         (toggle-region (lambda () (interactive)
                          (if include-region
                              (progn (setq include-region nil)
                                     (ai-workbench--read-with-prefix nil))
                            (setq include-region t)
                            (funcall cycle-prefix))))
         (edit-in-buffer
          (lambda () (interactive)
            (ai-workbench--edit-directive nil
              :initial (minibuffer-contents)
              :prompt (if include-region
                          (with-current-buffer cb (buffer-substring-no-properties
                                                   (region-beginning) (region-end)))
                        "# Edit prompt below")
              :setup (lambda () (goto-char (point-max)) (run-at-time 0 nil #'recenter))
              :callback (lambda (msg)
                          (if (not msg)
                              (minibuffer-quit-recursive-edit)
                            (delete-region (minibuffer-prompt-end) (point-max))
                            (insert msg) (exit-minibuffer))))))
         (minibuffer-local-map
          (make-composed-keymap (define-keymap
                                  "M-RET" toggle-region "C-c C-e" edit-in-buffer)
                                minibuffer-local-map)))
    (let ((user-prompt
           (minibuffer-with-setup-hook
               (lambda () (add-hook 'completion-at-point-functions
                               #'ai-workbench-preset-capf nil t)
                 (funcall cycle-prefix)
                 ;; HACK for lucid Emacs, where `make-separator-line' is wonky.  The
                 ;; minibuffer prompt gets cut off -- force redisplay to fix:
                 (insert " ") (redisplay) (delete-char -1))
             (read-string
              (or read-prompt
                  (concat (format "Ask %s" (ai-workbench-backend-name ai-workbench-backend))
                          (if (use-region-p) ;NOTE: not "include-region" as this is only read once
                            (concat " (" (propertize "M-RET" 'face 'help-key-binding)
                                    (propertize ": Include/Ignore selection" 'face 'default)
                                    "): ")
                            ": ")))
              nil 'ai-workbench--minibuffer-prompt-history))))
      (cons (funcall get-region) user-prompt))))

(defun ai-workbench--transient-read-number (prompt _initial-input history)
  "Read a numeric value from the minibuffer.

PROMPT, _INITIAL-INPUT and HISTORY are as in the transient reader
documention.  Return nil if user does not provide a number, for default."
  ;; Workaround for buggy transient behaviour when dealing with
  ;; non-string values.  See: https://github.com/magit/transient/issues/172
  (when-let* ((history-symbol (or (car-safe history) history))
              (val (and (symbolp history-symbol) (symbol-value history-symbol))))
    (unless (stringp (car val))
      (setcar val (number-to-string (car val)))))
  (let* ((minibuffer-default-prompt-format "")
	 (num (read-number prompt -1 history)))
    (if (= num -1) nil num)))

(defun ai-workbench-system-prompt--format ()
  "Format the system prompt for display in ai-workbench-engine's transient menus.

Handle formatting for system messages when the active `ai-workbench-model' does
not support system messages."
  (if (ai-workbench--model-capable-p 'nosystem)
      (concat (propertize "[No system message support for model "
                          'face 'transient-heading)
              (propertize (ai-workbench--model-name ai-workbench-model)
                          'face 'warning)
              (propertize "]" 'face 'transient-heading))
    (if ai-workbench-system-prompt
        (ai-workbench--describe-directive
         ai-workbench-system-prompt (max (- (window-width) 12) 14) "⮐ ")
      "[No system message set]")))

(defun ai-workbench--tools-init-value (obj)
  "Set the initial state of a tool OBJ in variable `ai-workbench-llm-tools'.

OBJ is a tool-infix of type `ai-workbench--switch'."
  (when-let* ((name (car (member (oref obj argument)
                                 (mapcar #'cadr
                                         (plist-get (transient-scope) :tools))))))
    (oset obj value (list (oref obj category) name))))

(defvar ai-workbench--crowdsourced-prompts-url
  "https://raw.githubusercontent.com/f/prompts.chat/main/prompts.csv"
  "URL for crowdsourced LLM system prompts.")

(defvar ai-workbench--crowdsourced-prompts
  (make-hash-table :test #'equal)
  "Crowdsourced LLM system prompts.")

(defun ai-workbench--read-csv-column ()
  "Read the next CSV column in the current buffer.

Supports RFC 4180 quoted and unquoted fields, including embedded
newlines and escaped quotes in quoted fields."
  (cond
   ((eobp) nil)
   ((eq (char-after) ?,)
    (forward-char 1)
    "")
   ((eq (char-after) ?\")
    (forward-char 1)
    (let ((parts nil)
          (start (point))
          done)
      (while (not done)
        (if (search-forward "\"" nil t)
            (if (eq (char-after) ?\")
                (progn
                  (push (buffer-substring-no-properties start (1- (point))) parts)
                  (push "\"" parts)
                  (forward-char 1)
                  (setq start (point)))
              (push (buffer-substring-no-properties start (1- (point))) parts)
              (setq done t))
          (push (buffer-substring-no-properties start (point-max)) parts)
          (goto-char (point-max))
          (setq done t)))
      (when (eq (char-after) ?,)
        (forward-char 1))
      (apply #'concat (nreverse parts))))
   (t
    (let ((start (point)))
      (while (and (not (eobp))
                  (not (memq (char-after) '(?, ?\n ?\r))))
        (forward-char 1))
      (prog1 (buffer-substring-no-properties start (point))
        (when (eq (char-after) ?,)
          (forward-char 1)))))))

(defun ai-workbench--crowdsourced-prompts ()
  "Acquire and read crowdsourced LLM system prompts.

These are stored in the variable `ai-workbench--crowdsourced-prompts',
which see."
  (when (hash-table-p ai-workbench--crowdsourced-prompts)
    (when (hash-table-empty-p ai-workbench--crowdsourced-prompts)
      (unless ai-workbench-crowdsourced-prompts-file
        (run-at-time 0 nil #'ai-workbench-system-prompt)
        (user-error "No crowdsourced prompts available"))
      (unless (and (file-exists-p ai-workbench-crowdsourced-prompts-file)
                   (time-less-p
                    (time-subtract (current-time) (days-to-time 14))
                    (file-attribute-modification-time
                     (file-attributes ai-workbench-crowdsourced-prompts-file))))
        (when (y-or-n-p
               (concat
                "Fetch crowdsourced system prompts from "
                (propertize ai-workbench--crowdsourced-prompts-url 'face 'link)
                "?"))
          ;; Fetch file
          (message "Fetching prompts...")
          (let ((dir (file-name-directory ai-workbench-crowdsourced-prompts-file)))
            (unless (file-exists-p dir) (mkdir dir 'create-parents))
            (if (url-copy-file ai-workbench--crowdsourced-prompts-url
                               ai-workbench-crowdsourced-prompts-file
                               'ok-if-already-exists)
		(message "Fetching prompts... done.")
              (message "Could not retrieve new prompts.")))))
      (if (not (file-readable-p ai-workbench-crowdsourced-prompts-file))
          (progn (message "No crowdsourced prompts available")
                 (call-interactively #'ai-workbench-system-prompt))
        (with-temp-buffer
          (insert-file-contents ai-workbench-crowdsourced-prompts-file)
          (goto-char (point-min))
          (forward-line 1)
          (while (not (eobp))
	    (when-let* ((act (ai-workbench--read-csv-column))
			(prompt (ai-workbench--read-csv-column)))
		(puthash act prompt ai-workbench--crowdsourced-prompts))
	      (forward-line 1)))))
    ai-workbench--crowdsourced-prompts))

;; FIXME(targeted-context): This does not handle :bounds and :lines.
(defun ai-workbench--describe-infix-context ()
  "Return a count of the number of context chunks."
  (if (null ai-workbench-context) "Context"
    (pcase-let*
        ((buffer-count (length ai-workbench-context))
         (`(,file-count ,ov-count)
          (if (> buffer-count 0)
              (cl-loop for entry in ai-workbench-context
                       for (buf-file . spec) = (ensure-list entry)
                       if (bufferp buf-file)
                       sum (max (length (plist-get spec :overlays)) 1)
                       into ov-count
                       else count (stringp buf-file) into file-count
                       finally return (list file-count ov-count))
            (list 0 0))))
      (concat "Context ("
              (propertize
               (concat
                (and (> ov-count 0)
                     (format "%d region%s in %d buffer%s"
                             ov-count (if (> ov-count 1) "s" "")
                             (- buffer-count file-count)
                             (if (> ( - buffer-count file-count) 1) "s" "")))
                (and (> file-count 0)
                     (format "%s%d file%s"
                             (if (> ov-count 0) ", " "") file-count
                             (if (> file-count 1) "s" ""))))
               'face 'warning)
              ")"))))

(defun ai-workbench--describe-suffix-send ()
  "Describe the action of `ai-workbench--suffix-send'."
  (cl-flet ((ptv (s) (propertize s 'face 'warning))
            (pth (s) (propertize s 'face 'transient-heading)))
    (let* ((args (or (and transient-current-command
                          (transient-args transient-current-command))
                     ;; Not yet exported, simulate.  HACK: We are accessing
                     ;; Transient's internal variables here for live updates.
                     (let* ((transient-current-command (oref transient--prefix command))
                            (transient-current-suffixes transient--suffixes))
                       (transient-args transient-current-command))))
           (lbeg (line-number-at-pos (if (use-region-p) (region-beginning)
                                       (point-min))))
           (lend (line-number-at-pos (if (use-region-p) (region-end)
                                       (point))))
           (ltext (ptv (if (> lend lbeg)
                           (format " (lines %d-%d)" lbeg lend)
                         (format " (line %d)" lbeg))))
           (dest) (context))
      (setq dest (cond
                  ((member "e" args) (ptv "echo area"))
                  ((member "k" args) (ptv "kill-ring"))
                  ((cl-some (lambda (s)
                              (and (stringp s) (memq (aref s 0) '(?g ?b))
                                   (not (equal (substring s 1) (buffer-name)))
                                   (concat (pth "buffer ") (ptv (substring s 1)))))
                            args))))
      (setq context
            (and ai-workbench-context
                 (let ((lc (length ai-workbench-context)))
                   (concat (pth " along with ") (ptv (format "%d" lc))
                           (pth (concat " context source" (and (/= lc 1) "s")))))))
      (cond ((member "m" args)
             (concat (pth "Read prompt from ") (ptv "minibuffer")
                     context
                     (if dest (concat (pth ", response to ") dest)
                       (concat (pth ", insert response at point")))))
            ((member "y" args)
             (concat (pth "Send prompt from ")
                     (concat (ptv "kill-ring (")
                             (if-let* ((val (current-kill 0))
                                       (val (substring-no-properties val))
                                       (len (length val)))
                                 (ptv (concat
                                       "\"" (string-replace
                                             "\n" "⮐"
                                             (truncate-string-to-width
                                              val 20 nil nil t))
                                       "\"" (when (> len 20)
                                              (concat
                                               ", "
                                               (file-size-human-readable len 'si " ")
                                               " chars"))))
                               (propertize "empty" 'face 'error))
                             (ptv ")"))
                     context
                     (if dest (concat (pth ", response to ") dest)
                       (concat (pth ", insert response at point")))))
            ((member "i" args)
             (let* ((reg (use-region-p))
                    (src (ptv (if reg "selection" (buffer-name)))))
               (if dest (concat (pth "Send ") src ltext context (pth ", with response to ")
                                (ptv dest) (pth "; kill") ltext
                                (and (not reg) (concat (pth " in ") src)))
                 (concat (pth "Replace ") src ltext (pth " with response")
                         (and context
                              (concat (pth " ( with") (substring context 11) " )"))))))
            ((use-region-p)
             (concat (pth "Send ") (ptv "selection") ltext
                     context (if dest (concat (pth ", with response to ") dest)
                               (concat (pth ", insert response at region end")))))
            (t (concat (pth "Send ") (ptv (buffer-name)) ltext
                       context (if dest (concat (pth ", with response to ") dest)
                                 (concat (pth ", insert response at point")))))))))

(defun ai-workbench--format-preset-string ()
  "Format the preset indicator display for `ai-workbench-menu'."
  (if (and ai-workbench--known-presets ai-workbench--preset)
      (apply
       #'format " (%s%s)"
       (let ((mismatch (ai-workbench--preset-mismatch-p ai-workbench--preset)))
         (list (propertize "@" 'face (if mismatch 'transient-key
                                       '( :inherit transient-key
                                          :inherit secondary-selection
                                          :box -1 :weight bold)))
               (propertize (format "%s" ai-workbench--preset) 'face
                           (if mismatch
                               '(:inherit warning :strike-through t)
                             '(:inherit secondary-selection :box -1))))))
    (format " (%s%s)"
            (propertize "@" 'face 'transient-key)
            (propertize "preset" 'face 'transient-inactive-value))))

(defun ai-workbench--transient-fix-evil-visual (fn)
  "Let evil-mode set up the region correctly before displaying a transient.

This is supposed to be used in the `:environment' slot of
`transient-define-prefix'.

The transient display code may be called from an entry in `post-command-hook',
which may happen to run late, i.e., after evil-mode's entry in that hook has
already teared down the temporary expanding of the region to a possibly existing
visual selection.  This environment will ensure that the region is always
expanded before calling the transient display code in FN.

If evil-mode is not in use, this function is a no-op and calls FN directly."
  (if (and (boundp 'evil-visual-region-expanded)
           (not evil-visual-region-expanded)
           (fboundp 'evil-visual-expand-region)
           (fboundp 'evil-visual-contract-region))
      (progn
        (evil-visual-expand-region)
        (funcall fn)
        (when evil-visual-region-expanded
          (evil-visual-contract-region)))
    (funcall fn)))


;; * Transient classes and methods for ai-workbench-engine

;; ** Class for generic ai-workbench-engine elisp variables

(defclass ai-workbench-lisp-variable (transient-lisp-variable)
  ((display-nil :initarg :display-nil)  ;String to display if value if nil
   (display-map :initarg :display-map :initform nil)) ;Display string from alist display-map
  "Lisp variables that show :display-nil instead of nil.")

(cl-defmethod transient-format-value ((obj ai-workbench-lisp-variable))
  (let ((display-value
         (with-slots (value display-nil display-map) obj
           (cond ((null value) display-nil)
                 (display-map (or (cdr (assoc value display-map)) value))
                 (t value)))))
    (propertize
     (if (stringp display-value) display-value (prin1-to-string display-value))
     'face 'transient-value)))

(cl-defmethod transient-infix-set ((obj ai-workbench-lisp-variable) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)
           ai-workbench--set-buffer-locally))

;; ** Class for managing ai-workbench-engine tools

(defclass ai-workbench--switch (transient-switch)
  ((category :initarg :category))
  "Class used for arguments that share a category.")

(cl-defmethod transient-infix-set ((obj ai-workbench--switch) value)
  "Set VALUE of a `ai-workbench--switch' OBJ.

It is a list of the category and argument, e.g.
 (\"filesystem\" \"read_file\")."
  (let ((state (transient-scope))
        (category (oref obj category)))
    (if value
        (progn
          (cl-pushnew (list category value)
                      (plist-get state :tools) :test #'equal)
          (oset obj value (list category value)))
      (plist-put state :tools
                 (delete (list category (oref obj argument))
                         (plist-get state :tools)))
      (oset obj value nil))
    (oset transient--prefix scope state)))

;; ** Class for managing ai-workbench-engine tool categories

(defclass ai-workbench--switch-category (transient-switch)
  ((category :initarg :category))
  "Class used for arguments that switch a group of other arguments.

Their own value is ignored")

(cl-defmethod transient-format-value ((obj ai-workbench--switch-category))
  (let* ((category (oref obj category))
         (active-count
          (cl-count-if (lambda (tl) (equal (car tl) category))
                       (plist-get (transient-scope) :tools)))
         (total-count (length (cdr (assoc category ai-workbench--known-tools)))))
    (if (> active-count 0)
        (propertize (format "(%d/%d)" active-count total-count) 'face 'transient-value)
      (propertize (format "(0/%d)" total-count) 'face 'transient-inactive-value))))

;; Pressing a tool category key should have different behaviors in different
;; contexts:
;; - If the tools for the category are not shown, show them, do nothing else
;; - If the tools are showing and any of them are selected, deselect all
;; - If the tools are showing and none of them are selected, select all

;; To do this we independently track whether the category tools are visible
;; ("active"), and whether any category tools have been "selected":
(cl-defmethod transient-infix-read ((obj ai-workbench--switch-category))
  "Determine OBJ value according to category toggle settings."
  (let* ((category (oref obj category))
         (active (equal category (plist-get (transient-scope) :category)))
         (selected (cl-some (lambda (tool-spec) (equal category (car tool-spec)))
                            (plist-get (transient-scope) :tools))))
    (if (not active)
        (oref obj value)
      (if selected nil (oref obj argument)))))

(cl-defmethod transient-infix-set ((obj ai-workbench--switch-category) value)
  "When setting VALUE, set all options in the category of OBJ."
  (dolist (suffix-obj transient--suffixes)
    ;; Find all suffixes that have this category
    (when-let* (((cl-typep suffix-obj 'ai-workbench--switch))
                ((equal (oref suffix-obj category)
                        (oref obj category)))
                (arg (if (slot-boundp suffix-obj 'argument)
                         (oref suffix-obj argument)
                       (oref obj argument-format))))
      (if value                         ; Turn on/off all members in category
          (transient-infix-set suffix-obj arg)
        (transient-infix-set suffix-obj nil))))
  ;; Update the active menu category and key in the prefix scope
  (plist-put (transient-scope) :category (oref obj category))
  (plist-put (transient-scope) :key (oref obj key))
  ;; Finally set the "value" of the category itself
  (oset obj value value))

;; ** Class for ai-workbench-engine options that are three-way switches

(defclass ai-workbench--switches (ai-workbench-lisp-variable)
  ((display-if-true :initarg :display-if-true :initform "True")
   (display-if-false :initarg :display-if-false :initform "False"))
  "Boolean Lisp variable class for ai-workbench-transient.")

(cl-defmethod transient-infix-read ((obj ai-workbench--switches))
  "Cycle through the mutually exclusive switches for OBJ."
  (not (oref obj value)))

(cl-defmethod transient-format-value ((obj ai-workbench--switches))
  (with-slots (value display-if-true display-if-false) obj
      (format
       (propertize "(%s)" 'face 'transient-delimiter)
       (concat
        (propertize display-if-false
                    'face (if value 'transient-inactive-value 'transient-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize display-if-true
                    'face (if value 'transient-value 'transient-inactive-value))))))

;; ** Class for ai-workbench-engine's scope management, singleton

(defclass ai-workbench--scope (ai-workbench--switches)
  ((display-if-true :initarg :display-if-true :initform "buffer")
   (display-if-false :initarg :display-if-false :initform "global"))
  "Singleton Lisp variable class for `ai-workbench--set-buffer-locally'.

This is used only for setting this variable via `ai-workbench-menu'.")

(cl-defmethod transient-infix-read ((obj ai-workbench--scope))
  "Cycle through the mutually exclusive switches for OBJ."
  (with-slots (value) obj
    (pcase value
      ('t (message "Parameters will be set for the next request only"))
      ('nil (message "Parameters will be set buffer-locally"))
      (1 (message "Parameters will be set globally")))
    (pcase value ('t 1) ('nil t) (1 nil))))

(cl-defmethod transient-format-value ((obj ai-workbench--scope))
  (with-slots (value display-if-true display-if-false) obj
      (format
       (propertize "(%s)" 'face 'transient-delimiter)
       (concat
        (propertize display-if-false
                    'face (if (null value) 'transient-value 'transient-inactive-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize display-if-true
                    'face (if (eq value t) 'transient-value 'transient-inactive-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize "oneshot" 'face
                    (if (eql value 1) 'transient-value 'transient-inactive-value))))))

(cl-defmethod transient-infix-set ((obj ai-workbench--scope) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

;; ** Class for managing ai-workbench-engine's backend and model, singleton

(defclass ai-workbench-provider-variable (transient-lisp-variable)
  ((backend       :initarg :backend)
   (backend-value :initarg :backend-value)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "Class used for ai-workbench-backends.")

(cl-defmethod transient-format-value ((obj ai-workbench-provider-variable))
  (propertize (concat
               (ai-workbench-backend-name
                (buffer-local-value (oref obj backend) transient--original-buffer)) ":"
               (ai-workbench--model-name (oref obj value)))
              'face 'transient-value))

(cl-defmethod transient-infix-set ((obj ai-workbench-provider-variable) value)
  (pcase-let ((`(,backend-value ,model-value) value))
    (funcall (oref obj set-value)
             (oref obj variable)
             (oset obj value model-value)
             ai-workbench--set-buffer-locally)
    (funcall (oref obj set-value)
             (oref obj backend)
             (oset obj backend-value backend-value)
             ai-workbench--set-buffer-locally))
  (transient-setup))

;; ** Class for infix options with in-buffer overlay display

(defclass ai-workbench-option-overlaid (transient-option)
  ((display-nil :initarg :display-nil)
   (overlay :initarg :overlay))
  "Transient options for overlays displayed in the working buffer.")

(cl-defmethod transient-format-value ((obj ai-workbench-option-overlaid))
  "Set up the in-buffer overlay for additional directive, a string.

Also format the value of OBJ in the transient menu."
  (let ((value (oref obj value))
        (ov    (oref obj overlay))
        (argument (oref obj argument)))
    ;; Making an overlay
    (if (or (not value) (string-empty-p value))
        (when ov (delete-overlay ov))
      (with-current-buffer transient--original-buffer
        (oset obj overlay (ai-workbench--instructions-make-overlay value ov)))
      (letrec ((ov-clear-hook
                (lambda () (when-let* ((ov (oref obj overlay))
                                  ((overlayp ov)))
                        (remove-hook 'transient-exit-hook
                                     ov-clear-hook)
                        (delete-overlay ov)))))
        (add-hook 'transient-exit-hook ov-clear-hook)))
    ;; Updating transient menu display
    (if value
        (propertize (concat argument (truncate-string-to-width value 35 nil nil t))
                    'face 'transient-value)
      (propertize
       (concat "(" (symbol-name (oref obj display-nil)) ")")
       'face 'transient-inactive-value))))


;; * Transient Prefixes

;;;###autoload (autoload 'ai-workbench-menu "ai-workbench-transient" nil t)
(transient-define-prefix ai-workbench-menu ()
  "Change parameters of prompt to send to the LLM."
  :incompatible '(("m" "y" "i") ("e" "g" "b" "k"))
  :environment #'ai-workbench--transient-fix-evil-visual
  ;; :value (list (concat "b" (buffer-name)))
  [:description ai-workbench-system-prompt--format
   [""
    :if (lambda () (not (ai-workbench--model-capable-p 'nosystem)))
    "Instructions"
    ("s" "Set system message" ai-workbench-system-prompt :transient t)
    (ai-workbench--infix-add-directive)]
   [:pad-keys t ""
    (:info #'ai-workbench--describe-infix-context
     :face transient-heading :format "%d")
    (ai-workbench--infix-context-add-current-kill)
    (ai-workbench--infix-context-add-region)
    (ai-workbench--infix-context-add-buffer)
    (ai-workbench--infix-context-add-file)
    (ai-workbench--infix-context-remove-all)
    (ai-workbench--suffix-context-buffer)]
   [:pad-keys t
    :if (lambda () (and ai-workbench-use-tools
                   (or ai-workbench--known-tools (featurep 'ai-workbench-integrations))))
    "" (:info
        (lambda ()
          (concat
           "Tools" (and ai-workbench-llm-tools
                        (concat " (" (propertize (format "%d selected"
                                                         (length ai-workbench-llm-tools))
                                                 'face 'warning)
                                ")"))))
        :format "%d" :face transient-heading)
    ("t" "Select tools" ai-workbench-llm-tools :transient t)
    ("T" "Continue tool calls"
     (lambda () (interactive) (ai-workbench--handle-tool-use ai-workbench--fsm-last))
     :if (lambda () (and ai-workbench--fsm-last
                    (eq (ai-workbench-fsm-state ai-workbench--fsm-last) 'TOOL))))]]
  [[(ai-workbench-preset
     :transient t
     :key "@" :format "%d"
     :description
     (lambda ()
       (concat (propertize "Request Parameters" 'face 'transient-heading)
               (ai-workbench--format-preset-string))))
    (ai-workbench--infix-variable-scope)
    (ai-workbench--infix-provider)
    (ai-workbench--infix-max-tokens)
    (ai-workbench--infix-num-messages-to-send
     :if (lambda () (and ai-workbench-expert-commands
                    (or ai-workbench-mode ai-workbench-track-response))))
    (ai-workbench--infix-temperature :if (lambda () ai-workbench-expert-commands))
    (ai-workbench--infix-use-context)
    (ai-workbench--infix-include-reasoning)
    (ai-workbench--infix-use-tools)
    (ai-workbench--infix-track-response
     :if (lambda () (and ai-workbench-expert-commands (not ai-workbench-mode))))
    (ai-workbench--infix-track-media :if (lambda () ai-workbench-mode))]
   [" <Prompt from"
    ("m" "Minibuffer instead" "m")
    ("y" "Kill-ring instead" "y")
    ""
    ("i" "Respond in place" "i")]
   [" >Response to"
    ("e" "Echo area" "e")
    ("b" "Other buffer" "b"
     :class transient-option
     :prompt "Output to buffer: "
     :reader (lambda (prompt _ _history)
               (read-buffer prompt (buffer-name (other-buffer)) nil)))
    ("g" "ai-workbench-engine session" "g"
     :class transient-option
     :prompt "Existing or new ai-workbench-engine session: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer
        prompt (generate-new-buffer-name
                (concat "*" (ai-workbench-backend-name ai-workbench-backend) "*"))
        nil (lambda (buf-name)
              (if (consp buf-name) (setq buf-name (car buf-name)))
              (let ((buf (get-buffer buf-name)))
                (and (buffer-local-value 'ai-workbench-mode buf)
                     (not (eq (current-buffer) buf))))))))
    ("k" "Kill-ring" "k")]]
  [[:description (lambda () (concat (and ai-workbench--rewrite-overlays "Continue ")
                               "Rewrite"))
    :if (lambda () (or (use-region-p)
                  (and ai-workbench--rewrite-overlays
                       (ai-workbench--rewrite-sanitize-overlays))))
    ("r"
     (lambda () (if (get-char-property (point) 'ai-workbench-rewrite)
               "Iterate" "Rewrite"))
     ai-workbench-rewrite)]
   ["Tweak Response" :if ai-workbench--in-response-p :pad-keys t
    ("SPC" "Mark" ai-workbench--mark-response)
    ("M-RET" "Regenerate" ai-workbench--regenerate :if ai-workbench--in-response-p)
    ("P" "Previous variant" ai-workbench--previous-variant
     :if ai-workbench--at-response-history-p
     :transient t)
    ("N" "Next variant" ai-workbench--previous-variant
     :if ai-workbench--at-response-history-p
     :transient t)
    ("E" "Ediff previous" ai-workbench--ediff
     :if ai-workbench--at-response-history-p)]
   ["Dry Run" :if (lambda () (or ai-workbench-log-level ai-workbench-expert-commands))
    ("I" "Inspect query (Lisp)"
     (lambda ()
       "Inspect the query that will be sent as a lisp object."
       (interactive)
       (ai-workbench--sanitize-model)
       (ai-workbench--inspect-query
        (ai-workbench--suffix-send
         (cons "I" (transient-args transient-current-command))))))
    ("J" "Inspect query (JSON)"
     (lambda ()
       "Inspect the query that will be sent as a JSON object."
       (interactive)
       (ai-workbench--sanitize-model)
       (ai-workbench--inspect-query
        (ai-workbench--suffix-send
         (cons "I" (transient-args transient-current-command)))
        'json)))]
   ["Logging"
    :if (lambda () (or ai-workbench-log-level ai-workbench-expert-commands))
    ("-l" "Log level" "-l"
     :class ai-workbench-lisp-variable
     :variable ai-workbench-log-level
     :set-value ai-workbench--set-with-scope
     :display-nil "Off"
     :prompt "Log level: "
     :reader
     (lambda (prompt _ _)
       "Manage ai-workbench-engine's logging."
       (let ((state (completing-read
                     prompt '("off" "info" "debug") nil t)))
         (message "Log level set to %s" state)
         (if (string= state "off") nil (intern state)))))
    ("L" "Inspect Log"
     (lambda () (interactive)
       (pop-to-buffer (get-buffer-create ai-workbench--log-buffer-name)))
     :format "  %k %d")]]
  [(ai-workbench--suffix-send)]
  (interactive)
  (ai-workbench--sanitize-model)
  (when ai-workbench-context        ;MAYBE: Move this to a dedicated sanitize function?
    (setq ai-workbench-context
          (cl-delete-if
           (lambda (entry)
             (let ((first (or (car-safe entry) entry)))
               (and (bufferp first) (not (buffer-live-p first)))))
           ai-workbench-context)))
  (transient-setup 'ai-workbench-menu))

;; ** Prefix for setting the system prompt.

(defun ai-workbench--setup-directive-menu (sym msg &optional external)
  "Return a list of infix definitions for setting ai-workbench-engine directives.

SYM is the symbol whose value is set to the selected directive..
MSG is the meaning of symbol, used when messaging.
If EXTERNAL is non-nil, include external sources of directives."
  (cl-loop for (type . prompt) in ai-workbench-directives
           ;; Avoid clashes with the custom directive key
           with unused-keys = (delete ?s (nconc (number-sequence ?a ?z)
                                                (number-sequence ?0 ?9)))
           with width = (window-width)
           for name = (symbol-name type)
           for key = (seq-find (lambda (k) (member k unused-keys)) name (seq-first unused-keys))
           do (setq unused-keys (delete key unused-keys))
           ;; The explicit declaration ":transient transient--do-return" here
           ;; appears to be required for Transient v0.5 and up.  Without it, these
           ;; are treated as suffixes when invoking `ai-workbench-system-prompt' directly,
           ;; and infixes when going through `ai-workbench-menu'.
           ;; TODO: Raise an issue with Transient.
           collect
           (list (key-description (list key))
                 (concat (capitalize name) " "
                         (propertize " " 'display '(space :align-to 20))
                         (propertize
                          (concat "(" (ai-workbench--describe-directive prompt (- width 30)) ")")
                          'face 'shadow))
                 `(lambda () (interactive)
                    (message "%s: %s" ,msg ,(ai-workbench--describe-directive prompt 100 "⮐ "))
                    (ai-workbench--set-with-scope ',sym ',prompt ai-workbench--set-buffer-locally))
	         :transient 'transient--do-return)
           into prompt-suffixes
           finally return
           (nconc
            prompt-suffixes
            (list (list "DEL" "None"
                        `(lambda () (interactive)
                           (message "%s unset" ,msg)
                           (ai-workbench--set-with-scope ',sym nil ai-workbench--set-buffer-locally))
                        :transient 'transient--do-return))
            (and external
                 (list (list "SPC" "Pick crowdsourced prompt"
                             'ai-workbench--read-crowdsourced-prompt
		             ;; NOTE: Quitting the completing read when picking a
		             ;; crowdsourced prompt will cause the transient to exit
		             ;; instead of returning to the system prompt menu.
                             :transient 'transient--do-exit))))))

;;;###autoload (autoload 'ai-workbench-system-prompt "ai-workbench-transient" nil t)
(transient-define-prefix ai-workbench-system-prompt ()
  "Set the LLM system message for LLM interactions.

The \"system message\" establishes directives for the chat
session and modifies the behavior of the LLM. Some examples of
system prompts are:

You are a helpful assistant. Answer as concisely as possible.
Reply only with shell commands and no prose.
You are a poet. Reply only in verse.

More extensive system messages can be useful for specific tasks.

Customize `ai-workbench-directives' for task-specific prompts."
  [:description ai-workbench-system-prompt--format
   [(ai-workbench--suffix-system-message)]
   [(ai-workbench--infix-variable-scope)]]
   [:class transient-column
    :setup-children
    (lambda (_) (transient-parse-suffixes
            'ai-workbench-system-prompt
            (ai-workbench--setup-directive-menu
             'ai-workbench-system-prompt "Directive" t)))
    :pad-keys t])

;; ** Prefix for saving and applying presets
;;;###autoload
(defun ai-workbench-preset (preset &optional setter)
  "Load ai-workbench-engine PRESET with SETTER.

Interactively, query for PRESET, allow the preset scope to be set
dynamically, and offer to save the current ai-workbench-engine settings as a new or
existing preset, as well."
  (interactive
   (let ((hint (concat (propertize "C-s" 'face 'transient-value)
                       (propertize " Save as preset"
                                   'face 'transient-inactive-value)
                       ", " (propertize "=" 'face 'transient-value)
                       (propertize " Scope " 'face 'transient-inactive-argument)))
         (scope-obj (ai-workbench--scope :variable 'ai-workbench--set-buffer-locally))
         (completion-extra-properties
          (list :annotation-function
                (lambda (cand)
                  (and-let* ((str (plist-get
                                   (cdr (assq (intern cand) ai-workbench--known-presets))
                                   :description)))
                    (concat (propertize " " 'display '(space :align-to 25))
                            (truncate-string-to-width str (- (frame-width) 40)
                                                      nil nil t)))))))
     (cl-flet* ((key-hint-ov ()
                  (or (cdr-safe (get-char-property-and-overlay (point-min) 'ai-workbench-engine))
                      (let ((ov (make-overlay (point-min) (minibuffer-prompt-end))))
                        (overlay-put ov 'ai-workbench-engine 'prefix)
                        (overlay-put ov 'before-string
                                     (concat hint (transient-format-value scope-obj)
                                             "\n"))
                        ov)))
                (save-preset ()
                  (interactive)
                  (when (minibufferp)
                    (run-at-time 0 nil (lambda (menu) (call-interactively #'ai-workbench--save-preset)
                                         (when menu (transient-setup 'ai-workbench-menu)))
                                 transient--prefix)
                    (minibuffer-quit-recursive-edit)))
                (update-scope ()
                  (interactive)
                  (transient-infix-set
                   scope-obj
                   (pcase ai-workbench--set-buffer-locally (1 nil) ('nil t) ('t 1)))
                  (overlay-put (key-hint-ov) 'before-string
                               (concat hint
                                       (transient-format-value scope-obj)
                                       "\n"))))
       (minibuffer-with-setup-hook
           (lambda () (use-local-map (make-composed-keymap
                                 (define-keymap "C-s" #'save-preset "=" #'update-scope)
                                 (current-local-map)))
             (key-hint-ov))
         (list (intern (completing-read (format "Load preset: ")
                                        ai-workbench--known-presets nil t)))))))
  (ai-workbench--apply-preset preset
                       (or setter (lambda (sym val) (ai-workbench--set-with-scope
                                                     sym val ai-workbench--set-buffer-locally))))
  (when transient--prefix
    (transient-setup 'ai-workbench-menu)))

;; ** Prefix for selecting tools

;; ai-workbench-llm-tools offers a two-level menu for selecting tools, its design is a
;; little convoluted so here's an explanation:
;;
;; Normally a transient prefix exports its value via transient-args, to be
;; consumed by suffixes, where these args are determined by the state of the
;; menu at the time of export.  The ai-workbench-llm-tools menu is dynamic and needs to
;; store tool selections that may not be visible in the meny any more, so we
;; cannot use the transient-args.
;;
;; We can not (should not?) control the value of the prefix directly, so we
;; instead use the scope (a secondary value) of the prefix to maintain the
;; history of selections.  When running a suffix, we gather tool selections from
;; the scope.  The scope is also used as a message channel for connecting the
;; category menu and the tool list menu for that category.

;;;###autoload (autoload 'ai-workbench-llm-tools "ai-workbench-transient" nil t)
(transient-define-prefix ai-workbench-llm-tools ()
  "Select tools to include with ai-workbench-engine requests.

Tools are organized into categories.  Selecting the category
toggles all the tools with that category.

To add tools to this list, use `ai-workbench-make-tool', which see.

Using the scope option, you can set tools to use with ai-workbench-engine
requests globally, in this buffer or for the next request
only (\"oneshot\")."
  :refresh-suffixes t
  [:description "Provide the LLM with tools to run tasks for you"
   [""
    (ai-workbench--infix-variable-scope)
    (ai-workbench--infix-use-tools)
    (ai-workbench--infix-confirm-tool-calls)
    (ai-workbench--infix-include-tool-results)]
   [""
    ("RET" "Confirm selection"
     (lambda (tools)
       ;; We don't care about the transient args of this prefix at all, since
       ;; the state is managed entirely through its transient-scope:
       (interactive (list (plist-get (transient-scope 'ai-workbench-llm-tools) :tools)))
       (ai-workbench--set-with-scope
        'ai-workbench-llm-tools
        (mapcar (lambda (category-and-name)
                  (map-nested-elt ai-workbench--known-tools category-and-name))
                (cl-delete-if-not #'consp tools))
        ai-workbench--set-buffer-locally))
     :transient transient--do-return)
    ("q" "Cancel" transient-quit-one)]]
  [[:class transient-column             ;Display known categories
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'ai-workbench-llm-tools
       (cl-loop          ;loop through ai-workbench--known tools and collect categories
        for (category . tools-alist) in ai-workbench--known-tools
        with unused-keys = (nconc (delete ?q (number-sequence ?a ?z))
                                  (number-sequence ?0 ?9)
                                  (delete ?M (number-sequence ?A ?Z))) ;M used by MCP integration
        for category-key = (seq-find (lambda (k) (member k unused-keys))
                                     (string-remove-prefix "mcp-" category)
                                     (seq-first unused-keys))
        do (setq unused-keys (delete category-key unused-keys))
        collect (list (key-description (list category-key))
                      (concat (propertize category 'face 'transient-heading)
                              (make-string (max (- 14 (length category)) 0) ? ))
                      (char-to-string category-key)
                      :format " %k %d %v"
                      :class 'ai-workbench--switch-category
                      :category category)
        into categories
        finally do (plist-put (transient-scope) :keys unused-keys)
        finally return categories)))]
   [:class transient-column           ;Display known tools for selected category
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'ai-workbench-llm-tools
       (when-let* ((category (plist-get (transient-scope) :category))
                   (tool-keys (plist-get (transient-scope) :keys)))
         (cl-loop                   ;for each category, collect tools as infixes
          with tools-alist = (cdr (assoc category ai-workbench--known-tools))
          for (name . tool) in tools-alist
          for tool-key = (seq-find (lambda (k) (member k tool-keys)) name (seq-first tool-keys))
          do (setq tool-keys (delete tool-key tool-keys))
          collect          ;Each list is a transient infix of type ai-workbench--switch
          (list (key-description (list tool-key))
                (concat (make-string (max (- 20 (length name)) 0) ? )
                        (propertize
                         (concat "(" (ai-workbench--describe-directive
                                      (ai-workbench-tool-description tool) (- (window-width) 60))
                                 ")")
                         'face 'shadow))
                (ai-workbench-tool-name tool)
                :format " %k %v %d"
                :init-value #'ai-workbench--tools-init-value
                :class 'ai-workbench--switch
                :category category)
          into infixes-for-category
          finally return
          (cons (list :info
                      (lambda () (concat
                             (propertize (plist-get (transient-scope) :key)
                                         'face 'transient-key)
                             (propertize " toggle all" 'face 'transient-heading)))
                      :format " %d")
                infixes-for-category)))))]]
  (interactive)
  (transient-setup
   'ai-workbench-llm-tools nil nil
   :scope (list :tools (mapcar (lambda (tool) (list (or (ai-workbench-tool-category tool) "misc")
                                               (ai-workbench-tool-name tool)))
                               ai-workbench-llm-tools))))


;; * Transient Infixes

;; ** Infixes for context aggregation

(transient-define-infix ai-workbench--infix-use-context ()
  "Describe target destination for context injection.

ai-workbench-engine will include with the LLM request any additional context
added with `ai-workbench-add'.  This context can be ignored, included
with the system message or included with the user prompt.

Where in the request this context is included depends on the
value of `ai-workbench-use-context', set from here."
  :description "Include context"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-use-context
  :format " %k %d %v"
  :set-value #'ai-workbench--set-with-scope
  :display-nil "No"
  :display-map '((nil    . "No")
                 (system . "with system message")
                 (user   . "with user prompt"))
  :key "-i"
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("No"                  . nil)
                              ("with system message" . system)
                              ("with user prompt"    . user)))
                   (destination (completing-read prompt choices nil t)))
              (cdr (assoc destination choices)))))

;; ** Infixes for model parameters

(transient-define-infix ai-workbench--infix-variable-scope ()
  "Set ai-workbench-engine's model parameters and system message in this buffer or globally."
  :argument "scope"
  :variable 'ai-workbench--set-buffer-locally
  :class 'ai-workbench--scope
  :format "  %k %d %v"
  :key "="
  :description (propertize "Scope" 'face 'transient-inactive-argument))

(transient-define-infix ai-workbench--infix-num-messages-to-send ()
  "Number of recent messages to send with each exchange.

By default, the full conversation history is sent with every new
prompt.  This retains the full context of the conversation, but
can be expensive in token size.  Set how many recent messages to
include."
  :description "previous responses"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench--num-messages-to-send
  :set-value #'ai-workbench--set-with-scope
  :display-nil 'all
  :format " %k %v %d"
  :key "-n"
  :prompt "Number of past messages to include for context (leave empty for all): "
  :reader 'ai-workbench--transient-read-number)

(transient-define-infix ai-workbench--infix-max-tokens ()
  "Max tokens per response.

This is roughly the number of words in the response.  100-300 is a
reasonable range for short answers, 400 or more for longer
responses."
  :description "Response length (tokens)"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-max-tokens
  :set-value #'ai-workbench--set-with-scope
  :display-nil 'auto
  :key "-c"
  :prompt "Response length in tokens (leave empty: default, 80-200: short, 200-500: long): "
  :reader 'ai-workbench--transient-read-number)

;; TODO(links): Run `ai-workbench-refresh-buffer-hook' after a model change
(transient-define-infix ai-workbench--infix-provider ()
  "AI Provider for Chat."
  :description "Model"
  :class 'ai-workbench-provider-variable
  :prompt "Model: "
  :variable 'ai-workbench-model
  :set-value #'ai-workbench--set-with-scope
  :backend 'ai-workbench-backend
  :key "-m"
  :reader (lambda (prompt &rest _)
            (cl-loop
             for (name . backend) in ai-workbench--known-backends
             nconc (cl-loop for model in (ai-workbench-backend-models backend)
                            collect (list (concat name ":" (ai-workbench--model-name model))
                                          backend model))
             into models-alist
             with completion-extra-properties =
             `(:annotation-function
               ,(lambda (comp)
		  (let* ((model (nth 2 (assoc comp models-alist)))
			 (desc (get model :description))
			 (caps (get model :capabilities))
			 (context (get model :context-window))
			 (input-cost (get model :input-cost))
			 (output-cost (get model :output-cost))
			 (cutoff (get model :cutoff-date)))
		    (when (or desc caps context input-cost output-cost cutoff)
		      (concat
		       (propertize " " 'display `(space :align-to 40))
		       (when desc (truncate-string-to-width desc 70 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 112))
		       (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 134))
		       (when context (format "%5dk" context))
		       " " (propertize " " 'display `(space :align-to 142))
		       (when input-cost (format "$%5.2f in" input-cost))
		       (if (and input-cost output-cost) "," " ")
		       " " (propertize " " 'display `(space :align-to 153))
		       (when output-cost (format "$%6.2f out" output-cost))
		       " " (propertize " " 'display `(space :align-to 166))
		       cutoff)))))
             finally return
             (cdr (assoc (completing-read prompt models-alist nil t nil nil
					  (concat (ai-workbench-backend-name ai-workbench-backend) ":"
						  (ai-workbench--model-name ai-workbench-model)))
                         models-alist)))))

(transient-define-infix ai-workbench--infix-temperature ()
  "Temperature of request."
  :description "Temperature (0 - 2.0)"
  :display-nil "default"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-temperature
  :set-value #'ai-workbench--set-with-scope
  :key "-T"
  :prompt "Temperature controls the response randomness (0.0-2.0, leave empty for API default): "
  :reader 'ai-workbench--transient-read-number)

(transient-define-infix ai-workbench--infix-track-response ()
  "Distinguish between user messages and LLM responses.

When creating a prompt to send to the LLM, ai-workbench-engine distinguishes
between text entered by the user and past LLM responses.  This is
required for multi-turn conversations, and is always the case in
dedicated chat buffers (in `ai-workbench-mode').

In regular buffers, you can toggle this behavior here or by
customizing `ai-workbench-track-response'.  When response tracking is
turned off, all text will be assigned the \"user\" role when
querying the LLM."
  :description "Track LLM responses"
  :class 'ai-workbench--switches
  :variable 'ai-workbench-track-response
  :set-value #'ai-workbench--set-with-scope
  :display-if-true "Yes"
  :display-if-false "No"
  :key "-R")

(transient-define-suffix ai-workbench--infix-track-media ()
  "Send media from links in the prompt.

ai-workbench-engine can send images or other media from links in the buffer to the
LLM.  What link types are sent depends on the mime-types the model
supports.  See `ai-workbench-track-media' for more information."
  :description "Send media from links"
  :transient t
  :class 'ai-workbench--switches
  :variable 'ai-workbench-track-media
  :set-value #'ai-workbench--set-with-scope
  :display-if-true "Yes"
  :display-if-false "No"
  :key "-I"
  (interactive)
  (let ((obj (transient-suffix-object)))
    (transient-infix-set obj (transient-infix-read obj))
    (transient--show))
  (if ai-workbench-track-media
      (run-hooks 'ai-workbench-refresh-buffer-hook)
    (ai-workbench--annotate-link-clear)))

;; ** Infixes for adding and removing context

(declare-function ai-workbench-context--at-point "ai-workbench-context")
(declare-function ai-workbench-add "ai-workbench-context")
(declare-function ai-workbench-context-add-current-kill "ai-workbench-context")

(transient-define-suffix ai-workbench--infix-context-add-current-kill (&optional arg)
  "Add current kill to ai-workbench-engine's context."
  :transient 'transient--do-stay
  :key "C-y"
  :if (lambda () ai-workbench-expert-commands)
  :description
  "Yank to context"
  (interactive "P")
  (require 'ai-workbench-context)
  (ai-workbench-context-add-current-kill arg)
  (transient-setup))

(transient-define-suffix ai-workbench--infix-context-add-region ()
  "Add current region to ai-workbench-engine's context."
  :transient 'transient--do-stay
  :key "-r"
  :if (lambda () (or (use-region-p)
                (and (fboundp 'ai-workbench-context--at-point)
                     (ai-workbench-context--at-point))))
  :description
  (lambda ()
    (if (and (fboundp 'ai-workbench-context--at-point)
             (ai-workbench-context--at-point))
        "Remove context at point"
      "Add region to context"))
  (interactive)
  (ai-workbench-add)
  (transient-setup))

(transient-define-suffix ai-workbench--infix-context-add-buffer ()
  "Add a buffer to ai-workbench-engine's context."
  :transient 'transient--do-stay
  :key "-b"
  :description "Add a buffer to context"
  (interactive)
  (ai-workbench-add '(4))
  (transient-setup))

(declare-function ai-workbench-add-file "ai-workbench-context")
(declare-function ai-workbench-context-remove-all "ai-workbench-context")

(transient-define-suffix ai-workbench--infix-context-add-file ()
  "Add a file to ai-workbench-engine's context."
  :transient 'transient--do-stay
  :key "-f"
  :description "Add a file to context"
  (interactive)
  (call-interactively #'ai-workbench-add-file)
  (transient-setup))

(transient-define-suffix ai-workbench--infix-context-remove-all ()
  "Clear ai-workbench-engine's context."
  :if (lambda () ai-workbench-context)
  :transient 'transient--do-stay
  :key "-d"
  :description "Remove all"
  (interactive)
  (ai-workbench-context-remove-all t)
  (transient-setup))

;; ** Infix for additional directive

(transient-define-infix ai-workbench--infix-add-directive ()
  "Additional directive intended for the next query only.

This is useful to define a quick task on top of a more extensive
or detailed system message.

For example, with code/text selected:

- Rewrite this function to do X while avoiding Y.
- Change the tone of the following paragraph to be more direct.

Or in an extended conversation:

- Phrase you next response in ten words or less.
- Pretend for now that you're an anthropologist."
  :class 'ai-workbench-option-overlaid
  ;; :variable 'ai-workbench--instructions
  :display-nil 'none
  :overlay nil
  :argument ":"
  :prompt (concat "Add instructions for next request only ("
                  ai-workbench--read-with-prefix-help ") ")
  ;; TODO: Add the ability to edit this in a separate buffer, with
  ;; `ai-workbench--edit-directive'.  This requires setting up ai-workbench-menu with the
  ;; result as the :scope.
  :reader (lambda (prompt initial history)
            (let* ((directive
                    (car-safe (ai-workbench--parse-directive ai-workbench-system-prompt 'raw)))
                   (cycle-prefix (lambda () (interactive)
                                   (ai-workbench--read-with-prefix directive)))
                   (minibuffer-local-map
                    (make-composed-keymap
                     (define-keymap "TAB" cycle-prefix "<tab>" cycle-prefix)
                     minibuffer-local-map))
                   (extra (minibuffer-with-setup-hook cycle-prefix
                            (read-string prompt (or initial " ") history))))
              (unless (string-empty-p extra) extra)))
  :format " %k %d %v"
  :key "d"
  :argument ":"
  :description "Add instruction"
  :transient t)

;; ** Infix for reasoning block control

(transient-define-infix ai-workbench--infix-include-reasoning ()
  "How to handle reasoning/thinking response blocks.

Some LLMs include in their response a \"thinking\" section.  This
text improves the quality of the LLM's final output, but may not
be interesting to you by itself.

You can control how ai-workbench-engine should handle the thinking blocks via
this option, or by setting the variable `ai-workbench-include-reasoning'
via elisp, which see.

Available behaviors are
- to include thinking blocks with the response,
- to omit them entirely,
- to include them but ignore them in consequent conversation turns, and
- to append them to a buffer of your choosing."
  :description "Include reasoning"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-include-reasoning
  :format " %k %d %v"
  :set-value #'ai-workbench--set-with-scope
  :display-nil "No"
  :display-map '((nil    . "No")
                 (ignore . "and ignore")
                 (t      . "with response"))
  :key "-v"
  :prompt "Include reasoning: "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("no"     . nil)
                              ("ignore" . ignore)
                              ("yes"    . t)
                              ("other buffer" . buffer)))
                   (destination
                    (completing-read prompt choices nil t)))
              (if (equal destination "other buffer")
                  (read-buffer "Append reasoning to buffer: ")
                (cdr (assoc destination choices))))))

;; ** Infixes for tool use

(transient-define-infix ai-workbench--infix-use-tools ()
  "Whether LLM tool use with ai-workbench-engine is enabled.

This is a three-way toggle.  Assuming one or more tools to be
sent with requests have been selected, tool use can be

- disabled,
- enabled, where the LLM may choose to respond with tool calls
- forced, where the LLM must respond with one or more tool calls.

You can set this here or by customizing `ai-workbench-use-tools', which
see."
  :description "Use tools"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-use-tools
  :set-value (lambda (sym value scope)
               (ai-workbench--set-with-scope sym value scope)
               (transient-setup))
  :display-nil "off"
  :display-map '((nil   . "off")
                 (t     . "on")
                 (force . "force"))
  :prompt "Use tools? "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("disable" . nil)
                              ("enable"  . t)
                              ("force"   . force)))
                   (pref (completing-read prompt choices nil t)))
              (cdr (assoc pref choices))))
  :key "-t")

(transient-define-infix ai-workbench--infix-confirm-tool-calls ()
  "Whether tool calls should wait for the user to run them.

This is a three-way toggle between these behaviors:

- All tool calls run without confirmation.
- All tool calls wait for confirmation.
- Decided per-tool, according to the value of the tool spec's
  :confirm slot.

This sets the variable `ai-workbench-confirm-tool-calls', which see."
  :key "-c"
  :description "Confirm tool calls"
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-confirm-tool-calls
  :set-value #'ai-workbench--set-with-scope
  :display-nil "never"
  :display-map '((nil . "never")
                 (t   . "always")
                 (auto . "auto"))
  :prompt "Tool calls require confirmation? "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("no"   . nil)
                              ("always" . t)
                              ("tool decides" . auto)))
                   (pref (completing-read prompt choices nil t)))
              (cdr (assoc pref choices)))))

(transient-define-infix ai-workbench--infix-include-tool-results ()
  "Whether tool call results should be included in the response.

This is a three-way toggle between these behaviors:

- All tool results are included.
- No tool results are included.
- Decided per-tool, according to the value of the tool spec's
  :include slot.

This sets the variable `ai-workbench-include-tool-results', which see."
  :key "-i"
  :description "Include results   "
  :class 'ai-workbench-lisp-variable
  :variable 'ai-workbench-include-tool-results
  :set-value #'ai-workbench--set-with-scope
  :display-nil "never"
  :display-map '((nil . "never")
                 (t   . "always")
                 (auto . "auto"))
  :prompt "Include tool results in LLM response? "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("never"   . nil)
                              ("always" . t)
                              ("tool decides" . auto)))
                   (pref (completing-read prompt choices nil t)))
              (cdr (assoc pref choices)))))


;; * Transient Suffixes

;; ** Suffix to send prompt

(transient-define-suffix ai-workbench--suffix-send (args)
  "Call `ai-workbench-send' with ARGS."
  :key "RET"
  :description #'ai-workbench--describe-suffix-send
  (interactive (list (transient-args
                      (or transient-current-command 'ai-workbench-menu))))
  (let ((stream ai-workbench-stream)
        (in-place (and (member "i" args) t))
        (redirect-output)
        (backend ai-workbench-backend)
        (model ai-workbench-model)
        (backend-name (ai-workbench-backend-name ai-workbench-backend))
        (buffer) (position)
        (callback) (ai-workbench-buffer-name)
        (system-extra (ai-workbench--get-directive args))
        (dry-run (and (member "I" args) t))
        ;; Input redirection: grab prompt from elsewhere?
        (prompt
         (cond
          ((member "m" args) (ai-workbench--read-minibuffer-prompt))
          ((member "y" args)
           (unless (car-safe kill-ring)
             (user-error "`kill-ring' is empty!  Nothing to send"))
           (if current-prefix-arg
               (read-from-kill-ring "Prompt from kill-ring: ")
             (current-kill 0))))))

    ;; Output redirection: Send response elsewhere?
    (cond
     ((member "e" args)                 ;Send to echo-area
      (setq redirect-output t)
      (setq stream nil)
      (setq callback
            (lambda (resp info &optional _raw)
              (pcase resp
                ((pred stringp) (message "%s response: %s" backend-name resp))
                (`(tool-call . ,tool-calls) (ai-workbench--display-tool-calls tool-calls info 'minibuffer))
                (_ (when (and (null resp) (plist-get info :error))
                     (message "%s response error: %s"
                              backend-name (plist-get info :status))))))))
     ((member "k" args)                 ;Send to kill-ring
      (setq redirect-output t)
      (setq stream nil)
      (setq callback
            (let ((accum))
              (lambda (resp info &optional _raw)
                (pcase resp
                  ((pred stringp) (push resp accum)
                   (unless (plist-get info :tool-use)
                     (kill-new (apply #'concat (nreverse accum)))
                     (message "%s response: \"%s\" copied to kill-ring." backend-name
                              (truncate-string-to-width resp 30 nil nil t))))
                  (`(tool-call . ,tool-calls) (ai-workbench--display-tool-calls tool-calls info 'minibuffer))
                  (_ (when (and (null resp) (plist-get info :error))
                       (if accum (kill-new (apply #'concat (nreverse accum))))
                       (message
                        (concat "%s response error: %s."
                                (and accum "  Partial response copied to kill-ring."))
                                backend-name (plist-get info :status)))))))))
     ((setq ai-workbench-buffer-name           ;Send to ai-workbench-engine buffer
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "g" s)
                                 (substring s 1)))
                     args))
      (setq redirect-output t)
      (let* ((reduced-prompt            ;For inserting into the ai-workbench-engine buffer as
                                        ;context, not the prompt used for the
                                        ;request itself
              (or prompt
                  (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (buffer-substring-no-properties
                     (save-excursion
                       (text-property-search-backward
                        'ai-workbench-engine 'response
                        (when (get-char-property (max (point-min) (1- (point)))
                                                 'ai-workbench-engine)
                          t))
                       (point))
                     (ai-workbench--at-word-end (point))))))
             (ai-workbench-buffer (get-buffer ai-workbench-buffer-name))
             (ai-workbench-buffer-mode
              (if (buffer-live-p ai-workbench-buffer)
                  (buffer-local-value 'major-mode ai-workbench-buffer)
                ai-workbench-default-mode)))
        ;; Add code fences or Org src markers around the reduced-prompt
        (cond ((and (stringp prompt) (eq major-mode ai-workbench-buffer-mode)))
              ((provided-mode-derived-p ai-workbench-buffer-mode 'org-mode)
               (setq reduced-prompt
                     (if (consp reduced-prompt);either (region . prompt) or prompt
                         (concat (and (car reduced-prompt)
                                      (concat "#+begin_src " (ai-workbench--strip-mode-suffix major-mode)
                                              "\n" (org-escape-code-in-string (car reduced-prompt))
                                              "\n#+end_src\n\n"))
                                 (cdr reduced-prompt))
                       (concat "#+begin_src " (ai-workbench--strip-mode-suffix major-mode)
                               "\n" (org-escape-code-in-string
                                     (or (cdr-safe reduced-prompt) reduced-prompt))
                               "\n#+end_src"))))
              (t (setq reduced-prompt
                       (if (consp reduced-prompt);either (region . prompt) or prompt
                           (concat (and (car reduced-prompt)
                                        (concat  "``` " (ai-workbench--strip-mode-suffix major-mode) "\n"
                                                 (car reduced-prompt) "\n```\n\n"))
                                   (cdr reduced-prompt))
                         (concat "``` " (ai-workbench--strip-mode-suffix major-mode) "\n"
                                 (or (cdr-safe reduced-prompt) reduced-prompt) "\n```" )))))
        (cond
         ((buffer-live-p ai-workbench-buffer)
          ;; Insert into existing ai-workbench-engine session
          (setq buffer ai-workbench-buffer)
          (with-current-buffer buffer
            (goto-char (point-max))
            (unless (or buffer-read-only
                        (get-char-property (point) 'read-only))
              (unless (bolp) (insert "\n"))
              (insert reduced-prompt))
            (setq position (point-marker))
            (when (and ai-workbench-mode (not dry-run))
              (ai-workbench--update-status " Waiting..." 'warning))))
         ;; Insert into new ai-workbench-engine session
         (t (setq buffer
                  (ai-workbench-engine ai-workbench-buffer-name
                         (condition-case nil
                             (ai-workbench--get-api-key)
                           ((error user-error)
                            (setq ai-workbench-api-key
                                  (read-passwd
                                   (format "%s API key: "
                                           (ai-workbench-backend-name
                                            ai-workbench-backend))))))
                         reduced-prompt))
            ;; Set backend and model in new session from current buffer
            (with-current-buffer buffer
              (setq ai-workbench-backend backend)
              (setq ai-workbench-model model)
              (unless dry-run
                (ai-workbench--update-status " Waiting..." 'warning))
              (setq position (point-marker)))))))
     ((setq ai-workbench-buffer-name           ;Send to specified buffer
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "b" s)
                                 (substring s 1)))
                     args))
      (setq redirect-output t)
      (setq buffer (get-buffer-create ai-workbench-buffer-name))
      (with-current-buffer buffer (setq position (point-marker)))))

    ;; MAYBE: This is no a good way to handle two-part (region + instruction) prompts
    ;; If the prompt is a cons (region-text . instructions), collapse it
    (when (consp prompt) (setq prompt (concat (car prompt) "\n\n" (cdr prompt))))

    (prog1 (ai-workbench-request prompt
             :position position
             :in-place in-place
             :stream stream
             :system
             (if system-extra
                 (ai-workbench--merge-additional-directive system-extra)
               ai-workbench-system-prompt)
             :callback callback
             :transforms ai-workbench-prompt-transform-functions
             :fsm (ai-workbench-make-fsm :handlers ai-workbench-send--handlers)
             :dry-run dry-run)

      (unless dry-run
        (ai-workbench--update-status " Waiting..." 'warning))

      ;; NOTE: Possible future race condition here if Emacs ever drops the GIL.
      ;; The HTTP request callback might modify the buffer before the in-place
      ;; text is killed below.
      (when in-place
        (if (or buffer-read-only (get-char-property (point) 'read-only))
            (cond
             ((derived-mode-p 'vterm-mode)
              (require 'ai-workbench-integrations)
              (ai-workbench--vterm-delete))
             (t (message "Not replacing prompt: region is read-only")))
          (let ((beg (if (use-region-p)
                         (region-beginning)
                       (max (previous-single-property-change
                             (point) 'ai-workbench-engine nil (point-min))
                            (previous-single-property-change
                             (point) 'read-only nil (point-min)))))
                (end (if (use-region-p) (region-end) (point))))
            (unless redirect-output
              ;; store the killed text in ai-workbench-history
              (ai-workbench--attach-response-history
               (list (buffer-substring-no-properties beg end))))
            (kill-region beg end))))

      (when (and redirect-output ai-workbench-buffer-name)
        (message (concat "Prompt sent to buffer: "
                         (propertize ai-workbench-buffer-name 'face 'help-key-binding)))
        (display-buffer
         buffer '((display-buffer-reuse-window
                   display-buffer-pop-up-window)
                  (reusable-frames . visible)))))))

(defun ai-workbench--merge-additional-directive (additional &optional full)
  "Merge ADDITIONAL ai-workbench-engine directive with the full system message.

The ADDITIONAL directive is typically specified from `ai-workbench-menu'
and applies only to the next ai-workbench-engine request, see
`ai-workbench--infix-add-directive'.

FULL defaults to the active, full system message.  It may be a
string, a list of prompts or a function, see `ai-workbench-directives'
for details."
  (setq full (or full ai-workbench-system-prompt))
  (cl-typecase full
    (string (concat full "\n\n" additional))
    (cons (let ((copy (copy-sequence full)))
            (setcar copy (concat (car copy) "\n\n" additional))
            copy))
    (function (lambda () (ai-workbench--merge-additional-directive
                     additional (funcall full))))
    (otherwise additional)))

;; Allow calling from elisp
(put 'ai-workbench--suffix-send 'interactive-only nil)

;; ** Suffix to regenerate response

(defun ai-workbench--regenerate ()
  "Regenerate ai-workbench-engine response at point."
  (interactive)
  (when (ai-workbench--in-response-p)
    (pcase-let* ((`(,beg . ,end) (ai-workbench--get-response-bounds))
                 (history (get-char-property (point) 'ai-workbench-history))
                 (prev-responses (cons (buffer-substring-no-properties beg end)
                                       history)))
      (when ai-workbench-mode                  ;Remove prefix/suffix
        (save-excursion
          (goto-char beg)
          (when (looking-back (concat "\n+" (regexp-quote (ai-workbench-response-prefix-string)))
                              (point-min) 'greedy)
            (setq beg (match-beginning 0)))
          (goto-char end)
          (when (looking-at
                 (concat "\n+" (regexp-quote (ai-workbench-prompt-prefix-string))))
            (setq end (match-end 0)))))
      (delete-region beg end)
      (ai-workbench--attach-response-history prev-responses)
      (call-interactively #'ai-workbench--suffix-send))))

;; ** Set system message
(defun ai-workbench--read-crowdsourced-prompt ()
  "Pick a crowdsourced system prompt for ai-workbench-engine.

This uses the prompts in the variable
`ai-workbench--crowdsourced-prompts', which see."
  (interactive)
  (if (not (hash-table-empty-p (ai-workbench--crowdsourced-prompts)))
      (let ((choice
             (completing-read
              "Pick and edit prompt: "
              (lambda (str pred action)
                (if (eq action 'metadata)
                    `(metadata
                      ( affixation-function .
                        (lambda (cands)
                          (mapcar
                           (lambda (c)
                             ( list c ""
                               (concat
                                (propertize " " 'display '(space :align-to 22))
                                " " (propertize
                                     (ai-workbench--describe-directive
                                      (gethash c ai-workbench--crowdsourced-prompts)
                                      54 " ")
                                     'face 'completions-annotations))))
                           cands))))
                  (complete-with-action action ai-workbench--crowdsourced-prompts str pred)))
              nil t)))
        (when-let* ((prompt (gethash choice ai-workbench--crowdsourced-prompts)))
          (ai-workbench--set-with-scope
           'ai-workbench-system-prompt prompt ai-workbench--set-buffer-locally)
          (ai-workbench--edit-directive 'ai-workbench-system-prompt
            :callback (lambda (_) (call-interactively #'ai-workbench-menu)))))
    (message "No prompts available.")))

(transient-define-suffix ai-workbench--suffix-system-message (&optional cancel)
  "Edit LLM system message.

CANCEL is used to avoid touching dynamic system messages,
generated from functions."
  :transient 'transient--do-exit
  :description "Set or edit system message"
  :format " %k   %d"
  :key "s"
  (interactive
   (list (and (functionp ai-workbench-system-prompt)
              (not (y-or-n-p
                    "Active directive is dynamically generated: Edit its current value instead?")))))
  (if cancel (progn (message "Edit canceled")
                    (call-interactively #'ai-workbench-menu))
    (ai-workbench--edit-directive 'ai-workbench-system-prompt
      :setup #'activate-mark
      :callback (lambda (_) (call-interactively #'ai-workbench-menu)))))

;; MAYBE: Eventually can be simplified with string-edit, after we drop support
;; for Emacs 28.2.
(cl-defun ai-workbench--edit-directive (&optional sym &key prompt initial callback setup buffer)
  "Edit a ai-workbench-engine directive in a dedicated buffer.

Store the result in SYM, a symbol.  PROMPT and INITIAL are the heading
and initial text.  If SETUP is a function, run it after setting up the
buffer.  If CALLBACK is specified, it is run after exiting the edit.  It
is called with one argument: the buffer text or with nil depending on
whether the action is confirmed/cancelled."
  (declare (indent 1))
  (let ((orig-buf (or buffer (current-buffer)))
        (msg-start (make-marker))
        (directive (symbol-value sym)))
    (when (functionp directive)
      (setq directive (funcall directive)))
    ;; TODO: Handle editing list-of-strings directives
    (with-current-buffer (get-buffer-create "*ai-workbench-prompt*")
      (let ((inhibit-read-only t) (inhibit-message t))
        (erase-buffer)
        (text-mode)
        (visual-line-mode 1)
        (setq header-line-format
              (concat "Edit your instructions below and press "
                      (propertize "C-c C-c" 'face 'help-key-binding)
                      " when ready, or "
                      (propertize "C-c C-k" 'face 'help-key-binding)
                      " to abort."))
        (insert
         (or prompt
             (concat
              "# Example: You are a helpful assistant. Answer as concisely as possible.\n"
              "# Example: Reply only with shell commands and no prose.\n"
              "# Example: You are a poet. Reply only in verse."))
         "\n\n")
        (add-text-properties
         (point-min) (point)
         (list 'read-only t 'face 'font-lock-comment-face 'front-sticky t 'rear-nonsticky t))
        (set-marker msg-start (point))
        (save-excursion
          ;; If it's a list, insert only the system message part
          ;; If all is nil, insert "" at least
          (insert (or initial (car-safe (ai-workbench--parse-directive directive 'raw)) ""))
          (push-mark nil 'nomsg))
        (and (functionp setup) (funcall setup)))
      (display-buffer (current-buffer)
                      `((display-buffer-below-selected
                         display-buffer-use-some-window)
                        (some-window   . lru)
                        (body-function . ,#'select-window)
                        (window-height . ,#'fit-window-to-buffer)))
      (let ((quit-to-menu
             (lambda () "Cancel system message update and return."
               (quit-window)
               (unless (minibufferp)
                 (display-buffer orig-buf
                                 `((display-buffer-reuse-window
                                    display-buffer-use-some-window)
                                   (body-function . ,#'select-window)))))))
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c"
            (lambda () "Confirm system message and return."
              (interactive)
              (let ((system-message
                     (buffer-substring-no-properties msg-start (point-max))))
                (when sym
                  (with-current-buffer orig-buf
                    (ai-workbench--set-with-scope
                     sym (if (cdr-safe directive) ;Handle list of strings
                             (prog1 directive (setcar directive system-message))
                           system-message)
                     ai-workbench--set-buffer-locally)))
                (funcall quit-to-menu)
                (when (functionp callback) (funcall callback system-message))))
            "C-c C-k" (lambda () (interactive)
                        (funcall quit-to-menu)
                        (when (functionp callback) (funcall callback nil))))
          text-mode-map))))))

;; ** Suffix for displaying and removing context
(declare-function ai-workbench-context--buffer-setup "ai-workbench-context")
(declare-function ai-workbench-context--collect "ai-workbench-context")

(transient-define-suffix ai-workbench--suffix-context-buffer ()
  "Display all contexts from all buffers & files."
  :transient 'transient--do-exit
  :key " C"
  :if (lambda () ai-workbench-context)
  :description "Inspect context"
  (interactive)
  (ai-workbench-context--buffer-setup nil nil ai-workbench-context))

(provide 'ai-workbench-transient)
;;; ai-workbench-transient.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; eval: (outline-minor-mode 1)
;; End:

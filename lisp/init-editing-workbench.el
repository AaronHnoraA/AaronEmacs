;;; init-editing-workbench.el --- Focused editing utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Small editing commands adapted to this config's leader and lifecycle model.

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function better-jumper-set-jump "better-jumper" (&optional pos))
(declare-function copy-as-format "copy-as-format" ())
(declare-function ediff-buffers "ediff" (buffer-A buffer-B &optional startup-hooks job-name))
(declare-function ediff-copy-diff "ediff-util" (n from-to &optional batch-invert-regions-p
                                                   select prefer-to-copy))
(declare-function ediff-get-region-contents "ediff-util" (n buf-type ctl-buf &optional
                                                              region-bounds))
(declare-function ediff-regions-wordwise "ediff" (buffer-A buffer-B region-A region-B
                                                     &optional startup-hooks job-name))
(declare-function eros-eval-defun "eros" (&optional debug))
(declare-function eros-eval-last-sexp "eros" (&optional eval-last-sexp-arg-internal))
(declare-function LaTeX-narrow-to-environment "latex" (&optional count))
(declare-function org-narrow-to-block "org" ())
(declare-function org-narrow-to-subtree "org" ())
(declare-function relint-current-buffer "relint" ())
(declare-function string-inflection-cycle "string-inflection" ())

(defvar ediff-control-buffer)
(defvar ediff-current-difference)
(defvar ediff-mode-map)
(defvar ediff-after-quit-hook-internal)
(defvar copy-as-format-default)
(defvar kill-ring)

(defgroup my/editing-workbench nil
  "Focused editing and navigation utilities."
  :group 'editing)

(defvar my/last-change-marker nil
  "Marker for the most recent visible text edit in an eligible buffer.")

(defvar my/transpose-regions--buffer nil
  "Buffer holding the first region for `my/transpose-regions-dwim'.")

(defvar my/transpose-regions--start nil
  "Start marker for the first staged transpose region.")

(defvar my/transpose-regions--end nil
  "End marker for the first staged transpose region.")

(defvar my/transpose-regions--overlay nil
  "Overlay highlighting the first staged transpose region.")

(defun my/last-change--eligible-buffer-p ()
  "Return non-nil when the current buffer should record a text edit."
  (and (eq (current-buffer) (window-buffer (selected-window)))
       (not (minibufferp))
       (not buffer-read-only)
       (derived-mode-p 'prog-mode 'text-mode 'comint-mode)))

(defun my/last-change--record-h (&rest _ignored)
  "Record point after a visible text edit."
  (when (my/last-change--eligible-buffer-p)
    (when (markerp my/last-change-marker)
      (set-marker my/last-change-marker nil))
    (setq my/last-change-marker (copy-marker (point) t))))

(defun my/last-change-clear ()
  "Release the recorded last-change marker."
  (when (markerp my/last-change-marker)
    (set-marker my/last-change-marker nil))
  (setq my/last-change-marker nil))

(define-minor-mode my/last-change-mode
  "Track the last visible text change across eligible buffers."
  :global t
  :init-value t
  :lighter nil
  :group 'my/editing-workbench
  (if my/last-change-mode
      (add-hook 'after-change-functions #'my/last-change--record-h)
    (remove-hook 'after-change-functions #'my/last-change--record-h)
    (my/last-change-clear)))

(defun my/last-change-jump ()
  "Jump to the most recent visible text change across buffers."
  (interactive)
  (unless (and (markerp my/last-change-marker)
               (marker-buffer my/last-change-marker))
    (my/last-change-clear)
    (user-error "No live text change has been recorded"))
  (when (fboundp 'better-jumper-set-jump)
    (better-jumper-set-jump))
  (let ((marker my/last-change-marker))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point)))))

(defun my/narrow-or-widen-dwim (force-narrow)
  "Widen, or narrow to the region, Org block/subtree, environment, or defun.
With FORCE-NARROW, do not widen an already narrowed buffer."
  (interactive "P")
  (cond
   ((and (buffer-narrowed-p) (not force-narrow))
    (widen))
   ((use-region-p)
    (narrow-to-region (region-beginning) (region-end)))
   ((derived-mode-p 'org-mode)
    (require 'org)
    (let ((block
           (seq-find
            (lambda (element)
              (string-match-p "block" (symbol-name (org-element-type element))))
            (org-element-lineage (org-element-context) nil t))))
      (if block
          (save-excursion
            (goto-char (org-element-property :begin block))
            (org-narrow-to-block))
        (org-narrow-to-subtree))))
   ((and (derived-mode-p 'latex-mode 'LaTeX-mode)
         (fboundp 'LaTeX-narrow-to-environment))
    (LaTeX-narrow-to-environment))
   (t
    (narrow-to-defun))))

(defalias 'narrow-or-widen #'my/narrow-or-widen-dwim)

(defun my/transpose-regions-reset ()
  "Cancel and release a staged region transpose."
  (interactive)
  (when (overlayp my/transpose-regions--overlay)
    (delete-overlay my/transpose-regions--overlay))
  (dolist (marker (list my/transpose-regions--start my/transpose-regions--end))
    (when (markerp marker)
      (set-marker marker nil)))
  (when (buffer-live-p my/transpose-regions--buffer)
    (with-current-buffer my/transpose-regions--buffer
      (remove-hook 'kill-buffer-hook #'my/transpose-regions-reset t)))
  (setq my/transpose-regions--buffer nil
        my/transpose-regions--start nil
        my/transpose-regions--end nil
        my/transpose-regions--overlay nil))

(defun my/transpose-regions--stage (beg end)
  "Stage the region from BEG to END for a later transpose."
  (my/transpose-regions-reset)
  (setq my/transpose-regions--buffer (current-buffer)
        my/transpose-regions--start (copy-marker beg)
        my/transpose-regions--end (copy-marker end t)
        my/transpose-regions--overlay (make-overlay beg end nil nil t))
  (overlay-put my/transpose-regions--overlay 'face 'region)
  (add-hook 'kill-buffer-hook #'my/transpose-regions-reset nil t)
  (deactivate-mark)
  (message "First region staged; select the second region and repeat"))

(defun my/transpose-regions-dwim (cancel)
  "Stage or transpose two non-overlapping regions in one buffer.
With CANCEL, discard the staged region."
  (interactive "P")
  (cond
   (cancel
    (my/transpose-regions-reset)
    (message "Staged region cleared"))
   ((not (use-region-p))
    (user-error "Select a region first"))
   ((not (and (buffer-live-p my/transpose-regions--buffer)
              (markerp my/transpose-regions--start)
              (marker-buffer my/transpose-regions--start)))
    (my/transpose-regions--stage (region-beginning) (region-end)))
   ((not (eq (current-buffer) my/transpose-regions--buffer))
    (user-error "The second region must be in %s"
                (buffer-name my/transpose-regions--buffer)))
   (t
    (let* ((a-beg (marker-position my/transpose-regions--start))
           (a-end (marker-position my/transpose-regions--end))
           (b-beg (region-beginning))
           (b-end (region-end))
           (ordered (if (< a-beg b-beg)
                        (list a-beg a-end b-beg b-end)
                      (list b-beg b-end a-beg a-end))))
      (when (> (nth 1 ordered) (nth 2 ordered))
        (user-error "The regions overlap"))
      (transpose-regions (nth 0 ordered) (nth 1 ordered)
                         (nth 2 ordered) (nth 3 ordered))
      (my/transpose-regions-reset)
      (deactivate-mark)
      (message "Regions transposed")))))

(defun my/hash-dwim (algorithm beg end)
  "Hash BEG through END with ALGORITHM and copy the digest."
  (interactive
   (list (intern (completing-read "Hash: "
                                  '(sha256 sha512 sha384 sha224 sha1 md5)
                                  nil t nil nil "sha256"))
         (if (use-region-p) (region-beginning) (point-min))
         (if (use-region-p) (region-end) (point-max))))
  (let ((digest (secure-hash algorithm (current-buffer) beg end)))
    (kill-new digest)
    (message "%s copied: %s" algorithm digest)
    digest))

(defun my/ediff-last-two-yanks ()
  "Compare the two newest kill-ring entries with Ediff."
  (interactive)
  (unless (and (consp kill-ring) (cdr kill-ring))
    (user-error "The kill ring needs at least two entries"))
  (require 'ediff)
  (let ((a (generate-new-buffer "*yank A*"))
        (b (generate-new-buffer "*yank B*"))
        control)
    (cl-labels ((cleanup ()
                  (when (buffer-live-p control)
                    (with-current-buffer control
                      (remove-hook 'ediff-cleanup-hook #'cleanup t)
                      (remove-hook 'kill-buffer-hook #'cleanup t)))
                  (when (buffer-live-p a) (kill-buffer a))
                  (when (buffer-live-p b) (kill-buffer b))))
      (condition-case err
          (progn
            (with-current-buffer a (insert (nth 0 kill-ring)))
            (with-current-buffer b (insert (nth 1 kill-ring)))
            (ediff-buffers
             a b
             (list (lambda ()
                     (setq control (current-buffer))
                     (add-hook 'ediff-cleanup-hook #'cleanup nil t)
                     (add-hook 'kill-buffer-hook #'cleanup nil t)))))
        (error
         (cleanup)
         (signal (car err) (cdr err)))))))

(defun my/ediff--ordinary-windows ()
  "Return live, non-minibuffer, non-dedicated windows on the frame."
  (seq-filter (lambda (window)
                (and (window-live-p window)
                     (not (window-minibuffer-p window))
                     (not (window-dedicated-p window))))
              (window-list)))

(defun my/ediff--restore-frame-windows (frame control)
  "Restore FRAME's saved layout after its last Ediff CONTROL quits."
  (when (frame-live-p frame)
    (let* ((state (frame-parameter frame 'my/ediff-window-state))
           (sessions (plist-get state :sessions)))
      (when (memq control sessions)
        (let ((remaining
               (seq-filter (lambda (session)
                             (and (not (eq session control))
                                  (buffer-live-p session)))
                           sessions)))
          (if remaining
              (set-frame-parameter frame 'my/ediff-window-state
                                   (plist-put state :sessions remaining))
            (set-frame-parameter frame 'my/ediff-window-state nil)
            (when-let* ((layout (plist-get state :layout))
                        ((window-configuration-p layout)))
              (set-window-configuration layout t))))))))

(defun my/ediff--setup-windows-a (original &rest args)
  "Track each Ediff setup in its originating frame around ORIGINAL."
  (let* ((frame (selected-frame))
         (state (frame-parameter frame 'my/ediff-window-state))
         (sessions (seq-filter #'buffer-live-p (plist-get state :sessions)))
         (before (current-window-configuration))
         (layout (if sessions
                     (plist-get state :layout)
                   before))
         control completed)
    (unwind-protect
        (progn
          (setq control (apply original args))
          (when (and (frame-live-p frame) (buffer-live-p control))
            (set-frame-parameter frame 'my/ediff-window-state
                                 (list :layout layout
                                       :sessions (cons control sessions)))
            (with-current-buffer control
        (let ((restore (lambda ()
                         (my/ediff--restore-frame-windows frame control))))
          (add-hook 'ediff-after-quit-hook-internal restore nil t)
          ;; Normal Ediff quit kills the control before its after-quit hooks;
          ;; defer this fallback so Ediff can finish rearranging windows.
          (add-hook 'kill-buffer-hook
                    (lambda () (run-at-time 0 nil restore)) nil t))))
          (setq completed t)
          control)
      (unless completed
        (when (frame-live-p frame)
          (set-frame-parameter frame 'my/ediff-window-state state)
          (set-window-configuration before t))))))

(defun my/ediff-dwim ()
  "Choose an Ediff operation from the current editing context."
  (interactive)
  (require 'ediff)
  (cond
   ((use-region-p)
    (call-interactively #'ediff-regions-wordwise))
   ((= (length (my/ediff--ordinary-windows)) 2)
    (let* ((windows (my/ediff--ordinary-windows))
           (a (window-buffer (nth 0 windows)))
           (b (window-buffer (nth 1 windows))))
      (when (eq a b)
        (user-error "The two windows show the same buffer"))
      (ediff-buffers a b)))
   ((and buffer-file-name (vc-registered buffer-file-name))
    (call-interactively #'vc-ediff))
   (t
    (call-interactively #'ediff-buffers))))

(defun my/ediff-copy-both-to-C ()
  "Copy the current A difference followed by B into Ediff buffer C."
  (interactive)
  (unless (and (boundp 'ediff-current-difference)
               (integerp ediff-current-difference)
               (buffer-live-p ediff-control-buffer))
    (user-error "No active Ediff difference"))
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun my/copy-as-format-region-or-buffer (format)
  "Copy the active region or whole buffer using native FORMAT."
  (require 'copy-as-format)
  (let ((copy-as-format-default format)
        (current-prefix-arg nil)
        (deactivate-mark nil))
    (save-mark-and-excursion
      (if (use-region-p)
          (copy-as-format)
        (let ((transient-mark-mode t))
          (goto-char (point-min))
          (push-mark (point-max) t t)
          (copy-as-format))))))

(defmacro my/copy-as-format-define (name format)
  "Define NAME as a region-or-buffer wrapper for FORMAT."
  `(defun ,name ()
     (interactive)
     (my/copy-as-format-region-or-buffer ,format)))

(my/copy-as-format-define my/copy-as-format-markdown "markdown")
(my/copy-as-format-define my/copy-as-format-github "github")
(my/copy-as-format-define my/copy-as-format-org "org-mode")
(my/copy-as-format-define my/copy-as-format-html "html")
(my/copy-as-format-define my/copy-as-format-slack "slack")
(my/copy-as-format-define my/copy-as-format-rst "rst")
(my/copy-as-format-define my/copy-as-format-asciidoc "asciidoc")

(transient-define-prefix copy-as-format-menu ()
  "Copy the active region or buffer in a presentation format."
  [["Copy as"
    ("m" "Markdown" my/copy-as-format-markdown)
    ("g" "GitHub" my/copy-as-format-github)
    ("o" "Org" my/copy-as-format-org)
    ("h" "HTML" my/copy-as-format-html)]
   ["More"
    ("s" "Slack" my/copy-as-format-slack)
    ("r" "reStructuredText" my/copy-as-format-rst)
    ("a" "AsciiDoc" my/copy-as-format-asciidoc)]])

(transient-define-prefix editing-menu ()
  "Focused editing operations."
  [["Transform"
    ("i" "cycle identifier style" string-inflection-cycle)
    ("t" "transpose regions" my/transpose-regions-dwim)
    ("T" "clear staged region" my/transpose-regions-reset)]
   ["Copy / inspect"
    ("c" "copy as format" copy-as-format-menu)
    ("h" "hash region/buffer" my/hash-dwim)
    ("y" "diff last two yanks" my/ediff-last-two-yanks)]
   ["Compare / image"
    ("d" "Ediff DWIM" my/ediff-dwim)
    ("p" "crop image" image-crop)]])

(use-package string-inflection
  :ensure t
  :commands string-inflection-cycle)

(use-package copy-as-format
  :ensure t
  :commands (copy-as-format copy-as-format-markdown copy-as-format-github
             copy-as-format-org-mode copy-as-format-html
             copy-as-format-slack copy-as-format-rst
             copy-as-format-asciidoc))

(use-package eros
  :ensure t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eros-mode)
  :commands (eros-eval-defun eros-eval-last-sexp))

(use-package relint
  :ensure t
  :commands relint-current-buffer)

(defun my/ediff-keymap-setup-h ()
  "Install the copy-both command in the current Ediff control keymap."
  (define-key ediff-mode-map (kbd "C-c C-b") #'my/ediff-copy-both-to-C))

(with-eval-after-load 'ediff-util
  (add-hook 'ediff-keymap-setup-hook #'my/ediff-keymap-setup-h))

(use-package ediff
  :ensure nil
  :config
  (advice-add 'ediff-setup :around #'my/ediff--setup-windows-a)
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(with-eval-after-load 'elisp-mode
  (my/local-leader!
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "i" 'info-lookup-symbol
    "ed" 'eros-eval-defun
    "ee" 'eros-eval-last-sexp
    "el" 'load-library
    "er" 'relint-current-buffer
    "gf" 'find-function
    "gv" 'find-variable
    "gl" 'find-library))

(my/last-change-mode 1)
(add-hook 'kill-emacs-hook #'my/last-change-clear)

(provide 'init-editing-workbench)
;;; init-editing-workbench.el ends here

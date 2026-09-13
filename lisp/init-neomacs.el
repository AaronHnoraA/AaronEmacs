;;; init-neomacs.el --- Neomacs runtime compatibility -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Neomacs is a Rust rewrite that tracks the GNU Emacs 31.1 Lisp layer but not
;; every C/Objective-C runtime detail.  This module is the single boundary that
;; keeps the user-visible behaviour of this configuration identical on both
;; builds, so no other module has to branch on the host build.
;;
;; Three differences change daily feel on macOS:
;;
;; 1. Physical modifiers.  GNU Emacs honours `mac-option-modifier' and
;;    `mac-command-modifier', which `init-macos.el' sets to `hyper' and `meta'.
;;    Neomacs has no such variables: its input bridge hard-codes Option to Meta
;;    and Command to Super, and its event protocol carries no Hyper bit at all
;;    (shift/ctrl/meta/super only).  The whole `H-' layer in `init-macos.el' and
;;    the `H-C-M-' mouse layer in `init-mouse.el' would be unreachable.  We
;;    restore the same feel by renaming modifiers in `key-translation-map'.
;;
;; 2. Fringe frame parameters.  `init-ui.el' calls `set-fringe-mode' with a nil
;;    width, meaning "default".  GNU Emacs still reports the resolved width from
;;    `frame-parameter'; Neomacs returns nil while `window-fringes' reports the
;;    real width.  `diff-hl-define-bitmaps' then evaluates `(min nil 16)' and
;;    drops into the debugger whenever a file with VC state is opened.
;;
;; 3. Startup frame lifetime.  Neomacs already owns a live graphic frame during
;;    early init, so the pre-hide in `early-init.el' - inert on GNU Emacs, which
;;    has no graphic display that early - could unmap the real window.  The
;;    guard lives in `early-init.el'; `my/neomacs-reveal-frames' is the net.
;;
;; Everything here is a no-op on GNU Emacs.

;;; Code:

(require 'config)

(defconst my/neomacs-p (fboundp 'neomacs-core-backend)
  "Non-nil when this Emacs is Neomacs rather than a GNU Emacs build.")

(defgroup my/neomacs nil
  "Compatibility layer for the Neomacs runtime."
  :group 'environment)

;;;; Modifier renaming -------------------------------------------------------

(defconst my/neomacs-default-modifier-remap '((meta . hyper) (super . meta))
  "Modifier renaming that reproduces this configuration's macOS layout.
Neomacs delivers physical Option as Meta and physical Command as Super.
`init-macos.el' is written for Option as Hyper and Command as Meta, so both
names shift by one.  Renaming is simultaneous: an incoming `C-M-s-a' becomes
`C-H-M-a', which is what the same physical keys produce under GNU Emacs.")

(config-defvar my/neomacs-modifier-remap nil
  "Modifier renaming applied to Neomacs input, as an alist of (FROM . TO).
nil means automatic: use `my/neomacs-default-modifier-remap' on Neomacs GUI
frames running macOS, and nothing anywhere else.  The symbol `none' disables
renaming and leaves Neomacs' own Option-as-Meta layout in place."
  :type 'sexp
  :group 'my/neomacs
  :on-change #'my/neomacs-apply-modifier-remap)

(defconst my/neomacs--remap-base-chars (number-sequence ?\s ?~)
  "Printable ASCII base events covered by modifier renaming.")

(defconst my/neomacs--remap-base-keys
  (append '(tab backtab return escape backspace delete
            up down left right home end prior next insert
            begin print pause menu help
            kp-add kp-subtract kp-multiply kp-divide kp-decimal kp-enter
            kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9)
          (mapcar (lambda (i) (intern (format "f%d" i))) (number-sequence 1 24)))
  "Symbolic key base events covered by modifier renaming.")

(defconst my/neomacs--remap-base-mouse
  (append (let (events)
            (dolist (button '(1 2 3 4 5))
              (dolist (shape '("mouse-%d" "down-mouse-%d" "drag-mouse-%d"
                               "double-mouse-%d" "triple-mouse-%d"))
                (push (intern (format shape button)) events)))
            (nreverse events))
          '(wheel-up wheel-down wheel-left wheel-right))
  "Mouse base events covered by modifier renaming.
Pointing devices that emit modified clicks go through the same rename as the
keyboard, so `init-mouse.el' keeps working unchanged.")

(defvar my/neomacs--translation-map nil
  "Keymap holding the installed modifier renames, or nil when not installed.
It is spliced in as the parent of `key-translation-map' so the user's own
entries keep priority and removal is a single `set-keymap-parent'.")

(defun my/neomacs--effective-modifier-remap ()
  "Return the modifier rename table to install, or nil for none."
  (cond
   ((eq my/neomacs-modifier-remap 'none) nil)
   ((consp my/neomacs-modifier-remap) my/neomacs-modifier-remap)
   ((and my/neomacs-p (eq system-type 'darwin))
    my/neomacs-default-modifier-remap)))

(defun my/neomacs--modifier-subsets (modifiers)
  "Return every non-empty subset of MODIFIERS, as lists."
  (let ((subsets '(())))
    (dolist (modifier modifiers)
      (setq subsets (append subsets
                            (mapcar (lambda (subset) (append subset (list modifier)))
                                    subsets))))
    (delq nil subsets)))

(defun my/neomacs--remap-entries (remap)
  "Return (SOURCE-KEY . TARGET-KEY) pairs renaming modifiers per REMAP.
Both are one-event key vectors.  Pairs that would translate an event to
itself, or that the running build cannot represent, are dropped."
  (let ((subsets (my/neomacs--modifier-subsets (mapcar #'car remap)))
        (entries nil))
    (dolist (base (append my/neomacs--remap-base-chars
                          my/neomacs--remap-base-keys
                          my/neomacs--remap-base-mouse))
      ;; Shift is folded into the character itself, so only symbolic and mouse
      ;; events need an explicit shift variant.
      (let ((extras (if (integerp base)
                        '(() (control))
                      '(() (control) (shift) (control shift)))))
        (dolist (subset subsets)
          (dolist (extra extras)
            (let ((source (ignore-errors
                            (event-convert-list
                             (append subset extra (list base)))))
                  (target (ignore-errors
                            (event-convert-list
                             (append (mapcar (lambda (modifier)
                                               (alist-get modifier remap modifier))
                                             subset)
                                     extra (list base))))))
              (when (and source target (not (equal source target)))
                (push (cons (vector source) (vector target)) entries)))))))
    (nreverse entries)))

(defun my/neomacs--translation-for (target)
  "Return a `key-translation-map' binding producing TARGET on graphic frames.
Terminal frames are left alone: a TTY has no Command key, so renaming Meta
there would only take Meta away."
  (lambda (_prompt) (and (display-graphic-p) target)))

(defun my/neomacs-remove-modifier-remap ()
  "Remove the installed modifier renaming from `key-translation-map'."
  (interactive)
  (when my/neomacs--translation-map
    (when (eq (keymap-parent key-translation-map) my/neomacs--translation-map)
      (set-keymap-parent key-translation-map
                         (keymap-parent my/neomacs--translation-map)))
    (setq my/neomacs--translation-map nil)))

(defun my/neomacs-apply-modifier-remap ()
  "Install the modifier renaming described by `my/neomacs-modifier-remap'."
  (interactive)
  (my/neomacs-remove-modifier-remap)
  (when-let* ((remap (my/neomacs--effective-modifier-remap)))
    ;; Build the alist by hand instead of using `define-key'.  `define-key'
    ;; rewrites a meta *character* into an `ESC' prefix sequence, but the
    ;; translation step of `read-key-sequence' looks the incoming event up
    ;; verbatim, so a rewritten entry is never consulted and every `M-<char>'
    ;; would stay untranslated.  Symbolic and mouse events are unaffected;
    ;; storing all of them literally keeps one code path.
    (let ((bindings nil))
      (dolist (entry (my/neomacs--remap-entries remap))
        (push (cons (aref (car entry) 0)
                    (my/neomacs--translation-for (cdr entry)))
              bindings))
      (let ((map (cons 'keymap (nreverse bindings))))
        (set-keymap-parent map (keymap-parent key-translation-map))
        (set-keymap-parent key-translation-map map)
        (setq my/neomacs--translation-map map)))))

;;;; Fringe frame parameters -------------------------------------------------

(defconst my/neomacs--fallback-fringe-width 8
  "Fringe width to assume when the frame reports none.
This is the GNU Emacs default that `set-fringe-mode' with a nil width means.")

(defun my/neomacs-repair-fringe-parameters (&optional frame)
  "Give FRAME numeric `left-fringe' and `right-fringe' parameters.
Neomacs leaves them nil when `set-fringe-mode' asked for the default width,
which breaks every consumer that treats them as numbers, `diff-hl' first."
  (let ((frame (or frame (selected-frame))))
    (when (and my/neomacs-p (frame-live-p frame) (display-graphic-p frame))
      (let* ((window (frame-selected-window frame))
             (fringes (and (window-live-p window) (window-fringes window))))
        (dolist (side '(left-fringe right-fringe))
          (unless (integerp (frame-parameter frame side))
            (set-frame-parameter
             frame side
             (or (nth (if (eq side 'left-fringe) 0 1) fringes)
                 my/neomacs--fallback-fringe-width))))))))

;;;; Startup frame visibility ------------------------------------------------

(defvar my/neomacs--reveal-timer nil
  "One-shot timer backing `my/neomacs-reveal-frames', or nil.")

(defun my/neomacs--cancel-reveal-timer ()
  "Cancel the pending startup reveal timer."
  (when (timerp my/neomacs--reveal-timer)
    (cancel-timer my/neomacs--reveal-timer))
  (setq my/neomacs--reveal-timer nil))

(defun my/neomacs-reveal-frames ()
  "Make every invisible graphic frame visible again.
`early-init.el' can pre-hide the first frame; on Neomacs the render thread
may create that frame before the reveal runs, so re-assert it here."
  (interactive)
  (my/neomacs--cancel-reveal-timer)
  (dolist (frame (frame-list))
    (when (and (frame-live-p frame)
               (display-graphic-p frame)
               (not (frame-parameter frame 'visibility)))
      (set-frame-parameter frame 'my-startup-hidden nil)
      (make-frame-visible frame)
      (redraw-frame frame))))

(defun my/neomacs--schedule-reveal ()
  "Re-check frame visibility shortly after startup settles."
  (my/neomacs--cancel-reveal-timer)
  (setq my/neomacs--reveal-timer
        (run-at-time 1 nil #'my/neomacs-reveal-frames)))

;;;; Missing xwidget-webkit primitives ---------------------------------------

(defvar my/neomacs--xwidget-cookie-warned nil
  "Non-nil once the missing cookie-storage primitive has been reported.")

(defun my/neomacs--xwidget-webkit-set-cookie-storage-file (&rest _args)
  "Stand in for the webkit cookie-storage primitive Neomacs does not provide.
Neomacs ships GNU Emacs' `xwidget.el' verbatim, and
`xwidget-webkit--create-new-session-buffer' calls this primitive
unconditionally whenever `xwidget-webkit-cookie-file' is set.  The Rust
webkit backend never defined it, so every new webkit session - the browser,
Noema, the Lean infoview - died with a void-function error.

There is no Lisp-level substitute: cookies simply are not persisted to the
configured file on this build.  Report that once and let the session open."
  (unless my/neomacs--xwidget-cookie-warned
    (setq my/neomacs--xwidget-cookie-warned t)
    (message "Neomacs: webkit cookie storage is unavailable; %s is ignored"
             (or (bound-and-true-p xwidget-webkit-cookie-file) "the cookie file")))
  nil)

(defun my/neomacs-install-xwidget-stubs ()
  "Define the webkit primitives Neomacs lacks but its `xwidget.el' calls.
Each is defined only when absent, so a future Neomacs that implements one
keeps its own."
  (unless (fboundp 'xwidget-webkit-set-cookie-storage-file)
    (defalias 'xwidget-webkit-set-cookie-storage-file
      #'my/neomacs--xwidget-webkit-set-cookie-storage-file)))

;;;; Installation ------------------------------------------------------------

(defun my/neomacs-repair-all-fringe-parameters ()
  "Repair fringe parameters on every live graphic frame."
  (mapc #'my/neomacs-repair-fringe-parameters (frame-list)))

(when my/neomacs-p
  (my/neomacs-apply-modifier-remap)
  (my/neomacs-install-xwidget-stubs)

  (my/neomacs-repair-fringe-parameters)
  (add-hook 'after-make-frame-functions #'my/neomacs-repair-fringe-parameters)
  ;; `init-ui.el' loads later and calls `set-fringe-mode' with a nil width,
  ;; which clears the parameters again on every existing frame, so repair once
  ;; more after the module graph has finished loading.
  (add-hook 'window-setup-hook #'my/neomacs-repair-all-fringe-parameters 101)
  (add-hook 'emacs-startup-hook #'my/neomacs-repair-all-fringe-parameters 101)

  (add-hook 'window-setup-hook #'my/neomacs-reveal-frames 101)
  (add-hook 'emacs-startup-hook #'my/neomacs--schedule-reveal 101)
  (add-hook 'kill-emacs-hook #'my/neomacs--cancel-reveal-timer))

(provide 'init-neomacs)
;;; init-neomacs.el ends here

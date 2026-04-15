;;; init-ratex.el --- RaTeX popup preview integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Use ratex.el as an edit-time popup preview backend for Org math fragments.
;; Keep the existing Org/XeLaTeX preview pipeline intact and only add RaTeX's
;; edit-time popup, so Org overlays and RaTeX can coexist.

;;; Code:

(require 'cl-lib)

(declare-function ratex-mode                 "ratex"        (&optional arg))
(declare-function ratex-handle-buffer-switch "ratex-render")
(declare-function ratex-handle-post-command  "ratex-render")
(declare-function ratex--hide-edit-preview   "ratex-render")
(declare-function evil-insert-state-p        "evil")

(defvar my/org-latex--allow-native-preview)
(defvar ratex-auto-download-backend)
(defvar ratex-backend-root)
(defvar ratex-edit-preview)
(defvar ratex-debug)
(defvar ratex-font-size)
(defvar ratex-font-dir)
(defvar ratex-hide-source-while-preview)
(defvar ratex-inline-preview)
(defvar ratex-posframe-background-color)
(defvar ratex-posframe-border-color)
(defvar ratex-posframe-poshandler)           ; not forward-declared by ratex-render
(defvar ratex-render-cache-limit)
(defvar ratex-render-cache-ttl)
(defvar ratex-render-color)
(defvar-local ratex-mode)
(defvar-local ratex--active-fragment)
(defvar-local my/ratex-preview-timer      nil)
(defvar-local my/ratex--initialized      nil)
(defvar-local my/ratex--tracking-enabled nil)
(defvar-local my/ratex--last-point       nil
  "Point position recorded when the last preview timer was scheduled.")
(defvar-local my/ratex--last-tick        nil
  "Buffer modification tick recorded when the last preview timer was scheduled.")

(defcustom my/ratex-evil-insert-only t
  "When non-nil, only show RaTeX edit previews in Evil insert state."
  :type 'boolean)

(defcustom my/ratex-preview-style 'posframe
  "Preview UI used for RaTeX edit previews in conservative mode.
\\='minibuffer shows the rendered image in the echo area without moving buffer text.
\\='window shows the rendered image in a popup side window.
\\='overlay shows the rendered image above the formula using a buffer overlay.
\\='posframe uses a floating child frame.
nil disables the edit-time popup."
  :type '(choice (const :tag "Posframe"               posframe)
                 (const :tag "Minibuffer / echo area"   minibuffer)
                 (const :tag "Popup side window"        window)
                 (const :tag "Overlay above formula"    overlay)
                 (const :tag "Disable"                nil)))

(defcustom my/ratex-preview-idle-delay 0.15
  "Seconds of idle time before running RaTeX preview updates."
  :type 'number)

(defcustom my/ratex-math-scan-lines 4
  "Lines above/below point to scan for math delimiters before scheduling preview.
A value of 4 catches most display-math blocks while keeping the scan O(8 lines)
rather than O(full org-element parse)."
  :type 'natnum)

(defconst my/ratex--math-delimiter-re
  "\\$\\|\\\\(\\|\\\\\\[\\|\\\\begin{"
  "Regexp matching LaTeX math delimiters ($, \\(, \\[, \\begin{).")

(defconst my/ratex-root
  (expand-file-name "site-lisp/ratex.el" user-emacs-directory)
  "Local checkout root for ratex.el.")

(defconst my/ratex-font-dir
  (expand-file-name "vendor/ratex-core/fonts" my/ratex-root)
  "Font directory used by the RaTeX backend.")

(defun my/org-ratex-available-p ()
  "Return non-nil when the local ratex.el checkout is available."
  (file-directory-p (expand-file-name "lisp" my/ratex-root)))

(defun my/ratex-supported-buffer-p ()
  "Return non-nil when the current buffer should enable RaTeX previews."
  (derived-mode-p 'org-mode
                  'latex-mode
                  'LaTeX-mode
                  'tex-mode
                  'TeX-mode
                  'plain-tex-mode
                  'plain-TeX-mode
                  'docTeX-mode))

(defun my/org-ratex-preview-active-p ()
  "Return non-nil when RaTeX edit preview should be active now."
  (or (not my/ratex-evil-insert-only)
      (not (bound-and-true-p evil-local-mode))
      (evil-insert-state-p)))

(defun my/ratex-buffer-visible-p (&optional buffer)
  "Return non-nil when BUFFER is currently shown in a live window."
  (get-buffer-window (or buffer (current-buffer)) t))

(defun my/ratex-near-math-p ()
  "Return non-nil if point is plausibly near a LaTeX math fragment.
Scans `my/ratex-math-scan-lines' lines above and below point for any of
`my/ratex--math-delimiter-re'.  This O(N-lines) check prevents the idle timer
— and therefore ratex's org-element-context call — from being scheduled when
the cursor is clearly outside any math environment."
  (let ((lo (save-excursion
              (forward-line (- my/ratex-math-scan-lines))
              (line-beginning-position)))
        (hi (save-excursion
              (forward-line my/ratex-math-scan-lines)
              (line-end-position))))
    (save-excursion
      (goto-char lo)
      (re-search-forward my/ratex--math-delimiter-re hi t))))

(defun my/ratex--debounced-post-command (orig-fn &rest args)
  "Run `ratex-handle-post-command' through a short idle debounce.

Guards evaluated cheapest-first:
  1. Mode / buffer / visibility / Evil-state sanity.
  2. `my/ratex-near-math-p' regexp scan — skip when no math delimiter is
     nearby, avoiding a ratex org-element-context call entirely.
  3. Point + buffer-tick dedup — if neither cursor position nor buffer
     content changed since the last schedule, keep any pending timer alive
     instead of canceling + rescheduling it.

When all guards pass and state has changed: cancel old timer, reschedule."
  (if (not (and (my/ratex-supported-buffer-p)
                (bound-and-true-p ratex-mode)
                (my/ratex-buffer-visible-p)
                (my/org-ratex-preview-active-p)
                (my/ratex-near-math-p)))
      ;; Any guard failed → cancel and hide immediately.
      (progn
        (my/ratex-cancel-pending-preview)
        (my/ratex-hide-preview-now))
    ;; All guards pass.  Only reschedule when something actually changed.
    (let ((pt   (point))
          (tick (buffer-chars-modified-tick)))
      (if (and (eql pt   my/ratex--last-point)
               (eql tick my/ratex--last-tick))
          ;; Nothing changed — preserve any in-flight timer; don't thrash.
          nil
        (my/ratex-cancel-pending-preview)
        (setq-local my/ratex--last-point pt
                    my/ratex--last-tick  tick)
        (setq-local
         my/ratex-preview-timer
         (run-with-idle-timer
          my/ratex-preview-idle-delay nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq my/ratex-preview-timer nil)
                (when (and (my/ratex-supported-buffer-p)
                           (bound-and-true-p ratex-mode)
                           (my/ratex-buffer-visible-p)
                           (my/org-ratex-preview-active-p))
                  (condition-case err
                      (apply orig-fn args)
                    (error
                     (message "[ratex] preview error: %S" err)
                     (my/ratex-hide-preview-now)))))))
          (current-buffer)))))))

(defun my/ratex-enable-post-command-tracking ()
  "Enable RaTeX post-command tracking in the current buffer."
  (when (and (bound-and-true-p ratex-mode)
             (not my/ratex--tracking-enabled))
    (setq-local my/ratex--tracking-enabled t)
    (add-hook 'post-command-hook #'ratex-handle-post-command nil t)))

(defun my/ratex-disable-post-command-tracking ()
  "Disable RaTeX post-command tracking in the current buffer."
  (when my/ratex--tracking-enabled
    (setq-local my/ratex--tracking-enabled nil)
    (remove-hook 'post-command-hook #'ratex-handle-post-command t)))

(defun my/ratex-refresh-post-command-soon ()
  "Queue one near-immediate RaTeX post-command refresh for the current buffer."
  (let ((buffer (current-buffer)))
    (run-with-idle-timer
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (bound-and-true-p ratex-mode)
                      my/ratex--tracking-enabled
                      (my/org-ratex-preview-active-p)
                      (fboundp 'ratex-handle-post-command))
             (ratex-handle-post-command))))))))

(defun my/ratex-cancel-pending-preview ()
  "Cancel any queued RaTeX preview timer in the current buffer."
  (when (timerp my/ratex-preview-timer)
    (cancel-timer my/ratex-preview-timer)
    (setq-local my/ratex-preview-timer nil)))

(defun my/ratex-hide-preview-now ()
  "Cancel pending preview work and hide any visible RaTeX preview."
  (my/ratex-cancel-pending-preview)
  ;; Reset dedup state so the next cursor arrival always reschedules.
  (setq-local ratex--active-fragment nil
              my/ratex--last-point   nil
              my/ratex--last-tick    nil)
  (when (fboundp 'ratex--hide-edit-preview)
    (ratex--hide-edit-preview)))

(defun my/ratex-handle-buffer-switch (orig-fn &rest args)
  "Clear pending/visible RaTeX previews before delegating to ORIG-FN."
  (when (bound-and-true-p ratex-mode)
    (my/ratex-hide-preview-now))
  (apply orig-fn args))

(defun my/org-ratex-sync-evil-state ()
  "Enable RaTeX edit preview only when the current Evil state should show it."
  (when (and (my/ratex-supported-buffer-p)
             (bound-and-true-p ratex-mode))
    (if (my/org-ratex-preview-active-p)
        (progn
          (setq-local ratex-edit-preview my/ratex-preview-style)
          (my/ratex-enable-post-command-tracking)
          (my/ratex-refresh-post-command-soon))
      (setq-local ratex-edit-preview nil)
      (my/ratex-disable-post-command-tracking)
      (my/ratex-hide-preview-now))))

(defun my/org-ratex-enable ()
  "Enable popup-only RaTeX previews alongside Org previews."
  (when (and (my/org-ratex-available-p)
             (display-graphic-p)
             (my/ratex-supported-buffer-p))
    ;; Keep Org's async preview stack active; RaTeX only adds the edit popup.
    (when (derived-mode-p 'org-mode)
      (setq-local my/org-latex--allow-native-preview nil))
    (unless (bound-and-true-p ratex-mode)
      (ratex-mode 1))
    (unless my/ratex--initialized
      (setq-local my/ratex--initialized t)
      (add-hook 'change-major-mode-hook   #'my/ratex-hide-preview-now    nil t)
      (add-hook 'kill-buffer-hook         #'my/ratex-hide-preview-now    nil t)
      ;; Clear the overlay the moment a yasnippet expansion starts so the
      ;; image never pushes snippet fields off-screen.
      (add-hook 'yas-before-expand-snippet-hook #'my/ratex-hide-preview-now nil t))
    ;; `ratex-mode' enables tracking eagerly. We only want it while previews
    ;; should actually be active, otherwise Org insert latency suffers.
    (my/ratex-disable-post-command-tracking)
    (my/org-ratex-sync-evil-state)))

(use-package ratex
  :if (my/org-ratex-available-p)
  :load-path "~/.emacs.d/site-lisp/ratex.el/lisp"
  :commands (ratex-mode
             ratex-refresh-previews
             ratex-download-backend
             ratex-diagnose-backend)
  :init
  (setq ratex-backend-root my/ratex-root
        ratex-font-dir my/ratex-font-dir
        ratex-font-size 30.0
        ratex-edit-preview my/ratex-preview-style
        ratex-debug nil
        ratex-hide-source-while-preview nil
        ratex-inline-preview nil
        ratex-render-cache-limit 24   ; up from 12 — more unique formulas cached
        ratex-render-cache-ttl   60   ; up from 30 — survives longer editing sessions
        ratex-auto-download-backend t
        ratex-render-color "#d8dee9"
        ratex-posframe-background-color "#2b3140"
        ratex-posframe-border-color "#5f6f8f"
        ratex-posframe-poshandler
        #'ratex-posframe-poshandler-point-top-left-corner-offset)
  :hook ((org-mode . my/org-ratex-enable)
         (latex-mode . my/org-ratex-enable)
         (LaTeX-mode . my/org-ratex-enable)
         (tex-mode . my/org-ratex-enable)
         (TeX-mode . my/org-ratex-enable)
         (plain-tex-mode . my/org-ratex-enable)
         (plain-TeX-mode . my/org-ratex-enable)
         (docTeX-mode . my/org-ratex-enable)))

(with-eval-after-load 'ratex-render
  (advice-add 'ratex-handle-buffer-switch   :around #'my/ratex-handle-buffer-switch)
  (advice-add 'ratex-handle-post-command    :around #'my/ratex--debounced-post-command))

;; Only hook insert-state entry/exit.  Normal → visual → motion → replace
;; transitions do not change tracking eligibility (all are non-insert), so
;; they need no hooks — saving 4 extra dispatch calls on every state switch.
(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'my/org-ratex-sync-evil-state)
  (add-hook 'evil-insert-state-exit-hook  #'my/org-ratex-sync-evil-state))

(provide 'init-ratex)
;;; init-ratex.el ends here

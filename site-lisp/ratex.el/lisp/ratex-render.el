;;; ratex-render.el --- Async rendering client -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ratex-core)
(require 'ratex-math-detect)
(require 'ratex-overlays)
;; posframe is optional; load it dynamically when enabled.

(defvar ratex-mode)
(defvar ratex-render-color)
(defvar ratex-edit-preview)
(defvar ratex-edit-preview-idle-delay)
(defvar ratex-edit-preview-scan-lines)
(defvar ratex-inline-preview)
(defvar ratex-font-dir)
(defvar ratex-hide-source-while-preview)
(defvar ratex-initial-render-scope)
(defvar ratex-visible-region-margin)
(defvar ratex-max-svg-chars)
(defvar ratex-render-cache-limit)
(defvar ratex-render-cache-ttl)
(defvar ratex-posframe-background-color)
(defvar ratex-posframe-border-color)
(defvar ratex-posframe-max-pixel-width)
(defvar ratex-posframe-max-pixel-height)
(defvar ratex-posframe-poshandler)
(defvar ratex--posframe-owner-buffer nil
  "Buffer that currently owns the shared RaTeX posframe.")
(defvar ratex--posframe-owner-window nil
  "Window that owns the shared RaTeX posframe.")
(defvar-local ratex--render-cache nil)
(defvar-local ratex--render-cache-access nil)
(defvar-local ratex--inflight-requests nil)
(defvar-local ratex--inflight-waiters nil)
(defvar-local ratex--last-error nil)
(defvar-local ratex--active-fragment nil)
(defvar-local ratex--posframe-visible nil)
(defvar-local ratex--posframe-fragment nil)
(defvar-local ratex--posframe-last-anchor nil)
(defvar-local ratex--preview-enabled nil)
(defvar-local ratex--preview-timer nil)
(defvar-local ratex--force-preview-timer nil
  "Real-time fallback timer to refresh the preview during continuous editing.
Unlike `ratex--preview-timer' (idle-based), this fires unconditionally after
`ratex-edit-preview-max-staleness' seconds so the preview never goes fully
stale during rapid input.  Not reset by keystrokes; cancelled when the user
leaves the formula or the idle timer renders first.")
(defvar-local ratex--last-point nil)
(defvar-local ratex--last-tick nil)
(defvar-local ratex--preview-fragment nil)
(defvar-local ratex--preview-key nil)
(defvar-local ratex--edit-source-overlay nil)
(defvar-local ratex--posframe-last-override-params nil
  "Last override-parameters used for the posframe, cached for position updates.")
(defvar-local ratex--posframe-showing-p nil)
(defvar ratex--suppress-scroll-side-effects nil)
(defvar ratex--cache-gc-timer nil)
(defconst ratex--posframe-buffer " *ratex-preview*")
(defconst ratex--posframe-display-offset-y 28)
(defconst ratex--posframe-gap-x 10)
(defconst ratex--posframe-gap-y 8)
(defconst ratex--math-delimiter-re
  "\\$\\|\\\\(\\|\\\\\\[\\|\\\\begin{"
  "Regexp matching likely LaTeX math delimiters near point.")

(defun ratex--clear-posframe-state (&optional buffer)
  "Clear posframe bookkeeping in BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local ratex--posframe-visible nil)
    (setq-local ratex--posframe-fragment nil)
    (setq-local ratex--posframe-last-anchor nil)
    (setq-local ratex--posframe-last-override-params nil)
    (setq-local ratex--posframe-showing-p nil)))

(defun ratex--source-window (&optional buffer)
  "Return a live source window displaying BUFFER or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (cond
     ((and (window-live-p ratex--posframe-owner-window)
           (eq (window-buffer ratex--posframe-owner-window) buffer))
      ratex--posframe-owner-window)
     ((eq (window-buffer (selected-window)) buffer)
      (selected-window))
     (t
     (get-buffer-window buffer t)))))

(defun ratex--normalize-buffer (buffer-or-window)
  "Return a buffer for BUFFER-OR-WINDOW, or the current buffer."
  (cond
   ((windowp buffer-or-window)
    (window-buffer buffer-or-window))
   ((bufferp buffer-or-window)
    buffer-or-window)
   (t
    (current-buffer))))

(defun ratex--buffer-windows (&optional buffer-or-window)
  "Return all live windows currently showing BUFFER-OR-WINDOW."
  (let ((buffer (ratex--normalize-buffer buffer-or-window)))
    (seq-filter
     (lambda (window)
       (and (window-live-p window)
            (eq (window-buffer window) buffer)))
     (get-buffer-window-list buffer nil t))))

(defun ratex--capture-window-state (&optional window)
  "Capture scrolling state for WINDOW."
  (when-let* ((window (and (window-live-p window) window)))
    (list :window window
          :buffer (window-buffer window)
          :start (window-start window)
          :point (window-point window)
          :hscroll (window-hscroll window))))

(defun ratex--capture-buffer-window-states (&optional buffer-or-window)
  "Capture scrolling state for every live window showing BUFFER-OR-WINDOW."
  (mapcar #'ratex--capture-window-state
          (ratex--buffer-windows buffer-or-window)))

(defun ratex--restore-window-state (state)
  "Restore WINDOW scrolling STATE captured by `ratex--capture-window-state'."
  (when-let* ((window (plist-get state :window))
              ((window-live-p window))
              (buffer (plist-get state :buffer))
              ((eq (window-buffer window) buffer)))
    (let ((start (plist-get state :start))
          (point (plist-get state :point))
          (hscroll (plist-get state :hscroll)))
      (when (number-or-marker-p start)
        (set-window-start window start t))
      (when (number-or-marker-p point)
        (set-window-point window point))
      (when (integerp hscroll)
        (set-window-hscroll window hscroll)))))

(defun ratex--restore-buffer-window-states (states)
  "Restore a list of captured window STATES."
  (dolist (state states)
    (when state
      (ratex--restore-window-state state))))

(defun ratex--call-isolated-from-scroll (fn &optional buffer-or-window)
  "Call FN while isolating BUFFER-OR-WINDOW from scroll side effects."
  (let ((states (ratex--capture-buffer-window-states
                 buffer-or-window)))
    (let ((window-scroll-functions nil)
          (window-size-change-functions nil)
          (ratex--suppress-scroll-side-effects t))
      (unwind-protect
          (save-selected-window
            (funcall fn))
        (ratex--restore-buffer-window-states states)))))

(defun ratex--preserving-window-start (fn &optional buffer)
  "Call FN and restore window starts for BUFFER to prevent display scroll jumps.

Unlike `ratex--call-isolated-from-scroll', this does not suppress hooks or
alter the selected window, making it safe to call from async contexts.

Restoration is guaranteed even if FN signals an error (via `unwind-protect').
Each saved start is validated against current buffer bounds before restoring,
and `set-window-start' errors are silently swallowed so a stale position never
propagates outward."
  (let* ((buf (or buffer (current-buffer)))
         (wins (seq-filter #'window-live-p
                           (get-buffer-window-list buf nil t)))
         (saved (mapcar (lambda (w) (cons w (window-start w))) wins)))
    (unwind-protect
        (funcall fn)
      (with-current-buffer buf
        (dolist (entry saved)
          (let ((win (car entry))
                (pos (cdr entry)))
            (when (and (window-live-p win)
                       (eq (window-buffer win) buf)
                       (number-or-marker-p pos)
                       (<= (point-min) pos (point-max))
                       (/= (window-start win) pos))
              (condition-case nil
                  (set-window-start win pos t)
                (error nil)))))))))

(defun ratex--set-posframe-owner (buffer fragment override-params)
  "Record BUFFER as the owner of the shared posframe for FRAGMENT."
  (when (and (buffer-live-p ratex--posframe-owner-buffer)
             (not (eq ratex--posframe-owner-buffer buffer)))
    (ratex--clear-posframe-state ratex--posframe-owner-buffer))
  (setq ratex--posframe-owner-buffer buffer)
  (setq ratex--posframe-owner-window (ratex--source-window buffer))
  (with-current-buffer buffer
    (setq-local ratex--posframe-visible t)
    (setq-local ratex--posframe-fragment fragment)
    (setq-local ratex--posframe-last-anchor
                (ratex--preview-anchor-position fragment))
    (setq-local ratex--posframe-last-override-params override-params)))

(defun ratex-reset-buffer-state ()
  "Reset buffer-local rendering state."
  (when (eq ratex--posframe-owner-buffer (current-buffer))
    (ratex--hide-posframe))
  (setq-local ratex--render-cache (make-hash-table :test #'equal))
  (setq-local ratex--render-cache-access (make-hash-table :test #'equal))
  (setq-local ratex--inflight-requests (make-hash-table :test #'equal))
  (setq-local ratex--inflight-waiters (make-hash-table :test #'equal))
  (setq-local ratex--last-error nil)
  (setq-local ratex--active-fragment nil)
  (setq-local ratex--posframe-visible nil)
  (setq-local ratex--posframe-fragment nil)
  (setq-local ratex--posframe-last-anchor nil)
  (setq-local ratex--posframe-last-override-params nil)
  (setq-local ratex--posframe-showing-p nil)
  (setq-local ratex--preview-fragment nil)
  (setq-local ratex--preview-key nil)
  (when (overlayp ratex--edit-source-overlay)
    (delete-overlay ratex--edit-source-overlay))
  (setq-local ratex--edit-source-overlay nil)
  (setq-local ratex--preview-enabled nil)
  (when (timerp ratex--preview-timer)
    (cancel-timer ratex--preview-timer))
  (setq-local ratex--preview-timer nil)
  (when (timerp ratex--force-preview-timer)
    (cancel-timer ratex--force-preview-timer))
  (setq-local ratex--force-preview-timer nil)
  (setq-local ratex--last-point nil)
  (setq-local ratex--last-tick nil))

(defun ratex-refresh-previews (&optional include-active)
  "Refresh math previews in current buffer.

When INCLUDE-ACTIVE is non-nil, render all formulas, including the one
currently under point."
  (interactive)
  (if (not ratex-inline-preview)
      (ratex-clear-overlays)
    (let* ((fragments (ratex-fragments-in-buffer))
           (active (ratex-fragment-at-point))
           (targets (if include-active
                        fragments
                      (ratex--fragments-to-render fragments active)))
           (target-keys (mapcar #'ratex--fragment-key targets)))
      (ratex--drop-stale-overlays target-keys)
      (dolist (fragment targets)
        (ratex--ensure-fragment-preview fragment)))))

(defun ratex--window-visible-range (&optional window)
  "Return `(BEG . END)' for the visible portion of WINDOW in the current buffer."
  (when-let* ((window (or window (get-buffer-window (current-buffer) t))))
    (let ((beg (window-start window))
          (end (window-end window t)))
      (when (and beg end)
        (save-excursion
          (goto-char beg)
          (forward-line (- ratex-visible-region-margin))
          (setq beg (line-beginning-position))
          (goto-char end)
          (forward-line ratex-visible-region-margin)
          (setq end (line-end-position))
          (cons beg end))))))

(defun ratex--initial-fragments ()
  "Return fragments to render for initial inline preview."
  (pcase ratex-initial-render-scope
    ('all
     (ratex-fragments-in-buffer))
    (_
     (if-let* ((range (ratex--window-visible-range)))
         (ratex-fragments-in-region (car range) (cdr range))
       (ratex-fragments-in-buffer)))))

(defun ratex-initialize-previews ()
  "Render all formulas once and initialize point tracking."
  (setq ratex--preview-enabled (and (ratex--preview-style) t))
  (setq ratex--active-fragment (ratex-fragment-at-point))
  (if ratex-inline-preview
      (progn
        (let* ((fragments (ratex--initial-fragments))
               (target-keys (mapcar #'ratex--fragment-key fragments)))
          (ratex--drop-stale-overlays target-keys)
          (dolist (fragment fragments)
            (ratex--ensure-fragment-preview fragment)))
        (when ratex--active-fragment
          (ratex-remove-overlay (ratex--fragment-key ratex--active-fragment))))
    (ratex-clear-overlays)
    (when (and ratex--active-fragment
               (ratex--preview-enabled-p))
      (ratex--handle-preview-at-point ratex--active-fragment))))

(defun ratex-handle-post-command ()
  "Update previews only when point enters/leaves math fragments."
  (when ratex-mode
    (when (ratex--preview-visible-p)
      (ratex--update-posframe-position))
    (let* ((current (ratex--active-fragment-at-point))
           (previous ratex--active-fragment)
           (active (or current
                       (and previous
                            (ratex--point-in-fragment-p previous)
                            previous))))
      (ratex-debug-log "post-command point=%s previous=%S current=%S"
                       (point) previous current)
      (unless (and previous active
                   (ratex--same-active-context-p previous active))
        (when previous
          (ratex--ensure-fragment-preview previous)))
      (cond
       ((not active)
        (ratex--hide-edit-preview))
       ((ratex--preview-current-fragment-p active)
        nil)
       (t
        (ratex-remove-overlay (ratex--fragment-key active))
        (when (ratex--preview-enabled-p)
          (ratex--handle-preview-at-point active))))
      (setq ratex--active-fragment active))))

(defun ratex--handle-preview-at-point (fragment)
  "Show edit preview for FRAGMENT when enabled; otherwise hide it."
  (if (and fragment (ratex--preview-enabled-p))
      (let* ((image (ratex--overlay-image-for-fragment fragment))
             (cached (unless image (ratex--cached-response-for-fragment fragment)))
             (style (ratex--preview-style)))
        (ratex-remove-overlay (ratex--fragment-key fragment))
        (pcase style
          ('posframe
           (unless (ratex--display-edit-preview fragment cached image)
             (ratex--ensure-fragment-preview fragment))
           (ratex--update-posframe-position))
          (_
           (ratex--ensure-fragment-preview fragment))))
    (ratex--hide-edit-preview)))

(defun ratex--overlay-image-for-fragment (fragment)
  "Return cached overlay image for FRAGMENT, or nil."
  (let ((key (ratex--fragment-key fragment)))
    (ratex-overlay-image-for-key key)))

(defun ratex--cached-response-for-fragment (fragment)
  "Return cached backend response for FRAGMENT, or nil."
  (let ((cache-key (ratex--cache-key fragment)))
    (ratex--cache-get cache-key)))

(defun ratex--cache-now ()
  "Return current wall-clock time as a float."
  (float-time))

(defun ratex--cache-get (cache-key)
  "Return cached response for CACHE-KEY and refresh its access time."
  (when (hash-table-p ratex--render-cache)
    (let ((response (gethash cache-key ratex--render-cache)))
      (when response
        (puthash cache-key (ratex--cache-now) ratex--render-cache-access))
      response)))

(defun ratex--cache-put (cache-key response)
  "Store RESPONSE for CACHE-KEY and trim stale cache entries."
  (when (hash-table-p ratex--render-cache)
    (when (bound-and-true-p ratex-mode)
      (ratex--ensure-cache-gc-timer))
    (puthash cache-key response ratex--render-cache)
    (puthash cache-key (ratex--cache-now) ratex--render-cache-access)
    (ratex--trim-render-cache)))

(defun ratex--cache-delete (cache-key)
  "Delete CACHE-KEY from render caches."
  (when (hash-table-p ratex--render-cache)
    (remhash cache-key ratex--render-cache))
  (when (hash-table-p ratex--render-cache-access)
    (remhash cache-key ratex--render-cache-access)))

(defun ratex--ensure-cache-gc-timer ()
  "Ensure the global render-cache cleanup timer is running."
  (unless (timerp ratex--cache-gc-timer)
    (setq ratex--cache-gc-timer
          (run-with-idle-timer 30 t #'ratex--cleanup-render-caches))))

(defun ratex--cancel-cache-gc-timer ()
  "Cancel the global render-cache cleanup timer."
  (when (timerp ratex--cache-gc-timer)
    (cancel-timer ratex--cache-gc-timer))
  (setq ratex--cache-gc-timer nil))

(defun ratex--trim-render-cache ()
  "Trim the current buffer render cache."
  (when (and (hash-table-p ratex--render-cache)
             (integerp ratex-render-cache-limit)
             (> ratex-render-cache-limit 0))
    (while (> (hash-table-count ratex--render-cache) ratex-render-cache-limit)
      (let (oldest-key oldest-time)
        (maphash
         (lambda (key time)
           (when (or (null oldest-time) (< time oldest-time))
             (setq oldest-key key)
             (setq oldest-time time)))
         ratex--render-cache-access)
        (if oldest-key
            (ratex--cache-delete oldest-key)
          (clrhash ratex--render-cache)
          (clrhash ratex--render-cache-access))))))

(defun ratex--cleanup-render-caches ()
  "Drop expired render-cache entries from all live RaTeX buffers."
  (let ((now (ratex--cache-now))
        (ttl (and (integerp ratex-render-cache-ttl)
                  (> ratex-render-cache-ttl 0)
                  ratex-render-cache-ttl))
        (live-ratex-buffer nil))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (bound-and-true-p ratex-mode)
            (setq live-ratex-buffer t)
            (when (and ttl
                       (hash-table-p ratex--render-cache-access))
              (let (expired)
                (maphash
                 (lambda (key last-used)
                   (when (> (- now last-used) ttl)
                     (push key expired)))
                 ratex--render-cache-access)
                (dolist (key expired)
                  (ratex--cache-delete key))))))))
    (unless live-ratex-buffer
      (ratex--cancel-cache-gc-timer))))

(defun ratex--image-from-response (response)
  "Build an image object from backend RESPONSE."
  (let* ((svg (alist-get 'svg response))
         (baseline (or (alist-get 'baseline response) 0.0))
         (height (max 0.001 (or (alist-get 'height response) 0.0))))
    (when (and (stringp svg)
               (or (null ratex-max-svg-chars)
                   (<= (length svg) ratex-max-svg-chars)))
      (create-image
       svg
       'svg t
       :ascent (floor (* 100.0 (/ baseline height)))))))

(defun ratex--string-at-p (pos string)
  "Return non-nil when STRING is present at buffer position POS."
  (let ((len (and (stringp string) (length string))))
    (and len
         (<= (point-min) pos)
         (<= (+ pos len) (point-max))
         (string= string
                  (buffer-substring-no-properties pos (+ pos len))))))

(defun ratex--find-fragment-close (close start)
  "Return the end position of CLOSE found after START, ignoring escapes."
  (let ((close-len (length close))
        found)
    (save-excursion
      (goto-char start)
      (while (and (not found)
                  (search-forward close nil t))
        (let ((close-begin (- (point) close-len)))
          (unless (ratex--escaped-at-p close-begin)
            (setq found (point))))))
    found))

(defun ratex--refresh-active-delimited-fragment (fragment)
  "Refresh active delimited FRAGMENT using local delimiter search."
  (let* ((begin (plist-get fragment :begin))
         (open (plist-get fragment :open))
         (close (plist-get fragment :close))
         (open-len (and (stringp open) (length open)))
         (close-len (and (stringp close) (length close))))
    (when (and (integer-or-marker-p begin)
               open-len
               close-len
               (> open-len 0)
               (> close-len 0)
               (<= (point-min) begin)
               (<= begin (point))
               (ratex--string-at-p begin open))
      (let* ((content-begin (+ begin open-len))
             (end (ratex--find-fragment-close close content-begin))
             (content-end (and end (- end close-len))))
        (when (and end
                   content-end
                   (<= content-begin content-end)
                   (< (point) end))
          (list :begin begin
                :end end
                :content (buffer-substring-no-properties content-begin content-end)
                :open open
                :close close))))))

(defun ratex--refresh-active-environment-fragment (fragment)
  "Refresh active environment FRAGMENT using local environment delimiters."
  (let* ((begin (plist-get fragment :begin))
         (env (plist-get fragment :environment)))
    (when (and (integer-or-marker-p begin)
               (stringp env)
               (<= (point-min) begin)
               (<= begin (point))
               (ratex--string-at-p begin (format "\\begin{%s}" env)))
      (save-excursion
        (goto-char begin)
        (when (re-search-forward
               (format "\\\\end{%s}" (regexp-quote env)) nil t)
          (let ((end (match-end 0)))
            (when (< (point) end)
              (list :begin begin
                    :end end
                    :content (buffer-substring-no-properties begin end)
                    :open (format "\\begin{%s}" env)
                    :close (format "\\end{%s}" env)
                    :environment env))))))))

(defun ratex--refresh-active-fragment-fast (fragment)
  "Refresh active FRAGMENT without invoking the full point parser.
This handles the dominant post-command case: point is still inside the same
formula, so local delimiter search is enough to update bounds and content after
ordinary edits."
  (when (ratex--point-in-fragment-p fragment)
    (or (and (plist-get fragment :environment)
             (ratex--refresh-active-environment-fragment fragment))
        (ratex--refresh-active-delimited-fragment fragment)
        fragment)))

(defun ratex--active-fragment-at-point ()
  "Return editable fragment at point, including rendered overlay fallback."
  (unless (ratex--code-context-at-p (point))
    (or (ratex--refresh-active-fragment-fast ratex--active-fragment)
        (ratex-overlay-fragment-at-point)
        (ratex-fragment-at-point)
        (when (ratex--point-in-fragment-p ratex--active-fragment)
          ratex--active-fragment))))

(defun ratex--point-in-fragment-p (fragment)
  "Return non-nil if point is within FRAGMENT."
  (when fragment
    (let ((begin (plist-get fragment :begin))
          (end (plist-get fragment :end)))
      (and (integer-or-marker-p begin)
           (integer-or-marker-p end)
           (<= begin (point))
           (< (point) end)))))


(defun ratex--fragments-to-render (fragments active)
  "Return FRAGMENTS excluding ACTIVE."
  (cl-remove-if
   (lambda (fragment)
     (and active (ratex--same-fragment-p fragment active)))
   fragments))

(defun ratex--same-fragment-p (a b)
  "Return non-nil when fragments A and B represent the same range."
  (and (= (plist-get a :begin) (plist-get b :begin))
       (= (plist-get a :end) (plist-get b :end))
       (equal (plist-get a :content) (plist-get b :content))))

(defun ratex--same-active-context-p (a b)
  "Return non-nil when A and B are part of the same editing fragment."
  (or (ratex--same-fragment-p a b)
      (ratex--fragments-overlap-p a b)))

(defun ratex--preview-session-matches-p (fragment)
  "Return non-nil when the current preview session already matches FRAGMENT."
  (and ratex--preview-fragment
       (ratex--same-fragment-p ratex--preview-fragment fragment)
       (equal ratex--preview-key (ratex--fragment-key fragment))))

(defun ratex--preview-current-fragment-p (fragment)
  "Return non-nil when the visible preview still corresponds to FRAGMENT."
  (and (ratex--edit-preview-visible-p)
       (or (ratex--preview-session-matches-p fragment)
           (and ratex--active-fragment
                (ratex--same-fragment-p ratex--active-fragment fragment)))))

(defun ratex--fragment-key (fragment)
  "Return stable overlay key for FRAGMENT."
  (format "%d:%d:%s"
          (plist-get fragment :begin)
          (plist-get fragment :end)
          (plist-get fragment :content)))

(defun ratex--cache-key (fragment)
  "Return render cache key for FRAGMENT."
  (list (ratex--render-latex fragment)
        ratex-font-size
        ratex-svg-padding
        (ratex--normalized-render-color)))

(defun ratex--render-latex (fragment)
  "Return backend-ready LaTeX for FRAGMENT.

RaTeX core does not accept `\\(' or `\\[' delimiters directly, so we strip
those wrappers during detection. To preserve display-style rendering for
bracketed math, re-add the semantic hint as `\\displaystyle'. Environment
fragments are normalized for preview compatibility when needed."
  (let ((content (string-trim (plist-get fragment :content)))
        (open (plist-get fragment :open)))
    (cond
     ((plist-get fragment :environment)
      (ratex--render-environment-latex fragment content))
     ((equal open "\\[")
      (if (string-empty-p content)
          "\\displaystyle"
        (concat "\\displaystyle " content)))
     (t
      content))))

(defun ratex--render-environment-latex (fragment content)
  "Return preview-ready LaTeX for environment FRAGMENT with CONTENT."
  (let* ((environment (plist-get fragment :environment))
         (preview-environment (ratex--preview-environment-name environment)))
    (if (equal preview-environment environment)
        content
      (string-replace
       (format "\\end{%s}" environment)
       (format "\\end{%s}" preview-environment)
       (string-replace
        (format "\\begin{%s}" environment)
        (format "\\begin{%s}" preview-environment)
        content)))))

(defun ratex--preview-environment-name (environment)
  "Return a preview-safe environment name for ENVIRONMENT."
  (pcase environment
    ((or "equation" "align" "alignat" "gather" "multline" "flalign"
         "eqnarray" "dmath" "dseries" "dgroup")
     (concat environment "*"))
    (_ environment)))

(defun ratex--normalized-render-color ()
  "Return a normalized render color string, or nil."
  (let ((value ratex-render-color))
    (when (stringp value)
      (let ((trimmed (string-trim value)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun ratex--inflight-table ()
  "Return request-tracking table for current buffer."
  (unless (hash-table-p ratex--inflight-requests)
    (setq-local ratex--inflight-requests (make-hash-table :test #'equal)))
  ratex--inflight-requests)

(defun ratex--inflight-waiters-table ()
  "Return waiter table for in-flight requests in current buffer."
  (unless (hash-table-p ratex--inflight-waiters)
    (setq-local ratex--inflight-waiters (make-hash-table :test #'equal)))
  ratex--inflight-waiters)

(defun ratex--enqueue-waiter (cache-key fragment-key fragment)
  "Track FRAGMENT for CACHE-KEY while backend render is in flight."
  (let* ((table (ratex--inflight-waiters-table))
         (waiters (gethash cache-key table)))
    (unless (assoc fragment-key waiters)
      (puthash cache-key
               (cons (cons fragment-key fragment) waiters)
               table))))

(defun ratex--fragment-valid-p (fragment)
  "Return non-nil when FRAGMENT still matches current buffer text."
  (let ((begin (plist-get fragment :begin))
        (end (plist-get fragment :end))
        (open (plist-get fragment :open))
        (content (plist-get fragment :content))
        (close (plist-get fragment :close)))
    (and (integer-or-marker-p begin)
         (integer-or-marker-p end)
         (<= (point-min) begin end (point-max))
         (string=
          (buffer-substring-no-properties begin end)
          (if (plist-get fragment :environment)
              content
            (concat open content close))))))

(defun ratex--drop-stale-overlays (target-keys)
  "Delete overlays not present in TARGET-KEYS."
  (let ((keep (make-hash-table :test #'equal)))
    (dolist (key target-keys)
      (puthash key t keep))
    (dolist (key (ratex-overlay-keys))
      (unless (gethash key keep)
        (ratex-remove-overlay key)))))

(defun ratex--render-payload (fragment)
  "Build the render request payload for FRAGMENT."
  (let ((payload
         (append
          `((type . "render")
            (latex . ,(ratex--render-latex fragment))
            (font_size . ,ratex-font-size)
            (padding . ,ratex-svg-padding)
            (embed_glyphs . t))
          (when-let* ((color (ratex--normalized-render-color)))
            `((color . ,color)))
          (when-let* ((font-dir (or ratex-font-dir
                                    (ignore-errors (ratex-default-font-dir))))
                      ((file-directory-p font-dir)))
            `((font_dir . ,(expand-file-name font-dir)))))))
    (ratex-debug-log "render-payload fragment=%S payload=%S" fragment payload)
    payload))

(defun ratex--ensure-fragment-preview (fragment)
  "Ensure FRAGMENT preview is displayed or requested."
  (let* ((fragment-key (ratex--fragment-key fragment))
         (cache-key (ratex--cache-key fragment))
         (cached (ratex--cache-get cache-key))
         (inflight (gethash cache-key (ratex--inflight-table))))
    (cond
     ((not (ratex--fragment-valid-p fragment))
      (ratex-remove-overlay fragment-key))
     (cached
      (ratex--preserving-window-start
       (lambda ()
         (ratex--display-response fragment-key fragment cached))))
     (inflight
      (ratex--enqueue-waiter cache-key fragment-key fragment))
     (t
      (ratex--enqueue-waiter cache-key fragment-key fragment)
      (puthash cache-key t (ratex--inflight-table))
      (ratex-request
       (ratex--render-payload fragment)
       (lambda (response)
         (remhash cache-key (ratex--inflight-table))
         (let ((waiters (gethash cache-key (ratex--inflight-waiters-table))))
           (remhash cache-key (ratex--inflight-waiters-table))
           (when (alist-get 'ok response)
             (ratex--cache-put cache-key response))
           (when ratex-mode
             (ratex--preserving-window-start
              (lambda ()
                (dolist (entry waiters)
                  (ratex--display-if-visible
                   (car entry)
                   (cdr entry)
                   response))))))))))))

(defun ratex--display-if-visible (fragment-key fragment response)
  "Display RESPONSE for FRAGMENT-KEY if FRAGMENT should still be visible."
  (let ((active (ratex--active-fragment-at-point)))
    (cond
     ((not (ratex--fragment-valid-p fragment))
      (ratex-remove-overlay fragment-key))
     ((and active (ratex--same-active-context-p fragment active))
      (if (ratex--preview-enabled-p)
          (progn
            (ratex-remove-overlay fragment-key)
            (unless (ratex--preview-current-fragment-p fragment)
              (unless (ratex--display-edit-preview fragment response)
                (ratex--ensure-fragment-preview fragment))))
        (ratex-remove-overlay fragment-key)))
     (t
      (if ratex-inline-preview
          (ratex--display-response fragment-key fragment response 'inline)
        (ratex-remove-overlay fragment-key))))))

(defun ratex--display-response (fragment-key fragment response &optional style)
  "Display backend RESPONSE for FRAGMENT identified by FRAGMENT-KEY."
  (if (not (alist-get 'ok response))
      (progn
        (setq ratex--last-error (alist-get 'error response))
        (ratex-remove-overlay fragment-key)
        (when ratex--last-error
          (message "RaTeX render failed: %s" ratex--last-error)))
    (let ((image (ratex--image-from-response response)))
      (setq ratex--last-error nil)
      (if (not image)
          (ratex-remove-overlay fragment-key)
        (cond
         ((and (ratex--preview-enabled-p)
               (ratex--point-in-fragment-p fragment))
          (ratex-remove-overlay fragment-key)
          (unless (ratex--preview-current-fragment-p fragment)
            (ratex--display-edit-preview fragment response image)))
         (ratex-inline-preview
          (ratex-show-overlay
           fragment-key
           (plist-get fragment :begin)
           (plist-get fragment :end)
           image
           (format "RaTeX %s" (if (alist-get 'cached response) "cached" "rendered"))
           fragment
           (or style 'inline)))
         (t
          (ratex-remove-overlay fragment-key)))))))



(defun ratex--preview-enabled-p ()
  "Return non-nil when edit preview is enabled and currently allowed."
  (and ratex--preview-enabled
       (ratex--preview-style)
       (ratex--preview-active-p)))

(defun ratex--preview-visible-p ()
  "Return non-nil when the preview UI is active right now."
  (and ratex--preview-enabled
       (ratex--preview-style)))

(defun ratex--edit-preview-visible-p ()
  "Return non-nil when the active edit preview UI is currently visible."
  (and (eq (ratex--preview-style) 'posframe)
       ratex--posframe-visible))

(defun ratex--preview-style ()
  "Return active edit preview style or nil."
  ratex-edit-preview)

(defun ratex--preview-active-p ()
  "Return non-nil when edit preview should be visible in the current state."
  t)

(defun ratex--buffer-visible-p (&optional buffer)
  "Return non-nil when BUFFER is currently visible in a live window."
  (get-buffer-window (or buffer (current-buffer)) t))

(defun ratex--near-math-p ()
  "Return non-nil when point is plausibly near a math fragment."
  (let ((lo (save-excursion
              (forward-line (- ratex-edit-preview-scan-lines))
              (line-beginning-position)))
        (hi (save-excursion
              (forward-line ratex-edit-preview-scan-lines)
              (line-end-position))))
    (save-excursion
      (goto-char lo)
      (re-search-forward ratex--math-delimiter-re hi t))))

(defun ratex--cancel-pending-preview ()
  "Cancel any queued edit-preview update."
  (when (timerp ratex--preview-timer)
    (cancel-timer ratex--preview-timer)
    (setq-local ratex--preview-timer nil)))

(defun ratex--cancel-force-preview ()
  "Cancel any running real-time force-preview timer."
  (when (timerp ratex--force-preview-timer)
    (cancel-timer ratex--force-preview-timer)
    (setq-local ratex--force-preview-timer nil)))

(defun ratex--force-refresh-preview (buffer)
  "Force-refresh the preview in BUFFER and reschedule the force timer.
Called unconditionally by `ratex--force-preview-timer' so the preview
never goes fully stale during rapid continuous input."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local ratex--force-preview-timer nil)
      (when (and ratex-mode
                 (ratex--buffer-visible-p)
                 ratex--active-fragment
                 ratex-edit-preview-max-staleness)
        (ratex--cancel-pending-preview)
        (ratex--refresh-preview-now)
        ;; Reschedule as long as we are still editing a formula.
        (when (and ratex--active-fragment
                   ratex-edit-preview-max-staleness)
          (setq-local ratex--force-preview-timer
                      (run-with-timer
                       ratex-edit-preview-max-staleness nil
                       #'ratex--force-refresh-preview
                       buffer)))))))

(defun ratex--refresh-preview-now ()
  "Refresh the active preview immediately in the current buffer."
  (condition-case err
      (ratex-handle-post-command)
    (error
     (message "[ratex] preview error: %S" err)
     (ratex--hide-edit-preview))))

(defun ratex-post-command ()
  "Debounce active-fragment updates after post-command."
  ;; Check mode/visibility unconditionally so we always hide when the buffer
  ;; becomes invisible, regardless of point-cache state.
  (if (not (and ratex-mode (ratex--buffer-visible-p)))
      (ratex--hide-edit-preview)
    ;; Dedup by point+tick BEFORE calling ratex--near-math-p (regex scan).
    ;; Also update last-point/tick unconditionally so that when the cursor is
    ;; away from math the regex is not re-run on every command at the same pos.
    (let ((pt (point))
          (tick (buffer-chars-modified-tick)))
      (unless (and (eql pt ratex--last-point)
                   (eql tick ratex--last-tick))
        (setq-local ratex--last-point pt
                    ratex--last-tick tick)
        (if (or ratex--active-fragment (ratex--near-math-p))
            (progn
              (ratex--cancel-pending-preview)
              (setq-local ratex--preview-timer
                          (run-with-idle-timer
                           ratex-edit-preview-idle-delay nil
                           (lambda (buffer)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (setq ratex--preview-timer nil)
                                 (when (and ratex-mode
                                            (ratex--buffer-visible-p))
                                   ;; Idle timer fired first — cancel the real-time
                                   ;; fallback so they don't double-render.
                                   (ratex--cancel-force-preview)
                                   (ratex--refresh-preview-now)))))
                           (current-buffer))))
          (ratex--hide-edit-preview))))))

(defun ratex--display-edit-preview (fragment &optional response image)
  "Display edit preview for FRAGMENT using the configured style.

When `posframe' is selected, use a child frame."
  (let* ((preview-image (or image (and response (ratex--image-from-response response))))
         (shown
          (pcase (ratex--preview-style)
            ('posframe
             (ratex-debug-log "display-edit-preview style=posframe fragment=%S response=%s image=%s"
                              fragment (not (null response)) (not (null image)))
             (ratex--display-posframe fragment response preview-image))
            (_ nil))))
    (if shown
        (progn
          (setq-local ratex--preview-fragment fragment)
          (setq-local ratex--preview-key (ratex--fragment-key fragment)))
      (setq-local ratex--preview-fragment nil)
      (setq-local ratex--preview-key nil))
    shown))

(defun ratex--ensure-posframe-loaded ()
  "Return non-nil when posframe is available; load it if needed."
  (or (featurep 'posframe)
      (require 'posframe nil t)))

(defun ratex--image-size-pixels (image)
  "Return IMAGE size in pixels."
  (image-size image t))

(defun ratex--posframe-pixel-size (image &optional border-width)
  "Return capped pixel `(WIDTH . HEIGHT)' for IMAGE in the posframe."
  (let* ((size (ratex--image-size-pixels image))
         (ibw (or border-width 8))
         (max-w (min ratex-posframe-max-pixel-width
                     (max 64 (- (frame-pixel-width) 32))))
         (max-h (min ratex-posframe-max-pixel-height
                     (max 64 (- (frame-pixel-height) 32))))
         (px-w (min (+ (ceiling (car size)) (* 2 ibw) 2) max-w))
         (px-h (min (+ (ceiling (cdr size)) (* 2 ibw) 2) max-h)))
    (cons px-w px-h)))

(defun ratex--display-posframe (fragment &optional response image)
  "Display IMAGE (or RESPONSE) in a posframe for FRAGMENT."
  (when (and (eq (ratex--preview-style) 'posframe)
             (ratex--ensure-posframe-loaded)
             (featurep 'posframe)
             (ratex--point-in-fragment-p fragment))
    (let ((image (or image (and response (ratex--image-from-response response)))))
      (when image
        (condition-case nil
            (let* ((anchor (ratex--preview-anchor-position fragment))
                   (ibw 8)
                   (dims (ratex--posframe-pixel-size image ibw))
                   (px-w (car dims))
                   (px-h (cdr dims))
                   (override-params
                    `((width . (text-pixels . ,px-w))
                      (height . (text-pixels . ,px-h)))))
              (ratex-debug-log "try posframe fragment=%S size=%S" fragment dims)
              (with-current-buffer (get-buffer-create ratex--posframe-buffer)
                (setq-local truncate-lines nil)
                (setq-local word-wrap nil)
                (erase-buffer)
                (insert (propertize " " 'display image)))
              (when (and (buffer-live-p ratex--posframe-owner-buffer)
                         (not (eq ratex--posframe-owner-buffer (current-buffer))))
                (ratex--hide-posframe))
              (ratex--call-isolated-from-scroll
               (lambda ()
                 (posframe-show
                  ratex--posframe-buffer
                  :position anchor
                  :poshandler (ratex--posframe-poshandler-for-fragment fragment)
                  :internal-border-width ibw
                  :border-width 1
                  :border-color ratex-posframe-border-color
                  :background-color ratex-posframe-background-color
                  :override-parameters override-params)
                 (ratex--mask-edit-source fragment)))
              (ratex--set-posframe-owner (current-buffer) fragment override-params)
              (setq-local ratex--posframe-showing-p t)
              (ratex-debug-log "posframe success")
              t)
          (error
           (ratex-debug-log "posframe failed")
           nil))))))

(defun ratex--point-position-relative-to-native-frame (&optional point window)
  "Return pixel position of POINT in WINDOW relative to the native frame.
Returns nil when POINT is not currently visible in WINDOW, so callers must
guard against nil rather than computing coordinates from garbage data."
  (let* ((window (or window (selected-window)))
         (point  (or point (window-point window)))
         (pos    (pos-visible-in-window-p point window t)))
    (when pos
      (let ((edges (window-edges window nil nil t)))
        (cons (+ (car pos)  (car  edges) (frame-char-width))
              (+ (cadr pos) (cadr edges)))))))

(defun ratex--preview-anchor-position (fragment)
  "Return the buffer position used to anchor FRAGMENT edit previews.

Keep the preview anchored to the fragment opening position so it doesn't
follow point while editing."
  (plist-get fragment :begin))

(defun ratex--posframe-info-position (info)
  "Return the buffer position embedded in posframe INFO, or current point."
  (or (plist-get info :position)
      (point)))

(defun ratex--posframe-window-bottom (&optional window)
  "Return the usable bottom pixel y of WINDOW in native-frame coordinates.

Mirrors company-box's `company-box--point-bottom': walks down to the
bottommost sibling window and uses its mode-line edge as the true floor,
so the posframe never overlaps the mode-line or mini-buffer."
  (let* ((win (or window (selected-window)))
         (bottom-win (let ((tmp win))
                       (while (window-in-direction 'below tmp t)
                         (setq tmp (window-in-direction 'below tmp t)))
                       tmp))
         (edges (window-edges bottom-win t nil t)))
    ;; cadr = top-pixel of window; add pixel-height to get bottom
    (+ (cadr edges) (window-pixel-height bottom-win))))

(defun ratex--posframe-clamp-x (x width)
  "Clamp posframe X so a box of WIDTH stays within the frame."
  (max ratex--posframe-gap-x
       (min x (- (frame-inner-width) width ratex--posframe-gap-x))))

(defun ratex-posframe-poshandler-point-bottom-left-corner-offset (info)
  "Position posframe below point with a small gap from the cursor."
  (let* ((point-pos (ratex--point-position-relative-to-native-frame
                     (ratex--posframe-info-position info)))
         (width (or (plist-get info :posframe-width) 0)))
    (if (not point-pos)
        (cons ratex--posframe-gap-x ratex--posframe-gap-y)
      (cons (ratex--posframe-clamp-x (+ (car point-pos) ratex--posframe-gap-x) width)
            (+ (cdr point-pos) (frame-char-height) ratex--posframe-gap-y)))))

(defun ratex-posframe-poshandler-point-top-left-corner-offset (info)
  "Position posframe preferring above point, with full four-edge boundary clamping.

Strategy mirrors company-box's frame-position logic:
  1. Measure real usable space above and below using the source window's
     true bottom (mode-line edge), not the raw frame height.
  2. Prefer above when space fits; fall back to below.
  3. When neither side fits fully, choose the roomier side and clamp to
     the nearest frame edge so the posframe is never partially off-screen.
  4. Clamp x so the box stays within [gap, frame-right - width - gap].
  5. Return a safe fallback when the anchor is not visible in the window."
  (let* ((anchor    (ratex--posframe-info-position info))
         (win       (or (plist-get info :parent-window) (selected-window)))
         (point-pos (or (ratex--point-position-relative-to-native-frame anchor win)
                        ;; Anchor scrolled out of view: try current point.
                        (ratex--point-position-relative-to-native-frame nil win))))
    (if (not point-pos)
        ;; Complete fallback: nothing visible — park at top-left.
        (cons ratex--posframe-gap-x ratex--posframe-gap-y)
      (let* ((raw-x  (+ (car point-pos) ratex--posframe-gap-x))
             (raw-y  (cdr point-pos))
             (em     (frame-char-height))
             (width  (or (plist-get info :posframe-width)  0))
             (height (or (plist-get info :posframe-height) 0))
             ;; Use the real window bottom (mode-line), not frame-inner-height.
             (win-bottom    (ratex--posframe-window-bottom win))
             ;; Available pixels above / below the cursor line.
             (space-above   (- raw-y ratex--posframe-gap-y))
             (space-below   (- win-bottom (+ raw-y em ratex--posframe-gap-y)))
             (y (cond
                  ;; Preferred: enough room above.
                  ((>= space-above height)
                   (- raw-y height ratex--posframe-gap-y))
                  ;; Fall back: enough room below.
                  ((>= space-below height)
                   (+ raw-y em ratex--posframe-gap-y))
                  ;; Neither fits fully — pick the roomier side and clamp.
                  ((>= space-above space-below)
                   (max 0 (- raw-y height ratex--posframe-gap-y)))
                  (t
                   (min (max 0 (- (frame-inner-height) height))
                        (+ raw-y em ratex--posframe-gap-y))))))
        (cons (ratex--posframe-clamp-x raw-x width) y)))))

(defun ratex-posframe-poshandler-point-bottom-left-corner-upward-offset (info)
  "Position posframe above point, falling back to below when near screen top."
  (let* ((above-base (posframe-poshandler-point-bottom-left-corner-upward info))
         (above-y (- (cdr above-base) ratex--posframe-display-offset-y))
         (posframe-h (or (plist-get info :posframe-height) 0)))
    (if (>= (- above-y posframe-h) 0)
        (cons (car above-base) above-y)
      (let* ((below-base (posframe-poshandler-point-bottom-left-corner info))
             (below-y (+ (cdr below-base) ratex--posframe-display-offset-y)))
        (cons (car below-base) below-y)))))

(defun ratex--posframe-poshandler-for-fragment (fragment)
  "Return the best posframe poshandler for FRAGMENT."
  (or ratex-posframe-poshandler
      #'ratex-posframe-poshandler-point-top-left-corner-offset))

(defun ratex--hide-posframe ()
  "Hide the posframe preview."
  (when (or ratex--posframe-showing-p
            (buffer-live-p ratex--posframe-owner-buffer))
    (let ((owner-buffer (and (buffer-live-p ratex--posframe-owner-buffer)
                             ratex--posframe-owner-buffer))
          (owner-window (and (window-live-p ratex--posframe-owner-window)
                             ratex--posframe-owner-window)))
      (when owner-buffer
        (with-current-buffer owner-buffer
          (ratex--unmask-edit-source)
          (ratex--clear-posframe-state)))
      (setq ratex--posframe-owner-buffer nil)
      (setq ratex--posframe-owner-window nil)
      (when (featurep 'posframe)
        (when (get-buffer ratex--posframe-buffer)
          (ratex--call-isolated-from-scroll
           (lambda ()
             (posframe-hide ratex--posframe-buffer))
           (or owner-window owner-buffer (current-buffer))))
        (ratex--clear-posframe-state))
      (when owner-buffer
        (with-current-buffer owner-buffer
          (ratex--unmask-edit-source)))
      (ratex--unmask-edit-source))))

(defun ratex--dismiss-edit-preview ()
  "Hide the edit preview UI without ending the current formula session."
  (ratex--cancel-pending-preview)
  (ratex--cancel-force-preview)
  (ratex--hide-posframe)
  (ratex--unmask-edit-source))

(defun ratex--hide-edit-preview ()
  "Hide the active edit preview."
  (ratex--dismiss-edit-preview)
  (setq-local ratex--active-fragment nil
              ratex--last-point nil
              ratex--last-tick nil
              ratex--preview-fragment nil
              ratex--preview-key nil))

(defun ratex--update-posframe-position ()
  "Keep floating preview aligned with the active fragment anchor."
  (when (and ratex--posframe-visible
             (eq (ratex--preview-style) 'posframe)
             (ratex--ensure-posframe-loaded)
             (featurep 'posframe))
    (if (ratex--point-in-fragment-p ratex--posframe-fragment)
        (let ((anchor (ratex--preview-anchor-position
                       ratex--posframe-fragment)))
          (condition-case nil
              (progn
                (unless (equal anchor ratex--posframe-last-anchor)
                  (ratex--call-isolated-from-scroll
                   (lambda ()
                     (posframe-show
                      ratex--posframe-buffer
                      :position anchor
                      :poshandler (ratex--posframe-poshandler-for-fragment
                                   ratex--posframe-fragment)
                      :border-width 1
                      :border-color ratex-posframe-border-color
                      :background-color ratex-posframe-background-color
                      :override-parameters ratex--posframe-last-override-params)
                     (ratex--mask-edit-source ratex--posframe-fragment)))
                  (setq-local ratex--posframe-last-anchor anchor)))
            (error (ratex--hide-posframe))))
      (ratex--hide-posframe))))

(defun ratex--mask-edit-source (fragment)
  "Hide the source text of FRAGMENT while edit preview is visible."
  (when (and ratex-hide-source-while-preview fragment)
    (unless (overlayp ratex--edit-source-overlay)
      (setq ratex--edit-source-overlay (make-overlay 1 1 nil t nil))
      (overlay-put ratex--edit-source-overlay 'evaporate t))
    (move-overlay ratex--edit-source-overlay
                  (plist-get fragment :begin)
                  (plist-get fragment :end))
    (overlay-put ratex--edit-source-overlay 'display "")
    (overlay-put ratex--edit-source-overlay 'invisible t)))

(defun ratex--unmask-edit-source ()
  "Show source text previously hidden for edit preview."
  (when (overlayp ratex--edit-source-overlay)
    (overlay-put ratex--edit-source-overlay 'invisible nil)
    (delete-overlay ratex--edit-source-overlay))
  (setq ratex--edit-source-overlay nil))

(defun ratex-toggle-preview-at-point ()
  "Toggle the RaTeX posframe preview for the formula at point."
  (interactive)
  (when (ratex--preview-style)
    (setq ratex--preview-enabled (not ratex--preview-enabled))
    (if ratex--preview-enabled
        (ratex--handle-preview-at-point (ratex--active-fragment-at-point))
      (ratex--hide-edit-preview)
      (let ((fragment (ratex--active-fragment-at-point)))
        (when fragment
          (ratex--ensure-fragment-preview fragment))))))

(defun ratex-handle-buffer-switch ()
  "Hide the shared edit preview when its owner buffer is no longer selected."
  (when (and (ratex--preview-style)
             (buffer-live-p ratex--posframe-owner-buffer)
             (not (eq (window-buffer (selected-window))
                      ratex--posframe-owner-buffer)))
    (ratex--hide-posframe)))

(provide 'ratex-render)

;;; ratex-render.el ends here

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
(defvar ratex-inline-preview)
(defvar ratex-font-dir)
(defvar ratex-hide-source-while-preview)
(defvar ratex-render-cache-limit)
(defvar ratex-render-cache-ttl)
(defvar ratex-posframe-background-color)
(defvar ratex-posframe-border-color)
(defvar ratex-posframe-poshandler)
(defvar-local ratex--render-cache nil)
(defvar-local ratex--render-cache-access nil)
(defvar-local ratex--inflight-requests nil)
(defvar-local ratex--inflight-waiters nil)
(defvar-local ratex--last-error nil)
(defvar-local ratex--active-fragment nil)
(defvar-local ratex--posframe-visible nil)
(defvar-local ratex--posframe-fragment nil)
(defvar-local ratex--preview-enabled nil)
(defvar-local ratex--edit-source-overlay nil)
(defvar-local ratex--posframe-last-override-params nil
  "Last override-parameters used for the posframe, cached for position updates.")
(defvar ratex--cache-gc-timer nil)
(defconst ratex--posframe-buffer " *ratex-preview*")
(defconst ratex--posframe-offset-y 72)
(defconst ratex--posframe-display-offset-y 28)

(defun ratex-reset-buffer-state ()
  "Reset buffer-local rendering state."
  (setq-local ratex--render-cache (make-hash-table :test #'equal))
  (setq-local ratex--render-cache-access (make-hash-table :test #'equal))
  (setq-local ratex--inflight-requests (make-hash-table :test #'equal))
  (setq-local ratex--inflight-waiters (make-hash-table :test #'equal))
  (setq-local ratex--last-error nil)
  (setq-local ratex--active-fragment nil)
  (setq-local ratex--posframe-visible nil)
  (setq-local ratex--posframe-fragment nil)
  (setq-local ratex--posframe-last-override-params nil)
  (when (overlayp ratex--edit-source-overlay)
    (delete-overlay ratex--edit-source-overlay))
  (setq-local ratex--edit-source-overlay nil)
  (setq-local ratex--preview-enabled nil)
  (ratex--ensure-cache-gc-timer))

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

(defun ratex-initialize-previews ()
  "Render all formulas once and initialize point tracking."
  (setq ratex--preview-enabled (and (ratex--preview-style) t))
  (setq ratex--active-fragment (ratex-fragment-at-point))
  (if ratex-inline-preview
      (progn
        (ratex-refresh-previews t)
        (when ratex--active-fragment
          (ratex-remove-overlay (ratex--fragment-key ratex--active-fragment))))
    (ratex-clear-overlays)
    (when (and ratex--active-fragment
               (ratex--preview-enabled-p))
      (ratex--handle-preview-at-point ratex--active-fragment))))

(defun ratex-handle-post-command ()
  "Update previews only when point enters/leaves math fragments."
  (when ratex-mode
    (when (ratex--preview-enabled-p)
      (ratex--update-posframe-position))
    (let ((current (ratex--active-fragment-at-point))
          (previous ratex--active-fragment))
      (ratex-debug-log "post-command point=%s previous=%S current=%S"
                       (point) previous current)
      (unless (and previous current
                   (ratex--same-active-context-p previous current))
        (when previous
          (ratex--ensure-fragment-preview previous)))
      (when current
        (ratex-remove-overlay (ratex--fragment-key current)))
      (when (ratex--preview-enabled-p)
        (ratex--handle-preview-at-point current))
      (setq ratex--active-fragment current))))

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
                  ratex-render-cache-ttl)))
    (when ttl
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and (boundp 'ratex-mode)
                       ratex-mode
                       (hash-table-p ratex--render-cache-access))
              (let (expired)
                (maphash
                 (lambda (key last-used)
                   (when (> (- now last-used) ttl)
                     (push key expired)))
                 ratex--render-cache-access)
                (dolist (key expired)
                  (ratex--cache-delete key))))))))))

(defun ratex--image-from-response (response)
  "Build an image object from backend RESPONSE."
  (let* ((svg (alist-get 'svg response))
         (baseline (or (alist-get 'baseline response) 0.0))
         (height (max 0.001 (or (alist-get 'height response) 0.0))))
    (when svg
      (create-image
       svg
       'svg t
       :ascent (floor (* 100.0 (/ baseline height)))))))

(defun ratex--active-fragment-at-point ()
  "Return editable fragment at point, including rendered overlay fallback."
  (or (ratex-fragment-at-point)
      (ratex-overlay-fragment-at-point)
      (when (ratex--point-in-fragment-p ratex--active-fragment)
        ratex--active-fragment)))

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
          (when ratex-font-dir
            `((font_dir . ,(expand-file-name ratex-font-dir)))))))
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
      (ratex--display-response fragment-key fragment cached))
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
             (dolist (entry waiters)
               (ratex--display-if-visible
                (car entry)
                (cdr entry)
                response))))))))))

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
            (unless (ratex--display-edit-preview fragment response)
              (ratex--ensure-fragment-preview fragment)))
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
      (if (and (ratex--preview-enabled-p)
               (ratex--point-in-fragment-p fragment))
          (progn
            (ratex-remove-overlay fragment-key)
            (ratex--display-edit-preview fragment response image))
        (if ratex-inline-preview
            (ratex-show-overlay
             fragment-key
             (plist-get fragment :begin)
             (plist-get fragment :end)
             image
             (format "RaTeX %s" (if (alist-get 'cached response) "cached" "rendered"))
             fragment
             (or style 'inline))
          (ratex-remove-overlay fragment-key)))))) 



(defun ratex--preview-enabled-p ()
  "Return non-nil when the preview toggle and posframe are enabled."
  (and ratex--preview-enabled
       (ratex--preview-style)))

(defun ratex--preview-style ()
  "Return active edit preview style or nil."
  ratex-edit-preview)

(defun ratex--display-edit-preview (fragment &optional response image)
  "Display edit preview for FRAGMENT using the configured style.

When `posframe' is selected, always use posframe placement."
  (pcase (ratex--preview-style)
    ('posframe
     (ratex-debug-log "display-edit-preview style=posframe fragment=%S response=%s image=%s"
                      fragment (not (null response)) (not (null image)))
     (let ((preview-image (or image (and response (ratex--image-from-response response)))))
       (ratex--display-posframe fragment response preview-image)))
    (_ nil)))

(defun ratex--ensure-posframe-loaded ()
  "Return non-nil when posframe is available; load it if needed."
  (or (featurep 'posframe)
      (require 'posframe nil t)))

(defun ratex--display-posframe (fragment &optional response image)
  "Display IMAGE (or RESPONSE) in a posframe for FRAGMENT."
  (when (and (eq (ratex--preview-style) 'posframe)
             (ratex--ensure-posframe-loaded)
             (featurep 'posframe)
             (ratex--point-in-fragment-p fragment))
    (let ((image (or image (and response (ratex--image-from-response response)))))
      (when image
        (condition-case nil
            (let* ((size (image-size image t))
                   (ibw 8)
                   (px-w (+ (ceiling (car size)) (* 2 ibw) 2))
                   (px-h (+ (ceiling (cdr size)) (* 2 ibw) 2))
                   (override-params
                    `((width . (text-pixels . ,px-w))
                      (height . (text-pixels . ,px-h)))))
              (ratex-debug-log "try posframe fragment=%S size=%S" fragment size)
              (with-current-buffer (get-buffer-create ratex--posframe-buffer)
                (setq-local truncate-lines nil)
                (setq-local word-wrap nil)
                (erase-buffer)
                (insert (propertize " " 'display image)))
              (setq ratex--posframe-last-override-params override-params)
              (posframe-show
               ratex--posframe-buffer
               :position (point)
               :poshandler (ratex--posframe-poshandler-for-fragment fragment)
               :internal-border-width ibw
               :border-width 1
               :border-color ratex-posframe-border-color
               :background-color ratex-posframe-background-color
               :override-parameters override-params)
              (ratex--mask-edit-source fragment)
              (setq ratex--posframe-visible t)
              (setq ratex--posframe-fragment fragment)
              (ratex-debug-log "posframe success")
              t)
          (error
           (ratex-debug-log "posframe failed")
           nil))))))

(defun ratex-posframe-poshandler-point-bottom-left-corner-offset (info)
  "Position posframe 5px below `posframe-poshandler-point-bottom-left-corner`."
  (let* ((base (posframe-poshandler-point-bottom-left-corner info))
         (x (car base))
         (y (cdr base)))
    (cons x (+ y ratex--posframe-offset-y))))

(defun ratex-posframe-poshandler-point-top-left-corner-offset (info)
  "Position posframe above point, falling back to below when near screen top."
  (let* ((above-base (posframe-poshandler-point-top-left-corner info))
         (above-y (- (cdr above-base) ratex--posframe-offset-y))
         (posframe-h (or (plist-get info :posframe-height) 0)))
    (if (>= (- above-y posframe-h) 0)
        (cons (car above-base) above-y)
      ;; Not enough room above — place below the cursor instead.
      (let* ((below-base (posframe-poshandler-point-bottom-left-corner info))
             (below-y (+ (cdr below-base) ratex--posframe-display-offset-y)))
        (cons (car below-base) below-y)))))

(defun ratex--display-fragment-p (fragment)
  "Return non-nil when FRAGMENT should use display-style popup placement."
  (or (plist-get fragment :environment)
      (equal (plist-get fragment :open) "\\[")))

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
  (if (ratex--display-fragment-p fragment)
      #'ratex-posframe-poshandler-point-bottom-left-corner-upward-offset
    (or ratex-posframe-poshandler
        #'ratex-posframe-poshandler-point-bottom-left-corner-offset)))

(defun ratex--hide-posframe ()
  "Hide the posframe preview."
  (when (featurep 'posframe)
    (when ratex--posframe-visible
      (posframe-hide ratex--posframe-buffer))
    (setq ratex--posframe-visible nil)
    (setq ratex--posframe-fragment nil))
  (ratex--unmask-edit-source))

(defun ratex--hide-edit-preview ()
  "Hide the posframe edit preview."
  (ratex--hide-posframe)
  (ratex--unmask-edit-source))

(defun ratex--update-posframe-position ()
  "Keep floating preview aligned with point while editing."
  (when (and ratex--posframe-visible
             (eq (ratex--preview-style) 'posframe)
             (ratex--ensure-posframe-loaded)
             (featurep 'posframe))
    (if (ratex--point-in-fragment-p ratex--posframe-fragment)
        (condition-case nil
            (progn
              (posframe-show
               ratex--posframe-buffer
               :position (point)
               :poshandler (ratex--posframe-poshandler-for-fragment
                            ratex--posframe-fragment)
               :border-width 1
               :border-color ratex-posframe-border-color
               :background-color ratex-posframe-background-color
               :override-parameters ratex--posframe-last-override-params)
              (ratex--mask-edit-source ratex--posframe-fragment))
          (error (ratex--hide-posframe)))
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
  "Clear previews for all ratex buffers when switching buffers."
  (when (ratex--preview-style)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when ratex-mode
          (ratex--hide-edit-preview))))))

(provide 'ratex-render)

;;; ratex-render.el ends here

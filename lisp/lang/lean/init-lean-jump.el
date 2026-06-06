;;; init-lean-jump.el --- M-]/M-[ infoview-change jump for Lean -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Implements M-]/M-[ as "jump to next/previous position where the Lean
;; infoview content ($/lean/plainGoal) changes".
;;
;; Algorithm:
;;  forward : exponential probe (1, 2, 4, …) within the current declaration's
;;            syntactic bounds, then bisect to the exact boundary.
;;  backward: find the start of the current same-goal run within bounds;
;;            if already at the start, jump to the previous run's start.
;;
;; Syntactic fallback (lean-jump-syntactic-fallback):
;;  When goal info is nil at point (broken tactic, e.g. `rw [a, broken, b]'),
;;  the goal-based search cannot proceed.  The fallback steps through tactic
;;  argument separators (`,', `[', `]', `;', newline) using text-only search,
;;  so M-]/M-[ remain usable while debugging broken commands.
;;
;; Scoping: every search is capped to the current Lean declaration
;; (theorem / lemma / def / example / …).  This prevents the exponential
;; probing from escaping across declarations or into the file header when
;; the server returns no info for error / incomplete positions.
;;
;; Caching: probed positions → fingerprint, stored in a buffer-local hash
;; table keyed by buffer-chars-modified-tick.  Nil responses (server not
;; ready, tactic error) are cached too, so a flaky position is only probed
;; once per edit rather than hammering the server on every step.

;;; Code:

(require 'cl-lib)

(declare-function eglot-current-server "eglot" ())
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot--TextDocumentPositionParams "eglot" ())
(declare-function eglot--pos-to-lsp-position "eglot" (&optional pos))
(declare-function jsonrpc-request "jsonrpc" (connection method params &rest args))
(declare-function my/register-jump-handler "init-funcs")

;;; ── Customization ────────────────────────────────────────────────────────────

(defgroup lean-jump nil
  "Infoview-change jump for Lean."
  :group 'lean)

(defcustom lean-jump-include-term-goal nil
  "When non-nil, also consider $/lean/plainTermGoal changes as boundaries.
Default nil: only stop where the tactic goal state (plainGoal) changes.
Set to t for sub-expression granularity (stops at every identifier)."
  :type 'boolean
  :group 'lean-jump)

(defcustom lean-jump-request-timeout 1.5
  "Seconds to wait for each LSP request when probing."
  :type 'number
  :group 'lean-jump)

(defcustom lean-jump-syntactic-fallback t
  "When non-nil, fall back to tactic-argument stepping when goal info is
unavailable (e.g. a broken `rw'/`simp' lemma).
Keeps M-]/M-[ usable while debugging broken tactics."
  :type 'boolean
  :group 'lean-jump)

;;; ── Cache ────────────────────────────────────────────────────────────────────

(defvar-local lean-jump--cache nil)
(defvar-local lean-jump--cache-tick nil)
;; Sentinel distinguishing "not yet cached" from "cached as nil".
(defconst lean-jump--uncached (make-symbol "lean-jump-uncached"))

(defun lean-jump--cache ()
  "Return the cache, resetting it when the buffer has been modified."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq tick lean-jump--cache-tick)
      (setq lean-jump--cache-tick tick
            lean-jump--cache (make-hash-table :test #'eql)))
    lean-jump--cache))

;;; ── Declaration bounds ───────────────────────────────────────────────────────

(defconst lean-jump--decl-re
  "^\\(?:theorem\\|lemma\\|def\\|abbrev\\|example\\|instance\\|class\\|structure\\|noncomputable\\|private\\|protected\\)"
  "Regex matching the first word of a top-level Lean declaration.")

(defun lean-jump--decl-bounds ()
  "Return (BEG . END) of the current Lean declaration, or buffer bounds.
BEG is the start of the declaration keyword line.  END is the start of the
next declaration, or point-max."
  (save-excursion
    (let* ((beg (progn
                  ;; Move past the current line so we do not match a decl
                  ;; keyword that the cursor itself sits on, then search back.
                  (end-of-line)
                  (if (re-search-backward lean-jump--decl-re nil t)
                      (point)
                    (point-min))))
           (end (progn
                  (goto-char (1+ beg))
                  (if (re-search-forward lean-jump--decl-re nil t)
                      (match-beginning 0)
                    (point-max)))))
      (cons beg end))))

;;; ── Fingerprint ──────────────────────────────────────────────────────────────

(defun lean-jump--info-at (pos)
  "Return the goal fingerprint string at POS, or nil when unavailable.
Both valid and nil results are cached for the current modification tick."
  (let* ((cache  (lean-jump--cache))
         (cached (gethash pos cache lean-jump--uncached)))
    (if (not (eq cached lean-jump--uncached))
        cached  ; already computed (may be nil)
      (let ((result
             (when-let* ((server (ignore-errors (eglot-current-server))))
               (save-excursion
                 (goto-char pos)
                 (condition-case nil
                     (let* ((params (eglot--TextDocumentPositionParams))
                            (goal
                             (ignore-errors
                               (jsonrpc-request server
                                                :$/lean/plainGoal
                                                params
                                                :timeout lean-jump-request-timeout)))
                            (rendered (plist-get goal :rendered)))
                       (if lean-jump-include-term-goal
                           (let* ((tg (ignore-errors
                                        (jsonrpc-request server
                                                         :$/lean/plainTermGoal
                                                         params
                                                         :timeout lean-jump-request-timeout)))
                                  (tgtext (plist-get tg :goal)))
                             (when (or rendered tgtext)
                               (concat (or rendered "") "\0" (or tgtext ""))))
                         ;; goal-only mode: nil goal → nil result (no info)
                         rendered))
                   (error nil))))))
        (puthash pos result cache)
        result))))

;;; ── Boundary search ──────────────────────────────────────────────────────────

(defun lean-jump--find-forward (base-fp start end)
  "Return the first position in (START END) where goal differs from BASE-FP.
END is exclusive (start of next declaration or point-max); the search never
probes or returns END itself.  Nil-fingerprint positions are skipped.
Returns nil when no boundary is found."
  (let ((last    (1- end))   ; last valid position inside this declaration
        (step    1)
        (probe   start)
        (changed nil))
    (while (and (not changed) (< probe last))
      (setq probe (min last (+ probe step)))
      (let ((fp (lean-jump--info-at probe)))
        (when (and fp (not (equal fp base-fp)))
          (setq changed probe)))
      (setq step (* step 2)))
    (when changed
      ;; Bisect [start .. changed] to find the exact first differing position.
      (let ((lo start) (hi changed))
        (while (> (- hi lo) 1)
          (let* ((mid (/ (+ lo hi) 2))
                 (fp  (lean-jump--info-at mid)))
            (if (and fp (not (equal fp base-fp)))
                (setq hi mid)
              (setq lo mid))))
        ;; Land on the first non-whitespace character of the new region,
        ;; clamped to last so we never land on the next declaration.
        (save-excursion
          (goto-char hi)
          (skip-chars-forward " \t\n")
          (min (point) last))))))

(defun lean-jump--run-start (fp pos beg)
  "Return the leftmost position >= BEG in POS's same-fingerprint run.
Nil-fingerprint positions are treated as boundaries (they mark the edge
of the region where the server has valid goal info).
Returns POS itself when no backward boundary is found."
  (let ((step  1)
        (probe pos)
        (bnd   nil))          ; bnd = (lo . hi) where lo has different/nil fp
    (while (and (not bnd) (> probe beg))
      (let* ((cand (max beg (- probe step)))
             (cfp  (lean-jump--info-at cand)))
        (if (or (null cfp) (not (equal cfp fp)))
            (setq bnd (cons cand probe))
          (setq probe cand)))
      (setq step (* step 2)))
    (if bnd
        ;; Bisect: find the leftmost position that still has fp.
        (let ((lo (car bnd)) (hi (cdr bnd)))
          (while (> (- hi lo) 1)
            (let* ((mid (/ (+ lo hi) 2))
                   (mfp (lean-jump--info-at mid)))
              (if (and mfp (equal mfp fp))
                  (setq hi mid)
                (setq lo mid))))
          hi)
      ;; No boundary found within [beg..pos]: run extends to beg.
      (max beg probe))))

;;; ── Syntactic fallback (text-only, no LSP) ───────────────────────────────────

(defconst lean-jump--sep-re "[,;]\\|\\[\\|\\]\\|\n"
  "Regexp matching tactic argument separators used by the syntactic fallback.")

(defun lean-jump--syntactic-forward (pos end)
  "Return the next tactic-argument start after POS, within END.
Searches for a separator (`,', `;', `[', `]', newline) then skips whitespace
to land on a non-whitespace character.  Returns nil when no such character
exists within END (e.g. after the closing `]' of a tactic list)."
  (save-excursion
    (goto-char pos)
    (when (re-search-forward lean-jump--sep-re end t)
      (skip-chars-forward " \t\n" end)
      (let* ((dest (point))
             (ch   (char-after)))
        (when (and (> dest pos) (<= dest end)
                   ch (not (memq ch '(?\s ?\t ?\n ?\r))))
          dest)))))

(defun lean-jump--syntactic-backward (pos beg)
  "Return the previous tactic-argument start before POS, within BEG.
If not at the current token start, returns that start.
If already at a token start, returns the previous token's start."
  (let ((cur (save-excursion
               (goto-char pos)
               (if (re-search-backward lean-jump--sep-re beg t)
                   (progn (forward-char 1)
                          (skip-chars-forward " \t\n")
                          (point))
                 (goto-char beg)
                 (skip-chars-forward " \t\n")
                 (point)))))
    (cond
     ((> cur pos) nil)
     ;; Not at token start yet → go there.
     ((< cur pos) (when (>= cur beg) cur))
     ;; Already at token start: find the separator before this token,
     ;; then the separator before the previous token.
     (t
      (save-excursion
        (goto-char pos)
        (when (re-search-backward lean-jump--sep-re beg t)
          (let ((dest
                 (if (re-search-backward lean-jump--sep-re beg t)
                     (progn (forward-char 1)
                            (skip-chars-forward " \t\n")
                            (point))
                   (goto-char beg)
                   (skip-chars-forward " \t\n")
                   (point))))
            (when (and (>= dest beg) (< dest pos)) dest))))))))

;;; ── Interactive commands ─────────────────────────────────────────────────────

(defun lean-jump-forward ()
  "Jump to the next position where the Lean goal changes, within the declaration.
Falls back to syntactic argument stepping when goal info is unavailable."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "No Lean LSP server connected"))
  (let* ((pos    (point))
         (bounds (lean-jump--decl-bounds))
         (end    (cdr bounds))
         (last   (1- end))
         (fp     (lean-jump--info-at pos))
         (dest   (and fp (lean-jump--find-forward fp pos end))))
    (cond
     (dest (goto-char dest))
     ((and lean-jump-syntactic-fallback
           (setq dest (lean-jump--syntactic-forward pos last)))
      (goto-char dest))
     (fp (user-error "No goal change found forward in this declaration"))
     (t  (user-error "No Lean goal info at point (incomplete tactic or error)")))))

(defun lean-jump-backward ()
  "Jump to the start of the previous Lean goal region, within the declaration.
Falls back to syntactic argument stepping when goal info is unavailable."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "No Lean LSP server connected"))
  (let* ((pos    (point))
         (bounds (lean-jump--decl-bounds))
         (beg    (car bounds))
         (fp     (lean-jump--info-at pos))
         (dest
          (when fp
            (let ((run-start (lean-jump--run-start fp pos beg)))
              (if (< run-start pos)
                  run-start
                (let* ((prev    (max beg (1- run-start)))
                       (prev-fp (lean-jump--info-at prev)))
                  (when (and prev-fp (not (equal prev-fp fp)))
                    (lean-jump--run-start prev-fp prev beg))))))))
    (cond
     (dest (goto-char dest))
     ((and lean-jump-syntactic-fallback
           (setq dest (lean-jump--syntactic-backward pos beg)))
      (goto-char dest))
     (fp (user-error "No goal change found backward in this declaration"))
     (t  (user-error "No Lean goal info at point (incomplete tactic or error)")))))

;;; ── Registration ─────────────────────────────────────────────────────────────

(with-eval-after-load 'init-funcs
  (when (fboundp 'my/register-jump-handler)
    (my/register-jump-handler 'lean-mode
                              :forward  #'lean-jump-forward
                              :backward #'lean-jump-backward)))

(provide 'init-lean-jump)
;;; init-lean-jump.el ends here

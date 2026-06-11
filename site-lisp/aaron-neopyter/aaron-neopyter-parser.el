;;; aaron-neopyter-parser.el --- Jupytext percent-format cell parser -*- lexical-binding: t -*-

;;; Commentary:
;; Parses Jupytext percent-format scripts (# %% cell separators).
;; Mirrors the logic in lua/neopyter/parser/percent.lua from the Neopyter upstream.
;;
;; Supported patterns:
;;   # %%                     → code cell
;;   # %% [markdown]          → markdown cell
;;   # %% [md]                → markdown cell
;;   # %% [raw]               → raw cell
;;   # %% title [markdown]    → markdown cell with title
;;   # %% tags=["foo"]        → code cell with metadata string
;;
;; The first cell before any separator has no_separator=t and defaults to code.

;;; Code:

(require 'cl-lib)

(cl-defstruct (aaron-neopyter-cell
               (:constructor aaron-neopyter--make-cell)
               (:copier nil))
  "One parsed Jupytext percent cell."
  (index       0)        ; integer: 0-based position in the notebook
  (type        'code)    ; symbol: 'code | 'markdown | 'raw
  (start       0)        ; integer: buffer position of separator line (or 1 for no_separator)
  (end         0)        ; integer: buffer position of last char of cell (inclusive)
  (body-start  0)        ; integer: buffer position of first content char
  (body-end    0)        ; integer: buffer position of last content char
  (text        "")       ; string: cell content (without separator line)
  (title       nil)      ; string or nil
  (metadata    nil)      ; string or nil (raw metadata text from separator)
  (no-separator nil))    ; boolean: first cell has no # %% line

;;; Parsing helpers

(defconst aaron-neopyter-parser--sep-re
  "^# %%\\(\\( .*\\)?\\)$"
  "Regexp matching a percent-format cell separator line.
Group 1 (and 2) is everything after '# %%', may be empty.")

(defun aaron-neopyter-parser--parse-sep-suffix (suffix)
  "Parse the trailing SUFFIX of a separator (the part after '# %%').
Return a plist (:type TYPE :title TITLE :metadata META)."
  (if (or (null suffix) (string-match-p "\\`[[:space:]]*\\'" suffix))
      (list :type 'code :title nil :metadata nil)
    (let* ((s (string-trim suffix))
           type title metadata)
      ;; Check for [type] marker
      (if (string-match "\\[\\([^]]+\\)\\]" s)
          (let* ((bracket-start (match-beginning 0))
                 (bracket-end   (match-end 0))
                 (tag           (downcase (string-trim (match-string 1 s)))))
            (setq type (cond ((or (string= tag "md") (string= tag "markdown")) 'markdown)
                             ((string= tag "raw") 'raw)
                             (t 'code)))
            ;; text before bracket is the title
            (let ((pre (string-trim (substring s 0 bracket-start))))
              (setq title (and (not (string-empty-p pre)) pre)))
            ;; text after bracket is metadata
            (let ((post (string-trim (substring s bracket-end))))
              (setq metadata (and (not (string-empty-p post)) post))))
        ;; No [type] marker: check for keyword pairs like tags=[...]
        (setq type 'code)
        (setq title nil)
        (setq metadata (and (not (string-empty-p s)) s)))
      (list :type type :title title :metadata metadata))))

(defun aaron-neopyter-parser--line-end-pos (pos)
  "Return buffer position of end of line containing POS."
  (save-excursion
    (goto-char pos)
    (line-end-position)))

(defun aaron-neopyter-parser--line-start-pos (pos)
  "Return buffer position of start of line containing POS."
  (save-excursion
    (goto-char pos)
    (line-beginning-position)))

(defun aaron-neopyter-parser--markdown-uncomment-line (line)
  "Remove one script comment prefix from markdown LINE.
Jupytext percent-format Python/R markdown cells are usually written as
comments so the language server can still parse the script buffer."
  (if (string-match "\\`#\\(?:[ \t]\\)?\\(.*\\)\\'" line)
      (match-string 1 line)
    line))

(defun aaron-neopyter-parser--markdown-rpc-text (text)
  "Return markdown TEXT as it should be sent to JupyterLab."
  (mapconcat #'aaron-neopyter-parser--markdown-uncomment-line
             (split-string text "\n")
             "\n"))

;;; Main entry point

(defun aaron-neopyter-parse-buffer ()
  "Parse the current buffer as a Jupytext percent-format script.
Return a list of `aaron-neopyter-cell' structs in order."
  (save-excursion
    (save-restriction
      (widen)
      (let ((cells '())
            (index 0)
            ;; We accumulate lines between separators
            current-sep-start current-sep-end
            current-type current-title current-meta current-no-sep)
        ;; Helper: close and push the current cell up to END-POS
        (cl-flet ((push-cell (body-start body-end)
                    (let* ((text (if (and (> body-end 0) (>= body-end body-start))
                                    (buffer-substring-no-properties body-start body-end)
                                  ""))
                           ;; Trim trailing whitespace/newlines from text
                           (trimmed (string-trim-right text))
                           (cell (aaron-neopyter--make-cell
                                  :index index
                                  :type (or current-type 'code)
                                  :start (or current-sep-start body-start)
                                  :end body-end
                                  :body-start body-start
                                  :body-end body-end
                                  :text trimmed
                                  :title current-title
                                  :metadata current-meta
                                  :no-separator current-no-sep)))
                      (push cell cells)
                      (cl-incf index))))
          ;; Does the buffer start before any separator?
          (goto-char (point-min))
          (if (not (looking-at aaron-neopyter-parser--sep-re))
              ;; First cell has no separator
              (progn
                (setq current-sep-start 1
                      current-sep-end   1
                      current-type      'code
                      current-title     nil
                      current-meta      nil
                      current-no-sep    t))
            ;; Buffer starts immediately with a separator - no pre-sep content
            (setq current-sep-start nil))

          ;; Walk through the buffer looking for separators
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line-start (line-beginning-position))
                  (line-end   (line-end-position)))
              (if (looking-at aaron-neopyter-parser--sep-re)
                  (let* ((suffix  (match-string 1))
                         (parsed  (aaron-neopyter-parser--parse-sep-suffix suffix))
                         (sep-end (line-end-position)))
                    ;; Close previous cell (if any)
                    (when current-sep-start
                      ;; Previous body ran from current-sep-end to just before this line
                      (let ((body-start (if current-no-sep
                                            current-sep-start
                                          (1+ current-sep-end)))
                            (body-end   (1- line-start)))
                        (when (>= body-end body-start)
                          (push-cell body-start body-end))
                        (unless (>= body-end body-start)
                          ;; Empty cell
                          (push-cell body-start body-start))))
                    ;; Start new cell
                    (setq current-sep-start line-start
                          current-sep-end   sep-end
                          current-type      (plist-get parsed :type)
                          current-title     (plist-get parsed :title)
                          current-meta      (plist-get parsed :metadata)
                          current-no-sep    nil))
                ;; Not a separator line - continue
                nil)
              (forward-line 1)))

          ;; Close the last cell
          (when current-sep-start
            (let ((body-start (if current-no-sep
                                  current-sep-start
                                (1+ current-sep-end)))
                  (body-end   (point-max)))
              (push-cell body-start body-end))))

        (nreverse cells)))))

;;; Point → cell lookup

(defun aaron-neopyter-cell-at-pos (pos cells)
  "Return the `aaron-neopyter-cell' in CELLS that contains buffer position POS.
Return nil if CELLS is empty."
  (when cells
    (let ((result (car cells)))
      (dolist (cell cells result)
        (when (<= (aaron-neopyter-cell-start cell) pos)
          (setq result cell))))))

(defun aaron-neopyter-cell-index-at-pos (pos cells)
  "Return the 0-based index of the cell containing POS in CELLS, or 0."
  (let ((cell (aaron-neopyter-cell-at-pos pos cells)))
    (if cell (aaron-neopyter-cell-index cell) 0)))

(defun aaron-neopyter-cells-to-rpc (cells)
  "Convert CELLS list to a vector of alist maps for fullSync RPC.
Each map has string keys \"source\" and \"cell_type\"."
  (apply #'vector
         (mapcar (lambda (cell)
                   (let* ((raw-type (aaron-neopyter-cell-type cell))
                          (type-str (pcase raw-type
                                      ('markdown "markdown")
                                      ('raw      "raw")
                                      (_         "code")))
                          (source (if (eq raw-type 'markdown)
                                      (aaron-neopyter-parser--markdown-rpc-text
                                       (aaron-neopyter-cell-text cell))
                                    (aaron-neopyter-cell-text cell))))
                     (list (cons "source"    source)
                           (cons "cell_type" type-str))))
                 cells)))

(provide 'aaron-neopyter-parser)
;;; aaron-neopyter-parser.el ends here

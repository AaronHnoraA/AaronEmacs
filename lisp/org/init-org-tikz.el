;;; init-org-tikz.el --- Org TikZ and GeoGebra helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicit Org helpers for math graphics authoring.  The GeoGebra cleaner is
;; command-driven and the raw-code fold overlay is only installed for generated
;; blocks, so ordinary Org editing has no new background cost.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup my/org-tikz nil
  "Org helpers for TikZ, tikz-cd, and GeoGebra exports."
  :group 'org)

(defcustom my/org-tikz-geogebra-include-points t
  "When non-nil, cleaned GeoGebra TikZ keeps point markers."
  :type 'boolean
  :group 'my/org-tikz)

(defcustom my/org-tikz-geogebra-include-labels t
  "When non-nil, cleaned GeoGebra TikZ keeps point labels."
  :type 'boolean
  :group 'my/org-tikz)

(defcustom my/org-tikz-geogebra-round-coordinates t
  "When non-nil, cleaned GeoGebra TikZ rounds numeric coordinates."
  :type 'boolean
  :group 'my/org-tikz)

(defcustom my/org-tikz-geogebra-coordinate-precision 3
  "Number of decimal places kept when cleaning GeoGebra TikZ."
  :type 'integer
  :group 'my/org-tikz)

(defvar-local my/org-tikz--raw-fold-overlays nil
  "Fold overlays hiding generated GeoGebra raw TikZ blocks.")

(declare-function my/org-latex--preview-range "init-org-latex"
                  (beg end &optional origin priority))

(defun my/org-tikz--cleanup-fold-overlays ()
  "Drop dead GeoGebra raw fold overlays from the current buffer."
  (setq my/org-tikz--raw-fold-overlays
        (seq-filter #'overlay-buffer my/org-tikz--raw-fold-overlays)))

(defun my/org-tikz--round-number (number)
  "Round NUMBER using `my/org-tikz-geogebra-coordinate-precision'."
  (let* ((precision (max 0 my/org-tikz-geogebra-coordinate-precision))
         (scale (expt 10 precision)))
    (/ (round (* number scale)) (float scale))))

(defun my/org-tikz--number-string (number)
  "Return NUMBER as a compact decimal string."
  (let* ((precision (max 0 my/org-tikz-geogebra-coordinate-precision))
         (text (format (format "%%.%df" precision) number)))
    (setq text (replace-regexp-in-string "\\.?0+\\'" "" text))
    (if (string-empty-p text) "0" text)))

(defun my/org-tikz--round-number-string (value)
  "Return numeric string VALUE rounded for GeoGebra output."
  (condition-case nil
      (my/org-tikz--number-string
       (my/org-tikz--round-number (string-to-number value)))
    (error value)))

(defun my/org-tikz--format-coordinate (coord &optional round)
  "Format numeric TikZ COORD.
When ROUND is non-nil, numeric coordinates are rounded."
  (let ((trimmed (string-trim coord)))
    (if (string-match
         "\\`(?[ \t]*\\([^,()]+\\)[ \t]*,[ \t]*\\([^,()]+\\)[ \t]*)?\\'"
         trimmed)
        (let ((x (match-string 1 trimmed))
              (y (match-string 2 trimmed)))
          (if round
              (format "(%s,%s)"
                      (my/org-tikz--round-number-string x)
                      (my/org-tikz--round-number-string y))
            (format "(%s,%s)" (string-trim x) (string-trim y))))
      trimmed)))

(defun my/org-tikz--parse-coordinate (coord)
  "Return numeric cons cell for COORD, or nil."
  (let ((trimmed (string-trim coord)))
    (when (string-match
           "\\`(?[ \t]*\\([-+0-9.eE]+\\)[ \t]*,[ \t]*\\([-+0-9.eE]+\\)[ \t]*)?\\'"
           trimmed)
      (cons (string-to-number (match-string 1 trimmed))
            (string-to-number (match-string 2 trimmed))))))

(defun my/org-tikz--same-coordinate-p (left right)
  "Return non-nil when LEFT and RIGHT are the same numeric coordinate."
  (let ((a (my/org-tikz--parse-coordinate left))
        (b (my/org-tikz--parse-coordinate right)))
    (and a b
         (< (abs (- (car a) (car b))) 0.001)
         (< (abs (- (cdr a) (cdr b))) 0.001))))

(defun my/org-tikz--sanitize-coordinate-name (label index)
  "Return a TikZ coordinate name derived from LABEL or INDEX."
  (let* ((plain (replace-regexp-in-string "\\\\[[:alpha:]]+" "" label))
         (plain (replace-regexp-in-string "[{}$^_[:space:]]+" "" plain)))
    (if (string-match-p "\\`[[:alpha:]][[:alnum:]_-]*\\'" plain)
        plain
      (format "P%d" index))))

(defun my/org-tikz--line-style (style)
  "Return a compact TikZ line style from GeoGebra STYLE."
  (let ((style (or style "")))
    (cond
     ((string-match-p "dash pattern=on 1pt off 1pt on 1pt off 4pt" style)
      "dash dot")
     ((or (string-match-p "dash pattern=on 1pt off 1pt" style)
          (string-match-p "\\bdash" style))
      "dashed")
     ((string-match-p "\\bdotted\\b" style)
      "dotted")
     (t ""))))

(defun my/org-tikz--extract-tikzpicture (code)
  "Return the TikZ picture body from CODE, or CODE when no wrapper exists."
  (if (string-match "\\\\begin{tikzpicture}\\(\\(?:.\\|\n\\)*?\\)\\\\end{tikzpicture}" code)
      (match-string 1 code)
    code))

(defun my/org-tikz--collect-regexp (regexp text)
  "Return all matches of REGEXP in TEXT as match-string vectors."
  (let ((start 0)
        matches)
    (while (string-match regexp text start)
      (push (cl-loop for i from 0 below (/ (length (match-data)) 2)
                     collect (match-string i text))
            matches)
      (setq start (max (1+ (match-beginning 0)) (match-end 0))))
    (nreverse matches)))

(defun my/org-tikz--collect-points (code round)
  "Return point plists extracted from GeoGebra CODE."
  (let* ((point-regexp "\\\\draw[ \t\n]*\\[fill=[^]]+\\][ \t\n]*(\\([^)]*\\))[ \t\n]*circle[ \t\n]*(\\([^)]*\\));")
         (label-regexp "\\\\draw\\[color=[^]]+\\][ \t\n]*(\\([^)]*\\))[ \t\n]*node[ \t\n]*{\\$\\([^$]+\\)\\$};")
         (points (my/org-tikz--collect-regexp point-regexp code))
         (labels (seq-remove
                  (lambda (match)
                    (string-match-p "\\\\textrm{\\\\degre}" (or (nth 2 match) "")))
                  (my/org-tikz--collect-regexp label-regexp code)))
         result)
    (cl-loop for point in points
             for index from 1
             for label-match = (nth (1- index) labels)
             for raw-label = (or (nth 2 label-match) (format "P%d" index))
             for name = (my/org-tikz--sanitize-coordinate-name raw-label index)
             for coord = (my/org-tikz--format-coordinate (nth 1 point) round)
             do (push (list :name name
                            :label raw-label
                            :coord coord)
                      result))
    (nreverse result)))

(defun my/org-tikz--point-name-at (coord points)
  "Return point name at COORD from POINTS, or nil."
  (seq-some
   (lambda (point)
     (and (my/org-tikz--same-coordinate-p coord (plist-get point :coord))
          (plist-get point :name)))
   points))

(defun my/org-tikz--point-ref-or-coordinate (coord points round)
  "Return a named point reference for COORD, or a formatted coordinate."
  (or (my/org-tikz--point-name-at coord points)
      (my/org-tikz--format-coordinate coord round)))

(defun my/org-tikz--format-point-target (target)
  "Return TARGET as a TikZ coordinate reference."
  (if (string-match-p "\\`[[:alpha:]][[:alnum:]_-]*\\'" target)
      (format "(%s)" target)
    target))

(defun my/org-tikz--smart-label-position (point points)
  "Return a basic label position for POINT relative to POINTS."
  (let* ((coord (my/org-tikz--parse-coordinate (plist-get point :coord)))
         (x (or (car coord) 0.0))
         (y (or (cdr coord) 0.0))
         (near-right nil)
         (near-left nil)
         (near-above nil)
         (near-below nil))
    (dolist (other points)
      (unless (eq other point)
        (when-let* ((other-coord (my/org-tikz--parse-coordinate
                                  (plist-get other :coord))))
          (let ((dx (- (car other-coord) x))
                (dy (- (cdr other-coord) y)))
            (when (and (> dx 0) (< (abs dx) 0.8) (< (abs dy) 0.8))
              (setq near-right t))
            (when (and (< dx 0) (< (abs dx) 0.8) (< (abs dy) 0.8))
              (setq near-left t))
            (when (and (> dy 0) (< (abs dx) 0.8) (< (abs dy) 0.8))
              (setq near-above t))
            (when (and (< dy 0) (< (abs dx) 0.8) (< (abs dy) 0.8))
              (setq near-below t))))))
    (cond
     ((not near-above) "above")
     ((not near-right) "right")
     ((not near-left) "left")
     ((not near-below) "below")
     (t "above right"))))

(defun my/org-tikz--collect-lines (code points round)
  "Return compact line draw statements extracted from CODE."
  (let* ((regexp "\\\\draw[ \t\n]*\\(\\[[^]]+\\]\\)?[ \t\n]*(\\([^)]*\\))[ \t\n]*--[ \t\n]*(\\([^)]*\\));")
         (matches (my/org-tikz--collect-regexp regexp code))
         (seen (make-hash-table :test 'equal))
         result)
    (dolist (match matches)
      (let* ((style (my/org-tikz--line-style (nth 1 match)))
             (left (my/org-tikz--point-ref-or-coordinate (nth 2 match) points round))
             (right (my/org-tikz--point-ref-or-coordinate (nth 3 match) points round))
             (key (mapconcat #'identity (sort (list left right) #'string<) "--")))
        (unless (gethash key seen)
          (puthash key t seen)
          (push (format "  \\draw%s %s -- %s;"
                        (if (string-empty-p style) "" (format "[%s]" style))
                        (my/org-tikz--format-point-target left)
                        (my/org-tikz--format-point-target right))
                result))))
    (nreverse result)))

(defun my/org-tikz--collect-circles (code points round)
  "Return compact circle draw statements extracted from CODE."
  (let* ((regexp "\\\\draw[ \t\n]*\\[\\([^]]*line[^]]*\\)\\][ \t\n]*(\\([^)]*\\))[ \t\n]*circle[ \t\n]*(\\([^)]*\\));")
         (matches (my/org-tikz--collect-regexp regexp code))
         result)
    (dolist (match matches)
      (let* ((style (my/org-tikz--line-style (nth 1 match)))
             (center (my/org-tikz--point-ref-or-coordinate (nth 2 match) points round))
             (radius (if round
                         (my/org-tikz--round-number-string (nth 3 match))
                       (string-trim (nth 3 match)))))
        (push (format "  \\draw%s %s circle (%s);"
                      (if (string-empty-p style) "" (format "[%s]" style))
                      (my/org-tikz--format-point-target center)
                      radius)
              result)))
    (nreverse result)))

(defun my/org-tikz--collect-ellipses (code points round)
  "Return compact ellipse draw statements extracted from CODE."
  (let* ((regexp "\\\\draw[ \t\n]*\\[\\([^]]*\\)\\][ \t\n]*(\\([^)]*\\))[ \t\n]*ellipse[ \t\n]*(\\([^)]*\\));")
         (matches (my/org-tikz--collect-regexp regexp code))
         result)
    (dolist (match matches)
      (let* ((options (nth 1 match))
             (style (my/org-tikz--line-style options))
             (center (my/org-tikz--point-ref-or-coordinate (nth 2 match) points round))
             (radii (nth 3 match))
             (parts (split-string radii "[ \t]+and[ \t]+" t))
             (x-radius (or (car parts) radii))
             (y-radius (or (cadr parts) radii))
             (rotation (and (string-match "rotate around={[ \t]*\\([^:}]+\\):\\([^}]+\\)}" options)
                            (format "rotate around={%s:%s}"
                                    (match-string 1 options)
                                    (match-string 2 options))))
             (draw-options (delq nil (list (unless (string-empty-p style) style)
                                            rotation))))
        (push (format "  \\draw%s %s ellipse (%s and %s);"
                      (if draw-options
                          (format "[%s]" (string-join draw-options ", "))
                        "")
                      (my/org-tikz--format-point-target center)
                      (string-trim x-radius)
                      (string-trim y-radius))
              result)))
    (nreverse result)))

(defun my/org-tikz--collect-functions (code round)
  "Return compact function plot statements extracted from CODE."
  (let* ((clip-regexp "\\\\clip[ \t\n]*(\\([^,]+\\),\\([^)]*\\))[ \t\n]*rectangle[ \t\n]*(\\([^,]+\\),\\([^)]*\\));")
         (plot-regexp "\\\\draw\\[\\([^]]*\\)\\][ \t\n]*plot[ \t\n]*(\\\\x,{\\([^}]+\\)})")
         (clip (car (my/org-tikz--collect-regexp clip-regexp code)))
         (plots (my/org-tikz--collect-regexp plot-regexp code))
         result)
    (when plots
      (push "  \\begin{scope}" result)
      (when clip
        (let ((left (my/org-tikz--format-coordinate
                     (format "(%s,%s)" (nth 1 clip) (nth 2 clip)) round))
              (right (my/org-tikz--format-coordinate
                      (format "(%s,%s)" (nth 3 clip) (nth 4 clip)) round)))
          (push (format "    \\clip%s rectangle %s;" left right) result)))
      (dolist (plot plots)
        (let* ((options (nth 1 plot))
               (style (my/org-tikz--line-style options))
               (domain (if (string-match "domain=\\([^]:,]+\\):\\([^],]+\\)" options)
                           (format "domain=%s:%s" (match-string 1 options) (match-string 2 options))
                         "domain=-5:5"))
               (draw-options (string-join (delq nil (list domain
                                                           (unless (string-empty-p style) style)
                                                           "smooth"
                                                           "samples=100"))
                                          ", ")))
          (push (format "    \\draw[%s] plot (\\x,{%s});"
                        draw-options
                        (nth 2 plot))
                result)))
      (push "  \\end{scope}" result))
    (nreverse result)))

(defun my/org-tikz--collect-angle-labels (code round)
  "Return angle label draw statements extracted from CODE."
  (let* ((regexp "\\\\draw\\[color=[^]]+\\][ \t\n]*(\\([^)]*\\))[ \t\n]*node[ \t\n]*{\\$\\([^}]+\\)\\\\textrm{\\\\degre}\\$};")
         (matches (my/org-tikz--collect-regexp regexp code))
         result)
    (dolist (match matches)
      (let ((coord (my/org-tikz--format-coordinate (nth 1 match) round))
            (label (string-trim (nth 2 match))))
        (push (format "  \\draw %s node {$%s^{\\circ}$};" coord label)
              result)))
    (nreverse result)))

(defun my/org-tikz--collect-text-labels (code round)
  "Return generic text label draw statements extracted from CODE."
  (let* ((regexp "\\\\draw[ \t\n]*(\\([^)]*\\))[ \t\n]*node[ \t\n]*\\[\\([^]]+\\)\\][ \t\n]*{\\([^}]+\\)};")
         (matches (my/org-tikz--collect-regexp regexp code))
         result)
    (dolist (match matches)
      (let ((coord (my/org-tikz--format-coordinate (nth 1 match) round))
            (options (string-trim (nth 2 match)))
            (content (string-trim (nth 3 match))))
        (push (format "  \\draw %s node[%s] {%s};" coord options content)
              result)))
    (nreverse result)))

(defun my/org-tikz--collect-angle-fills (code points round)
  "Return compact angle-fill statements extracted from CODE."
  (let* ((regexp "\\\\draw[ \t\n]*\\[shift={(\\([^,]+\\),\\([^)]*\\))}[^]]*\\][ \t\n]*(0,0)[ \t\n]*--[ \t\n]*(\\([^:]+\\):\\([^)]*\\))[ \t\n]*arc[ \t\n]*(\\([^:]+\\):\\([^:]+\\):\\([^)]*\\))[ \t\n]*--[ \t\n]*cycle;")
         (matches (my/org-tikz--collect-regexp regexp code))
         result)
    (dolist (match matches)
      (let* ((center-raw (format "(%s,%s)" (nth 1 match) (nth 2 match)))
             (center (my/org-tikz--point-ref-or-coordinate center-raw points round))
             (start (if round (my/org-tikz--round-number-string (nth 3 match)) (nth 3 match)))
             (radius (if round (my/org-tikz--round-number-string (nth 4 match)) (nth 4 match)))
             (arc-start (if round (my/org-tikz--round-number-string (nth 5 match)) (nth 5 match)))
             (arc-end (if round (my/org-tikz--round-number-string (nth 6 match)) (nth 6 match)))
             (arc-radius (if round (my/org-tikz--round-number-string (nth 7 match)) (nth 7 match))))
        (push (format "  \\fill[shift={%s}, gray!30] (0,0) -- (%s:%s) arc (%s:%s:%s) -- cycle;"
                      (my/org-tikz--format-point-target center)
                      start radius arc-start arc-end arc-radius)
              result)))
    (nreverse result)))

(defun my/org-tikz-clean-geogebra-code (code)
  "Return cleaned TikZ code for GeoGebra exported CODE."
  (let* ((round my/org-tikz-geogebra-round-coordinates)
         (body (my/org-tikz--extract-tikzpicture code))
         (points (my/org-tikz--collect-points body round))
         (functions (my/org-tikz--collect-functions body round))
         (lines (my/org-tikz--collect-lines body points round))
         (circles (my/org-tikz--collect-circles body points round))
         (ellipses (my/org-tikz--collect-ellipses body points round))
         (angle-fills (my/org-tikz--collect-angle-fills body points round))
         (angle-labels (my/org-tikz--collect-angle-labels body round))
         (text-labels (my/org-tikz--collect-text-labels body round))
         (out '("\\begin{tikzpicture}[scale=1]")))
    (when points
      (setq out (append out '("  % Coordinate points")))
      (dolist (point points)
        (setq out (append out
                          (list (format "  \\coordinate (%s) at %s;"
                                        (plist-get point :name)
                                        (plist-get point :coord))))))
      (setq out (append out '(""))))
    (when functions
      (setq out (append out '("  % Function plots") functions '(""))))
    (when (or lines circles ellipses angle-fills)
      (setq out (append out '("  % Geometry") angle-fills circles ellipses lines '(""))))
    (when (and my/org-tikz-geogebra-include-points points)
      (setq out (append out '("  % Point markers")))
      (dolist (point points)
        (setq out (append out
                          (list (format "  \\draw[fill=black] (%s) circle (1pt);"
                                        (plist-get point :name))))))
      (setq out (append out '(""))))
    (when (and my/org-tikz-geogebra-include-labels points)
      (setq out (append out '("  % Point labels")))
      (dolist (point points)
        (setq out (append out
                          (list (format "  \\node[%s] at (%s) {$%s$};"
                                        (my/org-tikz--smart-label-position point points)
                                        (plist-get point :name)
                                        (plist-get point :label))))))
      (setq out (append out '(""))))
    (when angle-labels
      (setq out (append out '("  % Angle labels") angle-labels '(""))))
    (when text-labels
      (setq out (append out '("  % Text labels") text-labels '(""))))
    (setq out (append out '("\\end{tikzpicture}")))
    (string-trim-right (string-join out "\n"))))

(defun my/org-tikz--generated-raw-name ()
  "Return a fresh generated GeoGebra raw block name."
  (format "ggb-raw-%s" (format-time-string "%Y%m%dT%H%M%S")))

(defun my/org-tikz--format-geogebra-workflow (raw cleaned)
  "Return Org text containing folded RAW code and CLEANED display LaTeX."
  (format "#+name: %s\n#+begin_src latex :eval never :exports none :results none\n%s\n#+end_src\n\n#+begin_display_latex\n%s\n#+end_display_latex\n"
          (my/org-tikz--generated-raw-name)
          (string-trim raw)
          (string-trim cleaned)))

(defun my/org-tikz--source-from-context ()
  "Return plist describing the selected GeoGebra source."
  (cond
   ((use-region-p)
    (list :text (buffer-substring-no-properties (region-beginning) (region-end))
          :begin (region-beginning)
          :end (region-end)))
   ((derived-mode-p 'org-mode)
    (let ((element (org-element-context)))
      (pcase (org-element-type element)
        ('src-block
         (list :text (org-element-property :value element)
               :begin (org-element-property :begin element)
               :end (org-element-property :end element)))
        ((or 'example-block 'special-block)
         (let ((begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (contents-begin (org-element-property :contents-begin element))
               (contents-end (org-element-property :contents-end element)))
           (when (and contents-begin contents-end)
             (list :text (buffer-substring-no-properties contents-begin contents-end)
                   :begin begin
                   :end end)))))))
   (t nil)))

(defun my/org-geogebra-tikz-clean-dwim (&optional prompt)
  "Clean GeoGebra TikZ from region, current Org block, or kill ring.
With PROMPT, ask for point/label/rounding options before generating output."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (when prompt
    (setq my/org-tikz-geogebra-include-points
          (y-or-n-p "Include point markers? "))
    (setq my/org-tikz-geogebra-include-labels
          (y-or-n-p "Include point labels? "))
    (setq my/org-tikz-geogebra-round-coordinates
          (y-or-n-p "Round coordinates? ")))
  (let* ((source (my/org-tikz--source-from-context))
         (raw (or (plist-get source :text)
                  (ignore-errors (current-kill 0 t))
                  (user-error "No region, Org block, or kill-ring TikZ code found")))
         (cleaned (my/org-tikz-clean-geogebra-code raw))
         (insert-beg (or (plist-get source :begin) (point)))
         (insert-end (or (plist-get source :end) (point)))
         (text (my/org-tikz--format-geogebra-workflow raw cleaned)))
    (goto-char insert-beg)
    (delete-region insert-beg insert-end)
    (insert text)
    (let ((inserted-end (point)))
      (goto-char insert-beg)
      (my/org-tikz-fold-generated-raw-blocks insert-beg inserted-end)
      (search-forward "#+begin_display_latex" inserted-end t)
      (forward-line 1))
    (message "GeoGebra TikZ cleaned; raw export folded")))

(defun my/org-tikz--raw-block-name-before-p (pos)
  "Return non-nil when a generated GeoGebra name is before POS."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (looking-at-p "^[ \t]*#\\+name:[ \t]+ggb-raw-")))

(defun my/org-tikz--raw-src-block-at-point ()
  "Return generated raw src block bounds at point, or nil."
  (when (derived-mode-p 'org-mode)
    (let ((element (org-element-context)))
      (when (eq (org-element-type element) 'src-block)
        (let* ((begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (language (downcase (or (org-element-property :language element) "")))
               content-beg content-end)
          (when (and (string= language "latex")
                     (my/org-tikz--raw-block-name-before-p begin))
            (save-excursion
              (goto-char begin)
              (forward-line 1)
              (setq content-beg (point))
              (goto-char end)
              (re-search-backward "^[ \t]*#\\+end_src\\b" begin t)
              (setq content-end (line-beginning-position)))
            (list :begin begin
                  :end end
                  :contents-begin content-beg
                  :contents-end content-end)))))))

(defun my/org-tikz--fold-overlays-in (beg end)
  "Return GeoGebra raw fold overlays between BEG and END."
  (seq-filter
   (lambda (overlay)
     (and (overlay-buffer overlay)
          (overlay-get overlay 'my/org-tikz-raw-fold)
          (< (overlay-start overlay) end)
          (> (overlay-end overlay) beg)))
   my/org-tikz--raw-fold-overlays))

(defun my/org-tikz--fold-raw-block (block)
  "Hide generated raw GeoGebra BLOCK with a compact overlay."
  (let* ((beg (plist-get block :contents-begin))
         (end (plist-get block :contents-end))
         (line-count (max 0 (count-lines beg end)))
         (summary (propertize
                   (format "  GeoGebra raw TikZ hidden (%d lines). Run my/org-tikz-toggle-raw-fold-at-point to show it.\n"
                           line-count)
                   'face 'shadow)))
    (when (< beg end)
      (unless (my/org-tikz--fold-overlays-in beg end)
        (let ((overlay (make-overlay beg end nil t nil)))
          (overlay-put overlay 'my/org-tikz-raw-fold t)
          (overlay-put overlay 'display summary)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'modification-hooks
                       (list (lambda (ov _flag _beg _end &optional _len)
                               (delete-overlay ov))))
          (push overlay my/org-tikz--raw-fold-overlays)
          overlay)))))

(defun my/org-tikz-toggle-raw-fold-at-point ()
  "Toggle the generated GeoGebra raw TikZ fold overlay at point."
  (interactive)
  (my/org-tikz--cleanup-fold-overlays)
  (let* ((block (or (my/org-tikz--raw-src-block-at-point)
                    (save-excursion
                      (when (re-search-backward "^[ \t]*#\\+name:[ \t]+ggb-raw-" nil t)
                        (forward-line 1)
                        (my/org-tikz--raw-src-block-at-point)))))
         (beg (plist-get block :contents-begin))
         (end (plist-get block :contents-end)))
    (unless block
      (user-error "Point is not in a generated GeoGebra raw src block"))
    (let ((overlays (my/org-tikz--fold-overlays-in beg end)))
      (if overlays
          (progn
            (mapc #'delete-overlay overlays)
            (setq my/org-tikz--raw-fold-overlays
                  (seq-difference my/org-tikz--raw-fold-overlays overlays))
            (message "GeoGebra raw TikZ shown"))
        (my/org-tikz--fold-raw-block block)
        (message "GeoGebra raw TikZ folded")))))

(defun my/org-tikz-fold-generated-raw-blocks (&optional beg end)
  "Fold generated GeoGebra raw src blocks between BEG and END.
When called interactively, scan the whole buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((beg (or beg (point-min)))
        (end (or end (point-max)))
        (count 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^[ \t]*#\\+name:[ \t]+ggb-raw-" end t)
        (forward-line 1)
        (when (looking-at-p "^[ \t]*#\\+begin_src[ \t]+latex\\b")
          (let ((block (my/org-tikz--raw-src-block-at-point)))
            (when (and block (my/org-tikz--fold-raw-block block))
              (setq count (1+ count)))))))
    (when (called-interactively-p 'interactive)
      (message "Folded %d GeoGebra raw TikZ block%s"
               count (if (= count 1) "" "s")))
    count))

(defun my/org-tikz--wrap-region-or-insert (open close)
  "Insert OPEN and CLOSE around region or at point."
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert open text close))
    (insert open close)
    (forward-char (- (length close)))))

(defun my/org-tikz-insert-tikzpicture ()
  "Insert an Org display_latex TikZ picture block."
  (interactive)
  (my/org-tikz--wrap-region-or-insert
   "#+begin_display_latex\n\\begin{tikzpicture}[scale=1]\n"
   "\n\\end{tikzpicture}\n#+end_display_latex\n"))

(defun my/org-tikz-insert-tikzcd ()
  "Insert an Org display_latex tikz-cd block."
  (interactive)
  (my/org-tikz--wrap-region-or-insert
   "#+begin_display_latex\n\\begin{tikzcd}\n"
   "\n\\end{tikzcd}\n#+end_display_latex\n"))

(defun my/org-tikz--display-latex-block-at-point ()
  "Return display_latex block bounds at point, or nil."
  (let ((element (org-element-context)))
    (when (and (eq (org-element-type element) 'special-block)
               (string= (downcase (or (org-element-property :type element) ""))
                        "display_latex"))
      (list :begin (org-element-property :begin element)
            :end (org-element-property :end element)
            :contents-begin (org-element-property :contents-begin element)
            :contents-end (org-element-property :contents-end element)))))

(defun my/org-tikz-preview-display-latex-block ()
  "Preview the current display_latex block with the async LaTeX renderer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((block (or (my/org-tikz--display-latex-block-at-point)
                   (user-error "Point is not inside a display_latex block"))))
    (my/org-latex--preview-range
     (plist-get block :begin)
     (plist-get block :end)
     'manual
     'front)
    (message "Queued display_latex preview")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x g") #'my/org-geogebra-tikz-clean-dwim)
  (define-key org-mode-map (kbd "C-c C-x G") #'my/org-tikz-toggle-raw-fold-at-point)
  (define-key org-mode-map (kbd "C-c C-x t") #'my/org-tikz-insert-tikzpicture)
  (define-key org-mode-map (kbd "C-c C-x d") #'my/org-tikz-insert-tikzcd))

(provide 'init-org-tikz)
;;; init-org-tikz.el ends here

;;; init-note-reference.el --- Typst reference helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Typst-side replacement for the Org reference helpers.  It can create stable
;; labels, copy `@label' references, pick targets from the current note, another
;; indexed note, or an arbitrary file, and insert a Typst reference/link.

;;; Code:

(require 'bibtex)
(require 'cl-lib)
(require 'init-note)
(require 'seq)
(require 'subr-x)

(declare-function consult--jump-preview "consult" ())
(declare-function consult--read "consult" (table &rest options))
(declare-function my/org-reference-create-target-dwim "init-org-utility" ())
(declare-function my/org-reference-insert-link "init-org-utility" ())
(declare-function vertico--candidate "vertico" (&optional highlight))

(defconst my/note-reference--label-regexp
  "<\\([a-zA-Z][a-zA-Z0-9_:.-]*\\)>"
  "Regexp matching a Typst label definition.")

(defcustom my/note-reference-excluded-labels '("note")
  "Label names that are infrastructure and should not appear as candidates."
  :type '(repeat string)
  :group 'my/note)

(defcustom my/note-reference-include-citations t
  "When non-nil, local reference insertion also offers BibTeX citations."
  :type 'boolean
  :group 'my/note)

(defcustom my/note-bibliography-files nil
  "BibTeX files used by Typst citation insertion.
When nil, use `pv/org-bibtex-files' if it is bound, otherwise
`references/references.bib' below `my/note-root'."
  :type '(choice (const :tag "Use Org bibliography/default" nil)
                 (repeat file))
  :group 'my/note)

(defface my/note-reference-scope-candidate
  '((t :inherit font-lock-keyword-face :weight semibold))
  "Face for Typst reference candidates that can be drilled into."
  :group 'my/note)

(defface my/note-reference-target-candidate
  '((t :inherit font-lock-variable-name-face))
  "Face for selectable Typst reference targets."
  :group 'my/note)

(defface my/note-reference-file-candidate
  '((t :inherit font-lock-string-face :weight semibold))
  "Face for file-level Typst reference candidates."
  :group 'my/note)

(defvar my/note-reference--cache (make-hash-table :test 'equal)
  "Target cache keyed by (FILE . MTIME-FLOAT).")

(defvar my/note-reference--minibuffer-lookup nil
  "Lookup table used while reading Typst reference targets.")

(defvar my/note-reference--minibuffer-candidates nil
  "Target candidates visible to the active Typst reference minibuffer.")

(defvar my/note-reference--target-read-map nil
  "Extra minibuffer bindings used only while reading Typst reference targets.")

(setq my/note-reference--target-read-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") #'my/note-reference--confirm-minibuffer-target)
        (define-key map (kbd "<return>") #'my/note-reference--confirm-minibuffer-target)
        (define-key map (kbd "C-m") #'my/note-reference--confirm-minibuffer-target)
        (define-key map (kbd "TAB") #'my/note-reference--drill-minibuffer-target)
        (define-key map (kbd "<tab>") #'my/note-reference--drill-minibuffer-target)
        (define-key map (kbd "C-i") #'my/note-reference--drill-minibuffer-target)
        (define-key map (kbd "M-RET") #'my/note-reference--drill-minibuffer-target)
        (define-key map (kbd "M-<return>") #'my/note-reference--drill-minibuffer-target)
        (define-key map (kbd "C-c C-o") #'my/note-reference--drill-minibuffer-target)
        map))

(defun my/note-reference--typst-buffer-p ()
  "Return non-nil when the current buffer is a Typst note buffer."
  (derived-mode-p 'typst-ts-mode 'typst-mode 'my/typst-mode))

(defun my/note-reference--require-typst-buffer ()
  "Signal a user error unless the current buffer is a Typst note buffer."
  (unless (my/note-reference--typst-buffer-p)
    (user-error "Not in a Typst note buffer")))

(defun my/note-reference--compact-text (text &optional width)
  "Return TEXT as one compact line, truncated to WIDTH."
  (let* ((single-line (replace-regexp-in-string
                       "[ \t\n\r]+" " "
                       (string-trim (or text ""))))
         (width (or width 96)))
    (truncate-string-to-width single-line width nil nil "...")))

(defun my/note-reference--path-safe (text)
  "Return TEXT safe for slash-delimited completion paths."
  (string-replace "/" "|" (or text "")))

(defun my/note-reference--line-comment-position ()
  "Return the position of a Typst line comment on the current line, or nil."
  (save-excursion
    (let ((end (line-end-position)))
      (goto-char (line-beginning-position))
      (search-forward "//" end t))))

(defun my/note-reference--commented-position-p (position)
  "Return non-nil when POSITION is after a Typst line comment marker."
  (save-excursion
    (goto-char position)
    (when-let* ((comment (my/note-reference--line-comment-position)))
      (<= comment position))))

(defun my/note-reference--clean-line (line)
  "Return LINE without labels, comments, or extra whitespace."
  (let ((line (replace-regexp-in-string
               my/note-reference--label-regexp "" (or line ""))))
    (when-let* ((comment (string-match-p "//" line)))
      (setq line (substring line 0 comment)))
    (my/note-reference--compact-text line 120)))

(defun my/note-reference--candidate-line ()
  "Return a compact preview line at point."
  (my/note-reference--clean-line
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun my/note-reference--labels-on-current-line ()
  "Return non-commented labels on the current line."
  (let ((end (line-end-position))
        labels)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (re-search-forward my/note-reference--label-regexp end t)
        (let ((label (match-string-no-properties 1)))
          (unless (or (member label my/note-reference-excluded-labels)
                      (my/note-reference--commented-position-p
                       (match-beginning 0)))
            (push label labels)))))
    (nreverse labels)))

(defun my/note-reference--first-label-on-current-line ()
  "Return the first non-commented label on the current line, or nil."
  (car (my/note-reference--labels-on-current-line)))

(defun my/note-reference--heading-on-current-line ()
  "Return (LEVEL . TITLE) when the current line is a Typst heading."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[ \t]*\\(=+\\)[ \t]+\\(.+\\)$")
      (cons (length (match-string 1))
            (my/note-reference--clean-line
             (match-string-no-properties 2))))))

(defun my/note-reference--heading-context ()
  "Return the nearest preceding Typst heading text."
  (save-excursion
    (when (re-search-backward "^=+[ \t]+\\(.+\\)$" nil t)
      (my/note-reference--clean-line (match-string-no-properties 1)))))

(defun my/note-reference--target-kind (label line)
  "Return a target kind for LABEL on LINE."
  (cond
   ((or (string-prefix-p "eq-" label)
        (string-match-p "\\$\\|\\\\=" line))
    'formula)
   ((or (string-prefix-p "fig-" label)
        (string-match-p "#\\(?:figure\\|image\\)\\b" line))
    'figure)
   (t 'label)))

(defun my/note-reference--extract-context-lines (start limit count)
  "Return up to COUNT useful context lines between START and LIMIT."
  (save-excursion
    (goto-char start)
    (let (lines)
      (while (and (< (point) limit)
                  (not (eobp))
                  (< (length lines) count))
        (let ((line (my/note-reference--candidate-line)))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line)
                      (string-prefix-p "=" line)
                      (string-prefix-p "//" line))
            (push line lines)))
        (forward-line 1))
      (when lines
        (my/note-reference--compact-text
         (mapconcat #'identity (nreverse lines) " | ")
         120)))))

(defun my/note-reference--context-start-pos (start limit)
  "Return first useful text position between START and LIMIT."
  (save-excursion
    (goto-char start)
    (catch 'pos
      (while (and (< (point) limit)
                  (not (eobp)))
        (let ((line (my/note-reference--candidate-line)))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line)
                      (string-prefix-p "=" line)
                      (string-prefix-p "//" line))
            (throw 'pos (line-beginning-position))))
        (forward-line 1))
      start)))

(defun my/note-reference--all-labels ()
  "Return all non-excluded labels in the current buffer."
  (let (labels)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward my/note-reference--label-regexp nil t)
          (let ((label (match-string-no-properties 1)))
            (unless (or (member label my/note-reference-excluded-labels)
                        (my/note-reference--commented-position-p
                         (match-beginning 0)))
              (push label labels))))))
    (delete-dups (nreverse labels))))

(defun my/note-reference--scope-candidate-p (target)
  "Return non-nil when TARGET can be drilled into."
  (memq (plist-get target :kind) '(scope-heading)))

(defun my/note-reference--scope-segment-name (candidate)
  "Return the path segment for scope CANDIDATE, without trailing slash."
  (my/note-reference--path-safe
   (or (plist-get candidate :label)
       (plist-get candidate :target)
       "")))

(defun my/note-reference--candidate-name (candidate)
  "Return the path segment name for CANDIDATE."
  (pcase (plist-get candidate :kind)
    ('file
     (or (plist-get candidate :label) ""))
    ('scope-heading
     (concat (my/note-reference--scope-segment-name candidate) "/"))
    ('citation
     (concat "@" (or (plist-get candidate :target) "")))
    (_
     (my/note-reference--path-safe
      (or (plist-get candidate :target)
          (plist-get candidate :label)
          "")))))

(defun my/note-reference--candidate-face (candidate)
  "Return the completion face for reference CANDIDATE."
  (pcase (plist-get candidate :kind)
    ('file 'my/note-reference-file-candidate)
    ('scope-heading 'my/note-reference-scope-candidate)
    (_ 'my/note-reference-target-candidate)))

(defun my/note-reference--candidate-in-scope-p (candidate scope)
  "Return non-nil when CANDIDATE is inside SCOPE."
  (let ((pos (plist-get candidate :pos))
        (begin (plist-get scope :begin))
        (end (plist-get scope :end)))
    (and pos begin end
         (< begin pos)
         (<= pos end))))

(defun my/note-reference--filter-scope-children (candidates scope)
  "Return useful CANDIDATES under SCOPE."
  (let* ((scope-level (plist-get scope :level))
         (scope-begin (plist-get scope :begin))
         (children
          (seq-filter
           (lambda (candidate)
             (and (not (eq candidate scope))
                  (let ((pos (plist-get candidate :pos))
                        (kind (plist-get candidate :kind)))
                    (if (= (or pos -1) (or scope-begin -2))
                        (not (eq kind 'scope-heading))
                      (my/note-reference--candidate-in-scope-p
                       candidate scope)))))
           candidates)))
    (seq-filter
     (lambda (candidate)
       (not (and (eq (plist-get candidate :kind) 'scope-heading)
                 (<= (or (plist-get candidate :level)
                         most-positive-fixnum)
                     (or scope-level 0)))))
     children)))

(defun my/note-reference--scope-children (scope &optional candidates)
  "Return child candidates for SCOPE."
  (and (my/note-reference--scope-candidate-p scope)
       (my/note-reference--filter-scope-children
        (or candidates my/note-reference--minibuffer-candidates)
        scope)))

(defun my/note-reference--scope-has-children-p (scope &optional candidates)
  "Return non-nil when SCOPE has child targets."
  (not (null (my/note-reference--scope-children scope candidates))))

(defun my/note-reference--prune-dead-scopes (candidates)
  "Remove scope headings that cannot be selected or drilled into."
  (let ((current candidates)
        changed)
    (while
        (progn
          (setq changed nil)
          (let ((next
                 (seq-remove
                  (lambda (candidate)
                    (and (my/note-reference--scope-candidate-p candidate)
                         (not (my/note-reference--target-has-label-p
                               candidate))
                         (not (my/note-reference--scope-has-children-p
                               candidate current))))
                  current)))
            (unless (= (length next) (length current))
              (setq changed t
                    current next)))
          changed))
    current))

(defun my/note-reference--path-candidates-at (all scope-parts &optional universe)
  "Return candidates at SCOPE-PARTS from ALL."
  (let ((universe (or universe all)))
    (if (null scope-parts)
        all
      (let* ((head (car scope-parts))
             (scope (seq-find
                     (lambda (candidate)
                       (and (my/note-reference--scope-candidate-p candidate)
                            (string= (my/note-reference--scope-segment-name
                                      candidate)
                                     head)))
                     all)))
        (when scope
          (my/note-reference--path-candidates-at
           (my/note-reference--filter-scope-children universe scope)
           (cdr scope-parts)
           universe))))))

(defun my/note-reference--minibuffer-candidate ()
  "Return the currently selected minibuffer candidate string."
  (cond
   ((and (fboundp 'vertico--candidate)
         (vertico--candidate))
    (vertico--candidate))
   (t
    (minibuffer-contents))))

(defun my/note-reference--minibuffer-target ()
  "Return the target plist for the current minibuffer selection."
  (let* ((candidate (my/note-reference--minibuffer-candidate))
         (plain (and candidate (substring-no-properties candidate)))
         (full (and candidate
                    (get-text-property 0 'my/note-reference-full candidate))))
    (or (and candidate
             (get-text-property 0 'my/note-reference-target candidate))
        (and full my/note-reference--minibuffer-lookup
             (gethash full my/note-reference--minibuffer-lookup))
        (and plain my/note-reference--minibuffer-lookup
             (gethash plain my/note-reference--minibuffer-lookup)))))

(defun my/note-reference--confirm-minibuffer-target ()
  "Confirm the currently selected reference target."
  (interactive)
  (let ((target (my/note-reference--minibuffer-target)))
    (cond
     ((and (my/note-reference--scope-candidate-p target)
           (not (my/note-reference--target-has-label-p target)))
      (if (my/note-reference--scope-has-children-p target)
          (user-error "Scope has no label; use TAB to enter it")
        (user-error "Scope has no label or child targets")))
     (target
      (throw 'my/note-reference-select target))
     (t
      (user-error "No target selected")))))

(defun my/note-reference--drill-minibuffer-target ()
  "Enter the currently selected scope target."
  (interactive)
  (let ((target (my/note-reference--minibuffer-target)))
    (cond
     ((and (my/note-reference--scope-candidate-p target)
           (my/note-reference--scope-has-children-p target))
      (throw 'my/note-reference-drill (cons 'drill target)))
     ((my/note-reference--scope-candidate-p target)
      (message "Scope has no child targets"))
     (target
      (message "This candidate is not a scope"))
     (t
      (message "No target selected")))))

(defun my/note-reference--install-target-read-map ()
  "Install Typst reference target keys in the current minibuffer only."
  (let ((base (current-local-map)))
    (use-local-map
     (if base
         (make-composed-keymap my/note-reference--target-read-map base)
       my/note-reference--target-read-map))))

(defun my/note-reference--with-target-read-map (fn)
  "Call FN with Typst target keys scoped to the minibuffer it opens."
  (minibuffer-with-setup-hook #'my/note-reference--install-target-read-map
    (funcall fn)))

(defun my/note-reference--completing-read-with-target-map (&rest args)
  "Call `completing-read' with Typst target keys scoped to its minibuffer."
  (my/note-reference--with-target-read-map
   (lambda ()
     (apply #'completing-read args))))

(defun my/note-reference--lookup-selection (selected candidates lookup)
  "Return target plist for SELECTED using CANDIDATES and LOOKUP."
  (let* ((plain (and selected (substring-no-properties selected)))
         (full (and selected
                    (get-text-property 0 'my/note-reference-full selected)))
         (candidate (or (and selected
                             (get-text-property 0
                                                'my/note-reference-target
                                                selected))
                        (and full (gethash full lookup))
                        (and plain (gethash plain lookup)))))
    (or candidate
        (seq-some
         (lambda (candidate-string)
           (let ((plain-candidate (substring-no-properties candidate-string))
                 (full-candidate
                  (get-text-property 0
                                     'my/note-reference-full
                                     candidate-string)))
             (or (get-text-property 0 'my/note-reference-target
                                    candidate-string)
                 (and full-candidate (gethash full-candidate lookup))
                 (gethash plain-candidate lookup))))
         candidates))))

(defun my/note-reference--make-path-table (all-candidates)
  "Return (TABLE-FN . LOOKUP-HASH) for path-style target completion."
  (let* ((lookup (make-hash-table :test 'equal))
         (annotate
          (lambda (candidate-string)
            (when-let* ((plain (substring-no-properties candidate-string))
                        (full (get-text-property 0 'my/note-reference-full
                                                 candidate-string))
                        (target (or (get-text-property
                                     0 'my/note-reference-target
                                     candidate-string)
                                    (and full (gethash full lookup))
                                    (gethash plain lookup))))
              (let* ((context (plist-get target :context))
                     (preview (plist-get target :preview))
                     (label (plist-get target :label))
                     (text (or context preview label)))
                (when (and text (not (string-empty-p text)))
                  (propertize
                   (concat "  " (my/note-reference--compact-text text 110))
                   'face 'completions-annotations)))))))
    (cons
     (lambda (string pred action)
       (cond
        ((eq action 'metadata)
         `(metadata (category . my-note-target)
                    (annotation-function . ,annotate)))
        ((eq (car-safe action) 'boundaries)
         (let* ((slash (string-match-p "/[^/]*\\'" string))
                (left (if slash (1+ slash) 0))
                (right (length (cdr action))))
           `(boundaries ,left . ,right)))
        (t
         (let* ((slash (string-match-p "/[^/]*\\'" string))
                (prefix (if slash (substring string 0 (1+ slash)) ""))
                (current (if slash (substring string (1+ slash)) string))
                (parts (and (not (string-empty-p prefix))
                            (split-string (string-remove-suffix "/" prefix)
                                          "/" t)))
                (level (or (my/note-reference--path-candidates-at
                            all-candidates parts)
                           '()))
                (completions
                 (mapcar
                  (lambda (candidate)
                    (let* ((segment (my/note-reference--candidate-name
                                     candidate))
                           (full (concat prefix segment)))
                      (puthash segment candidate lookup)
                      (puthash full candidate lookup)
                      (propertize segment
                                  'face (my/note-reference--candidate-face
                                         candidate)
                                  'my/note-reference-target candidate
                                  'my/note-reference-full full)))
                  level)))
           (complete-with-action action completions current pred)))))
     lookup)))

(defun my/note-reference--preview-pos (target buffer)
  "Return the best BUFFER position to preview TARGET."
  (let ((pos (plist-get target :pos))
        (kind (plist-get target :kind)))
    (with-current-buffer buffer
      (save-excursion
        (pcase kind
          ('scope-heading
           (let ((begin (or (plist-get target :begin) pos))
                 (end (or (plist-get target :end) (point-max))))
             (goto-char begin)
             (forward-line 1)
             (my/note-reference--context-start-pos (point) end)))
          (_
           (or pos (point-min))))))))

(defun my/note-reference--target-preview-state (file lookup)
  "Return a Consult preview state for reference targets in FILE."
  (if (not (and (fboundp 'consult--jump-preview)
                (require 'consult nil t)))
      (lambda (&rest _))
    (let ((preview (consult--jump-preview))
          origin-window
          origin-buffer
          origin-point)
      (lambda (action candidate-string)
        (pcase action
          ('setup
           (setq origin-window (selected-window)
                 origin-buffer (current-buffer)
                 origin-point (point-marker))
           (funcall preview action nil))
          ('preview
           (condition-case nil
               (if (not candidate-string)
                   (funcall preview 'preview nil)
                 (let* ((plain (substring-no-properties candidate-string))
                        (full (get-text-property
                               0 'my/note-reference-full candidate-string))
                        (target (or (get-text-property
                                     0 'my/note-reference-target
                                     candidate-string)
                                    (and full (gethash full lookup))
                                    (gethash plain lookup)))
                        (pos (and target (plist-get target :pos))))
                   (when (and pos (> pos 0))
                     (let* ((buffer (find-file-noselect file))
                            (preview-pos
                             (my/note-reference--preview-pos target buffer))
                            (marker (make-marker)))
                       (set-marker marker preview-pos buffer)
                       (funcall preview 'preview marker)
                       (when (eq (current-buffer) buffer)
                         (recenter 3))))))
             (error nil)))
          ((or 'exit 'return)
           (condition-case nil
               (progn
                 (funcall preview 'preview nil)
                 (when (and (window-live-p origin-window)
                            (buffer-live-p origin-buffer))
                   (set-window-buffer origin-window origin-buffer)
                   (when (marker-buffer origin-point)
                     (set-window-point origin-window origin-point)))
                 (when (eq action 'return)
                   (set-marker origin-point nil)))
             (error nil)))
          (_
           (funcall preview action nil)))))))

(defun my/note-reference--modified-file-buffer (file)
  "Return the modified live buffer visiting FILE, or nil."
  (let ((expanded (expand-file-name file)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (and buffer-file-name
              (buffer-modified-p buffer)
              (or (equal (expand-file-name buffer-file-name) expanded)
                  (ignore-errors
                    (file-equal-p buffer-file-name expanded))))))
     (buffer-list))))

(defun my/note-reference--insert-source-contents (file)
  "Insert FILE contents, preferring unsaved live buffer contents."
  (if-let* ((buffer (my/note-reference--modified-file-buffer file)))
      (let ((text (with-current-buffer buffer
                    (save-restriction
                      (widen)
                      (buffer-substring-no-properties
                       (point-min) (point-max))))))
        (insert text))
    (insert-file-contents file)))

(defun my/note-reference--heading-scope-end (scope scopes max)
  "Return end position for SCOPE from SCOPES, bounded by MAX."
  (let ((begin (plist-get scope :begin))
        (level (plist-get scope :level))
        end)
    (dolist (candidate scopes)
      (let ((candidate-begin (plist-get candidate :begin))
            (candidate-level (plist-get candidate :level)))
        (when (and (> candidate-begin begin)
                   (<= candidate-level level)
                   (or (not end) (< candidate-begin end)))
          (setq end (1- candidate-begin)))))
    (or end max)))

(defun my/note-reference--fill-scope-ends (candidates scopes max)
  "Return CANDIDATES with scope end positions filled from SCOPES and MAX."
  (mapcar
   (lambda (candidate)
     (if (eq (plist-get candidate :kind) 'scope-heading)
         (plist-put (copy-sequence candidate)
                    :end
                    (my/note-reference--heading-scope-end
                     candidate scopes max))
       candidate))
   candidates))

(defun my/note-reference--scan-typst-targets (file)
  "Return reference target candidates from Typst FILE."
  (let (candidates scopes seen)
    (with-temp-buffer
      (my/note-reference--insert-source-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (line-beginning-position))
               (line (line-number-at-pos pos))
               (raw (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
               (preview (my/note-reference--candidate-line))
               (heading (my/note-reference--heading-on-current-line))
               (labels (my/note-reference--labels-on-current-line)))
          (when heading
            (let* ((label (car labels))
                   (scope (list :kind 'scope-heading
                                :target label
                                :label (cdr heading)
                                :line line
                                :pos pos
                                :begin pos
                                :level (car heading)
                                :preview preview)))
              (push scope scopes)
              (push scope candidates)))
          (dolist (label labels)
            (unless (member label seen)
              (push label seen)
              (push (list :kind (my/note-reference--target-kind label raw)
                          :target label
                          :label (or (and heading (cdr heading)) label)
                          :line line
                          :pos pos
                          :heading (my/note-reference--heading-context)
                          :preview preview)
                    candidates))))
        (forward-line 1))
      (let ((max (point-max)))
        (setq candidates
              (my/note-reference--fill-scope-ends
               (nreverse candidates)
               (nreverse scopes)
               max))
        (mapcar
         (lambda (candidate)
           (if (eq (plist-get candidate :kind) 'scope-heading)
               (let* ((begin (plist-get candidate :begin))
                      (end (plist-get candidate :end))
                      (context (my/note-reference--extract-context-lines
                                begin (min end (+ begin 1200)) 5)))
                 (plist-put (copy-sequence candidate) :context context))
             candidate))
         candidates)))))

(defun my/note-reference--scan-text-targets (file)
  "Return lightweight reference candidates from non-Typst FILE."
  (let ((extension (downcase (or (file-name-extension file) "")))
        candidates)
    (with-temp-buffer
      (my/note-reference--insert-source-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (line-number-at-pos))
               (pos (point))
               (text (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))))
          (when (cond
                 ((member extension '("md" "markdown"))
                  (string-match-p "\\`#{1,6}[ \t]+\\S-" text))
                 ((member extension '("el" "py" "js" "ts" "tsx" "jsx" "rs"
                                      "go" "java" "c" "cpp" "h" "hpp" "sh"
                                      "zsh" "nix"))
                  (string-match-p
                   "\\`[ \t]*\\(?:\\(?:def\\|class\\|function\\|const\\|let\\|var\\|fn\\|struct\\|enum\\|interface\\|type\\|;;;\\|;;\\|//\\|#\\)[ \t]+\\S-\\)"
                   text))
                 (t nil))
            (push (list :kind 'line
                        :target line
                        :label text
                        :line line
                        :pos pos
                        :preview text)
                  candidates)))
        (forward-line 1)))
    (nreverse candidates)))

(defun my/note-reference--file-targets (file)
  "Return reference candidates for FILE, including a file-level candidate."
  (let* ((extension (downcase (or (file-name-extension file) "")))
         (typst-p (string= extension "typ"))
         (targets (if typst-p
                      (my/note-reference--scan-typst-targets file)
                    (my/note-reference--scan-text-targets file))))
    (cons (list :kind 'file
                :target nil
                :label (file-name-base file)
                :line 1
                :pos 1
                :preview (format "File: %s" (file-name-nondirectory file)))
          targets)))

(defun my/note-reference--file-targets-cached (file)
  "Return target plists for FILE, re-scanning only when mtime changes."
  (if (my/note-reference--modified-file-buffer file)
      (my/note-reference--file-targets file)
    (let* ((mtime (float-time
                   (file-attribute-modification-time
                    (file-attributes file))))
           (key (cons file mtime))
           (hit (gethash key my/note-reference--cache)))
      (or hit
          (let ((targets (my/note-reference--file-targets file)))
            (maphash
             (lambda (cached-key _)
               (when (and (consp cached-key)
                          (equal (car cached-key) file))
                 (remhash cached-key my/note-reference--cache)))
             my/note-reference--cache)
            (puthash key targets my/note-reference--cache)
            targets)))))

(defun my/note-reference--read-target (file &optional require-label)
  "Read a reference target from FILE using path-style completion.
When REQUIRE-LABEL is non-nil, omit the file-level target because
same-file Typst references need a concrete label."
  (let* ((targets (my/note-reference--file-targets-cached file))
         (all (my/note-reference--prune-dead-scopes
               (if require-label
                   (seq-filter
                    (lambda (target)
                      (or (my/note-reference--scope-candidate-p target)
                          (my/note-reference--target-has-label-p target)))
                    targets)
                 targets)))
         (pair (my/note-reference--make-path-table all))
         (table (car pair))
         (lookup (cdr pair)))
    (unless all
      (user-error "No reference targets found in %s" file))
    (catch 'done
      (let ((prefix ""))
        (while t
          (let* ((my/note-reference--minibuffer-lookup lookup)
                 (my/note-reference--minibuffer-candidates all)
                 (raw
                  (catch 'my/note-reference-select
                    (catch 'my/note-reference-drill
                      (let ((selected
                             (if (and (fboundp 'consult--read)
                                      (require 'consult nil t))
                                 (my/note-reference--with-target-read-map
                                  (lambda ()
                                    (consult--read
                                     table
                                     :prompt "Target: "
                                     :initial prefix
                                     :require-match nil
                                     :state
                                     (my/note-reference--target-preview-state
                                      file lookup)
                                     :preview-key '(:debounce 0.15 any)
                                     :keymap my/note-reference--target-read-map
                                     :lookup
                                     (lambda (selected candidates &rest _)
                                       (my/note-reference--lookup-selection
                                        selected candidates lookup)))))
                               (let ((choice
                                      (my/note-reference--completing-read-with-target-map
                                       "Target: " table nil nil prefix)))
                                 (my/note-reference--lookup-selection
                                  choice
                                  (all-completions choice table)
                                  lookup)))))
                        selected))))
                 (drilled (and (consp raw) (eq (car raw) 'drill)))
                 (target (or (and drilled (cdr raw))
                             (and (listp raw) (plist-get raw :kind) raw)
                             (user-error "No target selected"))))
            (if (and drilled (my/note-reference--scope-candidate-p target))
                (setq prefix (concat prefix
                                     (my/note-reference--candidate-name
                                      target)))
              (throw 'done target))))))))

(defun my/note-reference--source-from-file (file &optional kind)
  "Return a reference source plist for FILE.
Optional KIND overrides the inferred source kind."
  (let* ((file (expand-file-name file))
         (note (and (string= (downcase (or (file-name-extension file) ""))
                             "typ")
                    (ignore-errors (my/note-parse-file file)))))
    (list :kind (or kind (if note 'note 'file))
          :file file
          :id (plist-get note :id)
          :title (or (plist-get note :title)
                     (file-name-base file)))))

(defun my/note-reference--current-source ()
  "Return a source plist for the current buffer."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (my/note-reference--source-from-file buffer-file-name))

(defun my/note-reference--read-source ()
  "Read a source file or note node for Typst reference insertion."
  (let* ((current-file (buffer-file-name (buffer-base-buffer)))
         (choices (append (when current-file '("Current buffer"))
                          '("Note node" "File path")))
         (choice (completing-read "Reference from: " choices nil t nil nil
                                  (car choices))))
    (cond
     ((string= choice "Current buffer")
      (my/note-reference--current-source))
     ((string= choice "Note node")
      (let ((node (my/note-read-node "Note node: ")))
        (list :kind 'note
              :file (plist-get node :file)
              :id (plist-get node :id)
              :title (plist-get node :title))))
     (t
      (let* ((base (or (and current-file
                            (file-name-directory current-file))
                       default-directory))
             (file (read-file-name "Reference file: "
                                   base current-file t
                                   (and current-file
                                        (file-name-nondirectory
                                         current-file)))))
        (my/note-reference--source-from-file file))))))

(defun my/note-reference--same-file-p (file)
  "Return non-nil when FILE is the current buffer file."
  (let ((current-file (buffer-file-name (buffer-base-buffer))))
    (and current-file
         file
         (file-exists-p current-file)
         (file-exists-p file)
         (file-equal-p (expand-file-name file)
                       (expand-file-name current-file)))))

(defun my/note-reference--relative-file (file)
  "Return FILE path relative to the current buffer when possible."
  (let ((base (or (and buffer-file-name (file-name-directory buffer-file-name))
                  default-directory)))
    (file-relative-name file base)))

(defun my/note-reference--default-description (source target)
  "Return default description for SOURCE TARGET."
  (or (plist-get target :label)
      (plist-get target :preview)
      (plist-get target :target)
      (plist-get source :title)
      (file-name-base (plist-get source :file))))

(defun my/note-reference--read-description (source target)
  "Read visible link text for SOURCE TARGET."
  (let ((default (my/note-reference--default-description source target)))
    (read-string (format "Description (default %s): " default)
                 nil nil default)))

(defun my/note-reference--target-has-label-p (target)
  "Return non-nil when TARGET has a concrete Typst label."
  (and (stringp (plist-get target :target))
       (not (string-empty-p (plist-get target :target)))
       (not (memq (plist-get target :kind) '(file line citation)))))

(defun my/note-reference--build-cross-file-link (source target description)
  "Build a Typst link from SOURCE to TARGET using DESCRIPTION."
  (let ((description (my/note--typst-content-escape description)))
    (cond
     ((and (eq (plist-get source :kind) 'note)
           (plist-get source :id))
      (format "#note(%S)[%s]" (plist-get source :id) description))
     (t
      (format "#link(%S)[%s]"
              (my/note-reference--relative-file (plist-get source :file))
              description)))))

(defun my/note-reference--build-link (source target &optional description)
  "Build a Typst reference/link for SOURCE and TARGET."
  (pcase (plist-get target :kind)
    ('citation
     (format "@%s" (plist-get target :target)))
    (_
     (if (my/note-reference--same-file-p (plist-get source :file))
         (if (my/note-reference--target-has-label-p target)
             (format "@%s" (plist-get target :target))
           (user-error "Target has no Typst label"))
       (my/note-reference--build-cross-file-link
        source
        target
        (or description
            (my/note-reference--default-description source target)))))))

(defun my/note-reference--local-target-candidates ()
  "Return local Typst label candidates from the current buffer."
  (let* ((source (my/note-reference--current-source))
         (targets (my/note-reference--file-targets (plist-get source :file)))
         (label-targets (seq-filter #'my/note-reference--target-has-label-p
                                    targets)))
    (seq-remove
     (lambda (candidate)
       (and (eq (plist-get candidate :kind) 'scope-heading)
            (seq-some
             (lambda (other)
               (and (not (eq other candidate))
                    (not (eq (plist-get other :kind) 'scope-heading))
                    (equal (plist-get other :target)
                           (plist-get candidate :target))))
             label-targets)))
     label-targets)))

(defun my/note-reference--read-local-target ()
  "Read a local label target from the current Typst buffer."
  (let* ((candidates (append
                      (my/note-reference--local-target-candidates)
                      (when my/note-reference-include-citations
                        (my/note-reference--citation-candidates))))
         (alist (mapcar (lambda (candidate)
                          (cons (my/note-reference--display-flat-candidate
                                 candidate)
                                candidate))
                        candidates)))
    (unless alist
      (user-error "No Typst labels or BibTeX citations found"))
    (cdr (assoc (completing-read "Reference: " alist nil t) alist))))

(defun my/note-reference--display-flat-candidate (candidate)
  "Return a flat completion display string for CANDIDATE."
  (pcase (plist-get candidate :kind)
    ('citation
     (format "%-10s %-30s %s%s%s"
             "cite"
             (plist-get candidate :target)
             (or (plist-get candidate :author) "")
             (let ((year (plist-get candidate :year)))
               (if (and year (not (string-empty-p year)))
                   (format " (%s)" year)
                 ""))
             (let ((title (plist-get candidate :title)))
               (if (and title (not (string-empty-p title)))
                   (format "  %s" title)
                 ""))))
    (_
     (format "%-10s %-30s L%-4d %s%s"
             (symbol-name (plist-get candidate :kind))
             (or (plist-get candidate :target) "")
             (or (plist-get candidate :line) 1)
             (or (plist-get candidate :heading) "")
             (let ((preview (plist-get candidate :preview)))
               (if (and preview (not (string-empty-p preview)))
                   (format "  %s" preview)
                 ""))))))

;;; Citations

(defun my/note-reference--bibliography-files ()
  "Return readable BibTeX files for Typst citation insertion."
  (let ((files (or my/note-bibliography-files
                   (and (boundp 'pv/org-bibtex-files)
                        (symbol-value 'pv/org-bibtex-files))
	                   (list (expand-file-name
	                          "references/references.bib"
	                          (file-name-as-directory my/note-root))))))
    (delete-dups
     (seq-filter #'file-readable-p
                 (mapcar #'expand-file-name files)))))

(defun my/note-reference--bib-field (entry field)
  "Return cleaned FIELD value from BibTeX ENTRY."
  (when-let* ((raw (cdr (assoc field entry))))
    (let ((value (string-trim raw)))
      (when (and (>= (length value) 2)
                 (or (and (string-prefix-p "{" value)
                          (string-suffix-p "}" value))
                     (and (string-prefix-p "\"" value)
                          (string-suffix-p "\"" value))))
        (setq value (substring value 1 -1)))
      (my/note-reference--compact-text value 120))))

(defun my/note-reference--bib-entry-at-point (file)
  "Return a citation plist for the BibTeX entry at point in FILE."
  (let* ((key (bibtex-key-in-head))
         (entry (bibtex-parse-entry))
         (title (my/note-reference--bib-field entry "title"))
         (author (my/note-reference--bib-field entry "author"))
         (year (or (my/note-reference--bib-field entry "year")
                   (my/note-reference--bib-field entry "date")))
         (doi (my/note-reference--bib-field entry "doi")))
    (when (and key (not (string-empty-p key)))
      (list :kind 'citation
            :target key
            :title title
            :author author
            :year year
            :doi doi
            :file file))))

(defun my/note-reference--bib-file-candidates (file)
  "Return citation candidate plists from BibTeX FILE."
  (let (candidates)
    (with-temp-buffer
      (insert-file-contents file)
      (bibtex-mode)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*@[[:alpha:]]+[[:space:]]*[({]"
                                nil t)
        (goto-char (match-beginning 0))
        (let ((start (point)))
          (condition-case nil
              (progn
                (when-let* ((candidate
                             (my/note-reference--bib-entry-at-point file)))
                  (push candidate candidates))
                (goto-char start)
                (bibtex-end-of-entry)
                (forward-char 1))
            (error
             (goto-char start)
             (forward-line 1))))))
    (nreverse candidates)))

(defun my/note-reference--citation-candidates ()
  "Return citation candidate plists from configured BibTeX files."
  (let (candidates seen)
    (dolist (file (my/note-reference--bibliography-files))
      (dolist (candidate (my/note-reference--bib-file-candidates file))
        (let ((key (plist-get candidate :target)))
          (unless (member key seen)
            (push key seen)
            (push candidate candidates)))))
    (nreverse candidates)))

;;; Target creation and copying

(defun my/note-reference--label-kind-and-text ()
  "Return (PREFIX . TEXT) for a new label at point."
  (let* ((line (my/note-reference--candidate-line))
         (heading (my/note-reference--heading-on-current-line)))
    (cond
     (heading
      (cons "sec" (cdr heading)))
     ((string-match-p "#\\(?:figure\\|image\\)\\b" line)
      (cons "fig" line))
     ((string-match-p "\\$\\|\\\\=" line)
      (cons "eq" line))
     ((not (string-empty-p line))
      (cons "ref" line))
     (t
      (user-error "Current line is empty")))))

(defun my/note-reference--label-slug (value)
  "Return a Typst-label-safe slug for VALUE."
  (let ((slug (downcase (string-trim (or value "")))))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "-+" "-" slug))
    (setq slug (string-trim slug "-+" "-+"))
    (when (> (length slug) 48)
      (setq slug (string-trim (substring slug 0 48) "-+" "-+")))
    (or slug "item")))

(defun my/note-reference--unique-label (prefix text)
  "Return a unique label using PREFIX and TEXT."
  (let* ((base (format "%s-%s" prefix (my/note-reference--label-slug text)))
         (existing (my/note-reference--all-labels))
         (candidate base)
         (index 2))
    (while (member candidate existing)
      (setq candidate (format "%s-%d" base index)
            index (1+ index)))
    candidate))

(defun my/note-reference--insert-position-on-line ()
  "Return the best position for a label on the current line."
  (save-excursion
    (if-let* ((comment (my/note-reference--line-comment-position)))
        (goto-char (max (line-beginning-position) (- comment 2)))
      (goto-char (line-end-position)))
    (skip-chars-backward " \t" (line-beginning-position))
    (point)))

;;;###autoload
(defun my/note-reference-ensure-target-at-point ()
  "Create or reuse a Typst label on the current line.
When called interactively, copy the raw label name."
  (interactive)
  (my/note-reference--require-typst-buffer)
  (let ((label (or (my/note-reference--first-label-on-current-line)
                   (pcase-let* ((`(,prefix . ,text)
                                 (my/note-reference--label-kind-and-text))
                                (label
                                 (my/note-reference--unique-label prefix text))
                                (pos
                                 (my/note-reference--insert-position-on-line)))
                     (save-excursion
                       (goto-char pos)
                       (insert " <" label ">"))
                     label))))
    (when (called-interactively-p 'interactive)
      (kill-new label)
      (message "Typst label: %s" label))
    label))

(defun my/note-reference-link-at-point ()
  "Return a Typst `@label' reference to the current line, creating it if needed."
  (my/note-reference--require-typst-buffer)
  (format "@%s" (my/note-reference-ensure-target-at-point)))

;;;###autoload
(defun my/note-copy-reference-link-at-point ()
  "Copy a Typst `@label' reference to the current line."
  (interactive)
  (let ((link (my/note-reference-link-at-point)))
    (kill-new link)
    (message "Copied Typst reference: %s" link)))

;;;###autoload
(defun my/note-reference-create-target-dwim ()
  "Create or reuse a Typst label at point, then copy its `@label' reference."
  (interactive)
  (my/note-copy-reference-link-at-point))

;;; Insertion commands

;;;###autoload
(defun my/note-reference-insert-local ()
  "Insert a local Typst `@label' or `@citekey' reference."
  (interactive)
  (my/note-reference--require-typst-buffer)
  (let* ((target (my/note-reference--read-local-target))
         (link (my/note-reference--build-link
                (my/note-reference--current-source)
                target)))
    (insert link)
    (message "Inserted Typst reference: %s" link)))

;;;###autoload
(defun my/note-citation-insert ()
  "Insert a Typst citation from configured BibTeX files."
  (interactive)
  (my/note-reference--require-typst-buffer)
  (let* ((my/note-reference-include-citations t)
         (target (let* ((candidates (my/note-reference--citation-candidates))
                        (alist (mapcar
                                (lambda (candidate)
                                  (cons (my/note-reference--display-flat-candidate
                                         candidate)
                                        candidate))
                                candidates)))
                   (unless candidates
                     (user-error "No readable BibTeX citations found"))
                   (cdr (assoc (completing-read "Citation: " alist nil t)
                               alist))))
         (link (my/note-reference--build-link nil target)))
    (insert link)
    (message "Inserted Typst citation: %s" link)))

;;;###autoload
(defun my/note-reference-insert (&optional current-only)
  "Insert a Typst reference link.
By default, choose a source just like `my/org-reference-insert-link':
current buffer, indexed note node, or file path, then choose a target
with path-style completion.  With prefix CURRENT-ONLY, only choose a
local label/citation from the current buffer."
  (interactive "P")
  (my/note-reference--require-typst-buffer)
  (if current-only
      (my/note-reference-insert-local)
    (let* ((source (my/note-reference--read-source))
           (target (my/note-reference--read-target
                    (plist-get source :file)
                    (my/note-reference--same-file-p
                     (plist-get source :file))))
           (description (unless (or (eq (plist-get target :kind) 'citation)
                                    (and (my/note-reference--same-file-p
                                          (plist-get source :file))
                                         (my/note-reference--target-has-label-p
                                          target)))
                          (my/note-reference--read-description source target)))
           (link (my/note-reference--build-link source target description)))
      (insert link)
      (message "Inserted Typst reference: %s" link))))

;;;###autoload
(defun my/reference-insert-dispatch ()
  "Insert a reference link appropriate to the current major mode."
  (interactive)
  (cond
   ((my/note-reference--typst-buffer-p)
    (call-interactively #'my/note-reference-insert))
   ((derived-mode-p 'org-mode)
    (call-interactively #'my/org-reference-insert-link))
   (t
    (user-error "No reference-insert handler for %s" major-mode))))

;;;###autoload
(defun my/reference-create-target-dispatch ()
  "Create a reference target appropriate to the current major mode."
  (interactive)
  (cond
   ((my/note-reference--typst-buffer-p)
    (call-interactively #'my/note-reference-create-target-dwim))
   ((derived-mode-p 'org-mode)
    (call-interactively #'my/org-reference-create-target-dwim))
   (t
    (user-error "No reference-target handler for %s" major-mode))))

(provide 'init-note-reference)
;;; init-note-reference.el ends here

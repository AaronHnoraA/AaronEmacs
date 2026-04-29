;;; init-org-utility.el --- Org utility commands -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Small Org helpers that do not belong to agenda, capture, roam, or export.

;;; Code:

(require 'init-org-core)
(require 'cl-lib)
(require 'org-id)
(require 'org-element)
(require 'subr-x)

(defgroup my/org-utility nil
  "Personal Org utility helpers."
  :group 'org)

(defcustom my/org-download-image-root "img"
  "Directory, relative to the Org file, where pasted images are stored."
  :type 'string
  :group 'my/org-utility)

(defcustom my/org-reference-display-math-search-limit 4000
  "Maximum nearby characters searched when detecting a \\[...\\] formula."
  :type 'integer
  :group 'my/org-utility)

(defvar org-download-image-dir)
(defvar org-download-timestamp)
(defvar org-roam-directory)
(defvar my/org-reference--minibuffer-candidates nil)

(declare-function consult--jump-preview "consult" ())
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--lookup-prop "consult" (prop candidates input))
(declare-function org-download-clipboard "org-download")
(declare-function org-download--dir "org-download")
(declare-function org-download-screenshot "org-download")
(declare-function org-roam-node-file "org-roam-node" (node))
(declare-function org-roam-node-id "org-roam-node" (node))
(declare-function org-roam-node-read "org-roam-node" (&optional initial-input filter-fn sort-fn require-match prompt))
(declare-function org-roam-node-title "org-roam-node" (node))
(declare-function vertico--candidate "vertico" (&optional highlight))

(defun my/org-reference--display-math-bounds ()
  "Return bounds of the surrounding Org \\[...\\] formula, or nil."
  (let ((origin (point))
        (limit (max (point-min)
                    (- (point) my/org-reference-display-math-search-limit)))
        begin end)
    (save-excursion
      (when (search-backward "\\[" limit t)
        (setq begin (point))
        (when (search-forward "\\]" nil t)
          (setq end (point)))))
    (when (and begin end
               (<= begin origin)
               (<= origin end))
      (cons begin end))))

(defun my/org-reference--target-after-formula (pos)
  "Return dedicated target immediately after formula ending at POS, or nil."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (when (looking-at "<<\\([^>\n]+\\)>>")
      (match-string-no-properties 1))))

(defun my/org-reference--formula-target-at (bounds)
  "Return dedicated target around formula BOUNDS, or nil.
Targets are stored after the formula closing delimiter."
  (my/org-reference--target-after-formula (cdr bounds)))

(defun my/org-reference--block-bounds ()
  "Return bounds and type of the surrounding Org #+begin block, or nil."
  (let ((origin (point))
        (case-fold-search t)
        begin end type)
    (save-excursion
      ;; Jump to end of current line so re-search-backward includes this line.
      (goto-char (line-end-position))
      (when (re-search-backward "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\b" nil t)
        (setq begin (line-beginning-position)
              type (downcase (match-string-no-properties 1)))
        (when (re-search-forward
               (format "^[ \t]*#\\+end_%s\\b" (regexp-quote type)) nil t)
          (setq end (line-end-position)))))
    (when (and begin end
               (<= begin origin)
               (<= origin end))
      (list :begin begin :end end :type type))))

(defun my/org-reference--target-before-block (pos)
  "Return dedicated target on the line immediately before block starting at POS, or nil."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (when (looking-at "[ \t]*<<\\([^>\n]+\\)>>[ \t]*$")
      (match-string-no-properties 1))))

(defun my/org-reference-create-block-target ()
  "Create or return a dedicated target before the current Org #+begin block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((bounds (or (my/org-reference--block-bounds)
                     (user-error "Point is not inside a #+begin block")))
         (begin (plist-get bounds :begin))
         (type (plist-get bounds :type))
         (target (or (my/org-reference--target-before-block begin)
                     (my/org-reference--unique-target type))))
    (unless (my/org-reference--target-before-block begin)
      (save-excursion
        (goto-char begin)
        (insert "<<" target ">>\n")))
    (kill-new target)
    (message "Org block target: %s" target)
    target))

(defun my/org-reference--unique-target (&optional prefix)
  "Return a buffer-unique dedicated target name using PREFIX."
  (let* ((stem (or prefix "eq"))
         (base (format "%s-%s" stem (format-time-string "%Y%m%dT%H%M%S")))
         (candidate base)
         (index 2))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (format "<<%s>>" (regexp-quote candidate)) nil t)
        (setq candidate (format "%s-%d" base index)
              index (1+ index))))
    candidate))

(defun my/org-reference-create-formula-target ()
  "Create or return a dedicated target after the current \\[...\\] formula."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((bounds (or (my/org-reference--display-math-bounds)
                     (user-error "Point is not inside a \\[...\\] formula")))
         (target (or (my/org-reference--formula-target-at bounds)
                     (my/org-reference--unique-target "eq"))))
    (unless (my/org-reference--formula-target-at bounds)
      (save-excursion
        (goto-char (cdr bounds))
        (insert "     <<" target ">>")))
    (kill-new target)
    (message "Org formula target: %s" target)
    target))

(defun my/org-reference-ensure-target-at-point ()
  "Create a stable Org reference target at point.
On a headline, create or return its `ID'.  Inside a \\[...\\] formula, create
or return its dedicated target.  Inside a #+begin block, create or return a
dedicated target after the block."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cond
   ((org-before-first-heading-p)
    (cond
     ((my/org-reference--block-bounds)
      (my/org-reference-create-block-target))
     ((my/org-reference--display-math-bounds)
      (my/org-reference-create-formula-target))
     (t
      (user-error "Move point to a headline, a \\[...\\] formula, or a #+begin block"))))
   ((org-at-heading-p)
    (let ((id (org-id-get-create)))
      (kill-new id)
      (message "Org heading ID: %s" id)
      id))
   ((my/org-reference--block-bounds)
    (my/org-reference-create-block-target))
   ((my/org-reference--display-math-bounds)
    (my/org-reference-create-formula-target))
   (t
    (org-back-to-heading t)
    (let ((id (org-id-get-create)))
      (kill-new id)
      (message "Org heading ID: %s" id)
      id))))

(defun my/org-reference-create-target-dwim ()
  "Create or reuse a target at point, then copy its Org link.
This is the main interactive reference command: use it on a heading or inside
a \\[...\\] formula."
  (interactive)
  (my/org-copy-reference-link-at-point))

(defun my/org-reference--heading-link ()
  "Return an id link to the current heading, creating the ID if needed."
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    (format "[[id:%s]]" (org-id-get-create))))

(defun my/org-reference--formula-link ()
  "Return a link to the current \\[...\\] formula target, creating it if needed."
  (format "[[%s]]" (my/org-reference-create-formula-target)))

(defun my/org-reference--block-link ()
  "Return a link to the current #+begin block target, creating it if needed."
  (format "[[%s]]" (my/org-reference-create-block-target)))

(defun my/org-reference-link-at-point ()
  "Return an Org link to the current heading, \\[...\\] formula or block."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cond
   ((my/org-reference--block-bounds)
    (my/org-reference--block-link))
   ((my/org-reference--display-math-bounds)
    (my/org-reference--formula-link))
   ((org-before-first-heading-p)
    (user-error "Move point to a headline, a \\[...\\] formula, or a #+begin block"))
   (t
    (my/org-reference--heading-link))))

(defun my/org-copy-reference-link-at-point ()
  "Copy an open Org link to the current heading, \\[...\\] formula or block.
The copied text is [[target][ — the description and closing ]] are left for
the user to fill in."
  (interactive)
  (let* ((link (my/org-reference-link-at-point))
         (open (concat (substring link 0 -1) "[")))
    (kill-new open)
    (message "Copied Org link: %s" open)))

(defun my/org-insert-id-link ()
  "Insert an Org id link with prompted ID."
  (interactive)
  (let ((id (string-trim (read-string "ID: "))))
    (when (string-empty-p id)
      (user-error "ID is empty"))
    (insert (format "[[id:%s]]" id))))

(defun my/org-insert-target-link ()
  "Insert an Org dedicated-target link with prompted target."
  (interactive)
  (let ((target (string-trim (read-string "Target: "))))
    (when (string-empty-p target)
      (user-error "Target is empty"))
    (insert (format "[[%s]]" target))))

(defun my/org-reference--read-source ()
  "Read a source file or Org Roam node for reference insertion."
  (let* ((choice (completing-read "Reference from: "
                                  '("Org Roam node" "File path")
                                  nil t))
         (roam-p (string= choice "Org Roam node")))
    (if roam-p
        (progn
          (require 'org-roam)
          (let ((node (org-roam-node-read nil nil nil t "Roam node: ")))
            (list :kind 'roam
                  :file (org-roam-node-file node)
                  :id (org-roam-node-id node)
                  :title (org-roam-node-title node))))
      (let* ((current-file (buffer-file-name (buffer-base-buffer)))
             (file (read-file-name "Reference file: "
                                   (or (and current-file
                                            (file-name-directory current-file))
                                       default-directory)
                                   current-file
                                   t
                                   (and current-file
                                        (file-name-nondirectory current-file)))))
        (list :kind 'file
              :file (expand-file-name file)
              :title (file-name-base file))))))

(defun my/org-reference--candidate-line ()
  "Return a compact preview line at point."
  (string-trim
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun my/org-reference--compact-text (text &optional width)
  "Return TEXT as one compact preview line, truncated to WIDTH."
  (let* ((single-line (replace-regexp-in-string
                       "[ \t\n\r]+" " "
                       (string-trim (or text ""))))
         (width (or width 96)))
    (if (> (string-width single-line) width)
        (truncate-string-to-width single-line width nil nil "...")
      single-line)))

(defun my/org-reference--nearby-preview (pos)
  "Return a compact context preview near POS in the current buffer."
  (save-excursion
    (goto-char pos)
    (let ((line (my/org-reference--candidate-line)))
      (cond
       ((string-match-p "\\\\][ \t]*<<[^>\n]+>>[ \t]*\\'" line)
        (let ((end pos)
              begin)
          (when (search-backward "\\[" nil t)
            (setq begin (point)))
          (my/org-reference--compact-text
           (if begin
               (buffer-substring-no-properties begin end)
             line))))
       ((not (string-match-p "\\`[ \t]*<<[^>\n]+>>[ \t]*\\'" line))
        (my/org-reference--compact-text line))
       (t
        (let ((end (line-beginning-position))
              begin)
          (forward-line -1)
          (while (and (not (bobp))
                      (not (looking-at-p "[ \t]*\\(?:\\*+\\|#\\+begin_\\|#\\+end_\\)"))
                      (not (and begin (looking-at-p "[ \t]*$"))))
            (unless (looking-at-p "[ \t]*$")
              (setq begin (line-beginning-position)))
            (forward-line -1))
          (when (and (not begin)
                     (not (looking-at-p "[ \t]*$")))
            (setq begin (line-beginning-position)))
          (my/org-reference--compact-text
           (if begin
               (buffer-substring-no-properties begin end)
             line))))))))

(defun my/org-reference--target-formula-preview (pos)
  "Return nearby display math text before target at POS, or nil."
  (save-excursion
    (goto-char pos)
    (when (search-backward "\\]" nil t)
      (let ((end (point)))
        (when (search-backward "\\[" nil t)
          (my/org-reference--compact-text
           (buffer-substring-no-properties (point) end)
           140))))))

(defun my/org-reference--formula-target-p (target preview)
  "Return non-nil when TARGET/PREVIEW looks like a formula target."
  (or (string-prefix-p "eq-" (or target ""))
      (string-match-p "\\\\[([]" (or preview ""))))

(defun my/org-reference--heading-scope (pos)
  "Return outline path containing POS, or nil."
  (save-excursion
    (goto-char pos)
    (when (org-back-to-heading t)
      (my/org-reference--compact-text
       (mapconcat #'identity (org-get-outline-path t t) " / ")
       80))))

(defun my/org-reference--block-scope (pos)
  "Return nearest Org block type containing POS, or nil."
  (save-excursion
    (goto-char pos)
    (let ((case-fold-search t)
          begin type)
      (when (re-search-backward "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\b" nil t)
        (setq begin (point)
              type (downcase (match-string-no-properties 1)))
        (when (and (< begin pos)
                   (re-search-forward
                    (format "^[ \t]*#\\+end_%s\\b" (regexp-quote type)) nil t)
                   (<= pos (line-end-position)))
          type)))))

(defun my/org-reference--scan-block-scopes ()
  "Return block scope candidates in the current Org buffer."
  (let ((case-fold-search t)
        scopes)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)\\b.*$" nil t)
        (let* ((type (downcase (match-string-no-properties 1)))
               (begin (line-beginning-position))
               (label (format "%s block" type)))
          (when (re-search-forward
                 (format "^[ \t]*#\\+end_%s\\b" (regexp-quote type)) nil t)
            (push (list :kind 'scope-block
                        :target nil
                        :label label
                        :line (line-number-at-pos begin)
                        :pos begin
                        :begin begin
                        :end (line-end-position)
                        :block type
                        :preview (my/org-reference--compact-text
                                  (format "#+begin_%s" type)))
                  scopes)))))
    (nreverse scopes)))

(defun my/org-reference--latex-comment-line-p (pos)
  "Return non-nil when POS is on a LaTeX comment-style line."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (looking-at-p "[ \t]*%")))

(defun my/org-reference--scan-org-targets (file)
  "Return reference target candidates from Org FILE."
  (let (candidates)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-with-wide-buffer
       (let ((tree (org-element-parse-buffer)))
         (org-element-map tree 'headline
           (lambda (headline)
             (let* ((begin (org-element-property :begin headline))
                    (end (org-element-property :end headline))
                    (raw-title (org-element-property :raw-value headline))
                    (id (org-element-property :ID headline))
                    (custom-id (org-element-property :CUSTOM_ID headline))
                    (level (org-element-property :level headline)))
               (push (list :kind 'scope-heading
                           :target raw-title
                           :label raw-title
                           :line (line-number-at-pos begin)
                           :pos begin
                           :begin begin
                           :end end
                           :level level
                           :preview (format "%s %s"
                                            (make-string (max 0 (1- level)) ?*)
                                            raw-title))
                     candidates)
               (when id
                 (push (list :kind 'id
                             :target id
                             :label raw-title
                             :line (line-number-at-pos begin)
                             :pos begin
                             :preview (format "ID %s  %s" id raw-title))
                       candidates))
               (when custom-id
                 (push (list :kind 'custom-id
                             :target custom-id
                             :label raw-title
                             :line (line-number-at-pos begin)
                             :pos begin
                             :preview (format "CUSTOM_ID %s  %s" custom-id raw-title))
                       candidates)))))
         (org-element-map tree 'target
           (lambda (target)
             (let ((value (org-element-property :value target))
                   (begin (org-element-property :begin target)))
               (unless (my/org-reference--latex-comment-line-p begin)
                 (let* ((preview (or (my/org-reference--target-formula-preview begin)
                                     (my/org-reference--nearby-preview begin)))
                        (formula-p (my/org-reference--formula-target-p value preview))
                        (heading-scope (my/org-reference--heading-scope begin))
                        (block-scope (my/org-reference--block-scope begin)))
                   (push (list :kind (if formula-p 'formula 'target)
                               :target value
                               :label (if formula-p preview value)
                               :line (line-number-at-pos begin)
                               :pos begin
                               :begin begin
                               :end begin
                               :scope heading-scope
                               :block block-scope
                               :preview preview)
                         candidates)))))))
       (setq candidates (append candidates (my/org-reference--scan-block-scopes)))))
    (nreverse candidates)))

(defun my/org-reference--scan-text-targets (file)
  "Return lightweight reference candidates from non-Org FILE."
  (let ((ext (downcase (or (file-name-extension file) "")))
        candidates)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (line-number-at-pos))
               (pos (point))
               (text (my/org-reference--candidate-line)))
          (when (cond
                 ((member ext '("md" "markdown"))
                  (string-match-p "\\`#{1,6}[ \t]+\\S-" text))
                 ((member ext '("el" "py" "js" "ts" "tsx" "jsx" "rs" "go" "java" "c" "cpp" "h" "hpp" "sh" "zsh" "nix"))
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

(defun my/org-reference--file-targets (file)
  "Return reference candidates for FILE, including a file-level candidate."
  (let* ((org-p (string= (downcase (or (file-name-extension file) "")) "org"))
         (targets (if org-p
                      (my/org-reference--scan-org-targets file)
                    (my/org-reference--scan-text-targets file))))
    (cons (list :kind 'file
                :target nil
                :label (file-name-base file)
                :line 1
                :pos 1
                :preview (format "File: %s" (file-name-nondirectory file)))
          targets)))

(defun my/org-reference--candidate-in-scope-p (candidate scope)
  "Return non-nil when CANDIDATE is inside SCOPE."
  (let ((pos (plist-get candidate :pos))
        (begin (plist-get scope :begin))
        (end (plist-get scope :end)))
    (and pos begin end
         (< begin pos)
         (<= pos end))))

(defun my/org-reference--filter-scope-children (candidates scope)
  "Return immediate useful children of SCOPE from CANDIDATES."
  (let* ((scope-kind (plist-get scope :kind))
         (scope-level (plist-get scope :level))
         (scope-begin (plist-get scope :begin))
         (children (seq-filter
                    (lambda (candidate)
                      (and (not (eq candidate scope))
                           (not (= (or (plist-get candidate :pos) -1)
                                   (or scope-begin -2)))
                           (my/org-reference--candidate-in-scope-p candidate scope)))
                    candidates)))
    (if (eq scope-kind 'scope-heading)
        (seq-filter
         (lambda (candidate)
           (let ((kind (plist-get candidate :kind)))
             (not (and (eq kind 'scope-heading)
                       (<= (or (plist-get candidate :level) most-positive-fixnum)
                           (or scope-level 0))))))
         children)
      children)))

(defun my/org-reference--scope-candidate-p (target)
  "Return non-nil when TARGET can be drilled into."
  (memq (plist-get target :kind) '(scope-heading scope-block)))

(defun my/org-reference--target-candidates (file &optional targets)
  "Return propertized completion candidates for reference targets in FILE.
When TARGETS is non-nil, use that candidate plist list instead of scanning FILE."
  (mapcar
   (lambda (target)
     (propertize
      (let ((kind (plist-get target :kind)))
        (cond
         ((eq kind 'formula)
            (format "%-9s %5s  %-28s  %-12s  %-64s  %s"
                    kind
                    (plist-get target :line)
                    (my/org-reference--compact-text
                     (or (plist-get target :scope) "")
                     28)
                    (my/org-reference--compact-text
                     (or (plist-get target :block) "")
                     12)
                    (my/org-reference--compact-text
                     (or (plist-get target :preview) "")
                     64)
                    (or (plist-get target :target) "")))
         ((memq kind '(scope-heading scope-block))
          (format "%-9s %5s  %-40s  %s"
                  (if (eq kind 'scope-heading) 'heading 'block)
                  (plist-get target :line)
                  (my/org-reference--compact-text
                   (or (plist-get target :label) "")
                   40)
                  "TAB to enter"))
         (t
          (format "%-9s %5s  %-40s  %s"
                  kind
                  (plist-get target :line)
                  (my/org-reference--compact-text
                   (or (plist-get target :label) "")
                   40)
                  (my/org-reference--compact-text
                   (or (plist-get target :preview) "")
                   90)))))
      'my/org-reference-target target))
   (or targets (my/org-reference--file-targets file))))

(defun my/org-reference--target-preview-state (file)
  "Return Consult preview state for candidates in FILE."
  (if (not (fboundp 'consult--jump-preview))
      (lambda (&rest _))
    (require 'consult)
    (let ((preview (consult--jump-preview)))
      (lambda (action cand)
        (let* ((target (and cand (get-text-property 0 'my/org-reference-target cand)))
               (pos (plist-get target :pos)))
          (when (and target pos)
            (funcall preview action
                     (and (eq action 'preview)
                          (let ((marker (make-marker)))
                            (set-marker marker pos (find-file-noselect file))
                            marker)))))))))

(defun my/org-reference--minibuffer-selected-target ()
  "Return the current target plist selected in the minibuffer."
  (let* ((selected (cond
                    ((fboundp 'vertico--candidate)
                     (vertico--candidate))
                    (t
                     (minibuffer-contents))))
         (target (and selected
                      (get-text-property 0 'my/org-reference-target selected))))
    (or target
        (and selected
             (get-text-property
              0 'my/org-reference-target
              (seq-find (lambda (candidate)
                          (string= (substring-no-properties candidate)
                                   (substring-no-properties selected)))
                        my/org-reference--minibuffer-candidates))))))

(defun my/org-reference--drill-minibuffer ()
  "Enter the selected heading/block scope from the reference minibuffer."
  (interactive)
  (let ((target (my/org-reference--minibuffer-selected-target)))
    (cond
     ((my/org-reference--scope-candidate-p target)
      (throw 'my/org-reference-drill target))
     (target
      (message "This candidate is a target; press RET to insert it"))
     (t
      (message "No target selected")))))

(defun my/org-reference--target-read-keymap ()
  "Return keymap used by the target browser."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'my/org-reference--drill-minibuffer)
    (define-key map (kbd "<tab>") #'my/org-reference--drill-minibuffer)
    map))

(defun my/org-reference--read-target-from-candidates (file candidates prompt)
  "Read one target from CANDIDATES in FILE using PROMPT."
  (let ((formatted (my/org-reference--target-candidates file candidates)))
    (unless formatted
      (user-error "No reference targets found in this scope"))
    (let ((my/org-reference--minibuffer-candidates formatted))
      (if (fboundp 'consult--read)
          (progn
            (require 'consult)
            (consult--read formatted
                           :prompt prompt
                           :require-match t
                           :state (my/org-reference--target-preview-state file)
                           :preview-key '(:debounce 0.15 any)
                           :keymap (my/org-reference--target-read-keymap)
                           :lookup (apply-partially
                                    #'consult--lookup-prop
                                    'my/org-reference-target)))
        (let ((choice (completing-read prompt formatted nil t)))
          (or (get-text-property 0 'my/org-reference-target choice)
              (user-error "No target selected")))))))

(defun my/org-reference--read-target (file)
  "Read one reference target from FILE."
  (let* ((all-candidates (my/org-reference--file-targets file))
         (current all-candidates)
         (scope-labels nil))
    (unless all-candidates
      (user-error "No reference targets found in %s" file))
    (catch 'done
      (while t
        (let* ((prompt (if scope-labels
                           (format "Target [%s]: "
                                   (mapconcat #'identity
                                              (reverse scope-labels)
                                              " / "))
                         "Target: "))
               (choice
                (catch 'my/org-reference-drill
                  (my/org-reference--read-target-from-candidates
                   file current prompt))))
          (if (my/org-reference--scope-candidate-p choice)
              (let ((children (my/org-reference--filter-scope-children
                               all-candidates choice)))
                (unless children
                  (user-error "No targets under %s"
                              (plist-get choice :label)))
                (setq current children
                      scope-labels (cons (plist-get choice :label)
                                         scope-labels)))
            (throw 'done
                   (or (and (listp choice) choice)
                       (user-error "No target selected")))))))))

(defun my/org-reference--relative-file (file)
  "Return FILE path relative to the current buffer when possible."
  (let ((base (or (and buffer-file-name (file-name-directory buffer-file-name))
                  default-directory)))
    (file-relative-name file base)))

(defun my/org-reference--target-search (target)
  "Return Org file search suffix for TARGET plist."
  (pcase (plist-get target :kind)
    ('file nil)
    ('id (concat "*" (plist-get target :label)))
    ('scope-heading (concat "*" (plist-get target :target)))
    ('heading (concat "*" (plist-get target :target)))
    ('custom-id (concat "#" (plist-get target :target)))
    ('formula (plist-get target :target))
    ('target (plist-get target :target))
    ('line (number-to-string (plist-get target :line)))
    (_ nil)))

(defun my/org-reference--same-file-p (file)
  "Return non-nil when FILE is the current buffer file."
  (let ((current-file (buffer-file-name (buffer-base-buffer))))
    (and current-file
         file
         (file-equal-p (expand-file-name file)
                       (expand-file-name current-file)))))

(defun my/org-reference--build-link (source target description)
  "Build an Org link for SOURCE plist, TARGET plist and DESCRIPTION."
  (let* ((roam-p (eq (plist-get source :kind) 'roam))
         (desc (string-trim (or description "")))
         (desc (unless (string-empty-p desc) desc))
         (search (my/org-reference--target-search target))
         (raw
          (if (and roam-p (memq (plist-get target :kind) '(file id)))
              (format "id:%s"
                      (if (eq (plist-get target :kind) 'id)
                          (plist-get target :target)
                        (plist-get source :id)))
            (if (and search
                     (my/org-reference--same-file-p (plist-get source :file)))
                (or search "")
              (concat "file:" (my/org-reference--relative-file (plist-get source :file))
                      (when search
                        (concat "::" search)))))))
    (if desc
        (org-link-make-string raw desc)
      (org-link-make-string raw))))

(defun my/org-reference-insert-link ()
  "Interactively insert a robust Org reference link.
Choose an Org Roam node or ordinary file, pick a file/ID/target/heading with
preview, then enter the visible link text."
  (interactive)
  (let* ((source (my/org-reference--read-source))
         (file (plist-get source :file))
         (target (my/org-reference--read-target file))
         (default-description (or (plist-get target :label)
                                  (plist-get source :title)
                                  (file-name-base file)))
         (description (read-string
                       (format "Description (default %s): " default-description)
                       nil nil default-description))
         (link (my/org-reference--build-link source target description)))
    (insert link)
    (message "Inserted Org reference: %s" link)))

(defun my/org-download--buffer-basename ()
  "Return a stable basename for the current Org buffer."
  (file-name-base
   (or (buffer-file-name (buffer-base-buffer))
       (buffer-name (buffer-base-buffer)))))

(defun my/org-download-image-dir ()
  "Return the image directory used by `org-download' for this buffer."
  (file-name-concat my/org-download-image-root
                    (my/org-download--buffer-basename)))

(defun my/org-download-setup ()
  "Set up buffer-local `org-download' storage for Org buffers."
  (setq-local org-download-image-dir (my/org-download-image-dir)))

(defun my/org-download-file-format (filename)
  "Return a timestamped, non-conflicting image FILENAME."
  (let* ((base (concat (format-time-string org-download-timestamp) filename))
         (candidate base)
         (stem (file-name-sans-extension base))
         (ext (file-name-extension base t))
         (index 2))
    (while (file-exists-p (expand-file-name candidate (org-download--dir)))
      (setq candidate (format "%s-%d%s" stem index (or ext ""))
            index (1+ index)))
    candidate))

(defun my/org-download-clipboard ()
  "Insert a macOS clipboard image into the current Org buffer."
  (interactive)
  (require 'org-download)
  (unless (eq system-type 'darwin)
    (user-error "my/org-download-clipboard is configured for macOS only"))
  (unless (executable-find "pngpaste")
    (user-error "Install pngpaste first: brew install pngpaste"))
  (let ((org-download-screenshot-method "pngpaste %s"))
    (org-download-screenshot)))

(use-package org-download
  :ensure t
  :after org
  :hook (org-mode . my/org-download-setup)
  :custom
  (org-download-method 'directory)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-file-format-function #'my/org-download-file-format)
  :bind (:map org-mode-map
              ("C-c i d" . org-download-delete)
              ("C-c n I" . my/org-insert-id-link)
              ("C-c n T" . my/org-insert-target-link)
              ("C-c n p" . my/org-reference-insert-link)))

(provide 'init-org-utility)
;;; init-org-utility.el ends here

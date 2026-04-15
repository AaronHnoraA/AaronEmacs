;;; init-org-latex.el --- Org LaTeX export and preview helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(declare-function my/refresh-environment-from-shell nil)
(declare-function my/shell-command-executable "init-utils")
(declare-function org-fragtog--disable-frag "org-fragtog" (frag &optional renew))

(require 'init-org-core)
(require 'init-org-ui)

(use-package cdlatex
  :ensure t
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; org-appear 隐藏括号后 ^{} / _{} 内外无法区分。
  ;; 改为只插入开括号 ^{ / _{，不自动补全 }，由用户手动收尾。
  (with-eval-after-load 'org
    (keymap-set org-cdlatex-mode-map "^" (lambda () (interactive) (insert "^{")))
    (keymap-set org-cdlatex-mode-map "_" (lambda () (interactive) (insert "_{")))))

(use-package org-fragtog
  :ensure t
  :custom
  (org-fragtog-preview-delay 0.15)
  :hook (org-mode . my/org-enable-org-fragtog-maybe))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography pv/org-bibtex-files)
  (bibtex-completion-library-path (list pv/org-bibtex-dir))
  (bibtex-completion-pdf-open-function
   (lambda (fpath) (call-process "open" nil 0 nil fpath))))

(use-package org-ref
  :ensure t
  :after org)

;;;; 按需渲染：滚动停止后 idle 0.3s 预览可见区域（节流 + 去重）

(defgroup my/org-latex-preview nil
  "On-demand LaTeX preview helpers."
  :group 'org)

(defconst my/org-latex-export-magic-comment
  "% !TEX program = xelatex\n"
  "Magic comment prepended to Org-exported LaTeX files.")

(defconst my/org-latex-export-cjk-preamble
  "\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage{amsthm}\n\\usepackage{xeCJK}\n\\setCJKmainfont{Songti SC}\n\\setCJKsansfont{Hiragino Sans GB}\n"
  "Chinese package and font setup injected into Org-exported LaTeX preambles.")

(defconst my/org-latex-export-default-packages-alist
  '(("" "graphicx" t)
    ("" "longtable" nil)
    ("" "wrapfig" nil)
    ("" "rotating" nil)
    ("normalem" "ulem" t)
    ("" "capt-of" nil)
    ("" "hyperref" nil))
  "Package set used for Org LaTeX export.")

(defcustom my/org-latex-embedded-class-alist nil
  "Alist mapping LaTeX class names to embedded class sources.
Each value may be a readable `.cls' file path or literal class source."
  :type '(alist :key-type string :value-type string)
  :group 'org)

(defcustom my/org-latex-default-class-name "default"
  "Default LaTeX class used by Org export helpers."
  :type 'string
  :group 'org)

(defconst my/org-latex-default-sectioning
  '(("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "Fallback sectioning used for embedded Org LaTeX classes.")

(defcustom my/org-latex-preview-idle-delay 0.4
  "Idle delay (seconds) before previewing visible region after scrolling."
  :type 'number
  :group 'my/org-latex-preview)

(defcustom my/org-latex-preview-min-chars 400
  "Minimum visible region size (chars) required to trigger preview."
  :type 'integer
  :group 'my/org-latex-preview)

(defcustom my/org-latex-preview-max-concurrency 3
  "Maximum number of concurrent LaTeX preview render jobs."
  :type 'integer
  :group 'my/org-latex-preview)

(defcustom my/org-latex-preview-edit-idle-delay 0.2
  "Idle delay (seconds) before pre-rendering the fragment being edited."
  :type 'number
  :group 'my/org-latex-preview)

(defcustom my/latex-preview-math-font "GFS Neohellenic Math"
  "Fallback math font family used by XeLaTeX-based preview pipelines."
  :type 'string
  :group 'my/org-latex-preview)

(defun my/org-latex--source-directory (&optional info)
  "Return the source directory for Org export INFO."
  (let ((input-file (plist-get info :input-file)))
    (file-name-as-directory
     (expand-file-name
      (or (and input-file (file-name-directory input-file))
          default-directory)))))

(defun my/org-latex--project-root (&optional info)
  "Return the current project root for Org export INFO."
  (let* ((source-dir (my/org-latex--source-directory info))
         (project (when (fboundp 'project-current)
                    (project-current nil source-dir))))
    (file-name-as-directory
     (expand-file-name
      (or (and project
               (if (fboundp 'project-root)
                   (project-root project)
                 (car project)))
          source-dir)))))

(defun my/org-latex--class-search-directories (&optional info)
  "Return class search directories for Org export INFO."
  (delete-dups
   (delq nil
         (list
          (expand-file-name "latex" (my/org-latex--project-root info))
          (expand-file-name "latex" user-emacs-directory)))))

(defun my/org-latex--find-class-file (class &optional info)
  "Return the first readable `.cls' file for CLASS in export INFO."
  (catch 'file
    (dolist (dir (my/org-latex--class-search-directories info))
      (let ((candidate (expand-file-name (concat class ".cls") dir)))
        (when (file-readable-p candidate)
          (throw 'file candidate))))))

(defun my/org-latex--resolve-embedded-class-source (class info)
  "Return embedded class source for CLASS in export INFO, if configured."
  (let* ((configured-source (cdr (assoc class my/org-latex-embedded-class-alist)))
         (base-dir (my/org-latex--source-directory info))
         (class-file
          (cond
           ((and (stringp configured-source)
                 (not (string-match-p "\n" configured-source))
                 (file-readable-p configured-source))
            configured-source)
           ((and (stringp configured-source)
                 (not (string-match-p "\n" configured-source))
                 (file-readable-p (expand-file-name configured-source base-dir)))
            (expand-file-name configured-source base-dir))
           (t
            (my/org-latex--find-class-file class info)))))
    (cond
     (class-file
      (with-temp-buffer
        (insert-file-contents class-file)
        (buffer-string)))
     ((stringp configured-source)
      configured-source))))

(defun my/org-latex--make-filecontents-block (class source)
  "Return a `filecontents*' block that embeds CLASS with SOURCE."
  (concat "\\begin{filecontents*}[overwrite]{" class ".cls}\n"
          source
          (unless (string-suffix-p "\n" source)
            "\n")
          "\\end{filecontents*}\n"))

(defun my/org-latex--inject-embedded-class (latex info)
  "Embed a configured class definition into exported LATEX using INFO."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (when (re-search-forward "^\\\\documentclass\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}" nil t)
      (let* ((class (match-string 1))
             (source (my/org-latex--resolve-embedded-class-source class info)))
        (when (and source
                   (not (save-excursion
                          (goto-char (point-min))
                          (re-search-forward
                           (format "\\\\begin{filecontents\\*?}\\(?:\\[[^]]*\\]\\)?{%s\\.cls}"
                                   (regexp-quote class))
                           nil t))))
          (goto-char (match-beginning 0))
          (insert (my/org-latex--make-filecontents-block class source)))))
    (buffer-string)))

(defun my/org-latex--ensure-embedded-class-registered (info)
  "Register an embedded LaTeX class from INFO in `org-latex-classes'."
  (when-let* ((class (plist-get info :latex-class))
              (_ (my/org-latex--resolve-embedded-class-source class info)))
    (let ((classes (or (plist-get info :latex-classes) org-latex-classes)))
      (unless (assoc class classes)
        (let* ((class-options (plist-get info :latex-class-options))
               (documentclass
                (if class-options
                    (format "\\documentclass[%s]{%s}" class-options class)
                  (format "\\documentclass{%s}" class)))
               (article (assoc "article" classes))
               (sectioning (or (and article (cddr article))
                               my/org-latex-default-sectioning))
               (updated-classes (cons `(,class ,documentclass ,@sectioning)
                                      classes)))
          (setq org-latex-classes updated-classes)
          (setf (plist-get info :latex-classes) updated-classes))))))

(defun my/org-latex--latex-package-present-p (latex package)
  "Return non-nil when LATEX already loads PACKAGE."
  (catch 'found
    (dolist (line (split-string latex "\n"))
      (when (or (string-match-p "\\\\usepackage" line)
                (string-match-p "\\\\RequirePackage" line))
        (let ((open (string-match "{" line)))
          (when open
            (let ((close (string-match "}" line open)))
              (when close
                (dolist (pkg (split-string (substring line (1+ open) close)
                                           "," t "[[:space:]]*"))
                  (when (string= package (string-trim pkg))
                    (throw 'found t)))))))))
    nil))

(defun my/org-latex--latex-cjk-present-p (latex)
  "Return non-nil when LATEX already configures Chinese support."
  (or (my/org-latex--latex-package-present-p latex "xeCJK")
      (catch 'found
        (dolist (line (split-string latex "\n"))
          (when (and (or (string-match-p "\\\\documentclass" line)
                         (string-match-p "\\\\LoadClass" line))
                     (string-match-p "{ctex" line))
            (throw 'found t)))
        nil)))

(defun my/org-latex--keyword-value (keyword)
  "Return the first Org keyword value for KEYWORD in the current buffer."
  (when-let* ((entry (assoc keyword (org-collect-keywords (list keyword))))
              (raw (car (cdr entry))))
    (if (listp raw) (car raw) raw)))

(defconst my/org-latex-assignment-keyword-command-alist
  '(("ASSIGNMENT_INSTITUTION" . "assignmentinstitution")
    ("ASSIGNMENT_COURSE_CODE" . "assignmentcoursecode")
    ("ASSIGNMENT_TERM" . "assignmentterm")
    ("ASSIGNMENT_TITLE" . "assignmenttitle")
    ("ASSIGNMENT_DATE" . "assignmentdate")
    ("ASSIGNMENT_STUDENT_NAME" . "studentname")
    ("ASSIGNMENT_STUDENT_ID" . "studentid")
    ("ASSIGNMENT_AFFILIATION" . "assignmentaffiliation")
    ("ASSIGNMENT_AUTHOR_URL" . "authorurl"))
  "Org keyword to LaTeX command mapping for the `assignment' class.")

(defun my/org-latex--class-command-lines (info)
  "Return class-specific command lines derived from export INFO."
  (pcase (plist-get info :latex-class)
    ("assignment"
     (delq nil
           (mapcar
            (lambda (entry)
              (when-let* ((value (my/org-latex--keyword-value (car entry))))
                (format "\\%s{%s}" (cdr entry) value)))
            my/org-latex-assignment-keyword-command-alist)))
    (_ nil)))

(defun my/org-latex--inject-class-commands (latex info)
  "Insert class-specific LaTeX commands into exported LATEX using INFO."
  (if-let* ((commands (my/org-latex--class-command-lines info)))
      (with-temp-buffer
        (insert latex)
        (goto-char (point-min))
        (when (re-search-forward "^\\\\documentclass.*$" nil t)
          (end-of-line)
          (insert "\n" (mapconcat #'identity commands "\n") "\n"))
        (buffer-string))
    latex))

(defun my/org-latex--inject-export-preamble (latex)
  "Add XeLaTeX, math, and Chinese support boilerplate to exported LATEX."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (unless (looking-at-p (regexp-quote my/org-latex-export-magic-comment))
      (insert my/org-latex-export-magic-comment))
    (let (lines)
      (unless (my/org-latex--latex-package-present-p (buffer-string) "amsmath")
        (setq lines (append lines '("\\usepackage{amsmath}"))))
      (unless (my/org-latex--latex-package-present-p (buffer-string) "amssymb")
        (setq lines (append lines '("\\usepackage{amssymb}"))))
      (unless (my/org-latex--latex-package-present-p (buffer-string) "amsthm")
        (setq lines (append lines '("\\usepackage{amsthm}"))))
      (unless (my/org-latex--latex-cjk-present-p (buffer-string))
        (setq lines
              (append
               lines
               '("\\usepackage{xeCJK}"
                 "\\setCJKmainfont{Songti SC}"
                 "\\setCJKsansfont{Hiragino Sans GB}"))))
      (when lines
        (goto-char (point-min))
        (when (re-search-forward "^\\\\documentclass.*$" nil t)
          (end-of-line)
          (insert "\n" (mapconcat #'identity lines "\n") "\n"))))
    (buffer-string)))

(defun my/org-latex--strip-redundant-assignment-toc (latex info)
  "Remove Org's standalone TOC when the `assignment' class already provides one."
  (if (string= (plist-get info :latex-class) "assignment")
      (replace-regexp-in-string
       "\\\\maketitle[[:space:]\n\r]*\\\\tableofcontents[[:space:]\n\r]*"
       "\\\\maketitle\n"
       latex)
    latex))

(defun my/org-latex-template-advice (orig contents info)
  "Force Org LaTeX export through XeLaTeX-friendly Chinese defaults."
  (my/org-latex--ensure-embedded-class-registered info)
  (my/org-latex--inject-export-preamble
   (my/org-latex--strip-redundant-assignment-toc
    (my/org-latex--inject-class-commands
     (my/org-latex--inject-embedded-class
      (funcall orig contents info)
      info)
     info)
    info)))

(defun my/org-latex--available-class-names (&optional info)
  "Return available LaTeX class names for Org export INFO."
  (let (classes)
    (dolist (dir (my/org-latex--class-search-directories info))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir nil "\\.cls\\'"))
          (push (file-name-sans-extension file) classes))))
    (dolist (entry my/org-latex-embedded-class-alist)
      (push (car entry) classes))
    (setq classes (delete-dups (delq nil classes)))
    (sort classes #'string-lessp)))

(defun my/org-latex--default-class (&optional info)
  "Return the preferred LaTeX class name for Org export INFO."
  (let ((explicit (my/org-latex--keyword-value "LATEX_CLASS")))
    (or explicit
        (and (my/org-latex--find-class-file my/org-latex-default-class-name info)
             my/org-latex-default-class-name)
        my/org-latex-default-class-name)))

(defun my/org-latex--read-class (&optional info)
  "Read a LaTeX class name for Org export INFO."
  (let* ((default-class (my/org-latex--default-class info))
         (choices (or (my/org-latex--available-class-names info)
                      (list default-class))))
    (completing-read
     (format "LaTeX class (default %s): " default-class)
     choices nil t nil nil default-class)))

(defun my/org-latex--export-ext-plist (class)
  "Return export plist for LaTeX CLASS."
  `(:latex-class ,class :latex-compiler "xelatex"))

(defun my/org-latex--export-to-tex-with-class (class)
  "Export current Org buffer to LaTeX using CLASS."
  (let* ((export-info (my/org-latex--export-ext-plist class))
         (outfile (org-export-output-file-name ".tex" nil)))
    (my/org-latex--ensure-embedded-class-registered export-info)
    (org-export-to-file 'latex outfile nil nil nil nil
                        export-info)))

(defun my/org-latex--latexmk-command (texfile)
  "Return (PROJECT-ROOT . COMMAND) used to compile TEXFILE with latexmk."
  (let* ((project-root (my/org-latex--project-root))
         (relative-texfile (file-relative-name texfile project-root))
         (command
          (format "%s -xelatex -file-line-error -synctex=1 -interaction=nonstopmode %s"
                  (my/shell-command-executable "latexmk")
                  (shell-quote-argument relative-texfile))))
    (cons project-root command)))

(defun my/org-latex--compile-tex-in-project (texfile)
  "Compile TEXFILE to PDF with latexmk in the current project root."
  (when (fboundp 'my/refresh-environment-from-shell)
    (my/refresh-environment-from-shell))
  (pcase-let* ((`(,project-root . ,command)
                (my/org-latex--latexmk-command texfile))
               (default-directory project-root))
    (compilation-start command 'compilation-mode
                       (lambda (_mode)
                         (format "*Org LaTeX Export: %s*"
                                 (file-name-nondirectory texfile))))))

(defun my/org-latex-export-to-tex (&optional class)
  "Export current Org buffer to LaTeX using CLASS or the default class."
  (interactive)
  (let* ((resolved-class (or class (my/org-latex--default-class)))
         (texfile (my/org-latex--export-to-tex-with-class resolved-class)))
    (message "Exported %s with class %s" texfile resolved-class)
    texfile))

(defun my/org-latex-export-to-tex-with-class (class)
  "Prompt for CLASS and export current Org buffer to LaTeX."
  (interactive (list (my/org-latex--read-class)))
  (my/org-latex-export-to-tex class))

(defun my/org-latex-export-to-pdf (&optional class)
  "Export current Org buffer to PDF using CLASS or the default class."
  (interactive)
  (let* ((resolved-class (or class (my/org-latex--default-class)))
         (texfile (my/org-latex--export-to-tex-with-class resolved-class))
         (pdf-file (concat (file-name-sans-extension texfile) ".pdf")))
    (my/org-latex--compile-tex-in-project texfile)
    (message "Exporting %s with class %s via project latexmk" pdf-file resolved-class)
    pdf-file))

(defun my/org-latex-export-to-pdf-with-class (class)
  "Prompt for CLASS and export current Org buffer to PDF."
  (interactive (list (my/org-latex--read-class)))
  (my/org-latex-export-to-pdf class))

(defun my/latex-preview--preferred-math-font-file ()
  "Return the first preferred math-font file available on this machine."
  (let ((candidates
         (list (expand-file-name "~/Library/Fonts/GFSNeohellenicMath.otf")
               (expand-file-name "~/Library/Fonts/STIXTwoMath-Regular.ttf")
               (expand-file-name "~/Library/Fonts/LibertinusMath-Regular.otf")
               "/System/Library/Fonts/Supplemental/STIXTwoMath.otf")))
    (or
     (catch 'font
       (dolist (candidate candidates)
         (when (file-exists-p candidate)
           (throw 'font candidate))))
     (when (executable-find "kpsewhich")
       (let ((lm-math
              (string-trim
               (shell-command-to-string "kpsewhich latinmodern-math.otf 2>/dev/null"))))
         (unless (string-empty-p lm-math)
           lm-math))))))

(defun my/latex-preview-math-font-line ()
  "Return the `\\setmathfont' line used in preview snippet headers."
  (if-let* ((font-file (my/latex-preview--preferred-math-font-file)))
      (format "\\setmathfont[Path=%s]{%s}"
              (file-name-as-directory (file-name-directory font-file))
              (file-name-nondirectory font-file))
    (format "\\setmathfont{%s}" my/latex-preview-math-font)))

(defvar-local my/org-latex--preview-timer nil)
(defvar-local my/org-latex--last-preview-range nil)
(defvar-local my/org-latex--render-queue nil)
(defvar-local my/org-latex--render-running 0)
(defvar-local my/org-latex--render-processes nil)
(defvar-local my/org-latex--pending-renders nil)
(defvar-local my/org-latex--edit-preview-timer nil)
(defvar-local my/org-latex--edit-preview-marker nil)
(defvar-local my/org-latex--post-command-point nil)
(defvar-local my/org-latex--scroll-preview-enabled nil)
(defvar my/org-latex--allow-native-preview nil)

(defun my/org-latex--current-fragment (&optional pos)
  "Return the LaTeX fragment at POS, or at point when POS is nil."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((datum (org-element-context)))
      (when (org-element-type-p datum '(latex-environment latex-fragment))
        datum))))

(defun my/org-latex--fragment-range (frag)
  "Return the trimmed source range for LaTeX fragment FRAG."
  (when frag
    (let ((beg (org-element-property :begin frag))
          (end (save-excursion
                 (goto-char (org-element-end frag))
                 (skip-chars-backward " \r\t\n")
                 (point))))
      (cons beg end))))

(defun my/org-latex--point-editing-fragment-p (beg end)
  "Return non-nil when point is actively inside fragment range BEG END."
  (and (<= beg (point))
       (< (point) end)
       (let ((frag (my/org-latex--current-fragment)))
         (and frag
              (equal (my/org-latex--fragment-range frag)
                     (cons beg end))))))

(defun my/org-latex--preview-fragment (frag)
  "Queue or place preview for LaTeX fragment FRAG without toggle semantics."
  (when-let* ((range (my/org-latex--fragment-range frag))
              (beg (car range))
              (end (cdr range))
              ((< beg end)))
    (my/org-latex--preview-range beg end)
    t))

(defun my/org-latex--async-preview-active-p ()
  "Return non-nil when Org async LaTeX preview should be used here."
  (and (derived-mode-p 'org-mode)
       (display-graphic-p)
       (not my/org-latex--allow-native-preview)))

(defun my/org-latex--buffer-visible-p (&optional buffer)
  "Return non-nil when BUFFER is displayed in a live window."
  (get-buffer-window (or buffer (current-buffer)) t))

(defun my/org-latex--fragment-around-change (beg end)
  "Return the LaTeX fragment touched by a change from BEG to END."
  (or (my/org-latex--current-fragment beg)
      (and (> end beg)
           (my/org-latex--current-fragment (max beg (1- end))))
      (my/org-latex--current-fragment)))

(defun my/org-latex--cancel-edit-preview-timer ()
  "Cancel the pending edit-preview timer for the current buffer."
  (when (timerp my/org-latex--edit-preview-timer)
    (cancel-timer my/org-latex--edit-preview-timer))
  (setq my/org-latex--edit-preview-timer nil))

(defun my/org-latex--clear-edit-preview-marker ()
  "Release the marker used to track the fragment being edited."
  (when (markerp my/org-latex--edit-preview-marker)
    (set-marker my/org-latex--edit-preview-marker nil))
  (setq my/org-latex--edit-preview-marker nil))

(defun my/org-latex--run-edit-preview (buffer marker)
  "Pre-render the edited fragment in BUFFER tracked by MARKER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq marker my/org-latex--edit-preview-marker)
        (setq my/org-latex--edit-preview-timer nil)
        (when-let* (((marker-buffer marker))
                    (frag (my/org-latex--current-fragment
                           (marker-position marker))))
          (my/org-latex--preview-fragment frag))))))

(defun my/org-latex--schedule-edit-preview (frag)
  "Debounce async preview work for the fragment currently being edited."
  (when-let* ((range (my/org-latex--fragment-range frag))
              (beg (car range)))
    (my/org-latex--cancel-edit-preview-timer)
    (my/org-latex--clear-edit-preview-marker)
    (setq my/org-latex--edit-preview-marker (copy-marker beg))
    (setq my/org-latex--edit-preview-timer
          (run-with-idle-timer my/org-latex-preview-edit-idle-delay nil
                               #'my/org-latex--run-edit-preview
                               (current-buffer)
                               my/org-latex--edit-preview-marker))))

(defun my/org-latex-after-change-function (beg end _len)
  "Pre-render edited LaTeX fragments after changes between BEG and END."
  (when (my/org-latex--async-preview-active-p)
    (when-let* ((frag (my/org-latex--fragment-around-change beg end)))
      (my/org-latex--schedule-edit-preview frag))))

(defun my/org-latex-post-command-function ()
  "Re-enable preview when point leaves a LaTeX fragment."
  (when (my/org-latex--async-preview-active-p)
    (let* ((prev-point my/org-latex--post-command-point)
           (prev-frag (and prev-point
                           (my/org-latex--current-fragment prev-point)))
           (curr-frag (my/org-latex--current-fragment))
           (prev-range (and prev-frag (my/org-latex--fragment-range prev-frag)))
           (curr-range (and curr-frag (my/org-latex--fragment-range curr-frag))))
      (unless (equal prev-range curr-range)
        (when prev-frag
          (my/org-latex--preview-fragment prev-frag))
        (unless curr-frag
          (my/org-latex--clear-edit-preview-marker))))
    (setq my/org-latex--post-command-point (point))))

(defun my/org-latex--visible-range (&optional window)
  "Return (beg . end) for WINDOW's visible range in the current buffer."
  (when-let* ((win (cond
                    ((and (window-live-p window)
                          (eq (window-buffer window) (current-buffer)))
                     window)
                    ((eq (window-buffer (selected-window)) (current-buffer))
                     (selected-window))
                    (t
                     (get-buffer-window (current-buffer) t))))
              (beg (window-start win))
              (end (window-end win t)))
    (cons (max (point-min) (min beg end))
          (min (point-max) (max beg end)))))

(defun my/org-latex--range-similar-p (r1 r2)
  "Return non-nil if ranges R1 and R2 are similar enough to skip re-preview."
  (when (and r1 r2)
    (let ((b1 (car r1)) (e1 (cdr r1))
          (b2 (car r2)) (e2 (cdr r2)))
      (let* ((span (max 1 (- e2 b2)))
             (tol  (max 200 (/ span 6))))
        (and (<= (abs (- b1 b2)) tol)
             (<= (abs (- e1 e2)) tol))))))

(defun my/org-latex--ensure-state ()
  "Initialize async preview state for the current Org buffer."
  (unless (hash-table-p my/org-latex--pending-renders)
    (setq my/org-latex--pending-renders (make-hash-table :test 'equal))))

(defun my/org-latex--preview-base-directory ()
  "Return the directory used to store Org preview images."
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (base (if (or (not file) (file-remote-p file))
                   temporary-file-directory
                 default-directory)))
    (file-name-as-directory (file-truename base))))

(defun my/org-latex--make-waiter (spec)
  "Build a waiter from fragment SPEC."
  (list :beg (copy-marker (plist-get spec :beg))
        :end (copy-marker (plist-get spec :end) t)
        :value (plist-get spec :value)))

(defun my/org-latex--release-waiters (job)
  "Release marker resources tracked by JOB."
  (dolist (waiter (plist-get job :waiters))
    (set-marker (plist-get waiter :beg) nil)
    (set-marker (plist-get waiter :end) nil)))

(defun my/org-latex--waiter-present-p (job spec)
  "Return non-nil when JOB already tracks SPEC."
  (catch 'found
    (dolist (waiter (plist-get job :waiters))
      (let ((beg (plist-get waiter :beg))
            (end (plist-get waiter :end)))
        (when (and (marker-buffer beg)
                   (marker-buffer end)
                   (= (marker-position beg) (plist-get spec :beg))
                   (= (marker-position end) (plist-get spec :end))
                   (equal (plist-get waiter :value) (plist-get spec :value)))
          (throw 'found t))))
    nil))

(defun my/org-latex--add-waiter (job spec)
  "Attach SPEC to JOB unless it is already tracked."
  (unless (my/org-latex--waiter-present-p job spec)
    (setf (plist-get job :waiters)
          (cons (my/org-latex--make-waiter spec)
                (plist-get job :waiters)))))

(defun my/org-latex--place-preview (beg end value file imagetype)
  "Overlay FILE as preview between BEG and END when VALUE is unchanged."
  (when (and (file-exists-p file)
             (< beg end)
             (<= end (point-max))
             (string= (buffer-substring-no-properties beg end) value)
             (not (my/org-latex--point-editing-fragment-p beg end)))
    (org-clear-latex-preview beg end)
    (let ((max-image-size nil))
      (org--make-preview-overlay beg end file imagetype))))

(defun my/org-latex--place-waiter-preview (waiter file imagetype)
  "Place preview FILE for WAITER using IMAGETYPE."
  (let ((beg-marker (plist-get waiter :beg))
        (end-marker (plist-get waiter :end)))
    (when (and (marker-buffer beg-marker)
               (marker-buffer end-marker)
               (eq (marker-buffer beg-marker) (current-buffer)))
      (let ((beg (marker-position beg-marker))
            (end (marker-position end-marker)))
        (when (and beg end)
          (my/org-latex--place-preview
           beg end (plist-get waiter :value) file imagetype))))))

(defun my/org-latex--fragment-spec (beg end source-value render-value)
  "Return render metadata for a LaTeX fragment between BEG and END.
SOURCE-VALUE is the exact buffer text covered by the preview overlay.
RENDER-VALUE is the snippet sent to the LaTeX renderer."
  (save-excursion
    (goto-char beg)
    (let* ((processing-type org-preview-latex-default-process)
           (processing-info
            (or (cdr (assq processing-type org-preview-latex-process-alist))
                (user-error "Unknown Org LaTeX preview process: %s" processing-type)))
           (dir (my/org-latex--preview-base-directory))
           (prefix (concat org-preview-latex-image-directory "org-ltximg"))
           (face (or (face-at-point nil t) 'default))
           (fg
            (let ((color (plist-get org-format-latex-options :foreground)))
              (cond
               ((eq color 'auto) (face-attribute face :foreground nil 'default))
               ((eq color 'default) (face-attribute 'default :foreground nil))
               (t color))))
           (bg
            (let ((color (plist-get org-format-latex-options :background)))
              (cond
               ((eq color 'auto) (face-attribute face :background nil 'default))
               ((eq color 'default) (face-attribute 'default :background nil))
               (t color))))
           (hash (sha1 (prin1-to-string
                        (list org-format-latex-header
                              nil
                              nil
                              org-format-latex-options
                              t render-value fg bg))))
           (imagetype (or (plist-get processing-info :image-output-type) "png"))
           (movefile (format "%s_%s.%s"
                             (expand-file-name prefix dir)
                             hash
                             imagetype))
           (options
            (org-combine-plists
             org-format-latex-options
             `(:foreground ,fg :background ,bg))))
      (list :beg beg
            :end end
            :value source-value
            :render-value render-value
            :dir dir
            :file movefile
            :imagetype imagetype
            :options options
            :processing-type processing-type))))

(defun my/org-latex--collect-fragments (beg end)
  "Collect LaTeX fragments between BEG and END."
  (let ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
        fragments)
    (setq beg (max (point-min) (min beg end)))
    (setq end (min (point-max) (max beg end)))
    (when (< beg end)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward math-regexp end t)
          (let* ((context (org-element-context))
                 (type (org-element-type context)))
            (when (memq type '(latex-environment latex-fragment))
              (let* ((frag-beg (org-element-begin context))
                     (frag-end (save-excursion
                                 (goto-char (org-element-end context))
                                 (skip-chars-backward " \r\t\n")
                                 (point)))
                     (source-beg frag-beg)
                     (source-end frag-end)
                     (source-value
                      (buffer-substring-no-properties source-beg source-end))
                     (render-value source-value))
                (push (my/org-latex--fragment-spec
                       source-beg source-end source-value render-value)
                      fragments)
                ;; Keep point within the original search bound for the next
                ;; `re-search-forward'; large fragments may extend past END.
                (goto-char (min end (max (point) source-end)))))))
        (nreverse fragments)))))

(defun my/org-latex--cleanup-job-files (job)
  "Delete temporary files created for JOB."
  (let ((texfilebase (plist-get job :texfilebase)))
    (when texfilebase
      (dolist (ext (plist-get job :post-clean))
        (let ((target (concat texfilebase ext)))
          (when (file-exists-p target)
            (delete-file target)))))))

(defun my/org-latex--prepare-render (job)
  "Materialize the external render command for JOB."
  (let* ((processing-type (plist-get job :processing-type))
         (processing-info (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (image-output-type (plist-get processing-info :image-output-type))
         (post-clean (or (plist-get processing-info :post-clean)
                         '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                           ".svg" ".png" ".jpg" ".jpeg" ".out")))
         (latex-header
          (or (plist-get processing-info :latex-header)
              (let ((org-latex-default-packages-alist nil)
                    (org-latex-packages-alist nil))
                (org-latex-make-preamble
                 (org-export-get-environment (org-export-get-backend 'latex))
                 org-format-latex-header
                 'snippet))))
         (latex-header
          (if (listp latex-header)
              (mapconcat #'identity latex-header "")
            latex-header))
         (latex-compiler (plist-get processing-info :latex-compiler))
         (tmpdir (file-name-as-directory (file-truename temporary-file-directory)))
         (texfilebase (make-temp-name (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (options (plist-get job :options))
         (scale (* (car image-size-adjust)
                   (or (plist-get options :scale) 1.0)))
         (dpi (* scale (if (display-graphic-p) (org--get-display-dpi) 140.0)))
         (fg (or (plist-get options :foreground) "Black"))
         (bg (or (plist-get options :background) "Transparent"))
         (image-converter
          (or (and (string= bg "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (snippet (copy-sequence (plist-get job :value))))
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
        (setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (setq bg (cond
              ((eq bg 'default) (org-latex-color :background))
              ((string= bg "Transparent") nil)
              (t (org-latex-color-format bg))))
    (if (string-suffix-p "\n" snippet)
        (aset snippet (1- (length snippet)) ?%)
      (setq snippet (concat snippet "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" fg "}%\n"
              (if bg
                  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
                          "\n\\pagecolor{bg}%\n")
                "")
              "\n{\\color{fg}\n"
              snippet
              "\n}\n"
              "\n\\end{document}\n"))
    (let* ((err-msg
            (format "Please adjust `%s' part of `org-preview-latex-process-alist'."
                    processing-type))
           (image-input-file (concat texfilebase "." image-input-type))
           (converter-spec
            `((?D . ,(shell-quote-argument (format "%s" dpi)))
              (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))
           (command
            (let ((default-directory (plist-get job :dir)))
              (mapconcat
               #'identity
               (append
                (org-compile-file-commands texfile latex-compiler image-input-type nil err-msg)
                (org-compile-file-commands image-input-file image-converter image-output-type
                                           converter-spec err-msg))
               " && "))))
      (setf (plist-get job :command) command)
      (setf (plist-get job :texfilebase) texfilebase)
      (setf (plist-get job :image-output-file) (concat texfilebase "." image-output-type))
      (setf (plist-get job :post-clean) post-clean)
      job)))

(defun my/org-latex--render-sentinel (process _event)
  "Finalize PROCESS for an async Org LaTeX preview render."
  (when (memq (process-status process) '(exit signal))
    (let* ((buffer (process-get process 'my/org-latex-buffer))
           (job (process-get process 'my/org-latex-job))
           (target (plist-get job :file))
           (success (and (eq (process-status process) 'exit)
                         (= (process-exit-status process) 0)
                         (file-exists-p (plist-get job :image-output-file)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq my/org-latex--render-processes
                (delq process my/org-latex--render-processes))
          (setq my/org-latex--render-running
                (max 0 (1- my/org-latex--render-running)))
          (when (hash-table-p my/org-latex--pending-renders)
            (remhash target my/org-latex--pending-renders))
          (when success
            (make-directory (file-name-directory target) t)
            (copy-file (plist-get job :image-output-file) target t)
            (dolist (waiter (plist-get job :waiters))
              (my/org-latex--place-waiter-preview waiter target
                                                  (plist-get job :imagetype))))
          (unless success
            (message "[org-latex] Preview failed for %s"
                     (file-name-nondirectory target)))
          (my/org-latex--pump-render-queue)))
      (my/org-latex--release-waiters job)
      (my/org-latex--cleanup-job-files job))))

(defun my/org-latex--start-render (job)
  "Start an async render JOB for the current buffer."
  (let* ((buffer (current-buffer))
         (job (my/org-latex--prepare-render job))
         (default-directory (plist-get job :dir))
         (log-buffer (get-buffer-create "*Org Async LaTeX Preview*"))
         (process
          (make-process
           :name (format "org-latex-preview-%s"
                         (substring (sha1 (plist-get job :file)) 0 8))
           :buffer log-buffer
           :command (list shell-file-name shell-command-switch
                          (plist-get job :command))
           :noquery t
           :sentinel #'my/org-latex--render-sentinel)))
    (process-put process 'my/org-latex-buffer buffer)
    (process-put process 'my/org-latex-job job)
    (push process my/org-latex--render-processes)))

(defun my/org-latex--pump-render-queue ()
  "Start queued Org LaTeX renders up to the concurrency limit."
  (while (and my/org-latex--render-queue
              (< my/org-latex--render-running my/org-latex-preview-max-concurrency))
    (let ((job (pop my/org-latex--render-queue)))
      (setq my/org-latex--render-running (1+ my/org-latex--render-running))
      (condition-case err
          (my/org-latex--start-render job)
        (error
         (setq my/org-latex--render-running
               (max 0 (1- my/org-latex--render-running)))
         (when (hash-table-p my/org-latex--pending-renders)
           (remhash (plist-get job :file) my/org-latex--pending-renders))
         (my/org-latex--release-waiters job)
         (my/org-latex--cleanup-job-files job)
         (message "[org-latex] %s" (error-message-string err)))))))

(defun my/org-latex--enqueue-fragment (spec)
  "Place or queue preview work described by SPEC."
  (let ((target (plist-get spec :file)))
    (if (file-exists-p target)
        (my/org-latex--place-preview
         (plist-get spec :beg)
         (plist-get spec :end)
         (plist-get spec :value)
         target
         (plist-get spec :imagetype))
      (progn
        (make-directory (file-name-directory target) t)
        (my/org-latex--ensure-state)
        (let ((job (gethash target my/org-latex--pending-renders)))
          (if job
              (my/org-latex--add-waiter job spec)
            (setq job (list :dir (plist-get spec :dir)
                            :file target
                            :imagetype (plist-get spec :imagetype)
                            :options (plist-get spec :options)
                            :processing-type (plist-get spec :processing-type)
                            :value (plist-get spec :render-value)
                            :waiters (list (my/org-latex--make-waiter spec))))
            (puthash target job my/org-latex--pending-renders)
            (setq my/org-latex--render-queue
                  (nconc my/org-latex--render-queue (list job)))
            (my/org-latex--pump-render-queue)))))))

(defun my/org-latex--preview-range (beg end)
  "Queue LaTeX preview work between BEG and END."
  (setq beg (max (point-min) (min beg end)))
  (setq end (min (point-max) (max beg end)))
  (when (and (my/org-latex--async-preview-active-p)
             (or (called-interactively-p 'interactive)
                 (my/org-latex--buffer-visible-p))
             (< beg end))
    (dolist (spec (my/org-latex--collect-fragments beg end))
      (my/org-latex--enqueue-fragment spec))))

(defun my/org-latex--section-range ()
  "Return the current Org section range as (BEG . END)."
  (cons (if (org-before-first-heading-p) (point-min)
          (save-excursion
            (org-with-limited-levels (org-back-to-heading t) (point))))
        (org-with-limited-levels (org-entry-end-position))))

(defun my/org-latex-cancel-pending-renders ()
  "Cancel all queued and running async LaTeX preview renders."
  (interactive)
  (when (timerp my/org-latex--preview-timer)
    (cancel-timer my/org-latex--preview-timer))
  (my/org-latex--cancel-edit-preview-timer)
  (dolist (process my/org-latex--render-processes)
    (when (process-live-p process)
      (delete-process process)))
  (setq my/org-latex--preview-timer nil
        my/org-latex--last-preview-range nil
        my/org-latex--post-command-point nil
        my/org-latex--render-processes nil
        my/org-latex--render-queue nil
        my/org-latex--render-running 0)
  (my/org-latex--clear-edit-preview-marker)
  (when (hash-table-p my/org-latex--pending-renders)
    (maphash (lambda (_key job)
               (my/org-latex--release-waiters job))
             my/org-latex--pending-renders)
    (clrhash my/org-latex--pending-renders)))

(defun my/org-latex-preview-command (&optional arg)
  "Asynchronously preview Org LaTeX fragments like `org-latex-preview'."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ((and untrusted-content (not org--latex-preview-when-risky)) nil)
   ((equal arg '(64))
    (my/org-latex-cancel-pending-renders)
    (org-clear-latex-preview (point-min) (point-max))
    (message "LaTeX previews removed from buffer"))
   ((equal arg '(16))
    (my/org-latex--preview-range (point-min) (point-max))
    (message "Queueing LaTeX previews for buffer..."))
   ((equal arg '(4))
    (pcase-let ((`(,beg . ,end) (if (use-region-p)
                                    (cons (region-beginning) (region-end))
                                  (my/org-latex--section-range))))
      (my/org-latex-cancel-pending-renders)
      (org-clear-latex-preview beg end)
      (message "LaTeX previews removed")))
   ((use-region-p)
    (my/org-latex--preview-range (region-beginning) (region-end))
    (message "Queueing LaTeX previews for region..."))
   ((let ((datum (my/org-latex--current-fragment)))
      (and (org-element-type-p datum '(latex-environment latex-fragment))
           (pcase-let ((`(,beg . ,end) (my/org-latex--fragment-range datum)))
             (if (org-clear-latex-preview beg end)
                 (message "LaTeX preview removed")
               (my/org-latex--preview-fragment datum)
               (message "Queueing LaTeX preview..."))
             t))))
   (t
    (pcase-let ((`(,beg . ,end) (my/org-latex--section-range)))
      (my/org-latex--preview-range beg end)
      (message "Queueing LaTeX previews for section...")))))

(defun my/org-latex-preview-advice (orig &optional arg)
  "Route `org-latex-preview' through the async renderer in Org buffers."
  (if (or my/org-latex--allow-native-preview
          (not (derived-mode-p 'org-mode)))
      (funcall orig arg)
    (my/org-latex-preview-command arg)))

(defun my/org-latex-preview-visible-now (&optional window)
  "Preview visible Org LaTeX fragments asynchronously in WINDOW."
  (interactive)
  (when-let* (((derived-mode-p 'org-mode))
              ((my/org-latex--async-preview-active-p))
              (range (my/org-latex--visible-range window)))
    (let* ((beg (car range))
           (end (cdr range))
           (force (called-interactively-p 'interactive)))
      (when (and (or force
                     (> (- end beg) my/org-latex-preview-min-chars))
                 (or force
                     (not (my/org-latex--range-similar-p
                           my/org-latex--last-preview-range
                           range))))
        (setq my/org-latex--last-preview-range range)
        (my/org-latex--preview-range beg end)))))

(defun my/org-latex-preview-visible-debounced (&optional window)
  "Debounced preview of WINDOW's visible area after scrolling stops."
  (when (and (derived-mode-p 'org-mode)
             (my/org-latex--async-preview-active-p)
             (my/org-latex--buffer-visible-p))
    (when (timerp my/org-latex--preview-timer)
      (cancel-timer my/org-latex--preview-timer))
    (setq my/org-latex--preview-timer
          (run-with-idle-timer my/org-latex-preview-idle-delay nil
                               #'my/org-latex-preview-visible-now
                               window))))

(defun my/org-latex--window-scroll-preview-hook (win _start)
  "Schedule preview refresh after WIN scrolls."
  (when (eq (window-buffer win) (current-buffer))
    (my/org-latex-preview-visible-debounced win)))

(defun my/org-latex--window-size-preview-hook (_frame)
  "Schedule preview refresh after a window-size change."
  (my/org-latex-preview-visible-debounced
   (get-buffer-window (current-buffer) t)))

(defun my/org-latex-cleanup-scroll-preview ()
  "Stop async scroll-preview state in the current Org buffer."
  (interactive)
  (setq-local my/org-latex--scroll-preview-enabled nil)
  (my/org-latex-cancel-pending-renders))

(defun my/org-latex-preview-visible-initial (buffer)
  "Trigger one visible-area LaTeX preview for BUFFER once it is displayable."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* (((derived-mode-p 'org-mode))
                  (window (get-buffer-window buffer t)))
        ;; Force one initial render path instead of waiting for the first
        ;; scroll-driven debounce cycle.
        (setq my/org-latex--last-preview-range nil)
        (condition-case err
            (my/org-latex-preview-visible-now window)
          (error
           (message "Org LaTeX initial preview skipped: %s"
                    (error-message-string err))))))))

(defun my/org-fragtog-enable-frag-advice (orig frag)
  "Make `org-fragtog' re-enable FRAG through async preview."
  (if (not (my/org-latex--async-preview-active-p))
      (funcall orig frag)
    (save-excursion
      (org-fragtog--disable-frag frag)
      (my/org-latex--preview-fragment frag))))

(defun my/org-latex-enable-scroll-preview ()
  "Enable on-demand LaTeX preview for visible area after scrolling."
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (not my/org-latex--scroll-preview-enabled))
    (setq-local my/org-latex--scroll-preview-enabled t)
    (my/org-latex--ensure-state)
    (add-hook 'after-change-functions #'my/org-latex-after-change-function nil t)
    (add-hook 'post-command-hook #'my/org-latex-post-command-function nil t)
    (add-hook 'window-scroll-functions #'my/org-latex--window-scroll-preview-hook nil t)
    (add-hook 'window-size-change-functions #'my/org-latex--window-size-preview-hook nil t)
    (add-hook 'change-major-mode-hook #'my/org-latex-cleanup-scroll-preview nil t)
    (add-hook 'kill-buffer-hook #'my/org-latex-cleanup-scroll-preview nil t)
    (run-with-idle-timer 0.15 nil
                         #'my/org-latex-preview-visible-initial
                         (current-buffer))))

(add-hook 'org-mode-hook #'my/org-latex-enable-scroll-preview)

;; 手动刷新绑定
(with-eval-after-load 'org
  (advice-add 'org-latex-preview :around #'my/org-latex-preview-advice)
  (define-key org-mode-map (kbd "C-c C-x C-l") #'my/org-latex-preview-command)
  (define-key org-mode-map (kbd "C-c C-x v") #'my/org-latex-preview-visible-now))

(with-eval-after-load 'org-fragtog
  (advice-add 'org-fragtog--enable-frag :around #'my/org-fragtog-enable-frag-advice))

(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "xelatex")
  (setq org-latex-default-class my/org-latex-default-class-name)
  (unless (assoc my/org-latex-default-class-name org-latex-classes)
    (add-to-list 'org-latex-classes
                 `(,my/org-latex-default-class-name
                   ,(format "\\documentclass{%s}" my/org-latex-default-class-name)
                   ,@my/org-latex-default-sectioning)))
  (setq org-latex-default-packages-alist
        (copy-tree my/org-latex-export-default-packages-alist))
  (setq org-latex-packages-alist nil)
  (advice-add 'org-latex-template :around #'my/org-latex-template-advice))

(with-eval-after-load 'org
  (setq org-latex-default-packages-alist
        (copy-tree my/org-latex-export-default-packages-alist))
  (setq org-latex-packages-alist nil)

  ;; 2. 定义处理程序 (保持不变)
  (let ((tool (expand-file-name "tools/org-xdvisvgm-hires" user-emacs-directory)))
    (add-to-list 'org-preview-latex-process-alist
                 `(xdvisvgm-hires-script
                   :programs ("xelatex" "dvisvgm")
                   :description "xelatex -> xdv -> (dvisvgm via script) -> svg"
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.0 . 1.0)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -halt-on-error -output-directory %o %f")
                   :image-converter (,(format "%s %%f %%O" (shell-quote-argument tool))))))

  (setq org-preview-latex-default-process 'xdvisvgm-hires-script)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; 3. 极简 Header (确保没有占位符)
  (setq org-format-latex-header
        (format "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{fontspec}
\\usepackage{unicode-math}
%s
\\pagestyle{empty}
"
                (my/latex-preview-math-font-line))))

(provide 'init-org-latex)
;;; init-org-latex.el ends here

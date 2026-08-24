;;; init-auctex.el --- AUCTeX settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(declare-function my/typography-setup-prose-buffer "init-base")
(declare-function my/refresh-environment-from-shell nil)
(declare-function my/shell-command-executable "init-utils")
(declare-function my/mouse-code-actions "init-mouse" (event))
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function TeX-command-run-all "tex" (arg))
(declare-function TeX-master-directory "tex" ())
(declare-function TeX-master-file "tex" (&optional extension nondirectory ask))
(declare-function TeX-master-output-file "tex" (extension))
(declare-function TeX-view "tex" ())
(declare-function TeX-view-mouse "tex" (event))
(declare-function completion-preview-mode "completion-preview" (&optional arg))
(declare-function pdf-sync-backward-search-mouse "pdf-sync" (ev))
(declare-function pdf-sync-locate-synctex-file "pdf-sync" (pdffile))
(declare-function texpresso "texpresso" (&optional filename))
(declare-function texpresso-display-output "texpresso" ())
(declare-function texpresso-mode "texpresso" (&optional arg))
(declare-function texpresso-move-to-cursor "texpresso" (&optional position))

;; --- XeLaTeX 与 pdflatex 的编译命令，注入 -synctex=1 ---

(defvar TeX-source-correlate-map)
(defvar TeX-auto-local)
(defvar TeX-auto-save-aggregate)
(defvar TeX-current-process-region-p)
(defvar TeX-default-extension)
(defvar TeX-engine)
(defvar TeX-style-path)
(defvar completion-preview-mode)
(defvar lsp-mode-map)
(defvar texpresso--process)
(defvar texpresso--state)
(defvar texpresso-arguments)
(defvar texpresso-binary)
(defvar texpresso-distribution)
(defvar texpresso-follow-cursor)
(defvar texpresso-follow-edition)
(defvar texpresso-mode)

(defconst my/texpresso-root
  (expand-file-name "var/texpresso/" user-emacs-directory)
  "Local TeXpresso source and build root.")

(defconst my/texpresso-elisp-directory
  (expand-file-name "emacs/" my/texpresso-root)
  "Directory containing the upstream TeXpresso Emacs mode.")

(defconst my/texpresso-binary
  (expand-file-name "build/texpresso" my/texpresso-root)
  "Locally built TeXpresso executable.")

(defconst my/auctex-xelatexmk-command
  (concat (my/shell-command-executable "latexmk")
          " -xelatex -synctex=1 %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t")
  "latexmk command that explicitly builds with XeLaTeX.")

(defconst my/auctex-pdflatexmk-command
  (concat (my/shell-command-executable "latexmk")
          " -pdf -synctex=1 %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t")
  "latexmk command that explicitly builds with pdfLaTeX.")

(defun my/auctex-refresh-shell-environment (&rest _)
  "Refresh Emacs environment before starting TeX commands."
  (when (fboundp 'my/refresh-environment-from-shell)
    (my/refresh-environment-from-shell)))

(defun my/pdf-tools-activate ()
  "Enable PDF Tools without forcing an unnecessary rebuild.

If the bundled `epdfinfo' is already executable and healthy, reuse it.
Return non-nil when the PDF Tools server is usable.  Missing or broken
`epdfinfo' should not abort Emacs startup."
  (let ((bundled-epdfinfo
         (expand-file-name "elpa/pdf-tools-20260102.1101/epdfinfo"
                           user-emacs-directory)))
    (when (file-executable-p bundled-epdfinfo)
      (setq pdf-info-epdfinfo-program bundled-epdfinfo))
    (cond
     ((ignore-errors (pdf-info-check-epdfinfo) t)
      (pdf-tools-install)
      t)
     ((ignore-errors
        (pdf-tools-install :no-query)
        (pdf-info-check-epdfinfo)
        t)
      t)
     (t
      (display-warning
       'init-auctex
       (format "PDF Tools disabled because epdfinfo is not executable: %s"
               pdf-info-epdfinfo-program)
       :warning)
      nil))))

(defun my/auctex-register-command (entry)
  "Register TeX command ENTRY without duplicating existing items."
  (setq TeX-command-list
        (cons entry (assoc-delete-all (car entry) TeX-command-list))))

(config-defvar my/pdf-view-auto-refresh-interval nil
  "Polling fallback interval for PDF buffers when file notifications are absent."
  :type 'number
  :group 'TeX-command)

(config-defvar my/auctex-auto-cache-root
  (expand-file-name "var/auctex/auto/" user-emacs-directory)
  "Root directory for generated AUCTeX auto style files."
  :type 'directory
  :group 'TeX-file)

(defun my/auctex--source-cache-key (&optional directory)
  "Return a stable cache key for DIRECTORY."
  (secure-hash 'sha1 (expand-file-name (or directory default-directory))))

(defun my/auctex--cache-directory (root &optional directory)
  "Return a per-source cache directory under ROOT for DIRECTORY."
  (file-name-as-directory
   (expand-file-name (my/auctex--source-cache-key directory) root)))

(defun my/auctex--master-directory ()
  "Return the current TeX master directory, falling back to `default-directory'."
  (or (ignore-errors (TeX-master-directory))
      default-directory))

(defun my/auctex-setup-auto-cache ()
  "Keep generated AUCTeX style information outside project directories."
  (let* ((source-dir (my/auctex--master-directory))
         (old-auto TeX-auto-local)
         (old-auto-dir (and old-auto (expand-file-name old-auto source-dir)))
         (auto-dir (my/auctex--cache-directory
                    my/auctex-auto-cache-root
                    source-dir)))
    (make-directory auto-dir t)
    (setq-local TeX-auto-local auto-dir
                TeX-auto-save-aggregate t)
    (when (boundp 'TeX-style-path)
      (setq-local
       TeX-style-path
       (cons auto-dir
             (cl-remove-if
              (lambda (path)
                (and old-auto-dir
                     (stringp path)
                     (string= (expand-file-name path source-dir)
                              old-auto-dir)))
              (remove auto-dir TeX-style-path)))))))

(defun my/latex-preview--master-tex-file ()
  "Return the current AUCTeX master TeX file, never the region file."
  (expand-file-name (TeX-master-file t nil t)))

(defun my/latex-preview--tex-window-event-p (event)
  "Return non-nil when EVENT happened in a TeX or LaTeX buffer."
  (let* ((start (event-start event))
         (window (and start (posn-window start)))
         (buffer (and (windowp window) (window-buffer window))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (derived-mode-p 'LaTeX-mode 'TeX-mode 'latex-mode 'tex-mode)))))

(defun my/latex-preview--output-pdf-file ()
  "Return the current AUCTeX master PDF path, never `_region_.pdf'."
  (expand-file-name (TeX-master-output-file "pdf")))

(defun my/auctex-setup-build-workflow ()
  "Prefer latexmk-based builds in LaTeX buffers."
  (my/auctex-setup-auto-cache)
  (when (eq TeX-engine 'default)
    (setq-local TeX-engine 'xetex))
  (setq-local TeX-command-default
              (if (eq TeX-engine 'xetex) "XeLaTeXMk" "PdfLaTeXMk"))
  (setq-local TeX-save-query nil))

(defun my/pdf-view-enable-auto-refresh ()
  "Auto-refresh PDF buffers when the underlying file changes."
  (setq-local auto-revert-use-notify t
              auto-revert-avoid-polling nil
              auto-revert-interval my/pdf-view-auto-refresh-interval)
  (auto-revert-mode 1))

(defconst my/pdf-tools-enabled-modes
  '(pdf-history-minor-mode
    pdf-isearch-minor-mode
    pdf-links-minor-mode
    pdf-misc-minor-mode
    pdf-outline-minor-mode
    pdf-misc-size-indication-minor-mode
    pdf-misc-menu-bar-minor-mode
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-misc-context-menu-minor-mode
    pdf-cache-prefetch-minor-mode
    pdf-occur-global-minor-mode)
  "PDF Tools minor modes that should be active by default.")

(defun my/pdf-view-enable-capabilities ()
  "Enable the full PDF Tools interaction stack in the current PDF buffer."
  (dolist (mode my/pdf-tools-enabled-modes)
    (when (fboundp mode)
      (funcall mode 1))))

(defun my/pdf-view-mouse-follow-link (event)
  "Follow the PDF link at mouse EVENT, if one exists."
  (interactive "e")
  (let* ((pos (event-start event))
         (window (posn-window pos))
         (image-pos (posn-object-x-y pos)))
    (unless (and (windowp window) image-pos)
      (user-error "Mouse is not over a PDF page"))
    (with-selected-window window
      (let* ((page (if pdf-view-roll-minor-mode
                       (/ (+ 3 (posn-point pos)) 4)
                     (pdf-view-current-page window)))
             (relative-pos (pdf-util-scale-pixel-to-relative
                            image-pos nil t window))
             (link (cl-find-if
                    (lambda (candidate)
                      (pdf-util-edges-inside-p
                       (alist-get 'edges candidate)
                       relative-pos
                       0.01))
                    (pdf-cache-pagelinks page))))
        (if link
            (pdf-links-action-perform link)
          (message "No PDF link at click position"))))))

(defun my/pdf-view-setup-interaction ()
  "Configure local interaction keys for PDF buffers."
  (local-set-key (kbd "<down>") #'pdf-view-next-page-command)
  (local-set-key (kbd "<up>") #'pdf-view-previous-page-command)
  (local-set-key (kbd "M-r") #'revert-buffer)
  (local-set-key [mouse-2] #'my/pdf-view-mouse-follow-link)
  (local-set-key [double-mouse-1] #'my/pdf-view-mouse-follow-link)
  (local-set-key (kbd "TAB") #'pdf-outline)
  (local-set-key (kbd "C-c C-o") #'pdf-outline)
  (local-set-key (kbd "C-c C-s") #'pdf-occur)
  (when (featurep 'evil)
    (dolist (state '(normal motion))
      (evil-local-set-key state (kbd "j") #'pdf-view-next-page-command)
      (evil-local-set-key state (kbd "k") #'pdf-view-previous-page-command)
      (evil-local-set-key state (kbd "<down>") #'pdf-view-next-page-command)
      (evil-local-set-key state (kbd "<up>") #'pdf-view-previous-page-command)
      (evil-local-set-key state (kbd "M-r") #'revert-buffer)
      (evil-local-set-key state (kbd "TAB") #'pdf-outline)
      (evil-local-set-key state (kbd "go") #'pdf-outline)
      (evil-local-set-key state (kbd "gs") #'pdf-occur))))

(defun my/pdf-view-configure-open-buffers ()
  "Enable PDF Tools capabilities in already-open PDF buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'pdf-view-mode)
        (my/pdf-view-enable-capabilities)
        (my/pdf-view-enable-auto-refresh)
        (my/pdf-view-setup-interaction)))))

(defun my/pdf-sync--open-pdf-candidates ()
  "Return currently opened PDF buffer files."
  (let (pdfs)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'pdf-view-mode)
                   buffer-file-name)
          (push (expand-file-name buffer-file-name) pdfs))))
    (nreverse pdfs)))

(defun my/pdf-sync--master-pdf-candidate ()
  "Return the expected master PDF for the current TeX buffer, if known."
  (when (and buffer-file-name
             (boundp 'TeX-master)
             (stringp TeX-master))
    (expand-file-name
     (concat (file-name-sans-extension TeX-master) ".pdf")
     (file-name-directory buffer-file-name))))

(config-defvar my/pdf-sync-parent-search-depth nil
  "How many parent directories to search for a master SyncTeX file."
  :type 'integer
  :group 'TeX-view)

(defun my/pdf-sync--ancestor-directories (dir)
  "Return DIR and up to `my/pdf-sync-parent-search-depth' parents."
  (let ((current (and dir (file-name-as-directory (expand-file-name dir))))
        (depth 0)
        roots)
    (while (and current (<= depth my/pdf-sync-parent-search-depth))
      (push current roots)
      (let* ((trimmed (directory-file-name current))
             (parent (file-name-directory trimmed)))
        (setq current
              (unless (or (null parent)
                          (string= current parent))
                (file-name-as-directory parent))))
      (setq depth (1+ depth)))
    (nreverse roots)))

(defun my/pdf-sync--search-roots ()
  "Return candidate roots used to search for SyncTeX databases."
  (delete-dups
   (delq nil
         (append
          (when-let* ((master-pdf (my/pdf-sync--master-pdf-candidate)))
            (list (file-name-directory master-pdf)))
          (when-let* ((project (and (fboundp 'project-current)
                                    (project-current nil))))
            (list (expand-file-name
                   (if (fboundp 'project-root)
                       (project-root project)
                     (car project)))))
          (my/pdf-sync--ancestor-directories
           (or (and buffer-file-name (file-name-directory buffer-file-name))
               default-directory))))))

(defun my/pdf-sync--project-pdf-candidates ()
  "Return PDF candidates inferred from SyncTeX databases in the project."
  (let (pdfs)
    (dolist (root (my/pdf-sync--search-roots))
      (when (and root (file-directory-p root))
        (condition-case nil
            (dolist (synctex (directory-files-recursively
                              root "\\.synctex\\(?:\\.gz\\)?\\'"))
              (let ((pdf (replace-regexp-in-string
                          "\\.synctex\\(?:\\.gz\\)?\\'" ".pdf" synctex)))
                (when (file-exists-p pdf)
                  (push pdf pdfs))))
          (file-error nil))))
    (nreverse pdfs)))

(defun my/pdf-sync-master-pdf-for-current-buffer ()
  "Return the best matching master PDF for the current TeX buffer."
  (when buffer-file-name
    (let (candidates)
      (dolist (pdf (my/pdf-sync--open-pdf-candidates))
        (push pdf candidates))
      (when-let* ((master-pdf (my/pdf-sync--master-pdf-candidate)))
        (push master-pdf candidates))
      (dolist (pdf (my/pdf-sync--project-pdf-candidates))
        (push pdf candidates))
      (catch 'match
        (dolist (pdf (delete-dups (nreverse candidates)))
          (when (and (file-exists-p pdf)
                     (pdf-sync-locate-synctex-file pdf)
                     (pdf-sync-synctex-file-name buffer-file-name pdf))
            (throw 'match pdf)))))))

(defun my/pdf-sync-forward-correlate-with-pdf (pdf &optional line column)
  "Run forward search for LINE and COLUMN against PDF."
  (unless line
    (setq line (line-number-at-pos nil t)))
  (unless column
    (setq column (current-column)))
  (let ((source (or (pdf-sync-synctex-file-name (buffer-file-name) pdf)
                    (buffer-file-name))))
    (cons pdf
          (condition-case err
              (let-alist (pdf-info-synctex-forward-search source line column pdf)
                (cons .page .edges))
            (error
             (message "%s" (error-message-string err))
             (list nil nil nil nil nil))))))

(defun my/pdf-sync-forward-correlate-advice (orig &optional line column)
  "Resolve included TeX subfiles against the matching master SyncTeX file."
  (if-let* (((and buffer-file-name
                  (or (derived-mode-p 'TeX-mode)
                      (derived-mode-p 'latex-mode))))
            (pdf (my/pdf-sync-master-pdf-for-current-buffer)))
      (my/pdf-sync-forward-correlate-with-pdf pdf line column)
    (funcall orig line column)))

(defun my/pdf-sync-forward-search-with-pdf (pdf &optional line column)
  "Display the PDF location for LINE and COLUMN in PDF."
  (cl-destructuring-bind (resolved-pdf page _x1 y1 _x2 _y2)
      (my/pdf-sync-forward-correlate-with-pdf pdf line column)
    (let ((buffer (or (find-buffer-visiting resolved-pdf)
                      (find-file-noselect resolved-pdf))))
      (with-selected-window (display-buffer buffer pdf-sync-forward-display-action)
        (pdf-util-assert-pdf-window)
        (when page
          (pdf-view-goto-page page (selected-window))
          (when y1
            (let ((top (* y1 (cdr (pdf-view-image-size)))))
              (pdf-util-tooltip-arrow (round top))))))
      (with-current-buffer buffer
        (run-hooks 'pdf-sync-forward-hook)))))

(defun my/TeX-view-subfile-advice (orig &rest args)
  "Redirect `TeX-view' from included subfiles to the real master PDF."
  (let ((output-file (my/latex-preview--output-pdf-file)))
    (if (or (file-exists-p output-file)
            (not buffer-file-name))
        (apply orig args)
      (if-let* ((pdf (my/pdf-sync-master-pdf-for-current-buffer)))
          (if (and TeX-source-correlate-mode
                   (fboundp 'pdf-sync-forward-search))
              (my/pdf-sync-forward-search-with-pdf pdf)
            (pop-to-buffer (or (find-buffer-visiting pdf)
                               (find-file-noselect pdf))))
        (apply orig args)))))

(defun my/pdf-view-display-in-right-window (buffer &optional _alist)
  "Display PDF BUFFER in a reusable right-side window."
  (let ((window (or (get-buffer-window buffer)
                    (split-window (selected-window) nil 'right))))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window nil)
    (window-preserve-size window t nil)
    window))

;; =========================
;; AUCTeX 基础配置
;; =========================

(defun my/latex-preview--ensure-tex-buffer ()
  "Signal unless the current buffer is a LaTeX/AUCTeX buffer."
  (unless (and buffer-file-name
               (or (derived-mode-p 'LaTeX-mode 'TeX-mode)
                   (derived-mode-p 'latex-mode 'tex-mode)))
    (user-error "Not in a file-backed LaTeX buffer")))

(defun my/texpresso--ensure-available ()
  "Load the locally built TeXpresso integration or report how to install it."
  (unless (file-executable-p my/texpresso-binary)
    (user-error "TeXpresso is not built; run `make texpresso-install' in %s"
                user-emacs-directory))
  (unless (featurep 'texpresso)
    (unless (file-readable-p
             (expand-file-name "texpresso.el" my/texpresso-elisp-directory))
      (user-error "TeXpresso Emacs mode is missing; run `make texpresso-install'"))
    (let ((load-path (cons my/texpresso-elisp-directory load-path)))
      (unless (require 'texpresso nil t)
        (user-error "Cannot load the TeXpresso Emacs mode from %s"
                    my/texpresso-elisp-directory))))
  (setq texpresso-binary my/texpresso-binary
        texpresso-distribution 'texlive
        texpresso-arguments nil
        texpresso-follow-edition t
        texpresso-follow-cursor nil))

(defun my/texpresso-running-p ()
  "Return non-nil when the TeXpresso viewer process is running."
  (and (featurep 'texpresso)
       (process-live-p texpresso--process)))

(defun my/texpresso-running-for-current-document-p ()
  "Return non-nil when TeXpresso is showing the current AUCTeX document."
  (and (my/texpresso-running-p)
       (equal (process-get texpresso--process 'my/texpresso-master-file)
              (ignore-errors (my/latex-preview--master-tex-file)))))

(defun my/texpresso-start ()
  "Start TeXpresso for the current AUCTeX master and show the source position."
  (interactive)
  (my/latex-preview--ensure-tex-buffer)
  (my/texpresso--ensure-available)
  (when (fboundp 'my/refresh-environment-from-shell)
    (my/refresh-environment-from-shell))
  (if (my/texpresso-running-for-current-document-p)
      (texpresso-move-to-cursor)
    (when (buffer-modified-p)
      (save-buffer))
    (setq TeX-current-process-region-p nil)
    (let* ((master-file (my/latex-preview--master-tex-file))
           (default-directory (file-name-directory master-file)))
      (texpresso master-file)
      (unless (process-live-p texpresso--process)
        (user-error "TeXpresso failed to start; inspect *texpresso-stderr*"))
      (set-process-query-on-exit-flag texpresso--process nil)
      (process-put texpresso--process 'my/texpresso-master-file master-file)
      (texpresso-move-to-cursor)
      (message "TeXpresso live preview: %s"
               (abbreviate-file-name master-file)))))

(defun my/texpresso-stop ()
  "Stop TeXpresso and release its synchronization hooks and buffer state."
  (interactive)
  (when (featurep 'texpresso)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p texpresso-mode)
          (texpresso-mode -1))
        (when (local-variable-p 'texpresso--state)
          (setq texpresso--state nil))))
    (when (process-live-p texpresso--process)
      (delete-process texpresso--process))
    (setq texpresso--process nil))
  (message "TeXpresso live preview stopped"))

(defun my/texpresso-toggle ()
  "Stop the active TeXpresso viewer, or start it for the current document."
  (interactive)
  (if (my/texpresso-running-p)
      (my/texpresso-stop)
    (my/texpresso-start)))

(defun my/texpresso-display-output ()
  "Display TeXpresso errors and warnings in Emacs."
  (interactive)
  (my/texpresso--ensure-available)
  (texpresso-display-output))

(defun my/latex-preview-open-pdf (&optional file)
  "Open FILE, or the current TeX master PDF, in a right-side PDF window."
  (interactive)
  (let ((pdf (expand-file-name (or file (my/latex-preview--output-pdf-file))))
        (source-window (selected-window)))
    (unless (file-exists-p pdf)
      (user-error "PDF does not exist yet: %s" pdf))
    (let ((buffer (find-file-noselect pdf)))
      (display-buffer buffer '(my/pdf-view-display-in-right-window))
      (with-current-buffer buffer
        (when (derived-mode-p 'pdf-view-mode)
          (my/pdf-view-enable-capabilities)
          (my/pdf-view-enable-auto-refresh)
          (my/pdf-view-setup-interaction)))
      (when (window-live-p source-window)
        (select-window source-window))
      buffer)))

(defun my/latex-sync-forward (&optional line column)
  "Show the current source location in TeXpresso or the built PDF."
  (interactive)
  (my/latex-preview--ensure-tex-buffer)
  (if (my/texpresso-running-for-current-document-p)
      (texpresso-move-to-cursor
       (when line
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- line))
           (move-to-column (or column 0))
           (point))))
    (require 'pdf-sync)
    (let ((pdf (my/latex-preview--output-pdf-file)))
      (cond
       ((and pdf
             (file-exists-p pdf)
             (pdf-sync-locate-synctex-file pdf))
        (my/pdf-sync-forward-search-with-pdf pdf line column))
       ((and pdf (file-exists-p pdf))
        (my/latex-preview-open-pdf pdf))
       (t
        (my/texpresso-start))))))

(defun my/latex-sync-forward-mouse (event)
  "Show the LaTeX source position at mouse EVENT in the active preview."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (point (posn-point start)))
    (unless (and (windowp window) (integer-or-marker-p point))
      (user-error "No source position at click"))
    (with-selected-window window
      (goto-char point)
      (my/latex-sync-forward))))

(defun my/latex-sync-forward-mouse-or-code-actions (event)
  "Sync the TeX preview on Cmd-click, otherwise show the code-action menu."
  (interactive "e")
  (if (my/latex-preview--tex-window-event-p event)
      (my/latex-sync-forward-mouse event)
    (if (fboundp 'my/mouse-code-actions)
        (my/mouse-code-actions event)
      (user-error "No code action command is available"))))

(defun my/latex-preview-current-buffer ()
  "Open or synchronize TeXpresso live preview for the current AUCTeX master."
  (interactive)
  (my/texpresso-start))

(defun my/latex-preview-compile-and-view (&optional arg)
  "Run AUCTeX's compile-and-view command."
  (interactive "P")
  (TeX-command-run-all arg))

(defun my/latex-preview-view-pdf ()
  "View the current LaTeX document PDF via AUCTeX."
  (interactive)
  (TeX-view))

(transient-define-prefix my/latex-preview-dispatch ()
  "LaTeX live-preview and PDF workflow."
  [["Workbench"
    ("p" "TeXpresso preview" my/latex-preview-current-buffer :transient transient--do-exit)
    ("g" "sync preview" my/latex-sync-forward :transient transient--do-exit)
    ("e" "errors/warnings" my/texpresso-display-output :transient transient--do-exit)
    ("l" "toggle TeXpresso" my/texpresso-toggle :transient transient--do-exit)
    ("v" "view built PDF" my/latex-preview-open-pdf :transient transient--do-exit)]
   ["Build"
    ("a" "compile/view" my/latex-preview-compile-and-view :transient transient--do-exit)
    ("V" "AUCTeX view" my/latex-preview-view-pdf :transient transient--do-exit)]])

(defun my/auctex-setup-preview-workflow ()
  "Expose TeXpresso and PDF fallback commands in LaTeX buffers."
  (when (bound-and-true-p completion-preview-mode)
    (completion-preview-mode -1))
  (local-set-key (kbd "C-c C-p") #'my/latex-preview-current-buffer)
  (local-set-key (kbd "C-c C-g") #'my/latex-sync-forward)
  (local-set-key (kbd "M-RET") #'my/latex-sync-forward)
  (local-set-key (kbd "M-<return>") #'my/latex-sync-forward))

(defun my/auctex-install-preview-keys ()
  "Install final live-preview keys after AUCTeX and preview-latex maps load."
  (dolist (map-symbol '(LaTeX-mode-map TeX-mode-map latex-mode-map tex-mode-map))
    (when (and (boundp map-symbol)
               (keymapp (symbol-value map-symbol)))
      (define-key (symbol-value map-symbol)
                  (kbd "C-c C-p")
                  #'my/latex-preview-current-buffer)
      (define-key (symbol-value map-symbol)
                  (kbd "C-c C-g")
                  #'my/latex-sync-forward))))

(use-package tex
  :ensure auctex
  :defer t
  :hook
  (LaTeX-mode . my/typography-setup-prose-buffer)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . my/auctex-setup-build-workflow)
  (LaTeX-mode . my/auctex-setup-preview-workflow)
  (TeX-mode . my/auctex-setup-build-workflow)
  (TeX-mode . my/auctex-setup-preview-workflow)
  :config
  (setq TeX-engine 'xetex)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (my/auctex-install-preview-keys)
  (with-eval-after-load 'latex
    (my/auctex-install-preview-keys))
  (with-eval-after-load 'preview
    (my/auctex-install-preview-keys))
  (define-key TeX-source-correlate-map [M-down-mouse-1]
              #'my/latex-sync-forward-mouse)
  (define-key TeX-source-correlate-map [M-mouse-1]
              #'my/latex-sync-forward-mouse)

  (with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map [M-down-mouse-1]
                #'my/latex-sync-forward-mouse-or-code-actions)
    (define-key lsp-mode-map [M-mouse-1]
                #'my/latex-sync-forward-mouse-or-code-actions))

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (unless (advice-member-p #'my/auctex-refresh-shell-environment 'TeX-command)
    (advice-add 'TeX-command :before #'my/auctex-refresh-shell-environment))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (my/auctex-register-command
   `("XeLaTeXMk"
     ,my/auctex-xelatexmk-command
     TeX-run-TeX nil (LaTeX-mode docTeX-mode)
     :help "Run latexmk with XeLaTeX"))

  (my/auctex-register-command
   `("PdfLaTeXMk"
     ,my/auctex-pdflatexmk-command
     TeX-run-TeX nil (LaTeX-mode docTeX-mode)
     :help "Run latexmk with pdfLaTeX"))

  ;; 默认走 PDF Tools，保留 SyncTeX 正反向同步。
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")))
  (setq pdf-sync-forward-display-action '(my/pdf-view-display-in-right-window))

  (setq TeX-interactive-mode t)
  (setq LaTeX-item-indent 0))

(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-tools-enabled-modes my/pdf-tools-enabled-modes)
  (setq pdf-outline-enable-imenu t)
  (when (my/pdf-tools-activate)
    (require 'pdf-sync)
    (require 'pdf-links)
    (require 'pdf-history)
    (require 'pdf-outline)
    (require 'pdf-annot)
    (require 'pdf-occur)
    (require 'pdf-misc)
    (require 'pdf-cache)

    (define-key pdf-sync-minor-mode-map [M-down-mouse-1]
                #'pdf-sync-backward-search-mouse)
    (define-key pdf-sync-minor-mode-map [M-mouse-1]
                #'pdf-sync-backward-search-mouse)

    (advice-add 'pdf-sync-forward-correlate :around
                #'my/pdf-sync-forward-correlate-advice)

    (advice-add 'TeX-view :around #'my/TeX-view-subfile-advice)

    (add-hook 'pdf-view-mode-hook #'my/pdf-view-enable-capabilities)
    (add-hook 'pdf-view-mode-hook #'my/pdf-view-enable-auto-refresh)
    (add-hook 'pdf-view-mode-hook #'my/pdf-view-setup-interaction)
    (my/pdf-view-configure-open-buffers)))

(defun pdf-view-kill-rmn-ring-save ()
  "Copy the region to the `kill-ring' after remove all newline characters."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (replace-regexp-in-string
               "\n" " "
               (car (pdf-view-active-region-text)))))
    (pdf-view-deactivate-region)
    (kill-new txt)))

(use-package pdf-view
  :after pdf-tools
  :bind
  (:map pdf-view-mode-map
        ("C-c C-w" . pdf-view-kill-rmn-ring-save)))

(provide 'init-auctex)

;;; init-auctex.el ends here

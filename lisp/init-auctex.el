;;; init-auctex.el --- AUCTeX settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function my/typography-setup-prose-buffer "init-base")
(declare-function my/refresh-environment-from-shell nil)
(declare-function my/shell-command-executable "init-utils")
(declare-function evil-local-set-key "evil-core" (state key def))

;; --- XeLaTeX 与 pdflatex 的编译命令，注入 -synctex=1 ---

(defconst my/auctex-xelatexmk-command
  (concat (my/shell-command-executable "latexmk")
          " -xelatex -synctex=1 %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t")
  "latexmk command that explicitly builds with XeLaTeX.")

(defconst my/auctex-xelatexmk-pvc-command
  (concat (my/shell-command-executable "latexmk")
          " -xelatex -synctex=1 -pvc -view=none %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t")
  "latexmk command that explicitly watches with XeLaTeX.")

(defconst my/auctex-pdflatexmk-command
  (concat (my/shell-command-executable "latexmk")
          " -pdf -synctex=1 %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t")
  "latexmk command that explicitly builds with pdfLaTeX.")

(defconst my/auctex-pdflatexmk-pvc-command
  (concat (my/shell-command-executable "latexmk")
          " -pdf -synctex=1 -pvc -view=none %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t")
  "latexmk command that explicitly watches with pdfLaTeX.")

(defun my/auctex-refresh-shell-environment (&rest _)
  "Refresh Emacs environment before starting TeX commands."
  (when (fboundp 'my/refresh-environment-from-shell)
    (my/refresh-environment-from-shell)))

(defun my/pdf-tools-activate ()
  "Enable PDF Tools without forcing an unnecessary rebuild.

If the bundled `epdfinfo' is already executable and healthy, reuse it.
Only fall back to `pdf-tools-install' when the checker fails."
  (let ((bundled-epdfinfo
         (expand-file-name "elpa/pdf-tools-20260102.1101/epdfinfo"
                           user-emacs-directory)))
    (when (file-executable-p bundled-epdfinfo)
      (setq pdf-info-epdfinfo-program bundled-epdfinfo))
    (if (ignore-errors (pdf-info-check-epdfinfo) t)
        (pdf-tools-install)
      (pdf-tools-install :no-query))))

(defun my/auctex-register-command (entry)
  "Register TeX command ENTRY without duplicating existing items."
  (setq TeX-command-list
        (cons entry (assoc-delete-all (car entry) TeX-command-list))))

(defcustom my/auctex-live-save-idle-delay 1.0
  "Idle seconds before saving a modified LaTeX buffer in live preview mode."
  :type 'number
  :group 'TeX-command)

(defvar-local my/auctex-live-preview--save-timer nil)

(defun my/auctex-live-preview--cancel-save-timer ()
  "Cancel the pending live-preview save timer in the current buffer."
  (when (timerp my/auctex-live-preview--save-timer)
    (cancel-timer my/auctex-live-preview--save-timer)
    (setq my/auctex-live-preview--save-timer nil)))

(defun my/auctex-live-preview--save-buffer-if-live (buffer)
  "Save BUFFER when live preview mode is active and it is modified."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/auctex-live-preview--save-timer nil)
      (when (and my/auctex-live-preview-mode
                 buffer-file-name
                 (buffer-modified-p))
        (save-buffer)))))

(defun my/auctex-live-preview--schedule-save (&rest _ignore)
  "Schedule an idle save for live preview."
  (when (and my/auctex-live-preview-mode
             buffer-file-name
             (buffer-modified-p))
    (my/auctex-live-preview--cancel-save-timer)
    (setq my/auctex-live-preview--save-timer
          (run-with-idle-timer my/auctex-live-save-idle-delay nil
                               #'my/auctex-live-preview--save-buffer-if-live
                               (current-buffer)))))

(defun my/auctex-start-live-compilation ()
  "Start a `latexmk -pvc' process for the current TeX master."
  (interactive)
  (unless (derived-mode-p 'LaTeX-mode)
    (user-error "Not in LaTeX mode"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (if-let* ((process (TeX-active-process)))
      (message "TeX process already running: %s" (process-name process))
    (let ((cmd (if (eq TeX-engine 'xetex) "XeLaTeXMk-PVC" "PdfLaTeXMk-PVC")))
      (TeX-command cmd #'TeX-master-file -1))
    (when (file-exists-p (TeX-active-master (TeX-output-extension)))
      (TeX-view))))

(defun my/auctex-stop-live-compilation ()
  "Stop the active `latexmk -pvc' process for the current TeX master."
  (interactive)
  (my/auctex-live-preview--cancel-save-timer)
  (when-let* ((process (TeX-active-process)))
    (kill-process process)
    (message "Stopped TeX process: %s" (process-name process))))

(defun my/auctex-live-preview--after-save ()
  "Ensure the live preview watcher is running after saving."
  (when (and my/auctex-live-preview-mode
             buffer-file-name
             (not (TeX-active-process)))
    (my/auctex-start-live-compilation)))

(defun my/auctex-live-preview--enable ()
  "Enable buffer-local hooks and start live preview compilation."
  (add-hook 'after-change-functions #'my/auctex-live-preview--schedule-save nil t)
  (add-hook 'after-save-hook #'my/auctex-live-preview--after-save nil t)
  (my/auctex-start-live-compilation))

(defun my/auctex-live-preview--disable ()
  "Disable buffer-local hooks and stop live preview compilation."
  (remove-hook 'after-change-functions #'my/auctex-live-preview--schedule-save t)
  (remove-hook 'after-save-hook #'my/auctex-live-preview--after-save t)
  (my/auctex-stop-live-compilation))

(define-minor-mode my/auctex-live-preview-mode
  "Continuously compile the current TeX master with `latexmk -pvc'."
  :lighter " LiveTeX"
  (if my/auctex-live-preview-mode
      (my/auctex-live-preview--enable)
    (my/auctex-live-preview--disable)))

(defun my/auctex-setup-build-workflow ()
  "Prefer latexmk-based builds in LaTeX buffers."
  (setq-local TeX-command-default
              (if (eq TeX-engine 'xetex) "XeLaTeXMk" "PdfLaTeXMk"))
  (setq-local TeX-save-query nil))

(defun my/pdf-view-enable-auto-refresh ()
  "Auto-refresh PDF buffers when the underlying file changes."
  (setq-local auto-revert-interval 0.5)
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

(defcustom my/pdf-sync-parent-search-depth 3
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
  (let ((output-file (TeX-active-master (TeX-output-extension))))
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

(use-package tex
  :ensure auctex
  :defer t
  :hook
  (LaTeX-mode . my/typography-setup-prose-buffer)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . my/auctex-setup-build-workflow)
  :config
  (setq TeX-engine 'xetex)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)

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
   `("XeLaTeXMk-PVC"
     ,my/auctex-xelatexmk-pvc-command
     TeX-run-TeX nil (LaTeX-mode docTeX-mode)
     :help "Run latexmk continuously with XeLaTeX"))

  (my/auctex-register-command
   `("PdfLaTeXMk"
     ,my/auctex-pdflatexmk-command
     TeX-run-TeX nil (LaTeX-mode docTeX-mode)
     :help "Run latexmk with pdfLaTeX"))

  (my/auctex-register-command
   `("PdfLaTeXMk-PVC"
     ,my/auctex-pdflatexmk-pvc-command
     TeX-run-TeX nil (LaTeX-mode docTeX-mode)
     :help "Run latexmk continuously with pdfLaTeX"))

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
  (my/pdf-tools-activate)
  (require 'pdf-sync)
  (require 'pdf-links)
  (require 'pdf-history)
  (require 'pdf-outline)
  (require 'pdf-annot)
  (require 'pdf-occur)
  (require 'pdf-misc)
  (require 'pdf-cache)

  (advice-add 'pdf-sync-forward-correlate :around
              #'my/pdf-sync-forward-correlate-advice)

  (advice-add 'TeX-view :around #'my/TeX-view-subfile-advice)

  (add-hook 'pdf-view-mode-hook #'my/pdf-view-enable-capabilities)
  (add-hook 'pdf-view-mode-hook #'my/pdf-view-enable-auto-refresh)
  (add-hook 'pdf-view-mode-hook #'my/pdf-view-setup-interaction)
  (my/pdf-view-configure-open-buffers))

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

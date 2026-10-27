;;; init-latex-preview-menu-tests.el --- LaTeX preview entry tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lang/auctex/config)

(ert-deftest latex-preview-menu-is-searchable-and-uses-existing-actions ()
  (should (member "latex-preview-menu"
                  (all-completions "latex-preview-menu" obarray #'commandp)))
  (dolist (entry '(("g" . +latex/sync-forward)
                   ("v" . +latex/preview-open-pdf)
                   ("a" . +latex/preview-compile-and-view)
                   ("V" . +latex/preview-view-pdf)))
    (should (eq (plist-get (cdr (transient-get-suffix
                                'latex-preview-menu (car entry))) :command)
                (cdr entry)))))

(ert-deftest latex-eshell-build-is-auctex-key ()
  (require 'latex)
  (with-temp-buffer
    (LaTeX-mode)
    (should (eq (key-binding (kbd "C-c C-c")) #'TeX-command-master))
    (should (eq (key-binding (kbd "C-c C-p"))
                #'+latex/preview-compile-and-view))))

(ert-deftest latex-eshell-build-keeps-auctex-command-selection ()
  "Only the selected master latexmk commands leave AUCTeX for Eshell."
  (let (routed delegated)
    (cl-letf (((symbol-function '+auctex/eshell-build)
               (lambda (&rest args) (setq routed args))))
      (+auctex-route-latexmk-a
       (lambda (&rest args) (setq delegated args))
       "PdfLaTeXMk" #'TeX-master-file '(4))
      (should (equal routed '(nil "PdfLaTeXMk" (4))))
      (should-not delegated)
      (+auctex-route-latexmk-a
       (lambda (&rest args) (setq delegated args))
       "View" #'TeX-master-file nil)
      (should (equal delegated '("View" TeX-master-file nil))))))

(ert-deftest latex-eshell-build-auctex-master-choice-routes-to-eshell ()
  "AUCTeX still asks for a command and routes its latexmk choice."
  (require 'latex)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/latex-choice-test.tex"
          default-directory "/tmp/")
    (LaTeX-mode)
    (setq-local TeX-master t)
    (let (routed)
      (cl-letf (((symbol-function 'TeX-command-query)
                 (lambda (&rest _) "XeLaTeXMk"))
                ((symbol-function '+auctex/eshell-build)
                 (lambda (&rest args) (setq routed args))))
        (TeX-command-master)
        (should (equal routed '(nil "XeLaTeXMk" nil)))))))

(ert-deftest latex-preview-actions-reject-non-tex-buffer-before-native-calls ()
  (with-temp-buffer
    (dolist (command '(+auctex/eshell-build
                       +latex/sync-forward
                       +latex/preview-open-pdf
                       +latex/preview-compile-and-view
                       +latex/preview-view-pdf))
      (should (equal (error-message-string
                      (should-error (funcall command) :type 'user-error))
                     "Not in a file-backed LaTeX buffer")))))

(ert-deftest latex-eshell-build-uses-master-and-shows-only-failures ()
  "A subfile builds its master quietly and shows the queued shell on errors."
  (let* ((directory (file-name-as-directory (make-temp-file "latex-eshell-" t)))
         (master (expand-file-name "main.tex" directory))
         (subfile (expand-file-name "chapter.tex" directory))
         (pdf (expand-file-name "main.pdf" directory))
         (shell (get-buffer-create " *latex-eshell-test*"))
         queued viewed displayed reverted saved)
    (unwind-protect
        (progn
          (with-temp-file master (insert "\\documentclass{article}\n"))
          (with-temp-file subfile (insert "Chapter\n"))
          (with-temp-buffer
            (setq buffer-file-name subfile
                  default-directory directory
                  major-mode 'LaTeX-mode
                  TeX-master "main"
                  TeX-engine 'xetex
                  TeX-command-extra-options "")
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _) (setq saved t)))
                      ((symbol-function 'TeX-save-document)
                       (lambda (&rest _) nil))
                      ((symbol-function '+auctex-latexmk-executable)
                       (lambda () "/target/bin/latexmk"))
                      ((symbol-function '+eshell-run-build)
                       (lambda (command &rest keys)
                         (setq queued (cons command keys)) shell))
                      ((symbol-function 'TeX-revert-document-buffer)
                       (lambda (file) (setq reverted file)))
                      ((symbol-function '+latex/preview-open-pdf)
                       (lambda (file) (setq viewed file)))
                      ((symbol-function 'display-buffer)
                       (lambda (buffer &rest _) (setq displayed buffer))))
              (+auctex/eshell-build t)
              (should saved)
              (should (equal (plist-get (cdr queued) :directory) directory))
              (should-not (plist-get (cdr queued) :display))
              (should (string-match-p "-xelatex" (car queued)))
              (should (string-match-p "main\\.tex\\'" (car queued)))
              (let ((callback (plist-get (cdr queued) :callback)))
                (funcall callback (list :status 0 :buffer shell))
                (should (equal reverted pdf))
                (should (equal viewed pdf))
                (should-not displayed)
                (funcall callback (list :status 12 :buffer shell))
                (should (eq displayed shell))))))
      (kill-buffer shell)
      (delete-directory directory t))))

(ert-deftest latex-eshell-build-preserves-output-dir-and-engine ()
  "The automatic command carries AUCTeX's engine and output directory."
  (with-temp-buffer
    (setq TeX-engine 'pdftex
          TeX-output-dir "build"
          TeX-command-extra-options "-shell-escape")
    (cl-letf (((symbol-function '+auctex-latexmk-executable)
               (lambda () "latexmk"))
              ((symbol-function 'TeX--master-output-dir)
               (lambda (&rest _) "build/")))
      (let ((command (+auctex--eshell-build-command
                      "/tmp/paper/main file.tex" "/tmp/paper/")))
        (should (string-match-p "-pdf " command))
        (should (string-match-p "-outdir=build/" command))
        (should (string-match-p "-shell-escape" command))
        (should (string-match-p "main\\\\ file\\.tex\\'" command)))
      (setq TeX-engine 'xetex)
      (should (string-match-p
               "-pdf "
               (+auctex--eshell-build-command
                "/tmp/paper/main.tex" "/tmp/paper/" "PdfLaTeXMk"))))))

(ert-deftest latex-eshell-build-normalizes-path-for-texlive ()
  "A trailing PATH slash must not reach TeX Live's executable lookup."
  (let ((process-environment
         (cons "PATH=/opt/homebrew/bin/:/usr/bin/::/" process-environment)))
    (should (equal (+auctex--eshell-build-path)
                   "/opt/homebrew/bin:/usr/bin::/"))
    (with-temp-buffer
      (setq TeX-engine 'xetex
            TeX-command-extra-options "")
      (cl-letf (((symbol-function '+auctex-latexmk-executable)
                 (lambda () "/opt/homebrew/bin/latexmk")))
        (should (string-prefix-p
                 (format "env PATH=%s "
                         (shell-quote-argument
                          "/opt/homebrew/bin:/usr/bin::/"))
                 (+auctex--eshell-build-command
                  "/tmp/main.tex" "/tmp/")))))))

(ert-deftest latex-eshell-build-produces-pdf-in-named-session ()
  "The actual Eshell queue compiles a small document without opening it."
  (skip-unless (and (executable-find "latexmk")
                    (executable-find "pdflatex")))
  (let* ((directory (file-name-as-directory (make-temp-file "latex-build-" t)))
         (source (expand-file-name "main.tex" directory))
         (pdf (expand-file-name "main.pdf" directory))
         (kill-buffer-query-functions nil)
         shell source-buffer)
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "\\documentclass{article}\n\\begin{document}\n"
                    "Eshell build.\n\\end{document}\n"))
          (setq source-buffer (find-file-noselect source))
          (with-current-buffer source-buffer
            (LaTeX-mode)
            (setq-local TeX-master t
                        TeX-engine 'pdftex)
            (let ((process-environment
                   (if (and (file-executable-p "/opt/homebrew/bin/latexmk")
                            (file-executable-p "/opt/homebrew/bin/pdflatex"))
                       (cons "PATH=/opt/homebrew/bin/:/usr/bin:/bin"
                             process-environment)
                     process-environment)))
              (setq shell (+auctex/eshell-build))))
          (let ((deadline (+ (float-time) 30)))
            (while (and (+eshell-busy-p shell) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should-not (+eshell-busy-p shell))
          (should (file-exists-p pdf))
          (with-current-buffer shell
            (should (eql eshell-last-command-status 0))))
      (when (buffer-live-p shell) (kill-buffer shell))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (delete-directory directory t))))

(ert-deftest latex-preview-open-renders-pdf-window ()
  "Opening a PDF initializes the right-side PDF window."
  (let ((pdf-buffer (generate-new-buffer " *latex-pdf-render-test*"))
        rendered)
    (unwind-protect
        (progn
          (with-current-buffer pdf-buffer
            (setq major-mode 'pdf-view-mode))
          (cl-letf (((symbol-function 'aaron-open-call-with-file-context)
                     (lambda (_file fn &rest _)
                       (if (eq fn #'file-exists-p) t nil)))
                    ((symbol-function 'aaron-open-visit-file-noselect)
                     (lambda (&rest _) pdf-buffer))
                    ((symbol-function 'display-buffer)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function '+pdf-view-enable-capabilities)
                     (lambda () nil))
                    ((symbol-function '+pdf-view-enable-auto-refresh)
                     (lambda () nil))
                    ((symbol-function '+pdf-view-setup-interaction)
                     (lambda () nil))
                    ((symbol-function '+pdf-view-render-window)
                     (lambda (window) (setq rendered window))))
            (+latex/preview-open-pdf "/tmp/demo.pdf")
            (should (eq rendered (selected-window)))))
      (kill-buffer pdf-buffer))))

(ert-deftest latex-preview-reinitializes-a-raw-pdf-window ()
  "A PDFView buffer without a rendered overlay is reinitialized in its window."
  (let ((pdf-buffer (generate-new-buffer " *latex-pdf-raw-test*"))
        initialized rendered)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) pdf-buffer)
          (with-current-buffer pdf-buffer
            (setq major-mode 'pdf-view-mode)
            (setq-local image-mode-winprops-alist nil))
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'pdf-view-mode)
                     (lambda () (setq initialized (selected-window))))
                    ((symbol-function 'pdf-view-goto-page)
                     (lambda (page window)
                       (setq rendered (cons page window))))
                    ((symbol-function '+pdf-view-start-prefetch-h)
                     (lambda (&rest _) nil)))
            (+pdf-view-render-window (selected-window))
            (should (eq initialized (selected-window)))
            (should (equal rendered (cons 1 (selected-window))))))
      (kill-buffer pdf-buffer))))

(ert-deftest latex-preview-defers-prefetch-until-a-page-exists ()
  "No prefetch timer starts before PDF Tools has a numbered page."
  (with-temp-buffer
    (setq-local image-mode-winprops-alist nil)
    (let (started)
      (cl-letf (((symbol-function 'pdf-cache-prefetch-minor-mode)
                 (lambda (&rest _) (setq started t))))
        (+pdf-view-start-prefetch-h)
        (should-not started)
        (image-mode-window-put 'page 1)
        (+pdf-view-start-prefetch-h)
        (should started)))))

(ert-deftest latex-preview-reload-renders-an-open-pdf ()
  "Reloading the AUCTeX module repairs a visible PDF buffer."
  (let ((pdf-buffer (generate-new-buffer " *latex-pdf-reload-test*"))
        rendered)
    (unwind-protect
        (progn
          (with-current-buffer pdf-buffer
            (setq major-mode 'pdf-view-mode))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&rest _) (list pdf-buffer)))
                    ((symbol-function 'get-buffer-window-list)
                     (lambda (&rest _) (list (selected-window))))
                    ((symbol-function '+pdf-view-enable-capabilities)
                     (lambda () nil))
                    ((symbol-function '+pdf-view-enable-auto-refresh)
                     (lambda () nil))
                    ((symbol-function '+pdf-view-setup-interaction)
                     (lambda () nil))
                    ((symbol-function '+pdf-view-render-window)
                     (lambda (window) (setq rendered window))))
            (+pdf-view-configure-open-buffers)
            (should (eq rendered (selected-window)))))
      (kill-buffer pdf-buffer))))

(ert-deftest latex-preview-explicit-local-pdf-stays-local-from-remote-buffer ()
  (let ((preview (generate-new-buffer " *local-pdf-preview*"))
        checked opened visit-directory)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/fs:box:/work/")
          (let ((source (current-buffer)))
            (cl-letf (((symbol-function 'file-exists-p)
                       (lambda (file)
                         (setq checked file)
                         (should (equal default-directory
                                        temporary-file-directory))
                         t))
                      ((symbol-function 'find-file-noselect)
                       (lambda (file &rest _)
                         (setq opened file visit-directory default-directory)
                         preview))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil)))
              (+latex/preview-open-pdf "/Users/test/project/result.pdf"))
            (should (equal checked "/Users/test/project/result.pdf"))
            (should (equal opened checked))
            (should (equal visit-directory temporary-file-directory))
            (should (equal (buffer-local-value 'default-directory source)
                           "/fs:box:/work/"))))
      (kill-buffer preview))))

(ert-deftest +auctex-fill-keeps-inline-math-on-one-line ()
  "Filling a LaTeX paragraph does not break a line inside inline math."
  (require 'latex)
  (with-temp-buffer
    (LaTeX-mode)
    (setq fill-column 30)
    ;; The formula is longer than the line, so plain filling breaks it.
    (insert "Words $a + b + c + d + e + f + g + h + i + j = k$ end of it here.")
    (goto-char (point-min))
    (fill-paragraph)
    (should (string-match-p (regexp-quote "$a + b + c + d + e + f + g + h + i + j = k$")
                            (buffer-string)))))

(ert-deftest +pdf-large-files-open-without-the-size-question ()
  "Large PDFs skip the size question; other large files still ask."
  (require 'lang/auctex/config)
  (let* ((dir (file-name-as-directory (make-temp-file "large-" t)))
         (pdf (expand-file-name "book.pdf" dir))
         (csv (expand-file-name "data.csv" dir))
         (large-file-warning-threshold 10)
         asked)
    (unwind-protect
        (progn
          ;; The question is only asked for a file that can be read.
          (write-region "x" nil pdf nil 'silent)
          (write-region "x" nil csv nil 'silent)
          (cl-letf (((symbol-function 'files--ask-user-about-large-file)
                     (lambda (&rest _) (setq asked t) nil)))
            (abort-if-file-too-large 1000 "open" pdf)
            (should-not asked)
            (abort-if-file-too-large 1000 "open" csv)
            (should asked)))
      (delete-directory dir t))))

(provide 'init-latex-preview-menu-tests)
;;; init-latex-preview-menu-tests.el ends here

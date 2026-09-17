;;; init-writing-tests.el --- prose and LaTeX integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-languagetool)
(require 'init-latex)

(defun my/test-languagetool-response ()
  "Return a small LanguageTool response used by diagnostics tests."
  '((matches . [((message . "Possible spelling mistake")
                 (shortMessage . "Spelling")
                 (offset . 0)
                 (length . 4)
                 (replacements . [((value . "This"))])
                 (rule . ((id . "MORFOLOGIK_RULE_EN_US")
                          (issueType . "misspelling")
                          (category . ((id . "TYPOS"))))))])))

(ert-deftest my/languagetool-maps-utf16-offsets ()
  (should (= (my/languagetool--utf16-position "a😀bc" 3 10) 12))
  (should (= (my/languagetool--utf16-position "a😀bc" 4 10) 13)))

(ert-deftest my/languagetool-builds-actionable-flymake-diagnostics ()
  (with-temp-buffer
    (insert "Ths sentence.")
    (let* ((diagnostics
            (my/languagetool--flymake-diagnostics
             (my/test-languagetool-response) (current-buffer)
             (point-min) (buffer-string)))
           (diagnostic (car diagnostics))
           (data (flymake-diagnostic-data diagnostic)))
      (should (= (length diagnostics) 1))
      (should (eq (plist-get data :source) 'languagetool))
      (should (equal (plist-get data :suggestions) '("This")))
      (should (equal (plist-get data :original) "Ths "))
      (my/languagetool--replace diagnostic "This ")
      (should (equal (buffer-string) "This sentence.")))))

(ert-deftest my/languagetool-skips-markup-faced-matches ()
  (with-temp-buffer
    (insert (propertize "Ths " 'face 'markdown-code-face) "sentence.")
    (should-not
     (my/languagetool--flymake-diagnostics
      (my/test-languagetool-response) (current-buffer)
      (point-min) (buffer-string)))))

(ert-deftest my/languagetool-diagnostics-use-only-a-quiet-context-menu ()
  (with-temp-buffer
    (insert "Ths sentence.")
    (let* ((diagnostic
            (car (my/languagetool--flymake-diagnostics
                  (my/test-languagetool-response) (current-buffer)
                  (point-min) (buffer-string))))
           (properties (flymake--diag-overlay-properties diagnostic)))
      (should-not (lookup-key my/languagetool-diagnostic-map [mouse-1]))
      (should (eq (lookup-key my/languagetool-diagnostic-map [mouse-3])
                  #'my/languagetool-menu-mouse))
      (should-not (assq 'mouse-face properties))
      (should (equal (alist-get 'help-echo properties)
                     "Possible spelling mistake")))))

(ert-deftest my/languagetool-report-suppresses-only-its-eol-summary ()
  (let ((flymake-show-diagnostics-at-end-of-line 'short)
        observed)
    (my/languagetool--report
     (lambda (&rest args)
       (setq observed
             (list flymake-show-diagnostics-at-end-of-line args)))
     '(diagnostic) :region '(1 . 2))
    (should-not (car observed))
    (should (equal (cadr observed)
                   '((diagnostic) :region (1 . 2))))
    (should (eq flymake-show-diagnostics-at-end-of-line 'short))))

(ert-deftest my/latex-server-selection-prefers-texlab ()
  (cl-letf (((symbol-function 'my/language-server-executable-find)
             (lambda (program)
               (pcase program
                 ("texlab" "/target/bin/texlab")
                 ("digestif" "/target/bin/digestif")))))
    (should (equal (my/latex-language-server-selection)
                   '(texlab . "/target/bin/texlab")))
    (should (equal (my/latex-language-server-command)
                   '("/target/bin/texlab")))))

(ert-deftest my/latex-server-selection-falls-back-to-digestif ()
  (cl-letf (((symbol-function 'my/language-server-executable-find)
             (lambda (program)
               (and (string= program "digestif") "/target/bin/digestif"))))
    (should (equal (my/latex-language-server-selection)
                   '(digestif . "/target/bin/digestif")))
    (should-not
     (my/latex-language-server-workspace-configuration
      '(digestif . "/target/bin/digestif")))))

(ert-deftest my/latex-texlab-settings-use-supported-placeholders ()
  (cl-letf (((symbol-function 'my/language-server-executable-find)
             (lambda (program)
               (pcase program
                 ("latexmk" "/target/bin/latexmk")
                 ("chktex" "/target/bin/chktex")))))
    (let* ((configuration
            (my/latex-language-server-workspace-configuration
             '(texlab . "/target/bin/texlab")))
           (texlab (plist-get configuration :texlab))
           (build (plist-get texlab :build)))
      (should (equal (plist-get build :executable) "/target/bin/latexmk"))
      (should (equal (append (plist-get build :args) nil)
                     '("-xelatex" "-interaction=nonstopmode" "-synctex=1"
                       "-file-line-error" "%f")))
      (should-not
       (seq-some (lambda (arg) (string-match-p "%OUTDIR%" arg))
                 (append (plist-get build :args) nil)))
      (should (eq (plist-get build :onSave) :json-false)))))

(provide 'init-writing-tests)
;;; init-writing-tests.el ends here

;;; init-writing-tests.el --- prose and LaTeX integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-languagetool)
(require 'init-latex)

(ert-deftest my/noema-jutext-registers-ratex-preview ()
  (should (memq #'ratex-turn-on noema-research-mode-hook)))

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

;;; Shared LaTeX assets -------------------------------------------------------
;;
;; The Emacs preview and Noema's KaTeX renderer must agree on the macro set.
;; Noema already keeps its JS and Go parsers aligned through
;; `site-lisp/noema/shared/katex-macro-fixtures.json'; these tests hold the
;; Elisp parser to the same contract instead of letting it drift into a third
;; implementation that merely looks similar.

(defconst my/test-katex-macro-fixtures-file
  (expand-file-name "site-lisp/noema/shared/katex-macro-fixtures.json"
                    user-emacs-directory))

(defun my/test-katex-macro-fixtures ()
  "Return the shared macro parser fixtures, or nil when Noema is absent."
  (when (file-readable-p my/test-katex-macro-fixtures-file)
    (with-temp-buffer
      (insert-file-contents my/test-katex-macro-fixtures-file)
      (json-parse-buffer :object-type 'alist :array-type 'list
                         :false-object nil :null-object nil))))

(ert-deftest my/latex-macro-parser-matches-shared-fixtures ()
  "The Elisp macro parser must agree with the JS and Go implementations."
  (let ((fixtures (my/test-katex-macro-fixtures)))
    (skip-unless fixtures)
    (dolist (case fixtures)
      (let* ((text (mapconcat (lambda (file) (alist-get 'text file))
                              (alist-get 'files case)
                              "\n"))
             (expected (alist-get 'macros case))
             (parsed (my/latex-parse-macro-definitions text)))
        (should (= (length parsed) (length expected)))
        (dolist (pair expected)
          (let ((entry (assoc (format "%s" (car pair)) parsed)))
            (should entry)
            (should (equal (nth 2 entry) (cdr pair)))))))))

(ert-deftest my/latex-macro-parser-handles-every-supported-form ()
  (let ((parsed (my/latex-parse-macro-definitions
                 (concat "% comment with \\newcommand{\\ignored}{x}\n"
                         "\\newcommand{\\R}{\\mathbb{R}}\n"
                         "\\newcommand{\\abs}[1]{\\left|#1\\right|}\n"
                         "\\newcommand\\bare{plain}\n"
                         "\\DeclareMathOperator*{\\argmax}{arg\\,max}\n"
                         "\\def\\pair#1#2{(#1,#2)}\n"
                         "\\renewcommand{\\R}{\\mathbb{Q}}\n"))))
    (should (equal (nth 2 (assoc "\\R" parsed)) "\\mathbb{Q}"))
    (should (equal (nth 1 (assoc "\\abs" parsed)) 1))
    (should (equal (nth 2 (assoc "\\bare" parsed)) "plain"))
    (should (equal (nth 2 (assoc "\\argmax" parsed)) "\\operatorname*{arg\\,max}"))
    (should (equal (nth 1 (assoc "\\pair" parsed)) 2))
    (should-not (assoc "\\ignored" parsed))))

(ert-deftest my/latex-macro-preamble-emits-def-forms ()
  "Definitions must be emitted as `\\def': several shared names collide with
engine builtins, and `\\newcommand' errors on redefinition, which would fail
the whole preamble and with it every preview."
  (should (equal (my/latex-compile-macro-preamble
                  '(("\\R" 0 "\\mathbb{R}") ("\\abs" 1 "\\left|#1\\right|")))
                 "\\def\\R{\\mathbb{R}}\\def\\abs#1{\\left|#1\\right|}")))

(ert-deftest my/latex-shared-macro-directory-resolves ()
  "The shared macro directory must not be a dangling link."
  (let ((directory (or (and (boundp 'my/noema--katex-macros-dir)
                            my/noema--katex-macros-dir)
                       my/latex-katex-macros-directory)))
    (should (file-directory-p directory))
    (should (my/latex--macro-files directory))))

(ert-deftest my/latex-tex-compat-rewrite-uses-shared-rules ()
  (skip-unless (file-readable-p my/latex-tex-compat-rules-file))
  (should (equal (my/latex-tex-compat-rewrite
                  "\\begin{multline} a \\\\ b \\end{multline}")
                 "\\begin{gathered} a \\\\ b \\end{gathered}"))
  (should (equal (my/latex-tex-compat-rewrite "\\begin{align} a \\end{align}")
                 "\\begin{align} a \\end{align}")))

(ert-deftest my/latex-noema-asset-links-resolve ()
  "Every Noema asset link must resolve; a dangling one silently disables
global macros and export templates on the Emacs side."
  (dolist (relative '("etc/katex-macros"
                      "etc/prose-accepted-words.txt"
                      "templates/latex"
                      "templates/tex"
                      "templates/noema"))
    (let ((path (expand-file-name relative user-emacs-directory)))
      (should (file-exists-p path)))))

;;; init-evil-vim-parity-tests.el --- Evil commands that follow native Vim -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-evil-vim-parity-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'evil)

(defun init-evil-vim-parity-tests--join (mode text &optional count)
  "Return TEXT after joining COUNT lines from its start in MODE."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (goto-char (point-min))
    (evil-join (point) (line-end-position (or count 2)))
    (buffer-string)))

(ert-deftest +evil-join-drops-line-comment-leaders ()
  "J on two comment lines keeps one leader, as Vim's formatoptions j does."
  (should (equal (init-evil-vim-parity-tests--join
                  #'emacs-lisp-mode ";; first\n  ;; second\n")
                 ";; first second\n"))
  (should (equal (init-evil-vim-parity-tests--join
                  #'c-mode "int a; // first\n// second\n")
                 "int a; // first second\n"))
  (should (equal (init-evil-vim-parity-tests--join
                  #'python-mode "# one\n# two\n# three\n" 3)
                 "# one two three\n")))

(ert-deftest +evil-join-drops-block-comment-continuation ()
  "J inside a C block comment drops the \" * \" of the joined line."
  (should (equal (init-evil-vim-parity-tests--join
                  #'c-mode "/* first\n * second */\n")
                 "/* first second */\n")))

(ert-deftest +evil-join-keeps-code-and-comments-apart ()
  "Only a line that ends in a comment loses the next line's leader."
  (should (equal (init-evil-vim-parity-tests--join
                  #'c-mode "int a;\n// note\n")
                 "int a; // note\n"))
  (should (equal (init-evil-vim-parity-tests--join
                  #'c-mode "// note\nint a;\n")
                 "// note int a;\n"))
  (should (equal (init-evil-vim-parity-tests--join
                  #'emacs-lisp-mode "(foo\n bar)\n")
                 "(foo bar)\n")))

(ert-deftest +evil-fill-keeps-inner-spacing ()
  "gq keeps the spacing inside a line, as Vim does."
  (with-temp-buffer
    (text-mode)
    (setq fill-column 70)
    (insert "a  b.  c\nd\n")
    (evil-fill (point-min) (point-max))
    (should (equal (buffer-string) "a  b.  c d\n"))))

;;; init-evil-vim-parity-tests.el ends here

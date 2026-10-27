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

(ert-deftest +evil-insert-state-typing-replaces-the-selection ()
  "Typing over an active region in Insert state replaces it."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (evil-local-mode 1)
    (insert "hello world")
    (evil-insert-state)
    (unwind-protect
        (progn
          (set-mark 1)
          (goto-char 6)
          (activate-mark)
          (should delete-selection-mode)
          (setq last-command-event ?X
                this-command 'self-insert-command)
          (run-hooks 'pre-command-hook)
          (self-insert-command 1 ?X)
          (should (equal (buffer-string) "X world")))
      (evil-normal-state)
      (should-not delete-selection-mode))))

;;; Neovim's default mappings

(defmacro init-evil-vim-parity-tests--with-keys (mode text &rest body)
  "Run BODY in a selected MODE buffer holding TEXT, point at its start."
  (declare (indent 2))
  `(let ((buffer (generate-new-buffer "*evil-parity*")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer buffer)
           (funcall ,mode)
           (insert ,text)
           (goto-char (point-min))
           (evil-local-mode 1)
           (evil-normal-state)
           ,@body)
       (kill-buffer buffer))))

(defun init-evil-vim-parity-tests--type (keys)
  "Type KEYS, a `kbd' string, in the current buffer."
  (execute-kbd-macro (kbd keys)))

(ert-deftest +evil-bracket-space-adds-blank-lines-and-keeps-point ()
  "] SPC and [ SPC add COUNT blank lines without moving off the text."
  (init-evil-vim-parity-tests--with-keys #'text-mode "one\ntwo\n"
    (forward-line 1)
    (forward-char 1)
    (init-evil-vim-parity-tests--type "2] SPC")
    (should (equal (buffer-string) "one\ntwo\n\n\n"))
    (should (looking-at "wo"))
    (init-evil-vim-parity-tests--type "[ SPC")
    (should (equal (buffer-string) "one\n\ntwo\n\n\n"))
    (should (looking-at "wo"))))

(ert-deftest +evil-gc-toggles-comments-linewise ()
  "gcc and gc{motion} toggle comments line by line, as Neovim's gc.
Lines that are all comments lose one leader; otherwise every line gains one,
already commented lines included."
  (init-evil-vim-parity-tests--with-keys #'emacs-lisp-mode "(a)\n(b)\n(c)\n"
    (init-evil-vim-parity-tests--type "gcc")
    (should (equal (buffer-string) ";; (a)\n(b)\n(c)\n"))
    (init-evil-vim-parity-tests--type "gcc")
    (should (equal (buffer-string) "(a)\n(b)\n(c)\n"))
    (init-evil-vim-parity-tests--type "gcj")
    (should (equal (buffer-string) ";; (a)\n;; (b)\n(c)\n"))
    (init-evil-vim-parity-tests--type "gcG")
    (should (equal (buffer-string) ";; ;; (a)\n;; ;; (b)\n;; (c)\n"))))

(ert-deftest +evil-visual-star-searches-the-selection-literally ()
  "Visual * and # search for the selected text, not the word under point."
  (init-evil-vim-parity-tests--with-keys #'text-mode "a.b x a.b y a.b z axb"
    (init-evil-vim-parity-tests--type "vll*")
    (should (evil-normal-state-p))
    (should (= (point) 7))
    (init-evil-vim-parity-tests--type "n")
    (should (= (point) 13))
    (init-evil-vim-parity-tests--type "vll#")
    (should (= (point) 7))))

(ert-deftest +evil-linewise-visual-at-runs-the-register-per-line ()
  "In linewise Visual, @q runs register q on every selected line."
  (init-evil-vim-parity-tests--with-keys #'text-mode "a\nb\nc\n"
    (evil-set-register ?q (kbd "A; <escape>"))
    (init-evil-vim-parity-tests--type "Vj@q")
    (should (equal (buffer-string) "a;\nb;\nc\n"))))

(ert-deftest +evil-ig-selects-the-whole-buffer ()
  "yig yanks the whole buffer from anywhere in it."
  (init-evil-vim-parity-tests--with-keys #'text-mode "one\ntwo\nthree"
    (forward-line 1)
    (kill-new "sentinel")
    (init-evil-vim-parity-tests--type "yig")
    (should (equal (current-kill 0) "one\ntwo\nthree"))))

(ert-deftest +evil-ex-file-names-take-vim-modifiers ()
  "%:h, %:t, %:r and %:e work as in Vim; an escaped \\% stays literal."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/evil-parity/dir/name.el")
    (should (equal (evil-ex-replace-special-filenames "%:h/other.el")
                   "/tmp/evil-parity/dir/other.el"))
    (should (equal (evil-ex-replace-special-filenames "%:t:r") "name"))
    (should (equal (evil-ex-replace-special-filenames "%:e") "el"))
    (should (equal (evil-ex-replace-special-filenames "%:p:h:h")
                   "/tmp/evil-parity"))
    (should (equal (evil-ex-replace-special-filenames "\\%:h") "%:h"))
    (should (equal (evil-ex-replace-special-filenames "%")
                   "/tmp/evil-parity/dir/name.el"))
    (setq buffer-file-name nil)))

(ert-deftest +evil-neovim-bracket-keys-are-bound ()
  "]b [b ]q [q ]d [d are the buffer, error and diagnostic walks."
  (init-evil-vim-parity-tests--with-keys #'text-mode ""
    (should (eq (key-binding (kbd "]b")) #'+evil/next-buffer))
    (should (eq (key-binding (kbd "[b")) #'+evil/previous-buffer))
    (should (eq (key-binding (kbd "]q")) #'+evil/next-error))
    (should (eq (key-binding (kbd "[q")) #'+evil/previous-error))
    (should (eq (key-binding (kbd "]d")) #'+evil/next-diagnostic))
    (should (eq (key-binding (kbd "[d")) #'+evil/previous-diagnostic))))

(ert-deftest +evil-wgrep-delete-marks-read-only-lines ()
  "d on a wgrep line with a read-only prefix marks it for deletion."
  (should (eq (lookup-key (progn (require 'wgrep) wgrep-mode-map)
                          [remap evil-delete])
              #'+evil:wgrep-delete)))

(ert-deftest +evil-picking-a-place-is-a-jump ()
  "After imenu or a consult picker moves point, C-o comes back."
  (require 'consult)
  (dolist (command '(imenu consult-imenu))
    (let ((file (make-temp-file "jump" nil ".el")))
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (insert "(defun alpha () 1)\n" (make-string 200 ?\n)
                    "(defun beta () 2)\n")
            (switch-to-buffer (current-buffer))
            (evil-local-mode 1)
            (goto-char (point-min))
            (forward-line 5)
            (let ((start (point)))
              ;; The command loop's hooks around an interactive command.
              (cl-letf (((symbol-function 'consult--read)
                         (lambda (&rest _)
                           (cons nil (save-excursion
                                       (goto-char (point-max))
                                       (forward-line -1)
                                       (point-marker))))))
                (setq this-command command)
                (run-hooks 'pre-command-hook)
                (if (eq command 'imenu) (imenu "beta") (consult-imenu))
                (run-hooks 'post-command-hook))
              (should (> (point) start))
              (setq this-command 'evil-jump-backward)
              (run-hooks 'pre-command-hook)
              (evil-jump-backward)
              (run-hooks 'post-command-hook)
              (should (= (point) start)))
            (set-buffer-modified-p nil)
            (kill-buffer))
        (delete-file file)))))

;;; init-evil-vim-parity-tests.el ends here

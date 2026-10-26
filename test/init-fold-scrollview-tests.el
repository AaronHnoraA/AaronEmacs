;;; init-fold-scrollview-tests.el --- Folding and overview tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-fold-scrollview-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'hideshow)
(require 'scrollview)

(ert-deftest +scrollview-layout-and-groups ()
  (should (equal fringe-mode '(nil)))
  (should-not scroll-bar-mode)
  (should (eq scrollview-area 'fringe))
  (should (eq scrollview-side 'right))
  (should (eq scrollview-visibility 'info))
  (should (equal scrollview-signs-on-startup
                 '(search diagnostics vc bookmarks symbol-overlay))))

(ert-deftest +scrollview-auto-enable-scope ()
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "scrollview-test"))
    (emacs-lisp-mode)
    (+scrollview-turn-on)
    (should scrollview-mode))
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "scrollview-special-test"))
    (special-mode)
    (+scrollview-turn-on)
    (should-not scrollview-mode)))

(ert-deftest +hideshow-keeps-c-closing-line-visible ()
  (with-temp-buffer
    (c-mode)
    (insert "if (condition) {\n  work();\n} else {\n  fallback();\n}\n")
    (goto-char (point-min))
    (search-forward "{")
    (hs-minor-mode 1)
    (hs-hide-block)
    (let* ((overlay
            (seq-find (lambda (candidate)
                        (eq (overlay-get candidate 'hs) 'code))
                      (overlays-in (point-min) (point-max))))
           (closing-line
            (save-excursion
              (goto-char (point-min))
              (search-forward "} else")
              (match-beginning 0)))
           (display (and overlay (overlay-get overlay 'display))))
      (should overlay)
      (should (<= (overlay-end overlay) closing-line))
      (should (= (+hs-hidden-line-count overlay) 1))
      (should (string-match-p "1 line" display))
      (should (eq (lookup-key (get-text-property 0 'keymap display)
                              [mouse-1])
                  #'+fold/hs-mouse-toggle)))))

(ert-deftest +fold-hideshow-backend-is-exclusive ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (+fold--ensure-backend)
    (should hs-minor-mode)
    (should-not (bound-and-true-p treesit-fold-mode))))

(ert-deftest +fold-treesit-backend-is-exclusive ()
  (skip-unless (and (fboundp 'python-ts-mode)
                    (fboundp 'treesit-ready-p)
                    (treesit-ready-p 'python t)))
  (with-temp-buffer
    (python-ts-mode)
    (insert "def example():\n    return 1\n")
    (goto-char (point-min))
    (+fold--ensure-backend)
    (should (bound-and-true-p treesit-fold-mode))
    (should-not (bound-and-true-p hs-minor-mode))))

(provide 'init-fold-scrollview-tests)

(ert-deftest +fold-reveal-point-opens-the-hiding-fold ()
  "A jump into a hidden block opens the block, and only that block."
  (with-temp-buffer
    (emacs-lisp-mode)
    (hs-minor-mode 1)
    (insert "(defun a ()\n  (target))\n\n(defun b ()\n  (other))\n")
    (goto-char (point-min))
    (hs-hide-all)
    (search-forward "(target")
    (should (invisible-p (point)))
    (should (memq #'+fold-reveal-point-h xref-after-jump-hook))
    (+fold-reveal-point-h)
    (should-not (invisible-p (point)))
    (search-forward "(other")
    (should (invisible-p (point)))))

;;; init-fold-scrollview-tests.el ends here

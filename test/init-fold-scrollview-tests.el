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

(ert-deftest my/scrollview-layout-and-groups ()
  (should (equal fringe-mode '(nil)))
  (should-not scroll-bar-mode)
  (should (eq scrollview-area 'fringe))
  (should (eq scrollview-side 'right))
  (should (eq scrollview-visibility 'info))
  (should (equal scrollview-signs-on-startup
                 '(search diagnostics vc bookmarks symbol-overlay))))

(ert-deftest my/scrollview-auto-enable-scope ()
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "scrollview-test"))
    (emacs-lisp-mode)
    (my/scrollview-turn-on)
    (should scrollview-mode))
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "scrollview-special-test"))
    (special-mode)
    (my/scrollview-turn-on)
    (should-not scrollview-mode)))

(ert-deftest my/hideshow-keeps-c-closing-line-visible ()
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
      (should (= (my/hs-hidden-line-count overlay) 1))
      (should (string-match-p "1 line" display))
      (should (eq (lookup-key (get-text-property 0 'keymap display)
                              [mouse-1])
                  #'my/fold-hs-mouse-toggle)))))

(ert-deftest my/fold-hideshow-backend-is-exclusive ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (my/fold--ensure-backend)
    (should hs-minor-mode)
    (should-not (bound-and-true-p treesit-fold-mode))))

(ert-deftest my/fold-treesit-backend-is-exclusive ()
  (skip-unless (and (fboundp 'python-ts-mode)
                    (fboundp 'treesit-ready-p)
                    (treesit-ready-p 'python t)))
  (with-temp-buffer
    (python-ts-mode)
    (insert "def example():\n    return 1\n")
    (goto-char (point-min))
    (my/fold--ensure-backend)
    (should (bound-and-true-p treesit-fold-mode))
    (should-not (bound-and-true-p hs-minor-mode))))

(provide 'init-fold-scrollview-tests)

;;; init-fold-scrollview-tests.el ends here

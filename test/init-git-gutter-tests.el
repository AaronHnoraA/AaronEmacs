;;; init-git-gutter-tests.el --- Tests for the diff-hl gutter bitmaps -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `+git--diff-hl-tall-bitmaps-a' makes the hunk bitmaps taller than any line,
;; so consecutive changed lines of a taller font (CJK, Org headings) join up.
;; Run with the full configuration loaded (scripts/test-all).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'diff-hl)

(ert-deftest git-gutter-hunk-bitmaps-outgrow-any-line ()
  (let (defined)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'frame-char-height) (lambda (&rest _) 22))
              ((symbol-function 'frame-parameter) (lambda (&rest _) 8))
              ((symbol-function 'define-fringe-bitmap)
               (lambda (name bits &optional height width align)
                 (push (list name (length bits) height width align bits) defined))))
      (+git--diff-hl-tall-bitmaps-a))
    (should (equal (sort (mapcar #'car defined) #'string<)
                   '(diff-hl-bmp-bottom diff-hl-bmp-middle diff-hl-bmp-top)))
    (pcase-dolist (`(,name ,len ,height ,_width ,align ,bits) defined)
      ;; Six rows of 22px: taller than a CJK line or an Org heading.
      (should (= len height 132))
      (pcase name
        ('diff-hl-bmp-middle (should (eq align 'center)))
        ('diff-hl-bmp-top
         (should (eq align 'top))
         (should (= (aref bits 0) 255)))
        ('diff-hl-bmp-bottom
         (should (eq align 'bottom))
         (should (= (aref bits (1- len)) 255)))))))

(ert-deftest git-gutter-tall-bitmaps-follow-diff-hl-redefinitions ()
  (should (advice-member-p #'+git--diff-hl-tall-bitmaps-a 'diff-hl-define-bitmaps)))

(provide 'init-git-gutter-tests)
;;; init-git-gutter-tests.el ends here

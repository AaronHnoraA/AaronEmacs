;;; ratex-tests.el --- Tests for ratex.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'ratex)
(require 'ratex-core)
(require 'ratex-render)
(require 'ratex-math-detect)

(ert-deftest ratex-detects-typst-inline-math ()
  (with-temp-buffer
    (insert "hello $x^2 + 1$ world")
    (goto-char 10)
    (let ((fragment (ratex-fragment-at-point)))
      (should fragment)
      (should (equal (plist-get fragment :open) "$"))
      (should (equal (plist-get fragment :content) "x^2 + 1")))))

(ert-deftest ratex-detects-simple-typst-variable ()
  (with-temp-buffer
    (insert "hello $x$ world")
    (goto-char 9)
    (let ((fragment (ratex-fragment-at-point)))
      (should fragment)
      (should (equal (plist-get fragment :content) "x")))))

(ert-deftest ratex-detects-typst-display-style-math ()
  (with-temp-buffer
    (insert "before\n$\nsum_(i=1)^n x_i\n$\nafter")
    (goto-char (point-min))
    (search-forward "sum_")
    (let ((fragment (ratex-fragment-at-point)))
      (should fragment)
      (should (equal (plist-get fragment :open) "$"))
      (should (equal (string-trim (plist-get fragment :content))
                     "sum_(i=1)^n x_i")))))

(ert-deftest ratex-detects-org-single-dollar-fragment-via-org-element ()
  (with-temp-buffer
    (org-mode)
    (insert "a $x+1$ b")
    (goto-char 6)
    (let ((fragment (ratex-fragment-at-point)))
      (should fragment)
      (should (equal (plist-get fragment :open) "$"))
      (should (equal (plist-get fragment :content) "x+1")))))

(ert-deftest ratex-only-detects-single-dollar-math ()
  (with-temp-buffer
    (insert "old $$x^2$$ and $y$")
    (let ((fragments (ratex-fragments-in-buffer)))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :open) "$"))
      (should (equal (plist-get (car fragments) :content) "y")))))

(ert-deftest ratex-org-mode-does-not-detect-double-dollar-math ()
  (with-temp-buffer
    (org-mode)
    (insert "old $$x^2$$ and $y$")
    (let ((fragments (ratex-fragments-in-buffer)))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :content) "y")))))

(ert-deftest ratex-does-not-treat-currency-as-math ()
  (with-temp-buffer
    (insert "price is $5 and another $7 here")
    (should-not (ratex-fragments-in-buffer))))

(ert-deftest ratex-ignores-escaped-dollar-delimiters ()
  (with-temp-buffer
    (insert "literal \\$x^2\\$ and math $y^2$")
    (let ((fragments (ratex-fragments-in-buffer)))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :content) "y^2")))))

(ert-deftest ratex-skips-formulas-in-code-context ()
  (with-temp-buffer
    (insert "$x$ $y$")
    (cl-letf (((symbol-function 'ratex--code-context-at-p)
               (lambda (pos)
                 (<= pos 3))))
      (let ((fragments (ratex-fragments-in-buffer)))
        (should (= (length fragments) 1))
        (should (equal (plist-get (car fragments) :content) "y"))))))

(ert-deftest ratex-fragment-at-point-skips-code-context ()
  (with-temp-buffer
    (insert "$x$")
    (goto-char 2)
    (cl-letf (((symbol-function 'ratex--code-context-at-p)
               (lambda (_pos) t)))
      (should-not (ratex-fragment-at-point)))))

(ert-deftest ratex-fragment-at-point-detects-delimiter-position ()
  (with-temp-buffer
    (insert "$\na+b\n$")
    (goto-char (point-min))
    (let ((fragment (ratex-fragment-at-point)))
      (should fragment)
      (should (equal (plist-get fragment :open) "$")))
    (goto-char (point-max))
    (backward-char 1)
    (let ((fragment (ratex-fragment-at-point)))
      (should fragment)
      (should (equal (plist-get fragment :close) "$")))))

(ert-deftest ratex-fragments-in-buffer-detects-multiple-single-dollar ()
  (with-temp-buffer
    (insert "a $x$ b $y+1$ c")
    (let ((fragments (ratex-fragments-in-buffer)))
      (should (= (length fragments) 2))
      (should (equal (mapcar (lambda (fragment)
                               (plist-get fragment :content))
                             fragments)
                     '("x" "y+1"))))))

(ert-deftest ratex-fragments-in-region-stays-local ()
  (with-temp-buffer
    (insert "a $x$\n\nb $y+1$ c")
    (let ((fragments (ratex-fragments-in-region 1 7)))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :content) "x")))))

(ert-deftest ratex-fragments-to-render-excludes-active ()
  (with-temp-buffer
    (insert "a $x$ b $y$ c")
    (goto-char 4)
    (let* ((fragments (ratex-fragments-in-buffer))
           (active (ratex-fragment-at-point))
           (targets (ratex--fragments-to-render fragments active)))
      (should (= (length fragments) 2))
      (should (= (length targets) 1))
      (should (equal (plist-get (car targets) :content) "y")))))

(ert-deftest ratex-refresh-previews-renders-all-non-active ()
  (with-temp-buffer
    (insert "a $x$ b $y$ c")
    (goto-char 4)
    (let (rendered)
      (cl-letf (((symbol-function 'ratex--ensure-fragment-preview)
                 (lambda (fragment)
                   (push (plist-get fragment :content) rendered)))
                ((symbol-function 'ratex--drop-stale-overlays)
                 (lambda (_keys) nil)))
        (ratex-refresh-previews)
        (should (equal rendered '("y")))))))

(ert-deftest ratex-refresh-previews-renders-all-with-include-active ()
  (with-temp-buffer
    (insert "a $x$ b $y$ c")
    (goto-char 4)
    (let (rendered)
      (cl-letf (((symbol-function 'ratex--ensure-fragment-preview)
                 (lambda (fragment)
                   (push (plist-get fragment :content) rendered)))
                ((symbol-function 'ratex--drop-stale-overlays)
                 (lambda (_keys) nil)))
        (ratex-refresh-previews t)
        (should (equal (sort rendered #'string<) '("x" "y")))))))

(ert-deftest ratex-normalized-render-color ()
  (let ((ratex-render-color "  #ff00aa  "))
    (should (equal (ratex--normalized-render-color) "#ff00aa")))
  (let ((ratex-render-color "   "))
    (should-not (ratex--normalized-render-color))))

(ert-deftest ratex-cache-key-is-typst ()
  (let ((ratex-render-backend 'typst)
        (ratex-font-size 12)
        (ratex-svg-padding 2)
        (ratex-render-color nil)
        (ratex-typst-font nil)
        (ratex-typst-font-dir nil))
    (let ((key (ratex--cache-key
                '(:begin 1 :end 4 :content "x" :open "$" :close "$"))))
      (should (eq (car key) 'typst))
      (should (equal (cadr key) "x")))))

(ert-deftest ratex-typst-source-includes-font-family ()
  (let ((ratex-font-size 16)
        (ratex-render-color "#abcdef")
        (ratex-typst-font "GFS Neohellenic Math"))
    (let ((source (ratex--typst-source
                   '(:begin 1 :end 4 :content "x" :open "$" :close "$"))))
      (should (string-match-p "GFS Neohellenic Math" source))
      (should (string-match-p "rgb(\"#abcdef\")" source)))))

(ert-deftest ratex-typst-display-style-follows-leading-space ()
  (let ((ratex-render-backend 'typst))
    (should-not
     (ratex--typst-display-fragment-p
      '(:begin 1 :end 4 :content "x" :open "$" :close "$")))
    (should
     (ratex--typst-display-fragment-p
      '(:begin 1 :end 5 :content " x" :open "$" :close "$")))
    (should
     (ratex--typst-display-fragment-p
      '(:begin 1 :end 5 :content "\nx" :open "$" :close "$")))))

(ert-deftest ratex-typst-source-keeps-inline-dollars-tight ()
  (let ((ratex-render-backend 'typst)
        (ratex-font-size 16)
        (ratex-render-color "#abcdef")
        (ratex-typst-font nil))
    (let ((source (ratex--typst-source
                   '(:begin 1 :end 4 :content "x" :open "$" :close "$"))))
      (should (string-match-p "\n\\$x\\$\n\\'" source)))))

(ert-deftest ratex-typst-source-uses-display-dollars-for-leading-space ()
  (let ((ratex-render-backend 'typst)
        (ratex-font-size 16)
        (ratex-render-color "#abcdef")
        (ratex-typst-font nil))
    (let ((source (ratex--typst-source
                   '(:begin 1 :end 5 :content " x" :open "$" :close "$"))))
      (should (string-match-p "\n\\$\nx\n\\$\n\\'" source)))))

(ert-deftest ratex-typst-source-uses-display-dollars-for-leading-newline ()
  (let ((ratex-render-backend 'typst)
        (ratex-font-size 16)
        (ratex-render-color "#abcdef")
        (ratex-typst-font nil))
    (let ((source
           (ratex--typst-source
            '(:begin 1
              :end 33
              :content "\na, b in { 1/2, sqrt(4 a b) }\n"
              :open "$"
              :close "$"))))
      (should (string-match-p
               "\n\\$\na, b in { 1/2, sqrt(4 a b) }\n\\$\n\\'"
               source)))))

(ert-deftest ratex-cache-key-separates-typst-display-style ()
  (let ((ratex-render-backend 'typst)
        (ratex-font-size 12)
        (ratex-svg-padding 2)
        (ratex-render-color nil)
        (ratex-typst-font nil)
        (ratex-typst-font-dir nil))
    (should-not
     (equal
      (ratex--cache-key
       '(:begin 1 :end 4 :content "x" :open "$" :close "$"))
      (ratex--cache-key
       '(:begin 1 :end 5 :content " x" :open "$" :close "$"))))))

(ert-deftest ratex-overlay-style-follows-typst-display-spacing ()
  (let ((ratex-render-backend 'typst))
    (should
     (eq (ratex--overlay-style-for-fragment
          '(:begin 1 :end 4 :content "x" :open "$" :close "$"))
         'inline))
    (should
     (eq (ratex--overlay-style-for-fragment
          '(:begin 1 :end 5 :content " x" :open "$" :close "$"))
         'below))))

(ert-deftest ratex-overlay-style-follows-typst-leading-newline ()
  (let ((ratex-render-backend 'typst))
    (should
     (eq (ratex--overlay-style-for-fragment
          '(:begin 1
            :end 33
            :content "\na, b in { 1/2, sqrt(4 a b) }\n"
            :open "$"
            :close "$"))
         'below))))

(ert-deftest ratex-json-response-uses-symbol-keys ()
  (let* ((json-object-type 'alist)
         (json-key-type 'symbol)
         (json-array-type 'list)
         (json-false :false)
         (payload (json-read-from-string
                   "{\"id\":1,\"ok\":true,\"height\":2.0,\"baseline\":1.0}")))
    (should (equal (alist-get 'id payload) 1))
    (should (equal (alist-get 'ok payload) t))
    (should (equal (alist-get 'height payload) 2.0))
    (should (equal (alist-get 'baseline payload) 1.0))))

(ert-deftest ratex-project-root-follows-library-location ()
  (let ((default-directory "/tmp/"))
    (let ((root (directory-file-name (ratex-root))))
      (should (file-directory-p root))
      (should (file-exists-p (expand-file-name "backend/Cargo.toml" root)))
      (should (file-directory-p (expand-file-name "lisp" root))))))

(ert-deftest ratex-backend-root-override-wins ()
  (let ((ratex-backend-root "/tmp/ratex-root/"))
    (should (equal (ratex-root)
                   (file-name-as-directory
                    (expand-file-name "/tmp/ratex-root/"))))))

(ert-deftest ratex-default-font-dir-follows-library-root ()
  (let ((default-directory "/tmp/"))
    (let ((dir (ratex-default-font-dir)))
      (should (stringp dir))
      (should (file-directory-p dir))
      (should (file-exists-p (expand-file-name "KaTeX_Main-Regular.ttf" dir))))))

(ert-deftest ratex-typst-mode-does-not-start-rust-backend ()
  (with-temp-buffer
    (let ((ratex-render-backend 'typst))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'ratex--supported-buffer-p)
                 (lambda () t))
                ((symbol-function 'ratex-start-backend)
                 (lambda (&rest _args)
                   (error "Typst backend should not start Rust backend")))
                ((symbol-function 'ratex-initialize-previews)
                 (lambda () nil))
                ((symbol-function 'ratex--refresh-preview-now)
                 (lambda () nil)))
        (unwind-protect
            (ratex-mode 1)
          (ratex-mode -1))))))

(ert-deftest ratex-renders-typst-response-in-origin-buffer ()
  (let ((origin (generate-new-buffer " *ratex-origin*"))
        (other (generate-new-buffer " *ratex-other*")))
    (unwind-protect
        (with-current-buffer origin
          (insert "$x$")
          (let ((ratex-render-backend 'typst)
                (ratex-inline-preview t)
                (ratex-edit-preview nil)
                callback
                displayed-buffer
                displayed-key)
            (setq-local ratex-mode t)
            (ratex-reset-buffer-state)
            (cl-letf (((symbol-function 'ratex--typst-request)
                       (lambda (_fragment cb)
                         (setq callback cb)
                         t))
                      ((symbol-function 'ratex--display-if-visible)
                       (lambda (fragment-key _fragment _response)
                         (setq displayed-buffer (current-buffer))
                         (setq displayed-key fragment-key))))
              (ratex--ensure-fragment-preview (car (ratex-fragments-in-buffer)))
              (with-current-buffer other
                (funcall callback
                         '((ok . t)
                           (svg . "<svg xmlns=\"http://www.w3.org/2000/svg\"/>")
                           (height . 1.0)
                           (baseline . 1.0))))
              (should (eq displayed-buffer origin))
              (should (equal displayed-key "1:4:x")))))
      (kill-buffer origin)
      (kill-buffer other))))

(ert-deftest ratex-active-typst-error-does-not-rerender-loop ()
  (with-temp-buffer
    (insert "$x$")
    (goto-char 2)
    (let* ((fragment (ratex-fragment-at-point))
           (fragment-key (ratex--fragment-key fragment))
           (ratex-edit-preview 'posframe))
      (setq-local ratex-mode t)
      (setq-local ratex--preview-enabled t)
      (cl-letf (((symbol-function 'ratex--ensure-fragment-preview)
                 (lambda (_fragment)
                   (error "should not rerender failed active fragment")))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (ratex--display-if-visible
         fragment-key
         fragment
         '((ok) (error . "typst exited with 1")))
        (should (equal ratex--last-error "typst exited with 1"))))))

(ert-deftest ratex-active-fragment-at-point-reuses-active-without-parser ()
  (with-temp-buffer
    (insert "a $xy$ b")
    (goto-char 4)
    (setq-local ratex--active-fragment (ratex-fragment-at-point))
    (goto-char 5)
    (cl-letf (((symbol-function 'ratex-fragment-at-point)
               (lambda ()
                 (error "full parser should not run"))))
      (let ((fragment (ratex--active-fragment-at-point)))
        (should (equal (plist-get fragment :content) "xy"))))))

(ert-deftest ratex-post-command-refreshes-active-fragment-without-parser ()
  (with-temp-buffer
    (insert "a $x$ b")
    (goto-char 4)
    (setq-local ratex-mode t)
    (setq-local ratex--active-fragment (ratex-fragment-at-point))
    (insert "y")
    (cl-letf (((symbol-function 'ratex-fragment-at-point)
               (lambda ()
                 (error "full parser should not run"))))
      (ratex-handle-post-command)
      (should (equal (plist-get ratex--active-fragment :content) "yx"))
      (should (= (plist-get ratex--active-fragment :end) 7)))))

(ert-deftest ratex-post-command-keeps-preview-while-staying-in-same-fragment ()
  (with-temp-buffer
    (insert "a $xy$ b")
    (goto-char 4)
    (setq-local ratex-mode t)
    (setq-local ratex-edit-preview 'posframe)
    (setq-local ratex--preview-enabled t)
    (setq-local ratex--active-fragment (ratex-fragment-at-point))
    (setq-local ratex--posframe-visible t)
    (let (removed handled)
      (cl-letf (((symbol-function 'ratex-remove-overlay)
                 (lambda (key)
                   (push key removed)))
                ((symbol-function 'ratex--handle-preview-at-point)
                 (lambda (fragment)
                   (push (plist-get fragment :content) handled)))
                ((symbol-function 'ratex--update-posframe-position)
                 (lambda () nil)))
        (goto-char 5)
        (ratex-handle-post-command)
        (should-not removed)
        (should-not handled)
        (should (equal (plist-get ratex--active-fragment :content) "xy"))))))

(ert-deftest ratex-rendered-overlay-at-point-p-detects-range ()
  (with-temp-buffer
    (insert "abcdef")
    (ratex-show-overlay "1:4:x" 1 4 "IMG")
    (goto-char 1)
    (should (ratex-rendered-overlay-at-point-p))
    (goto-char 3)
    (should (ratex-rendered-overlay-at-point-p))
    (goto-char 4)
    (should-not (ratex-rendered-overlay-at-point-p))))

(ert-deftest ratex-overlay-fragment-at-point-returns-fragment ()
  (with-temp-buffer
    (insert "abcdef")
    (ratex-show-overlay
     "2:5:x" 2 5 "IMG" nil
     '(:begin 2 :end 5 :content "x" :open "$" :close "$"))
    (goto-char 3)
    (let ((fragment (ratex-overlay-fragment-at-point)))
      (should (equal (plist-get fragment :content) "x")))))

(provide 'ratex-tests)
;;; ratex-tests.el ends here

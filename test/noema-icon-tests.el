;;; noema-icon-tests.el --- Noema file icon integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-ui)
(require 'treemacs)

(ert-deftest noema-icon-svg-is-valid-and-separate-from-markdown ()
  (require 'xml)
  (should (file-readable-p my/noema-work-icon-file))
  (should-not (equal my/noema-icon-file my/noema-work-icon-file))
  (with-temp-buffer
    (insert-file-contents my/noema-work-icon-file)
    (should (eq 'svg (caar (xml-parse-region (point-min) (point-max)))))))

(ert-deftest noema-icon-svg-caches-by-kind-and-size-without-hot-path-io ()
  (let ((my/file-icon-image-cache (make-hash-table :test #'equal)) (created 0))
    ;; Existing Markdown cache entries must not be returned for .noema.
    (puthash 15 'markdown-image my/file-icon-image-cache)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (file _type _data &rest props)
                 (should (equal file my/noema-work-icon-file))
                 (cl-incf created)
                 (list 'image :file file :height (plist-get props :height)))))
      (let* ((icon (my/file-icon-for-file "proof.noema" :image-height 15))
             (image (get-text-property 0 'display icon)))
        (should (eq 'image (car image)))
        (cl-letf (((symbol-function 'file-readable-p) (lambda (&rest _) (ert-fail "Cache hit performed file I/O"))))
          (dotimes (_ 1000)
            (should (eq image (get-text-property 0 'display
                                                (my/file-icon-for-file "/fs:local:/proof.NOEMA" :image-height 15)))))))
        (my/file-icon-for-file "proof.noema" :image-height 20)
        (should (= 2 created)))))

(ert-deftest noema-icon-terminal-fallback-does-not-load-fonts-or-images ()
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'create-image) (lambda (&rest _) (ert-fail "Terminal loaded image")))
            ((symbol-function 'nerd-icons-mdicon) (lambda (&rest _) (ert-fail "Terminal required Nerd Font"))))
    (should (equal "◇" (substring-no-properties (my/file-icon-for-file "proof.noema"))))
    (should-not (my/file-icon-for-file nil))
    (should-not (my/file-icon-for-file "plain.txt"))))

(ert-deftest noema-icon-material-provider-covers-file-list-consumers ()
  (require 'material-icon-utils)
  (should (equal my/noema-work-icon-file (gethash "noema" material-icon-file-icon-table)))
  (should (equal my/noema-work-icon-file (material-icon-get-icon-for-file "proof.NOEMA")))
  (should-not (equal my/noema-work-icon-file (material-icon-get-icon-for-file "proof.md")))
  (should-not (equal my/noema-work-icon-file (material-icon-get-icon-for-file "folder.noema" t))))

(ert-deftest noema-icon-font-provider-covers-modeline-and-research-mode ()
  (require 'nerd-icons)
  (let ((expected (substring-no-properties (nerd-icons-mdicon "nf-md-graph_outline"))))
    (should (equal expected (substring-no-properties (nerd-icons-icon-for-file "proof.noema"))))
    (should (equal expected (substring-no-properties (nerd-icons-icon-for-mode 'noema-research-mode))))))

(ert-deftest noema-icon-tab-label-keeps-file-identity-and-click-target ()
  (require 'init-tabbar)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/proof.noema")
    (let ((label (my/tab-line-tab-string (current-buffer))))
      (should (string-match-p "◇" (substring-no-properties label)))
      (should (eq (current-buffer) (get-text-property 0 'tab label))))))

(ert-deftest noema-icon-treemacs-registers-the-extension-and-terminal-fallback ()
  (require 'treemacs)
  (should (equal "◇ " (substring-no-properties (treemacs-get-icon-value "noema" t))))
  (should (treemacs-get-icon-value "noema")))

;;; noema-icon-tests.el ends here

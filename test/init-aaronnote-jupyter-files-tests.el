;;; init-aaronnote-jupyter-files-tests.el --- Notebook file workflows -*- lexical-binding: t; -*-
(require 'ert)
(require 'init-aaronnote-jupyter-files)

(defun my/notebook-files-test--document (&optional source)
  (json-parse-string
   (format "{\"nbformat\":4,\"nbformat_minor\":5,\"metadata\":{},\"cells\":[{\"id\":\"a\",\"cell_type\":\"code\",\"metadata\":{},\"source\":%s,\"execution_count\":1,\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":\"saved output\"}]}]}"
           (json-encode-string (or source "x = 1"))) :null-object nil :false-object :json-false))

(ert-deftest my/notebook-files-snapshot-merges-unsaved-source-with-disk-output ()
  (let ((file (make-temp-file "notebook-snapshot-" nil ".ipynb")))
    (unwind-protect
        (with-temp-buffer
          (let ((document (my/notebook-files-test--document)))
            (my/noema-jupyter-notebook--write-raw file document)
            (setq buffer-file-name file)
            (my/noema-jupyter-notebook--install-projection document)
            (goto-char (point-min)) (search-forward "x = 1") (replace-match "x = 2")
            (let* ((snapshot (my/noema-jupyter-files--snapshot))
                   (cell (aref (gethash "cells" snapshot) 0)))
              (should (string-match-p "x = 2" (gethash "source" cell)))
              (should (equal "saved output" (gethash "text" (aref (gethash "outputs" cell) 0))))
              (should (buffer-modified-p))
              (should (equal "x = 1" (gethash "source" (aref (gethash "cells" (my/noema-jupyter-notebook--read-raw file)) 0)))))))
      (delete-file file))))

(ert-deftest my/notebook-diff-is-cell-aware-and-components-are-optional ()
  (let* ((document (my/notebook-files-test--document "print(42)"))
         (source (my/noema-jupyter-diff--text document nil))
         (complete (my/noema-jupyter-diff--text document '("outputs" "metadata"))))
    (should (string-match-p "Cell a \\[code\\]" source))
    (should (string-match-p "print(42)" source))
    (should-not (string-match-p "saved output" source))
    (should (string-match-p "saved output" complete))
    (should (string-match-p "Notebook metadata" complete))
    (should (equal (my/noema-jupyter-diff--json (json-parse-string "{\"a\":1,\"b\":2}"))
                   (my/noema-jupyter-diff--json (json-parse-string "{\"b\":2,\"a\":1}"))))))

(ert-deftest my/notebook-export-stages-relative-image-through-owning-directory ()
  (let ((document (my/notebook-files-test--document)) opened)
    (puthash "cell_type" "markdown" (aref (gethash "cells" document) 0))
    (puthash "source" "![figure](images/a.png) ![outside](../secret.png) ![URL](https://example.org/a.png)"
             (aref (gethash "cells" document) 0))
    (cl-letf (((symbol-function 'insert-file-contents-literally)
               (lambda (file &rest _) (push file opened) (insert "png bytes"))))
      (let ((assets (my/noema-jupyter-export--assets document "/fs:lab:/notebooks/")))
        (should (equal opened '("/fs:lab:/notebooks/images/a.png")))
        (should (equal (gethash "images/a.png" (car assets)) (base64-encode-string "png bytes" t)))))))

(ert-deftest my/notebook-export-rejects-work-document-before-reading ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/work.noema" my/noema-jupyter-notebook--projection-p t)
    (cl-letf (((symbol-function 'my/noema-jupyter-notebook--read-raw) (lambda (&rest _) (ert-fail "Read work document"))))
      (should-error (my/noema-jupyter-export "html" "/tmp/work.html") :type 'user-error))))

(ert-deftest my/notebook-git-diff-uses-real-revision-and-cleans-comparison-buffers ()
  (require 'ediff)
  (let* ((directory (make-temp-file "notebook-git-diff-" t))
         (file (expand-file-name "audit.ipynb" directory))
         (default-directory (file-name-as-directory directory))
         (ediff-window-setup-function #'ediff-setup-windows-plain)
         control variants)
    (unwind-protect
        (with-temp-buffer
          (should (zerop (call-process "git" nil nil nil "init" "-q")))
          (my/noema-jupyter-notebook--write-raw file (my/notebook-files-test--document))
          (should (zerop (call-process "git" nil nil nil "add" "audit.ipynb")))
          (should (zerop (call-process "git" nil nil nil "-c" "user.name=Notebook Test" "-c" "user.email=notebook@example.invalid" "commit" "-qm" "baseline")))
          (setq buffer-file-name file)
          (my/noema-jupyter-notebook--install-projection (my/notebook-files-test--document))
          (goto-char (point-min)) (search-forward "x = 1") (replace-match "x = 2")
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "HEAD")))
            (setq control (my/noema-jupyter-compare "Git revision")))
          (with-current-buffer control
            (setq variants (list ediff-buffer-A ediff-buffer-B))
            (should (with-current-buffer ediff-buffer-A (string-match-p "x = 1" (buffer-string))))
            (should (with-current-buffer ediff-buffer-B (string-match-p "x = 2" (buffer-string))))
            (ediff-really-quit nil))
          (should (cl-every (lambda (buffer) (not (buffer-live-p buffer))) variants)))
      (when (buffer-live-p control) (with-current-buffer control (ediff-really-quit nil)))
      (delete-directory directory t))))

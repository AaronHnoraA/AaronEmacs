;;; init-aaronnote-jupyter-notebook-tests.el --- Native ipynb tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name "../lisp/roam"
                               (file-name-directory (or load-file-name
                                                        buffer-file-name))))
(require 'init-aaronnote-jupyter-notebook)

(defun my/noema-jupyter-notebook-test--document ()
  "Return a representative notebook JSON object."
  (let* ((document (make-hash-table :test #'equal))
         (metadata (make-hash-table :test #'equal))
         (kernelspec (make-hash-table :test #'equal))
         (language-info (make-hash-table :test #'equal))
         (noema (make-hash-table :test #'equal))
         (cell (make-hash-table :test #'equal))
         (cell-metadata (make-hash-table :test #'equal))
         (custom (make-hash-table :test #'equal))
         (output (make-hash-table :test #'equal)))
    (puthash "name" "python3" kernelspec)
    (puthash "display_name" "Python 3" kernelspec)
    (puthash "language" "python" kernelspec)
    (puthash "name" "python" language-info)
    (puthash "source_file" "/tmp/note.md" noema)
    (puthash "session" "default" noema)
    (puthash "language" "python" noema)
    (puthash "storage" "ipynb" noema)
    (puthash "kernelspec" kernelspec metadata)
    (puthash "language_info" language-info metadata)
    (puthash "noema" noema metadata)
    (puthash "kept" t custom)
    (puthash "custom" custom cell-metadata)
    (puthash "output_type" "stream" output)
    (puthash "name" "stdout" output)
    (puthash "text" "42\n" output)
    (puthash "cell_type" "code" cell)
    (puthash "id" "cell-a" cell)
    (puthash "metadata" cell-metadata cell)
    (puthash "source" "answer = 41" cell)
    (puthash "execution_count" 3 cell)
    (puthash "outputs" (vector output) cell)
    (puthash "cells" (vector cell) document)
    (puthash "metadata" metadata document)
    (puthash "nbformat" 4 document)
    (puthash "nbformat_minor" 5 document)
    document))

(ert-deftest my/noema-jupyter-notebook-visits-source-and-preserves-results ()
  (let ((file (make-temp-file "noema-ipynb-" nil ".ipynb"))
        buffer)
    (unwind-protect
        (progn
          (my/noema-jupyter-notebook--write-raw
           file (my/noema-jupyter-notebook-test--document))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should my/noema-jupyter-notebook--projection-p)
            (should (derived-mode-p 'python-mode))
            (should (equal (buffer-string)
                           "# %% id=cell-a\nanswer = 41\n\n"))
            (goto-char (point-min))
            (search-forward "answer = 41")
            (replace-match "answer = 42")
            (save-buffer))
          (let* ((saved (my/noema-jupyter-notebook--read-raw file))
                 (cell (aref (gethash "cells" saved) 0)))
            (should (equal (gethash "id" cell) "cell-a"))
            (should (equal (gethash "source" cell) "answer = 42"))
            (should (= (gethash "execution_count" cell) 3))
            (should (equal (gethash "text" (aref (gethash "outputs" cell) 0))
                           "42\n"))
            (should (eq (gethash "kept"
                                 (gethash "custom" (gethash "metadata" cell)))
                        t))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (ignore-errors (delete-file file)))))

(ert-deftest my/noema-jupyter-notebook-output-mirror-uses-standard-fields ()
  (let* ((document (my/noema-jupyter-notebook-test--document))
         (cell (aref (gethash "cells" document) 0))
         (mirror (my/noema-jupyter-notebook-output-mirror document))
         (saved (cdr (assq 'cell-a (alist-get 'cells mirror)))))
    (should (= (gethash "executionCount" saved) 3))
    (should (= (length (gethash "outputs" saved)) 1))
    (let ((private (make-hash-table :test #'equal)))
      (puthash "status" "ok" private)
      (puthash "executionCount" 9 private)
      (puthash "outputs" nil private)
      (my/noema-jupyter-notebook-apply-output-mirror
       document `((cells . ((cell-a . ,private)))))
      (should (= (gethash "execution_count" cell) 9))
      (should (equal (gethash "status"
                              (gethash "noema" (gethash "metadata" cell)))
                     "ok"))
      (should-not (gethash "executionCount"
                           (gethash "noema" (gethash "metadata" cell)))))))

(ert-deftest my/noema-jupyter-notebook-projects-markdown-with-stable-id ()
  (let* ((document (my/noema-jupyter-notebook-test--document))
         (cell (make-hash-table :test #'equal)))
    (puthash "cell_type" "markdown" cell)
    (puthash "id" "intro-cell" cell)
    (puthash "metadata" (make-hash-table :test #'equal) cell)
    (puthash "source" "# Heading\ntext" cell)
    (puthash "cells" (vector cell) document)
    (with-temp-buffer
      (setq-local my/noema-jupyter-notebook--document document)
      (my/noema-jupyter-notebook--render document)
      (should (equal (buffer-string)
                     "# %% [markdown] id=intro-cell\n# # Heading\n# text\n\n"))
      (let ((projected (car (my/noema-jupyter-notebook-projection-cells
                             document))))
        (should (equal (plist-get projected :id) "intro-cell"))
        (should (equal (plist-get projected :source) "# Heading\ntext"))))))

(provide 'init-aaronnote-jupyter-notebook-tests)

;;; init-aaronnote-jupyter-notebook-tests.el ends here

;;; init-aaronnote-jupyter-notebook-tests.el --- Native ipynb tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name "../lisp/roam"
                               (file-name-directory (or load-file-name
                                                        buffer-file-name))))
(require 'init-aaronnote-jupyter-notebook)
(unless (fboundp 'my/noema-command)
  (defun my/noema-command (&rest _args)
    "Test stub for the Noema event bridge."
    nil))
(require 'init-aaronnote-jupyter-cell)

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

(ert-deftest my/noema-jupyter-notebook-save-merges-noema-results-from-disk ()
  (let ((file (make-temp-file "noema-ipynb-merge-" nil ".ipynb"))
        buffer)
    (unwind-protect
        (progn
          (my/noema-jupyter-notebook--write-raw
           file (my/noema-jupyter-notebook-test--document))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "answer = 41")
            (replace-match "answer = 43"))
          ;; Simulate Noema completing an execution while Emacs still holds
          ;; an edited source projection.
          (let* ((runtime-copy (my/noema-jupyter-notebook--read-raw file))
                 (cell (aref (gethash "cells" runtime-copy) 0))
                 (output (aref (gethash "outputs" cell) 0)))
            (puthash "execution_count" 8 cell)
            (puthash "text" "runtime output\n" output)
            (my/noema-jupyter-notebook--write-raw file runtime-copy))
          (with-current-buffer buffer
            (my/noema-jupyter-notebook--write-contents))
          (let* ((saved (my/noema-jupyter-notebook--read-raw file))
                 (cell (aref (gethash "cells" saved) 0)))
            (should (equal (gethash "source" cell) "answer = 43"))
            (should (= (gethash "execution_count" cell) 8))
            (should (equal (gethash "text" (aref (gethash "outputs" cell) 0))
                           "runtime output\n"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (ignore-errors (delete-file file)))))

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

(ert-deftest my/noema-jupyter-notebook-preserves-missing-ids-in-ordinary-file ()
  (let ((file (make-temp-file "ordinary-notebook-" nil ".ipynb")) buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (json-serialize
              '((cells . [((cell_type . "code")
                           (metadata . ())
                           (execution_count . nil)
                           (outputs . [])
                           (source . "value = 1\n"))])
                (metadata . ((kernelspec . ((name . "python3")
                                            (language . "python")))))
                (nbformat . 4)
                (nbformat_minor . 4)))
             "\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should my/noema-jupyter-notebook--projection-p)
            (goto-char (point-min))
            (should (looking-at "# %%$"))
            (should (equal (get-text-property
                            (point) 'my/noema-jupyter-cell-id)
                           "cell-1"))
            (forward-line 1)
            (search-forward "value = 1")
            (replace-match "value = 2")
            (save-buffer))
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((saved (json-parse-string
                           (buffer-string) :object-type 'hash-table
                           :array-type 'array))
                   (cell (aref (gethash "cells" saved) 0)))
              (should-not (gethash "id" cell))
              (should (equal (gethash "source" cell) "value = 2\n"))
              (should (= (gethash "nbformat_minor" saved) 4)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (ignore-errors (delete-file file)))))

(ert-deftest my/noema-jupyter-notebook-upgrades-managed-missing-id ()
  (let* ((document (my/noema-jupyter-notebook-test--document))
         (cell (aref (gethash "cells" document) 0)))
    (remhash "id" cell)
    (puthash "nbformat_minor" 4 document)
    (my/noema-jupyter-notebook--normalize document)
    (should (equal (gethash "id" cell) "cell-1"))
    (should (= (gethash "nbformat_minor" document) 5))
    (should (eq (my/noema-jupyter-notebook--transient-id cell)
                my/noema-jupyter-notebook--transient-id-not-found))))

(ert-deftest my/noema-jupyter-notebook-promotes-bare-managed-marker-before-run ()
  (with-temp-buffer
    (let ((document (my/noema-jupyter-notebook-test--document)))
      (my/noema-jupyter-notebook--install-projection document)
      (goto-char (point-max))
      (insert "# %%\nprint(answer)\n")
      (my/noema-jupyter-notebook--canonicalize-managed-markers)
      (goto-char (point-min))
      (re-search-forward "^# %% id=\\([A-Za-z0-9_-]+\\)$" nil t 2)
      (let ((new-id (match-string-no-properties 1)))
        (should new-id)
        (my/noema-jupyter-notebook--sync-document document)
        (let* ((cells (gethash "cells" document))
               (new-cell (aref cells 1)))
          (should (= (length cells) 2))
          (should (equal (gethash "id" new-cell) new-id))
          (should (equal (gethash "source" new-cell) "print(answer)\n")))))))

(ert-deftest my/noema-jupyter-notebook-keeps-standalone-noema-ui-id-transient ()
  (let* ((document (my/noema-jupyter-notebook-test--document))
         (metadata (gethash "metadata" document))
         (noema (gethash "noema" metadata))
         (cell (aref (gethash "cells" document) 0)))
    (puthash "source_file" "/tmp/standalone.ipynb" noema)
    (remhash "id" cell)
    (puthash "nbformat_minor" 4 document)
    (my/noema-jupyter-notebook--normalize document)
    (should (equal (gethash "id" cell) "cell-1"))
    (should (= (gethash "nbformat_minor" document) 4))
    (should (eq (my/noema-jupyter-notebook--transient-id cell)
                my/noema-jupyter-notebook--transient-id-missing))))

(ert-deftest my/noema-jupyter-notebook-mode-switch-preserves-projection ()
  (with-temp-buffer
    (let ((document (my/noema-jupyter-notebook-test--document)))
      (my/noema-jupyter-notebook--install-projection document)
      (goto-char (point-max))
      (insert "# unsaved")
      (my/noema-jupyter-notebook-switch-editor-mode 'text-mode)
      (should (eq major-mode 'text-mode))
      (should (eq my/noema-jupyter-notebook--editor-mode 'text-mode))
      (should my/noema-jupyter-notebook--projection-p)
      (should (eq my/noema-jupyter-notebook--document document))
      (should (memq #'my/noema-jupyter-notebook--write-contents
                    write-contents-functions))
      (should (buffer-modified-p))
      (should (string-suffix-p "# unsaved" (buffer-string))))))

(ert-deftest my/noema-jupyter-cell-ui-keeps-editor-mode-out-of-kernel-header ()
  (with-temp-buffer
    (let ((document (my/noema-jupyter-notebook--normalize
                     (json-parse-string
                      "{\"cells\":[{\"cell_type\":\"code\",\"metadata\":{},\"outputs\":[],\"source\":\"x = 1\"}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":4}"
                      :object-type 'hash-table :array-type 'array))))
      (setq-local my/noema-jupyter-notebook--document document)
      (my/noema-jupyter-notebook--render document)
      (goto-char (point-min))
      (should (equal (plist-get (my/noema-jupyter-cell--bounds-at-point) :id)
                     "cell-1"))
      (setq-local my/noema-jupyter-notebook--editor-mode 'python-mode)
      (let ((header (substring-no-properties
                     (apply #'concat (my/noema-jupyter-cell--header-line)))))
        (should (string-match-p "Kernel:" header))
        (should-not (string-match-p "Editor/LSP:" header)))
      ;; Editor/LSP remains available as an explicit advanced command, but it
      ;; is no longer presented as a peer of the Noema kernel selector.
      (should (eq (lookup-key my/noema-jupyter-cell-mode-map (kbd "C-c i l"))
                  #'my/noema-jupyter-cell-select-editor-mode)))))

(ert-deftest my/noema-jupyter-cell-kernel-catalog-is-not-language-filtered ()
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (let* ((catalog
            (json-parse-string
             "{\"selections\":[{\"kind\":\"none\",\"value\":\"\",\"label\":\"No Kernel\"},{\"kind\":\"start\",\"value\":\"python3\",\"name\":\"python3\",\"label\":\"Start · Kernel Specs · Python  [python3]\"},{\"kind\":\"start\",\"value\":\"bash\",\"name\":\"bash\",\"label\":\"Start · Kernel Specs · Bash  [bash]\"},{\"kind\":\"start\",\"value\":\"maple\",\"name\":\"maple\",\"label\":\"Start · Kernel Specs · Maple  [maple]\"},{\"kind\":\"connect\",\"value\":\"kernel-here\",\"name\":\"python3\",\"label\":\"Connect · Running Kernel · python3 · idle  [kernel-here]\"}]}"
             :object-type 'hash-table :array-type 'array))
           (choices (my/noema-jupyter-cell--kernel-choices catalog))
           (labels (mapcar #'car choices)))
      (should (= (length choices) 5))
      (should (equal (car labels) "No Kernel"))
      (should (seq-some (lambda (label) (string-match-p "Bash" label)) labels))
      (should (seq-some (lambda (label) (string-match-p "Maple" label)) labels))
      (should (seq-some (lambda (label) (string-match-p "kernel-here" label)) labels))
      (should-not (seq-some (lambda (label) (string-match-p "kernel-other" label)) labels)))))

(ert-deftest my/noema-jupyter-cell-mode-has-no-session-polling-timer ()
  (should-not (boundp 'my/noema-jupyter-cell-session-refresh-interval))
  (should-not (fboundp 'my/noema-jupyter-cell--start-session-refresh-timer))
  (should-not (fboundp 'my/noema-jupyter-cell--stop-session-refresh-timer)))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-allows-server-timeout ()
  (with-temp-buffer
    (insert "# %% id=cell-a\nanswer\n")
    (goto-char (point-min))
    (search-forward "answer")
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-kernel "python3")
    (setq-local my/noema-jupyter-cell-session "default")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t)
          seen-timeout)
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (_channel _body timeout)
                   (setq seen-timeout timeout)
                   '((ok . t) (supported . :json-false) (found . :json-false)))))
        (should (my/noema-jupyter-cell--introspect "inspect" nil t))
        (should (= seen-timeout my/noema-jupyter-cell-inspect-timeout))))))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-surfaces-transport-error ()
  (with-temp-buffer
    (insert "# %% id=cell-a\nanswer\n")
    (goto-char (point-min))
    (search-forward "answer")
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-kernel "python3")
    (setq-local my/noema-jupyter-cell-session "default")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t))
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (&rest _) (error "gateway timed out"))))
        (should-error
         (my/noema-jupyter-cell--introspect "inspect" nil t)
         :type 'user-error)))))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-uses-dotted-expression ()
  (with-temp-buffer
    (insert "# %% id=cell-a\nvalue = np.array([1])\n")
    (goto-char (point-min))
    (search-forward "np.arr")
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-kernel "python3")
    (setq-local my/noema-jupyter-cell-session "default")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t)
          requests)
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (_channel body _timeout)
                   (push body requests)
                   '((ok . t) (supported . t) (found . t)))))
        (should (my/noema-jupyter-cell--json-true-p
                 (my/noema-jupyter-cell--introspect "inspect" nil t)
                 'found)))
      (should (= (length requests) 1))
      (let ((request (car requests)))
        (should (equal (alist-get 'code request) "np.array"))
        (should (= (alist-get 'cursorPos request) (length "np.array")))))))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-uses-enclosing-call-in-string ()
  (with-temp-buffer
    (python-mode)
    (insert "# %% id=cell-a\nprint(\"sadsad\")\n")
    (goto-char (point-min))
    (search-forward "sad")
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t)
          request)
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (_channel body _timeout)
                   (setq request body)
                   '((ok . t) (supported . t) (found . t)))))
        (should (my/noema-jupyter-cell--json-true-p
                 (my/noema-jupyter-cell--introspect "inspect" nil t)
                 'found)))
      (should (equal (alist-get 'code request) "print"))
      (should (= (alist-get 'cursorPos request) (length "print"))))))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-uses-call-after-closing-paren ()
  (with-temp-buffer
    (python-mode)
    (insert "# %% id=cell-a\nprint(\"sadsad\")\n")
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t)
          request)
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (_channel body _timeout)
                   (setq request body)
                   '((ok . t) (supported . t) (found . t)))))
        (my/noema-jupyter-cell--introspect "inspect" nil t))
      (should (equal (alist-get 'code request) "print"))
      (should (= (alist-get 'cursorPos request) (length "print"))))))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-maps-python-number-to-type ()
  (with-temp-buffer
    (python-mode)
    (insert "# %% id=cell-a\na = 3\n")
    (goto-char (point-min))
    (search-forward "3")
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t)
          request)
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (_channel body _timeout)
                   (setq request body)
                   '((ok . t) (supported . t) (found . t)))))
        (my/noema-jupyter-cell--introspect "inspect" nil t))
      (should (equal (alist-get 'code request) "int"))
      (should (= (alist-get 'cursorPos request) (length "int"))))))

(ert-deftest my/noema-jupyter-cell-successful-inspect-replaces-stale-error-message ()
  (with-temp-buffer
    (python-mode)
    (insert "# %% id=cell-a\na = 3\n")
    (goto-char (point-min))
    (search-forward "3")
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-source-file "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-language "python")
    (let ((my/noema--ready t)
          shown-message)
      (cl-letf (((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (&rest _)
                   '((ok . t)
                     (supported . t)
                     (found . t)
                     (data . (("text/plain" . "int documentation"))))))
                ((symbol-function 'display-buffer) (lambda (&rest _) t))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq shown-message (apply #'format format-string args)))))
        (my/noema-jupyter-cell-inspect))
      (should (equal shown-message
                     "Noema Jupyter: showing kernel documentation for `int'")))))

(provide 'init-aaronnote-jupyter-notebook-tests)

;;; init-aaronnote-jupyter-notebook-tests.el ends here

(ert-deftest my/noema-jupyter-notebook-refuses-to-save-over-an-unreadable-file ()
  "Saving must not fall back to the opened-at document.

Noema writes execution results straight into the .ipynb while the projection
is open.  Silently reverting to the in-memory copy would write those results
back out as if they had never happened."
  (let ((file (make-temp-file "noema-jupyter-unreadable" nil ".ipynb")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (json-serialize
                     (my/noema-jupyter-notebook-test--document))))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (should my/noema-jupyter-notebook--projection-p)
                  (goto-char (point-max))
                  (insert "\n# %% id=extra\nprint(1)\n")
                  ;; The file is still there, but no longer parseable.  Adopt
                  ;; the new modtime so the save reaches the projection writer
                  ;; instead of stopping at Emacs' supersession prompt.
                  (with-temp-file file (insert "{ this is not json"))
                  (set-visited-file-modtime)
                  (should-error (save-buffer) :type 'user-error)
                  ;; Nothing was written: the damaged file is untouched.
                  (should (equal (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string))
                                 "{ this is not json")))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-file file))))

(ert-deftest my/noema-jupyter-cell-reactivation-keeps-the-original-header-line ()
  "Every reload re-enters the enable branch; only the first may save the header.

Recording the header line again would store this mode's own :eval form as the
\"original\", leaving it installed for good once the mode is turned off."
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local header-line-format "original")
    (cl-letf (((symbol-function 'my/noema-jupyter-cell--ensure-noema-host)
               #'ignore))
      (my/noema-jupyter-cell-mode 1)
      (should (equal my/noema-jupyter-cell--saved-header-line-format
                     "original"))
      ;; A structural mutation reloads the projection and activates again.
      (my/noema-jupyter-cell-mode 1)
      (my/noema-jupyter-cell-mode 1)
      (should (equal my/noema-jupyter-cell--saved-header-line-format
                     "original"))
      (my/noema-jupyter-cell-mode -1)
      (should (equal header-line-format "original")))))

(ert-deftest my/noema-jupyter-notebook-edits-a-malformed-file-as-text ()
  "A parse error here would abort `find-file' itself.

The mode is reached from `auto-mode-alist', so an unhandled `json-parse-error'
leaves the buffer in an unrelated mode showing raw JSON with no explanation."
  (let ((file (make-temp-file "noema-jupyter-malformed" nil ".ipynb")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{ not a notebook"))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (should-not my/noema-jupyter-notebook--projection-p)
                  (should-not (bound-and-true-p my/noema-jupyter-cell-mode))
                  ;; The text is left exactly as it is on disk.
                  (should (equal (buffer-string) "{ not a notebook")))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-file file))))

(ert-deftest my/noema-jupyter-cell-implicit-introspection-backs-off ()
  "A kernel that misses the deadline must not be asked again per keystroke."
  (with-temp-buffer
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (should-not (my/noema-jupyter-cell--introspect-backoff-p))
    (my/noema-jupyter-cell--introspect-failed)
    (should (my/noema-jupyter-cell--introspect-backoff-p))
    ;; Answering again clears it immediately; the backoff is not a fixed
    ;; penalty window the user has to wait out.
    (my/noema-jupyter-cell--introspect-succeeded)
    (should-not (my/noema-jupyter-cell--introspect-backoff-p))))

(ert-deftest my/noema-jupyter-cell-explicit-inspect-ignores-backoff ()
  "Backoff throttles typing-triggered completion, never an explicit request."
  (with-temp-buffer
    (insert "# %% id=cell-a\nanswer\n")
    (goto-char (point-max))
    (setq-local buffer-file-name "/tmp/notebook.ipynb")
    (setq-local my/noema-jupyter-cell-mode t)
    (my/noema-jupyter-cell--introspect-failed)
    (let ((asked nil))
      (cl-letf (((symbol-function 'my/noema--api-call-sync)
                 (lambda (&rest _) (setq asked t) nil))
                ((symbol-function 'my/noema-jupyter-cell--api-sync)
                 (lambda (&rest _) (setq asked t) '((found . t)))))
        (let ((my/noema--ready t))
          (my/noema-jupyter-cell--introspect "inspect" nil t))
        (should asked)))))

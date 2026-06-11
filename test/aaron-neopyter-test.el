;;; aaron-neopyter-test.el --- ERT tests for aaron-neopyter -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q \
;;     -L site-lisp/aaron-neopyter \
;;     -L site-lisp \
;;     -l test/aaron-neopyter-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Add package to load path when running standalone
(let ((root (file-name-directory
             (or load-file-name buffer-file-name default-directory))))
  (dolist (dir '("site-lisp/aaron-neopyter" "site-lisp"))
    (add-to-list 'load-path (expand-file-name dir (concat root "../")))))

(require 'aaron-neopyter-parser)
(require 'aaron-neopyter-rpc)

;;; ──────────────────────────────────────────────────────────────────────
;;; Parser tests
;;; ──────────────────────────────────────────────────────────────────────

(defmacro with-temp-script (content &rest body)
  "Evaluate BODY with a temp buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ,@body))

(ert-deftest anp-parser-empty-buffer ()
  "Empty buffer yields one empty code cell (the implicit first cell)."
  (with-temp-script ""
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (= 1 (length cells)))
      (should (eq 'code (aaron-neopyter-cell-type (car cells))))
      (should (aaron-neopyter-cell-no-separator (car cells))))))

(ert-deftest anp-parser-single-separator ()
  "Buffer starting with a separator."
  (with-temp-script "# %%\nx = 1\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (= 1 (length cells)))
      (let ((c (car cells)))
        (should (eq 'code (aaron-neopyter-cell-type c)))
        (should (string-match-p "x = 1" (aaron-neopyter-cell-text c)))))))

(ert-deftest anp-parser-two-code-cells ()
  "Two code cells separated by # %%."
  (with-temp-script "# %%\na = 1\n# %%\nb = 2\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (= 2 (length cells)))
      (should (eq 'code (aaron-neopyter-cell-type (nth 0 cells))))
      (should (eq 'code (aaron-neopyter-cell-type (nth 1 cells))))
      (should (string-match-p "a = 1" (aaron-neopyter-cell-text (nth 0 cells))))
      (should (string-match-p "b = 2" (aaron-neopyter-cell-text (nth 1 cells)))))))

(ert-deftest anp-parser-markdown-bracket ()
  "# %% [markdown] produces a markdown cell."
  (with-temp-script "# %% [markdown]\n# Hello\n# World\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (= 1 (length cells)))
      (should (eq 'markdown (aaron-neopyter-cell-type (car cells)))))))

(ert-deftest anp-parser-md-shorthand ()
  "# %% [md] is normalised to markdown."
  (with-temp-script "# %% [md]\n# text\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (eq 'markdown (aaron-neopyter-cell-type (car cells)))))))

(ert-deftest anp-parser-raw-cell ()
  "# %% [raw] produces a raw cell."
  (with-temp-script "# %% [raw]\nsome raw text\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (eq 'raw (aaron-neopyter-cell-type (car cells)))))))

(ert-deftest anp-parser-mixed ()
  "Three-cell mixed notebook."
  (with-temp-script "# %%\nimport pandas as pd\n# %% [markdown]\n# ## Title\n# %% [raw]\nraw\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (= 3 (length cells)))
      (should (eq 'code     (aaron-neopyter-cell-type (nth 0 cells))))
      (should (eq 'markdown (aaron-neopyter-cell-type (nth 1 cells))))
      (should (eq 'raw      (aaron-neopyter-cell-type (nth 2 cells)))))))

(ert-deftest anp-parser-index-ordering ()
  "Cell indices are 0-based and contiguous."
  (with-temp-script "# %%\na\n# %%\nb\n# %%\nc\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      (should (= 0 (aaron-neopyter-cell-index (nth 0 cells))))
      (should (= 1 (aaron-neopyter-cell-index (nth 1 cells))))
      (should (= 2 (aaron-neopyter-cell-index (nth 2 cells)))))))

(ert-deftest anp-parser-title-metadata ()
  "Title is parsed correctly from separator."
  (with-temp-script "# %% my-section [markdown]\n# text\n"
    (let* ((cells (aaron-neopyter-parse-buffer))
           (c     (car cells)))
      (should (eq 'markdown (aaron-neopyter-cell-type c)))
      (should (string= "my-section" (aaron-neopyter-cell-title c))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Filename mapper tests
;;; ──────────────────────────────────────────────────────────────────────

;; We test the fallback mapping path in aaron-neopyter-sync directly.
;; The primary path reuses my/jupytext--default-notebook-file which is
;; tested as part of init-jupyter-core.

(ert-deftest anp-sync-notebook-path-fallback ()
  "Fallback mapper: foo.ju.py → foo.ipynb when jupytext helper returns nil."
  (require 'aaron-neopyter-sync)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/foo.ju.py")
    (let ((jupytext-mode nil)
          (my/jupytext-notebook-file nil))
      ;; Mock helper to return nil → forces own mapper
      (cl-letf (((symbol-function 'my/jupytext--default-notebook-file)
                 (lambda () nil)))
        (should (string= "/tmp/foo.ipynb"
                         (aaron-neopyter-sync--notebook-path)))))))

(ert-deftest anp-sync-notebook-path-ju-r ()
  "Fallback mapper: bar.ju.r → bar.ipynb when jupytext helper returns nil."
  (require 'aaron-neopyter-sync)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/bar.ju.r")
    (let ((jupytext-mode nil)
          (my/jupytext-notebook-file nil))
      (cl-letf (((symbol-function 'my/jupytext--default-notebook-file)
                 (lambda () nil)))
        (should (string= "/tmp/bar.ipynb"
                         (aaron-neopyter-sync--notebook-path)))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Point → cell index tests
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest anp-cell-index-at-pos ()
  "Point-to-cell mapping is correct."
  (with-temp-script "# %%\naaa\n# %%\nbbb\n# %%\nccc\n"
    (let ((cells (aaron-neopyter-parse-buffer)))
      ;; Find 'bbb' line and check it maps to index 1
      (goto-char (point-min))
      (search-forward "bbb")
      (should (= 1 (aaron-neopyter-cell-index-at-pos (point) cells)))
      ;; First char → index 0
      (should (= 0 (aaron-neopyter-cell-index-at-pos (point-min) cells))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Cells-to-RPC encoding tests
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest anp-cells-to-rpc-basic ()
  "cells-to-rpc returns a vector of alists with string keys."
  (with-temp-script "# %%\nx = 1\n# %% [markdown]\n# text\n"
    (let* ((cells   (aaron-neopyter-parse-buffer))
           (rpc-vec (aaron-neopyter-cells-to-rpc cells)))
      (should (vectorp rpc-vec))
      (should (= 2 (length rpc-vec)))
      (let ((c0 (aref rpc-vec 0))
            (c1 (aref rpc-vec 1)))
        (should (string= "code"     (alist-get "cell_type" c0 nil nil #'string=)))
        (should (string= "markdown" (alist-get "cell_type" c1 nil nil #'string=)))
        (should (string-match-p "x = 1" (alist-get "source" c0 nil nil #'string=)))))))

(ert-deftest anp-cells-to-rpc-strips-markdown-comment-prefix ()
  "Markdown cells are uncommented when sent to JupyterLab."
  (with-temp-script "# %% [markdown]\n# # Heading\n#\n# - item\nplain line\n"
    (let* ((cells   (aaron-neopyter-parse-buffer))
           (rpc-vec (aaron-neopyter-cells-to-rpc cells))
           (cell    (aref rpc-vec 0)))
      (should (string= "markdown" (alist-get "cell_type" cell nil nil #'string=)))
      (should (string= "# Heading\n\n- item\nplain line"
                       (alist-get "source" cell nil nil #'string=))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; msgpack-rpc encode/decode round-trip tests
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest anp-rpc-encode-decode-request ()
  "Request encodes and decodes correctly through base64+msgpack."
  (let* ((obj    (vector 0 42 "fullSync" (vector)))
         (b64    (aaron-neopyter-rpc--encode obj))
         (result (aaron-neopyter-rpc--decode b64)))
    (should (equal (list 0 42 "fullSync" nil) result))))

(ert-deftest anp-rpc-encode-decode-notification ()
  "Notification round-trips through the codec."
  (let* ((obj    (vector 2 "activeCellChanged" (vector 3)))
         (b64    (aaron-neopyter-rpc--encode obj))
         (result (aaron-neopyter-rpc--decode b64)))
    (should (= 2 (car result)))
    (should (string= "activeCellChanged" (nth 1 result)))))

(ert-deftest anp-rpc-encode-cell-map ()
  "Cell alist encodes as msgpack map and decodes back to alist."
  (let* ((cell   (list (cons "source" "x = 1") (cons "cell_type" "code")))
         (msg    (vector 0 1 "fullSync" (vector (apply #'vector (list cell)))))
         (b64    (aaron-neopyter-rpc--encode msg))
         (result (aaron-neopyter-rpc--decode b64)))
    ;; result is (0 1 "fullSync" [[cell-map]])
    (should (listp result))
    (let* ((params     (nth 3 result))  ; the [[cell-map]] as a list
           (cells-arr  (if (vectorp params) (aref params 0) (car params)))
           (cell-map   (if (vectorp cells-arr) (aref cells-arr 0) (car cells-arr))))
      (should (string= "x = 1" (alist-get "source" cell-map nil nil #'string=))))))

(ert-deftest anp-rpc-b64-unibyte ()
  "b64-to-unibyte produces a unibyte string."
  (let ((b64 (base64-encode-string (unibyte-string 0 1 2 127 128 255) t)))
    (let ((raw (aaron-neopyter-rpc--b64-to-unibyte b64)))
      (should (not (multibyte-string-p raw)))
      (should (= 6 (length raw))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; RPC path conversion tests
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest anp-rpc-path-strips-root ()
  "rpc-path strips the jupyter root when set."
  (require 'aaron-neopyter-sync)
  (let ((aaron-neopyter-jupyter-root "/Users/hc/Documents/AaronNote"))
    (should (string= "project/lab/foo.ipynb"
                     (aaron-neopyter-sync--rpc-path
                      "/Users/hc/Documents/AaronNote/project/lab/foo.ipynb")))))

(ert-deftest anp-rpc-path-passthrough-when-no-root ()
  "rpc-path is a no-op when aaron-neopyter-jupyter-root is nil."
  (require 'aaron-neopyter-sync)
  (let ((aaron-neopyter-jupyter-root nil))
    (should (string= "/Users/hc/Documents/AaronNote/project/lab/foo.ipynb"
                     (aaron-neopyter-sync--rpc-path
                      "/Users/hc/Documents/AaronNote/project/lab/foo.ipynb")))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Debounce / timer tests
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest anp-sync-timer-cancel-on-second-call ()
  "Scheduling sync twice cancels the first timer."
  (require 'aaron-neopyter-sync)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/anp-timer-test.ju.py")
    (setq aaron-neopyter--session (aaron-neopyter--make-session :notebook-path "/tmp/anp.ipynb"))
    (unwind-protect
        (progn
          ;; Use a mock conn that can't actually connect
          (let ((mock-conn (aaron-neopyter--conn-create :host "127.0.0.1" :port 9999)))
            (aaron-neopyter-sync-schedule mock-conn 10.0)
            (let ((first-timer (aaron-neopyter--session-sync-timer aaron-neopyter--session)))
              (should (timerp first-timer))
              (aaron-neopyter-sync-schedule mock-conn 10.0)
              (let ((second-timer (aaron-neopyter--session-sync-timer aaron-neopyter--session)))
                (should (timerp second-timer))
                ;; First timer should be cancelled (not the same object)
                (should (not (eq first-timer second-timer)))
                (cancel-timer second-timer)))))
      (setq aaron-neopyter--session nil))))

(provide 'aaron-neopyter-test)
;;; aaron-neopyter-test.el ends here

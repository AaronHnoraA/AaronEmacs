;;; jupyter-export-live-smoke.el --- Actual nbconvert and Remote file writes -*- lexical-binding: t; -*-
(require 'init-aaronnote-jupyter-files)
(require 'remote-config)
(remote-config-load)
(remote-fs-install)
(let* ((target (getenv "JUPYTER_EXPORT_LIVE_TARGET"))
       (root (or (getenv "JUPYTER_EXPORT_LIVE_ROOT") (make-temp-file "notebook-export-live-" t)))
       (directory (if target (remote-make-file-name target (file-name-as-directory root)) (file-name-as-directory root)))
       (file (concat directory "audit.ipynb"))
       (document (json-parse-string
                  "{\"nbformat\":4,\"nbformat_minor\":5,\"metadata\":{\"kernelspec\":{\"name\":\"python3\",\"display_name\":\"Python 3\",\"language\":\"python\"},\"language_info\":{\"name\":\"python\",\"file_extension\":\".py\",\"nbconvert_exporter\":\"python\"}},\"cells\":[{\"id\":\"m\",\"cell_type\":\"markdown\",\"metadata\":{},\"source\":\"# Export audit\\nStored notebook output below.\"},{\"id\":\"a\",\"cell_type\":\"code\",\"metadata\":{},\"source\":\"raise RuntimeError('EXPORT_MUST_NOT_EXECUTE')\",\"execution_count\":1,\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":\"stored output 42\\n\"}]}]}"
                  :null-object nil :false-object :json-false)))
  (unwind-protect
      (with-temp-buffer
        (make-directory directory t)
        (my/noema-jupyter-notebook--write-raw file document)
        (make-directory (concat directory "images/") t)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert (base64-decode-string "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGP4//8/AAX+Av4N70a4AAAAAElFTkSuQmCC"))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point-min) (point-max) (concat directory "images/dot.png") nil 'silent)))
        (puthash "source" "# Export audit\n![dot](images/dot.png)\nStored notebook output below."
                 (aref (gethash "cells" document) 0))
        (my/noema-jupyter-notebook--write-raw file document)
        (setq buffer-file-name file default-directory directory)
        (my/noema-jupyter-notebook--install-projection document)
        (dolist (format '("script" "html" "pdf"))
          (let ((output (concat directory "result." format)) done failure)
            (my/noema-jupyter-export format output (lambda (error) (setq failure error done t)))
            (let ((deadline (+ (float-time) 120)))
              (while (and (not done) (< (float-time) deadline)) (accept-process-output nil .05)))
            (unless done (error "Export timed out: %s" format))
            (when failure (error "%s" failure))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally output)
              (unless (string-match-p (pcase format ("pdf" "%PDF-") ("html" "stored output 42") (_ "raise RuntimeError")) (buffer-string))
                (error "Invalid %s export" format))
              (when (equal format "html")
                (unless (string-match-p "data:image/png;base64," (buffer-string))
                  (error "Linked image did not get embedded"))))
            (princ (format "PASS real %s export%s, no cell execution\n" format (if target " through Remote storage" ""))))))
    (delete-directory directory t)))

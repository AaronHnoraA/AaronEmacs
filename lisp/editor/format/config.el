;;; editor/format/config.el --- Formatting helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'aaron-funcs)
(require 'cl-lib)
(require 'remote-process)
(require 'subr-x)

(declare-function +current-language-server-backend "tools/lsp/config")
(declare-function +language-server/format-buffer "tools/lsp/config")

(defvar-local +format-on-save-mode nil
  "Whether current buffer should format itself before save.")

(defvar +format-external-rules
  '((sh-mode . ("shfmt" "--filename" :file))
    (bash-ts-mode . ("shfmt" "--filename" :file))
    (c-mode . ("clang-format" :assume-file))
    (c-ts-mode . ("clang-format" :assume-file))
    (c++-mode . ("clang-format" :assume-file))
    (c++-ts-mode . ("clang-format" :assume-file))
    (js-mode . ("prettier" "--stdin-filepath" :file))
    (js-ts-mode . ("prettier" "--stdin-filepath" :file))
    (typescript-mode . ("prettier" "--stdin-filepath" :file))
    (typescript-ts-mode . ("prettier" "--stdin-filepath" :file))
    (tsx-ts-mode . ("prettier" "--stdin-filepath" :file))
    (json-mode . ("prettier" "--stdin-filepath" :file))
    (json-ts-mode . ("prettier" "--stdin-filepath" :file))
    (css-mode . ("prettier" "--stdin-filepath" :file))
    (css-ts-mode . ("prettier" "--stdin-filepath" :file))
    (html-mode . ("prettier" "--stdin-filepath" :file))
    (html-ts-mode . ("prettier" "--stdin-filepath" :file))
    (web-mode . ("prettier" "--stdin-filepath" :file)))
  "Mode to target-side formatter argv; :file denotes the native file path.")

(defvar-local +format--external-process nil
  "External formatter currently running for this buffer.")

(defun +format--external-command ()
  "Return the target-side formatter command for the current file."
  (unless buffer-file-name
    (user-error "External formatting needs a file-backed buffer"))
  (let* ((rule (cl-find-if (lambda (entry) (derived-mode-p (car entry)))
                           +format-external-rules))
         (argv (cdr rule))
         (file (remote-file-local-name buffer-file-name)))
    (unless argv
      (user-error "No external formatter registered for %s" major-mode))
    (unless (remote-executable-find (car argv))
      (user-error "%s is unavailable on this target" (car argv)))
    (mapcar (lambda (arg)
              (pcase arg
                (:file file)
                (:assume-file (concat "--assume-filename=" file))
                (_ arg)))
            argv)))

(defun +format--external-finished
    (process _event buffer tick file mode had-content stdout stderr)
  "Apply PROCESS output to BUFFER if its text and context are unchanged."
  (when (memq (process-status process) '(exit signal))
    (unwind-protect
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (eq process +format--external-process)
              (setq +format--external-process nil)
              (cond
               ((not (zerop (process-exit-status process)))
                (message "External formatter exited %d: %s"
                         (process-exit-status process)
                         (with-current-buffer stderr
                           (string-trim (buffer-string)))))
               ((and had-content
                     (with-current-buffer stdout (= (buffer-size) 0)))
                (message "External formatting skipped: no output"))
               ((or (/= tick (buffer-chars-modified-tick))
                    (not (equal file buffer-file-name))
                    (not (eq mode major-mode))
                    buffer-read-only)
                (message "External formatting skipped: buffer changed"))
               (t
                (let ((position (point)))
                  (save-restriction
                    (widen)
                    (replace-region-contents
                     (point-min) (point-max) stdout 0.5)
                    (goto-char (min position (point-max)))))
                (message "External formatting finished"))))))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (when (buffer-live-p stderr) (kill-buffer stderr)))))

(defun +format/external-buffer ()
  "Format the current file asynchronously with a target-side CLI."
  (interactive)
  (let* ((command (+format--external-command))
         (buffer (current-buffer))
         (tick (buffer-chars-modified-tick))
         (file buffer-file-name)
         (mode major-mode)
         (source (save-restriction
                   (widen)
                   (buffer-substring-no-properties (point-min) (point-max))))
         (had-content (not (string-empty-p source)))
         (stdout (generate-new-buffer " *external-format-stdout*"))
         (stderr (generate-new-buffer " *external-format-stderr*"))
         process)
    (when (process-live-p +format--external-process)
      (let ((old +format--external-process))
        (setq +format--external-process nil)
        (delete-process old)))
    (condition-case err
        (progn
          (setq process
                (remote-make-process
                 :name "external-formatter" :buffer stdout :stderr stderr
                 :command command :remote-adapter "process"
                 :remote-directory default-directory
                 :connection-type 'pipe :coding 'utf-8-unix :noquery t
                 :sentinel
                 (lambda (finished event)
                   (+format--external-finished
                    finished event buffer tick file mode had-content
                    stdout stderr))))
          (setq +format--external-process process)
          (process-send-string process source)
          (process-send-eof process)
          process)
      (error
       (setq +format--external-process nil)
       (when (process-live-p process) (delete-process process))
       (when (buffer-live-p stdout) (kill-buffer stdout))
       (when (buffer-live-p stderr) (kill-buffer stderr))
       (signal (car err) (cdr err))))))

(defun +format/buffer-dwim ()
  "Format with the active language server, or an external formatter."
  (interactive)
  (if (and (fboundp '+current-language-server-backend)
           (+current-language-server-backend))
      (+language-server/format-buffer)
    (+format/external-buffer)))

(declare-function lsp-format-region "lsp-mode" (s e))
(declare-function lsp-feature? "lsp-mode" (method))

(defun +format/region-or-buffer ()
  "Format the selection, or the whole buffer when nothing is selected.
From Doom's `+format/region-or-buffer'.  The selection goes to the
language server's range formatting, which not every server offers."
  (interactive)
  (if (not (use-region-p))
      (+format/buffer-dwim)
    (unless (and (fboundp '+current-language-server-backend)
                 (+current-language-server-backend))
      (user-error "No active language server in the current buffer"))
    (unless (lsp-feature? "textDocument/rangeFormatting")
      (user-error "The language server cannot format a selection"))
    (let ((beg (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (lsp-format-region beg end))))

(defun +format-buffer-maybe ()
  "Format the current buffer when `+format-on-save-mode' is enabled."
  (when (and +format-on-save-mode
             (fboundp '+current-language-server-backend)
             (+current-language-server-backend))
    (condition-case err
        (+language-server/format-buffer)
      (error
       (message "Format-on-save skipped: %s" (error-message-string err))))))

(define-minor-mode +format-on-save-mode
  "Buffer-local format-on-save toggle."
  :init-value nil
  :lighter " FmtSave"
  (if +format-on-save-mode
      (add-hook 'before-save-hook #'+format-buffer-maybe nil t)
    (remove-hook 'before-save-hook #'+format-buffer-maybe t)))

(defun +format/toggle-on-save ()
  "Toggle buffer-local format-on-save."
  (interactive)
  (if +format-on-save-mode
      (+format-on-save-mode -1)
    (+format-on-save-mode 1))
  (message "Format-on-save %s" (if +format-on-save-mode "enabled" "disabled")))

(use-package ws-butler
  :ensure t
  :hook (after-init . ws-butler-global-mode)
  :config
  (dolist (mode '(special-mode
                  comint-mode
                  term-mode
                  eshell-mode
                  diff-mode))
    (add-to-list 'ws-butler-global-exempt-modes mode)))

(define-leader-key!
  "c F" '(:def +format/toggle-on-save :which-key "format on save"))

(provide 'editor/format/config)
;;; editor/format/config.el ends here

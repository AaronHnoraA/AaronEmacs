;;; lsp-live-smoke.el --- Real local language-server smoke checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after init.el:
;;   emacs ... -l test/lsp-live-smoke.el -f my/lsp-live-smoke-batch
;;
;; Unlike the isolated ERT suites, this check starts the installed clangd,
;; Python language server and JDTLS against disposable projects.  It is kept
;; out of the default health target because those system tools are optional.

;;; Code:

(require 'cl-lib)
(require 'init-lsp)
(require 'imenu)
(require 'seq)

(defconst my/lsp-live-smoke-specs
  '((c
     :file "main.c"
     :mode c-mode
     :content "int main(void) { return 0; }\n"
     :marker "compile_commands.json"
     :marker-content "[]\n"
     :server my-clangd)
    (python
     :file "main.py"
     :mode python-mode
     :content "value: int = 1\n"
     :marker "pyproject.toml"
     :marker-content "[project]\nname = \"lsp-smoke\"\nversion = \"0.0.0\"\n"
     :server my-python)
    (java
     :file "src/main/java/Smoke.java"
     :mode java-mode
     :content "public class Smoke { public static void main(String[] args) {} }\n"
     :marker "pom.xml"
     :marker-content "<project><modelVersion>4.0.0</modelVersion><groupId>test</groupId><artifactId>smoke</artifactId><version>1</version></project>\n"
     :server jdtls))
  "Disposable projects used by `my/lsp-live-smoke-batch'.")

(defun my/lsp-live-smoke--write (file content)
  "Write CONTENT to FILE, creating its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(defun my/lsp-live-smoke--initialized-p ()
  "Return non-nil when the current buffer owns an initialized workspace."
  (and (bound-and-true-p lsp-managed-mode)
       (seq-some
        (lambda (workspace)
          (eq (ignore-errors (lsp--workspace-status workspace)) 'initialized))
        (ignore-errors (lsp-workspaces)))))

(defun my/lsp-live-smoke--wait (seconds)
  "Wait up to SECONDS for the current language server to initialize."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (my/lsp-live-smoke--initialized-p))
                (< (float-time) deadline))
      (sit-for 0.1))
    (my/lsp-live-smoke--initialized-p)))

(defun my/lsp-live-smoke--typed-imenu (index)
  "Return flattened (LABEL . SYMBOL-KIND) pairs from Imenu INDEX."
  (let (result)
    (dolist (item index (nreverse result))
      (when (and (consp item) (stringp (car item)))
        (let ((label (car item)))
          (push (cons (substring-no-properties label)
                      (and (> (length label) 0)
                           (get-text-property 0 'my/lsp-symbol-kind label)))
                result)
          (when (imenu--subalist-p item)
            (setq result
                  (nconc (nreverse (my/lsp-live-smoke--typed-imenu (cdr item)))
                         result))))))))

(defun my/lsp-live-smoke--run-one (spec)
  "Run one live language-server smoke SPEC and return its result plist."
  (let* ((language (car spec))
         (properties (cdr spec))
         (directory (make-temp-file (format "lsp-%s-smoke-" language) t))
         (file (expand-file-name (plist-get properties :file) directory))
         (marker (expand-file-name (plist-get properties :marker) directory))
         buffer
         result)
    (unwind-protect
        (progn
          (my/lsp-live-smoke--write file (plist-get properties :content))
          (my/lsp-live-smoke--write marker
                                    (plist-get properties :marker-content))
          (setq buffer (find-file-noselect file))
          ;; `lsp-deferred' intentionally waits for a visible source buffer.
          ;; Show the disposable buffer in the selected batch window so this
          ;; smoke exercises the same startup path as an interactive visit.
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (funcall (plist-get properties :mode))
            (setq-local lsp-auto-guess-root t
                        lsp-guess-root-without-session t
                        my/language-server--manual-start t)
            ;; Batch frames are not considered visible by every Emacs build;
            ;; the modified flag is the other supported `lsp-deferred'
            ;; visibility signal.
            (set-buffer-modified-p t)
            (my/language-server-ensure)
            (when (and (bound-and-true-p lsp--buffer-deferred)
                       (fboundp 'lsp--init-if-visible))
              (lsp--init-if-visible))
            (let* ((ok (my/lsp-live-smoke--wait
                        (if (eq language 'java) 120 30)))
                   (imenu-result
                    (and ok
                         (condition-case error
                             (list :symbols
                                   (my/lsp-live-smoke--typed-imenu
                                    (imenu--make-index-alist t)))
                           (error
                            (list :error (error-message-string error)))))))
              (setq result
                    (list
                     :language language
                     :ok ok
                     :managed (bound-and-true-p lsp-managed-mode)
                     :runtime-state my/language-server-runtime-state
                     :waiting-runtime my/language-server--waiting-for-runtime
                     :waiting-direnv my/lsp-mode--waiting-for-direnv
                     :imenu imenu-result
                     :contact (and (not ok)
                                   (my/language-server-contact-available-p))
                     :workspaces
                     (mapcar
                      (lambda (workspace)
                        (list
                         (my/language-server--lsp-workspace-id workspace)
                         (ignore-errors (lsp--workspace-status workspace))))
                      (ignore-errors (lsp-workspaces)))
                     :messages
                     (and
                      (not ok)
                      (with-current-buffer "*Messages*"
                        (buffer-substring-no-properties
                         (max (point-min) (- (point-max) 4000))
                         (point-max)))))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (let* ((workspaces
                  (copy-sequence (ignore-errors (lsp-workspaces))))
                 (processes
                  (delq nil
                        (mapcar
                         (lambda (workspace)
                           (ignore-errors (lsp--workspace-cmd-proc workspace)))
                         workspaces))))
            (dolist (workspace workspaces)
              (ignore-errors
                (my/lsp-mode-shutdown-workspace workspace 'live-smoke)))
            (let ((deadline (+ (float-time) 3)))
              (while (and (seq-some #'process-live-p processes)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))))
        (kill-buffer buffer))
      (delete-directory directory t))
    result))

;;;###autoload
(defun my/lsp-live-smoke-batch ()
  "Start real local servers for C, Python and Java, then exit for batch use."
  (let* ((requested
          (and-let* ((value (getenv "LSP_SMOKE_LANGUAGES")))
            (mapcar #'intern (split-string value "," t "[[:space:]]+"))))
         (specs
          (if requested
              (seq-filter (lambda (spec) (memq (car spec) requested))
                          my/lsp-live-smoke-specs)
            my/lsp-live-smoke-specs))
         (results (mapcar #'my/lsp-live-smoke--run-one specs)))
    (dolist (result results)
      (princ (format "%S\n" result)))
    (kill-emacs (if (seq-every-p (lambda (result) (plist-get result :ok)) results)
                    0
                  1))))

(provide 'lsp-live-smoke)
;;; lsp-live-smoke.el ends here

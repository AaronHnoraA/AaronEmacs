;;; init-lean.el --- Lean4 config with upstream lsp-mode integration -*- lexical-binding: t -*-

;;; Commentary:
;; Lean 4 works best with the upstream `lean4-mode' + `lsp-mode' integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(my/package-ensure-installed-list '(dash magit-section lsp-mode))
(my/package-ensure-vc 'lean4-mode "https://github.com/leanprover-community/lean4-mode.git")

(defvar lean4-mode-hook)
(declare-function lean4-lake-find-dir "lean4-lake")
(declare-function lean4-get-executable "lean4-util" (exe-name))
(declare-function lsp-workspace-root "lsp-mode" (&optional path))
(declare-function my/language-server-executable-find "init-lsp" (program))
(declare-function my/project-current-root "init-project")
(declare-function my/symbols-make-file-line-candidate "init-symbols"
                  (root file line match))
(declare-function my/symbols-read-file-line-candidates "init-symbols"
                  (candidates &optional prompt history category))
(declare-function my/symbols-register-project-fallback "init-symbols"
                  (mode function))

(defvar my/lean-project-symbol-history nil)
(defvar lean4-executable-name)
(defvar lean4-show-file-progress)
(defvar lsp-semantic-tokens-enable)
(defvar-local lean4-fringe-delay-timer nil)
(defvar-local lean4-rootdir nil)
(defvar-local my/lean4-remote-ui-error-reported nil)

(defun my/lean4-remote-rootdir ()
  "Return Lean's remote executable root as a plain Unix path."
  (when-let* ((executable (ignore-errors
                            (my/language-server-executable-find
                             lean4-executable-name)))
              ((stringp executable))
              (rootdir (file-name-directory
                        (directory-file-name
                         (file-name-directory executable)))))
    (file-name-as-directory rootdir)))

(defconst my/lean-project-symbol-rg-regexp
  (concat
   "^[[:space:]]*"
   "(?:@[[:alnum:]_.]+[[:space:]]+)*"
   "(?:(?:private|protected|noncomputable|unsafe|partial|scoped|local)"
   "[[:space:]]+)*"
   "(?:class[[:space:]]+inductive|inductive|instance|structure|class|theorem"
   "|axiom|lemma|definition|def|constant|abbrev|opaque)\\\\b")
  "Ripgrep regexp used to discover top-level Lean declarations.")

(defun my/lean-root-dir-p (dir)
  "Return non-nil when DIR looks like a Lean project root."
  (or (file-exists-p (expand-file-name "lakefile.lean" dir))
      (file-exists-p (expand-file-name "lakefile.toml" dir))
      (file-exists-p (expand-file-name "lean-toolchain" dir))))

(defun my/lean-project-root ()
  "Return the current Lean project root."
  (or (and (fboundp 'lsp-workspace-root)
           (ignore-errors (lsp-workspace-root)))
      (and (fboundp 'lean4-lake-find-dir)
           (ignore-errors (lean4-lake-find-dir)))
      (when-let* ((file (or buffer-file-name default-directory)))
        (locate-dominating-file file #'my/lean-root-dir-p))
      (and (fboundp 'my/project-current-root)
           (my/project-current-root))
      default-directory))

(defun my/lean--declaration-kind-and-name (text)
  "Extract declaration kind and name from Lean declaration TEXT."
  (when (string-match
         (rx string-start
             (* (any " \\t"))
             (* (seq "@" (+ (not (any " \\t\\n\\r"))) (+ blank)))
             (* (seq (or "private"
                         "protected"
                         "noncomputable"
                         "unsafe"
                         "partial"
                         "scoped"
                         "local")
                     (+ blank)))
             (group (or (seq "class" (+ blank) "inductive")
                        "inductive"
                        "instance"
                        "structure"
                        "class"
                        "theorem"
                        "axiom"
                        "lemma"
                        "definition"
                        "def"
                        "constant"
                        "abbrev"
                        "opaque"))
             (+ blank)
             (group (+ (not (any " \\t\\n\\r:={([,")))))
         text)
    (cons (match-string 1 text)
          (match-string 2 text))))

(defun my/lean--project-symbol-lines (root)
  "Return ripgrep declaration hits under Lean project ROOT."
  (when-let* ((rg (executable-find "rg")))
    (let ((default-directory root))
      (condition-case nil
          (process-lines
           rg
           "--color" "never"
           "--line-number"
           "--no-heading"
           "--glob" "*.lean"
           "-e" my/lean-project-symbol-rg-regexp
           ".")
        (error nil)))))

(defun my/lean--project-symbol-candidates ()
  "Return file-line candidates for Lean declarations in the current project."
  (let ((root (file-name-as-directory (expand-file-name (my/lean-project-root)))))
    (cl-loop for hit in (my/lean--project-symbol-lines root)
             when (string-match "\\\\`\\\\([^:]+\\\\):\\\\([0-9]+\\\\):\\\\(.*\\\\)\\\\'" hit)
             for relative-file = (match-string 1 hit)
             for line = (string-to-number (match-string 2 hit))
             for text = (string-trim (match-string 3 hit))
             for kind-and-name = (my/lean--declaration-kind-and-name text)
             for summary = (if kind-and-name
                               (format "%s %s"
                                       (car kind-and-name)
                                       (cdr kind-and-name))
                             text)
             collect (my/symbols-make-file-line-candidate
                      root
                      (expand-file-name relative-file root)
                      line
                      summary))))

(defun my/lean-project-symbols-fallback ()
  "Fallback project symbol search for Lean projects."
  (my/symbols-read-file-line-candidates
   (my/lean--project-symbol-candidates)
   "Lean project symbol: "
   'my/lean-project-symbol-history
   'my-lean-project-symbol))

(with-eval-after-load 'magit-mode
  ;; `magit-region-highlight-hook' may reference
  ;; `magit-diff-update-hunk-region' before `magit-diff' is loaded.
  (require 'magit-diff nil t))

(defvar magit-region-highlight-hook)

(defun my/lean4-info-buffer-setup (&rest _)
  "Adjust the Lean goal buffer after it is created or shown."
  (let ((buffer (get-buffer "*Lean Goal*")))
    (when buffer
      (with-current-buffer buffer
        ;; Lean's info buffer uses `magit-section-mode' for rendering, but it
        ;; is not a diff buffer and does not need Magit's diff-specific region
        ;; highlighting hook.
        (setq-local magit-region-highlight-hook nil)))))

(with-eval-after-load 'lean4-info
  (advice-add 'lean4-ensure-info-buffer :after #'my/lean4-info-buffer-setup)
  (advice-add 'lean4-toggle-info-buffer :after #'my/lean4-info-buffer-setup))

(defun my/lean4-setup-remote-rootdir ()
  "Make `lean4-mode' resolve Lean toolchain paths correctly over TRAMP."
  (when (file-remote-p default-directory)
    (when-let* ((rootdir (my/lean4-remote-rootdir)))
      (setq-local lean4-rootdir rootdir))))

(defun my/lean4-disable-remote-ui ()
  "Disable Lean UI features that are unstable over TRAMP."
  (when (file-remote-p default-directory)
    (setq-local lean4-show-file-progress nil
                lsp-semantic-tokens-enable nil)
    (remove-hook 'post-command-hook #'lean4-info-buffer-redisplay-debounced t)
    (remove-hook 'flycheck-after-syntax-check-hook
                 #'lean4-info-buffer-redisplay-debounced
                 t)
    (remove-hook 'lsp-on-idle-hook #'lean4-info-buffer-refresh t)
    (when (timerp lean4-fringe-delay-timer)
      (cancel-timer lean4-fringe-delay-timer)
      (setq-local lean4-fringe-delay-timer nil))))

(defun my/lean4-report-remote-ui-error (error)
  "Report remote Lean UI ERROR only once per buffer."
  (unless my/lean4-remote-ui-error-reported
    (setq-local my/lean4-remote-ui-error-reported t)
    (message "Lean remote UI disabled after error: %s"
             (error-message-string error))))

(use-package lean4-mode
  :init
  (setq lean4-mode-hook nil)
  (my/register-lsp-mode-preference 'lean4-mode)
  (add-hook 'lean4-mode-hook #'my/lean4-setup-remote-rootdir)
  (add-hook 'lean4-mode-hook #'my/lean4-disable-remote-ui)
  (add-hook 'lean4-mode-hook #'my/lsp-mode-ensure)
  :mode ("\\\\.lean\\\\'" . lean4-mode))

(with-eval-after-load 'lean4-mode
  (define-advice lean4--server-cmd (:around (fn) my/lean4-remote-server-cmd)
    "Avoid synchronous version probes when starting Lean over TRAMP."
    (if (file-remote-p default-directory)
        (if (locate-dominating-file default-directory #'my/lean-root-dir-p)
            (list "lake" "serve")
          (list lean4-executable-name "--server"))
      (funcall fn))))

(with-eval-after-load 'lean4-fringe
  (define-advice lean4-fringe-update-progress-overlays
      (:around (fn) my/lean4-remote-safe-progress)
    "Ignore invalid remote progress overlays instead of wedging the session."
    (if (file-remote-p default-directory)
        (condition-case err
            (funcall fn)
          (error
           (my/lean4-disable-remote-ui)
           (my/lean4-report-remote-ui-error err)))
      (funcall fn))))

(with-eval-after-load 'lean4-info
  (define-advice lean4-info-buffer-redisplay
      (:around (fn) my/lean4-remote-safe-info)
    "Ignore invalid remote Lean goal refreshes instead of wedging the session."
    (if (file-remote-p default-directory)
        (condition-case err
            (funcall fn)
          (error
           (my/lean4-disable-remote-ui)
           (my/lean4-report-remote-ui-error err)))
      (funcall fn))))

(with-eval-after-load 'init-symbols
  (my/symbols-register-project-fallback 'lean4-mode
                                        #'my/lean-project-symbols-fallback))


(provide 'init-lean)

;;; init-lean.el ends here

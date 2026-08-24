;;; lsp-remote-live-smoke.el --- Real TRAMP/Remote lsp-mode smoke -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in because this test connects to a real SSH target.  Every target-side
;; mutation is confined to a fresh /tmp directory and removed on exit.
;;
;;   REMOTE_LSP_E2E=1 REMOTE_LSP_E2E_TARGET=target make lsp-remote-live-smoke

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'init-lsp)
(require 'remote-config)
(require 'remote-framework)
(require 'seq)
(load (expand-file-name "test/lsp-live-smoke.el" user-emacs-directory)
      nil 'nomessage)

(defun my/lsp-remote-live-smoke--target ()
  "Return the explicitly requested real Remote target."
  (remote-config-load)
  (let ((requested
         (or (getenv "REMOTE_LSP_E2E_TARGET")
             (getenv "REMOTE_E2E_TARGET"))))
    (unless requested
      (error "Set REMOTE_LSP_E2E_TARGET to a configured SSH target"))
    (or (remote-get-target requested)
        (seq-find
         (lambda (target)
           (equal (remote-target-label target) requested))
         (hash-table-values remote-targets))
        (error "Unknown Remote target: %s" requested))))

(defun my/lsp-remote-live-smoke--write (file content)
  "Write CONTENT to logical remote FILE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil file nil 'silent)))

(defun my/lsp-remote-live-smoke--wait-for-diagnostic (seconds)
  "Wait up to SECONDS for a Flymake diagnostic in the current buffer."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (null (flymake-diagnostics))
                (< (float-time) deadline))
      (sit-for 0.1))
    (flymake-diagnostics)))

(defconst my/lsp-remote-live-smoke-core-methods
  '("textDocument/completion"
    "textDocument/hover"
    "textDocument/definition"
    "textDocument/references"
    "textDocument/documentSymbol"
    "textDocument/rename"
    "textDocument/formatting"
    "textDocument/codeAction")
  "LSP methods whose advertised state is reported by the parity smoke.")

(defun my/lsp-remote-live-smoke--capabilities ()
  "Return advertised core and viewport-sensitive LSP capabilities."
  (mapcar
   (lambda (method)
     (cons method (and (ignore-errors (lsp-feature? method)) t)))
   (append my/lsp-remote-live-smoke-core-methods
           '("textDocument/codeLens" "textDocument/inlayHint"))))

(defun my/lsp-remote-live-smoke--watches-below (root)
  "Return Remote watcher summaries belonging below logical ROOT."
  (seq-filter
   (lambda (summary)
     (let ((file (plist-get summary :file)))
       (and (stringp file)
            (or (equal file (directory-file-name root))
                (string-prefix-p root file)))))
   (remote-file-watch-list)))

(defun my/lsp-remote-live-smoke--watch-probe (root)
  "Create one Remote watch below ROOT and prove that an event is delivered."
  (let* ((probe (expand-file-name ".emacs-remote-watch-probe" root))
         (events nil)
         (remote-current-adapter-id "language-server")
         (remote-file-watch-workspace (remote-workspace-for-path root))
         descriptor watch physical valid-before)
    (unwind-protect
        (progn
          (setq descriptor
                (file-notify-add-watch
                 root '(change attribute-change)
                 (lambda (event) (push event events))))
          (setq watch (remote-get-file-watch descriptor)
                physical
                (and watch
                     (remote-file-watch-physical-descriptor watch)))
          ;; TRAMP returns the process descriptor before the remote
          ;; inotifywait has necessarily installed its kernel watch.  Wait
          ;; for the public logical descriptor before creating the event.
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (file-notify-valid-p descriptor))
                        (< (float-time) deadline))
              (sit-for 0.1)))
          (setq valid-before (and (file-notify-valid-p descriptor) t))
          (write-region "watch" nil probe nil 'silent)
          (let ((deadline (+ (float-time) 5)))
            (while (and (null events) (< (float-time) deadline))
              (sit-for 0.1)))
          (list :valid valid-before
                :registered (and (gethash descriptor file-notify-descriptors) t)
                :handler-valid
                (and
                 (remote-fs-handle-file-notify-valid-p descriptor)
                 t)
                :state (and watch (remote-file-watch-state watch))
                :physical (and physical (processp physical))
                :physical-status
                (and (processp physical) (process-status physical))
                :physical-valid
                (and physical (file-notify-valid-p physical) t)
                :event (and events (cadar events))))
      (when descriptor
        (ignore-errors (file-notify-rm-watch descriptor)))
      (when (file-exists-p probe)
        (delete-file probe)))))

(defconst my/lsp-remote-live-smoke-specs
  '((c
     :file "main.c"
     :mode c-mode
     :content "int main(void) { return missing_symbol; }\n"
     :marker "compile_commands.json"
     :marker-content "[]\n"
     :server my-clangd
     :timeout 45)
    (python
     :file "main.py"
     :mode python-mode
     ;; This is diagnosed by both Pyright and the lightweight pylsp/pyflakes
     ;; installation commonly present on teaching or shared SSH targets.
     :content
     "def greet(name: str) -> str:\n    return name + missing_name\n"
     :marker "pyproject.toml"
     :marker-content
     "[project]\nname = \"remote-lsp-smoke\"\nversion = \"0.0.0\"\n"
     :server my-python
     :timeout 60)
    (java
     :file "src/main/java/Smoke.java"
     :mode java-mode
     :content
     "public class Smoke { MissingType value; public static void main(String[] args) {} }\n"
     :marker "pom.xml"
     :marker-content
     "<project><modelVersion>4.0.0</modelVersion><groupId>test</groupId><artifactId>remote-smoke</artifactId><version>1</version></project>\n"
     :server jdtls
     :timeout 180))
  "Real target projects used by the Remote lsp-mode parity smoke.")

(defun my/lsp-remote-live-smoke--run-one (target spec)
  "Run language-server SPEC through a physical TRAMP buffer on TARGET."
  (let* ((language (car spec))
         (properties (cdr spec))
         (target-id (remote-target-id target))
         (bootstrap-root (remote-make-file-name target-id "/tmp/"))
         (bootstrap-context (remote-context bootstrap-root))
         (native-directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-lsp-e2e.XXXXXX")
            :context bootstrap-context :adapter "language-server" :check t)))
         (logical-directory
          (file-name-as-directory
           (remote-make-file-name target-id native-directory)))
         ;; Opening the source through this physical spelling is intentional:
         ;; it proves ordinary find-file/TRAMP buffers enter the same Remote
         ;; LSP process layer instead of requiring users to visit /fs: names.
         (physical-directory
          (file-name-as-directory
           (remote-project-file-name
            logical-directory nil 'file-read "emacs-file")))
         (logical-file
          (expand-file-name (plist-get properties :file) logical-directory))
         (physical-file
          (expand-file-name (plist-get properties :file) physical-directory))
         buffer result)
    (unwind-protect
        (progn
          (my/lsp-remote-live-smoke--write
           logical-file (plist-get properties :content))
          (my/lsp-remote-live-smoke--write
           (expand-file-name
            (plist-get properties :marker) logical-directory)
           (plist-get properties :marker-content))
          (my/lsp-remote-live-smoke--write
           (expand-file-name ".projectile" logical-directory) "")
          (setq buffer (find-file-noselect physical-file))
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (funcall (plist-get properties :mode))
            (setq-local lsp-auto-guess-root t
                        lsp-guess-root-without-session t
                        my/language-server--manual-start t)
            (set-buffer-modified-p t)
            (my/language-server-ensure)
            (when (and (bound-and-true-p lsp--buffer-deferred)
                       (fboundp 'lsp--init-if-visible))
              (lsp--init-if-visible))
            (let* ((ok
                    (my/lsp-live-smoke--wait
                     (plist-get properties :timeout)))
                   (workspace (car (ignore-errors (lsp-workspaces))))
                   (process
                    (and workspace
                         (ignore-errors
                           (lsp--workspace-cmd-proc workspace))))
                   (route (and process (process-get process 'remote-route)))
                   (diagnostics
                    (and ok
                         (my/lsp-remote-live-smoke--wait-for-diagnostic 10)))
                   (symbols
                    (and ok
                         (my/lsp-live-smoke--typed-imenu
                          (imenu--make-index-alist t))))
                   (capabilities
                    (and ok
                         (my/lsp-remote-live-smoke--capabilities)))
                   (watches
                    (and ok
                         (my/lsp-remote-live-smoke--watches-below
                          logical-directory)))
                   (watch-probe
                    (and ok
                         (my/lsp-remote-live-smoke--watch-probe
                          logical-directory)))
                   (remote-process-p
                    (and process
                         (remote-context-p
                          (process-get process 'remote-context))))
                   (completion-p
                    (and (memq #'lsp-completion-at-point
                               completion-at-point-functions)
                         t))
                   (core-capabilities-p
                    (and
                     capabilities
                     (seq-every-p
                      (lambda (method)
                        (cdr (assoc method capabilities)))
                      my/lsp-remote-live-smoke-core-methods)))
                   (watches-valid-p
                    (or
                     (null watches)
                     (seq-every-p
                      (lambda (summary)
                        (file-notify-valid-p
                         (plist-get summary :descriptor)))
                      watches)))
                   (parity-ok
                    (and
                     ok route remote-process-p diagnostics symbols
                     completion-p core-capabilities-p
                     lsp-enable-file-watchers watches-valid-p
                     (plist-get watch-probe :valid)
                     (plist-get watch-probe :event)
                     (eq
                      (my/language-server--lsp-workspace-id workspace)
                      (plist-get properties :server)))))
              (setq result
                    (list
                     :language language
                     :ok (and parity-ok t)
                     :managed (bound-and-true-p lsp-managed-mode)
                     :source physical-file
                     :logical-root logical-directory
                     :server
                     (and workspace
                          (my/language-server--lsp-workspace-id workspace))
                     :workspace-state
                     (and workspace (lsp--workspace-status workspace))
                     :route
                     (and route
                          (list (remote-route-target-id route)
                                (remote-route-link-plugin-id route)))
                     :remote-process
                     remote-process-p
                     :flymake (and (bound-and-true-p flymake-mode) t)
                     :diagnostics (length diagnostics)
                     :symbols symbols
                     :completion
                     completion-p
                     :capabilities capabilities
                     :watchers-enabled (and lsp-enable-file-watchers t)
                     :remote-watches (length watches)
                     :remote-watches-valid
                     watches-valid-p
                     :remote-watch-states
                     (mapcar
                     (lambda (summary)
                        (let* ((descriptor
                                (plist-get summary :descriptor))
                               (watch (remote-get-file-watch descriptor))
                               (physical
                                (and
                                 watch
                                 (remote-file-watch-physical-descriptor
                                  watch))))
                          (list
                           (plist-get summary :state)
                           (file-notify-valid-p descriptor)
                           (and physical
                                (file-notify-valid-p physical))
                           (and (processp physical)
                                (process-status physical))
                           (plist-get summary :file))))
                      watches)
                     :watch-probe watch-probe
                     :messages
                     (and
                      (not ok)
                      (with-current-buffer "*Messages*"
                        (buffer-substring-no-properties
                         (max (point-min) (- (point-max) 6000))
                         (point-max)))))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (dolist (workspace (copy-sequence (ignore-errors (lsp-workspaces))))
            (ignore-errors
              (my/lsp-mode-shutdown-workspace
               workspace 'remote-live-smoke))))
        (kill-buffer buffer))
      (when (string-match-p
             "\\`/tmp/emacs-lsp-e2e\\.[[:alnum:]]+\\'"
             native-directory)
        (ignore-errors
          (remote-exec
           "rm" :args (list "-rf" native-directory)
           :context bootstrap-context :adapter "language-server" :check t))))
    result))

;;;###autoload
(defun my/lsp-remote-live-smoke-batch ()
  "Run real TRAMP/Remote LSP checks and exit with their status."
  (unless (equal (getenv "REMOTE_LSP_E2E") "1")
    (error "Set REMOTE_LSP_E2E=1 to run real target-side LSP checks"))
  (let* ((target (my/lsp-remote-live-smoke--target))
         (requested
          (and-let* ((value (getenv "REMOTE_LSP_E2E_LANGUAGES")))
            (mapcar #'intern (split-string value "," t "[[:space:]]+"))))
         (specs
          (if requested
              (seq-filter
               (lambda (spec) (memq (car spec) requested))
               my/lsp-remote-live-smoke-specs)
            my/lsp-remote-live-smoke-specs))
         (results
          (mapcar
           (lambda (spec)
             (my/lsp-remote-live-smoke--run-one target spec))
           specs)))
    (dolist (result results)
      (princ (format "%S\n" result)))
    (kill-emacs
     (if (seq-every-p (lambda (result) (plist-get result :ok)) results)
         0
       1))))

(provide 'lsp-remote-live-smoke)
;;; lsp-remote-live-smoke.el ends here

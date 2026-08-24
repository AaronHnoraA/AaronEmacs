;;; init-aaronnote-jupyter-lsp.el --- Kernel-aware LSP runtimes -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve a Noema cell's selected kernelspec through the same Remote target
;; that owns the kernel.  The language server remains a normal target tool;
;; only its interpreter, import paths, and environment follow the kernel.

;;; Code:

(require 'cl-lib)
(require 'config)
(require 'init-aaronnote-jupyter-cell)
(require 'init-aaronnote-jupyter-runtime)
(require 'init-lsp-runtime)
(require 'json)
(require 'remote-core)
(require 'remote-environment)
(require 'remote-process)
(require 'subr-x)

(config-defvar my/noema-jupyter-cell-lsp-idle-timeout 600
  "Seconds to keep an unused kernel-specific language server warm."
  :type 'integer
  :group 'my/noema)

(defvar-local my/noema-jupyter-cell--runtime-probe nil)

(defvar-local my/noema-jupyter-cell--runtime-probe-timer nil
  "Deadline timer for this buffer's in-flight runtime probe.")

(defcustom my/noema-jupyter-cell-lsp-probe-timeout 30
  "Seconds to wait for a kernel runtime probe before giving up.
`remote-exec-async\=' has no deadline of its own, so an unreachable target
would otherwise leave the buffer waiting for a language server forever, with
its status stuck on \"preparing\" and no way to find out why."
  :type 'number
  :group 'my/noema)

(defconst my/noema-jupyter-cell--python-runtime-probe
  (concat
   "import json,os,sys;"
   "print(json.dumps({"
   "'executable':os.path.realpath(sys.executable),"
   "'prefix':os.path.realpath(sys.prefix),"
   "'base_prefix':os.path.realpath(getattr(sys,'base_prefix',sys.prefix)),"
   "'path':[os.path.realpath(p) for p in sys.path "
   "if isinstance(p,str) and p and os.path.isabs(p)],"
   "'version':sys.version.split()[0]}))")
  "Python expression used to identify a kernel's effective runtime.")

(declare-function lsp-disconnect "lsp-mode" ())
(declare-function lsp-workspaces "lsp-mode" ())
(declare-function my/language-server-ensure-deferred "init-lsp" ())
(defvar lsp-completion-mode)
(defvar lsp-managed-mode)
(defvar lsp-mode)

(defun my/noema-jupyter-cell--lsp-get (key alist)
  "Return KEY from decoded ALIST, accepting symbol and string keys."
  (or (alist-get key alist)
      (and (symbolp key)
           (alist-get (symbol-name key) alist nil nil #'string=))
      (and (stringp key) (alist-get (intern key) alist))))

(defun my/noema-jupyter-cell--lsp-list (value)
  "Return JSON array VALUE as a list."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)
        (t nil)))

(defun my/noema-jupyter-cell--lsp-kernel-environment (spec)
  "Return SPEC's string environment alist."
  (let (result)
    (dolist (entry (my/noema-jupyter-cell--lsp-get 'env spec))
      (when (and (car entry) (stringp (cdr entry)))
        (push (cons (format "%s" (car entry)) (cdr entry)) result)))
    (nreverse result)))

(defun my/noema-jupyter-cell--lsp-workspace (executable paths)
  "Build Python analyzer settings for EXECUTABLE and import PATHS."
  (let ((extra (vconcat (delete-dups (delq nil paths)))))
    `(:python (:pythonPath ,executable
               :analysis (:autoSearchPaths t
                          :useLibraryCodeForTypes t
                          ,@(when (> (length extra) 0)
                              `(:extraPaths ,extra))))
      :pyright (:pythonPath ,executable)
      :basedpyright (:pythonPath ,executable)
      :pylsp (:plugins (:jedi (:environment ,executable))))))

(defun my/noema-jupyter-cell--lsp-unprobeable-connector (kernel)
  "Return an expected fallback when KERNEL has no target process to probe.
Both connectors named here reach a kernel that this Target did not launch, so
there is no interpreter whose `sys.path\=' would describe the workspace."
  (cond
   ((string-prefix-p "attach:" (or kernel ""))
    (my/language-server-runtime-fallback-create
     :reason "attached kernels do not expose a reproducible launch environment"
     :expected t))
   ((string-prefix-p "server:" (or kernel ""))
    (my/language-server-runtime-fallback-create
     :reason "kernels on a Jupyter server are not reachable as a target process"
     :expected t))))

(defun my/noema-jupyter-cell--lsp-probe-command (kernel entry)
  "Return the Python probe argv for KERNEL and kernelspec ENTRY.
A returned fallback object is an expected unsupported reason; a returned
string is an unexpected resolution error."
  (cond
   ;; Without this a `server:\=' kernel fell through to kernelspec lookup and
   ;; reported `kernelspec "server:id:python3" was not found on target local\=',
   ;; which describes neither the kernel nor the reason.
   ((my/noema-jupyter-cell--lsp-unprobeable-connector kernel))
   ((not entry)
    (or (and (stringp my/noema-jupyter-cell-kernel-spec-error)
             (not (string-empty-p my/noema-jupyter-cell-kernel-spec-error))
             my/noema-jupyter-cell-kernel-spec-error)
        (format "kernelspec `%s' was not found" kernel)))
   (t
    (let* ((spec (my/noema-jupyter-cell--lsp-get 'spec entry))
           (argv (my/noema-jupyter-cell--lsp-list
                  (my/noema-jupyter-cell--lsp-get 'argv spec)))
           (metadata (my/noema-jupyter-cell--lsp-get 'metadata spec))
           (aaron (my/noema-jupyter-cell--lsp-get 'aaron metadata))
           (runtime (my/noema-jupyter-cell--lsp-get 'runtime aaron))
           (declared (my/noema-jupyter-cell--lsp-list
                      (my/noema-jupyter-cell--lsp-get 'probe_argv runtime)))
           (module-pos (cl-position "-m" argv :test #'string=))
           (module (and module-pos (nth (1+ module-pos) argv))))
      (cond
       ((and module (string-match-p "remote_ikernel" module))
        (my/language-server-runtime-fallback-create
         :reason
         "legacy remote_ikernel kernelspecs cannot reveal the kernel interpreter"
         :expected t))
       ((and declared (stringp (car declared)))
        (append declared
                (list "-c" my/noema-jupyter-cell--python-runtime-probe)))
       ((and module-pos (stringp (car argv))
             (or (and module
                      (string-match-p "\\(?:ipykernel\\|sage\\)" module))
                 (string-match-p "\\(?:python\\|sage\\)" (car argv))))
        (append (cl-subseq argv 0 module-pos)
                (list "-c" my/noema-jupyter-cell--python-runtime-probe)))
       ((and (stringp (car argv))
             (string-match-p "python[0-9.]*\\'"
                             (file-name-nondirectory (car argv))))
        (list (car argv) "-c" my/noema-jupyter-cell--python-runtime-probe))
       (t
        (my/language-server-runtime-fallback-create
         :reason
         (format "kernelspec `%s' uses an opaque launcher; add metadata.aaron.runtime.probe_argv"
                 kernel)
         :expected t)))))))

(defun my/noema-jupyter-cell--lsp-runtime-from-probe
    (context root kernel session entry base-environment payload)
  "Build a runtime for KERNEL from decoded Python probe PAYLOAD."
  (let* ((reported-executable
          (my/noema-jupyter-cell--lsp-get 'executable payload))
         (executable
          (if (and (stringp reported-executable)
                   (file-name-absolute-p reported-executable))
              reported-executable
            (error "kernel probe did not return an absolute Python executable")))
         (prefix (my/noema-jupyter-cell--lsp-get 'prefix payload))
         (base-prefix (my/noema-jupyter-cell--lsp-get 'base_prefix payload))
         (paths (my/noema-jupyter-cell--lsp-list
                 (my/noema-jupyter-cell--lsp-get 'path payload)))
         (spec (my/noema-jupyter-cell--lsp-get 'spec entry))
         (vars (my/noema-jupyter-cell--lsp-kernel-environment spec))
         (vars (if (and (stringp prefix) (stringp base-prefix)
                        (not (string= prefix base-prefix))
                        (not (assoc-string "VIRTUAL_ENV" vars t)))
                   (cons (cons "VIRTUAL_ENV" prefix) vars)
                 vars))
         (fingerprint
          (secure-hash
           'sha1
           (prin1-to-string
            (list (remote-context-target-id context) root kernel executable
                  prefix vars paths))))
         ;; Runtime ids also become remote-environment ids; `remote-id-regexp'
         ;; deliberately excludes colons.
         (id (format "jupyter-%s" (substring fingerprint 0 16)))
         (workspace (my/noema-jupyter-cell--lsp-workspace executable paths))
         (profile
          (list :id id :label (format "Jupyter %s (%s)" kernel executable)
                :family 'python :executable executable
                :path-prepend (list (file-name-directory executable))
                :env vars :workspace workspace :kind 'jupyter-runtime
                :runtime-controlled t))
         (runtime-environment
          (if vars
              (remote-environment-derive
               base-environment id :scope 'runtime :vars vars :source 'jupyter)
            base-environment)))
    (my/language-server-runtime-create
     :id id
     :label (format "%s · Python %s" kernel
                    (or (my/noema-jupyter-cell--lsp-get 'version payload) ""))
     :provider 'noema-jupyter :family 'python :context context :root root
     :tool-environment base-environment :environment runtime-environment
     :profile profile :workspace-configuration workspace
     :idle-timeout my/noema-jupyter-cell-lsp-idle-timeout
     :metadata (list :kernel kernel :session session :kernelspec entry
                     :target (remote-context-target-id context)))))

(defun my/noema-jupyter-cell--cancel-runtime-probe ()
  "Cancel this buffer's in-flight runtime probe and its deadline."
  (when (timerp my/noema-jupyter-cell--runtime-probe-timer)
    (cancel-timer my/noema-jupyter-cell--runtime-probe-timer))
  (setq my/noema-jupyter-cell--runtime-probe-timer nil)
  (when (processp my/noema-jupyter-cell--runtime-probe)
    (when (process-live-p my/noema-jupyter-cell--runtime-probe)
      (delete-process my/noema-jupyter-cell--runtime-probe)))
  (setq my/noema-jupyter-cell--runtime-probe nil))

(defun my/noema-jupyter-cell--cancel-runtime-probe-h ()
  "Release an in-flight probe when its notebook buffer goes away."
  (ignore-errors (my/noema-jupyter-cell--cancel-runtime-probe)))

(defun my/noema-jupyter-cell--lsp-kernelspec (kernel entries)
  "Return KERNEL's entry from normalized kernelspec ENTRIES."
  (seq-find
   (lambda (entry)
     (equal kernel (my/noema-jupyter-cell--lsp-get 'name entry)))
   entries))

(defun my/noema-jupyter-cell--lsp-callback-later (callback runtime error)
  "Call CALLBACK with RUNTIME and ERROR after the resolver returns."
  (run-at-time 0 nil callback runtime error))

(defun my/noema-jupyter-cell--lsp-start-runtime-probe
    (origin context root kernel session entry base-environment callback)
  "Probe ENTRY and call CALLBACK with its runtime for ORIGIN.
CONTEXT, ROOT, KERNEL, SESSION and BASE-ENVIRONMENT describe the owning target."
  (let ((probe (my/noema-jupyter-cell--lsp-probe-command kernel entry)))
    (if (or (stringp probe)
            (my/language-server-runtime-fallback-p probe))
        (my/noema-jupyter-cell--lsp-callback-later callback nil probe)
      (let* ((spec (my/noema-jupyter-cell--lsp-get 'spec entry))
             (vars (my/noema-jupyter-cell--lsp-kernel-environment spec))
             (probe-environment
              (if vars
                  (remote-environment-derive
                   base-environment "jupyter-probe" :scope 'invocation
                   :vars vars :source 'jupyter)
                base-environment)))
        (condition-case err
            (let* ((settled nil)
                   (settle
                    ;; The deadline timer and the process callback race, and
                    ;; the resolver must be answered exactly once.
                    (lambda (runtime error)
                      (unless settled
                        (setq settled t)
                        (when (buffer-live-p origin)
                          (with-current-buffer origin
                            (my/noema-jupyter-cell--cancel-runtime-probe)))
                        (funcall callback runtime error))))
                   (process
                   (remote-exec-async
                    (car probe) :args (cdr probe) :context context
                    :environment probe-environment
                    :name "noema-kernel-runtime-probe"
                    :callback
                    (lambda (result)
                      (if (zerop (remote-exec-result-status result))
                          (condition-case parse-error
                              (funcall
                               settle
                               (my/noema-jupyter-cell--lsp-runtime-from-probe
                                context root kernel session entry
                                base-environment
                                (json-parse-string
                                 (remote-exec-result-stdout result)
                                 :object-type 'alist :array-type 'list))
                               nil)
                            (error
                             (funcall settle nil
                                      (format
                                       "kernel runtime probe returned invalid data: %s"
                                       (error-message-string parse-error)))))
                        (funcall settle nil
                                 (format "kernel runtime probe failed (%s): %s"
                                         (remote-exec-result-status result)
                                         (string-trim
                                          (remote-exec-result-stderr result)))))))))
              (when (buffer-live-p origin)
                (with-current-buffer origin
                  (setq my/noema-jupyter-cell--runtime-probe process)
                  ;; A callback that already ran synchronously leaves nothing
                  ;; to time out.
                  (unless settled
                    (setq my/noema-jupyter-cell--runtime-probe-timer
                          (run-at-time
                         my/noema-jupyter-cell-lsp-probe-timeout nil
                         (lambda ()
                           (funcall
                            settle nil
                            (format
                             "kernel runtime probe timed out after %ss on target %s"
                             my/noema-jupyter-cell-lsp-probe-timeout
                             (remote-context-target-id context)))))))
                  ;; Killing the notebook mid-probe must not leave the routed
                  ;; process and its buffers running until Emacs exits.
                  (add-hook 'kill-buffer-hook
                            #'my/noema-jupyter-cell--cancel-runtime-probe-h
                            nil t))))
          (error
           (my/noema-jupyter-cell--lsp-callback-later
            callback nil
            (format "kernel runtime probe could not start: %s"
                    (error-message-string err)))))))))

(defun my/noema-jupyter-cell--lsp-discover-and-probe
    (origin source context root kernel session base-environment callback)
  "Rediscover KERNEL for SOURCE, then probe it for ORIGIN.
Discovery runs through CONTEXT so local, container and SSH targets use their
own kernelspec registry instead of the Emacs host's registry."
  (let ((project-entry
         (my/noema-jupyter-cell--lsp-kernelspec
          kernel
          (my/noema-jupyter--project-kernelspecs
           (remote-canonicalize-file-name source)))))
    (if project-entry
        (my/noema-jupyter-cell--lsp-start-runtime-probe
         origin context root kernel session project-entry base-environment
         callback)
      (condition-case err
          (let ((process
                 (remote-exec-async
                  "jupyter" :args '("kernelspec" "list" "--json")
                  :context context :environment base-environment
                  :name "noema-kernelspec-discovery"
                  :callback
                  (lambda (result)
                    (when (buffer-live-p origin)
                      (with-current-buffer origin
                        (setq my/noema-jupyter-cell--runtime-probe nil)))
                    (if (zerop (remote-exec-result-status result))
                        (condition-case parse-error
                            (let* ((payload
                                    (json-parse-string
                                     (remote-exec-result-stdout result)
                                     :object-type 'alist :array-type 'list))
                                   (entry
                                    (my/noema-jupyter-cell--lsp-kernelspec
                                     kernel
                                     (my/noema-jupyter--normalize-kernelspecs
                                      payload))))
                              (if entry
                                  (my/noema-jupyter-cell--lsp-start-runtime-probe
                                   origin context root kernel session entry
                                   base-environment callback)
                                (funcall callback nil
                                         (format
                                          "kernelspec `%s' was not found on target `%s'"
                                          kernel
                                          (remote-context-target-id context)))))
                          (error
                           (funcall callback nil
                                    (format
                                     "kernelspec discovery returned invalid data: %s"
                                     (error-message-string parse-error)))))
                      (funcall callback nil
                               (format "kernelspec discovery failed (%s): %s"
                                       (remote-exec-result-status result)
                                       (string-trim
                                        (remote-exec-result-stderr result)))))))))
            (when (buffer-live-p origin)
              (with-current-buffer origin
                (setq my/noema-jupyter-cell--runtime-probe process))))
        (error
         (my/noema-jupyter-cell--lsp-callback-later
          callback nil
          (format "kernelspec discovery could not start: %s"
                  (error-message-string err))))))))

(defun my/noema-jupyter-cell--lsp-runtime-provider (_buffer callback)
  "Resolve the selected Jupyter kernel and call CALLBACK asynchronously."
  (when (and my/noema-jupyter-cell-mode
             (derived-mode-p 'python-mode 'python-ts-mode))
    (let* ((origin (current-buffer))
           (kernel my/noema-jupyter-cell-kernel)
           (session my/noema-jupyter-cell-session)
           (entry my/noema-jupyter-cell-kernel-spec)
           (source (or my/noema-jupyter-cell-source-file buffer-file-name))
           (context (my/noema-jupyter--context source))
           (project-root
            (when-let* ((project (project-current nil default-directory)))
              (project-root project)))
           (root
            (file-name-as-directory
             (remote-canonicalize-file-name
              (or project-root
                  (remote-context-workspace-root context)
                  (file-name-directory source)))))
           (_ (setf (remote-context-workspace-root context) root))
           (base-environment (remote-environment-resolve context)))
      (if-let* ((unsupported
                 (my/noema-jupyter-cell--lsp-unprobeable-connector kernel)))
          (list :unsupported unsupported)
        (if (and entry
                 (equal kernel
                        (my/noema-jupyter-cell--lsp-get 'name entry)))
            (my/noema-jupyter-cell--lsp-start-runtime-probe
             origin context root kernel session entry base-environment callback)
          ;; The UI event is a cache, not an authority.  It may be emitted while
          ;; the gateway is reconnecting or by an older Noema process.  Resolve
          ;; again on the target instead of permanently falling back.
          (my/noema-jupyter-cell--lsp-discover-and-probe
           origin source context root kernel session base-environment callback))
        'pending))))

(defun my/noema-jupyter-cell-lsp-runtime-changing ()
  "Detach from the old kernel runtime before cell metadata changes."
  (when (fboundp 'my/language-server-runtime--buffer-leaving-h)
    (my/language-server-runtime--buffer-leaving-h))
  ;; Detach this buffer from the old kernel's workspace without tearing the
  ;; server down: `lsp-disconnect' sends didClose, removes the buffer from the
  ;; workspace and clears lsp-mode's buffer state.  Its didClose call explicitly
  ;; keeps the workspace alive, leaving the warm timer above in charge.
  (when (or (bound-and-true-p lsp-managed-mode)
            (bound-and-true-p lsp-mode)
            (ignore-errors (lsp-workspaces)))
    (lsp-disconnect))
  (setq my/language-server-runtime--workspace nil)
  (my/language-server-runtime-invalidate))

(defun my/noema-jupyter-cell--lsp-capf-priority-h ()
  "Keep live-kernel completion ahead of static LSP completion.
This restores Eglot-era hook-depth semantics.  lsp-mode inserts its CAPF with
`add-to-list', which otherwise jumps ahead of the Noema CAPF regardless of the
-10 hook depth used by `my/noema-jupyter-cell-mode'.  The kernel CAPF is
non-exclusive, so Pyright still supplies ordinary static candidates whenever
the live kernel has no answer or is in introspection backoff."
  (when (bound-and-true-p my/noema-jupyter-cell-mode)
    (setq-local
     completion-at-point-functions
     (cons #'my/noema-jupyter-cell-capf
           (delq #'my/noema-jupyter-cell-capf
                 completion-at-point-functions)))))

(defun my/noema-jupyter-cell--lsp-ui-h ()
  "Keep Noema's controls in its header below the shared tab-line."
  (when (bound-and-true-p my/noema-jupyter-cell-mode)
    (setq-local header-line-format
                '(:eval (my/noema-jupyter-cell--header-line)))
    (my/noema-jupyter-cell--lsp-capf-priority-h)
    (force-mode-line-update t)))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-managed-mode-hook #'my/noema-jupyter-cell--lsp-ui-h))

(with-eval-after-load 'lsp-completion
  (add-hook 'lsp-completion-mode-hook
            #'my/noema-jupyter-cell--lsp-capf-priority-h))

(add-hook 'my/noema-jupyter-cell-mode-hook #'my/noema-jupyter-cell--lsp-ui-h)

(my/register-language-server-runtime-provider
 'noema-jupyter #'my/noema-jupyter-cell--lsp-runtime-provider
 :priority 100 :modes '(python-mode python-ts-mode)
 :source 'init-aaronnote-jupyter-lsp
 :cleanup-function #'my/noema-jupyter-cell--cancel-runtime-probe)

(provide 'init-aaronnote-jupyter-lsp)
;;; init-aaronnote-jupyter-lsp.el ends here

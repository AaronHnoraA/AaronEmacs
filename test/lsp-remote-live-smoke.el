;;; lsp-remote-live-smoke.el --- Real TRAMP/Remote lsp-mode smoke -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in because this test connects to a real SSH target.  Every target-side
;; mutation is confined to a fresh /tmp directory and removed on exit.
;;
;;   REMOTE_LSP_E2E=1 REMOTE_LSP_E2E_TARGET=target make lsp-remote-live-smoke
;;   REMOTE_LSP_E2E_VISIT=logical visits the /fs: name for local parity checks.
;;   REMOTE_LSP_E2E_RECONNECT=1 tests a forced reconnect with two source buffers.
;;   REMOTE_LSP_E2E_RECONNECT=auto injects a transport failure for the scheduler.
;;   REMOTE_LSP_E2E_RECONNECT=transport kills this batch Emacs' RPC transport.
;;   REMOTE_LSP_E2E_RECONNECT=transport-fallback also disables the early hook.
;;   REMOTE_LSP_E2E_RECONNECT=stall drops only this batch Emacs' RPC replies.
;;   REMOTE_LSP_E2E_RECONNECT=blackhole drops this batch's SSH TCP traffic.
;;   REMOTE_LSP_E2E_TYPING_ROUNDS=40 measures synchronous edit-hook latency.
;;   REMOTE_LSP_E2E_REDISPLAY=1 also times forced terminal/frame redisplay.
;;   REMOTE_LSP_E2E_CHANGE_BATCH=1 checks the server sees queued edits before requests.
;;   Python always verifies Company CAPF candidates for `prin'.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'init-lsp)
(require 'remote-board)
(require 'remote-config)
(require 'remote-framework)
(require 'seq)
(load (expand-file-name "test/lsp-live-smoke.el" user-emacs-directory)
      nil 'nomessage)

(defvar my/lsp-remote-live-smoke--blackhole-relay nil
  "Private TCP relay state for the opt-in SSH blackhole smoke.")

(defun my/lsp-remote-live-smoke--ssh-config-value (pipeline name)
  "Return OpenSSH's effective NAME for PIPELINE without connecting."
  (let* ((host (plist-get (remote-pipeline-config pipeline) :host))
         (file (remote-transport-ssh-config-file pipeline))
         (args (append (list "-G") (when file (list "-F" file))
                       (list host))))
    (unless (and (stringp host) (not (string-empty-p host)))
      (error "Blackhole smoke needs an SSH host"))
    (with-temp-buffer
      (let ((default-directory temporary-file-directory))
        (unless (zerop (apply #'call-process "ssh" nil t nil args))
          (error "Cannot inspect SSH config for %s" host)))
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^" (regexp-quote name) "[[:space:]]+\\([^\n]+\\)$")
             nil t)
        (match-string-no-properties 1)))))

(defun my/lsp-remote-live-smoke--prepare-blackhole (target)
  "Route TARGET's batch SSH traffic through a private loopback relay."
  (let* ((pipelines (remote-pipelines-for-target (remote-target-id target)))
         (pipeline (car pipelines))
         (python (executable-find "python3"))
         (netcat (executable-find "nc"))
         (host (and pipeline
                    (my/lsp-remote-live-smoke--ssh-config-value
                     pipeline "hostname")))
         (port (and pipeline
                    (my/lsp-remote-live-smoke--ssh-config-value
                     pipeline "port")))
         (proxy-command
          (and pipeline
               (my/lsp-remote-live-smoke--ssh-config-value
                pipeline "proxycommand")))
         (proxy-jump
          (and pipeline
               (my/lsp-remote-live-smoke--ssh-config-value
                pipeline "proxyjump")))
         (marker (make-temp-name
                  (expand-file-name "remote-ssh-blackhole-"
                                    temporary-file-directory)))
         (buffer (generate-new-buffer " *remote-ssh-blackhole-relay*"))
         process relay-port saved)
    (unless (and (= (length pipelines) 1)
                 python netcat host port
                 (string-match-p "\\`[0-9]+\\'" port)
                 (or (null proxy-command)
                     (equal proxy-command "none"))
                 (or (null proxy-jump)
                     (equal proxy-jump "none")))
      (kill-buffer buffer)
      (error "Blackhole smoke needs one direct SSH pipeline and local Python/nc"))
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "remote-ssh-blackhole-relay"
                 :buffer buffer :noquery t
                 :command
                 (list python
                       (expand-file-name
                        "test/ssh-blackhole-relay.py" user-emacs-directory)
                       host port marker)))
          (let ((deadline (+ (float-time) 5)))
            (while (and (process-live-p process)
                        (< (float-time) deadline)
                        (not relay-port))
              (accept-process-output process 0.1)
              (with-current-buffer buffer
                (goto-char (point-min))
                (when (looking-at "\\([0-9]+\\)\n")
                  (setq relay-port (string-to-number (match-string 1)))))))
          (unless relay-port
            (error "SSH blackhole relay did not start"))
          (setq saved (remote-pipeline-config pipeline))
          (let* ((config (copy-sequence saved))
                 (current (plist-get config :ssh-options))
                 (current (cond ((null current) nil)
                                ((stringp current) (list current))
                                ((listp current) current)))
                 (options
                  (append
                   (list
                    (format "ProxyCommand=%s 127.0.0.1 %d"
                            (shell-quote-argument netcat) relay-port)
                    "ServerAliveInterval=1"
                    "ServerAliveCountMax=2")
                   current)))
            (setf (remote-pipeline-config pipeline)
                  (plist-put config :ssh-options options)))
          (setq my/lsp-remote-live-smoke--blackhole-relay
                (list :process process :buffer buffer :marker marker
                      :pipeline pipeline :original-config saved))
          my/lsp-remote-live-smoke--blackhole-relay)
      (unless my/lsp-remote-live-smoke--blackhole-relay
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun my/lsp-remote-live-smoke--stop-blackhole ()
  "Stop and clean up this batch's private SSH relay."
  (when-let* ((state my/lsp-remote-live-smoke--blackhole-relay))
    (setq my/lsp-remote-live-smoke--blackhole-relay nil)
    (when (file-exists-p (plist-get state :marker))
      (delete-file (plist-get state :marker)))
    (setf (remote-pipeline-config (plist-get state :pipeline))
          (plist-get state :original-config))
    (when (process-live-p (plist-get state :process))
      (delete-process (plist-get state :process)))
    (when (buffer-live-p (plist-get state :buffer))
      (kill-buffer (plist-get state :buffer)))))

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

(defun my/lsp-remote-live-smoke--drain (seconds test)
  "Wait up to SECONDS for TEST, keeping batch Emacs' event sources running.
A watch backend may deliver its events through the command loop's
special-event queue (`insert-special-event').  Batch Emacs runs no command
loop, so that queue has to be drained explicitly or the callback never runs
even though the event has already arrived."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall test)) (< (float-time) deadline))
      (accept-process-output nil 0.1)
      (ignore-errors (read-event nil nil 0.1)))
    (funcall test)))

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
          (my/lsp-remote-live-smoke--drain 10 (lambda () events))
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
     ;; Pyright provides diagnostics and navigation but does not advertise
     ;; textDocument/formatting.  Keep that optional in this language probe.
     :required-methods
     ("textDocument/completion" "textDocument/hover"
      "textDocument/definition" "textDocument/references"
      "textDocument/documentSymbol" "textDocument/rename"
      "textDocument/codeAction")
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

(defun my/lsp-remote-live-smoke--python-completion-probe ()
  "Request real Python CAPF candidates and report latency or failure."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "return name" nil t)
      (error "Python completion probe lost its source anchor"))
    (let ((started (float-time)))
      (condition-case error
          (let* ((capf (lsp-completion-at-point))
                 (candidates
                  (and capf
                       (all-completions "name" (nth 2 capf)))))
            (list :ok (and (member "name" candidates) t)
                  :elapsed (- (float-time) started)
                  :count (length candidates)))
        (error
         (list :ok nil
               :elapsed (- (float-time) started)
               :error (error-message-string error)))))))

(defun my/lsp-remote-live-smoke--typing-probe (rounds)
  "Measure synchronous edit hooks for ROUNDS simulated keypresses.
The buffer is restored afterward.  Process output is serviced between keys,
so language-server notifications and idle completion may run.  Opt-in
redisplay timing requires an interactive frame; keyboard delivery is excluded."
  (save-excursion
    (goto-char (point-max))
    (let ((begin (point))
          (render-p (and (not noninteractive)
                         (equal (getenv "REMOTE_LSP_E2E_REDISPLAY") "1")))
          (initial-overlays (length (overlays-in (point-min) (point-max))))
          (initial-size (buffer-size))
          latencies render-latencies)
      (unwind-protect
          (condition-case error-data
              (progn
                (insert "\n# ")
                (dotimes (index rounds)
                  (let ((last-command-event (+ ?a (mod index 26)))
                        (this-command 'self-insert-command)
                        (started (float-time)))
                    (run-hooks 'pre-command-hook)
                    (self-insert-command 1)
                    (run-hooks 'post-command-hook)
                    (push (* 1000 (- (float-time) started)) latencies)
                    (when render-p
                      (let ((render-start (float-time)))
                        (redisplay t)
                        (push (* 1000 (- (float-time) render-start))
                              render-latencies))))
                  (accept-process-output nil 0.02))
                (setq latencies (sort latencies #'<))
                (when render-p
                  (setq render-latencies (sort render-latencies #'<)))
                (list :ok t :rounds rounds
                      :window-system window-system
                      :frame-size (cons (frame-width) (frame-height))
                      :buffer-size initial-size
                      :overlay-count initial-overlays
                      :semantic-tokens-mode
                      (and (bound-and-true-p lsp-semantic-tokens-mode) t)
                      :inlay-hints-mode
                      (and (bound-and-true-p lsp-inlay-hints-mode) t)
                      :median-ms (nth (/ rounds 2) latencies)
                      :p95-ms (nth (min (1- rounds)
                                         (floor (* 0.95 rounds)))
                                    latencies)
                      :max-ms (car (last latencies))
                      :redisplay-median-ms
                      (and render-p (nth (/ rounds 2) render-latencies))
                      :redisplay-p95-ms
                      (and render-p
                           (nth (min (1- rounds) (floor (* 0.95 rounds)))
                                render-latencies))
                      :redisplay-max-ms
                      (and render-p (car (last render-latencies)))))
            (error
             (list :ok nil :error (error-message-string error-data))))
        (delete-region begin (point-max))))))

(defun my/lsp-remote-live-smoke--change-batch-probe ()
  "Verify a real Python server sees queued edits before symbol requests."
  (let* ((workspace (car (lsp-workspaces)))
         (name "remote_batch_probe_unique_7281")
         (identifier (lsp--text-document-identifier))
         (started (float-time))
         (queued nil)
         (present nil)
         (removed nil)
         (idle-drained nil)
         (first-ms nil))
    (cl-labels
        ((has-symbol-p ()
           (let ((symbols
                  (with-timeout
                      (15 (error "Remote documentSymbol request timed out"))
                    (lsp-request "textDocument/documentSymbol"
                                 (list :textDocument identifier)))))
             (seq-some
              (lambda (symbol)
                (equal (lsp-get symbol :name) name))
              symbols))))
      (save-excursion
        (goto-char (point-max))
        (let ((begin (point)))
          (unwind-protect
              (progn
                (insert (format "\n\ndef %s():\n    return 1\n" name))
                (setq queued
                      (and (gethash workspace my/lsp-remote-change--queues)
                           t))
                (let ((request-at (float-time)))
                  (setq present (has-symbol-p)
                        first-ms (* 1000 (- (float-time) request-at)))))
            (delete-region begin (point-max)))))
      (accept-process-output nil 0.1)
      (setq idle-drained
            (not (gethash workspace my/lsp-remote-change--queues))
            removed (not (has-symbol-p))))
    (list :ok (and queued present idle-drained removed t)
          :queued-before-request queued
          :visible-after-request present
          :idle-drained idle-drained
          :removed-after-request removed
          :first-request-ms first-ms
          :elapsed-ms (* 1000 (- (float-time) started)))))

(defun my/lsp-remote-live-smoke--python-company-probe ()
  "Ask the configured Company CAPF backend for `prin' candidates."
  (require 'company-capf)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "return name" nil t)
      (error "Company probe lost its Python source anchor"))
    (beginning-of-line)
    (let* ((begin (point))
           (snippet "    prin\n"))
      (unwind-protect
          (condition-case error-data
              (progn
                (insert snippet)
                (goto-char (+ begin 8))
                (accept-process-output nil 0.1)
                (let ((started (float-time))
                      prefix candidates)
                  (with-timeout (15 (error "Company candidate request timed out"))
                    (setq prefix (company-capf 'prefix)
                          candidates (company-capf 'candidates "prin" "")))
                  (list :ok (and (equal (car-safe prefix) "prin")
                                 (member "print" candidates) t)
                        :elapsed-ms (* 1000 (- (float-time) started))
                        :prefix (car-safe prefix)
                        :count (length candidates)
                        :contains-print (and (member "print" candidates) t)
                        :capf (car-safe company-capf--current-completion-data))))
            (error
             (list :ok nil :error (error-message-string error-data))))
        (delete-region begin (+ begin (length snippet)))))))

(defun my/lsp-remote-live-smoke--reconnect-probe
    (owner-root watch-root previous-process timeout)
  "Reconnect OWNER-ROOT and verify a new LSP process and watch event.
WATCH-ROOT is the source spelling used for the event probe.  Run from the
source buffer after the initial parity checks."
  (let* ((owner (remote-get-workspace owner-root))
         (started-at (float-time))
         (deadline (+ started-at timeout))
         (resources-before
          (and owner
               (mapcar (lambda (resource)
                         (list (remote-workspace-resource-kind resource)
                               (remote-workspace-resource-state resource)))
                       (remote-workspace-resources owner))))
         current process job owner-opened-at lsp-ready-at
         stall-query-elapsed blackhole-transport-elapsed)
    (unless (and owner (remote-workspace-live-p owner))
      (error "LSP root has no open Remote workspace"))
    (pcase (getenv "REMOTE_LSP_E2E_RECONNECT")
      ("auto"
       (remote-workspace-handle-transport-failure
        (or (remote-workspace-primary-route owner)
            (error "LSP workspace has no tracked route"))
        '(remote-transport-error "injected SSH transport failure")))
      ((or "transport" "transport-fallback" "stall" "blackhole")
       (let* ((physical
               (remote-project-file-name
                owner-root nil 'file-read "emacs-file"))
              (vec (tramp-dissect-file-name physical))
              (connection (tramp-rpc--get-connection vec))
              (transport (plist-get connection :process)))
         (unless (and (processp transport)
                      (process-live-p transport)
                      (eq connection
                          (process-get transport :tramp-rpc-connection)))
           (error "No live RPC transport owned by this Emacs process"))
         (when (equal (getenv "REMOTE_LSP_E2E_RECONNECT")
                      "transport-fallback")
           (advice-remove
            'tramp-rpc--connection-transport-death
            #'remote-backend-tramp-rpc--transport-death-before-a))
         (pcase (getenv "REMOTE_LSP_E2E_RECONNECT")
           ("stall"
            (let ((started (float-time))
                  (filter (process-filter transport))
                  timed-out)
              (unwind-protect
                  (progn
                    ;; Keep the SSH process live while no RPC response is
                    ;; delivered.  The next read must retire this generation.
                    (set-process-filter transport (lambda (_proc _data)))
                    (condition-case err
                        (file-attributes
                         (expand-file-name
                          (format "rpc-stall-probe-%d" (random 1000000000))
                          watch-root))
                      (remote-file-error
                       (setq timed-out
                             (string-match-p
                              "[Tt]imeout waiting for RPC response"
                              (error-message-string err)))))
                    (setq stall-query-elapsed (- (float-time) started))
                    (unless (and timed-out
                                 (< stall-query-elapsed 8)
                                 (not (process-live-p transport)))
                      (error
                       "Stalled query did not time out and retire RPC transport: %s %.2fs"
                       timed-out stall-query-elapsed)))
                (when (process-live-p transport)
                  (ignore-errors
                    (set-process-filter transport filter))))))
           ("blackhole"
            (let* ((state my/lsp-remote-live-smoke--blackhole-relay)
                   (marker (plist-get state :marker))
                   (started (float-time))
                   (expiry (+ started 5))
                   (sentinel (process-sentinel transport)))
              (unless (and state
                           (process-live-p (plist-get state :process))
                           sentinel)
                (error "No live private SSH blackhole relay"))
              (let ((argv (process-command transport)))
                (unless (and (member "ServerAliveInterval=1" argv)
                             (member "ServerAliveCountMax=2" argv)
                             (seq-some
                              (lambda (arg)
                                (string-prefix-p "ProxyCommand=" arg))
                              argv))
                  (error "RPC SSH transport did not use the isolated relay")))
              (unwind-protect
                  (progn
                    ;; No file query is issued here.  The idle SSH transport
                    ;; must close from OpenSSH's own server-alive deadline.
                    (set-process-sentinel
                     transport
                     (lambda (proc event)
                       (process-put proc 'remote-test-blackhole-exit-at
                                    (float-time))
                       (funcall sentinel proc event)))
                    (write-region (format "%.6f" expiry)
                                  nil marker nil 'silent)
                    (while (and (not (process-get
                                      transport 'remote-test-blackhole-exit-at))
                                (< (- (float-time) started) 30))
                      (sit-for 0.1))
                    (let ((exited-at
                           (process-get
                            transport 'remote-test-blackhole-exit-at)))
                      (unless (and exited-at (< exited-at expiry))
                        (error
                         "SSH did not detect the TCP blackhole before traffic resumed"))
                      (setq blackhole-transport-elapsed
                            (- exited-at started))))
                (when (file-exists-p marker)
                  (delete-file marker)))))
           (_ (delete-process transport)))))
      (_ (remote-workspace-reconnect owner)))
    (setq job (plist-get (remote-workspace-metadata owner)
                         :reconnect-job))
    (when (remote-workspace-live-p owner)
      (setq owner-opened-at (float-time)))
    (while (and (< (float-time) deadline)
                (not
                 (progn
                   (setq current (car (ignore-errors (lsp-workspaces)))
                         process
                         (and current
                              (ignore-errors
                                (lsp--workspace-cmd-proc current))))
                   (and process
                        (not (eq process previous-process))
                        (process-live-p process)
                        (eq (lsp--workspace-status current)
                            'initialized)))))
      (sit-for 0.1)
      (when (and (null owner-opened-at)
                 (remote-workspace-live-p owner))
        (setq owner-opened-at (float-time))))
    (let* ((restarted
            (and process
                 (not (eq process previous-process))
                 (process-live-p process)
                 (eq (lsp--workspace-status current) 'initialized)))
           (_ready-clock
            (when restarted (setq lsp-ready-at (float-time))))
           (diagnostics
            (and restarted
                 (my/lsp-remote-live-smoke--wait-for-diagnostic 10)))
           (watch
            (and restarted
                 (my/lsp-remote-live-smoke--watch-probe watch-root))))
      (list :ok (and restarted diagnostics
                     (plist-get watch :valid)
                     (plist-get watch :event)
                     (memq #'lsp-completion-at-point
                           completion-at-point-functions)
                     t)
            :elapsed (- (float-time) started-at)
            :stall-query-elapsed stall-query-elapsed
            :blackhole-transport-elapsed blackhole-transport-elapsed
            :connection-elapsed
            (and owner-opened-at (- owner-opened-at started-at))
            :lsp-elapsed
            (and lsp-ready-at (- lsp-ready-at started-at))
            :job-attempts
            (and job (remote-background-job-attempts job))
            :job-state (and job (remote-background-job-state job))
            :owner-state (remote-workspace-state owner)
            :resources-before resources-before
            :resources-after
            (mapcar (lambda (resource)
                      (list (remote-workspace-resource-kind resource)
                            (remote-workspace-resource-state resource)))
                    (remote-workspace-resources owner))
            :workspace-state (and current
                                  (lsp--workspace-status current))
            :new-process (and process
                              (not (eq process previous-process))
                              (process-live-p process)
                              t)
            :diagnostics (length diagnostics)
            :completion
            (and (memq #'lsp-completion-at-point
                       completion-at-point-functions)
                 t)
            :watch watch))))

(defun my/lsp-remote-live-smoke--reconnect-with-sibling
    (owner-root watch-root previous-process timeout)
  "Probe reconnect with a second source buffer in the same LSP workspace."
  (let* ((mode major-mode)
         (extension (or (file-name-extension buffer-file-name t) ".txt"))
         (file (expand-file-name
                (concat "reconnect-sibling" extension) watch-root))
         sibling)
    (unwind-protect
        (progn
          (with-temp-file file (insert "\n"))
          (setq sibling (find-file-noselect file))
          (with-current-buffer sibling
            (funcall mode)
            (setq-local lsp-auto-guess-root t
                        lsp-guess-root-without-session t)
            (lsp)
            (unless (my/lsp-live-smoke--wait 10)
              (error "Second source buffer did not join LSP"))
            (unless (eq (lsp--workspace-cmd-proc
                         (car (lsp-workspaces)))
                        previous-process)
              (error "Second source buffer started a separate LSP process")))
          (let* ((result
                  (my/lsp-remote-live-smoke--reconnect-probe
                   owner-root watch-root previous-process timeout))
                 (new-workspace (car (ignore-errors (lsp-workspaces))))
                 (sibling-ok
                  (with-current-buffer sibling
                    (and (bound-and-true-p lsp-managed-mode)
                         (memq new-workspace (lsp-workspaces))
                         (eq (lsp--workspace-status new-workspace)
                             'initialized)))))
            (plist-put result :sibling (and sibling-ok t))
            (plist-put result :ok
                       (and (plist-get result :ok) sibling-ok t))))
      (when (buffer-live-p sibling)
        (kill-buffer sibling)))))

(defun my/lsp-remote-live-smoke--run-one (target spec)
  "Run language-server SPEC through a physical TRAMP buffer on TARGET."
  (let* ((language (car spec))
         (properties (cdr spec))
         (started-at (float-time))
         (target-id (remote-target-id target))
         (bootstrap-root (remote-make-file-name target-id "/tmp/"))
         (bootstrap-context (remote-context bootstrap-root))
         (native-directory
          (string-trim
           (remote-exec-output
            "mktemp" :args '("-d" "/tmp/emacs-lsp-e2e.XXXXXX")
            :context bootstrap-context :adapter "language-server" :check t)))
         (bootstrapped-at (float-time))
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
         (visited-file
          (if (equal (getenv "REMOTE_LSP_E2E_VISIT") "logical")
              logical-file
            physical-file))
         buffer folder-buffer result written-at folder-opened-at
         source-visit-started-at client-prewarm-seconds
         visited-at requested-at initialized-at)
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
          (setq written-at (float-time))
          (when (equal (getenv "REMOTE_LSP_E2E_OPEN_FOLDER") "1")
            (setq folder-buffer
                  (remote-open-folder target native-directory))
            (unless (remote-workspace-live-p
                     (remote-get-workspace logical-directory))
              (error "Opening the folder did not establish a workspace")))
          (setq folder-opened-at (float-time))
          (when (equal (getenv "REMOTE_LSP_E2E_PREWARM") "1")
            (unless (timerp my/language-server--folder-prewarm-timer)
              (error "Managed folder did not schedule client LSP prewarm"))
            (cancel-timer my/language-server--folder-prewarm-timer)
            (let ((started (float-time)))
              ;; Batch Emacs has no interactive idle command loop.  Run the
              ;; actual scheduled callback to simulate a pause in Dired.
              (my/language-server--folder-prewarm-run)
              (setq client-prewarm-seconds (- (float-time) started)))
            (unless my/language-server--folder-prewarmed
              (error "Managed folder did not preload the LSP client")))
          (setq source-visit-started-at (float-time))
          (setq buffer (find-file-noselect visited-file))
          (setq visited-at (float-time))
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (funcall (plist-get properties :mode))
            (setq-local lsp-auto-guess-root t
                        lsp-guess-root-without-session t
                        my/language-server--manual-start t)
            (set-buffer-modified-p t)
            (my/language-server-ensure)
            (setq requested-at (float-time))
            (when (and (bound-and-true-p lsp--buffer-deferred)
                       (fboundp 'lsp--init-if-visible))
              (lsp--init-if-visible))
            (let* ((ok
                    (my/lsp-live-smoke--wait
                     (plist-get properties :timeout)))
                   (_initialization-clock
                    (setq initialized-at (float-time)))
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
                   (completion-p
                    (and (memq #'lsp-completion-at-point
                               completion-at-point-functions)
                         t))
                   (completion-probe
                    (when (and ok completion-p (eq language 'python))
                      (my/lsp-remote-live-smoke--python-completion-probe)))
                   (typing-rounds
                    (min 200 (max 0 (string-to-number
                                     (or (getenv "REMOTE_LSP_E2E_TYPING_ROUNDS")
                                         "0")))))
                   (typing-probe
                    (when (and ok (> typing-rounds 0))
                      (my/lsp-remote-live-smoke--typing-probe
                       typing-rounds)))
                   (change-batch-probe
                    (when (and ok (eq language 'python)
                               (equal (getenv "REMOTE_LSP_E2E_CHANGE_BATCH")
                                      "1"))
                      (my/lsp-remote-live-smoke--change-batch-probe)))
                   (company-probe
                    (when (and ok (eq language 'python))
                      (my/lsp-remote-live-smoke--python-company-probe)))
                   (remote-process-p
                    (and process
                         (remote-context-p
                          (process-get process 'remote-context))))
                   ;; This smoke runs inside one long callback, so even an
                   ;; interactive TTY frame cannot enter the idle command
                   ;; loop.  Run the pending snippet callback explicitly.
                   (_snippet-idle-callback
                    (when (and (boundp 'my/yas--pending-enable)
                               (timerp my/yas--pending-enable))
                      (cancel-timer my/yas--pending-enable)
                      (my/yas--enable-after-idle
                       (current-buffer) major-mode)))
                   (snippets-enabled (and (bound-and-true-p yas-minor-mode) t))
                   (core-capabilities-p
                    (and
                     capabilities
                     (seq-every-p
                      (lambda (method)
                        (cdr (assoc method capabilities)))
                      (or (plist-get properties :required-methods)
                          my/lsp-remote-live-smoke-core-methods))))
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
                     completion-p snippets-enabled core-capabilities-p
                     (or (not (eq language 'python))
                         (plist-get completion-probe :ok))
                     (or (null typing-probe)
                         (plist-get typing-probe :ok))
                     (or (null change-batch-probe)
                         (plist-get change-batch-probe :ok))
                     (or (null company-probe)
                         (plist-get company-probe :ok))
                     lsp-enable-file-watchers watches-valid-p
                     (plist-get watch-probe :valid)
                     (plist-get watch-probe :event)
                     (eq
                      (my/language-server--lsp-workspace-id workspace)
                      (plist-get properties :server))))
                   (reconnect-requested
                    (member (getenv "REMOTE_LSP_E2E_RECONNECT")
                            '("1" "auto" "transport"
                              "transport-fallback" "stall" "blackhole")))
                   (reconnect
                    (when (and parity-ok reconnect-requested)
                      (condition-case error
                          (my/lsp-remote-live-smoke--reconnect-with-sibling
                           (my/language-server--lsp-workspace-root workspace)
                           logical-directory process
                           (min (if (equal (getenv "REMOTE_LSP_E2E_RECONNECT")
                                           "blackhole")
                                    45 15)
                                (plist-get properties :timeout)))
                        (error
                         (list :ok nil
                               :error (error-message-string error)))))))
              (setq result
                    (list
                     :language language
                     :ok (and parity-ok
                              (or (not reconnect-requested)
                                  (plist-get reconnect :ok))
                              t)
                     :timings
                     (list :bootstrap (- bootstrapped-at started-at)
                           :seed (- written-at bootstrapped-at)
                           :folder-open (- folder-opened-at written-at)
                           :client-prewarm client-prewarm-seconds
                           :visit (- visited-at source-visit-started-at)
                           :start-request (- requested-at visited-at)
                           :initialize (- initialized-at requested-at)
                           :postchecks (- (float-time) initialized-at))
                     :managed (bound-and-true-p lsp-managed-mode)
                     :source visited-file
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
                     :completion-mode
                     (and (boundp 'lsp-completion-mode) lsp-completion-mode)
                     :completion-capfs completion-at-point-functions
                     :completion-probe completion-probe
                     :typing-probe typing-probe
                     :change-batch-probe change-batch-probe
                     :company-probe company-probe
                     :completion-prewarm
                     (and workspace
                          (plist-get
                           (gethash
                            workspace
                            my/lsp-completion--prewarm-state)
                           :state))
                     :snippets-enabled snippets-enabled
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
                     :reconnect reconnect
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
      (when-let* ((folder-workspace
                   (remote-get-workspace logical-directory)))
        (remote-workspace-close folder-workspace 'remote-live-smoke))
      (when (and (buffer-live-p folder-buffer)
                 (not (eq folder-buffer buffer)))
        (kill-buffer folder-buffer))
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
  (when (and (display-graphic-p)
             (getenv "REMOTE_LSP_E2E_FRAME_COLUMNS")
             (getenv "REMOTE_LSP_E2E_FRAME_ROWS"))
    (set-frame-size
     (selected-frame)
     (max 40 (string-to-number (getenv "REMOTE_LSP_E2E_FRAME_COLUMNS")))
     (max 20 (string-to-number (getenv "REMOTE_LSP_E2E_FRAME_ROWS"))))
    (redisplay t))
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
         (repeat (max 1 (string-to-number
                         (or (getenv "REMOTE_LSP_E2E_REPEAT") "1"))))
         results)
    (unwind-protect
        (progn
          (when (equal (getenv "REMOTE_LSP_E2E_RECONNECT") "blackhole")
            (my/lsp-remote-live-smoke--prepare-blackhole target))
          (setq results
                (cl-loop repeat repeat
                         append (mapcar
                                 (lambda (spec)
                                   (my/lsp-remote-live-smoke--run-one
                                    target spec))
                                 specs))))
      (my/lsp-remote-live-smoke--stop-blackhole))
    (when-let* ((result-file (getenv "REMOTE_LSP_E2E_RESULT_FILE")))
      (with-temp-file result-file
        (dolist (result results)
          (prin1 result (current-buffer))
          (terpri (current-buffer)))))
    (dolist (result results)
      (princ (format "%S\n" result)))
    (kill-emacs
     (if (seq-every-p (lambda (result) (plist-get result :ok)) results)
         0
       1))))

(provide 'lsp-remote-live-smoke)
;;; lsp-remote-live-smoke.el ends here

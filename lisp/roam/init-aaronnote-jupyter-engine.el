;;; init-aaronnote-jupyter-engine.el --- Emacs-owned Noema Jupyter documents -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs is the authority for Noema Jupyter documents.  The web application is
;; deliberately only a document/output projection: kernel requests, execution
;; ordering, output persistence and structural cell edits are coordinated here.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'remote-gateway)
(require 'init-aaronnote-jupyter-manager)
(require 'init-aaronnote-jupyter-notebook)
(require 'jupyter-client)
(require 'jupyter-messages)
(require 'jupyter-monads)
(require 'jupyter-repl)

(declare-function my/noema-jupyter-output-open "init-aaronnote"
                  (&optional cell-id focus))
(declare-function my/jupyter-board-open "init-jupyter-board" ())
(declare-function my/noema-jupyter-cell-activate-buffer
                  "init-aaronnote-jupyter-cell" (&optional payload))
(declare-function my/noema-jupyter-cell--engine-params
                  "init-aaronnote-jupyter-cell" (&optional cell-id require-cell))
(declare-function my/noema-jupyter-cell--goto-id
                  "init-aaronnote-jupyter-cell" (cell-id))
(declare-function my/noema-jupyter-cell--set-kernelspec-header
                  "init-aaronnote-jupyter-cell" (name))

(cl-defstruct (my/noema-jupyter-document
               (:constructor my/noema-jupyter-document-create))
  key script-file source-file language kernel session manager-session-id
  running last-used)

(defvar my/noema-jupyter-documents (make-hash-table :test #'equal)
  "Document/output controllers keyed only by canonical script path.")

(defvar my/noema-jupyter-pending-input (make-hash-table :test #'equal)
  "Outstanding kernel input requests keyed by execution run id.")

(defun my/noema-jupyter-engine--get (key object)
  "Return KEY from OBJECT represented by a plist, alist or hash table."
  (let ((name (if (symbolp key) (symbol-name key) key)))
    (cond
     ((hash-table-p object) (or (gethash name object) (gethash key object)))
     ((and (listp object) (keywordp (car object)))
      (plist-get object (if (keywordp key) key (intern (concat ":" name)))))
     ((listp object)
      (or (alist-get key object)
          (alist-get name object nil nil #'string=)
          (and (stringp name) (alist-get (intern name) object)))))))

(defun my/noema-jupyter-engine--json-value (value)
  "Convert VALUE from Jupyter plist form into gateway-safe JSON form."
  (cond
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (push (cons (cond
                              ((keywordp key)
                               (intern (substring (symbol-name key) 1)))
                              ((symbolp key) key)
                              (t (intern (format "%s" key))))
                             (my/noema-jupyter-engine--json-value item))
                       result))
               value)
      (nreverse result)))
   ((vectorp value)
    (vconcat (mapcar #'my/noema-jupyter-engine--json-value value)))
   ((and (listp value) (keywordp (car value)))
    (cl-loop for (key item) on value by #'cddr
             collect (cons (intern (substring (symbol-name key) 1))
                           (my/noema-jupyter-engine--json-value item))))
   ((and (listp value) (consp (car value))
         (or (symbolp (caar value)) (stringp (caar value))))
    (mapcar
     (lambda (entry)
       (cons (if (symbolp (car entry))
                 (car entry)
               (intern (car entry)))
             (my/noema-jupyter-engine--json-value (cdr entry))))
     value))
   ((listp value)
    (vconcat (mapcar #'my/noema-jupyter-engine--json-value value)))
   ((eq value jupyter--false) :json-false)
   (t value)))

(defun my/noema-jupyter-engine--token (value fallback)
  "Return trimmed VALUE, or FALLBACK."
  (let ((text (string-trim (format "%s" (or value "")))))
    (if (string-empty-p text) fallback text)))

(defun my/noema-jupyter-engine--slug (value fallback)
  "Return filesystem-safe VALUE, or FALLBACK."
  (let* ((text (string-trim (format "%s" (or value ""))))
         (text (replace-regexp-in-string "\\`\\.+" "" text))
         (text (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" text))
         (text (replace-regexp-in-string "\\`-+\\|-+\\'" "" text)))
    (if (string-empty-p text) fallback (substring text 0 (min 90 (length text))))))

(defun my/noema-jupyter-engine--language-for-kernel (kernel requested)
  "Resolve language from KERNEL and REQUESTED language."
  (let ((explicit (downcase (my/noema-jupyter-engine--token requested "")))
        (kernel (downcase (my/noema-jupyter-engine--token kernel ""))))
    (cond
     ((or (string-match-p "lean" kernel) (member explicit '("lean" "lean4"))) "lean4")
     ((member explicit '("bash" "sh" "shell" "zsh")) "bash")
     ((not (string-empty-p explicit)) explicit)
     ((string-match-p "sage" kernel) "python")
     ((string-match-p "python\\|\\`py\\'" kernel) "python")
     ((string-match-p "julia" kernel) "julia")
     ((string-match-p "\\`i?r\\'" kernel) "r")
     ((string-match-p "bash\\|zsh\\|shell" kernel) "bash")
     ((string-match-p "typescript\\|\\`ts\\'" kernel) "typescript")
     ((string-match-p "javascript\\|node\\|\\`js\\'" kernel) "javascript")
     (t "python"))))

(defun my/noema-jupyter-engine--hidden-script (source language session)
  "Return hidden notebook path for SOURCE, LANGUAGE and SESSION."
  (let* ((base (file-name-base source))
         (language (my/noema-jupyter-engine--slug language "python"))
         (session (my/noema-jupyter-engine--slug session "default")))
    (expand-file-name
     (format "%s.%s.%s.ipynb"
             (my/noema-jupyter-engine--slug base "note") language session)
     (expand-file-name ".cell" (file-name-directory source)))))

(defun my/noema-jupyter-engine--script-identity (script source)
  "Return authoritative (:language :session) encoded by SCRIPT for SOURCE."
  (let* ((stem (file-name-sans-extension
                (file-name-nondirectory script)))
         (source-base (and source (file-name-base source)))
         (prefix (and source-base (concat source-base ".")))
         (tail (if (and prefix (string-prefix-p prefix stem))
                   (substring stem (length prefix))
                 stem)))
    (when (string-match "\\`\\([^.]+\\)\\.\\(.+\\)\\'" tail)
      (list :language (match-string 1 tail)
            :session (match-string 2 tail)))))

(defun my/noema-jupyter-engine--metadata (&optional params)
  "Resolve notebook metadata from PARAMS and standard ipynb metadata."
  (let* ((file (my/noema-jupyter-engine--token
                (or (my/noema-jupyter-engine--get 'scriptFile params)
                    (my/noema-jupyter-engine--get 'file params)) ""))
         (script-p (and (not (string-empty-p file))
                        (string-match-p "\\(?:\\`\\|/\\)\\.cell/[^/]+\\.ipynb\\'" file)))
         (explicit-source
          (my/noema-jupyter-engine--token
           (my/noema-jupyter-engine--get 'sourceFile params) ""))
         (source (if (string-empty-p explicit-source)
                     (unless script-p file)
                   explicit-source))
         (kernel (my/noema-jupyter-engine--token
                  (or (my/noema-jupyter-engine--get 'kernelSpecName params)
                      (my/noema-jupyter-engine--get 'kernel params)) "python3"))
         (session (my/noema-jupyter-engine--token
                   (my/noema-jupyter-engine--get 'session params) "default"))
         (language (my/noema-jupyter-engine--language-for-kernel
                    kernel (my/noema-jupyter-engine--get 'language params)))
         (script (if script-p file
                   (and (not (string-empty-p source))
                        (my/noema-jupyter-engine--hidden-script source language session)))))
    (unless (and script (not (string-empty-p script)))
      (error "Missing Jupyter script or source file"))
    (setq script (expand-file-name script))
    (when (file-readable-p script)
      (let ((notebook (my/noema-jupyter-notebook-metadata script)))
        (setq source (or (and (not (string-empty-p
                                    (plist-get notebook :source)))
                              (plist-get notebook :source))
                         source)
              kernel (or (plist-get notebook :kernel) kernel)
              language (or (plist-get notebook :language) language)
              session (or (plist-get notebook :session) session))))
    (list :script-file script :source-file source :kernel kernel
          :session session :language language)))

(defun my/noema-jupyter-engine--cells (script)
  "Return ordered cell plists projected from notebook SCRIPT."
  (unless (file-readable-p script)
    (error "Jupyter notebook does not exist: %s" script))
  (my/noema-jupyter-notebook-cells script))

(defun my/noema-jupyter-engine--output-file (metadata)
  "Return METADATA's sole notebook path for output persistence."
  (plist-get metadata :script-file))

(defun my/noema-jupyter-engine--read-json (file)
  "Read FILE as the engine's output projection."
  (when (file-readable-p file)
    (condition-case nil
        (my/noema-jupyter-notebook-output-mirror
         (my/noema-jupyter-notebook-read file))
      (error nil))))

(defun my/noema-jupyter-engine--output-cell (mirror id)
  "Return ID's saved result from MIRROR."
  (let ((cells (my/noema-jupyter-engine--get 'cells mirror)))
    (or (my/noema-jupyter-engine--get id cells)
        (my/noema-jupyter-engine--get (intern-soft id) cells))))

(defun my/noema-jupyter-engine--revision (code)
  "Return stable revision for CODE."
  (secure-hash 'sha256 (replace-regexp-in-string "\r\n?" "\n" (or code ""))))

(defun my/noema-jupyter-engine-document-snapshot (params)
  "Return an authoritative document snapshot described by PARAMS."
  (let* ((metadata (my/noema-jupyter-engine--metadata params))
         (script (plist-get metadata :script-file))
         (mirror (my/noema-jupyter-engine--read-json
                  (my/noema-jupyter-engine--output-file metadata)))
         (cells (my/noema-jupyter-engine--cells script))
         (state (gethash (my/noema-jupyter-engine--state-key metadata)
                         my/noema-jupyter-documents))
         (manager-session
          (my/noema-jupyter-manager-session script t metadata))
         (manager-kernel
          (my/noema-jupyter-manager-kernel-for-session manager-session)))
    `((ok . t)
      (documentRevision . ,(secure-hash
                            'sha256
                            (mapconcat (lambda (cell)
                                         (concat (plist-get cell :id) "\0"
                                                 (plist-get cell :code)))
                                       cells "\0")))
      (document . ((scriptFile . ,script)
                   (sourceFile . ,(or (plist-get metadata :source-file) ""))
                   (language . ,(plist-get metadata :language))
                   (kernel . ,(plist-get metadata :kernel))
                   (kernelSpecName . ,(plist-get metadata :kernel))
                   (kernelId . ,(or (and manager-kernel
                                         (my/noema-jupyter-kernel-id manager-kernel)) ""))
                   (session . ,(plist-get metadata :session))
                   (sessionName . ,(plist-get metadata :session))
                   (sessionId . ,(or (and manager-session
                                          (my/noema-jupyter-session-id
                                           manager-session)) ""))))
      (kernelStatus . ,(cond
                        ((and manager-kernel
                              (> (or (my/noema-jupyter-kernel-running
                                      manager-kernel) 0) 0)) "busy")
                        (manager-kernel
                         (symbol-name
                          (or (my/noema-jupyter-kernel-status manager-kernel)
                              'idle)))
                        (t "disconnected")))
      (cells . ,(vconcat (mapcar
                  (lambda (cell)
                    (let* ((saved (my/noema-jupyter-engine--output-cell
                                   mirror (plist-get cell :id)))
                           (revision (my/noema-jupyter-engine--revision
                                      (plist-get cell :code)))
                           (saved-revision (my/noema-jupyter-engine--get
                                            'revision saved))
                           (saved-runtime (my/noema-jupyter-engine--get
                                           'widgetRuntime saved))
                           (current-runtime (and state
                                                 (my/noema-jupyter-engine--widget-runtime
                                                  state)))
                           (widget-live
                            (and current-runtime saved-runtime
                                 (equal (my/noema-jupyter-engine--get
                                         'id saved-runtime)
                                        (my/noema-jupyter-engine--get
                                         'id current-runtime))
                                 (= (or (my/noema-jupyter-engine--get
                                         'generation saved-runtime) 1)
                                    (or (my/noema-jupyter-engine--get
                                         'generation current-runtime) 1)))))
                      `((id . ,(plist-get cell :id))
                        (index . ,(cl-position cell cells :test #'eq))
                        (line . ,(plist-get cell :line))
                        (revision . ,revision)
                        (code . ,(plist-get cell :code))
                        (stale . ,(if (and saved-revision
                                           (not (equal revision saved-revision)))
                                      t :json-false))
                        (status . ,(or (my/noema-jupyter-engine--get 'status saved)
                                       "idle"))
                        (executionCount . ,(my/noema-jupyter-engine--get
                                            'executionCount saved))
                        (outputs . ,(let ((outputs
                                          (my/noema-jupyter-engine--get
                                           'outputs saved)))
                                      (if (vectorp outputs) outputs
                                        (vconcat (or outputs nil)))))
                        (widgetMessages
                         . ,(let ((messages (my/noema-jupyter-engine--get
                                             'widgetMessages saved)))
                              (if (vectorp messages) messages
                                (vconcat (or messages nil)))))
                        (widgetOutputs . ,(or (my/noema-jupyter-engine--get
                                              'widgetOutputs saved) '()))
                        (widgetRuntime . ,(and widget-live current-runtime))
                        (live . ,(if widget-live t :json-false))
                        (outputUi . ,(or (my/noema-jupyter-engine--get 'ui saved)
                                        '())))))
                  cells))))))

(defun my/noema-jupyter-engine--write-json (file value)
  "Apply output projection VALUE to notebook FILE."
  (let ((document (my/noema-jupyter-notebook-read file)))
    (my/noema-jupyter-notebook-apply-output-mirror document value)
    (my/noema-jupyter-notebook-write file document)))

(defun my/noema-jupyter-engine--persist-result (metadata cell result)
  "Persist CELL RESULT in METADATA's notebook."
  (let* ((file (my/noema-jupyter-engine--output-file metadata))
         (mirror (or (my/noema-jupyter-engine--read-json file) '()))
         (old-cells (my/noema-jupyter-engine--get 'cells mirror))
         (cells (if (listp old-cells) (copy-tree old-cells) nil))
         (id (plist-get cell :id))
         (current (my/noema-jupyter-engine--output-cell mirror id))
         (ui (or (my/noema-jupyter-engine--get 'ui current) '()))
         (saved (append result
                        `((revision . ,(my/noema-jupyter-engine--revision
                                        (plist-get cell :code)))
                          (ui . ,ui)
                          (savedAt . ,(format-time-string "%FT%T.%3NZ" nil t))
                          (kernel . ,(plist-get metadata :kernel))
                          (session . ,(plist-get metadata :session))
                          (language . ,(plist-get metadata :language))))))
    (setq cells
          (cons (cons (intern id) saved)
                (cl-remove-if (lambda (entry)
                                (equal (format "%s" (car entry)) id))
                              cells)))
    (my/noema-jupyter-engine--write-json
     file
     `((version . 1)
       (source . ,(or (plist-get metadata :source-file) ""))
       (kernel . ,(plist-get metadata :kernel))
       (session . ,(plist-get metadata :session))
       (language . ,(plist-get metadata :language))
       (cells . ,cells)))))

(defun my/noema-jupyter-engine--publish (payload)
  "Publish Jupyter PAYLOAD to every Noema projection."
  (when-let* ((client (remote-gateway-find-client "aaronnote")))
    (remote-gateway-notify client "aaronnote.jupyter.publish" payload)))

(defun my/noema-jupyter-engine--state-key (metadata)
  "Return document-controller state key for METADATA."
  (expand-file-name (plist-get metadata :script-file)))

(defun my/noema-jupyter-engine--manager-session (state)
  "Return global Jupyter session associated with document STATE."
  (or (and state
           (gethash (my/noema-jupyter-document-manager-session-id state)
                    my/noema-jupyter-manager-sessions-by-id))
      (and state
           (my/noema-jupyter-manager-session
            (my/noema-jupyter-document-script-file state) t))))

(defun my/noema-jupyter-engine--manager-kernel (state)
  "Return global Jupyter kernel associated with document STATE."
  (my/noema-jupyter-manager-kernel-for-session
   (my/noema-jupyter-engine--manager-session state)))

(defun my/noema-jupyter-engine--client (state)
  "Return STATE's session client, or nil."
  (when-let* ((session (my/noema-jupyter-engine--manager-session state)))
    (my/noema-jupyter-session-client session)))

(defun my/noema-jupyter-engine--widget-runtime (state)
  "Return browser widget runtime metadata for live document STATE."
  (when-let* ((kernel (my/noema-jupyter-engine--manager-kernel state)))
    `((id . ,(my/noema-jupyter-kernel-id kernel))
      (name . ,(my/noema-jupyter-kernel-kernelspec kernel))
      (generation . ,(or (my/noema-jupyter-kernel-generation kernel) 1)))))

(defun my/noema-jupyter-engine--ensure-state (metadata)
  "Return a connected Emacs Jupyter state for METADATA."
  (let* ((state-key (my/noema-jupyter-engine--state-key metadata))
         (state (gethash state-key my/noema-jupyter-documents))
         (session (my/noema-jupyter-manager-ensure-session metadata)))
    (unless state
      (setq state
            (my/noema-jupyter-document-create
             :key state-key :script-file (plist-get metadata :script-file)
             :source-file (plist-get metadata :source-file)
             :language (plist-get metadata :language)
             :kernel (plist-get metadata :kernel)
             :session (plist-get metadata :session)
             :manager-session-id (my/noema-jupyter-session-id session)
             :running 0 :last-used (float-time)))
      (puthash state-key state my/noema-jupyter-documents))
    (setf (my/noema-jupyter-document-manager-session-id state)
          (my/noema-jupyter-session-id session)
          (my/noema-jupyter-document-kernel state)
          (my/noema-jupyter-session-kernelspec session))
    state))

(defun my/noema-jupyter-engine--live-set (identity outputs index)
  "Publish OUTPUTS at INDEX as a live patch for IDENTITY."
  (my/noema-jupyter-engine--publish
   (append identity
           `((phase . "events")
             (events . [((kind . "set") (index . ,index)
                         (output . ,(nth index outputs)))])))))

(defun my/noema-jupyter-engine--execute-one (metadata document cell callback)
  "Execute CELL through DOCUMENT and invoke CALLBACK with its result."
  (let* ((manager-session (my/noema-jupyter-engine--manager-session document))
         (client (my/noema-jupyter-engine--client document))
         (task (my/noema-jupyter-manager-task-start
                manager-session (plist-get cell :id)))
         (run-id (substring (secure-hash
                             'sha256
                             (format "%s:%s:%s" (float-time) (random)
                                     (plist-get cell :id))) 0 24))
         (identity `((key . ,(my/noema-jupyter-document-key document))
                     (runId . ,run-id)
                     (cellId . ,(plist-get cell :id))
                     (file . ,(or (plist-get metadata :source-file)
                                  (plist-get metadata :script-file)))
                     (scriptFile . ,(plist-get metadata :script-file))
                     (kernel . ,(plist-get metadata :kernel))
                     (session . ,(plist-get metadata :session))))
         (outputs nil) (widget-messages nil) execution-count
         (status "ok") (message "") finished)
    (cl-incf (my/noema-jupyter-document-running document))
    (setf (my/noema-jupyter-document-last-used document) (float-time))
    (my/noema-jupyter-engine--publish (append identity '((phase . "start"))))
    (cl-labels
        ((finish
          ()
          (unless finished
            (setq finished t)
            (remhash run-id my/noema-jupyter-pending-input)
            (cl-decf (my/noema-jupyter-document-running document))
            (my/noema-jupyter-manager-task-finish
             task (unless (equal status "ok") message))
            (let ((result
                   `((ok . ,(if (equal status "ok") t :json-false))
                     (cellId . ,(plist-get cell :id))
                     (kernel . ,(plist-get metadata :kernel))
                     (session . ,(plist-get metadata :session))
                     (status . ,status)
                     (executionCount . ,execution-count)
                     (outputs . ,(vconcat outputs))
                     (widgetMessages . ,(vconcat widget-messages))
                     (widgetRuntime . ,(my/noema-jupyter-engine--widget-runtime document))
                     (taskId . ,(my/noema-jupyter-task-id task))
                     (kernelId . ,(my/noema-jupyter-task-kernel-id task))
                     (sessionId . ,(my/noema-jupyter-task-session-id task))
                     (live . t)
                     ,@(when (not (string-empty-p message))
                         `((message . ,message))))))
              (my/noema-jupyter-engine--persist-result metadata cell result)
              (my/noema-jupyter-engine--publish
               (append identity
                       `((phase . "end") (status . ,status)
                         (executionCount . ,execution-count))))
              (funcall callback result))))
         (handle
          (msg)
          (let* ((type (jupyter-message-type msg))
                 (content (jupyter-message-content msg))
                 (json-content (my/noema-jupyter-engine--json-value content)))
            (pcase type
              ("status"
               (let ((state-name (or (plist-get content :execution_state) "")))
                 (my/noema-jupyter-engine--publish
                  (append identity
                          `((phase . "events")
                            (events . [((kind . "status")
                                        (state . ,state-name))]))))
                 (when (equal state-name "idle") (finish))))
              ("execute_input"
               (setq execution-count (plist-get content :execution_count))
               (my/noema-jupyter-engine--publish
                (append identity
                        `((phase . "events")
                          (events . [((kind . "executionCount")
                                      (value . ,execution-count))])))))
              ("stream"
               (let* ((name (or (plist-get content :name) "stdout"))
                      (text (or (plist-get content :text) ""))
                      (last (car (last outputs))))
                 (if (and last (equal (my/noema-jupyter-engine--get 'output_type last)
                                      "stream")
                          (equal (my/noema-jupyter-engine--get 'name last) name))
                     (setcdr (assoc 'text last)
                             (concat (or (my/noema-jupyter-engine--get 'text last) "") text))
                   (setq outputs
                         (append outputs
                                 (list `((output_type . "stream")
                                         (name . ,name) (text . ,text))))))
                 (my/noema-jupyter-engine--live-set
                  identity outputs (1- (length outputs)))))
              ((or "display_data" "execute_result" "update_display_data")
               (let ((output
                      `((output_type . ,type)
                        ,@(when (equal type "execute_result")
                            `((execution_count . ,(plist-get content :execution_count))))
                        (data . ,(my/noema-jupyter-engine--json-value
                                  (plist-get content :data)))
                        (metadata . ,(my/noema-jupyter-engine--json-value
                                      (plist-get content :metadata)))
                        ,@(when (plist-get content :transient)
                            `((transient . ,(my/noema-jupyter-engine--json-value
                                             (plist-get content :transient))))))))
                 (setq outputs (append outputs (list output)))
                 (my/noema-jupyter-engine--live-set
                  identity outputs (1- (length outputs)))))
              ("error"
               (setq status "error"
                     message (format "%s: %s"
                                     (or (plist-get content :ename) "Error")
                                     (or (plist-get content :evalue) "")))
               (setq outputs
                     (append outputs
                             (list `((output_type . "error")
                                     (ename . ,(plist-get content :ename))
                                     (evalue . ,(plist-get content :evalue))
                                     (traceback . ,(or (plist-get content :traceback)
                                                       []))))))
               (my/noema-jupyter-engine--live-set
                identity outputs (1- (length outputs))))
              ("clear_output"
               (setq outputs nil)
               (my/noema-jupyter-engine--publish
                (append identity
                        '((phase . "events")
                          (events . [((kind . "clear"))])))))
              ("execute_reply"
               (unless (equal (plist-get content :status) "ok")
                 (setq status "error"
                       message (or (plist-get content :evalue) message)))
               (setq execution-count
                     (or (plist-get content :execution_count) execution-count)))
              ("input_request"
               (puthash run-id client my/noema-jupyter-pending-input)
               (my/noema-jupyter-engine--publish
                (append identity
                        `((phase . "stdin")
                          (prompt . ,(or (plist-get content :prompt) ""))
                          (password . ,(if (eq (plist-get content :password) t)
                                           t :json-false))))))
              ((or "comm_open" "comm_msg" "comm_close")
               (setq widget-messages
                     (append widget-messages
                             (list
                              `((channel . "iopub")
                                (header . ,(my/noema-jupyter-engine--json-value
                                            (jupyter-message-header msg)))
                                (parent_header
                                 . ,(my/noema-jupyter-engine--json-value
                                     (jupyter-message-parent-header msg)))
                                (metadata . ,(my/noema-jupyter-engine--json-value
                                              (jupyter-message-metadata msg)))
                                (content . ,json-content))))))))))
      (condition-case-unless-debug error
          (let* ((handlers
                  (mapcar (lambda (type) (list type #'handle))
                          '("status" "execute_input" "stream" "display_data"
                            "execute_result" "update_display_data" "error"
                            "clear_output" "execute_reply" "input_request"
                            "comm_open" "comm_msg" "comm_close"))))
            (jupyter-run-with-client client
              (jupyter-sent
               (jupyter-message-subscribed
                (jupyter-execute-request
                 :code (plist-get cell :code)
                 :store-history t :allow-stdin t :stop-on-error t
                 :handlers nil)
                handlers))))
        (error
         (setq status "error" message (error-message-string error))
         (finish))))))

(defun my/noema-jupyter-engine--execution-plan (params cells)
  "Return CELLS selected by PARAMS using JupyterLab command semantics."
  (let* ((target-id (my/noema-jupyter-engine--token
                     (or (my/noema-jupyter-engine--get 'cellId params)
                         (my/noema-jupyter-engine--get 'id params)) ""))
         (target-index (cl-position target-id cells
                                    :key (lambda (cell) (plist-get cell :id))
                                    :test #'equal))
         (mode (my/noema-jupyter-engine--token
                (or (my/noema-jupyter-engine--get 'mode params)
                    (my/noema-jupyter-engine--get 'runMode params)
                    (my/noema-jupyter-engine--get 'executionMode params))
                "current"))
         (selected (append (or (my/noema-jupyter-engine--get 'cellIds params)
                               (my/noema-jupyter-engine--get 'selectedCellIds params)
                               []) nil)))
    (unless (or target-index (member mode '("all" "selected")))
      (error "Jupyter cell not found: %s" target-id))
    (pcase mode
      ("current"
       (list (nth target-index cells)))
      ("all" cells)
      ("above" (seq-take cells (1+ target-index)))
      ("below" (seq-drop cells target-index))
      ("selected"
       (seq-filter (lambda (cell) (member (plist-get cell :id) selected)) cells))
      (_ (error "Unsupported Jupyter execution mode: %s" mode)))))

(defun my/noema-jupyter-engine-execute (params callback)
  "Execute cells described by PARAMS and invoke CALLBACK with (RESULT ERROR)."
  (run-at-time
   0 nil
   (lambda ()
     (condition-case-unless-debug error
         (let* ((metadata (my/noema-jupyter-engine--metadata params))
                (cells (my/noema-jupyter-engine--cells
                        (plist-get metadata :script-file)))
                (plan (my/noema-jupyter-engine--execution-plan params cells))
                (state (my/noema-jupyter-engine--ensure-state metadata))
                results)
           (cl-labels
               ((next
                 (remaining)
                 (if (null remaining)
                     (let* ((requested-id
                             (my/noema-jupyter-engine--get 'cellId params))
                            (target
                             (or (seq-find
                                  (lambda (result)
                                    (equal (my/noema-jupyter-engine--get
                                            'cellId result) requested-id))
                                  results)
                                 (car (last results))))
                            (aggregate-status
                             (if (seq-some
                                  (lambda (result)
                                    (equal (my/noema-jupyter-engine--get
                                            'status result) "error"))
                                  results)
                                 "error" "ok")))
                       (funcall callback
                                `((ok . t)
                                  (status . ,aggregate-status)
                                  (cellId . ,(my/noema-jupyter-engine--get
                                              'cellId target))
                                  (kernel . ,(plist-get metadata :kernel))
                                  (session . ,(plist-get metadata :session))
                                  (executionCount
                                   . ,(my/noema-jupyter-engine--get
                                       'executionCount target))
                                  (outputs . ,(or (my/noema-jupyter-engine--get
                                                  'outputs target) []))
                                  (widgetMessages
                                   . ,(or (my/noema-jupyter-engine--get
                                           'widgetMessages target) []))
                                  (widgetOutputs
                                   . ,(or (my/noema-jupyter-engine--get
                                           'widgetOutputs target) '()))
                                  (widgetRuntime
                                   . ,(my/noema-jupyter-engine--get
                                       'widgetRuntime target))
                                  (live . t)
                                  ,@(when-let* ((message
                                                 (my/noema-jupyter-engine--get
                                                  'message target)))
                                      `((message . ,message)))
                                  (results . ,(vconcat results))
                                  (plan . ,(vconcat
                                            (mapcar
                                             (lambda (cell)
                                               `((cellId . ,(plist-get cell :id))
                                                 (mode . ,(or
                                                           (my/noema-jupyter-engine--get
                                                            'mode params)
                                                           "current"))))
                                             plan))))
                                nil))
                   (my/noema-jupyter-engine--execute-one
                    metadata state (car remaining)
                    (lambda (result)
                      (setq results (append results (list result)))
                      (if (equal (my/noema-jupyter-engine--get 'status result) "error")
                          (next nil)
                        (next (cdr remaining))))))))
             (if plan (next plan)
               (funcall callback '((ok . t) (status . "ok")
                                    (results . []) (plan . [])) nil))))
       (error (funcall callback nil (error-message-string error)))))))

(defun my/noema-jupyter-engine--gateway-execute (params _client)
  "Gateway wrapper for an asynchronous document execution request."
  (let ((deferred (remote-gateway-defer 3600)))
    (my/noema-jupyter-engine-execute
     params
     (lambda (result error)
       (if error
           (remote-gateway-reject deferred -32603 error)
         (remote-gateway-resolve deferred result))))
    deferred))

(defun my/noema-jupyter-engine--gateway-snapshot (params _client)
  "Gateway document snapshot method."
  (my/noema-jupyter-engine-document-snapshot params))

(defun my/noema-jupyter-engine--state-for-params (params &optional require-live)
  "Return document state for PARAMS, optionally REQUIRE-LIVE."
  (let* ((metadata (my/noema-jupyter-engine--metadata params))
         (state (gethash (my/noema-jupyter-engine--state-key metadata)
                         my/noema-jupyter-documents)))
    (when (and require-live
               (not (and state (my/noema-jupyter-engine--client state))))
      (error "No running Emacs Jupyter kernel for this document"))
    (cons metadata state)))

(defun my/noema-jupyter-engine-introspect (kind params &optional timeout)
  "Send a non-launching Jupyter introspection request of KIND for PARAMS.
Return a gateway-safe alist.  TIMEOUT defaults to 1.5 seconds."
  (pcase-let* ((`(,_metadata . ,state)
                 (my/noema-jupyter-engine--state-for-params params nil)))
    (if (not (and state (my/noema-jupyter-engine--client state)))
        `((ok . t) (supported . :json-false)
          ,@(pcase kind
              ('complete '((matches . []) (items . [])
                            (cursorStart . 0) (cursorEnd . 0)))
              ('inspect '((found . :json-false) (data . ()) (metadata . ())))
              ('is-complete '((status . "unknown") (indent . "")))
              ('history '((history . [])))
              ('comm-info '((comms . ())))))
      (let* ((client (my/noema-jupyter-engine--client state))
             (code (format "%s" (or (my/noema-jupyter-engine--get 'code params) "")))
             (pos (truncate (or (my/noema-jupyter-engine--get 'cursorPos params) 0)))
             (request
              (pcase kind
                ('complete (jupyter-complete-request :code code :pos pos :handlers nil))
                ('inspect (jupyter-inspect-request
                           :code code :pos pos
                           :detail (if (= (or (my/noema-jupyter-engine--get
                                               'detailLevel params) 0) 1) 1 0)
                           :handlers nil))
                ('is-complete (jupyter-is-complete-request :code code :handlers nil))
                ('history (jupyter-history-request
                           :output t :raw t :hist-access-type "tail"
                           :n (truncate (or (my/noema-jupyter-engine--get 'n params) 20))
                           :handlers nil))
                ('comm-info (jupyter-comm-info-request
                             :target-name (format "%s" (or (my/noema-jupyter-engine--get
                                                             'targetName params) ""))
                             :handlers nil))
                (_ (error "Unsupported Jupyter introspection kind: %s" kind))))
             (reply
              (condition-case nil
                  (jupyter-run-with-client client
                    (jupyter-reply request (or timeout 1.5)))
                (error nil))))
        (if (not reply)
            '((ok . t) (supported . t) (timedOut . t))
          (let ((content (jupyter-message-content reply)))
            (pcase kind
              ('complete
               `((ok . t) (supported . t)
                 (matches . ,(or (plist-get content :matches) []))
                 (items . [])
                 (cursorStart . ,(or (plist-get content :cursor_start) 0))
                 (cursorEnd . ,(or (plist-get content :cursor_end) pos))))
              ('inspect
               `((ok . t) (supported . t)
                 (found . ,(if (eq (plist-get content :found) t) t :json-false))
                 (data . ,(my/noema-jupyter-engine--json-value
                           (plist-get content :data)))
                 (metadata . ,(my/noema-jupyter-engine--json-value
                               (plist-get content :metadata)))))
              ('is-complete
               `((ok . t) (supported . t)
                 (status . ,(or (plist-get content :status) "unknown"))
                 (indent . ,(or (plist-get content :indent) ""))))
              ('history
               `((ok . t) (supported . t)
                 (history . ,(my/noema-jupyter-engine--json-value
                               (or (plist-get content :history) [])))))
              ('comm-info
               `((ok . t) (supported . t)
                 (comms . ,(my/noema-jupyter-engine--json-value
                             (or (plist-get content :comms) '()))))))))))))

(defun my/noema-jupyter-engine--gateway-introspect (kind params)
  "Gateway wrapper for introspection KIND and PARAMS."
  (my/noema-jupyter-engine-introspect kind params 3.0))

(defun my/noema-jupyter-engine-variables (params)
  "Return a passive variable snapshot for the live kernel in PARAMS."
  (let* ((pair (my/noema-jupyter-engine--state-for-params params nil))
         (metadata (car pair))
         (state (cdr pair))
         (kernel (plist-get metadata :kernel)))
    (cond
     ((not (string-match-p "python\\|sage" (downcase kernel)))
      `((ok . t) (supported . :json-false) (kernel . ,kernel)
        (variables . [])))
     ((not (and state (my/noema-jupyter-engine--client state)))
      `((ok . t) (supported . :json-false) (kernel . ,kernel)
        (variables . [])))
     (t
      (let* ((client (my/noema-jupyter-engine--client state))
             ;; Base64 makes Python's repr of the result unambiguous to
             ;; decode on the Emacs side while keeping this a history-free
             ;; execute request.
             (code
              (concat
               "__import__('base64').b64encode(__import__('json').dumps(["
               "{'name': n, 'type': type(v).__name__, "
               "'summary': (repr(v)[:157] + '...' if len(repr(v)) > 160 else repr(v)), "
               "'shape': (list(getattr(v, 'shape')) if hasattr(v, 'shape') else None)} "
               "for n, v in sorted(globals().items()) "
               "if not n.startswith('_') and n not in "
               "('In','Out','get_ipython','exit','quit') and type(v).__name__ != 'module'], "
               "default=str).encode()).decode()"))
             (raw (let ((jupyter-current-client client))
                    (jupyter-eval code)))
             (encoded (string-trim (format "%s" (or raw "")))))
        (when (string-match "\\`['\"]\\([A-Za-z0-9+/=]+\\)['\"]\\'" encoded)
          (setq encoded (match-string 1 encoded)))
        (condition-case err
            (let ((variables
                   (json-parse-string
                    (decode-coding-string
                     (base64-decode-string encoded) 'utf-8)
                    :object-type 'alist :array-type 'list
                    :null-object nil :false-object :json-false)))
              `((ok . t) (supported . t) (kernel . ,kernel)
                (session . ,(plist-get metadata :session))
                (variables . ,(vconcat variables))))
          (error
           `((ok . :json-false) (supported . t) (kernel . ,kernel)
             (variables . [])
             (message . ,(error-message-string err))))))))))

(defun my/noema-jupyter-engine--gateway-input (params _client)
  "Reply to a pending kernel stdin request."
  (let* ((run-id (my/noema-jupyter-engine--token
                  (my/noema-jupyter-engine--get 'runId params) ""))
         (client (gethash run-id my/noema-jupyter-pending-input)))
    (unless client (error "Jupyter input request is no longer active"))
    (remhash run-id my/noema-jupyter-pending-input)
    (if (eq (my/noema-jupyter-engine--get 'cancel params) t)
        (jupyter-interrupt-kernel client)
      (jupyter-run-with-client client
        (jupyter-sent
         (jupyter-input-reply
          :value (format "%s" (or (my/noema-jupyter-engine--get 'value params) ""))
          :handlers nil))))
    '((ok . t))))

(defun my/noema-jupyter-engine--gateway-control (action params)
  "Apply kernel ACTION to the state described by PARAMS."
  (pcase-let* ((`(,_metadata . ,state)
                 (my/noema-jupyter-engine--state-for-params params t))
                (session (my/noema-jupyter-engine--manager-session state))
                (kernel-id (my/noema-jupyter-session-kernel-id session)))
    (my/noema-jupyter-manager-control kernel-id action)
    `((ok . t) (action . ,(symbol-name action))
      (kernelId . ,kernel-id)
      (sessionId . ,(my/noema-jupyter-session-id session)))))

(defun my/noema-jupyter-engine--gateway-kernel-channel (params _client)
  "Return the local forwarded channel for opaque manager kernel PARAMS."
  (let* ((id (my/noema-jupyter-engine--token
              (or (my/noema-jupyter-engine--get 'id params)
                  (my/noema-jupyter-engine--get 'kernelId params)) ""))
         (kernel (gethash id my/noema-jupyter-manager-kernels))
         (runtime (and kernel (my/noema-jupyter-kernel-runtime kernel))))
    (unless (and kernel runtime)
      (error "Jupyter widget runtime is not live"))
    (setf (my/noema-jupyter-kernel-last-used kernel) (float-time))
    `((kind . "zmq")
      (connectionInfo
       . ,(my/noema-jupyter-runtime-client-connection
           runtime)))))

(defun my/noema-jupyter-engine--clear-output (params &optional all)
  "Clear current cell output, or ALL outputs, described by PARAMS."
  (let* ((metadata (my/noema-jupyter-engine--metadata params))
         (file (my/noema-jupyter-engine--output-file metadata))
         (mirror (or (my/noema-jupyter-engine--read-json file) '()))
         (cells (copy-tree (or (my/noema-jupyter-engine--get 'cells mirror) nil)))
         (id (my/noema-jupyter-engine--token
              (or (my/noema-jupyter-engine--get 'cellId params)
                  (my/noema-jupyter-engine--get 'id params)) "")))
    (unless all
      (setq cells (cl-remove-if (lambda (entry)
                                  (equal (format "%s" (car entry)) id))
                                cells)))
    (my/noema-jupyter-engine--write-json
     file `((version . 1)
            (source . ,(or (plist-get metadata :source-file) ""))
            (kernel . ,(plist-get metadata :kernel))
            (session . ,(plist-get metadata :session))
            (language . ,(plist-get metadata :language))
            (cells . ,(if all nil cells))))
    `((ok . t) (cellId . ,id))))

(defun my/noema-jupyter-engine--comment-prefix (language)
  "Return line comment prefix for LANGUAGE."
  (cond
   ((member (downcase language)
            '("javascript" "typescript" "c" "cpp" "java" "rust" "go"
              "swift" "kotlin" "csharp")) "//")
   ((member (downcase language) '("sql" "lean" "lean4")) "--")
   ((member (downcase language) '("elisp" "lisp" "scheme" "clojure")) ";")
   (t "#")))

(defun my/noema-jupyter-engine--new-cell-id ()
  "Return a stable new cell identifier."
  (format "cell-%s"
          (substring
           (secure-hash 'sha256
                        (format "%s:%s:%s" (float-time) (random) (emacs-pid)))
           0 12)))

(defun my/noema-jupyter-engine--cell-block (metadata id &optional code)
  "Return a generated cell block for ID and CODE using METADATA."
  (let ((prefix (my/noema-jupyter-engine--comment-prefix
                 (plist-get metadata :language))))
    (format "%s %%%% id=%s\n%s%s"
            prefix id (or code "")
            (if (or (null code) (string-suffix-p "\n" code)) "" "\n"))))

(defun my/noema-jupyter-engine--source-marker (metadata id)
  "Return the Markdown marker for ID described by METADATA."
  (format "@@cell(%s, %s) [%s]"
          (plist-get metadata :language)
          (plist-get metadata :session)
          id))

(defun my/noema-jupyter-engine--source-marker-bounds (id)
  "Return line bounds for Markdown Cell ID in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^[ \t]*@@cell[^\n]*\\[%s\\][ \t]*$"
                   (regexp-quote id)) nil t)
      (cons (line-beginning-position)
            (min (point-max) (1+ (line-end-position)))))))

(defun my/noema-jupyter-engine--projection-buffer (metadata)
  "Return writable owner-note buffer for METADATA, or nil."
  (when-let* ((source (plist-get metadata :source-file))
              ((string-match-p "\\.\\(?:md\\|markdown\\|mdown\\|mkd\\)\\'" source))
              ((file-readable-p source)))
    (let ((buffer (or (find-buffer-visiting source) (find-file-noselect source))))
      (when (buffer-modified-p buffer)
        (error "Owner note has unsaved edits; save it before changing Cell structure"))
      buffer)))

(defun my/noema-jupyter-engine--patch-projection
    (metadata operation anchor-id active-id peer-id)
  "Apply structural OPERATION to the owner Markdown projection.
ANCHOR-ID is the original selected cell, ACTIVE-ID the resulting selection,
and PEER-ID the neighboring cell involved in a move or merge."
  (when-let* ((buffer (my/noema-jupyter-engine--projection-buffer metadata)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (anchor (my/noema-jupyter-engine--source-marker-bounds anchor-id))
            (peer (and peer-id
                       (my/noema-jupyter-engine--source-marker-bounds peer-id))))
        (pcase operation
          ((or "insertAbove" "insertBelow" "duplicate" "split")
           (let ((marker (concat (my/noema-jupyter-engine--source-marker
                                  metadata active-id) "\n")))
             (if anchor
                 (goto-char (if (equal operation "insertAbove")
                                (car anchor) (cdr anchor)))
               (goto-char (point-max))
               (unless (bolp) (insert "\n")))
             (insert marker)))
          ("delete"
           (when anchor (delete-region (car anchor) (cdr anchor))))
          ((or "moveUp" "moveDown")
           (when (and anchor peer)
             (let* ((first (if (< (car anchor) (car peer)) anchor peer))
                    (second (if (< (car anchor) (car peer)) peer anchor))
                    (first-text (buffer-substring-no-properties
                                 (car first) (cdr first)))
                    (second-text (buffer-substring-no-properties
                                  (car second) (cdr second))))
               ;; Exchange only marker lines: prose between Cell projections
               ;; belongs to the note and must not move with the marker.
               (goto-char (car second))
               (delete-region (car second) (cdr second))
               (insert first-text)
               (goto-char (car first))
               (delete-region (car first) (cdr first))
               (insert second-text))))
          ((or "mergeAbove" "mergeBelow")
           (let ((removed (if (equal active-id anchor-id) peer anchor)))
             (when removed (delete-region (car removed) (cdr removed))))))
        (when (buffer-modified-p) (save-buffer))))))

(defun my/noema-jupyter-engine--delete-saved-cell (metadata id)
  "Remove ID's saved output from METADATA's notebook."
  (my/noema-jupyter-engine--clear-output
   `((scriptFile . ,(plist-get metadata :script-file))
     (cellId . ,id)
     (kernel . ,(plist-get metadata :kernel))
     (session . ,(plist-get metadata :session))
     (language . ,(plist-get metadata :language))) nil))

(defun my/noema-jupyter-engine-document-mutate (params)
  "Apply a structural Cell mutation described by PARAMS."
  (let* ((metadata (my/noema-jupyter-engine--metadata params))
         (script (plist-get metadata :script-file))
         (operation (my/noema-jupyter-engine--token
                     (my/noema-jupyter-engine--get 'op params) ""))
         (anchor-id (my/noema-jupyter-engine--token
                     (or (my/noema-jupyter-engine--get 'cellId params)
                         (my/noema-jupyter-engine--get 'id params)) ""))
         (cells (my/noema-jupyter-engine--cells script))
         (index (cl-position anchor-id cells
                             :key (lambda (cell) (plist-get cell :id))
                             :test #'equal))
         (anchor (and index (nth index cells)))
         (new-id (my/noema-jupyter-engine--new-cell-id))
         active-id peer-id removed-id)
    (unless anchor (error "Jupyter cell not found: %s" anchor-id))
    ;; Validate owner-note conflicts before changing the script.
    (my/noema-jupyter-engine--projection-buffer metadata)
    (with-current-buffer (or (find-buffer-visiting script)
                             (find-file-noselect script))
      (let ((inhibit-read-only t))
        (pcase operation
          ("insertAbove"
           (goto-char (plist-get anchor :block-beg))
           (insert (my/noema-jupyter-engine--cell-block metadata new-id))
           (setq active-id new-id))
          ("insertBelow"
           (goto-char (plist-get anchor :block-end))
           (insert (my/noema-jupyter-engine--cell-block metadata new-id))
           (setq active-id new-id))
          ("duplicate"
           (goto-char (plist-get anchor :block-end))
           (insert (my/noema-jupyter-engine--cell-block
                    metadata new-id (plist-get anchor :code)))
           (setq active-id new-id))
          ("delete"
           (setq active-id
                 (or (plist-get (nth (1+ index) cells) :id)
                     (and (> index 0)
                          (plist-get (nth (1- index) cells) :id))
                     ""))
           (delete-region (plist-get anchor :block-beg)
                          (plist-get anchor :block-end))
           (setq removed-id anchor-id))
          ((or "moveUp" "moveDown")
           (let* ((peer-index (if (equal operation "moveUp")
                                  (1- index) (1+ index)))
                  (peer (and (>= peer-index 0) (< peer-index (length cells))
                             (nth peer-index cells))))
             (when peer
               (setq peer-id (plist-get peer :id))
               (let* ((first (if (< (plist-get anchor :block-beg)
                                    (plist-get peer :block-beg)) anchor peer))
                      (second (if (eq first anchor) peer anchor))
                      (first-text (buffer-substring-no-properties
                                   (plist-get first :block-beg)
                                   (plist-get first :block-end)))
                      (second-text (buffer-substring-no-properties
                                    (plist-get second :block-beg)
                                    (plist-get second :block-end))))
                 (goto-char (plist-get first :block-beg))
                 (delete-region (plist-get first :block-beg)
                                (plist-get second :block-end))
                 (insert second-text first-text)))
             (setq active-id anchor-id)))
          ((or "mergeAbove" "mergeBelow")
           (let* ((peer-index (if (equal operation "mergeAbove")
                                  (1- index) (1+ index)))
                  (peer (and (>= peer-index 0) (< peer-index (length cells))
                             (nth peer-index cells))))
             (unless peer (error "No Cell available to merge"))
             (setq peer-id (plist-get peer :id))
             (let* ((keep (if (equal operation "mergeAbove") peer anchor))
                    (drop (if (eq keep anchor) peer anchor))
                    (combined (concat (string-trim-right (plist-get keep :code))
                                      "\n"
                                      (string-trim-left (plist-get drop :code)))))
               (goto-char (plist-get keep :body-beg))
               (delete-region (plist-get keep :body-beg)
                              (plist-get keep :body-end))
               (insert combined)
               (unless (string-suffix-p "\n" combined) (insert "\n"))
               ;; Reparse because editing KEEP may have shifted DROP.
               (let ((fresh-drop
                      (seq-find (lambda (cell)
                                  (equal (plist-get cell :id)
                                         (plist-get drop :id)))
                                (my/noema-jupyter-engine--cells script))))
                 (delete-region (plist-get fresh-drop :block-beg)
                                (plist-get fresh-drop :block-end)))
               (setq active-id (plist-get keep :id)
                     removed-id (plist-get drop :id)))))
          ("split"
           (let* ((offset (max 0 (min (length (plist-get anchor :code))
                                      (truncate (or (my/noema-jupyter-engine--get
                                                     'offset params) 0)))))
                  (code (plist-get anchor :code))
                  (left (substring code 0 offset))
                  (right (substring code offset)))
             (goto-char (plist-get anchor :body-beg))
             (delete-region (plist-get anchor :body-beg)
                            (plist-get anchor :body-end))
             (insert left)
             (unless (string-suffix-p "\n" left) (insert "\n"))
             (let ((fresh-anchor
                    (seq-find (lambda (cell)
                                (equal (plist-get cell :id) anchor-id))
                              (my/noema-jupyter-engine--cells script))))
               (goto-char (plist-get fresh-anchor :block-end))
               (insert (my/noema-jupyter-engine--cell-block metadata new-id right)))
             (setq active-id new-id)))
          (_ (error "Unsupported Cell mutation: %s" operation)))
        (when (buffer-modified-p) (save-buffer))))
    (when removed-id (my/noema-jupyter-engine--delete-saved-cell metadata removed-id))
    (my/noema-jupyter-engine--patch-projection
     metadata operation anchor-id (or active-id anchor-id) peer-id)
    (let ((snapshot (my/noema-jupyter-engine-document-snapshot
                     `((scriptFile . ,script)
                       (kernel . ,(plist-get metadata :kernel))
                       (session . ,(plist-get metadata :session))
                       (language . ,(plist-get metadata :language))))))
      (append `((ok . t) (operation . ,operation)
                (activeCellId . ,(or active-id anchor-id))
                ,@(when (and new-id (member operation
                                             '("insertAbove" "insertBelow"
                                               "duplicate" "split")))
                    `((newCellId . ,new-id))))
              snapshot))))

(defun my/noema-jupyter-engine--save-output-ui (params)
  "Persist output folding state described by PARAMS."
  (let* ((metadata (my/noema-jupyter-engine--metadata params))
         (file (my/noema-jupyter-engine--output-file metadata))
         (mirror (or (my/noema-jupyter-engine--read-json file) '()))
         (cells (copy-tree (or (my/noema-jupyter-engine--get 'cells mirror) nil)))
         (id (my/noema-jupyter-engine--token
              (my/noema-jupyter-engine--get 'cellId params) ""))
         (current (or (my/noema-jupyter-engine--output-cell mirror id)
                      '((ok . t) (status . "idle") (outputs . []))))
         (next (cons
                (cons 'ui
                      `((outputFolded . ,(if (eq (my/noema-jupyter-engine--get
                                                  'outputFolded params) t)
                                             t :json-false))
                        (outputExpanded . ,(if (eq (my/noema-jupyter-engine--get
                                                    'outputExpanded params) t)
                                               t :json-false))))
                (cl-remove-if (lambda (entry) (equal (format "%s" (car entry)) "ui"))
                              current))))
    (setq cells (cons (cons (intern id) next)
                      (cl-remove-if (lambda (entry)
                                      (equal (format "%s" (car entry)) id))
                                    cells)))
    (my/noema-jupyter-engine--write-json
     file `((version . 1)
            (source . ,(or (plist-get metadata :source-file) ""))
            (kernel . ,(plist-get metadata :kernel))
            (session . ,(plist-get metadata :session))
            (language . ,(plist-get metadata :language))
            (cells . ,cells)))
    `((ok . t) (cellId . ,id) (output . ,next))))

(defun my/noema-jupyter-engine--gateway-tasks (_params _client)
  "Return the passive global Emacs Jupyter manager snapshot."
  (my/noema-jupyter-manager-snapshot))

(defun my/noema-jupyter-engine--controller-buffer (params)
  "Return the authoritative script controller buffer described by PARAMS."
  (let ((script
         (my/noema-jupyter-engine--token
          (or (my/noema-jupyter-engine--get 'scriptFile params)
              (my/noema-jupyter-engine--get 'file params)) "")))
    (unless (and (not (string-empty-p script))
                 (string-match-p "\\(?:\\`\\|/\\)\\.cell/[^/]+\\.ipynb\\'" script))
      (error "Missing or invalid Jupyter scriptFile"))
    (let ((buffer (or (find-buffer-visiting script)
                      (find-file-noselect script))))
      (with-current-buffer buffer
        (unless (bound-and-true-p my/noema-jupyter-cell-mode)
          (my/noema-jupyter-cell-activate-buffer)))
      buffer)))

(defun my/noema-jupyter-engine--gateway-script-action (params _client)
  "Dispatch PARAMS action through its script buffer controller."
  (let* ((buffer (my/noema-jupyter-engine--controller-buffer params))
         (action (my/noema-jupyter-engine--token
                  (my/noema-jupyter-engine--get 'action params) ""))
         (cell-id (my/noema-jupyter-engine--token
                   (my/noema-jupyter-engine--get 'cellId params) "")))
    (with-current-buffer buffer
      (when (and (not (string-empty-p cell-id))
                 (fboundp 'my/noema-jupyter-cell--goto-id))
        (my/noema-jupyter-cell--goto-id cell-id))
      (pcase action
        ((or "run" "run-current" "run-above" "run-below" "run-all"
             "run-selected")
         (let* ((mode (pcase action
                        ((or "run" "run-current") "current")
                        ("run-above" "above")
                        ("run-below" "below")
                        ("run-selected" "selected")
                        (_ "all")))
                (deferred (remote-gateway-defer 3600))
                (engine-params
                 (append
                  (my/noema-jupyter-cell--engine-params
                   (unless (string-empty-p cell-id) cell-id)
                   (not (equal mode "all")))
                  `((mode . ,mode))
                  (when-let* ((ids (or (my/noema-jupyter-engine--get
                                        'cellIds params)
                                       (my/noema-jupyter-engine--get
                                        'selectedCellIds params))))
                    `((cellIds . ,ids))))))
           (my/noema-jupyter-engine-execute
            engine-params
            (lambda (result error)
              (if error
                  (remote-gateway-reject deferred -32603 error)
                (remote-gateway-resolve deferred result))))
           deferred))
        ((or "insertAbove" "insertBelow" "duplicate" "delete" "moveUp"
             "moveDown" "split" "mergeAbove" "mergeBelow")
         (my/noema-jupyter-engine-document-mutate
          (append (my/noema-jupyter-cell--engine-params cell-id t)
                  `((op . ,action))
                  (when-let* ((offset (my/noema-jupyter-engine--get
                                       'offset params)))
                    `((offset . ,offset))))))
        ("clear-output"
         (my/noema-jupyter-engine--clear-output
          (my/noema-jupyter-cell--engine-params cell-id t) nil))
        ("clear-all-outputs"
         (my/noema-jupyter-engine--clear-output
          (my/noema-jupyter-cell--engine-params nil nil) t))
        ((or "interrupt" "restart" "shutdown")
         (my/noema-jupyter-engine--gateway-control
          (intern action)
          (my/noema-jupyter-cell--engine-params nil nil)))
        (_ (error "Unsupported Jupyter script action: %s" action))))))

(defun my/noema-jupyter-engine--gateway-session-select (params _client)
  "Apply a JupyterLab-style kernel selection for PARAMS script session."
  (let* ((buffer (my/noema-jupyter-engine--controller-buffer params))
         (kind (intern
                (my/noema-jupyter-engine--token
                 (my/noema-jupyter-engine--get 'kind params) "none")))
         selection session)
    (with-current-buffer buffer
      (setq session
            (my/noema-jupyter-manager-session
             buffer-file-name nil
             (my/noema-jupyter-engine--metadata
              (my/noema-jupyter-cell--engine-params nil nil))))
      (setq selection
            (pcase kind
              ('start
               (let ((spec (my/noema-jupyter-engine--token
                            (or (my/noema-jupyter-engine--get
                                 'kernelSpecName params)
                                (my/noema-jupyter-engine--get 'kernel params)) "")))
                 (my/noema-jupyter-cell--set-kernelspec-header spec)
                 (list :kind 'start :kernelspec spec)))
              ('connect
               (let* ((id (my/noema-jupyter-engine--token
                           (my/noema-jupyter-engine--get 'kernelId params) ""))
                      (kernel (or (gethash id my/noema-jupyter-manager-kernels)
                                  (error "Unknown Jupyter kernel: %s" id))))
                 (my/noema-jupyter-cell--set-kernelspec-header
                  (my/noema-jupyter-kernel-kernelspec kernel))
                 (list :kind 'connect :kernel-id id)))
              ('none (list :kind 'none))
              (_ (error "Unsupported Jupyter session selection: %s" kind))))
      (my/noema-jupyter-manager-select session selection t)
      (my/noema-jupyter-engine-document-snapshot
       (my/noema-jupyter-cell--engine-params nil nil)))))

(defun my/noema-jupyter-engine--gateway-kernel-control (params _client)
  "Control an opaque global kernel described by PARAMS."
  (let* ((id (my/noema-jupyter-engine--token
              (my/noema-jupyter-engine--get 'kernelId params) ""))
         (action (intern
                  (my/noema-jupyter-engine--token
                   (my/noema-jupyter-engine--get 'action params) ""))))
    (my/noema-jupyter-manager-control id action)
    `((ok . t) (kernelId . ,id) (action . ,(symbol-name action)))))

(dolist (entry
         `(("aaronnote.jupyter.document.snapshot" .
            ,#'my/noema-jupyter-engine--gateway-snapshot)
           ("aaronnote.jupyter.manager.snapshot" .
            ,#'my/noema-jupyter-engine--gateway-tasks)
           ("aaronnote.jupyter.script.snapshot" .
            ,#'my/noema-jupyter-engine--gateway-snapshot)
           ("aaronnote.jupyter.script.action" .
            ,#'my/noema-jupyter-engine--gateway-script-action)
           ("aaronnote.jupyter.session.select" .
            ,#'my/noema-jupyter-engine--gateway-session-select)
           ("aaronnote.jupyter.kernel.control" .
            ,#'my/noema-jupyter-engine--gateway-kernel-control)
           ("aaronnote.jupyter.document.execute" .
            ,#'my/noema-jupyter-engine--gateway-execute)
           ("aaronnote.jupyter.document.mutate" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine-document-mutate params)))
           ("aaronnote.jupyter.stdin.reply" .
            ,#'my/noema-jupyter-engine--gateway-input)
           ("aaronnote.jupyter.output.clear" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--clear-output params nil)))
           ("aaronnote.jupyter.output.clear-all" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--clear-output params t)))
           ("aaronnote.jupyter.output.save-ui" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--save-output-ui params)))
           ("aaronnote.jupyter.introspect.complete" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-introspect 'complete params)))
           ("aaronnote.jupyter.introspect.inspect" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-introspect 'inspect params)))
           ("aaronnote.jupyter.introspect.is-complete" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-introspect 'is-complete params)))
           ("aaronnote.jupyter.introspect.history" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-introspect 'history params)))
           ("aaronnote.jupyter.introspect.comm-info" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-introspect 'comm-info params)))
           ("aaronnote.jupyter.variables" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine-variables params)))
           ("aaronnote.jupyter.kernel.interrupt" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-control 'interrupt params)))
           ("aaronnote.jupyter.kernel.restart" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-control 'restart params)))
           ("aaronnote.jupyter.kernel.shutdown" .
            ,(lambda (params _client)
               (my/noema-jupyter-engine--gateway-control 'shutdown params)))
           ("aaronnote.jupyter.kernel.channel" .
            ,#'my/noema-jupyter-engine--gateway-kernel-channel)
           ("aaronnote.jupyter.board.open" .
            ,(lambda (_params _client)
               (require 'init-jupyter-board)
               (my/jupyter-board-open)
               '((ok . t))))
           ("aaronnote.jupyter.tasks" . ,#'my/noema-jupyter-engine--gateway-tasks)))
  (remote-gateway-register-method (car entry) (cdr entry)))

(provide 'init-aaronnote-jupyter-engine)

;;; init-aaronnote-jupyter-engine.el ends here

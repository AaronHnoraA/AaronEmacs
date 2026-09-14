;;; noema-research-tests.el --- Research notebook ERT suite -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name "../lisp/roam"
                               (file-name-directory (or load-file-name
                                                        buffer-file-name))))
(require 'noema-research)
(require 'noema-research-mode)
(require 'noema-research-graph)
(require 'noema-agent-promote)
(require 'noema-agent-worker)
(require 'noema-agent-takeover)
(require 'noema-research-inspector)
(require 'noema-research-synthesis)
(require 'noema-compose)

(defmacro noema-research-test--with-directory (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "noema-research-" t))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun noema-research-test--cell (id kind title source &rest pairs)
  "Return cell ID of KIND with TITLE, SOURCE and research metadata PAIRS."
  (let* ((meta (make-hash-table :test #'equal))
         (metadata (noema-research--table "custom" (noema-research--table "kept" t)))
         (cell (noema-research--table "cell_type" (if (equal kind "work") "code" "markdown")
                                      "id" id
                                      "metadata" metadata
                                      "source" source)))
    (when (or (member kind noema-research-graph-kinds)
              (member kind noema-research-kinds))
      (puthash "kind" kind meta))
    (unless (string-empty-p title)
      (puthash "title" title meta))
    (while pairs
      (puthash (car pairs) (cadr pairs) meta)
      (setq pairs (cddr pairs)))
    (when (> (hash-table-count meta) 0)
      (puthash noema-research-namespace meta metadata))
    (when (equal kind "work")
      (puthash "execution_count" :null cell)
      (puthash "outputs" [] cell))
    cell))

(defun noema-research-test--document ()
  "Return a representative research notebook."
  (let ((document (noema-research-create-document "Mixing time")))
    (puthash "schema" noema-research-legacy-schema
             (noema-research-notebook-meta document))
    (puthash "cells"
             (vector
              (noema-research-test--cell "c-q" "question" "Mixing-time problem"
                                         "Can O(n log n) become O(n)?")
              (noema-research-test--cell "c-w" "work" "Spectral exploration"
                                         "Investigate the log factor."
                                         "state" "open" "lineage" ["c-q"])
              (noema-research-test--cell "c-n" "note" "Current interpretation"
                                         "The loss may be an artifact.")
              (noema-research-test--cell "c-k" "checkpoint" "Reversibility invalid"
                                         "Lemma 4 needs exact reversibility.\n\n%% not a header"
                                         "lineage" ["c-w"]))
             document)
    (noema-research-normalize-document document)))

(defun noema-research-test--work-id (document cell-id)
  "Return CELL-ID's WorkNode id in DOCUMENT."
  (noema-research-cell-work-node-id (noema-research-find-cell document cell-id)))

(defun noema-research-test--append-work-cell (document id kind title source
                                                       &optional lineage depends)
  "Append a cell bound to an independent WorkNode and return the cell."
  (let ((cell (noema-research-test--cell id "note" "" source)))
    (puthash "cells" (vconcat (gethash "cells" document) (vector cell)) document)
    (noema-research-bind-cell document cell kind title)
    (when lineage (noema-research-set-relation document id "lineage" lineage))
    (when depends (noema-research-set-relation document id "depends" depends))
    cell))

(defmacro noema-research-test--with-jutext (document &rest body)
  "Render DOCUMENT into a temporary JuText buffer and run BODY there."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local noema-research--document ,document)
     (noema-research--render noema-research--document)
     ,@body))

(defun noema-research-test--ids (document)
  "Return the cell ids of DOCUMENT."
  (mapcar #'noema-research-cell-id (noema-research-cells document)))

(ert-deftest noema-research-serialize-matches-json-stringify ()
  (let ((document (noema-research--table
                   "a" 1 "b" [] "c" (make-hash-table :test #'equal)
                   "d" :false "e" :null "f" "x\"y" "g" (vector 1 "z") "h" t)))
    (should (equal (noema-research-serialize document)
                   (concat "{\n  \"a\": 1,\n  \"b\": [],\n  \"c\": {},\n  \"d\": false,\n"
                           "  \"e\": null,\n  \"f\": \"x\\\"y\",\n  \"g\": [\n    1,\n"
                           "    \"z\"\n  ],\n  \"h\": true\n}\n")))
    (should (equal (noema-research-serialize
                    (noema-research-parse-json (noema-research-serialize document)))
                   (noema-research-serialize document)))))

(ert-deftest noema-research-jutext-round-trip-preserves-the-document ()
  (let* ((document (noema-research-test--document))
         (before (noema-research-serialize document)))
    (noema-research-test--with-jutext document
      (should (string-match-p "^%% question Mixing-time problem$" (buffer-string)))
      (should (string-match-p "^%% work Spectral exploration$" (buffer-string)))
      (should (string-match-p "^%% Current interpretation$" (buffer-string)))
      (should (string-match-p "^\\\\%% not a header$" (buffer-string)))
      (should-not (string-match-p "^%% code" (buffer-string)))
      (should-not (string-match-p "c-w" (buffer-string)))
      (noema-research-mode--sync)
      (should (equal (noema-research-serialize noema-research--document) before)))))

(ert-deftest noema-research-renaming-keeps-identity-and-relations ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-w")
    (end-of-line)
    (insert-and-inherit " route")
    (let ((document (noema-research-mode--sync)))
      (should (equal (noema-research-cell-title (noema-research-find-cell document "c-w") document)
                     "Spectral exploration route"))
      (should (equal (noema-research-cell-relation (noema-research-find-cell document "c-k")
                                                   "lineage" document)
                     (list (noema-research-test--work-id document "c-w")))))))

(ert-deftest noema-research-inspector-projects-run-artifacts-by-work-node ()
  (noema-research-test--with-directory root
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" root)
                  nil 'silent)
    (let* ((document (noema-research-test--document))
           (work-node-id (noema-research-test--work-id document "c-w"))
           (inspector (get-buffer-create "*Noema Inspector*"))
           (seen nil)
           (my/noema--ready t))
      (unwind-protect
          (with-temp-buffer
            (setq-local buffer-file-name (expand-file-name "research.noema" root))
            (setq-local noema-research--document document)
            (noema-research--render document)
            (noema-research-goto-cell "c-w")
            (cl-letf (((symbol-function 'my/noema-api-call)
                       (lambda (channel args callback &optional _timeout)
                         (setq seen (list channel args))
                         (funcall callback
                                  (noema-research--table
                                   "links"
                                   (vector
                                    (noema-research--table
                                     "relation" "created"
                                     "sourceUri" "noema://file/src/baseline.py"
                                     "artifact" (noema-research--table
                                                 "kind" "run-file"
                                                 "sha256" "abcdef0123456789"))))
                                  nil)))
                      ((symbol-function 'display-buffer) #'ignore))
              (noema-research-inspect))
            (should (equal (car seen) "aaronnote:api:research:artifact:links"))
            (let ((request (aref (cadr seen) 0)))
              (should (equal (gethash "workNodeId" request) work-node-id)))
            (with-current-buffer inspector
              (should (string-match-p "Artifacts" (buffer-string)))
              (should (string-match-p "src/baseline.py" (buffer-string)))
              (should (string-match-p "created.*run-file" (buffer-string)))))
        (kill-buffer inspector)))))

(ert-deftest noema-research-continue-creates-a-lineage-child ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-k")
    (noema-research-continue)
    (insert "Repair proof")
    (let* ((document (noema-research-mode--sync))
           (ids (noema-research-test--ids document))
           (new-id (nth (1+ (seq-position ids "c-k")) ids))
           (cell (noema-research-find-cell document new-id)))
      (should (string-match-p "\\`c-[0-9a-f]\\{12\\}\\'" new-id))
      (should (equal (noema-research-cell-kind cell document) "work"))
      (should (equal (noema-research-cell-title cell document) "Repair proof"))
      (should (equal (noema-research-cell-state cell document) "open"))
      (should (equal (noema-research-cell-relation cell "lineage" document)
                     (list (noema-research-test--work-id document "c-k"))))
      (should (equal (car (last ids)) new-id))
      ;; The id is now attached to the header, so a second sync is stable.
      (should (equal (noema-research-test--ids (noema-research-mode--sync)) ids)))))

(ert-deftest noema-research-work-block-uses-code-storage-without-becoming-programming-code ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-w")
    (forward-line 1)
    (insert "@@agent(codex)\n@@ctx(lineage)\n\n")
    (let* ((document (noema-research-mode--sync))
           (work (noema-research-find-cell document "c-w"))
           (work-id (noema-research-test--work-id document "c-w")))
      (should (equal (gethash "cell_type" work) "code"))
      (should (equal (noema-research-cell-kind work document) "work"))
      (should (equal (noema-research-cell-work-node-id work) work-id))
      (should (equal (gethash "outputs" work) []))
      (should (= (length (seq-filter
                          (lambda (node)
                            (equal (noema-research-work-node-id node) work-id))
                          (noema-research-work-nodes document)))
                 1)))))

(ert-deftest noema-research-validates-only-leading-work-directives ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-find-cell document "c-w")))
    (puthash "source"
             (concat "@@agent(codex)\n@@session(fresh)\n"
                     "@@ctx(cell:c-q)\n@@ctx(result:wn_prior)\n"
                     "@@ctx(file:notes/input.md)\n@@skill(proof-review)\n\n"
                     "Review the evidence.\n@@agent(pi)")
             work)
    (should-not (noema-research--directive-errors document))
    (puthash "source" "@@agent(codex)\n@@agent(pi)\n\nReview it." work)
    (should (string-match-p "conflicting @@agent"
                            (mapconcat #'identity
                                       (noema-research--directive-errors document) "\n")))
    (puthash "source" "@@budget(10)\n\nReview it." work)
    (should (string-match-p "unsupported directive"
                            (mapconcat #'identity
                                       (noema-research--directive-errors document) "\n")))))

(ert-deftest noema-research-output-context-carries-the-manifest-project-root ()
  (noema-research-test--with-directory root
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" root)
                  nil 'silent)
    (let ((document (noema-research-test--document)))
      (noema-research-test--with-jutext document
        (setq-local buffer-file-name (expand-file-name "research.noema" root))
        (set-buffer-modified-p nil)
        (noema-research-goto-cell "c-w")
        (let ((context (noema-research--output-context t)))
          (should (equal (plist-get context :project-root) root))
          (cl-letf (((symbol-function 'my/noema--host-file) #'identity))
            (should (equal (alist-get 'projectRoot
                                      (noema-research--output-payload context))
                           root))))))))

(ert-deftest noema-research-execute-dispatches-work-to-an-agent-run ()
  (noema-research-test--with-directory root
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" root)
                  nil 'silent)
    (let ((document (noema-research-test--document))
          captured)
      (noema-research-test--with-jutext document
        (setq-local buffer-file-name (expand-file-name "research.noema" root))
        (set-buffer-modified-p nil)
        (noema-research-goto-cell "c-w")
        (cl-letf (((symbol-function 'noema-agent-worker-run-work-cell)
                   (lambda (file cell-id &rest _)
                     (setq captured (list file cell-id default-directory)))))
          (noema-research-execute-current))
        (should (equal captured
                       (list (expand-file-name "research.noema" root)
                             "c-w" root)))))))

(ert-deftest noema-agent-worker-cancel-run-controls-the-physical-worker ()
  (let* ((worker (noema-agent-worker--create :run-id "run_cancel"))
         (cancelled nil))
    (puthash "run_cancel" worker noema-agent-worker--runs)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-worker--cancel)
                   (lambda (value) (setq cancelled value))))
          (noema-agent-worker-cancel-run "run_cancel")
          (should (eq cancelled worker)))
      (remhash "run_cancel" noema-agent-worker--runs))))

(ert-deftest noema-research-copied-headers-get-new-ids ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-q")
    (let ((header (buffer-substring (line-beginning-position) (line-end-position))))
      (goto-char (point-max))
      (insert "\n" header "\nA second question.\n"))
    (let* ((document (noema-research-mode--sync))
           (ids (noema-research-test--ids document)))
      (should (equal (car ids) "c-q"))
      (should (= (length ids) 5))
      (should (= (length (delete-dups (copy-sequence ids))) 5)))))

(ert-deftest noema-research-deleting-a-block-strips-references ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (let ((entry (seq-find (lambda (item) (equal (plist-get item :id) "c-w"))
                           (noema-research--scan))))
      (delete-region (plist-get entry :header-beg) (plist-get entry :block-end)))
    (let ((document (noema-research-mode--sync)))
      (should-not (noema-research-find-cell document "c-w"))
      (should-not (noema-research-cell-relation (noema-research-find-cell document "c-k")
                                                "lineage" document))
      (should-not (plist-get (noema-research-validate document) :warnings)))))

(ert-deftest noema-research-text-before-the-first-header-becomes-a-note ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (goto-char (point-min))
    (insert "Loose preface.\n\n")
    (let ((document (noema-research-mode--sync)))
      (should (= (length (noema-research-cells document)) 5))
      (should (equal (noema-research-cell-source (car (noema-research-cells document)))
                     "Loose preface."))
      (should (equal (noema-research-test--ids (noema-research-mode--sync))
                     (noema-research-test--ids document))))))

(ert-deftest noema-research-work-dependency-dag-must-remain-acyclic ()
  (let ((document (noema-research-test--document)))
    (noema-research-set-relation document "c-k" "depends" '("c-w"))
    (should-error (noema-research-set-relation document "c-w" "depends" '("c-k"))
                  :type 'user-error)
    (should-error (noema-research-set-relation document "c-w" "depends" '("c-w"))
                  :type 'user-error)
    ;; A mixed lineage/depends cycle is still a cycle in the project DAG.
    (should-error (noema-research-set-relation document "c-q" "lineage" '("c-k"))
                  :type 'user-error)))

(ert-deftest noema-research-state-changes-are-validated ()
  (let ((document (noema-research-test--document)))
    (noema-research-set-state document "c-w" "dropped" "needs a uniform gap")
    (should (equal (noema-research-work-node-field
                    (noema-research-work-node-for-cell document
                                                       (noema-research-find-cell document "c-w"))
                    "dropped_reason")
                   "needs a uniform gap"))
    (noema-research-set-state document "c-w" "active")
    (should-not (noema-research-work-node-field
                 (noema-research-work-node-for-cell document
                                                    (noema-research-find-cell document "c-w"))
                 "dropped_reason"))
    (noema-research-set-outcome document "c-w" "inconclusive")
    (should (equal (noema-research-cell-outcome (noema-research-find-cell document "c-w") document)
                   "inconclusive"))
    (should-error (noema-research-set-state document "c-q" "done") :type 'user-error)
    (should-error (noema-research-set-state document "c-w" "finished") :type 'user-error)))

(ert-deftest noema-research-projection-folds-and-focuses ()
  (let ((document (noema-research-test--document)))
    (noema-research-test--append-work-cell
     document "c-s" "work" "Numerics" ""
     (list (noema-research-test--work-id document "c-q")))
    (noema-research-test--append-work-cell
     document "c-r" "work" "Repair" ""
     (list (noema-research-test--work-id document "c-k"))
     (list (noema-research-test--work-id document "c-s")))
    (noema-research-test--append-work-cell
     document "c-a" "work" "Alternative" ""
     (list (noema-research-test--work-id document "c-k")))
    (let* ((q (noema-research-test--work-id document "c-q"))
           (w (noema-research-test--work-id document "c-w"))
           (k (noema-research-test--work-id document "c-k"))
           (r (noema-research-test--work-id document "c-r"))
           (s (noema-research-test--work-id document "c-s"))
           (folded (noema-research-projection document :folds (list w)))
           (ids (mapcar (lambda (node) (plist-get node :id)) (plist-get folded :nodes))))
      (should (equal ids (list q w s)))
      (should (= (plist-get (seq-find (lambda (node) (equal (plist-get node :id) w))
                                      (plist-get folded :nodes))
                            :folded)
                 3))
      (should (member (list s w "depends") (plist-get folded :edges)))
      (let* ((focused (noema-research-projection document :folds (list w) :focus r))
             (focused-ids (mapcar (lambda (node) (plist-get node :id))
                                  (plist-get focused :nodes))))
        (should (equal focused-ids (list q w k r)))
        (should (equal (plist-get focused :focus) r))))))

(ert-deftest noema-research-files-detect-revision-conflicts ()
  (noema-research-test--with-directory directory
    (let* ((file (expand-file-name "research/bound.noema" directory))
           (revision (noema-research-write-file file (noema-research-test--document))))
      (should (equal revision (noema-research-file-revision file)))
      (should (noema-research-notebook-p (noema-research-read-file file)))
      (should-not (file-exists-p (expand-file-name ".agent" directory)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (insert " ")
        (write-region (point-min) (point-max) file nil 'silent))
      (should-error (noema-research-write-file file (noema-research-test--document) revision)
                    :type 'noema-research-revision-conflict)
      (let ((invalid (noema-research-test--document)))
        (noema-research-cell-set (noema-research-find-cell invalid "c-q") "kind" "task")
        (should-error (noema-research-write-file file invalid) :type 'user-error)))))

(ert-deftest noema-research-views-live-under-the-repository-agent-directory ()
  (noema-research-test--with-directory directory
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let* ((file (expand-file-name "research/deep/bound.noema" directory))
           (document (noema-research-test--document))
           (path (noema-research-view-write file document "c-w" '("c-q"))))
      (should (string-prefix-p (expand-file-name ".agent/views/" directory) path))
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name ".agent/.gitignore" directory))
                       (buffer-string))
                     "*\n"))
      (should (equal (noema-research-view-read file document)
                     '(:focus "c-w" :folds ("c-q")))))))

(ert-deftest noema-research-views-persist-semantic-zoom ()
  (noema-research-test--with-directory directory
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let* ((file (expand-file-name "research/bound.noema" directory))
           (document (noema-research-test--document)))
      (noema-research-view-write file document "c-w" '("c-q") "detail")
      (should (equal (noema-research-view-read file document)
                     '(:focus "c-w" :folds ("c-q") :zoom "detail"))))))

(ert-deftest noema-project-enable-creates-one-portable-identity ()
  (noema-research-test--with-directory directory
    (let ((ignore (expand-file-name ".gitignore" directory)))
      (write-region "dist/\n" nil ignore nil 'silent)
      (let* ((first (noema-project-enable directory))
             (manifest (plist-get first :manifest))
             (manifest-text (with-temp-buffer
                              (insert-file-contents manifest)
                              (buffer-string))))
        (should (plist-get first :created))
        (should (string-match-p
                 (concat "\\`schema = 1\nrepository_id = "
                         "\\\"[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-7[0-9a-f]\\{3\\}-"
                         "[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\\"\n\\'")
                 manifest-text))
        (should-not (plist-get (noema-project-enable directory) :created))
        (should (equal manifest-text
                       (with-temp-buffer
                         (insert-file-contents manifest)
                         (buffer-string))))
        (should (equal (with-temp-buffer
                         (insert-file-contents ignore)
                         (buffer-string))
                       "dist/\n.agent/\n"))))))

(ert-deftest noema-research-mode-saves-through-the-projection ()
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "bound.noema" directory))
          (noema-research-sync-host nil)
          (make-backup-files nil))
      (noema-research-write-file file (noema-research-test--document))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (noema-research-mode)
              (should (string-match-p "^%% work Spectral exploration$" (buffer-string)))
              (should-not (buffer-modified-p))
              (noema-research-goto-cell "c-w")
              (end-of-line)
              (insert-and-inherit " v2")
              (save-buffer)
              (should-not (buffer-modified-p))
              (let ((saved (noema-research-read-file file)))
                (should (equal (noema-research-cell-title (noema-research-find-cell saved "c-w") saved)
                               "Spectral exploration v2"))
                (should (equal (noema-research-cell-relation (noema-research-find-cell saved "c-k")
                                                             "lineage" saved)
                               (list (noema-research-test--work-id saved "c-w"))))
                (should (gethash "custom" (gethash "metadata"
                                                   (noema-research-find-cell saved "c-q")))))
              (should (equal noema-research--revision (noema-research-file-revision file))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest noema-research-ipynb-dispatch-selects-the-research-mode ()
  (require 'init-aaronnote-jupyter-notebook)
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "bound.noema" directory))
          (noema-research-sync-host nil))
      (noema-research-write-file file (noema-research-test--document))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (should (eq major-mode 'noema-research-mode))
              (should (string-match-p "^%% checkpoint Reversibility invalid$" (buffer-string))))
          (kill-buffer buffer))))))

(ert-deftest noema-research-agent-output-uses-the-canonical-noema-document ()
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "work.noema" directory))
          (noema-research-sync-host nil)
          opened)
      (noema-research-write-file file (noema-research-test--document))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (noema-research-mode)
              (noema-research-goto-cell "c-w")
              (cl-letf (((symbol-function 'my/noema-jupyter-output-open-document)
                         (lambda (payload &optional focus)
                           (setq opened (list payload focus)))))
                (noema-research-open-outputs))
              (let ((payload (car opened)))
                (should (file-equal-p (alist-get 'scriptFile payload) file))
                (should (file-equal-p (alist-get 'sourceFile payload) file))
                (should (file-equal-p (alist-get 'projectRoot payload) directory))
                (should (equal (alist-get 'cellId payload) "c-w"))
                (should-not (alist-get 'kernelName payload))))
          (kill-buffer buffer))))))

(ert-deftest noema-research-jutext-output-and-inspector-bindings-do-not-conflict ()
  (should (eq (lookup-key noema-research-mode-map (kbd "C-c C-i"))
                #'noema-research-open-outputs))
  (should (eq (lookup-key noema-research-mode-map (kbd "C-c C-o"))
                #'noema-research-open-outputs))
  (should (eq (lookup-key noema-research-mode-map (kbd "C-c j i"))
              #'noema-research-inspect)))

(ert-deftest noema-research-kernel-commands-are-disabled ()
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "work.noema" directory))
          (noema-research-sync-host nil))
      (noema-research-write-file file (noema-research-test--document))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (noema-research-mode)
              (noema-research-goto-cell "c-w")
              (should-error (noema-research-execute-all) :type 'user-error)
              (should-error (noema-research-restart-kernel) :type 'user-error)
              (should-error (noema-research-select-kernel) :type 'user-error)
              (should-error (noema-research-insert-code) :type 'user-error))
          (kill-buffer buffer))))))

(ert-deftest noema-research-agent-output-merge-preserves-live-jutext-edits ()
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "work.noema" directory))
          (noema-research-sync-host nil)
          (make-backup-files nil))
      (noema-research-write-file file (noema-research-test--document))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (noema-research-mode)
              (noema-research-goto-cell "c-w")
              (forward-line 1)
              (end-of-line)
              (insert "\nEdited while the agent was running.")
              (let* ((disk (noema-research-read-file file))
                     (disk-work (noema-research-find-cell disk "c-w")))
                (puthash "execution_count" :null disk-work)
                (puthash "outputs"
                         (vector (noema-research--table
                                  "output_type" "display_data"
                                  "metadata" (make-hash-table :test #'equal)
                                  "data" (noema-research--table
                                          "text/markdown" "finished\n")))
                         disk-work)
                (noema-research-write-file file disk))
              (noema-research-merge-disk-outputs)
              (should (buffer-modified-p))
              (let* ((work (noema-research-find-cell noema-research--document "c-w"))
                     (output (aref (gethash "outputs" work) 0)))
                (should (eq (gethash "execution_count" work) :null))
                (should (equal (gethash "text/markdown" (gethash "data" output))
                               "finished\n")))
              (save-buffer)
              (let ((saved (noema-research-find-cell
                            (noema-research-read-file file) "c-w")))
                (should (string-match-p "Edited while the agent was running"
                                        (noema-research-cell-source saved)))
                (should (= (length (gethash "outputs" saved)) 1))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest noema-research-jupyter-output-selects-source-by-stable-cell-id ()
  (let (opened navigated)
    (cl-letf (((symbol-function 'find-file)
               (lambda (file) (setq opened file)))
              ((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'noema-research-mode modes)))
              ((symbol-function 'noema-research-goto-cell)
               (lambda (cell-id) (setq navigated cell-id))))
      (my/noema-jupyter-cell-select-source
       '((scriptFile . "/work/research.noema")
         (cellId . "cell-analysis"))))
    (should (equal opened "/work/research.noema"))
    (should (equal navigated "cell-analysis"))))

(ert-deftest noema-research-graph-buffer-renders-only-the-dag ()
  (let ((source (generate-new-buffer " *noema-dag-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local noema-research--document (noema-research-test--document))
            (noema-research--render noema-research--document))
          (with-temp-buffer
            (noema-research-graph-mode)
            (setq-local noema-research-graph--source source)
            (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                      ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                      ((symbol-function 'noema-research-graph--insert-image)
                       (lambda (_projection) (insert "<DAG>") t))
                      ((symbol-function 'noema-research-graph--insert-outline)
                       (lambda (&rest _) (ert-fail "text outline must not render"))))
              (noema-research-graph-refresh))
            (should (equal (buffer-string) "<DAG>"))))
      (kill-buffer source))))

(ert-deftest noema-research-relation-editor-filters-combined-dag-cycles ()
  (dolist (editor '(noema-research-edit-lineage noema-research-edit-depends))
    (noema-research-test--with-jutext (noema-research-test--document)
      (noema-research-goto-cell "c-w")
      (let ((question (noema-research-test--work-id noema-research--document "c-q"))
            (checkpoint (noema-research-test--work-id noema-research--document "c-k"))
            candidates ids)
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt collection &rest _)
                     (setq candidates collection)
                     nil)))
          (funcall editor))
        (setq ids (mapcar (lambda (candidate)
                            (noema-research-cell-work-node-id (cdr candidate)))
                          candidates))
        (should (member question ids))
        (should-not (member checkpoint ids))))))

(ert-deftest noema-research-graph-shows-semantic-fold-status-and-activity ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (checkpoint (noema-research-test--work-id document "c-k"))
         (noema-research-graph--zoom "branch")
         (noema-research-graph--folds (list work))
         (noema-research-graph--runs
          `(((workNodeId . ,work) (id . "run-7") (status . "completed")
             (finishedAt . "2026-09-14T08:31:00Z"))))
         (noema-research-graph--events
          `(((work_node_id . ,checkpoint) (type . "checkpoint.created")
             (ts . "2026-09-14T08:35:00Z"))))
         projection node label)
    (noema-research-set-state document work "dropped" "baseline remains stronger")
    (noema-research-set-outcome document work "dead_end")
    (setq projection (noema-research-graph--projection document)
          node (seq-find (lambda (candidate)
                           (equal (plist-get candidate :id) work))
                         (plist-get projection :nodes))
          label (noema-research-graph--label node))
    (should (equal (plist-get (plist-get node :fold-summary) :nodes) 2))
    (should (string-match-p "dead end" label))
    (should (string-match-p "2 nodes" label))
    (should (string-match-p "2026-09-14 08:35" label))
    (should (string-match-p "baseline remains stronger" label))))

(ert-deftest noema-research-graph-fold-summary-reports-tied-outcomes ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (child (noema-research-test--append-work-cell
                 document "c-tie" "work" "Alternative route" "Compare it." (list work)))
         (child-work (noema-research-cell-work-node-id child))
         (noema-research-graph--zoom "branch")
         (noema-research-graph--folds (list work))
         (noema-research-graph--selected work)
         projection node label)
    (noema-research-set-outcome document work "supported")
    (noema-research-set-outcome document child-work "dead_end")
    (setq projection (noema-research-graph--projection document)
          node (seq-find (lambda (candidate)
                           (equal (plist-get candidate :id) work))
                         (plist-get projection :nodes))
          label (noema-research-graph--label node))
    (should (string-match-p "▸ supported 1 · dead end 1" label))
    (should-not (string-match-p "▸ ·" label))
    (should-not (string-match-p "last —" label))))

(ert-deftest noema-research-graph-manual-fold-preserves-current-path ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (checkpoint (noema-research-test--work-id document "c-k"))
         (current-cell (noema-research-test--append-work-cell
                        document "c-current" "work" "Current work" "Continue."
                        (list checkpoint)))
         (current (noema-research-cell-work-node-id current-cell))
         (side-cell (noema-research-test--append-work-cell
                     document "c-side" "work" "Side branch" "Archive."
                     (list work)))
         (side (noema-research-cell-work-node-id side-cell))
         (noema-research-graph--zoom "branch")
         (noema-research-graph--folds (list work))
         (noema-research-graph--selected current)
         (projection (noema-research-graph--projection document))
         (ids (mapcar (lambda (node) (plist-get node :id))
                      (plist-get projection :nodes)))
         (fold-node (seq-find (lambda (node) (equal (plist-get node :id) work))
                              (plist-get projection :nodes))))
    (should (member work ids))
    (should (member checkpoint ids))
    (should (member current ids))
    (should-not (member side ids))
    (should (plist-get (plist-get fold-node :fold-summary) :path-preserved))
    (should (string-match-p "current path preserved"
                            (noema-research-graph--label fold-node)))))

(ert-deftest noema-research-graph-dot-reserves-bold-multiline-label-space ()
  (let* ((projection
          (list :nodes
                (list (list :id "question-long" :kind "question"
                            :title "Can expansion remove the logarithmic loss?"
                            :state "open" :dropped-reason
                            "A deliberately long reason that must remain inside the node"))
                :edges nil))
         (dot (car (noema-research-graph--dot-source projection))))
    (should (string-match-p "fontname=\\\"Helvetica Bold\\\"" dot))
    (should (string-match-p (regexp-quote "label=\"? Can expansion") dot))
    (should (string-match-p
             (regexp-quote
              "logarithmic loss?\\nopen\\nA deliberately long reason")
             dot))
    (should-not (string-match-p (regexp-quote "loss?\\\\nopen") dot))))

(ert-deftest noema-research-graph-focus-contracts-omitted-branches ()
  (let ((document (noema-research-test--document)))
    (let ((question (noema-research-test--work-id document "c-q"))
          (checkpoint (noema-research-test--work-id document "c-k")))
      (noema-research-test--append-work-cell document "c-r" "work" "Repair" ""
                                                (list checkpoint))
      (noema-research-test--append-work-cell document "c-s" "work" "Earlier modelling" ""
                                                (list question))
      (noema-research-test--append-work-cell
       document "c-s2" "checkpoint" "Archived result" ""
       (list (noema-research-test--work-id document "c-s")))
      (let* ((noema-research-graph--focus
              (noema-research-test--work-id document "c-r"))
             (noema-research-graph--zoom "branch")
             (projection (noema-research-graph--projection document))
             (summary (seq-find (lambda (node) (plist-get node :summary))
                                (plist-get projection :nodes))))
        (should summary)
        (should (equal (plist-get summary :title) "Related branches"))
        (should (= (plist-get (plist-get summary :fold-summary) :nodes) 2))
        (should (member (list question (plist-get summary :id) "lineage")
                        (plist-get projection :edges)))))))

(ert-deftest noema-research-graph-overview-auto-folds-but-protects-current-path ()
  (let* ((document (noema-research-test--document))
         (question (noema-research-test--work-id document "c-q"))
         (work (noema-research-test--work-id document "c-w"))
         (checkpoint (noema-research-test--work-id document "c-k"))
         (noema-research-graph--zoom "overview")
         (noema-research-graph--selected question)
         (projection nil))
    (noema-research-set-state document work "done")
    (setq projection (noema-research-graph--projection document))
    (should (numberp
             (plist-get (seq-find (lambda (node) (equal (plist-get node :id) work))
                                  (plist-get projection :nodes))
                        :folded)))
    (setq noema-research-graph--selected checkpoint
          projection (noema-research-graph--projection document))
    (should-not (plist-get (seq-find (lambda (node) (equal (plist-get node :id) work))
                                    (plist-get projection :nodes))
                           :folded))))

(ert-deftest noema-research-graph-detail-projects-the-latest-agent-run ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (noema-research-graph--zoom "detail")
         (noema-research-graph--runs
          `(((workNodeId . ,work) (id . "run-9") (status . "running")
             (agent . "codex") (startedAt . "2026-09-14T09:00:00Z"))))
         (noema-research-graph--artifacts
          `(((workNodeId . ,work) (runId . "run-9") (relation . "created")
             (sourceUri . "noema://file/results/table.csv")
             (createdAt . "2026-09-14T09:02:00Z")
             (artifact . ((id . "artifact-3") (mediaType . "text/csv"))))))
         (projection (noema-research-graph--projection document))
         (run (seq-find (lambda (node) (equal (plist-get node :id) "run:run-9"))
                        (plist-get projection :nodes)))
         (artifact (seq-find (lambda (node) (plist-get node :artifact))
                             (plist-get projection :nodes))))
    (should run)
    (should (equal (plist-get run :kind) "run"))
    (should (equal (plist-get run :agent) "codex"))
    (should (member (list work "run:run-9" "run")
                    (plist-get projection :edges)))
    (should artifact)
    (should (equal (plist-get artifact :media-type) "text/csv"))
    (should (member (list work (plist-get artifact :id) "artifact")
                    (plist-get projection :edges)))))

(ert-deftest noema-research-graph-loads-events-by-absolute-document-path ()
  (noema-research-test--with-directory directory
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let ((source (generate-new-buffer " *noema-event-source*"))
          (graph (generate-new-buffer " *noema-event-graph*"))
          (file (expand-file-name "research.noema" directory))
          requested-args)
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq-local buffer-file-name file)
              (setq-local noema-research--document (noema-research-test--document))
              (noema-research--render noema-research--document)
              (set-buffer-modified-p nil))
            (with-current-buffer graph
              (noema-research-graph-mode)
              (setq noema-research-graph--source source)
              (cl-letf (((symbol-function 'my/noema-api-call)
                         (lambda (_channel args callback &optional _timeout)
                           (setq requested-args args)
                           (funcall callback
                                    (noema-research--table "events" []) nil)))
                        ((symbol-function 'noema-research-graph-refresh) #'ignore))
                (noema-research-graph-refresh-events)))
            (should (equal (alist-get 'file (aref requested-args 0)) file)))
        (kill-buffer source)
        (kill-buffer graph)))))

(ert-deftest noema-research-graph-runs-selected-work-through-agent-worker ()
  (noema-research-test--with-directory directory
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let ((source (generate-new-buffer " *noema-run-source*"))
          (graph (generate-new-buffer " *noema-run-graph*"))
          (file (expand-file-name "work.noema" directory))
          called)
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq-local buffer-file-name file)
              (setq-local noema-research--document (noema-research-test--document))
              (noema-research--render noema-research--document)
              (set-buffer-modified-p nil))
            (with-current-buffer graph
              (noema-research-graph-mode)
              (setq noema-research-graph--source source
                    noema-research-graph--selected
                    (noema-research-test--work-id
                     (buffer-local-value 'noema-research--document source) "c-w"))
              (cl-letf (((symbol-function 'noema-agent-worker-run-work-cell)
                         (lambda (&rest args) (setq called args))))
                (noema-research-graph--run "fresh")))
            (should (equal called (list file "c-w" "fresh" nil))))
        (kill-buffer source)
        (kill-buffer graph)))))

(ert-deftest noema-research-graph-sync-is-an-explicit-pop-up-command ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (setq-local major-mode 'noema-research-mode)
    (noema-research-goto-cell "c-w")
    (let (popped followed)
      (cl-letf (((symbol-function 'noema-research-graph-pop-buffer)
                 (lambda (buffer) (setq popped buffer)))
                ((symbol-function 'noema-research-graph-follow-source)
                 (lambda (source id)
                   (setq followed (list source id))
                   " *temporary-dag*")))
        (noema-research-sync-graph))
      (should (equal popped " *temporary-dag*"))
      (should (eq (car followed) (current-buffer)))
      (should (equal (cadr followed)
                     (noema-research-test--work-id noema-research--document "c-w"))))))

(ert-deftest noema-research-command-return-syncs-outputarea-not-the-dag ()
  (should (eq (lookup-key noema-research-mode-map (kbd "s-<return>"))
              'noema-research-open-outputs))
  (should-not (eq (lookup-key noema-research-mode-map (kbd "s-<return>"))
                  'noema-research-sync-graph)))

(ert-deftest noema-research-graph-visit-closes-its-temporary-pop-up ()
  (let ((source (generate-new-buffer " *noema-dag-return-source*"))
        (graph (generate-new-buffer " *noema-dag-return-graph*"))
        closed popped visited)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local noema-research--document (noema-research-test--document))
            (setq-local major-mode 'noema-research-mode))
          (with-current-buffer graph
            (noema-research-graph-mode)
            (setq-local noema-research-graph--source source
                        noema-research-graph--selected
                        (noema-research-test--work-id
                         (buffer-local-value 'noema-research--document source) "c-w"))
            (cl-letf (((symbol-function 'quit-window)
                       (lambda (&rest args) (setq closed args)))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _) (setq popped buffer)))
                      ((symbol-function 'noema-research-mode--sync)
                       (lambda (&rest _) noema-research--document))
                      ((symbol-function 'noema-research-goto-cell)
                       (lambda (id) (setq visited id))))
              (noema-research-graph-visit)))
          (should closed)
          (should (eq popped source))
          (should (equal visited
                         (noema-research-test--work-id
                          (buffer-local-value 'noema-research--document source)
                          "c-w"))))
      (kill-buffer source)
      (kill-buffer graph))))

(ert-deftest noema-research-graph-hjkl-and-arrows-use-layout-geometry ()
  (with-temp-buffer
    (noema-research-graph-mode)
    (setq noema-research-graph--layout-cache
          '(:nodes ((:id "root" :x 50.0 :y 10.0)
                    (:id "right" :x 100.0 :y 10.0)
                    (:id "down" :x 50.0 :y 80.0)))
          noema-research-graph--selected "root")
    (cl-letf (((symbol-function 'noema-research-graph-refresh) #'ignore))
      (noema-research-graph-move-right)
      (should (equal noema-research-graph--selected "right"))
      (noema-research-graph-move-left)
      (should (equal noema-research-graph--selected "root"))
      (noema-research-graph-move-down)
      (should (equal noema-research-graph--selected "down"))
      (noema-research-graph-move-up)
      (should (equal noema-research-graph--selected "root")))
    (should (eq (key-binding (kbd "h")) 'noema-research-graph-move-left))
    (should (eq (key-binding (kbd "<down>")) 'noema-research-graph-move-down))
    (should (eq (key-binding (kbd "?")) 'noema-research-graph-help))))

(ert-deftest noema-research-graph-can-delete-an-orphan-work-node ()
  (let ((source (generate-new-buffer " *noema-orphan-source*"))
        (graph (generate-new-buffer " *noema-orphan-graph*")))
    (unwind-protect
        (let (work)
          (with-current-buffer source
            (setq-local noema-research--document (noema-research-test--document))
            (noema-research--render noema-research--document)
            (setq work (noema-research-test--work-id noema-research--document "c-w"))
            (noema-research-goto-cell "c-w")
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (noema-research-delete-current-cell)))
          (with-current-buffer graph
            (noema-research-graph-mode)
            (setq noema-research-graph--source source
                  noema-research-graph--selected work)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (noema-research-graph-delete-work-node)))
          (with-current-buffer source
            (should-not (noema-research-find-work-node noema-research--document work))))
      (kill-buffer source)
      (kill-buffer graph))))

(ert-deftest noema-research-graph-keys-win-in-evil-normal-state ()
  (with-temp-buffer
    (noema-research-graph-mode)
    (when (fboundp 'evil-normal-state)
      (evil-normal-state))
    (should (eq (key-binding (kbd "e")) 'noema-research-graph-execute))
    (should (eq (key-binding (kbd "F")) 'noema-research-graph-run-fork))
    (should (eq (key-binding (kbd "X")) 'noema-research-graph-structure))
    (should (eq (key-binding (kbd "z")) 'noema-research-graph-cycle-zoom))))

(ert-deftest noema-research-structure-commands-preserve-distinct-identities ()
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-w")
    (let ((work (noema-research-test--work-id noema-research--document "c-w")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (noema-research-unbind-current-cell))
      (should (noema-research-find-work-node noema-research--document work))
      (should (equal (noema-research--get
                      (noema-research-find-cell noema-research--document "c-w") "cell_type")
                     "markdown"))
      (should-not (noema-research-cell-work-node-id
                   (noema-research-find-cell noema-research--document "c-w")))))
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-w")
    (let ((work (noema-research-test--work-id noema-research--document "c-w")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (noema-research-delete-current-cell))
      (should-not (noema-research-find-cell noema-research--document "c-w"))
      (should (noema-research-find-work-node noema-research--document work))))
  (noema-research-test--with-jutext (noema-research-test--document)
    (noema-research-goto-cell "c-w")
    (let ((work (noema-research-test--work-id noema-research--document "c-w")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (noema-research-delete-current-work-node))
      (should-not (noema-research-find-work-node noema-research--document work))
      (should (equal (noema-research--get
                      (noema-research-find-cell noema-research--document "c-w") "cell_type")
                     "markdown")))))

(ert-deftest noema-research-graph-layout-uses-graphviz ()
  (skip-unless (executable-find noema-research-graph-dot-program))
  (let* ((projection (noema-research-projection (noema-research-test--document)))
         (layout (noema-research-graph--layout projection)))
    (should (= (length (plist-get layout :nodes)) 3))
    (should (= (length (plist-get layout :edges)) 2))
    (dolist (node (plist-get layout :nodes))
      (should (numberp (plist-get node :x)))
      (should (> (plist-get node :width) 0)))
    (should (car (noema-research-graph--svg projection layout "c-w")))))

(ert-deftest noema-research-graph-is-one-temporary-pop-up-buffer ()
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "workspace.noema" directory))
          (second-file (expand-file-name "second.noema" directory))
          (noema-research-sync-host nil)
          source second-source graph)
      (noema-research-write-file file (noema-research-test--document))
      (noema-research-write-file second-file (noema-research-test--document))
      (unwind-protect
          (save-window-excursion
            (setq source (find-file-noselect file))
            (setq second-source (find-file-noselect second-file))
            (switch-to-buffer source)
            (setq graph (noema-research-graph-buffer source))
            (should (eq graph (noema-research-graph-buffer source)))
            (noema-research-graph-open)
            (switch-to-buffer source)
            (noema-research-graph-open)
            (let ((graph-windows
                   (seq-filter
                    (lambda (window)
                      (eq (window-buffer window) graph))
                    (window-list nil 'no-minibuffer))))
              (should (= (length graph-windows) 1))
              (should-not (window-parameter (car graph-windows)
                                          'noema-research-workspace-graph)))
            (switch-to-buffer second-source)
            (noema-research-graph-open)
            (should (eq graph (noema-research-graph-buffer second-source))))
        (when (buffer-live-p graph) (kill-buffer graph))
        (when (buffer-live-p second-source) (kill-buffer second-source))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest noema-research-p0-workflow-survives-close-and-reopen ()
  "Exercise the complete zero-AI research-memory path against a real file."
  (noema-research-test--with-directory directory
    (write-region "schema = 1\nrepository_id = \"01993f0f-4f40-7000-8000-000000000001\"\n"
                  nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let* ((file (expand-file-name "research.noema" directory))
           (document (noema-research-create-document "P0 acceptance"))
           (question (noema-research-test--cell
                      "c-p0-question" "note" ""
                      "Exercise the full local workflow."))
           (noema-research-sync-host nil)
           (make-backup-files nil)
           work checkpoint branch-a branch-b expected)
      (puthash "cells" (vector question) document)
      (noema-research-bind-cell document question "question" "Can the memory loop survive?")
      (noema-research-write-file file document)
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buffer
              (noema-research-mode)
              (cl-labels
                  ((new-id-after
                    (before)
                    (car (seq-difference
                          (noema-research-test--ids
                           (noema-research-mode--sync))
                          before #'equal))))
                (noema-research-goto-cell "c-p0-question")
                (let ((before (noema-research-test--ids noema-research--document)))
                  (noema-research-continue)
                  (insert "Primary route\nWork body survives restart.")
                  (setq work (new-id-after before)))
                (noema-research-goto-cell work)
                (let ((before (noema-research-test--ids noema-research--document)))
                  (noema-research-new-checkpoint)
                  (insert "Evidence changed\nCheckpoint body survives restart.")
                  (setq checkpoint (new-id-after before)))
                (noema-research-goto-cell checkpoint)
                (let ((before (noema-research-test--ids noema-research--document)))
                  (noema-research-continue)
                  (insert "Branch without data\nThis route will be dropped.")
                  (setq branch-a (new-id-after before)))
                (noema-research-goto-cell checkpoint)
                (let ((before (noema-research-test--ids noema-research--document)))
                  (noema-research-continue)
                  (insert "Alternative route\nThis route is reparented.")
                  (setq branch-b (new-id-after before)))
                ;; Relations are identities, so changing labels must not affect them.
                (noema-research-set-relation noema-research--document branch-b
                                             "lineage" (list work))
                (noema-research-set-relation noema-research--document branch-b
                                             "depends" (list branch-a))
                (noema-research-set-state noema-research--document branch-a
                                          "dropped" "source data unavailable")
                (noema-research-goto-cell work)
                (end-of-line)
                (insert " renamed")
                (setq noema-research--document (noema-research-mode--sync))
                (noema-research-view-write file noema-research--document
                                           (noema-research-test--work-id noema-research--document branch-b)
                                           (list (noema-research-test--work-id
                                                  noema-research--document checkpoint)))
                (save-buffer)
                (setq expected (noema-research-serialize
                                (noema-research-read-file file)))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      ;; Derived state may disappear without changing notebook or view semantics.
      (let ((agent (expand-file-name ".agent" directory)))
        (dolist (name '("cache" "runtime"))
          (make-directory (expand-file-name name agent) t))
        (write-region "derived" nil (expand-file-name "search.sqlite" agent) nil 'silent)
        (delete-file (expand-file-name "search.sqlite" agent))
        (delete-directory (expand-file-name "cache" agent) t)
        (delete-directory (expand-file-name "runtime" agent) t))
      (let ((reopened (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer reopened
              (noema-research-mode)
              (should (equal (noema-research-serialize noema-research--document)
                             expected))
              (should (= (noema-research--get noema-research--document "nbformat") 4))
              (should (>= (noema-research--get noema-research--document "nbformat_minor") 5))
              (should (equal (noema-research-cell-title
                              (noema-research-find-cell noema-research--document work)
                              noema-research--document)
                             "Primary route renamed"))
              (should (equal (noema-research-cell-source
                              (noema-research-find-cell noema-research--document work))
                             "Work body survives restart."))
              (should (equal (noema-research-cell-relation
                              (noema-research-find-cell noema-research--document branch-b)
                              "lineage" noema-research--document)
                             (list (noema-research-test--work-id noema-research--document work))))
              (should (equal (noema-research-cell-relation
                              (noema-research-find-cell noema-research--document branch-b)
                              "depends" noema-research--document)
                             (list (noema-research-test--work-id noema-research--document branch-a))))
              (should (equal (noema-research-cell-state
                              (noema-research-find-cell noema-research--document branch-a)
                              noema-research--document)
                             "dropped"))
              (should (equal (noema-research-work-node-field
                              (noema-research-work-node-for-cell
                               noema-research--document
                               (noema-research-find-cell noema-research--document branch-a))
                              "dropped_reason")
                             "source data unavailable"))
              (should (equal (noema-research-view-read file noema-research--document)
                             (list :focus (noema-research-test--work-id noema-research--document branch-b)
                                   :folds (list (noema-research-test--work-id
                                                 noema-research--document checkpoint)))))
              (should-not (plist-get (noema-research-validate noema-research--document)
                                     :errors)))
          (kill-buffer reopened))))))

(ert-deftest noema-agent-promote-projects-agent-shell-native-identity ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode
          default-directory "/tmp/noema-project/")
    (setq-local agent-shell--state
                '((:agent-config . ((:identifier . magent)))
                  (:session . ((:id . "native-magent-1")
                               (:title . "Existing investigation")
                               (:model-id . "gpt-test")
                               (:mode-id . "code")))
                  (:supports-session-list . t)
                  (:supports-session-load . t)
                  (:supports-session-resume . t)
                  (:supports-session-fork)))
    (let ((payload (noema-agent-promote--session-spec)))
      (should (equal (alist-get 'agent payload) "magent"))
      (should (equal (alist-get 'nativeSessionId payload) "native-magent-1"))
      (should (equal (alist-get 'executionTarget payload) "/tmp/noema-project/"))
      (should (equal (alist-get 'title payload) "Existing investigation"))
      (should (equal (alist-get 'sessionResume (alist-get 'capabilities payload)) t))
      (should-not (alist-get 'sessionFork (alist-get 'capabilities payload))))))

(ert-deftest noema-agent-promote-calls-the-durable-research-channel ()
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode
          default-directory "/tmp/noema-project/")
    (setq-local agent-shell--state
                '((:agent-config . ((:identifier . codex)))
                  (:session . ((:id . "native-codex-1")))))
    (let ((old-ready (and (boundp 'my/noema--ready) my/noema--ready))
          requested-channel requested-args)
      (unwind-protect
          (progn
            (setq my/noema--ready t)
            (cl-letf (((symbol-function 'my/noema-api-call)
                       (lambda (channel args callback &optional _timeout)
                         (setq requested-channel channel
                               requested-args args)
                         (funcall callback (noema-research--table "id" "ses_noema") nil))))
              (noema-agent-promote-current-session "Bound" "Improve it")
              (should (equal requested-channel
                             "aaronnote:api:research:session:promote"))
              (should (equal (alist-get 'goal (aref requested-args 0)) "Improve it"))
              (should (equal noema-agent-promote--session-id "ses_noema"))))
        (setq my/noema--ready old-ready)))))

(ert-deftest noema-agent-worker-normalizes-actions-without-trusting-raw-input ()
  (let ((action (noema-agent-worker--action
                 '((:kind . "edit")
                   (:raw-input . ((command . ["git" "status"])))
                   (:locations . (((path . "notes/bound.md"))))))))
    (should (equal (alist-get 'kind action) "edit"))
    (should (equal (append (alist-get 'paths action) nil) '("notes/bound.md")))
    (should (equal (append (alist-get 'argv action) nil) '("git" "status")))))

(ert-deftest noema-agent-worker-embeds-frozen-context-or-uses-a-deterministic-text-fallback ()
  (let* ((worker (noema-agent-worker--create
                  :spec (noema-research--table "prompt" "Prove the claim")
                  :context-items
                  (list (noema-research--table
                         "ref" "file:notes/known.md"
                         "resolvedUri" "noema://file/notes/known.md"
                         "mediaType" "text/markdown"
                         "contentBase64" (base64-encode-string "Known lemma." t)))))
         (agent-shell--state '((:prompt-capabilities . ((:embedded-context . t)))))
         (embedded (noema-agent-worker--content-blocks worker)))
    (should (= (length embedded) 2))
    (should (equal (map-elt (cadr embedded) 'type) "resource"))
    (let ((agent-shell--state '((:prompt-capabilities . ((:embedded-context . :false))))))
      (should (string-match-p "Known lemma\."
                              (map-elt (car (noema-agent-worker--content-blocks worker)) 'text))))))

(ert-deftest noema-agent-worker-projects-frozen-http-mcp-servers-into-agent-shell ()
  (let* ((worker (noema-agent-worker--create
                  :spec (noema-research--table
                         "mcp_servers"
                         (vector (noema-research--table
                                  "name" "noema" "type" "http"
                                  "url" "http://127.0.0.1:6806/mcp")))))
         (server (car (noema-agent-worker--mcp-servers worker))))
    (should (equal (alist-get 'name server) "noema"))
    (should (equal (alist-get 'type server) "http"))
    (should (equal (alist-get 'url server) "http://127.0.0.1:6806/mcp"))))

(ert-deftest noema-agent-worker-applies-only-the-authoritative-permission-command ()
  (let* ((worker (noema-agent-worker--create
		  :run-id "run_1" :session-id "ses_1" :epoch 3
		  :pending-permissions '("perm_1")))
         (received nil))
    (puthash "perm_1" (cons worker (lambda (option) (setq received option)))
             noema-agent-worker--permissions)
    (unwind-protect
        (progn
          (noema-agent-worker-apply-command
           (noema-research--table "type" "permission-decision"
				  "runId" "run_1" "sessionId" "ses_1" "epoch" 3
				  "permissionId" "perm_1" "optionId" "allow_once"))
          (should (equal received "allow_once"))
          (should-not (gethash "perm_1" noema-agent-worker--permissions)))
      (remhash "perm_1" noema-agent-worker--permissions))))

(ert-deftest noema-agent-worker-rejects-stale-decision-epochs-and-applies-input ()
  (let* ((worker (noema-agent-worker--create
		  :run-id "run_1" :session-id "ses_1" :epoch 9
		  :pending-permissions '("perm_stale") :pending-inputs '("input_1")))
	 (permission nil)
	 (answer nil))
    (puthash "perm_stale" (cons worker (lambda (option) (setq permission option)))
	     noema-agent-worker--permissions)
    (puthash "input_1" (cons worker (lambda (value) (setq answer value)))
	     noema-agent-worker--inputs)
    (unwind-protect
	(progn
	  (noema-agent-worker-apply-command
	   (noema-research--table "type" "permission-decision" "runId" "run_1"
				  "sessionId" "ses_1" "epoch" 8
				  "permissionId" "perm_stale" "optionId" "allow_once"))
	  (should-not permission)
	  (should (gethash "perm_stale" noema-agent-worker--permissions))
	  (noema-agent-worker-apply-command
	   (noema-research--table "type" "input-response" "runId" "run_1"
				  "sessionId" "ses_1" "epoch" 9 "requestId" "input_1"
				  "answer" (noema-research--table "choice" "spectral")))
	  (should (equal (noema-agent-worker--string answer "choice") "spectral"))
	  (should-not (gethash "input_1" noema-agent-worker--inputs)))
      (remhash "perm_stale" noema-agent-worker--permissions)
      (remhash "input_1" noema-agent-worker--inputs))))

(ert-deftest noema-agent-takeover-launches-recorded-argv-and-hands-back-before-resume ()
  (let* ((origin (generate-new-buffer " *noema-agent-origin*"))
	 (terminal (generate-new-buffer " *noema-agent-terminal*"))
	 (resumed (generate-new-buffer " *noema-agent-resumed*"))
	 (shutdown nil)
	 (launched-shell nil)
	 (handback-body nil)
	 (resumed-native nil)
	 (result (noema-research--table
		  "root" "/tmp/project/"
		  "command" ["codex" "resume" "native-1"]
		  "session" (noema-research--table "id" "ses_1" "nativeSessionId" "native-1")
		  "intervention" (noema-research--table "id" "manual_1" "version" 1))))
    (unwind-protect
	(progn
	  (require 'vterm)
	  (with-current-buffer origin
	    (setq-local agent-shell--state '(:agent-config (:identifier codex))))
	  (cl-letf (((symbol-function 'executable-find) (lambda (_name) "/bin/codex"))
		    ((symbol-function 'noema-agent-acp-shutdown)
                     (lambda (_buffer) (setq shutdown t)))
		    ((symbol-function 'vterm)
		     (lambda (&optional _name) (setq launched-shell vterm-shell) terminal))
		    ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
	    (should (eq (noema-agent-takeover--launch origin result) terminal)))
	  (should shutdown)
	  (should (equal launched-shell "codex resume native-1"))
	  (with-current-buffer terminal
	    (should (equal noema-agent-takeover--intervention-id "manual_1"))
	    (cl-letf (((symbol-function 'my/noema-api-call)
		       (lambda (_channel args callback &optional _timeout)
			 (setq handback-body (aref args 0))
			 (funcall callback (noema-research--table "ok" t) nil)))
		      ((symbol-function 'noema-agent-acp-start)
		       (lambda (&rest args)
			 (setq resumed-native (plist-get args :session-id))
			 resumed)))
	      (noema-agent-handback-session)))
	  (should (equal (alist-get 'interventionId handback-body) "manual_1"))
	  (should (equal resumed-native "native-1"))
	  (with-current-buffer resumed
	    (should (equal noema-agent-promote--session-id "ses_1"))))
      (dolist (buffer (list origin terminal resumed))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest noema-agent-worker-reports-the-frozen-notebook-identity-and-bounded-result ()
  (let* ((worker (noema-agent-worker--create
                  :run-id "run_1" :session-id "ses_1" :root "/tmp/noema-project/"
                  :spec (noema-research--table
                         "source" (noema-research--table "kind" "work-cell"
                                                        "file" "research/bound.noema" "cell_id" "c-work"))
                  :result-parts '("answer" "proof ")))
         (body (noema-agent-worker--worker-body worker)))
    (should (equal (alist-get 'notebookFile body) "/tmp/noema-project/research/bound.noema"))
    (should (equal (alist-get 'cellId body) "c-work"))
    (should (equal (noema-agent-worker--result-text worker) "proof answer"))))

(ert-deftest noema-agent-worker-terminal-batches-the-last-segment-before-status ()
  (let* ((worker (noema-agent-worker--create
                  :run-id "run_1" :session-id "ses_1" :root "/tmp/noema-project/"
                  :started t :epoch 1 :segments '("world" "hello ")
                  :result-parts '("world" "hello ")))
         captured)
    (cl-letf (((symbol-function 'noema-agent-worker--report)
               (lambda (_worker events &optional callback)
                 (setq captured events)
                 (when callback (funcall callback nil nil)))))
      (noema-agent-worker--terminal worker "completed")
      (should (= (length captured) 2))
      (should (equal (alist-get 'type (car captured)) "run.content.segment"))
      (should (equal (alist-get 'text (alist-get 'payload (car captured))) "hello world"))
      (should (equal (alist-get 'status (alist-get 'payload (cadr captured))) "completed"))
      (should (equal (alist-get 'transcript_text (alist-get 'payload (cadr captured))) "hello world")))))

(ert-deftest noema-agent-worker-makes-bootstrap-failure-durable-before-finishing ()
  (let* ((worker (noema-agent-worker--create
                  :run-id "run_boot" :root "/tmp/noema-project/"
                  :queue-state 'preparing))
         channel body)
    (puthash "run_boot" worker noema-agent-worker--runs)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-worker--api)
                   (lambda (actual-channel actual-body callback &optional _timeout)
                     (setq channel actual-channel body actual-body)
                     (funcall callback '((run . ((status . "failed")))) nil))))
          (noema-agent-worker--fail-prepared worker "adapter missing")
          (should (equal channel "aaronnote:api:research:run:fail-preparing"))
          (should (equal (alist-get 'failureReason body) "adapter missing"))
          (should (noema-agent-worker-terminal worker))
          (should-not (gethash "run_boot" noema-agent-worker--runs)))
      (remhash "run_boot" noema-agent-worker--runs))))

(ert-deftest noema-agent-worker-renewal-keeps-a-three-heartbeat-lease-window ()
  (let ((noema-agent-worker-lease-renew-seconds 10)
        (worker (noema-agent-worker--create
                 :run-id "run_renew" :session-id "ses_renew"
                 :root "/tmp/noema-project/" :epoch 4))
        channel body)
    (cl-letf (((symbol-function 'noema-agent-worker--api)
               (lambda (actual-channel actual-body callback &optional _timeout)
                 (setq channel actual-channel body actual-body)
                 (funcall callback '((lease . ((epoch . 4)))) nil))))
      (noema-agent-worker--renew worker)
      (should (equal channel "aaronnote:api:research:worker:lease"))
      (should (= (alist-get 'ttlMillis body) 30000)))))

(ert-deftest noema-agent-worker-reconciles-a-lost-start-response-without-reprompting ()
  (let* ((worker (noema-agent-worker--create
                  :run-id "run_ambiguous" :session-id "ses_1"
                  :root "/tmp/noema-project/" :epoch 3 :queue-state 'preparing))
         channels)
    (cl-letf (((symbol-function 'noema-agent-worker--api)
               (lambda (channel _body callback &optional _timeout)
                 (push channel channels)
                 (pcase channel
                   ("aaronnote:api:research:run:fail-preparing"
                    (funcall callback nil '((message . "cannot fail before dispatch from running"))))
                   ("aaronnote:api:research:run:get"
                    (funcall callback '((run . ((status . "running")))) nil))
                   ("aaronnote:api:research:worker:events"
                    (funcall callback '((events . [])) nil))))))
      (noema-agent-worker--fail-prepared worker "start response lost")
      (should (equal (nreverse channels)
                     '("aaronnote:api:research:run:fail-preparing"
                       "aaronnote:api:research:run:get"
                       "aaronnote:api:research:worker:events")))
      (should (noema-agent-worker-started worker))
      (should (noema-agent-worker-terminal worker)))))

(ert-deftest noema-agent-worker-bounds-results-on-a-valid-utf8-boundary ()
  (let ((noema-agent-worker-result-max-bytes 5)
        (worker (noema-agent-worker--create :result-parts '("甲乙丙"))))
    (should (equal (noema-agent-worker--result-text worker)
                   "甲\n\n[Result truncated by Noema]"))))

(ert-deftest noema-agent-worker-json-reader-preserves-false-values ()
  (should (eq (noema-agent-worker--value '((enabled . :false)) "enabled" t) :false))
  (should (eq (noema-agent-worker--value (noema-research--table "enabled" :false) "enabled" t) :false)))

(ert-deftest noema-agent-worker-projects-runs-and-items-into-a-replayable-magent-ledger ()
  (let* ((worker (noema-agent-worker--create
                  :run-id "run_ledger" :session-id "ses_ledger" :root "/tmp/noema-project/"
                  :spec (noema-research--table "prompt" "Prove it")
                  :segments nil :result-parts nil))
         thread turn replay)
    (noema-agent-worker--ledger-init worker)
    (noema-agent-worker--ledger-start worker)
    (noema-agent-worker--ledger-segment worker "Proof text")
    (noema-agent-worker--ledger-action
     worker '((:tool-call-id . "call_1") (:status . "completed") (:title . "Read file")))
    (noema-agent-worker--ledger-terminal worker "completed" nil)
    (setq thread (noema-agent-worker-ledger worker)
          turn (magent-thread-find-turn thread (noema-agent-worker-ledger-turn-id worker)))
    (should (eq (magent-thread-turn-status turn) 'completed))
    (should (= (length (magent-thread-turn-items turn)) 2))
    (should (seq-every-p #'magent-thread-terminal-item-p (magent-thread-turn-items turn)))
    (setq replay (magent-thread-replay
                  (magent-thread-create :id "ses_ledger" :session-id "ses_ledger")
                  (copy-sequence (magent-thread-journal thread))))
    (should (eq (magent-thread-turn-status
                 (magent-thread-find-turn replay (noema-agent-worker-ledger-turn-id worker)))
                'completed))))

(ert-deftest noema-agent-worker-freezes-only-the-active-preparation-ticket ()
  (let ((magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-ticket-adapters (make-hash-table :test #'eq))
        (noema-agent-worker--submissions (make-hash-table :test #'equal))
        calls callbacks)
    (cl-letf (((symbol-function 'my/noema--ensure-server)
               (lambda (callback) (funcall callback)))
              ((symbol-function 'noema-agent-worker--api)
               (lambda (channel body callback &optional _timeout)
                 (push (list channel body) calls)
                 (push callback callbacks))))
      (let ((first (noema-agent-worker--enqueue-preparation "/tmp/project/" '((cellId . "c-1"))))
            (second (noema-agent-worker--enqueue-preparation "/tmp/project/" '((cellId . "c-2")))))
        (should (= (length calls) 1))
        (should (eq (noema-agent-worker-queue-state
                     (gethash second noema-agent-worker--submissions))
                    'queued))
        (funcall (car callbacks) nil '((message . "first failed")))
        (should (= (length calls) 2))
        (should-not (gethash first noema-agent-worker--submissions))
        (should (eq (noema-agent-worker-queue-state
                     (gethash second noema-agent-worker--submissions))
                    'preparing))
        (funcall (car callbacks) nil '((message . "second failed")))
        (should-not magent-runtime-queue--arbiter-active)
        (should-not (gethash second noema-agent-worker--submissions))))))

(ert-deftest noema-agent-worker-waits-for-web-host-before-preparing-run ()
  (let ((magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-ticket-adapters (make-hash-table :test #'eq))
        (noema-agent-worker--submissions (make-hash-table :test #'equal))
        ready calls)
    (cl-letf (((symbol-function 'my/noema--ensure-server)
               (lambda (callback) (setq ready callback)))
              ((symbol-function 'noema-agent-worker--api)
               (lambda (channel _body _callback &optional _timeout)
                 (push channel calls))))
      (noema-agent-worker--enqueue-preparation
       "/tmp/project/" '((cellId . "c-cold")))
      (should (functionp ready))
      (should-not calls)
      (funcall ready)
      (should (equal calls '("aaronnote:api:research:run:prepare"))))))

(ert-deftest noema-research-attention-renders-versioned-shared-permission-actions ()
  (with-temp-buffer
    (noema-research-attention-mode)
    (setq-local noema-research-attention--origin "/tmp/noema-project/")
    (noema-research-attention--render
     (noema-research--table
      "permissions"
      (vector (noema-research--table
               "id" "perm_1" "version" 3
               "action" (noema-research--table "kind" "execute" "argv" ["go" "test" "./..."])
               "options" (vector (noema-research--table "optionId" "allow_once" "label" "Allow once")
                                 (noema-research--table "optionId" "reject_once" "label" "Reject"))))
      "inputRequests"
	  (vector (noema-research--table "id" "input_1" "runId" "run_2"
					"prompt" "Choose a proof" "inputKind" "text"))
      "inputRuns" (vector (noema-research--table "id" "run_2" "sessionId" "ses_1"))
      "proposals"
      (vector (noema-research--table
               "id" "prop_1" "kind" "cell.create" "status" "pending"
               "version" 1 "proposedBy" "agent:magent:curator"
               "payload" (noema-research--table
                           "cell" (noema-research--table "title" "Ghost route")))))
     nil)
    (should (string-match-p "Permissions (1)" (buffer-string)))
    (should (string-match-p "go test ./\\.\\.\\." (buffer-string)))
    (should (string-match-p "Input required (1)" (buffer-string)))
    (should (string-match-p "Proposals (1)" (buffer-string)))
    (should (string-match-p "Ghost route" (buffer-string)))
    (let ((button (next-button (point-min)))
          (count 0))
      (while button
        (setq count (1+ count)
              button (next-button (button-end button))))
      (should (= count 5)))))

(ert-deftest noema-research-synthesis-parses-only-nonauthoritative-json ()
  (let ((value (noema-research-synthesis--parse-model-json
                "{\"kind\":\"work\",\"title\":\"Route\",\"source\":\"Check it\"}")))
    (should (equal (gethash "title" value) "Route")))
  (should-error (noema-research-synthesis--parse-model-json
                 "```json\n{\"title\":\"Route\"}\n```")
                :type 'user-error)
  (should-error (noema-research-synthesis--parse-model-json
                 "{\"decision\":\"accept\",\"title\":\"Route\"}")
                :type 'user-error))

(ert-deftest noema-research-synthesis-cell-request-freezes-notebook-identity ()
  (noema-research-test--with-directory directory
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let* ((document (noema-research-test--document))
           (file (expand-file-name "research/bound.noema" directory))
           (model (noema-research--table
                   "kind" "work" "title" "Ghost route" "source" "Investigate"
                   "file" "../../escape.noema" "notebookId" "nb_hostile"
                   "acceptedRef" "noema://cell/nb_hostile/c-bad"))
           (body (noema-research-synthesis--build-request
                  "cell.create" model document file "sha256:abc" "magent:test" "c-q"))
           (payload (alist-get 'payload body))
           (cell (gethash "cell" payload)))
      (should (equal (alist-get 'kind body) "cell.create"))
      (should (equal (alist-get 'proposedBy body) "agent:magent:curator"))
      (should (equal (gethash "file" cell) "research/bound.noema"))
      (should (equal (gethash "notebookId" cell)
                     (noema-research-notebook-id document)))
      (should (equal (gethash "expectedRevision" cell) "sha256:abc"))
      (should (equal (gethash "lineageParent" cell) "c-q"))
      (should-not (gethash "acceptedRef" cell)))))

(ert-deftest noema-research-synthesis-refuses-local-only-model-context ()
  (let ((document (noema-research-test--document)))
    (noema-research-work-node-set
     (noema-research-work-node-for-cell document (noema-research-find-cell document "c-w"))
     "disclosure" "local_only")
    (noema-research-test--with-jutext document
      (noema-research-goto-cell "c-w")
      (should (noema-research-synthesis--local-context-p document)))))

(ert-deftest noema-research-graph-projects-pending-cells-as-noneditable-ghosts ()
  (let* ((document (noema-research-test--document))
         (notebook-id (noema-research-notebook-id document))
         (proposal (noema-research--table
                    "id" "prop_ghost" "kind" "cell.create" "status" "pending"
                    "payload" (noema-research--table
                               "cell" (noema-research--table
                                       "notebookId" notebook-id "cellId" "c-prop-ghost"
                                       "kind" "work" "title" "Pending route"
                                       "lineageParent" "c-q" "depends" ["c-w"]))))
         (projection (noema-research-graph--with-proposals
                      (noema-research-projection document) document (list proposal)))
         (ghost (seq-find (lambda (node) (equal (plist-get node :id) "c-prop-ghost"))
                          (plist-get projection :nodes))))
    (should (plist-get ghost :ghost))
    (should (equal (plist-get ghost :state) "pending"))
    (should (member (list (noema-research-test--work-id document "c-q")
                          "c-prop-ghost" "lineage")
                    (plist-get projection :edges)))
    (should (member (list (noema-research-test--work-id document "c-w")
                          "c-prop-ghost" "depends")
                    (plist-get projection :edges)))
    (with-temp-buffer
      (noema-research-graph-mode)
      (setq-local noema-research-graph--proposals (list proposal))
      (should-error (noema-research-graph--require-materialized "c-prop-ghost")
                    :type 'user-error))))

(ert-deftest noema-research-magent-sampler-has-no-tools-and-only-creates-a-proposal ()
  (require 'magent-llm)
  (require 'magent-llm-gptel)
  (noema-research-test--with-directory directory
    (write-region "schema = 1\n" nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let* ((file (expand-file-name "research/magent.noema" directory))
           (document (noema-research-test--document))
           (revision (noema-research-write-file file document))
           (buffer (find-file-noselect file))
           request api-channel api-body)
      (unwind-protect
          (with-current-buffer buffer
            (unless (derived-mode-p 'noema-research-mode)
              (noema-research-mode))
            (setq noema-research--revision revision)
            (set-buffer-modified-p nil)
            (cl-letf (((symbol-function 'magent-llm-gptel-sample)
                       (lambda (value)
                         (setq request value)
                         (funcall (magent-llm-request-callback value)
                                  (magent-llm-completed-event
                                   "{\"kind\":\"work\",\"title\":\"Proposed route\",\"source\":\"Check it.\"}"))
                         'mock-handle))
                      ((symbol-function 'my/noema-api-call)
                       (lambda (channel args callback &optional _timeout)
                         (setq api-channel channel api-body (aref args 0))
                         (funcall callback
                                  (noema-research--table
                                   "proposal" (noema-research--table "id" "prop_magent"))
                                  nil))))
              (noema-research-propose-with-magent "cell.create"))
            (should-not (magent-llm-request-tools request))
            (should (plist-get (magent-llm-request-metadata request)
                               :disable-provider-tools))
            (should (string-match-p "UNTRUSTED_RESEARCH_DATA"
                                    (magent-llm-request-prompt request)))
            (should (equal api-channel "aaronnote:api:research:proposal:create"))
            (should (equal (alist-get 'proposedBy api-body) "agent:magent:curator"))
            (should (equal (alist-get 'sourceAdapter api-body) "magent/gptel"))
            (should-not (buffer-modified-p)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest noema-loads-agent-and-compose-implementations-from-its-own-tree ()
  (dolist (library '("gptel" "agent-shell" "acp" "shell-maker"
                     "magent-agent-shell"))
    (let ((path (file-truename (or (locate-library library)
                                   (ert-fail (format "Missing %s" library))))))
      (should (string-prefix-p
               (file-truename
                (expand-file-name "site-lisp/noema/upstream/"
                                  user-emacs-directory))
               path))))
  (should-not (locate-library "ai-workbench")))

(ert-deftest noema-compose-delegates-to-the-internalized-gptel-ui ()
  (require 'gptel-rewrite)
  (require 'gptel-transient)
  (require 'gptel-context)
  (let (sent menu rewrite context)
    (cl-letf (((symbol-function 'gptel-send) (lambda (&optional arg) (setq sent arg)))
              ((symbol-function 'gptel-menu) (lambda () (setq menu t)))
              ((symbol-function 'gptel-rewrite) (lambda () (setq rewrite t)))
              ((symbol-function 'gptel-add) (lambda (&optional arg) (setq context arg))))
      (noema-compose-send '(4))
      (noema-compose-menu)
      (noema-compose-rewrite)
      (noema-compose-add-context '(4))
      (should (equal sent '(4)))
      (should menu)
      (should rewrite)
      (should (equal context '(4))))))

;;; noema-research-tests.el ends here

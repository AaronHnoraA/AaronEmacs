;;; noema-research-tests.el --- Research notebook ERT suite -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name "../lisp/roam"
                               (file-name-directory (or load-file-name
                                                        buffer-file-name))))
(require 'noema-research)


(require 'noema-agent-promote)
(require 'noema-agent-worker)
(require 'noema-agent-takeover)
(require 'noema-research-inspector)
(require 'noema-research-synthesis)
(require 'tools/agent/compose)
(require 'noema-pi-router)
(require 'noema-sessions)

(defconst noema-research-test--project-manifest "schema = 1\n[project]\nid = \"test\"\n"
  "A minimal D-038 Project manifest.")

(defun noema-research-test--accept-proposed (&rest args)
  "Stand in for `completing-read', accepting its default (the proposal)."
  (nth 6 args))

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





(defun noema-research-test--chain-document (length)
  "Return the representative notebook extended by a LENGTH-step work chain."
  (let* ((document (noema-research-test--document))
         (parent (noema-research-test--work-id document "c-k")))
    (dotimes (index length)
      (setq parent (noema-research-cell-work-node-id
                    (noema-research-test--append-work-cell
                     document (format "c-chain-%d" index) "work"
                     (format "Step %d of a long investigation" index)
                     "Continue." (list parent)))))
    document))

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













(ert-deftest noema-research-links-are-atomic-and-keep-the-dag-acyclic ()
  (let* ((document (noema-research-test--document))
         (q (noema-research-test--work-id document "c-q"))
         (w (noema-research-test--work-id document "c-w"))
         (k (noema-research-test--work-id document "c-k")))
    (should (noema-research-add-relation document q k "depends"))
    (should-not (noema-research-add-relation document q k "depends"))
    (should (equal (noema-research-relation-parents document k "depends") (list q)))
    (should (equal (noema-research-relation-children document q "depends") (list k)))
    (should-error (noema-research-add-relation document k q "lineage") :type 'user-error)
    (should-error (noema-research-add-relation document w w "depends") :type 'user-error)
    (should (noema-research-remove-relation document q k "depends"))
    (should-not (noema-research-remove-relation document q k "depends"))
    (should (equal (noema-research-relation-parents document k "lineage") (list w)))))

(ert-deftest noema-research-deleting-a-middle-node-can-reconnect-its-children ()
  (let* ((document (noema-research-test--document))
         (q (noema-research-test--work-id document "c-q"))
         (w (noema-research-test--work-id document "c-w"))
         (k (noema-research-test--work-id document "c-k")))
    (noema-research-delete-work-node document w nil t)
    (should-not (noema-research-find-work-node document w))
    (should (equal (noema-research-relation-parents document k "lineage") (list q)))
    (should-not (plist-get (noema-research-validate document) :errors))))

(ert-deftest noema-research-work-node-choices-are-unique-human-labels ()
  (let* ((document (noema-research-test--document))
         (w (noema-research-test--work-id document "c-w")))
    (noema-research-test--append-work-cell
     document "c-dup" "work" "Spectral exploration" "Second attempt.")
    (let* ((orphan (noema-research-work-node-id
                    (noema-research-bind-cell
                     document (noema-research-test--cell "c-loose" "note" "" "")
                     "checkpoint" "Loose end")))
           (choices (noema-research-work-node-choices document :near w))
           (labels (mapcar #'car choices)))
      (should (= (length labels) (length (delete-dups (copy-sequence labels)))))
      (should-not (seq-some (lambda (label) (string-match-p "wn_\\|\\[" label)) labels))
      (should (member "work: Spectral exploration ⟨1⟩" labels))
      (should (member "work: Spectral exploration ⟨2⟩" labels))
      (should (member "checkpoint: Loose end · no Cell" labels))
      (should (equal (cdr (car choices)) (noema-research-test--work-id document "c-q")))
      (should (equal (cdr (car (last choices))) orphan)))))















































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
        ;; Focus makes the node the root: a leaf focus draws only itself.
        (should (equal focused-ids (list r)))
        (should (equal (plist-get focused :focus) r)))
      ;; Focusing the folded node shows its branch; the fold stays recorded
      ;; for the unfocused board but does not contract the focus.
      (let ((focused-ids (mapcar (lambda (node) (plist-get node :id))
                                 (plist-get (noema-research-projection
                                             document :folds (list w) :focus w)
                                            :nodes))))
        (should (equal focused-ids
                       (list w k r (noema-research-test--work-id document "c-a"))))))))

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
    (write-region noema-research-test--project-manifest nil (expand-file-name "noema.toml" directory) nil 'silent)
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
    (write-region noema-research-test--project-manifest nil (expand-file-name "noema.toml" directory) nil 'silent)
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
                 (concat "\\`schema = 1\n\n\\[project\\]\nid = "
                         "\\\"[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-7[0-9a-f]\\{3\\}-"
                         "[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\\"\n\\'")
                 manifest-text))
        (should-not (plist-get (noema-project-enable directory) :created))
        (should (equal manifest-text
                       (with-temp-buffer
                         (insert-file-contents manifest)
                         (buffer-string))))
        ;; A Project is not a Git concept: `.agent/' ignores itself and the
        ;; person's ignore file is left alone.
        (should (equal (with-temp-buffer (insert-file-contents ignore) (buffer-string))
                       "dist/\n"))
        (should (equal (with-temp-buffer
                         (insert-file-contents (expand-file-name ".agent/.gitignore" directory))
                         (buffer-string))
                       "*\n"))))))

(ert-deftest noema-project-enable-keeps-a-repository-and-a-legacy-id ()
  (noema-research-test--with-directory directory
    (let ((manifest (expand-file-name "noema.toml" directory)))
      ;; A Wiki repository chosen as a Project keeps its repository identity.
      (write-region "schema = 1\nrepository_id = \"repo\"\nnamespace = \"Math\"\n" nil manifest nil 'silent)
      (should-not (noema-project-root directory))
      (should (plist-get (noema-project-enable directory) :created))
      (let ((text (with-temp-buffer (insert-file-contents manifest) (buffer-string))))
        (should (string-prefix-p "schema = 1\nrepository_id = \"repo\"\nnamespace = \"Math\"\n\n[project]\nid = \"" text))
        (should-not (equal (noema-project-id directory) "repo")))
      ;; A manifest whose Runs predate D-038 keeps the id they recorded.
      (write-region "schema = 1\nrepository_id = \"legacy\"\n" nil manifest nil 'silent)
      (make-directory (expand-file-name ".agent" directory) t)
      (write-region "" nil (expand-file-name ".agent/state.sqlite" directory) nil 'silent)
      (should (equal (noema-project-root directory) directory))
      (should (equal (noema-project-ensure (expand-file-name "a.noema" directory)) directory))
      (should (equal (noema-project-id directory) "legacy"))
      (should (plist-get (noema-project--manifest directory) :project-p)))))

(ert-deftest noema-project-root-is-a-query-for-the-nearest-project ()
  (noema-research-test--with-directory directory
    (let* ((nested (expand-file-name "a/b/" directory))
           (file (expand-file-name "new.noema" nested)))
      (make-directory nested t)
      (should-not (noema-project-root file))
      (should-not (file-exists-p (expand-file-name "noema.toml" nested)))
      ;; A Wiki repository manifest alone does not make a Project.
      (write-region "schema = 1\nrepository_id = \"vault\"\n" nil
                    (expand-file-name "noema.toml" directory) nil 'silent)
      (should-not (noema-project-root file))
      (should (equal (noema-project-scope file) nested))
      (write-region noema-research-test--project-manifest nil
                    (expand-file-name "noema.toml" (expand-file-name "a/" directory)) nil 'silent)
      (should (equal (noema-project-root file) (expand-file-name "a/" directory)))
      (should (equal (noema-project-root nested) (expand-file-name "a/" directory))))))

(ert-deftest noema-project-default-root-follows-code-not-vaults ()
  (noema-research-test--with-directory directory
    (let ((nested (expand-file-name "notes/sub/" directory)))
      (make-directory nested t)
      ;; In a code repository the Project spans the repository.
      (let ((project-find-functions (list (lambda (_) (cons 'transient directory)))))
        (should (equal (noema-project-default-root nested) directory))
        ;; In a vault, which already carries a manifest, it does not.
        (write-region "schema = 1\nrepository_id = \"vault\"\n" nil
                      (expand-file-name "noema.toml" directory) nil 'silent)
        (should (equal (noema-project-default-root nested) nested))
        (should (equal (noema-project-candidate-roots nested)
                       (list nested (expand-file-name "notes/" directory) directory))))
      (let ((project-find-functions nil))
        (should (equal (noema-project-default-root nested) nested))))))

(ert-deftest noema-project-workspace-defaults-to-the-root-and-round-trips ()
  (noema-research-test--with-directory directory
    (let ((root (expand-file-name "notes/lce/" directory))
          (code (expand-file-name "code/lce/" directory)))
      (make-directory root t)
      (make-directory code t)
      (noema-project-enable root)
      (should (equal (noema-project-workspace root) root))
      (should (equal (noema-project-set-workspace root code) code))
      ;; Outside the Project a workspace is written absolute (`~/…' under
      ;; home), so moving the notes does not break it; inside, relative.
      (should (equal (plist-get (noema-project--manifest root) :workspace)
                     (directory-file-name (abbreviate-file-name code))))
      (make-directory (expand-file-name "src/" root))
      (noema-project-set-workspace root (expand-file-name "src/" root))
      (should (equal (plist-get (noema-project--manifest root) :workspace) "src"))
      (noema-project-set-workspace root code)
      (should (equal (noema-project-workspace root) code))
      (noema-project-set-workspace root nil)
      (should-not (plist-get (noema-project--manifest root) :workspace))
      (should (equal (noema-project-workspace root) root))
      (should-error (noema-project-set-workspace root (expand-file-name "missing/" directory))
                    :type 'user-error))))

(ert-deftest noema-project-paths-are-native-at-the-host-boundary ()
  (noema-research-test--with-directory directory
    (write-region noema-research-test--project-manifest nil
                  (expand-file-name "noema.toml" directory) nil 'silent)
    (let ((logical (concat "/fs:local:" directory)))
      (cl-letf (((symbol-function 'remote-client-file-name)
                 (lambda (name &rest _) (string-remove-prefix "/fs:local:" name))))
        (should (equal (noema-project-client-path (concat logical "x.noema"))
                       (expand-file-name "x.noema" directory)))
        (should (equal (noema-project-root (concat logical "x.noema")) directory))
        ;; Every file sent to the host, such as an index sync, is native.
        (should (equal (noema-project-host-file (concat logical "x.noema"))
                       (expand-file-name "x.noema" directory))))
      (cl-letf (((symbol-function 'remote-client-file-name) (lambda (&rest _) nil)))
        (should-not (noema-project-client-path "/fs:gpu:/srv/p/x.noema"))
        (should-error (noema-project-ensure "/fs:gpu:/srv/p/x.noema") :type 'user-error)))))

(ert-deftest noema-project-ensure-asks-before-creating-at-the-proposed-root ()
  (noema-research-test--with-directory directory
    (let* ((nested (expand-file-name "notes/sub/" directory))
           (file (expand-file-name "a.noema" nested))
           (project-find-functions (list (lambda (_) (cons 'transient directory))))
           proposed)
      (make-directory nested t)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest args)
                   (setq proposed (nth 6 args))
                   proposed)))
        (should (equal (noema-project-ensure file) directory)))
      (should (equal (expand-file-name proposed) directory))
      (should (file-regular-p (expand-file-name "noema.toml" directory)))
      (should-not (file-exists-p (expand-file-name "noema.toml" nested)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (error "An existing project must not prompt"))))
        (should (equal (noema-project-ensure file) directory))))))

(ert-deftest noema-project-ensure-writes-nothing-when-declined-or-misplaced ()
  (noema-research-test--with-directory directory
    (let ((file (expand-file-name "work/a.noema" directory))
          (elsewhere (file-name-as-directory (make-temp-file "noema-elsewhere-" t)))
          (project-find-functions nil))
      (make-directory (expand-file-name "work/" directory))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (signal 'quit nil))))
              ;; `should-error' does not catch `quit'.
              (should (eq (condition-case nil (noema-project-ensure file) (quit 'quit))
                          'quit)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) elsewhere)))
              (should-error (noema-project-ensure file) :type 'user-error))
            (should-not (noema-project-root file))
            (should-not (file-exists-p (expand-file-name "noema.toml" elsewhere))))
        (delete-directory elsewhere t)))))





















(ert-deftest noema-research-jupyter-output-reuses-logical-remote-source-buffer ()
  (let ((source (generate-new-buffer " *noema-logical-source*"))
        (logical "/fs:remote:/work/research.noema")
        (host "/work/research.noema"))
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq buffer-file-name logical))
          (cl-letf (((symbol-function '+noema--host-file)
                     (lambda (file) (if (equal file logical) host file)))
                    ((symbol-function 'find-file-noselect)
                     (lambda (&rest _) (ert-fail "Opened duplicate remote buffer"))))
            (should (eq (+noema-jupyter-cell--source-buffer host) source))))
      (kill-buffer source))))

(ert-deftest noema-research-jupyter-source-opens-local-host-file-from-remote-buffer ()
  (let ((default-directory "/fs:box:/work/")
        visited context)
    (cl-letf (((symbol-function 'find-file-noselect)
               (lambda (file &rest _)
                 (setq visited file context default-directory))))
      (+noema-jupyter-cell--source-buffer
       "/Users/test/notes/source.noema"))
    (should (equal visited "/Users/test/notes/source.noema"))
    (should-not (file-remote-p context))))













(ert-deftest noema-research-projection-drops-upward-edges-into-a-fold ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (checkpoint (noema-research-test--work-id document "c-k"))
         (current (noema-research-cell-work-node-id
                   (noema-research-test--append-work-cell
                    document "c-current" "work" "Current work" "Continue."
                    (list checkpoint))))
         (after (noema-research-cell-work-node-id
                 (noema-research-test--append-work-cell
                  document "c-after" "work" "Hidden follow-up" "Later."
                  (list current) (list checkpoint))))
         (projection (noema-research-projection
                      document :folds (list work) :protect (list current)))
         (ids (mapcar (lambda (node) (plist-get node :id))
                      (plist-get projection :nodes)))
         (edges (plist-get projection :edges)))
    (should (member current ids))
    (should-not (member after ids))
    ;; Edges into the hidden follow-up must not be redirected upward to the
    ;; fold owner, which would draw a false cycle.
    (should-not (member (list current work "lineage") edges))
    (should-not (member (list checkpoint work "depends") edges))
    (should-not (seq-find (lambda (edge)
                            (and (equal (nth 1 edge) work)
                                 (member (nth 0 edge) (list checkpoint current))))
                          edges))
    ;; The legitimate downward path through the fold owner remains.
    (should (member (list work checkpoint "lineage") edges))
    (should (member (list checkpoint current "lineage") edges))))





(ert-deftest noema-research-graph-focus-makes-the-node-the-root ()
  (let* ((document (noema-research-test--document))
         (question (noema-research-test--work-id document "c-q"))
         (work (noema-research-test--work-id document "c-w"))
         (checkpoint (noema-research-test--work-id document "c-k"))
         (repair (noema-research-cell-work-node-id
                  (noema-research-test--append-work-cell
                   document "c-r" "work" "Repair" "" (list checkpoint))))
         (earlier (noema-research-cell-work-node-id
                   (noema-research-test--append-work-cell
                    document "c-s" "work" "Earlier modelling" "" (list question))))
         (projection (noema-research-projection
                      document :focus work :depth 2 :folds (list work)
                      :protect (list work)))
         (ids (mapcar (lambda (node) (plist-get node :id)) (plist-get projection :nodes))))
    ;; No ancestor or sibling of the focus is drawn, the focus has no drawn
    ;; parent, and a fold on the focus itself does not hide its branch.
    (should (equal ids (list work checkpoint repair)))
    (should-not (member question ids))
    (should-not (member earlier ids))
    (should-not (plist-get (car (plist-get projection :nodes)) :parents))
    (should-not (plist-get projection :folds))))













































(ert-deftest noema-research-views-keep-graph-state-and-unknown-keys ()
  (noema-research-test--with-directory directory
    (write-region noema-research-test--project-manifest nil (expand-file-name "noema.toml" directory) nil 'silent)
    (let ((file (expand-file-name "research.noema" directory))
          (document (noema-research-test--document)))
      (noema-research-view-update
       file document
       (lambda (view)
         (puthash "future" "kept" view)
         (puthash "unfolds" ["c-w"] view)
         (puthash "focus_depth" 3 view)
         (puthash "viewport" (noema-research--table "dx" -40 "dy" 12 "scale" 1.25) view)))
      (noema-research-view-write file document "c-q" nil "overview")
      (should (equal (noema-research-view-read file document)
                     '(:focus "c-q" :folds nil :zoom "overview" :unfolds ("c-w")
                       :focus-depth 3 :viewport (:dx -40 :dy 12 :scale 1.25))))
      (should (equal (noema-research--get
                      (noema-research--view-object
                       (noema-research-view-file file document))
                      "future")
                     "kept")))))















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
    (let ((old-ready (and (boundp '+noema--ready) +noema--ready))
          requested-channel requested-args)
      (unwind-protect
          (progn
            (setq +noema--ready t)
            (cl-letf (((symbol-function '+noema-api-call)
                       (lambda (channel args callback &optional _timeout)
                         (setq requested-channel channel
                               requested-args args)
                         (funcall callback (noema-research--table "id" "ses_noema") nil))))
              (noema-agent-promote-current-session "Bound" "Improve it")
              (should (equal requested-channel
                             "noema:api:research:session:promote"))
              (should (equal (alist-get 'goal (aref requested-args 0)) "Improve it"))
              (should (equal noema-agent-promote--session-id "ses_noema"))))
        (setq +noema--ready old-ready)))))

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
	 (launched-command nil)
	 (handback-body nil)
	 (resumed-native nil)
	 (result (noema-research--table
		  "root" "/tmp/project/"
		  "command" ["codex" "resume" "native-1"]
		  "session" (noema-research--table "id" "ses_1" "nativeSessionId" "native-1")
		  "intervention" (noema-research--table "id" "manual_1" "version" 1))))
    (unwind-protect
	(progn
	  (require 'term/portal/config)
	  (with-current-buffer origin
	    (setq-local agent-shell--state '(:agent-config (:identifier codex))))
	  (cl-letf (((symbol-function 'executable-find) (lambda (_name) "/bin/codex"))
		    ((symbol-function '+agent-acp-shutdown)
                     (lambda (_buffer) (setq shutdown t)))
		    ((symbol-function '+portal-create)
		     (lambda (&rest args)
		       (setq launched-command (plist-get args :command))
		       terminal))
		    ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
	    (should (eq (noema-agent-takeover--launch origin result) terminal)))
	  (should shutdown)
	  (should (equal launched-command '("codex" "resume" "native-1")))
	  (with-current-buffer terminal
	    (should (equal noema-agent-takeover--intervention-id "manual_1"))
	    (cl-letf (((symbol-function '+noema-api-call)
		       (lambda (_channel args callback &optional _timeout)
			 (setq handback-body (aref args 0))
			 (funcall callback (noema-research--table "ok" t) nil)))
		      ((symbol-function '+agent-acp-start)
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

(ert-deftest noema-agent-worker-does-not-report-unobserved-acp-usage ()
  (let* ((worker (noema-agent-worker--create
                  :run-id "run_1" :session-id "ses_1" :root "/tmp/noema-project/"
                  :started t :usage-baseline 0))
         (usage '(:total 0 :input 0 :output 0 :thought 0
                         :cached-read 0 :cached-write 0 :context-used 0 :context-size 0
                         :report-seq 0)))
    (cl-letf (((symbol-function '+agent-acp-usage) (lambda (_buffer) usage)))
      (should-not (alist-get 'sessionUsage (noema-agent-worker--worker-body worker)))
      (setq usage (plist-put usage :context-size 1000))
      (should-not (alist-get 'sessionUsage (noema-agent-worker--worker-body worker)))
      (setq usage (plist-put usage :report-seq 1))
      (should (equal (alist-get 'contextSize
                               (alist-get 'sessionUsage
                                          (noema-agent-worker--worker-body worker)))
                     1000))
      (setf (noema-agent-worker-usage-baseline worker) 1)
      (should-not (alist-get 'sessionUsage (noema-agent-worker--worker-body worker))))))

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
          (should (equal channel "noema:api:research:run:fail-preparing"))
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
      (should (equal channel "noema:api:research:worker:lease"))
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
                   ("noema:api:research:run:fail-preparing"
                    (funcall callback nil '((message . "cannot fail before dispatch from running"))))
                   ("noema:api:research:run:get"
                    (funcall callback '((run . ((status . "running")))) nil))
                   ("noema:api:research:worker:events"
                    (funcall callback '((events . [])) nil))))))
      (noema-agent-worker--fail-prepared worker "start response lost")
      (should (equal (nreverse channels)
                     '("noema:api:research:run:fail-preparing"
                       "noema:api:research:run:get"
                       "noema:api:research:worker:events")))
      (should (noema-agent-worker-started worker))
      (should (noema-agent-worker-terminal worker)))))

(ert-deftest noema-agent-worker-bounds-results-on-a-valid-utf8-boundary ()
  (let ((noema-agent-worker-result-max-bytes 5)
        (worker (noema-agent-worker--create :started t)))
    ;; The hot-path accumulator enforces the limit before retaining parts.
    (cl-letf (((symbol-function 'run-at-time) #'ignore)
              ((symbol-function 'noema-agent-worker--ledger-segment) #'ignore))
      (noema-agent-worker--queue-segment worker "甲乙丙")
      (should (= (noema-agent-worker-result-bytes worker) 3))
      (should (equal (noema-agent-worker--result-text worker)
                     "甲\n\n[Result truncated by Noema]")))))

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

(ert-deftest noema-agent-worker-bounds-long-lived-ledger-projections ()
  (let ((noema-agent-worker--ledgers (make-hash-table :test #'equal))
        (noema-agent-worker-ledger-turn-limit 2)
        workers)
    (dotimes (index 2)
      (let ((worker (noema-agent-worker--create
                     :run-id (format "run_rotate_%d" index) :session-id "ses_rotate"
                     :root "/tmp/noema-project/" :spec (noema-research--table "prompt" "Go")
                     :segments nil :result-parts nil)))
        (noema-agent-worker--ledger-init worker)
        (noema-agent-worker--ledger-start worker)
        (should (gethash "ses_rotate" noema-agent-worker--ledgers))
        (noema-agent-worker--ledger-terminal worker "completed" nil)
        (push worker workers)))
    ;; Both Runs shared one projection; reaching the limit rotates it away.
    (should (eq (noema-agent-worker-ledger (car workers))
                (noema-agent-worker-ledger (cadr workers))))
    (should-not (gethash "ses_rotate" noema-agent-worker--ledgers))
    ;; A fresh Run's provisional Run-id key does not outlive its Session.
    (let ((fresh (noema-agent-worker--create
                  :run-id "run_fresh" :root "/tmp/noema-project/"
                  :spec (noema-research--table "prompt" "Go") :segments nil :result-parts nil)))
      (noema-agent-worker--ledger-init fresh)
      (should (gethash "run_fresh" noema-agent-worker--ledgers))
      (setf (noema-agent-worker-session-id fresh) "ses_fresh")
      (noema-agent-worker--ledger-attach-session fresh)
      (should-not (gethash "run_fresh" noema-agent-worker--ledgers))
      (should (gethash "ses_fresh" noema-agent-worker--ledgers)))))

(ert-deftest noema-agent-worker-freezes-only-the-active-preparation-ticket ()
  (let ((magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-ticket-adapters (make-hash-table :test #'eq))
        (noema-agent-worker--submissions (make-hash-table :test #'equal))
        calls callbacks)
    (cl-letf (((symbol-function '+noema--ensure-server)
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

;; Adopted from Pisper's bounded agent slots: freezing stays serial, a
;; dispatched Run releases the queue, and execution is capped.
(defmacro noema-research-test--with-run-slots (limit &rest body)
  "Run BODY with an isolated arbiter, LIMIT slots and recorded prepare calls.
BODY sees `calls' (newest first) and `callbacks' for prepare responses."
  (declare (indent 1))
  `(let ((magent-runtime-queue--arbiter-active nil)
         (magent-runtime-queue--arbiter-pending nil)
         (magent-runtime-queue--active nil)
         (magent-runtime-queue--pending nil)
         (magent-runtime-queue--arbiter-ticket-adapters (make-hash-table :test #'eq))
         (noema-agent-worker--submissions (make-hash-table :test #'equal))
         (noema-agent-worker--runs (make-hash-table :test #'equal))
         (noema-agent-worker--slot-waiting nil)
         (noema-agent-worker--busy-waiting nil)
         (noema-agent-worker-max-concurrent-runs ,limit)
         calls callbacks)
     (cl-letf (((symbol-function '+noema--ensure-server) (lambda (callback) (funcall callback)))
               ((symbol-function 'run-at-time)
                (lambda (_time _repeat function &rest args) (apply function args) nil))
               ((symbol-function 'noema-agent-worker--ledger-init) #'ignore)
               ((symbol-function 'noema-agent-worker--dispatch) #'ignore)
               ((symbol-function 'noema-agent-worker--api)
                (lambda (channel body callback &optional _timeout)
                  (push (list channel body) calls)
                  (push callback callbacks))))
       ,@body)))

(defun noema-research-test--prepared (run-id)
  "Return a prepare response for RUN-ID."
  (noema-research--table "root" "/tmp/project/"
                         "run" (noema-research--table "id" run-id)
                         "spec" (noema-research--table "agent" "codex")))

(ert-deftest noema-agent-worker-dispatch-frees-the-queue-for-parallel-runs ()
  (noema-research-test--with-run-slots 3
    (noema-agent-worker--enqueue-preparation "/tmp/project/" '((cellId . "c-1")))
    (noema-agent-worker--enqueue-preparation "/tmp/project/" '((cellId . "c-2")))
    ;; Freezing is serial: only the first RunSpec is being prepared.
    (should (= (length calls) 1))
    (funcall (car callbacks) (noema-research-test--prepared "run_1") nil)
    ;; Dispatching the first releases the queue while it keeps executing.
    (should (gethash "run_1" noema-agent-worker--runs))
    (should (= (length calls) 2))
    (funcall (car callbacks) (noema-research-test--prepared "run_2") nil)
    (should (= (noema-agent-worker--executing-count) 2))
    (should-not magent-runtime-queue--arbiter-active)))

(ert-deftest noema-agent-worker-waits-in-order-for-an-execution-slot ()
  (noema-research-test--with-run-slots 1
    (noema-agent-worker--enqueue-preparation "/tmp/project/" '((cellId . "c-1")))
    (funcall (car callbacks) (noema-research-test--prepared "run_1") nil)
    (let ((second (noema-agent-worker--enqueue-preparation "/tmp/project/" '((cellId . "c-2")))))
      ;; The slot is taken: the second Run parks without freezing a RunSpec.
      (should (= (length calls) 1))
      (should (equal noema-agent-worker--slot-waiting
                     (list (gethash second noema-agent-worker--submissions))))
      (should-not magent-runtime-queue--arbiter-active)
      ;; The first Run ends and its slot goes to the waiting Run.
      (let ((first (gethash "run_1" noema-agent-worker--runs)))
        (remhash "run_1" noema-agent-worker--runs)
        (noema-agent-worker--finish-queue first 'completed))
      (should-not noema-agent-worker--slot-waiting)
      (should (= (length calls) 2))
      (should (equal (alist-get 'cellId (cadr (car calls))) "c-2")))))

(ert-deftest noema-agent-worker-waits-for-web-host-before-preparing-run ()
  (let ((magent-runtime-queue--arbiter-active nil)
        (magent-runtime-queue--arbiter-pending nil)
        (magent-runtime-queue--active nil)
        (magent-runtime-queue--pending nil)
        (magent-runtime-queue--arbiter-ticket-adapters (make-hash-table :test #'eq))
        (noema-agent-worker--submissions (make-hash-table :test #'equal))
        ready calls)
    (cl-letf (((symbol-function '+noema--ensure-server)
               (lambda (callback) (setq ready callback)))
              ((symbol-function 'noema-agent-worker--api)
               (lambda (channel _body _callback &optional _timeout)
                 (push channel calls))))
      (noema-agent-worker--enqueue-preparation
       "/tmp/project/" '((cellId . "c-cold")))
      (should (functionp ready))
      (should-not calls)
      (funcall ready)
      (should (equal calls '("noema:api:research:run:prepare"))))))

(ert-deftest noema-research-attention-previews-proposed-file-changes ()
  (let ((noema-agent-worker--previews (make-hash-table :test #'equal)))
    (puthash "perm_1"
             (noema-agent-worker--diff-preview
              '((:diffs . (((:old . "a\nb\n") (:new . "a\nc\nd\n") (:file . "/w/src/m.py"))))))
             noema-agent-worker--previews)
    (with-temp-buffer
      (noema-research-attention--insert-preview "perm_1")
      (noema-research-attention--insert-preview "perm_unknown")
      (should (equal (buffer-string) "  change: /w/src/m.py  +3 -2  diff\n"))
      (should (button-at (- (point-max) 2))))
    (noema-research-attention--show-diff (car (noema-agent-worker-permission-preview "perm_1")))
    (with-current-buffer "*Noema proposed change: m.py*"
      (should (string-match-p "^-b$" (buffer-string)))
      (should (string-match-p "^\\+d$" (buffer-string)))
      (kill-buffer))))

(ert-deftest noema-research-attention-renders-versioned-shared-permission-actions ()
  (with-temp-buffer
    (noema-research-attention-mode)
    (setq-local noema-research-attention--origin "/tmp/noema-project/")
    (noema-research-attention--render
     (noema-research--table
      "permissions"
      (vector (noema-research--table
               "id" "perm_1" "version" 3
               "policyReason" "src/model.py is also being edited by open Run run_9"
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
    (should (string-match-p "why: src/model.py is also being edited by open Run run_9" (buffer-string)))
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
    (write-region noema-research-test--project-manifest nil (expand-file-name "noema.toml" directory) nil 'silent)
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













(ert-deftest noema-loads-package-managed-agents-and-embedded-composition ()
  (dolist (library '("agent-shell" "acp" "shell-maker"))
    (should (file-in-directory-p (locate-library library) package-user-dir)))
  (dolist (library '("gptel" "magent-agent-shell"))
    (let ((path (file-truename (or (locate-library library)
                                   (ert-fail (format "Missing %s" library))))))
      (should (string-prefix-p
               (file-truename
                (expand-file-name "site-lisp/"
                                  user-emacs-directory))
               path))))
  (should-not (locate-library "ai-workbench")))

(ert-deftest +agent/compose-uses-document-context-and-shared-agent-actions ()
  (require 'tools/agent/menu)
  (with-temp-buffer
    (setq major-mode 'markdown-mode buffer-file-name "/tmp/agent-task.md")
    (let (sent menu rewrite context)
      (cl-letf (((symbol-function '+agent-editor-action) (lambda (&rest _) nil))
                ((symbol-function '+agent-context--buffer-file) (lambda (_) buffer-file-name))
                ((symbol-function '+agent-context--file-references) (lambda (file) (list file)))
                ((symbol-function '+agent-context--send) (lambda (&rest args) (setq sent args)))
                ((symbol-function '+agent-session-menu) (lambda () (interactive) (setq menu t)))
                ((symbol-function '+agent-context-send-region) (lambda () (setq rewrite t)))
                ((symbol-function '+agent-selection-add) (lambda (arg) (setq context arg))))
        (+agent/compose-send '(4))
        (+agent/compose-menu)
        (+agent/compose-rewrite)
        (+agent/compose-add-context '(4))
        (should (equal (plist-get sent :pick) '(4)))
        (should (equal (plist-get sent :references) '("/tmp/agent-task.md")))
        (should menu) (should rewrite) (should (equal context '(4)))
        (setq major-mode 'fundamental-mode)
        (should-error (+agent/compose-send) :type 'user-error)))))

(ert-deftest noema-pi-router-root-anchors-on-the-nearest-noema-toml ()
  (noema-research-test--with-directory root
    (write-region noema-research-test--project-manifest nil (expand-file-name "noema.toml" root) nil 'silent)
    (let ((nested (expand-file-name "nested/deeper/" root)))
      (make-directory nested t)
      (should (equal (noema-pi-router--root nested) (file-name-as-directory root))))))

(ert-deftest noema-pi-router-open-reuses-a-live-buffer-for-the-same-root ()
  (noema-research-test--with-directory root
    (let ((buffer (generate-new-buffer " *noema-pi-router-fake*"))
          started popped)
      (unwind-protect
          (progn
            (puthash (noema-pi-router--root root) buffer noema-pi-router--buffers)
            (with-current-buffer buffer (setq major-mode 'agent-shell-mode))
            (cl-letf (((symbol-function 'noema-pi-emacs-ensure) #'ignore)
                      ((symbol-function '+agent-acp-start)
                       (lambda (&rest _) (setq started t) buffer))
                      ((symbol-function '+agent-acp-show-buffer)
                       (lambda (target &rest _) (setq popped target))))
              (noema-pi-router-open root))
            (should-not started)
            (should (eq popped buffer)))
        (kill-buffer buffer)))))

;;; D-031 ~ D-034: named sessions, Pi coordinator, agent buffers, disk sync

(defmacro noema-pi-test--with-agent-directory (root &rest body)
  "Run BODY with the Pi manager deployed under ROOT, never in the real one."
  (declare (indent 1))
  `(let ((noema-pi-router--emacs-buffer nil)
         (noema-pi-router--buffers (make-hash-table :test #'equal))
         (noema-pi-agent-directory (expand-file-name "pi-agent/" ,root))
         (noema-pi-config-directory (expand-file-name "pi-config/" ,root))
         (noema-pi-companion-directory (expand-file-name "companion/" ,root))
         (noema-pi-credentials-file (expand-file-name "no-credentials/auth.json" ,root))
         (noema-pi-codex-auth-file (expand-file-name "no-codex/auth.json" ,root))
         (noema-pi-model "")
         (noema-pi-model-pool '("openai-codex/gpt-5.6-luna"
                                "deepseek/deepseek-v4-flash"
                                "openai-codex/gpt-5.6-sol"
                                "openai-codex/gpt-6-astra"
                                "anthropic/claude-sonnet-5"))
         (noema-pi-thinking-level "off"))
     ,@body))

(ert-deftest noema-pi-project-resumes-native-id-with-progressive-api ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let ((buffer (generate-new-buffer " *noema-pi-child*"))
            (parent (generate-new-buffer " *noema-pi-emacs*"))
            started)
        (unwind-protect
            (cl-letf (((symbol-function 'noema-pi-emacs-ensure) (lambda (&optional _) parent))
                      ((symbol-function 'noema-pi-router--remembered) (lambda (_) "native-pi"))
                      ((symbol-function '+agent-acp-pi-config) (lambda (_ &optional _command) (list (cons :identifier 'pi))))
                      ((symbol-function '+agent-acp-start) (lambda (&rest args) (setq started args) buffer))
                      ((symbol-function '+agent-acp-subscribe) #'ignore)
                      ((symbol-function '+noema--ensure-server) (lambda (&rest _) (ert-fail "Pi startup must not require a host"))))
              (setq noema-pi-router--emacs-buffer parent)
              (noema-pi-project-ensure root)
              (should (equal (plist-get started :session-id) "native-pi"))
              (should-not (plist-get started :focus))
              (let ((servers (alist-get :mcp-servers (plist-get started :config))))
                (should (equal (mapcar (lambda (server) (alist-get 'name server)) servers) '("noema-api")))
                (should (stringp (alist-get 'command (car servers))))
                (should (equal (alist-get 'env (car servers)) [])))
              (should (eq (buffer-local-value 'noema-pi-parent buffer) parent))
              (setq started nil)
              (should (eq (noema-pi-project-ensure root) buffer))
              (should-not started))
          (kill-buffer buffer)
          (kill-buffer parent))))))

(ert-deftest noema-pi-emacs-and-project-have-distinct-reusable-native-identities ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let ((parent (generate-new-buffer " *pi-parent-test*"))
            (child (generate-new-buffer " *pi-child-test*"))
            (started 0) callbacks)
        (unwind-protect
            (cl-letf (((symbol-function '+agent-acp-pi-config) (lambda (_ &optional _command) (list (cons :identifier 'pi))))
                      ((symbol-function '+agent-acp-start)
                       (lambda (&rest args)
                         (cl-incf started)
                         (if (equal (plist-get args :directory) noema-pi-companion-directory) parent child)))
                      ((symbol-function '+agent-acp-subscribe)
                       (lambda (&rest args) (push (plist-get args :callback) callbacks)))
                      ((symbol-function '+agent-acp-state-value)
                       (lambda (buffer &rest _) (if (eq buffer parent) "native-main" "native-child")))
                      ((symbol-function '+agent-acp-busy-p) (lambda (_) nil)))
              (should (eq (noema-pi-emacs-ensure) parent))
              (should (eq (noema-pi-emacs-ensure) parent))
              (should (eq (noema-pi-project-ensure root) child))
              (should (eq (noema-pi-project-ensure root) child))
              (should (= started 2))
              ;; Initialization persists only true native IDs, with separate keys.
              (noema-pi-router--remember "emacs" parent)
              (noema-pi-router--remember (noema-pi-router--root root) child)
              (should (equal (noema-pi-router--remembered "emacs") "native-main"))
              (should (equal (noema-pi-router--remembered (noema-pi-router--root root)) "native-child"))
              (should (eq (buffer-local-value 'noema-pi-parent child) parent))
              (let ((status (noema-pi-api '((action . "list")))))
                (should (equal (alist-get 'companion status) "native-main"))
                (should (= (length (alist-get 'children status)) 1))))
          (kill-buffer parent)
          (kill-buffer child))))))

(ert-deftest noema-pi-deploy-writes-a-manager-only-agent-directory ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let* ((credentials (expand-file-name "home-pi/auth.json" root))
             (noema-pi-credentials-file credentials)
             (noema-pi-model "anthropic/claude-haiku-4-5")
             (settings-file (expand-file-name "settings.json" noema-pi-agent-directory)))
        (make-directory (file-name-directory credentials) t)
        (write-region "{\"anthropic\": {\"type\": \"api_key\"}}" nil credentials nil 'silent)
        (make-directory noema-pi-agent-directory t)
        (write-region "{\"theme\": \"dark\", \"defaultTools\": [\"bash\"]}" nil settings-file nil 'silent)
        (let ((environment (noema-pi-deploy)))
          (should (equal environment (noema-pi-deploy)))
          (should (string-prefix-p "PI_CODING_AGENT_DIR=" (car environment))))
        (let ((settings (noema-research-parse-json
                         (with-temp-buffer (insert-file-contents settings-file) (buffer-string)))))
          (should (equal (gethash "theme" settings) "dark"))
          (should (equal (gethash "defaultTools" settings) []))
          (should (equal (gethash "defaultThinkingLevel" settings) "off"))
          (should (eq (gethash "enableSkillCommands" settings) :false))
          (should (equal (gethash "defaultProvider" settings) "anthropic"))
          (should (equal (gethash "defaultModel" settings) "claude-haiku-4-5"))
          (should (equal (append (gethash "enabledModels" settings) nil)
                         noema-pi-model-pool)))
        (should (string-match-p "Emacs companion"
                                (with-temp-buffer
                                  (insert-file-contents (expand-file-name "SYSTEM.md" noema-pi-agent-directory))
                                  (buffer-string))))
        (should (string-match-p "mcp__noema-"
                                (with-temp-buffer
                                  (insert-file-contents (expand-file-name "extensions/noema-manager/index.ts"
                                                                          noema-pi-agent-directory))
                                  (buffer-string))))
        (let ((link (expand-file-name "auth.json" noema-pi-agent-directory)))
          (should (file-symlink-p link))
          (should (equal (file-truename link) (file-truename (expand-file-name "auth.json" noema-pi-config-directory)))))))))

(ert-deftest noema-pi-deploy-bridges-codex-login-without-changing-personal-pi-auth ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let* ((noema-pi-credentials-file (expand-file-name "home-pi/auth.json" root))
             (noema-pi-codex-auth-file (expand-file-name "home-codex/auth.json" root))
             (private (expand-file-name "auth.json" noema-pi-agent-directory))
             (original "{\"deepseek\":{\"type\":\"api_key\",\"key\":\"test-key\"}}"))
        (make-directory (file-name-directory noema-pi-credentials-file) t)
        (make-directory (file-name-directory noema-pi-codex-auth-file) t)
        (make-directory noema-pi-agent-directory t)
        (write-region original nil noema-pi-credentials-file nil 'silent)
        (write-region "{\"auth_mode\":\"chatgpt\",\"tokens\":{\"access_token\":\"test-token\"}}"
                      nil noema-pi-codex-auth-file nil 'silent)
        (write-region "{\"providers\":{\"anthropic\":{\"baseUrl\":\"https://example.test\"}},\"other\":true}"
                      nil (expand-file-name "models.json" noema-pi-agent-directory) nil 'silent)
        (noema-pi-deploy)
        (should (file-symlink-p private))
        (should (= (file-modes private) #o600))
        (should (equal (noema-pi-router--credentials) "Codex CLI login"))
        (should (equal (noema-pi-router--read-file noema-pi-credentials-file) original))
        (let* ((auth (noema-research-parse-json (noema-pi-router--read-file private)))
               (models (noema-research-parse-json
                        (noema-pi-router--read-file
                         (expand-file-name "models.json" noema-pi-agent-directory))))
               (codex (gethash "openai-codex" (gethash "providers" models))))
          (should (gethash "deepseek" auth))
          (should-not (gethash "openai-codex" auth))
          (should (eq (gethash "other" models) t))
          (should (equal (gethash "baseUrl" (gethash "anthropic" (gethash "providers" models)))
                         "https://example.test"))
          (should (string-prefix-p "!" (gethash "apiKey" codex)))
          (should-not (string-match-p "test-token" (noema-pi-router--read-file private)))
          (should (equal (gethash "apiKey" codex) (noema-pi-router--codex-token-command))))
        (let ((before (noema-pi-router--read-file private)))
          (noema-pi-deploy)
          (should (equal (noema-pi-router--read-file private) before)))))))

(ert-deftest noema-pi-model-pool-default-and-saved-choice ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let ((settings-file (expand-file-name "settings.json" noema-pi-agent-directory)))
        (let ((settings (noema-pi-router--settings settings-file)))
          (should (equal (gethash "defaultProvider" settings) "openai-codex"))
          (should (equal (gethash "defaultModel" settings) "gpt-5.6-luna")))
        (make-directory noema-pi-agent-directory t)
        (write-region "{\"defaultProvider\":\"deepseek\",\"defaultModel\":\"deepseek-v4-flash\"}"
                      nil settings-file nil 'silent)
        (let ((settings (noema-pi-router--settings settings-file)))
          (should (equal (gethash "defaultProvider" settings) "deepseek"))
          (should (equal (gethash "defaultModel" settings) "deepseek-v4-flash"))
          (should (equal (append (gethash "enabledModels" settings) nil)
                         noema-pi-model-pool)))))))

(ert-deftest noema-pi-model-pool-switches-only-advertised-models ()
  (with-temp-buffer
    (setq-local +agent-acp-session-agent "pi")
    (noema-pi-router-model-mode 1)
    (let ((noema-pi-model-pool '("openai-codex/gpt-5.6-luna"
                                 "deepseek/deepseek-v4-flash"
                                 "openai-codex/gpt-5.6-sol"))
          (current "openai-codex/gpt-5.6-luna")
          selected)
      (cl-letf (((symbol-function '+agent-acp-available-model-ids)
                 (lambda (&optional _) '("openai-codex/gpt-5.6-luna"
                                         "deepseek/deepseek-v4-flash")))
                ((symbol-function '+agent-acp-current-model-id)
                 (lambda (&optional _) current))
                ((symbol-function '+agent-acp-set-model)
                 (lambda (model &optional _) (setq selected model current model))))
        (noema-pi-cycle-model)
        (should (equal selected "deepseek/deepseek-v4-flash"))
        (noema-pi-cycle-model)
        (should (equal selected "openai-codex/gpt-5.6-luna"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal candidates '("openai-codex/gpt-5.6-luna"
                                                 "deepseek/deepseek-v4-flash")))
                     "deepseek/deepseek-v4-flash")))
          (noema-pi-select-model)
          (should (equal selected "deepseek/deepseek-v4-flash")))))))





(ert-deftest noema-agent-worker-sweep-spares-used-visible-personal-and-pi-sessions ()
  "The warm sweep stops only idle, hidden, background worker sessions.
Idle time counts from the buffer's last change, not from its last display."
  (let* ((root "/tmp/noema-sweep-project/")
         (buffers (mapcar (lambda (name) (generate-new-buffer (format " *sweep-%s*" name)))
                          '(stale active manual visible pi)))
         (stale (nth 0 buffers)) (active (nth 1 buffers))
         (manual (nth 2 buffers)) (visible (nth 3 buffers))
         (pi (nth 4 buffers))
         (noema-agent-worker-warm-idle-seconds 60)
         stopped)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                   (lambda (buffer) (and (memq buffer buffers) (buffer-live-p buffer))))
                  ((symbol-function '+agent-acp-resumable-p)
                   (lambda (buffer) (memq buffer buffers)))
                  ((symbol-function '+agent-acp-kill)
                   (lambda (buffer) (push buffer stopped) t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (buffer &optional _all) (and (eq buffer visible) 'window))))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (setq-local agent-shell--state '((:init-finished . t))
                          +agent-acp-session-root root
                          +agent-acp-session-origin
                          (cond ((eq buffer manual) 'foreign)
                                ((eq buffer pi) 'pi)
                                (t 'run))
                          +agent-acp-last-used-at (- (float-time) 3600))
              (+agent-acp--track-activity-h)
              ;; `--track-activity-h' keeps an existing timestamp.
              (should (< +agent-acp-last-used-at (- (float-time) 3000)))))
          ;; Streaming output or typed input counts as use.
          (with-current-buffer active (insert "agent output"))
          (noema-agent-worker-sweep-warm-buffers)
          (should (equal stopped (list stale))))
      (mapc #'kill-buffer buffers))))

(ert-deftest noema-agent-worker-stop-buffer-never-cuts-an-interactive-turn ()
  (let ((buffer (generate-new-buffer " *noema-agent-turn*"))
        shutdown)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                   (lambda (candidate) (eq candidate buffer)))
                  ((symbol-function '+agent-acp-shutdown)
                   (lambda (_) (setq shutdown t))))
          (with-current-buffer buffer (setq-local shell-maker--busy t))
          (should-not (noema-agent-worker-stop-buffer buffer))
          (should-not shutdown)
          (should (buffer-live-p buffer)))
      (kill-buffer buffer))))

(ert-deftest +agent-acp-mark-session-buffer-retires-an-older-holder ()
  (let ((old (generate-new-buffer " *noema-agent-old*"))
        (new (generate-new-buffer " *noema-agent-new*")))
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                   (lambda (buffer) (and (memq buffer (list old new)) (buffer-live-p buffer)))))
          (+agent-acp-mark-session-buffer old "baseline" "codex" "/tmp/noema-project/")
          (+agent-acp-mark-session-buffer new "baseline" "codex" "/tmp/noema-project/")
          (should (eq (+agent-acp-session-buffer "baseline" "/tmp/noema-project/") new))
          (should-not (buffer-local-value '+agent-acp-session-name old))
          (should (string-suffix-p " (retired)" (buffer-name old))))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest noema-agent-worker-carries-out-pi-session-control-requests ()
  (let* ((running (generate-new-buffer " *noema-agent-running*"))
         (idle (generate-new-buffer " *noema-agent-idle*"))
         (worker (noema-agent-worker--create
                  :run-id "run_pi_cancel" :buffer running
                  :routing (noema-research--table
                            "sessionName" (noema-research--table "name" "baseline"))))
         cancelled stopped)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-worker--api)
                   (lambda (_channel _body callback &optional _timeout)
                     (funcall callback
                              (noema-research--table
                               "requests"
                               (vector (noema-research--table
                                        "kind" "session.cancel"
                                        "payload" (noema-research--table "name" "baseline"))
                                       (noema-research--table
                                        "kind" "session.close"
                                        "payload" (noema-research--table "name" "helper"))))
                              nil)))
                  ((symbol-function 'noema-agent-worker--cancel) (lambda (target) (push target cancelled)))
                  ((symbol-function '+agent-acp-session-buffer)
                   (lambda (name _root) (and (equal name "helper") idle)))
                  ((symbol-function 'noema-agent-worker-stop-buffer)
                   (lambda (buffer) (push buffer stopped) t)))
          (puthash "run_pi_cancel" worker noema-agent-worker--runs)
          (noema-agent-worker-claim-coordinator-requests "/tmp/noema-project/")
          (should (equal cancelled (list worker)))
          (should (equal stopped (list idle))))
      (remhash "run_pi_cancel" noema-agent-worker--runs)
      (kill-buffer running)
      (kill-buffer idle))))

(ert-deftest noema-agent-worker-stop-buffer-spares-an-agent-in-use ()
  (let* ((buffer (generate-new-buffer " *noema-agent-in-use*"))
         (worker (noema-agent-worker--create :run-id "run_in_use" :buffer buffer))
         shutdowns)
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-agent-buffer-p) (lambda (target) (eq target buffer)))
                  ((symbol-function '+agent-acp-shutdown) (lambda (target) (push target shutdowns))))
          (puthash "run_in_use" worker noema-agent-worker--runs)
          (should (noema-agent-worker-buffer-busy-p buffer))
          (should-not (noema-agent-worker-stop-buffer buffer))
          (should (buffer-live-p buffer))
          (setf (noema-agent-worker-terminal worker) t)
          (should (noema-agent-worker-stop-buffer buffer))
          (should (equal shutdowns (list buffer)))
          (should-not (buffer-live-p buffer)))
      (remhash "run_in_use" noema-agent-worker--runs)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))



(ert-deftest noema-pi-router-imports-d028-into-independent-sessions-without-research-writes ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let* ((legacy (expand-file-name ".agent/pi.json" root))
             (source "{\"nativeSessionId\": \"native-7\"}"))
        (make-directory (file-name-directory legacy) t)
        (write-region source nil legacy nil 'silent)
        (cl-letf (((symbol-function '+noema-api-call)
                   (lambda (&rest _) (ert-fail "Native identity import must not write Research"))))
          (should (equal (noema-pi-router--remembered (noema-pi-router--root root)) "native-7"))
          (should (= (length (noema-pi-sessions-records)) 1))
          (should (file-exists-p legacy))
          (should (equal (noema-pi-router--read-file legacy) source)))))))

(ert-deftest noema-pi-doctor-checks-the-deployment-deterministically ()
  (noema-research-test--with-directory root
    (noema-pi-test--with-agent-directory root
      (let ((noema-pi-credentials-file (expand-file-name "home-pi/auth.json" root))
            (process-environment
             (seq-remove (lambda (entry)
                           (string-match-p "\\`\\(?:ANTHROPIC\\|OPENAI\\|GEMINI\\|XAI\\|OPENROUTER\\)_API_KEY=" entry))
                         process-environment)))
        (make-directory (file-name-directory noema-pi-credentials-file) t)
        (write-region "{}" nil noema-pi-credentials-file nil 'silent)
        (let ((runtime (expand-file-name "runtime.js" root)))
          (write-region "fixture" nil runtime nil 'silent)
        (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name)))
                  ((symbol-function 'noema-pi-router--runtime-command) (lambda () (list "/bin/node" runtime)))
                  ((symbol-function 'process-lines)
                   (lambda (_program &rest args) (list (if (equal (car args) runtime) "0.8.0" "v26.7.0"))))
                  ((symbol-function '+agent-acp-config-for) (lambda (_) '((:identifier . pi)))))
          (let ((checks (noema-pi-router-checks)))
            (should (seq-every-p #'car (seq-take checks 3)))
            (should (equal (nth 1 (nth 1 checks)) "pi-acp >= 0.8.0"))
            ;; D-035: an empty credential store is not a login.
            (should (equal (nth 1 (nth 3 checks)) "Pi credentials"))
            (should-not (car (nth 3 checks))))
          (write-region "{\"anthropic\": {\"type\": \"oauth\"}}" nil noema-pi-credentials-file nil 'silent)
          (should (car (nth 3 (noema-pi-router-checks))))
          (write-region "{}" nil noema-pi-credentials-file nil 'silent))
          (let ((process-environment (cons "OPENROUTER_API_KEY=test" process-environment)))
            (should (car (nth 3 (noema-pi-router-checks)))))))))
  (should-not (noema-pi-router--version-at-least-p "0.8.0" "0.7.9"))
  (should (noema-pi-router--version-at-least-p "22.19.0" "v26.7.0")))

(ert-deftest +agent-acp-start-never-displays-unless-asked ()
  (let ((buffer (generate-new-buffer " *noema-focus-test*")) calls shown)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args) (push (plist-get args :no-focus) calls) buffer))
                  ((symbol-function '+agent-acp--install-workspace-tabs) #'ignore)
                  ((symbol-function '+agent-acp-hide-client-stderr) #'ignore)
                  ((symbol-function '+agent-acp-subscribe) #'ignore)
                  ((symbol-function '+agent-acp-show-buffer)
                   (lambda (target) (push target shown))))
          (+agent-acp-start :config nil :directory temporary-file-directory)
          (should-not shown)
          (+agent-acp-start :config nil :directory temporary-file-directory :focus t)
          ;; Upstream is always hidden; the adapter explicitly displays once.
          (should (equal calls '(t t)))
          (should (equal shown (list buffer))))
      (kill-buffer buffer))))

(ert-deftest noema-agent-worker-keeps-one-hidden-named-buffer-per-session ()
  (let* ((buffer (generate-new-buffer " *noema-agent-fake*"))
         (worker (noema-agent-worker--create
                  :target "/tmp/noema-project/" :root "/tmp/noema-project/" :agent "codex"
                  :routing (noema-research--table
                            "sessionName" (noema-research--table "name" "baseline"))))
         args)
    (unwind-protect
        (cl-letf (((symbol-function 'noema-agent-worker--config) (lambda (_) nil))
                  ((symbol-function '+agent-acp-start) (lambda (&rest actual) (setq args actual) buffer))
                  ((symbol-function '+agent-acp-subscribe) #'ignore))
          (noema-agent-worker--start-shell worker "native-existing-session")
          (should-not (plist-get args :focus))
          (with-current-buffer buffer
            (should tab-line-exclude)
            (should (equal +agent-acp-session-name "baseline"))
            (should (string-prefix-p "*Agent · baseline · codex @ noema-project*" (buffer-name))))
          (cl-letf (((symbol-function '+agent-acp-agent-buffer-p) (lambda (candidate) (eq candidate buffer))))
            (should (eq (+agent-acp-session-buffer "baseline" "/tmp/noema-project/") buffer))
            (should-not (+agent-acp-session-buffer "baseline" "/tmp/elsewhere/"))))
      (kill-buffer buffer))))

(ert-deftest noema-agent-worker-lighter-counts-runs-and-pending-decisions ()
  (let ((noema-agent-worker--runs (make-hash-table :test #'equal))
        (noema-agent-worker--slot-waiting nil)
        (noema-agent-worker--attention-count 0))
    (should-not (noema-agent-worker--attention-lighter))
    (puthash "run_1" t noema-agent-worker--runs)
    (should (equal (noema-agent-worker--attention-lighter) " Noema[▶1]"))
    (setq noema-agent-worker--attention-count 2)
    (should (equal (noema-agent-worker--attention-lighter) " Noema[▶1 !2]"))
    (setq noema-agent-worker--slot-waiting (list 'waiting))
    (should (equal (noema-agent-worker--attention-lighter) " Noema[▶1 ⋯1 !2]"))))

(ert-deftest noema-agent-worker-notifies-only-when-emacs-is-not-focused ()
  (let (sent (focused nil))
    (let ((noema-agent-worker-notify-function (lambda (title body) (push (list title body) sent))))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) focused)))
        (noema-agent-worker--notify "Noema" "waiting")
        (setq focused t)
        (noema-agent-worker--notify "Noema" "ignored")))
    (should (equal sent '(("Noema" "waiting"))))
    (let ((noema-agent-worker-notify-function nil))
      (should-not (noema-agent-worker--notify "Noema" "off")))))

(ert-deftest noema-agent-worker-waits-for-a-busy-named-session-instead-of-failing ()
  (let* ((worker (noema-agent-worker--create
                  :submission-id "noema-submission:busy" :queue-state 'preparing
                  :target "/tmp/noema-project/" :root "/tmp/noema-project/"))
         (noema-agent-worker--busy-waiting nil)
         finished retried failed)
    (puthash "noema-submission:busy" worker noema-agent-worker--submissions)
    (unwind-protect
        (cl-letf (((symbol-function 'magent-runtime-queue-arbiter-finish) (lambda (&rest _) (setq finished t)))
                  ((symbol-function 'run-at-time) (lambda (&rest _) nil))
                  ((symbol-function 'message) (lambda (&rest args) (setq failed args)))
                  ((symbol-function 'noema-agent-worker--begin-preparation) (lambda (value) (setq retried value))))
          (noema-agent-worker--accept-prepared
           "/tmp/noema-project/" nil
           (noema-research--table "code" "ERR_RESEARCH_SESSION_BUSY"
                                  "message" "Session baseline is busy with another Run")
           worker)
          (should finished)
          (should-not failed)
          (should (eq (noema-agent-worker-queue-state worker) 'queued))
          (should (memq worker noema-agent-worker--busy-waiting))
          (should (gethash "noema-submission:busy" noema-agent-worker--submissions))
          (noema-agent-worker--retry-waiting worker)
          (should (eq retried worker))
          (should-not (memq worker noema-agent-worker--busy-waiting)))
      (remhash "noema-submission:busy" noema-agent-worker--submissions))))

(ert-deftest noema-agent-worker-runs-claimed-coordinator-requests-as-ordinary-runs ()
  (let (claimed runs)
    (cl-letf (((symbol-function 'noema-agent-worker--api)
               (lambda (channel body callback &optional _timeout)
                 (should (equal channel "noema:api:research:coordinator:claim"))
                 (setq claimed body)
                 (funcall callback
                          (noema-research--table
                           "requests"
                           (vector (noema-research--table
                                    "kind" "run.start"
                                    "payload" (noema-research--table "file" "/tmp/p/work.noema"
                                                                     "cellId" "c-w" "sessionName" "baseline"))
                                   (noema-research--table "kind" "unknown")
                                   (noema-research--table "kind" "worknode.state")))
                          nil)))
              ((symbol-function 'noema-agent-worker-run-work-cell)
               (lambda (&rest args) (push (cons default-directory args) runs)))
              ((symbol-function 'noema-agent-worker--ack-coordinator-request)
               (lambda (_root request status &optional reason)
                 (if (equal (noema-research--get request "kind") "run.start")
                     (should (equal status "done"))
                   (should (equal status "failed"))
                   (should (stringp reason))))))
      (noema-agent-worker-claim-coordinator-requests "/tmp/p/")
      (should (equal (alist-get 'cwd claimed) "/tmp/p/"))
      (should (equal runs '(("/tmp/p/" "/tmp/p/work.noema" "c-w" nil nil "baseline")))))))

















;; Adopted from Pisper's session organization: attention is the kernel's
;; projection of the latest Run, and reading clears only `unread'.
(ert-deftest noema-sessions-attention-marks-and-order ()
  (let ((quiet (noema-research--table "name" "quiet" "state" "active" "aliases" []))
        (new (noema-research--table "name" "new" "unread" t "state" "active" "aliases" []))
        (failed (noema-research--table "name" "failed" "failed" t "needsAttention" t
                                       "attentionReason" "failed" "unread" :false
                                       "state" "active" "aliases" []))
        (fresh-failure (noema-research--table "name" "boom" "failed" t "needsAttention" t
                                              "attentionReason" "failed" "unread" t
                                              "state" "active" "aliases" []))
        (approve (noema-research--table "name" "approve" "needsAttention" t
                                        "attentionReason" "permission" "state" "active" "aliases" [])))
    (should (equal (mapcar #'noema-sessions--attention (list quiet new failed fresh-failure approve))
                   '("" "new" "failed" "!failed" "!approve")))
    (should (equal (mapcar (lambda (entry) (noema-sessions--string entry "name"))
                           (seq-sort-by #'noema-sessions--attention-rank #'<
                                        (list quiet new failed approve)))
                   '("failed" "approve" "new" "quiet")))
    (cl-letf (((symbol-function 'noema-sessions--time) (lambda (_) "09-28 10:00")))
      (should (equal (noema-sessions--last-run
                      (noema-research--table "status" "failed" "failureKind" "rate_limit" "retryable" t))
                     "09-28 10:00 failed (rate limit, retry)"))
      (should (equal (noema-sessions--last-run
                      (noema-research--table "status" "failed" "failureKind" "auth"))
                     "09-28 10:00 failed (auth)"))
      (should (equal (noema-sessions--last-run (noema-research--table "status" "completed"))
                     "09-28 10:00 completed")))
    ;; Visiting marks read only when there is unread news.
    (let (calls)
      (cl-letf (((symbol-function 'noema-sessions--api)
                 (lambda (channel body _callback) (push (cons channel body) calls))))
        (noema-sessions--note-read quiet "/tmp/p/")
        (noema-sessions--note-read fresh-failure "/tmp/p/"))
      (should (equal calls '(("noema:api:research:session:name:read"
                              (cwd . "/tmp/p/") (name . "boom"))))))))

(ert-deftest noema-sessions-running-status-shows-a-quiet-agent ()
  (with-temp-buffer
    (let ((noema-sessions-idle-threshold 30))
      (setq-local +agent-acp-last-used-at (- (float-time) 5))
      (should (equal (noema-sessions--running-status (current-buffer)) "running"))
      (setq-local +agent-acp-last-used-at (- (float-time) 45))
      (should (equal (noema-sessions--running-status (current-buffer)) "running, idle 45s"))
      (setq-local +agent-acp-last-used-at (- (float-time) 600))
      (should (equal (noema-sessions--running-status (current-buffer)) "running, idle 10m"))
      (should (equal (noema-sessions--running-status nil) "running")))))



(ert-deftest noema-sessions-side-chat-is-one-per-session-and-starts-empty ()
  (let (started shown)
    (cl-letf (((symbol-function '+agent-acp-config-for) (lambda (agent) (list :agent agent)))
              ((symbol-function '+agent-acp-start)
               (cl-function
                (lambda (&key config directory origin &allow-other-keys)
                  (let ((buffer (generate-new-buffer " *side*")))
                    (with-current-buffer buffer (setq-local major-mode 'agent-shell-mode))
                    (push (list config directory origin) started)
                    buffer))))
              ((symbol-function '+agent-acp-adopt)
               (cl-function
                (lambda (buffer &key root &allow-other-keys)
                  (with-current-buffer buffer
                    (setq-local +agent-acp-session-root root))
                  buffer)))
              ((symbol-function '+agent-acp-show-buffer) (lambda (buffer) (push buffer shown))))
      (let ((first (noema-sessions--open-side-chat "/tmp/p/" "baseline" "codex" "/tmp/work/"))
            (again (noema-sessions--open-side-chat "/tmp/p/" "baseline" "codex" "/tmp/work/")))
        (unwind-protect
            (progn
              (should (eq first again))
              ;; No session id, no fork: the parent's history is not copied.
              (should (equal started '(((:agent "codex") "/tmp/work/" side))))
              (should (equal (buffer-local-value '+agent-acp-side-parent first) "baseline"))
              (should (equal shown (list first first))))
          (kill-buffer first))))))

(ert-deftest noema-sessions-switch-offers-names-and-unnamed-agent-buffers ()
  (let ((named (generate-new-buffer " *noema-named*"))
        (orphan (generate-new-buffer " *noema-orphan*")))
    (unwind-protect
        (cl-letf (((symbol-function '+agent-acp-agent-buffer-p)
                   (lambda (buffer) (memq buffer (list named orphan))))
                  ((symbol-function 'noema-sessions--execution-live-p)
                   (lambda (buffer) (eq buffer named))))
          (with-current-buffer named
            (setq-local +agent-acp-session-name "baseline"
                        +agent-acp-session-root "/tmp/p/"))
          (let ((choices (noema-sessions--switch-candidates
                          (list (noema-research--table "name" "baseline" "agent" "codex"
                                                       "state" "active" "aliases" []))
                          "/tmp/p/")))
            (should (= (length choices) 2))
            (should (eq (noema-sessions--live-buffer (cadr (car choices)) "/tmp/p/") named))
            (should (eq (cddr (cadr choices)) orphan))))
      (kill-buffer named)
      (kill-buffer orphan))))



(ert-deftest noema-agent-worker-reports-are-fifo-even-if-callback-throws ()
  (let ((worker (noema-agent-worker--create :run-id "r" :session-id "s" :epoch 1 :started t :root "/tmp/"))
        callbacks batches)
    (cl-letf (((symbol-function 'noema-agent-worker--api)
               (lambda (_channel body callback &optional _timeout)
                 (push (alist-get 'events body) batches) (push callback callbacks))))
      (noema-agent-worker--report worker '(content) (lambda (&rest _) (error "UI failed")))
      (noema-agent-worker--report worker '(terminal))
      (should (= (length batches) 1))
      (funcall (pop callbacks) nil nil)
      (should (equal (reverse batches) '([content] [terminal])))
      (funcall (pop callbacks) nil nil)
      (should-not (noema-agent-worker-report-busy worker)))))

(ert-deftest noema-agent-worker-sync-report-error-does-not-strand-queue ()
  (let ((worker (noema-agent-worker--create :run-id "r" :session-id "s" :epoch 1 :started t :root "/tmp/")) failure)
    (cl-letf (((symbol-function 'noema-agent-worker--api) (lambda (&rest _) (error "offline"))))
      (noema-agent-worker--report worker '(terminal) (lambda (_result error) (setq failure error)))
      (should failure)
      (should-not (noema-agent-worker-report-busy worker)))))

(ert-deftest noema-agent-worker-lost-terminal-ack-queries-without-replaying ()
  (let ((worker (noema-agent-worker--create :run-id "r" :session-id "s" :epoch 1 :started t :root "/tmp/"))
        channels)
    (cl-letf (((symbol-function 'noema-agent-worker--completion-retry) #'ignore)
              ((symbol-function 'noema-agent-worker--api)
               (lambda (channel _body callback &optional _timeout)
                 (push channel channels)
                 (if (string-suffix-p "run:get" channel)
                     (funcall callback '((run . ((status . "completed")))) nil)
                   (funcall callback nil "lost response")))))
      (noema-agent-worker--terminal worker "completed")
      (should-not (noema-agent-worker-terminal-acked worker))
      (should (noema-agent-worker-terminal-events worker))
      (noema-agent-worker-check-completion worker)
      (should (noema-agent-worker-terminal-acked worker))
      (should (equal (reverse channels) '("noema:api:research:worker:events" "noema:api:research:run:get"))))))

(ert-deftest noema-agent-worker-done-prose-is-not-a-completion-receipt ()
  (let ((worker (noema-agent-worker--create :run-id "r" :result-parts '("All done."))))
    (should (eq (noema-agent-worker-check-completion worker) 'pending))
    (should-not (noema-agent-worker-terminal worker))))

(ert-deftest noema-agent-worker-cancel-preserves-already-completed-receipt ()
  (with-temp-buffer
    (setq-local +agent-acp--prompt-receipt '(:run-id "r" :status completed :response ((stopReason . "end_turn"))))
    (let ((worker (noema-agent-worker--create :run-id "r" :session-id "s" :epoch 1 :started t :root "/tmp/" :buffer (current-buffer))))
      (cl-letf (((symbol-function '+agent-acp-restore-prompt) #'ignore)
                ((symbol-function '+agent-acp-interrupt) (lambda (&rest _) (ert-fail "Interrupted completed turn")))
                ((symbol-function 'noema-agent-worker--api) (lambda (_channel _body callback &optional _) (funcall callback nil nil))))
        (noema-agent-worker--cancel worker)
        (should (equal (noema-agent-worker-terminal-status worker) "completed"))
        (should (noema-agent-worker-terminal-acked worker))))))

(ert-deftest noema-agent-worker-waits-for-model-and-mode-initialization ()
  (with-temp-buffer
    (let ((worker (noema-agent-worker--create :run-id "r" :agent "opencode" :root "/tmp/" :target "/tmp/"))
          subscribed callback ready)
      (cl-letf (((symbol-function 'noema-agent-worker--config) #'ignore)
                ((symbol-function '+agent-acp-start) (lambda (&rest _) (current-buffer)))
                ((symbol-function '+agent-acp-mark-session-buffer) #'ignore)
                ((symbol-function '+agent-acp-unsubscribe) #'ignore)
                ((symbol-function '+agent-acp-subscribe)
                 (lambda (&rest args) (setq subscribed (plist-get args :event) callback (plist-get args :callback)) 'token))
                ((symbol-function 'noema-agent-worker--on-session-ready) (lambda (&rest _) (setq ready t))))
        (noema-agent-worker--start-shell worker nil)
        (should (eq subscribed 'init-finished))
        (should-not ready)
        (funcall callback nil)
        (should ready)))))




;;;; Regression

(defun noema-research-test--regression-document ()
  "Return a document whose work fans out over both relation types."
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (lineage-child (noema-research-cell-work-node-id
                         (noema-research-test--append-work-cell
                          document "c-l" "work" "Derived bound" "Follows from it."
                          (list work))))
         (depends-child (noema-research-cell-work-node-id
                         (noema-research-test--append-work-cell
                          document "c-d" "work" "Benchmark" "Needs the bound."
                          nil (list work))))
         (grandchild (noema-research-cell-work-node-id
                      (noema-research-test--append-work-cell
                       document "c-g" "work" "Write-up" "Rests on the benchmark."
                       (list depends-child))))
         (running (noema-research-cell-work-node-id
                   (noema-research-test--append-work-cell
                    document "c-a" "work" "Still going" "In flight."
                    (list work))))
         (abandoned (noema-research-cell-work-node-id
                     (noema-research-test--append-work-cell
                      document "c-z" "work" "Abandoned" "Gave up."
                      (list work)))))
    (dolist (id (list work lineage-child depends-child grandchild))
      (noema-research-set-state document id "done"))
    (noema-research-set-state document running "active")
    (noema-research-set-state document abandoned "dropped" "superseded")
    (list document work lineage-child depends-child grandchild running abandoned)))

(ert-deftest noema-research-regression-reaches-finished-work-over-both-edge-types ()
  "A regression invalidates finished work below it, lineage and depends alike."
  (pcase-let ((`(,document ,work ,lineage-child ,depends-child ,grandchild
                 ,running ,abandoned)
               (noema-research-test--regression-document)))
    (let ((targets (noema-research-regression-targets document work)))
      (should (equal (car targets) work))
      (dolist (id (list lineage-child depends-child grandchild))
        (should (member id targets)))
      ;; Work in flight and work already abandoned are left alone: a
      ;; regression reopens finished claims, it does not restart everything.
      (should-not (member running targets))
      (should-not (member abandoned targets)))))

(ert-deftest noema-research-regression-targets-skip-questions-and-checkpoints ()
  "Only work carries state, so only work is regressed."
  (pcase-let ((`(,document ,work . ,_) (noema-research-test--regression-document)))
    (let ((question (noema-research-test--work-id document "c-q")))
      (should-not (member question (noema-research-regression-targets document work)))
      ;; The checkpoint hangs off the work node but never carries state.
      (should-not (member (noema-research-test--work-id document "c-k")
                          (noema-research-regression-targets document work))))))



(ert-deftest noema-research-regressed-is-a-supported-state-and-validates ()
  "`regressed' round-trips through validation like any other work state."
  (should (member "regressed" noema-research-work-states))
  (pcase-let ((`(,document ,work . ,_) (noema-research-test--regression-document)))
    (noema-research-set-state document work "regressed" "broke")
    (should-not (plist-get (noema-research-validate document) :errors))
    (should (equal (noema-research-work-node-field
                    (noema-research-find-work-node document work) "state")
                   "regressed"))))






;;;; Coordinator-claimed work state






;;;; Orchestration board

(defun noema-research-test--orchestration-snapshot ()
  "Return a snapshot shaped like `orchestration:snapshot' returns one."
  (noema-research--table
   "root" "/tmp/project"
   "tasks" (vector (noema-research--table
                    "id" "task_1" "title" "Parse the fence grammar" "state" "open"
                    "priority" 2 "dependsOn" (vector "task_0") "createdBy" "human"
                    "updatedAt" "2026-09-20T10:00:00Z")
                   (noema-research--table
                    "id" "task_2" "title" "" "state" "blocked" "parentTaskId" "task_1"
                    "dependsOn" (vector) "createdBy" "agent" "updatedAt" ""))
   "jobs" (vector (noema-research--table
                   "id" "job_1" "kind" "acp" "state" "queued" "taskId" "task_1"
                   "dependsOn" (vector "job_0") "attemptsStarted" 1
                   "effects" (noema-research--table "class" "workspace-mutating")
                   "retry" (noema-research--table "policy" "safe_only")))
   "workers" (vector (noema-research--table
                      "id" "worker_1" "kind" "acp" "state" "available"
                      "transport" "stdio" "inferenceCapable" t
                      "capabilities" (vector "code" "shell") "lastSeenAt" ""))
   "delegations" (vector (noema-research--table
                          "id" "deleg_1" "parentTaskId" "task_1" "childTaskId" "task_2"
                          "childJobIds" (vector "job_1" "job_2")
                          "requestedBy" (noema-research--table "type" "agent" "id" "run_9")
                          "createdAt" ""))
   "events" (vector (noema-research--table "seq" 1 "type" "task.created" "ts" "")
                    (noema-research--table "seq" 2 "type" "job.queued" "ts" ""
                                           "runId" "run_9"))))

(defmacro noema-research-test--with-orchestration (&rest body)
  "Run BODY in an orchestration board holding a fixture snapshot."
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer " *noema-orchestration-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (noema-orchestration-mode)
           (setq noema-orchestration--root "/tmp/project/"
                 noema-orchestration--snapshot (noema-research-test--orchestration-snapshot))
           ,@body)
       (kill-buffer buffer))))

(ert-deftest noema-orchestration-renders-every-view-from-one-snapshot ()
  "Switching views reads the cached snapshot; it never refetches."
  (require 'noema-orchestration)
  (noema-research-test--with-orchestration
    (cl-letf (((symbol-function 'noema-orchestration-refresh)
               (lambda () (ert-fail "switching views must not refetch"))))
      (dolist (view noema-orchestration-views)
        (noema-orchestration-set-view view)
        (should (equal noema-orchestration--view view))
        (should (equal mode-name (format "Noema-Orchestration[%s]" view)))
        ;; Every fixture view has rows, and every row matches its columns.
        (should tabulated-list-entries)
        (dolist (entry tabulated-list-entries)
          (should (= (length (cadr entry)) (length tabulated-list-format))))))))

(ert-deftest noema-orchestration-task-rows-carry-state-and-dependency-counts ()
  "The task view answers what is blocked behind what."
  (require 'noema-orchestration)
  (noema-research-test--with-orchestration
    (noema-orchestration-set-view "tasks")
    (let ((first (cadr (nth 0 tabulated-list-entries)))
          (second (cadr (nth 1 tabulated-list-entries))))
      (should (equal (aref first 0) "Parse the fence grammar"))
      (should (equal (aref first 1) "open"))
      (should (equal (aref first 3) "1"))
      (should (equal (aref first 4) ""))
      ;; A task with no title falls back to its id rather than rendering blank,
      ;; and a subtask is marked as one.
      (should (equal (aref second 0) "task_2"))
      (should (equal (aref second 4) "sub")))))

(ert-deftest noema-orchestration-delegation-rows-read-parent-to-child ()
  "Delegation is the one relation the Graph Board cannot show."
  (require 'noema-orchestration)
  (noema-research-test--with-orchestration
    (noema-orchestration-set-view "delegations")
    (let ((row (cadr (car tabulated-list-entries))))
      (should (equal (aref row 0) "task_1"))
      (should (equal (aref row 2) "task_2"))
      (should (equal (aref row 3) "2"))
      (should (equal (aref row 4) "agent:run_9")))))

(ert-deftest noema-orchestration-cycles-views-in-both-directions ()
  "TAB and S-TAB walk the same ring."
  (require 'noema-orchestration)
  (noema-research-test--with-orchestration
    (noema-orchestration-set-view "tasks")
    (noema-orchestration-cycle-view)
    (should (equal noema-orchestration--view "jobs"))
    (noema-orchestration-cycle-view t)
    (should (equal noema-orchestration--view "tasks"))
    (noema-orchestration-cycle-view t)
    (should (equal noema-orchestration--view (car (last noema-orchestration-views))))))

(ert-deftest noema-orchestration-events-view-shows-the-tail ()
  "Events are append-only; the board bounds what it draws."
  (require 'noema-orchestration)
  (noema-research-test--with-orchestration
    (let ((noema-orchestration-event-limit 1))
      (noema-orchestration-set-view "events")
      (should (= (length tabulated-list-entries) 1))
      (should (equal (aref (cadr (car tabulated-list-entries)) 1) "job.queued")))))


;;;; Claim warnings

(defun noema-research-test--warning-codes (document id)
  "Return the warning code prefixes DOCUMENT reports for WorkNode ID."
  (delq nil
        (mapcar (lambda (entry)
                  (when (equal (car entry) id)
                    (car (split-string (cdr entry) ":"))))
                (plist-get (noema-research-validate document) :warnings))))

(ert-deftest noema-research-warns-when-a-done-claim-has-no-basis ()
  "A node marked done with no Run and no outcome is noticed, not refused."
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w")))
    (noema-research-set-state document work "done")
    (should (member "done-without-basis" (noema-research-test--warning-codes document work)))
    ;; An outcome is a basis the document itself records.
    (noema-research-set-outcome document work "supported")
    (should-not (member "done-without-basis" (noema-research-test--warning-codes document work)))))

(ert-deftest noema-research-warns-when-the-run-disagrees-with-the-claim ()
  "Marked done while its last Run failed is the mismatch nothing used to compare."
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (cell (noema-research-primary-cell document work)))
    (puthash "outputs"
             (vector (noema-research--table
                      "data" (noema-research--table
                              "application/vnd.noema.run+json"
                              (noema-research--table "run_id" "run_1" "status" "failed"))))
             cell)
    (noema-research-set-state document work "done")
    (should (member "done-run-mismatch" (noema-research-test--warning-codes document work)))))

(ert-deftest noema-research-warns-when-finished-work-rests-on-a-regression ()
  "A done node above a regressed one is the state the cascade exists to prevent."
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w"))
         (child (noema-research-cell-work-node-id
                 (noema-research-test--append-work-cell
                  document "c-child" "work" "Rests on it" "Body." (list work)))))
    (noema-research-set-state document child "done")
    (noema-research-set-outcome document child "supported")
    (noema-research-set-state document work "regressed" "broke")
    ;; Re-greening the child by hand is exactly the case worth noticing.
    (noema-research-set-state document child "done")
    (should (member "done-over-regressed" (noema-research-test--warning-codes document child)))))

(ert-deftest noema-research-warns-when-active-work-has-no-run ()
  (let* ((document (noema-research-test--document))
         (work (noema-research-test--work-id document "c-w")))
    (noema-research-set-state document work "active")
    (should (member "active-without-run" (noema-research-test--warning-codes document work)))))



(ert-deftest noema-research-questions-and-checkpoints-are-not-claim-checked ()
  "Only work carries state, so only work is judged on its basis."
  (let* ((document (noema-research-test--document))
         (question (noema-research-test--work-id document "c-q")))
    (should-not (noema-research-test--warning-codes document question))))


;;;; Claim warnings in JuText









;;; noema-research-tests.el ends here

;;; tools/agent/topology.el --- Who collaborates on what: Pi, projects, files, agents -*- lexical-binding: t; -*-

;;; Commentary:
;; The model of the pool (C69): Pi above the projects it manages, each
;; project's documents, the collaborators bound to them and the files they
;; touched besides, with a Graphviz layout (`dot -Tjson').  It is a projection
;; of the session snapshots and the collaboration records and never asks an
;; agent anything.  The interactive graph that draws it is `tools/agent/board'
;; (C78), shown in the collaboration panel; Treemacs marks use the same model.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'tools/agent/acp)
(require 'tools/agent/binding)

(declare-function +agent-collab-suggestions "tools/agent/collab" (file &optional all))
(declare-function +agent-collab-threads "tools/agent/collab" (file))
(declare-function +agent-collab-task-p "tools/agent/collab" (thread))
(declare-function +agent-collab--client-file "tools/agent/collab" (file))
(defvar +agent-collab-changed-functions)

(defcustom +agent-topology-dot-program "dot"
  "Graphviz program that lays out the topology."
  :type 'string :group '+agent-session)

(defconst +agent-topology--margin 16 "Blank border around the drawing, in pixels.")

;;;; Model

(defun +agent-topology--state (session)
  "Return a short word for SESSION's state."
  (cond ((eq (plist-get session :process-state) 'disconnected) "disconnected")
        ((plist-get session :attention-kind) "needs you")
        ((memq (plist-get session :turn-state) '(running stopping)) "working")
        (t "idle")))

(defun +agent-topology-file-counts (file)
  "Return (:pending N :threads M) for FILE.
Pending counts review cards that are not rejected and suggestions; threads
counts those not resolved."
  (require 'tools/agent/collab)
  (let* ((client (+agent-collab--client-file file))
         (cards (if (and (string-match-p "\\.\\(?:md\\|markdown\\)\\'" client) (file-readable-p client))
                    (with-temp-buffer
                      (insert-file-contents client)
                      (let ((count 0))
                        (goto-char (point-min))
                        (while (re-search-forward "^#\\+begin ediff\\b.*\n" nil t)
                          (unless (looking-at-p "\\(?:@@ediff-\\(?:author\\|comment\\) .*\n\\)*@@ediff-status rejected")
                            (setq count (1+ count))))
                        count))
                  0)))
    (list :pending (+ cards (length (ignore-errors (+agent-collab-suggestions client))))
          :threads (seq-count (lambda (thread) (and (+agent-collab-task-p thread)
                                                    (not (equal (alist-get 'status thread) "resolved"))))
                              (ignore-errors (+agent-collab-threads client))))))

(defun +agent-topology--file-label (file counts)
  "Return the label of FILE with COUNTS."
  (let ((pending (or (plist-get counts :pending) 0))
        (threads (or (plist-get counts :threads) 0)))
    (concat (file-name-nondirectory file)
            (if (or (> pending 0) (> threads 0))
                (concat "\n" (string-join
                              (delq nil (list (and (> pending 0) (format "✎%d" pending))
                                              (and (> threads 0) (format "%d thread%s" threads
                                                                         (if (= threads 1) "" "s")))))
                              "  "))
              ""))))

(defun +agent-topology-model (sessions counts)
  "Return (:nodes NODES :edges EDGES) for SESSIONS.
A session plist carries the snapshot keys plus :kind and :touched.  COUNTS is
called with a file and returns its (:pending :threads).  A node is
(:id :kind :label :target); an edge is (FROM TO STYLE)."
  (let ((nodes (make-hash-table :test #'equal)) order edges)
    (cl-flet ((node (id kind label target)
                (unless (gethash id nodes)
                  (push id order)
                  (puthash id (list :id id :kind kind :label label :target target) nodes)))
              (edge (from to style)
                (unless (member (list from to style) edges)
                  (push (list from to style) edges))))
      (dolist (session sessions)
        (let* ((root (plist-get session :root))
               (project (format "project:%s" root))
               (id (format "session:%s" (plist-get session :buffer)))
               (label (format "%s\n%s" (or (plist-get session :name) (plist-get session :agent))
                              (+agent-topology--state session))))
          (when root
            (node project 'project
                  (file-name-nondirectory (directory-file-name root)) (list :root root)))
          (if (eq (plist-get session :kind) 'pi)
              (progn (node id 'pi label (list :buffer (plist-get session :buffer)))
                     (when root (edge id project 'manages)))
            (node id 'agent label (list :buffer (plist-get session :buffer)))
            (if-let* ((file (plist-get session :file)))
                (let ((file-id (format "file:%s" file)))
                  (node file-id 'file (+agent-topology--file-label file (funcall counts file))
                        (list :file file))
                  (when root (edge project file-id 'binding))
                  (edge file-id id 'binding))
              (when root (edge project id 'binding)))
            (dolist (file (plist-get session :touched))
              (let ((file-id (format "file:%s" file)))
                (node file-id 'file (+agent-topology--file-label file (funcall counts file))
                      (list :file file))
                (edge id file-id 'touched)))))))
    (list :nodes (mapcar (lambda (id) (gethash id nodes)) (nreverse order))
          :edges (nreverse edges))))

(defun +agent-topology--sessions ()
  "Return every pool session with :kind and :touched."
  (mapcar (lambda (session)
            (let ((buffer (plist-get session :buffer)))
              (append (list :kind (+agent-session-kind buffer)
                            :touched (+agent-binding-touched buffer))
                      session)))
          (+agent-pool-sessions nil nil 'all)))

;;;; Layout (Graphviz) and drawing — the research Graph Board's engine

(defun +agent-topology--dot-quote (text)
  "Return TEXT as a quoted Graphviz string."
  (let ((escaped (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" text)))
    (setq escaped (replace-regexp-in-string "\r" "" escaped)
          escaped (replace-regexp-in-string "\n" "\\n" escaped nil t))
    (concat "\"" escaped "\"")))

(defun +agent-topology--dot-source (model)
  "Return (DOT . NAMES) for MODEL, NAMES mapping node ids to DOT names."
  (let ((names (make-hash-table :test #'equal)) (index 0))
    (with-temp-buffer
      (insert "digraph agents {\n  rankdir=TB;\n  nodesep=0.35;\n  ranksep=0.55;\n"
              "  node [fontname=\"Helvetica Bold\", fontsize=11, margin=\"0.14,0.10\"];\n")
      (dolist (node (plist-get model :nodes))
        (let ((name (format "n%d" index)))
          (puthash (plist-get node :id) name names)
          (setq index (1+ index))
          (insert (format "  %s [shape=%s, label=%s];\n" name
                          (pcase (plist-get node :kind)
                            ('pi "ellipse") ('project "box") ('file "note") (_ "box"))
                          (+agent-topology--dot-quote (plist-get node :label))))))
      (dolist (edge (plist-get model :edges))
        (insert (format "  %s -> %s%s;\n" (gethash (nth 0 edge) names) (gethash (nth 1 edge) names)
                        (pcase (nth 2 edge)
                          ('touched " [style=dashed, constraint=false]")
                          ('manages " [style=dotted]")
                          (_ "")))))
      (insert "}\n")
      (cons (buffer-string) names))))

(defun +agent-topology--run-dot (source)
  "Return Graphviz JSON for DOT SOURCE, or nil without Graphviz.
Graphviz is a client-side tool, run on this machine whatever buffer is current."
  (let ((default-directory temporary-file-directory))
    (when-let* ((program (executable-find +agent-topology-dot-program)))
      (with-temp-buffer
        (insert source)
        (when (zerop (call-process-region (point-min) (point-max) program t t nil "-Tjson"))
          (json-parse-string (buffer-string) :object-type 'alist :array-type 'array
                             :null-object nil :false-object nil))))))

(defun +agent-topology--points (points height)
  "Convert Graphviz POINTS to top-left coordinates for a drawing of HEIGHT."
  (mapcar (lambda (point) (cons (float (aref point 0)) (- height (aref point 1))))
          (append points nil)))

(defun +agent-topology--layout (model)
  "Return MODEL's layout (:width :height :nodes :edges), or nil without Graphviz."
  (pcase-let* ((`(,source . ,names) (+agent-topology--dot-source model))
               (json (+agent-topology--run-dot source)))
    (when json
      (let* ((bb (mapcar #'string-to-number (split-string (or (alist-get 'bb json) "0,0,0,0") ",")))
             (height (float (nth 3 bb)))
             (ids (make-hash-table :test #'equal))
             nodes edges)
        (maphash (lambda (id name) (puthash name id ids)) names)
        (seq-doseq (object (or (alist-get 'objects json) []))
          (when-let* ((id (gethash (alist-get 'name object) ids))
                      (pos (alist-get 'pos object)))
            (let ((xy (mapcar #'string-to-number (split-string pos ","))))
              (push (list :id id :x (float (nth 0 xy)) :y (- height (nth 1 xy))
                          :width (* 72 (string-to-number (format "%s" (alist-get 'width object))))
                          :height (* 72 (string-to-number (format "%s" (alist-get 'height object)))))
                    nodes))))
        (seq-doseq (edge (or (alist-get 'edges json) []))
          (let (points arrow)
            (seq-doseq (op (or (alist-get '_draw_ edge) []))
              (when (member (alist-get 'op op) '("b" "B"))
                (setq points (+agent-topology--points (alist-get 'points op) height))))
            (seq-doseq (op (or (alist-get '_hdraw_ edge) []))
              (when (member (alist-get 'op op) '("P" "p"))
                (setq arrow (+agent-topology--points (alist-get 'points op) height))))
            (push (list :points points :arrow arrow :style (alist-get 'style edge)) edges)))
        (list :width (float (nth 2 bb)) :height height
              :nodes (nreverse nodes) :edges (nreverse edges))))))

(defun +agent-topology--path (points margin)
  "Return an SVG cubic path through POINTS offset by MARGIN."
  (let ((shifted (mapcar (lambda (point)
                           (format "%.1f %.1f" (+ margin (car point)) (+ margin (cdr point))))
                         points)))
    (concat "M " (car shifted) " C " (string-join (cdr shifted) " "))))

;;;; View

;;;###autoload
(defun +agent/topology ()
  "Show who collaborates on what: the collaboration panel's graph (C78)."
  (interactive)
  (require 'tools/agent/panel)
  (+agent-panel--show-tab 'graph))

(declare-function +agent-panel--show-tab "tools/agent/panel" (tab))

(defalias 'agent-topology #'+agent/topology
  "Show the collaborator graph of the agent pool.")

(provide 'tools/agent/topology)
;;; topology.el ends here

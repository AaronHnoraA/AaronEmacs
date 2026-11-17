;;; tools/agent/topology.el --- Who collaborates on what: Pi, projects, files, agents -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only picture of the pool (C69): Pi above the projects it manages,
;; each project's documents, and the collaborators bound to them, with the
;; files they touched besides.  It is a projection of the session snapshots and
;; the collaboration records; it never asks an agent anything and has no
;; polling timer: state events redraw it, coalesced.
;;
;; The drawing engine is the retired research Graph Board's: Graphviz lays the
;; graph out (`dot -Tjson') and the scene is drawn here as SVG.  Below the
;; picture, the same nodes are a list the keyboard moves through.

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
          :threads (seq-count (lambda (thread) (not (equal (alist-get 'status thread) "resolved")))
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

(defun +agent-topology--color (face attribute fallback)
  "Return FACE's ATTRIBUTE colour, or FALLBACK."
  (let ((value (face-attribute face attribute nil t)))
    (if (and (stringp value) (not (string-prefix-p "unspecified" value))) value fallback)))

(defun +agent-topology--palette ()
  "Return the drawing colours, following the current theme."
  (list :text (+agent-topology--color 'default :foreground "#333333")
        :edge (+agent-topology--color 'shadow :foreground "#888888")
        :background (+agent-topology--color 'default :background "#ffffff")
        :selected (+agent-topology--color 'cursor :background "#2a7ae2")
        :fills `((pi . ,(+agent-topology--color 'font-lock-keyword-face :foreground "#7a4ea8"))
                 (project . ,(+agent-topology--color 'font-lock-type-face :foreground "#2a7a6a"))
                 (file . ,(+agent-topology--color 'font-lock-string-face :foreground "#8a6a2a"))
                 (agent . ,(+agent-topology--color 'font-lock-function-name-face :foreground "#2a5aa8")))))

(defun +agent-topology--svg (model layout selected)
  "Draw MODEL with LAYOUT, outlining the node SELECTED, and return the SVG."
  (let* ((margin +agent-topology--margin)
         (palette (+agent-topology--palette))
         (svg (svg-create (ceiling (+ (* 2 margin) (plist-get layout :width)))
                          (ceiling (+ (* 2 margin) (plist-get layout :height)))))
         (by-id (make-hash-table :test #'equal)))
    (dolist (node (plist-get model :nodes)) (puthash (plist-get node :id) node by-id))
    (dolist (edge (plist-get layout :edges))
      (when-let* ((points (plist-get edge :points)))
        (dom-append-child
         svg (dom-node 'path `((d . ,(+agent-topology--path points margin)) (fill . "none")
                               (stroke . ,(plist-get palette :edge)) (stroke-width . "1.2")
                               ,@(pcase (plist-get edge :style)
                                   ("dashed" '((stroke-dasharray . "5 4")))
                                   ("dotted" '((stroke-dasharray . "2 4"))))))))
      (when-let* ((arrow (plist-get edge :arrow)))
        (svg-polygon svg (mapcar (lambda (point) (cons (+ margin (car point)) (+ margin (cdr point)))) arrow)
                     :fill (plist-get palette :edge) :stroke (plist-get palette :edge))))
    (dolist (box (plist-get layout :nodes))
      (let* ((node (gethash (plist-get box :id) by-id))
             (width (plist-get box :width)) (height (plist-get box :height))
             (x0 (+ margin (- (plist-get box :x) (/ width 2.0))))
             (y0 (+ margin (- (plist-get box :y) (/ height 2.0))))
             (color (alist-get (plist-get node :kind) (plist-get palette :fills)))
             (stroke (if (equal (plist-get box :id) selected) (plist-get palette :selected) color))
             (lines (split-string (plist-get node :label) "\n"))
             (text-y (+ y0 (/ height 2.0) 4 (- (/ (* (1- (length lines)) 13) 2.0)))))
        (if (eq (plist-get node :kind) 'pi)
            (svg-ellipse svg (+ x0 (/ width 2.0)) (+ y0 (/ height 2.0)) (/ width 2.0) (/ height 2.0)
                         :fill (plist-get palette :background) :stroke stroke
                         :stroke-width (if (equal (plist-get box :id) selected) 2.5 1.2))
          (svg-rectangle svg x0 y0 width height :rx 6 :ry 6
                         :fill (plist-get palette :background) :stroke stroke
                         :stroke-width (if (equal (plist-get box :id) selected) 2.5 1.2)))
        (cl-loop for line in lines for index from 0
                 do (svg-text svg line :x (+ x0 (/ width 2.0)) :y (+ text-y (* index 13))
                              :text-anchor "middle" :font-family "Helvetica"
                              :font-size (if (zerop index) 11 9.5)
                              :font-weight (if (zerop index) "600" "400")
                              :fill (if (zerop index) color (plist-get palette :text))))))
    svg))

;;;; View

(defvar-local +agent-topology--model nil "The model this view shows.")
(defvar-local +agent-topology--layout nil "Its layout, or nil without Graphviz.")
(defvar-local +agent-topology--selected nil "Id of the selected node.")
(defvar-local +agent-topology--timer nil "Pending coalesced redraw.")

(defun +agent-topology--render ()
  "Draw the current view from its model."
  (let ((inhibit-read-only t) (selected +agent-topology--selected))
    (erase-buffer)
    (if (not +agent-topology--layout)
        (insert (propertize "Graphviz (dot) is not available; the nodes are listed below.\n\n"
                            'face 'shadow))
      (insert-image (svg-image (+agent-topology--svg +agent-topology--model +agent-topology--layout selected)
                               :ascent 'center))
      (insert "\n\n"))
    (dolist (node (plist-get +agent-topology--model :nodes))
      (let ((start (point)))
        (insert (format "%-8s %s\n" (symbol-name (plist-get node :kind))
                        (replace-regexp-in-string "\n" "  " (plist-get node :label))))
        (put-text-property start (point) '+agent-topology-node (plist-get node :id))
        (when (equal (plist-get node :id) selected)
          (add-face-text-property start (1- (point)) 'highlight))))
    (insert (propertize "\nn/p select · RET open · S-RET chat · g refresh · q quit\n" 'face 'shadow))
    (if-let* ((at (text-property-any (point-min) (point-max) '+agent-topology-node selected)))
        (goto-char at)
      (goto-char (point-min)))))

(defun +agent-topology-refresh ()
  "Rebuild the model and layout, then redraw."
  (interactive)
  (setq +agent-topology--model (+agent-topology-model (+agent-topology--sessions)
                                                      #'+agent-topology-file-counts)
        +agent-topology--layout (+agent-topology--layout +agent-topology--model))
  (unless (seq-find (lambda (node) (equal (plist-get node :id) +agent-topology--selected))
                    (plist-get +agent-topology--model :nodes))
    (setq +agent-topology--selected (plist-get (car (plist-get +agent-topology--model :nodes)) :id)))
  (+agent-topology--render))

(defun +agent-topology--select (direction)
  "Move the selection by DIRECTION nodes."
  (let* ((ids (mapcar (lambda (node) (plist-get node :id)) (plist-get +agent-topology--model :nodes)))
         (index (or (cl-position +agent-topology--selected ids :test #'equal) 0)))
    (when ids
      (setq +agent-topology--selected (nth (mod (+ index direction) (length ids)) ids))
      (+agent-topology--render))))

(defun +agent-topology-next () "Select the next node." (interactive) (+agent-topology--select 1))
(defun +agent-topology-previous () "Select the previous node." (interactive) (+agent-topology--select -1))

(defun +agent-topology--node ()
  "Return the node at point or the selected one."
  (let ((id (or (get-text-property (point) '+agent-topology-node) +agent-topology--selected)))
    (seq-find (lambda (node) (equal (plist-get node :id) id)) (plist-get +agent-topology--model :nodes))))

(defun +agent-topology--open-file (file)
  "Open FILE in another window, at its first pending review card."
  (require 'tools/agent/collab)
  (find-file-other-window (+agent-collab--client-file file))
  (goto-char (point-min))
  (if (re-search-forward "^#\\+begin ediff\\b" nil t)
      (beginning-of-line)
    (goto-char (point-min))))

(defun +agent-topology-visit ()
  "Open the node: a file at its first card, a collaborator at its document."
  (interactive)
  (let* ((node (or (+agent-topology--node) (user-error "No node here")))
         (target (plist-get node :target)))
    (setq +agent-topology--selected (plist-get node :id))
    (cond ((plist-get target :file) (+agent-topology--open-file (plist-get target :file)))
          ((and (plist-get target :buffer) (+agent-binding-file (plist-get target :buffer)))
           (+agent-topology--open-file (+agent-binding-file (plist-get target :buffer))))
          ((plist-get target :buffer) (+agent-acp-show-buffer (plist-get target :buffer)))
          ((plist-get target :root) (dired-other-window (plist-get target :root))))))

(defun +agent-topology-chat ()
  "Open the selected collaborator's chat for a longer discussion."
  (interactive)
  (let ((buffer (plist-get (plist-get (or (+agent-topology--node) (user-error "No node here")) :target)
                           :buffer)))
    (unless (buffer-live-p buffer) (user-error "Not a session"))
    (+agent-acp-show-buffer buffer)))

(defun +agent-topology--changed-h (&rest _)
  "Redraw every topology view once, shortly after a burst of state events."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p '+agent-topology-mode) (not +agent-topology--timer))
        (setq +agent-topology--timer
              (run-with-idle-timer
               0.5 nil
               (lambda (view)
                 (when (buffer-live-p view)
                   (with-current-buffer view
                     (setq +agent-topology--timer nil)
                     (+agent-topology-refresh))))
               buffer))))))

(defun +agent-topology--teardown-h ()
  "Cancel this view's pending redraw, and stop listening after the last view."
  (when (timerp +agent-topology--timer) (cancel-timer +agent-topology--timer))
  (setq +agent-topology--timer nil)
  (unless (seq-some (lambda (buffer)
                      (and (not (eq buffer (current-buffer)))
                           (eq (buffer-local-value 'major-mode buffer) '+agent-topology-mode)))
                    (buffer-list))
    (remove-hook '+agent-acp-changed-functions #'+agent-topology--changed-h)
    (remove-hook '+agent-collab-changed-functions #'+agent-topology--changed-h)))

(defvar-keymap +agent-topology-mode-map
  :parent special-mode-map
  "n" #'+agent-topology-next
  "p" #'+agent-topology-previous
  "j" #'+agent-topology-next
  "k" #'+agent-topology-previous
  "RET" #'+agent-topology-visit
  "S-<return>" #'+agent-topology-chat
  "S-RET" #'+agent-topology-chat
  "g" #'+agent-topology-refresh)

(define-derived-mode +agent-topology-mode special-mode "Agent topology"
  "The pool as Pi, projects, documents and their collaborators (C69)."
  (setq truncate-lines t)
  (add-hook 'kill-buffer-hook #'+agent-topology--teardown-h nil t))

;;;###autoload
(defun +agent/topology ()
  "Show who collaborates on what: Pi, projects, documents and agents (C69)."
  (interactive)
  (require 'tools/agent/collab)
  (let ((buffer (get-buffer-create "*Agent Topology*")))
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-topology-mode) (+agent-topology-mode))
      (add-hook '+agent-acp-changed-functions #'+agent-topology--changed-h)
      (add-hook '+agent-collab-changed-functions #'+agent-topology--changed-h)
      (+agent-topology-refresh))
    (pop-to-buffer buffer)))

(defalias 'agent-topology #'+agent/topology
  "Show the collaborator topology of the agent pool.")

(provide 'tools/agent/topology)
;;; topology.el ends here

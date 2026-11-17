;;; tools/agent/board.el --- The collaboration graph board -*- lexical-binding: t; -*-

;;; Commentary:
;; The interactive DAG of Pi, projects, documents and their agents (C78), with
;; the research Graph Board's hand: Graphviz lays the graph out, the board
;; draws one window-sized SVG whose scene group carries a
;; translate/scale transform, so panning and zooming only change the
;; transform (el-easydraw's view model).  Click selects, drag pans, double
;; click visits, the wheel pans and C-wheel zooms around the pointer; h/j/k/l
;; move geometrically, H/L along the edges, TAB folds a project or document's
;; branch, and `?' lists every command.  Commands act on the selected node:
;; open its document or chat, start an agent for it, hand it a task, stop,
;; restart or close it.
;;
;; The board belongs to the collaboration panel (C75), whose Graph tab shows
;; it.  It redraws on state events only while shown, once per burst.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'transient)
(require 'tools/agent/acp)
(require 'tools/agent/binding)
(require 'tools/agent/topology)

(declare-function +agent-shell/here "tools/agent/config" (agent &optional file))
(declare-function +agent-shell--read-agent "tools/agent/config" (prompt))
(declare-function +agent-panel-header "tools/agent/panel" (&optional status))
(declare-function +agent-panel--show-tab "tools/agent/panel" (tab))
(declare-function +agent-panel-show-tasks "tools/agent/panel" (file scope))
(declare-function +agent-panel--editing-window "tools/agent/panel" ())

(defconst +agent-board-buffer-name "*Agent Graph*")
(defconst +agent-board--margin 24 "Blank border around the drawing, in pixels.")
(defconst +agent-board--scale-range '(0.25 . 3.0) "Smallest and largest view scale.")
(defconst +agent-board--zoom-step 1.25 "Magnification of one zoom step.")
(defconst +agent-board--pan-step 80 "Pixels one pan key moves.")
(defconst +agent-board--follow-margin 32 "Pixels a selected node keeps from the edge.")

(defconst +agent-board--palettes
  '((light :fills ((pi . "#f3e8fd") (project . "#e6f4ea") (file . "#fef7e0") (agent . "#e8f0fe"))
           :default-fill "#f1f3f4" :stroke "#9aa0a6" :edge "#80868b" :title "#202124" :text "#5f6368"
           :selected "#1a73e8" :working "#1a73e8" :attention "#d93025")
    (dark :fills ((pi . "#3a2850") (project . "#1d3d2c") (file . "#4a3e14") (agent . "#1c3a5e"))
          :default-fill "#303134" :stroke "#5f6368" :edge "#9aa0a6" :title "#e8eaed" :text "#bdc1c6"
          :selected "#8ab4f8" :working "#8ab4f8" :attention "#f28b82"))
  "Board palettes by background.")

(defvar-local +agent-board--model nil "The drawn model, folds applied.")
(defvar-local +agent-board--layout nil "Its Graphviz layout.")
(defvar-local +agent-board--scene nil "Scene plist: :svg :group :width :height.")
(defvar-local +agent-board--view nil "Viewport (:dx DX :dy DY :scale SCALE).")
(defvar-local +agent-board--selected nil "Id of the selected node.")
(defvar-local +agent-board--folds nil "Ids of folded nodes: their branches are hidden.")
(defvar-local +agent-board--image nil "The displayed image.")
(defvar-local +agent-board--size nil "Window body size the image was drawn for.")
(defvar-local +agent-board--timer nil "Pending coalesced redraw.")

(defun +agent-board--palette ()
  "Return the palette of the current background."
  (alist-get (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light) +agent-board--palettes))

;;;; Model

(defun +agent-board--hidden (model folds)
  "Return the ids MODEL hides below the FOLDS."
  (let ((children (make-hash-table :test #'equal)) hidden)
    (pcase-dolist (`(,from ,to ,_) (plist-get model :edges))
      (push to (gethash from children)))
    (dolist (fold folds)
      (let ((queue (gethash fold children)))
        (while queue
          (let ((id (pop queue)))
            (unless (or (member id hidden) (member id folds) (equal id fold))
              (push id hidden)
              (setq queue (append (gethash id children) queue)))))))
    hidden))

(defun +agent-board-model (sessions counts folds)
  "Return the topology of SESSIONS with COUNTS, branches below FOLDS hidden.
A folded node's label says how much it hides."
  (let* ((model (+agent-topology-model sessions counts))
         (hidden (+agent-board--hidden model folds)))
    (list :nodes (delq nil (mapcar (lambda (node)
                                     (unless (member (plist-get node :id) hidden)
                                       (if (member (plist-get node :id) folds)
                                           (plist-put (copy-sequence node) :label
                                                      (concat (plist-get node :label)
                                                              (format "\n▸ %d hidden"
                                                                      (length (+agent-board--hidden model (list (plist-get node :id)))))))
                                         node)))
                                   (plist-get model :nodes)))
          :edges (seq-remove (lambda (edge) (or (member (nth 0 edge) hidden) (member (nth 1 edge) hidden)))
                             (plist-get model :edges))
          :sessions sessions)))

(defun +agent-board--node (id)
  "Return the model node ID."
  (seq-find (lambda (node) (equal (plist-get node :id) id)) (plist-get +agent-board--model :nodes)))

(defun +agent-board--session (node)
  "Return the session snapshot NODE stands for, or nil."
  (when-let* ((buffer (plist-get (plist-get node :target) :buffer)))
    (seq-find (lambda (session) (eq (plist-get session :buffer) buffer))
              (plist-get +agent-board--model :sessions))))

;;;; Drawing

(defun +agent-board--svg (layout)
  "Draw the model with LAYOUT; return (SVG . GROUP)."
  (let* ((margin +agent-board--margin)
         (palette (+agent-board--palette))
         (svg (svg-create (ceiling (+ (* 2 margin) (plist-get layout :width)))
                          (ceiling (+ (* 2 margin) (plist-get layout :height)))))
         (group (dom-node 'g '((id . "agent-scene")))))
    (dom-append-child svg group)
    (dolist (edge (plist-get layout :edges))
      (when-let* ((points (plist-get edge :points)))
        (dom-append-child
         group (dom-node 'path `((d . ,(+agent-topology--path points margin)) (fill . "none")
                                 (stroke . ,(plist-get palette :edge)) (stroke-width . "1.2")
                                 ,@(pcase (plist-get edge :style)
                                     ("dashed" '((stroke-dasharray . "5 4")))
                                     ("dotted" '((stroke-dasharray . "2 4"))))))))
      (when-let* ((arrow (plist-get edge :arrow)))
        (svg-polygon group (mapcar (lambda (p) (cons (+ margin (car p)) (+ margin (cdr p)))) arrow)
                     :fill (plist-get palette :edge) :stroke (plist-get palette :edge))))
    (dolist (box (plist-get layout :nodes))
      (let* ((id (plist-get box :id))
             (node (+agent-board--node id))
             (kind (plist-get node :kind))
             (session (+agent-board--session node))
             (state (and session (+agent-topology--state session)))
             (selected (equal id +agent-board--selected))
             (width (plist-get box :width)) (height (plist-get box :height))
             (x0 (+ margin (- (plist-get box :x) (/ width 2.0))))
             (y0 (+ margin (- (plist-get box :y) (/ height 2.0))))
             (cx (+ x0 (/ width 2.0))) (cy (+ y0 (/ height 2.0)))
             (stroke (cond (selected (plist-get palette :selected))
                           ((equal state "needs you") (plist-get palette :attention))
                           ((equal state "working") (plist-get palette :working))
                           (t (plist-get palette :stroke))))
             (stroke-width (cond (selected 2.5) ((member state '("needs you" "working")) 2.0) (t 1)))
             (item (dom-node 'g (when (equal state "disconnected") '((opacity . "0.45")))))
             (args (list :fill (or (alist-get kind (plist-get palette :fills)) (plist-get palette :default-fill))
                         :stroke stroke :stroke-width stroke-width
                         :stroke-dasharray (if (member id +agent-board--folds) "4 3" "none")))
             (lines (split-string (plist-get node :label) "\n"))
             (text-y (+ cy 4 (- (/ (* (1- (length lines)) 13) 2.0)))))
        (pcase kind
          ('pi (apply #'svg-ellipse item cx cy (/ width 2.0) (/ height 2.0) args))
          ('project (apply #'svg-rectangle item x0 y0 width height :rx 12 :ry 12 args))
          (_ (apply #'svg-rectangle item x0 y0 width height :rx 6 :ry 6 args)))
        (cl-loop for line in lines for index from 0
                 do (svg-text item line :x cx :y (+ text-y (* index 13)) :text-anchor "middle"
                              :font-size (if (zerop index) 11 9.5) :font-weight (if (zerop index) "600" "400")
                              :font-family "Helvetica" :stroke "none"
                              :fill (plist-get palette (if (zerop index) :title :text))))
        (dom-append-child group item)))
    (cons svg group)))

(defun +agent-board--view-size ()
  "Return the (WIDTH . HEIGHT) in pixels the image fills."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (cons (max 120 (- (window-body-width window t) 2)) (max 80 (- (window-body-height window t) 2)))
    (cons 480 640)))

(defun +agent-board--clamp (view)
  "Return VIEW with its scale in range and part of the drawing visible."
  (pcase-let* ((`(,width . ,height) (+agent-board--view-size))
               (keep 32)
               (scale (min (cdr +agent-board--scale-range)
                           (max (car +agent-board--scale-range) (float (plist-get view :scale)))))
               (drawn-width (* scale (plist-get +agent-board--scene :width)))
               (drawn-height (* scale (plist-get +agent-board--scene :height))))
    (list :dx (round (max (- keep drawn-width) (min (- width keep) (plist-get view :dx))))
          :dy (round (max (- keep drawn-height) (min (- height keep) (plist-get view :dy))))
          :scale scale)))

(defun +agent-board--fit-view ()
  "Return the view showing the whole drawing, centered, at most at 100%."
  (pcase-let* ((`(,width . ,height) (+agent-board--view-size))
               (scene-width (plist-get +agent-board--scene :width))
               (scene-height (plist-get +agent-board--scene :height))
               (scale (min 1.0 (/ (float width) scene-width) (/ (float height) scene-height))))
    (list :dx (/ (- width (* scale scene-width)) 2.0)
          :dy (max 0 (/ (- height (* scale scene-height)) 2.0))
          :scale scale)))

(defun +agent-board--box (id)
  "Return the layout box of node ID."
  (seq-find (lambda (box) (equal (plist-get box :id) id)) (plist-get +agent-board--layout :nodes)))

(defun +agent-board--center (view id)
  "Return the viewport pixel (X . Y) of node ID's center under VIEW."
  (when-let* ((box (+agent-board--box id)))
    (let ((scale (plist-get view :scale)))
      (cons (+ (plist-get view :dx) (* scale (+ +agent-board--margin (plist-get box :x))))
            (+ (plist-get view :dy) (* scale (+ +agent-board--margin (plist-get box :y))))))))

(defun +agent-board--revealed (view id)
  "Return VIEW minimally panned so node ID is inside the follow margin."
  (if-let* ((box (+agent-board--box id)) (center (+agent-board--center view id)))
      (pcase-let* ((`(,width . ,height) (+agent-board--view-size))
                   (scale (plist-get view :scale))
                   (margin +agent-board--follow-margin)
                   (half-width (/ (* scale (plist-get box :width)) 2.0))
                   (half-height (/ (* scale (plist-get box :height)) 2.0)))
        (cl-flet ((shift (low high limit)
                    (cond ((< low margin) (- margin low))
                          ((> high (- limit margin)) (- (min (- high (- limit margin)) (- low margin))))
                          (t 0))))
          (list :dx (+ (plist-get view :dx) (shift (- (car center) half-width) (+ (car center) half-width) width))
                :dy (+ (plist-get view :dy) (shift (- (cdr center) half-height) (+ (cdr center) half-height) height))
                :scale scale)))
    view))

(defun +agent-board--zoomed (view magnification x y)
  "Return VIEW zoomed by MAGNIFICATION around viewport pixel X, Y."
  (let* ((old (float (plist-get view :scale)))
         (new (min (cdr +agent-board--scale-range) (max (car +agent-board--scale-range) (* old magnification))))
         (ratio (/ new old)))
    (list :dx (- (* ratio (plist-get view :dx)) (* (- ratio 1) x))
          :dy (- (* ratio (plist-get view :dy)) (* (- ratio 1) y))
          :scale new)))

(defun +agent-board--node-at (x y)
  "Return the id of the node drawn at viewport pixel X, Y, or nil."
  (when-let* ((view +agent-board--view))
    (let* ((scale (float (plist-get view :scale)))
           (lx (- (/ (- x (plist-get view :dx)) scale) +agent-board--margin))
           (ly (- (/ (- y (plist-get view :dy)) scale) +agent-board--margin)))
      (plist-get (seq-find (lambda (box)
                             (and (<= (abs (- lx (plist-get box :x))) (/ (plist-get box :width) 2.0))
                                  (<= (abs (- ly (plist-get box :y))) (/ (plist-get box :height) 2.0))))
                           (plist-get +agent-board--layout :nodes))
                 :id))))

(defun +agent-board--status ()
  "Return the board's status: scale, folds, the selection's state."
  (let* ((node (+agent-board--node +agent-board--selected))
         (session (and node (+agent-board--session node))))
    (string-replace
     "%" "%%"
     (concat (when +agent-board--view (format "%d%%" (round (* 100 (plist-get +agent-board--view :scale)))))
             (when +agent-board--folds (format " · %d folded" (length +agent-board--folds)))
             (when node (format " · %s" (car (split-string (plist-get node :label) "\n"))))
             (when session (format " (%s)" (+agent-topology--state session)))
             "   ? help"))))

(defun +agent-board--display ()
  "Show the scene as one window-sized image."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when +agent-board--image (ignore-errors (image-flush +agent-board--image)))
    (setq +agent-board--image nil)
    (cond
     ((null (plist-get +agent-board--model :nodes))
      (insert "\n  No agents yet.  H-T starts one for the document you are on.\n"))
     ((not (and +agent-board--scene (display-images-p) (image-type-available-p 'svg)))
      (insert "\n  The graph needs a graphical Emacs and Graphviz (dot).\n\n")
      (dolist (node (plist-get +agent-board--model :nodes))
        (insert (format "  %-8s %s\n" (plist-get node :kind)
                        (replace-regexp-in-string "\n" "  " (plist-get node :label))))))
     (t
      (pcase-let ((`(,width . ,height) (+agent-board--view-size))
                  (svg (plist-get +agent-board--scene :svg))
                  (view +agent-board--view))
        (dom-set-attribute svg 'width width)
        (dom-set-attribute svg 'height height)
        (dom-set-attribute svg 'viewBox (format "0 0 %d %d" width height))
        (dom-set-attribute (plist-get +agent-board--scene :group) 'transform
                           (format "translate(%.2f %.2f) scale(%.4f)"
                                   (plist-get view :dx) (plist-get view :dy) (plist-get view :scale)))
        (setq +agent-board--image (create-image (with-temp-buffer (svg-print svg) (buffer-string))
                                                'svg t :scale 1.0)
              +agent-board--size (cons width height))
        (insert (propertize " " 'display +agent-board--image)))))
    (goto-char (point-min))
    (force-mode-line-update)))

(defun +agent-board--set-view (view)
  "Install VIEW, kept in range, and redisplay."
  (when +agent-board--scene
    (setq +agent-board--view (+agent-board--clamp view))
    (+agent-board--display)))

(defun +agent-board--rebuild-scene ()
  "Redraw the scene from the model and layout, keeping the view."
  (setq +agent-board--scene
        (when +agent-board--layout
          (pcase-let ((`(,svg . ,group) (+agent-board--svg +agent-board--layout)))
            (list :svg svg :group group
                  :width (+ (* 2 +agent-board--margin) (plist-get +agent-board--layout :width))
                  :height (+ (* 2 +agent-board--margin) (plist-get +agent-board--layout :height)))))))

(defun +agent-board-refresh (&optional keep-view)
  "Rebuild the model, layout and scene; fit the view unless KEEP-VIEW."
  (interactive)
  (with-current-buffer (get-buffer-create +agent-board-buffer-name)
    (when (timerp +agent-board--timer) (cancel-timer +agent-board--timer))
    (setq +agent-board--timer nil)
    (let* ((sessions (+agent-topology--sessions))
           (ids (mapcar (lambda (n) (plist-get n :id)) (plist-get (+agent-topology-model sessions #'ignore) :nodes))))
      ;; A fold of a node that is gone is forgotten.
      (setq +agent-board--folds (seq-filter (lambda (id) (member id ids)) +agent-board--folds)
            +agent-board--model (+agent-board-model sessions #'+agent-topology-file-counts +agent-board--folds)))
    (setq +agent-board--layout (and (plist-get +agent-board--model :nodes)
                                    (+agent-topology--layout +agent-board--model)))
    (unless (+agent-board--node +agent-board--selected)
      (setq +agent-board--selected (plist-get (car (plist-get +agent-board--model :nodes)) :id)))
    (+agent-board--rebuild-scene)
    (setq +agent-board--view (and +agent-board--scene
                                  (if (and keep-view +agent-board--view) (+agent-board--clamp +agent-board--view)
                                    (+agent-board--fit-view))))
    (+agent-board--display)))

(defun +agent-board-select (id)
  "Select node ID, keeping it in view."
  (setq +agent-board--selected id)
  (+agent-board--rebuild-scene)
  (if +agent-board--view
      (+agent-board--set-view (+agent-board--revealed +agent-board--view id))
    (+agent-board--display)))

;;;; Viewport commands

(defun +agent-board--pan (dx dy)
  "Move the drawing by DX, DY pixels."
  (when +agent-board--view
    (+agent-board--set-view (list :dx (+ (plist-get +agent-board--view :dx) dx)
                                  :dy (+ (plist-get +agent-board--view :dy) dy)
                                  :scale (plist-get +agent-board--view :scale)))))

(defun +agent-board-pan-left () "Show more to the left." (interactive) (+agent-board--pan +agent-board--pan-step 0))
(defun +agent-board-pan-right () "Show more to the right." (interactive) (+agent-board--pan (- +agent-board--pan-step) 0))
(defun +agent-board-pan-up () "Show more above." (interactive) (+agent-board--pan 0 +agent-board--pan-step))
(defun +agent-board-pan-down () "Show more below." (interactive) (+agent-board--pan 0 (- +agent-board--pan-step)))

(defun +agent-board--zoom-by (magnification &optional x y)
  "Zoom by MAGNIFICATION around X, Y, else the selection, else the middle."
  (when +agent-board--view
    (pcase-let* ((`(,width . ,height) (+agent-board--view-size))
                 (selected (+agent-board--center +agent-board--view +agent-board--selected))
                 (anchor (or (and x y (cons x y))
                             (and selected (<= 0 (car selected) width) (<= 0 (cdr selected) height) selected)
                             (cons (/ width 2.0) (/ height 2.0)))))
      (+agent-board--set-view (+agent-board--zoomed +agent-board--view magnification (car anchor) (cdr anchor))))))

(defun +agent-board-zoom-in () "Magnify." (interactive) (+agent-board--zoom-by +agent-board--zoom-step))
(defun +agent-board-zoom-out () "Shrink." (interactive) (+agent-board--zoom-by (/ 1.0 +agent-board--zoom-step)))
(defun +agent-board-zoom-reset () "Show at 100%." (interactive)
  (when +agent-board--view (+agent-board--zoom-by (/ 1.0 (plist-get +agent-board--view :scale)))))
(defun +agent-board-fit () "Fit the whole graph." (interactive)
  (when +agent-board--scene (+agent-board--set-view (+agent-board--fit-view))))

(defun +agent-board-center ()
  "Center the selected node."
  (interactive)
  (when-let* ((view +agent-board--view) (center (+agent-board--center view +agent-board--selected)))
    (pcase-let ((`(,width . ,height) (+agent-board--view-size)))
      (+agent-board--set-view (list :dx (+ (plist-get view :dx) (- (/ width 2.0) (car center)))
                                    :dy (+ (plist-get view :dy) (- (/ height 2.0) (cdr center)))
                                    :scale (plist-get view :scale))))))

(defun +agent-board--event-xy (event)
  "Return the viewport pixel of mouse EVENT, or nil."
  (let ((xy (posn-x-y (event-start event))))
    (and (consp xy) (numberp (car xy)) (numberp (cdr xy)) xy)))

(defun +agent-board-mouse-down (event)
  "Select the node under EVENT on a click, or pan by dragging."
  (interactive "e")
  (when-let* ((window (posn-window (event-start event))) ((windowp window)))
    (select-window window))
  (when-let* ((start (and +agent-board--view (+agent-board--event-xy event))))
    (let ((origin +agent-board--view) moved done)
      (track-mouse
        (setq track-mouse 'dragging)
        (while (not done)
          (let ((next (read-event)))
            (if (mouse-movement-p next)
                (when-let* ((xy (+agent-board--event-xy next)))
                  (when (or moved (> (+ (abs (- (car xy) (car start))) (abs (- (cdr xy) (cdr start)))) 3))
                    (setq moved t)
                    (+agent-board--set-view (list :dx (+ (plist-get origin :dx) (- (car xy) (car start)))
                                                  :dy (+ (plist-get origin :dy) (- (cdr xy) (cdr start)))
                                                  :scale (plist-get origin :scale)))))
              (setq done t)
              (unless (memq (event-basic-type next) '(mouse-1 mouse-2 mouse-3))
                (push next unread-command-events))))))
      (unless moved
        (when-let* ((id (+agent-board--node-at (car start) (cdr start))))
          (+agent-board-select id))))))

(defun +agent-board-mouse-visit (event)
  "Visit the node double-clicked with EVENT."
  (interactive "e")
  (when-let* ((xy (+agent-board--event-xy event)) (id (+agent-board--node-at (car xy) (cdr xy))))
    (setq +agent-board--selected id)
    (+agent-board-visit)))

(defun +agent-board-mouse-menu (event)
  "Select the node under EVENT and open the command menu."
  (interactive "e")
  (when-let* ((xy (+agent-board--event-xy event)) (id (+agent-board--node-at (car xy) (cdr xy))))
    (+agent-board-select id))
  (call-interactively #'+agent-board-help))

(defun +agent-board-wheel (event)
  "Pan with the wheel EVENT; with Control, zoom around the pointer."
  (interactive "e")
  (when +agent-board--view
    (let* ((type (event-basic-type event))
           (vertical (memq type '(wheel-up wheel-down)))
           (delta (nth 4 event))
           (precise (and (consp delta) (numberp (car delta)) (numberp (cdr delta))
                         (abs (if vertical (cdr delta) (car delta)))))
           (pixels (if (and precise (> precise 0)) precise 40))
           (xy (+agent-board--event-xy event)))
      (cond
       ((and vertical (memq 'control (event-modifiers event)))
        (let ((step (expt +agent-board--zoom-step (min 1.0 (/ pixels 40.0)))))
          (+agent-board--zoom-by (if (eq type 'wheel-up) step (/ 1.0 step)) (car xy) (cdr xy))))
       ((and vertical (not (memq 'shift (event-modifiers event))))
        (+agent-board--pan 0 (if (eq type 'wheel-up) pixels (- pixels))))
       (t (+agent-board--pan (if (if vertical (eq type 'wheel-up)
                                   (eq type (if mouse-wheel-flip-direction 'wheel-left 'wheel-right)))
                                 pixels (- pixels))
                             0))))))

;;;; Navigation

(defun +agent-board--move (direction)
  "Select the nearest node in DIRECTION."
  (let* ((boxes (plist-get +agent-board--layout :nodes))
         (current (or (+agent-board--box +agent-board--selected) (car boxes)))
         best best-score)
    (unless current (user-error "The graph has no nodes"))
    (dolist (box boxes)
      (unless (eq box current)
        (let* ((dx (- (plist-get box :x) (plist-get current :x)))
               (dy (- (plist-get box :y) (plist-get current :y)))
               (horizontal (memq direction '(left right)))
               (score (+ (abs (if horizontal dx dy)) (* 2.0 (abs (if horizontal dy dx)))))
               (eligible (pcase direction ('left (< dx -0.5)) ('right (> dx 0.5)) ('up (< dy -0.5)) ('down (> dy 0.5)))))
          (when (and eligible (or (null best-score) (< score best-score)))
            (setq best box best-score score)))))
    (+agent-board-select (plist-get (or best current) :id))))

(defun +agent-board-move-left () "Select the node to the left." (interactive) (+agent-board--move 'left))
(defun +agent-board-move-right () "Select the node to the right." (interactive) (+agent-board--move 'right))
(defun +agent-board-move-up () "Select the node above." (interactive) (+agent-board--move 'up))
(defun +agent-board-move-down () "Select the node below." (interactive) (+agent-board--move 'down))

(defun +agent-board-select-parent ()
  "Select a node with an edge into the selection."
  (interactive)
  (when-let* ((edge (seq-find (lambda (edge) (equal (nth 1 edge) +agent-board--selected))
                              (plist-get +agent-board--model :edges))))
    (+agent-board-select (nth 0 edge))))

(defun +agent-board-select-child ()
  "Select the first node the selection has an edge to."
  (interactive)
  (when-let* ((edge (seq-find (lambda (edge) (equal (nth 0 edge) +agent-board--selected))
                              (plist-get +agent-board--model :edges))))
    (+agent-board-select (nth 1 edge))))

(defun +agent-board-goto (id)
  "Select the node ID, read by its name."
  (interactive
   (let ((choices (mapcar (lambda (node) (cons (replace-regexp-in-string "\n" " · " (plist-get node :label))
                                               (plist-get node :id)))
                          (plist-get +agent-board--model :nodes))))
     (list (cdr (assoc (completing-read "Go to: " choices nil t) choices)))))
  (+agent-board-select id))

(defun +agent-board-toggle-fold ()
  "Fold or unfold the selected node's branch."
  (interactive)
  (setq +agent-board--folds (if (member +agent-board--selected +agent-board--folds)
                                (delete +agent-board--selected +agent-board--folds)
                              (cons +agent-board--selected +agent-board--folds)))
  (+agent-board-refresh t))

(defun +agent-board-unfold-all ()
  "Unfold every branch."
  (interactive)
  (setq +agent-board--folds nil)
  (+agent-board-refresh t))

;;;; Acting on the selection

(defun +agent-board--selection ()
  "Return the selected node, or signal."
  (or (+agent-board--node +agent-board--selected) (user-error "Nothing selected")))

(defun +agent-board--buffer ()
  "Return the selected session's buffer, or signal."
  (let ((buffer (plist-get (plist-get (+agent-board--selection) :target) :buffer)))
    (unless (buffer-live-p buffer) (user-error "Select an agent"))
    buffer))

(defun +agent-board--file ()
  "Return the document of the selection: a file, or an agent's primary file."
  (let* ((target (plist-get (+agent-board--selection) :target)))
    (or (plist-get target :file)
        (and (plist-get target :buffer) (+agent-binding-file (plist-get target :buffer))))))

(defun +agent-board--open-file (file)
  "Open FILE in the editing area."
  (require 'tools/agent/collab)
  (select-window (+agent-panel--editing-window))
  (let ((client (+agent-collab--client-file file)))
    (if (and (string-match-p "\\.\\(?:md\\|markdown\\)\\'" client) (fboundp '+noema/open-file))
        (+noema/open-file client)
      (find-file client))))

(declare-function +noema/open-file "noema/bridge/config" (file &optional origin-window))

(defun +agent-board-visit ()
  "Open the selection: a document, an agent's document, a project, or Pi."
  (interactive)
  (let* ((node (+agent-board--selection)) (target (plist-get node :target)))
    (pcase (plist-get node :kind)
      ('pi (+agent-panel--show-tab 'pi))
      ('project (select-window (+agent-panel--editing-window)) (dired (plist-get target :root)))
      (_ (if-let* ((file (+agent-board--file)))
             (+agent-board--open-file file)
           (+agent-board-chat))))))

(defun +agent-board-chat ()
  "Open the selected agent's chat."
  (interactive)
  (let ((buffer (+agent-board--buffer)))
    (select-window (+agent-panel--editing-window))
    (+agent-acp-show-buffer buffer)))

(defun +agent-board-tasks ()
  "Show the tasks of the selected document, or project."
  (interactive)
  (let* ((node (+agent-board--selection)))
    (if (eq (plist-get node :kind) 'project)
        (+agent-panel-show-tasks (expand-file-name ".noema-root" (plist-get (plist-get node :target) :root)) 'project)
      (+agent-panel-show-tasks (or (+agent-board--file) (user-error "No document here")) 'file))))

(defun +agent-board-new-agent (agent)
  "Start AGENT for the selected document, or in the selected project."
  (interactive (list (+agent-shell--read-agent "New agent: ")))
  (let* ((node (+agent-board--selection))
         (file (and (memq (plist-get node :kind) '(file agent)) (+agent-board--file)))
         (root (plist-get (plist-get node :target) :root)))
    (save-window-excursion
      (let ((default-directory (or root (and file (file-name-directory file)) default-directory)))
        (+agent-shell/here agent (and file (+agent-binding-canonical file)))))
    (+agent-board-refresh t)))

(defun +agent-board-stop ()
  "Stop the selected agent's turn."
  (interactive)
  (+agent-acp-stop (+agent-board--buffer)))

(defun +agent-board-restart ()
  "Restart the selected agent's client, keeping its conversation."
  (interactive)
  (+agent-acp-restart-native (+agent-board--buffer)))

(defun +agent-board-close ()
  "Close the selected agent; its history remains."
  (interactive)
  (let ((buffer (+agent-board--buffer)))
    (when (y-or-n-p (format "Close %s? " (buffer-local-value '+agent-acp-session-name buffer)))
      (+agent-acp-close buffer))))

(transient-define-prefix +agent-board-help ()
  "Commands of the collaboration graph."
  [["Navigate"
    ("h/j/k/l" "move by direction" +agent-board-move-down)
    ("H" "parent" +agent-board-select-parent)
    ("L" "child" +agent-board-select-child)
    ("/" "go to…" +agent-board-goto)
    ("TAB" "fold branch" +agent-board-toggle-fold)
    ("U" "unfold all" +agent-board-unfold-all)]
   ["View"
    ("+" "zoom in" +agent-board-zoom-in)
    ("-" "zoom out" +agent-board-zoom-out)
    ("0" "100%" +agent-board-zoom-reset)
    ("=" "fit" +agent-board-fit)
    ("." "center" +agent-board-center)
    ("g" "refresh" +agent-board-refresh)]
   ["Selection"
    ("RET" "open" +agent-board-visit)
    ("o" "chat" +agent-board-chat)
    ("t" "tasks" +agent-board-tasks)
    ("n" "new agent for it…" +agent-board-new-agent)]
   ["Agent"
    ("s" "stop turn" +agent-board-stop)
    ("r" "restart client" +agent-board-restart)
    ("x" "close" +agent-board-close)]])

;;;; Mode

(defconst +agent-board--bindings
  '(("RET" . +agent-board-visit) ("o" . +agent-board-chat) ("S-<return>" . +agent-board-chat)
    ("t" . +agent-board-tasks) ("n" . +agent-board-new-agent) ("s" . +agent-board-stop)
    ("r" . +agent-board-restart) ("x" . +agent-board-close) ("g" . +agent-board-refresh)
    ("?" . +agent-board-help) ("TAB" . +agent-board-toggle-fold) ("U" . +agent-board-unfold-all)
    ("/" . +agent-board-goto) ("H" . +agent-board-select-parent) ("L" . +agent-board-select-child)
    ("h" . +agent-board-move-left) ("j" . +agent-board-move-down)
    ("k" . +agent-board-move-up) ("l" . +agent-board-move-right)
    ("<left>" . +agent-board-move-left) ("<down>" . +agent-board-move-down)
    ("<up>" . +agent-board-move-up) ("<right>" . +agent-board-move-right)
    ("+" . +agent-board-zoom-in) ("=" . +agent-board-fit) ("-" . +agent-board-zoom-out)
    ("0" . +agent-board-zoom-reset) ("." . +agent-board-center)
    ("S-<left>" . +agent-board-pan-left) ("S-<right>" . +agent-board-pan-right)
    ("S-<up>" . +agent-board-pan-up) ("S-<down>" . +agent-board-pan-down)
    ("<down-mouse-1>" . +agent-board-mouse-down) ("<mouse-1>" . ignore)
    ("<double-down-mouse-1>" . ignore) ("<double-mouse-1>" . +agent-board-mouse-visit)
    ("<mouse-3>" . +agent-board-mouse-menu)
    ("<wheel-up>" . +agent-board-wheel) ("<wheel-down>" . +agent-board-wheel)
    ("<wheel-left>" . +agent-board-wheel) ("<wheel-right>" . +agent-board-wheel)
    ("S-<wheel-up>" . +agent-board-wheel) ("S-<wheel-down>" . +agent-board-wheel)
    ("C-<wheel-up>" . +agent-board-wheel) ("C-<wheel-down>" . +agent-board-wheel))
  "Board keys, shared by plain Emacs and Evil normal state.")

(defvar +agent-board-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (dolist (binding +agent-board--bindings) (define-key map (kbd (car binding)) (cdr binding)))
    map)
  "Keymap of `+agent-board-mode'.")

(defun +agent-board--window-size-h (window)
  "Redraw the board when its WINDOW changed size."
  (with-current-buffer (window-buffer window)
    (when (and (derived-mode-p '+agent-board-mode) +agent-board--scene
               (not (equal +agent-board--size (+agent-board--view-size))))
      (+agent-board--set-view +agent-board--view))))

(defun +agent-board--teardown-h ()
  "Cancel the board's pending redraw."
  (when (timerp +agent-board--timer) (cancel-timer +agent-board--timer)))

(define-derived-mode +agent-board-mode special-mode "Agent-Graph"
  "The interactive graph of Pi, projects, documents and agents (C78).

\\{+agent-board-mode-map}"
  (setq-local truncate-lines t
              cursor-type nil
              scroll-margin 0
              auto-hscroll-mode nil
              mwheel-coalesce-scroll-events nil)
  (setq-local header-line-format '(:eval (+agent-panel-header (+agent-board--status))))
  (add-hook 'window-size-change-functions #'+agent-board--window-size-h nil t)
  (add-hook 'kill-buffer-hook #'+agent-board--teardown-h nil t)
  (when (fboundp 'evil-local-set-key)
    (dolist (binding +agent-board--bindings)
      (evil-local-set-key 'normal (kbd (car binding)) (cdr binding)))))

(defun +agent-board-buffer ()
  "Return the board buffer, created and drawn on first use."
  (or (get-buffer +agent-board-buffer-name)
      (with-current-buffer (get-buffer-create +agent-board-buffer-name)
        (+agent-board-mode)
        (current-buffer))))

(defun +agent-board-changed-h (&rest _)
  "Redraw the board once after a burst of state events, when it is shown."
  (when-let* ((buffer (get-buffer +agent-board-buffer-name))
              ((get-buffer-window buffer t)))
    (with-current-buffer buffer
      (unless +agent-board--timer
        (setq +agent-board--timer (run-with-idle-timer 0.5 nil #'+agent-board-refresh t))))))

(provide 'tools/agent/board)
;;; board.el ends here

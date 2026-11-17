;;; agent-board-tests.el --- The collaboration graph board (C78) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'tools/agent/board)

(defun agent-board-tests--sessions ()
  (list (list :buffer 'pi :name "pi" :agent "pi" :root "/p/" :kind 'pi :turn-state 'idle)
        (list :buffer 'a :name "claude·SAI" :agent "claude" :root "/p/" :file "/p/SAI.md" :turn-state 'running)
        (list :buffer 'b :name "codex·SAI" :agent "codex" :root "/p/" :file "/p/SAI.md" :turn-state 'idle)))

(ert-deftest agent-board-c78-folding-hides-a-branch-and-says-how-much ()
  (let* ((model (+agent-board-model (agent-board-tests--sessions) #'ignore '("file:/p/SAI.md")))
         (ids (mapcar (lambda (n) (plist-get n :id)) (plist-get model :nodes))))
    (should (member "file:/p/SAI.md" ids))
    (should-not (member "session:a" ids))
    (should-not (member "session:b" ids))
    (should (string-match-p "▸ 2 hidden" (plist-get (seq-find (lambda (n) (equal (plist-get n :id) "file:/p/SAI.md"))
                                                               (plist-get model :nodes))
                                                     :label)))
    (should-not (seq-find (lambda (edge) (equal (nth 1 edge) "session:a")) (plist-get model :edges)))))

(defmacro agent-board-tests--with-layout (&rest body)
  "Run BODY in a board with three laid-out boxes."
  `(with-temp-buffer
     (+agent-board-mode)
     (setq +agent-board--model (+agent-board-model (agent-board-tests--sessions) #'ignore nil)
           +agent-board--layout (list :width 300 :height 200
                                      :nodes (list (list :id "project:/p/" :x 150 :y 20 :width 80 :height 30)
                                                   (list :id "file:/p/SAI.md" :x 150 :y 100 :width 80 :height 30)
                                                   (list :id "session:a" :x 60 :y 180 :width 80 :height 30)
                                                   (list :id "session:b" :x 240 :y 180 :width 80 :height 30)))
           +agent-board--selected "file:/p/SAI.md")
     (cl-letf (((symbol-function '+agent-board--view-size) (lambda () (cons 400 300)))
               ((symbol-function '+agent-board--display) #'ignore))
       (+agent-board--rebuild-scene)
       (setq +agent-board--view (list :dx 0 :dy 0 :scale 1.0))
       ,@body)))

(ert-deftest agent-board-c78-a-click-finds-the-node-through-the-view ()
  (agent-board-tests--with-layout
   (should (equal (+agent-board--node-at (+ 24 150) (+ 24 100)) "file:/p/SAI.md"))
   (should-not (+agent-board--node-at 5 5))
   (setq +agent-board--view (list :dx 10 :dy 0 :scale 2.0))
   (should (equal (+agent-board--node-at (+ 10 (* 2 (+ 24 60))) (* 2 (+ 24 180))) "session:a"))))

(ert-deftest agent-board-c78-keys-move-by-direction-and-along-edges ()
  (agent-board-tests--with-layout
   (+agent-board-move-down)
   (should (member +agent-board--selected '("session:a" "session:b")))
   (+agent-board-move-right)
   (should (equal +agent-board--selected "session:b"))
   (+agent-board-move-left)
   (should (equal +agent-board--selected "session:a"))
   (+agent-board-select-parent)
   (should (equal +agent-board--selected "file:/p/SAI.md"))
   (+agent-board-select-parent)
   (should (equal +agent-board--selected "project:/p/"))))

(ert-deftest agent-board-c78-zoom-keeps-the-point-under-the-pointer ()
  (agent-board-tests--with-layout
   (let* ((view (+agent-board--zoomed (list :dx 0 :dy 0 :scale 1.0) 2.0 100 100)))
     (should (= (plist-get view :scale) 2.0))
     (should (= (plist-get view :dx) -100.0))
     (should (equal (plist-get (+agent-board--zoomed view 100 0 0) :scale) (cdr +agent-board--scale-range))))
   (let ((fit (+agent-board--fit-view)))
     (should (<= (plist-get fit :scale) 1.0)))))

(provide 'agent-board-tests)
;;; agent-board-tests.el ends here

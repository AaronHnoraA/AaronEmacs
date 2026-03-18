;;; init-expand-region.el --- Lightweight semantic expand-region -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'cl-lib)

(defvar-local my/expand-region-history nil
  "Previous region states for `my/expand-region'.")

(defun my/expand-region--push-history ()
  "Remember the current region state."
  (when (use-region-p)
    (push (cons (region-beginning) (region-end)) my/expand-region-history)))

(defun my/expand-region--tree-bounds ()
  "Return enclosing tree-sitter bounds at point."
  (when (and (fboundp 'treesit-node-at)
             (fboundp 'treesit-ready-p)
             (ignore-errors (treesit-ready-p major-mode)))
    (let ((node (treesit-node-at (point)))
          bounds)
      (while node
        (push (cons (treesit-node-start node)
                    (treesit-node-end node))
              bounds)
        (setq node (treesit-node-parent node)))
      bounds)))

(defun my/expand-region--fallback-bounds ()
  "Return generic expansion candidates around point."
  (let (bounds)
    (dolist (thing '(symbol sexp list sentence paragraph defun))
      (when-let* ((b (bounds-of-thing-at-point thing)))
        (push (cons (car b) (cdr b)) bounds)))
    (push (cons (line-beginning-position) (line-end-position)) bounds)
    (push (cons (point-min) (point-max)) bounds)
    bounds))

(defun my/expand-region--normalize-candidates (bounds)
  "Normalize and sort BOUNDS from smallest to largest."
  (sort
   (delete-dups
    (seq-filter
     (lambda (b)
       (and (consp b)
            (< (car b) (cdr b))))
     bounds))
   (lambda (a b)
     (< (- (cdr a) (car a))
        (- (cdr b) (car b))))))

(defun my/expand-region--next-bounds ()
  "Return the next larger candidate bounds."
  (let* ((current (if (use-region-p)
                      (cons (region-beginning) (region-end))
                    (cons (point) (point))))
         (candidates (my/expand-region--normalize-candidates
                      (append (my/expand-region--tree-bounds)
                              (my/expand-region--fallback-bounds)))))
    (seq-find
     (lambda (candidate)
       (or (< (car candidate) (car current))
           (> (cdr candidate) (cdr current))))
     candidates)))

(defun my/expand-region ()
  "Expand the active region semantically."
  (interactive)
  (when-let* ((next (my/expand-region--next-bounds)))
    (my/expand-region--push-history)
    (goto-char (car next))
    (push-mark (cdr next) t t)))

(defun my/contract-region ()
  "Contract the region to its previous state."
  (interactive)
  (unless my/expand-region-history
    (user-error "No previous region state"))
  (let ((state (pop my/expand-region-history)))
    (goto-char (car state))
    (push-mark (cdr state) t t)))

(my/leader-key-label "v" "selection")
(my/evil-global-leader-set "v" #'my/expand-region "expand region")
(my/evil-global-leader-set "V" #'my/contract-region "contract region")

(provide 'init-expand-region)
;;; init-expand-region.el ends here

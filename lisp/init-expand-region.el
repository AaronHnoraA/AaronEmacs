;;; init-expand-region.el --- Structural selection helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'cl-lib)
(require 'seq)

(defvar-local my/expand-region-history nil
  "Previous region states for `my/expand-region'.")

(defconst my/selection-definition-node-regexp
  (rx (or "function"
          "method"
          "lambda"
          "class"
          "interface"
          "struct"
          "enum"
          "trait"
          "impl"
          "namespace"
          "module"
          "definition"))
  "Tree-sitter node fragments treated as definition-like forms.")

(defconst my/selection-block-node-regexp
  (rx (or "block"
          "body"
          "suite"
          "statement_block"
          "statement_list"
          "declaration_list"
          "class_body"
          "enum_body"
          "object"
          "array"
          "list"
          "tuple"
          "dictionary"))
  "Tree-sitter node fragments treated as block-like forms.")

(defconst my/selection-statement-node-regexp
  (rx (or "statement"
          "declaration"
          "assignment"
          "return"
          "throw"
          "break"
          "continue"
          "if"
          "for"
          "while"
          "switch"
          "case"
          "match"
          "try"
          "catch"
          "with"
          "import"
          "export"))
  "Tree-sitter node fragments treated as statement-like forms.")

(defconst my/selection-expression-node-regexp
  (rx (or "expression"
          "call"
          "argument"
          "parameter"
          "binary"
          "unary"
          "attribute"
          "subscript"
          "pair"
          "element"))
  "Tree-sitter node fragments treated as expression-like forms.")

(defun my/selection--push-history ()
  "Remember the current region state."
  (when (use-region-p)
    (push (cons (region-beginning) (region-end)) my/expand-region-history)))

(defun my/selection--activate-bounds (bounds)
  "Activate region for BOUNDS."
  (unless (and (consp bounds)
               (< (car bounds) (cdr bounds)))
    (user-error "No structural region found here"))
  (my/selection--push-history)
  (goto-char (car bounds))
  (push-mark (cdr bounds) t t))

(defun my/selection--treesit-available-p ()
  "Return non-nil when tree-sitter APIs are ready for the current buffer."
  (and (fboundp 'treesit-node-at)
       (fboundp 'treesit-ready-p)
       (ignore-errors (treesit-ready-p nil t))))

(defun my/selection--node-at-point ()
  "Return the current tree-sitter node around point."
  (when (my/selection--treesit-available-p)
    (or (treesit-node-at (point))
        (and (> (point) (point-min))
             (treesit-node-at (1- (point)))))))

(defun my/selection--node-bounds (node)
  "Return bounds of tree-sitter NODE."
  (when node
    (cons (treesit-node-start node)
          (treesit-node-end node))))

(defun my/selection--ancestor-matching (regexp)
  "Return nearest ancestor node matching REGEXP."
  (when-let* ((node (my/selection--node-at-point)))
    (catch 'match
      (while node
        (when (string-match-p regexp (treesit-node-type node))
          (throw 'match node))
        (setq node (treesit-node-parent node))))))

(defun my/selection--descendant-matching (node regexp)
  "Return first descendant of NODE matching REGEXP."
  (when node
    (or (when (string-match-p regexp (treesit-node-type node))
          node)
        (let ((count (treesit-node-child-count node))
              found)
          (dotimes (index count found)
            (unless found
              (setq found
                    (my/selection--descendant-matching
                     (treesit-node-child node index)
                     regexp))))))))

(defun my/selection--matching-delimiter-p (open close)
  "Return non-nil when OPEN and CLOSE are matching delimiters."
  (or (and (eq open ?\{) (eq close ?\}))
      (and (eq open ?\[) (eq close ?\]))
      (and (eq open ?\() (eq close ?\)))))

(defun my/selection--trim-bounds (bounds)
  "Trim surrounding whitespace and paired delimiters from BOUNDS."
  (pcase-let ((`(,beg . ,end) bounds))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (goto-char end)
      (skip-chars-backward " \t\n")
      (setq end (point))
      (when (and (< beg end)
                 (my/selection--matching-delimiter-p
                  (char-after beg)
                  (char-before end)))
        (setq beg (1+ beg)
              end (1- end))
        (goto-char beg)
        (skip-chars-forward " \t\n")
        (setq beg (point))
        (goto-char end)
        (skip-chars-backward " \t\n")
        (setq end (point)))
      (and (< beg end)
           (cons beg end)))))

(defun my/selection--defun-bounds ()
  "Return bounds of the surrounding defun."
  (save-excursion
    (condition-case nil
        (progn
          (end-of-defun)
          (let ((end (point)))
            (beginning-of-defun)
            (cons (point) end)))
      (error nil))))

(defun my/selection--defun-body-bounds ()
  "Return bounds of the body inside the surrounding defun."
  (when-let* ((bounds (my/selection--defun-bounds)))
    (or (my/selection--trim-bounds bounds)
        bounds)))

(defun my/selection--line-bounds ()
  "Return trimmed bounds of the current line."
  (let ((bounds (cons (line-beginning-position) (line-end-position))))
    (or (my/selection--trim-bounds bounds)
        bounds)))

(defun my/selection--list-bounds ()
  "Return bounds of the nearest enclosing list-like form."
  (or (bounds-of-thing-at-point 'list)
      (save-excursion
        (condition-case nil
            (progn
              (backward-up-list)
              (let ((beg (point)))
                (forward-list)
                (cons beg (point))))
          (error nil)))))

(defun my/selection--expression-bounds ()
  "Return a predictable expression-sized region."
  (or (bounds-of-thing-at-point 'sexp)
      (my/selection--node-bounds (my/selection--expression-node))
      (bounds-of-thing-at-point 'symbol)))

(defun my/selection--statement-bounds ()
  "Return a predictable statement-sized region."
  (or (my/selection--node-bounds (my/selection--statement-node))
      (bounds-of-thing-at-point 'sexp)
      (my/selection--line-bounds)))

(defun my/selection--block-bounds ()
  "Return a predictable block-sized region."
  (or (my/selection--node-bounds (my/selection--block-node))
      (my/selection--list-bounds)
      (my/selection--defun-body-bounds)
      (my/selection--defun-bounds)))

(defun my/selection--inner-block-bounds ()
  "Return inner bounds of the current block."
  (or (when-let* ((block (my/selection--block-bounds)))
        (my/selection--trim-bounds block))
      (when-let* ((bounds (my/selection--list-bounds)))
        (my/selection--trim-bounds bounds))
      (my/selection--defun-body-bounds)))

(defun my/selection--definition-node ()
  "Return the nearest definition-like node."
  (my/selection--ancestor-matching my/selection-definition-node-regexp))

(defun my/selection--block-node ()
  "Return the nearest block-like node."
  (or (my/selection--ancestor-matching my/selection-block-node-regexp)
      (when-let* ((definition (my/selection--definition-node)))
        (my/selection--descendant-matching
         definition
         my/selection-block-node-regexp))))

(defun my/selection--statement-node ()
  "Return the nearest statement-like node."
  (my/selection--ancestor-matching my/selection-statement-node-regexp))

(defun my/selection--expression-node ()
  "Return the nearest expression-like node."
  (my/selection--ancestor-matching my/selection-expression-node-regexp))

(defun my/selection--interesting-node-p (node)
  "Return non-nil when NODE is useful for region expansion."
  (let ((type (treesit-node-type node)))
    (or (string-match-p my/selection-expression-node-regexp type)
        (string-match-p my/selection-statement-node-regexp type)
        (string-match-p my/selection-block-node-regexp type)
        (string-match-p my/selection-definition-node-regexp type))))

(defun my/expand-region--tree-bounds ()
  "Return enclosing tree-sitter bounds at point."
  (delq nil
        (list (my/selection--expression-bounds)
              (my/selection--statement-bounds)
              (my/selection--inner-block-bounds)
              (my/selection--block-bounds)
              (my/selection--defun-body-bounds)
              (my/selection--defun-bounds))))

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
    (my/selection--activate-bounds next)))

(defun my/contract-region ()
  "Contract the region to its previous state."
  (interactive)
  (unless my/expand-region-history
    (user-error "No previous region state"))
  (let ((state (pop my/expand-region-history)))
    (goto-char (car state))
    (push-mark (cdr state) t t)))

(defun my/select-defun ()
  "Select the surrounding function-like form.
Prefer Emacs defun navigation so `SPC v f' stays aligned with the
current major mode's notion of a function."
  (interactive)
  (my/selection--activate-bounds
   (or (my/selection--defun-bounds)
       (my/selection--node-bounds (my/selection--definition-node))
       (bounds-of-thing-at-point 'defun))))

(defun my/select-inner-defun ()
  "Select the body of the surrounding function-like form."
  (interactive)
  (my/selection--activate-bounds
   (or (my/selection--defun-body-bounds)
       (when-let* ((definition (my/selection--definition-node))
                   (body (my/selection--descendant-matching
                          definition
                          my/selection-block-node-regexp)))
         (my/selection--trim-bounds (my/selection--node-bounds body)))
       (bounds-of-thing-at-point 'defun))))

(defun my/select-statement ()
  "Select the surrounding statement-like form."
  (interactive)
  (my/selection--activate-bounds
   (my/selection--statement-bounds)))

(defun my/select-expression ()
  "Select the surrounding expression-like form."
  (interactive)
  (my/selection--activate-bounds
   (or (my/selection--expression-bounds)
       (my/selection--line-bounds))))

(defun my/select-block ()
  "Select the surrounding block-like form."
  (interactive)
  (my/selection--activate-bounds
   (my/selection--block-bounds)))

(defun my/select-inner-block ()
  "Select the body of the surrounding block."
  (interactive)
  (my/selection--activate-bounds
   (or (my/selection--inner-block-bounds)
       (my/selection--block-bounds))))

(defun my/select-parent-syntax-node ()
  "Select the next larger structural region."
  (interactive)
  (call-interactively #'my/expand-region))

(my/leader!
  "v"   '(:ignore t :which-key "selection")
  "v v" '(:def my/expand-region :which-key "expand region")
  "v V" '(:def my/contract-region :which-key "contract region")
  "v f" '(:def my/select-defun :which-key "select defun")
  "v F" '(:def my/select-inner-defun :which-key "select inner defun")
  "v s" '(:def my/select-statement :which-key "select statement")
  "v e" '(:def my/select-expression :which-key "select expression")
  "v b" '(:def my/select-block :which-key "select block")
  "v B" '(:def my/select-inner-block :which-key "select inner block")
  "v p" '(:def my/select-parent-syntax-node :which-key "select parent")
  "V" '(:def my/contract-region :which-key "contract region"))

(provide 'init-expand-region)
;;; init-expand-region.el ends here

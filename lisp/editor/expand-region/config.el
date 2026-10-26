;;; editor/expand-region/config.el --- Structural selection helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(require 'aaron-funcs)
(require 'cl-lib)
(require 'seq)

(defvar-local +expand-region-history nil
  "Previous region states for `+expand-region/expand-region'.")

(config-defvar +expand-region-history-limit nil
  "Maximum previous region states kept per buffer."
  :type 'integer
  :group 'editing)

(defconst +selection-definition-node-regexp
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

(defconst +selection-block-node-regexp
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

(defconst +selection-statement-node-regexp
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

(defconst +selection-expression-node-regexp
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

(defun +selection--push-history ()
  "Remember the current region state."
  (when (use-region-p)
    (push (cons (region-beginning) (region-end)) +expand-region-history)
    (when (and (integerp +expand-region-history-limit)
               (> +expand-region-history-limit 0))
      (when-let* ((tail (nthcdr (1- +expand-region-history-limit)
                                +expand-region-history)))
        (setcdr tail nil)))))

(defun +selection--activate-bounds (bounds)
  "Activate region for BOUNDS."
  (unless (and (consp bounds)
               (< (car bounds) (cdr bounds)))
    (user-error "No structural region found here"))
  (+selection--push-history)
  (goto-char (car bounds))
  (push-mark (cdr bounds) t t))

(defun +selection--treesit-available-p ()
  "Return non-nil when tree-sitter APIs are ready for the current buffer."
  (and (fboundp 'treesit-node-at)
       (fboundp 'treesit-ready-p)
       (ignore-errors (treesit-ready-p nil t))))

(defun +selection--node-at-point ()
  "Return the current tree-sitter node around point."
  (when (+selection--treesit-available-p)
    (or (treesit-node-at (point))
        (and (> (point) (point-min))
             (treesit-node-at (1- (point)))))))

(defun +selection--node-bounds (node)
  "Return bounds of tree-sitter NODE."
  (when node
    (cons (treesit-node-start node)
          (treesit-node-end node))))

(defun +selection--ancestor-matching (regexp)
  "Return nearest ancestor node matching REGEXP."
  (when-let* ((node (+selection--node-at-point)))
    (catch 'match
      (while node
        (when (string-match-p regexp (treesit-node-type node))
          (throw 'match node))
        (setq node (treesit-node-parent node))))))

(defun +selection--descendant-matching (node regexp)
  "Return first descendant of NODE matching REGEXP."
  (when node
    (or (when (string-match-p regexp (treesit-node-type node))
          node)
        (let ((count (treesit-node-child-count node))
              found)
          (dotimes (index count found)
            (unless found
              (setq found
                    (+selection--descendant-matching
                     (treesit-node-child node index)
                     regexp))))))))

(defun +selection--matching-delimiter-p (open close)
  "Return non-nil when OPEN and CLOSE are matching delimiters."
  (or (and (eq open ?\{) (eq close ?\}))
      (and (eq open ?\[) (eq close ?\]))
      (and (eq open ?\() (eq close ?\)))))

(defun +selection--trim-bounds (bounds)
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
                 (+selection--matching-delimiter-p
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

(defun +selection--defun-bounds ()
  "Return bounds of the surrounding defun."
  (save-excursion
    (condition-case nil
        (progn
          (end-of-defun)
          (let ((end (point)))
            (beginning-of-defun)
            (cons (point) end)))
      (error nil))))

(defun +selection--defun-body-bounds ()
  "Return bounds of the body inside the surrounding defun."
  (when-let* ((bounds (+selection--defun-bounds)))
    (or (+selection--trim-bounds bounds)
        bounds)))

(defun +selection--line-bounds ()
  "Return trimmed bounds of the current line."
  (let ((bounds (cons (line-beginning-position) (line-end-position))))
    (or (+selection--trim-bounds bounds)
        bounds)))

(defun +selection--list-bounds ()
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

(defun +selection--expression-bounds ()
  "Return a predictable expression-sized region."
  (or (bounds-of-thing-at-point 'sexp)
      (+selection--node-bounds (+selection--expression-node))
      (bounds-of-thing-at-point 'symbol)))

(defun +selection--statement-bounds ()
  "Return a predictable statement-sized region."
  (or (+selection--node-bounds (+selection--statement-node))
      (bounds-of-thing-at-point 'sexp)
      (+selection--line-bounds)))

(defun +selection--block-bounds ()
  "Return a predictable block-sized region."
  (or (+selection--node-bounds (+selection--block-node))
      (+selection--list-bounds)
      (+selection--defun-body-bounds)
      (+selection--defun-bounds)))

(defun +selection--inner-block-bounds ()
  "Return inner bounds of the current block."
  (or (when-let* ((block (+selection--block-bounds)))
        (+selection--trim-bounds block))
      (when-let* ((bounds (+selection--list-bounds)))
        (+selection--trim-bounds bounds))
      (+selection--defun-body-bounds)))

(defun +selection--definition-node ()
  "Return the nearest definition-like node."
  (+selection--ancestor-matching +selection-definition-node-regexp))

(defun +selection--block-node ()
  "Return the nearest block-like node."
  (or (+selection--ancestor-matching +selection-block-node-regexp)
      (when-let* ((definition (+selection--definition-node)))
        (+selection--descendant-matching
         definition
         +selection-block-node-regexp))))

(defun +selection--statement-node ()
  "Return the nearest statement-like node."
  (+selection--ancestor-matching +selection-statement-node-regexp))

(defun +selection--expression-node ()
  "Return the nearest expression-like node."
  (+selection--ancestor-matching +selection-expression-node-regexp))

(defun +selection--interesting-node-p (node)
  "Return non-nil when NODE is useful for region expansion."
  (let ((type (treesit-node-type node)))
    (or (string-match-p +selection-expression-node-regexp type)
        (string-match-p +selection-statement-node-regexp type)
        (string-match-p +selection-block-node-regexp type)
        (string-match-p +selection-definition-node-regexp type))))

(defun +expand-region--tree-bounds ()
  "Return enclosing tree-sitter bounds at point."
  (delq nil
        (list (+selection--expression-bounds)
              (+selection--statement-bounds)
              (+selection--inner-block-bounds)
              (+selection--block-bounds)
              (+selection--defun-body-bounds)
              (+selection--defun-bounds))))

(defun +expand-region--fallback-bounds ()
  "Return generic expansion candidates around point."
  (let (bounds)
    (dolist (thing '(symbol sexp list sentence paragraph defun))
      (when-let* ((b (bounds-of-thing-at-point thing)))
        (push (cons (car b) (cdr b)) bounds)))
    (push (cons (line-beginning-position) (line-end-position)) bounds)
    (push (cons (point-min) (point-max)) bounds)
    bounds))

(defun +expand-region--normalize-candidates (bounds)
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

(defun +expand-region--next-bounds ()
  "Return the next larger candidate bounds."
  (let* ((current (if (use-region-p)
                      (cons (region-beginning) (region-end))
                    (cons (point) (point))))
         (candidates (+expand-region--normalize-candidates
                      (append (+expand-region--tree-bounds)
                              (+expand-region--fallback-bounds)))))
    (seq-find
     (lambda (candidate)
       (or (< (car candidate) (car current))
           (> (cdr candidate) (cdr current))))
     candidates)))

(defun +expand-region/expand-region ()
  "Expand the active region semantically."
  (interactive)
  (when-let* ((next (+expand-region--next-bounds)))
    (+selection--activate-bounds next)))

(defun +expand-region/contract-region ()
  "Contract the region to its previous state."
  (interactive)
  (unless +expand-region-history
    (user-error "No previous region state"))
  (let ((state (pop +expand-region-history)))
    (goto-char (car state))
    (push-mark (cdr state) t t)))

(defun +expand-region/select-defun ()
  "Select the surrounding function-like form.
Prefer Emacs defun navigation so `SPC v f' stays aligned with the
current major mode's notion of a function."
  (interactive)
  (+selection--activate-bounds
   (or (+selection--defun-bounds)
       (+selection--node-bounds (+selection--definition-node))
       (bounds-of-thing-at-point 'defun))))

(defun +expand-region/select-inner-defun ()
  "Select the body of the surrounding function-like form."
  (interactive)
  (+selection--activate-bounds
   (or (+selection--defun-body-bounds)
       (when-let* ((definition (+selection--definition-node))
                   (body (+selection--descendant-matching
                          definition
                          +selection-block-node-regexp)))
         (+selection--trim-bounds (+selection--node-bounds body)))
       (bounds-of-thing-at-point 'defun))))

(defun +expand-region/select-statement ()
  "Select the surrounding statement-like form."
  (interactive)
  (+selection--activate-bounds
   (+selection--statement-bounds)))

(defun +expand-region/select-expression ()
  "Select the surrounding expression-like form."
  (interactive)
  (+selection--activate-bounds
   (or (+selection--expression-bounds)
       (+selection--line-bounds))))

(defun +expand-region/select-block ()
  "Select the surrounding block-like form."
  (interactive)
  (+selection--activate-bounds
   (+selection--block-bounds)))

(defun +expand-region/select-inner-block ()
  "Select the body of the surrounding block."
  (interactive)
  (+selection--activate-bounds
   (or (+selection--inner-block-bounds)
       (+selection--block-bounds))))

(defun +expand-region/select-parent-syntax-node ()
  "Select the next larger structural region."
  (interactive)
  (call-interactively #'+expand-region/expand-region))

(define-leader-key!
  "v"   '(:ignore t :which-key "selection")
  "v v" '(:def +expand-region/expand-region :which-key "expand region")
  "v V" '(:def +expand-region/contract-region :which-key "contract region")
  "v f" '(:def +expand-region/select-defun :which-key "select defun")
  "v F" '(:def +expand-region/select-inner-defun :which-key "select inner defun")
  "v s" '(:def +expand-region/select-statement :which-key "select statement")
  "v e" '(:def +expand-region/select-expression :which-key "select expression")
  "v b" '(:def +expand-region/select-block :which-key "select block")
  "v B" '(:def +expand-region/select-inner-block :which-key "select inner block")
  "v p" '(:def +expand-region/select-parent-syntax-node :which-key "select parent")
  "V" '(:def +expand-region/contract-region :which-key "contract region"))

(provide 'editor/expand-region/config)
;;; editor/expand-region/config.el ends here

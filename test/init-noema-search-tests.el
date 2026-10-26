;;; init-noema-search-tests.el --- Tests for kernel-backed page search -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'noema/search/config)

(defun init-noema-search-tests--hash (&rest pairs)
  "Return a hash table of string keys from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for (key value) on pairs by #'cddr do (puthash key value table))
    table))

(defun init-noema-search-tests--item (title namespace kind &rest more)
  "Return a result item TITLE in NAMESPACE found as KIND, with MORE fields."
  (apply #'init-noema-search-tests--hash
         "id" (concat "id-" title) "title" title "namespace" namespace
         "matchKind" kind "repositoryPath" (concat (downcase title) ".md")
         "file" (concat "/vault/" (downcase title) ".md")
         more))

(defmacro init-noema-search-tests--with-answers (answers &rest body)
  "Run BODY with `+noema-search' answering from ANSWERS, an alist by query.
Every query asked is pushed onto the variable `asked'."
  (declare (indent 1))
  `(let ((asked nil))
     (cl-letf (((symbol-function '+noema-search-available-p) (lambda () t))
               ((symbol-function '+noema-search)
                (lambda (query &rest _options)
                  (push query asked)
                  (cdr (assoc query ,answers)))))
       ,@body)))

(ert-deftest init-noema-search-shows-the-kernel-answer-unfiltered ()
  "A title the input does not spell is still a candidate, in kernel order."
  (let* ((answers
          (list (cons "readme: maintian"
                      (init-noema-search-tests--hash
                       "items" (vector
                                (init-noema-search-tests--item "maintain" "README" "fuzzy")
                                (init-noema-search-tests--item "Writing Good Tests" "README" "corrected"
                                                               "excerpt" "how to [[maintain]] a suite"))
                       "corrected" (init-noema-search-tests--hash "from" "maintian" "to" "maintain")
                       "create" (init-noema-search-tests--hash "title" "maintian" "namespace" "README")))))
         (state (make-hash-table :test #'equal))
         (table (+noema-search--table state)))
    (init-noema-search-tests--with-answers answers
      (let ((completion-styles '(+noema-search))
            (completion-category-overrides nil)
            (completion-category-defaults nil))
        (should (equal (completion-all-completions "readme: maintian" table nil 16)
                       '("README:maintain" "README:Writing Good Tests"
                         "+ Create “maintian” in README")))
        ;; Typing is not rewritten into a candidate.
        (should (equal (completion-try-completion "readme: maintian" table nil 16)
                       '("readme: maintian" . 16)))
        ;; One request per input, however often the UI asks.
        (completion-all-completions "readme: maintian" table nil 16)
        (should (equal asked '("readme: maintian"))))
      (let ((metadata (completion-metadata "" table nil)))
        (should (eq (completion-metadata-get metadata 'category) 'noema-page))
        (should (eq (completion-metadata-get metadata 'display-sort-function) 'identity))
        (let ((group (completion-metadata-get metadata 'group-function))
              (annotate (completion-metadata-get metadata 'annotation-function)))
          (should (equal (funcall group "README:maintain" nil) "Similar title"))
          (should (equal (funcall group "README:Writing Good Tests" nil)
                         "Text, reading it as “maintain”"))
          (should (equal (funcall group "+ Create “maintian” in README" nil) "New page"))
          (let ((detail (funcall annotate "README:Writing Good Tests")))
            (should (string-match-p "writing good tests\\.md" detail))
            (should (string-match-p "how to maintain a suite" detail))
            (should (eq (get-text-property (string-match "maintain" detail) 'face detail)
                        '+noema-search-match)))))
      (should (test-completion "README:maintain" table))
      (should-not (test-completion "README:nothing" table)))))

(ert-deftest init-noema-search-labels-and-choices ()
  "Matched title runes are highlighted; a repeated title is told apart."
  (let* ((first (init-noema-search-tests--item "Tensor" "Math" "title" "titleRanges" (vector (vector 0 3))))
         (label (+noema-search--label first)))
    (should (equal label "Math:Tensor"))
    (should (eq (get-text-property 5 'face label) '+noema-search-match))
    (should-not (get-text-property 0 'face label))
    (should-not (get-text-property 8 'face label)))
  ;; The gateway may hand ranges over as lists; a range outside the label is
  ;; ignored instead of breaking the minibuffer.
  (let ((label (+noema-search--label
                (init-noema-search-tests--item "Tensor" "Math" "title"
                                               "titleRanges" '((0 3) (40 44) (2))))))
    (should (eq (get-text-property 5 'face label) '+noema-search-match))
    (should-not (get-text-property 9 'face label)))
  (let ((answers
         (list (cons "tensor"
                     (init-noema-search-tests--hash
                      "items" (vector (init-noema-search-tests--item "Tensor" "Math" "exact")
                                      (init-noema-search-tests--item "Tensor" "Math" "content"
                                                                     "repositoryPath" "old/tensor.md"))))
               (cons "brand new"
                     (init-noema-search-tests--hash
                      "items" (vector (init-noema-search-tests--item "Other" "Math" "content"))
                      "create" (init-noema-search-tests--hash "title" "Brand New" "namespace" ""))))))
    (init-noema-search-tests--with-answers answers
      (let ((state (make-hash-table :test #'equal)))
        (should (equal (+noema-search--candidates state "tensor")
                       '("Math:Tensor" "Math:Tensor  old/tensor.md"))))
      ;; Confirming a typed title nobody has asks to create it, not to open
      ;; whatever page mentions it.
      (let ((state (make-hash-table :test #'equal)))
        (should (equal (+noema-search--choice state "brand new") '(:create "Brand New" "")))
        (should (equal (gethash "matchKind" (+noema-search--choice state "tensor")) "exact"))
        ;; A chosen candidate is itself.
        (should (equal (gethash "repositoryPath"
                                (+noema-search--choice state "Math:Tensor  old/tensor.md"))
                       "old/tensor.md"))))))

(ert-deftest init-noema-search-a-silent-host-is-asked-again ()
  "No answer is not remembered as an empty answer."
  (let ((state (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function '+noema-search)
               (lambda (&rest _) (cl-incf calls) nil)))
      (should-not (+noema-search--candidates state "x"))
      (should-not (+noema-search--candidates state "x"))
      (should (= calls 2)))))

(ert-deftest init-noema-search-references-carry-the-note-and-the-kernel-text ()
  "A reference lookup sends the note and returns the kernel's link text."
  (let (sent)
    (cl-letf (((symbol-function '+noema-search)
               (lambda (query &rest options)
                 (push (cons query options) sent)
                 (init-noema-search-tests--hash
                  "items" (vector
                           (init-noema-search-tests--hash
                            "kind" "heading" "label" "Lease expiry" "group" "This page"
                            "detail" "##" "level" 2 "fragment" "lease-expiry"
                            "insert" "[Lease expiry](#lease-expiry)" "titleRanges" '((0 5)))
                           (init-noema-search-tests--hash
                            "kind" "heading" "label" "Lease expiry" "group" "This page"
                            "detail" "##" "level" 2 "fragment" "lease-expiry-1"
                            "insert" "[Lease expiry](#lease-expiry-1)")
                           (init-noema-search-tests--hash
                            "kind" "page" "label" "Tensor" "group" "Pages"
                            "detail" "Math · tensor.md"
                            "insert" "[[roam://id-0|Tensor]]"))))))
      (let* ((state (make-hash-table :test #'equal))
             (candidates (+noema-search--reference-candidates
                          state "lease" "/vault/n.md" "## Lease expiry\n")))
        (should (equal candidates
                       '("  Lease expiry" "  Lease expiry  lease-expiry-1" "Tensor")))
        (should (equal (car sent)
                       '("lease" :mode "references" :file "/vault/n.md"
                         :content "## Lease expiry\n" :limit 30)))
        ;; The range is counted before the heading's indentation is added.
        (should (eq (get-text-property 2 'face (car candidates)) '+noema-search-match))
        (should (equal (+noema-search--get (car (gethash (nth 1 candidates) state)) "insert")
                       "[Lease expiry](#lease-expiry-1)"))
        (should (equal (cadr (gethash "Tensor" state)) "Pages"))
        ;; Asked once per input.
        (+noema-search--reference-candidates state "lease" "/vault/n.md" "")
        (should (= (length sent) 1))))))

(ert-deftest init-noema-search-display-text-is-the-persons ()
  "The kernel's label is a default the person may replace."
  (let ((mine "从系综到密度算符"))
    (should (equal (+noema-search--replace-display-text
                    "[[roam://id-1#frag|Density Operator]]" '(19 35) mine)
                   "[[roam://id-1#frag|从系综到密度算符]]"))
    (should (equal (+noema-search--replace-display-text "[Setup](roam://x@setup)" [1 6] mine)
                   "[从系综到密度算符](roam://x@setup)"))
    ;; Offsets count UTF-16 units: the mathematical D is two of them.
    (should (equal (+noema-search--display-text "[[roam://x|𝒟 old]]" '(11 17)) '(11 . 16)))
    (should (equal (+noema-search--replace-display-text "[[roam://x|𝒟 old]]" '(11 17) mine)
                   "[[roam://x|从系综到密度算符]]"))
    ;; Nothing typed keeps the default; no display text, nothing to replace.
    (should (equal (+noema-search--replace-display-text
                    "[[roam://id-1#frag|Density Operator]]" '(19 35) "  ")
                   "[[roam://id-1#frag|Density Operator]]"))
    (should (equal (+noema-search--replace-display-text "[[Brand New]]" nil mine)
                   "[[Brand New]]"))
    (should (equal (+noema-search--with-display-text "[[Brand New]]" nil) "[[Brand New]]"))))

(provide 'init-noema-search-tests)
;;; init-noema-search-tests.el ends here

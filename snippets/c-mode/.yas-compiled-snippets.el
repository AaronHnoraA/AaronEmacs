;;; "Compiled" snippets and support files for `c-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
                     '((".."
                        (progn
                          (progn
                            (when
                                (looking-back "[ 	]+"
                                              (line-beginning-position))
                              (delete-region (match-beginning 0) (match-end 0)))
                            (insert "->")))
                        ".. => -> (eat spaces, no newline)"
                        (not (nth 8 (syntax-ppss))) nil nil
                        "/Users/hc/.emacs.d/snippets/c-mode/dot-dot" nil nil)))


;;; Do not edit! File generated at Fri Feb 13 19:51:59 2026

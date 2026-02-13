;;; "Compiled" snippets and support files for `go-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("test"
                        "// Emacs invoke \"SPC m t t\"\nfunc TestHelloEmpty(t *testing.T) {\n	fmt.Println(\"this is demo\")\n	t.Fatal(\"this is an error\")\n}"
                        "test" t nil nil
                        "/Users/hc/.emacs.d/snippets/go-mode/test" nil "test")
                       ("pac"
                        "package main\nimport (\n    \"fmt\"\n)\nfunc main() {\n	fmt.Println(\"hello\")$0\n}"
                        "package" t nil nil
                        "/Users/hc/.emacs.d/snippets/go-mode/package" nil
                        "package-go")))


;;; Do not edit! File generated at Fri Feb 13 19:51:59 2026

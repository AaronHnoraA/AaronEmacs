;;; init-cpp-sibling-tests.el --- C/C++ sibling files -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run after loading the full configuration:
;;   emacs --batch --init-directory=. -q -l early-init.el -l init.el \
;;     -l test/init-cpp-sibling-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'lang/cpp/config)

(ert-deftest +cpp-find-sibling-file-pairs-source-and-header ()
  "A source file finds its header and back, beside the test siblings."
  (let ((dir (file-name-as-directory (make-temp-file "sibling-" t))))
    (unwind-protect
        (cl-flet ((siblings (name)
                    (sort (mapcar #'file-name-nondirectory
                                  (find-sibling-file-search (concat dir name)))
                          #'string<)))
          (dolist (name '("foo.cpp" "foo.hpp" "foo_test.cpp"
                          "bar.c" "bar.h" "bar.h.orig"))
            (write-region "" nil (concat dir name) nil 'silent))
          (should (equal (siblings "bar.c") '("bar.h")))
          (should (equal (siblings "bar.h") '("bar.c")))
          (should (equal (siblings "foo.cpp") '("foo.hpp" "foo_test.cpp")))
          (should (equal (siblings "foo.hpp") '("foo.cpp" "foo_test.cpp"))))
      (delete-directory dir t))))

;;; init-cpp-sibling-tests.el ends here

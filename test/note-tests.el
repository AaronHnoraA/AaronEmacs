;;; note-tests.el --- Tests for Typst note helpers -*- lexical-binding: t -*-

(require 'ert)
(require 'init-auto-insert)
(require 'init-note)
(require 'init-note-tools)

(ert-deftest my/note-parses-metadata-and-links ()
  (with-temp-buffer
    (insert "#metadata((\n"
            "  kind: \"note\",\n"
            "  id: \"n-1\",\n"
            "  title: \"First Note\",\n"
            "  date: \"2026-05-11\",\n"
            "  tags: (\"math\", \"draft\"),\n"
            "  aliases: (\"one\"),\n"
            ")) <note>\n\n"
            "#note(\"n-2\")[Second]\n")
    (let ((note (my/note-parse-current-buffer "/tmp/first.typ")))
      (should (equal (plist-get note :id) "n-1"))
      (should (equal (plist-get note :title) "First Note"))
      (should (equal (plist-get note :tags) '("math" "draft")))
      (should (equal (plist-get note :aliases) '("one")))
      (should (equal (plist-get (car (plist-get note :links)) :target) "n-2"))
      (should (equal (plist-get (car (plist-get note :links)) :label) "Second")))))

(ert-deftest my/note-syncs-database ()
  (skip-unless (my/note--sqlite-available-p))
  (let* ((root (make-temp-file "note-root-" t))
         (db-dir (make-temp-file "note-db-" t))
         (my/note-root root)
         (my/note-db-file (expand-file-name "note.db" db-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.typ" root)
            (insert "#metadata((id: \"a\", title: \"A\", tags: (\"x\"), aliases: ())) <note>\n"
                    "#note(\"b\")[B]\n"))
          (with-temp-file (expand-file-name "b.typ" root)
            (insert "#metadata((id: \"b\", title: \"B\", tags: (), aliases: ())) <note>\n"))
          (should (= (my/note-db-sync) 2))
          (should (equal (my/note--rows
                          "select title from nodes where id = ?"
                          ["a"])
                         '(("A"))))
          (should (equal (my/note--rows
                          "select source_id from links where target_id = ?"
                          ["b"])
                         '(("a"))))
          (should (file-exists-p
                   (expand-file-name "_typst/notes/a.typ" root)))
          (should (file-exists-p
                   (expand-file-name "_typst/notes/b.typ" root))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory db-dir t)))))

(ert-deftest my/note-helper-renders-styled-plain-text ()
  (let ((source (my/note--helper-source-for-notes
                 (list (list :id "a" :file "/tmp/note-root/roam/a.typ")))))
    (should (string-match-p "#let note-theme" source))
    (should (string-match-p "#let note-entry" source))
    (should (string-match-p "#let note-path" source))
    (should (string-match-p "#let note-include" source))
    (should (string-match-p "note-include-active\\.update(true)" source))
    (should (string-match-p "set page" source))
    (should (string-match-p "set text" source))
    (should (string-match-p "show math\\.equation" source))
    (should (string-match-p "show: note-theme" source))
    (should (string-match-p "outline(title: \\[目录\\]" source))
    (should (string-match-p "#let note-card" source))
    (should (string-match-p "#let definition" source))
    (should (string-match-p "underline(pos\\.at(1))" source))
    (should-not (string-match-p "link(" source))
    (should (string-match-p "/_typst/notes/\" \\+ id \\+ \"\\.typ" source))
    (should-not (string-match-p "/roam/a.typ" source))))

(ert-deftest my/note-writes-id-wrapper-files ()
  (let* ((root (make-temp-file "note-root-" t))
         (my/note-root root))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "roam" root))
          (let ((target (expand-file-name "roam/a.typ" root)))
            (with-temp-file target
              (insert "#let exported = 1\nBody\n"))
            (my/note-write-wrapper-files
             (list (list :id "a" :file target)))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "_typst/notes/a.typ" root))
                       (buffer-string))
                     "#import \"/roam/a.typ\": *\n#include \"/roam/a.typ\"\n"))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest my/note-detects-link-id-at-point ()
  (with-temp-buffer
    (insert "See #note(\"target-note\")[Target Note] for details.\n")
    (goto-char (point-min))
    (search-forward "target-note")
    (should (equal (my/note--link-id-at-point) "target-note"))
    (search-forward "Target Note")
    (should (equal (my/note--link-id-at-point) "target-note"))))

(ert-deftest my/note-detects-typst-reference-at-point ()
  (with-temp-buffer
    (insert "plain text @local-section more text\n")
    (goto-char (point-min))
    (search-forward "plain")
    (should-not (my/note--typst-reference-at-point-p))
    (search-forward "local")
    (should (my/note--typst-reference-at-point-p))))

(ert-deftest my/note-escapes-inserted-link-content ()
  (should (equal (my/note--typst-content-escape "A [B] \\ C")
                 "A \\[B\\] \\\\ C")))

(ert-deftest my/note-zotero-fills-placeholders-from-bibtex ()
  (with-temp-buffer
    (insert "Title: ${title}\n"
            "Author: ${author}\n"
            "Year: ${year}\n"
            "Key: ${citekey}\n"
            "DOI: ${doi}\n\n"
            "@article{sample2026,\n"
            "  title = {Sample Paper},\n"
            "  author = {Ada Lovelace},\n"
            "  year = {2026},\n"
            "  doi = {10.0000/example}\n"
            "}\n")
    (my/note-zotero-fill-metadata)
    (should (string-match-p "Title: Sample Paper" (buffer-string)))
    (should (string-match-p "Author: Ada Lovelace" (buffer-string)))
    (should (string-match-p "Year: 2026" (buffer-string)))
    (should (string-match-p "Key: sample2026" (buffer-string)))
    (should (string-match-p "DOI: 10.0000/example" (buffer-string)))))

(ert-deftest my/template-typst-assignment-links-project-styles ()
  (let ((root (make-temp-file "typst-assignment-" t))
        (user-emacs-directory (file-name-as-directory
                               (expand-file-name user-emacs-directory))))
    (unwind-protect
        (let ((file (expand-file-name "assg.typ" root)))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (let ((default-directory root))
                  (my/template--ensure-typst-style-links "assignment.typ")
                  (should (file-symlink-p (expand-file-name "_typst/assignment.typ" root)))
                  (should (file-symlink-p (expand-file-name "_typst/note.typ" root))))
              (kill-buffer (current-buffer)))))
      (ignore-errors (delete-directory root t)))))

(provide 'note-tests)
;;; note-tests.el ends here

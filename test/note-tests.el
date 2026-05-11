;;; note-tests.el --- Tests for Typst note helpers -*- lexical-binding: t -*-

(require 'ert)
(require 'init-note)

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
                         '(("a")))))
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
    (should (string-match-p "/roam/a.typ" source))))

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

(provide 'note-tests)
;;; note-tests.el ends here

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
                    "#note(\"b\")[B]\n"
                    "Body line\n"))
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
          (should (equal (my/note--rows
                          "select summary from nodes where id = ?"
                          ["a"])
                         '(("Body line"))))
          (should (= (my/note-db-sync) 2))
          (should (file-exists-p
                   (expand-file-name "_typst/notes/a.typ" root)))
          (should (file-exists-p
                   (expand-file-name "_typst/notes/b.typ" root))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory db-dir t)))))

(ert-deftest my/note-db-schema-has-versioned-summary-and-size ()
  (skip-unless (my/note--sqlite-available-p))
  (let* ((root (make-temp-file "note-root-" t))
         (db-dir (make-temp-file "note-db-" t))
         (my/note-root root)
         (my/note-db-file (expand-file-name "note.db" db-dir)))
    (unwind-protect
        (let ((db (my/note--db-open)))
          (unwind-protect
              (progn
                (my/note--db-init db)
                (should (= (my/note--db-version db) my/note--schema-version))
                (should (my/note--db-column-exists-p db "nodes" "summary"))
                (should (my/note--db-column-exists-p db "files" "size")))
            (sqlite-close db)))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory db-dir t)))))

(ert-deftest my/note-sync-runs-hooks ()
  (skip-unless (my/note--sqlite-available-p))
  (let* ((root (make-temp-file "note-root-" t))
         (db-dir (make-temp-file "note-db-" t))
         (my/note-root root)
         (my/note-db-file (expand-file-name "note.db" db-dir))
         before-ran
         after-args
         (my/note-before-sync-hook (list (lambda () (setq before-ran t))))
         (my/note-after-sync-hook
          (list (lambda (changed removed kept)
                  (setq after-args (list changed removed kept))))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.typ" root)
            (insert "#metadata((id: \"a\", title: \"A\", date: \"2026-05-12\", tags: (), aliases: ())) <note>\n"))
          (should (= (my/note-db-sync) 1))
          (should before-ran)
          (should (equal after-args '(1 0 0))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory db-dir t)))))

(ert-deftest my/note-doctor-detects-common-issues ()
  (skip-unless (my/note--sqlite-available-p))
  (let* ((root (make-temp-file "note-root-" t))
         (db-dir (make-temp-file "note-db-" t))
         (my/note-root root)
         (my/note-db-file (expand-file-name "note.db" db-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.typ" root)
            (insert "#metadata((id: \"a\", title: \"A\", date: \"2026-05-12\", tags: (), aliases: (\"same\"))) <note>\n"
                    "#note(\"b\")[B]\n"
                    "#note(\"missing\")[Missing]\n"))
          (with-temp-file (expand-file-name "b.typ" root)
            (insert "#metadata((id: \"b\", title: \"B\", tags: (), aliases: (\"same\"))) <note>\n"))
          (with-temp-file (expand-file-name "c.typ" root)
            (insert "#metadata((id: \"c\", title: \"C\", date: \"2026-05-12\", tags: (), aliases: ())) <note>\n"))
          (should (= (my/note-db-sync) 3))
          (let ((report (my/note-doctor--collect)))
            (should (= (length (plist-get report :dangling-links)) 1))
            (should (= (length (plist-get report :missing-date)) 1))
            (should (= (length (plist-get report :orphans)) 1))
            (should (= (length (plist-get report :alias-collisions)) 2))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory db-dir t)))))

(ert-deftest my/note-helper-renders-styled-plain-text ()
  (let ((source (my/note--helper-source-for-notes
                 (list (list :id "a" :file "/tmp/note-root/roam/a.typ")))))
    (should (string-match-p "#let note-theme" source))
    (should (string-match-p "#let note-entry" source))
    (should (string-match-p "#let note-entry-with" source))
    (should (string-match-p "#let note-path" source))
    (should (string-match-p "#let note-include" source))
    (should (string-match-p "note-include-active\\.update(true)" source))
    (should (string-match-p "set page" source))
    (should (string-match-p "set text" source))
    (should (string-match-p "show math\\.equation" source))
    (should (string-match-p "show: theme" source))
    (should (string-match-p "toc-title: \\[目录\\]" source))
    (should (string-match-p "outline(title: toc-title, depth: toc-depth)" source))
    (should (string-match-p "#let note-card" source))
    (should (string-match-p "#let zoterolink" source))
    (should (string-match-p "#let bib" source))
    (should (string-match-p "#let definition" source))
	    (should (string-match-p "underline(pos\\.at(1))" source))
	    (should (string-match-p "/_typst/notes/\" \\+ id \\+ \"\\.typ" source))
	    (should-not (string-match-p "/roam/a.typ" source))))

(ert-deftest my/note-writes-helper-modules ()
  (let ((root (make-temp-file "note-root-" t)))
    (unwind-protect
        (let ((my/note-root root))
          (my/note-write-helper-file nil)
          (should (file-exists-p (expand-file-name "_typst/note.typ" root)))
          (should (file-exists-p (expand-file-name "_typst/math.typ" root)))
          (should (file-exists-p (expand-file-name "_typst/extension.typ" root)))
          (should (file-symlink-p (expand-file-name "_typst/note.typ" root)))
          (should (file-symlink-p (expand-file-name "_typst/math.typ" root)))
          (should (file-symlink-p (expand-file-name "_typst/extension.typ" root)))
          (should (file-symlink-p (expand-file-name "_typst/publish.typ" root)))
          (should (file-symlink-p (expand-file-name "css/retro.css" root)))
          (should (file-symlink-p (expand-file-name "js/graph.js" root)))
          (should (file-symlink-p (expand-file-name "homepage.html" root)))
          (should (string-match-p
                   "@preview/pinit"
                   (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "_typst/extension.typ" root))
                     (buffer-string))))
          (should (string-match-p
                   "#let ket"
                   (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "_typst/math.typ" root))
                     (buffer-string)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest my/note-buffer-setup-ensures-helper-modules ()
  (let ((root (make-temp-file "note-root-" t)))
    (unwind-protect
        (let ((my/note-root root))
          (make-directory (expand-file-name "roam" root))
          (let ((file (expand-file-name "roam/a.typ" root)))
            (with-temp-file file
              (insert "#import \"/_typst/note.typ\": *\n"))
            (with-temp-buffer
              (setq buffer-file-name file)
              (my/note-buffer-setup)))
          (should (file-exists-p (expand-file-name "_typst/note.typ" root)))
          (should (file-exists-p (expand-file-name "_typst/math.typ" root)))
          (should (file-exists-p (expand-file-name "_typst/extension.typ" root)))
          (should (file-symlink-p (expand-file-name "_typst/publish.typ" root)))
          (should (file-symlink-p (expand-file-name "css/retro.css" root)))
          (should (file-symlink-p (expand-file-name "js/graph.js" root))))
      (ignore-errors (delete-directory root t)))))

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

(ert-deftest my/note-new-of-type-creates-literature-note ()
  (skip-unless (my/note--sqlite-available-p))
  (let* ((root (make-temp-file "note-root-" t))
         (db-dir (make-temp-file "note-db-" t))
         (my/note-root root)
         (my/note-directory (expand-file-name "roam/" root))
         (my/note-db-file (expand-file-name "note.db" db-dir))
         (file nil))
    (unwind-protect
        (progn
          (setq file (my/note-new-of-type
                      (my/note--type-by-name 'literature)
                      "Graph Paper"))
          (should (file-exists-p file))
          (should (string-match-p "/roam/lit-graph-paper\\.typ\\'" file))
          (should (equal (my/note--rows
                          "select title from nodes where file = ?"
                          (vector (file-truename file)))
                         '(("Graph Paper"))))
          (should (equal (my/note--rows
                          "select tag from tags where node_id = (select node_id from files where path = ?)"
                          (vector (file-truename file)))
                         '(("literature"))))
          (should (string-match-p
                   "== Reference"
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))))
      (when file
        (ignore-errors
          (when-let* ((buffer (get-file-buffer file)))
            (kill-buffer buffer))))
      (ignore-errors (delete-directory root t))
      (ignore-errors (delete-directory db-dir t)))))

(ert-deftest my/note-detects-link-id-at-point ()
  (with-temp-buffer
    (insert "See #note(\"target-note\")[Target Note] for details.\n")
    (goto-char (point-min))
    (search-forward "target-note")
    (should (equal (my/note--link-id-at-point) "target-note"))
    (search-forward "Target Note")
    (should (equal (my/note--link-id-at-point) "target-note"))))

(ert-deftest my/note-detects-zotero-link-at-point ()
  (with-temp-buffer
    (insert "See #zoterolink(\"zotero://select/items/1_BJ9K8NAX\")[Paper] for details.\n")
    (goto-char (point-min))
    (search-forward "BJ9K8NAX")
    (should (equal (my/note--zotero-url-at-point)
                   "zotero://select/items/1_BJ9K8NAX"))
    (search-forward "Paper")
    (should (equal (my/note--zotero-url-at-point)
                   "zotero://select/items/1_BJ9K8NAX"))))

(ert-deftest my/note-open-or-preview-sync-prefers-zotero-link ()
  (with-temp-buffer
    (insert "#zoterolink(\"zotero://select/items/1_BJ9K8NAX\")[Paper]\n")
    (goto-char (point-min))
    (search-forward "Paper")
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buffer program &rest args)
                 (list :program program :args args))))
      (should (equal (my/note-open-or-preview-sync)
                     "Opening Zotero link: zotero://select/items/1_BJ9K8NAX")))))

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
                  (should (file-symlink-p (expand-file-name "_typst/note.typ" root)))
                  (should (file-symlink-p (expand-file-name "_typst/extension.typ" root))))
              (kill-buffer (current-buffer)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest my/template-typst-slide-links-beamer-style ()
  (let ((root (make-temp-file "typst-slide-" t))
        (user-emacs-directory (file-name-as-directory
                               (expand-file-name user-emacs-directory))))
    (unwind-protect
        (let ((file (expand-file-name "slides.typ" root)))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (let ((default-directory root))
                  (my/template--ensure-typst-style-links "slide.typ")
                  (should (file-symlink-p (expand-file-name "_typst/beamer.typ" root)))
                  (should (file-symlink-p (expand-file-name "_typst/note.typ" root)))
                  (should (file-symlink-p (expand-file-name "_typst/extension.typ" root))))
              (kill-buffer (current-buffer)))))
      (ignore-errors (delete-directory root t)))))

(provide 'note-tests)
;;; note-tests.el ends here

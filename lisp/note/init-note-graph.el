;;; init-note-graph.el --- Interactive Typst note graph -*- lexical-binding: t; -*-

;;; Commentary:
;; Generate local graph data for the shared published D3 graph frontend.

;;; Code:

(require 'browse-url)
(require 'init-note)
(require 'json)
(require 'subr-x)
(require 'url-util)

(defcustom my/note-graph-file
  (expand-file-name
   "note-graph.html"
   (if (boundp 'my/state-dir)
       (expand-file-name "note" my/state-dir)
     (expand-file-name "var/note" user-emacs-directory)))
  "Generated HTML file used by `my/note-graph'."
  :type 'file
  :group 'my/note)

(defcustom my/note-graph-asset-directory
  (locate-user-emacs-file "lisp/note/assets")
  "Directory containing symlinked shared site graph assets."
  :type 'directory
  :group 'my/note)

(defun my/note-graph--asset-file (relative)
  "Return graph asset RELATIVE below `my/note-graph-asset-directory'."
  (expand-file-name relative
                    (file-name-as-directory my/note-graph-asset-directory)))

(defun my/note-graph--asset-url (relative)
  "Return file URL for graph asset RELATIVE."
  (url-encode-url
   (concat "file://" (expand-file-name (my/note-graph--asset-file relative)))))

(defun my/note-graph--file-url (file)
  "Return a browser URL for local FILE."
  (url-encode-url (concat "file://" (file-truename file))))

(defun my/note-graph--group-key (file)
  "Return the graph group key for note FILE."
  (let ((relative (file-relative-name
                   (file-truename file)
                   (file-name-as-directory (file-truename my/note-root)))))
    (or (directory-file-name (file-name-directory relative))
        "Root")))

(defun my/note-graph--group-label (group-key)
  "Return display label for GROUP-KEY."
  (if (or (null group-key) (string= group-key "Root"))
      "Root"
    (let* ((parts (split-string group-key "/" t))
           (leaf (car (last parts))))
      (if (string-match-p "\\`[A-Z0-9-]+\\'" leaf)
          leaf
        (mapconcat #'capitalize (split-string leaf "[-_]" t) " ")))))

(defun my/note-graph--section-name (group-key)
  "Return top-level graph section for GROUP-KEY."
  (if (or (null group-key) (string= group-key "Root"))
      "Root"
    (car (split-string group-key "/" t))))

(defun my/note-graph--summary-from-file (file)
  "Return a compact summary extracted from Typst FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (lines)
      (while (and (not (eobp)) (< (length lines) 3))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line)
                      (string-prefix-p "=" line)
                      (string-prefix-p "//" line))
            (push line lines)))
        (forward-line 1))
      (truncate-string-to-width
       (replace-regexp-in-string
        "[ \t\n\r]+" " "
        (string-trim (mapconcat #'identity (nreverse lines) " ")))
       180 nil nil t))))

(defun my/note-graph--site-data ()
  "Return note index data using the published site's SITE_DATA schema."
  (let* ((node-rows
          (my/note--rows
           "select n.id,
                   n.file,
                   n.title,
                   coalesce(n.date, ''),
                   coalesce((select group_concat(tag, ',')
                             from tags where node_id = n.id), '')
            from nodes n
            order by lower(n.title), n.file"))
         (link-rows
          (my/note--rows
           "select source_id, target_id from links order by source_id, target_id"))
         (refs-by-source (make-hash-table :test 'equal))
         (backlinks-by-target (make-hash-table :test 'equal))
         (tag-set (make-hash-table :test 'equal)))
    (dolist (row link-rows)
      (pcase-let ((`(,source ,target) row))
        (unless (string= source target)
          (puthash source
                   (cons target (gethash source refs-by-source))
                   refs-by-source)
          (puthash target
                   (cons source (gethash target backlinks-by-target))
                   backlinks-by-target))))
    (let ((notes
           (mapcar
            (lambda (row)
              (pcase-let* ((`(,id ,file ,title ,date ,tags-raw) row)
                           (tags (if (and tags-raw
                                          (not (string-empty-p tags-raw)))
                                     (sort (split-string tags-raw "," t)
                                           #'string<)
                                   nil))
                           (group-key (my/note-graph--group-key file)))
                (dolist (tag tags)
                  (puthash tag t tag-set))
                `(("key" . ,id)
                  ("id" . ,id)
                  ("title" . ,title)
                  ("link" . ,(my/note-graph--file-url file))
                  ("date" . ,date)
                  ("summary" . ,(my/note-graph--summary-from-file file))
                  ("groupKey" . ,group-key)
                  ("groupLabel" . ,(my/note-graph--group-label group-key))
                  ("section" . ,(my/note-graph--section-name group-key))
                  ("hidden" . :json-false)
                  ("tags" . ,(vconcat tags))
                  ("refs" . ,(vconcat (sort (delete-dups
                                             (gethash id refs-by-source))
                                            #'string<)))
                  ("backlinks" . ,(vconcat (sort (delete-dups
                                                  (gethash id backlinks-by-target))
                                                 #'string<))))))
            node-rows)))
      `(("meta" . (("generatedAt" . ,(format-time-string "%Y-%m-%d %H:%M:%S %z"))
                   ("noteCount" . ,(length notes))
                   ("tagCount" . ,(hash-table-count tag-set))))
        ("notes" . ,(vconcat notes))))))

(defun my/note-graph--write-site-data (directory)
  "Write note graph SITE_DATA into DIRECTORY/js/data.js."
  (let ((file (expand-file-name "js/data.js" directory))
        (json-encoding-pretty-print nil))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "const SITE_DATA = ")
      (insert (json-encode (my/note-graph--site-data)))
      (insert ";\n"))
    file))

(defun my/note-graph--html ()
  "Return standalone HTML using the shared published graph frontend."
  (format
   "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
  <title>Note Graph</title>
  <link rel=\"stylesheet\" href=\"%s\" />
  <script src=\"https://d3js.org/d3.v7.min.js\"></script>
</head>
<body class=\"site-home site-archive\">
  <div class=\"site-shell\">
    <main class=\"site-main\">
      <section class=\"section section-wide\">
        <h1>Note Graph</h1>
        <p class=\"section-aside\">Local Typst notes rendered through the shared published graph.</p>
        <div id=\"graph-container\" aria-label=\"Interactive knowledge graph\"></div>
        <div id=\"graph-focus\" class=\"graph-focus empty\" aria-live=\"polite\"></div>
      </section>
    </main>
  </div>
  <script src=\"js/data.js\"></script>
  <script src=\"%s\"></script>
  <script src=\"%s\"></script>
</body>
</html>
"
   (my/note-graph--asset-url "css/retro.css")
   (my/note-graph--asset-url "js/knowledge.js")
   (my/note-graph--asset-url "js/graph.js")))

;;;###autoload
(defun my/note-graph ()
  "Generate and open the shared-site interactive note graph."
  (interactive)
  (my/note--ensure-db)
  (let ((directory (file-name-directory my/note-graph-file)))
    (make-directory directory t)
    (my/note-graph--write-site-data directory))
  (with-temp-file my/note-graph-file
    (insert (my/note-graph--html)))
  (browse-url-of-file my/note-graph-file)
  (message "Note graph written: %s" my/note-graph-file))

(provide 'init-note-graph)
;;; init-note-graph.el ends here

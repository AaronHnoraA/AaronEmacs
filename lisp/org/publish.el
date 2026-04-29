;;; publish.el --- org-publish config -*- lexical-binding: t; -*-

(require 'json)
(require 'ox-html)
(require 'ox-latex)
(require 'ox-publish)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defconst my/site-asset-version "20260426-19")
(defconst my/site-roam-directory
  (expand-file-name "roam/" (file-name-directory load-file-name)))
(defconst my/site-roam-html-asset-directory "assets")
(defconst my/site-roam-image-extensions
  '("png" "jpg" "jpeg" "gif" "svg" "webp" "avif"))
(defconst my/site-org-file-extensions '("org" "org.gpg")
  "File extensions that should be published as HTML instead of assets.")
(defconst my/site-timestamp-directory
  (expand-file-name ".cache/org-timestamps/" (file-name-directory load-file-name)))
(defconst my/site-org-id-locations-file
  (expand-file-name ".cache/org-id-locations" (file-name-directory load-file-name)))
(defvar my/site-id-link-table nil
  "Hash table mapping uppercase Org IDs to source files.")
(defvar my/site-publish-root nil
  "Current org-publish source root.")

(defun my/site-export-roam-source-p (info)
  "Return non-nil when export INFO belongs to an org-roam source file."
  (my/site-file-in-directory-p (my/site-export-source-file info)
                               my/site-roam-directory))

(defun my/site-managed-special-block-p (block)
  "Return non-nil when BLOCK is generated for local display only."
  (let ((type (downcase (or (org-element-property :type block) "")))
        (parameters (or (org-element-property :parameters block) "")))
    (or (string= type "toc")
        (and (string= type "overview")
             (string-match-p
              "\\(?:\\`\\|[[:space:]]\\):toc\\(?:[[:space:]]\\|\\'\\)"
              parameters)))))

(defun my/site-remove-managed-blocks (tree backend info)
  "Remove local-only managed blocks from HTML export TREE."
  (when (and (org-export-derived-backend-p backend 'html)
             (my/site-export-roam-source-p info))
    (org-element-map tree 'special-block
      (lambda (block)
        (when (my/site-managed-special-block-p block)
          (org-element-extract-element block)))))
  tree)

(defun my/site-display-latex-special-block-p (block)
  "Return non-nil when BLOCK is a display_latex wrapper."
  (string= (downcase (or (org-element-property :type block) ""))
           "display_latex"))

(defun my/site-html-special-block-a (orig special-block contents info)
  "Export display_latex SPECIAL-BLOCK as CONTENTS, otherwise call ORIG.
The block is an editing and preview marker only; publishing should let the
inner LaTeX fragments export exactly as if the wrapper did not exist."
  (if (my/site-display-latex-special-block-p special-block)
      (or contents "")
    (funcall orig special-block contents info)))

(defun my/site-latex-special-block-a (orig special-block contents info)
  "Export display_latex SPECIAL-BLOCK as CONTENTS, otherwise call ORIG."
  (if (my/site-display-latex-special-block-p special-block)
      (or contents "")
    (funcall orig special-block contents info)))

(defun my/site-id-property-paragraph-p (paragraph)
  "Return non-nil when PARAGRAPH is a leaked file-level ID drawer line."
  (let ((text (string-trim
               (org-element-interpret-data
                (org-element-contents paragraph)))))
    (string-match-p
     "\\`:ID:[ \t]+[[:alnum:]-]+\\'"
     text)))

(defun my/site-remove-leaked-id-paragraphs (tree backend info)
  "Remove visible ID drawer residue from included roam files in TREE.
File-level property drawers in included Org files can degrade into a plain
`:ID:' paragraph.  The heading anchors and `id:' links are still exported
normally, so removing that paragraph only hides local Org metadata."
  (when (and (org-export-derived-backend-p backend 'html)
             (my/site-export-roam-source-p info))
    (org-element-map tree 'paragraph
      (lambda (paragraph)
        (when (my/site-id-property-paragraph-p paragraph)
          (org-element-extract-element paragraph)))))
  tree)

(defun my/site-file-in-directory-p (file directory)
  "Return non-nil when FILE is inside DIRECTORY."
  (and file directory
       (file-exists-p file)
       (file-directory-p directory)
       (file-in-directory-p (file-truename file) (file-truename directory))))

(defun my/site-image-file-p (file)
  "Return non-nil when FILE should be treated as a publishable image."
  (member (downcase (or (file-name-extension file) ""))
          my/site-roam-image-extensions))

(defun my/site-org-source-file-p (file)
  "Return non-nil when FILE is an Org source file."
  (let ((name (downcase (or (file-name-nondirectory file) ""))))
    (seq-some (lambda (extension)
                (string-suffix-p (concat "." extension) name))
              my/site-org-file-extensions)))

(defun my/site-normalize-id (id)
  "Return canonical lookup form for Org ID string ID."
  (upcase (string-trim (or id ""))))

(defun my/site-collect-id-links (root)
  "Return a hash table of Org IDs found below ROOT."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (file (directory-files-recursively root "\\.org\\'"))
      (unless (string-match-p "\\`\\(?:public/\\|ltximg/\\|CV/.+\\.org\\'\\)"
                              (file-relative-name file root))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:ID:[ \t]*\\(.+\\)$" nil t)
            (let ((id (my/site-normalize-id (match-string 1))))
              (unless (string-empty-p id)
                (puthash id file table)))))))
    table))

(defun my/site-id-link-table (info)
  "Return the ID lookup table for export INFO."
  (let* ((root (file-truename
                (or my/site-publish-root
                    (plist-get info :base-directory)
                    (file-name-directory load-file-name)))))
    (unless (and my/site-id-link-table
                 (equal (gethash :root my/site-id-link-table) root))
      (setq my/site-id-link-table (my/site-collect-id-links root))
      (puthash :root root my/site-id-link-table))
    my/site-id-link-table))

(defun my/site-file-html-link (file info)
  "Return site-relative HTML link for Org FILE from export INFO."
  (let* ((source-file (my/site-export-source-file info))
         (output-dir (my/site-export-output-directory info))
         (root (file-truename
                (or my/site-publish-root
                    (plist-get info :base-directory)
                    (file-name-directory load-file-name))))
         (rel (file-relative-name file root))
         (html-file (expand-file-name
                     (concat (file-name-sans-extension rel) ".html")
                     (file-truename (plist-get info :publishing-directory)))))
    (if (and source-file (file-equal-p file source-file))
        ""
      (file-relative-name html-file output-dir))))

(defun my/site-current-file-ids (info)
  "Return normalized Org IDs from the current export source file."
  (let ((source-file (my/site-export-source-file info))
        ids)
    (when (and source-file (file-regular-p source-file))
      (with-temp-buffer
        (insert-file-contents source-file)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*:ID:[ \t]*\\(.+\\)$" nil t)
          (let ((id (my/site-normalize-id (match-string 1))))
            (unless (string-empty-p id)
              (push id ids))))))
    (delete-dups (nreverse ids))))

(defun my/site-file-keyword (file keyword)
  "Return FILE keyword value for KEYWORD, or nil."
  (when (and file (file-regular-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (let ((case-fold-search t))
        (goto-char (point-min))
        (when (re-search-forward
               (format "^[ \t]*#\\+%s:[ \t]*\\(.+\\)$"
                       (regexp-quote keyword))
               nil t)
          (string-trim (match-string 1)))))))

(defun my/site-clean-date-keyword (date)
  "Return a compact readable date string from Org DATE keyword."
  (let ((text (string-trim (or date ""))))
    (cond
     ((string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" text)
      (match-string 1 text))
     ((string-empty-p text) "Undated")
     (t text))))

(defun my/site-export-source-file (info)
  "Return the source Org file from export INFO."
  (or (plist-get info :input-file)
      (buffer-file-name (buffer-base-buffer))))

(defun my/site-export-output-directory (info)
  "Return output directory for export INFO."
  (file-name-directory
   (expand-file-name
    (or (plist-get info :output-file)
        (org-export-output-file-name ".html" nil)))))

(defun my/site-resolve-file-link (path info)
  "Resolve local file link PATH against source file in export INFO."
  (let ((expanded (substitute-in-file-name path)))
    (if (file-name-absolute-p expanded)
        (expand-file-name expanded)
      (expand-file-name
       expanded
       (file-name-directory
        (or (my/site-export-source-file info)
            (expand-file-name "publish.org" default-directory)))))))

(defun my/site-asset-relative-path (source-file asset-file info)
  "Return a stable copied asset path for ASSET-FILE linked from SOURCE-FILE."
  (let* ((source-dir (and source-file (file-name-directory source-file)))
         (root (file-truename
                (or my/site-publish-root
                    (plist-get info :base-directory)
                    (file-name-directory load-file-name))))
         (local-rel (and source-dir (file-relative-name asset-file source-dir))))
    (if (and local-rel
             (not (string-prefix-p "../" local-rel))
             (not (string= local-rel "..")))
        local-rel
      (file-relative-name asset-file root))))

(defun my/site-copy-local-asset (asset-file info)
  "Copy local ASSET-FILE for HTML export described by INFO."
  (let* ((source-file (my/site-export-source-file info))
         (output-dir (my/site-export-output-directory info))
         (asset-rel (my/site-asset-relative-path source-file asset-file info))
         (target-file (expand-file-name
                       asset-rel
                       (expand-file-name my/site-roam-html-asset-directory
                                         output-dir))))
    (make-directory (file-name-directory target-file) t)
    (unless (and (file-exists-p target-file)
                 (or (file-equal-p asset-file target-file)
                     (let ((source-attrs (file-attributes asset-file))
                           (target-attrs (file-attributes target-file)))
                       (and (= (file-attribute-size source-attrs)
                               (file-attribute-size target-attrs))
                            (equal (file-attribute-modification-time source-attrs)
                                   (file-attribute-modification-time target-attrs))))))
      (copy-file asset-file target-file t t))
    (file-relative-name target-file output-dir)))

(defun my/site-copy-local-file-links (tree backend info)
  "Copy local file links and rewrite them for HTML export TREE."
  (when (org-export-derived-backend-p backend 'html)
    (org-element-map tree 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (let* ((path (org-element-property :path link))
                 (asset-file (and path (my/site-resolve-file-link path info)))
                 (root (file-truename
                        (or my/site-publish-root
                            (plist-get info :base-directory)
                            (file-name-directory load-file-name)))))
            (when (and asset-file
                       (file-regular-p asset-file)
                       (not (my/site-org-source-file-p asset-file))
                       (my/site-file-in-directory-p asset-file root))
              (org-element-put-property
               link :path
               (my/site-copy-local-asset asset-file info))))))))
  tree)

(defun my/site-html-link-a (orig link desc info)
  "Render Org links with site-specific ID support, otherwise call ORIG."
  (let ((type (org-element-property :type link))
        (raw-path (org-element-property :path link)))
    (cond
     ((string= type "id")
      (let* ((id (my/site-normalize-id raw-path))
             (target-file (gethash id (my/site-id-link-table info))))
        (if target-file
            (let* ((href (concat (my/site-file-html-link target-file info)
                                 "#ID-" id))
                   (label (or (org-string-nw-p desc)
                              (file-name-base target-file))))
              (format "<a href=\"%s\">%s</a>"
                      (org-html-encode-plain-text href)
                      label))
          (funcall orig link desc info))))
     ((member type '("http" "https" "mailto" "doi"))
      (let ((html (funcall orig link desc info)))
        (if (string-match-p "\\`<a " html)
            (replace-regexp-in-string
             "\\`<a "
             "<a target=\"_blank\" rel=\"noopener noreferrer\" "
             html t t)
          html)))
     (t
      (funcall orig link desc info)))))

(defun my/site-add-hidden-id-anchors (output backend info)
  "Insert hidden file-level ID anchors into HTML OUTPUT."
  (if (not (org-export-derived-backend-p backend 'html))
      output
    (let* ((anchors
            (mapconcat
             (lambda (id)
               (format "<a id=\"ID-%s\" class=\"org-id-anchor\" aria-hidden=\"true\"></a>"
                       (org-html-encode-plain-text id)))
             (my/site-current-file-ids info)
             "\n")))
      (if (string-empty-p anchors)
          output
        (replace-regexp-in-string
         "\\(<div id=\"content\" class=\"content\">\\)"
         (concat "\\1\n" anchors)
         output t)))))

(defun my/site-html-attribute-escape (value)
  "Escape VALUE for use in an HTML attribute."
  (replace-regexp-in-string
   "\"" "&quot;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string
     ">" "&gt;"
     (replace-regexp-in-string "&" "&amp;" (or value "") t t)
     t t)
    t t)
   t t))

(defun my/site-note-body-attributes (info)
  "Return final note page body attributes for export INFO."
  (let* ((source-file (my/site-export-source-file info))
         (title (or (my/site-file-keyword source-file "title")
                    (and source-file (file-name-base source-file))
                    "Working Note"))
         (date (my/site-clean-date-keyword
                (my/site-file-keyword source-file "date")))
         (root (file-truename
                (or my/site-publish-root
                    (plist-get info :base-directory)
                    (file-name-directory load-file-name))))
         (rel (and source-file (file-relative-name source-file root)))
         (group (if (and rel (string-prefix-p "roam/" rel))
                    (or (cadr (split-string rel "/" t)) "Note")
                  "Note")))
    (format "class=\"note-page\" data-note-title=\"%s\" data-note-group=\"%s\" data-note-date=\"%s\""
            (my/site-html-attribute-escape (string-trim title))
            (my/site-html-attribute-escape group)
            (my/site-html-attribute-escape (string-trim date)))))

(defun my/site-apply-note-body-attributes (output backend info)
  "Make note CSS apply before JavaScript enhancement runs."
  (if (not (org-export-derived-backend-p backend 'html))
      output
    (replace-regexp-in-string
     "<body[^>]*>"
     (concat "<body " (my/site-note-body-attributes info) ">")
     output t t)))

(defun my/site-apply-source-title (output backend info)
  "Keep the visible H1 tied to the current source file title."
  (if (not (org-export-derived-backend-p backend 'html))
      output
    (let* ((source-file (my/site-export-source-file info))
           (title (or (my/site-file-keyword source-file "title")
                      (and source-file (file-name-base source-file)))))
      (if (not title)
          output
        (replace-regexp-in-string
         "<h1 class=\"title\">.*?</h1>"
         (format "<h1 class=\"title\">%s</h1>"
                 (org-html-encode-plain-text title))
         output t t)))))

(dolist (filter '(my/site-remove-leaked-id-paragraphs
                  my/site-remove-managed-blocks
                  my/site-copy-local-file-links))
  (add-to-list 'org-export-filter-parse-tree-functions filter))

(add-to-list 'org-export-filter-final-output-functions
             #'my/site-add-hidden-id-anchors)
(add-to-list 'org-export-filter-final-output-functions
             #'my/site-apply-note-body-attributes)
(add-to-list 'org-export-filter-final-output-functions
             #'my/site-apply-source-title)

(advice-add 'org-html-link :around #'my/site-html-link-a)
(advice-add 'org-html-special-block :around #'my/site-html-special-block-a)
(advice-add 'org-latex-special-block :around #'my/site-latex-special-block-a)

(defun my/site-html-head (plist filename pub-dir)
  "Publish FILENAME to PUB-DIR with assets referenced relative to the output path."
  (let* ((root (expand-file-name (plist-get plist :base-directory)))
         (rel-path (file-relative-name filename root))
         (rel-dir (file-name-directory rel-path))
         (depth (length (split-string (or rel-dir "") "/" t)))
         (prefix (if (> depth 0)
                     (mapconcat (lambda (_) "..") (number-sequence 1 depth) "/")
                   "."))
         (site-root (if (string= prefix ".") "./" (concat prefix "/")))
         (current-link (concat (file-name-sans-extension rel-path) ".html"))
         (head-snippet
          (mapconcat
           #'identity
           (list
           "<meta name=\"color-scheme\" content=\"light\" />"
            (format "<link rel=\"stylesheet\" href=\"%s/css/retro.css?v=%s\" />" prefix my/site-asset-version)
            "<link rel=\"shortcut icon\" href=\"https://raw.githubusercontent.com/AaronHnoraA/AaronHnoraA.github.io/refs/heads/master/css/cv.svg\" type=\"image/x-icon\">"
            "<script src=\"https://d3js.org/d3.v7.min.js\"></script>"
            (format "<script>window.SITE_ROOT_PATH=%S;window.CURRENT_NOTE_LINK=%S;</script>" site-root current-link)
            (format "<script defer src=\"%s/js/data.js?v=%s\"></script>" prefix my/site-asset-version)
            (format "<script defer src=\"%s/js/knowledge.js?v=%s\"></script>" prefix my/site-asset-version)
            (format "<script defer src=\"%s/js/graph.js?v=%s\"></script>" prefix my/site-asset-version)
            (format "<script defer src=\"%s/js/note-page.js?v=%s\"></script>" prefix my/site-asset-version))
           "\n")))
    (org-html-publish-to-html
     (let ((export-plist (copy-sequence plist)))
       (setq export-plist (plist-put export-plist :html-head head-snippet))
       (setq export-plist (plist-put export-plist :html-doctype "html5"))
       (setq export-plist (plist-put export-plist :html-html5-fancy t))
       (setq export-plist (plist-put export-plist :html-head-include-default-style nil))
       (setq export-plist (plist-put export-plist :html-postamble nil))
       (setq export-plist (plist-put export-plist :with-author nil))
       (setq export-plist (plist-put export-plist :with-creator nil))
       (setq export-plist (plist-put export-plist :with-date nil))
       (setq export-plist (plist-put export-plist :section-numbers nil))
       export-plist)
     filename pub-dir)))

(defun my/site-clean-text (text)
  "Strip lightweight Org markup from TEXT for summaries."
  (let ((clean (string-trim (or text ""))))
    (setq clean (replace-regexp-in-string "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" clean))
    (setq clean (replace-regexp-in-string "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" clean))
    (setq clean (replace-regexp-in-string "\\[\\[[^]]+\\]\\]" "" clean))
    (setq clean (replace-regexp-in-string "\\\\[()\\[\\]]" " " clean))
    (setq clean (replace-regexp-in-string "\\\\[[:alpha:]]+\\*?" " " clean))
    (setq clean (replace-regexp-in-string "[{}]" "" clean))
    (setq clean (replace-regexp-in-string "[=*~/+]" "" clean))
    (setq clean (replace-regexp-in-string "[[:space:]\n\r]+" " " clean))
    (string-trim clean)))

(defun my/site-prose-line-p (line)
  "Return non-nil when LINE looks like readable prose for summaries."
  (let* ((clean (my/site-clean-text line))
         (letters (length (replace-regexp-in-string "[^[:alpha:][:multibyte:]]" "" clean)))
         (symbols (length (replace-regexp-in-string "[[:alpha:][:multibyte:][:digit:][:space:][:punct:]]" "" clean))))
    (and (>= (length clean) 18)
         (>= letters 6)
         (<= symbols (/ (max letters 1) 2))
         (not (string-match-p "\\`[-=+*/^_(){}<>.,:;0-9 ]+\\'" clean)))))

(defun my/site-normalize-tags (tags)
  "Normalize TAGS into a sorted unique vector."
  (vconcat
   (sort
    (delete-dups
     (delq nil
           (mapcar
            (lambda (tag)
              (let ((clean (downcase (string-trim (or tag "")))))
                (unless (string-empty-p clean) clean)))
            tags)))
    #'string<)))

(defun my/site-group-key (rel-path)
  "Return the logical group key for REL-PATH."
  (let ((dir-name (file-name-directory rel-path)))
    (if (and dir-name (not (string= dir-name "./")))
        (directory-file-name dir-name)
      "Root")))

(defun my/site-group-label (group-key)
  "Return a display label for GROUP-KEY."
  (if (string= group-key "Root")
      "Root"
    (let* ((parts (split-string group-key "/" t))
           (leaf (car (last parts))))
      (if (string-match-p "\\`[A-Z0-9-]+\\'" leaf)
          leaf
        (mapconcat
         (lambda (part)
           (if (string-empty-p part)
               part
             (concat (upcase (substring part 0 1)) (substring part 1))))
         (split-string leaf "[-_]" t)
         " ")))))

(defun my/site-section-name (group-key)
  "Return the top-level section for GROUP-KEY."
  (if (string= group-key "Root")
      "Root"
    (car (split-string group-key "/" t))))

(defun my/site-extract-buffer-data (file)
  "Read FILE and return a plist with id, summary and outgoing id-links."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t)
          (outgoing '())
          id
          description
          summary)
      (goto-char (point-min))
      (when (re-search-forward "^:ID:[ \t]*\\(.+\\)$" nil t)
        (setq id (upcase (string-trim (match-string 1)))))

      (goto-char (point-min))
      (when (re-search-forward "^#\\+description:[ \t]*\\(.+\\)$" nil t)
        (setq description (my/site-clean-text (match-string 1))))

      (goto-char (point-min))
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" nil t)
        (push (upcase (string-trim (match-string 1))) outgoing))

      (setq summary description)

      (unless (and summary (not (string-empty-p summary)))
        (goto-char (point-min))
        (let ((lines '())
              (started nil))
          (while (and (not (eobp)) (< (length lines) 4) (not summary))
            (let ((line (string-trim (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
              (cond
               ((string-empty-p line)
                (when started
                  (setq summary (mapconcat #'identity (nreverse lines) " "))))
               ((or (string-prefix-p "#+" line)
                    (string-prefix-p ":" line)
                    (string-prefix-p "*" line)
                    (string-prefix-p "|" line)
                    (string-prefix-p "\\begin" line)
                    (string-prefix-p "\\end" line)
                    (string-match-p "\\`[-+][ \t]" line)
                    (string-match-p "\\`[0-9]+[.)][ \t]" line))
                nil)
               ((my/site-prose-line-p line)
                (setq started t)
                (push line lines))
               (t
                nil)))
            (forward-line 1))
          (unless summary
            (setq summary (mapconcat #'identity (nreverse lines) " "))))

        (unless (and summary (not (string-empty-p summary)))
          (goto-char (point-min))
          (let ((lines '())
                (started nil))
            (while (and (not (eobp)) (< (length lines) 4) (not summary))
              (let ((line (string-trim (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
                (cond
                 ((string-empty-p line)
                  (when started
                    (setq summary (mapconcat #'identity (nreverse lines) " "))))
                 ((or (string-prefix-p "#+" line)
                      (string-prefix-p ":" line)
                      (string-prefix-p "*" line)
                      (string-prefix-p "|" line)
                      (string-prefix-p "\\begin" line)
                      (string-prefix-p "\\end" line)
                      (string-match-p "\\`[-+][ \t]" line)
                      (string-match-p "\\`[0-9]+[.)][ \t]" line))
                  nil)
                 (t
                  (setq started t)
                  (push line lines))))
              (forward-line 1))
            (unless summary
              (setq summary (mapconcat #'identity (nreverse lines) " ")))))

      (list :id id
            :summary (truncate-string-to-width (my/site-clean-text summary) 180 nil nil t)
            :outgoing (delete-dups (nreverse outgoing)))))))

(defun my/site-note-json (note)
  "Convert NOTE plist to a JSON-friendly alist."
  `(("key" . ,(plist-get note :key))
    ("id" . ,(or (plist-get note :id) json-null))
    ("title" . ,(plist-get note :title))
    ("link" . ,(plist-get note :link))
    ("date" . ,(plist-get note :date))
    ("summary" . ,(plist-get note :summary))
    ("groupKey" . ,(plist-get note :group-key))
    ("groupLabel" . ,(plist-get note :group-label))
    ("section" . ,(plist-get note :section))
    ("tags" . ,(vconcat (plist-get note :tags)))
    ("refs" . ,(vconcat (plist-get note :refs)))
    ("backlinks" . ,(vconcat (plist-get note :backlinks)))))

(defun my/generate-data-and-index (_title _list)
  "Generate js/data.js and copy homepage.html to public/index.html."
  (let* ((project-name "site-org")
         (project-entry (assoc project-name org-publish-project-alist))
         (project-plist (cdr project-entry))
         (base-dir (expand-file-name (plist-get project-plist :base-directory)))
         (pub-dir (expand-file-name (plist-get project-plist :publishing-directory)))
         (js-dir (expand-file-name "js" pub-dir))
         (template-file (expand-file-name "homepage.html" base-dir))
         (notes-template-file (expand-file-name "notes.html" base-dir))
         (files (seq-filter
                 (lambda (file)
                   (let ((rel (file-relative-name file base-dir)))
                     (not (string-match-p "\\`\\(?:public/\\|js/\\|css/\\|ltximg/\\|CV/.+\\.org\\'\\)" rel))))
                 (directory-files-recursively base-dir "\\.org\\'")))
         (notes-by-key (make-hash-table :test 'equal))
         (id-to-key (make-hash-table :test 'equal))
         (backlinks (make-hash-table :test 'equal))
         (note-order '()))
    (unless project-entry
      (error "Missing org-publish project: %s" project-name))

    (dolist (file files)
      (let* ((rel-path (file-relative-name file base-dir))
             (filename (file-name-nondirectory file)))
        (unless (or (string-match-p "\\`\\." filename)
                    (string= filename "sitemap-log.org"))
          (let* ((buffer-data (my/site-extract-buffer-data file))
                 (group-key (my/site-group-key rel-path))
                 (key (or (plist-get buffer-data :id)
                          (concat "path:" (file-name-sans-extension rel-path))))
                 (note (list
                        :key key
                        :id (plist-get buffer-data :id)
                        :title (or (org-publish-find-title file project-entry)
                                   (file-name-base file))
                        :link (concat (file-name-sans-extension rel-path) ".html")
                        :date (format-time-string "%Y-%m-%d" (org-publish-find-date file project-entry))
                        :summary (plist-get buffer-data :summary)
                        :group-key group-key
                        :group-label (my/site-group-label group-key)
                        :section (my/site-section-name group-key)
                        :tags (append (my/site-normalize-tags
                                       (org-publish-find-property file :filetags project-entry))
                                      nil)
                        :refs '()
                        :raw-refs (plist-get buffer-data :outgoing)
                        :backlinks '())))
            (puthash key note notes-by-key)
            (push key note-order)
            (when (plist-get buffer-data :id)
              (puthash (plist-get buffer-data :id) key id-to-key))))))

    (dolist (key note-order)
      (let* ((note (gethash key notes-by-key))
             (resolved-refs '()))
        (dolist (target-id (plist-get note :raw-refs))
          (let ((target-key (gethash target-id id-to-key)))
            (when (and target-key (not (string= target-key key)))
              (push target-key resolved-refs)
              (puthash target-key (cons key (gethash target-key backlinks)) backlinks))))
        (setq resolved-refs (sort (delete-dups resolved-refs) #'string<))
        (puthash key (plist-put note :refs resolved-refs) notes-by-key)))

    (dolist (key note-order)
      (let* ((note (gethash key notes-by-key))
             (note-backlinks (sort (delete-dups (gethash key backlinks)) #'string<)))
        (setq note (plist-put note :backlinks note-backlinks))
        (setq note (plist-put note :raw-refs nil))
        (puthash key note notes-by-key)))

    (let* ((notes
            (mapcar (lambda (key) (gethash key notes-by-key))
                    note-order))
           (sorted-notes
            (sort notes
                  (lambda (a b)
                    (let ((date-a (plist-get a :date))
                          (date-b (plist-get b :date)))
                      (if (string= date-a date-b)
                          (string< (plist-get a :title) (plist-get b :title))
                        (string< date-b date-a))))))
           (tag-set (make-hash-table :test 'equal))
           (json-encoding-pretty-print nil))
      (dolist (note sorted-notes)
        (dolist (tag (plist-get note :tags))
          (puthash tag t tag-set)))

      (unless (file-exists-p js-dir)
        (make-directory js-dir t))

      (with-temp-file (expand-file-name "data.js" js-dir)
        (insert "const SITE_DATA = ")
        (insert
         (json-encode
          `(("meta" . (("generatedAt" . ,(format-time-string "%Y-%m-%d %H:%M:%S %z"))
                       ("noteCount" . ,(length sorted-notes))
                       ("tagCount" . ,(hash-table-count tag-set))))
            ("notes" . ,(vconcat (mapcar #'my/site-note-json sorted-notes))))))
        (insert ";\n"))

      (if (file-exists-p template-file)
          (copy-file template-file (expand-file-name "index.html" pub-dir) t)
        (message "homepage.html not found, skipping public/index.html"))

      (if (file-exists-p notes-template-file)
          (copy-file notes-template-file (expand-file-name "notes.html" pub-dir) t)
        (message "notes.html not found, skipping public/notes.html"))

      "* Sitemap generation log (ignore this file)")))

(setq org-publish-project-alist
      '(("site-org"
         :base-directory "~/HC/Org/"
         :base-extension "org"
         :publishing-directory "~/HC/Org/public/"
         :recursive t
         :publishing-function my/site-html-head
         :exclude "public/\\|ltximg/\\|homepage.html\\|js/\\|css/\\|CV/.+\\.org$"
         :auto-sitemap t
         :sitemap-title "Ignored"
         :sitemap-filename "sitemap-log.org"
         :sitemap-function my/generate-data-and-index)
        ("site-static"
         :base-directory "~/HC/Org/"
         :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\|avif\\|pdf"
         :publishing-directory "~/HC/Org/public/"
         :recursive t
         :exclude "public/\\|ltximg/\\|CV/jpg/"
         :publishing-function org-publish-attachment)
        ("site"
         :components ("site-org" "site-static"))))

(setq org-export-with-broken-links t)
(setq org-html-validation-link nil)
(setq org-publish-timestamp-directory my/site-timestamp-directory)
(setq org-id-locations-file my/site-org-id-locations-file)
(setq my/site-publish-root (expand-file-name "~/HC/Org/"))

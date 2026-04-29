;;; init-org-integrations.el --- Org external links and templates -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'init-org-code)
(require 'init-org-core)
(require 'ox)
(require 'ox-html)
(require 'ox-latex)
(require 'org-element)

(defcustom my/org-html-roam-assets-directory "assets"
  "Directory beside exported HTML files for copied org-roam image assets."
  :type 'string
  :group 'org)

(defconst my/org-html-roam-image-extensions
  '("png" "jpg" "jpeg" "gif" "svg" "webp" "avif")
  "Image extensions copied for org-roam HTML export.")

(defun my/org-export--managed-special-block-p (block)
  "Return non-nil when BLOCK is generated for local display, not export."
  (let ((type (downcase (or (org-element-property :type block) "")))
        (parameters (or (org-element-property :parameters block) "")))
    (or (string= type "toc")
        (and (string= type "overview")
             (string-match-p "\\_<:toc\\_>" parameters)))))

(defun my/org-export-remove-managed-blocks (tree _backend _info)
  "Remove generated overview/toc blocks from Org export parse TREE."
  (org-element-map tree 'special-block
    (lambda (block)
      (when (my/org-export--managed-special-block-p block)
        (org-element-extract-element block))))
  tree)

(defun my/org-export-display-latex-special-block-p (block)
  "Return non-nil when BLOCK is a display_latex wrapper."
  (string= (downcase (or (org-element-property :type block) ""))
           "display_latex"))

(defun my/org-export-html-special-block-a (orig special-block contents info)
  "Export display_latex SPECIAL-BLOCK as CONTENTS, otherwise call ORIG.
The block is only an editing and preview marker; exported HTML should let the
inner LaTeX fragments render as if the wrapper did not exist."
  (if (my/org-export-display-latex-special-block-p special-block)
      (or contents "")
    (funcall orig special-block contents info)))

(defun my/org-export-latex-special-block-a (orig special-block contents info)
  "Export display_latex SPECIAL-BLOCK as CONTENTS, otherwise call ORIG."
  (if (my/org-export-display-latex-special-block-p special-block)
      (or contents "")
    (funcall orig special-block contents info)))

(defun my/org-html-roam--image-file-p (file)
  "Return non-nil when FILE has an image extension."
  (member (downcase (or (file-name-extension file) ""))
          my/org-html-roam-image-extensions))

(defun my/org-html-roam--source-file (info)
  "Return source Org file from export INFO."
  (or (plist-get info :input-file)
      (buffer-file-name (buffer-base-buffer))))

(defun my/org-html-roam--output-directory (info)
  "Return the directory that should contain assets for export INFO."
  (let ((output-file (or (plist-get info :output-file)
                         (ignore-errors
                           (org-export-output-file-name ".html" nil)))))
    (file-name-directory
     (expand-file-name
      (or output-file
          (concat (file-name-sans-extension
                   (or (my/org-html-roam--source-file info)
                       (expand-file-name "org-export.html" default-directory)))
                  ".html"))))))

(defun my/org-html-roam--file-in-directory-p (file directory)
  "Return non-nil when FILE is inside DIRECTORY."
  (and file directory
       (file-exists-p file)
       (file-directory-p directory)
       (file-in-directory-p (file-truename file) (file-truename directory))))

(defun my/org-html-roam--resolve-file-link (path info)
  "Resolve local file link PATH against the source file in export INFO."
  (let ((expanded (substitute-in-file-name path)))
    (if (file-name-absolute-p expanded)
        (expand-file-name expanded)
      (expand-file-name
       expanded
       (file-name-directory
        (or (my/org-html-roam--source-file info)
            (expand-file-name "org-export.org" default-directory)))))))

(defun my/org-html-roam--asset-relative-path (source-file image-file)
  "Return a stable asset subpath for IMAGE-FILE linked from SOURCE-FILE."
  (let* ((source-dir (and source-file (file-name-directory source-file)))
         (local-rel (and source-dir (file-relative-name image-file source-dir))))
    (if (and local-rel
             (not (string-prefix-p "../" local-rel))
             (not (string= local-rel "..")))
        local-rel
      (file-relative-name image-file (file-truename my-org-roam-dir)))))

(defun my/org-html-roam--copy-image-asset (image-file info)
  "Copy org-roam IMAGE-FILE for HTML export described by INFO.
Return a relative path from the exported HTML file to the copied asset."
  (let* ((source-file (my/org-html-roam--source-file info))
         (output-dir (my/org-html-roam--output-directory info))
         (asset-rel (my/org-html-roam--asset-relative-path source-file image-file))
         (target-file (expand-file-name
                       asset-rel
                       (expand-file-name my/org-html-roam-assets-directory
                                         output-dir))))
    (make-directory (file-name-directory target-file) t)
    (unless (and (file-exists-p target-file)
                 (or (file-equal-p image-file target-file)
                     (let ((source-attrs (file-attributes image-file))
                           (target-attrs (file-attributes target-file)))
                       (and (= (file-attribute-size source-attrs)
                               (file-attribute-size target-attrs))
                            (equal (file-attribute-modification-time source-attrs)
                                   (file-attribute-modification-time target-attrs))))))
      (copy-file image-file target-file t t))
    (file-relative-name target-file output-dir)))

(defun my/org-html-roam-copy-image-links (tree backend info)
  "Copy org-roam local images and rewrite links in HTML export TREE."
  (when (and (org-export-derived-backend-p backend 'html)
             (my/org-html-roam--file-in-directory-p
              (my/org-html-roam--source-file info)
              my-org-roam-dir))
    (org-element-map tree 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (let* ((path (org-element-property :path link))
                 (image-file (and path
                                  (my/org-html-roam--resolve-file-link path info))))
            (when (and image-file
                       (file-regular-p image-file)
                       (my/org-html-roam--image-file-p image-file)
                       (my/org-html-roam--file-in-directory-p image-file my-org-roam-dir))
              (org-element-put-property
               link :path
               (my/org-html-roam--copy-image-asset image-file info))))))))
  tree)

(dolist (filter '(my/org-export-remove-managed-blocks
                  my/org-html-roam-copy-image-links))
  (add-to-list 'org-export-filter-parse-tree-functions filter))

(advice-add 'org-html-special-block :around
            #'my/org-export-html-special-block-a)
(advice-add 'org-latex-special-block :around
            #'my/org-export-latex-special-block-a)

;; External App Links (Zotero, MarginNote)
(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
    :follow (lambda (path)
              (let ((url (concat "zotero:" path))
                    (command (if (eq system-type 'darwin) "open" "xdg-open")))
                (start-process "zotero-opener" nil command url))))

  (let ((marginnote-link-types
         '("marginnote1app" "marginnote2app" "marginnote3app" "marginnote4app")))
    (dolist (type marginnote-link-types)
      (org-link-set-parameters
       type
       :follow
       (lambda (path)
         (if (eq system-type 'darwin)
             (let ((url (concat "marginnote4app:" path)))
               (start-process "marginnote" nil "open" url))
           (message "[org] MarginNote link only supported on macOS (got %s)" system-type)))))))

(with-eval-after-load 'org-tempo
  (dolist (template '(("dot" . "src dot :file images/graph.svg")
                      ("ditaa" . "src ditaa :file images/diagram.png")
                      ("pic" . "src ditaa :file images/diagram.png")))
    (add-to-list 'org-structure-template-alist template t)))

(require 'org-tempo) 

(provide 'init-org-integrations)
;;; init-org-integrations.el ends here

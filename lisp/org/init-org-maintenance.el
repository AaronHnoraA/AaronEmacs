;;; init-org-maintenance.el --- Org library maintenance board -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Manual maintenance helpers for Org projects.  Scans are command-driven:
;; there are no hooks, timers or background refreshes.

;;; Code:

(require 'cl-lib)
(require 'init-org-core)
(require 'init-open nil t)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'browse-url)
(require 'project nil t)

(declare-function my/open-system-target "init-open" (target))

(defgroup my/org-maintenance nil
  "Maintenance helpers for personal Org projects."
  :group 'org)

(defcustom my/org-maintenance-root my-org-root
  "Primary Org root used when the current buffer is inside the Org library."
  :type 'directory
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-image-directory-names '("img" "images")
  "Managed image directory names used for empty-directory cleanup.
Image cleanup itself considers local image files in the current maintenance
scope, not only files below these directory names."
  :type '(repeat string)
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-attachment-directory-names '("attachments" "attach" "files")
  "Directory names whose non-Org files are treated as cleanable attachments."
  :type '(repeat string)
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-image-extensions
  '("avif" "bmp" "gif" "jpeg" "jpg" "png" "svg" "webp")
  "Image filename extensions tracked by Org maintenance helpers."
  :type '(repeat string)
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-excluded-directory-names
  '(".git" ".cache" ".direnv" ".venv" "node_modules"
    "public" "publish" "dist" "build" "css" "js" "CV"
    "ltximg" "org-latex-preview-cache")
  "Directory names ignored while scanning media and attachment candidates.
Published output and Org LaTeX preview cache directories are deliberately
excluded from media cleanup."
  :type '(repeat string)
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-protected-file-prefix "keep-"
  "Filename prefix that exempts media and attachments from cleanup."
  :type 'string
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-latex-cache-directory-name "ltximg"
  "Directory name used by Org LaTeX preview cache files."
  :type 'string
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-latex-cache-max-files-per-dir 256
  "Number of newest LaTeX preview cache files to keep per cache directory."
  :type 'integer
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-cache-enabled t
  "Whether to reuse maintenance reports while the scope file signature is stable."
  :type 'boolean
  :group 'my/org-maintenance)

(defcustom my/org-maintenance-board-preview-limit 24
  "Maximum number of detailed rows shown per board section."
  :type 'integer
  :group 'my/org-maintenance)

(defconst my/org-maintenance-board-buffer-name "*Org Maintenance Board*"
  "Buffer name for the Org maintenance board.")

(defvar-local my/org-maintenance-board--report nil
  "Most recent Org maintenance report for the current board.")

(defvar my/org-maintenance--scan-cache nil
  "Last maintenance scan cache.")

(defun my/org-maintenance--configured-root ()
  "Return the normalized configured Org maintenance root."
  (file-name-as-directory (file-truename my/org-maintenance-root)))

(defun my/org-maintenance--default-directory ()
  "Return a safe local directory for scoped maintenance scans."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory
        my-org-root))))

(defun my/org-maintenance--inside-dir-p (file dir)
  "Return non-nil when FILE is inside DIR."
  (and file dir
       (file-in-directory-p (file-truename (expand-file-name file))
                            (file-name-as-directory
                             (file-truename (expand-file-name dir))))))

(defun my/org-maintenance--project-root ()
  "Return the current project root, or nil."
  (when (fboundp 'project-current)
    (when-let* ((project (project-current nil)))
      (file-name-as-directory (expand-file-name (project-root project))))))

(defun my/org-maintenance-scope-root ()
  "Return the root used by the current maintenance command.
Inside `my/org-maintenance-root', scan that Org library.  Outside it, scan the
current project.  If there is no project, scan the current directory."
  (let ((dir (my/org-maintenance--default-directory))
        (root (my/org-maintenance--configured-root)))
    (cond
     ((my/org-maintenance--inside-dir-p dir root) root)
     ((my/org-maintenance--project-root))
     (t dir))))

(defun my/org-maintenance--file-size (file)
  "Return FILE size in bytes, or 0 when it cannot be read."
  (or (file-attribute-size (file-attributes file)) 0))

(defun my/org-maintenance--format-bytes (bytes)
  "Return BYTES as a human-readable size."
  (file-size-human-readable (or bytes 0)))

(defun my/org-maintenance--sum-file-sizes (files)
  "Return total byte size for FILES."
  (cl-loop for file in files
           sum (my/org-maintenance--file-size file)))

(defun my/org-maintenance--latex-cache-name-p (name)
  "Return non-nil when NAME denotes an Org LaTeX preview cache artifact."
  (or (string= name my/org-maintenance-latex-cache-directory-name)
      (string= name "org-latex-preview-cache")
      (string-prefix-p "org-ltximg" name)))

(defun my/org-maintenance--excluded-dir-p (dir &optional include-latex-cache)
  "Return non-nil when DIR should be skipped.
When INCLUDE-LATEX-CACHE is non-nil, LaTeX cache directories themselves are
not skipped, but published output and ordinary ignored directories still are."
  (let ((base (file-name-nondirectory (directory-file-name dir))))
    (and (member base my/org-maintenance-excluded-directory-names)
         (not (and include-latex-cache
                   (my/org-maintenance--latex-cache-name-p base))))))

(defun my/org-maintenance--path-segments (path root)
  "Return PATH segments relative to ROOT."
  (let ((rel (file-relative-name (expand-file-name path)
                                 (file-name-as-directory root))))
    (split-string rel "/" t)))

(defun my/org-maintenance--excluded-path-p (path root &optional include-latex-cache)
  "Return non-nil when PATH has a directory segment that should be ignored."
  (seq-some
   (lambda (segment)
     (and (member segment my/org-maintenance-excluded-directory-names)
          (not (and include-latex-cache
                    (my/org-maintenance--latex-cache-name-p segment)))))
   (my/org-maintenance--path-segments path root)))

(defun my/org-maintenance--latex-cache-path-p (path root)
  "Return non-nil when PATH is in or names an Org LaTeX preview cache artifact."
  (let ((base (file-name-nondirectory (directory-file-name path))))
    (or (my/org-maintenance--latex-cache-name-p base)
        (seq-some #'my/org-maintenance--latex-cache-name-p
                  (my/org-maintenance--path-segments path root)))))

(defun my/org-maintenance--external-globs (&optional include-latex-cache)
  "Return ripgrep glob arguments for maintenance scans."
  (mapcan
   (lambda (name)
     (unless (and include-latex-cache
                  (my/org-maintenance--latex-cache-name-p name))
       (list "--glob" (format "!**/%s/**" name))))
   my/org-maintenance-excluded-directory-names))

(defun my/org-maintenance--process-lines (program args &optional ok-statuses)
  "Run PROGRAM with ARGS and return stdout lines."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (unless (memq status (or ok-statuses '(0)))
        (error "%s failed with status %s: %s"
               program status (string-trim (buffer-string))))
      (split-string (buffer-string) "\n" t))))

(defun my/org-maintenance--external-files (root &optional globs include-latex-cache)
  "Return regular files below ROOT using rg or fd.
GLOBS are ripgrep-style glob filters."
  (condition-case nil
      (cond
       ((executable-find "rg")
        (let* ((default-directory root)
               (args (append '("--files" "--hidden")
                             (my/org-maintenance--external-globs include-latex-cache)
                             globs
                             '("."))))
          (mapcar (lambda (file)
                    (file-truename (expand-file-name file root)))
                  (my/org-maintenance--process-lines "rg" args '(0 1)))))
       ((executable-find "fd")
        (let* ((default-directory root)
               (args (append '("--hidden" "--type" "f")
                             (mapcan (lambda (name)
                                       (unless (and include-latex-cache
                                                    (my/org-maintenance--latex-cache-name-p name))
                                         (list "--exclude" name)))
                                     my/org-maintenance-excluded-directory-names)
                             '("."))))
          (seq-filter
           (lambda (file)
             (not (my/org-maintenance--excluded-path-p
                   file root include-latex-cache)))
           (mapcar (lambda (file)
                     (file-truename (expand-file-name file root)))
                   (my/org-maintenance--process-lines "fd" args '(0)))))))
    (error nil)))

(defun my/org-maintenance--files-recursive (dir predicate &optional include-latex-cache)
  "Return regular files below DIR for which PREDICATE returns non-nil."
  (let (files)
    (when (and (file-directory-p dir)
               (not (file-remote-p dir)))
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
        (cond
         ((and (file-directory-p entry)
               (not (file-symlink-p entry))
               (not (my/org-maintenance--excluded-dir-p entry include-latex-cache)))
          (setq files
                (nconc (my/org-maintenance--files-recursive
                        entry predicate include-latex-cache)
                       files)))
         ((and (file-regular-p entry)
               (funcall predicate entry))
          (push (file-truename entry) files)))))
    files))

(defun my/org-maintenance--directories-named (root names &optional include-latex-cache)
  "Return non-symlink directories below ROOT whose basename is in NAMES."
  (let ((external
         (condition-case nil
             (when (executable-find "fd")
               (let ((default-directory root)
                     (args (append '("--hidden" "--type" "d")
                                   (mapcan (lambda (name)
                                             (unless (and include-latex-cache
                                                          (my/org-maintenance--latex-cache-name-p name))
                                               (list "--exclude" name)))
                                           my/org-maintenance-excluded-directory-names)
                                   '("."))))
                 (seq-filter
                  (lambda (dir)
                    (and (member (file-name-nondirectory
                                  (directory-file-name dir))
                                 names)
                         (not (my/org-maintenance--excluded-path-p
                               dir root include-latex-cache))))
                  (mapcar (lambda (dir)
                            (file-name-as-directory
                             (file-truename (expand-file-name dir root))))
                          (my/org-maintenance--process-lines "fd" args '(0))))))
           (error nil))))
    (or external
        (let (dirs)
          (when (file-directory-p root)
            (dolist (entry (directory-files root t directory-files-no-dot-files-regexp))
              (when (and (file-directory-p entry)
                         (not (file-symlink-p entry))
                         (not (my/org-maintenance--excluded-dir-p
                               entry include-latex-cache)))
                (let ((base (file-name-nondirectory (directory-file-name entry))))
                  (when (member base names)
                    (push (file-name-as-directory (file-truename entry)) dirs))
                  (setq dirs
                        (nconc (my/org-maintenance--directories-named
                                entry names include-latex-cache)
                               dirs))))))
          dirs))))

(defun my/org-maintenance--directory-empty-p (dir)
  "Return non-nil when DIR has no entries except . and ..."
  (null (directory-files dir nil directory-files-no-dot-files-regexp)))

(defun my/org-maintenance--image-file-p (file)
  "Return non-nil when FILE has a tracked image extension."
  (member (downcase (or (file-name-extension file) ""))
          my/org-maintenance-image-extensions))

(defun my/org-maintenance--attachment-file-p (file root)
  "Return non-nil when FILE is inside a managed attachment directory."
  (and (not (string-match-p "\\.org\\'" file))
       (seq-some
        (lambda (segment)
          (member segment my/org-maintenance-attachment-directory-names))
        (my/org-maintenance--path-segments file root))))

(defun my/org-maintenance--media-kind (file root)
  "Return FILE media kind inside ROOT: `attachment', `image' or nil.
Files inside managed attachment directories are treated as attachments even
when their extension is an image extension, because attachments use project-wide
reference analysis."
  (cond
   ((my/org-maintenance--attachment-file-p file root) 'attachment)
   ((my/org-maintenance--image-file-p file) 'image)))

(defun my/org-maintenance--protected-file-p (file)
  "Return non-nil when FILE is exempt from cleanup by prefix."
  (let ((prefix my/org-maintenance-protected-file-prefix)
        (base (file-name-nondirectory file)))
    (and (not (string-empty-p prefix))
         (string-prefix-p prefix base))))

(defun my/org-maintenance--org-files (root)
  "Return Org files under ROOT."
  (sort
   (delete-dups
    (or (my/org-maintenance--external-files root '("--glob" "*.org"))
        (my/org-maintenance--files-recursive
         root
         (lambda (file)
           (and (not (my/org-maintenance--excluded-path-p file root))
                (string-match-p "\\.org\\'" file))))))
   #'string<))

(defun my/org-maintenance--media-files (root)
  "Return cleanable media and attachment candidates under ROOT."
  (sort
   (delete-dups
    (or (seq-filter
         (lambda (file)
           (and (not (my/org-maintenance--excluded-path-p file root))
                (not (my/org-maintenance--latex-cache-path-p file root))
                (my/org-maintenance--media-kind file root)))
         (my/org-maintenance--external-files root))
        (my/org-maintenance--files-recursive
         root
         (lambda (file)
           (and (not (my/org-maintenance--excluded-path-p file root))
                (not (my/org-maintenance--latex-cache-path-p file root))
                (my/org-maintenance--media-kind file root))))))
   #'string<))

(defun my/org-maintenance--split-file-link-path (path)
  "Return (CLEAN-PATH . SUFFIX) for Org file link PATH."
  (when (stringp path)
    (let ((unescaped (org-link-unescape path)))
      (if (string-match "\\(.*\\)\\(::.*\\)\\'" unescaped)
          (cons (match-string 1 unescaped) (match-string 2 unescaped))
        (cons unescaped "")))))

(defun my/org-maintenance--clean-file-link-path (path)
  "Return local filesystem part of Org file link PATH."
  (when-let* ((split (my/org-maintenance--split-file-link-path path))
              (clean (car split)))
    (unless (or (string-empty-p clean)
                (file-remote-p clean)
                (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" clean))
      clean)))

(defun my/org-maintenance--resolve-file-link (source-file path)
  "Resolve file link PATH from SOURCE-FILE, returning an expanded path."
  (when-let* ((clean (my/org-maintenance--clean-file-link-path path)))
    (expand-file-name clean (file-name-directory source-file))))

(defun my/org-maintenance--mark-reference-p (kind source-file target)
  "Return non-nil when SOURCE-FILE should mark TARGET of KIND referenced."
  (pcase kind
    ('attachment t)
    ('image (file-in-directory-p (expand-file-name target)
                                 (file-name-directory source-file)))))

(defun my/org-maintenance--rg-file-link-lines (root)
  "Return ripgrep lines containing Org file links below ROOT."
  (when (executable-find "rg")
    (let ((default-directory root)
          (args (append '("--no-heading" "--line-number" "--color" "never"
                         "--hidden")
                        (my/org-maintenance--external-globs)
                        '("--glob" "*.org" "--" "file:" "."))))
      (my/org-maintenance--process-lines "rg" args '(0 1)))))

(defun my/org-maintenance--parse-rg-line (root line)
  "Parse a ripgrep LINE into (:file FILE :line LINE :text TEXT)."
  (when (string-match "\\`\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)\\'" line)
    (list :file (file-truename (expand-file-name (match-string 1 line) root))
          :line (string-to-number (match-string 2 line))
          :text (match-string 3 line))))

(defun my/org-maintenance--file-link-paths-in-text (text)
  "Return possible Org file link paths found in TEXT."
  (let (paths start)
    (setq start 0)
    (while (string-match "\\[\\[file:\\([^]\n]+\\)\\]" text start)
      (push (match-string 1 text) paths)
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match "\\(?:^\\|[[:space:]([{:]\\)file:\\([^][ \t\n)>,}]+\\)" text start)
      (push (match-string 1 text) paths)
      (setq start (match-end 0)))
    (delete-dups (nreverse paths))))

(defun my/org-maintenance--record-file-link-reference
    (source-file path root referenced broken)
  "Record one Org file link PATH from SOURCE-FILE."
  (let* ((target (my/org-maintenance--resolve-file-link source-file path))
         (kind (and target (my/org-maintenance--media-kind target root))))
    (when (and target
               kind
               (my/org-maintenance--inside-dir-p target root)
               (not (my/org-maintenance--excluded-path-p target root))
               (not (my/org-maintenance--latex-cache-path-p target root)))
      (if (file-regular-p target)
          (when (my/org-maintenance--mark-reference-p kind source-file target)
            (puthash (file-truename target) t referenced))
        (push (list :source source-file
                    :target target
                    :kind kind)
              broken))))
  broken)

(defun my/org-maintenance--scan-org-links-rg (root referenced broken)
  "Scan Org file links below ROOT using ripgrep."
  (dolist (line (my/org-maintenance--rg-file-link-lines root))
    (when-let* ((entry (my/org-maintenance--parse-rg-line root line))
                (source-file (plist-get entry :file))
                (text (plist-get entry :text)))
      (dolist (path (my/org-maintenance--file-link-paths-in-text text))
        (setq broken
              (my/org-maintenance--record-file-link-reference
               source-file path root referenced broken)))))
  broken)

(defun my/org-maintenance--scan-org-links (source-file root referenced broken)
  "Scan SOURCE-FILE into REFERENCED hash table and BROKEN list.
Image links only count when they point to the Org file's own directory tree.
Attachment links count from any Org file in the current maintenance scope."
  (with-temp-buffer
    (insert-file-contents source-file)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree 'link
        (lambda (link)
          (when (string= (org-element-property :type link) "file")
            (let* ((path (org-element-property :path link))
                   (target (my/org-maintenance--resolve-file-link source-file path))
                   (kind (and target (my/org-maintenance--media-kind target root))))
              (when (and target
                         kind
                         (my/org-maintenance--inside-dir-p target root)
                         (not (my/org-maintenance--excluded-path-p target root))
                         (not (my/org-maintenance--latex-cache-path-p target root)))
                (if (file-regular-p target)
                    (when (my/org-maintenance--mark-reference-p
                           kind source-file target)
                      (puthash (file-truename target) t referenced))
                  (push (list :source source-file
                              :target target
                              :kind kind)
                        broken)))))))))
  broken)

(defun my/org-maintenance--referenced-media (org-files root)
  "Return a plist with media references scanned from ORG-FILES under ROOT."
  (let ((referenced (make-hash-table :test 'equal))
        broken)
    (if (executable-find "rg")
        (setq broken
              (condition-case err
                  (my/org-maintenance--scan-org-links-rg root referenced broken)
                (error
                 (push (list :source root
                             :target nil
                             :kind nil
                             :error (error-message-string err))
                       broken))))
      (dolist (file org-files)
        (setq broken
              (condition-case err
                  (my/org-maintenance--scan-org-links file root referenced broken)
                (error
                 (push (list :source file
                             :target nil
                             :kind nil
                             :error (error-message-string err))
                       broken))))))
    (list :referenced referenced
          :broken (nreverse broken))))

(defun my/org-maintenance--latex-cache-dirs (root)
  "Return Org LaTeX preview cache directories below ROOT."
  (sort
   (delete-dups
    (my/org-maintenance--directories-named
     root
     (delete-dups
      (list my/org-maintenance-latex-cache-directory-name
            "org-latex-preview-cache"))
     t))
   #'string<))

(defun my/org-maintenance--latex-cache-files (dirs)
  "Return regular files below LaTeX cache DIRS."
  (let (files)
    (dolist (dir dirs)
      (setq files
            (nconc
             (or (my/org-maintenance--external-files dir nil t)
                 (my/org-maintenance--files-recursive dir (lambda (_file) t) t))
             files)))
    (sort (delete-dups files) #'string<)))

(defun my/org-maintenance--mtime-float (file)
  "Return FILE modification time as a float."
  (float-time (file-attribute-modification-time (file-attributes file))))

(defun my/org-maintenance--latex-prunable-files (dirs)
  "Return old LaTeX cache files in DIRS beyond the configured retention limit."
  (let ((keep (max 0 my/org-maintenance-latex-cache-max-files-per-dir))
        prunable)
    (dolist (dir dirs)
      (let* ((files (sort (or (my/org-maintenance--external-files dir nil t)
                              (my/org-maintenance--files-recursive
                               dir (lambda (_file) t) t))
                          (lambda (a b)
                            (> (my/org-maintenance--mtime-float a)
                               (my/org-maintenance--mtime-float b)))))
             (old (nthcdr keep files)))
        (setq prunable (nconc old prunable))))
    (sort prunable #'string<)))

(defun my/org-maintenance--file-signature (files)
  "Return a stable modification signature for FILES."
  (mapcar
   (lambda (file)
     (list file
           (my/org-maintenance--file-size file)
           (my/org-maintenance--mtime-float file)))
   files))

(defun my/org-maintenance--scan-signature (root org-files media-files latex-files)
  "Return the cache signature for ROOT, ORG-FILES, MEDIA-FILES and LATEX-FILES."
  (list :root root
        :image-extensions my/org-maintenance-image-extensions
        :attachment-dirs my/org-maintenance-attachment-directory-names
        :excluded-dirs my/org-maintenance-excluded-directory-names
        :protected-prefix my/org-maintenance-protected-file-prefix
        :latex-keep my/org-maintenance-latex-cache-max-files-per-dir
        :org-files (my/org-maintenance--file-signature org-files)
        :media-files (my/org-maintenance--file-signature media-files)
        :latex-files (my/org-maintenance--file-signature latex-files)))

(defun my/org-maintenance--invalidate-cache ()
  "Clear the maintenance scan cache."
  (setq my/org-maintenance--scan-cache nil))

(defun my/org-maintenance--build-report
    (root org-files media-files latex-dirs latex-files)
  "Build a fresh maintenance report for ROOT."
  (let* ((reference-report (my/org-maintenance--referenced-media org-files root))
         (referenced (plist-get reference-report :referenced))
         (broken (plist-get reference-report :broken))
         (protected (seq-filter #'my/org-maintenance--protected-file-p media-files))
         (unreferenced
          (seq-remove (lambda (file)
                        (or (gethash file referenced)
                            (my/org-maintenance--protected-file-p file)))
                      media-files))
         (image-files (seq-filter
                       (lambda (file)
                         (eq (my/org-maintenance--media-kind file root) 'image))
                       media-files))
         (attachment-files (seq-filter
                            (lambda (file)
                              (eq (my/org-maintenance--media-kind file root)
                                  'attachment))
                            media-files))
         (unreferenced-images (seq-filter
                               (lambda (file)
                                 (eq (my/org-maintenance--media-kind file root)
                                     'image))
                               unreferenced))
         (unreferenced-attachments (seq-filter
                                    (lambda (file)
                                      (eq (my/org-maintenance--media-kind file root)
                                          'attachment))
                                    unreferenced))
         (latex-prunable (my/org-maintenance--latex-prunable-files latex-dirs)))
    (list :root root
          :scope-root root
          :cache-hit nil
          :org-files org-files
          :media-files media-files
          :image-files image-files
          :attachment-files attachment-files
          :protected-media protected
          :referenced-media referenced
          :unreferenced-media unreferenced
          :unreferenced-images unreferenced-images
          :unreferenced-attachments unreferenced-attachments
          :broken-media-links broken
          :latex-cache-dirs latex-dirs
          :latex-cache-files latex-files
          :latex-prunable-files latex-prunable
          :media-bytes (my/org-maintenance--sum-file-sizes media-files)
          :image-bytes (my/org-maintenance--sum-file-sizes image-files)
          :attachment-bytes (my/org-maintenance--sum-file-sizes attachment-files)
          :protected-media-bytes (my/org-maintenance--sum-file-sizes protected)
          :unreferenced-media-bytes
          (my/org-maintenance--sum-file-sizes unreferenced)
          :unreferenced-image-bytes
          (my/org-maintenance--sum-file-sizes unreferenced-images)
          :unreferenced-attachment-bytes
          (my/org-maintenance--sum-file-sizes unreferenced-attachments)
          :latex-cache-bytes (my/org-maintenance--sum-file-sizes latex-files)
          :latex-prunable-bytes
          (my/org-maintenance--sum-file-sizes latex-prunable))))

(defun my/org-maintenance-scan (&optional force)
  "Return a structured report for the Org maintenance board.
With FORCE, ignore the scan cache.  The cache avoids reparsing Org links while
the scope file signature is unchanged."
  (interactive "P")
  (let* ((root (my/org-maintenance-scope-root))
         (org-files (my/org-maintenance--org-files root))
         (media-files (my/org-maintenance--media-files root))
         (latex-dirs (my/org-maintenance--latex-cache-dirs root))
         (latex-files (my/org-maintenance--latex-cache-files latex-dirs))
         (signature (my/org-maintenance--scan-signature
                     root org-files media-files latex-files))
         (cached (and my/org-maintenance-cache-enabled
                      (not force)
                      my/org-maintenance--scan-cache
                      (equal signature
                             (plist-get my/org-maintenance--scan-cache
                                        :signature)))))
    (if cached
        (let ((report (copy-sequence
                       (plist-get my/org-maintenance--scan-cache :report))))
          (plist-put report :cache-hit t)
          report)
      (let ((report (my/org-maintenance--build-report
                     root org-files media-files latex-dirs latex-files)))
        (when my/org-maintenance-cache-enabled
          (setq my/org-maintenance--scan-cache
                (list :signature signature :report report)))
        report))))

(defun my/org-maintenance--delete-files (files)
  "Delete FILES and return the number of deleted files."
  (let ((count 0))
    (dolist (file files)
      (when (file-regular-p file)
        (delete-file file)
        (cl-incf count)))
    (when (> count 0)
      (my/org-maintenance--invalidate-cache))
    count))

(defun my/org-maintenance--confirm-delete (prompt files bytes force)
  "Ask whether PROMPT should delete FILES using BYTES as size hint.
When FORCE is non-nil, return t without prompting."
  (or force
      (and files
           (yes-or-no-p
            (format "%s %d files (%s)? "
                    prompt
                    (length files)
                    (my/org-maintenance--format-bytes bytes))))))

(defun my/org-maintenance--refresh-board-if-visible ()
  "Refresh the maintenance board when it is visible."
  (when-let* ((buffer (get-buffer my/org-maintenance-board-buffer-name)))
    (with-current-buffer buffer
      (when (derived-mode-p 'my/org-maintenance-board-mode)
        (my/org-maintenance-board-refresh t)))))

(defun my/org-maintenance-clean-unreferenced-media (&optional force)
  "Delete unreferenced image and attachment candidates in the current scope."
  (interactive "P")
  (let* ((report (my/org-maintenance-scan))
         (files (plist-get report :unreferenced-media))
         (bytes (plist-get report :unreferenced-media-bytes)))
    (if (null files)
        (message "No unreferenced Org media in current scope")
      (when (my/org-maintenance--confirm-delete
             "Delete unreferenced Org media" files bytes force)
        (let ((count (my/org-maintenance--delete-files files)))
          (my/org-maintenance-delete-empty-managed-dirs t)
          (my/org-maintenance--refresh-board-if-visible)
          (message "Deleted %d unreferenced Org media files" count))))))

(defun my/org-maintenance-delete-unreferenced-images (&optional force)
  "Delete unreferenced local image files in the current scope."
  (interactive "P")
  (let* ((report (my/org-maintenance-scan))
         (files (plist-get report :unreferenced-images))
         (bytes (plist-get report :unreferenced-image-bytes)))
    (if (null files)
        (message "No unreferenced Org images in current scope")
      (when (my/org-maintenance--confirm-delete
             "Delete unreferenced Org images" files bytes force)
        (let ((count (my/org-maintenance--delete-files files)))
          (my/org-maintenance-delete-empty-managed-dirs t)
          (my/org-maintenance--refresh-board-if-visible)
          (message "Deleted %d unreferenced Org images" count))))))

(defun my/org-maintenance-delete-unreferenced-attachments (&optional force)
  "Delete unreferenced attachment files in the current scope."
  (interactive "P")
  (let* ((report (my/org-maintenance-scan))
         (files (plist-get report :unreferenced-attachments))
         (bytes (plist-get report :unreferenced-attachment-bytes)))
    (if (null files)
        (message "No unreferenced Org attachments in current scope")
      (when (my/org-maintenance--confirm-delete
             "Delete unreferenced Org attachments" files bytes force)
        (let ((count (my/org-maintenance--delete-files files)))
          (my/org-maintenance-delete-empty-managed-dirs t)
          (my/org-maintenance--refresh-board-if-visible)
          (message "Deleted %d unreferenced Org attachments" count))))))

(defun my/org-maintenance-prune-latex-cache (&optional force)
  "Delete old LaTeX preview cache files beyond the retention limit."
  (interactive "P")
  (let* ((report (my/org-maintenance-scan))
         (files (plist-get report :latex-prunable-files))
         (bytes (plist-get report :latex-prunable-bytes)))
    (if (null files)
        (message "No old Org LaTeX preview cache files to prune")
      (when (my/org-maintenance--confirm-delete
             (format "Delete Org LaTeX preview cache files beyond newest %d per dir"
                     my/org-maintenance-latex-cache-max-files-per-dir)
             files bytes force)
        (let ((count (my/org-maintenance--delete-files files)))
          (my/org-maintenance-delete-empty-managed-dirs t)
          (my/org-maintenance--refresh-board-if-visible)
          (message "Pruned %d Org LaTeX preview cache files" count))))))

(defun my/org-maintenance-clear-latex-cache (&optional force)
  "Delete all LaTeX preview cache files in the current scope."
  (interactive "P")
  (let* ((report (my/org-maintenance-scan))
         (files (plist-get report :latex-cache-files))
         (bytes (plist-get report :latex-cache-bytes)))
    (if (null files)
        (message "No Org LaTeX preview cache files to clear")
      (when (my/org-maintenance--confirm-delete
             "Delete all Org LaTeX preview cache files" files bytes force)
        (let ((count (my/org-maintenance--delete-files files)))
          (my/org-maintenance-delete-empty-managed-dirs t)
          (my/org-maintenance--refresh-board-if-visible)
          (message "Cleared %d Org LaTeX preview cache files" count))))))

(defun my/org-maintenance--collect-dirs-recursively (dir &optional include-latex-cache)
  "Return DIR and all non-symlink child directories."
  (let ((dirs (list (file-name-as-directory (file-truename dir)))))
    (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
      (when (and (file-directory-p entry)
                 (not (file-symlink-p entry))
                 (not (my/org-maintenance--excluded-dir-p entry include-latex-cache)))
        (setq dirs
              (nconc (my/org-maintenance--collect-dirs-recursively
                      entry include-latex-cache)
                     dirs))))
    dirs))

(defun my/org-maintenance--empty-managed-dirs ()
  "Return empty managed media/cache directories in the current scope."
  (let* ((root (my/org-maintenance-scope-root))
         (media-roots
          (my/org-maintenance--directories-named
           root
           (delete-dups
            (append my/org-maintenance-image-directory-names
                    my/org-maintenance-attachment-directory-names))))
         (latex-roots (my/org-maintenance--latex-cache-dirs root))
         dirs)
    (dolist (dir (append media-roots latex-roots))
      (when (file-directory-p dir)
        (setq dirs
              (nconc (my/org-maintenance--collect-dirs-recursively
                      dir
                      (member dir latex-roots))
                     dirs))))
    (setq dirs (delete-dups dirs))
    (setq dirs
          (seq-remove
           (lambda (dir)
             (string= (file-name-as-directory (file-truename dir))
                      (file-name-as-directory (file-truename root))))
           dirs))
    (sort (seq-filter #'my/org-maintenance--directory-empty-p dirs)
          (lambda (a b) (> (length a) (length b))))))

(defun my/org-maintenance-delete-empty-managed-dirs (&optional force)
  "Delete empty managed media/cache directories in the current scope."
  (interactive "P")
  (let ((dirs (my/org-maintenance--empty-managed-dirs))
        (count 0))
    (if (null dirs)
        (message "No empty Org maintenance directories")
      (when (or force
                (yes-or-no-p
                 (format "Delete %d empty Org maintenance directories? "
                         (length dirs))))
        (dolist (dir dirs)
          (when (and (file-directory-p dir)
                     (my/org-maintenance--directory-empty-p dir))
            (delete-directory dir)
            (cl-incf count)))
        (when (> count 0)
          (my/org-maintenance--invalidate-cache))
        (my/org-maintenance--refresh-board-if-visible)
        (message "Deleted %d empty Org maintenance directories" count)))))

(defun my/org-maintenance-clean-managed-artifacts (&optional force)
  "Run the practical cleanup set for the current scope."
  (interactive "P")
  (let* ((report (my/org-maintenance-scan))
         (media (plist-get report :unreferenced-media))
         (latex (plist-get report :latex-prunable-files))
         (files (append media latex))
         (bytes (+ (plist-get report :unreferenced-media-bytes)
                   (plist-get report :latex-prunable-bytes))))
    (if (null files)
        (progn
          (my/org-maintenance-delete-empty-managed-dirs t)
          (message "No Org maintenance artifacts to delete"))
      (when (my/org-maintenance--confirm-delete
             "Delete unreferenced media and old LaTeX cache" files bytes force)
        (let ((count (my/org-maintenance--delete-files files)))
          (my/org-maintenance-delete-empty-managed-dirs t)
          (my/org-maintenance--refresh-board-if-visible)
          (message "Deleted %d Org maintenance artifacts" count))))))

(defun my/org-maintenance-open-root ()
  "Open the current maintenance scope root in Dired."
  (interactive)
  (dired (my/org-maintenance-scope-root)))

(defun my/org-maintenance-open-docs ()
  "Open Org maintenance documentation."
  (interactive)
  (find-file (expand-file-name "docs/maintenance.md" user-emacs-directory)))

(defun my/org-maintenance--ensure-org ()
  "Ensure the current buffer is an Org buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer")))

(defun my/org-maintenance--org-link-at-point ()
  "Return Org link element at point, or nil."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-element-context))
    (let ((context (ignore-errors (org-element-context))))
      (when (eq (ignore-errors (org-element-type context)) 'link)
        context))))

(defun my/org-maintenance--link-description (link)
  "Return LINK description text, or nil."
  (when-let* ((beg (org-element-property :contents-begin link))
              (end (org-element-property :contents-end link)))
    (let ((description (buffer-substring-no-properties beg end)))
      (unless (string-empty-p description)
        description))))

(defun my/org-maintenance--current-file-link-info ()
  "Return a plist describing the current Org file link."
  (my/org-maintenance--ensure-org)
  (let ((link (my/org-maintenance--org-link-at-point)))
    (unless (and link (string= (org-element-property :type link) "file"))
      (user-error "Point is not on an Org file link"))
    (let* ((path (org-element-property :path link))
           (split (my/org-maintenance--split-file-link-path path))
           (clean (and split (car split)))
           (suffix (or (and split (cdr split)) ""))
           (source (or buffer-file-name
                       (expand-file-name "buffer.org" default-directory)))
           (target (my/org-maintenance--resolve-file-link source path)))
      (unless (and clean target)
        (user-error "Cannot resolve this Org file link"))
      (list :link link
            :path path
            :clean clean
            :suffix suffix
            :source source
            :target target
            :description (my/org-maintenance--link-description link)))))

(defun my/org-maintenance--replace-current-link-target (info new-target)
  "Replace current Org file link described by INFO with NEW-TARGET."
  (let* ((link (plist-get info :link))
         (suffix (plist-get info :suffix))
         (description (plist-get info :description))
         (source-dir (file-name-directory (plist-get info :source)))
         (relative (file-relative-name new-target source-dir))
         (new-link (org-link-make-string
                    (concat "file:" (org-link-escape relative) suffix)
                    description))
         (begin (org-element-property :begin link))
         (end (org-element-property :end link)))
    (goto-char begin)
    (delete-region begin end)
    (insert new-link)
    (when (derived-mode-p 'org-mode)
      (ignore-errors (org-redisplay-inline-images)))))

(defun my/org-maintenance--rename-current-link-file-to-name (new-name)
  "Rename the current file link target to NEW-NAME and update the Org link."
  (let* ((info (my/org-maintenance--current-file-link-info))
         (target (expand-file-name (plist-get info :target)))
         (scope (my/org-maintenance-scope-root))
         (old-ext (file-name-extension target))
         (file-name (if (and old-ext
                             (not (file-name-extension new-name)))
                        (concat new-name "." old-ext)
                      new-name))
         (new-target (expand-file-name file-name
                                       (file-name-directory target))))
    (unless (file-regular-p target)
      (user-error "Linked file does not exist: %s" target))
    (when (file-name-directory file-name)
      (user-error "Use a file name, not a path"))
    (unless (my/org-maintenance--inside-dir-p target scope)
      (user-error "Linked file is outside current maintenance scope"))
    (when (string= (expand-file-name target) (expand-file-name new-target))
      (message "Linked file name unchanged")
      (cl-return-from my/org-maintenance--rename-current-link-file-to-name target))
    (when (file-exists-p new-target)
      (user-error "Target name already exists: %s" new-target))
    (rename-file target new-target nil)
    (my/org-maintenance--replace-current-link-target info new-target)
    (my/org-maintenance--invalidate-cache)
    (message "Renamed linked file to %s" (file-name-nondirectory new-target))
    new-target))

(defun my/org-maintenance-rename-link-file (new-name)
  "Rename current Org file link target and update the link text."
  (interactive
   (let* ((info (my/org-maintenance--current-file-link-info))
          (target (plist-get info :target)))
     (list (read-string "New linked file name: "
                        (file-name-nondirectory target)))))
  (when (string-empty-p (string-trim new-name))
    (user-error "Empty file name"))
  (my/org-maintenance--rename-current-link-file-to-name
   (string-trim new-name)))

(defun my/org-maintenance--protect-file-name (file)
  "Return protected filename for FILE by adding the configured prefix."
  (let ((base (file-name-nondirectory file))
        (prefix my/org-maintenance-protected-file-prefix))
    (when (string-empty-p prefix)
      (user-error "Org maintenance protected prefix is empty"))
    (if (string-prefix-p prefix base)
        base
      (concat prefix base))))

(defun my/org-maintenance-protect-link-file ()
  "Rename current Org file link target with the cleanup-exempt prefix."
  (interactive)
  (let* ((info (my/org-maintenance--current-file-link-info))
         (target (plist-get info :target))
         (new-name (my/org-maintenance--protect-file-name target)))
    (if (string= new-name (file-name-nondirectory target))
        (message "Linked file already has cleanup exemption")
      (my/org-maintenance--rename-current-link-file-to-name new-name))))

(defun my/org-maintenance--org-link-open-target ()
  "Return the Org link at point as a system-open target, or nil."
  (when-let* ((link (my/org-maintenance--org-link-at-point)))
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link)))
      (cond
       ((member type '("http" "https"))
        (concat type ":" path))
       ((string= type "file")
        (when-let* ((source (or buffer-file-name
                                (expand-file-name "buffer.org" default-directory)))
                    (target (my/org-maintenance--resolve-file-link source path)))
          target))
       ((and type path
             (not (member type '("custom-id" "fuzzy" "id"))))
        (concat type ":" path))))))

(defun my/org-maintenance--open-target-at-point ()
  "Return a file, URL or Org link target near point."
  (or (when (use-region-p)
        (string-trim
         (buffer-substring-no-properties (region-beginning) (region-end))))
      (my/org-maintenance--org-link-open-target)
      (thing-at-point 'url t)
      (let ((file (thing-at-point 'filename t)))
        (and file (file-exists-p (expand-file-name file)) file))
      buffer-file-name))

(defun my/org-maintenance-open-link-system ()
  "Open the Org link, image file or filename at point with the system opener."
  (interactive)
  (if-let* ((target (my/org-maintenance--open-target-at-point)))
      (if (fboundp 'my/open-system-target)
          (my/open-system-target target)
        (browse-url target))
    (user-error "No link or file target at point")))

(define-derived-mode my/org-maintenance-board-mode special-mode "Org-Maint"
  "Major mode for `my/org-maintenance-board'.")

(defun my/org-maintenance-board--insert-button (label action help)
  "Insert a text button with LABEL, ACTION and HELP."
  (insert-button label
                 'action action
                 'follow-link t
                 'help-echo help)
  (insert " "))

(defun my/org-maintenance-board--insert-openable-path (path &optional dired-p)
  "Insert PATH as a clickable button.
When DIRED-P is non-nil, open PATH with Dired."
  (my/org-maintenance-board--insert-button
   (abbreviate-file-name path)
   (lambda (_button)
     (if dired-p
         (dired path)
       (find-file path)))
   path))

(defun my/org-maintenance-board--insert-value-line (label value)
  "Insert LABEL and VALUE on one board row."
  (insert (format "%-22s %s\n" label value)))

(defun my/org-maintenance-board--insert-path-line (label path &optional dired-p)
  "Insert LABEL and clickable PATH on one board row."
  (insert (format "%-22s " label))
  (my/org-maintenance-board--insert-openable-path path dired-p)
  (insert "\n"))

(defun my/org-maintenance-board--insert-section (title)
  "Insert a board section TITLE."
  (insert "\n" title "\n")
  (insert (make-string (length title) ?-) "\n"))

(defun my/org-maintenance-board--insert-action-line ()
  "Insert board action buttons."
  (my/org-maintenance-board--insert-button
   "[Clean media]"
   (lambda (_button) (my/org-maintenance-clean-unreferenced-media))
   "Delete unreferenced images and attachments")
  (my/org-maintenance-board--insert-button
   "[Clean images]"
   (lambda (_button) (my/org-maintenance-delete-unreferenced-images))
   "Delete unreferenced local images")
  (my/org-maintenance-board--insert-button
   "[Clean attachments]"
   (lambda (_button) (my/org-maintenance-delete-unreferenced-attachments))
   "Delete unreferenced attachments")
  (my/org-maintenance-board--insert-button
   "[Prune LaTeX]"
   (lambda (_button) (my/org-maintenance-prune-latex-cache))
   "Keep only the newest LaTeX preview cache files")
  (my/org-maintenance-board--insert-button
   "[Clear LaTeX]"
   (lambda (_button) (my/org-maintenance-clear-latex-cache))
   "Delete all LaTeX preview cache files in the current scope")
  (my/org-maintenance-board--insert-button
   "[Empty dirs]"
   (lambda (_button) (my/org-maintenance-delete-empty-managed-dirs))
   "Delete empty managed media/cache directories")
  (my/org-maintenance-board--insert-button
   "[All]"
   (lambda (_button) (my/org-maintenance-clean-managed-artifacts))
   "Clean unreferenced media, old LaTeX cache and empty directories")
  (my/org-maintenance-board--insert-button
   "[Docs]"
   (lambda (_button) (my/org-maintenance-open-docs))
   "Open Org maintenance docs")
  (insert "\n"))

(defun my/org-maintenance-board--insert-file-list (files &optional limit)
  "Insert FILES with sizes.
LIMIT defaults to `my/org-maintenance-board-preview-limit'."
  (let* ((limit (or limit my/org-maintenance-board-preview-limit))
         (shown (seq-take files limit)))
    (if (null files)
        (insert "  none\n")
      (dolist (file shown)
        (insert (format "%9s  " (my/org-maintenance--format-bytes
                                 (my/org-maintenance--file-size file))))
        (my/org-maintenance-board--insert-openable-path file)
        (insert "\n"))
      (when (> (length files) limit)
        (insert (format "  ... %d more\n" (- (length files) limit)))))))

(defun my/org-maintenance-board--insert-broken-links (links &optional limit)
  "Insert broken media LINKS with source files."
  (let* ((limit (or limit my/org-maintenance-board-preview-limit))
         (shown (seq-take links limit)))
    (if (null links)
        (insert "  none\n")
      (dolist (link shown)
        (let ((source (plist-get link :source))
              (target (plist-get link :target))
              (kind (plist-get link :kind))
              (error (plist-get link :error)))
          (insert "  ")
          (if source
              (my/org-maintenance-board--insert-openable-path source)
            (insert "<unknown>"))
          (insert (format " -> %s"
                          (or (and kind (symbol-name kind)) "link")))
          (insert ": ")
          (insert (or (and target (abbreviate-file-name target))
                      error
                      "<unresolved>"))
          (insert "\n")))
      (when (> (length links) limit)
        (insert (format "  ... %d more\n" (- (length links) limit)))))))

(defun my/org-maintenance-board-refresh (&optional force)
  "Refresh the maintenance board.
With FORCE, ignore the scan cache."
  (interactive "P")
  (let* ((inhibit-read-only t)
         (report (my/org-maintenance-scan force))
         (root (plist-get report :root))
         (org-files (plist-get report :org-files))
         (media (plist-get report :media-files))
         (images (plist-get report :image-files))
         (attachments (plist-get report :attachment-files))
         (protected (plist-get report :protected-media))
         (unreferenced (plist-get report :unreferenced-media))
         (unreferenced-images (plist-get report :unreferenced-images))
         (unreferenced-attachments (plist-get report :unreferenced-attachments))
         (broken (plist-get report :broken-media-links))
         (latex-dirs (plist-get report :latex-cache-dirs))
         (latex-files (plist-get report :latex-cache-files))
         (latex-prunable (plist-get report :latex-prunable-files)))
    (setq-local my/org-maintenance-board--report report)
    (erase-buffer)
    (insert "Org Maintenance Board\n")
    (insert "=====================\n")
    (insert "Keys: g refresh, m clean media, i images, x attachments, l prune latex, L clear latex, e empty dirs, a all, o root, d docs\n")

    (my/org-maintenance-board--insert-section "Overview")
    (my/org-maintenance-board--insert-path-line "Scope root" root t)
    (my/org-maintenance-board--insert-value-line
     "Report cache"
     (if (plist-get report :cache-hit) "reused" "fresh"))
    (my/org-maintenance-board--insert-value-line
     "Org files scanned" (number-to-string (length org-files)))
    (my/org-maintenance-board--insert-value-line
     "Image dirs" (string-join my/org-maintenance-image-directory-names ", "))
    (my/org-maintenance-board--insert-value-line
     "Attachment dirs"
     (string-join my/org-maintenance-attachment-directory-names ", "))
    (my/org-maintenance-board--insert-value-line
     "Exempt names"
     (format "prefix %s" my/org-maintenance-protected-file-prefix))
    (my/org-maintenance-board--insert-value-line
     "Media candidates"
     (format "%d (%s)"
             (length media)
             (my/org-maintenance--format-bytes
              (plist-get report :media-bytes))))
    (my/org-maintenance-board--insert-value-line
     "Images"
     (format "%d (%s)"
             (length images)
             (my/org-maintenance--format-bytes
              (plist-get report :image-bytes))))
    (my/org-maintenance-board--insert-value-line
     "Attachments"
     (format "%d (%s)"
             (length attachments)
             (my/org-maintenance--format-bytes
              (plist-get report :attachment-bytes))))
    (my/org-maintenance-board--insert-value-line
     "Protected media"
     (format "%d (%s)"
             (length protected)
             (my/org-maintenance--format-bytes
              (plist-get report :protected-media-bytes))))
    (my/org-maintenance-board--insert-value-line
     "Unreferenced media"
     (format "%d (%s)"
             (length unreferenced)
             (my/org-maintenance--format-bytes
              (plist-get report :unreferenced-media-bytes))))
    (my/org-maintenance-board--insert-value-line
     "Broken media links" (number-to-string (length broken)))
    (my/org-maintenance-board--insert-value-line
     "LaTeX cache"
     (format "%d files in %d dirs (%s)"
             (length latex-files)
             (length latex-dirs)
             (my/org-maintenance--format-bytes
              (plist-get report :latex-cache-bytes))))
    (my/org-maintenance-board--insert-value-line
     "LaTeX prunable"
     (format "%d (%s), keeping newest %d per dir"
             (length latex-prunable)
             (my/org-maintenance--format-bytes
              (plist-get report :latex-prunable-bytes))
             my/org-maintenance-latex-cache-max-files-per-dir))

    (my/org-maintenance-board--insert-section "Actions")
    (my/org-maintenance-board--insert-action-line)

    (my/org-maintenance-board--insert-section "Unreferenced Images")
    (my/org-maintenance-board--insert-file-list unreferenced-images)

    (my/org-maintenance-board--insert-section "Unreferenced Attachments")
    (my/org-maintenance-board--insert-file-list unreferenced-attachments)

    (my/org-maintenance-board--insert-section "Protected Media")
    (my/org-maintenance-board--insert-file-list protected)

    (my/org-maintenance-board--insert-section "Broken Local Media Links")
    (my/org-maintenance-board--insert-broken-links broken)

    (my/org-maintenance-board--insert-section "LaTeX Cache Dirs")
    (if (null latex-dirs)
        (insert "  none\n")
      (dolist (dir latex-dirs)
        (insert "  ")
        (my/org-maintenance-board--insert-openable-path dir t)
        (insert "\n")))
    (goto-char (point-min))))

(defun my/org-maintenance-board ()
  "Open the Org maintenance board."
  (interactive)
  (let ((buffer (get-buffer-create my/org-maintenance-board-buffer-name)))
    (with-current-buffer buffer
      (my/org-maintenance-board-mode)
      (unless (local-key-binding (kbd "g"))
        (local-set-key (kbd "g") #'my/org-maintenance-board-refresh)
        (local-set-key (kbd "m") #'my/org-maintenance-clean-unreferenced-media)
        (local-set-key (kbd "i") #'my/org-maintenance-delete-unreferenced-images)
        (local-set-key (kbd "x") #'my/org-maintenance-delete-unreferenced-attachments)
        (local-set-key (kbd "l") #'my/org-maintenance-prune-latex-cache)
        (local-set-key (kbd "L") #'my/org-maintenance-clear-latex-cache)
        (local-set-key (kbd "e") #'my/org-maintenance-delete-empty-managed-dirs)
        (local-set-key (kbd "a") #'my/org-maintenance-clean-managed-artifacts)
        (local-set-key (kbd "o") #'my/org-maintenance-open-root)
        (local-set-key (kbd "d") #'my/org-maintenance-open-docs))
      (my/org-maintenance-board-refresh))
    (pop-to-buffer buffer)))

(provide 'init-org-maintenance)
;;; init-org-maintenance.el ends here

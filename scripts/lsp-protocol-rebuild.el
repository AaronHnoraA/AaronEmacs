;;; lsp-protocol-rebuild.el --- Rebuild installed LSP clients for plists -*- lexical-binding: t; -*-

;; Run through `make lsp-protocol-rebuild'.  lsp-protocol expands accessors at
;; compile time, so every client sharing those macros must use the same mode.

(require 'cl-lib)
(require 'package)

(defconst aaron-lsp-protocol-client-packages
  '(lsp-mode lsp-ui lsp-docker lsp-java lsp-treemacs sideline-lsp
    dap-mode lean4-mode elsa)
  "Installed packages whose bytecode uses or loads LSP protocol objects.")

(defun aaron-lsp-protocol--package-directory (name)
  "Return the active installation directory for package NAME."
  (let* ((descriptors (alist-get name package-alist))
         (descriptor (car descriptors))
         (directory (and descriptor (package-desc-dir descriptor))))
    (unless (and directory (file-directory-p directory))
      (error "LSP client package %s is not installed" name))
    directory))

(defun aaron-lsp-protocol--native-artifacts (source)
  "Return existing native artifacts compiled from SOURCE."
  (when (and (require 'comp nil t)
             (fboundp 'comp-el-to-eln-filename))
    (cl-remove-if-not
     #'file-exists-p
     (cons (comp-el-to-eln-filename source)
           (when (file-exists-p (concat source "c"))
             (list (comp-el-to-eln-filename (concat source "c"))))))))

(defun aaron-lsp-protocol-rebuild ()
  "Recompile installed LSP packages for `LSP_USE_PLISTS=1'.
Back up their bytecode and native artifacts first; restore them on failure.
Run in a fresh batch Emacs before loading `lsp-protocol'."
  (interactive)
  (unless (equal (getenv "LSP_USE_PLISTS") "1")
    (error "LSP_USE_PLISTS must be 1 before package compilation"))
  (when (featurep 'lsp-protocol)
    (error "Run LSP protocol rebuild in a fresh batch Emacs"))
  (let* ((directories (mapcar #'aaron-lsp-protocol--package-directory
                              aaron-lsp-protocol-client-packages))
         (sources (cl-mapcan
                   (lambda (directory)
                     (directory-files-recursively directory "\\.el\\'"))
                   directories))
         (compiled-sources
          (cl-remove-if-not
           (lambda (source) (file-exists-p (concat source "c"))) sources))
         (artifacts
          (delete-dups
           (append (mapcar (lambda (source) (concat source "c"))
                           compiled-sources)
                   (cl-mapcan #'aaron-lsp-protocol--native-artifacts sources))))
         (backup (make-temp-file "lsp-protocol-rebuild-" t))
         (saved nil)
         (restore-needed nil)
         (succeeded nil))
    (unwind-protect
        (progn
          (cl-loop for artifact in artifacts
                   for index from 0
                   for copy = (expand-file-name (number-to-string index) backup)
                   do (copy-file artifact copy)
                   do (push (cons artifact copy) saved))
          (setq restore-needed t)
          (dolist (artifact artifacts)
            (delete-file artifact))
          ;; Source must be loaded after the old bytecode and ELN are gone.
          (let ((native-comp-jit-compilation nil)
                (native-comp-deferred-compilation nil))
            (load (expand-file-name "lsp-protocol.el" (car directories)) nil t)
            (unless (and lsp-use-plists
                         (listp (lsp-make-completion-item :label "probe")))
              (error "lsp-protocol did not load in plist mode"))
            (dolist (directory directories)
              (message "Recompiling LSP client %s" directory)
              (byte-recompile-directory directory 0 t)))
          (dolist (source compiled-sources)
            (unless (file-exists-p (concat source "c"))
              (error "LSP client bytecode was not rebuilt: %s" source)))
          (setq succeeded t)
          (message "Rebuilt %d LSP client packages for plist objects"
                   (length directories)))
      (when (and restore-needed (not succeeded))
        (dolist (source sources)
          (let ((bytecode (concat source "c")))
            (when (file-exists-p bytecode)
              (delete-file bytecode)))
          (dolist (native (aaron-lsp-protocol--native-artifacts source))
            (delete-file native)))
        (dolist (entry saved)
          (copy-file (cdr entry) (car entry) t)))
      (delete-directory backup t))))

(provide 'lsp-protocol-rebuild)
;;; lsp-protocol-rebuild.el ends here

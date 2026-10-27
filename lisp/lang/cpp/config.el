;;; lang/cpp/config.el --- C/C++ config (lsp-mode + clangd) -*- lexical-binding: t -*-

;;; Commentary:
;; C/C++ development environment
;; - clangd through lsp-mode
;; - tree-sitter support
;; - cmake support
;; - snippets

;;; Code:

(require 'aaron-funcs)
(require 'cl-lib)

(declare-function set-language-server! "tools/lsp/config")
(declare-function +language-server-executable-find "tools/lsp/config" (program))
(declare-function +eshell-run-build "term/shell/eshell" (build-command &rest keys))
(declare-function +eshell-build-root "term/shell/eshell" (source-directory markers))
(declare-function +eshell-build-output-file "term/shell/eshell" (source))
(defvar lsp-enabled-clients)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find sibling files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package files
  :ensure nil
  :config
  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) (or ".hpp" ".cpp" ".cc") eos)
                 "\\1_test.cpp" "\\1_test.cc"
                 "\\1_benchmark.cpp" "\\1_benchmark.cc"))

  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) "_test" (or ".cpp" ".cc") eos)
                 "\\1.cpp" "\\1.cc" "\\1.hpp"))

  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) "_benchmark" (or ".cpp" ".cc") eos)
                 "\\1.cpp" "\\1.cc" "\\1.hpp"))

  ;; From Doom: a source file and its header in the same directory.
  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) "." (or "c" "cc" "cpp" "cxx") eos)
                 "\\1\\.\\(?:h\\|hh\\|hpp\\|hxx\\)\\'"))
  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) "." (or "h" "hh" "hpp" "hxx") eos)
                 "\\1\\.\\(?:c\\|cc\\|cpp\\|cxx\\)\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cc-mode
  :ensure nil
  :mode (("\\.cxx\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode))

  :hook
  (c-mode . (lambda ()
              (setq comment-start "// "
                    comment-end "")))

  :custom
  (c-basic-offset 2)
  (tab-width 2)
  (indent-tabs-mode nil))

;; A small build/run path for ordinary C and C++ files.  It uses the shared
;; Eshell task API; project-specific choices stay with the language config.
(defgroup +cpp-eshell-build nil
  "C and C++ builds sent to a named Eshell session."
  :group 'languages)

(defcustom +cpp-eshell-build-command nil
  "Project build command, or nil to use make/compile the current source.
Set this in a trusted .dir-locals.el when a project needs a specific target."
  :type '(choice (const :tag "Automatic" nil) string)
  :group '+cpp-eshell-build)

(defcustom +cpp-eshell-run-command nil
  "Project run command after a successful build, or nil for single-file output.
A Makefile project's executable cannot be inferred safely; set this when
enabling run-after-build for that project."
  :type '(choice (const :tag "Automatic single file" nil) string)
  :group '+cpp-eshell-build)

(defcustom +cpp-eshell-run-after-build nil
  "If non-nil, `C-c C-c' builds and then runs C/C++ programs.
A prefix argument also requests a run for one invocation."
  :type 'boolean
  :group '+cpp-eshell-build)

(defcustom +cpp-eshell-c-compiler "cc"
  "Compiler for a standalone C source file."
  :type 'string
  :group '+cpp-eshell-build)

(defcustom +cpp-eshell-cxx-compiler "c++"
  "Compiler for a standalone C++ source file."
  :type 'string
  :group '+cpp-eshell-build)

(defun +cpp-eshell--makefile-p (directory)
  "Return non-nil if DIRECTORY contains a makefile."
  (cl-some (lambda (name)
             (file-regular-p (expand-file-name name directory)))
           '("Makefile" "makefile" "GNUmakefile")))

(defun +cpp/eshell-build (&optional run)
  "Build this C/C++ file or Makefile project in its named Eshell.
With prefix RUN, also execute the program after a successful build."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Visit a C or C++ source file before building"))
  (when (file-remote-p buffer-file-name)
    (user-error "This C/C++ build demo currently supports local files only"))
  (require 'term/shell/eshell)
  (save-buffer)
  (let* ((source buffer-file-name)
         (source-directory (file-name-directory source))
         (root (+eshell-build-root source-directory
                                  '("Makefile" "makefile" "GNUmakefile")))
         (make-project (+cpp-eshell--makefile-p root))
         (binary (+eshell-build-output-file source))
         (compiler (if (memq major-mode '(c++-mode c++-ts-mode))
                       +cpp-eshell-cxx-compiler
                     +cpp-eshell-c-compiler))
         (build (or +cpp-eshell-build-command
                    (if make-project
                        "make"
                      (format "%s %s -o %s"
                              compiler
                              (shell-quote-argument
                               (file-name-nondirectory source))
                              (shell-quote-argument binary)))))
         (run-command
          (when (or run +cpp-eshell-run-after-build)
            (or +cpp-eshell-run-command
                (unless (or make-project +cpp-eshell-build-command)
                  (shell-quote-argument (concat "./" binary)))))))
    (when (and (or run +cpp-eshell-run-after-build)
               (not run-command))
      (user-error "Set +cpp-eshell-run-command for this project"))
    (+eshell-run-build build :directory root :run run-command)))

(defun +cpp-eshell-build-setup-h ()
  "Make C-c C-c the direct C/C++ project build entry."
  (local-set-key (kbd "C-c C-c") #'+cpp/eshell-build))

(dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
  (add-hook (intern (format "%s-hook" mode)) #'+cpp-eshell-build-setup-h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +c-ts-indent-4 ()
  "Use 2 space indentation in c-ts-mode."
  (setq-local c-ts-mode-indent-offset 2)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "RET") #'newline-and-indent))

(add-hook 'c-ts-mode-hook #'+c-ts-indent-4)
(add-hook 'c++-ts-mode-hook #'+c-ts-indent-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clangd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +cpp-clangd-command ()
  "Return the clangd command for the active target.
Resolving through `+language-server-executable-find' keeps the binary on
the same target that will run it; a bare \"clangd\" would be looked up on
the client."
  (list (or (+language-server-executable-find "clangd") "clangd")
        "-j=2"
        "--background-index"
        "--clang-tidy"
        "--completion-style=bundled"
        "--header-insertion-decorators"))

(defun +cpp-language-server-setup-h ()
  "Keep the target-aware clangd client authoritative in C-family buffers."
  (setq-local lsp-enabled-clients '(+clangd)))

(add-hook 'c-mode-common-hook #'+cpp-language-server-setup-h)
(add-hook 'c-ts-mode-hook #'+cpp-language-server-setup-h)
(add-hook 'c++-ts-mode-hook #'+cpp-language-server-setup-h)

(with-eval-after-load 'lsp-mode
  (when (fboundp 'set-language-server!)
    ;; A higher priority than lsp-mode's stock clangd client so these
    ;; arguments stay authoritative.
    (set-language-server!
     '(c-mode c++-mode c-ts-mode c++-ts-mode objc-mode)
     #'+cpp-clangd-command
     :server-id '+clangd
     :upstream-id 'clangd
     :priority 1
     :label "clangd"
     :executables '("clangd")
     :note "C/C++ buffers use clangd through lsp-mode.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler explorer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rmsbolt
  :ensure t
  :commands rmsbolt-compile
  :custom
  (rmsbolt-asm-format nil)
  (rmsbolt-default-directory temporary-file-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bison / Flex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package bison-mode
  :ensure t
  :mode (("\\.l\\'" . flex-mode)
         ("\\.y\\'" . bison-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLVM IR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (locate-library "llvm-mode")
  ;; Some Emacs 31 snapshots no longer ship this optional mode.  Avoid a
  ;; `use-package' compile-time require so byte compilation still works when
  ;; the library is absent from that particular build.
  (autoload 'llvm-mode "llvm-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TableGen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (locate-library "tablegen-mode")
  (autoload 'tablegen-mode "tablegen-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.td\\'" . tablegen-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets (tempo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tempo
  :ensure nil
  :after cc-mode
  :hook ((c-mode . c-mode-tempo-setup)
         (c++-mode . c++-mode-tempo-setup))

  :config
  (defvar c-tempo-tags nil)
  (defvar c++-tempo-tags nil)

  (defun c-mode-tempo-setup ()
    (tempo-use-tag-list 'c-tempo-tags))

  (defun c++-mode-tempo-setup ()
    (tempo-use-tag-list 'c-tempo-tags)
    (tempo-use-tag-list 'c++-tempo-tags))

  ;; main()
  (tempo-define-template
   "c-main"
   '("int main(int argc, char* argv[]) {" > n>
     p n
     "}" > n>)
   "main"
   "Insert main function"
   'c-tempo-tags)

  ;; #ifndef
  (tempo-define-template
   "c-ifndef"
   '("#ifndef " (P "Macro: " clause) > n
     "#define " (s clause) n> p n
     "#endif // " (s clause) n>)
   "ifndef"
   "Header guard"
   'c-tempo-tags)

  ;; extern C
  (tempo-define-template
   "c-extern-C"
   '("#ifdef __cplusplus" n
     "extern \"C\" {" n
     "#endif" n
     p n
     "#ifdef __cplusplus" n
     "}" n
     "#endif" n)
   "externC"
   "extern C block"
   'c-tempo-tags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun +cpp-indent-setup-h ()
  (setq-local c-basic-offset 2)
  (setq-local c-ts-mode-indent-offset 2)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "RET") #'newline-and-indent))

(add-hook 'c-mode-common-hook #'+cpp-indent-setup-h)
(add-hook 'c-ts-mode-hook #'+cpp-indent-setup-h)
(add-hook 'c++-ts-mode-hook #'+cpp-indent-setup-h)





(provide 'lang/cpp/config)

;;; lang/cpp/config.el ends here

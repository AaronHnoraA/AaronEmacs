;;; init-cpp.el --- C/C++ config (lsp-mode + clangd) -*- lexical-binding: t -*-

;;; Commentary:
;; C/C++ development environment
;; - clangd through lsp-mode
;; - tree-sitter support
;; - cmake support
;; - snippets

;;; Code:

(require 'init-funcs)

(declare-function my/register-language-server "init-lsp")
(declare-function my/language-server-executable-find "init-lsp" (program))
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
                 "\\1.cpp" "\\1.cc" "\\1.hpp")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/c-ts-indent-4 ()
  "Use 2 space indentation in c-ts-mode."
  (setq-local c-ts-mode-indent-offset 2)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "RET") #'newline-and-indent))

(add-hook 'c-ts-mode-hook #'my/c-ts-indent-4)
(add-hook 'c++-ts-mode-hook #'my/c-ts-indent-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clangd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/cpp-clangd-command ()
  "Return the clangd command for the active target.
Resolving through `my/language-server-executable-find' keeps the binary on
the same target that will run it; a bare \"clangd\" would be looked up on
the client."
  (list (or (my/language-server-executable-find "clangd") "clangd")
        "-j=2"
        "--background-index"
        "--clang-tidy"
        "--completion-style=bundled"
        "--header-insertion-decorators"))

(defun my/cpp-language-server-setup-h ()
  "Keep the target-aware clangd client authoritative in C-family buffers."
  (setq-local lsp-enabled-clients '(my-clangd)))

(add-hook 'c-mode-common-hook #'my/cpp-language-server-setup-h)
(add-hook 'c-ts-mode-hook #'my/cpp-language-server-setup-h)
(add-hook 'c++-ts-mode-hook #'my/cpp-language-server-setup-h)

(with-eval-after-load 'lsp-mode
  (when (fboundp 'my/register-language-server)
    ;; A higher priority than lsp-mode's stock clangd client so these
    ;; arguments stay authoritative.
    (my/register-language-server
     '(c-mode c++-mode c-ts-mode c++-ts-mode objc-mode)
     #'my/cpp-clangd-command
     :server-id 'my-clangd
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



(defun my-c-unified-indent-setup ()
  (setq-local c-basic-offset 2)
  (setq-local c-ts-mode-indent-offset 2)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "RET") #'newline-and-indent))

(add-hook 'c-mode-common-hook #'my-c-unified-indent-setup)
(add-hook 'c-ts-mode-hook #'my-c-unified-indent-setup)
(add-hook 'c++-ts-mode-hook #'my-c-unified-indent-setup)





(provide 'init-cpp)

;;; init-cpp.el ends here

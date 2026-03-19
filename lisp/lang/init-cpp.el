;;; init-cpp.el --- C/C++ config (eglot + clangd) -*- lexical-binding: t -*-

;;; Commentary:
;; C/C++ development environment
;; - clangd + eglot
;; - tree-sitter support
;; - cmake support
;; - snippets

;;; Code:

(require 'init-funcs)

(declare-function my/eglot-ensure "init-lsp")
(declare-function my/register-eglot-server-program "init-lsp" (modes program &rest props))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find sibling files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package files
  :ensure nil
  :config
  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) (or ".hpp" ".cpp") eos)
                 "\\1_test.cpp" "\\1_benchmark.cpp"))

  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) "_test.cpp" eos)
                 "\\1.cpp" "\\1.hpp"))

  (add-to-list 'find-sibling-rules
               `(,(rx (group (+ (not "/"))) "_benchmark.cpp" eos)
                 "\\1.cpp" "\\1.hpp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cc-mode
  :ensure nil
  :mode ("\\.cxx\\'" . c++-mode)

  :hook
  (c-mode . (lambda ()
              (setq comment-start "// "
                    comment-end "")))

  :custom
  (c-basic-offset 4)
  (tab-width 4)
  (indent-tabs-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/c-ts-indent-4 ()
  "Use 4 space indentation in c-ts-mode."
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "RET") #'newline-and-indent))

(add-hook 'c-ts-mode-hook #'my/c-ts-indent-4)
(add-hook 'c++-ts-mode-hook #'my/c-ts-indent-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot + clangd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :hook ((c-mode . my/eglot-ensure)
         (c++-mode . my/eglot-ensure)
         (c-ts-mode . my/eglot-ensure)
         (c++-ts-mode . my/eglot-ensure))
  :config
  (my/register-eglot-server-program
   '(c-mode c++-mode c-ts-mode c++-ts-mode)
   '("clangd"
     "-j=2"
     "--background-index"
     "--clang-tidy"
     "--completion-style=bundled"
     "--header-insertion-decorators")
   :label "clangd"
   :executables '("clangd")
   :note "C/C++ buffers use clangd through Eglot."))

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

(use-package llvm-mode
  :ensure nil
  :mode ("\\.ll\\'" . llvm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TableGen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tablegen-mode
  :ensure nil
  :mode ("\\.td\\'" . tablegen-mode))

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
  (setq-local c-basic-offset 4)
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "RET") #'newline-and-indent))

(add-hook 'c-mode-common-hook #'my-c-unified-indent-setup)
(add-hook 'c-ts-mode-hook #'my-c-unified-indent-setup)
(add-hook 'c++-ts-mode-hook #'my-c-unified-indent-setup)





(provide 'init-cpp)

;;; init-cpp.el ends here

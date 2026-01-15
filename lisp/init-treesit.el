;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; ========== Tree-sitter grammars sources ==========
(setq treesit-language-source-alist
      '((c   "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-python")))

;; 建议显式指定 Emacs 放 grammar 动态库的位置（与你报错里一致）
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter/" user-emacs-directory)))

(provide 'init-treesit)



;;; init-base.el ends here

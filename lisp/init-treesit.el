;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'seq)

;; ========== Tree-sitter grammars sources ==========
(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (c          "https://github.com/tree-sitter/tree-sitter-c")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (java       "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; 建议显式指定 Emacs 放 grammar 动态库的位置（与你报错里一致）
(setq treesit-extra-load-path
      (list (or (and (boundp 'my/treesit-state-dir) my/treesit-state-dir)
                (expand-file-name "var/tree-sitter/" user-emacs-directory))))


(my/package-ensure-vc 'treesit-fold "https://github.com/emacs-tree-sitter/treesit-fold.git")

(use-package treesit-fold
  :commands (treesit-fold-mode treesit-fold-toggle))





;; treesit-auto, solve treesit issues
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)          ;; 关键：不要在打开文件时提示/安装
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; Keep Markdown on classic `markdown-mode`; its UI/customizations are better
  ;; aligned with this config than `markdown-ts-mode`.
  (setq auto-mode-alist
        (cl-remove-if
         (lambda (entry)
           (equal entry '("\\.md\\'" . markdown-ts-mode)))
         auto-mode-alist))
  (global-treesit-auto-mode -1))      ;; 打开时不接管

(defconst my/treesit-major-mode-remap-candidates
  '((sh-mode . (bash-ts-mode . bash))
    (c-mode . (c-ts-mode . c))
    (c++-mode . (c++-ts-mode . cpp))
    (css-mode . (css-ts-mode . css))
    (go-mode . (go-ts-mode . go))
    (html-mode . (html-ts-mode . html))
    (java-mode . (java-ts-mode . java))
    (js-mode . (js-ts-mode . javascript))
    (js2-mode . (js-ts-mode . javascript))
    (json-mode . (json-ts-mode . json))
    (python-mode . (python-ts-mode . python))
    (rust-mode . (rust-ts-mode . rust))
    (typescript-mode . (typescript-ts-mode . typescript))
    (conf-toml-mode . (toml-ts-mode . toml))
    (yaml-mode . (yaml-ts-mode . yaml)))
  "Major modes to remap to tree-sitter variants when the grammar is ready.")

(defun my/treesit-ready-mode-p (ts-mode language)
  "Return non-nil when TS-MODE and LANGUAGE are ready for use."
  (and (fboundp ts-mode)
       (fboundp 'treesit-ready-p)
       (ignore-errors
         (treesit-ready-p language t))))

(defun my/treesit-supported-major-mode-remaps ()
  "Return supported entries for `major-mode-remap-alist'."
  (let (result)
    (dolist (entry my/treesit-major-mode-remap-candidates (nreverse result))
      (let* ((base (car entry))
             (spec (cdr entry))
             (ts-mode (car spec))
             (language (cdr spec)))
        (when (my/treesit-ready-mode-p ts-mode language)
          (push (cons base ts-mode) result))))))

;; 优先使用已经安装 grammar 的 ts-mode，没装好就留在原 mode。
(setq major-mode-remap-alist
      (my/treesit-supported-major-mode-remaps))

(jit-lock-mode 1)
(font-lock-mode 1)

(setq idle-update-delay 5.0)
;; 或更保守
(setq treesit-font-lock-level 3)     ;; 还卡就 2
;; 关键：避免 refontify 时先把整段清空造成“白一下”
;(setq jit-lock-antiblink t)
;; 你可以同时保持即时刷新（不积攒）
;(setq jit-lock-defer-time 0)
;; 可选：关掉 stealth，减少后台偷偷重染造成的跳变
;(setq jit-lock-stealth-time nil)
;(setq jit-lock-chunk-size 2000)
;(setq jit-lock-context-time 0.1)


(provide 'init-treesit)



;;; init-base.el ends here

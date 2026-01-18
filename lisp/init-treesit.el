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


(unless (package-installed-p 'treesit-fold)
  (package-vc-install
   '(treesit-fold
     :url "https://github.com/emacs-tree-sitter/treesit-fold.git"
     :rev :last-release)))

(use-package treesit-fold)





;; treesit-auto, solve treesit issues
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)          ;; 关键：不要在打开文件时提示/安装
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode -1))      ;; 打开时不接管

(defcustom pv/treesit-idle-delay 0.8
  "Seconds to wait (idle) before switching to *-ts-mode."
  :type 'number)

(defun pv/treesit--switch-current-buffer ()
  "Switch current buffer to its *-ts-mode if treesit-auto can apply."
  (when (and buffer-file-name
             (not (minibufferp))
             ;; 避免对特别小/特殊 buffer 折腾，你也可按需加条件
             (fboundp 'treesit-auto--maybe-apply))
    ;; treesit-auto 的核心切换逻辑：能切就切，不能切就不动
    (treesit-auto--maybe-apply)))

(defun pv/treesit-switch-after-open ()
  "Schedule treesit switch after opening a file."
  (let ((buf (current-buffer)))
    (run-with-idle-timer
     pv/treesit-idle-delay nil
     (lambda (b)
       (when (buffer-live-p b)
         (with-current-buffer b
           (pv/treesit--switch-current-buffer))))
     buf)))

(add-hook 'find-file-hook #'pv/treesit-switch-after-open)

;; 也就是把 python-mode 重新映射到 python-ts-mode
(setq major-mode-remap-alist
 '((python-mode . python-ts-mode)
   (js-mode . js-ts-mode)
   (css-mode . css-ts-mode)
   (c-mode . c-ts-mode)
   (c++-mode . cpp-ts-mode)
   ;; 添加其他你需要映射的语言
   ))

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

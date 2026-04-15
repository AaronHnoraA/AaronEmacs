;;; init-snippets.el --- yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:
;; - 所有 prog-mode 自动启用 yas-minor-mode
;; - TAB 不给 yas 用（缩进 / company 用）
;; - `C-c y` 是 snippet 前缀
;; - `C-c y y` 展开 snippet
;; - `C-c y i` 打开 snippet 菜单

;;; Code:

(declare-function yas-activate-extra-mode "yasnippet" (mode))
(declare-function yas-reload-all "yasnippet" (&optional no-jit interactive))
(declare-function yas-next-field "yasnippet" (&optional arg))
(declare-function yas-prev-field "yasnippet" (&optional arg))

(defconst my/yas-treesit-extra-modes
  '((bash-ts-mode sh-mode)
    (c-ts-mode c-mode)
    (c++-ts-mode c++-mode)
    (css-ts-mode css-mode)
    (go-ts-mode go-mode)
    (html-ts-mode html-mode)
    (java-ts-mode java-mode)
    (js-ts-mode js-mode js2-mode)
    (json-ts-mode json-mode)
    (markdown-ts-mode markdown-mode)
    (python-ts-mode python-mode)
    (rust-ts-mode rust-mode)
    (toml-ts-mode conf-toml-mode)
    (typescript-ts-mode typescript-mode)
    (yaml-ts-mode yaml-mode))
  "Snippet parent modes reused by tree-sitter major modes.")

(defun my/yas-setup-auctex-extra-modes ()
  "Make AUCTeX buffers reuse `latex-mode' and `tex-mode' snippets."
  (yas-activate-extra-mode 'latex-mode)
  (yas-activate-extra-mode 'tex-mode))

(defun my/yas-setup-treesit-extra-modes ()
  "Make tree-sitter buffers reuse snippets from their original major modes."
  (when-let* ((extra-modes (alist-get major-mode my/yas-treesit-extra-modes)))
    (dolist (mode (if (listp extra-modes) extra-modes (list extra-modes)))
      (yas-activate-extra-mode mode))))

(defun my/yas-org-cleanup-trailing-newline ()
  "Silently delete a trailing newline left by a snippet at point.
Replicates the per-snippet `inhibit-modification-hooks' cleanup that was
previously inlined in every org-mode snippet file."
  (save-excursion
    (when (and (not (eobp))
               (eq (char-after) ?\n))
      (let ((inhibit-modification-hooks t))
        (ignore-errors (delete-char 1))))))

(defun my/yas-setup-org-behavior ()
  "Keep Org snippet expansion conservative around indentation and newlines."
  (setq-local yas-indent-line 'fixed)
  (setq-local yas-also-indent-empty-lines nil)
  ;; After each snippet exits, clean up any trailing newline it may have left.
  ;; This replicates the per-snippet inline lisp that was removed from snippet
  ;; files, while keeping the inhibit-modification-hooks suppression centralized.
  (add-hook 'yas-after-exit-snippet-hook
            #'my/yas-org-cleanup-trailing-newline nil t))

(use-package yasnippet
  :ensure t
  :defer 2
  :commands (yas-expand yas-insert-snippet yas-new-snippet yas-visit-snippet-file)
  :init
  ;; 你自己的 snippets 目录：~/.emacs.d/snippets
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)
   (org-mode . my/yas-setup-org-behavior)
   (yas-minor-mode . my/yas-setup-treesit-extra-modes)
   (LaTeX-mode . my/yas-setup-auctex-extra-modes)
   (plain-TeX-mode . my/yas-setup-auctex-extra-modes))
  :config
  (yas-reload-all)

  (with-eval-after-load 'yasnippet
    ;; snippet 会话中的 field 跳转
    (define-key yas-keymap (kbd "M-[") #'yas-prev-field)
    ;; 不让 yas 抢 TAB
    (define-key yas-keymap (kbd "TAB") nil)
    (define-key yas-keymap (kbd "<tab>") nil)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :defer t
  :config
  ;; 把包自带 snippets 也加入
  (add-to-list 'yas-snippet-dirs
               (expand-file-name
                "snippets"
                (file-name-directory
                 (locate-library "yasnippet-snippets"))))
  (yas-reload-all))

;; 全局 snippet 前缀，避免覆盖 `rg' 默认的 `C-c s' 搜索入口。
(define-prefix-command 'my/snippet-map)
(global-set-key (kbd "C-c y") #'my/snippet-map)
(keymap-set my/snippet-map "y" #'yas-expand)
(keymap-set my/snippet-map "i" #'yas-insert-snippet)
(keymap-set my/snippet-map "n" #'yas-new-snippet)
(keymap-set my/snippet-map "v" #'yas-visit-snippet-file)

(provide 'init-snippets)
;;; init-snippets.el ends here

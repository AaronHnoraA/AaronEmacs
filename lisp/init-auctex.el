;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;; =========================
;; AUCTeX 基础配置
;; =========================

(use-package tex
  :ensure auctex
  :defer t
  :hook
  (LaTeX-mode . LaTeX-math-mode)   ;; 快速输入数学符号
  (LaTeX-mode . turn-on-reftex)    ;; 引用、label、ref
  :config
  ;; 使用 PDFLaTeX / XeLaTeX
  (setq TeX-engine 'xetex)

  ;; 保存后自动解析文档（非常重要）
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; PDF 自动刷新
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")))

  ;; 编译时不烦你
  (setq TeX-interactive-mode t)

  ;; 让 \item 自动对齐
  (setq LaTeX-item-indent 0))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))


(defun pdf-view-kill-rmn-ring-save ()
  "Copy the region to the `kill-ring' after remove all newline characters."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (replace-regexp-in-string "\n" " "
        (car (pdf-view-active-region-text)))))
    (pdf-view-deactivate-region)
	(kill-new txt)))

(use-package pdf-view-mode
  :bind
  ("C-c C-w" . pdf-view-kill-rmn-ring-save))

(provide 'init-auctex)



;;; init-base.el ends here

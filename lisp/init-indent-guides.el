;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  ;; 关键：用填充块，而不是字符线
  (highlight-indent-guides-method 'fill)

  ;; 当前层级高亮
  (highlight-indent-guides-responsive 'top)

  ;; 实时显示
  (highlight-indent-guides-delay 0)

  ;; 自动启用
  (highlight-indent-guides-auto-enabled t))

(with-eval-after-load 'highlight-indent-guides
  (custom-set-faces
   ;; 普通缩进：非常亮的灰青
   '(highlight-indent-guides-fill-face
     ((t (:background "#D8DEE9"))))

   ;; 当前作用域：接近白的亮青色
   '(highlight-indent-guides-top-fill-face
     ((t (:background "#E5E9F0"))))))
(provide 'init-indent-guides)



;;; init-base.el ends here

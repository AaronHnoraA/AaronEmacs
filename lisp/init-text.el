;;; init-text.el --- Writing -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `org-mode' is too huge to place here.

;;; Code:

;; ReStructuredText
(use-package rst
  :ensure nil
  :hook ((rst-mode . visual-line-mode)
         (rst-adjust . rst-toc-update)))

(provide 'init-text)
;;; init-text.el ends here

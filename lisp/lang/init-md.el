;;; init-md.el --- Markdown support -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function my/typography-setup-prose-buffer "init-base")

(my/package-ensure-vc 'markdown-mode "https://github.com/jrblevin/markdown-mode.git")

;; Normalize Markdown file associations to `markdown-mode`.
(setq auto-mode-alist
      (append '(("README\\(?:\\.md\\)?\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)
                ("\\.md\\'" . markdown-mode))
              (cl-remove-if
               (lambda (entry)
                 (member entry '(("README\\(?:\\.md\\)?\\'" . markdown-mode)
                                 ("\\.markdown\\'" . markdown-mode)
                                 ("\\.md\\'" . markdown-mode)
                                 ("README\\(?:\\.md\\)?\\'" . markdown-ts-mode)
                                 ("\\.markdown\\'" . markdown-ts-mode)
                                 ("\\.md\\'" . markdown-ts-mode)
                                 ("README\\(?:\\.md\\)?\\'" . gfm-mode)
                                 ("\\.markdown\\'" . markdown-mode)
                                 ("\\.md\\'" . markdown-mode))))
               auto-mode-alist)))

;; Pixel alignment for markdown tables.
(use-package valign
  :ensure t
  :hook ((markdown-mode . valign-mode)
         (markdown-ts-mode . valign-mode)))

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "multimarkdown")
  :mode (("README\\(?:\\.md\\)?\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . my/typography-setup-prose-buffer)
         (markdown-ts-mode . visual-line-mode)
         (markdown-ts-mode . my/typography-setup-prose-buffer))
  :bind (:map markdown-mode-style-map
         ("r" . markdown-insert-ruby-tag)
         :map markdown-mode-map
         ("C-c C-e" . markdown-do))
  :config
  (defun markdown-insert-ruby-tag (text ruby)
    "Insert ruby tag with `TEXT' and `RUBY' quickly."
    (interactive "sText: \nsRuby: \n")
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-enable-prefix-prompts nil)
  (markdown-fontify-code-blocks-natively t))

(provide 'init-md)

;;; init-md.el ends here

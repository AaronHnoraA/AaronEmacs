;;; init-md.el --- Markdown support -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(declare-function my/typography-setup-prose-buffer "init-base")

(defvar my/markdown-ui--theme-signature nil
  "Last theme signature applied by `my/markdown-apply-ui'.")

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

(defun my/markdown--unspecified-color-p (value)
  "Return non-nil when VALUE is an unspecified face color."
  (or (null value)
      (eq value 'unspecified)
      (and (stringp value)
           (string-prefix-p "unspecified" value))))

(defun my/markdown-apply-ui ()
  "Apply local UI styling to Markdown faces."
  (when (display-graphic-p)
    (let ((signature (list custom-enabled-themes
                           (face-attribute 'default :background nil t)
                           (face-attribute 'default :foreground nil t))))
      (unless (equal signature my/markdown-ui--theme-signature)
        (setq my/markdown-ui--theme-signature signature)
        (let* ((base-bg (face-attribute 'default :background nil t))
               (base-bg (if (my/markdown--unspecified-color-p base-bg)
                            "#292D40"
                          base-bg))
               (strong-weight (if (boundp 'my/font-strong-weight) my/font-strong-weight 'medium))
               (title-weight (if (boundp 'my/font-title-weight) my/font-title-weight 'medium))
               (popout-weight (if (boundp 'my/font-popout-weight) my/font-popout-weight 'semibold))
               (mantle "#1b2231")
               (overlay1 "#90a0c0")
               (subtext0 "#9ba8c7")
               (subtext1 "#bfcae2")
               (text "#e4ecff")
               (rosewater "#f5e0dc")
               (yellow "#f9e2af")
               (blue "#a9cbff")
               (lavender "#b4befe")
               (mauve "#cba6f7")
               (teal "#a8f0e3")
               (green "#bbf7b8")
               (meta-bg "#244438")
               (meta-fg "#e1f6dd"))
          (dolist
              (spec `((markdown-header-face . (:foreground ,rosewater :weight ,popout-weight))
                      (markdown-header-face-1 . (:foreground ,yellow :weight ,popout-weight))
                      (markdown-header-face-2 . (:foreground ,blue :weight ,popout-weight))
                      (markdown-header-face-3 . (:foreground ,mauve :weight ,strong-weight))
                      (markdown-header-face-4 . (:foreground ,teal :weight ,strong-weight))
                      (markdown-header-face-5 . (:foreground ,rosewater :weight ,title-weight))
                      (markdown-header-face-6 . (:foreground ,lavender :weight ,title-weight))
                      (markdown-header-delimiter-face . (:foreground ,overlay1 :weight ,strong-weight))
                      (markdown-markup-face . (:foreground ,overlay1))
                      (markdown-list-face . (:foreground ,yellow :weight ,strong-weight))
                      (markdown-metadata-key-face . (:background ,meta-bg :foreground ,meta-fg :weight ,strong-weight))
                      (markdown-metadata-value-face . (:background ,meta-bg :foreground ,text))
                      (markdown-language-keyword-face . (:foreground ,teal :weight ,strong-weight))
                      (markdown-language-info-face . (:foreground ,subtext1))
                      (markdown-link-face . (:foreground ,blue :underline t))
                      (markdown-reference-face . (:foreground ,lavender :underline t))
                      (markdown-url-face . (:foreground ,teal :underline t))
                      (markdown-plain-url-face . (:foreground ,teal :underline t))
                      (markdown-inline-code-face . (:background ,mantle :foreground ,yellow :weight ,strong-weight))
                      (markdown-code-face . (:background ,mantle :foreground ,yellow))
                      (markdown-pre-face . (:background ,mantle :foreground ,text :extend t))
                      (markdown-table-face . (:background ,base-bg :foreground ,subtext1))
                      (markdown-blockquote-face . (:foreground ,subtext0 :slant italic))
                      (markdown-comment-face . (:foreground ,overlay1))
                      (markdown-footnote-face . (:foreground ,green :weight ,strong-weight))
                      (markdown-bold-face . (:foreground ,text :weight ,popout-weight))
                      (markdown-italic-face . (:foreground ,rosewater :slant italic))))
            (when (facep (car spec))
              (apply #'set-face-attribute (car spec) nil (cdr spec)))))))))

(add-hook 'markdown-mode-hook #'my/markdown-apply-ui)
(add-hook 'markdown-ts-mode-hook #'my/markdown-apply-ui)
(add-hook 'after-load-theme-hook #'my/markdown-apply-ui)

(with-eval-after-load 'markdown-mode
  (my/markdown-apply-ui))

(provide 'init-md)

;;; init-md.el ends here

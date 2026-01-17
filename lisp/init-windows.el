;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:



(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-c p F" . consult-find)
         ("C-c p g" . consult-ripgrep)
         ("C-c k" . kill-current-buffer)
         ))

(use-package ibuffer
  :ensure nil
  :custom (ibuffer-expert t)
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Code"  (or (mode . c-mode) (mode . c++-mode) (mode . python-mode)
                        (mode . emacs-lisp-mode) (mode . rust-mode) (mode . go-mode)
                        (mode . java-mode) (mode . js-mode) (mode . typescript-mode)))
           ("Dired" (mode . dired-mode))
           ("Shell" (or (mode . eshell-mode) (mode . shell-mode) (mode . term-mode)))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*Backtrace\\*$"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-do-sort-by-recency))))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package winner
  :ensure nil
  :config (winner-mode 1))

(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings 'meta))

;;;; =========================
;;;; Dirvish (Dired UI) - practical + pretty + icons
;;;; =========================

;; 基础依赖
(use-package dired
  :ensure nil
  :custom
  ;; macOS 上 ls 参数可能不支持 GNU 选项；你用 coreutils 的 gls 更稳。
  ;; 如果你没有 gls，就把下面 dired-use-ls-dired 设为 nil 并简化 listing switches。
  (dired-listing-switches "-alh --group-directories-first --time-style=long-iso")
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  :config
  ;; 让 dired 用外部 ls（GNU ls）能力更全
  (setq dired-use-ls-dired t))

;; 目录高亮（很实用）
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode)
  :custom
  (diredfl-global-mode t))

;; 图标依赖（必须先装）
(use-package nerd-icons
  :ensure t)

(use-package dirvish
  :ensure t
  :after dired
  :init
 (add-to-list 'load-path
               (expand-file-name "extensions"
                                 (file-name-directory (locate-library "dirvish"))))
  ;; 让 Dirvish 接管 dired（放 init 最合适）
  (dirvish-override-dired-mode)

  :bind (("C-c o d" . dirvish-dwim)
         ("C-c o f" . dirvish-fd)
         :map dirvish-mode-map
         ("q" . dirvish-quit)
         ("a" . dirvish-quick-access)
         ("f" . dirvish-file-info-menu)
         ("y" . dirvish-yank-menu)
         ("s" . dirvish-quicksort)
         ("TAB" . dirvish-subtree-toggle)
         ("M-t" . dirvish-layout-toggle)
         ("M-b" . dirvish-history-go-backward)
         ("M-f" . dirvish-history-go-forward)
         ("M-n" . dirvish-narrow)
         ("M-m" . dirvish-mark-menu)
         ("?" . dirvish-dispatch)
         ;("P" . dirvish-peek-mode) ; 如果没有该函数，下面 config 会兜底处理
         ;; 鼠标（可选）
         ("<mouse-1>" . dirvish-subtree-toggle-or-open)
         ("<mouse-2>" . dired-mouse-find-file-other-window)
         ("<mouse-3>" . dired-mouse-find-file))

  :custom
  (dirvish-side-width 36)
  (dirvish-header-line-height '(26 . 36))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))

  :config
  ;; 1) 先给一个“基础属性集合”（绝对不会触发 dirvish-vc）
  (setq dirvish-attributes
        '(all-the-icons file-size subtree-state collapse file-time))

  ;; 2) Peek / Side-follow：有就开（没有就跳过，不报错）
  (when (fboundp 'dirvish-peek-mode)
    (dirvish-peek-mode 1))
  (when (fboundp 'dirvish-side-follow-mode)
    (dirvish-side-follow-mode 1))

  ;; 3) VC/Git：尝试静默加载 dirvish-vc，成功才追加属性
  (when (require 'dirvish-vc nil t)
    (setq dirvish-attributes
          '(nerd-icons vc-state git-msg file-size subtree-state collapse file-time)))

  ;; 4) 如果你希望 P 键在没有 peek 函数时也不报错，做一个安全绑定
  (unless (fboundp 'dirvish-peek-mode)
    (define-key dirvish-mode-map (kbd "P") #'ignore)))


;; 搜索/过滤：配合 dirvish-fd、consult 更舒服
(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-initialism orderless-prefixes orderless-regexp)))

(use-package consult
  :ensure t)




(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-style "bar"
    centaur-tabs-height 22
    centaur-tabs-set-icons t
    centaur-tabs-plain-icons t
    centaur-tabs-gray-out-icons t
    centaur-tabs-set-close-button t
    centaur-tabs-set-modified-marker t
    centaur-tabs-show-navigation-buttons t
    centaur-tabs-set-bar 'left
    centaur-tabs-cycle-scope 'tabs
    x-underline-at-descent-line nil)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((ignore-errors
     (and (string= "*xwidget" (substring (buffer-name) 0 8))
          (not (string= "*xwidget-log*" (buffer-name)))))
       "Xwidget")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
       (memq major-mode '(magit-process-mode
                  magit-status-mode
                  magit-diff-mode
                  magit-log-mode
                  magit-file-mode
                  magit-blob-mode
                  magit-blame-mode
                  )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
              help-mode))
       "Help")
      ((memq major-mode '(org-mode
              org-agenda-clockreport-mode
              org-src-mode
              org-agenda-mode
              org-beamer-mode
              org-indent-mode
              org-bullets-mode
              org-cdlatex-mode
              org-agenda-log-mode
              diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-c b" . centaur-tabs-backward)
  ("C-c n" . centaur-tabs-forward)
  ("C-c m" . centaur-tabs-forward-group)
  ("C-c v" . centaur-tabs-backward-group))

(use-package bookmark
  :ensure nil
  :bind (("C-x r m" . bookmark-set)
     ("C-x r d" . bookmark-delete)
     ("C-x r j" . bookmark-jump)))



(provide 'init-windows)
;;; init-base.el ends here

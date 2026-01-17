;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Ivy
(use-package ivy
  :ensure t
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq tramp-default-method "ssh")
  ;; 明确告诉 TRAMP 去读 ~/.ssh/config 里的 Host 别名
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; 关键：启用 ssh-config 补全（TRAMP 会利用 ssh -G / config 解析）
  (setq tramp-completion-use-auth-sources t)
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-posframe-mode 1))


(defun my/safe-counsel-M-x ()
  "Call `counsel-M-x' unless minibuffer is already active."
  (interactive)
  (if (active-minibuffer-window)
      (keyboard-quit)
    (counsel-M-x)))




(global-set-key (kbd "M-x") #'my/safe-counsel-M-x)

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-alt-done))

;; Ivy Posframe（让 Ivy 在浮窗里显示）
(use-package ivy-posframe
  :ensure t
  :after ivy
  :config
  (ivy-posframe-mode 1))

;; Counsel
(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x g"   . counsel-git)))

;; Swiper
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config
  (setq swiper-action-recenter t
        swiper-include-line-number-in-search t))

;; 强制关闭递归 minibuffer，避免 M-x 套娃
(setq enable-recursive-minibuffers nil)

(provide 'init-search)



;;; init-base.el ends here

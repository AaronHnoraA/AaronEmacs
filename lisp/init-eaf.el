;;; init-eaf.el --- EAF configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs Application Framework configuration.

;;; Code:

;; https://github.com/emacs-eaf/emacs-application-framework
;; git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
;; cd emacs-application-framework
;; chmod +x ./install-eaf.py
;; ./install-eaf.py


(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  ;; 声明触发加载的入口命令。当调用这些命令时，才会真正加载 EAF
  :commands (eaf-open eaf-open-browser eaf-open-bookmark)
  
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  
  :init
  ;; 将 alias 放在 init 块中，确保在 EAF 加载前就能识别 browse-web 命令
  (defalias 'browse-web #'eaf-open-browser)
  
  :config
  ;; 将所有组件的 require 移入 config 块中，确保仅在 EAF 被触发时按需加载
  (require 'eaf-image-viewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-browser)
  (require 'eaf-mind-elixir)
  (require 'eaf-markdown-previewer)
  (require 'eaf-mindmap)
  (require 'eaf-video-player)
  (require 'eaf-org-previewer)
  (require 'eaf-git)
  ) ;; unbind, see more in the Wiki


;; 1. 将 pdf-view-mode 绑定到对应的文件扩展名和魔术字，触发懒加载
(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

;; 2. 当 pdf-view 真正被加载时，再静默执行底层工具链的安装与核心设置
(with-eval-after-load 'pdf-view
  (pdf-tools-install :no-query))

;; ----------------------------------------------------------------------
;; 一键清理 EAF 相关的 Buffer 和 Python 进程
;; ----------------------------------------------------------------------
(defun my-eaf-kill-all ()
  "关闭所有 EAF 相关的 Buffer 并杀掉 EAF 后台 Python 进程。"
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'eaf-mode)
          (kill-buffer buf)
          (setq count (1+ count)))))
    (when (fboundp 'eaf-kill-process)
      (eaf-kill-process))
    (message "已清理 %d 个 EAF Buffer 及后台进程！" count)))

(defun my/eaf-restart ()
  "Restart EAF and clear its old buffers."
  (interactive)
  (my-eaf-kill-all)
  (require 'eaf nil t)
  (message "EAF restarted."))

(provide 'init-eaf)
;;; init-eaf.el ends here

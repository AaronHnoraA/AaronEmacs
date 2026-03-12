;;; init-eaf.el --- EAF configuration -*- lexical-binding: t -*-

;;; Commentary:
;;  Emacs Application Framework (EAF) 延迟加载配置

;;; Code:

;; https://github.com/emacs-eaf/emacs-application-framework
;; git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
;; cd emacs-application-framework
;; chmod +x ./install-eaf.py
;; ./install-eaf.py

(defconst my/eaf-load-path
  (expand-file-name "site-lisp/emacs-application-framework" user-emacs-directory))

(use-package eaf
  :load-path my/eaf-load-path
  ;; 核心：只有当你调用以下命令时，才会真正加载 EAF 及其 Python 进程
  :commands (eaf-open-browser eaf-open eaf-open-pdf eaf-open-mindmap browse-web eaf-open-git)
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  ;; 将你原本散落在外面的 setq 整合进 :custom，更加规范
  (eaf-jupyter-search-function nil)
  (eaf-browser-chrome-browser-name "Brave")
  (eaf-browser-auto-import-chrome-cookies t)
  
  :config
  ;; 当上面 :commands 里的任何一个命令被触发时，才会执行以下 require
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-mind-elixir)
  (require 'eaf-markdown-previewer)
  (require 'eaf-mindmap)
  (require 'eaf-video-player)
  (require 'eaf-org-previewer)
  (require 'eaf-git)

  (defalias 'browse-web #'eaf-open-browser))

;; ----------------------------------------------------------------------
;; 一键清理 EAF 相关的 Buffer 和 Python 进程
;; ----------------------------------------------------------------------
(defun my-eaf-kill-all ()
  "关闭所有 EAF 相关的 Buffer 并杀掉 EAF 后台 Python 进程。"
  (interactive)
  (let ((count 0))
    ;; 1. 关闭所有 EAF Buffer
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'eaf-mode)
          (kill-buffer buf)
          (setq count (1+ count)))))
    ;; 2. 杀掉 EAF 的 Python 后台进程 (如果存在的话)
    (when (fboundp 'eaf-kill-process)
      (eaf-kill-process))
    (message "已清理 %d 个 EAF Buffer 及后台进程！" count)))


(provide 'init-eaf)

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

(defconst my/eaf-app-load-paths
  (mapcar (lambda (dir)
            (expand-file-name dir my/eaf-load-path))
          '("app/browser"
            "app/git"
            "app/image-viewer"
            "app/markdown-previewer"
            "app/mind-elixir"
            "app/mindmap"
            "app/org-previewer"
            "app/pdf-viewer"
            "app/video-player")))

(dolist (dir (cons my/eaf-load-path my/eaf-app-load-paths))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(autoload 'eaf-open "eaf" nil t)
(autoload 'eaf-open-browser "eaf-browser" nil t)
(autoload 'eaf-open-pdf "eaf-pdf-viewer" nil t)
(autoload 'eaf-open-mindmap "eaf-mindmap" nil t)
(autoload 'eaf-open-git "eaf-git" nil t)

(defalias 'browse-web #'eaf-open-browser)

(use-package eaf
  :if (file-directory-p my/eaf-load-path)
  :defer t
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  ;; 将你原本散落在外面的 setq 整合进 :custom，更加规范
  (eaf-jupyter-search-function nil)
  (eaf-browser-chrome-browser-name "Brave")
  (eaf-browser-auto-import-chrome-cookies t))

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

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

(defun my/eaf-require (&rest features)
  "Load EAF core and FEATURES lazily."
  (require 'eaf)
  (dolist (feature features)
    (require feature)))

(defun eaf-open (url &optional app-name args always-new)
  "Lazy wrapper for the real `eaf-open'."
  (interactive "G[EAF] EAF Open: ")
  (my/eaf-require)
  (funcall (symbol-function 'eaf-open) url app-name args always-new))

(defun eaf-open-browser (url &optional args)
  "Lazy wrapper for the real `eaf-open-browser'."
  (interactive "M[EAF/browser] URL: ")
  (my/eaf-require 'eaf-browser)
  (funcall (symbol-function 'eaf-open-browser) url args))

(defun eaf-open-pdf (file)
  "Open FILE with the EAF PDF viewer."
  (interactive "f[EAF/pdf] Open PDF: ")
  (my/eaf-require 'eaf-pdf-viewer)
  (eaf-open file "pdf-viewer"))

(defun eaf-open-mindmap (file)
  "Lazy wrapper for the real `eaf-open-mindmap'."
  (interactive "F[EAF/mindmap] Select Mindmap file: ")
  (my/eaf-require 'eaf-mindmap)
  (funcall (symbol-function 'eaf-open-mindmap) file))

(defun eaf-open-git ()
  "Lazy wrapper for the real `eaf-open-git'."
  (interactive)
  (my/eaf-require 'eaf-git)
  (call-interactively (symbol-function 'eaf-open-git)))

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

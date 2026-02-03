;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;

;https://github.com/emacs-eaf/emacs-application-framework
    
;git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
;cd emacs-application-framework
;chmod +x ./install-eaf.py
;./install-eaf.py

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)

  ) ;; unbind, see more in the Wiki

(require 'eaf-browser)
(require 'eaf-image-viewer)
(require 'eaf-pdf-viewer)
(require 'eaf-mind-elixir)
(require 'eaf-markdown-previewer)
(require 'eaf-mindmap)
(require 'eaf-video-player)
(require 'eaf-org-previewer)

(setq eaf-jupyter-search-function nil)


(setq eaf-browser-chrome-browser-name "Brave")
(setq eaf-browser-auto-import-chrome-cookies t)

(provide 'init-eaf)


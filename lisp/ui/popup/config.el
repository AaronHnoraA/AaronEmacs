;;; ui/popup/config.el --- Popup windows -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The popup system: shackle shows a buffer that pops up according to the
;; rules modules declare with `set-popup-rule!' (autoload/popup.el).  This
;; module declares the default rules for common built-in buffers; a module
;; declares the rules for its own buffers, which take precedence.  The sizes
;; below are shared by those module rules.

;;; Code:

(require 'config)

(defgroup +display-rules nil
  "Popup window sizes shared by module popup rules."
  :group 'windows)

(config-defvar +display-rules-bottom-size nil
  "Default bottom-side window size for development buffers."
  :type 'float
  :group '+display-rules)

(config-defvar +display-rules-side-size nil
  "Default side window size for side-panel style buffers."
  :type 'float
  :group '+display-rules)

(config-defvar +display-rules-roam-size nil
  "Width ratio for roam UI side panels."
  :type 'float
  :group '+display-rules)

(use-package shackle
  :ensure t
  :defer 1
  :config
  (shackle-mode 1)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below))

;; Default rules for buffers no module owns.  Module rules are declared
;; later and so take precedence.
(set-popup-rules!
 '((vc-annotate-mode         :select t :inhibit-window-quit t :same t)
   ("*quickrun*"             :select t :inhibit-window-quit t :same t)
   (profiler-report-mode     :select t)
   (xwidget-webkit-mode      :select t :same t)
   (comint-mode              :select t :align t :size 0.4)
   (grep-mode                :select t :align t)
   (rg-mode                  :select t :align t)
   ;; See also `help-window-select'
   (apropos-mode             :select nil :align t :size 0.4)
   (help-mode                :select nil :align t :size 0.4)
   ("*Backtrace*"               :select t   :align t :size 15)
   ("*Shell Command Output*"    :select nil :align t :size 0.4)
   ("*Async Shell Command*"     :select nil :align t :size 0.4)
   ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
   ("*Process List*"            :select t   :align t :size 0.3)
   ("*Occur*"                   :select t   :align t)
   ("\\*eldoc\\( for \\)?.*\\*" :select t   :align t :size 15 :regexp t)))

(provide 'ui/popup/config)
;;; ui/popup/config.el ends here

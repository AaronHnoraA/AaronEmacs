;;; init-languagetool.el --- LanguageTool entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the Flymake integration and retain languagetool.el as the asynchronous
;; local CLI fallback used when a manual NAS check cannot connect.

;;; Code:
(require 'config)
(require 'init-languagetool-flymake)

(my/package-ensure-vc
 'languagetool "https://github.com/PillFall/languagetool.el.git")

(use-package languagetool
  :ensure nil
  :defer t
  :commands (languagetool-set-language)
  :hook (text-mode . my/languagetool-auto-mode)
  :bind (("C-c i g" . my/languagetool-check)
         ("C-c i x" . my/languagetool-clear)
         ("C-c i a" . my/languagetool-correct-at-point)
         ("C-c i A" . my/languagetool-correct)))

(provide 'init-languagetool)
;;; init-languagetool.el ends here

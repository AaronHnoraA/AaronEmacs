;;; checkers/grammar/config.el --- LanguageTool entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the Flymake integration and retain languagetool.el as the asynchronous
;; local CLI fallback used when a manual NAS check cannot connect.

;;; Code:
(require 'config)
(require 'checkers/grammar/flymake)

(aaron-package-ensure-vc
 'languagetool "https://github.com/PillFall/languagetool.el.git")

(use-package languagetool
  :ensure nil
  :defer t
  :commands (languagetool-set-language)
  :hook (text-mode . +languagetool-auto-mode)
  :bind (("C-c i g" . +languagetool/check)
         ("C-c i x" . +languagetool/clear)
         ("C-c i a" . +languagetool/correct-at-point)
         ("C-c i A" . +languagetool/correct)))

(provide 'checkers/grammar/config)
;;; checkers/grammar/config.el ends here

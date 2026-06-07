;;; ai-workbench-adapter-gptel.el --- Backward-compat shim -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Redirects to ai-workbench-chat.el, which is the canonical home for the
;; HTTP chat engine.  This file exists only for backward compatibility with
;; any code that does (require 'ai-workbench-adapter-gptel).

;;; Code:

(require 'ai-workbench-chat)

(provide 'ai-workbench-adapter-gptel)
;;; ai-workbench-adapter-gptel.el ends here

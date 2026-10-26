;;; noema/jupyter/config.el --- Noema Jupyter bridge entry -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The Jupyter bridge for ordinary `.ipynb' notebooks and Markdown `@@cell'
;; sidecars: kernelspec discovery and the five-channel connection (`runtime'),
;; remote Jupyter servers (`server'), the `.cell/' script mode (`cell'),
;; kernel-following lsp-mode (`lsp'), project context, debugging and files.
;; See docs/design/jupyter-workflow.md.  The parts load in this order.

;;; Code:

(require 'noema/jupyter/notebook)
(require 'noema/jupyter/cell)
(require 'noema/jupyter/runtime)
(require 'noema/jupyter/server)
(require 'noema/jupyter/project)
(require 'noema/jupyter/lsp)
(require 'noema/jupyter/debug)
(require 'noema/jupyter/files)

(provide 'noema/jupyter/config)
;;; noema/jupyter/config.el ends here

;;; ui/scrollview/config.el --- Document overview scrollbar -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Keep exact-line indicators in the left fringe and render the document-wide
;; scrollbar/sign overview in the right fringe.

;;; Code:

(declare-function scrollview-mode "scrollview" (&optional arg))

(defgroup +scrollview nil
  "Document overview integration."
  :group 'convenience)

(defconst +scrollview-vc-revision
  "90e1742ed94302503636cce26cabe13c58d71f69"
  "Tested scrollview.el revision used by this configuration.")

(defconst +scrollview-auto-mode-families
  '(prog-mode text-mode conf-mode)
  "Major-mode families where scrollview is enabled automatically.")

(defconst +scrollview-auto-excluded-modes
  '(special-mode comint-mode term-mode portal-terminal-mode eshell-mode
    image-mode doc-view-mode pdf-view-mode)
  "Major-mode families excluded from automatic scrollview activation.")

(defun +scrollview-auto-buffer-p ()
  "Return non-nil when the current buffer should use scrollview automatically."
  (and (not (minibufferp))
       (not (string-prefix-p " " (buffer-name)))
       (not (apply #'derived-mode-p +scrollview-auto-excluded-modes))
       (or buffer-file-name
           (apply #'derived-mode-p +scrollview-auto-mode-families))))

(defun +scrollview-turn-on ()
  "Enable `scrollview-mode' in editing and reading buffers."
  (when (+scrollview-auto-buffer-p)
    (scrollview-mode 1)))

(define-globalized-minor-mode +global-scrollview-mode
  scrollview-mode +scrollview-turn-on
  :group '+scrollview
  :lighter "")

(aaron-package-ensure-vc
 'scrollview
 "https://github.com/roife/scrollview.el.git"
 +scrollview-vc-revision)

(use-package scrollview
  :ensure nil
  :demand t
  :custom
  (scrollview-area 'fringe)
  (scrollview-fallback-to-margin t)
  (scrollview-side 'right)
  (scrollview-visibility 'info)
  (scrollview-current-window-only nil)
  (scrollview-line-limit 20000)
  (scrollview-byte-limit 1000000)
  (scrollview-signs-on-startup
   '(search diagnostics vc bookmarks symbol-overlay))
  (scrollview-refresh-delay 0.05)
  :config
  (+global-scrollview-mode 1))

(aaron-leader!
  "j n" '(:def scrollview-next :which-key "next overview sign")
  "j p" '(:def scrollview-prev :which-key "previous overview sign")
  "j v" '(:def scrollview-legend :which-key "overview legend"))

(provide 'ui/scrollview/config)

;;; ui/scrollview/config.el ends here

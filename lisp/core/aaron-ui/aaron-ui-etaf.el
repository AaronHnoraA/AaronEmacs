;;; aaron-ui-etaf.el --- Aaron palette for ETAF applications -*- lexical-binding: t; -*-

;;; Commentary:
;; ETAF owns layout, input and retained state.  Aaron UI owns the colors that
;; its catalog Components inherit.  Load this bridge only when an ETAF surface
;; is opened; the ordinary Emacs theme does not need the ETAF runtime.

;;; Code:

(require 'aaron-ui)
(require 'etaf)
(require 'etaf-ui)

(defvar aaron-ui-etaf--theme-ref nil
  "Shared reactive Theme for Aaron UI ETAF applications.")

(defun aaron-ui-etaf-theme (&optional variant)
  "Return ETAF and ETAF UI Theme tokens for Aaron VARIANT.
VARIANT defaults to the currently selected Aaron theme."
  (let* ((variant (or variant aaron-ui-current-variant))
         (bg (aaron-ui-color 'bg-base nil variant))
         (fg (aaron-ui-color 'fg-main nil variant)))
    (append
     (list :bgcolor bg :color fg
           :ui-mode (if (eq variant 'lotus) 'light 'dark))
     (etaf-ui-theme-tokens
      (list :bg bg :fg fg
            :panel (aaron-ui-color 'bg-panel nil variant)
            :track (aaron-ui-color 'bg-elevated nil variant)
            :border (aaron-ui-color 'border-subtle nil variant)
            :muted (aaron-ui-color 'fg-muted nil variant)
            :dim (aaron-ui-color 'fg-faint nil variant)
            :accent (aaron-ui-color 'accent-cyan nil variant)
            :accent-fg bg
            :accent-soft (aaron-ui-color 'bg-surface-strong nil variant)
            :success (aaron-ui-color 'accent-green nil variant)
            :success-bg (aaron-ui-color 'bg-success nil variant)
            :warning (aaron-ui-color 'accent-yellow nil variant)
            :danger (aaron-ui-color 'accent-red-strong nil variant)
            :danger-bg (aaron-ui-color 'bg-danger nil variant)
            :info (aaron-ui-color 'accent-blue nil variant))))))

(defun aaron-ui-etaf--refresh-theme (&rest _)
  "Update mounted ETAF applications after an Aaron theme change."
  (when aaron-ui-etaf--theme-ref
    (etaf-set-value aaron-ui-etaf--theme-ref (aaron-ui-etaf-theme))))

(defun aaron-ui-etaf-theme-ref ()
  "Return the shared reactive Aaron Theme for ETAF Components."
  (unless aaron-ui-etaf--theme-ref
    (setq aaron-ui-etaf--theme-ref (etaf-ref (aaron-ui-etaf-theme)))
    (add-hook 'after-load-theme-hook #'aaron-ui-etaf--refresh-theme))
  aaron-ui-etaf--theme-ref)

(defun aaron-ui-etaf-provide-theme ()
  "Give this ETAF subtree Aaron colors and declare them to a panel host."
  (let ((theme (aaron-ui-etaf-theme-ref)))
    (etaf-theme-provide theme)
    (let ((surface (etaf-surface-host)))
      (when surface
        (etaf-surface-declare surface :theme theme)))))

(etaf-define-component aaron-ui-etaf-preview-page ()
  "Show the standard Aaron palette on official ETAF UI Components."
  :bindings ((status (etaf-ref "Ready")))
  :setup (aaron-ui-etaf-provide-theme)
  :view
  (column :width 'stretch :padding '((lh 1) (ch 2)) :item-gap '(lh 1)
    (etaf-label :text "Aaron UI · ETAF" :variant 'heading)
    (etaf-label :text "ETAF View 和状态；ETAF UI 控件；Aaron 主题色。"
                :variant 'muted)
    (etaf-panel :title "组件预览"
      (column :width 'stretch :item-gap '(lh 1)
        (row :item-gap '(ch 1)
          (etaf-badge :text "Ready" :variant 'success)
          (etaf-badge :text "Notice" :variant 'info)
          (etaf-badge :text "Warning" :variant 'warning))
        (etaf-label :text status)
        (row :item-gap '(ch 1)
          (etaf-button :label "Update" :variant 'primary
                       :on-press (lambda ()
                                   (setf (etaf-value status) "Updated")))
          (etaf-button :label "Reset" :variant 'secondary
                       :on-press (lambda ()
                                   (setf (etaf-value status) "Ready"))))))
    (etaf-label :text "Tab 切换控件；RET 激活；q 关闭图形浮窗。"
                :variant 'muted)))

;;;###autoload
(defun aaron-ui-etaf-preview ()
  "Open a small Aaron themed ETAF UI development preview."
  (interactive)
  (let ((view (etaf-view (aaron-ui-etaf-preview-page))))
    (if (display-graphic-p)
        (progn
          (require 'aaron-ui-float)
          (aaron-ui-float-show-view
           view :key 'aaron-ui-etaf-preview
           :title "Aaron UI · ETAF" :width 54 :height 15))
      (pop-to-buffer (etaf-mount "*Aaron UI ETAF Preview*" view)))))

(provide 'aaron-ui-etaf)
;;; aaron-ui-etaf.el ends here

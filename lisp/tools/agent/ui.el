;;; tools/agent/ui.el --- Shared agent page presentation -*- lexical-binding: t; -*-

;;; Code:
(require 'aaron-ui-etaf)
(require 'face-remap)
(require 'subr-x)

(defface +agent-ui-heading '((t :inherit default :weight semi-bold))
  "Headings in native agent tables." :group 'aaron-ui)
(defface +agent-ui-muted '((t :inherit shadow))
  "Supporting text in agent pages." :group 'aaron-ui)
(defface +agent-ui-working '((t :inherit success))
  "Active agent work." :group 'aaron-ui)
(defface +agent-ui-waiting '((t :inherit warning))
  "Agent work waiting for attention." :group 'aaron-ui)
(defface +agent-ui-failed '((t :inherit error))
  "Failed or disconnected agent work." :group 'aaron-ui)
(defvar-local +agent-ui--header-remap nil)

(defun +agent-ui--apply-faces-h (&rest _)
  "Follow the active Aaron palette without changing table geometry."
  (dolist (entry '((+agent-ui-heading role-strong) (+agent-ui-muted role-faded)
                   (+agent-ui-working accent-cyan) (+agent-ui-waiting accent-yellow)
                   (+agent-ui-failed role-critical)))
    (aaron-ui-set-face (car entry) :foreground (cadr entry) :background 'unspecified)))
(+agent-ui--apply-faces-h)
(add-hook 'enable-theme-functions #'+agent-ui--apply-faces-h)

(defun +agent-ui-state-face (state)
  "Return a semantic face for the already reported STATE."
  (pcase (format "%s" state)
    ((or "running" "busy" "working") '+agent-ui-working)
    ((or "approval" "answer" "waiting" "stopping" "held") '+agent-ui-waiting)
    ((or "failed" "dead" "disconnected" "stop-unconfirmed") '+agent-ui-failed)
    (_ '+agent-ui-muted)))

(defun +agent-ui-count (value)
  "Format a reported count VALUE; nil remains unknown and zero stays zero."
  (cond ((not (numberp value)) "—")
        ((>= value 1000000) (format "%.1fM" (/ value 1e6)))
        ((>= value 1000) (format "%.1fk" (/ value 1e3)))
        (t (format "%d" value))))

(defun +agent-ui-usage-count (usage key)
  "Format USAGE's KEY without presenting normalized, unreported zeros as data."
  (let ((value (plist-get usage key)))
    (+agent-ui-count (and (numberp value) (> value 0) value))))

(defun +agent-ui-cost (usage)
  "Format a reported cost in USAGE, including an explicit zero."
  (let ((amount (plist-get usage :cost)))
    (if (numberp amount)
        (format "%s %.2f" (or (plist-get usage :currency) "USD") amount)
      "—")))

(etaf-define-component +agent-ui-metric (&key label value)
  "One metric with the standard ETAF typography and an Aaron surface."
  :setup
  (etaf-theme-provide
   (etaf-computed
    (lambda ()
      (plist-put (copy-sequence (etaf-value (aaron-ui-etaf-theme-ref)))
                 :bgcolor (aaron-ui-color 'bg-elevated)))))
  :view
  (column :flex-grow 1 :flex-basis '(% 30) :min-width '(ch 14)
          :padding '((lh 0.5) (ch 2)) :bgcolor (aaron-ui-color 'bg-elevated)
    (etaf-statistic :label label :value value)))

(etaf-define-component +agent-ui-context (&key usage)
  "Display a bounded context meter only when capacity was actually reported."
  :render
  (let ((used (plist-get usage :context-used)) (size (plist-get usage :context-size)))
    (if (and (numberp used) (>= used 0) (numberp size) (> size 0))
        (etaf-view
         (etaf-progress :value (min used size) :max size
                        :label (format "Context · %s / %s" (+agent-ui-count used) (+agent-ui-count size))
                        :variant (cond ((>= (/ (float used) size) 0.9) 'danger)
                                       ((>= (/ (float used) size) 0.7) 'warning) (t 'info))))
      (etaf-view (etaf-label :text "Context · not reported" :variant 'muted)))))

(defun +agent-ui-table-init ()
  "Give a native agent table breathing room while keeping its native keys."
  (setq-local tabulated-list-use-header-line nil
              line-spacing 0.15 left-margin-width 1 right-margin-width 1
              truncate-lines t)
  (+agent-ui--table-cleanup-h)
  (setq +agent-ui--header-remap
        (face-remap-add-relative 'tabulated-list-header '+agent-ui-heading))
  (add-hook 'change-major-mode-hook #'+agent-ui--table-cleanup-h nil t)
  (hl-line-mode 1))

(defun +agent-ui--table-cleanup-h ()
  "Release this table's header typography before changing modes."
  (when +agent-ui--header-remap (face-remap-remove-relative +agent-ui--header-remap))
  (setq +agent-ui--header-remap nil))

(defun +agent-ui-table-summary (title summary &optional empty)
  "Set native table TITLE and SUMMARY, with EMPTY text when there are no rows."
  (setq-local header-line-format
              (list (propertize (concat " " title "  ") 'face '+agent-ui-heading)
                    (propertize (replace-regexp-in-string "%" "%%" summary)
                                'face '+agent-ui-muted)))
  (when (and empty (null tabulated-list-entries))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert "\n  " (propertize empty 'face '+agent-ui-muted) "\n")))))

(provide 'tools/agent/ui)
;;; ui.el ends here

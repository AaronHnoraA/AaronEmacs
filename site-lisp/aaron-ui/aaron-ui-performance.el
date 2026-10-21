;;; aaron-ui-performance.el --- ETAF Performance Watch page -*- lexical-binding: t -*-

;;; Commentary:
;; Retained layout for the large Performance Watch frame.  Its data and plain
;; report remain owned by init-performance; this module only presents them.

;;; Code:

(require 'etaf)
(require 'aaron-ui)
(require 'aaron-ui-board)
(require 'seq)

(declare-function my/performance-watch-refresh "init-performance" ())
(declare-function my/performance-watch-toggle-auto-refresh "init-performance" ())
(declare-function my/performance-toggle-recording "init-performance" ())
(declare-function my/performance-save-record "init-performance" (&optional sample))
(declare-function my/performance-profiler-start-cpu "init-performance" ())

(defun aaron-ui-performance--color (token fallback)
  "Resolve Aaron UI TOKEN, falling back to FALLBACK."
  (aaron-ui-color token fallback))

(etaf-define-component aaron-ui-performance-action
    (&key label command owner)
  "Render one Performance Watch COMMAND."
  :view
  (box :role 'button :tab-index 0
       :padding '((lh 0) (ch 2))
       :border (aaron-ui-performance--color 'border-subtle "#4B5268")
       :bgcolor (aaron-ui-performance--color 'bg-elevated "#2A3247")
       :color (aaron-ui-performance--color 'fg-main "#E4ECFF")
       :on-press (lambda ()
                   (with-current-buffer owner
                     (call-interactively command)))
    (text (expr label))))

(etaf-define-component aaron-ui-performance-tab
    (&key label value selected)
  "Render one Performance Watch tab VALUE."
  :view
  (box :ref (intern (format "performance-tab-%s" value))
       :role 'button :tab-index 0
       :padding '((lh 0) (ch 2))
       :border (aaron-ui-performance--color 'border-subtle "#4B5268")
       :bgcolor (aaron-ui-performance--color
                 (if (eq (etaf-value selected) value)
                     'bg-panel 'bg-elevated)
                 "#2A3247")
       :color (aaron-ui-performance--color
               (if (eq (etaf-value selected) value)
                   'accent-cyan 'fg-muted)
               "#9DE7FF")
       :on-press (lambda () (setf (etaf-value selected) value))
    (text (expr label))))

(etaf-define-component aaron-ui-performance-section (&key section)
  "Render one Performance Watch SECTION."
  :view
  (column :width 'stretch
    (box :padding '((lh 1) (ch 1))
         :border (aaron-ui-performance--color 'border-subtle "#4B5268")
         :color (aaron-ui-performance--color 'accent-cyan "#9DE7FF")
         :font-weight 'bold
      (text (expr (format "%s  %s"
                          (aaron-ui-board-icon 'section)
                          (plist-get section :title)))))
    (box :padding '((lh 0) (ch 2))
         :color (aaron-ui-performance--color 'fg-main "#E4ECFF")
      (text (expr (plist-get section :body))))))

(etaf-define-component aaron-ui-performance-app
    (&key state selected owner)
  "Render retained Performance Watch STATE for OWNER."
  :view
  (column :width 'stretch
    (box :padding '((lh 1) (ch 2))
         :bgcolor (aaron-ui-performance--color 'bg-elevated "#2A3247")
      (column :width 'stretch
        (text :font-weight 'bold
              :color (aaron-ui-performance--color 'fg-strong "#F4F7FF")
              (expr (format "%s  Performance Watch"
                            (aaron-ui-board-icon 'chart))))
        (text :color (aaron-ui-performance--color 'fg-muted "#8B90A8")
              (expr (or (plist-get (etaf-value state) :summary)
                        "Loading…")))))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 0) (ch 2))
      (aaron-ui-performance-tab
       :label "Overview" :value 'overview :selected selected)
      (aaron-ui-performance-tab
       :label "Processes" :value 'processes :selected selected)
      (aaron-ui-performance-tab
       :label "Buffers" :value 'buffers :selected selected)
      (aaron-ui-performance-tab
       :label "Hooks & timers" :value 'hooks :selected selected))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 1) (ch 2))
      (aaron-ui-performance-action
       :label "Refresh" :command #'my/performance-watch-refresh :owner owner)
      (aaron-ui-performance-action
       :label (if (plist-get (etaf-value state) :auto)
                  "Auto: on" "Auto: off")
       :command #'my/performance-watch-toggle-auto-refresh :owner owner)
      (aaron-ui-performance-action
       :label (if (plist-get (etaf-value state) :recording)
                  "Recording: on" "Recording: off")
       :command #'my/performance-toggle-recording :owner owner)
      (aaron-ui-performance-action
       :label "Save record" :command #'my/performance-save-record :owner owner)
      (aaron-ui-performance-action
       :label "Profiler" :command #'my/performance-profiler-start-cpu :owner owner))
    (aaron-ui-performance-section
     :for (section (seq-filter
                    (lambda (item)
                      (eq (plist-get item :tab)
                          (etaf-value selected)))
                    (plist-get (etaf-value state) :sections)))
     :key (plist-get section :key)
     :section section)
    (box :padding '((lh 1) (ch 2))
         :color (aaron-ui-performance--color 'fg-muted "#8B90A8")
      (text "g refresh  G collect  y copy page  a auto  s save  R record  o folder  p/P profiler  q quit"))))

(defun aaron-ui-performance-view (state selected owner)
  "Return an ETAF View for STATE and SELECTED in OWNER."
  (etaf-view
   (aaron-ui-performance-app
    :state state :selected selected :owner owner)))

(provide 'aaron-ui-performance)
;;; aaron-ui-performance.el ends here

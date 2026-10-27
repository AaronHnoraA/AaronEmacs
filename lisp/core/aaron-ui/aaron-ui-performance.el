;;; aaron-ui-performance.el --- ETAF Performance Watch page -*- lexical-binding: t -*-

;;; Commentary:
;; Retained layout for the large Performance Watch frame.  Its data and plain
;; report remain owned by init-performance; this module only presents them.

;;; Code:

(require 'etaf)
(require 'aaron-ui)
(require 'aaron-ui-etaf)
(require 'aaron-ui-board)
(require 'aaron-ui-activity)
(require 'seq)

(declare-function +performance/watch-refresh-current "config/performance/config" ())
(declare-function +performance/watch-toggle-auto-current "config/performance/config" ())
(declare-function +performance/toggle-recording "config/performance/config" ())
(declare-function +performance/save-record "config/performance/config" (&optional sample))
(declare-function +performance/profiler-start-cpu "config/performance/config" ())
(declare-function +performance/watch-select-tab "config/performance/config" (tab))
(declare-function +activity-board/toggle-timers "ui/activity/config" ())
(declare-function +activity-board/filter "ui/activity/config" (pattern))
(declare-function +performance/open-buffer "config/performance/config" (buffer))
(declare-function +performance/noema-open-wiki "config/performance/config" ())
(declare-function +performance/noema-open-agenda "config/performance/config" ())
(declare-function +performance/noema-stop-host "config/performance/config" ())

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
    (&key label value selected owner)
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
       :on-press (lambda ()
                   (with-current-buffer owner
                     (+performance/watch-select-tab value)))
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

(etaf-define-component aaron-ui-performance-buffer-row (&key row owner)
  "Render one actionable monitor buffer ROW."
  :view
  (box :ref (plist-get row :ref) :role 'button :tab-index 0
       :padding '((lh 0) (ch 2))
       :bgcolor (aaron-ui-performance--color 'bg-surface "#1E2536")
       :color (aaron-ui-performance--color 'fg-main "#E4ECFF")
       :on-press (lambda ()
                   (with-current-buffer owner
                     (+performance/open-buffer (plist-get row :buffer))))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 2))
      (box :min-width '(ch 30) :flex-grow 1
        (text (expr (plist-get row :name))))
      (box :width '(ch 10)
        (text (expr (format "%d" (plist-get row :size)))))
      (box :width '(ch 18)
        (text (expr (format "%s" (plist-get row :mode))))))
    (box :if (plist-get row :file)
         :color (aaron-ui-performance--color 'fg-dim "#A9BED3")
      (text (expr (plist-get row :file))))))

(etaf-define-component aaron-ui-performance-app
    (&key state selected activity-state owner)
  "Render retained Performance Watch STATE for OWNER."
  :setup (aaron-ui-etaf-provide-theme)
  :view
  (column :ref 'performance-scroll :width 'stretch :height '(vh 100)
          :box-sizing 'border-box :overflow 'scroll
    (box :padding '((lh 1) (ch 2))
         :bgcolor (aaron-ui-performance--color 'bg-elevated "#2A3247")
      (column :width 'stretch
        (text :font-weight 'bold
              :color (aaron-ui-performance--color 'fg-strong "#F4F7FF")
              (expr (format "%s  Performance Watch"
                            (aaron-ui-board-icon 'chart))))
        (text :color (aaron-ui-performance--color 'fg-muted "#8B90A8")
              (expr (if (eq (etaf-value selected) 'activity)
                        (or (plist-get (etaf-value activity-state) :summary)
                            "Loading tasks…")
                      (or (plist-get (etaf-value state) :summary)
                          "Loading…"))))))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 0) (ch 2))
      (aaron-ui-performance-tab
       :label "Overview" :value 'overview :selected selected :owner owner)
      (aaron-ui-performance-tab
       :label "Processes" :value 'processes :selected selected :owner owner)
      (aaron-ui-performance-tab
       :label "Buffers" :value 'buffers :selected selected :owner owner)
      (aaron-ui-performance-tab
       :label "Hooks & timers" :value 'hooks :selected selected :owner owner)
      (aaron-ui-performance-tab
       :label "后台任务" :value 'activity :selected selected :owner owner)
      (aaron-ui-performance-tab
       :label "Noema" :value 'noema :selected selected :owner owner))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 1) (ch 2))
      (aaron-ui-performance-action
       :label "Refresh" :command #'+performance/watch-refresh-current
       :owner owner)
      (aaron-ui-performance-action
       :label (if (plist-get (etaf-value
                             (if (eq (etaf-value selected) 'activity)
                                 activity-state state)) :auto)
                  "Auto: on" "Auto: off")
       :command #'+performance/watch-toggle-auto-current :owner owner)
      (box :if (eq (etaf-value selected) 'activity)
        (aaron-ui-performance-action
         :label (if (plist-get (etaf-value activity-state) :timers)
                    "Timers: shown" "Timers: hidden")
         :command #'+activity-board/toggle-timers :owner owner))
      (box :if (eq (etaf-value selected) 'activity)
        (aaron-ui-performance-action
         :label "Filter" :command #'+activity-board/filter :owner owner))
      (box :if (eq (etaf-value selected) 'noema)
        (aaron-ui-performance-action
         :label "Wiki" :command #'+performance/noema-open-wiki :owner owner))
      (box :if (eq (etaf-value selected) 'noema)
        (aaron-ui-performance-action
         :label "Agenda" :command #'+performance/noema-open-agenda :owner owner))
      (box :if (eq (etaf-value selected) 'noema)
        (aaron-ui-performance-action
         :label "Tasks" :command (lambda () (interactive)
                                    (+performance/watch-select-tab 'activity))
         :owner owner))
      (box :if (eq (etaf-value selected) 'noema)
        (aaron-ui-performance-action
         :label "Stop host" :command #'+performance/noema-stop-host :owner owner))
      (box :if (not (memq (etaf-value selected) '(activity noema)))
        (aaron-ui-performance-action
         :label (if (plist-get (etaf-value state) :recording)
                    "Recording: on" "Recording: off")
         :command #'+performance/toggle-recording :owner owner))
      (box :if (not (memq (etaf-value selected) '(activity noema)))
        (aaron-ui-performance-action
         :label "Save record" :command #'+performance/save-record :owner owner))
      (box :if (not (memq (etaf-value selected) '(activity noema)))
        (aaron-ui-performance-action
         :label "Profiler" :command #'+performance/profiler-start-cpu
         :owner owner)))
    (aaron-ui-performance-section
     :for (section (seq-filter
                    (lambda (item)
                      (eq (plist-get item :tab)
                          (etaf-value selected)))
                    (plist-get (etaf-value state) :sections)))
     :key (plist-get section :key)
     :section section)
    (box :if (eq (etaf-value selected) 'processes)
      (aaron-ui-activity-section
       :for (section (seq-filter
                      (lambda (item)
                        (memq (plist-get item :key)
                              '(task service connection)))
                      (plist-get (etaf-value activity-state) :sections)))
       :key (plist-get section :key)
       :section section :owner owner))
    (box :if (eq (etaf-value selected) 'buffers)
      (column :width 'stretch
        (box :padding '((lh 1) (ch 1))
             :color (aaron-ui-performance--color 'accent-cyan "#9DE7FF")
          (text "Buffer management · RET open · menu key/right-click actions"))
        (aaron-ui-performance-buffer-row
         :for (row (plist-get (etaf-value state) :buffer-rows))
         :key (plist-get row :ref)
         :row row :owner owner)))
    (box :if (eq (etaf-value selected) 'activity)
      (aaron-ui-activity-content :state activity-state :owner owner
                                 :embedded t))
    (box :if (not (eq (etaf-value selected) 'activity))
         :padding '((lh 1) (ch 2))
         :color (aaron-ui-performance--color 'fg-muted "#8B90A8")
      (text "g refresh  G collect  y copy page  a auto  s save  R record  o folder  p/P profiler  q quit"))))

(defun aaron-ui-performance-view (state selected activity-state owner)
  "Return an ETAF View for STATE, SELECTED and ACTIVITY-STATE in OWNER."
  (etaf-view
   (aaron-ui-performance-app
    :state state :selected selected :activity-state activity-state
    :owner owner)))

(provide 'aaron-ui-performance)
;;; aaron-ui-performance.el ends here

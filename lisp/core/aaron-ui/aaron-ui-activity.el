;;; aaron-ui-activity.el --- ETAF Activity page -*- lexical-binding: t -*-

;;; Commentary:
;; The Activity page is the one Aaron UI surface backed by ETAF.  It borrows
;; its colours from the same semantic palette as Dashboard and the other
;; boards, while ETAF owns layout, focus and retained updates.

;;; Code:

(require 'etaf)
(require 'aaron-ui)
(require 'aaron-ui-board)

(declare-function +activity-board/refresh "ui/activity/config" ())
(declare-function +activity-board/toggle-auto-refresh "ui/activity/config" ())
(declare-function +activity-board/toggle-timers "ui/activity/config" ())
(declare-function +activity-board/filter "ui/activity/config" (pattern))
(declare-function +activity-board--activate-row "ui/activity/config" (row))

(defun aaron-ui-activity--color (token fallback)
  "Return Aaron UI TOKEN or FALLBACK for the current theme."
  (aaron-ui-color token fallback))

(etaf-define-component aaron-ui-activity-button (&key label command owner primary)
  "Render one Activity COMMAND named LABEL for OWNER."
  :view
  (box :role 'button :tab-index 0
       :padding '((lh 0) (ch 2))
       :border (aaron-ui-activity--color 'border-subtle "#4B5268")
       :bgcolor (aaron-ui-activity--color
                 (if primary 'accent-blue 'bg-elevated)
                 (if primary "#A9CBFF" "#2A3247"))
       :color (if primary "#1B2435"
                (aaron-ui-activity--color 'fg-main "#E4ECFF"))
       :on-press (lambda ()
                   (with-current-buffer owner
                     (call-interactively command)))
    (text (expr label))))

(etaf-define-component aaron-ui-activity-stat (&key label tone)
  "Render one compact Activity status badge."
  :view
  (box :padding '((lh 0) (ch 1))
       :border (aaron-ui-activity--color 'border-subtle "#4B5268")
       :bgcolor (aaron-ui-activity--color 'bg-surface "#1E2536")
       :color (aaron-ui-activity--color
               (pcase tone
                 ('success 'accent-green)
                 ('warning 'accent-yellow)
                 ('danger 'accent-red-strong)
                 ('muted 'fg-muted)
                 (_ 'accent-blue))
               "#A9CBFF")
    (text (expr label))))

(etaf-define-component aaron-ui-activity-row (&key row owner)
  "Render one focused Activity ROW in OWNER."
  :view
  (box :ref (plist-get row :ref)
       :role 'button :tab-index 0
       :padding '((lh 0) (ch 2))
       :bgcolor (aaron-ui-activity--color 'bg-surface "#1E2536")
       :color (aaron-ui-activity--color 'fg-main "#E4ECFF")
       :on-press
       (lambda ()
         (with-current-buffer owner
           (+activity-board--activate-row row)))
    (column :width 'stretch
      (flex :flex-flow '(row nowrap) :gap '((lh 0) (ch 2))
        (box :width '(ch 2)
             :color (aaron-ui-activity--color 'accent-cyan "#9DE7FF")
          (text (expr (or (plist-get row :icon)
                          (aaron-ui-board-icon 'status)))))
        (box :width '(ch 12)
          :color (aaron-ui-activity--color
                  (pcase (plist-get row :kind)
                    ('task 'accent-yellow)
                    ('service 'accent-cyan)
                    ('connection 'accent-blue)
                    (_ 'fg-muted))
                  "#A9CBFF")
          (text (expr (plist-get row :badge))))
        (box :flex-grow 1 :flex-shrink 1 :min-width '(ch 16)
          (text (expr (plist-get row :title))))
        (box :width '(ch 11)
          :color (aaron-ui-activity--color 'fg-muted "#8B90A8")
          (text (expr (or (plist-get row :meta) "")))))
      (box :if (and (plist-get row :detail)
                    (not (equal (plist-get row :detail) "")))
           :padding '((lh 0) (ch 16))
           :color (aaron-ui-activity--color 'fg-dim "#A9BED3")
        (text (expr (plist-get row :detail)))))))

(etaf-define-component aaron-ui-activity-section (&key section owner)
  "Render a titled Activity SECTION for OWNER."
  :view
  (column :width 'stretch
    (box :padding '((lh 1) (ch 1))
         :border (aaron-ui-activity--color 'border-subtle "#4B5268")
         :color (aaron-ui-activity--color 'accent-cyan "#9DE7FF")
         :font-weight 'bold
      (text (expr (format "%s  %d"
                          (plist-get section :title)
                          (length (plist-get section :rows))))))
    (box :if (null (plist-get section :rows))
         :padding '((lh 0) (ch 2))
         :color (aaron-ui-activity--color 'fg-muted "#8B90A8")
      (text "Nothing here"))
    (aaron-ui-activity-row
     :for (row (plist-get section :rows))
     :key (plist-get row :ref)
     :row row :owner owner)))

(etaf-define-component aaron-ui-activity-app (&key state owner)
  "Render OWNER's retained Activity STATE."
  :view
  (column :width 'stretch
    (box :padding '((lh 1) (ch 2))
         :bgcolor (aaron-ui-activity--color 'bg-elevated "#2A3247")
      (column :width 'stretch
        (text :font-weight 'bold
              :color (aaron-ui-activity--color 'fg-strong "#F4F7FF")
              (expr (format "%s  Activity"
                            (aaron-ui-board-icon 'process))))
        (text :color (aaron-ui-activity--color 'fg-muted "#8B90A8")
              (expr (or (plist-get (etaf-value state) :summary) "Loading…")))))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 0) (ch 2))
      (aaron-ui-activity-stat
       :for (stat (plist-get (etaf-value state) :stats))
       :key (car stat) :label (car stat) :tone (cdr stat)))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 1) (ch 2))
      (aaron-ui-activity-button
       :label "Refresh" :command #'+activity-board/refresh
       :owner owner :primary t)
      (aaron-ui-activity-button
       :label (if (plist-get (etaf-value state) :auto) "Auto: on" "Auto: off")
       :command #'+activity-board/toggle-auto-refresh :owner owner)
      (aaron-ui-activity-button
       :label (if (plist-get (etaf-value state) :timers) "Timers: shown" "Timers: hidden")
       :command #'+activity-board/toggle-timers :owner owner)
      (aaron-ui-activity-button
       :label "Filter" :command #'+activity-board/filter :owner owner))
    (box :if (plist-get (etaf-value state) :filter)
         :padding '((lh 0) (ch 2))
         :color (aaron-ui-activity--color 'accent-yellow "#F9E2AF")
      (text (expr (format "FILTER  %s"
                          (plist-get (etaf-value state) :filter)))))
    (aaron-ui-activity-section
     :for (section (plist-get (etaf-value state) :sections))
     :key (plist-get section :key)
     :section section :owner owner)
    (box :padding '((lh 1) (ch 2))
         :color (aaron-ui-activity--color 'fg-muted "#8B90A8")
      (column :width 'stretch
        (text "j/k move   RET open   x stop   i interrupt   X kill   d details   w copy")
        (text "g refresh   a auto   / filter   t timers   q close")))
    (box :padding '((lh 1) (ch 2))
         :color (aaron-ui-activity--color 'accent-cyan "#9DE7FF")
      (text "Built-in managers"))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 0) (ch 2))
      (aaron-ui-activity-button
       :for (link (plist-get (etaf-value state) :native-links))
       :key (plist-get link :label)
       :label (plist-get link :label)
       :command (plist-get link :command) :owner owner))
    (box :padding '((lh 1) (ch 2))
         :color (aaron-ui-activity--color 'accent-cyan "#9DE7FF")
      (text "This configuration"))
    (flex :flex-flow '(row wrap) :gap '((lh 0) (ch 1))
          :padding '((lh 0) (ch 2))
      (aaron-ui-activity-button
       :for (link (plist-get (etaf-value state) :config-links))
       :key (plist-get link :label)
       :label (plist-get link :label)
       :command (plist-get link :command) :owner owner))))

(defun aaron-ui-activity-view (state owner)
  "Return an ETAF Activity View for STATE and OWNER."
  (etaf-view (aaron-ui-activity-app :state state :owner owner)))

(provide 'aaron-ui-activity)
;;; aaron-ui-activity.el ends here

;;; init-display-rules.el --- Extra window display rules -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defgroup my/display-rules nil
  "Extra shackle rules for development buffers."
  :group 'windows)

(defcustom my/display-rules-bottom-size 0.32
  "Default bottom-side window size for development buffers."
  :type 'float
  :group 'my/display-rules)

(defcustom my/display-rules-side-size 0.33
  "Default side window size for side-panel style buffers."
  :type 'float
  :group 'my/display-rules)

(defvar shackle-rules)

(defcustom my/display-rules-roam-size 0.36
  "Width ratio for roam UI side panels."
  :type 'float
  :group 'my/display-rules)

(defconst my/display-rules-shackle-rules
  `((compilation-mode             :select t :align below :size ,my/display-rules-bottom-size)
    ("\\*test\\*"                :select t :align below :size ,my/display-rules-bottom-size :regexp t)
    ("\\*task\\*"                :select t :align below :size ,my/display-rules-bottom-size :regexp t)
    ("\\*run\\*"                 :select t :align below :size ,my/display-rules-bottom-size :regexp t)
    ("\\*xref\\*"                :select t :align below :size ,my/display-rules-bottom-size :regexp t)
    ("\\*claude-code-ide\\*"     :select t :align right :size ,my/display-rules-side-size :regexp t)
    ("\\*Diagnostics:.*\\*"      :select t :align below :size ,my/display-rules-bottom-size :regexp t)
    ;; Roam UI panels — right side, consistent width
    ("\\*roam-\\|\\*aaronnote-roam-" :select t :align right :size ,my/display-rules-roam-size :regexp t))
  "Additional `shackle-rules' for development workflows.")

(with-eval-after-load 'shackle
  (dolist (rule (reverse my/display-rules-shackle-rules))
    (add-to-list 'shackle-rules rule)))

(provide 'init-display-rules)
;;; init-display-rules.el ends here

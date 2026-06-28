;;; init-display-rules.el --- Extra window display rules -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(defgroup my/display-rules nil
  "Extra shackle rules for development buffers."
  :group 'windows)

(config-defvar my/display-rules-bottom-size nil
  "Default bottom-side window size for development buffers."
  :type 'float
  :group 'my/display-rules)

(config-defvar my/display-rules-side-size nil
  "Default side window size for side-panel style buffers."
  :type 'float
  :group 'my/display-rules)

(defvar shackle-rules)

(config-defvar my/display-rules-roam-size nil
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

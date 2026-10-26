;;; ui/popup/config.el --- Extra window display rules -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'config)

(defgroup +display-rules nil
  "Extra shackle rules for development buffers."
  :group 'windows)

(config-defvar +display-rules-bottom-size nil
  "Default bottom-side window size for development buffers."
  :type 'float
  :group '+display-rules)

(config-defvar +display-rules-side-size nil
  "Default side window size for side-panel style buffers."
  :type 'float
  :group '+display-rules)

(defvar shackle-rules)

(config-defvar +display-rules-roam-size nil
  "Width ratio for roam UI side panels."
  :type 'float
  :group '+display-rules)

(defconst +display-rules-shackle-rules
  `((compilation-mode             :select t :align below :size ,+display-rules-bottom-size)
    ("\\*test\\*"                :select t :align below :size ,+display-rules-bottom-size :regexp t)
    ("\\*task\\*"                :select t :align below :size ,+display-rules-bottom-size :regexp t)
    ("\\*run\\*"                 :select t :align below :size ,+display-rules-bottom-size :regexp t)
    ("\\*xref\\*"                :select t :align below :size ,+display-rules-bottom-size :regexp t)
    ("\\*Diagnostics:.*\\*"      :select t :align below :size ,+display-rules-bottom-size :regexp t)
    ;; Roam UI panels — right side, consistent width
    ("\\*roam-\\|\\*Noema roam " :select t :align right :size ,+display-rules-roam-size :regexp t))
  "Additional `shackle-rules' for development workflows.")

(with-eval-after-load 'shackle
  (dolist (rule (reverse +display-rules-shackle-rules))
    (add-to-list 'shackle-rules rule)))

(provide 'ui/popup/config)
;;; ui/popup/config.el ends here

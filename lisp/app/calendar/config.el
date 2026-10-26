;;; app/calendar/config.el --- Calendar workbench -*- lexical-binding: t; -*-

;;; Commentary:
;; A full-year view kept separate from the ordinary three-month calendar.

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defvar-local +year-calendar-year nil
  "Year displayed in the current full-year calendar buffer.")

(defvar +year-calendar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ">") #'+calendar/year-calendar-forward)
    (define-key map (kbd "<") #'+calendar/year-calendar-backward)
    (define-key map (kbd "]") #'+calendar/year-calendar-forward)
    (define-key map (kbd "[") #'+calendar/year-calendar-backward)
    (define-key map (kbd "g") #'+calendar/year-calendar-current)
    map)
  "Keymap for `+year-calendar-mode'.")

(define-minor-mode +year-calendar-mode
  "Minor mode for navigating the full-year calendar view."
  :init-value nil
  :lighter " Year"
  :keymap +year-calendar-mode-map)

(defun +year-calendar--insert (year)
  "Render YEAR as four rows of three months in the current buffer."
  (let ((inhibit-read-only t)
        (month 0))
    (widen)
    (erase-buffer)
    (dotimes (_row 4)
      (let ((row-start (point-max)))
        (narrow-to-region row-start row-start)
        (dotimes (column 3)
          (calendar-generate-month (cl-incf month) year (+ 3 (* 25 column))))
        (widen)
        (goto-char (point-max))
        (insert "\n")))
    (goto-char (point-min))))

(defun +calendar/calendar-year (&optional year)
  "Display all twelve months of YEAR.
YEAR defaults to the current calendar year."
  (interactive)
  (require 'cl-lib)
  (let ((year (or year (string-to-number (format-time-string "%Y"))))
        (buffer (get-buffer-create "*Year Calendar*")))
    (pop-to-buffer buffer)
    (unless (derived-mode-p 'calendar-mode)
      (calendar-mode))
    (setq-local +year-calendar-year year
                displayed-month 1
                displayed-year year)
    (+year-calendar-mode 1)
    (+year-calendar--insert year)
    (run-hooks 'calendar-move-hook)
    buffer))

(defun +calendar/year-calendar-forward (&optional count)
  "Move the full-year calendar forward by COUNT years."
  (interactive "p")
  (unless +year-calendar-mode
    (user-error "This is not a full-year calendar"))
  (+calendar/calendar-year (+ +year-calendar-year (or count 1))))

(defun +calendar/year-calendar-backward (&optional count)
  "Move the full-year calendar backward by COUNT years."
  (interactive "p")
  (+calendar/year-calendar-forward (- (or count 1))))

(defun +calendar/year-calendar-current ()
  "Return the full-year calendar to the current year."
  (interactive)
  (+calendar/calendar-year (string-to-number (format-time-string "%Y"))))

(provide 'app/calendar/config)
;;; app/calendar/config.el ends here

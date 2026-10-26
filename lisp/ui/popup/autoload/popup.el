;;; ui/popup/autoload/popup.el --- Popup rule registration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `set-popup-rule!' and `set-popup-rules!', Doom's interface for declaring
;; how a buffer is shown when it pops up.  Each module declares its own
;; popups; this module turns the declarations into `shackle-rules'.  The
;; functions are autoloaded, so a module may call them before or after
;; ui/popup loads.
;;
;; A rule is (PREDICATE . PLIST) in shackle's terms: PREDICATE is a buffer
;; name, a regexp (with :regexp t) or a major mode; PLIST takes shackle's
;; keys (:select :align :size :same :inhibit-window-quit ...).  As in Doom,
;; a rule declared later takes precedence over an earlier one.
;;
;; Adapted from Doom Emacs (MIT, Henrik Lissner):
;;   modules/ui/popup/autoload/settings.el @ 42a0479

;;; Code:

(require 'cl-lib)

(defvar shackle-rules)

;;;###autoload
(defvar +popup-rules nil
  "Declared popup rules, highest precedence first.")

(defun +popup--apply ()
  "Make `shackle-rules' the declared rules."
  (setq shackle-rules (copy-sequence +popup-rules)))

;;;###autoload
(defun set-popup-rule! (predicate &rest plist)
  "Show buffers matching PREDICATE as a popup described by PLIST.
PREDICATE and PLIST are a shackle rule: a buffer name, a regexp with
:regexp t, or a major mode, and shackle's keys.  A rule declared later
takes precedence; declaring PREDICATE again replaces its rule."
  (setq +popup-rules
        (cons (cons predicate plist)
              (cl-remove predicate +popup-rules :key #'car :test #'equal)))
  (+popup--apply))

;;;###autoload
(defun set-popup-rules! (&rest rulesets)
  "Declare every rule in RULESETS, lists of (PREDICATE . PLIST).
Within the call, an earlier rule keeps precedence over a later one; the
whole block takes precedence over rules declared before it."
  (let ((rules (apply #'append rulesets)))
    (setq +popup-rules
          (append rules
                  (cl-remove-if (lambda (rule) (assoc (car rule) rules))
                                +popup-rules))))
  (+popup--apply))

;;; ui/popup/autoload/popup.el ends here

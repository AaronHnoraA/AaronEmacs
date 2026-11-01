;;; os/neomacs/trial.el --- Opt-in Neomacs startup experiment -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded only by bin/neomacs-trial, after the existing compatibility module.
;; Keep experimental changes out of the normal GNU Emacs startup path.

;;; Code:

(require 'aaron-lib)
(require 'cl-lib)

(defvar +neomacs-p)
(defvar +neomacs-modifier-remap)
(defvar +background--frames)
(defvar save-place-alist)
(defvar save-place-abbreviate-file-names)
(defvar +neomacs--normalizing-save-place nil)
(declare-function +neomacs/apply-modifier-remap "os/neomacs/config" ())

(defun +neomacs-native-modifiers-p ()
  "Return non-nil when Neomacs honours native macOS modifier settings."
  (and +neomacs-p (fboundp 'neomacs-set-modifier-policy)))

(defun +neomacs--native-modifier-remap-a (fn)
  "Use FN's legacy translation only when native modifier policy is absent."
  (unless (and (null +neomacs-modifier-remap)
               (+neomacs-native-modifiers-p))
    (funcall fn)))

(defun +neomacs--swap-native-modifiers-a (fn)
  "Swap native modifiers when supported, otherwise call FN."
  (if (+neomacs-native-modifiers-p)
      (progn
        (cl-rotatef mac-option-modifier mac-command-modifier)
        (message "Option: %s, Command: %s"
                 mac-option-modifier mac-command-modifier))
    (funcall fn)))

(defun +neomacs--clear-font-cache-a (&rest _args)
  "Keep custom faces alive when a font refresh asks to clear caches.
Neomacs 0.0.20 clears its runtime face definitions as well as font caches,
but leaves Lisp's `facep' data intact.  Setting face attributes still
refreshes fonts; avoid the destructive cache clear in this experiment."
  nil)

(defun +neomacs--background-frame-p (frame)
  "Return non-nil when FRAME was deliberately hidden by the background app."
  (or (memq frame (bound-and-true-p +background--frames))
      (memq '+background--hide-all-h window-setup-hook)))

(defun +neomacs--reveal-startup-frames-a (fn)
  "Call FN without frames that the background app deliberately hid."
  (let* ((frame-list-function (symbol-function 'frame-list))
         (frames (cl-remove-if #'+neomacs--background-frame-p
                               (funcall frame-list-function))))
    (cl-letf (((symbol-function 'frame-list) (lambda () frames)))
      (funcall fn))))

(defun +neomacs--normalize-pdf-save-place-a (fn &rest args)
  "Preserve numeric PDF page metadata while FN normalizes saved file names."
  (if +neomacs--normalizing-save-place
      (apply fn args)
    (let ((pages (make-hash-table :test #'equal))
          (normalize (if save-place-abbreviate-file-names
                         #'abbreviate-file-name #'expand-file-name))
          (non-essential t)
          (+neomacs--normalizing-save-place t))
      (setq save-place-alist
            (mapcar
             (lambda (entry)
               (if-let* ((metadata (and (listp (cdr entry)) (cdr entry)))
                         (page (assq 'pdf-page metadata))
                         ((integerp (cdr page))))
                   (let ((file (funcall normalize (car entry))))
                     (unless (gethash file pages)
                       (puthash file metadata pages))
                     (cons (car entry) (cdr page)))
                 entry))
             save-place-alist))
      (unwind-protect
          (apply fn args)
        (dolist (entry save-place-alist)
          (when-let* ((metadata (gethash (funcall normalize (car entry)) pages)))
            (setcdr entry metadata)))))))

(when +neomacs-p
  (require 'server)
  (when (and (not noninteractive) (equal server-name "server"))
    (setq server-name "neomacs-trial"))
  (advice-add '+neomacs--effective-modifier-remap :around
              #'+neomacs--native-modifier-remap-a)
  (+neomacs/apply-modifier-remap)
  (advice-add '+mac-swap-option-and-command :around
              #'+neomacs--swap-native-modifiers-a)
  (advice-add 'clear-font-cache :override #'+neomacs--clear-font-cache-a)
  (advice-add '+neomacs/reveal-frames :around #'+neomacs--reveal-startup-frames-a)
  (after! saveplace
    (advice-add 'save-place--normalize-alist :around
                #'+neomacs--normalize-pdf-save-place-a)))

(provide 'os/neomacs/trial)
;;; os/neomacs/trial.el ends here

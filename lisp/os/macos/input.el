;;; os/macos/input.el --- Keep the input method out of Evil commands -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; With Pinyin selected, the keys of Normal state go to the input method:
;; `j' opens a candidate window instead of moving down.  As Doom's
;; :input chinese does with fcitx, leaving Insert state selects an ASCII
;; input source, and entering Insert state again brings back the source that
;; was in use.  The `macos-input' module (tools/macos-input, built by
;; `bin/emacs-app build') reads and selects macOS input sources; without it
;; nothing is switched.

;;; Code:

(require 'config)
(require 'aaron-lib)

(declare-function macos-input-source "ext:macos-input" ())
(declare-function macos-input-select-source "ext:macos-input" (id))

(defvar +macos-input-module
  (expand-file-name "var/macos-input/macos-input.dylib" user-emacs-directory)
  "The built `macos-input' module.")

(config-defvar +macos-input-switch t
  "Whether leaving Evil Insert state selects `+macos-input-ascii-source'.
Entering Insert state again selects the source that was in use."
  :type 'boolean
  :group '+macos)

(config-defvar +macos-input-ascii-source "com.apple.keylayout.ABC"
  "ID of the input source selected outside Evil Insert state."
  :type 'string
  :group '+macos)

(defvar +macos-input--insert-source nil
  "The source in use when Insert state last ended, unless it was the ASCII one.")

(defun +macos-input-available-p ()
  "Return non-nil when the `macos-input' module is loaded or loadable."
  (or (featurep 'macos-input)
      (and (fboundp 'module-load)
           (file-exists-p +macos-input-module)
           (ignore-errors (module-load +macos-input-module) t))))

(defun +macos-input--active-p ()
  "Return non-nil when input sources should follow Evil states."
  (and +macos-input-switch
       ;; Batch runs (tests, builds) change Evil states too, and must not
       ;; change the input source of the person at the keyboard.
       (not noninteractive)
       (+macos-input-available-p)))

(defun +macos-input-leave-insert-h ()
  "Remember the current input source and select the ASCII one."
  (when (+macos-input--active-p)
    (let ((current (macos-input-source)))
      (unless (equal current +macos-input-ascii-source)
        (setq +macos-input--insert-source current)
        (macos-input-select-source +macos-input-ascii-source)))))

(defun +macos-input-enter-insert-h ()
  "Select again the input source that was in use when Insert state ended."
  (when (and +macos-input--insert-source (+macos-input--active-p))
    (macos-input-select-source +macos-input--insert-source)
    (setq +macos-input--insert-source nil)))

(after! evil
  (add-hook 'evil-insert-state-exit-hook #'+macos-input-leave-insert-h)
  (add-hook 'evil-insert-state-entry-hook #'+macos-input-enter-insert-h))

(provide 'os/macos/input)
;;; os/macos/input.el ends here

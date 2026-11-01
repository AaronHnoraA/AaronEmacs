;;; tools/agent/appearance.el --- Native chat appearance and input cues -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the native transcript, folds and input.  A single moving overlay shows
;; submission semantics without entering the draft, transcript or undo history.

;;; Code:
(require 'aaron-ui)
(require 'agent-shell)
(require 'face-remap)
(require 'map)
(require 'subr-x)

(defvar +agent-acp-changed-functions)
(defvar +agent-acp-run-owned-functions)
(defvar +agent-acp--held-prompts)
(declare-function +agent-acp-busy-p "tools/agent/acp" (buffer))
(declare-function +agent-acp-client-exited-p "tools/agent/acp" (buffer))
(declare-function +agent-acp-held-prompt-count "tools/agent/acp" (buffer))
(declare-function +agent-acp-pending-prompt-count "tools/agent/acp" (buffer))
(declare-function +agent-acp-subscribe "tools/agent/acp" (&rest args))
(declare-function +agent-acp-unsubscribe "tools/agent/acp" (&rest args))
(declare-function +agent-shell-busy-submit-steer-or-queue "tools/agent/config" (prompt))

(defun +agent-appearance-apply-faces-h (&rest _)
  "Apply Aaron's current palette to native agent faces."
  (dolist (entry '((agent-shell-prompt accent-teal semi-bold)
                   (agent-shell-chat-me-label accent-teal semi-bold)
                   (agent-shell-chat-agent-label role-faded normal)
                   (agent-shell-section-heading role-strong semi-bold)
                   (agent-shell-section-annotation role-faded normal)
                   (agent-shell-thought-body role-faded normal)
                   (agent-shell-secondary role-faded normal)
                   (agent-shell-pending role-faded normal)
                   (agent-shell-success accent-green normal)
                   (agent-shell-warning accent-yellow semi-bold)
                   (agent-shell-error role-critical semi-bold)))
    (aaron-ui-set-face (car entry) :foreground (nth 1 entry)
                      :background 'unspecified :weight (nth 2 entry)
                      :box nil :inverse-video nil :extend nil))
  ;; Authored text keeps the normal body face; emphasis belongs to the prompt.
  (aaron-ui-set-face 'agent-shell-input :inherit 'default
                    :foreground 'unspecified :background 'unspecified
                    :weight 'normal :box 'unspecified :extend nil))

(defvar-local +agent-input--overlay nil)
(defvar-local +agent-input--subscriptions nil)
(defvar-local +agent-input--face-remap nil)

(defun +agent-input--submission (function)
  "Describe the actual busy submission FUNCTION without invoking it."
  (cond
   ((run-hook-with-args-until-success '+agent-acp-run-owned-functions (current-buffer))
    "queue")
   ((eq function #'+agent-shell-busy-submit-steer-or-queue)
    (if (agent-shell-steering-supported-p) "steer" "queue"))
   ((memq function '(agent-shell-busy-submit-queue agent-shell-prompt-queue)) "queue")
   ((eq function 'agent-shell-busy-submit-steer) "steer")
   (t "send")))

(defun +agent-input--hint ()
  "Return a bounded input hint using only this session's in-memory state."
  (let* ((buffer (current-buffer))
         (busy (+agent-acp-busy-p buffer))
         (count (+agent-acp-pending-prompt-count buffer))
         (held (+agent-acp-held-prompt-count buffer))
         (next (or (car +agent-acp--held-prompts)
                   (car (map-elt agent-shell--state :pending-prompts))))
         (hint (cond
                ((+agent-acp-client-exited-p buffer) " Client exited · /restart to reconnect")
                ((not (map-elt agent-shell--state :init-finished)) " Connecting · draft stays editable")
                (busy
                 (format " RET %s · M-RET %s · C-c C-c stop"
                         (+agent-input--submission agent-shell-busy-submit-default-function)
                         (+agent-input--submission agent-shell-busy-submit-override-function)))
                (t " RET send · / commands · C-c C-z actions"))))
    (concat
     (propertize hint 'face 'agent-shell-secondary)
     (when (> count 0)
       (concat
        "\n"
        (propertize (format " Queue %d%s · /queue" count
                            (if (> held 0) (format " (%d held)" held) ""))
                    'face (if (> held 0) 'agent-shell-warning 'agent-shell-secondary))
        (when (stringp next)
          (propertize
           (format " · %s: %s" (if (> held 0) "held" "next")
                   (truncate-string-to-width
                    (string-trim (replace-regexp-in-string
                                  "[\n\r\t ]+" " " (substring next 0 (min 160 (length next)))))
                    40 nil nil "…"))
           'face 'agent-shell-secondary))))
     "\n")))

(defun +agent-input--refresh (&optional buffer)
  "Refresh BUFFER's one input overlay, without changing text or point."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (bound-and-true-p +agent-input-mode)
        (if (agent-shell--prompt-input-start)
            (let ((position (marker-position (car comint-last-prompt)))
                  (hint (+agent-input--hint)))
              (unless (overlayp +agent-input--overlay)
                (setq +agent-input--overlay (make-overlay position position nil t t)))
              (move-overlay +agent-input--overlay position position)
              (unless (equal hint (overlay-get +agent-input--overlay 'before-string))
                (overlay-put +agent-input--overlay 'before-string hint)))
          (when (overlayp +agent-input--overlay)
            (delete-overlay +agent-input--overlay)))))))

(defun +agent-input-beginning-of-line (arg)
  "Move ARG lines as usual, stopping at the editable input's boundary."
  (interactive "p")
  (let* ((start (agent-shell--prompt-input-start))
         (input (and start (>= (point) (marker-position (car comint-last-prompt))))))
    (move-beginning-of-line arg)
    (when (and input (< (point) start)) (goto-char start))))

(defvar +agent-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap move-beginning-of-line] #'+agent-input-beginning-of-line)
    map))

(defun +agent-input--cleanup-h ()
  "Release the input overlay and its native prompt subscription."
  (when (overlayp +agent-input--overlay) (delete-overlay +agent-input--overlay))
  (dolist (subscription +agent-input--subscriptions)
    (+agent-acp-unsubscribe :buffer (current-buffer) :subscription subscription))
  (when +agent-input--face-remap (face-remap-remove-relative +agent-input--face-remap))
  (setq +agent-input--overlay nil +agent-input--subscriptions nil +agent-input--face-remap nil))

(define-minor-mode +agent-input-mode
  "Show native submission and queue cues above this agent's editable prompt."
  :lighter nil :keymap +agent-input-mode-map
  (if +agent-input-mode
      (progn
        (unless (derived-mode-p 'agent-shell-mode)
          (setq +agent-input-mode nil)
          (user-error "Open an agent-shell buffer first"))
        (+agent-input--cleanup-h)
        ;; Comint faces submitted input independently of agent-shell's own
        ;; input face.  Keep this change local to the chat, leaving shells alone.
        (setq +agent-input--face-remap
              (face-remap-add-relative 'comint-highlight-input :inherit 'default :weight 'normal))
        (let ((buffer (current-buffer)))
          (dolist (event '(init-finished prompt-ready))
            (push (+agent-acp-subscribe
                   :buffer buffer :event event
                   :callback (lambda (_event) (+agent-input--refresh buffer)))
                  +agent-input--subscriptions)))
        (add-hook 'kill-buffer-hook #'+agent-input--cleanup-h nil t)
        (add-hook 'change-major-mode-hook #'+agent-input--cleanup-h nil t)
        (+agent-input--refresh))
    (+agent-input--cleanup-h)
    (remove-hook 'kill-buffer-hook #'+agent-input--cleanup-h t)
    (remove-hook 'change-major-mode-hook #'+agent-input--cleanup-h t)))

(defun +agent-input-enable-h ()
  "Enable input cues in the current native chat."
  (+agent-input-mode 1))

(+agent-appearance-apply-faces-h)
(add-hook 'enable-theme-functions #'+agent-appearance-apply-faces-h)
(add-hook 'agent-shell-mode-hook #'+agent-input-enable-h)
(add-hook '+agent-acp-changed-functions #'+agent-input--refresh)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-shell-mode) (+agent-input-enable-h))))

(provide 'tools/agent/appearance)
;;; tools/agent/appearance.el ends here

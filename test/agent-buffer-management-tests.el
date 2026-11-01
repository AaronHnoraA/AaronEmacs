;;; agent-buffer-management-tests.el --- Visible sessions and side-window splits -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tools/agent/acp)
(require 'ui/windows/config)
(require 'completion/minibuffer/config)
(require 'config/default/config)
(require 'portal-input)
(require 'ibuffer)

(defmacro agent-buffer-management-tests--layout (&rest body)
  (declare (indent 0))
  `(let ((editor (generate-new-buffer "agent-buffer-editor"))
         (panel (generate-new-buffer "agent-buffer-panel"))
         (buffers (buffer-list)))
     (save-window-excursion
       (unwind-protect
           (progn
             (delete-other-windows)
             (switch-to-buffer editor)
             (let* ((main (selected-window))
                    (side (display-buffer-in-side-window
                           panel '((side . right) (slot . 0) (window-width . 0.3)))))
               (select-window side)
               ,@body))
         (dolist (buffer (cons editor (cons panel (list (get-buffer "*Ibuffer*")))))
           (when (and (buffer-live-p buffer) (not (memq buffer buffers)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun agent-buffer-management-tests--goto-row (buffer)
  (goto-char (point-min))
  (while (and (not (eobp)) (not (eq (ibuffer-current-buffer) buffer)))
    (forward-line 1))
  (should (eq (ibuffer-current-buffer) buffer)))

(ert-deftest c24-agent-buffers-are-listed-individually-in-ibuffer-and-consult ()
  (let ((first (generate-new-buffer " *Agent · first · codex @ project*"))
        (second (generate-new-buffer " *Agent · second · codex @ project*"))
        (listing-before (get-buffer "*Ibuffer*")))
    (save-window-excursion
      (unwind-protect
          (progn
            (dolist (buffer (list first second))
              (with-current-buffer buffer
                (setq major-mode 'agent-shell-mode)
                (setq-local +agent-acp-display-buffer-function nil
                            agent-shell--state '((:session . ((:id . "retained-id"))))))
              (should-not (memq buffer (+consult-buffer-list)))
              (+agent-acp--install-workspace-tabs buffer)
              (should-not (string-prefix-p " " (buffer-name buffer)))
              (should (buffer-local-value 'tab-line-exclude buffer))
              (should (equal (buffer-name buffer)
                             (buffer-local-value 'shell-maker--buffer-name-override buffer)))
              (should (equal (buffer-local-value 'agent-shell--state buffer)
                             '((:session . ((:id . "retained-id"))))))
              (should (memq buffer (+consult-buffer-list))))
            (switch-to-buffer first)
            (should (string-match-p "F[0-9]+" (+ibuffer-frame-tags first)))
            (ibuffer)
            (ibuffer-update nil t)
            (dolist (buffer (list first second))
              (agent-buffer-management-tests--goto-row buffer)
              (should (assoc "Agent" ibuffer-filter-groups))))
        (dolist (buffer (list first second
                              (and (not listing-before) (get-buffer "*Ibuffer*"))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest c24-popup-agent-migration-preserves-display-owner-and-process ()
  (let* ((agent (generate-new-buffer " *Agent · popup · codex @ project*"))
         (process (make-pipe-process :name "visible-agent-buffer-test" :buffer agent :noquery t)))
    (unwind-protect
        (progn
          (with-current-buffer agent
            (setq major-mode 'agent-shell-mode)
            (setq-local +agent-acp-display-buffer-function #'ignore))
          (+agent-acp--install-workspace-tabs agent)
          (should-not (string-prefix-p " " (buffer-name agent)))
          (should (eq (buffer-local-value '+agent-acp-display-buffer-function agent) #'ignore))
          (should (eq (get-buffer-process agent) process))
          (should (process-live-p process)))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p agent) (kill-buffer agent)))))

(ert-deftest c25-open-base-from-side-uses-main-editor ()
  (agent-buffer-management-tests--layout
    (should (eq (aaron-open--base-window) main))
    (should (eq (selected-window) side))))

(ert-deftest c25-window-menu-splits-main-and-keeps-side-window ()
  (dolist (command '(+windows/split-right +windows/split-below))
    (agent-buffer-management-tests--layout
      (call-interactively command)
      (should (window-live-p side))
      (should (eq (window-buffer side) panel))
      (should (eq (window-buffer main) editor))
      (should-not (window-parameter (selected-window) 'window-side))
      (should (derived-mode-p 'ibuffer-mode))
      (should (= (length (window-list)) 3)))))

(ert-deftest c25-native-split-keys-are-routed-from-side-window ()
  (dolist (key '("C-x 2" "C-x 3"))
    (agent-buffer-management-tests--layout
      (call-interactively (key-binding (kbd key)))
      (should (eq (window-buffer side) panel))
      (should-not (window-parameter (selected-window) 'window-side))
      (should (= (length (window-list)) 3)))))

(ert-deftest c25-evil-splits-main-and-keeps-side-window ()
  (dolist (command '(evil-window-split evil-window-vsplit))
    (agent-buffer-management-tests--layout
      (call-interactively command)
      (should (eq (window-buffer side) panel))
      (should-not (window-parameter (selected-window) 'window-side))
      (should (= (length (window-list)) 3)))))

(ert-deftest c25-portal-splits-main-and-keeps-side-window ()
  (agent-buffer-management-tests--layout
    (portal-input-split-right)
    (should (eq (window-buffer side) panel))
    (should (eq (window-buffer main) editor))
    (should (derived-mode-p 'ibuffer-mode))
    (should-not (window-parameter (selected-window) 'window-side))))

(ert-deftest c25-ibuffer-side-list-keeps-row-and-opens-in-main-split ()
  (agent-buffer-management-tests--layout
    (let ((target (generate-new-buffer "agent-buffer-chosen-target")))
      (unwind-protect
          (progn
            (ibuffer)
            (ibuffer-update nil t)
            (let ((listing (current-buffer)))
              (agent-buffer-management-tests--goto-row target)
              (ibuffer-set-mark ibuffer-marked-char)
              (aaron/ibuffer-visit-right)
              (should (eq (window-buffer side) listing))
              (should (eq (window-buffer main) editor))
              (should (eq (current-buffer) target))
              (should-not (window-parameter (selected-window) 'window-side))
              (with-current-buffer listing
                (should (memq target (ibuffer-get-marked-buffers))))))
        (kill-buffer target)))))

(ert-deftest c25-open-entry-failure-keeps-side-selection-and-layout ()
  (agent-buffer-management-tests--layout
    (let ((before (window-state-get nil t)))
      (should-error (+open-entry-in-split 'right (lambda () (error "Open failed"))))
      (should (eq (selected-window) side))
      (should (eq (current-buffer) panel))
      (should (equal before (window-state-get nil t))))))

(ert-deftest c25-window-picker-failure-restores-layout ()
  (agent-buffer-management-tests--layout
    (let ((before (window-state-get nil t)))
      (cl-letf (((symbol-function 'ibuffer) (lambda (&rest _) (error "Picker failed"))))
        (should-error (+windows/split-right)))
      (should (eq (selected-window) side))
      (should (equal before (window-state-get nil t))))))

(ert-deftest c25-native-file-placement-from-side-keeps-tool ()
  (let ((file (make-temp-file "agent-buffer-place" nil ".txt" "contents")))
    (unwind-protect
        (agent-buffer-management-tests--layout
          (aaron-open-place file 'right)
          (should (eq (window-buffer side) panel))
          (should (eq (window-buffer main) editor))
          (should (eq (current-buffer) (find-buffer-visiting file)))
          (should-not (window-parameter (selected-window) 'window-side)))
      (when-let* ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest c25-explicit-here-placement-preserves-native-side-window-choice ()
  (let ((file (make-temp-file "agent-buffer-here" nil ".txt" "contents")))
    (unwind-protect
        (agent-buffer-management-tests--layout
          (let ((native-visit (symbol-function 'aaron-open-visit-file)) requested-from)
            (cl-letf (((symbol-function 'aaron-open-visit-file)
                       (lambda (path &optional wildcards)
                         (setq requested-from (selected-window))
                         (funcall native-visit path wildcards))))
              (aaron-open-place file 'here))
            (should (eq requested-from side))
            (should (find-buffer-visiting file))))
      (when-let* ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest c26-ending-agent-uses-owner-confirmation-and-preserves-refused-work ()
  "Renaming a busy, queued or drafted agent cannot bypass its close guard."
  (dolist (name '("*Agent · close-test*" "Agent close-test" " *Agent close-test*"))
    (dolist (work '(busy queued draft))
      (let* ((agent (generate-new-buffer name))
             (process (make-pipe-process :name "c26-agent-close" :buffer agent :noquery t))
             prompted)
        (unwind-protect
            (save-window-excursion
              (with-current-buffer agent
                (setq major-mode 'agent-shell-mode)
                (insert (if (eq work 'draft) "unsent draft" ""))
                (setq-local comint-last-prompt (cons (point-min-marker) (point-min-marker))))
              (+agent-acp--install-workspace-tabs agent)
              (switch-to-buffer agent)
              (should (eq +buffer-end-function #'+agent-acp-close))
              (cl-letf (((symbol-function '+agent-acp-busy-p)
                         (lambda (_) (eq work 'busy)))
                        ((symbol-function '+agent-acp-pending-prompt-count)
                         (lambda (_) (if (eq work 'queued) 1 0)))
                        ((symbol-function 'yes-or-no-p)
                         (lambda (_) (setq prompted t) nil)))
                (+windows/kill-buffer-dwim))
              (should prompted)
              (should (buffer-live-p agent))
              (should (process-live-p process))
              (should (eq (current-buffer) agent))
              (when (eq work 'draft) (should (equal (buffer-string) "unsent draft"))))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p agent)
            (with-current-buffer agent (set-buffer-modified-p nil))
            (+agent-acp-kill agent)))))))

(ert-deftest c26-ending-agent-confirmation-ends-only-the-chosen-session ()
  (let* ((agent (generate-new-buffer "*Agent · chosen*"))
         (other (generate-new-buffer "*Agent · retained*"))
         (process (make-pipe-process :name "c26-chosen" :buffer agent :noquery t))
         (other-process (make-pipe-process :name "c26-retained" :buffer other :noquery t)))
    (unwind-protect
        (save-window-excursion
          (dolist (buffer (list agent other))
            (with-current-buffer buffer (setq major-mode 'agent-shell-mode))
            (+agent-acp--install-workspace-tabs buffer))
          (switch-to-buffer agent)
          (cl-letf (((symbol-function '+agent-acp-busy-p) (lambda (_) t))
                    ((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (+windows/kill-buffer-dwim))
          (should-not (buffer-live-p agent))
          (should-not (process-live-p process))
          (should (buffer-live-p other))
          (should (process-live-p other-process)))
      (dolist (proc (list process other-process))
        (when (process-live-p proc) (delete-process proc)))
      (dolist (buffer (list agent other))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'agent-buffer-management-tests)

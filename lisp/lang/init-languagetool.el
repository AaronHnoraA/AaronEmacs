;;; init-languagetool.el --- hybrid prose checking -*- lexical-binding: t; -*-
;;; Commentary:
;; Prefer the NAS LanguageTool API and fall back to the Homebrew CLI.

;;; Code:
(require 'config)
(require 'subr-x)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(config-defvar my/languagetool-language "en-US"
  "Language passed to LanguageTool for manual prose checks."
  :type 'string
  :group 'languages)

(config-defvar my/languagetool-server-url "http://10.243.90.222:8765"
  "Preferred LanguageTool HTTP server used for manual prose checks."
  :type 'string
  :group 'languages)

(config-defvar my/languagetool-server-timeout 5.0
  "Seconds to wait for the preferred LanguageTool server."
  :type 'number
  :group 'languages)

(config-defvar my/languagetool-auto-idle-delay 1.6
  "Idle seconds before an automatic visible-region check."
  :type 'number
  :group 'languages)

(config-defvar my/languagetool-auto-padding 4096
  "Characters of context around the visible Emacs region."
  :type 'integer
  :group 'languages)

(config-defvar my/languagetool-auto-max-chars 32768
  "Maximum characters sent by one automatic Emacs check."
  :type 'integer
  :group 'languages)

(config-defvar my/languagetool-auto-retry-delay 30.0
  "Seconds to pause automatic checks after the NAS is unavailable."
  :type 'number
  :group 'languages)

(defvar-local my/languagetool--request-timer nil
  "Fallback timer for the current LanguageTool HTTP request buffer.")

(defvar-local my/languagetool--active-request nil
  "Identifier of the latest LanguageTool request for this source buffer.")

(defvar-local my/languagetool--auto-timer nil
  "Idle timer for automatic LanguageTool checks in this buffer.")

(defvar-local my/languagetool--auto-suspended-until 0.0
  "Time before which automatic NAS checks remain suspended.")

(defun my/languagetool--homebrew-prefix ()
  "Return the prefix of the installed Homebrew LanguageTool formula."
  (when-let* ((binary (executable-find "languagetool"))
              (bin-dir (file-name-directory (file-truename binary))))
    (file-name-directory (directory-file-name bin-dir))))

(defun my/languagetool--configure-console ()
  "Configure `languagetool.el' to use the system Homebrew installation."
  (let* ((prefix (my/languagetool--homebrew-prefix))
         (jar (and prefix
                   (expand-file-name "libexec/languagetool-commandline.jar"
                                     prefix))))
    (unless (and jar (file-readable-p jar))
      (user-error "LanguageTool is unavailable; run `brew install languagetool'"))
    (setq languagetool-java-bin (or (executable-find "java") "java")
          languagetool-java-arguments
          '("-Dfile.encoding=UTF-8" "-Xms32m" "-Xmx512m")
          languagetool-console-command jar
          languagetool-console-arguments nil
          languagetool-correction-language my/languagetool-language
          languagetool-suggestion-level "picky")))

(defun my/languagetool--local-check (buffer begin end &optional reason)
  "Check BUFFER from BEGIN to END with the local CLI after REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when reason
        (message "NAS LanguageTool unavailable (%s); using local CLI" reason))
      (my/languagetool--configure-console)
      (languagetool-console-check begin end))))

(defun my/languagetool--utf16-position (text offset base)
  "Map UTF-16 OFFSET in TEXT to an Emacs position starting at BASE."
  (let ((index 0)
        (units 0)
        (length (length text)))
    (while (and (< index length) (< units offset))
      (setq units (+ units (if (> (aref text index) #xffff) 2 1))
            index (1+ index)))
    (+ base index)))

(defun my/languagetool--apply-response (parsed buffer begin tick text quiet)
  "Apply PARSED NAS diagnostics to BUFFER at BEGIN if TICK is current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (/= tick (buffer-chars-modified-tick))
          (message "LanguageTool result discarded because the buffer changed")
        (setq my/languagetool--auto-suspended-until 0.0)
        (languagetool-core-clear-buffer)
        (let ((matches (alist-get 'matches parsed))
              (count 0))
          (dolist (correction (append matches nil))
            (let* ((offset (alist-get 'offset correction))
                   (size (alist-get 'length correction))
                   (start (my/languagetool--utf16-position text offset begin))
                   (end (my/languagetool--utf16-position
                         text (+ offset size) begin))
                   (word (buffer-substring-no-properties start end)))
              (unless (languagetool-core-correct-p word)
                (languagetool-issue-create-overlay start end correction)
                (setq count (1+ count)))))
          (unless quiet
            (message "LanguageTool finished via NAS: %d issue%s"
                     count (if (= count 1) "" "s"))))))))

(defun my/languagetool--http-finished
    (status buffer begin end tick text request-id allow-fallback quiet)
  "Handle LanguageTool HTTP STATUS for BUFFER from BEGIN to END."
  (let ((response-buffer (current-buffer))
        (timer my/languagetool--request-timer)
        active
        fallback-reason
        parsed)
    (when (timerp timer)
      (cancel-timer timer))
    (setq my/languagetool--request-timer nil)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq my/languagetool--active-request request-id)
          (setq my/languagetool--active-request nil
                active t))))
    (when active
      (condition-case err
          (cond
           ((plist-get status :error)
            (setq fallback-reason
                  (error-message-string (plist-get status :error))))
           ((not (eq url-http-response-status 200))
            (setq fallback-reason (format "HTTP %s" url-http-response-status)))
           (t
            (goto-char url-http-end-of-headers)
            (setq parsed (json-read))))
        (error (setq fallback-reason (error-message-string err)))))
    (when (buffer-live-p response-buffer)
      (kill-buffer response-buffer))
    (when active
      (if fallback-reason
          (if allow-fallback
              (my/languagetool--local-check buffer begin end fallback-reason)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq my/languagetool--auto-suspended-until
                      (+ (float-time) my/languagetool-auto-retry-delay))))
            (unless quiet (message "NAS LanguageTool unavailable: %s"
                                   fallback-reason)))
        (my/languagetool--apply-response parsed buffer begin tick text quiet)))))

(defun my/languagetool--http-timeout
    (request-buffer buffer begin end tick request-id allow-fallback quiet)
  "Cancel REQUEST-BUFFER and fall back to the CLI for BUFFER."
  (when (buffer-live-p request-buffer)
    (with-current-buffer request-buffer
      (setq my/languagetool--request-timer nil)
      (when-let* ((process (get-buffer-process request-buffer)))
        (set-process-sentinel process #'ignore)
        (delete-process process)))
    (kill-buffer request-buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq my/languagetool--active-request request-id)
          (setq my/languagetool--active-request nil)
          (when (= tick (buffer-chars-modified-tick))
            (if allow-fallback
                (my/languagetool--local-check buffer begin end "timeout")
              (setq my/languagetool--auto-suspended-until
                    (+ (float-time) my/languagetool-auto-retry-delay))
              (unless quiet
                (message "NAS LanguageTool request timed out")))))))))

(defun my/languagetool--server-check
    (begin end &optional allow-fallback quiet)
  "Check the current buffer region from BEGIN to END through the NAS API."
  (let* ((buffer (current-buffer))
         (tick (buffer-chars-modified-tick))
         (request-id (gensym "languagetool-request-"))
         (text (buffer-substring-no-properties begin end))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (url-build-query-string
           `(("language" ,my/languagetool-language)
             ("level" "picky")
             ("text" ,text))))
         (endpoint (concat (string-remove-suffix "/" my/languagetool-server-url)
                           "/v2/check"))
         (request-buffer
          (url-retrieve endpoint #'my/languagetool--http-finished
                        (list buffer begin end tick text request-id
                              allow-fallback quiet)
                        t t)))
    (setq my/languagetool--active-request request-id)
    (if (not (buffer-live-p request-buffer))
        (progn
          (setq my/languagetool--active-request nil)
          (if allow-fallback
              (my/languagetool--local-check
               buffer begin end "request startup failed")
            (setq my/languagetool--auto-suspended-until
                  (+ (float-time) my/languagetool-auto-retry-delay))
            (unless quiet
              (message "NAS LanguageTool request could not start"))))
      (with-current-buffer request-buffer
        (setq my/languagetool--request-timer
              (run-at-time my/languagetool-server-timeout nil
                           #'my/languagetool--http-timeout
                           request-buffer buffer begin end tick request-id
                           allow-fallback quiet))))))

(defun my/languagetool--visible-scope ()
  "Return a bounded visible-region scope for the current buffer."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (let* ((visible-begin (window-start window))
           (visible-end (or (window-end window t) visible-begin))
           (begin (max (point-min)
                       (- visible-begin my/languagetool-auto-padding)))
           (end (min (point-max)
                     (+ visible-end my/languagetool-auto-padding)))
           (max-chars my/languagetool-auto-max-chars))
      (when (> (- end begin) max-chars)
        (let ((center (/ (+ visible-begin visible-end) 2)))
          (setq begin (max (point-min) (- center (/ max-chars 2)))
                end (min (point-max) (+ begin max-chars)))
          (when (< (- end begin) max-chars)
            (setq begin (max (point-min) (- end max-chars))))))
      (save-excursion
        (goto-char begin)
        (setq begin (line-beginning-position))
        (goto-char end)
        (setq end (line-end-position)))
      (cons begin end))))

(defun my/languagetool--cancel-auto-timer ()
  "Cancel this buffer's pending automatic LanguageTool check."
  (when (timerp my/languagetool--auto-timer)
    (cancel-timer my/languagetool--auto-timer))
  (setq my/languagetool--auto-timer nil))

(defun my/languagetool--auto-run (buffer)
  "Run a visible-region NAS check for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/languagetool--auto-timer nil)
      (when (and my/languagetool-auto-mode
                 (get-buffer-window buffer t))
        (require 'languagetool)
        (when-let* ((scope (my/languagetool--visible-scope)))
          (my/languagetool--server-check (car scope) (cdr scope) nil t))))))

(defun my/languagetool--schedule-auto (&rest _)
  "Schedule a NAS-only check after an input or scrolling pause."
  (my/languagetool--cancel-auto-timer)
  (when (and my/languagetool-auto-mode
             (get-buffer-window (current-buffer) t)
             (>= (float-time) my/languagetool--auto-suspended-until))
    (setq my/languagetool--auto-timer
          (run-at-time my/languagetool-auto-idle-delay nil
                       #'my/languagetool--auto-run
                       (current-buffer)))))

(define-minor-mode my/languagetool-auto-mode
  "Check visible prose through the NAS LanguageTool server while idle."
  :lighter " LT"
  (if my/languagetool-auto-mode
      (progn
        (add-hook 'after-change-functions #'my/languagetool--schedule-auto nil t)
        (add-hook 'window-scroll-functions #'my/languagetool--schedule-auto nil t)
        (my/languagetool--schedule-auto))
    (remove-hook 'after-change-functions #'my/languagetool--schedule-auto t)
    (remove-hook 'window-scroll-functions #'my/languagetool--schedule-auto t)
    (my/languagetool--cancel-auto-timer)
    (setq my/languagetool--active-request nil
          my/languagetool--auto-suspended-until 0.0)))

(defun my/languagetool-check (&optional whole-buffer)
  "Run one manual LanguageTool check with CLI fallback.

Check the active region, otherwise the visible padded scope.  With
WHOLE-BUFFER non-nil, check the entire buffer."
  (interactive "P")
  (require 'languagetool)
  (my/languagetool--cancel-auto-timer)
  (my/languagetool--configure-console)
  (pcase-let ((`(,begin ,end)
               (cond
                ((region-active-p)
                 (list (region-beginning) (region-end)))
                (whole-buffer (list (point-min) (point-max)))
                ((my/languagetool--visible-scope)
                 (let ((scope (my/languagetool--visible-scope)))
                   (list (car scope) (cdr scope))))
                (t (list (point-min) (point-max))))))
    (my/languagetool--server-check begin end t nil)))

(my/package-ensure-vc
 'languagetool "https://github.com/PillFall/languagetool.el.git")

(use-package languagetool
  :ensure nil
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language)
  :hook (text-mode . my/languagetool-auto-mode)
  :bind (("C-c i g" . my/languagetool-check)
         ("C-c i x" . languagetool-clear-suggestions)
         ("C-c i a" . languagetool-correct-at-point)
         ("C-c i A" . languagetool-correct-buffer)))

(provide 'init-languagetool)
;;; init-languagetool.el ends here

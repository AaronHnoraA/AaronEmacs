;;; init-languagetool-flymake.el --- interactive LanguageTool diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Flymake adapter for the bounded NAS/CLI transport configured by
;; `init-languagetool'.  Diagnostics stay on the standard diagnostics surface
;; and carry enough data for keyboard and context-menu corrections.  They are
;; deliberately quieter than compiler diagnostics: no end-of-line summary and
;; no primary-click action.
;;
;; The behaviour follows Noema's checker: spelling, grammar and style are
;; coloured red, amber and blue; the accepted-word list and the turned-off
;; rules are the files Noema writes (`my/languagetool-words-file',
;; `my/languagetool-disabled-rules-file'); and results are cached per
;; paragraph, so an edit re-sends only the paragraph it touched.

;;; Code:
(require 'cl-lib)
(require 'config)
(require 'flymake)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar flymake-no-changes-timeout)
(defvar languagetool-java-bin)
(defvar languagetool-java-arguments)
(defvar languagetool-console-command)
(defvar languagetool-console-arguments)
(defvar languagetool-correction-language)
(defvar languagetool-suggestion-level)

(config-defvar my/languagetool-language "en-US"
  "Language passed to LanguageTool for prose checks."
  :type 'string
  :group 'languages)

(config-defvar my/languagetool-server-url "http://10.243.90.222:8765"
  "Preferred LanguageTool HTTP server."
  :type 'string
  :group 'languages)

(config-defvar my/languagetool-server-timeout 5.0
  "Seconds to wait for the preferred LanguageTool server."
  :type 'number
  :group 'languages)

(config-defvar my/languagetool-auto-idle-delay 1.6
  "Idle seconds before LanguageTool checks changed prose."
  :type 'number
  :group 'languages)

(config-defvar my/languagetool-auto-padding 4096
  "Characters of context around the visible Emacs region."
  :type 'integer
  :group 'languages)

(config-defvar my/languagetool-auto-max-chars 32768
  "Maximum characters sent by one automatic check."
  :type 'integer
  :group 'languages)

(config-defvar my/languagetool-auto-retry-delay 30.0
  "Seconds to pause automatic checks after the NAS is unavailable."
  :type 'number
  :group 'languages)

(defvar my/languagetool-words-file
  (expand-file-name "etc/prose-accepted-words.txt" user-emacs-directory)
  "Accepted-word list shared with Noema's prose checker.")

(defvar my/languagetool-builtin-words-file
  (expand-file-name "site-lisp/noema/resources/prose-accepted-words.txt"
                    user-emacs-directory)
  "Noema's built-in technical vocabulary.")

(defvar my/languagetool-disabled-rules-file
  (expand-file-name "etc/prose-disabled-rules.txt" user-emacs-directory)
  "LanguageTool rules turned off for Noema and Emacs alike.")

(defconst my/languagetool-categories
  '((spelling "Spelling" "#e5484d" flymake-warning)
    (grammar "Grammar" "#d97706" flymake-warning)
    (punctuation "Punctuation" "#ca8a04" flymake-warning)
    (word "Word choice" "#8b5cf6" flymake-warning)
    (typography "Typography" "#0d9488" flymake-note)
    (style "Style" "#3b82f6" flymake-note))
  "LanguageTool classes: label, underline colour and Flymake category.
They match the classes and colours of Noema's prose diagnostics.")

;; One face and one Flymake diagnostic type per class.  Severity (and so the
;; mode-line count and `flymake-goto-next-error' filtering) comes from the
;; Flymake category; the colour from the class.
(pcase-dolist (`(,class ,label ,colour ,category) my/languagetool-categories)
  (let ((symbol (intern (format "my/languagetool-%s" class))))
    (custom-declare-face
     symbol `((t :underline (:style wave :color ,colour)))
     (format "Face for LanguageTool %s issues." (downcase label))
     :group 'languages)
    (put symbol 'flymake-category category)
    (put symbol 'face symbol)))

(defconst my/languagetool--style-issue-types
  '("style" "register" "locale-violation" "inconsistency"))

(defconst my/languagetool--style-categories
  '("STYLE" "REDUNDANCY" "PLAIN_ENGLISH" "REPETITIONS_STYLE" "WIKIPEDIA"
    "GENDER_NEUTRALITY" "MISC"))

(defconst my/languagetool--word-choice-categories
  '("CONFUSED_WORDS" "SEMANTICS" "COLLOCATIONS" "NONSTANDARD_PHRASES"
    "FALSE_FRIENDS" "COMPOUNDING"))

(defconst my/languagetool--paragraph-cache-limit 4000
  "Paragraph results kept before the cache is emptied.")

(defvar my/languagetool--paragraph-cache (make-hash-table :test #'equal)
  "LanguageTool matches keyed by language, level and paragraph text.
Offsets are relative to the paragraph, in UTF-16 units.")

(defvar my/languagetool--file-sets nil
  "Alist of FILE to (MTIME . TABLE) for the shared word and rule files.")

(defconst my/languagetool--ignored-limit 500
  "Ignored occurrences kept per buffer.")

(defconst my/languagetool--ignored-faces
  '(font-lock-comment-face font-lock-constant-face
    font-lock-function-name-face font-lock-keyword-face
    font-lock-string-face font-lock-type-face font-lock-variable-name-face
    fixed-pitch markdown-code-face markdown-inline-code-face
    markdown-markup-face org-block org-block-begin-line org-block-end-line
    org-code org-formula org-latex-and-related org-verbatim
    font-latex-math-face font-latex-sedate-face font-latex-string-face
    font-latex-verbatim-face)
  "Faces whose contents should not become prose diagnostics.")

(defvar-local my/languagetool--request-buffer nil)
(defvar-local my/languagetool--request-timer nil)
(defvar-local my/languagetool--local-process nil)
(defvar-local my/languagetool--active-request nil)
(defvar-local my/languagetool--scroll-timer nil)
(defvar-local my/languagetool--auto-suspended-until 0.0)
(defvar-local my/languagetool--requested-scope nil)
(defvar-local my/languagetool--allow-local-fallback nil)
(defvar-local my/languagetool--last-viewport-request nil)
(defvar-local my/languagetool--cleared-tick nil)
(defvar-local my/languagetool--ignored-matches nil)
(defvar-local my/languagetool-disabled-rules nil)
(defvar-local my/languagetool--report-fn nil)

(defun my/languagetool--homebrew-prefix ()
  "Return the prefix of the installed Homebrew LanguageTool formula."
  (when-let* ((binary (executable-find "languagetool"))
              (bin-dir (file-name-directory (file-truename binary))))
    (file-name-directory (directory-file-name bin-dir))))

(defun my/languagetool--configure-console ()
  "Configure `languagetool.el' for the Homebrew installation."
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

(defun my/languagetool--utf16-position (text offset base)
  "Map UTF-16 OFFSET in TEXT to an Emacs position starting at BASE."
  (let ((index 0)
        (units 0)
        (length (length text)))
    (while (and (< index length) (< units offset))
      (setq units (+ units (if (> (aref text index) #xffff) 2 1))
            index (1+ index)))
    (+ base index)))

(defun my/languagetool--visible-scope ()
  "Return a bounded, line-aligned visible scope for the current buffer."
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
      (and (< begin end) (cons begin end)))))

(defun my/languagetool--face-ignored-p (position)
  "Return non-nil when POSITION belongs to markup or literal text."
  (seq-some (lambda (face) (memq face my/languagetool--ignored-faces))
            (ensure-list (get-text-property position 'face))))

(defun my/languagetool--local-word-p (word)
  "Return non-nil when WORD occurs in a file-local LocalWords line."
  (and (not (string-empty-p word))
       (save-excursion
         (goto-char (point-min))
         (re-search-forward
          (concat "^[[:space:];#%/]*LocalWords:[[:space:]].*\\_<"
                  (regexp-quote word) "\\_>")
          nil t))))

(defun my/languagetool--replacement-values (correction)
  "Return replacement strings from LanguageTool CORRECTION."
  (mapcar (lambda (replacement) (alist-get 'value replacement))
          (append (alist-get 'replacements correction) nil)))

(defun my/languagetool--match-fingerprint (correction begin end original)
  "Return a fingerprint for CORRECTION at BEGIN END with ORIGINAL text.
It names the rule, the text and up to 24 characters on each side within
its line, so an ignored occurrence stays ignored while other text changes."
  (save-excursion
    (list (alist-get 'id (alist-get 'rule correction))
          (buffer-substring-no-properties
           (max (progn (goto-char begin) (line-beginning-position)) (- begin 24))
           begin)
          original
          (buffer-substring-no-properties
           end (min (progn (goto-char end) (line-end-position)) (+ end 24))))))

(defun my/languagetool--category (correction)
  "Return the class of LanguageTool CORRECTION, as Noema classifies it."
  (let* ((rule (alist-get 'rule correction))
         (issue (downcase (or (alist-get 'issueType rule) "")))
         (category (upcase (or (alist-get 'id (alist-get 'category rule)) ""))))
    (cond ((or (equal issue "misspelling") (equal category "TYPOS")) 'spelling)
          ((equal category "PUNCTUATION") 'punctuation)
          ((or (member category '("TYPOGRAPHY" "CASING"))
               (member issue '("whitespace" "typographical")))
           'typography)
          ((member category my/languagetool--word-choice-categories) 'word)
          ((or (member issue my/languagetool--style-issue-types)
               (member category my/languagetool--style-categories))
           'style)
          (t 'grammar))))

(defun my/languagetool--diagnostic-type (correction)
  "Return the Flymake type for LanguageTool CORRECTION."
  (intern (format "my/languagetool-%s" (my/languagetool--category correction))))

(defun my/languagetool--category-label (correction)
  "Return the human label for CORRECTION's LanguageTool class."
  (cadr (assq (my/languagetool--category correction) my/languagetool-categories)))

(defun my/languagetool--hint (correction message suggestions)
  "Return the one-line hint for CORRECTION with MESSAGE and SUGGESTIONS."
  (concat (my/languagetool--category-label correction) ": " message
          (when suggestions
            (concat " → "
                    (string-join
                     (mapcar (lambda (value)
                               (if (string-empty-p value) "(remove)" value))
                             (seq-take suggestions 3))
                     ", ")))))

(defun my/languagetool--file-set (file normalize)
  "Return a hash table of FILE's lines passed through NORMALIZE.
The table is reread only when FILE's modification time changes."
  (let* ((attributes (file-attributes file))
         (mtime (and attributes (file-attribute-modification-time attributes)))
         (cached (cdr (assoc file my/languagetool--file-sets))))
    (if (and cached (equal (car cached) mtime))
        (cdr cached)
      (let ((table (make-hash-table :test #'equal)))
        (when attributes
          (with-temp-buffer
            (insert-file-contents file)
            (dolist (line (split-string (buffer-string) "\n" t))
              (when-let* ((value (funcall normalize line)))
                (puthash value t table)))))
        (setf (alist-get file my/languagetool--file-sets nil nil #'equal)
              (cons mtime table))
        table))))

(defun my/languagetool--normalize-word (value)
  "Return VALUE as an accepted-word key, or nil.  Matches Noema's rule."
  (let ((word (string-trim value)))
    (and (string-match-p "\\`[A-Za-z][A-Za-z'’-]\\{1,63\\}\\'" word)
         (downcase word))))

(defun my/languagetool--normalize-rule (value)
  "Return the rule id on line VALUE, or nil."
  (let ((rule (string-trim (replace-regexp-in-string "#.*" "" value))))
    (and (string-match-p "\\`[A-Za-z0-9_.:-]\\{1,128\\}\\'" rule) rule)))

(defun my/languagetool--accepted-word-p (word)
  "Return non-nil when WORD is in Noema's built-in or shared word list."
  (when-let* ((key (my/languagetool--normalize-word word)))
    (or (gethash key (my/languagetool--file-set
                      my/languagetool-words-file
                      #'my/languagetool--normalize-word))
        (gethash key (my/languagetool--file-set
                      my/languagetool-builtin-words-file
                      #'my/languagetool--normalize-word)))))

(defun my/languagetool--rule-disabled-p (rule-id)
  "Return non-nil when RULE-ID is turned off here or in the shared file."
  (and rule-id
       (or (member rule-id my/languagetool-disabled-rules)
           (gethash rule-id (my/languagetool--file-set
                             my/languagetool-disabled-rules-file
                             #'my/languagetool--normalize-rule)))))

(defun my/languagetool--update-list-file (file value add normalize)
  "ADD (or remove) VALUE in the sorted line list FILE, keyed by NORMALIZE."
  (let* ((key (funcall normalize value))
         (lines (when (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (split-string (buffer-string) "\n" t "[ \t\r]+"))))
         (kept (seq-remove (lambda (line) (equal (funcall normalize line) key))
                           lines)))
    (when add (push (string-trim value) kept))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (dolist (line (sort kept (lambda (a b) (string< (downcase a) (downcase b)))))
        (insert line "\n")))))

(defvar my/languagetool-diagnostic-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'my/languagetool-menu-mouse)
    map)
  "Context-menu map installed on LanguageTool Flymake diagnostics.")

;; Also remove the old binding when this file is evaluated in a running Emacs.
(define-key my/languagetool-diagnostic-map [mouse-1] nil)

(defun my/languagetool--flymake-diagnostics (parsed buffer begin text)
  "Convert PARSED matches for BUFFER at BEGIN using source TEXT."
  (with-current-buffer buffer
    (let (diagnostics)
      (dolist (correction (append (alist-get 'matches parsed) nil))
        (let* ((offset (or (alist-get 'offset correction) 0))
               (size (or (alist-get 'length correction) 0))
               (start (my/languagetool--utf16-position text offset begin))
               (end (my/languagetool--utf16-position text (+ offset size) begin))
               (start (max (point-min) (min start (point-max))))
               (end (max start (min end (point-max))))
               (original (buffer-substring-no-properties start end))
               (rule (alist-get 'rule correction))
               (rule-id (alist-get 'id rule))
               (fingerprint (my/languagetool--match-fingerprint
                             correction start end original)))
          (unless (or (= start end)
                      (my/languagetool--face-ignored-p start)
                      (my/languagetool--rule-disabled-p rule-id)
                      (my/languagetool--accepted-word-p original)
                      (my/languagetool--local-word-p original)
                      (member fingerprint my/languagetool--ignored-matches))
            (let* ((message (or (alist-get 'message correction)
                                "LanguageTool issue"))
                   (suggestions (my/languagetool--replacement-values correction))
                   (hint (my/languagetool--hint correction message suggestions))
                   (data (list :source 'languagetool
                               :correction correction
                               :category (my/languagetool--category correction)
                               :suggestions suggestions
                               :rule-id rule-id
                               :original original)))
              (push
               (flymake-make-diagnostic
                buffer start end (my/languagetool--diagnostic-type correction)
                (list "LanguageTool" rule-id hint) data
                `((keymap . ,my/languagetool-diagnostic-map)
                  (help-echo . ,hint)))
               diagnostics)))))
      (nreverse diagnostics))))

(defun my/languagetool--report (report-fn diagnostics &rest args)
  "Call REPORT-FN with DIAGNOSTICS and Flymake ARGS, without EOL text.

`flymake-show-diagnostics-at-end-of-line' is dynamically bound only while
Flymake creates LanguageTool overlays.  Other Flymake backends in the same
buffer therefore retain their normal end-of-line summaries."
  (let ((flymake-show-diagnostics-at-end-of-line nil))
    (apply report-fn diagnostics args)))

(defun my/languagetool--flymake-cancel-request ()
  "Cancel and release the current buffer's LanguageTool request."
  (when (timerp my/languagetool--request-timer)
    (cancel-timer my/languagetool--request-timer))
  (setq my/languagetool--request-timer nil)
  (when (buffer-live-p my/languagetool--request-buffer)
    (when-let* ((process (get-buffer-process my/languagetool--request-buffer)))
      (set-process-sentinel process #'ignore)
      (delete-process process))
    (kill-buffer my/languagetool--request-buffer))
  (setq my/languagetool--request-buffer nil)
  (when (process-live-p my/languagetool--local-process)
    (set-process-sentinel my/languagetool--local-process #'ignore)
    (let ((output (process-buffer my/languagetool--local-process)))
      (delete-process my/languagetool--local-process)
      (when (buffer-live-p output) (kill-buffer output))))
  (setq my/languagetool--local-process nil
        my/languagetool--active-request nil))

(defun my/languagetool--request-current-p (request-id tick)
  "Return non-nil when REQUEST-ID and TICK still describe this buffer."
  (and (eq request-id my/languagetool--active-request)
       (= tick (buffer-chars-modified-tick))))

(defun my/languagetool--flymake-publish
    (parsed buffer begin end tick plan request-id report-fn quiet)
  "Publish PARSED LanguageTool output for PLAN through REPORT-FN.
PARSED is nil when every paragraph of PLAN was already cached."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (my/languagetool--request-current-p request-id tick)
        (setq my/languagetool--active-request nil
              my/languagetool--auto-suspended-until 0.0)
        (when parsed (my/languagetool--remember-plan plan parsed))
        (let ((diagnostics (my/languagetool--plan-diagnostics plan buffer)))
          (my/languagetool--report
           report-fn diagnostics :region (cons begin end))
          (unless quiet
            (message "LanguageTool: %s"
                     (my/languagetool--summary diagnostics))))))))

(defun my/languagetool--summary (diagnostics)
  "Return a count of DIAGNOSTICS by LanguageTool class."
  (if (null diagnostics)
      "no issues"
    (let ((counts (mapcar
                   (lambda (entry)
                     (cons (cadr entry)
                           (seq-count
                            (lambda (diagnostic)
                              (eq (plist-get (flymake-diagnostic-data diagnostic)
                                             :category)
                                  (car entry)))
                            diagnostics)))
                   my/languagetool-categories)))
      (format "%d issue%s: %s" (length diagnostics)
              (if (= (length diagnostics) 1) "" "s")
              (mapconcat (lambda (entry)
                           (format "%d %s" (cdr entry) (downcase (car entry))))
                         (seq-filter (lambda (entry) (> (cdr entry) 0)) counts)
                         ", ")))))

(defun my/languagetool--local-command ()
  "Return argv for a JSON-producing local LanguageTool invocation."
  (require 'languagetool)
  (my/languagetool--configure-console)
  (append
   (list languagetool-java-bin)
   languagetool-java-arguments
   (list "-jar" languagetool-console-command)
   languagetool-console-arguments
   (list "--encoding" "utf8" "--json"
         "--language" languagetool-correction-language
         "--level" (upcase languagetool-suggestion-level))))

(defun my/languagetool--parse-cli-buffer (buffer)
  "Parse the final JSON object from LanguageTool CLI BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (backward-sexp)
    (json-read)))

(defun my/languagetool--local-finished
    (process _event buffer begin end tick plan request-id report-fn quiet)
  "Handle completion of local LanguageTool PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let ((output (process-buffer process)) parsed failure)
      (if (= (process-exit-status process) 0)
          (condition-case err
              (setq parsed (my/languagetool--parse-cli-buffer output))
            (error (setq failure (error-message-string err))))
        (setq failure (format "local CLI exited %d"
                              (process-exit-status process))))
      (when (buffer-live-p output) (kill-buffer output))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq process my/languagetool--local-process)
            (setq my/languagetool--local-process nil)
            (if failure
                (when (my/languagetool--request-current-p request-id tick)
                  (setq my/languagetool--active-request nil)
                  (my/languagetool--report
                   report-fn nil :region (cons begin end)
                   :explanation failure)
                  (message "LanguageTool failed: %s" failure))
              (my/languagetool--flymake-publish
               parsed buffer begin end tick plan request-id report-fn quiet))))))))

(defun my/languagetool--start-local
    (buffer begin end tick plan request-id report-fn quiet reason)
  "Start a local fallback check for BUFFER after REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (condition-case err
          (let* ((output (generate-new-buffer " *LanguageTool CLI*"))
                 (process
                  (make-process
                   :name "languagetool-cli" :buffer output
                   :command (my/languagetool--local-command)
                   :connection-type 'pipe :coding 'utf-8-unix :noquery t
                   :sentinel
                   (lambda (proc event)
                     (my/languagetool--local-finished
                      proc event buffer begin end tick plan request-id
                      report-fn quiet)))))
            (setq my/languagetool--local-process process)
            (process-send-string process (plist-get plan :text))
            (process-send-eof process)
            (unless quiet
              (message "NAS LanguageTool unavailable (%s); using local CLI"
                       reason)))
        (error
         (when (my/languagetool--request-current-p request-id tick)
           (setq my/languagetool--active-request nil)
           (my/languagetool--report
            report-fn nil :region (cons begin end)
            :explanation (error-message-string err))
           (message "LanguageTool fallback failed: %s"
                    (error-message-string err))))))))

(defun my/languagetool--flymake-request-failed
    (buffer begin end tick plan request-id report-fn allow-fallback quiet reason)
  "Handle a failed request for BUFFER with REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (my/languagetool--request-current-p request-id tick)
        (if allow-fallback
            ;; URL can deliver both a low-level `deleted' event and the final
            ;; connection error.  They describe one request, so start at most
            ;; one JVM fallback.
            (unless (process-live-p my/languagetool--local-process)
              (my/languagetool--start-local
               buffer begin end tick plan request-id report-fn quiet reason))
          (setq my/languagetool--active-request nil
                my/languagetool--auto-suspended-until
                (+ (float-time) my/languagetool-auto-retry-delay))
          (my/languagetool--report
           report-fn nil :region (cons begin end)
           :explanation (format "NAS LanguageTool unavailable: %s"
                                reason)))))))

(defun my/languagetool--flymake-http-finished
    (status buffer begin end tick plan request-id report-fn allow-fallback quiet)
  "Handle LanguageTool HTTP STATUS for BUFFER."
  (let ((request-error (plist-get status :error)))
    ;; url.el can emit an intermediate "deleted" callback immediately before
    ;; the useful connection error.  Leave the response and timeout alive so
    ;; the latter owns fallback/reporting.
    (unless (and request-error
                 (seq-some
                  (lambda (item)
                    (and (stringp item)
                         (string= (string-trim item) "deleted")))
                  (ensure-list request-error)))
      (let ((response (current-buffer)) parsed failure)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; An obsolete callback must not clear a newer request's timeout.
            (when (eq response my/languagetool--request-buffer)
              (when (timerp my/languagetool--request-timer)
                (cancel-timer my/languagetool--request-timer))
              (setq my/languagetool--request-timer nil
                    my/languagetool--request-buffer nil))))
        (condition-case err
            (cond
             (request-error
              (setq failure (error-message-string request-error)))
             ((not (eq url-http-response-status 200))
              (setq failure (format "HTTP %s" url-http-response-status)))
             (t (goto-char url-http-end-of-headers) (setq parsed (json-read))))
          (error (setq failure (error-message-string err))))
        (when (buffer-live-p response) (kill-buffer response))
        (if failure
            (my/languagetool--flymake-request-failed
             buffer begin end tick plan request-id report-fn
             allow-fallback quiet failure)
          (my/languagetool--flymake-publish
           parsed buffer begin end tick plan request-id report-fn quiet))))))

(defun my/languagetool--flymake-timeout
    (request-buffer buffer begin end tick plan request-id report-fn
                    allow-fallback quiet)
  "Cancel REQUEST-BUFFER and handle a timeout for BUFFER."
  (when (buffer-live-p request-buffer)
    (when-let* ((process (get-buffer-process request-buffer)))
      (set-process-sentinel process #'ignore)
      (delete-process process))
    (kill-buffer request-buffer))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; The timer can race with a replacement request on a busy Emacs.
      (when (eq request-buffer my/languagetool--request-buffer)
        (setq my/languagetool--request-buffer nil
              my/languagetool--request-timer nil))))
  (my/languagetool--flymake-request-failed
   buffer begin end tick plan request-id report-fn
   allow-fallback quiet "timeout"))

(defun my/languagetool--utf16-length (text)
  "Return the length of TEXT in UTF-16 code units."
  (let ((units (length text)))
    (dotimes (index (length text))
      (when (> (aref text index) #xffff) (setq units (1+ units))))
    units))

(defun my/languagetool--paragraphs (begin end)
  "Return (POSITION . TEXT) for each paragraph with words in BEGIN..END."
  (let ((start begin) paragraphs)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward "\n[ \t]*\n" end t)
        (push (cons start (buffer-substring-no-properties
                           start (match-beginning 0)))
              paragraphs)
        (setq start (match-end 0)))
      (when (< start end)
        (push (cons start (buffer-substring-no-properties start end))
              paragraphs)))
    (seq-filter (lambda (paragraph) (string-match-p "[[:alpha:]]" (cdr paragraph)))
                (nreverse paragraphs))))

(defun my/languagetool--cache-key (text)
  "Return the paragraph-cache key for TEXT under the current settings."
  (concat my/languagetool-language "\0picky\0" text))

(defun my/languagetool--plan (begin end)
  "Plan a check of BEGIN..END: which paragraphs to send, joined as :text.
:missing holds (PARAGRAPH START END) in UTF-16 units of :text."
  (let ((paragraphs (my/languagetool--paragraphs begin end))
        (units 0) parts missing)
    (dolist (paragraph paragraphs)
      (unless (gethash (my/languagetool--cache-key (cdr paragraph))
                       my/languagetool--paragraph-cache)
        (when parts
          (push "\n\n" parts)
          (setq units (+ units 2)))
        (let ((length (my/languagetool--utf16-length (cdr paragraph))))
          (push (list paragraph units (+ units length)) missing)
          (setq units (+ units length)))
        (push (cdr paragraph) parts)))
    (list :paragraphs paragraphs
          :missing (nreverse missing)
          :text (apply #'concat (nreverse parts)))))

(defun my/languagetool--remember-plan (plan parsed)
  "Cache PARSED matches per paragraph of PLAN's :missing list."
  (let ((buckets (make-vector (length (plist-get plan :missing)) nil)))
    (dolist (match (append (alist-get 'matches parsed) nil))
      (let* ((offset (or (alist-get 'offset match) 0))
             (end (+ offset (or (alist-get 'length match) 0))))
        (cl-loop for (_paragraph start stop) in (plist-get plan :missing)
                 for index from 0
                 when (and (>= offset start) (<= end stop))
                 do (push (cons (cons 'offset (- offset start))
                                (assq-delete-all 'offset (copy-alist match)))
                          (aref buckets index))
                 and return nil)))
    (when (> (hash-table-count my/languagetool--paragraph-cache)
             my/languagetool--paragraph-cache-limit)
      (clrhash my/languagetool--paragraph-cache))
    (cl-loop for (paragraph) in (plist-get plan :missing)
             for index from 0
             do (puthash (my/languagetool--cache-key (cdr paragraph))
                         (vconcat (nreverse (aref buckets index)))
                         my/languagetool--paragraph-cache))))

(defun my/languagetool--plan-diagnostics (plan buffer)
  "Return Flymake diagnostics for every cached paragraph of PLAN in BUFFER."
  (cl-loop for (position . text) in (plist-get plan :paragraphs)
           for matches = (gethash (my/languagetool--cache-key text)
                                  my/languagetool--paragraph-cache)
           when (and matches (> (length matches) 0))
           append (my/languagetool--flymake-diagnostics
                   `((matches . ,matches)) buffer position text)))

(defun my/languagetool--flymake-server-check
    (begin end report-fn &optional allow-fallback quiet)
  "Check current buffer from BEGIN to END and report via REPORT-FN."
  (my/languagetool--flymake-cancel-request)
  (let* ((buffer (current-buffer))
         (tick (buffer-chars-modified-tick))
         (request-id (gensym "languagetool-request-"))
         (plan (my/languagetool--plan begin end))
         (text (plist-get plan :text))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         ;; Turned-off rules are filtered locally, which keeps cached
         ;; paragraphs valid when a rule is turned back on.
         (params `(("language" ,my/languagetool-language)
                   ("level" "picky") ("text" ,text)))
         (url-request-data (url-build-query-string params nil t))
         (endpoint (concat (string-remove-suffix "/" my/languagetool-server-url)
                           "/v2/check")))
    (setq my/languagetool--active-request request-id
          my/languagetool--report-fn report-fn)
    (if (null (plist-get plan :missing))
        (my/languagetool--flymake-publish
         nil buffer begin end tick plan request-id report-fn quiet)
      (condition-case err
          (let ((request
                 (url-retrieve
                  endpoint #'my/languagetool--flymake-http-finished
                  (list buffer begin end tick plan request-id report-fn
                        allow-fallback quiet)
                  t t)))
            (if (not (buffer-live-p request))
                (my/languagetool--flymake-request-failed
                 buffer begin end tick plan request-id report-fn allow-fallback
                 quiet "request startup failed")
              (setq my/languagetool--request-buffer request
                    my/languagetool--request-timer
                    (run-at-time
                     my/languagetool-server-timeout nil
                     #'my/languagetool--flymake-timeout
                     request buffer begin end tick plan request-id report-fn
                     allow-fallback quiet))))
        (error
         (my/languagetool--flymake-request-failed
          buffer begin end tick plan request-id report-fn allow-fallback quiet
          (error-message-string err)))))))

(defun my/languagetool-flymake-backend (report-fn &rest _args)
  "Flymake backend that checks a bounded prose scope with REPORT-FN."
  (let* ((manual-scope my/languagetool--requested-scope)
         (allow-fallback my/languagetool--allow-local-fallback)
         (scope (or manual-scope (my/languagetool--visible-scope))))
    (setq my/languagetool--requested-scope nil
          my/languagetool--allow-local-fallback nil
          my/languagetool--report-fn report-fn)
    (cond
     ((and my/languagetool--cleared-tick
           (= my/languagetool--cleared-tick (buffer-chars-modified-tick)))
      (my/languagetool--report
       report-fn nil :region (cons (point-min) (point-max))))
     ((null scope) (my/languagetool--report report-fn nil))
     ((and (not manual-scope)
           (< (float-time) my/languagetool--auto-suspended-until))
      (my/languagetool--report
       report-fn nil :region scope
       :explanation "LanguageTool retry is temporarily paused"))
     (t
      (my/languagetool--flymake-server-check
       (car scope) (cdr scope) report-fn allow-fallback
       (not manual-scope))))))

(defun my/languagetool--cancel-scroll-timer ()
  "Cancel the current buffer's viewport timer."
  (when (timerp my/languagetool--scroll-timer)
    (cancel-timer my/languagetool--scroll-timer))
  (setq my/languagetool--scroll-timer nil))

(defun my/languagetool--run-viewport-check (buffer signature)
  "Start a Flymake check in BUFFER for viewport SIGNATURE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/languagetool--scroll-timer nil)
      (when (and my/languagetool-auto-mode
                 (equal signature my/languagetool--last-viewport-request)
                 (get-buffer-window buffer t)
                 (>= (float-time) my/languagetool--auto-suspended-until))
        (flymake-start)))))

(defun my/languagetool--schedule-viewport-check (&rest _)
  "Coalesce a LanguageTool check after the visible region changes."
  (when (and my/languagetool-auto-mode
             (get-buffer-window (current-buffer) t))
    (when-let* ((scope (my/languagetool--visible-scope)))
      (let ((signature (list (buffer-chars-modified-tick)
                             (car scope) (cdr scope))))
        (unless (equal signature my/languagetool--last-viewport-request)
          (setq my/languagetool--last-viewport-request signature)
          (my/languagetool--cancel-scroll-timer)
          (setq my/languagetool--scroll-timer
                (run-with-idle-timer
                 my/languagetool-auto-idle-delay nil
                 #'my/languagetool--run-viewport-check
                 (current-buffer) signature)))))))

(defun my/languagetool--flymake-after-change (&rest _)
  "Invalidate one-shot LanguageTool state after a buffer edit.
Ignored occurrences are keyed by their surrounding text, so they stay."
  (setq my/languagetool--cleared-tick nil
        my/languagetool--last-viewport-request nil))

(defun my/languagetool--flymake-cleanup ()
  "Release all LanguageTool resources owned by the current buffer."
  (my/languagetool--cancel-scroll-timer)
  (my/languagetool--flymake-cancel-request)
  (remove-hook 'window-scroll-functions
               #'my/languagetool--schedule-viewport-check t)
  (remove-hook 'after-change-functions
               #'my/languagetool--flymake-after-change t)
  (remove-hook 'context-menu-functions
               #'my/languagetool--context-menu-function t)
  (remove-hook 'kill-buffer-hook #'my/languagetool--flymake-cleanup t))

(define-minor-mode my/languagetool-auto-mode
  "Provide bounded LanguageTool diagnostics through Flymake."
  :lighter " LT"
  (if my/languagetool-auto-mode
      (progn
        (add-hook 'flymake-diagnostic-functions
                  #'my/languagetool-flymake-backend nil t)
        (add-hook 'window-scroll-functions
                  #'my/languagetool--schedule-viewport-check nil t)
        (add-hook 'after-change-functions
                  #'my/languagetool--flymake-after-change nil t)
        (add-hook 'context-menu-functions
                  #'my/languagetool--context-menu-function nil t)
        (add-hook 'kill-buffer-hook #'my/languagetool--flymake-cleanup nil t)
        (setq-local flymake-no-changes-timeout
                    my/languagetool-auto-idle-delay)
        (flymake-mode 1)
        (my/languagetool--schedule-viewport-check))
    (remove-hook 'flymake-diagnostic-functions
                 #'my/languagetool-flymake-backend t)
    (my/languagetool--flymake-cleanup)
    (when (bound-and-true-p flymake-mode) (flymake-start))))

(defun my/languagetool--diagnostic-p (diagnostic)
  "Return non-nil when DIAGNOSTIC belongs to LanguageTool."
  (eq (plist-get (flymake-diagnostic-data diagnostic) :source)
      'languagetool))

(defun my/languagetool--diagnostic-at-point ()
  "Return one LanguageTool diagnostic at point."
  (seq-find #'my/languagetool--diagnostic-p (flymake-diagnostics (point))))

(defun my/languagetool--all-diagnostics ()
  "Return all LanguageTool diagnostics in the accessible buffer."
  (seq-filter #'my/languagetool--diagnostic-p (flymake-diagnostics)))

(defun my/languagetool--replace (diagnostic replacement)
  "Apply REPLACEMENT for LanguageTool DIAGNOSTIC."
  (let* ((begin (flymake-diagnostic-beg diagnostic))
         (end (flymake-diagnostic-end diagnostic))
         (expected (plist-get (flymake-diagnostic-data diagnostic) :original)))
    (unless (equal expected (buffer-substring-no-properties begin end))
      (user-error "This LanguageTool diagnostic is stale; check again"))
    (atomic-change-group
      (delete-region begin end)
      (goto-char begin)
      (insert replacement))))

(defun my/languagetool--ignore-once (diagnostic)
  "Ignore DIAGNOSTIC while its text and surroundings are unchanged."
  (let* ((data (flymake-diagnostic-data diagnostic))
         (correction (plist-get data :correction)))
    (when (>= (length my/languagetool--ignored-matches)
              my/languagetool--ignored-limit)
      (setq my/languagetool--ignored-matches nil))
    (cl-pushnew
     (my/languagetool--match-fingerprint
      correction (flymake-diagnostic-beg diagnostic)
      (flymake-diagnostic-end diagnostic) (plist-get data :original))
     my/languagetool--ignored-matches :test #'equal)
    (flymake-start)))

(defun my/languagetool--ignore-rule (diagnostic)
  "Ignore DIAGNOSTIC's rule in this buffer."
  (let ((rule-id (plist-get (flymake-diagnostic-data diagnostic) :rule-id)))
    (unless rule-id (user-error "This diagnostic has no LanguageTool rule ID"))
    (cl-pushnew rule-id my/languagetool-disabled-rules :test #'equal)
    (flymake-start)
    (message "LanguageTool rule %s ignored in this buffer" rule-id)))

(defun my/languagetool--turn-off-rule (diagnostic)
  "Turn DIAGNOSTIC's rule off in Emacs and Noema alike."
  (let ((rule-id (plist-get (flymake-diagnostic-data diagnostic) :rule-id)))
    (unless rule-id (user-error "This diagnostic has no LanguageTool rule ID"))
    (my/languagetool--update-list-file
     my/languagetool-disabled-rules-file rule-id t
     #'my/languagetool--normalize-rule)
    (flymake-start)
    (message "LanguageTool rule %s turned off (%s)" rule-id
             (abbreviate-file-name my/languagetool-disabled-rules-file))))

(defun my/languagetool-enable-rule (rule-id)
  "Turn LanguageTool RULE-ID back on in Emacs and Noema."
  (interactive
   (list (completing-read
          "Turn rule back on: "
          (hash-table-keys (my/languagetool--file-set
                            my/languagetool-disabled-rules-file
                            #'my/languagetool--normalize-rule))
          nil t)))
  (my/languagetool--update-list-file
   my/languagetool-disabled-rules-file rule-id nil
   #'my/languagetool--normalize-rule)
  (setq my/languagetool-disabled-rules
        (delete rule-id my/languagetool-disabled-rules))
  (when my/languagetool-auto-mode (flymake-start))
  (message "LanguageTool rule %s turned back on" rule-id))

(defun my/languagetool--add-word (diagnostic)
  "Add DIAGNOSTIC's text to the dictionary Emacs and Noema share."
  (let ((word (plist-get (flymake-diagnostic-data diagnostic) :original)))
    (unless (and word (my/languagetool--normalize-word word))
      (user-error "The diagnostic text is not a single dictionary word"))
    (my/languagetool--update-list-file
     my/languagetool-words-file word t #'my/languagetool--normalize-word)
    (flymake-start)
    (message "Added %s to the prose dictionary" (string-trim word))))

(defun my/languagetool--correction-candidates (diagnostic)
  "Return completion candidates for DIAGNOSTIC."
  (let ((data (flymake-diagnostic-data diagnostic)))
    (append
     (mapcar (lambda (replacement)
               (cons (format "Replace with: %s"
                             (if (string-empty-p replacement) "(remove)" replacement))
                     (cons 'replace replacement)))
             (plist-get data :suggestions))
     '(("Ignore this occurrence" . (skip)))
     (when (and (eq (plist-get data :category) 'spelling)
                (my/languagetool--normalize-word (or (plist-get data :original) "")))
       `((,(format "Add “%s” to dictionary" (string-trim (plist-get data :original)))
          . (add-word))))
     (when (plist-get data :rule-id)
       `((,(format "Turn off rule %s" (plist-get data :rule-id)) . (turn-off-rule))
         ("Ignore this rule in buffer" . (ignore-rule)))))))

(defun my/languagetool--context-menu-function (menu click)
  "Add LanguageTool actions to MENU for CLICK in a prose buffer."
  (when (mouse-event-p click)
    (mouse-set-point click))
  (let* ((diagnostic (and (bound-and-true-p flymake-mode)
                          (my/languagetool--diagnostic-at-point)))
         (submenu (make-sparse-keymap "LanguageTool")))
    (define-key-after submenu [check]
      '(menu-item "Check prose" my/languagetool-check))
    (define-key-after submenu [review]
      '(menu-item "Review issues" my/languagetool-correct))
    (when diagnostic
      (define-key-after submenu [separator] menu-bar-separator)
      (cl-loop for candidate in (my/languagetool--correction-candidates diagnostic)
               for index from 0
               do (let ((action (cdr candidate)))
                    (define-key-after submenu (vector (intern (format "action-%d" index)))
                      `(menu-item ,(car candidate)
                                  ,(lambda () (interactive)
                                     (my/languagetool--perform-action
                                      diagnostic action)))))))
    (define-key-after menu [languagetool] (cons "LanguageTool" submenu)))
  menu)

(defun my/languagetool--perform-action (diagnostic action)
  "Perform ACTION for LanguageTool DIAGNOSTIC."
  (pcase action
    (`(replace . ,replacement) (my/languagetool--replace diagnostic replacement))
    (`(skip) (my/languagetool--ignore-once diagnostic))
    (`(ignore-rule) (my/languagetool--ignore-rule diagnostic))
    (`(add-word) (my/languagetool--add-word diagnostic))
    (`(turn-off-rule) (my/languagetool--turn-off-rule diagnostic))))

(defun my/languagetool-correct-at-point (&optional diagnostic)
  "Choose and apply a correction for DIAGNOSTIC or the issue at point."
  (interactive)
  (let* ((diagnostic (or diagnostic (my/languagetool--diagnostic-at-point)))
         (_ (unless diagnostic (user-error "No LanguageTool issue at point")))
         (candidates (my/languagetool--correction-candidates diagnostic))
         (choice (completing-read
                  (format "%s: " (flymake-diagnostic-text diagnostic))
                  candidates nil t)))
    (my/languagetool--perform-action diagnostic (cdr (assoc choice candidates)))))

(defun my/languagetool-correct ()
  "Select a visible LanguageTool issue and correct it."
  (interactive)
  (let* ((diagnostics (my/languagetool--all-diagnostics))
         (_ (unless diagnostics (user-error "No LanguageTool issues")))
         (candidates
          (cl-loop for diagnostic in diagnostics for index from 1
                   collect
                   (cons (format "%d:%d  %s"
                                 (line-number-at-pos
                                  (flymake-diagnostic-beg diagnostic) t)
                                 index (flymake-diagnostic-text diagnostic))
                         diagnostic)))
         (diagnostic (cdr (assoc (completing-read
                                  "LanguageTool issue: " candidates nil t)
                                 candidates))))
    (goto-char (flymake-diagnostic-beg diagnostic))
    (my/languagetool-correct-at-point diagnostic)))

(defun my/languagetool-menu-mouse (event)
  "Show a correction menu for the LanguageTool issue at mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (let ((diagnostic (my/languagetool--diagnostic-at-point)))
    (unless diagnostic (user-error "No LanguageTool issue at click position"))
    (popup-menu
     (easy-menu-create-menu
      "LanguageTool"
      (mapcar
       (lambda (candidate)
         (let ((action (cdr candidate)))
           (vector (car candidate)
                   (lambda () (interactive)
                     (my/languagetool--perform-action diagnostic action))
                   t)))
       (my/languagetool--correction-candidates diagnostic)))
     event)))

(defun my/languagetool-check (&optional whole-buffer)
  "Run a manual check of region, visible scope, or WHOLE-BUFFER."
  (interactive "P")
  (unless my/languagetool-auto-mode (my/languagetool-auto-mode 1))
  (setq my/languagetool--requested-scope
        (cond ((region-active-p) (cons (region-beginning) (region-end)))
              (whole-buffer (cons (point-min) (point-max)))
              ((my/languagetool--visible-scope))
              (t (cons (point-min) (point-max))))
        my/languagetool--allow-local-fallback t
        my/languagetool--cleared-tick nil
        my/languagetool--auto-suspended-until 0.0)
  (flymake-start))

(defun my/languagetool-clear ()
  "Clear LanguageTool diagnostics until the buffer changes."
  (interactive)
  (my/languagetool--flymake-cancel-request)
  (setq my/languagetool--cleared-tick (buffer-chars-modified-tick))
  (flymake-start))

(provide 'init-languagetool-flymake)
;;; init-languagetool-flymake.el ends here

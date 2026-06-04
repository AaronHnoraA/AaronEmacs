;;; ai-workbench-tools.el --- Reference tools for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reference-oriented prompt helpers for ai-workbench.

;;; Code:

(require 'project)
(require 'thingatpt)
(require 'seq)
(require 'subr-x)
(require 'ai-workbench-session)
(require 'ai-workbench-profile)

(declare-function which-function "which-func" ())
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function ai-workbench-draft-string "ai-workbench" (backend prompt &optional project-root))
(declare-function ai-workbench-send-string "ai-workbench" (backend prompt &optional project-root))
(declare-function ai-workbench-open "ai-workbench" ())

(autoload 'ai-workbench-draft-string "ai-workbench")
(autoload 'ai-workbench-send-string "ai-workbench")
(autoload 'ai-workbench-open "ai-workbench" nil t)

(defvar flycheck-current-errors)

(defcustom ai-workbench-tools-project-files-limit 40
  "Maximum number of project files included in project file references."
  :type 'integer
  :group 'ai-workbench)

(defcustom ai-workbench-tools-test-failure-lines-limit 20
  "Maximum number of lines included in test failure references."
  :type 'integer
  :group 'ai-workbench)

(defconst ai-workbench-tools--context-template-fallback
  "{{task-section}}\n\n{{references}}"
  "Minimal fallback used only when editable context template is unavailable.")

(defconst ai-workbench-tools--writing-template-fallback
  "{{mode}}\n\n{{task}}\n\n{{context}}\n\n{{text}}"
  "Minimal fallback used only when editable writing template is unavailable.")

(defconst ai-workbench-writing-modes
  '(("润色" . polish)
    ("改写" . rewrite)
    ("总结" . summarize)
    ("翻译" . translate)
    ("提纲" . outline)
    ("续写" . continue)
    ("评论" . critique))
  "Writing modes exposed by `ai-workbench-writing-prompt'.")

(defconst ai-workbench-tool-choices
  '(("@本文件" . current-file)
    ("@本行" . current-line)
    ("@所选" . region)
    ("@符号" . current-symbol)
    ("@函数" . defun)
    ("@块" . block)
    ("@项目" . project-root)
    ("@项目文件" . project-files)
    ("@最近修改" . recent-changes)
    ("@Git状态" . git-status)
    ("@诊断" . diagnostics)
    ("@测试失败" . test-failures))
  "Choices exposed by `ai-workbench-context-prompt'.")

(defun ai-workbench-tools--relative-path (file project-root)
  "Return FILE relative to PROJECT-ROOT when possible."
  (if (and file project-root (file-in-directory-p file project-root))
      (file-relative-name file project-root)
    (abbreviate-file-name file)))

(defun ai-workbench-tools--current-file ()
  "Return the current file or signal an error."
  (or (buffer-file-name)
      (user-error "Current buffer is not visiting a file")))

(defun ai-workbench-tools--line-reference (file)
  "Return a reference string for FILE at point."
  (format "@line %s:%d:%d"
          file
          (line-number-at-pos)
          (current-column)))

(defun ai-workbench-tools--position-line-column (position)
  "Return POSITION as a cons cell of line and column."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos) (current-column))))

(defun ai-workbench-tools--format-range (file start end &optional label)
  "Return a FILE reference from START to END with optional LABEL."
  (let ((start-lc (ai-workbench-tools--position-line-column start))
        (end-lc (ai-workbench-tools--position-line-column end)))
    (string-join
     (delq nil
           (list
            (format "@range %s:%d:%d-%d:%d"
                    file
                    (car start-lc)
                    (cdr start-lc)
                    (car end-lc)
                    (cdr end-lc))
            label))
     " ")))

(defun ai-workbench-tools--symbol-reference (file)
  "Return a reference string for the symbol at point in FILE."
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (format "@symbol %s:%d:%d name=%s"
            file
            (line-number-at-pos)
            (current-column)
            symbol)))

(defun ai-workbench-tools--region-range ()
  "Return the current region range as a cons cell."
  (unless (use-region-p)
    (user-error "No active region"))
  (cons (region-beginning) (region-end)))

(defun ai-workbench-tools--defun-range ()
  "Return the current defun range as a plist."
  (or
   (ignore-errors
     (save-excursion
       (mark-defun)
       (let ((start (region-beginning))
             (end (region-end)))
         (deactivate-mark)
         (list :start-line (line-number-at-pos start)
               :end-line (line-number-at-pos end)
               :start start
               :end end
               :name (or (and (fboundp 'which-function) (which-function))
                         (thing-at-point 'symbol t)
                         "anonymous")))))
   (let ((range (ai-workbench-tools--block-range)))
     (plist-put range :name "context-block"))))

(defun ai-workbench-tools--block-range ()
  "Return the current block range as a plist."
  (save-excursion
    (let* ((ppss (syntax-ppss))
           (start (or (nth 1 ppss)
                      (save-excursion
                        (backward-paragraph)
                        (point))))
           (end (or (ignore-errors
                      (goto-char start)
                      (forward-sexp)
                      (point))
                    (save-excursion
                      (forward-paragraph)
                      (point)))))
      (list :start-line (line-number-at-pos start)
            :end-line (line-number-at-pos end)
            :start start
            :end end))))

(defun ai-workbench-tools--git-status-summary (project-root)
  "Return a concise Git status summary for PROJECT-ROOT."
  (when (and (executable-find "git")
             (eq 0 (let ((default-directory project-root))
                     (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree"))))
    (let ((default-directory project-root))
      (string-trim
       (with-temp-buffer
         (call-process "git" nil t nil "status" "--short" "--untracked-files=normal")
         (buffer-string))))))

(defun ai-workbench-tools--diagnostic-summary ()
  "Return a concise diagnostic summary for the current buffer."
  (cond
   ((bound-and-true-p flymake-mode)
    (let* ((diags (flymake-diagnostics (point-min) (point-max)))
           (count (length diags))
           (messages
            (seq-take
             (delq nil
                   (mapcar
                    (lambda (diag)
                      (when-let* ((text (flymake-diagnostic-text diag)))
                        (string-trim text)))
                    diags))
             5)))
      (string-join
       (append
        (list (format "%d flymake diagnostic(s)" count))
        messages)
       "\n")))
   ((bound-and-true-p flycheck-mode)
    (let ((messages
           (seq-take
            (delq nil
                  (mapcar
                   (lambda (err)
                     (when (fboundp 'flycheck-error-message)
                       (flycheck-error-message err)))
                   flycheck-current-errors))
            5)))
      (string-join
       (append
        (list (format "%d flycheck error(s), %d warning(s)"
                      (length flycheck-current-errors)
                      (length (seq-filter (lambda (err)
                                            (eq (flycheck-error-level err) 'warning))
                                          flycheck-current-errors))))
        messages)
       "\n")))
   (t
    "No active diagnostics backend")))

(defun ai-workbench-tools--project-file-summary (project-root)
  "Return a project file summary for PROJECT-ROOT."
  (let* ((default-directory project-root)
         (files (cond
                 ((and (fboundp 'project-current)
                       (project-current nil project-root)
                       (fboundp 'project-files))
                  (project-files (project-current nil project-root)))
                 (t nil)))
         (relative-files
          (mapcar (lambda (file)
                    (ai-workbench-tools--relative-path file project-root))
                  files))
         (shown (seq-take relative-files ai-workbench-tools-project-files-limit)))
    (unless relative-files
      (user-error "No project file list available"))
    (string-join
     (append
      shown
      (when (> (length relative-files) (length shown))
        (list (format "... %d more file(s)"
                      (- (length relative-files) (length shown))))))
     "\n")))

(defun ai-workbench-tools--recent-changes-summary (project-root)
  "Return recently changed files for PROJECT-ROOT."
  (unless (and (executable-find "git")
               (eq 0 (let ((default-directory project-root))
                       (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree"))))
    (user-error "Project is not inside a Git work tree"))
  (let ((default-directory project-root))
    (string-trim
     (with-temp-buffer
       (call-process "git" nil t nil "status" "--short" "--untracked-files=normal")
       (let* ((lines (split-string (buffer-string) "\n" t))
              (shown (seq-take lines 20)))
         (string-join
          (append
           shown
           (when (> (length lines) (length shown))
             (list (format "... %d more change(s)"
                           (- (length lines) (length shown))))))
          "\n"))))))

(defun ai-workbench-tools--failure-lines-from-buffer (buffer)
  "Return likely failure lines from BUFFER."
  (with-current-buffer buffer
    (let ((lines nil)
          (regexp "\\(FAIL\\|FAILED\\|ERROR\\|AssertionError\\|panic\\|panicked\\|Exception\\)"))
      (save-excursion
        (goto-char (point-min))
        (while (and (< (length lines) ai-workbench-tools-test-failure-lines-limit)
                    (re-search-forward regexp nil t))
          (push (string-trim
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
                lines)))
      (nreverse (delete-dups (seq-remove #'string-empty-p lines))))))

(defun ai-workbench-tools--test-failure-summary ()
  "Return a summary of test failures from active buffers."
  (let ((buffers
         (seq-filter
          (lambda (buffer)
            (with-current-buffer buffer
              (or (derived-mode-p 'compilation-mode)
                  (string-match-p "\\*.*test.*\\*" (buffer-name buffer))
                  (string-match-p "\\*.*compilation.*\\*" (buffer-name buffer)))))
          (buffer-list)))
        (lines nil))
    (dolist (buffer buffers)
      (setq lines
            (append lines
                    (ai-workbench-tools--failure-lines-from-buffer buffer))))
    (setq lines (seq-take (delete-dups (delq nil lines))
                          ai-workbench-tools-test-failure-lines-limit))
    (unless lines
      (user-error "No test failure summary found in active buffers"))
    (string-join lines "\n")))

(defun ai-workbench-tools--selected-symbols ()
  "Prompt for one or more tool symbols."
  (mapcar
   (lambda (choice)
     (alist-get choice ai-workbench-tool-choices nil nil #'string=))
   (completing-read-multiple
    "引用工具: "
    (mapcar #'car ai-workbench-tool-choices)
    nil t)))

(defun ai-workbench-tools--build-references (choices project-root)
  "Build reference lines for CHOICES in PROJECT-ROOT."
  (let* ((file (and (buffer-file-name) (ai-workbench-tools--relative-path
                                        (buffer-file-name) project-root)))
         (references nil))
    (dolist (choice choices)
      (pcase choice
        ('current-file
         (let ((path (ai-workbench-tools--relative-path
                      (ai-workbench-tools--current-file) project-root)))
           (push (format "@file %s" path) references)))
        ('current-line
         (let ((path (ai-workbench-tools--relative-path
                      (ai-workbench-tools--current-file) project-root)))
           (push (ai-workbench-tools--line-reference path) references)))
        ('region
         (let* ((path (ai-workbench-tools--relative-path
                       (ai-workbench-tools--current-file) project-root))
                (range (ai-workbench-tools--region-range)))
           (push (ai-workbench-tools--format-range
                  path (car range) (cdr range) "selection")
                 references)))
        ('current-symbol
         (let ((path (ai-workbench-tools--relative-path
                      (ai-workbench-tools--current-file) project-root)))
           (push (ai-workbench-tools--symbol-reference path) references)))
        ('defun
         (let* ((path (ai-workbench-tools--relative-path
                       (ai-workbench-tools--current-file) project-root))
                (range (ai-workbench-tools--defun-range)))
           (push (format "%s name=%s"
                         (ai-workbench-tools--format-range
                          path
                          (plist-get range :start)
                          (plist-get range :end)
                          "defun")
                         (plist-get range :name))
                 references)))
        ('block
         (let* ((path (ai-workbench-tools--relative-path
                       (ai-workbench-tools--current-file) project-root))
                (range (ai-workbench-tools--block-range)))
           (push (ai-workbench-tools--format-range
                  path
                  (plist-get range :start)
                  (plist-get range :end)
                  "block")
                 references)))
        ('project-root
         (push (format "@project-root %s"
                       (abbreviate-file-name project-root))
               references))
        ('project-files
         (push (format "@project-files\n%s"
                       (ai-workbench-tools--project-file-summary project-root))
               references))
        ('recent-changes
         (push (format "@recent-changes\n%s"
                       (ai-workbench-tools--recent-changes-summary project-root))
               references))
        ('git-status
         (when-let* ((status (ai-workbench-tools--git-status-summary project-root)))
           (push (format "@git-status\n%s" status) references)))
        ('diagnostics
         (push (format "@diagnostics %s" (ai-workbench-tools--diagnostic-summary))
               references))
        ('test-failures
         (push (format "@test-failures\n%s"
                       (ai-workbench-tools--test-failure-summary))
               references))))
    (list :references (nreverse references)
          :display-file file)))

(defun ai-workbench-tools--prompt-template (task references)
  "Return a prompt template using TASK and REFERENCES."
  (ai-workbench-profile-render-template
   (ai-workbench-profile-read-template
    "context-prompt"
    ai-workbench-tools--context-template-fallback)
   `(("task-section" . ,(if (string-empty-p task)
                            ""
                          (format "任务:\n\n%s" task)))
     ("references" . ,(string-join references "\n")))))

(defun ai-workbench-tools--writing-text ()
  "Return the text to use for a writing task."
  (when (derived-mode-p 'prog-mode)
    (user-error "Writing prompt does not copy source code; use context references instead"))
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((let ((text (string-trim
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (not (string-empty-p text)))
    (buffer-substring-no-properties (point-min) (point-max)))
   (t
    (user-error "No writing text available"))))

(defun ai-workbench-tools--writing-context (project-root)
  "Return lightweight writing context for PROJECT-ROOT."
  (let ((source (if-let* ((file (buffer-file-name)))
                    (format "source: %s"
                            (ai-workbench-tools--relative-path file project-root))
                  (format "buffer: %s" (buffer-name)))))
    (string-join
     (delq nil
           (list source
                 (when (use-region-p)
                   (format "range: %d-%d"
                           (line-number-at-pos (region-beginning))
                           (line-number-at-pos (region-end))))))
     "\n")))

(defun ai-workbench-tools--writing-prompt (mode task text context)
  "Return a writing prompt for MODE, TASK, TEXT, and CONTEXT."
  (ai-workbench-profile-render-template
   (ai-workbench-profile-read-template
    "writing-prompt"
    ai-workbench-tools--writing-template-fallback)
   `(("mode" . ,mode)
     ("task" . ,task)
     ("context" . ,context)
     ("text" . ,text))))

(defun ai-workbench-tools--context-dispatch (send-immediately)
  "Build context references, then draft or send based on SEND-IMMEDIATELY."
  (let* ((project-root (ai-workbench-project-root))
         (choices (ai-workbench-tools--selected-symbols))
         (task (read-string "AI task: "))
         (context (ai-workbench-tools--build-references choices project-root))
         (references (plist-get context :references))
         (backend (ai-workbench-session-backend project-root))
         (prompt (ai-workbench-tools--prompt-template task references)))
    (unless references
      (user-error "No references selected"))
    (unless (ai-workbench-session-initialized-p project-root)
      (ai-workbench-open)
      (setq backend (ai-workbench-session-backend project-root)))
    (if send-immediately
        (progn
          (ai-workbench-send-string backend prompt project-root)
          (message "ai-workbench sent prompt with %d reference(s)"
                   (length references)))
      (ai-workbench-draft-string backend prompt project-root)
      (message "ai-workbench drafted prompt with %d reference(s); press RET to submit"
               (length references)))))

(defun ai-workbench-writing-prompt ()
  "Draft an AI writing prompt from region or current buffer."
  (interactive)
  (let* ((project-root (ai-workbench-project-root))
         (mode-label (completing-read "Writing mode: "
                                      (mapcar #'car ai-workbench-writing-modes)
                                      nil t nil nil "润色"))
         (task (read-string "Writing task: "))
         (text (ai-workbench-tools--writing-text))
         (context (ai-workbench-tools--writing-context project-root))
         (backend (ai-workbench-session-backend project-root))
         (prompt (ai-workbench-tools--writing-prompt
                  mode-label task text context)))
    (unless (ai-workbench-session-initialized-p project-root)
      (ai-workbench-open)
      (setq backend (ai-workbench-session-backend project-root)))
    (ai-workbench-draft-string backend prompt project-root)
    (message "ai-workbench drafted writing prompt to %s; press RET to submit"
             backend)))

(defun ai-workbench-context-prompt ()
  "Draft a reference-style prompt into the current AI backend."
  (interactive)
  (ai-workbench-tools--context-dispatch nil))

(defun ai-workbench-context-send ()
  "Send a reference-style prompt directly to the current AI backend."
  (interactive)
  (ai-workbench-tools--context-dispatch t))

(provide 'ai-workbench-tools)
;;; ai-workbench-tools.el ends here

;;; ai-workbench-tools.el --- Reference tools for ai-workbench -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Reference-oriented prompt helpers for ai-workbench.

;;; Code:

(require 'thingatpt)
(require 'seq)
(require 'subr-x)
(require 'ai-workbench-session)

(declare-function which-function "which-func" ())
(declare-function ai-workbench-draft-string "ai-workbench" (backend prompt &optional project-root))
(declare-function ai-workbench-open "ai-workbench" ())

(defconst ai-workbench-tool-choices
  '(("@本文件" . current-file)
    ("@本行" . current-line)
    ("@所选" . region)
    ("@函数" . defun)
    ("@块" . block)
    ("@项目" . project-root)
    ("@Git状态" . git-status)
    ("@诊断" . diagnostics))
  "Choices exposed by `ai-workbench-context-prompt'.")

(defun ai-workbench-tools--relative-path (file project-root)
  "Return FILE relative to PROJECT-ROOT when possible."
  (if (and file project-root
           (string-prefix-p (expand-file-name project-root)
                            (expand-file-name file)))
      (file-relative-name file project-root)
    (abbreviate-file-name file)))

(defun ai-workbench-tools--current-file ()
  "Return the current file or signal an error."
  (or (buffer-file-name)
      (user-error "Current buffer is not visiting a file")))

(defun ai-workbench-tools--line-reference (file)
  "Return a reference string for FILE at point."
  (format "@line %s:%d"
          file
          (line-number-at-pos)))

(defun ai-workbench-tools--region-range ()
  "Return the current region range as a cons cell."
  (unless (use-region-p)
    (user-error "No active region"))
  (cons (line-number-at-pos (region-beginning))
        (line-number-at-pos (region-end))))

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
            :end-line (line-number-at-pos end)))))

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
           (count (length diags)))
      (format "%d flymake diagnostic(s)" count)))
   ((bound-and-true-p flycheck-mode)
    (format "%d flycheck error(s), %d warning(s)"
            (length flycheck-current-errors)
            (length (seq-filter (lambda (err)
                                  (eq (flycheck-error-level err) 'warning))
                                flycheck-current-errors))))
   (t
    "No active diagnostics backend")))

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
           (push (format "@range %s:%d-%d selection"
                         path (car range) (cdr range))
                 references)))
        ('defun
         (let* ((path (ai-workbench-tools--relative-path
                       (ai-workbench-tools--current-file) project-root))
                (range (ai-workbench-tools--defun-range)))
           (push (format "@defun %s:%d-%d name=%s"
                         path
                         (plist-get range :start-line)
                         (plist-get range :end-line)
                         (plist-get range :name))
                 references)))
        ('block
         (let* ((path (ai-workbench-tools--relative-path
                       (ai-workbench-tools--current-file) project-root))
                (range (ai-workbench-tools--block-range)))
           (push (format "@block %s:%d-%d"
                         path
                         (plist-get range :start-line)
                         (plist-get range :end-line))
                 references)))
        ('project-root
         (push (format "@project-root %s"
                       (abbreviate-file-name project-root))
               references))
        ('git-status
         (when-let* ((status (ai-workbench-tools--git-status-summary project-root)))
           (push (format "@git-status\n%s" status) references)))
        ('diagnostics
         (push (format "@diagnostics %s" (ai-workbench-tools--diagnostic-summary))
               references))))
    (list :references (nreverse references)
          :display-file file)))

(defun ai-workbench-tools--prompt-template (task references)
  "Return a prompt template using TASK and REFERENCES."
  (string-join
   (delq nil
         (list "请直接基于下面这些 Emacs 引用上下文工作，不要要求我重复粘贴源码。"
               "把这些引用当作权威上下文；需要修改时给出清晰、可审阅的变更。"
               (and (not (string-empty-p task))
                    (format "\n任务:\n%s" task))
               "\n引用:"
               (string-join references "\n")
               "\n输出要求:"
               "- 说明你将修改哪些文件或范围"
               "- 若产生改动，保持改动可通过 Emacs diff 审阅 / accept / reject"))
   "\n"))

(defun ai-workbench-context-prompt ()
  "Draft a reference-style prompt into the current AI backend."
  (interactive)
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
    (ai-workbench-draft-string backend prompt project-root)
    (message "ai-workbench drafted prompt with %d reference(s); press RET to submit"
             (length references))))

(provide 'ai-workbench-tools)
;;; ai-workbench-tools.el ends here

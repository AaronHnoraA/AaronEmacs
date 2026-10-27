;;; aaron-ui-docs.el --- Markdown reference in an ETAF panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read a repository Markdown document beside the current work without
;; replacing a window.  Parsing happens once, before ETAF mounts the View.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'aaron-ui-etaf)

(autoload 'config-board "config-tools" nil t)
(autoload '+config/ask-agent "tools/agent/config" nil t)

(defvar aaron-ui-docs--panel nil "The live ETAF document panel.")
(defvar aaron-ui-docs--entry nil "Reactive document shown in the panel.")
(defvar aaron-ui-docs--history nil "Documents visited in the current reader.")
(defvar aaron-ui-docs--history-index -1 "Current index in document history.")
(defvar aaron-ui-docs--catalog nil "Ordered Markdown pages linked from the docs home.")

(defconst aaron-ui-docs--panel-title "Emacs · Noema 文档"
  "Title used to identify the document reader in the ETAF host.")

(defun aaron-ui-docs--heading-ref (id)
  "Return the Host reference for Markdown heading ID."
  (intern (format "aaron-ui-doc-%s" id)))

(defun aaron-ui-docs--palette ()
  "Return the semantic Markdown colors for the current Aaron theme."
  (mapcar (lambda (entry)
            (cons (car entry) (aaron-ui-color (cdr entry))))
          '((:ink . fg-main) (:muted . fg-muted) (:faint . fg-faint)
            (:heading . fg-strong) (:accent . accent-cyan)
            (:accent-soft . bg-surface-strong) (:panel . bg-panel)
            (:code-bg . bg-code) (:border . border-subtle)
            (:quote-bg . bg-elevated) (:quote-bar . accent-blue)
            (:table-header . bg-panel) (:table-stripe . bg-elevated)
            (:ok . accent-green) (:ok-soft . bg-success)
            (:warn . accent-yellow) (:warn-soft . bg-elevated)
            (:danger . accent-red-strong) (:danger-soft . bg-danger)
            (:important . accent-mauve) (:important-soft . bg-elevated))))

(defun aaron-ui-docs--follow-link (url file actions)
  "Follow Markdown URL relative to FILE, keeping ACTIONS on linked pages."
  (cond
   ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url)
    (browse-url url))
   (t
    (let* ((parts (split-string url "#"))
           (relative (car parts))
           (heading (cadr parts)))
      (cond
       ((and (string-empty-p relative) heading)
        (etaf-scroll-into-view
         (etaf-runtime-for-buffer (current-buffer))
         (aaron-ui-docs--heading-ref heading) 'start))
       (t
        (let ((target (expand-file-name relative (file-name-directory file))))
          (if (and (file-readable-p target)
                   (equal (downcase (or (file-name-extension target) "")) "md"))
              (aaron-ui-document-panel target nil actions)
            (find-file target)))))))))

(defun aaron-ui-docs--navigate (delta)
  "Move DELTA entries through the document reader history."
  (let ((index (+ aaron-ui-docs--history-index delta)))
    (cond
     ((and (<= 0 index) (< index (length aaron-ui-docs--history)))
      (setq aaron-ui-docs--history-index index)
      (etaf-set-value aaron-ui-docs--entry
                      (nth index aaron-ui-docs--history)))
     ((and (> delta 0) aaron-ui-docs--catalog)
      (let* ((page (etaf-value aaron-ui-docs--entry))
             (tail (member (plist-get page :file) aaron-ui-docs--catalog))
             (next (cadr tail)))
        (if next
            (aaron-ui-document-panel next nil (plist-get page :actions))
          (message "已经是最后一篇文档"))))
     (t (message "没有上一页")))))

(defun aaron-ui-docs--reset ()
  "Release references held by the closed document reader."
  (setq aaron-ui-docs--panel nil
        aaron-ui-docs--entry nil
        aaron-ui-docs--history nil
        aaron-ui-docs--history-index -1
        aaron-ui-docs--catalog nil))

(defun aaron-ui-docs--close ()
  "Close the floating document reader."
  (interactive)
  (when (featurep 'aaron-ui-float)
    (aaron-ui-float-close aaron-ui-docs--panel))
  (aaron-ui-docs--reset))

(defun aaron-ui-docs--leave-panel (action)
  "Close the current floating document, then run ACTION."
  (aaron-ui-docs--close)
  (funcall action))

(defun aaron-ui-docs--action-field (action field)
  "Return FIELD from ACTION, accepting legacy label/function cons cells."
  (if (keywordp (car action))
      (plist-get action field)
    (pcase field
      (:label (car action))
      (:command (cdr action)))))

(etaf-define-component aaron-ui-docs-page (&key entry)
  "Render the reactive Markdown ENTRY in the floating reader."
  :setup (aaron-ui-etaf-provide-theme)
  :render
  (let* ((_theme (etaf-value (aaron-ui-etaf-theme-ref)))
         (page (etaf-value entry))
         (document (plist-get page :document))
         (file (plist-get page :file))
         (title (plist-get page :title))
         (actions (plist-get page :actions))
         (outline (plist-get document :outline))
         (blocks (plist-get document :children))
         (rows (max 12 (or (cdr (etaf-viewport)) 32)))
         (body-height (max 6 (- rows (if actions 3 1))))
         (env (list :palette (aaron-ui-docs--palette)
                    :features (plist-get document :features)
                    :line-grid t
                    :open-link (lambda (url)
                                 (aaron-ui-docs--follow-link url file actions)))))
    (etaf-view
     (column :width '(vw 100) :height '(vh 100) :overflow 'hidden
       (expr
        (when actions
          (etaf-view
           (row :height '(lh 2) :padding '((lh 0) (ch 2))
                :item-gap '(ch 1)
             (expr
              (mapcar
               (lambda (action)
                 (etaf-view
                  (etaf-button
                   :label (aaron-ui-docs--action-field action :label)
                   :variant 'secondary
                   :on-press
                   (lambda ()
                     (let ((command (aaron-ui-docs--action-field
                                     action :command)))
                       (if (aaron-ui-docs--action-field action :keep-open)
                           (funcall command)
                         (aaron-ui-docs--leave-panel command)))))))
               actions))))))
       (row :width 'stretch :height (list 'lh body-height)
         (column :width '(ch 24) :height (list 'lh body-height) :overflow 'scroll
                 :padding '((lh 1) (ch 1)) :item-gap '(lh 0)
           (etaf-label :text "目录" :variant 'strong)
           (expr
            (mapcar
             (lambda (heading)
               (let ((ref (aaron-ui-docs--heading-ref (nth 2 heading)))
                     (label (format "%s%s"
                                    (make-string (max 0 (1- (car heading))) ?\s)
                                    (nth 1 heading))))
                 (etaf-view
                  (etaf-button
                   :label label :variant 'ghost
                   :on-press
                   (lambda ()
                     (etaf-scroll-into-view
                      (etaf-runtime-for-buffer (current-buffer))
                      ref 'start))))))
             outline)))
         (column :ref 'aaron-ui-docs-scroll
                 :flex-grow 1 :flex-basis '(px 0) :min-width '(px 0)
                 :height (list 'lh body-height) :overflow 'scroll
                 :padding '((lh 1) (ch 2)) :item-gap '(lh 1)
           (expr
            (mapcar
             (lambda (block)
               (let ((ref (and (eq (plist-get block :type) 'heading)
                               (aaron-ui-docs--heading-ref
                                (plist-get block :id)))))
                 (etaf-view
                  (box :ref ref :width 'stretch
                    (expr (etaf-ui-markdown-view-block block env))))))
             blocks))))))))

;;;###autoload
(defun aaron-ui-document-panel (file &optional title actions)
  "Open local Markdown FILE in a floating ETAF document reader.
The graphical reader has a contents list and optional ACTIONS, each a
label/function pair.  TTY opens the original file."
  (interactive
   (list (read-file-name
          "Markdown document: "
          (expand-file-name "docs/" user-emacs-directory) nil t)))
  (let ((file (expand-file-name file)))
    (unless (and (file-readable-p file)
                 (equal (downcase (or (file-name-extension file) "")) "md"))
      (user-error "Not a readable Markdown document: %s" file))
    (if (not (display-graphic-p))
        (find-file-read-only file)
      (let* ((document (etaf-ui-markdown-parse
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
             (title (or title (plist-get document :title)
                        (file-name-base file)))
             (page (list :document document :file file
                         :title title :actions actions)))
        (require 'aaron-ui-float)
        (unless (and aaron-ui-docs--panel
                     (aaron-ui-float-live-p aaron-ui-docs--panel))
          (setq aaron-ui-docs--panel nil
                aaron-ui-docs--entry nil
                aaron-ui-docs--history nil
                aaron-ui-docs--history-index -1))
        (setq aaron-ui-docs--history
              (append (cl-subseq aaron-ui-docs--history 0
                                (1+ aaron-ui-docs--history-index))
                      (list page))
              aaron-ui-docs--history-index
              (1- (length aaron-ui-docs--history)))
        (if aaron-ui-docs--panel
            (progn
              (etaf-set-value aaron-ui-docs--entry page)
              (aaron-ui-float-reopen aaron-ui-docs--panel))
          (setq aaron-ui-docs--entry (etaf-ref page)
                aaron-ui-docs--panel
                (aaron-ui-float-show-view
                 (etaf-view (aaron-ui-docs-page :entry aaron-ui-docs--entry))
                 :key 'aaron-ui-document
                 :title aaron-ui-docs--panel-title
                 :width '(fraction 0.78) :height '(fraction 0.78)
                 :on-close (lambda (_panel) (aaron-ui-docs--reset))
                 :green-left (lambda () (aaron-ui-docs--navigate -1))
                 :green-right (lambda () (aaron-ui-docs--navigate 1))
                 :green-help "左键上一页，右键下一页；C-c C-= 改大小")))))))

;;;###autoload
(defun config-manual-panel ()
  "Read this configuration's own docs in an ETAF floating panel."
  (interactive)
  (aaron-ui-document-panel
   (expand-file-name "docs/design/config-management.md" user-emacs-directory)
   "我的配置文档"
   (list (cons "Customize" #'config-board)
         (list :label "Ask · 询问配置" :keep-open t
               :command (lambda ()
                          (call-interactively #'+config/ask-agent))))))

;;;###autoload
(defun aaron-ui-docs-home ()
  "Open the Emacs and Noema documentation map in an ETAF panel."
  (interactive)
  (let* ((file (expand-file-name "docs/guide/documentation-center.md"
                                 user-emacs-directory))
         (catalog
          (cons file
                (with-temp-buffer
                  (insert-file-contents file)
                  (let (links)
                    (goto-char (point-min))
                    (while (re-search-forward "\\]([^)#]+\\.md)" nil t)
                      (let* ((link (substring (match-string 0) 2 -1))
                             (target (expand-file-name link
                                                       (file-name-directory file))))
                        (when (file-readable-p target)
                          (push target links))))
                    (nreverse links))))))
    (setq aaron-ui-docs--history nil
          aaron-ui-docs--history-index -1)
    (aaron-ui-document-panel
     file "Emacs · Noema 文档"
     (list (list :label "Ask · 询问配置" :keep-open t
                 :command (lambda ()
                            (call-interactively #'+config/ask-agent)))
           (cons "Emacs 官方手册" #'info-emacs-manual)))
    (setq aaron-ui-docs--catalog catalog)))

(provide 'aaron-ui-docs)
;;; aaron-ui-docs.el ends here

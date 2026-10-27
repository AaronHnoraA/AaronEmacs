;;; init-agent-shell-ux-tests.el --- Agent panel details taken from neo-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remote images in agent replies are links, not downloads; a session does
;; not take the line at point; every agent picker offers the same agents.  Run with the full configuration loaded.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
(require 'term/portal/popup)
(require 'noema-agent-acp)
(require 'tools/agent/config)

(ert-deftest agent-shell-ux-remote-images-are-links-not-downloads ()
  (should (eq agent-shell-markdown-render-function #'+agent-shell-render-markdown))
  (cl-letf (((symbol-function 'agent-shell-markdown--fetch-remote-image)
             (lambda (_url directory)
               (when directory (error "Must not download"))
               nil)))
    (with-temp-buffer
      (insert "see ![chart](https://example.com/a.png) here\n")
      (+agent-shell-render-markdown :render-images t :complete t
                                    :image-cache-directory temporary-file-directory)
      (goto-char (point-min))
      (search-forward "chart")
      (should (eq (get-text-property (1- (point)) 'face) 'agent-shell-markdown-link)))))

(ert-deftest agent-shell-ux-remote-images-can-be-turned-back-on ()
  (let ((+agent-shell-fetch-remote-images t)
        seen)
    (cl-letf (((symbol-function 'agent-shell-markdown-replace-markup)
               (lambda (&rest args) (setq seen (plist-get args :image-cache-directory)))))
      (+agent-shell-render-markdown :image-cache-directory "/cache/"))
    (should (equal seen "/cache/"))))

(ert-deftest agent-shell-ux-session-does-not-take-the-line-at-point ()
  (should (equal agent-shell-context-sources '(files region error))))

(ert-deftest agent-shell-ux-pickers-offer-the-same-agents ()
  (should (equal (mapcar (lambda (item) (aref item 0)) (+portal-popup--agent-menu-items))
                 (mapcar #'cdr +agent-shell-agents)))
  (dolist (item (+portal-popup--agent-menu-items))
    (should (commandp (aref item 1))))
  (dolist (agent +agent-shell-agents)
    (should (noema-agent-acp-config-for (car agent))))
  (should-error (+agent-shell--check-agent 'nonsense) :type 'user-error))

(ert-deftest agent-shell-ux-config-question-uses-hidden-acp-and-echo ()
  "Docs Ask calls the existing ACP backend without displaying its shell."
  (let ((+config-ask-model "gpt-6-luna")
        started configured submitted callback finished)
    (cl-letf (((symbol-function 'noema-agent-acp-config-for)
               (lambda (_agent) '((:identifier . codex))))
              ((symbol-function 'noema-agent-acp-export-config)
               (lambda (config model)
                 (setq configured model)
                 config))
              ((symbol-function 'noema-agent-acp-start)
               (lambda (&rest args)
                 (setq started args)
                 (get-buffer-create " *config-question-test*")))
              ((symbol-function 'noema-agent-acp-subscribe)
               (lambda (&rest args)
                 (setq callback (plist-get args :callback))
                 17))
              ((symbol-function 'agent-shell-insert)
               (lambda (&rest args) (setq submitted args)))
              ((symbol-function '+config--ask-finish)
               (lambda (_buffer _subscription answer)
                 (setq finished answer))))
      (unwind-protect
          (progn
            (+config/ask-agent "配置值从哪里读取？")
            (should (equal configured "gpt-6-luna"))
            (should (eq (plist-get started :focus) nil))
            (should (eq (plist-get started :render-policy) 'never))
            (should (eq (plist-get started :origin) 'side))
            (should (equal (plist-get started :directory)
                           (file-name-as-directory
                            (file-truename user-emacs-directory))))
            (should (eq (plist-get submitted :submit) t))
            (should (eq (plist-get submitted :no-focus) t))
            (should (string-match-p "docs/guide/documentation-center.md"
                                    (plist-get submitted :text)))
            (funcall callback '((:event . agent-message-chunk)
                                (:data . ((:text-chunk . "从 store 读取。")))))
            (funcall callback '((:event . turn-complete)))
            (should (equal finished "从 store 读取。")))
        (when-let* ((buffer (get-buffer " *config-question-test*")))
          (kill-buffer buffer))))))

(ert-deftest agent-shell-ux-config-ask-shows-only-final-answer ()
  "A one-call Ask reply omits any ACP preamble from the echo area."
  (should (equal (+config--ask-final-answer
                  "我会先检查。\n<answer>按 C-x C-f 打开文件。</answer>")
                 "按 C-x C-f 打开文件。"))
  (should (equal (+config--ask-final-answer "直接回复。") "直接回复。")))

(ert-deftest agent-shell-ux-config-ask-permissions-stay-headless ()
  "The quick answer only authorizes reads and never opens permission UI."
  (let (selected)
    (should (+config--ask-permission
             `((:tool-call . ((:kind . "read")))
               (:options . (((:kind . "allow_once") (:option-id . "read"))))
               (:respond . ,(lambda (id) (setq selected id))))))
    (should (equal selected "read"))
    (should (+config--ask-permission
             `((:tool-call . ((:kind . "edit")))
               (:options . (((:kind . "reject_once") (:option-id . "deny"))))
               (:respond . ,(lambda (id) (setq selected id))))))
    (should (equal selected "deny"))))

(provide 'init-agent-shell-ux-tests)
;;; init-agent-shell-ux-tests.el ends here

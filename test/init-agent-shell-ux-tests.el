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

(ert-deftest agent-shell-ux-config-question-is-a-reviewable-draft ()
  "A Config question starts in this repo and remains unsent."
  (let (started drafted)
    (cl-letf (((symbol-function 'noema-agent-start)
               (lambda (agent)
                 (setq started (list agent default-directory))
                 (get-buffer-create " *config-question-test*")))
              ((symbol-function 'noema-agent-acp-draft)
               (lambda (buffer text)
                 (setq drafted (list buffer text)))))
      (unwind-protect
          (progn
            (+config/ask-agent "配置值从哪里读取？" 'codex)
            (should (equal started
                           (list 'codex
                                 (file-name-as-directory
                                  (expand-file-name user-emacs-directory)))))
            (should (eq (car drafted) (get-buffer " *config-question-test*")))
            (should (string-match-p "docs/guide/documentation-center.md"
                                    (cadr drafted)))
            (should (string-match-p "配置值从哪里读取" (cadr drafted))))
        (when-let* ((buffer (get-buffer " *config-question-test*")))
          (kill-buffer buffer))))))

(provide 'init-agent-shell-ux-tests)
;;; init-agent-shell-ux-tests.el ends here

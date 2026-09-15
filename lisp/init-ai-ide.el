;;; init-ai-ide.el --- AI-assisted IDE integrations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Noema is the single Emacs-native AI/research entry layer:
;;
;;   gptel source      arbitrary-buffer compose/context/preset/rewrite UI
;;   agent-shell/acp   structured external-agent sessions and permissions
;;   Magent            queue, ledger, tools and optional gptel-backed agent
;;
;; All three complete implementations live under `site-lisp/noema/upstream'.
;; They are loaded from this repository rather than installed as package
;; dependencies.  C-c A is the Noema command prefix.

;;; Code:

(require 'config)
(require 'init-package-utils)

(add-to-list 'load-path
             (file-name-as-directory
              (locate-user-emacs-file "site-lisp/noema/lisp")))
(require 'noema-upstream)

(autoload 'noema "noema" nil t)
(autoload 'noema-compose "noema-compose" nil t)
(autoload 'noema-compose-send "noema-compose" nil t)
(autoload 'noema-compose-menu "noema-compose" nil t)
(autoload 'noema-compose-add-context "noema-compose" nil t)
(autoload 'noema-compose-rewrite "noema-compose" nil t)
(autoload 'noema-agent-start "noema-agent-acp" nil t)
(autoload 'noema-agent-promote-current-session "noema-agent-promote" nil t)
(autoload 'noema-research-history-index "noema-agent-promote" nil t)
(autoload 'noema-agent-worker-run-work-cell "noema-agent-worker" nil t)
(autoload 'noema-agent-worker-run-prompt-file "noema-agent-worker" nil t)
(autoload 'noema-agent-worker-decide-permission "noema-agent-worker" nil t)
(autoload 'noema-agent-worker-cancel-queued "noema-agent-worker" nil t)
(autoload 'noema-agent-takeover-session "noema-agent-takeover" nil t)
(autoload 'noema-agent-handback-session "noema-agent-takeover" nil t)
(autoload 'noema-research-attention "noema-research-inspector" nil t)
(autoload 'noema-research-propose-with-magent "noema-research-synthesis" nil t)
(autoload 'noema-project-enable "noema-research" nil t)
(autoload 'noema-pi-router-open "noema-pi-router" nil t)
(autoload 'noema-pi-doctor "noema-pi-router" nil t)
(autoload 'noema-sessions "noema-sessions" nil t)
(autoload 'noema-sessions-switch "noema-sessions" nil t)

;; Standard compatibility infrastructure remains package-managed.  gptel,
;; acp.el, shell-maker, agent-shell and Magent themselves do not.
(use-package compat :ensure t :defer t)

(config-defvar magent-session-directory
  (locate-user-emacs-file "var/noema/magent/sessions/")
  "Directory for Magent sessions and their audit trail."
  :type 'directory
  :group 'ai)

(config-defvar magent-request-timeout 180
  "Inactivity timeout for API and structured CLI sampling."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-json-line-bytes (* 1024 1024)
  "Maximum buffered bytes for one coding-agent JSON event."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-answer-bytes (* 8 1024 1024)
  "Maximum assistant bytes retained from one coding-agent turn."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-diagnostic-bytes (* 256 1024)
  "Maximum diagnostic bytes retained from one coding-agent turn."
  :type 'integer
  :group 'ai)

(config-defvar noema-interaction-magent-cli-max-prompt-bytes (* 4 1024 1024)
  "Maximum combined Magent context and user prompt sent to a CLI."
  :type 'integer
  :group 'ai)

(declare-function noema-interaction-engine-cli-register
                  "noema-interaction-engine-cli" ())

(defvar-keymap my/noema-prefix-map
  :doc "Prefix map for Noema research and agent commands."
  "a" #'noema-agent-start
  "W" #'noema
  "c" #'noema-compose
  "s" #'noema-compose-send
  "m" #'noema-compose-menu
  "." #'noema-compose-add-context
  "r" #'noema-compose-rewrite
  "p" #'noema-agent-promote-current-session
  "I" #'noema-research-attention
  "P" #'noema-pi-router-open
  "D" #'noema-pi-doctor
  "S" #'noema-sessions
  "b" #'noema-sessions-switch)

(global-set-key (kbd "C-c M-a") #'noema-compose-add-context)
(global-set-key (kbd "C-c A") my/noema-prefix-map)

;;; ── Noema settings in the config registry (D-035) ─────────────────────────
;;
;; Noema keeps its tunables as ordinary defcustoms so its repository stays
;; independent.  This glue registers them with `config' once their library
;; loads, so they appear on the config board and persist to
;; `etc/config-noema.el' like every other setting.  Noema's own settings
;; center saves through `customize-save-variable', which `config-custom'
;; routes into the same store.

(defvar config--registry)
(declare-function noema-pi-project-root "noema-pi-router" (&optional directory))
(declare-function noema-pi-close-project "noema-pi-router" (&optional directory interrupt))
(declare-function persp-current-buffers "perspective" ())

(defconst my/noema-config-groups
  '(noema-research noema-research-graph noema-agent-worker noema-pi-router)
  "Custom groups whose options Noema exposes through `config'.")

(defun my/noema-config--type-arguments (symbol)
  "Return `config-register' type arguments for Custom option SYMBOL."
  (pcase (get symbol 'custom-type)
    ('boolean '(:type boolean))
    ((or 'integer 'natnum) '(:type integer))
    ('number '(:type number))
    ((or 'string 'file 'directory) '(:type string))
    (`(choice . ,options)
     (if (seq-every-p (lambda (option) (eq (car-safe option) 'const)) options)
         `(:type sexp :choices ,(mapcar (lambda (option) (car (last option))) options))
       '(:type sexp)))
    (_ '(:type sexp))))

(defun my/noema-config-register-groups (&rest _)
  "Register every loaded, not yet registered Noema option with `config'."
  (dolist (group my/noema-config-groups)
    (dolist (member (get group 'custom-group))
      (pcase-let ((`(,symbol ,kind) member))
        (when (and (eq kind 'custom-variable)
                   (boundp symbol)
                   (not (gethash symbol config--registry)))
          (apply #'config-register symbol
                 :group 'noema
                 :store-file "etc/config-noema.el"
                 :doc (car (split-string (or (documentation-property
                                              symbol 'variable-documentation t)
                                             "")
                                         "\n"))
                 (my/noema-config--type-arguments symbol)))))))

(dolist (feature '(noema-research-mode noema-research-graph noema-research-settings
                   noema-agent-worker noema-pi-router))
  (eval-after-load feature #'my/noema-config-register-groups))

(defun my/noema-close-projects-of-killed-perspective ()
  "Close Noema projects whose documents live only in the dying perspective.
`persp-killed-hook' runs inside that perspective, before its buffers go.  The
project's Pi and idle agents stop; a running Run finishes first."
  (when (and (featurep 'noema-pi-router) (fboundp 'persp-current-buffers))
    (let* ((inside (seq-filter #'buffer-live-p (persp-current-buffers)))
           (root-of (lambda (buffer)
                      (with-current-buffer buffer
                        (and buffer-file-name
                             (derived-mode-p 'noema-research-mode)
                             (noema-pi-project-root (file-name-directory buffer-file-name))))))
           (roots (delete-dups (delq nil (mapcar root-of inside)))))
      (dolist (root roots)
        (unless (seq-some (lambda (buffer)
                            (and (not (memq buffer inside))
                                 (equal (funcall root-of buffer) root)))
                          (buffer-list))
          (noema-pi-close-project root))))))

(add-hook 'persp-killed-hook #'my/noema-close-projects-of-killed-perspective)

;;; ── Claude Code ────────────────────────────────────────────────────────────

(config-defvar claude-code-ide-cli-path nil
  "Path to the Claude Code CLI binary."
  :type 'file
  :group 'ai)

(config-defvar claude-code-ide-window-side nil
  "Side used for the Claude Code IDE window."
  :type 'symbol
  :group 'ai)

(config-defvar claude-code-ide-window-width nil
  "Width used for the Claude Code IDE window."
  :type 'integer
  :group 'ai)

;; Claude Code compatibility source remains vendored for ACP adapter work, but
;; its standalone IDE UI is deliberately not autoloaded or globally bound.

;;; ── Codex CLI ──────────────────────────────────────────────────────────────

(config-defvar codex-cli-executable nil
  "Executable used by codex-cli."
  :type 'string
  :group 'ai)

(config-defvar codex-cli-terminal-backend nil
  "Terminal backend used by codex-cli."
  :type 'symbol
  :group 'ai)

(config-defvar codex-cli-side nil
  "Side used for the codex-cli window."
  :type 'symbol
  :group 'ai)

(config-defvar codex-cli-width nil
  "Width used for the codex-cli window."
  :type 'integer
  :group 'ai)

;; Codex CLI compatibility source likewise stays vendored without a separate
;; terminal UI or global key family.  Structured agents enter through Noema.

;; ── Native CLI fallback inside embedded gptel ──────────────────────────────

;; The normal agent path is ACP/agent-shell.  Keep the migrated one-shot CLI
;; sampler as a fallback backend without retaining the old gptel fork.
(with-eval-after-load 'gptel
  (require 'noema-interaction-engine-cli)
  (noema-interaction-engine-cli-register))

(provide 'init-ai-ide)
;;; init-ai-ide.el ends here

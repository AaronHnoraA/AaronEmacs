;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; Refactored and merged configuration for Company, LSP, and Debugging.

;;; Code:

;; -------------------------
;; 1. Company Mode (Completion)
;; -------------------------
;; https://company-mode.github.io/manual/

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :init
  (global-company-mode) ;; 全局启用
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-selection)
         )
  :after yasnippet
  :config
  (define-advice company-capf--candidates (:around (func &rest args))
    "Try default completion styles."
    (let ((completion-styles '(basic partial-completion)))
      (apply func args)))
  
  :custom
  ;; 核心体验设置
  (company-idle-delay 0.05)            ;; 立即触发补全
  (company-minimum-prefix-length 1) ;; 至少3个字符触发
  (company-show-numbers t)          ;; 显示编号 (M-1, M-2 选择)
  (company-show-quick-access t)     ;; 允许 M-<n> 快速选择
  (company-require-match nil)       ;; 不强制匹配
  
  ;; UI 设置
  (company-tooltip-width-grow-only t)
  (company-tooltip-align-annotations t)
  (company-format-margin-function nil) ;; No icons inside margin (cleaner)

  ;; Dabbrev 设置 (文本补全)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  
  ;; 文件/路径补全设置
  (company-files-exclusions '(".git/" ".DS_Store"))

  ;; Backends 设置
  ;; 注意：`company-files` 在这里确保了路径补全功能。
  ;; 当你输入 "/" 或 "./" 时，company-files 会接管。
  (company-backends 
        '((company-capf 
          company-files          ; <--- 移到这里，与 LSP 平级
          :with company-tempo 
          company-yasnippet)
          (company-dabbrev-code company-keywords)
          company-dabbrev))
  )

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))


;; -------------------------
;; 2. Aggressive Indent
;; -------------------------
(use-package aggressive-indent
  :ensure t
  :hook (
         (elisp-mode . aggressive-indent-mode)
         (python-mode  . aggressive-indent-mode)
s        (c++-mode     . aggressive-indent-mode)
         (c-mode       . aggressive-indent-mode)
         ))


;; -------------------------
;; 3. Flymake (Diagnostics)
;; -------------------------
(use-package flymake
  :ensure nil ; Emacs built-in
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c !" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-no-changes-timeout 0.3)
  (flymake-indicator-type 'fringes))

;; 光标停在报错位置时，在 minibuffer/eldoc 显示诊断
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-display-diagnostic-function
   #'flymake-diagnostic-at-point-display-minibuffer))


;; -------------------------
;; 4. LSP Mode (Core)
;; -------------------------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-deferred)
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename))
  :config
  (with-no-warnings
    (lsp-enable-which-key-integration t))
  
  :custom
  ;; 前缀键
  (lsp-keymap-prefix "C-c l")
  
  ;; 性能与功能开关
  (lsp-log-io nil)                          ;; debug only
  (lsp-auto-guess-root t)                   ;; 使用 projectile root
  (lsp-keep-workspace-alive nil)            ;; auto kill lsp server
  (lsp-enable-file-watchers nil)            ;; 性能优化：禁用文件监控
  
  ;; UI 简化 (Clean UI)
  (lsp-enable-links nil)                    ;; no clickable links
  (lsp-enable-folding nil)                  ;; use hideshow' instead
  (lsp-enable-text-document-color t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable t)    ;; 保持顶部干净
  (lsp-modeline-code-actions-enable nil)    ;; 保持 modeline 干净
  (lsp-modeline-diagnostics-enable nil)
  
  ;; 语义高亮
  (lsp-semantic-tokens-enable nil)
  (lsp-semantic-tokens-apply-modifiers nil)
  
  ;; 补全与 Snippets
  (lsp-completion-provider :none)           ;; 关键：设为 none，因为我们在 company 中手动配置了 capf
  (lsp-completion-enable t)
  (lsp-enable-snippet t)                    ;; 开启 snippet 支持 (参数模板)
  (lsp-completion-enable-additional-text-edit nil)
  
  ;; 提升 LSP / jsonrpc 吞吐
  (read-process-output-max (* 1024 1024)) ;; 1MB

  ;; 签名提示 (Signature Help)
  (lsp-signature-auto-activate t)           ;; 自动弹出函数签名
  (lsp-signature-render-documentation t)
  (lsp-signature-doc-lines 5)
  (lsp-eldoc-enable-hover nil)              ;; 禁用 eldoc hover，防止干扰 signature
  
  ;; 诊断 (Diagnostics)
  ;; 设为 :auto 以使用 flymake (因为上面配置了 flymake)
  ;; 如果要用 flycheck，需改为 :flycheck 并安装 flycheck 包
  (lsp-diagnostics-provider :auto))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode)


;; -------------------------
;; 5. LSP UI (Doc, Peek, Imenu)
;; -------------------------
(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-doc-show
             lsp-ui-doc-hide
             lsp-ui-doc-glance
             lsp-ui-peek-find-definitions
             lsp-ui-peek-find-references
             lsp-ui-peek-find-implementation)
  :bind
  (
   ("C-h d" . lsp-ui-doc-glance)                ; 快速看文档
   ("C-h D" . lsp-ui-doc-toggle)                ; 强制显示文档
   ("C-h c" . lsp-ui-doc-hide)                  ; 隐藏文档
   ("C-h e" . lsp-ui-peek-find-definitions)     ; 定义
   ("C-h r" . lsp-ui-peek-find-references)      ; 引用
   ("C-h i" . lsp-ui-peek-find-implementation)) ; 实现
  :custom
  ;; Doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 0.3)
  
  ;; Peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t))

(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle)
  :custom
  ;; 放左边或右边都行
  (imenu-list-position 'left)

  ;; 自动根据当前 buffer 更新
  ;(imenu-list-auto-update t)

  ;; 高亮当前光标所在符号
  (imenu-list-highlight-current-entry t)

  ;; 不要抢焦点
  (imenu-list-focus-after-activation nil)

  ;; 窗口宽度
  (imenu-list-size 0.25)
)



;; -------------------------
;; 6. DAP Mode (Debugging)
;; -------------------------
(use-package dap-mode
  :ensure t
  :after (hydra lsp-mode)
  :commands dap-debug
  :custom
  (dap-auto-configure-mode t)
  :config
  (dap-ui-mode 1)
  :hydra
  (hydra-dap-mode
   (:color pink :hint nil :foreign-keys run)
   "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
                  _sb_: List breakpoints
                  _sS_: List sessions
"
   ("n" dap-next)
   ("i" dap-step-in)
   ("o" dap-step-out)
   ("c" dap-continue)
   ("r" dap-restart-frame)
   ("ss" dap-switch-session)
   ("st" dap-switch-thread)
   ("sf" dap-switch-stack-frame)
   ("su" dap-up-stack-frame)
   ("sd" dap-down-stack-frame)
   ("sl" dap-ui-locals)
   ("sb" dap-ui-breakpoints)
   ("sS" dap-ui-sessions)
   ("bb" dap-breakpoint-toggle)
   ("ba" dap-breakpoint-add)
   ("bd" dap-breakpoint-delete)
   ("bc" dap-breakpoint-condition)
   ("bh" dap-breakpoint-hit-condition)
   ("bl" dap-breakpoint-log-message)
   ("dd" dap-debug)
   ("dr" dap-debug-recent)
   ("ds" dap-debug-restart)
   ("dl" dap-debug-last)
   ("de" dap-debug-edit-template)
   ("ee" dap-eval)
   ("ea" dap-ui-expressions-add)
   ("er" dap-eval-region)
   ("es" dap-eval-thing-at-point)
   ("q" nil "quit" :color blue)
   ("Q" dap-disconnect :color red)))


;; -------------------------
;; 7. Eglot (Alternative LSP, Disabled)
;; -------------------------
(use-package eglot
  :disabled
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c f" . eglot-format)
         ("C-c d" . eldoc-doc-buffer)
         ("C-c a" . eglot-code-actions)
         ("C-c r" . eglot-rename)
         ("C-c l" . eglot-command-map))
  :config
  (defvar-keymap eglot-command-map
    :prefix 'eglot-command-map
    "w q" #'eglot-shutdown
    "w r" #'eglot-reconnect
    "w s" #'eglot
    "w d" #'eglot-show-workspace-configuration
    "= =" #'eglot-format-buffer
    "= r" #'eglot-format
    "g a" #'xref-find-apropos
    "g d" #'eglot-find-declaration
    "g g" #'xref-find-definitions
    "g i" #'eglot-find-implementation
    "g r" #'xref-find-references
    "g t" #'eglot-find-typeDefinition
    "a q" #'eglot-code-action-quickfix
    "a r" #'eglot-code-action-rewrite
    "a i" #'eglot-code-action-inline
    "a e" #'eglot-code-action-extract
    "a o" #'eglot-code-action-organize-imports)
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities '(:documentLinkProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider
                                       :colorProvider
                                       :inlayHintProvider)))

;; -------------------------
;; 8. Misc & Language Init
;; -------------------------

(setq tab-always-indent 'complete)

;; Org-mode specific company setup
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local company-backends
                        ;; 使用双层括号 '((...)) 代表这是一个 Group (分组)
                        ;; 组内的 backend 会并行工作，结果合并显示
                        '((company-files          ; [路径] 输入 / 或 ./ 或 ../ 时触发文件名补全
                           company-yasnippet      ; [Snippet] 补全代码片段
                           company-capf           ; [Org] 原生补全 (比如 #+TITLE, Tags, Links)
                           company-dabbrev)))))   ; [单词] 补全当前 Buffer 里的文字

;; 让 C-c ' 打开的窗口自动启动 LSP
(add-hook 'org-src-mode-hook
          (lambda ()
            ;; 1. 只有特定的语言才启动 LSP (可选，根据你的需要调整)
            ;; (when (member major-mode '(python-mode c++-mode rust-mode))
            ;;   (lsp-deferred))
            
            ;; 或者 2. 只要是编程语言就尝试启动 (推荐)
            (lsp-deferred)))
(with-eval-after-load 'lsp-mode
  ;; 允许 LSP 在 org-src block 中根据当前 org 文件的位置来判断 root
  (setq lsp-auto-guess-root t))


;; Load other language specific configurations
(require 'init-cpp)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-bazel)
(require 'init-haskell)
(require 'init-python)
(require 'init-elisp)
(require 'init-vale)
(require 'init-sh)
(require 'init-lean)
(require 'init-md)
(require 'init-nix)
(require 'init-sage)

(provide 'init-lsp)
;;; init-lsp.el ends here

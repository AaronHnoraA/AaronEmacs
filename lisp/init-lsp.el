;;; init-lsp.el --- The completion engine and lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; Refactored configuration for Company, Eglot (LSP), and Debugging.
;; Switched from lsp-mode to eglot to provide a more lightweight, native experience,
;; while maintaining similar UI features (doc-box, breadcrumbs, diagnostics).

;;; Code:

;; -------------------------
;; 1. Company Mode (Completion)
;; -------------------------
;; [https://company-mode.github.io/manual/](https://company-mode.github.io/manual/)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :init
  (global-company-mode) ;; 全局启用
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         :map company-active-map
         ("C-s"     . company-filter-candidates)
         ([tab]     . company-complete-selection))
  :after yasnippet
  :config
  (define-advice company-capf--candidates (:around (func &rest args))
    "Try default completion styles."
    (let ((completion-styles '(basic partial-completion)))
      (apply func args)))
  
  :custom
  ;; 核心体验设置
  (company-idle-delay 0.05)            ;; 立即触发补全
  (company-minimum-prefix-length 1)    ;; 至少1个字符触发
  (company-show-numbers t)             ;; 显示编号 (M-1, M-2 选择)
  (company-show-quick-access t)        ;; 允许 M-<n> 快速选择
  (company-require-match nil)          ;; 不强制匹配
  
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

  ;; Backends 设置 (Eglot 原生使用 company-capf)
  (company-backends 
        '((company-yasnippet
          company-capf 
          company-files          ; 路径补全
          :with company-tempo 
          )
          (company-dabbrev-code company-keywords)
          company-dabbrev))
  (setq-default company-backends 
        '((company-yasnippet
          company-capf 
          company-files          ; 路径补全
          :with company-tempo 
          )
          (company-dabbrev-code company-keywords)
          company-dabbrev))
  )
 ;; 全局默认 backends


(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'company-backends))
  

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))


;; -------------------------
;; 2. Aggressive Indent
;; -------------------------
(use-package aggressive-indent
  :ensure t
  :hook ((elisp-mode . aggressive-indent-mode)
         (python-mode  . aggressive-indent-mode)
         (c++-mode     . aggressive-indent-mode)
         (c-mode       . aggressive-indent-mode)))


;; -------------------------
;; 3. Flymake (Diagnostics)
;; -------------------------
;; Eglot 默认无缝集成 Flymake
(use-package flymake
  :ensure nil ; Emacs built-in
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c !" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-no-changes-timeout nil) ; 不在输入停顿时自动检查
  (flymake-indicator-type 'fringes))

;; 光标停在报错位置时，在 minibuffer 显示诊断
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-display-diagnostic-function
   #'flymake-diagnostic-at-point-display-minibuffer))


;; -------------------------
;; 4. Eglot (LSP Client)
;; -------------------------
(use-package eglot
  :ensure nil ; Built-in since Emacs 29
  :hook ((prog-mode . eglot-ensure)
         ;; 关闭 Eglot 自带的 Inlay hints (类型推导提示)，保持 UI 干净
         (eglot-managed-mode . (lambda ()
                                 (when (bound-and-true-p eglot-inlay-hints-mode)
                                   (eglot-inlay-hints-mode -1)))))
  :bind (:map eglot-mode-map
         ("C-c f" . eglot-format-buffer)
         ("C-c d" . eldoc-doc-buffer)          ; 在独立 buffer 查看完整文档
         ("C-c a" . eglot-code-actions)
         ("C-c r" . eglot-rename)
         
         ;; Xref 代替 lsp-ui-peek 进行跳转
         ("C-h e" . xref-find-definitions)     ; 定义
         ("C-h r" . xref-find-references)      ; 引用
         ("C-h i" . eglot-find-implementation) ; 实现
         ("C-h t" . eglot-find-typeDefinition))
  :custom
  ;; 性能与功能开关 (对照 lsp-mode 优化)
  (eglot-sync-connect 0)                   ; 异步连接
  (eglot-autoshutdown t)                   ; auto kill server
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)             ; 等效于 lsp-log-io nil，不记录日志提升性能
  
  ;; 提升 jsonrpc 吞吐
  (read-process-output-max (* 1024 1024))) ; 1MB


;; -------------------------
;; 5. UI Emulation (Doc Box & Breadcrumb)
;; -------------------------

;; 替代 lsp-ui-doc：提供光标处悬浮文档框
(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :bind (:map eglot-mode-map
         ("C-h d" . eldoc-box-help-at-point)  ; 快速看文档弹窗
         ("C-h c" . eldoc-box-quit-frame))    ; 隐藏文档
  :custom
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-clear-with-C-g t))

;; 替代 lsp-headerline-breadcrumb：Eglot 作者出品的面包屑
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode 1))

;; 左侧/右侧符号导航 (完全兼容 Eglot)
(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle)
  :custom
  (imenu-list-position 'left)
  (imenu-list-highlight-current-entry t)
  (imenu-list-focus-after-activation nil)
  (imenu-list-size 0.25))


;; -------------------------
;; 6. Dape (Debugging)
;; -------------------------
;;
;; dape 是更适合 Eglot / 原生 Emacs 工作流的 DAP 客户端。
;; 入口命令是 `M-x dape`。
;; 推荐打开 `repeat-mode`，这样单步调试体验会更顺。
;;
;; 说明：
;; - 不再使用 dap-ui-mode / dap-auto-configure-mode
;; - 不再依赖 lsp-mode
;; - 部分 dap-mode 命令在 dape 中没有 1:1 同名接口，
;;   这里改成 dape 当前公开可用的命令体系
;;
(use-package dape
  :ensure t
  :after hydra
  :commands (dape
             dape-next
             dape-step-in
             dape-step-out
             dape-continue
             dape-pause
             dape-restart
             dape-quit
             dape-breakpoint-toggle
             dape-breakpoint-log
             dape-breakpoint-expression
             dape-breakpoint-hits
             dape-breakpoint-remove-at-point
             dape-evaluate-expression
             dape-watch-dwim
             dape-repl
             dape-repl-threads
             dape-repl-stack
             dape-repl-breakpoints
             dape-repl-scope
             dape-repl-watch)
  :hook
  ;; 退出 Emacs 时保存断点；启动后加载断点
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :custom
  ;; 让 dape 的侧边窗口更像 IDE
  (dape-buffer-window-arrangement 'right)
  ;; 如果你不想复用 gud 前缀，可设成 nil
  ;; (dape-key-prefix nil)
  :config
  ;; 推荐：让重复命令（next/step/continue 等）更顺手
  (repeat-mode 1)

  ;; 停住时高亮当前行（可选）
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; 调试开始前自动保存 buffer（对解释型语言尤其有用）
  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t)))

  ;; 可选：编译成功后自动关掉 compile buffer
  ;; (add-hook 'dape-compile-hook #'kill-buffer)

  ;; 兼容你原来的 Hydra 风格
  (defhydra hydra-dape-mode
    (:color pink :hint nil :foreign-keys run)
    "
^Stepping^          ^Switch/View^             ^Breakpoints^         ^Debug^                     ^Eval / Watch^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_n_: Next           _ss_: Sessions(REPL)      _bb_: Toggle          _dd_: Debug (dape)          _ee_: Eval
_i_: Step in        _st_: Threads             _bd_: Delete here     _dr_: Restart               _er_: Eval region
_o_: Step out       _sf_: Stack               _ba_: Log message     _dq_: Quit                  _es_: Eval thing
_c_: Continue       _sl_: Locals(scope)       _bc_: Condition       _dR_: REPL                  _ea_: Add watch
_p_: Pause          _sb_: Breakpoints         _bh_: Hit count
"
    ;; stepping
    ("n" dape-next)
    ("i" dape-step-in)
    ("o" dape-step-out)
    ("c" dape-continue)
    ("p" dape-pause)

    ;; switch / info
    ("ss" dape-repl)
    ("st" dape-repl-threads)
    ("sf" dape-repl-stack)
    ("sl" dape-repl-scope)
    ("sb" dape-repl-breakpoints)

    ;; breakpoints
    ("bb" dape-breakpoint-toggle)
    ("ba" dape-breakpoint-log)
    ("bd" dape-breakpoint-remove-at-point)
    ("bc" dape-breakpoint-expression)
    ("bh" dape-breakpoint-hits)

    ;; debug
    ("dd" dape)
    ("dr" dape-restart)
    ("dR" dape-repl)
    ("dq" dape-quit :color blue)

    ;; eval / watch
    ("ee" dape-evaluate-expression)
    ("ea" dape-watch-dwim)
    ("er" (if (use-region-p)
              (dape-evaluate-expression
               (or (ignore-errors (dape--live-connection 'stopped t))
                   (ignore-errors (dape--live-connection 'last)))
               (buffer-substring-no-properties
                (region-beginning) (region-end)))
            (user-error "No active region")))
    ("es" (let ((sym (thing-at-point 'symbol t)))
            (if sym
                (dape-evaluate-expression
                 (or (ignore-errors (dape--live-connection 'stopped t))
                     (ignore-errors (dape--live-connection 'last)))
                 sym)
              (user-error "No symbol at point"))))

    ("q" nil "quit" :color blue)))



;; -------------------------
;; 7. Misc & Language Init
;; -------------------------

(setq tab-always-indent 'complete)

;; Org-mode specific company setup
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-files          ; [路径] 输入 / 或 ./ 或 ../ 时触发文件名补全
                           company-yasnippet      ; [Snippet] 补全代码片段
                           company-capf           ; [Org / Eglot] 原生补全
                           company-dabbrev)))))   ; [单词] 补全当前 Buffer 里的文字

;; 让 C-c ' 打开的窗口自动启动 Eglot
(add-hook 'org-src-mode-hook
          (lambda ()
            (eglot-ensure)))


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
(require 'init-html)
(require 'init-js2)

;; eglot：永不自动重连（需要你手动 M-x eglot 重新连）
(setq-default eglot-autoreconnect nil)

;; 保存时检查（当前 buffer）
(add-hook 'after-save-hook
          (lambda ()
            (when (bound-and-true-p flymake-mode)
              (flymake-start))))






(provide 'init-lsp)
;;; init-lsp.el ends here


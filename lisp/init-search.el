;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; =========================
;; Ivy
;; =========================
(use-package ivy
  :ensure t
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)

  ;; TRAMP
  (setq tramp-default-method "ssh")
  ;; 明确告诉 TRAMP 去读 ~/.ssh/config 里的 Host 别名
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; 关键：启用 ssh-config 补全（TRAMP 会利用 ssh -G / config 解析）
  (setq tramp-completion-use-auth-sources t)

  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;; 如果你用 posframe，就开；否则可以关掉
  (ivy-posframe-mode 1))

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-alt-done))

;; =========================
;; Ivy Posframe
;; =========================
(use-package ivy-posframe
  :ensure t
  :after ivy
  :config
  (ivy-posframe-mode 1))

;; =========================
;; Counsel / M-x 防套娃
;; =========================
(defun my/safe-counsel-M-x ()
  "Call `counsel-M-x' unless minibuffer is already active."
  (interactive)
  (if (active-minibuffer-window)
      (keyboard-quit)
    (counsel-M-x)))

;; 强制关闭递归 minibuffer，避免 M-x 套娃
(setq enable-recursive-minibuffers nil)

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x"     . my/safe-counsel-M-x)   ;; 确保最终是安全版
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x g"   . counsel-git))
  :config

  ;; ============================================================
  ;; Counsel-find-file 过滤器（默认开）
  ;; - 精确名字黑名单（文件/目录名完全相等）
  ;; - 模式黑名单（后缀/临时文件等：例如 *.~undo-tree~）
  ;; 快捷键：C-c . 切换
  ;; ============================================================

  (defgroup my/counsel-find-file-filter nil
    "My counsel find-file filter."
    :group 'ivy)

  (defcustom my/counsel-find-file-filter-enabled t
    "Non-nil means enable my counsel find-file filter by default."
    :type 'boolean
    :group 'my/counsel-find-file-filter)

  ;; ------------------------------------------------------------
  ;; A) 精确名字黑名单：只匹配“条目名”本身
  ;; ------------------------------------------------------------
  (defconst my/counsel-find-file-ignore-exact-names
    '(
      ;; ---------- macOS ----------
      ".DS_Store" ".localized" "Icon\r"

      ;; ---------- Emacs / 编辑器常见生成物 ----------
      ".use-package-keywords.md"
      ".projectile"
      ".dir-locals-2.el"
      ".cache"

      ;; ---------- Python ----------
      "__pycache__" ".mypy_cache" ".pytest_cache" ".ruff_cache"
      ".coverage" "coverage"

      ;; ---------- Node / frontend ----------
      "node_modules" "dist" "build" "out" ".parcel-cache" ".turbo"

      ;; ---------- Rust/Java/Gradle/IDE ----------
      "target" ".gradle" ".idea" ".vscode" ".settings" ".classpath" ".project"

      ;; ---------- C/C++ / LSP cache ----------
      ".ccls-cache" ".clangd" ".cache-clangd"

      ;; ---------- Virtual env / direnv ----------
      ".venv" "venv" ".envrc"

      ;; ---------- Git / 常见 VCS ----------
      ".git" ".hg" ".svn"
      )
    "Exact entry names to ignore in `counsel-find-file`.")

  (defconst my/counsel-find-file-ignore-exact-names-regexp
    (concat "\\`"
            (regexp-opt my/counsel-find-file-ignore-exact-names 'symbols)
            "\\'")
    "Regexp matching exact entry names to ignore.")

  ;; ------------------------------------------------------------
  ;; B) 模式黑名单：匹配“条目名形态”，尤其是尾缀
  ;;    关键：undo-tree 过滤的是“任何以 .~undo-tree~ 结尾”
  ;; ------------------------------------------------------------
  (defconst my/counsel-find-file-ignore-patterns-regexp
    (concat
     "\\(?:"
     ;; ---- Emacs 备份：foo~ ----
     ".*~\\'"
     "\\|"
     ;; ---- Emacs 自动保存：#foo# ----
     "#.*#\\'"
     "\\|"
     ;; ---- Emacs 锁文件：.#foo ----
     "\\.#[^/]+\\'"
     "\\|"
     ;; ---- Undo-tree：任何以 .~undo-tree~ 结尾（例如 a.md.~undo-tree~）----
     ".*\\.~undo-tree~\\'"
     "\\|"
     ;; ---- Vim swap / undo / backup ----
     ".*\\(?:\\.swp\\|\\.swo\\|\\.swn\\|\\.swx\\)\\'"
     "\\|"
     ".*\\.un~\\'"
     "\\|"
     ".*\\(?:\\.bak\\|\\.tmp\\|\\.temp\\)\\'"
     "\\|"
     ;; ---- 常见系统/工具残留（多为目录名）----
     "\\.Trash\\'"
     ".DS_Store"
     "\\|"
     "\\.DocumentRevisions-V100\\'"
     "\\|"
     "\\.TemporaryItems\\'"
     "\\|"
     "\\.fseventsd\\'"
     "\\|"
     "\\.Spotlight-V100\\'"
     "\\)"
     )
    "Regexp matching common junk entry patterns to ignore.")

  ;; ------------------------------------------------------------
  ;; C) 最终规则：精确 OR 模式
  ;; ------------------------------------------------------------
  (defconst my/counsel-find-file-ignore-regexp
    (concat "\\(?:"
            my/counsel-find-file-ignore-exact-names-regexp
            "\\|"
            my/counsel-find-file-ignore-patterns-regexp
            "\\)")
    "Combined ignore regexp used by counsel find-file.")

  (defun my/counsel-apply-find-file-filter ()
    "Apply or clear `counsel-find-file-ignore-regexp` according to toggle."
    (setq counsel-find-file-ignore-regexp
          (when my/counsel-find-file-filter-enabled
            my/counsel-find-file-ignore-regexp)))

  (defun my/counsel-toggle-find-file-filter ()
    "Toggle my counsel find-file filter (default ON)."
    (interactive)
    (setq my/counsel-find-file-filter-enabled
          (not my/counsel-find-file-filter-enabled))
    (my/counsel-apply-find-file-filter)
    (message "Counsel file filter: %s"
             (if my/counsel-find-file-filter-enabled "ON" "OFF")))

  ;; 默认启用
  (my/counsel-apply-find-file-filter)

  ;; 你要的快捷键：C-c .
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "C-c .")
              #'my/counsel-toggle-find-file-filter))
  ;; ------------------------------------------------------------
  ;; D) recentf 同步过滤（避免 C-x C-r 里出现垃圾）
  ;; ------------------------------------------------------------
  (with-eval-after-load 'recentf
    ;; 1) 路径段中出现精确黑名单名字就排除（目录或文件都行）
    (dolist (name my/counsel-find-file-ignore-exact-names)
      (add-to-list 'recentf-exclude
                   (concat "/"
                           (regexp-quote name)
                           "\\(?:/\\|\\'\\)")))
    ;; 2) 末尾形态命中（尤其 *.~undo-tree~、*~、#*# 等）
    (add-to-list 'recentf-exclude my/counsel-find-file-ignore-patterns-regexp)))

;; =========================
;; Swiper
;; =========================
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config
  (setq swiper-action-recenter t
        swiper-include-line-number-in-search t))

(provide 'init-search)

;;; init-base.el ends here

;;; nix-env.el --- Simple env/toolchain overrides (edit-by-delete) -*- lexical-binding: t; -*-

;; 用法：
;; 1) 你不需要任何开关变量：想要哪个环境，就保留那一段；不想要就把那段删掉/注释掉。
;; 2) 入口只有一个：M-x nix-env-apply
;; 3) 建议配合 direnv：direnv 更新后再跑一次 nix-env-apply，保证工具链优先级。

(defgroup nix-env nil
  "Simple environment overrides (toolchains & languages)."
  :group 'environment)

(defconst nix-env-is-mac (eq system-type 'darwin))
(defconst nix-env-is-linux (eq system-type 'gnu/linux))

;; ============================================================
;; 0. 底层通用函数（别动）
;; ============================================================

(defun nix-env--path-split (s)
  (when (and s (> (length s) 0))
    (split-string s ":" t)))

(defun nix-env--path-join (xs)
  (mapconcat #'identity xs ":"))

(defun nix-env--uniq (xs)
  "Preserve order, remove duplicates."
  (let ((seen (make-hash-table :test 'equal))
        (out  nil))
    (dolist (x xs)
      (unless (gethash x seen)
        (puthash x t seen)
        (push x out)))
    (nreverse out)))

(defun nix-env--remove (xs removals)
  (if (not removals) xs
    (let ((out nil))
      (dolist (x xs)
        (unless (member x removals)
          (push x out)))
      (nreverse out))))

(defun nix-env--prepend-many (prefixes xs)
  (let ((out xs))
    (dolist (p (reverse prefixes))
      (setq out (nix-env--uniq (cons p out))))
    out))

(defun nix-env--append-many (suffixes xs)
  (nix-env--uniq (append xs suffixes)))

(defun nix-env--get-pathlike (var)
  (nix-env--path-split (getenv var)))

(defun nix-env--set-pathlike (var xs)
  (setenv var (nix-env--path-join xs)))

(defun nix-env--ensure-dirs (xs)
  "Filter only existing directories."
  (let (out)
    (dolist (x xs)
      (when (and (stringp x) (file-directory-p x))
        (push x out)))
    (nreverse out)))

(defun nix-env--prepend-to-path (dirs &optional remove-dirs)
  "Prepend DIRS to PATH and exec-path; optionally remove REMOVE-DIRS."
  (setq dirs (nix-env--ensure-dirs dirs))
  (let* ((cur (nix-env--get-pathlike "PATH"))
         (cur (nix-env--remove cur remove-dirs))
         (cur (nix-env--prepend-many dirs cur)))
    (nix-env--set-pathlike "PATH" cur))
  (when remove-dirs
    (dolist (d remove-dirs)
      (setq exec-path (delete d exec-path))))
  (setq exec-path (nix-env--prepend-many dirs exec-path)))

(defun nix-env--prepend-to-envpath (var dirs &optional remove-dirs)
  "Prepend DIRS to a path-like env VAR (e.g. CPATH)."
  (setq dirs (nix-env--ensure-dirs dirs))
  (let* ((cur (nix-env--get-pathlike var))
         (cur (nix-env--remove cur remove-dirs))
         (cur (nix-env--prepend-many dirs cur)))
    (nix-env--set-pathlike var cur)))

(defun nix-env--append-to-envpath (var dirs &optional remove-dirs)
  "Append DIRS to a path-like env VAR."
  (setq dirs (nix-env--ensure-dirs dirs))
  (let* ((cur (nix-env--get-pathlike var))
         (cur (nix-env--remove cur remove-dirs))
         (cur (nix-env--append-many dirs cur)))
    (nix-env--set-pathlike var cur)))

(defun nix-env--append-flag (var s)
  "Append flag string S to env var VAR (space-separated)."
  (when (and (stringp s) (> (length s) 0))
    (let ((cur (getenv var)))
      (setenv var (string-join (seq-filter #'identity (list cur s)) " ")))))

(defun nix-env--prepend-flag (var s)
  "Prepend flag string S to env var VAR (space-separated)."
  (when (and (stringp s) (> (length s) 0))
    (let ((cur (getenv var)))
      (setenv var (string-join (seq-filter #'identity (list s cur)) " ")))))

(defun nix-env--sdkroot ()
  "macOS SDKROOT path, or nil."
  (when nix-env-is-mac
    (let ((s (string-trim (shell-command-to-string "xcrun --show-sdk-path 2>/dev/null"))))
      (unless (string-empty-p s) s))))

;; ============================================================
;; 1. 清理（可选，但推荐保留）
;;    作用：反复 apply 不会“越叠越乱”，direnv 场景很有用
;; ============================================================

(defconst nix-env--known-injected-paths
  '("/opt/homebrew/opt/llvm/bin"
    "/opt/homebrew/opt/llvm/include"
    "/opt/homebrew/opt/llvm/lib"
    "/opt/homebrew/bin"
    "/opt/homebrew/include"
    "/opt/homebrew/lib"
    "/opt/homebrew/opt/python@3.12/libexec/bin"
    "/opt/homebrew/opt/node@20/bin")
  "Paths that nix-env may inject. Used for cleanup.")

(defun nix-env-clean ()
  "Remove known injected paths from PATH/exec-path and common env path-likes."
  (interactive)
  (let ((rm nix-env--known-injected-paths))
    ;; PATH & exec-path
    (let* ((cur (nix-env--get-pathlike "PATH"))
           (cur (nix-env--remove cur rm)))
      (nix-env--set-pathlike "PATH" cur))
    (dolist (d rm) (setq exec-path (delete d exec-path)))

    ;; Typical compiler-related env paths
    (dolist (var '("CPATH" "LIBRARY_PATH" "PKG_CONFIG_PATH"
                   "LD_LIBRARY_PATH" "DYLD_LIBRARY_PATH"))
      (when (getenv var)
        (nix-env--set-pathlike var (nix-env--remove (nix-env--get-pathlike var) rm)))))

  (message "nix-env: cleaned known injected paths"))

;; ============================================================
;; 2. 工具链：Homebrew LLVM（保留就启用；删掉就禁用）
;;    说明：这是“clang/clang++ + SDKROOT + include/lib/pkg-config + rpath”
;;    注意：默认不踢 /usr/bin（更稳），如你想硬隔离，看注释那两行
;; ============================================================

(defun nix-env-apply-toolchain-llvm ()
  "Apply Homebrew LLVM toolchain (macOS oriented)."
  (interactive)
  ;; 如果你坚持要踢系统目录，把下面这一行改成：
  ;; (let ((rm '("/usr/bin" "/usr/local/bin")))
  (let ((rm nil))
    (setenv "CC"  "/opt/homebrew/opt/llvm/bin/clang")
    (setenv "CXX" "/opt/homebrew/opt/llvm/bin/clang++")

    (nix-env--prepend-to-path
     '("/opt/homebrew/opt/llvm/bin" "/opt/homebrew/bin")
     rm)

    (nix-env--prepend-to-envpath "CPATH"
                                 '("/opt/homebrew/opt/llvm/include"
                                   "/opt/homebrew/include"))
    (nix-env--prepend-to-envpath "LIBRARY_PATH"
                                 '("/opt/homebrew/opt/llvm/lib"
                                   "/opt/homebrew/lib"))
    (nix-env--prepend-to-envpath "PKG_CONFIG_PATH"
                                 '("/opt/homebrew/opt/llvm/lib/pkgconfig"
                                   "/opt/homebrew/lib/pkgconfig"
                                   "/opt/homebrew/share/pkgconfig"))

    ;; macOS SDK（找 stdio.h 等系统头文件）
    (let ((sdk (nix-env--sdkroot)))
      (when sdk (setenv "SDKROOT" sdk)))

    ;; flags：用“前置/追加”，避免覆盖项目本身的 flags
    (nix-env--prepend-flag "CPPFLAGS" "-I/opt/homebrew/opt/llvm/include")
    (nix-env--prepend-flag "LDFLAGS"  "-L/opt/homebrew/opt/llvm/lib -Wl,-rpath,/opt/homebrew/opt/llvm/lib")))

;; ============================================================
;; 3. 工具链：Homebrew GCC 15（保留就启用；删掉就禁用）
;;    注意：一般和 LLVM 二选一；你想用哪个，就在 nix-env-apply 里保留哪个调用
;; ============================================================

(defun nix-env-apply-toolchain-gcc15 ()
  "Apply Homebrew GCC 15 toolchain."
  (interactive)
  ;; 同理：如要踢系统目录，把 rm 改成 '("/usr/bin" "/usr/local/bin")
  (let ((rm nil))
    (setenv "CC"  "/opt/homebrew/bin/gcc-15")
    (setenv "CXX" "/opt/homebrew/bin/g++-15")

    (nix-env--prepend-to-path '("/opt/homebrew/bin") rm)

    (nix-env--prepend-to-envpath "CPATH" '("/opt/homebrew/include"))
    (nix-env--prepend-to-envpath "LIBRARY_PATH" '("/opt/homebrew/lib"))
    (nix-env--prepend-to-envpath "PKG_CONFIG_PATH"
                                 '("/opt/homebrew/lib/pkgconfig"
                                   "/opt/homebrew/share/pkgconfig"))))

;; ============================================================
;; 4. 语言环境：Python（保留就启用；删掉就禁用）
;; ============================================================

(defun nix-env-apply-python ()
  "Apply Python-related PATH and env vars."
  (interactive)
  ;; 先 pyenv shims，再 brew python
  (nix-env--prepend-to-path
   (list (expand-file-name "~/.pyenv/shims")
         "/opt/homebrew/opt/python@3.12/libexec/bin"))

  (setenv "PYTHONDONTWRITEBYTECODE" "1")
  (setenv "PYTHONUNBUFFERED" "1"))

;; ============================================================
;; 5. 语言环境：Node.js（保留就启用；删掉就禁用）
;; ============================================================

(defun nix-env-apply-node ()
  "Apply Node.js PATH (Homebrew)."
  (interactive)
  (nix-env--prepend-to-path '("/opt/homebrew/bin/node")))

;; ============================================================
;; 6. 统一入口：你只需要改这里的“调用列表”
;;    想要哪个就保留 (nix-env-apply-xxx)，不想要就删掉那行
;; ============================================================

(defun nix-env-apply ()
  "Apply selected nix-env sections (edit-by-delete)."
  (interactive)
  ;; 推荐：每次先清理一遍（否则反复 apply 可能会叠加不同组合的残留）
  (nix-env-clean)

  ;; --------------------------
  ;; A) C/C++ 工具链（留一个）
  ;; --------------------------

  ;; 方案 1：LLVM（macOS 上更常用）
  ;;(nix-env-apply-toolchain-llvm)

  ;; 方案 2：GCC 15（如你要用 gcc 就用这个，并删掉上面 LLVM 那行）
  (nix-env-apply-toolchain-gcc15)

  ;; --------------------------
  ;; B) 语言环境（想要就留）
  ;; --------------------------
  ;;(nix-env-apply-python)
  (nix-env-apply-node)

  (message "nix-env: applied"))

(provide 'nix-env)
;;; nix-env.el ends here

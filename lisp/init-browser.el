;;; init-snippets.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;
    

;; 共享 Brave 的所有数据（需要关闭 Brave）
(setq xwidget-webkit-cookie-file 
      (expand-file-name "~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Cookies"))
(setq xwidget-webkit-cache-directory 
      (expand-file-name "~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Cache/"))
(setq xwidget-webkit-local-storage-directory 
      (expand-file-name "~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Local Storage/"))

;; === 其他有用的设置 ===
;; 启用 JavaScript
(setq xwidget-webkit-enable-javascript t)

;; 启用插件（如 Flash，虽然现在基本不用了）
(setq xwidget-webkit-enable-plugins t)

;; 启用媒体播放
(setq xwidget-webkit-enable-media t)

;; 设置用户代理（伪装成常规浏览器）
(setq xwidget-webkit-user-agent 
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")



;; 启用开发者工具
(setq xwidget-webkit-enable-developer-extras t)


;; 设置 xwidget-webkit 使用 Brave 浏览器的 cookie
(setq xwidget-webkit-cookie-file 
      (expand-file-name "~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Cookies"))



;;;; xwidget-webkit 基础配置（macOS / emacs-plus with-xwidgets）

(when (featurep 'xwidget-internal)
  ;; xwidget buffer 名字更清晰
  (setq xwidget-webkit-buffer-name-format "*xwidget: %t*")

  ;; 进入 xwidget buffer 时给常用键（不会污染全局）
  (with-eval-after-load 'xwidget
    (define-key xwidget-webkit-mode-map (kbd "q") #'quit-window)
    (define-key xwidget-webkit-mode-map (kbd "g") #'xwidget-webkit-reload)
    (define-key xwidget-webkit-mode-map (kbd "l") #'xwidget-webkit-browse-url)
    (define-key xwidget-webkit-mode-map (kbd "b") #'xwidget-webkit-back)
    (define-key xwidget-webkit-mode-map (kbd "f") #'xwidget-webkit-forward)
    (define-key xwidget-webkit-mode-map (kbd "y") #'xwidget-webkit-copy-selection-as-kill)

    ;; 有些版本提供复制 URL 的命令；若没有也不影响
    (when (fboundp 'xwidget-webkit-current-url)
      (define-key xwidget-webkit-mode-map (kbd "Y")
        (lambda ()
          (interactive)
          (kill-new (xwidget-webkit-current-url))
          (message "Copied URL."))))))

;;;; browse-url 智能分流：优先 EWW，遇到“复杂站点/关键词”走 xwidget

(defun my/url-looks-complex-p (url)
  "Heuristic: 判断 URL 是否可能需要 JS/复杂渲染。"
  (let ((u (downcase url)))
    (or (string-match-p
         (regexp-opt
          '("youtube.com" "bilibili.com" "github.com" "gitlab.com"
            "notion.so" "figma.com" "docs.google.com" "drive.google.com"
            "openai.com" "chatgpt.com" "accounts.google.com"
            "cloudflare" "login" "signin" "oauth" "sso"
            "stripe.com" "paypal.com")
          t)
         u)
        ;; URL 里带很多 query 参数也往往更复杂
        (> (length (or (url-filename (url-generic-parse-url u)) "")) 80))))

(defun my/browse-url (url &optional _new-window)
  "统一入口：简单页用 EWW，复杂页用 xwidget-webkit。"
  (interactive (browse-url-interactive-arg "URL: "))
  (unless (string-match-p "\\`https?://" url)
    (setq url (concat "https://" url)))
  (cond
   ;; 如果没有 xwidgets，就回落到系统浏览器
   ((not (featurep 'xwidget-internal))
    (browse-url-default-browser url))
   ;; 复杂站点：xwidget
   ((my/url-looks-complex-p url)
    (xwidget-webkit-browse-url url))
   ;; 其余：EWW
   (t
    (eww-browse-url url))))

;; 让所有点链接都走这个
(setq browse-url-browser-function #'my/browse-url)


(global-set-key (kbd "C-c w e") #'eww-browse-url)
(global-set-key (kbd "C-c w x") #'xwidget-webkit-browse-url)
(global-set-key (kbd "C-c w w") #'browse-url)


(with-eval-after-load 'eww
  (setq eww-search-prefix "https://duckduckgo.com/?q=")
  (define-key eww-mode-map (kbd "R") #'eww-readable))
;;; init-base.el ends here

;;; eww <-> xwidget-webkit 互转 (稳健版)

;; ---------- 1. 获取 URL 的工具函数 ----------

(defun my/eww-get-url ()
  "获取当前 EWW buffer 的 URL，带空值检查"
  (if (derived-mode-p 'eww-mode)
      (plist-get eww-data :url)
    nil))

(defun my/xwidget-get-url ()
  "获取当前 Xwidget buffer 的 URL，带空值检查"
  (if (eq major-mode 'xwidget-webkit-mode)
      (xwidget-webkit-uri (xwidget-webkit-current-session))
    nil))

;; ---------- 2. 核心切换逻辑 (带延迟清理) ----------

(defun my/eww-to-xwidget ()
  "从 EWW 切换到 Xwidget，成功后延迟清理旧 Buffer"
  (interactive)
  (let ((url (my/eww-get-url))
        (old-buf (current-buffer)))
    (if url
        (progn
          (message "正在切换至 Xwidget: %s" url)
          ;; 启动 xwidget
          (xwidget-webkit-browse-url url)
          ;; 【关键修正】：不要立即杀 buffer。
          ;; 使用 run-at-time 0 让 Emacs 先完成 buffer 切换和界面重绘，
          ;; 待事件循环空闲时再回头杀掉旧 buffer。
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 EWW URL"))))

(defun my/xwidget-to-eww ()
  "从 Xwidget 切换到 EWW，成功后延迟清理旧 Buffer"
  (interactive)
  (let ((url (my/xwidget-get-url))
        (old-buf (current-buffer)))
    (if url
        (progn
          (message "正在切换至 EWW: %s" url)
          ;; 启动 eww (eww 通常会在当前窗口复用 buffer)
          (eww url)
          ;; 【关键修正】：同样延迟清理，防止 xwidget 还没隐藏就被杀掉导致闪退
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 Xwidget URL"))))


;; 1. EWW 切换到 EAF 浏览器
(defun my/eww-to-eaf ()
  "从 EWW 切换到 EAF 浏览器，成功后延迟清理旧 Buffer"
  (interactive)
  (let ((url (eww-current-url))
        (old-buf (current-buffer)))
    (if url
        (progn
          (message "正在切换至 EAF: %s" url)
          ;; 启动 EAF 浏览器
          (eaf-open-browser url)
          ;; 延迟清理旧 buffer，防止显示问题
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 EWW URL"))))

;; 2. Xwidget 切换到 EAF 浏览器
(defun my/xwidget-to-eaf ()
  "从 Xwidget 切换到 EAF 浏览器，成功后延迟清理旧 Buffer"
  (interactive)
  (let ((url (xwidget-webkit-uri (xwidget-webkit-current-session)))
        (old-buf (current-buffer)))
    (if url
        (progn
          (message "正在切换至 EAF: %s" url)
          ;; 启动 EAF 浏览器
          (eaf-open-browser url)
          ;; 延迟清理旧 buffer
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 Xwidget URL"))))

;; 3. EAF 浏览器切换到 EWW
(defun my/eaf-to-eww ()
  "从 EAF 浏览器切换到 EWW，成功后延迟清理旧 Buffer"
  (interactive)
  (let ((url (if (derived-mode-p 'eaf-mode)
                 (eaf-call-sync "execute_function" eaf--buffer-id "get_url")
               nil))
        (old-buf (current-buffer)))
    (if url
        (progn
          (message "正在切换至 EWW: %s" url)
          ;; 启动 eww
          (eww url)
          ;; 延迟清理旧 buffer
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 EAF URL"))))

;; 4. EAF 浏览器切换到 Xwidget
(defun my/eaf-to-xwidget ()
  "从 EAF 浏览器切换到 Xwidget，成功后延迟清理旧 Buffer"
  (interactive)
  (let ((url (if (derived-mode-p 'eaf-mode)
                 (eaf-call-sync "execute_function" eaf--buffer-id "get_url")
               nil))
        (old-buf (current-buffer)))
    (if url
        (progn
          (message "正在切换至 Xwidget: %s" url)
          ;; 启动 xwidget-webkit
          (xwidget-webkit-browse-url url)
          ;; 延迟清理旧 buffer
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 EAF URL"))))




(defun eaf-open-search (search-term &optional engine browser)
  "Search SEARCH-TERM with selected search ENGINE and BROWSER."
  (interactive
   (list
    (read-string "Search: ")
    (intern (completing-read "Search Engine (default: bing): "
                            '("bing" "perplexity")
                            nil t nil nil "bing"))
    (intern (completing-read "Browser (default: xwidget): "
                            '("xwidget" "eaf")
                            nil t nil nil "xwidget"))))
  (require 'url-util)
  (let* ((encoded-query (url-hexify-string search-term))
         (search-url
          (pcase engine
            ('bing (format "https://www.bing.com/search?q=%s" encoded-query))
            ('perplexity (format "https://www.perplexity.ai/search?q=%s" encoded-query))
            (_ (format "https://www.bing.com/search?q=%s" encoded-query)))))
    (pcase browser
      ('xwidget (xwidget-webkit-browse-url search-url))
      ('eaf (eaf-open-browser search-url))
      (_ (xwidget-webkit-browse-url search-url)))))




(provide 'init-browser)


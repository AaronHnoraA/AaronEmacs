;;; init-browser.el --- Browser integration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;

(require 'general)
(require 'init-open)

(declare-function appine-open-url "appine" (url))
(declare-function my/appine-get-url "init-appine" ())
(declare-function my/appine-kill-all "init-appine" ())
(declare-function my/appine-back "init-appine" ())
(declare-function my/appine-close-tab "init-appine" ())
(declare-function my/appine-forward "init-appine" ())
(declare-function my/appine-next-tab "init-appine" ())
(declare-function my/appine-open-url "init-appine" (url))
(declare-function my/appine-open-file "init-appine" (path))
(declare-function my/appine-open-at-point "init-appine" ())
(declare-function my/appine-prev-tab "init-appine" ())
(declare-function my/appine-reload "init-appine" ())
(declare-function my/macos-open-url "init-macos" (url))
(declare-function my/open-normalize-backend "init-open" (backend))
(declare-function my/open-eww-url "init-open" (url &optional reuse-selected))
(declare-function my/open-read-backend "init-open" (kind &optional prompt default))
(declare-function my/open-resolve-backend "init-open" (kind &optional backend))
(declare-function my/open-url "init-open" (url &optional backend))
(declare-function my/open-url-with-backend "init-open" (url backend &optional reuse-selected))
(declare-function my/open-xwidget-url "init-open" (url &optional reuse-selected))
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
    (define-key xwidget-webkit-mode-map (kbd "M-r") #'my/refresh-current-content)
    (define-key xwidget-webkit-mode-map (kbd "l") #'my/open-xwidget-url)
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

;;;; browse-url 统一入口：默认策略由 init-open.el 维护

(defun my/browse-url (url &optional _new-window)
  "Open URL with the central URL route."
  (interactive (browse-url-interactive-arg "URL: "))
  (my/open-url url))

;; 让所有点链接都走这个
(setq browse-url-browser-function #'my/browse-url)


(general-define-key
 :keymaps 'global
 "C-c w e" #'my/open-eww-url
 "C-c w x" #'my/open-xwidget-url
 "C-c w a" #'my/appine-open-url
 "C-c w f" #'my/appine-open-file
 "C-c w g" #'my/appine-open-at-point
 "C-c w h" #'my/appine-back
 "C-c w l" #'my/appine-forward
 "C-c w [" #'my/appine-prev-tab
 "C-c w ]" #'my/appine-next-tab
 "C-c w 0" #'my/appine-close-tab
 "C-c w d" #'my/browser-close-current
 "C-c w ?" #'my/appine-board
 "C-c w w" #'browse-url
 "C-c w s" #'my/browser-switch-to
 "C-c w E" #'my/browser-switch-to-eww
 "C-c w X" #'my/browser-switch-to-xwidget
 "C-c w A" #'my/browser-switch-to-appine
 "C-c w O" #'my/browser-switch-to-open
 "C-c w k" #'my/appine-kill-all)


(with-eval-after-load 'eww
  (setq eww-search-prefix "https://duckduckgo.com/?q=")
  (define-key eww-mode-map (kbd "g") #'my/refresh-current-content)
  (define-key eww-mode-map (kbd "M-r") #'my/refresh-current-content)
  (define-key eww-mode-map (kbd "M-w") #'my/browser-close-current)
  (define-key eww-mode-map (kbd "R") #'eww-readable)
  (define-key eww-mode-map (kbd "X") #'my/eww-to-xwidget)
  (define-key eww-mode-map (kbd "A") #'my/eww-to-appine))
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

(defun my/browser-build-search-url (search-term engine)
  "Return a search URL for SEARCH-TERM on ENGINE."
  (require 'url-util)
  (let ((encoded-query (url-hexify-string search-term)))
    (pcase engine
      ('bing (format "https://www.bing.com/search?q=%s" encoded-query))
      ('perplexity (format "https://www.perplexity.ai/search?q=%s" encoded-query))
      ('duckduckgo (format "https://duckduckgo.com/?q=%s" encoded-query))
      (_ (format "https://www.bing.com/search?q=%s" encoded-query)))))

(defun my/browser-current-backend ()
  "Return the current browser backend symbol, or nil."
  (cond
   ((derived-mode-p 'eww-mode) 'eww)
   ((eq major-mode 'xwidget-webkit-mode) 'xwidget)
   ((and (fboundp 'my/appine-get-url)
         (ignore-errors (my/appine-get-url)))
    'appine)
   (t nil)))

(defun my/browser-current-url ()
  "Return the current page URL for the active browser backend."
  (pcase (my/browser-current-backend)
    ('eww (my/eww-get-url))
    ('xwidget (my/xwidget-get-url))
    ('appine (and (fboundp 'my/appine-get-url)
                  (my/appine-get-url)))
    (_ nil)))

(defun my/browser--kill-buffer-and-window ()
  "Kill the current browser buffer and delete its window when possible."
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (let ((kill-buffer-query-functions nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (when (and (window-live-p window)
               (> (length (window-list nil 'no-minibuf)) 1))
      (delete-window window))))

(defun my/browser-close-current ()
  "Close the current browser view or tab.
For EWW and xwidget-webkit, this kills the browser buffer and deletes its
window, avoiding orphan browser buffers/windows."
  (interactive)
  (pcase (my/browser-current-backend)
    ('appine
     (if (fboundp 'my/appine-close-tab)
         (my/appine-close-tab)
       (user-error "Appine close command is not available")))
    ('eww
     (my/browser--kill-buffer-and-window))
    ('xwidget
     (my/browser--kill-buffer-and-window))
    (_
     (user-error "当前 buffer 不是受支持的浏览后端"))))

(defun my/browser-open-url-with-backend (backend url &optional reuse-selected)
  "Open URL with browser BACKEND."
  (my/open-url-with-backend url backend reuse-selected))

(defun my/browser-cleanup-backend (backend buffer)
  "Clean up BACKEND using BUFFER after a successful switch."
  (pcase backend
    ('appine
     (when (fboundp 'my/appine-kill-all)
       (my/appine-kill-all)))
    (_
     (when (buffer-live-p buffer)
       (kill-buffer buffer)))))

(defun my/refresh-current-content ()
  "Refresh the current web view or file buffer."
  (interactive)
  (cond
   ((eq (my/browser-current-backend) 'appine)
    (call-interactively #'my/appine-reload))
   ((derived-mode-p 'eww-mode)
    (call-interactively #'eww-reload))
   ((eq major-mode 'xwidget-webkit-mode)
    (call-interactively #'xwidget-webkit-reload))
   ((or (buffer-file-name) (derived-mode-p 'dired-mode))
    (if (buffer-modified-p)
        (user-error "当前 buffer 有未保存修改，先保存再刷新")
      (revert-buffer :ignore-auto :noconfirm :preserve-modes)
      (message "已刷新: %s" (buffer-name))))
   (t
    (user-error "当前 buffer 不支持刷新"))))

(general-define-key
 :keymaps 'global
 "C-c w r" #'my/refresh-current-content)

(defun my/browser-switch-to (backend)
  "Switch the current browser page to BACKEND."
  (interactive
   (list (my/open-read-backend 'url "Switch browser to: ")))
  (let ((backend (my/open-normalize-backend backend))
        (source-backend (my/browser-current-backend))
        (url (my/browser-current-url))
        (old-buf (current-buffer)))
    (unless source-backend
      (user-error "当前 buffer 不是受支持的浏览后端"))
    (unless url
      (user-error "无法获取当前页面 URL"))
    (when (eq source-backend backend)
      (user-error "当前已经是 %s" backend))
    (message "正在切换至 %s: %s" backend url)
    (my/browser-open-url-with-backend backend url t)
    (run-at-time "0 sec" nil #'my/browser-cleanup-backend source-backend old-buf)))

(defun my/browser-switch-to-eww ()
  "Switch the current browser page to EWW."
  (interactive)
  (my/browser-switch-to 'eww))

(defun my/browser-switch-to-xwidget ()
  "Switch the current browser page to xwidget-webkit."
  (interactive)
  (my/browser-switch-to 'xwidget))

(defun my/browser-switch-to-appine ()
  "Switch the current browser page to Appine."
  (interactive)
  (my/browser-switch-to 'appine))

(defun my/browser-switch-to-open ()
  "Open the current browser page with macOS open."
  (interactive)
  (my/browser-switch-to 'system))

(defun my/eww-to-appine ()
  "Switch the current EWW page to Appine."
  (interactive)
  (my/browser-switch-to 'appine))

(defun my/xwidget-to-appine ()
  "Switch the current xwidget page to Appine."
  (interactive)
  (my/browser-switch-to 'appine))

(defun my/appine-to-eww ()
  "Switch the current Appine page to EWW."
  (interactive)
  (my/browser-switch-to 'eww))

(defun my/appine-to-xwidget ()
  "Switch the current Appine page to xwidget-webkit."
  (interactive)
  (my/browser-switch-to 'xwidget))

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
          (my/open-xwidget-url url t)
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
          ;; 启动 eww
          (my/open-eww-url url t)
          ;; 【关键修正】：同样延迟清理，防止 xwidget 还没隐藏就被杀掉导致闪退
          (run-at-time "0 sec" nil 
                       (lambda (b) 
                         (when (buffer-live-p b)
                           (kill-buffer b))) 
                       old-buf))
      (message "错误：无法获取 Xwidget URL"))))


(defun my/browser-open-search (search-term &optional engine browser)
  "Search SEARCH-TERM with selected search ENGINE and BROWSER."
  (interactive
   (list
    (read-string "Search: ")
     (intern (completing-read "Search Engine (default: bing): "
                            '("bing" "perplexity" "duckduckgo")
                            nil t nil nil "bing"))
     (my/open-read-backend 'search "Browser: ")))
  (my/browser-open-url-with-backend
   (or browser (my/open-resolve-backend 'search))
   (my/browser-build-search-url search-term engine)))

(with-eval-after-load 'xwidget
  (define-key xwidget-webkit-mode-map (kbd "M-w") #'my/browser-close-current)
  (define-key xwidget-webkit-mode-map (kbd "W") #'my/xwidget-to-eww)
  (define-key xwidget-webkit-mode-map (kbd "A") #'my/xwidget-to-appine))




(provide 'init-browser)

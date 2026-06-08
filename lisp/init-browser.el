;;; init-browser.el --- Browser integration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;;

(require 'general)
(require 'cl-lib)
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
(declare-function my/open--with-browser-window "init-open" (mode reuse-selected open-fn))
(declare-function my/open-normalize-url "init-open" (url))

;;;; xwidget API

(defvar my/xwidget--sessions (make-hash-table :test #'equal)
  "Stable xwidget session id to buffer map.")

(defvar my/xwidget--session-counter 0
  "Counter used for anonymous xwidget session ids.")

(defvar-local my/xwidget-session-url nil
  "Last URL recorded for this xwidget buffer.")

(defcustom my/xwidget-auto-focus-on-load t
  "Auto-focus xwidget buffer when its page finishes loading."
  :type 'boolean
  :group 'xwidget)

(defvar-local my/xwidget-focus-script nil
  "Optional JS to focus an editable element inside this xwidget buffer.
Set buffer-locally before the page loads (e.g. in the open function).")
(put 'my/xwidget-focus-script 'permanent-local t)

(defun my/xwidget-focus (&optional buffer)
  "Focus the xwidget in BUFFER (default: current buffer) for keyboard input.
Enables edit-mode, runs any buffer-local JS focus script, and switches
to evil insert state if evil is active in the buffer."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (and (buffer-live-p buf)
               (with-current-buffer buf
                 (eq major-mode 'xwidget-webkit-mode)))
      (with-current-buffer buf
        (when (fboundp 'xwidget-webkit-edit-mode)
          (ignore-errors (xwidget-webkit-edit-mode 1)))
        (when-let* ((session (and (fboundp 'xwidget-webkit-current-session)
                                  (ignore-errors (xwidget-webkit-current-session)))))
          (when (and my/xwidget-focus-script
                     (fboundp 'xwidget-webkit-execute-script))
            (ignore-errors
              (xwidget-webkit-execute-script session my/xwidget-focus-script))))
))))

(defun my/xwidget--load-finished-focus (xwidget _xwidget-event-type)
  "Schedule focus for XWIDGET's buffer after page load-finished."
  ;; Capture the load-finished detail immediately (last-input-event is ephemeral).
  (when (and my/xwidget-auto-focus-on-load
             (eq _xwidget-event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-finished"))
    (let ((buf (and (fboundp 'xwidget-buffer)
                    (ignore-errors (xwidget-buffer xwidget)))))
      (when (and (buffer-live-p buf)
                 (or (eq buf (current-buffer))
                     (get-buffer-window buf)))
        ;; Delay 0.3 s to let WebKit finish rendering before injecting focus.
        (run-at-time 0.3 nil #'my/xwidget-focus buf)))))

(defun my/xwidget--ensure-available ()
  "Ensure native xwidget-webkit primitives are available."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (require 'xwidget))
  (unless (fboundp 'xwidget-webkit-browse-url)
    (user-error "xwidget-webkit is not available in this Emacs")))

(defun my/xwidget-session-buffer (id)
  "Return live xwidget buffer for session ID, or nil."
  (let ((buffer (and id (gethash id my/xwidget--sessions))))
    (when (buffer-live-p buffer)
      buffer)))

(defun my/xwidget-current-url (&optional buffer)
  "Return current URL for xwidget BUFFER, defaulting to current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (or my/xwidget-session-url
            (and (eq major-mode 'xwidget-webkit-mode)
                 (fboundp 'xwidget-webkit-current-session)
                 (fboundp 'xwidget-webkit-uri)
                 (ignore-errors
                   (xwidget-webkit-uri (xwidget-webkit-current-session)))))))))

(defun my/xwidget--record-buffer (buffer id url)
  "Record BUFFER as xwidget session ID with URL."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local my/xwidget-session-url url))
    (when id
      (puthash id buffer my/xwidget--sessions))))

(defun my/xwidget--display-buffer (buffer display)
  "Display BUFFER according to DISPLAY."
  (pcase display
    ('none nil)
    ('current (switch-to-buffer buffer))
    (_ (pop-to-buffer buffer))))

(cl-defun my/xwidget-open-url (url &key id (display 'side) force-new reuse-selected)
  "Open URL through the customized xwidget API.
ID identifies a stable session.  DISPLAY may be `side', `current' or `none'.
When FORCE-NEW is non-nil, replace the old buffer for ID."
  (interactive
   (list (read-string "xwidget URL: ")
         :display 'side))
  (my/xwidget--ensure-available)
  (let* ((url (my/open-normalize-url url))
         (id (or id (format "xwidget-%d" (cl-incf my/xwidget--session-counter))))
         (existing (my/xwidget-session-buffer id)))
    (when (and existing force-new)
      (remhash id my/xwidget--sessions)
      (kill-buffer existing)
      (setq existing nil))
    (if existing
        (progn
          (if (and (fboundp 'xwidget-webkit-current-session)
                   (fboundp 'xwidget-webkit-goto-uri))
              (with-current-buffer existing
                (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url))
            (user-error "xwidget-webkit-goto-uri is not available"))
          (my/xwidget--record-buffer existing id url)
          (my/xwidget--display-buffer existing display)
          ;; Page already loaded — no load-finished will fire; schedule focus.
          (run-at-time 0.3 nil #'my/xwidget-focus existing)
          existing)
      (let ((buffer
             (if (eq display 'side)
                 (my/open--with-browser-window
                  'xwidget-webkit-mode reuse-selected
                  (lambda ()
                    (xwidget-webkit-browse-url url t)
                    (current-buffer)))
               (xwidget-webkit-browse-url url t)
               (current-buffer))))
        (my/xwidget--record-buffer buffer id url)
        (my/xwidget--display-buffer buffer display)
        buffer))))

(defun my/xwidget-open-url-current (url)
  "Open URL in xwidget using the selected window."
  (interactive (browse-url-interactive-arg "xwidget URL: "))
  (my/xwidget-open-url url :display 'current :reuse-selected t))

(defun my/xwidget-reload ()
  "Reload the current xwidget page through the customized API."
  (interactive)
  (if (fboundp 'xwidget-webkit-reload)
      (call-interactively #'xwidget-webkit-reload)
    (user-error "xwidget reload is not available")))

(defun my/xwidget-back ()
  "Navigate the current xwidget page backward."
  (interactive)
  (if (fboundp 'xwidget-webkit-back)
      (call-interactively #'xwidget-webkit-back)
    (user-error "xwidget back is not available")))

(defun my/xwidget-forward ()
  "Navigate the current xwidget page forward."
  (interactive)
  (if (fboundp 'xwidget-webkit-forward)
      (call-interactively #'xwidget-webkit-forward)
    (user-error "xwidget forward is not available")))

(defun my/xwidget-copy-selection ()
  "Copy selection from the current xwidget page."
  (interactive)
  (if (fboundp 'xwidget-webkit-copy-selection-as-kill)
      (call-interactively #'xwidget-webkit-copy-selection-as-kill)
    (user-error "xwidget copy selection is not available")))

(defun my/xwidget-copy-url ()
  "Copy current xwidget URL."
  (interactive)
  (if-let* ((url (my/xwidget-current-url)))
      (progn
        (kill-new url)
        (message "Copied URL."))
    (user-error "No xwidget URL available")))

(defun my/xwidget-keep-emacs-prefix-keys (map)
  "Remove xwidget bindings that should remain normal Emacs keys in MAP."
  (dolist (key '("M-x" "C-x C-f" "C-x" "C-c" "C-s" "C-g" "M-w" "M-q"))
    (define-key map (kbd key) nil)))

(defun my/xwidget-pass-editing-keys (map)
  "Send editing/navigation keys in MAP to WebKit instead of Emacs."
  (when (fboundp 'xwidget-webkit-pass-command-event)
    (dolist (key '("<escape>"
                   "<delete>"
                   "<backspace>"
                   "DEL"
                   "RET"
                   "<return>"
                   "TAB"
                   "<tab>"
                   "<backtab>"
                   "<iso-lefttab>"
                   "S-TAB"
                   "S-<tab>"
                   "<left>"
                   "<right>"
                   "<up>"
                   "<down>"
                   "<home>"
                   "<end>"
                   "<prior>"
                   "<next>"
                   ;; macOS: Cmd = Meta; pass clipboard shortcuts to WebKit
                   "M-c"
                   "M-v"))
      (define-key map (kbd key) #'xwidget-webkit-pass-command-event))))

(defun my/xwidget-pass-pointer-keys (map)
  "Send pointer wheel events in MAP to WebKit instead of Emacs scrolling."
  (when (fboundp 'xwidget-webkit-pass-command-event)
    (dolist (key '([wheel-up]
                   [wheel-down]
                   [wheel-left]
                   [wheel-right]
                   [double-wheel-up]
                   [double-wheel-down]
                   [double-wheel-left]
                   [double-wheel-right]
                   [triple-wheel-up]
                   [triple-wheel-down]
                   [triple-wheel-left]
                   [triple-wheel-right]
                   [mouse-4]
                   [mouse-5]))
      (define-key map key #'xwidget-webkit-pass-command-event))))

(defun my/xwidget--split-to-ibuffer (split-fn)
  "Run SPLIT-FN, select the new window, and show `ibuffer'."
  (let ((window (funcall split-fn)))
    (select-window window)
    (ibuffer)))

(defun my/xwidget-split-window-below-ibuffer ()
  "Split below from xwidget and show `ibuffer' in the new window."
  (interactive)
  (my/xwidget--split-to-ibuffer #'split-window-below))

(defun my/xwidget-split-window-right-ibuffer ()
  "Split right from xwidget and show `ibuffer' in the new window."
  (interactive)
  (my/xwidget--split-to-ibuffer #'split-window-right))

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



;;;; xwidget header-line navigation bar

(defun my/xwidget--nav-button (label action help)
  "Return a propertized header-line button for LABEL, ACTION, and HELP."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
      (lambda () (interactive) (funcall action)))
    (propertize (concat " " label " ")
                'mouse-face 'mode-line-highlight
                'help-echo help
                'local-map map)))

(defun my/xwidget-setup-header-line ()
  "Install back/forward/reload nav buttons in this xwidget buffer's header line."
  (setq-local header-line-format
              (list
               (my/xwidget--nav-button "◀" #'my/xwidget-back   "Back [b]")
               (my/xwidget--nav-button "▶" #'my/xwidget-forward "Forward [f]")
               (my/xwidget--nav-button "↺" #'my/xwidget-reload  "Reload [g]")
               "  "
               '(:eval (propertize (or (my/xwidget-current-url) "")
                                   'face 'shadow)))))

;;;; xwidget-webkit 基础配置（macOS / emacs-plus with-xwidgets）

(when (featurep 'xwidget-internal)
  ;; Keep this free of %-escapes: supported xwidget format keys vary across
  ;; Emacs builds, and unsupported keys raise `format-spec' errors.
  (setq xwidget-webkit-buffer-name-format "*xwidget*")

  ;; 进入 xwidget buffer 时给常用键（不会污染全局）
  (with-eval-after-load 'xwidget
    (my/xwidget-keep-emacs-prefix-keys xwidget-webkit-mode-map)
    (my/xwidget-keep-emacs-prefix-keys xwidget-webkit-edit-mode-map)
    (my/xwidget-pass-editing-keys xwidget-webkit-mode-map)
    (my/xwidget-pass-editing-keys xwidget-webkit-edit-mode-map)
    (my/xwidget-pass-pointer-keys xwidget-webkit-mode-map)
    (my/xwidget-pass-pointer-keys xwidget-webkit-edit-mode-map)
    (define-key xwidget-webkit-mode-map [remap split-window-below] #'my/xwidget-split-window-below-ibuffer)
    (define-key xwidget-webkit-mode-map [remap split-window-right] #'my/xwidget-split-window-right-ibuffer)
    (define-key xwidget-webkit-mode-map (kbd "q") #'quit-window)
    (define-key xwidget-webkit-mode-map (kbd "g") #'my/xwidget-reload)
    (define-key xwidget-webkit-mode-map (kbd "M-r") #'my/refresh-current-content)
    (define-key xwidget-webkit-mode-map (kbd "l") #'my/xwidget-open-url-current)
    (define-key xwidget-webkit-mode-map (kbd "b") #'my/xwidget-back)
    (define-key xwidget-webkit-mode-map (kbd "f") #'my/xwidget-forward)
    (define-key xwidget-webkit-mode-map (kbd "y") #'my/xwidget-copy-selection)
    (define-key xwidget-webkit-mode-map (kbd "Y") #'my/xwidget-copy-url)
    (define-key xwidget-webkit-mode-map (kbd "i") #'my/xwidget-focus)
    ;; Drive auto-focus from the load-finished event via callback advice.
    (advice-add 'xwidget-webkit-callback :after #'my/xwidget--load-finished-focus)))

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
 "C-c w x" #'my/xwidget-open-url
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
      (my/xwidget-current-url)
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
    (call-interactively #'my/xwidget-reload))
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
          (my/xwidget-open-url url :display 'current :reuse-selected t)
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
  (define-key xwidget-webkit-mode-map (kbd "A") #'my/xwidget-to-appine)
  (add-hook 'xwidget-webkit-mode-hook #'my/xwidget-setup-header-line))




(provide 'init-browser)

;;; init-appine.el --- Appine integration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Native macOS browser / document embedding via Appine.
;; The package itself stays lazy; the first interactive command will load it
;; and Appine will then prompt to download or compile its dynamic module.

;;; Code:

(my/package-ensure-vc 'appine "https://github.com/chaoswork/appine.git")

(defconst my/appine-buffer-name "*Appine Window*"
  "Buffer name used by Appine.")

(defconst my/appine-board-buffer-name "*Appine Board*"
  "Buffer name for the Appine management board.")

(defvar my/appine-last-url nil
  "URL of the currently active Appine tab (derived from `my/appine-tab-list').")

(defvar my/appine-tab-list nil
  "Ordered list of normalized URLs for each open Appine tab.")

(defvar my/appine-tab-index -1
  "Zero-based index into `my/appine-tab-list' for the active tab. -1 = none.")

(defvar my/appine-default-window-size 0.38
  "Default height ratio for the Appine window when splitting below.")

(defvar my/appine--refresh-timer nil
  "Timer used to refresh Appine after Emacs window changes settle.")

(declare-function appine--buffer "appine")
(declare-function appine--get-active-window-for-buffer "appine" (buffer-name))
(declare-function appine--sync-active-state "appine" (&rest args))
(declare-function appine--window-pixel-rect "appine" (win))
(declare-function appine--update-active-keymap "appine")
(declare-function appine-focus "appine")
(declare-function appine-refresh "appine")
(declare-function appine-open-file-by-file-chooser "appine")
(declare-function appine-native-action "appine" (name))
(declare-function my/macos-open-target "init-macos" (target))

(defun my/appine-current-file ()
  "Return the current Appine local file path, or nil for non-file tabs."
  (when (stringp my/appine-last-url)
    (cond
     ((string-prefix-p "file://" my/appine-last-url)
      (substring my/appine-last-url (length "file://")))
     ((string-prefix-p "/" my/appine-last-url)
      my/appine-last-url)
     (t nil))))

(defun my/appine-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is the Appine host buffer."
  (eq (get-buffer (or buffer (current-buffer)))
      (get-buffer my/appine-buffer-name)))

(defun my/appine-visible-p ()
  "Return non-nil when the Appine buffer is visible."
  (and (get-buffer my/appine-buffer-name)
       (get-buffer-window my/appine-buffer-name 'visible)))

(defun my/appine-active-p ()
  "Return non-nil when Appine is active in the selected window."
  (and (boundp 'appine--active)
       appine--active
       (my/appine-buffer-p)))

(defun my/appine--normalize-url (url)
  "Normalize URL before handing it to Appine."
  (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
      url
    (concat "https://" url)))

;;; ── Tab registry ─────────────────────────────────────────────────────────

(defun my/appine--tab-current-url ()
  "Return the tracked URL for the active tab, or nil."
  (and my/appine-tab-list
       (>= my/appine-tab-index 0)
       (< my/appine-tab-index (length my/appine-tab-list))
       (nth my/appine-tab-index my/appine-tab-list)))

(defun my/appine--tab-push (url)
  "Record URL as a newly opened tab (appended at end, becomes active)."
  (setq my/appine-tab-list (append my/appine-tab-list (list url))
        my/appine-tab-index (1- (length my/appine-tab-list))
        my/appine-last-url url))

(defun my/appine--tab-remove-current ()
  "Remove the active tab from the registry; focus the adjacent one."
  (when (and my/appine-tab-list (>= my/appine-tab-index 0)
             (< my/appine-tab-index (length my/appine-tab-list)))
    (setq my/appine-tab-list
          (append (seq-take my/appine-tab-list my/appine-tab-index)
                  (seq-drop my/appine-tab-list (1+ my/appine-tab-index))))
    (if my/appine-tab-list
        (setq my/appine-tab-index
              (max 0 (min my/appine-tab-index (1- (length my/appine-tab-list))))
              my/appine-last-url (my/appine--tab-current-url))
      (my/appine--tab-reset))))

(defun my/appine--tab-next ()
  "Update registry after switching to the next tab."
  (when my/appine-tab-list
    (setq my/appine-tab-index
          (mod (1+ my/appine-tab-index) (length my/appine-tab-list))
          my/appine-last-url (my/appine--tab-current-url))))

(defun my/appine--tab-prev ()
  "Update registry after switching to the previous tab."
  (when my/appine-tab-list
    (setq my/appine-tab-index
          (mod (1- my/appine-tab-index) (length my/appine-tab-list))
          my/appine-last-url (my/appine--tab-current-url))))

(defun my/appine--tab-reset ()
  "Clear the entire tab registry."
  (setq my/appine-tab-list nil
        my/appine-tab-index -1
        my/appine-last-url nil))

;;; ── Advisors ─────────────────────────────────────────────────────────────

(defun my/appine--track-open-url-a (orig-fn url &rest args)
  "Call ORIG-FN then push URL into the tab registry."
  ;; appine-open-file calls appine-open-url internally, so all opens
  ;; (both file:// and http/https) converge here exactly once.
  (prog1 (apply orig-fn url args)
    (my/appine--tab-push (my/appine--normalize-url url))))

(defun my/appine--track-next-tab-a (orig-fn &rest args)
  "Call ORIG-FN then advance the tab-registry index."
  (prog1 (apply orig-fn args) (my/appine--tab-next)))

(defun my/appine--track-prev-tab-a (orig-fn &rest args)
  "Call ORIG-FN then retreat the tab-registry index."
  (prog1 (apply orig-fn args) (my/appine--tab-prev)))

(defun my/appine--track-close-tab-a (orig-fn &rest args)
  "Call ORIG-FN, update the registry, and kill Appine after the last tab."
  (let ((last-tab-p (and my/appine-tab-list
                         (= (length my/appine-tab-list) 1))))
    (prog1 (apply orig-fn args)
      (when my/appine-tab-list
        (my/appine--tab-remove-current))
      (when last-tab-p
        (my/appine--kill-host-after-last-tab)))))

(defun my/appine--url-open-target (url)
  "Return URL as a target suitable for macOS open."
  (when (stringp url)
    (if (string-prefix-p "file://" url)
        (substring url (length "file://"))
      url)))

(defun my/appine--macos-open (target)
  "Open TARGET using macOS open."
  (if (fboundp 'my/macos-open-target)
      (my/macos-open-target target)
    (unless (executable-find "open")
      (user-error "Cannot find macOS open command"))
    (start-process "macos-open" nil "open" target)
    (message "open: %s" target)))

(defun my/appine--kill-host-after-last-tab ()
  "Kill the Appine host buffer after its final tracked tab closes."
  (my/appine--tab-reset)
  (when (fboundp 'appine-kill)
    (appine-kill))
  (message "Appine: closed last tab and killed host buffer."))

(defun my/appine-open-url (url)
  "Open URL in Appine (tab registry is updated via advice on `appine-open-url')."
  (interactive "sURL: ")
  (appine-open-url (my/appine--normalize-url url)))

(defun my/appine-open-at-point ()
  "Open the URL at point in Appine."
  (interactive)
  (let ((url (or (thing-at-point 'url t)
                 (read-string "URL: "))))
    (unless (and url (not (string-empty-p url)))
      (user-error "当前位置没有可用 URL"))
    (my/appine-open-url url)))

(defun my/appine-open-file (path)
  "Open PATH in Appine (tab registry is updated via advice on `appine-open-url')."
  (interactive "fFile: ")
  (appine-open-file path))

(defun my/appine--open-current-file ()
  "Open the remembered Appine file in Emacs."
  (interactive)
  (if-let* ((file (my/appine-current-file)))
      (find-file file)
    (user-error "当前 Appine tab 不是文件")))

(defun my/appine-open-current-with-macos ()
  "Open the current Appine file or URL with macOS open."
  (interactive)
  (if-let* ((target (or (my/appine-current-file)
                        (my/appine--url-open-target my/appine-last-url))))
      (my/appine--macos-open target)
    (user-error "当前 Appine tab 没有可 open 的文件或链接")))

(defun my/appine-get-url ()
  "Return the remembered Appine URL when the Appine view is active or visible."
  (when (or (my/appine-buffer-p) (my/appine-visible-p))
    my/appine-last-url))

(defun my/appine-kill-all ()
  "Kill the Appine view and clear all tab state."
  (interactive)
  (let ((count (if (get-buffer my/appine-buffer-name) 1 0)))
    (my/appine--cancel-refresh-timer)
    (my/appine--tab-reset)
    (when (fboundp 'appine-kill)
      (appine-kill))
    (message "已清理 %d 个 Appine Buffer/Window。" count)))

(defun my/appine-restart ()
  "Restart Appine after clearing old native views."
  (interactive)
  (my/appine-kill-all)
  (require 'appine nil t)
  (message "Appine restarted."))

(defun my/appine-back ()
  "Navigate Appine backward."
  (interactive)
  (call-interactively #'appine-web-go-back))

(defun my/appine-forward ()
  "Navigate Appine forward."
  (interactive)
  (call-interactively #'appine-web-go-forward))

(defun my/appine-reload ()
  "Reload the current Appine web view."
  (interactive)
  (call-interactively #'appine-web-reload))

(defun my/appine-url-type (&optional url)
  "Return the type of URL as a symbol: `file', `web', or `unknown'."
  (let ((u (or url my/appine-last-url)))
    (cond
     ((not (stringp u))          'unknown)
     ((string-prefix-p "file://" u) 'file)
     ((string-prefix-p "/" u)       'file)
     ((string-match-p "\\`https?://" u) 'web)
     (t                              'web))))   ; treat any other scheme as web

(defun my/appine-next-tab ()
  "Switch to the next Appine tab (registry updated via advice)."
  (interactive)
  (appine-next-tab))

(defun my/appine-prev-tab ()
  "Switch to the previous Appine tab (registry updated via advice)."
  (interactive)
  (appine-prev-tab))

(defun my/appine-close-tab ()
  "Close the current Appine tab (registry updated via advice)."
  (interactive)
  (appine-close-tab))

(defun my/appine-toggle-org-links ()
  "Toggle Appine integration for Org links, then refresh the board."
  (interactive)
  (call-interactively #'appine-toggle-use-for-org-links)
  (when (get-buffer my/appine-board-buffer-name)
    (with-current-buffer my/appine-board-buffer-name
      (my/appine-board-refresh))))

;;; ── Board UI helpers ────────────────────────────────────────────────────

(define-derived-mode my/appine-board-mode special-mode "Appine-Board"
  "Major mode for the Appine management board.")

(defun my/appine-board--insert-button (label action help)
  "Insert a text button with LABEL, ACTION (symbol or lambda), and HELP.
The action is wrapped so it always receives a single ignored button arg,
then calls the function interactively (prompting for any arguments)."
  (let ((wrapped (if (and (symbolp action) (commandp action))
                     (lambda (_b) (call-interactively action))
                   ;; Lambda passed directly — must already accept one arg.
                   action)))
    (insert-text-button label 'action wrapped 'follow-link t 'help-echo help)))

(defun my/appine-board--insert-openable-path (path &optional dired-p)
  "Insert PATH as clickable Emacs and macOS-open buttons.
With DIRED-P, the main path button opens via `dired'."
  (if (and path (stringp path))
      (let ((expanded (expand-file-name path)))
        (my/appine-board--insert-button
         (abbreviate-file-name expanded)
         (if dired-p
             (lambda (_b) (dired path))
           (lambda (_b) (find-file path)))
         (if dired-p "Open directory in Emacs" "Open this file in Emacs"))
        (insert " ")
        (my/appine-board--insert-button
         "[open]"
         (lambda (_b) (my/appine--macos-open expanded))
         "Open with macOS open"))
    (insert "<unset>")))

(defun my/appine-board--insert-openable-url (url)
  "Insert URL as clickable Appine and macOS-open buttons."
  (if (and url (stringp url))
      (progn
        (my/appine-board--insert-button
         url
         (lambda (_b) (my/appine-open-url url))
         "Open URL in Appine")
        (insert " ")
        (my/appine-board--insert-button
         "[open]"
         (lambda (_b) (my/appine--macos-open (my/appine--url-open-target url)))
         "Open with macOS open"))
    (insert "<none>")))

(defun my/appine-board--insert-value-line (label value)
  "Insert LABEL and VALUE as a formatted line."
  (insert (format "%-24s %s\n" label value)))

(defun my/appine-board--insert-path-line (label path &optional dired-p)
  "Insert LABEL and PATH as a button line."
  (insert (format "%-24s " label))
  (my/appine-board--insert-openable-path path dired-p)
  (insert "\n"))

(defun my/appine-board--insert-url-line (label url)
  "Insert LABEL and URL as a button line."
  (insert (format "%-24s " label))
  (my/appine-board--insert-openable-url url)
  (insert "\n"))

(defun my/appine-board--insert-tabs-line ()
  "Insert the Appine tab registry with macOS-open buttons."
  (insert (format "%-24s " "Tab registry"))
  (insert (format "%d/%d  "
                  (1+ my/appine-tab-index)
                  (length my/appine-tab-list)))
  (let ((i 0))
    (dolist (url my/appine-tab-list)
      (when (> i 0)
        (insert " | "))
      (let ((label (if (eq (my/appine-url-type url) 'file)
                       (file-name-nondirectory
                        (my/appine--url-open-target url))
                     (replace-regexp-in-string "\\`https?://" "" url)))
            (target (my/appine--url-open-target url)))
        (my/appine-board--insert-button
         (if (= i my/appine-tab-index)
             (format "[%s]" label)
           label)
         (lambda (_b) (my/appine--macos-open target))
         "Open with macOS open"))
      (setq i (1+ i))))
  (insert "\n"))

(defun my/appine-board--insert-action-line (pairs)
  "Insert a row of action buttons from PAIRS of (label fn help)."
  (dolist (pair pairs)
    (pcase-let ((`(,label ,fn ,help) pair))
      (my/appine-board--insert-button label fn help)
      (insert "  ")))
  (insert "\n"))

(defun my/appine-board--section (title)
  "Insert a bold section TITLE with an underline."
  (insert (format "\n%s\n%s\n" title (make-string (length title) ?-))))

(defun my/appine-board--hint-row (&rest pairs)
  "Insert a row of KEY DESC hints from PAIRS."
  (dolist (pair pairs)
    (insert (format "  %-6s %-18s" (car pair) (cadr pair))))
  (insert "\n"))

;;; ── Tool functions (scroll, find, plugins, JS) ──────────────────────────

(defun my/appine-new-tab ()
  "Open a new tab in Appine (appine always opens google.com for new tabs)."
  (interactive)
  (when (fboundp 'appine-new-tab)
    (appine-new-tab)
    ;; appine-new-tab bypasses appine-open-url, so push manually.
    (my/appine--tab-push "https://www.google.com")))

(defun my/appine-find ()
  "Open the in-page find bar."
  (interactive)
  (appine-native-action "find"))

(defun my/appine-scroll-top ()
  "Scroll Appine view to the top."
  (interactive)
  (appine-native-action "scrollToTop"))

(defun my/appine-scroll-bottom ()
  "Scroll Appine view to the bottom."
  (interactive)
  (appine-native-action "scrollToBottom"))

(defun my/appine-scroll-page-down ()
  "Scroll Appine view one page down."
  (interactive)
  (appine-native-action "scrollPageDown"))

(defun my/appine-scroll-page-up ()
  "Scroll Appine view one page up."
  (interactive)
  (appine-native-action "scrollPageUp"))

;;; Plugin directories:
;;;   User plugins live in etc/appine/plugins/ (version-controlled with user config).
;;;   Appine's native loader reads from {appine-root-dir}/browser-extension/plugins/.
;;;   We bridge the two with per-plugin symlinks inside the appine plugins dir,
;;;   so git pull never disturbs user plugins (symlinks are untracked).

(defconst my/appine-user-plugins-dir
  (expand-file-name "etc/appine/plugins" user-emacs-directory)
  "Local directory for user-managed Appine plugins.")

(defun my/appine--plugins-dir ()
  "Return the appine native plugins directory (where native code loads from)."
  (when (and (boundp 'appine-root-dir) appine-root-dir)
    (expand-file-name "browser-extension/plugins" appine-root-dir)))

(defun my/appine--ensure-user-plugins-dir ()
  "Create the user plugins dir and migrate any existing user plugins.
Any entry in the native plugins dir that is NOT one of appine's built-in
plugins (link-hints, selection-assistant) is moved to the user dir and
replaced with a symlink."
  (make-directory my/appine-user-plugins-dir t)
  (when-let* ((native-dir (my/appine--plugins-dir))
              (_ (file-directory-p native-dir)))
    (dolist (entry (directory-files native-dir nil "\\`[^.]"))
      (let ((src (expand-file-name entry native-dir))
            (dst (expand-file-name entry my/appine-user-plugins-dir)))
        (when (and (file-directory-p src)
                   (not (file-symlink-p src))
                   (not (member entry '("link-hints" "selection-assistant"))))
          (unless (file-exists-p dst)
            (copy-directory src dst t t t))
          (delete-directory src t)
          (make-symbolic-link dst src)
          (message "Appine: migrated plugin '%s' to %s" entry my/appine-user-plugins-dir))))))

(defun my/appine--user-plugin-link (name)
  "Ensure symlink NAME in native plugins dir → user plugins dir.
Returns the user plugin directory path."
  (my/appine--ensure-user-plugins-dir)
  (let* ((user-dir  (expand-file-name name my/appine-user-plugins-dir))
         (link      (expand-file-name name (my/appine--plugins-dir))))
    (make-directory user-dir t)
    (unless (file-symlink-p link)
      (when (file-exists-p link) (delete-directory link t))
      (make-symbolic-link user-dir link))
    user-dir))

(defun my/appine--list-plugins ()
  "Return a list of user-managed plugin names."
  (when (file-directory-p my/appine-user-plugins-dir)
    (seq-filter
     (lambda (f) (and (not (string-prefix-p "." f))
                      (file-directory-p
                       (expand-file-name f my/appine-user-plugins-dir))))
     (directory-files my/appine-user-plugins-dir nil nil t))))

(defun my/appine-open-plugins-dir ()
  "Open the user Appine plugins directory in Dired."
  (interactive)
  (dired my/appine-user-plugins-dir))

(defun my/appine-create-plugin (name js-code)
  "Create a new user Appine plugin NAME with JS-CODE.
The plugin is stored in `my/appine-user-plugins-dir' and symlinked
into the appine native plugins directory so it survives git pull."
  (interactive
   (list (read-string "Plugin name (no spaces): ")
         (read-string "JS snippet (will be wrapped in a plugin): ")))
  (let* ((user-dir  (my/appine--user-plugin-link name))
         (index-file (expand-file-name "index.js" user-dir)))
    (with-temp-file index-file
      (insert (format "// Appine plugin: %s\nexport default {\n  name: '%s',\n  setup(api) {\n    api.onLoad(() => {\n      %s\n    });\n  }\n};\n"
                      name name js-code)))
    (message "Plugin '%s' → %s" name user-dir)))

(defun my/appine-run-js-once (js-code)
  "Inject JS-CODE as a one-shot plugin (stored as _oneshot), then reload."
  (interactive "sJS code to run on next page load: ")
  (my/appine-create-plugin "_oneshot" js-code))

(defun my/appine-delete-plugin (name)
  "Delete user plugin NAME from both user dir and the native symlink."
  (interactive
   (list (completing-read "Delete plugin: " (my/appine--list-plugins) nil t)))
  (when (yes-or-no-p (format "Delete plugin '%s'? " name))
    (let ((link (expand-file-name name (or (my/appine--plugins-dir) "")))
          (dir  (expand-file-name name my/appine-user-plugins-dir)))
      (when (file-symlink-p link) (delete-file link))
      (when (file-directory-p dir) (delete-directory dir t)))
    (message "Plugin '%s' deleted." name)))

(defun my/appine-open-plugin (name)
  "Open a user plugin's index.js for editing."
  (interactive
   (list (completing-read "Open plugin: " (my/appine--list-plugins) nil t)))
  (find-file (expand-file-name (concat name "/index.js")
                               my/appine-user-plugins-dir)))

(defun my/appine-clear-cache ()
  "Delete the browser cache directory and confirm."
  (interactive)
  (if-let* ((dir (and (boundp 'xwidget-webkit-cache-directory)
                      xwidget-webkit-cache-directory))
            (_ (file-directory-p dir)))
      (when (yes-or-no-p (format "Delete cache at %s? " dir))
        (delete-directory dir t)
        (message "Cache cleared."))
    (user-error "Cache directory is not configured or doesn't exist")))

(defun my/appine-clear-cookies ()
  "Delete the browser cookie file."
  (interactive)
  (if-let* ((file (and (boundp 'xwidget-webkit-cookie-file)
                       xwidget-webkit-cookie-file))
            (_ (file-exists-p file)))
      (when (yes-or-no-p (format "Delete cookie file %s? " file))
        (delete-file file)
        (message "Cookies cleared."))
    (user-error "Cookie file is not configured or doesn't exist")))

;;; ── Brave cookie import ─────────────────────────────────────────────────

(defconst my/appine-brave-cookie-db
  (expand-file-name
   "~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Cookies")
  "Path to Brave Browser's SQLite cookie database.")

(defconst my/appine--brave-decrypt-py
  ;; Inline Python that decrypts Brave/Chrome AES-128-CBC cookie values.
  ;; Usage: python3 -c '<script>' <host> [<name>]
  "
import sys, sqlite3, subprocess, hashlib, base64, struct, os

db_path = os.path.expanduser(
    '~/Library/Application Support/BraveSoftware/Brave-Browser/Default/Cookies')

# 1. Get the master key from Keychain.
try:
    pw = subprocess.check_output(
        ['security', 'find-generic-password', '-wa', 'Brave Safe Storage'],
        stderr=subprocess.DEVNULL).strip()
except Exception:
    try:
        pw = subprocess.check_output(
            ['security', 'find-generic-password', '-wa', 'Brave'],
            stderr=subprocess.DEVNULL).strip()
    except Exception as e:
        sys.exit(f'Keychain error: {e}')

# 2. Derive AES key (Chrome/Brave macOS scheme).
key = hashlib.pbkdf2_hmac('sha1', pw, b'saltysalt', 1003, dklen=16)
iv  = b' ' * 16

# 3. Query cookies.
host = sys.argv[1] if len(sys.argv) > 1 else ''
name_filter = sys.argv[2] if len(sys.argv) > 2 else ''

try:
    con = sqlite3.connect(f'file:{db_path}?mode=ro', uri=True)
except Exception as e:
    sys.exit(f'DB error: {e}')

rows = con.execute(
    'SELECT host_key, name, encrypted_value, path, expires_utc, '
    '       is_secure, is_httponly, samesite '
    'FROM cookies WHERE host_key LIKE ? AND name LIKE ?',
    (f'%{host}%', f'%{name_filter}%' if name_filter else '%')
).fetchall()

# 4. Decrypt (AES-128-CBC, PKCS7 unpadding).
try:
    from Crypto.Cipher import AES
    def decrypt(blob):
        if not blob or blob[:3] != b'v10': return None
        c = AES.new(key, AES.MODE_CBC, iv)
        d = c.decrypt(blob[3:])
        return d[:-d[-1]].decode('utf-8', errors='replace')
except ImportError:
    # Fallback: use openssl via subprocess.
    import tempfile
    def decrypt(blob):
        if not blob or blob[:3] != b'v10': return None
        with tempfile.NamedTemporaryFile(delete=False, suffix='.bin') as f:
            f.write(blob[3:]); fname = f.name
        try:
            r = subprocess.run(
                ['openssl', 'enc', '-aes-128-cbc', '-d', '-nosalt',
                 '-K', key.hex(), '-iv', iv.hex(),
                 '-in', fname, '-nopad'],
                capture_output=True)
            raw = r.stdout
            return raw[:-raw[-1]].decode('utf-8', errors='replace') if raw else None
        finally:
            os.unlink(fname)

results = []
for host_key, name, enc_val, path, exp_utc, secure, httponly, ss in rows:
    value = decrypt(enc_val)
    if value is None: continue
    # Convert Chrome epoch (microseconds since 1601) to Unix epoch.
    expires = int((exp_utc / 1_000_000) - 11644473600) if exp_utc else 0
    results.append((host_key, name, value, path, expires, secure, httponly, ss))

# 5. Output as JSON.
import json
print(json.dumps([
    {'host': h, 'name': n, 'value': v, 'path': p,
     'expires': e, 'secure': bool(s), 'httponly': bool(ho), 'samesite': ss}
    for h, n, v, p, e, s, ho, ss in results
]))
"
  "Inline Python script for decrypting Brave cookies.")

(defun my/appine--brave-cookies-for-host (host)
  "Return a list of decrypted Brave cookies for HOST as alists, or nil."
  (unless (file-exists-p my/appine-brave-cookie-db)
    (user-error "Brave cookie DB not found at %s" my/appine-brave-cookie-db))
  (let* ((json-str
          (with-temp-buffer
            (let ((rc (call-process
                       "python3" nil t nil "-c"
                       my/appine--brave-decrypt-py host)))
              (unless (zerop rc)
                (user-error "Cookie decrypt failed: %s" (buffer-string))))
            (buffer-string)))
         (cookies (condition-case err
                      (json-parse-string json-str :object-type 'alist
                                                   :array-type 'list)
                    (error (user-error "JSON parse error: %s" err)))))
    cookies))

(defun my/appine--cookies-to-js (cookies)
  "Return a JS string that sets COOKIES via document.cookie."
  (mapconcat
   (lambda (c)
     (let* ((name    (alist-get 'name    c))
            (value   (alist-get 'value   c))
            (path    (or (alist-get 'path c) "/"))
            (expires (alist-get 'expires c))
            (secure  (alist-get 'secure  c))
            (host    (alist-get 'host    c))
            (domain  (if (string-prefix-p "." host) host
                       (concat "." host)))
            (exp-str (when (and expires (> expires 0))
                       (format-time-string
                        "; expires=%a, %d %b %Y %T GMT"
                        (seconds-to-time expires) t)))
            (sec-str (if secure "; Secure" "")))
       (format "document.cookie=%s;"
               (json-encode
                (concat name "=" value
                        "; domain=" domain
                        "; path=" path
                        (or exp-str "")
                        sec-str)))))
   ;; HttpOnly cookies cannot be set via JS — skip them.
   (seq-remove (lambda (c) (eq (alist-get 'httponly c) t)) cookies)
   "\n"))

(defun my/appine-import-brave-cookies (&optional host)
  "Import non-HttpOnly Brave cookies for HOST into Appine via a JS plugin.
HOST defaults to the domain of the current tab URL.
The plugin is written to _brave-cookies/index.js and activated on next reload.

Note: HttpOnly cookies (auth tokens, session cookies) cannot be imported
via JavaScript and are silently skipped.  For sites that require login,
you will need to log in again inside Appine."
  (interactive
   (list (read-string
          "从 Brave 导入 cookies，输入域名 (如 github.com): "
          (when (stringp my/appine-last-url)
            (ignore-errors
              (url-host (url-generic-parse-url my/appine-last-url)))))))
  (unless (and host (not (string-empty-p host)))
    (user-error "No host specified"))
  (message "Reading Brave cookies for %s …" host)
  (let* ((cookies (my/appine--brave-cookies-for-host host))
         (injectable (seq-remove (lambda (c) (eq (alist-get 'httponly c) t))
                                 cookies))
         (total    (length cookies))
         (n        (length injectable)))
    (if (zerop n)
        (message "No injectable cookies for %s (%d total, all HttpOnly or empty)."
                 host total)
      (let* ((js (my/appine--cookies-to-js injectable))
             (plugin-name "_brave-cookies")
             (plugin-dir  (my/appine--user-plugin-link plugin-name))
             (index-file  (expand-file-name "index.js" plugin-dir)))
        (with-temp-file index-file
          (insert (format
                   "// Auto-generated by my/appine-import-brave-cookies
// Host: %s  Cookies: %d/%d injectable (HttpOnly skipped)
export default {
  name: '_brave-cookies',
  setup(api) {
    api.onLoad(() => {
%s
    });
  }
};
"
                   host n total
                   (mapconcat (lambda (line) (concat "      " line))
                              (split-string js "\n") "\n"))))
        (message "Wrote %d/%d cookies for %s → %s"
                 n total host index-file)
        (message "Brave cookies plugin written. Reopen the target page if you need it to take effect.")))))

;;; ── Board ───────────────────────────────────────────────────────────────

(defun my/appine-board-refresh ()
  "Refresh the Appine management board."
  (interactive)
  (let* ((inhibit-read-only t)
         (url      my/appine-last-url)
         (url-type (my/appine-url-type url))
         (file     (my/appine-current-file))
         (alive    (get-buffer my/appine-buffer-name))
         (plugins  (my/appine--list-plugins)))
    (erase-buffer)

    ;; ── Header ──────────────────────────────────────────────────────────
    (insert "Appine Board\n")
    (insert "============\n")

    ;; ── Status ──────────────────────────────────────────────────────────
    (my/appine-board--section "Status")
    (my/appine-board--insert-value-line
     "Buffer"   (if alive my/appine-buffer-name "not created"))
    (my/appine-board--insert-value-line
     "Visible"  (if (my/appine-visible-p) "yes" "no"))
    (my/appine-board--insert-value-line
     "Active"   (if (my/appine-active-p)  "yes" "no"))
    (my/appine-board--insert-value-line
     "Org links" (if (and (boundp 'appine-use-for-org-links)
                          appine-use-for-org-links)
                     "enabled" "disabled"))
    (my/appine-board--insert-value-line
     "Window size" (format "%.0f%% of frame"
                           (* 100 my/appine-default-window-size)))
    (my/appine-board--insert-path-line
     "Appine root" (and (boundp 'appine-root-dir) appine-root-dir) t)

    ;; ── Current Tab ─────────────────────────────────────────────────────
    (my/appine-board--section "Current Tab")
    (my/appine-board--insert-value-line
     "Tab type" (pcase url-type
                  ('file "File  (PDF / Quick Look)")
                  ('web  "Web   (http/https)")
                  (_     "Unknown")))
    (my/appine-board--insert-url-line "URL" url)
    (when (eq url-type 'file)
      (my/appine-board--insert-path-line "File path" file))
    (when my/appine-tab-list
      (my/appine-board--insert-tabs-line))

    ;; ── Navigation ──────────────────────────────────────────────────────
    (my/appine-board--section "Navigation")
    (my/appine-board--insert-action-line
     '(("[H] back"       my/appine-back        "Go back")
       ("[L] forward"    my/appine-forward     "Go forward")))
    (my/appine-board--insert-action-line
     '(("[n] new tab"    my/appine-new-tab     "Open a new tab")
       ("[[]] prev/next" my/appine-prev-tab    "Prev tab (] for next)")
       ("[d] close tab"  my/appine-close-tab   "Close current tab")))

    ;; ── Open ────────────────────────────────────────────────────────────
    (my/appine-board--section "Open")
    (my/appine-board--insert-action-line
     '(("[a] focus/open"  my/appine-focus-or-open      "Focus or create Appine")
       ("[u] open URL"    my/appine-open-url            "Open URL in new tab")
       ("[f] open file"   my/appine-open-file           "Open local file in new tab")
       ("[o] edit source" my/appine--open-current-file  "Open current file in Emacs")
       ("mac open"        my/appine-open-current-with-macos "Open current file/URL with macOS open")))

    ;; ── Scroll / Find ───────────────────────────────────────────────────
    (my/appine-board--section "Scroll / Find")
    (my/appine-board--insert-action-line
     '(("[^] top"      my/appine-scroll-top       "Scroll to top")
       ("[v] bottom"   my/appine-scroll-bottom    "Scroll to bottom")
       ("[J] pg-down"  my/appine-scroll-page-down "Scroll page down")
       ("[K] pg-up"    my/appine-scroll-page-up   "Scroll page up")))
    (my/appine-board--insert-action-line
     '(("[/] find"     my/appine-find "Open in-page find bar")))

    ;; ── JS / Plugins ────────────────────────────────────────────────────
    (my/appine-board--section "JS Injection / Plugins")
    (insert "User plugins: ")
    (my/appine-board--insert-openable-path my/appine-user-plugins-dir t)
    (insert "\n")
    (insert "Native dir:   ")
    (my/appine-board--insert-openable-path (my/appine--plugins-dir) t)
    (insert " (symlinks → user plugins)\n")
    (if plugins
        (progn
          (insert (format "Installed (%d): %s\n"
                          (length plugins) (string-join plugins ", ")))
          (my/appine-board--insert-action-line
           '(("[pe] edit plugin"   my/appine-open-plugin   "Open plugin index.js")
             ("[px] del plugin"    my/appine-delete-plugin "Delete a plugin"))))
      (insert "No plugins installed.\n"))
    (my/appine-board--insert-action-line
     '(("[pc] create plugin" my/appine-create-plugin  "Create a new JS plugin")
       ("[pj] run JS once"   my/appine-run-js-once    "Inject one-shot JS (reloads page)")))
    (insert "(Plugins are injected at page load.)\n")

    ;; ── Browser Storage ─────────────────────────────────────────────────
    (my/appine-board--section "Browser Storage")
    (my/appine-board--insert-path-line
     "Cookie file"
     (and (boundp 'xwidget-webkit-cookie-file) xwidget-webkit-cookie-file))
    (my/appine-board--insert-path-line
     "Cache dir"
     (and (boundp 'xwidget-webkit-cache-directory) xwidget-webkit-cache-directory) t)
    (my/appine-board--insert-path-line
     "Local storage"
     (and (boundp 'xwidget-webkit-local-storage-directory)
          xwidget-webkit-local-storage-directory) t)
    (my/appine-board--insert-action-line
     '(("[c] open cookies"  my/appine-open-cookie-file            "View cookie file")
       ("[C] open cache"    my/appine-open-cache-directory        "Open cache dir")
       ("[S] open storage"  my/appine-open-local-storage-directory "Open local storage")))
    (my/appine-board--insert-action-line
     '(("[xc] clear cookies" my/appine-clear-cookies "Delete cookie file")
       ("[xC] clear cache"   my/appine-clear-cache   "Delete cache directory")))

    ;; ── Brave Cookies ───────────────────────────────────────────────────
    (my/appine-board--section "Brave Cookie Import")
    (insert (format "Brave DB: %s\n"
                    (if (file-exists-p my/appine-brave-cookie-db)
                        (concat (abbreviate-file-name my/appine-brave-cookie-db)
                                "  (found)")
                      "not found")))
    (insert "Non-HttpOnly cookies only (auth/session cookies need manual login).\n")
    (my/appine-board--insert-action-line
     '(("[bi] import for host" my/appine-import-brave-cookies
        "Decrypt & inject Brave cookies for a domain (then reload)")))

    ;; ── Manage ──────────────────────────────────────────────────────────
    (my/appine-board--section "Manage")
    (my/appine-board--insert-action-line
     '(("[t] toggle org links" my/appine-toggle-org-links "Toggle Appine for Org links")
       ("[k] kill all"         my/appine-kill-all          "Kill all views & reset state")
       ("[D] docs"             my/appine-open-docs         "Open Appine README")))

    ;; ── Key Reference ───────────────────────────────────────────────────
    (my/appine-board--section "Keys")
    (my/appine-board--hint-row '("g"  "refresh board") '("a"  "focus/open")
                               '("u"  "open URL")      '("f"  "open file"))
    (my/appine-board--hint-row '("n"  "new tab")       '("d"  "close tab")
                               '("k"  "kill all"))
    (my/appine-board--hint-row '("H/L" "back/fwd")     '("[/]" "prev/next tab")
                               '("o"  "edit src")      '("/"  "find"))
    (my/appine-board--hint-row '("^"  "scroll top")    '("v"  "scroll btm")
                               '("J"  "page down")     '("K"  "page up"))
    (my/appine-board--hint-row '("pc" "new plugin")    '("pj" "run JS once")
                               '("pe" "edit plugin")   '("px" "del plugin"))
    (my/appine-board--hint-row '("c"  "cookies")       '("C"  "cache")
                               '("S"  "storage")       '("xc/xC" "clear"))
    (my/appine-board--hint-row '("bi" "brave cookies") '("t"  "org links")
                               '("D"  "docs")          '("q"  "quit"))
    (insert "\n")
    (goto-char (point-min))))

(defun my/appine-board ()
  "Open the Appine management board."
  (interactive)
  (let ((buffer (get-buffer-create my/appine-board-buffer-name)))
    (with-current-buffer buffer
      (my/appine-board-mode)
      (let ((map (copy-keymap special-mode-map)))
        (use-local-map map)
        (define-key map (kbd "g")   #'my/appine-board-refresh)
        (define-key map (kbd "a")   #'my/appine-focus-or-open)
        (define-key map (kbd "u")   #'my/appine-open-url)
        (define-key map (kbd "f")   #'my/appine-open-file)
        (define-key map (kbd "n")   #'my/appine-new-tab)
        (define-key map (kbd "d")   #'my/appine-close-tab)
        (define-key map (kbd "k")   #'my/appine-kill-all)
        (define-key map (kbd "H")   #'my/appine-back)
        (define-key map (kbd "L")   #'my/appine-forward)
        (define-key map (kbd "[")   #'my/appine-prev-tab)
        (define-key map (kbd "]")   #'my/appine-next-tab)
        (define-key map (kbd "o")   #'my/appine--open-current-file)
        (define-key map (kbd "/")   #'my/appine-find)
        (define-key map (kbd "^")   #'my/appine-scroll-top)
        (define-key map (kbd "v")   #'my/appine-scroll-bottom)
        (define-key map (kbd "J")   #'my/appine-scroll-page-down)
        (define-key map (kbd "K")   #'my/appine-scroll-page-up)
        (define-key map (kbd "p")
          (let ((pm (make-sparse-keymap)))
            (define-key pm (kbd "c") #'my/appine-create-plugin)
            (define-key pm (kbd "j") #'my/appine-run-js-once)
            (define-key pm (kbd "e") #'my/appine-open-plugin)
            (define-key pm (kbd "x") #'my/appine-delete-plugin)
            pm))
        (define-key map (kbd "x")
          (let ((xm (make-sparse-keymap)))
            (define-key xm (kbd "c") #'my/appine-clear-cookies)
            (define-key xm (kbd "C") #'my/appine-clear-cache)
            xm))
        (define-key map (kbd "t")   #'my/appine-toggle-org-links)
        (define-key map (kbd "c")   #'my/appine-open-cookie-file)
        (define-key map (kbd "C")   #'my/appine-open-cache-directory)
        (define-key map (kbd "S")   #'my/appine-open-local-storage-directory)
        (define-key map (kbd "D")   #'my/appine-open-docs)
        (define-key map (kbd "b")
          (let ((bm (make-sparse-keymap)))
            (define-key bm (kbd "i") #'my/appine-import-brave-cookies)
            bm)))
      (my/appine-board-refresh))
    (pop-to-buffer buffer)))

(defun my/appine-focus-or-open ()
  "Focus Appine if visible, otherwise open it."
  (interactive)
  (if (get-buffer my/appine-buffer-name)
      (appine-focus)
    (call-interactively #'appine)))

(defun my/appine-open-cookie-file ()
  "Open the configured cookie file."
  (interactive)
  (if (and (boundp 'xwidget-webkit-cookie-file)
           xwidget-webkit-cookie-file)
      (find-file xwidget-webkit-cookie-file)
    (user-error "Cookie file is not configured")))

(defun my/appine-open-cache-directory ()
  "Open the configured browser cache directory."
  (interactive)
  (if (and (boundp 'xwidget-webkit-cache-directory)
           xwidget-webkit-cache-directory)
      (dired xwidget-webkit-cache-directory)
    (user-error "Cache directory is not configured")))

(defun my/appine-open-local-storage-directory ()
  "Open the configured local storage directory."
  (interactive)
  (if (and (boundp 'xwidget-webkit-local-storage-directory)
           xwidget-webkit-local-storage-directory)
      (dired xwidget-webkit-local-storage-directory)
    (user-error "Local storage directory is not configured")))

(defun my/appine-open-docs ()
  "Open the local Appine README."
  (interactive)
  (if (and (boundp 'appine-root-dir) appine-root-dir)
      (find-file (expand-file-name "README.zh-CN.md" appine-root-dir))
    (user-error "Appine root dir is not available")))

(defun my/appine--ensure-window-right-a (orig-fn &rest args)
  "Prefer splitting right for Appine windows before calling ORIG-FN with ARGS."
  (if-let* ((existing (and (fboundp 'appine--get-active-window-for-buffer)
                           (appine--get-active-window-for-buffer my/appine-buffer-name))))
      existing
    (let* ((base (selected-window))
           (new (split-window base nil 'right)))
      (set-window-buffer new (appine--buffer))
      (set-window-dedicated-p new nil)
      (with-current-buffer (appine--buffer)
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil)
        (setq-local cursor-type nil)
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "\nThis is the *Appine Window* buffer.\n")
          (insert "\nIf you can see this message, Emacs is currently displaying at least two *Appine Window* buffers.\n")
          (insert "\nThe embedded macOS view of Appine can only be attached to the active *Appine Window* buffer.\n")
          (insert "\nYou can press `C-x 1` to close this buffer.\n")))
      (let* ((total (window-total-width base))
             (target (max 8 (floor (* total my/appine-default-window-size)))))
        (ignore-errors
          (window-resize new (- target (window-total-width new)) t)))
      new)))

(defun my/appine--refresh-visible ()
  "Refresh visible Appine native view after window state changes."
  (when (and (featurep 'appine-module)
             (my/appine-visible-p))
    (ignore-errors (appine-refresh))
    (ignore-errors (appine--sync-active-state))))

(defun my/appine--cancel-refresh-timer ()
  "Cancel the delayed Appine refresh timer."
  (when (timerp my/appine--refresh-timer)
    (cancel-timer my/appine--refresh-timer))
  (setq my/appine--refresh-timer nil))

(defun my/appine--schedule-refresh-visible (&rest _args)
  "Schedule a delayed Appine refresh.
This keeps the native view attached to its host window after Emacs finishes
buffer/window selection changes."
  (if (and (featurep 'appine-module)
           (my/appine-visible-p))
      (progn
        (my/appine--cancel-refresh-timer)
        (setq my/appine--refresh-timer
              (run-at-time
               0.05 nil
               (lambda ()
                 (setq my/appine--refresh-timer nil)
                 (my/appine--refresh-visible)))))
    (my/appine--cancel-refresh-timer)))

(use-package appine
  :ensure nil
  :commands (appine
             appine-open-url
             appine-open-file
             appine-open-file-by-file-chooser
             appine-close
             appine-kill
             appine-toggle-use-for-org-links
             appine-close-tab
             appine-next-tab
             appine-prev-tab
             appine-focus
             appine-web-go-back
             appine-web-go-forward
             appine-web-reload))


(with-eval-after-load 'appine
  ;; Migrate any existing user plugins to etc/appine/plugins/ on first load.
  (my/appine--ensure-user-plugins-dir)
  (advice-add 'appine--ensure-window :around #'my/appine--ensure-window-right-a)
  ;; Tab registry: all opens go through appine-open-url (appine-open-file
  ;; calls appine-open-url internally), so one advisor covers both.
  (advice-add 'appine-open-url  :around #'my/appine--track-open-url-a)
  (advice-add 'appine-next-tab  :around #'my/appine--track-next-tab-a)
  (advice-add 'appine-prev-tab  :around #'my/appine--track-prev-tab-a)
  (advice-add 'appine-close-tab :around #'my/appine--track-close-tab-a)
  (add-hook 'window-configuration-change-hook #'my/appine--schedule-refresh-visible)
  (add-hook 'window-buffer-change-functions #'my/appine--schedule-refresh-visible)
  (add-hook 'window-selection-change-functions #'my/appine--schedule-refresh-visible)
  (when (boundp 'appine-active-map)
    (define-key appine-active-map (kbd "H")   #'my/appine-back)
    (define-key appine-active-map (kbd "L")   #'my/appine-forward)
    (define-key appine-active-map (kbd "M-w") #'my/appine-close-tab)
    (define-key appine-active-map (kbd "[")   #'my/appine-prev-tab)
    (define-key appine-active-map (kbd "]")   #'my/appine-next-tab)
    (define-key appine-active-map (kbd "d")   #'my/appine-close-tab)
    (define-key appine-active-map (kbd "n")   #'my/appine-new-tab)
    (define-key appine-active-map (kbd "/")   #'my/appine-find)
    (define-key appine-active-map (kbd "W")   #'my/appine-to-eww)
    (define-key appine-active-map (kbd "X") #'my/appine-to-xwidget)
    (define-key appine-active-map (kbd "Q") #'my/appine-kill-all)
    (define-key appine-active-map (kbd "?") #'my/appine-board)))

(provide 'init-appine)
;;; init-appine.el ends here

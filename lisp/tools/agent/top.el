;;; +agent-top.el --- Live agent sessions at a glance, btop-style -*- lexical-binding: t; -*-

;;; Commentary:
;; A btop-style board over every live agent session: subscription quota,
;; one row per session (state, model, context fill, tokens, cost, turns,
;; compactions, idle time) and a detail panel for the session at point, with
;; the lifecycle actions the Agent window already offers.
;;
;; Everything comes from state Emacs already holds: `+agent-acp-usage'
;; (what agent-shell recorded from the ACP connection), the Claude rate limits
;; `+agent-acp' records from `usage_update' metadata, and the Codex rate
;; limits in the newest Codex session file.  Nothing scans processes or polls:
;; the board renders when it opens, on `g', and -- while it is visible -- once
;; shortly after a session changes (`+agent-acp-changed-functions').
;;
;; Ideas taken from abtop (github.com/graykode/abtop): context-drop
;; compaction counting, Codex `token_count' rate limits, and quota windows
;; named by their length rather than by their position.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'aaron-ui-board)
(require 'url)
(require 'comint)
(require 'tools/agent/acp)

(declare-function +agent-session-restart "tools/agent/sessions" (&optional buffer))
(declare-function +agent-session-rename "tools/agent/sessions" (&optional buffer new-name))
(declare-function +agent-session-list "tools/agent/sessions" (&optional buffer))
(declare-function +agent-session-compact "tools/agent/sessions" (&optional buffer))

(defconst +agent-top--buffer-name "*Agent Top*")

(defvar-local +agent-top--root nil
  "Project root the board is restricted to, or nil for every live session.")

(defvar-local +agent-top--shown-id nil
  "Session whose detail panel is rendered, to re-render only when it changes.")

(defvar +agent-top--refresh-timer nil
  "One-shot timer coalescing usage reports into one render.")

(defconst +agent-top--refresh-delay 1.0
  "Seconds to coalesce session changes before re-rendering a visible board.")

(defvar-local +agent-top--folded nil
  "Panels (`quota', `remote', `cache', `sessions', `session') currently folded.")

(defvar-local +agent-top--default-root nil
  "Project directory from which the board was opened.")

(defvar +agent-top--codex-remote-state nil
  "Codex Remote state: on, off, error, checking, or nil before the first probe.")

(defvar +agent-top--codex-remote-note nil
  "Short diagnostic from the last Codex Remote command.")

(defvar +agent-top--codex-remote-checked-at 0
  "Time of the last Codex Remote state probe.")

(defvar +agent-top--codex-remote-baseline 'unknown
  "Whether Codex Remote was enabled before abtop changed it.")

(defvar +agent-top--codex-remote-owned nil
  "Non-nil after abtop starts a Codex Remote state-changing command.")

(defvar +agent-top--codex-remote-last-requested nil
  "Last enabled or disabled Codex Remote state requested by abtop.")

(defvar +agent-top--codex-remote-process nil
  "Codex Remote command currently running for abtop, if any.")

(defvar +agent-top--shutting-down nil
  "Non-nil while abtop cleans up Remote Control during Emacs exit.")

(defvar +agent-top--claude-remotes (make-hash-table :test #'equal)
  "Project directory to live Claude Remote Control process.")

(defvar +agent-top--claude-remote-errors (make-hash-table :test #'equal)
  "Project directory to the last Claude Remote exit and its console buffer.")

;;; Faces
;;
;; The board is a character grid, so its faces carry colour only: the scaled
;; heights of `aaron-ui-board-meta' and friends would break column alignment.

(defface +agent-top-border '((t :inherit shadow))
  "Panel borders and empty bar cells."
  :group '+agent-session)
(defface +agent-top-title '((t :inherit bold))
  "Panel titles."
  :group '+agent-session)
(defface +agent-top-dim '((t :inherit shadow))
  "Labels and secondary values."
  :group '+agent-session)
(defface +agent-top-faint '((t :inherit shadow))
  "Identifiers, paths and data ages."
  :group '+agent-session)
(defface +agent-top-key '((t :inherit bold))
  "Keys in the key-hint line."
  :group '+agent-session)

(defvar +agent-top--theme-signature nil
  "Theme the board faces were last coloured for.")

(defun +agent-top--apply-faces ()
  "Colour the board faces from the active `aaron-ui' palette, once per theme."
  (let ((signature (list custom-enabled-themes
                         (face-attribute 'default :background nil t))))
    (unless (equal signature +agent-top--theme-signature)
      (setq +agent-top--theme-signature signature)
      (aaron-ui-set-face '+agent-top-border :foreground 'border-muted)
      (aaron-ui-set-face '+agent-top-title :foreground 'role-strong :weight 'semi-bold)
      (aaron-ui-set-face '+agent-top-dim :foreground 'fg-muted)
      (aaron-ui-set-face '+agent-top-faint :foreground 'fg-faint)
      (aaron-ui-set-face '+agent-top-key :foreground 'accent-cyan :weight 'semi-bold))))

;;; Formatting

(defun +agent-top--tokens (number)
  "Format NUMBER of tokens compactly, or \"-\" when it is zero."
  (cond ((or (null number) (<= number 0)) "-")
        ((>= number 1000000) (format "%.1fM" (/ number 1e6)))
        ((>= number 1000) (format "%.1fk" (/ number 1e3)))
        (t (number-to-string number))))

(defun +agent-top--cost (amount currency)
  "Format AMOUNT in CURRENCY, or \"-\" when no cost was reported."
  (if (not amount)
      "-"
    (concat (if (member currency '(nil "USD")) "$" (concat currency " "))
            (format "%.2f" amount))))

(defun +agent-top--duration (seconds)
  "Format SECONDS compactly, as abtop does."
  (let ((seconds (max 0 (truncate seconds))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh%02dm" (/ seconds 3600) (/ (% seconds 3600) 60)))
          (t (format "%dd%02dh" (/ seconds 86400) (/ (% seconds 86400) 3600))))))

(defun +agent-top--window-name (minutes)
  "Name a quota window of MINUTES by its length: 5h, 7d, 30d."
  (cond ((not (numberp minutes)) "?")
        ((zerop (% minutes 1440)) (format "%dd" (/ minutes 1440)))
        ((zerop (% minutes 60)) (format "%dh" (/ minutes 60)))
        (t (format "%dm" minutes))))

(defun +agent-top--level (ratio)
  "Return the good/warn/bad face for RATIO of a limit used."
  (cond ((>= ratio 0.9) 'aaron-ui-board-bad)
        ((>= ratio 0.7) 'aaron-ui-board-warn)
        (t 'aaron-ui-board-good)))

(defun +agent-top--bar (ratio width)
  "Return a WIDTH-column bar filled to RATIO, coloured by level."
  (let* ((ratio (max 0.0 (min 1.0 ratio)))
         (filled (round (* ratio width))))
    (concat (propertize (make-string filled ?█) 'face (+agent-top--level ratio))
            (propertize (make-string (- width filled) ?░) 'face '+agent-top-border))))

(defun +agent-top--cell (text width &optional face right)
  "Return TEXT fitted to WIDTH columns, in FACE, right-aligned when RIGHT."
  (let* ((text (truncate-string-to-width (format "%s" text) width nil nil "…"))
         (pad (make-string (max 0 (- width (string-width text))) ?\s))
         (text (if face (propertize text 'face face) text)))
    (if right (concat pad text) (concat text pad))))

;;; Panels

(defun +agent-top--width ()
  "Return the column width to draw panels in."
  (let ((window (get-buffer-window (current-buffer))))
    (max 60 (- (if window (window-body-width window) 100) 1))))

(defun +agent-top--panel-open (panel title &optional note)
  "Insert the top border of PANEL named TITLE, with an optional NOTE.
Return non-nil when PANEL is open; a folded panel is this one line only."
  (let* ((open (not (memq panel +agent-top--folded)))
         (width (+agent-top--width))
         (label (concat (if open " ▾ " " ▸ ") title " "))
         (note (and note (concat " " note " ")))
         (fill (- width 4 (string-width label) (if note (string-width note) 0)))
         (start (point)))
    (insert (propertize (if open "╭─" "╶─") 'face '+agent-top-border)
            (propertize label 'face '+agent-top-title)
            (propertize (make-string (max 1 fill) ?─) 'face '+agent-top-border)
            (if note (propertize note 'face '+agent-top-dim) "")
            (propertize (if open "─╮" "─╴") 'face '+agent-top-border)
            "\n")
    (add-text-properties start (point)
                         `(+agent-top--panel ,panel
                           mouse-face aaron-ui-board-row-highlight
                           help-echo "TAB: fold or unfold"))
    open))

(defun +agent-top--panel-line (&rest parts)
  "Insert one panel line made of PARTS between the side borders."
  (let ((width (+agent-top--width)))
    (insert (propertize "│ " 'face '+agent-top-border)
            (apply #'concat parts)
            (propertize " " 'display `(space :align-to ,(- width 1)))
            (propertize "│" 'face '+agent-top-border)
            "\n")))

(defun +agent-top--panel-close ()
  "Insert the bottom border of a panel."
  (insert (propertize (concat "╰" (make-string (- (+agent-top--width) 2) ?─) "╯")
                      'face '+agent-top-border)
          "\n"))

;;; Codex quota, from Codex's own session files

(defvar +agent-top--codex-cache nil
  "(FILE MTIME . LIMITS) for the Codex session file read last.")

(defun +agent-top--codex-home ()
  "Return the client's Codex home.
The quota belongs to the account, so the newest session file of any Codex
client on this machine -- the TUI or a codex-acp session -- holds its latest
value; each window shows how old that value is."
  (file-name-as-directory (or (getenv "CODEX_HOME") (expand-file-name "~/.codex"))))

(defun +agent-top--newest-child (directory &optional regexp)
  "Return the lexically greatest entry of DIRECTORY matching REGEXP."
  (car (last (ignore-errors (directory-files directory t (or regexp "\\`[0-9]+\\'"))))))

(defun +agent-top--codex-file ()
  "Return the most recently written Codex session file, or nil.
Session files live under sessions/YYYY/MM/DD/, so only the newest day is
listed rather than the whole tree."
  (when-let* ((year (+agent-top--newest-child
                     (expand-file-name "sessions" (+agent-top--codex-home))))
              (month (+agent-top--newest-child year))
              (day (+agent-top--newest-child month)))
    ;; Sorting with `file-newer-than-file-p' stats the same files repeatedly.
    ;; Read each file's attributes once and select the latest in one pass.
    (let (newest newest-time)
      (dolist (entry (ignore-errors
                       (directory-files-and-attributes
                        day t "\\`rollout-.*\\.jsonl\\'" t)))
        (let ((mtime (file-attribute-modification-time (cdr entry))))
          (when (or (null newest-time) (time-less-p newest-time mtime))
            (setq newest (car entry) newest-time mtime))))
      newest)))

(defconst +agent-top--codex-tail-bytes 131072
  "Bytes read from the end of a Codex session file to find its last quota.")

(defun +agent-top--codex-parse (text)
  "Return rate-limit plists from the last `token_count' with limits in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-max))
    (catch 'found
      (while (search-backward "\"rate_limits\":{" nil t)
        (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))
               (event (ignore-errors
                        (json-parse-string line :object-type 'alist :null-object nil)))
               (limits (map-nested-elt event '(payload rate_limits))))
          (when limits
            (let ((updated (ignore-errors
                             (float-time (date-to-time (map-elt event 'timestamp))))))
              (throw 'found
                     (delq nil
                           (mapcar
                            (lambda (key)
                              (when-let* ((window (map-elt limits key))
                                          (used (map-elt window 'used_percent)))
                                (list :agent "codex"
                                      :window (+agent-top--window-name
                                               (map-elt window 'window_minutes))
                                      :utilization (/ used 100.0)
                                      :resets-at (map-elt window 'resets_at)
                                      :updated-at updated)))
                            '(primary secondary)))))))
        (beginning-of-line)))))

(defun +agent-top--codex-limits ()
  "Return Codex rate-limit plists, re-reading only a changed session file."
  (when-let* ((file (+agent-top--codex-file)))
    (let* ((attributes (file-attributes file))
           (stamp (list file (file-attribute-modification-time attributes)
                        (file-attribute-size attributes))))
      (unless (equal (car +agent-top--codex-cache) stamp)
        (let ((size (file-attribute-size attributes)))
          (setq +agent-top--codex-cache
                (cons stamp
                      (+agent-top--codex-parse
                       (with-temp-buffer
                         (insert-file-contents-literally
                          file nil (max 0 (- size +agent-top--codex-tail-bytes)) size)
                         (decode-coding-region (point-min) (point-max) 'utf-8)
                         (buffer-string)))))))
      (cdr +agent-top--codex-cache))))

(defun +agent-top--claude-window-name (type)
  "Name claude-agent-acp rate-limit TYPE like abtop does."
  (pcase type
    ("five_hour" "5h") ("seven_day" "7d")
    ("seven_day_opus" "7d opus") ("seven_day_sonnet" "7d sonnet")
    (_ (replace-regexp-in-string "_" " " (format "%s" type)))))

;;; Claude quota, from Claude's usage endpoint (explicit refresh only)
;;
;; claude-agent-acp only reports a window during a turn.  On an explicit
;; request -- opening the board or `g' -- and at most every
;; `+agent-top-claude-usage-interval', ask the endpoint Claude Code's
;; /usage uses.  The OAuth token is read from where Claude Code keeps it, used
;; for this one request and not retained.  It is never refreshed here: that
;; would rotate Claude Code's refresh token and log it out.  This is a
;; client-side account query, so it runs on the client, not on a target.

(defcustom +agent-top-claude-usage t
  "Whether the board asks Claude's usage endpoint for the Claude quota."
  :type 'boolean
  :group '+agent-session)

(defcustom +agent-top-claude-usage-interval 300
  "Seconds a fetched Claude quota stays fresh; a refresh sooner reuses it."
  :type 'integer
  :group '+agent-session)

(defconst +agent-top--claude-usage-url "https://api.anthropic.com/api/oauth/usage")

(defconst +agent-top--claude-usage-timeout 10
  "Seconds before an unanswered usage request is abandoned.")

(defvar +agent-top--claude-fetch nil
  "Plist describing the last Claude usage fetch: :running, :at and :error.")

(defun +agent-top--claude-credentials-file ()
  "Return Claude Code's credential file, used where there is no Keychain."
  (expand-file-name ".credentials.json"
                    (or (getenv "CLAUDE_CONFIG_DIR") (expand-file-name "~/.claude"))))

(defun +agent-top--claude-token (json)
  "Return the OAuth access token in credential JSON, or signal why not."
  (let* ((oauth (map-elt (json-parse-string json :object-type 'alist) 'claudeAiOauth))
         (token (map-elt oauth 'accessToken))
         (expires (map-elt oauth 'expiresAt)))
    (cond ((not (stringp token)) (error "no Claude login found"))
          ((and (numberp expires) (< (/ expires 1000.0) (float-time)))
           (error "Claude login expired; open Claude Code to renew it"))
          (t token))))

(defun +agent-top--claude-read-token (callback)
  "Call CALLBACK with the Claude OAuth token, or with nil and a reason.
The macOS Keychain is read by an asynchronous `security' process; elsewhere
Claude Code's credential file is read."
  (let ((default-directory temporary-file-directory))
    (if (not (executable-find "security"))
        (condition-case err
            (funcall callback
                     (+agent-top--claude-token
                      (with-temp-buffer
                        (insert-file-contents (+agent-top--claude-credentials-file))
                        (buffer-string))))
          (error (funcall callback nil (error-message-string err))))
      (let ((output (generate-new-buffer " *noema-claude-token*"))
            (errors (generate-new-buffer " *noema-claude-token-stderr*")))
        (condition-case err
            (make-process
             :name "noema-claude-token"
             :buffer output
             :command '("security" "find-generic-password"
                        "-s" "Claude Code-credentials" "-w")
             :connection-type 'pipe
             :noquery t
             :stderr errors
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (let ((json (and (buffer-live-p output)
                                  (with-current-buffer output (buffer-string)))))
                   (kill-buffer output)
                   (when (buffer-live-p errors)
                     (let ((kill-buffer-query-functions nil)) (kill-buffer errors)))
                   (if (and (zerop (process-exit-status process)) json)
                       (condition-case err
                           (funcall callback (+agent-top--claude-token json))
                         (error (funcall callback nil (error-message-string err))))
                     (funcall callback nil "no Claude login in the Keychain"))))))
          (error (kill-buffer output)
                 (kill-buffer errors)
                 (funcall callback nil (error-message-string err))))))))

(defun +agent-top--claude-record-usage (response)
  "Record the windows in usage endpoint RESPONSE; utilization there is 0-100."
  (dolist (window '(five_hour seven_day seven_day_opus seven_day_sonnet))
    (when-let* ((value (map-elt response window))
                (utilization (map-elt value 'utilization))
                ((numberp utilization)))
      (+agent-acp-put-rate-limit
       "claude" (symbol-name window) (/ utilization 100.0)
       :resets-at (let ((resets (map-elt value 'resets_at)))
                    (and (stringp resets)
                         (ignore-errors (float-time (date-to-time resets)))))))))

(defun +agent-top--claude-fetch-done (error-message)
  "Finish a Claude usage fetch, noting ERROR-MESSAGE, and redraw the board."
  (setq +agent-top--claude-fetch
        (list :at (float-time) :error error-message))
  (+agent-top--session-changed nil))

(defun +agent-top--claude-request (token)
  "Ask the usage endpoint with TOKEN; the token lives only in this request."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("anthropic-beta" . "oauth-2025-04-20")
            ("Content-Type" . "application/json")))
         (finished nil)
         (timer nil)
         (buffer nil))
    (setq buffer
          (condition-case err
              (url-retrieve
               +agent-top--claude-usage-url
               (lambda (status)
                 (unwind-protect
                     (unless finished
                       (setq finished t)
                       (when (timerp timer) (cancel-timer timer))
                       (condition-case err
                           (if-let* ((problem (plist-get status :error)))
                               (+agent-top--claude-fetch-done
                                (format "usage request failed: %s" (cadr problem)))
                             (goto-char (point-min))
                             (re-search-forward "\r?\n\r?\n" nil 'move)
                             (+agent-top--claude-record-usage
                              (json-parse-buffer :object-type 'alist :null-object nil))
                             (+agent-top--claude-fetch-done nil))
                         (error (+agent-top--claude-fetch-done
                                 (format "usage reply unreadable: %s" (error-message-string err))))))
                   (kill-buffer (current-buffer))))
               nil t t)
            (error (setq finished t)
                   (+agent-top--claude-fetch-done
                    (format "usage request failed: %s" (error-message-string err)))
                   nil)))
    (when buffer
      (setq timer
            (run-with-timer
             +agent-top--claude-usage-timeout nil
             (lambda ()
               (unless finished
                 (setq finished t)
                 (when (buffer-live-p buffer)
                   (let ((process (get-buffer-process buffer)))
                     (when process (delete-process process)))
                   (kill-buffer buffer))
                 (+agent-top--claude-fetch-done "usage request timed out"))))))))

(defun +agent-top--claude-fetch (&optional force)
  "Fetch the Claude quota unless a fetch is running or recent; FORCE ignores age."
  (when (and +agent-top-claude-usage
             (not (plist-get +agent-top--claude-fetch :running))
             (or force
                 (> (- (float-time) (or (plist-get +agent-top--claude-fetch :at) 0))
                    +agent-top-claude-usage-interval)))
    (setq +agent-top--claude-fetch
          (plist-put (copy-sequence +agent-top--claude-fetch) :running t))
    (+agent-top--claude-read-token
     (lambda (token &optional reason)
       (if token
           (+agent-top--claude-request token)
         (+agent-top--claude-fetch-done reason))))))

(defun +agent-top--opencode-auth-file ()
  "Return OpenCode's credential file on this machine."
  (expand-file-name "opencode/auth.json"
                    (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share"))))

(defvar +agent-top--opencode-cache nil
  "(MTIME . PROVIDERS) for the OpenCode credential file read last.")

(defun +agent-top--opencode-providers ()
  "Return the provider ids OpenCode is signed in to.
Only the ids are kept; the credentials in the file are never retained."
  (let* ((file (+agent-top--opencode-auth-file))
         (mtime (file-attribute-modification-time (file-attributes file))))
    (when mtime
      (unless (equal (car +agent-top--opencode-cache) mtime)
        (setq +agent-top--opencode-cache
              (cons mtime
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (sort (mapcar (lambda (entry) (symbol-name (car entry)))
                                      (json-parse-buffer :object-type 'alist))
                              #'string<))))))
      (cdr +agent-top--opencode-cache))))

(defconst +agent-top--opencode-notes
  '(("openai" . "ChatGPT login: shares the codex quota above")
    ("github-copilot" . "premium requests are only reported by GitHub's API")
    ("anthropic" . "Claude login: shares the claude quota above"))
  "What the quota panel says about each OpenCode provider.")

(defun +agent-top--quota-group (tool expected limits missing)
  "Return TOOL's quota rows: EXPECTED windows in order, then other LIMITS.
An expected window absent from LIMITS becomes a row noting MISSING."
  (let ((window-of (lambda (limit) (plist-get limit :window))))
    (cons tool
          (append
           (mapcar (lambda (window)
                     (or (cl-find window limits :key window-of :test #'equal)
                         (list :window window :note missing)))
                   expected)
           (sort (cl-remove-if (lambda (limit) (member (funcall window-of limit) expected))
                               limits)
                 (lambda (a b) (string< (funcall window-of a) (funcall window-of b))))))))

(defun +agent-top--quotas ()
  "Return (TOOL . ROWS) for Claude, Codex and OpenCode, in that order.
Every tool is listed with the windows it is expected to have, so a quota
nobody has reported yet shows as such rather than disappearing."
  (let ((acp (make-hash-table :test #'equal)))
    (maphash (lambda (_key limit)
               (push (plist-put (copy-sequence limit) :window
                                (+agent-top--claude-window-name
                                 (plist-get limit :window)))
                     (gethash (plist-get limit :agent) acp)))
             (+agent-acp-rate-limits))
    (append
     (list (+agent-top--quota-group
            "claude" '("5h" "7d") (gethash "claude" acp)
            (cond ((plist-get +agent-top--claude-fetch :running) "asking Claude…")
                  ((plist-get +agent-top--claude-fetch :error))
                  (t "reported during a turn, or on g")))
           (+agent-top--quota-group
            "codex" '("5h" "7d") (ignore-errors (+agent-top--codex-limits))
            "absent from the latest Codex report")
           (cons "opencode"
                 (or (mapcar (lambda (provider)
                               (list :window provider
                                     :note (or (cdr (assoc provider +agent-top--opencode-notes))
                                               "no local quota source")))
                             (+agent-top--opencode-providers))
                     (list (list :window "-" :note "not signed in")))))
     ;; An agent reporting limits that this list does not know yet.
     (let (others)
       (maphash (lambda (agent limits)
                  (unless (member agent '("claude" "codex" "opencode"))
                    (push (+agent-top--quota-group agent nil limits nil) others)))
                acp)
       others))))

;;; Rendering

(defun +agent-top--cache-metrics (usage)
  "Return reported fresh input, cache reads, writes and read share for USAGE.
ACP reports these as separate categories; the share is over their sum."
  (let* ((fresh (or (plist-get usage :input) 0))
         (read (or (plist-get usage :cached-read) 0))
         (write (or (plist-get usage :cached-write) 0))
         (total (+ fresh read write)))
    (list fresh read write (and (> total 0) (/ (float read) total)))))

(defun +agent-top--cache-summary (rows)
  "Return aggregate cache metrics for ROWS, each a (SESSION . USAGE)."
  (let ((fresh 0) (read 0) (write 0))
    (dolist (row rows)
      (pcase-let ((`(,f ,r ,w ,_) (+agent-top--cache-metrics (cdr row))))
        (cl-incf fresh f) (cl-incf read r) (cl-incf write w)))
    (+agent-top--cache-metrics
     (list :input fresh :cached-read read :cached-write write))))

(defun +agent-top--cache-description (metrics)
  "Describe cache METRICS in terms useful for comparing sessions."
  (pcase-let ((`(,fresh ,read ,write ,share) metrics))
    (format "read %s · fresh %s · write %s · reuse %s%s"
            (+agent-top--tokens read)
            (+agent-top--tokens fresh)
            (+agent-top--tokens write)
            (if share (format "%.0f%%" (* 100 share)) "?")
            (if (> write 0)
                (format " · read/write %.1fx" (/ (float read) write)) ""))))

(defun +agent-top--insert-cache (rows)
  "Insert the cache panel for ROWS."
  (when (+agent-top--panel-open 'cache "prompt cache" "ACP reported tokens")
    (+agent-top--panel-line
     (+agent-top--field "all" (+agent-top--cache-description
                                      (+agent-top--cache-summary rows))))
    (dolist (row rows)
      (let* ((session (car row))
             (name (or (plist-get session :name)
                       (buffer-name (plist-get session :buffer)))))
        (+agent-top--panel-line
         (+agent-top--field
          (truncate-string-to-width name 9 nil nil "…")
          (+agent-top--cache-description
           (+agent-top--cache-metrics (cdr row)))))))
    (+agent-top--panel-close)))

;;; Remote Control

(defun +agent-top--remote-redraw ()
  "Redraw a visible board after a remote process changes state."
  (unless +agent-top--shutting-down
    (when-let* ((board (get-buffer +agent-top--buffer-name))
                (window (get-buffer-window board 'visible)))
      (with-selected-window window (+agent-top-refresh)))))

(defun +agent-top--codex-remote-classify (message exit-code)
  "Classify a Codex status MESSAGE with EXIT-CODE.
An enabled daemon is only `on' after the CLI confirms its connection."
  (let ((case-fold-search t))
    (cond
     ((string-match-p "errored\\|\"status\"[[:space:]]*:[[:space:]]*\"error"
                      message) 'error)
     ((and (zerop exit-code)
           (or (string-match-p
                "\"status\"[[:space:]]*:[[:space:]]*\"connected\""
                message)
               (string-match-p "Remote control is .*connected" message))) 'on)
     ((string-match-p
       "not enabled\\|disabled\\|\"status\"[[:space:]]*:[[:space:]]*\"stopped\""
       message) 'off)
     ((string-match-p
       "connecting\\|\"status\"[[:space:]]*:[[:space:]]*\"connecting\""
       message) 'connecting)
     (t 'error))))

(defun +agent-top--codex-remote-enabled-state (state message)
  "Return `enabled', `disabled', or `unknown' from STATE and MESSAGE."
  (pcase state
    ('off 'disabled)
    ((or 'on 'connecting) 'enabled)
    ('error (if (string-match-p
                 "\\benabled\\b\\|\\\"status\\\"[[:space:]]*:[[:space:]]*\\\"errored\\\""
                 (or message ""))
                'enabled 'unknown))
    (_ 'unknown)))

(defun +agent-top--codex-remote-command (action)
  "Return the Codex command for Remote Control ACTION."
  (let ((codex (or (executable-find "codex")
                   (user-error "codex executable not found"))))
    (pcase action
      ('enable (list codex "app-server" "daemon" "enable-remote-control"))
      ('disable (list codex "app-server" "daemon" "disable-remote-control"))
      ('retry (list codex "remote-control" "--json" "start"))
      (_ (list codex "remote-control" "--json")))))

(defun +agent-top--codex-remote-prepare-action (action)
  "Remember Codex's original state before abtop runs mutating ACTION."
  (when (memq action '(enable disable retry))
    (when (eq +agent-top--codex-remote-baseline 'unknown)
      (setq +agent-top--codex-remote-baseline
            (+agent-top--codex-remote-enabled-state
             +agent-top--codex-remote-state
             +agent-top--codex-remote-note)))
    (when (eq +agent-top--codex-remote-baseline 'unknown)
      (user-error "Codex Remote's original state is unknown; refresh before changing it"))))

(defun +agent-top--codex-remote-run-actual (action)
  "Run Codex Remote Control ACTION asynchronously."
  (+agent-top--codex-remote-prepare-action action)
  (let* ((command (+agent-top--codex-remote-command action))
         (output (generate-new-buffer " *noema-codex-remote*"))
         (errors (generate-new-buffer " *noema-codex-remote-errors*")))
    (setq +agent-top--codex-remote-state 'checking
          +agent-top--codex-remote-note nil)
    (+agent-top--remote-redraw)
    (condition-case err
        (let ((process
               (make-process
         :name "noema-codex-remote"
         :buffer output :stderr errors :noquery t :command command
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (when (eq process +agent-top--codex-remote-process)
               (setq +agent-top--codex-remote-process nil))
             (let ((message (string-trim
                             (concat (with-current-buffer output (buffer-string))
                                     "\n" (with-current-buffer errors (buffer-string))))))
               (kill-buffer output)
               (kill-buffer errors)
               (setq +agent-top--codex-remote-checked-at (float-time))
               (cond
                ((process-get process '+agent-top--timed-out)
                 (setq +agent-top--codex-remote-state 'error
                       +agent-top--codex-remote-note
                       "Codex Remote command timed out; checking current state")
                 (unless (eq action 'status)
                   (run-at-time 2 nil #'+agent-top--codex-remote-run 'status)))
                ((zerop (process-exit-status process))
                   (if (eq action 'status)
                       (setq +agent-top--codex-remote-state
                             (+agent-top--codex-remote-classify message 0)
                             +agent-top--codex-remote-note
                             (unless (eq +agent-top--codex-remote-state 'on)
                               (car (last (split-string message "\n" t)))))
                     ;; An enable/retry can report success before the persistent
                     ;; daemon loses its connection.  Verify its actual state.
                     (setq +agent-top--codex-remote-state 'checking)
                     (run-at-time 2 nil #'+agent-top--codex-remote-run 'status)))
                (t
                 (setq +agent-top--codex-remote-state
                       (+agent-top--codex-remote-classify
                        message (process-exit-status process))
                       +agent-top--codex-remote-note
                       (if (string-match-p "enabled.*errored" message)
                           "enabled, but the connection is errored"
                         (car (last (split-string message "\n" t)))))))
               (when (and (eq action 'status)
                          (not +agent-top--codex-remote-owned)
                          (eq +agent-top--codex-remote-baseline 'unknown))
                 (setq +agent-top--codex-remote-baseline
                       (+agent-top--codex-remote-enabled-state
                        +agent-top--codex-remote-state message)))
               (+agent-top--remote-redraw)))))))
          (setq +agent-top--codex-remote-process process)
          (when (memq action '(enable disable retry))
            (setq +agent-top--codex-remote-owned t
                  +agent-top--codex-remote-last-requested
                  (if (eq action 'disable) 'disabled 'enabled)))
          (run-at-time 20 nil
                       (lambda ()
                         (when (process-live-p process)
                           (process-put process '+agent-top--timed-out t)
                           (delete-process process)))))
      (error (kill-buffer output)
             (kill-buffer errors)
             (setq +agent-top--codex-remote-state 'error
                   +agent-top--codex-remote-note (error-message-string err))
             (+agent-top--remote-redraw)))))

(defun +agent-top--codex-remote-run (action)
  "Run Codex Remote ACTION unless Emacs is shutting down."
  (unless +agent-top--shutting-down
    (+agent-top--codex-remote-run-actual action)))

(defun +agent-top--codex-remote-probe ()
  "Refresh Codex Remote state if its last result is stale."
  (when (and (not (eq +agent-top--codex-remote-state 'checking))
             (> (- (float-time) +agent-top--codex-remote-checked-at) 30))
    (+agent-top--codex-remote-run 'status)))

(defun +agent-top-codex-remote-toggle ()
  "Enable, retry, or disable machine-wide Codex Remote Control."
  (interactive)
  (when (eq +agent-top--codex-remote-state 'checking)
    (user-error "Codex Remote Control is still checking"))
  (+agent-top--codex-remote-run
   (pcase +agent-top--codex-remote-state
     ('on 'disable) ('error 'retry) (_ 'enable))))

(defun +agent-top--project-directory ()
  "Return the selected session's project or the board's opening project."
  (let ((buffer (get-text-property (point) 'aaron-ui-board--item-id)))
    (or (and (buffer-live-p buffer)
             (buffer-local-value '+agent-acp-session-root buffer))
        +agent-top--root +agent-top--default-root default-directory)))

(defun +agent-top--claude-remote-scan (tail chunk)
  "Read Claude connection state and web URL from TAIL followed by CHUNK.
Return a plist with :tail, :state, and :url.  Claude redraws a terminal
screen, so the last status word in the newest output is authoritative."
  (let ((text (concat (or tail "") chunk))
        (from 0)
        state url)
    (while (string-match
            "\\b\\(Connected\\|Connecting\\|Reconnecting\\|Disconnected\\)\\b"
            text from)
      (setq state (pcase (match-string 1 text)
                    ("Connected" 'connected)
                    ("Disconnected" 'disconnected)
                    (_ 'connecting))
            from (match-end 0)))
    (when (string-match
           (concat (regexp-quote "https://claude.ai/code?environment=")
                   "[[:alnum:]_]+") text)
      (setq url (substring-no-properties (match-string 0 text))))
    (list :tail (substring text (max 0 (- (length text) 256)))
          :state state :url url)))

(defun +agent-top--claude-remote-filter (process chunk)
  "Record Claude Remote connection updates in PROCESS from CHUNK."
  (comint-output-filter process chunk)
  (let* ((scan (+agent-top--claude-remote-scan
                (process-get process '+agent-top--tail) chunk))
         (state (or (plist-get scan :state)
                    (process-get process '+agent-top--state)))
         (url (or (plist-get scan :url)
                  (process-get process '+agent-top--url)))
         (changed (or (not (eq state (process-get process '+agent-top--state)))
                      (not (equal url (process-get process '+agent-top--url))))))
    (process-put process '+agent-top--tail (plist-get scan :tail))
    (process-put process '+agent-top--state state)
    (process-put process '+agent-top--url url)
    (when changed (+agent-top--remote-redraw))))

(defun +agent-top--claude-remote-failure (buffer exit-code)
  "Extract the CLI error in BUFFER, or describe EXIT-CODE."
  (let* ((text (and (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (buffer-substring-no-properties
                       (max (point-min) (- (point-max) 8192)) (point-max)))))
         (plain (and text (replace-regexp-in-string
                           "\e\\[[0-9;?]*[[:alpha:]]" "" text)))
         (from 0)
         reason)
    (while (and plain (string-match "Error:[ \t]*\\([^\r\n]+\\)" plain from))
      (setq reason (string-trim (match-string 1 plain))
            from (match-end 0)))
    (or reason (format "exit %s" exit-code))))

(defun +agent-top-claude-remote-start (directory)
  "Start Claude Remote Control for DIRECTORY and show it in the board."
  (interactive (list (read-directory-name "Claude Remote project: "
                                           (+agent-top--project-directory))))
  (unless (executable-find "claude") (user-error "claude executable not found"))
  (let* ((directory (file-name-as-directory (file-truename directory)))
         (existing (gethash directory +agent-top--claude-remotes)))
    (when (process-live-p existing)
      (user-error "Claude Remote Control is already running for %s" directory))
    (when-let* ((previous (gethash directory +agent-top--claude-remote-errors))
                (buffer (plist-get previous :buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (remhash directory +agent-top--claude-remote-errors)
    (let ((default-directory directory)
          (output (generate-new-buffer
                   (format "*Claude Remote: %s*"
                           (file-name-nondirectory (directory-file-name directory))))))
      (with-current-buffer output (comint-mode))
      (let ((process
             (make-process
              :name "noema-claude-remote" :buffer output :noquery t
              :connection-type 'pty :filter #'+agent-top--claude-remote-filter
              :command (list (executable-find "claude") "remote-control" "--name"
                             (file-name-nondirectory (directory-file-name directory))
                             "--spawn" "same-dir")
              :sentinel (lambda (process _event)
                          (unless (process-live-p process)
                            (remhash directory +agent-top--claude-remotes)
                            (if (process-get process '+agent-top--stopping)
                                (progn
                                  (remhash directory +agent-top--claude-remote-errors)
                                  (when (buffer-live-p output) (kill-buffer output)))
                              (puthash directory
                                       (list :reason
                                             (+agent-top--claude-remote-failure
                                              output (process-exit-status process))
                                             :buffer output)
                                       +agent-top--claude-remote-errors))
                            (+agent-top--remote-redraw))))))
        (process-put process '+agent-top--state 'connecting)
        (puthash directory process +agent-top--claude-remotes))
      (+agent-top--remote-redraw))))

(defun +agent-top-claude-remote-stop (&optional directory)
  "Stop Claude Remote Control for DIRECTORY, or dismiss a closed project."
  (interactive)
  (let* ((paths (delete-dups
                 (append (hash-table-keys +agent-top--claude-remotes)
                         (hash-table-keys +agent-top--claude-remote-errors))))
         (directory (or directory
                        (get-text-property (point) '+agent-top--remote-root)
                        (and (= (length paths) 1) (car paths))
                        (and paths (completing-read "Stop Claude Remote project: " paths nil t)))))
    (unless directory (user-error "No Claude Remote Control projects are listed"))
    (let ((process (gethash directory +agent-top--claude-remotes)))
      (cond ((process-live-p process)
             (process-put process '+agent-top--stopping t)
             (process-put process '+agent-top--state 'stopping)
             (interrupt-process process)
             (run-at-time 3 nil (lambda ()
                                  (when (process-live-p process)
                                    (delete-process process))))
             (+agent-top--remote-redraw))
            ((gethash directory +agent-top--claude-remote-errors)
             (when-let* ((result (gethash directory +agent-top--claude-remote-errors))
                         (buffer (and (listp result) (plist-get result :buffer))))
               (when (buffer-live-p buffer) (kill-buffer buffer)))
             (remhash directory +agent-top--claude-remote-errors)
             (+agent-top--remote-redraw))
            (t (user-error "Claude Remote Control is not listed for %s" directory))))))

(defun +agent-top-claude-remote-console (directory)
  "Show the interactive Claude Remote Control console for DIRECTORY."
  (interactive (list (or (get-text-property (point) '+agent-top--remote-root)
                         (user-error "Move to an open Claude Remote project"))))
  (let* ((process (gethash directory +agent-top--claude-remotes))
         (buffer (and (process-live-p process) (process-buffer process))))
    (unless (buffer-live-p buffer) (user-error "Claude Remote Control is not running"))
    (pop-to-buffer buffer)))

(defun +agent-top--button (label action)
  "Make LABEL an actionable board button calling ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive) (funcall action)))
    (define-key map (kbd "RET") (lambda () (interactive) (funcall action)))
    (propertize label 'face '+agent-top-key 'mouse-face 'highlight
                'help-echo "RET or click" 'follow-link t 'keymap map)))

(defun +agent-top--insert-remote ()
  "Insert global Codex and per-project Claude Remote Control switches."
  (when (+agent-top--panel-open 'remote "remote control")
    (+agent-top--panel-line
     (+agent-top--field
      "codex" (pcase +agent-top--codex-remote-state
                ('on "connected") ('error "enabled · connection error")
                ('off "disabled") ('connecting "connecting")
                ('checking "checking") (_ "unknown")))
     "   "
     (+agent-top--button
      (pcase +agent-top--codex-remote-state
        ('on "[E disable]") ('error "[E retry]")
        ('checking "[checking]") (_ "[E enable]"))
      #'+agent-top-codex-remote-toggle)
     (when (eq +agent-top--codex-remote-state 'error)
       (concat "  " (+agent-top--button
                      "[disable]"
                      (lambda () (+agent-top--codex-remote-run 'disable))))))
    (when +agent-top--codex-remote-note
      (+agent-top--panel-line
       (+agent-top--field
        "" (truncate-string-to-width
             +agent-top--codex-remote-note
             (max 20 (- (+agent-top--width) 14)) nil nil "…")
        '+agent-top-faint)))
    (+agent-top--panel-line
     (+agent-top--field "claude" "one server per project")
     "   " (+agent-top--button "[P open project]"
                                       (lambda () (call-interactively
                                                   #'+agent-top-claude-remote-start))))
    (if (= (hash-table-count +agent-top--claude-remotes) 0)
        (+agent-top--panel-line
         (propertize "  No active Claude Remote projects"
                     'face '+agent-top-faint))
      (maphash
       (lambda (directory process)
         (let ((start (point)))
           (+agent-top--panel-line
            (+agent-top--field
             "  path"
             (truncate-string-to-width
              (abbreviate-file-name directory)
              (max 20 (- (+agent-top--width) 15)) nil nil "…")
             '+agent-top-faint))
           (+agent-top--panel-line
            (+agent-top--field
             "  state" (if (process-live-p process)
                             (or (process-get process '+agent-top--state)
                                 'connecting)
                           'stopped))
            "   "
            (+agent-top--button
             "[console]" (lambda () (+agent-top-claude-remote-console directory)))
            (when-let* ((url (process-get process '+agent-top--url)))
              (concat " " (+agent-top--button
                            "[open Claude]" (lambda () (browse-url url)))))
            " "
            (+agent-top--button
             "[D stop]" (lambda () (+agent-top-claude-remote-stop directory))))
           (add-text-properties start (point)
                                `(+agent-top--remote-root ,directory))))
       +agent-top--claude-remotes))
    (maphash (lambda (directory result)
               (let ((start (point)))
                 (+agent-top--panel-line
                  (+agent-top--field
                   "  closed"
                   (truncate-string-to-width
                    (abbreviate-file-name directory)
                    (max 20 (- (+agent-top--width) 31)) nil nil "…")
                   '+agent-top-faint)
                  "   "
                  (when-let* ((buffer (and (listp result) (plist-get result :buffer)))
                              (_ (buffer-live-p buffer)))
                    (+agent-top--button
                     "[log]" (lambda () (pop-to-buffer buffer))))
                  " " (+agent-top--button
                       "[D clear]" (lambda () (+agent-top-claude-remote-stop directory))))
                 (+agent-top--panel-line
                  (+agent-top--field
                   "  reason"
                   (truncate-string-to-width
                    (if (stringp result) result (or (plist-get result :reason) "unknown"))
                    (max 20 (- (+agent-top--width) 15)) nil nil "…")
                   '+agent-top-dim))
                 (add-text-properties start (point)
                                      `(+agent-top--remote-root ,directory))))
             +agent-top--claude-remote-errors)
    (+agent-top--panel-close)))

(defun +agent-top--sessions ()
  "Return (SESSION . USAGE) for every live session in the board's scope."
  (delq nil
        (mapcar (lambda (session)
                  (when-let* ((usage (plist-get session :usage)))
                    (cons session usage)))
                (+agent-acp-sessions +agent-top--root))))

(defun +agent-top--state (buffer)
  "Return (LABEL . FACE) describing what agent BUFFER is doing."
  (let ((queued (+agent-acp-pending-prompt-count buffer)))
    (cond ((+agent-acp-busy-p buffer)
           (cons (if (> queued 0) (format "● work+%d" queued) "● work")
                 'aaron-ui-board-good))
          ((> queued 0) (cons (format "◐ wait+%d" queued) 'aaron-ui-board-warn))
          (t (cons "○ idle" '+agent-top-dim)))))

(defun +agent-top--quota-line (tool quota now)
  "Insert one quota panel line for QUOTA of TOOL (nil after its first line)."
  (let ((tool (+agent-top--cell (or tool "") 9 'aaron-ui-board-row-title))
        (window (+agent-top--cell (plist-get quota :window) 15 '+agent-top-dim)))
    (if-let* ((note (plist-get quota :note)))
        (+agent-top--panel-line
         tool window
         (propertize (make-string 20 ?·) 'face '+agent-top-border) "      "
         (propertize note 'face '+agent-top-faint))
      (let* ((ratio (or (plist-get quota :utilization) 0.0))
             (resets (plist-get quota :resets-at))
             (updated (plist-get quota :updated-at)))
        (+agent-top--panel-line
         tool window
         (+agent-top--bar ratio 20) " "
         (+agent-top--cell (if (plist-get quota :utilization)
                                      (format "%.0f%%" (* 100 ratio))
                                    (format "%s" (or (plist-get quota :status) "?")))
                                  5 (+agent-top--level ratio) t)
         (propertize (if (and resets (> resets now))
                         (format "  resets %s" (+agent-top--duration (- resets now)))
                       "")
                     'face '+agent-top-dim)
         (propertize (if updated
                         (format "  · %s ago" (+agent-top--duration (- now updated)))
                       "")
                     'face '+agent-top-faint))))))

(defun +agent-top--insert-quota ()
  "Insert the Quota panel: every tool, every window it should have."
  (when (+agent-top--panel-open 'quota "quota")
    (let ((now (float-time)))
      (pcase-dolist (`(,tool . ,rows) (+agent-top--quotas))
        (let ((first t))
          (dolist (quota rows)
            (+agent-top--quota-line (and first tool) quota now)
            (setq first nil)))))
    (+agent-top--panel-close)))

(defconst +agent-top--columns
  '(("" 8) ("Session" 18) ("Project" 12) ("Agent" 8) ("Model" 14)
    ("Context" 16) ("In" 7 t) ("Out" 7 t) ("Cost" 7 t) ("Turn" 5 t)
    ("Cmp" 4 t) ("Idle" 6 t))
  "Sessions table columns: (TITLE WIDTH RIGHT-ALIGNED).")

(defconst +agent-top--drop-order '(10 3 2 9 11 7 8)
  "Column indexes dropped, in this order, when the window is too narrow.")

(defun +agent-top--layout ()
  "Return (INDEX WIDTH RIGHT) for the columns that fit the panel.
Columns go in `+agent-top--drop-order' until the rest fit; spare
width then widens Session and Model."
  (let* ((available (- (+agent-top--width) 4))
         (columns (cl-loop for (_title width right) in +agent-top--columns
                           for index from 0
                           collect (list index width right)))
         (used (lambda () (cl-loop for (_ width) in columns sum (1+ width))))
         (drop +agent-top--drop-order))
    (while (and drop (> (funcall used) available))
      (setq columns (cl-remove (pop drop) columns :key #'car)))
    (let ((spare (- available (funcall used))))
      (dolist (index '(1 4))
        (when-let* ((column (assq index columns))
                    ((> spare 0)))
          (let ((extra (min spare (if (= index 1) 12 8))))
            (cl-incf (nth 1 column) extra)
            (cl-decf spare extra)))))
    columns))

(defun +agent-top--row-cells (session usage)
  "Return the table cells for SESSION with USAGE."
  (let* ((buffer (plist-get session :buffer))
         (root (plist-get session :root))
         (state (+agent-top--state buffer))
         (used (plist-get usage :context-used))
         (size (plist-get usage :context-size))
         (ratio (if (> size 0) (/ (float used) size) 0.0))
         (last-used (plist-get session :last-used)))
    (list (cons (car state) (cdr state))
          (cons (or (plist-get session :name) (buffer-name buffer)) 'aaron-ui-board-row-title)
          (cons (if root (file-name-nondirectory (directory-file-name root)) "-")
                '+agent-top-dim)
          (cons (format "%s" (or (plist-get session :agent) "-")) '+agent-top-dim)
          (cons (or (plist-get usage :model) (plist-get usage :model-id) "-") nil)
          (if (> size 0)
              (concat (+agent-top--bar ratio 10)
                      (propertize (format " %3.0f%%" (* 100 ratio))
                                  'face (+agent-top--level ratio)))
            (cons (+agent-top--tokens used) '+agent-top-dim))
          (cons (+agent-top--tokens (plist-get usage :input)) nil)
          (cons (+agent-top--tokens (plist-get usage :output)) nil)
          (cons (+agent-top--cost (plist-get usage :cost) (plist-get usage :currency)) nil)
          (cons (number-to-string (plist-get usage :turns)) '+agent-top-dim)
          (let ((count (plist-get usage :compactions)))
            (cons (if (> count 0) (number-to-string count) "-")
                  (if (> count 0) 'aaron-ui-board-warn '+agent-top-dim)))
          (cons (if (numberp last-used)
                    (+agent-top--duration (- (float-time) last-used))
                  "-")
                '+agent-top-dim))))

(defun +agent-top--format-row (cells layout)
  "Lay out CELLS in the columns of LAYOUT, from `+agent-top--layout'."
  (mapconcat
   (pcase-lambda (`(,index ,width ,right))
     (let ((cell (nth index cells)))
       (if (stringp cell)
           ;; A pre-rendered cell (the context bar) is exact width.
           (concat cell (make-string (max 0 (- width (string-width cell))) ?\s))
         (+agent-top--cell (car cell) width (cdr cell) right))))
   layout " "))

(defun +agent-top--insert-sessions (rows)
  "Insert the Sessions panel for ROWS."
  (when (+agent-top--panel-open
         'sessions (format "sessions %d" (length rows))
         (if +agent-top--root
             (abbreviate-file-name +agent-top--root)
           "all projects"))
  (let ((layout (+agent-top--layout)))
    (+agent-top--panel-line
     (+agent-top--format-row
      (mapcar (lambda (column) (cons (car column) '+agent-top-dim))
              +agent-top--columns)
      layout))
    (if (null rows)
        (+agent-top--panel-line
         (propertize "No live agent sessions" 'face 'aaron-ui-board-empty))
      (pcase-dolist (`(,session . ,usage) rows)
        (let ((start (point))
              (buffer (plist-get session :buffer)))
          (+agent-top--panel-line
           (+agent-top--format-row
            (+agent-top--row-cells session usage) layout))
          (add-text-properties
           start (point)
           `(aaron-ui-board--item-id ,buffer
             aaron-ui-board--row-action ,(lambda (_) (+agent-top--visit buffer))
             mouse-face aaron-ui-board-row-highlight
             help-echo "RET: open session"
             keymap ,aaron-ui-board-row-map))))))
  (let ((input 0) (output 0) (costs nil))
    (pcase-dolist (`(,_ . ,usage) rows)
      (cl-incf input (plist-get usage :input))
      (cl-incf output (plist-get usage :output))
      (when-let* ((amount (plist-get usage :cost)))
        (let ((cell (assoc (plist-get usage :currency) costs)))
          (if cell (cl-incf (cdr cell) amount)
            (push (cons (plist-get usage :currency) amount) costs)))))
    (+agent-top--panel-line
     (propertize (format "total  in %s · out %s%s"
                         (+agent-top--tokens input)
                         (+agent-top--tokens output)
                         (mapconcat (lambda (cell)
                                      (concat " · " (+agent-top--cost (cdr cell) (car cell))))
                                    costs ""))
                 'face '+agent-top-dim)))
  (+agent-top--panel-close)))

(defun +agent-top--field (label value &optional face)
  "Return a detail field LABEL: VALUE."
  (concat (propertize (format "%-9s" label) 'face '+agent-top-dim)
          (propertize (format "%s" value) 'face (or face 'aaron-ui-board-row-title))))

(defun +agent-top--insert-detail (row)
  "Insert the Detail panel for ROW, a (SESSION . USAGE), or nothing."
  (when row
    (pcase-let* ((`(,session . ,usage) row)
                 (buffer (plist-get session :buffer))
                 (used (plist-get usage :context-used))
                 (size (plist-get usage :context-size))
                 (ratio (if (> size 0) (/ (float used) size) 0.0)))
      (when (+agent-top--panel-open
             'session
             (format "session %s" (or (plist-get session :name) (buffer-name buffer)))
             (format "%s · %s" (or (plist-get session :origin) "-")
                     (or (plist-get session :native-session-id) "no session id")))
      (+agent-top--panel-line
       (+agent-top--field "model" (or (plist-get usage :model)
                                             (plist-get usage :model-id) "-"))
       "   "
       (+agent-top--field "state" (car (+agent-top--state buffer))
                                 (cdr (+agent-top--state buffer))))
      (+agent-top--panel-line
       (+agent-top--field "root" (abbreviate-file-name
                                         (or (plist-get session :root) "-"))
                                 '+agent-top-faint))
      (+agent-top--panel-line
       (+agent-top--field "context" "")
       (+agent-top--bar ratio 30)
       (propertize (format " %s / %s" (+agent-top--tokens used)
                           (if (> size 0) (+agent-top--tokens size) "?"))
                   'face (+agent-top--level ratio))
       (propertize (format "   peak %s · %d compaction%s"
                           (+agent-top--tokens (plist-get usage :context-peak))
                           (plist-get usage :compactions)
                           (if (= 1 (plist-get usage :compactions)) "" "s"))
                   'face '+agent-top-dim))
      (+agent-top--panel-line
       (+agent-top--field
        "tokens"
        (format "in %s · out %s · thought %s · cache r %s w %s · total %s"
                (+agent-top--tokens (plist-get usage :input))
                (+agent-top--tokens (plist-get usage :output))
                (+agent-top--tokens (plist-get usage :thought))
                (+agent-top--tokens (plist-get usage :cached-read))
                (+agent-top--tokens (plist-get usage :cached-write))
                (+agent-top--tokens (plist-get usage :total)))))
      (+agent-top--panel-line
       (+agent-top--field
        "cache"
        (+agent-top--cache-description
         (+agent-top--cache-metrics usage))))
      (+agent-top--panel-line
       (+agent-top--field "cost" (+agent-top--cost (plist-get usage :cost)
                                                                 (plist-get usage :currency)))
       "   "
       (+agent-top--field "turns" (plist-get usage :turns))
       "   "
       (+agent-top--field "queued" (+agent-acp-pending-prompt-count buffer)))
      (+agent-top--panel-close)))))

(defconst +agent-top--key-hints
  '(("RET" . "open") ("s" . "stop") ("K" . "kill") ("x" . "close")
    ("R" . "restart") ("r" . "rename") ("C" . "compact") ("l" . "sessions")
    ("E" . "codex remote") ("P" . "claude remote") ("D" . "stop remote")
    ("TAB" . "fold") ("t" . "scope") ("g" . "refresh"))
  "Keys shown under the board.")

(defun +agent-top--insert-key-hints ()
  "Insert the key-hint line."
  (insert " "
          (mapconcat (pcase-lambda (`(,key . ,label))
                       (concat (propertize key 'face '+agent-top-key) " "
                               (propertize label 'face '+agent-top-dim)))
                     +agent-top--key-hints "  ")
          "\n"))

(defun +agent-top--render ()
  "Render the whole board into the current buffer."
  (+agent-top--apply-faces)
  (let* ((remote-root (get-text-property (point) '+agent-top--remote-root))
         (remote-start (and remote-root
                            (text-property-any (point-min) (point-max)
                                               '+agent-top--remote-root remote-root)))
         (remote-line (and remote-start
                           (- (line-number-at-pos (point))
                              (line-number-at-pos remote-start))))
         (remote-column (current-column))
         (rows (+agent-top--sessions))
         (id (get-text-property (point) 'aaron-ui-board--item-id))
         (current (or (assq id (mapcar (lambda (row) (cons (plist-get (car row) :buffer) row))
                                       rows))
                      (and rows (cons nil (car rows))))))
    (setq +agent-top--shown-id (and current (plist-get (cadr current) :buffer)))
    (aaron-ui-board-set-header
     "Agent usage" 'process
     (format "%d session%s · %d busy · %s"
             (length rows) (if (= 1 (length rows)) "" "s")
             (cl-count-if (lambda (row) (plist-get (car row) :busy)) rows)
             (format-time-string "%H:%M:%S")))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header
        "Usage & quotas" :icon 'chart
        :subtitle "Session usage, reported quotas and remote controls"
        :stats (list (cons (format "%d sessions" (length rows)) 'info)
                     (cons (format "%d working" (cl-count-if
                                                 (lambda (row) (plist-get (car row) :busy)) rows)) 'muted)))
       (+agent-top--insert-quota)
       (+agent-top--insert-remote)
       (+agent-top--insert-cache rows)
       (+agent-top--insert-sessions rows)
       (+agent-top--insert-detail (cdr current))
       (+agent-top--insert-key-hints)))
    (when remote-root
      (when-let* ((start (text-property-any
                          (point-min) (point-max)
                          '+agent-top--remote-root remote-root)))
        (goto-char start)
        (forward-line (or remote-line 0))
        (move-to-column remote-column)))
    (unless (or (get-text-property (point) 'aaron-ui-board--item-id)
                (get-text-property (point) '+agent-top--remote-root)
                (get-text-property (point) 'keymap))
      (aaron-ui-board--goto-item-id +agent-top--shown-id))))

(defun +agent-top-refresh ()
  "Re-render the board."
  (interactive)
  (when (derived-mode-p '+agent-top-mode)
    (+agent-top--render)))

(defun +agent-top-reload ()
  "Re-render the board, first asking for the Claude quota if it is stale."
  (interactive)
  (+agent-top--claude-fetch)
  (+agent-top--codex-remote-probe)
  (+agent-top-refresh))

(defun +agent-top--follow-point ()
  "Re-render when point moves to another session, so the detail follows it."
  (let ((id (get-text-property (point) 'aaron-ui-board--item-id)))
    (when (and id (not (eq id +agent-top--shown-id)))
      (+agent-top--render))))

;;; Event-driven refresh

(defun +agent-top--visible-window (board)
  "Return a window on a visible frame showing BOARD, or nil."
  (get-buffer-window board 'visible))

(defun +agent-top--session-changed (_buffer)
  "Schedule one render of the board after a session changed.
A board nobody can see stops listening until it is shown again (see
`+agent-top--shown'), so a hidden board costs nothing."
  (let ((board (get-buffer +agent-top--buffer-name)))
    (cond ((not (and board (+agent-top--visible-window board)))
           (remove-hook '+agent-acp-changed-functions
                        #'+agent-top--session-changed))
          ((not +agent-top--refresh-timer)
           (setq +agent-top--refresh-timer
                 (run-with-timer +agent-top--refresh-delay nil
                                 #'+agent-top--timer-fire))))))

(defun +agent-top--shown (_window)
  "The board appeared in a window: listen again and catch up once."
  (unless (memq #'+agent-top--session-changed +agent-acp-changed-functions)
    (add-hook '+agent-acp-changed-functions #'+agent-top--session-changed)
    (+agent-top--session-changed nil)))

(defun +agent-top--timer-fire ()
  "Render the board once for the usage reports gathered since scheduling."
  (setq +agent-top--refresh-timer nil)
  (when-let* ((board (get-buffer +agent-top--buffer-name)))
    ;; Render in the board's window so point and window point agree and
    ;; the row under the cursor survives the redraw.
    (if-let* ((window (+agent-top--visible-window board)))
        (with-selected-window window (+agent-top-refresh))
      (with-current-buffer board (+agent-top-refresh)))))

(defun +agent-top--teardown ()
  "Stop listening for session changes when the board buffer goes."
  (remove-hook '+agent-acp-changed-functions #'+agent-top--session-changed)
  (when (timerp +agent-top--refresh-timer)
    (cancel-timer +agent-top--refresh-timer))
  (setq +agent-top--refresh-timer nil))

;;; Actions

(defun +agent-top--session-at-point ()
  "Return the session on this line, else the one in the detail panel."
  (when (get-text-property (point) '+agent-top--remote-root)
    (user-error "This is a Claude Remote project; use RET to open or D to stop it"))
  (let ((buffer (or (get-text-property (point) 'aaron-ui-board--item-id)
                    +agent-top--shown-id)))
    (unless (buffer-live-p buffer)
      (user-error "No live agent session; press g to refresh"))
    buffer))

(defmacro +agent-top--define-action (name doc &rest body)
  "Define board command NAME with DOC running BODY with `buffer' bound."
  (declare (indent 2) (doc-string 2))
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((buffer (+agent-top--session-at-point)))
       ,@body
       (+agent-top-refresh))))

(defun +agent-top--visit (buffer)
  "Show agent BUFFER."
  (unless (buffer-live-p buffer)
    (user-error "That session has ended; press g to refresh"))
  (+agent-acp-show-buffer buffer))

(defun +agent-top-visit ()
  "Show the Claude Remote console or agent session on this line."
  (interactive)
  (if-let* ((directory (get-text-property (point) '+agent-top--remote-root)))
      (let ((result (gethash directory +agent-top--claude-remote-errors)))
        (if (and (listp result) (buffer-live-p (plist-get result :buffer)))
            (pop-to-buffer (plist-get result :buffer))
          (+agent-top-claude-remote-console directory)))
    (+agent-top--visit (+agent-top--session-at-point))))

(defun +agent-top-mouse-visit (event)
  "Open a Claude Remote row clicked with EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when (get-text-property (point) '+agent-top--remote-root)
    (+agent-top-visit)))

(+agent-top--define-action +agent-top-stop
    "Stop the session's current work: cancel its Run, else interrupt its turn."
  (+agent-acp-stop buffer))

(+agent-top--define-action +agent-top-kill
    "Force-terminate the session on this line: its process and buffer go."
  (when (yes-or-no-p (format "Kill agent session %s%s? "
                             (buffer-name buffer)
                             (if (+agent-acp-busy-p buffer) " (busy)" "")))
    (+agent-acp-kill buffer)))

(+agent-top--define-action +agent-top-close
    "Close the session on this line; its name and history remain."
  (+agent-acp-close buffer))

(+agent-top--define-action +agent-top-restart
    "Restart the session on this line."
  (require 'tools/agent/sessions)
  (+agent-session-restart buffer))

(+agent-top--define-action +agent-top-rename
    "Rename the session on this line."
  (require 'tools/agent/sessions)
  (+agent-session-rename buffer))

(+agent-top--define-action +agent-top-compact
    "Roll the session on this line over to its latest handoff at its next Run."
  (require 'tools/agent/sessions)
  (+agent-session-compact buffer))

(defun +agent-top-sessions ()
  "Open the session list of the project of the session on this line."
  (interactive)
  (require 'tools/agent/sessions)
  (+agent-session-list (+agent-top--session-at-point)))

(defun +agent-top-toggle-scope ()
  "Toggle between every live session and those of one project."
  (interactive)
  (setq +agent-top--root
        (unless +agent-top--root
          (let ((buffer (get-text-property (point) 'aaron-ui-board--item-id)))
            (or (and (buffer-live-p buffer)
                     (buffer-local-value '+agent-acp-session-root buffer))
                (+agent-acp-project-root default-directory)))))
  (+agent-top-refresh))

(defun +agent-top-toggle-panel (&optional panel)
  "Fold or unfold PANEL, by default the one whose title is on this line.
Elsewhere, move to the next session row."
  (interactive)
  (if-let* ((panel (or panel (get-text-property (point) '+agent-top--panel))))
      (progn
        (setq +agent-top--folded
              (if (memq panel +agent-top--folded)
                  (delq panel +agent-top--folded)
                (cons panel +agent-top--folded)))
        (+agent-top-refresh))
    (+agent-top-next-session)))

(defun +agent-top-next-session ()
  "Move to the next session row, wrapping to the first."
  (interactive)
  (let* ((here (get-text-property (point) 'aaron-ui-board--item-id))
         (next (or (text-property-not-all
                    (or (next-single-property-change (point) 'aaron-ui-board--item-id)
                        (point-max))
                    (point-max) 'aaron-ui-board--item-id nil)
                   (text-property-not-all (point-min) (point-max)
                                          'aaron-ui-board--item-id nil))))
    (when (and next (not (eq here (get-text-property next 'aaron-ui-board--item-id))))
      (goto-char next))))

(defvar-keymap +agent-top-mode-map
  :parent aaron-ui-board-mode-map
  "RET" #'+agent-top-visit
  "<return>" #'+agent-top-visit
  "o" #'+agent-top-visit
  "s" #'+agent-top-stop
  "K" #'+agent-top-kill
  "x" #'+agent-top-close
  "R" #'+agent-top-restart
  "r" #'+agent-top-rename
  "C" #'+agent-top-compact
  "l" #'+agent-top-sessions
  "E" #'+agent-top-codex-remote-toggle
  "P" #'+agent-top-claude-remote-start
  "D" #'+agent-top-claude-remote-stop
  "<mouse-1>" #'+agent-top-mouse-visit
  "t" #'+agent-top-toggle-scope
  "TAB" #'+agent-top-toggle-panel
  "<tab>" #'+agent-top-toggle-panel
  "1" (lambda () (interactive) (+agent-top-toggle-panel 'quota))
  "2" (lambda () (interactive) (+agent-top-toggle-panel 'sessions))
  "3" (lambda () (interactive) (+agent-top-toggle-panel 'session))
  "4" (lambda () (interactive) (+agent-top-toggle-panel 'remote))
  "5" (lambda () (interactive) (+agent-top-toggle-panel 'cache)))

(define-derived-mode +agent-top-mode aaron-ui-board-mode "Agent-Top"
  "Live agent sessions: quota, model, state, context, tokens, cost.
\\{+agent-top-mode-map}"
  (setq-local aaron-ui-board-refresh-function #'+agent-top-reload)
  (add-hook 'window-buffer-change-functions #'+agent-top--shown nil t)
  (setq-local truncate-lines t)
  (add-hook 'post-command-hook #'+agent-top--follow-point nil t)
  (add-hook 'kill-buffer-hook #'+agent-top--teardown nil t)
  (add-hook '+agent-acp-changed-functions #'+agent-top--session-changed))

;;;###autoload
(defun +agent-top-report (&optional project)
  "Show every live agent session: quota, model, context, tokens and cost.
With prefix argument PROJECT, show only the current project's sessions."
  (interactive "P")
  (let ((opening-root (+agent-acp-project-root default-directory))
        (root (and project (+agent-acp-project-root default-directory)))
        (buffer (get-buffer-create +agent-top--buffer-name)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p '+agent-top-mode)
        (+agent-top-mode))
      (setq +agent-top--root root
            +agent-top--default-root opening-root)
      (+agent-top--claude-fetch)
      (+agent-top--codex-remote-probe)
      (+agent-top--render)
      (aaron-ui-board--goto-item-id +agent-top--shown-id))))

;;;###autoload
(defun +agent-top (&optional project)
  "Open the shared usage view; use the diagnostic report in a terminal."
  (interactive "P")
  (if (display-graphic-p)
      (progn (require 'tools/agent/manager) (+agent-manager-show 'usage project))
    (+agent-top-report project)))

(defun +agent-top--stop-claude-on-exit ()
  "Stop Claude Remote processes started by abtop before Emacs exits."
  (let (processes)
    (maphash (lambda (_directory process)
               (when (process-live-p process)
                 (push process processes)))
             +agent-top--claude-remotes)
    (dolist (process processes)
      (process-put process '+agent-top--stopping t)
      (condition-case nil
          (interrupt-process process)
        (error (delete-process process))))
    (let ((deadline (+ (float-time) 3)))
      (while (and (cl-some #'process-live-p processes)
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (dolist (process processes)
      (when (process-live-p process)
        (delete-process process)))
    (clrhash +agent-top--claude-remotes)))

(defun +agent-top--run-before-exit (command)
  "Run COMMAND before Emacs exits, waiting at most five seconds."
  (let ((process (make-process :name "noema-remote-restore"
                               :command command :noquery t))
        (deadline (+ (float-time) 5)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.05))
    (if (process-live-p process)
        (progn (delete-process process) nil)
      (and (eq (process-status process) 'exit)
           (zerop (process-exit-status process))))))

(defun +agent-top--restore-codex-on-exit ()
  "Restore Codex Remote's original enabled state if abtop changed it."
  (when-let* ((process +agent-top--codex-remote-process))
    (when (process-live-p process)
      (set-process-sentinel process #'ignore)
      (delete-process process))
    (setq +agent-top--codex-remote-process nil))
  (when (and +agent-top--codex-remote-owned
             (memq +agent-top--codex-remote-baseline '(enabled disabled))
             (not (eq +agent-top--codex-remote-baseline
                      +agent-top--codex-remote-last-requested)))
    (unless (+agent-top--run-before-exit
             (+agent-top--codex-remote-command
              (if (eq +agent-top--codex-remote-baseline 'enabled)
                  'enable 'disable)))
      (message "abtop: Codex Remote could not be restored before Emacs exit"))))

(defun +agent-top--shutdown ()
  "Release abtop's Remote Control changes before Emacs exits."
  (unless +agent-top--shutting-down
    (setq +agent-top--shutting-down t)
    (condition-case err
        (+agent-top--stop-claude-on-exit)
      (error (message "abtop: Claude Remote shutdown failed: %s"
                      (error-message-string err))))
    (condition-case err
        (+agent-top--restore-codex-on-exit)
      (error (message "abtop: Codex Remote restore failed: %s"
                      (error-message-string err))))))

(add-hook 'kill-emacs-hook #'+agent-top--shutdown)

(provide 'tools/agent/top)
;;; +agent-top.el ends here

;;; init-note-graph.el --- Interactive Typst note graph -*- lexical-binding: t; -*-

;;; Commentary:
;; Generate local graph data for the shared D3 graph frontend and display
;; it inside an xwidget-webkit buffer.  Node clicks are routed back to
;; Emacs via a localhost WebSocket bridge so they open as ordinary
;; buffers instead of navigating the webview.

;;; Code:

(require 'init-note)
(require 'json)
(require 'subr-x)
(require 'url-util)

(declare-function websocket-server "websocket" (port &rest plist))
(declare-function websocket-server-close "websocket" (conn))
(declare-function websocket-frame-payload "websocket" (frame))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-goto-url "xwidget" (url))
(declare-function xwidget-webkit-uri "xwidget" (xwidget))

(defcustom my/note-graph-file
  (expand-file-name
   "note-graph.html"
   (if (boundp 'my/state-dir)
       (expand-file-name "note" my/state-dir)
     (expand-file-name "var/note" user-emacs-directory)))
  "Generated HTML file used by `my/note-graph'."
  :type 'file
  :group 'my/note)

(defcustom my/note-graph-asset-directory
  (locate-user-emacs-file "lisp/note/assets")
  "Directory containing symlinked shared site graph assets."
  :type 'directory
  :group 'my/note)

(defvar my/note-graph--ws-server nil
  "Active websocket server process for the note graph, or nil.")

(defvar my/note-graph--ws-port nil
  "Port the note graph websocket server is listening on.")

(defvar my/note-graph--xwidget-buffer nil
  "Buffer that currently hosts the note graph xwidget session, or nil.")

(defvar-local my/note-graph--buffer-owned nil
  "Non-nil when the current xwidget buffer is owned by note graph.")

(defun my/note-graph--asset-file (relative)
  "Return graph asset RELATIVE below `my/note-graph-asset-directory'."
  (expand-file-name relative
                    (file-name-as-directory my/note-graph-asset-directory)))

(defun my/note-graph--asset-url (relative)
  "Return file URL for graph asset RELATIVE."
  (url-encode-url
   (concat "file://" (expand-file-name (my/note-graph--asset-file relative)))))

(defun my/note-graph--file-url (file)
  "Return a browser URL for local FILE."
  (url-encode-url (concat "file://" (file-truename file))))

(defun my/note-graph--group-key (file)
  "Return the graph group key for note FILE."
  (let ((relative (file-relative-name
                   (file-truename file)
                   (file-name-as-directory (file-truename my/note-root)))))
    (or (directory-file-name (file-name-directory relative))
        "Root")))

(defun my/note-graph--group-label (group-key)
  "Return display label for GROUP-KEY."
  (if (or (null group-key) (string= group-key "Root"))
      "Root"
    (let* ((parts (split-string group-key "/" t))
           (leaf (car (last parts))))
      (if (string-match-p "\\`[A-Z0-9-]+\\'" leaf)
          leaf
        (mapconcat #'capitalize (split-string leaf "[-_]" t) " ")))))

(defun my/note-graph--section-name (group-key)
  "Return top-level graph section for GROUP-KEY."
  (if (or (null group-key) (string= group-key "Root"))
      "Root"
    (car (split-string group-key "/" t))))

(defun my/note-graph--clean-search-text ()
  "Return a compact searchable text string for the current Typst buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#metadata[ \t\n]*((" nil t)
      (let ((start (match-beginning 0)))
        (when (search-forward "))" nil t)
          (when (looking-at "[ \t\n]*<note>")
            (goto-char (match-end 0)))
          (delete-region start (point)))))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\(?:#import\\|#show:\\|#set\\)[^\n]*" nil t)
      (replace-match " " t t))
    (let ((text (buffer-string)))
      (setq text (replace-regexp-in-string
                  "#note[ \t\n]*(\"[^\"]+\")[ \t\n]*\\[\\([^]\n]+\\)\\]"
                  "\\2" text))
      (setq text (replace-regexp-in-string "//.*" " " text))
      (setq text (replace-regexp-in-string "[#=*$`_{}()\\[\\],]" " " text))
      (setq text (replace-regexp-in-string "[ \t\n\r]+" " " text))
      (string-trim text))))

(defun my/note-graph--search-text (file)
  "Return searchable full text for Typst note FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (my/note-graph--clean-search-text)))

(defun my/note-graph--site-data ()
  "Return note index data using the published site's SITE_DATA schema."
  (let* ((node-rows
          (my/note--rows
           "select n.id,
                   n.file,
                   n.title,
                   coalesce(n.date, ''),
                   coalesce(n.summary, ''),
                   coalesce((select group_concat(tag, ',')
                             from tags where node_id = n.id), ''),
                   coalesce((select group_concat(alias, char(31))
                             from aliases where node_id = n.id), '')
            from nodes n
            order by lower(n.title), n.file"))
         (link-rows
          (my/note--rows
           "select source_id, target_id from links order by source_id, target_id"))
         (refs-by-source (make-hash-table :test 'equal))
         (backlinks-by-target (make-hash-table :test 'equal))
         (tag-set (make-hash-table :test 'equal))
         (alias-set (make-hash-table :test 'equal)))
    (dolist (row link-rows)
      (pcase-let ((`(,source ,target) row))
        (unless (string= source target)
          (puthash source
                   (cons target (gethash source refs-by-source))
                   refs-by-source)
          (puthash target
                   (cons source (gethash target backlinks-by-target))
                   backlinks-by-target))))
    (let ((notes
           (mapcar
            (lambda (row)
              (pcase-let* ((`(,id ,file ,title ,date ,summary ,tags-raw ,aliases-raw) row)
                           (tags (if (and tags-raw
                                          (not (string-empty-p tags-raw)))
                                     (sort (split-string tags-raw "," t)
                                           #'string<)
                                   nil))
                           (aliases (if (and aliases-raw
                                             (not (string-empty-p aliases-raw)))
                                        (sort (split-string aliases-raw "\x1f" t)
                                              #'string<)
                                      nil))
                           (relative-path
                            (file-relative-name
                             (file-truename file)
                             (file-name-as-directory
                              (file-truename my/note-root))))
                           (group-key (my/note-graph--group-key file)))
                (dolist (tag tags)
                  (puthash tag t tag-set))
                (dolist (alias aliases)
                  (puthash alias t alias-set))
                `(("key" . ,id)
                  ("id" . ,id)
                  ("title" . ,title)
                  ("link" . ,(my/note-graph--file-url file))
                  ("path" . ,relative-path)
                  ("date" . ,date)
                  ("summary" . ,summary)
                  ("searchText" . ,(my/note-graph--search-text file))
                  ("groupKey" . ,group-key)
                  ("groupLabel" . ,(my/note-graph--group-label group-key))
                  ("section" . ,(my/note-graph--section-name group-key))
                  ("hidden" . :json-false)
                  ("tags" . ,(vconcat tags))
                  ("aliases" . ,(vconcat aliases))
                  ("refs" . ,(vconcat (sort (delete-dups
                                             (gethash id refs-by-source))
                                            #'string<)))
                  ("backlinks" . ,(vconcat (sort (delete-dups
                                                  (gethash id backlinks-by-target))
                                                 #'string<))))))
            node-rows)))
      `(("meta" . (("generatedAt" . ,(format-time-string "%Y-%m-%d %H:%M:%S %z"))
                   ("noteCount" . ,(length notes))
                   ("tagCount" . ,(hash-table-count tag-set))
                   ("aliasCount" . ,(hash-table-count alias-set))))
        ("notes" . ,(vconcat notes))))))

(defun my/note-graph--write-site-data (directory)
  "Write note graph SITE_DATA into DIRECTORY/js/data.js."
  (let ((file (expand-file-name "js/data.js" directory))
        (json-encoding-pretty-print nil))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "const SITE_DATA = ")
      (insert (json-encode (my/note-graph--site-data)))
      (insert ";\n"))
    file))

(defun my/note-graph--open-note-id (id)
  "Open the Typst note with ID inside Emacs."
  (if-let* ((node (my/note--node-by-id id))
            (file (plist-get node :file)))
      (let ((buf (find-file-noselect file)))
        (pop-to-buffer buf)
        (select-frame-set-input-focus (selected-frame)))
    (message "note-graph: unknown note id %s" id)))

(defun my/note-graph--ws-on-message (_ws frame)
  "Handle a WebSocket FRAME from the note graph frontend."
  (let ((payload (websocket-frame-payload frame)))
    (when (and payload (stringp payload))
      (condition-case err
          (let* ((msg (json-parse-string payload :object-type 'alist))
                 (type (alist-get 'type msg))
                 (id (alist-get 'id msg)))
            (cond
             ((and (equal type "open") (stringp id))
              (run-at-time 0 nil #'my/note-graph--open-note-id id))))
        (error (message "note-graph ws decode error: %s" err))))))

(defun my/note-graph--ws-ensure ()
  "Start the note graph websocket server if needed; return port."
  (require 'websocket)
  (unless (and my/note-graph--ws-server
               (process-live-p my/note-graph--ws-server))
    (setq my/note-graph--ws-server
          (websocket-server
           0
           :host 'local
           :on-message #'my/note-graph--ws-on-message))
    (setq my/note-graph--ws-port
          (process-contact my/note-graph--ws-server :service)))
  my/note-graph--ws-port)

(defun my/note-graph--ws-shutdown ()
  "Stop the note graph websocket server."
  (when (and my/note-graph--ws-server
             (process-live-p my/note-graph--ws-server))
    (ignore-errors (websocket-server-close my/note-graph--ws-server)))
  (setq my/note-graph--ws-server nil
        my/note-graph--ws-port nil))

(add-hook 'kill-emacs-hook #'my/note-graph--ws-shutdown)

(defun my/note-graph--buffer-kill-h ()
  "Clean up note graph process state when its xwidget buffer is killed."
  (when my/note-graph--buffer-owned
    (setq my/note-graph--xwidget-buffer nil)
    (my/note-graph--ws-shutdown)))

(defun my/note-graph--html (ws-port)
  "Return standalone HTML wired to WS-PORT for the note graph."
  (let ((retro-css (my/note-graph--asset-url "css/retro.css"))
        (knowledge-js (my/note-graph--asset-url "js/knowledge.js"))
        (graph-js (my/note-graph--asset-url "js/graph.js")))
    (format
     "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
  <title>Note Graph</title>
  <script>window.__GRAPH_NO_AUTO_INIT__ = true;</script>
  <link rel=\"stylesheet\" href=\"%s\" />
  <script src=\"https://d3js.org/d3.v7.min.js\"></script>
  <style>
    html, body {
      margin: 0; padding: 0; height: 100%%;
      overflow: hidden;
    }
    #graph-container {
      position: fixed; inset: 16px;
      width: auto; height: auto; min-height: 0;
      overflow: hidden;
    }
  </style>
</head>
<body>
  <div id=\"graph-container\" data-graph-toolbar=\"true\" aria-label=\"Interactive knowledge graph\"></div>
  <script src=\"js/data.js\"></script>
  <script src=\"%s\"></script>
  <script src=\"%s\"></script>
  <script>
    (function () {
      var port = %d;
      var ws = null;
      var queue = [];
      var retryTimer = 0;
      function connect() {
        try {
          ws = new WebSocket(\"ws://127.0.0.1:\" + port + \"/\");
        } catch (e) {
          retryTimer = window.setTimeout(connect, 1500);
          return;
        }
        ws.addEventListener(\"open\", function () {
          while (queue.length) { ws.send(queue.shift()); }
        });
        ws.addEventListener(\"close\", function () {
          retryTimer = window.setTimeout(connect, 1500);
        });
        ws.addEventListener(\"error\", function () {
          try { ws.close(); } catch (e) {}
        });
      }
      function send(payload) {
        var msg = JSON.stringify(payload);
        if (ws && ws.readyState === 1) { ws.send(msg); }
        else { queue.push(msg); }
      }
      connect();
      document.addEventListener(\"DOMContentLoaded\", function () {
        window.initKnowledgeGraph({
          toolbar: true,
          onNoteOpen: function (note) {
            send({ type: \"open\", id: note.id || note.key });
          }
        });
      });
    })();
  </script>
</body>
</html>
"
     retro-css
     knowledge-js
     graph-js
     ws-port)))

(defun my/note-graph-kill ()
  "Kill the note graph xwidget buffer and shut down its websocket server."
  (interactive)
  (my/note-graph--ws-shutdown)
  (let* ((buffer (or (and (buffer-live-p my/note-graph--xwidget-buffer)
                          my/note-graph--xwidget-buffer)
                     (current-buffer)))
         (window (get-buffer-window buffer t)))
    (setq my/note-graph--xwidget-buffer nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (when (and (window-live-p window)
               (not (one-window-p t)))
      (ignore-errors (delete-window window)))))

(defun my/note-graph--setup-xwidget-buffer ()
  "Install note graph local keys and cleanup hooks in the current buffer."
  (setq-local my/note-graph--buffer-owned t)
  (local-set-key (kbd "M-w") #'my/note-graph-kill)
  (add-hook 'kill-buffer-hook #'my/note-graph--buffer-kill-h nil t))

(defun my/note-graph--open-in-xwidget (url)
  "Display URL inside the dedicated note graph xwidget buffer."
  (require 'xwidget)
  (unless (featurep 'xwidget-internal)
    (user-error "This Emacs build has no xwidget support"))
  (let ((buf my/note-graph--xwidget-buffer))
    (cond
     ((and buf (buffer-live-p buf))
      (with-current-buffer buf
        (my/note-graph--setup-xwidget-buffer)
        (xwidget-webkit-goto-url url))
      (pop-to-buffer buf))
     (t
      (xwidget-webkit-browse-url url t)
      (my/note-graph--setup-xwidget-buffer)
      (setq my/note-graph--xwidget-buffer (current-buffer))))))

;;;###autoload
(defun my/note-graph ()
  "Generate the note graph and display it in xwidget-webkit."
  (interactive)
  (my/note--ensure-db)
  (let ((directory (file-name-directory my/note-graph-file))
        (port (my/note-graph--ws-ensure)))
    (make-directory directory t)
    (my/note-graph--write-site-data directory)
    (with-temp-file my/note-graph-file
      (insert (my/note-graph--html port)))
    (my/note-graph--open-in-xwidget
     (concat "file://" my/note-graph-file))
    (message "Note graph ready (ws://127.0.0.1:%d)" port)))

(provide 'init-note-graph)
;;; init-note-graph.el ends here

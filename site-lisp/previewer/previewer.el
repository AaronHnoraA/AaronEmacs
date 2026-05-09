;;; previewer.el ---  preview text file.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2025 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2") (websocket "1.9"))
;; URL: https://github.com/honmaple/emacs-maple-preview


;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; preview text file.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'web-server)
(require 'websocket)

(declare-function org-export-as 'ox-html)
(declare-function org-element-at-point 'org-element)
(declare-function org-element-property 'org-element)
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))

(defgroup previewer nil
  "Org mode, Markdown or HTML realtime Preview."
  :group 'text
  :prefix "previewer-")

(defcustom previewer-host "127.0.0.1"
  "Preview http host."
  :type 'string
  :group 'previewer)

(defcustom previewer-port t
  "Preview http port, t means auto select unused port."
  :type 'integer
  :group 'previewer)

(defcustom previewer-delay 0.8
  "Idle delay before re-rendering preview content after edits."
  :type 'float
  :group 'previewer)

(defcustom previewer-auto-update t
  "Auto update when preview."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-auto-scroll t
  "Auto scroll when preview."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-auto-browser t
  "Auto open browser."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-browser-backend 'xwidget
  "Browser backend used by `previewer-open-browser'.
Use `xwidget' for an Emacs-local preview pane, or `system' for the default
external browser."
  :type '(choice (const :tag "xwidget-webkit" xwidget)
                 (const :tag "System browser" system))
  :group 'previewer)

(defcustom previewer-window-side 'auto
  "Side used for the Emacs-local preview window.
When set to `auto', wide windows split left/right with preview on the left,
and narrow/tall windows split above/below with preview above the source."
  :type '(choice (const auto) (const left) (const right) (const above) (const below))
  :group 'previewer)

(defcustom previewer-window-size 0.42
  "Window size ratio used by `previewer-open-browser' with xwidget."
  :type 'number
  :group 'previewer)

(defcustom previewer-wide-window-min-ratio 1.25
  "Minimum width/height ratio used for left/right Previewer splits."
  :type 'number
  :group 'previewer)

(defcustom previewer-sync-scroll-from-browser nil
  "When non-nil, scrolling the preview asks Emacs to reveal the source line.
The low-power workbench default is nil: source navigation still drives the
preview, while preview-to-source navigation is reserved for modified clicks."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-org-source-map t
  "When non-nil, send source anchors for Org preview synchronization."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-org-incremental-render t
  "When non-nil, update Org previews by replacing the edited heading chunk."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-org-incremental-heading-level 2
  "Maximum Org heading level used as a live-preview incremental chunk boundary."
  :type 'integer
  :group 'previewer)

(defcustom previewer-vendor-auto-update t
  "When non-nil, refresh bundled preview assets when they are stale."
  :type 'boolean
  :group 'previewer)

(defcustom previewer-vendor-update-interval-days 21
  "Number of days after which bundled preview assets are refreshed."
  :type 'integer
  :group 'previewer)

(defcustom previewer-katex-version "0.16.9"
  "KaTeX version bundled by Previewer."
  :type 'string
  :group 'previewer)

(defcustom previewer-styles
  '("/preview/static/css/markdown.css"
    "/preview/static/css/highlight.css"
    "/preview/static/css/previewer-org.css"
    "/preview/static/vendor/katex/katex.min.css")
  "Custom preview css style."
  :type 'list
  :group 'previewer)

(defcustom previewer-scripts
  '("/preview/static/js/jquery.min.js"
    "/preview/static/js/marked.min.js"
    "/preview/static/js/marked-highlight.min.js"
    "/preview/static/js/highlight.min.js"
    "/preview/static/js/mermaid.min.js"
    "/preview/static/vendor/katex/katex.min.js"
    "<script>
window.MathJax = {
  tex: {
    inlineMath: [['\\\\(', '\\\\)']],
    displayMath: [['\\\\[', '\\\\]'], ['$$', '$$']],
    processEscapes: true
  },
  options: { skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code'] }
};
</script>"
    "/preview/static/vendor/mathjax/tex-mml-chtml.js")
  "Custom preview js script."
  :type 'list
  :group 'previewer)

(defcustom previewer-render-alist
  '((org-mode . previewer-org-html-content)
    (web-mode . previewer-raw-content)
    (html-mode . previewer-raw-content)
    (html-ts-mode . previewer-raw-content)
    (mhtml-mode . previewer-raw-content)
    (vue-html-mode . previewer-raw-content)
    (t . previewer-markdown-content))
  "How to preview text, export to markdown or html."
  :type '(alist :key-type symbol :value-type function)
  :group 'previewer)

(defcustom previewer-render-modes
  '(org-mode markdown-mode markdown-ts-mode html-mode html-ts-mode mhtml-mode web-mode vue-html-mode)
  "Allow preview modes."
  :type 'list
  :group 'previewer)

(defcustom previewer-auto-hook nil
  "Hook for user specified auto preview instance.

This hook run within the procedure of `previewer-init' when
customized variable `previewer-auto-update' was non-nil.

The internal auto-preview type transferred
`previewer-send-to-server' to the `post-self-insert-hook',
this hook providing more customization functional for as."
  :type 'hook
  :group 'previewer)

(defcustom previewer-finialize-hook nil
  "Hooks for run with `previewer-finalize'.
It's useful to remove all dirty hacking with `previewer-auto-hook'."
  :type 'hook
  :group 'previewer)

(defvar previewer-server nil
  "`previewer' http server.")
(defvar previewer-websocket nil)
(defvar previewer-sending nil)
(defvar previewer-sync-sending nil)
(defvar previewer--content-timer nil)
(defvar previewer-source-buffer nil)
(defvar previewer--last-sync-key nil)
(defvar previewer--xwidget-buffer nil)
(defvar previewer--vendor-update-started nil)
(defvar-local previewer--buffer-hooks-installed nil)

(defvar previewer-home-path (file-name-directory load-file-name))
(defvar previewer-index-file (concat previewer-home-path "index.html"))
(defvar previewer-katex-vendor-path
  (expand-file-name "static/vendor/katex" previewer-home-path))
(defvar previewer-mathjax-vendor-path
  (expand-file-name "static/vendor/mathjax" previewer-home-path))
(defvar-local previewer--patch-timer nil)
(defvar-local previewer--changed-beg nil)
(defvar-local previewer--changed-end nil)
(defvar-local previewer--org-full-rendered nil)
(defvar previewer-org--export-source-buffer nil)
(defvar previewer-org--export-source-offset 0)
(defvar previewer-org--source-advice-installed nil)

(defun previewer-mime-type(path)
  "Guess mime type from PATH."
  (let ((mime (mm-default-file-type path)))
    (if (and (not mime) (string-suffix-p ".js" path))
        "application/javascript"
      mime)))

(defun previewer-position-percent ()
  "Preview position percent."
  (when previewer-auto-scroll
    (format
     "<div id=\"position-percentage\" style=\"display:none;\">%s</div>\n"
     (number-to-string
      (truncate (* 100 (/ (float (-  (line-number-at-pos) (/ (count-screen-lines (window-start) (point)) 2)))
                          (count-lines (point-min) (point-max)))))))))

(defun previewer-css-template ()
  "Css Template."
  (mapconcat
   (lambda (x)
     (if (string-match-p "^[\n\t ]*<style" x) x
       (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">" x)))
   previewer-styles "\n"))

(defun previewer-js-template ()
  "Css Template."
  (mapconcat
   (lambda (x)
     (if (string-match-p "^[\n\t ]*<script" x) x
       (format "<script src=\"%s\"></script>" x)))
   previewer-scripts "\n"))

(defun previewer-template ()
  "Template."
  (with-temp-buffer
    (insert-file-contents previewer-index-file)
    (when (search-forward "{{ css }}" nil t)
      (replace-match (previewer-css-template) t))
    (when (search-forward "{{ js }}" nil t)
      (replace-match (previewer-js-template) t))
    (when (search-forward "{{ websocket }}" nil t)
      (replace-match (previewer-listen) t))
    (buffer-string)))

(defun previewer-html-content ()
  "Get file html content."
  (concat (cond ((memq major-mode '(org-mode markdown-mode))
                 (unless (featurep 'ox-html) (require 'ox-html))
                 (let ((org-html-postamble nil)
                       (org-html-head-include-default-style nil)
                       (org-html-head-include-scripts nil)
                       (org-html-htmlize-output-type 'css))
                   (ignore org-html-postamble)
                   (org-export-as 'html)))
                (t (buffer-substring-no-properties (point-min) (point-max))))
          "<!-- iframe -->"))

(defun previewer-markdown-content ()
  "Get file markdown content."
  (cond ((eq major-mode 'org-mode)
         (unless (featurep 'ox-md) (require 'ox-md))
         (org-export-as 'md))
        ((memq major-mode '(web-mode html-mode html-ts-mode mhtml-mode vue-html-mode))
         (concat (buffer-substring-no-properties (point-min) (point-max)) "<!-- iframe -->"))
        (t (buffer-substring-no-properties (point-min) (point-max)))))

(defun previewer-raw-content ()
  "Return the current buffer as plain preview content."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun previewer--html-mode-p ()
  "Return non-nil when the current buffer should be rendered as HTML."
  (memq major-mode '(web-mode html-mode html-ts-mode mhtml-mode vue-html-mode)))

(defun previewer--line-at-pos (pos)
  "Return 1-based line number at POS."
  (line-number-at-pos pos t))

(defun previewer--buffer-line-count ()
  "Return current buffer line count."
  (max 1 (line-number-at-pos (point-max) t)))

(defun previewer--position-percent-number ()
  "Return preview position percent as a number."
  (if (not previewer-auto-scroll)
      :json-false
    (let* ((line (previewer--line-at-pos (point)))
           (total (previewer--buffer-line-count)))
      (max 0 (min 100 (round (* 100.0 (/ (float line) total))))))))

(defun previewer--point-window-ratio ()
  "Return point's approximate vertical ratio in its visible source window."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((start-line (previewer--line-at-pos (window-start window)))
             (end-line (previewer--line-at-pos (window-end window t)))
             (point-line (previewer--line-at-pos (point)))
             (span (max 1 (- end-line start-line))))
        (max 0.08 (min 0.92 (/ (float (- point-line start-line)) span))))
    0.45))

(defun previewer--source-line-text ()
  "Return the current source line text."
  (string-trim
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun previewer--anchor-kind-for-line (line)
  "Return a source-map kind for LINE."
  (cond
   ((string-match-p "\\`[ \t]*\\*+\\s-+" line) "heading")
   ((string-match-p "\\`[ \t]*#\\+begin_" line) "block")
   ((string-match-p "\\\\\\[\\|\\\\(\\|\\$\\$\\|\\`[ \t]*\\\\begin{" line) "math")
   (t "paragraph")))

(defun previewer--clean-anchor-text (line)
  "Return display text for source-map LINE."
  (let ((text (string-trim line)))
    (setq text (replace-regexp-in-string "\\`[ \t]*\\*+\\s-+" "" text))
    (setq text (replace-regexp-in-string "\\`[ \t]*[-+*]\\s-+\\(?:\\[[ X-]\\]\\s-+\\)?\\|\\`[ \t]*[0-9]+[.)]\\s-+" "" text))
    (setq text (replace-regexp-in-string "\\`[ \t]*#\\+begin_" "" text))
    (truncate-string-to-width text 120 nil nil t)))

(defun previewer-org-source-anchors (&optional beg end)
  "Return a vector of Org source anchors for browser synchronization.
When BEG and END are non-nil, only scan that source range."
  (let ((anchors nil)
        (total (previewer--buffer-line-count))
        (count 0)
        inside-block
        inside-math)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (or beg (point-min)))
        (while (and (not (eobp))
                    (< (point) (or end (point-max)))
                    (< count 1200))
          (let* ((pos (point))
                 (line-no (previewer--line-at-pos pos))
                 (line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (trimmed (string-trim line))
                 (lower (downcase trimmed))
                 (block-begin (string-match-p "\\`#\\+begin_" lower))
                 (math-begin (string-match-p "\\`\\(?:\\\\\\[\\|\\$\\$\\|\\\\begin{\\)" trimmed))
                 (math-end (string-match-p "\\(?:\\\\\\]\\|\\$\\$\\|\\\\end{\\)" trimmed)))
            (cond
             ((and inside-block (string-match-p "\\`#\\+end_" lower))
              (setq inside-block nil))
             ((and inside-math math-end)
              (setq inside-math nil))
             (inside-block nil)
             (inside-math nil)
             ((and (not (string-empty-p trimmed))
                   (not (and (string-prefix-p "#+" trimmed)
                             (not block-begin)))
                   (not (string-prefix-p ":" trimmed))
                   (not (string-prefix-p ":END:" (upcase trimmed))))
              (push `((pos . ,pos)
                      (line . ,line-no)
                      (percent . ,(round (* 100.0 (/ (float line-no) total))))
                      (kind . ,(previewer--anchor-kind-for-line line))
                      (text . ,(previewer--clean-anchor-text line)))
                    anchors)
              (setq count (1+ count))
              (when block-begin
                (setq inside-block t))
              (when (and math-begin (not math-end))
                (setq inside-math t)))))
          (forward-line 1))))
    (vconcat (nreverse anchors))))

(defun previewer-org--source-attrs-for-element (element)
  "Return source-map HTML attributes for Org ELEMENT."
  (when-let* ((source-buffer previewer-org--export-source-buffer)
              ((buffer-live-p source-buffer))
              (local-beg (org-element-property :begin element)))
    (let ((pos (max 1 (+ previewer-org--export-source-offset local-beg -1))))
      (with-current-buffer source-buffer
        (save-restriction
          (widen)
          (let* ((clamped (max (point-min) (min pos (point-max))))
                 (line (previewer--line-at-pos clamped))
                 (total (previewer--buffer-line-count))
                 (percent (round (* 100.0 (/ (float line) total)))))
            (format " data-source-pos=\"%d\" data-source-line=\"%d\" data-source-percent=\"%d\""
                    clamped line percent)))))))

(defun previewer-org--add-source-attrs-to-html (html element)
  "Add source-map attributes for ELEMENT to the first HTML tag in HTML."
  (if-let* (((stringp html))
            (attrs (previewer-org--source-attrs-for-element element))
            ((string-match "\\`[[:space:]\n]*<[[:alnum:]:-]+" html)))
      (concat (substring html 0 (match-end 0))
              attrs
              (substring html (match-end 0)))
    html))

(defun previewer-org--html-source-anchor-a (orig element &rest args)
  "Advice ORIG to annotate exported HTML for ELEMENT with source position."
  (previewer-org--add-source-attrs-to-html
   (apply orig element args)
   element))

(defun previewer-org--ensure-source-advice ()
  "Install lightweight source-map advice for Org HTML exporters."
  (unless previewer-org--source-advice-installed
    (dolist (fn '(org-html-headline
                  org-html-paragraph
                  org-html-item
                  org-html-special-block
                  org-html-src-block
                  org-html-example-block
                  org-html-quote-block
                  org-html-verse-block
                  org-html-latex-environment
                  org-html-latex-fragment
                  org-html-table
                  org-html-table-row))
      (when (fboundp fn)
        (advice-add fn :around #'previewer-org--html-source-anchor-a)))
    (setq previewer-org--source-advice-installed t)))

(defun previewer-org--strip-toc (html)
  "Remove any Org-generated table of contents from HTML."
  (let ((case-fold-search t))
    (while (string-match
            "<div id=\"table-of-contents\"\\(?:.\\|\n\\)*?</div>[ \t\n]*</div>[ \t\n]*"
            html)
      (setq html (replace-match "" t t html)))
    html))

(defun previewer-org-export-region-as-html (beg end)
  "Return Org HTML exported from BEG to END."
  (unless (featurep 'ox-html) (require 'ox-html))
  (let* ((source-buffer (current-buffer))
         (source-point (point-marker))
         (source-window (get-buffer-window source-buffer t))
         (source-window-start (and source-window (window-start source-window)))
         (source-default-directory default-directory)
         (source-file-name buffer-file-name)
         (text (buffer-substring-no-properties beg end)))
    (unwind-protect
        (previewer-org--strip-toc
         (with-temp-buffer
           (let ((default-directory source-default-directory)
                 (buffer-file-name source-file-name)
                 (previewer-org--export-source-buffer source-buffer)
                 (previewer-org--export-source-offset (1- beg)))
             (insert text)
             (delay-mode-hooks (org-mode))
             (previewer-org--ensure-source-advice)
             (let ((org-html-postamble nil)
                   (org-html-head-include-default-style nil)
                   (org-html-head-include-scripts nil)
                   (org-html-htmlize-output-type 'css)
                   (org-export-with-broken-links t)
                   (org-export-with-toc nil)
                   (org-export-use-babel nil))
               (org-export-as 'html nil nil t
                              '(:with-author nil
                                :with-creator nil
                                :with-date nil
                                :with-toc nil
                                :section-numbers nil))))))
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (goto-char (marker-position source-point))
          (when (window-live-p source-window)
            (set-window-start source-window source-window-start t)
            (set-window-point source-window (point)))))
      (set-marker source-point nil))))

(defun previewer-org-chunk-id (beg)
  "Return stable-enough preview chunk id for source position BEG."
  (if (= beg (point-min))
      "chunk-start"
    (format "chunk-%d" beg)))

(defun previewer-org-chunk-starts ()
  "Return sorted source positions that start incremental Org chunks."
  (let ((starts (list (point-min)))
        (max-level (max 1 previewer-org-incremental-heading-level)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+\\s-+" nil t)
          (let ((level (- (match-end 0) (match-beginning 0) 1)))
            (when (<= level max-level)
              (push (line-beginning-position) starts))))))
    (sort (delete-dups starts) #'<)))

(defun previewer-org-chunks ()
  "Return Org preview chunks as a list of (ID BEG END HTML)."
  (let* ((starts (previewer-org-chunk-starts))
         (chunks nil))
    (while starts
      (let* ((beg (pop starts))
             (end (or (car starts) (point-max)))
             (html (previewer-org-export-region-as-html beg end)))
        (push (list (previewer-org-chunk-id beg) beg end html) chunks)))
    (nreverse chunks)))

(defun previewer-org-html-content ()
  "Return chunked Org HTML for the live preview pane."
  (if (not previewer-org-incremental-render)
      (previewer-org-export-region-as-html (point-min) (point-max))
    (mapconcat
     (lambda (chunk)
       (pcase-let ((`(,id ,beg ,_end ,html) chunk))
         (format "<section data-preview-chunk=\"%s\" data-source-pos=\"%d\">%s</section>"
                 id beg html)))
     (previewer-org-chunks)
     "\n")))

(defun previewer-org-chunk-range-at (&optional pos)
  "Return (ID BEG END) for the incremental Org chunk at POS."
  (let* ((position (or pos (point)))
         (starts (previewer-org-chunk-starts))
         (beg (car starts))
         end)
    (while (and (cdr starts) (<= (cadr starts) position))
      (setq starts (cdr starts)
            beg (car starts)))
    (setq end (or (cadr starts) (point-max)))
    (list (previewer-org-chunk-id beg) beg end)))

(defun previewer--mode-name ()
  "Return current major mode as a string."
  (symbol-name major-mode))

(defun previewer-json-content ()
  "Return a structured preview payload for the current buffer."
  (let* ((org-p (derived-mode-p 'org-mode))
         (html-p (previewer--html-mode-p))
         (content-fn (or (cdr (assoc major-mode previewer-render-alist))
                         (cdr (assoc t previewer-render-alist))))
         (html (if org-p
                   (previewer-org-html-content)
                 (funcall content-fn)))
         (content-hash (secure-hash 'sha1 html)))
    (json-encode
     `((type . "content")
       (mode . ,(previewer--mode-name))
       (format . ,(cond (org-p "org-html")
                        (html-p "html")
                        (t "markdown")))
       (sourceFile . ,(or (buffer-file-name) ""))
       (contentHash . ,content-hash)
       (point . ,(point))
       (line . ,(previewer--line-at-pos (point)))
       (lineText . ,(previewer--source-line-text))
       (percent . ,(previewer--position-percent-number))
       (viewportRatio . ,(previewer--point-window-ratio))
       (syncScrollFromBrowser . ,(if previewer-sync-scroll-from-browser t :json-false))
       (html . ,html)
       (anchors . ,(if (and org-p previewer-org-source-map)
                       (previewer-org-source-anchors)
                     []))))))

(defun previewer-org-patch-content ()
  "Return a structured preview payload for the current Org chunk."
  (pcase-let* ((`(,id ,beg ,end) (previewer-org-chunk-range-at previewer--changed-beg))
               (html (previewer-org-export-region-as-html beg end)))
    (json-encode
     `((type . "patch")
       (mode . ,(previewer--mode-name))
       (format . "org-html")
       (sourceFile . ,(or (buffer-file-name) ""))
       (chunkId . ,id)
       (chunkBeg . ,beg)
       (chunkEnd . ,end)
       (contentHash . ,(secure-hash 'sha1 html))
       (point . ,(point))
       (line . ,(previewer--line-at-pos (point)))
       (lineText . ,(previewer--source-line-text))
       (percent . ,(previewer--position-percent-number))
       (viewportRatio . ,(previewer--point-window-ratio))
       (html . ,html)
       (anchors . ,(if previewer-org-source-map
                       (previewer-org-source-anchors beg end)
                     []))))))

(defun previewer-sync-content ()
  "Return a lightweight point synchronization payload."
  (json-encode
   `((type . "sync")
     (point . ,(point))
     (line . ,(previewer--line-at-pos (point)))
     (lineText . ,(previewer--source-line-text))
     (percent . ,(previewer--position-percent-number))
     (viewportRatio . ,(previewer--point-window-ratio)))))

(defun previewer--send-string (text &optional ws)
  "Send TEXT to WS or the active preview websocket."
  (when-let* ((process (or ws previewer-websocket))
              ((process-live-p process)))
    (process-send-string process (previewer-websocket-text text))))

(defun previewer--org-structural-change-p (beg end)
  "Return non-nil when an Org change between BEG and END affects chunk structure."
  (save-excursion
    (save-restriction
      (widen)
      (let ((scan-beg (progn (goto-char beg) (line-beginning-position)))
            (scan-end (progn (goto-char (min (point-max) (max beg end)))
                             (line-end-position))))
        (goto-char scan-beg)
        (re-search-forward "^\\(?:\\*+\\s-+\\|#\\+\\)" scan-end t)))))

(defun previewer--cancel-content-timer ()
  "Cancel the pending full-content preview render."
  (when (timerp previewer--content-timer)
    (cancel-timer previewer--content-timer))
  (setq previewer--content-timer nil
        previewer-sending nil))

(defun previewer--cancel-patch-timer ()
  "Cancel the pending incremental preview patch."
  (when (timerp previewer--patch-timer)
    (cancel-timer previewer--patch-timer))
  (setq previewer--patch-timer nil))

(defun previewer-send-content (&optional immediate)
  "Send full preview content.
When IMMEDIATE is non-nil, render now.  Otherwise wait for true editor idle and
reschedule on every edit."
  (setq previewer-source-buffer (current-buffer))
  (previewer--cancel-content-timer)
  (previewer--cancel-patch-timer)
  (if (or immediate (<= previewer-delay 0))
      (previewer-send-to-server)
    (setq previewer-sending t
          previewer--content-timer
          (run-with-idle-timer
           previewer-delay nil
           (lambda (buffer)
             (setq previewer--content-timer nil
                   previewer-sending nil)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (previewer-send-to-server))))
           (current-buffer)))))

(defun previewer-send-patch (&optional immediate)
  "Send an incremental Org preview patch.
When IMMEDIATE is non-nil, send now."
  (if (or (not (derived-mode-p 'org-mode))
          (not previewer-org-incremental-render)
          (not previewer--org-full-rendered)
          (not previewer-websocket)
          (null previewer--changed-beg))
      (previewer-send-content immediate)
    (previewer--cancel-patch-timer)
    (if immediate
        (progn
          (previewer--send-string (previewer-org-patch-content))
          (setq previewer--changed-beg nil
                previewer--changed-end nil))
      (setq previewer--patch-timer
            (run-with-idle-timer
             previewer-delay nil
             (lambda (buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq previewer--patch-timer nil)
                   (previewer--send-string (previewer-org-patch-content))
                   (setq previewer--changed-beg nil
                         previewer--changed-end nil)))))
             (current-buffer)))))

(defun previewer-after-change (beg end _len)
  "Schedule preview refresh after a source edit from BEG to END."
  (setq previewer-source-buffer (current-buffer))
  (if (and (derived-mode-p 'org-mode)
           previewer-org-incremental-render
           previewer--org-full-rendered
           (not (previewer--org-structural-change-p beg end)))
      (progn
        (setq previewer--changed-beg
              (if previewer--changed-beg (min previewer--changed-beg beg) beg)
              previewer--changed-end
              (if previewer--changed-end (max previewer--changed-end end) end))
        (previewer-send-patch))
    (setq previewer--org-full-rendered nil)
    (previewer-send-content)))

(defun previewer-send-content-now ()
  "Send full preview content immediately."
  (previewer-send-content t))

(defun previewer-send-sync ()
  "Send a lightweight point sync message to the preview."
  (when (and (bound-and-true-p previewer-mode)
             (member major-mode previewer-render-modes)
             previewer-websocket)
    (let ((key (list (current-buffer)
                     (point)
                     (window-start)
                     (buffer-chars-modified-tick))))
      (unless (equal key previewer--last-sync-key)
        (setq previewer--last-sync-key key
              previewer-source-buffer (current-buffer))
        (unless previewer-sync-sending
          (setq previewer-sync-sending t)
          (run-with-idle-timer
           0.08 nil
           (lambda ()
             (when (buffer-live-p previewer-source-buffer)
               (with-current-buffer previewer-source-buffer
                 (previewer--send-string (previewer-sync-content))))
             (setq previewer-sync-sending nil))))))))

(defun previewer--cleanup-buffer-hooks ()
  "Remove Previewer buffer-local hooks from the current buffer."
  (remove-hook 'after-change-functions #'previewer-after-change t)
  (remove-hook 'post-command-hook #'previewer-send-sync t)
  (remove-hook 'after-save-hook #'previewer-send-content-now t)
  (remove-hook 'kill-buffer-hook #'previewer--cleanup-buffer-hooks t)
  (previewer--cancel-patch-timer)
  (setq previewer--buffer-hooks-installed nil
        previewer--changed-beg nil
        previewer--changed-end nil
        previewer--org-full-rendered nil))

(defun previewer--install-buffer-hooks ()
  "Install Previewer buffer-local hooks for the current source buffer."
  (unless previewer--buffer-hooks-installed
    (when previewer-auto-update
      (add-hook 'after-change-functions #'previewer-after-change nil t)
      (add-hook 'post-command-hook #'previewer-send-sync nil t)
      (run-hooks 'previewer-auto-hook))
    (add-hook 'after-save-hook #'previewer-send-content-now nil t)
    (add-hook 'kill-buffer-hook #'previewer--cleanup-buffer-hooks nil t)
    (setq previewer--buffer-hooks-installed t)))

(defun previewer-goto-source (pos &optional passive)
  "Move the source buffer to POS.
When PASSIVE is non-nil, update the visible source window without stealing
focus from the preview pane."
  (when (and (buffer-live-p previewer-source-buffer)
             (integerp pos))
    (let ((buffer previewer-source-buffer))
      (if passive
          (when-let* ((window (get-buffer-window buffer t)))
            (save-selected-window
              (select-window window)
              (goto-char (max (point-min) (min pos (point-max))))
              (set-window-point window (point))
              (recenter)))
        (pop-to-buffer buffer)
        (goto-char (max (point-min) (min pos (point-max))))
        (recenter)))))

(defun previewer-handle-client-message (string &optional _ws)
  "Handle websocket message STRING from the preview page.
Return `ready' when the browser is asking for initial content."
  (when (and (stringp string)
             (string-prefix-p "{" string))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (message (json-read-from-string string))
               (type (alist-get 'type message nil nil #'equal)))
          (pcase type
            ("ready" 'ready)
            ("goto"
             (previewer-goto-source (alist-get 'pos message)
                                    (alist-get 'passive message))
             'handled)
            (_ nil)))
      (error nil))))

(defun previewer--send-current-content (&optional ws)
  "Send the current source buffer's full preview content to WS."
  (let ((buffer (or (and (buffer-live-p previewer-source-buffer)
                         previewer-source-buffer)
                    (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (bound-and-true-p previewer-mode)
                   (member major-mode previewer-render-modes))
          (setq previewer-source-buffer (current-buffer))
          (when (derived-mode-p 'org-mode)
            (setq previewer--org-full-rendered t
                  previewer--changed-beg nil
                  previewer--changed-end nil))
          (previewer--send-string (previewer-json-content) ws))))))

(defun previewer-send-to-server (&optional ws string)
  "Send preview content to WS clients, or handle incoming client STRING."
  (if string
      (when (eq (previewer-handle-client-message string ws) 'ready)
        (previewer--send-current-content ws))
    (previewer--send-current-content ws)))

(defun previewer-websocket-text(text)
  "Decode websocket TEXT,`ws-web-socket-frame` utf-8 is unsupported."
  (websocket-encode-frame
   (make-websocket-frame :opcode 'text
                         :payload (encode-coding-string
                                   text 'raw-text)
                         :completep t)
   nil))

(defun previewer-init-server()
  "Init server."
  (unless previewer-server
    (setq previewer-server
          (ws-start
           (lambda (request)
             (with-slots (process headers) request
               (if (ws-web-socket-connect request 'previewer-send-to-server)
                   (prog1 :keep-alive (setq previewer-websocket process))
                 (let ((path (substring (cdr (assoc :GET headers)) 1)))
                   (catch 'close-connection
                     (cond ((string= path "favicon.ico")
                            (ws-send-404 process))
                           ((string= path "preview")
                            (ws-response-header process 200 '("Content-type" . "text/html"))
                            (ws-send process (previewer-template)))
                           ((string-prefix-p "preview/" path)
                            (ws-send-file
                             process
                             (expand-file-name (string-trim-left path "preview/") previewer-home-path)
                             (previewer-mime-type path)))
                           ((ws-in-directory-p default-directory path)
                            (ws-send-file
                             process
                             (expand-file-name path default-directory)
                             (previewer-mime-type path)))
                           (t (ws-send-404 process))))))))
           previewer-port nil
           :host previewer-host
           ;; name is unvalid
           :name "previewer-server"))))

(defun previewer-listen()
  "Get listen address."
  (unless previewer-server
    (error "There is no listen address"))
  (format "%s:%s" previewer-host
          (if (booleanp previewer-port)
              (process-contact (ws-process previewer-server) :service t)
            previewer-port)))

(defun previewer--vendor-file-stale-p (file)
  "Return non-nil when FILE is missing or older than the update interval."
  (or (not (file-exists-p file))
      (let* ((age (float-time (time-subtract (current-time)
                                             (file-attribute-modification-time
                                              (file-attributes file)))))
             (max-age (* previewer-vendor-update-interval-days 24 60 60)))
        (> age max-age))))

(defun previewer--katex-dist-url (asset)
  "Return CDN URL for KaTeX ASSET."
  (format "https://cdn.jsdelivr.net/npm/katex@%s/dist/%s"
          previewer-katex-version asset))

(defun previewer--mathjax-dist-url (asset)
  "Return CDN URL for MathJax ASSET."
  (format "https://cdn.jsdelivr.net/npm/mathjax@3/es5/%s" asset))

(defun previewer--download-preview-asset (url file)
  "Download URL to FILE, creating parent directories first."
  (make-directory (file-name-directory file) t)
  (let ((url-request-timeout 8))
    (url-copy-file url file t)))

(defun previewer-update-vendor-assets (&optional force)
  "Refresh bundled Previewer web assets.
When FORCE is nil, only refresh stale or missing assets."
  (interactive "P")
  (let* ((css-file (expand-file-name "katex.min.css" previewer-katex-vendor-path))
         (js-file (expand-file-name "katex.min.js" previewer-katex-vendor-path))
         (mathjax-file (expand-file-name "tex-mml-chtml.js" previewer-mathjax-vendor-path)))
    (when (or force (previewer--vendor-file-stale-p css-file))
      (previewer--download-preview-asset
       (previewer--katex-dist-url "katex.min.css") css-file))
    (when (or force (previewer--vendor-file-stale-p js-file))
      (previewer--download-preview-asset
       (previewer--katex-dist-url "katex.min.js") js-file))
    (when (or force (previewer--vendor-file-stale-p mathjax-file))
      (previewer--download-preview-asset
       (previewer--mathjax-dist-url "tex-mml-chtml.js") mathjax-file))
    (when (file-exists-p css-file)
      (with-temp-buffer
        (insert-file-contents css-file)
        (goto-char (point-min))
        (while (re-search-forward "fonts/[^)'\"]+" nil t)
          (let* ((asset (match-string 0))
                 (file (expand-file-name asset previewer-katex-vendor-path)))
            (when (or force (previewer--vendor-file-stale-p file))
              (previewer--download-preview-asset
               (previewer--katex-dist-url asset) file)))))))
  (message "Previewer vendor assets refreshed"))

(defun previewer-maybe-update-vendor-assets ()
  "Refresh bundled assets once per session when they are stale."
  (when (and previewer-vendor-auto-update
             (not previewer--vendor-update-started))
    (let ((files (list (expand-file-name "katex.min.css" previewer-katex-vendor-path)
                       (expand-file-name "katex.min.js" previewer-katex-vendor-path)
                       (expand-file-name "tex-mml-chtml.js" previewer-mathjax-vendor-path))))
      (when (seq-some #'previewer--vendor-file-stale-p files)
        (setq previewer--vendor-update-started t)
        (run-at-time
         3 nil
         (lambda ()
           (condition-case error
               (previewer-update-vendor-assets)
             (error
              (message "Previewer vendor asset refresh failed: %s"
                       (error-message-string error))))))))))

(defun previewer--preview-url ()
  "Return the preview URL."
  (format "http://%s/preview" (previewer-listen)))

(defun previewer--split-side-for-window (window)
  "Return the concrete preview split side for WINDOW."
  (let ((side (or previewer-window-side 'auto)))
    (if (not (eq side 'auto))
        side
      (let ((width (float (max 1 (window-total-width window))))
            (height (float (max 1 (window-total-height window)))))
        (if (>= (/ width height) previewer-wide-window-min-ratio)
            'left
          'above)))))

(defun previewer--xwidget-preview-buffer-live-p ()
  "Return non-nil when the Previewer xwidget buffer is still live."
  (and (buffer-live-p previewer--xwidget-buffer)
       (get-buffer-window previewer--xwidget-buffer t)))

(defun previewer--open-xwidget (url)
  "Open URL in an Emacs xwidget preview window."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (require 'xwidget))
  (unless (fboundp 'xwidget-webkit-browse-url)
    (user-error "xwidget-webkit is not available in this Emacs"))
  (let* ((source-window (selected-window))
         (side (previewer--split-side-for-window source-window))
         (existing-window (and (buffer-live-p previewer--xwidget-buffer)
                               (get-buffer-window previewer--xwidget-buffer t)))
         (preview-window
          (or existing-window
              (split-window source-window nil side))))
    (unless existing-window
      (pcase side
        ((or 'left 'right)
         (let* ((total (window-total-width source-window))
                (target (max 24 (floor (* total previewer-window-size)))))
           (ignore-errors
             (window-resize preview-window
                            (- target (window-total-width preview-window))
                            t))))
        ((or 'above 'below)
         (let* ((total (window-total-height source-window))
                (target (max 8 (floor (* total previewer-window-size)))))
           (ignore-errors
             (window-resize preview-window
                            (- target (window-total-height preview-window))
                            nil))))))
    (select-window preview-window)
    (xwidget-webkit-browse-url url t)
    (setq previewer--xwidget-buffer (window-buffer preview-window))
    (when (window-live-p source-window)
      (select-window source-window))))

(defun previewer-open-browser ()
  "Open browser."
  (let ((url (previewer--preview-url)))
    (pcase previewer-browser-backend
      ('xwidget (previewer--open-xwidget url))
      (_ (browse-url url)))))

(defun previewer-init ()
  "Preview init."
  (previewer-init-server)
  (previewer-maybe-update-vendor-assets)
  (when previewer-auto-browser (previewer-open-browser))
  (previewer--install-buffer-hooks)
  (previewer-send-content t))

(defun previewer-finalize ()
  "Preview close."
  (setq previewer-sending nil
        previewer-sync-sending nil
        previewer--last-sync-key nil)
  (previewer--cancel-content-timer)
  (when previewer-server
    (ws-stop previewer-server)
    (setq previewer-server nil))
  (when previewer-websocket
    (setq previewer-websocket nil))
  (let ((buffer (and (buffer-live-p previewer-source-buffer)
                     previewer-source-buffer)))
    (when buffer
      (with-current-buffer buffer
        (previewer--cleanup-buffer-hooks)))))

;;;###autoload
(defun previewer-cleanup ()
  "Cleanup `previewer' mode."
  (interactive)
  (previewer-finalize)
  (run-hooks 'previewer-finialize-hook))

;;;###autoload
(define-minor-mode previewer-mode
  "Previewer mode."
  :group      'previewer
  :init-value nil
  :global     t
  (if previewer-mode (previewer-init) (previewer-finalize)))

;;;###autoload
(defun previewer-workbench ()
  "Open the current buffer with an Emacs-local Previewer workbench."
  (interactive)
  (unless (member major-mode previewer-render-modes)
    (user-error "Previewer does not handle %s" major-mode))
  (let ((previewer-browser-backend 'xwidget))
    (setq previewer-source-buffer (current-buffer))
    (unless previewer-mode
      (previewer-mode 1))
    (previewer-maybe-update-vendor-assets)
    (previewer--install-buffer-hooks)
    (unless (previewer--xwidget-preview-buffer-live-p)
      (previewer-open-browser))
    (previewer-send-content t)))

;;;###autoload
(defun previewer-org-workbench ()
  "Open the current Org buffer with an HTML preview workbench."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "previewer-org-workbench expects an Org buffer"))
  (previewer-workbench))

(provide 'previewer)
;;; previewer.el ends here

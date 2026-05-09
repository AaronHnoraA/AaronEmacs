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
(require 'web-server)
(require 'websocket)

(declare-function org-export-as 'ox-html)

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

(defcustom previewer-delay 0.1
  "Delay time when auto update preview."
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

(defcustom previewer-styles
  '("/preview/static/css/markdown.css"
    "/preview/static/css/highlight.css")
  "Custom preview css style."
  :type 'list
  :group 'previewer)

(defcustom previewer-scripts
  '("/preview/static/js/jquery.min.js"
    "/preview/static/js/marked.min.js"
    "/preview/static/js/marked-highlight.min.js"
    "/preview/static/js/highlight.min.js"
    "/preview/static/js/mermaid.min.js")
  "Custom preview js script."
  :type 'list
  :group 'previewer)

(defcustom previewer-render-alist
  '((t . previewer-markdown-content))
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

(defvar previewer-home-path (file-name-directory load-file-name))
(defvar previewer-index-file (concat previewer-home-path "index.html"))

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
                 (let ((org-html-postamble nil))
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

(defun previewer-send-content()
  "Send content to server with delay time."
  (if (> previewer-delay 0)
      (unless previewer-sending
        (setq previewer-sending t)
        (run-with-idle-timer
         previewer-delay nil
         (lambda()
           (previewer-send-to-server)
           (setq previewer-sending nil))))
    (previewer-send-to-server)))

(defun previewer-send-to-server (&optional ws _string)
  "Send STRING the `previewer' preview to WS clients."
  (when (and (bound-and-true-p previewer-mode)
             (member major-mode previewer-render-modes))
    (let ((text-content-func (cdr (assoc major-mode previewer-render-alist))))
      (unless text-content-func
        (setq text-content-func (cdr (assoc t previewer-render-alist))))
      (process-send-string (or ws previewer-websocket)
                           (previewer-websocket-text
                            (concat (previewer-position-percent) (funcall text-content-func)))))))

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

(defun previewer-open-browser ()
  "Open browser."
  (browse-url
   (format "http://%s/preview" (previewer-listen))))

(defun previewer-init ()
  "Preview init."
  (previewer-init-server)
  (when previewer-auto-browser (previewer-open-browser))
  (when previewer-auto-update
    (add-hook 'post-self-insert-hook #'previewer-send-content)
    (run-hooks 'previewer-auto-hook))
  (add-hook 'after-save-hook #'previewer-send-content))

(defun previewer-finalize ()
  "Preview close."
  (setq previewer-sending nil)
  (when previewer-server
    (ws-stop previewer-server)
    (setq previewer-server nil))
  (when previewer-websocket
    (setq previewer-websocket nil))
  (remove-hook 'post-self-insert-hook 'previewer-send-content)
  (remove-hook 'after-save-hook 'previewer-send-content))

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

(provide 'previewer)
;;; previewer.el ends here

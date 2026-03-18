;;; init-org-mmdc.el --- Mermaid CLI support for Org -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'ob)
(require 'json)

(defvar org-babel-load-languages)
(defvar org-src-lang-modes)
(defvar package-user-dir)

(defconst my/org-mmdc-browser-candidates
  '("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    "/Applications/Chromium.app/Contents/MacOS/Chromium"
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
    "~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    "~/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    "~/Applications/Chromium.app/Contents/MacOS/Chromium"
    "~/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
  "Preferred browser executables for Mermaid CLI.")

(defvar my/org-mermaid-babel-executor nil
  "Saved Mermaid babel executor from `ob-mermaid'.")

(defun my/org-mmdc--elpa-roots ()
  "Return candidate ELPA roots for Mermaid-related packages."
  (delete-dups
   (delq nil
         (list package-user-dir
               (expand-file-name "elpa" user-emacs-directory)
               (when load-file-name
                 (expand-file-name "../elpa"
                                   (file-name-directory load-file-name)))))))

(defun my/org-mmdc-add-package-to-load-path (package-name)
  "Add the newest PACKAGE-NAME directory from ELPA roots to `load-path'."
  (catch 'done
    (dolist (root (my/org-mmdc--elpa-roots))
      (when (file-directory-p root)
        (let ((matches
               (sort (file-expand-wildcards
                      (expand-file-name (format "%s-*" package-name) root))
                     #'string>)))
          (when matches
            (add-to-list 'load-path (car matches))
            (throw 'done t)))))))

(defun my/org-mmdc-find-browser ()
  "Return the first available browser executable for Mermaid CLI."
  (catch 'browser
    (dolist (path my/org-mmdc-browser-candidates)
      (let ((resolved-path (expand-file-name path)))
        (when (file-executable-p resolved-path)
          (throw 'browser resolved-path))))))

(defun my/org-mmdc-ensure-puppeteer-config ()
  "Write and return a Puppeteer config for Mermaid CLI when a browser exists."
  (let ((browser (my/org-mmdc-find-browser)))
    (when browser
      (let ((config-file
             (expand-file-name "var/mmdc-puppeteer-config.json"
                               user-emacs-directory)))
        (make-directory (file-name-directory config-file) t)
        (with-temp-file config-file
          (insert
           (json-encode
            `((executablePath . ,browser)
              (headless . t)))))
        config-file))))

(defun my/org-mmdc-ensure-mermaid-config ()
  "Write and return a Mermaid config tuned for Emacs SVG rendering."
  (let ((config-file
         (expand-file-name "var/mmdc-mermaid-config.json"
                           user-emacs-directory)))
    (make-directory (file-name-directory config-file) t)
    (with-temp-file config-file
      (insert
       (json-encode
        '((htmlLabels . :json-false)
          (flowchart . ((htmlLabels . :json-false)))))))
    config-file))

(my/org-mmdc-add-package-to-load-path "ob-mermaid")
(my/org-mmdc-add-package-to-load-path "mermaid-mode")

(use-package ob-mermaid
  :after org-src
  :init
  (with-eval-after-load 'org-src
    (add-to-list 'org-babel-load-languages '(mermaid . t))
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages))
  :custom
  (ob-mermaid-cli-path (or (executable-find "mmdc") "mmdc"))
  :config
  (setq ob-mermaid-default-config-file
        (my/org-mmdc-ensure-mermaid-config))
  (let ((puppeteer-config-file (my/org-mmdc-ensure-puppeteer-config)))
    (when puppeteer-config-file
      (setf (alist-get :puppeteer-config-file
                       org-babel-default-header-args:mermaid)
            puppeteer-config-file)))
  (setq my/org-mermaid-babel-executor
        (symbol-function 'org-babel-execute:mermaid)))

(use-package mermaid-mode
  :after org-src
  :custom
  (mermaid-mmdc-location (or (executable-find "mmdc") "mmdc"))
  :config
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid))
  ;; `mermaid-mode' also defines `org-babel-execute:mermaid'. Keep the
  ;; `ob-mermaid' implementation for Org Babel execution.
  (when my/org-mermaid-babel-executor
    (fset 'org-babel-execute:mermaid my/org-mermaid-babel-executor)))

(provide 'init-org-mmdc)
;;; init-org-mmdc.el ends here

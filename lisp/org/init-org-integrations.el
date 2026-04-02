;;; init-org-integrations.el --- Org external links and templates -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'init-org-code)

;; External App Links (Zotero, MarginNote)
(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
    :follow (lambda (path)
              (let ((url (concat "zotero:" path))
                    (command (if (eq system-type 'darwin) "open" "xdg-open")))
                (start-process "zotero-opener" nil command url))))

  (let ((marginnote-link-types
         '("marginnote1app" "marginnote2app" "marginnote3app" "marginnote4app")))
    (dolist (type marginnote-link-types)
      (org-link-set-parameters
       type
       :follow
       (lambda (path)
         (if (eq system-type 'darwin)
             (let ((url (concat "marginnote4app:" path)))
               (start-process "marginnote" nil "open" url))
           (message "[org] MarginNote link only supported on macOS (got %s)" system-type)))))))

(with-eval-after-load 'org-tempo
  (dolist (template '(("dot" . "src dot :file images/graph.svg")
                      ("ditaa" . "src ditaa :file images/diagram.png")
                      ("pic" . "src ditaa :file images/diagram.png")))
    (add-to-list 'org-structure-template-alist template t)))

(require 'org-tempo) 

(provide 'init-org-integrations)
;;; init-org-integrations.el ends here

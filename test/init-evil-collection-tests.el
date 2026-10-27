;;; init-evil-collection-tests.el --- evil-collection leaves the leader alone -*- lexical-binding: t -*-

;;; Commentary:
;; evil-collection's per-mode Normal-state maps outrank the global map that
;; holds the SPC leader, so the leader key is on its blacklist.

;;; Code:

(require 'ert)
(require 'evil)
(require 'evil-collection)
(require 'info)
(require 'man)

(ert-deftest +evil-collection-leaves-the-leader-to-every-mode ()
  (evil-collection-init '(help info man))
  (dolist (mode '(help-mode Info-mode Man-mode))
    (with-temp-buffer
      (funcall mode)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding (kbd "SPC s s")) '+search/line-forward)))))

(provide 'init-evil-collection-tests)
;;; init-evil-collection-tests.el ends here

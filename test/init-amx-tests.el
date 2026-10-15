;;; init-amx-tests.el --- Saved command history resilience -*- lexical-binding: t; -*-

(require 'ert)
(require 'amx)

(ert-deftest amx-prunes-retired-commands-before-cache-rebuild ()
  (let* ((retired 'my/noema-roam-new-note)
         (old-data amx-data)
         (old-history amx-history)
         (old-cache amx-cache))
    (unwind-protect
        (progn
          (setq amx-data (list (cons retired 1) (cons 'find-file 2))
                amx-history (list retired 'find-file)
                amx-cache nil)
          (should-not (commandp retired))
          (my/amx-prune-stale-commands)
          (should (equal amx-data '((find-file . 2))))
          (should (equal amx-history '(find-file)))
          (amx-rebuild-cache)
          (should-not (assq retired amx-cache))
          (should (assq 'roam-new-note amx-cache))
          (should (assq 'noema-agenda amx-cache))
          (should (assq 'noema-agenda-web amx-cache))
          (should (assq 'project-run amx-cache))
          (should (assq 'task-run amx-cache))
          (should (assq 'file-open amx-cache))
          (should (assq 'output-menu amx-cache))
          (should (assq 'output-switch amx-cache)))
      (setq amx-data old-data
            amx-history old-history
            amx-cache old-cache))))

(provide 'init-amx-tests)
;;; init-amx-tests.el ends here

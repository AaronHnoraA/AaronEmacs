;;; init-aaronnote-tests.el --- Aaronnote bridge tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(unless (fboundp 'general-define-key)
  (defalias 'general-define-key #'ignore))
(require 'init-aaronnote)

(ert-deftest my/noema-retire-legacy-commands-cleans-obarray-and-amx-state ()
  (let ((legacy (intern "my/aaronnote-test-command"))
        (history-symbols
         '(extended-command-history vertico-repeat-history
           amx-history amx-data amx-cache)))
    (fset legacy (lambda () (interactive)))
    (unwind-protect
        (cl-progv
            history-symbols
            (list
             '("my/aaronnote-test-command" "my/noema-roam-agenda")
             '((amx "aaronnote" "my/aaronnote-test-command")
               (amx "noema" "my/noema-roam-agenda"))
             '(my/aaronnote-test-command my/noema-roam-agenda)
             '((my/aaronnote-test-command . 2)
               (my/noema-roam-agenda . 1))
             '((my/aaronnote-test-command . 2)
               (my/noema-roam-agenda . 1)))
          (my/noema-retire-legacy-commands)
          (should-not (fboundp legacy))
          (dolist (variable history-symbols)
            (should-not
             (seq-some
              #'my/noema--legacy-command-history-entry-p
              (symbol-value variable)))))
      (when (fboundp legacy)
        (fmakunbound legacy)))))

(ert-deftest my/noema-canonical-file-uses-target-aware-home-expansion ()
  (let ((remote-mode t)
        call)
    (cl-letf
        (((symbol-function 'remote-expand-file-name)
          (lambda (file &optional directory target)
            (setq call (list file directory target))
            "/fs:local:/Users/me/Documents/Noema/")))
      (should
       (equal
        (my/noema--canonical-file "~/Documents/Noema/")
        "/fs:local:/Users/me/Documents/Noema/"))
      (should
       (equal call
              '("~/Documents/Noema/" nil "local"))))))

(ert-deftest my/noema-host-file-preserves-remote-logical-identity ()
  (let ((remote-mode t))
    (cl-letf
        (((symbol-function 'my/noema--canonical-file) #'identity)
         ((symbol-function 'remote-file-name-target)
          (lambda (file)
            (if (string-prefix-p "/fs:local:" file)
                "local"
              "remote")))
         ((symbol-function 'remote-file-local-name)
          (lambda (file)
            (string-remove-prefix "/fs:local:" file))))
      (should
       (equal
        (my/noema--host-file "/fs:remote:/srv/note.md")
        "/fs:remote:/srv/note.md"))
      (should
       (equal
        (my/noema--host-file "/fs:local:/tmp/note.md")
        "/tmp/note.md")))))

(ert-deftest my/noema-external-provider-reads-and-writes-through-logical-file-api ()
  (let* ((native (make-temp-file "aaronnote-remote-provider-" nil ".md"))
         (logical (remote-expand-file-name native nil "local")))
    (unwind-protect
        (progn
          (with-temp-file native
            (insert "# Initial\n"))
          (let* ((opened
                  (my/noema--external-file-read
                   `((file . ,logical)) nil))
                 (mtime (alist-get 'mtimeMs opened)))
            (should (equal (alist-get 'file opened) logical))
            (should (equal (alist-get 'content opened) "# Initial\n"))
            (let ((conflict
                   (my/noema--external-file-write
                    `((file . ,logical)
                      (content . "# Conflict\n")
                      (baseMtimeMs . ,(- mtime 10000)))
                    nil)))
              (should (eq (alist-get 'conflict conflict) t))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents native)
                  (buffer-string))
                "# Initial\n")))
            (let ((saved
                   (my/noema--external-file-write
                    `((file . ,logical)
                      (content . "# Saved\n")
                      (baseMtimeMs . ,mtime))
                    nil)))
              (should (eq (alist-get 'ok saved) t))
              (should (equal (alist-get 'file saved) logical))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents native)
                  (buffer-string))
                "# Saved\n")))))
      (when (file-exists-p native)
        (delete-file native)))))

(ert-deftest my/noema-external-watch-is-workspace-owned-and-bounded ()
  (let ((remote-mode t)
        (my/noema--external-file-watches
         (make-hash-table :test #'equal))
        (my/noema--external-file-watch-timers
         (make-hash-table :test #'equal))
        (my/noema--external-file-watch-suppressed
         (make-hash-table :test #'equal))
        opened closed)
    (cl-letf
        (((symbol-function 'remote-file-name-target)
          (lambda (_file) "remote"))
         ((symbol-function 'remote-context)
          (lambda (_file) 'mock-context))
         ((symbol-function 'remote-workspace-open)
          (lambda (_context &rest _args) 'mock-workspace))
         ((symbol-function 'remote-workspace-add-file-watch)
          (lambda (workspace file flags _callback &rest _args)
            (setq opened (list workspace file flags))
            'mock-resource))
         ((symbol-function 'remote-workspace-close-resource)
          (lambda (workspace resource &optional reason)
            (setq closed (list workspace resource reason)))))
      (my/noema--ensure-external-file-watch
       "/fs:remote:/srv/note.md")
      (my/noema--ensure-external-file-watch
       "/fs:remote:/srv/note.md")
      (should
       (equal opened
              '(mock-workspace
                "/fs:remote:/srv/note.md"
                (change attribute-change))))
      (should
       (= (hash-table-count my/noema--external-file-watches) 1))
      (my/noema--clear-external-file-watches)
      (should
       (equal closed
              '(mock-workspace mock-resource aaronnote-stop)))
      (should
       (zerop
        (hash-table-count my/noema--external-file-watches))))))

(ert-deftest my/noema-jupyter-defaults-read-project-config-without-eval ()
  (let ((root (make-temp-file "aaronnote-project" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" root)
            (insert "((nil . ((my/project-local-settings . "
                    "(:toolchain ((python . sage)) "
                    ":aaronnote-jupyter (:language sage :kernel sagemath :session research))))))\n"))
          (let ((my/noema--notes-root root))
            (should
             (equal (my/noema--jupyter-defaults)
                    '(:language "sage" :kernel "sagemath" :session "research")))
            (should
             (equal (my/noema--jupyter-default-environment)
                    '("AARONNOTE_JUPYTER_DEFAULT_LANGUAGE=sage"
                      "AARONNOTE_JUPYTER_DEFAULT_KERNEL=sagemath"
                      "AARONNOTE_JUPYTER_DEFAULT_SESSION=research")))))
      (delete-directory root t))))

(ert-deftest my/noema-prose-check-is-an-editor-command ()
  (should (commandp #'my/noema-prose-check))
  (let (sent)
    (cl-letf (((symbol-function 'my/noema-command)
               (lambda (command &optional detail)
                 (setq sent (list command detail)))))
      (my/noema-prose-check)
      (should (equal sent '("prose-check" nil))))))

(ert-deftest my/noema-refresh-does-not-send-extra-focus ()
  (let ((my/noema--ready t)
        (my/noema--app-buffer (current-buffer))
        calls)
    (cl-letf (((symbol-function 'my/noema-command)
               (lambda (command &optional _detail)
                 (push command calls))))
      (my/noema-refresh)
      (should (equal (nreverse calls) '("refresh"))))))

(ert-deftest my/noema-keys-mode-binds-history-chords ()
  (should (eq (lookup-key my/noema-keys-mode-map (kbd "M-z"))
              #'my/noema-undo))
  (should (eq (lookup-key my/noema-keys-mode-map (kbd "M-Z"))
              #'my/noema-redo))
  (should (eq (lookup-key my/noema-keys-mode-map (kbd "M-S-z"))
              #'my/noema-redo)))

(ert-deftest my/noema-jupyter-cell-point-move-does-not-sync ()
  (let ((buffer (generate-new-buffer "*Noema-jcell-test*"))
        calls)
    (unwind-protect
        (with-current-buffer buffer
          (insert "# Aaronnote cell source: /tmp/note.md\n"
                  "# Aaronnote cell kernel: python3\n"
                  "# Aaronnote cell session: default\n"
                  "# Aaronnote cell storage: script\n"
                  "# %% aaronnote-cell id=one\n"
                  "print(1)\n"
                  "# %% end-aaronnote-cell id=one\n\n"
                  "# %% aaronnote-cell id=two\n"
                  "print(2)\n"
                  "# %% end-aaronnote-cell id=two\n")
          (setq-local my/noema-jupyter-cell-source-file "/tmp/note.md")
          (setq-local my/noema-jupyter-cell-kernel "python3")
          (setq-local my/noema-jupyter-cell-session "default")
          (setq-local my/noema-jupyter-cell-storage "script")
          (my/noema-jupyter-cell-mode 1)
          (goto-char (point-min))
          (search-forward "print(2)")
          (cl-letf (((symbol-function 'my/noema-command)
                     (lambda (command &optional detail)
                       (push (cons command detail) calls))))
            (my/noema-jupyter-cell--post-command-h)
            (should (equal my/noema-jupyter-cell-current-id "two"))
            (should-not calls)
            (my/noema-jupyter-cell-sync-cursor)
            (should (= (length calls) 1))
            (should (equal (caar calls) "jupyter-select-cell"))
            (should (equal (alist-get 'cellId (cdar calls)) "two"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/noema-xwidget-redo-routes-only-aaronnote-buffer ()
  (let ((buffer (generate-new-buffer "*Noema-test*"))
        (my/noema--app-buffer nil)
        sent passed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode))
          (cl-letf (((symbol-function 'my/noema-command)
                     (lambda (command &optional detail)
                       (setq sent (list command detail))))
                    ((symbol-function 'xwidget-webkit-pass-command-event)
                     (lambda (event)
                       (setq passed event))))
            (with-current-buffer buffer
              (setq my/noema--app-buffer buffer)
              (my/noema-xwidget-redo 'aaronnote-event))
            (should (equal sent '("redo" nil)))
            (should-not passed)
            (setq sent nil)
            (with-current-buffer buffer
              (setq my/noema--app-buffer nil)
              (my/noema-xwidget-redo 'other-event))
            (should-not sent)
            (should (eq passed 'other-event))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/noema-xwidget-undo-routes-jupyter-buffer-to-generic-xwidget ()
  (let ((buffer (generate-new-buffer "*Noema-jupyter-test*"))
        (my/noema--app-buffer nil)
        sent passed undo-called)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local my/xwidget--session-id "aaronnote-jupyter"))
          (cl-letf (((symbol-function 'my/noema-command)
                     (lambda (command &optional detail)
                       (setq sent (list command detail))))
                    ((symbol-function 'xwidget-webkit-pass-command-event)
                     (lambda (event)
                       (setq passed event)))
                    ((symbol-function 'my/xwidget-undo)
                     (lambda ()
                       (setq undo-called t))))
            (with-current-buffer buffer
              (my/noema-xwidget-undo 'jupyter-event))
            (should undo-called)
            (should-not sent)
            (should-not passed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/noema-editable-split-opens-fresh-client-xwidget ()
  (let* ((file (make-temp-file "aaronnote-split" nil ".md"))
         (source (generate-new-buffer "*Noema-split-source*"))
         (xwidget-buffer (generate-new-buffer "*Noema-split-xwidget*"))
         (my/noema--port 4242)
         (my/noema--file-buffers (make-hash-table :test #'equal))
         (my/noema--client-buffers (make-hash-table :test #'equal))
         opened-url opened-id opened-display opened-force-new opened-reuse)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local buffer-file-name file))
          (switch-to-buffer source)
          (cl-letf (((symbol-function 'my/noema--ensure-server)
                     (lambda (callback) (funcall callback)))
                    ((symbol-function 'my/noema--split-window)
                     (lambda () (selected-window)))
                    ((symbol-function 'my/xwidget-open-url)
                     (lambda (url &rest args)
                       (setq opened-url url
                             opened-id (plist-get args :id)
                             opened-display (plist-get args :display)
                             opened-force-new (plist-get args :force-new)
                             opened-reuse (plist-get args :reuse-selected))
                       xwidget-buffer)))
            (my/noema-open-current-note-split))
          (should-not (string-match-p "readonly=1" opened-url))
          (should (string-match-p "client=aaronnote-split%3A" opened-url))
          (should (string-prefix-p "aaronnote-split:" opened-id))
          (should (eq opened-display 'current))
          (should opened-force-new)
          (should opened-reuse)
          (should-not (gethash (expand-file-name file) my/noema--file-buffers))
          (should (eq (gethash opened-id my/noema--client-buffers) xwidget-buffer))
          (with-current-buffer xwidget-buffer
            (should
             (equal my/noema-buffer-file-name
                    (my/noema--canonical-file file)))
            (should (equal my/noema--client-id opened-id))
            (should-not my/noema--registered-file)))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p xwidget-buffer) (kill-buffer xwidget-buffer))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest my/noema-split-registration-does-not-steal-canonical-file-buffer ()
  (let* ((file (make-temp-file "aaronnote-split-register" nil ".md"))
         (canonical (generate-new-buffer "*Noema-canonical*"))
         (split (generate-new-buffer "*Noema-split*"))
         (client "aaronnote-split:/tmp/note.md:7")
         (my/noema--file-buffers (make-hash-table :test #'equal))
         (my/noema--client-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (my/noema--register-buffer
           canonical file (my/noema--xwidget-session-id file) nil)
          (my/noema--register-buffer split file client nil)
          (should (eq (gethash (my/noema--canonical-file file)
                               my/noema--file-buffers)
                      canonical))
          (should (eq (gethash client my/noema--client-buffers) split))
          (should (eq (my/noema--buffer-for-file file) canonical))
          (with-current-buffer split
            (should (equal my/noema--xwidget-forced-name
                           (format "*Noema split 7: %s*"
                                   (file-name-nondirectory file))))))
      (when (buffer-live-p canonical) (kill-buffer canonical))
      (when (buffer-live-p split) (kill-buffer split))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest my/noema-run-emacs-key-queues-m-x ()
  (let ((buffer (generate-new-buffer "*Noema-test*"))
        (unread-command-events nil)
        (my/noema--app-buffer nil)
        focused-frame
        edit-mode-arg
        scheduled-fn
        scheduled-args)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq my/noema--app-buffer buffer)
          (cl-letf (((symbol-function 'select-frame-set-input-focus)
                     (lambda (frame) (setq focused-frame frame)))
                    ((symbol-function 'xwidget-webkit-edit-mode)
                     (lambda (arg) (setq edit-mode-arg arg)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest args)
                       (setq scheduled-fn function)
                       (setq scheduled-args args)
                       'mock-timer)))
            (my/noema--run-emacs-key "M-x"))
          (should (equal unread-command-events
                         (listify-key-sequence (kbd "M-x"))))
          (should (eq (selected-window) (get-buffer-window buffer 'visible)))
          (should (eq focused-frame (selected-frame)))
          (should (equal edit-mode-arg -1))
          (should (eq scheduled-fn #'my/noema--focus-forwarded-key-target))
          (should (equal scheduled-args
                         (list (get-buffer-window buffer 'visible)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/noema-run-emacs-key-queues-prefix-sequence ()
  (let ((unread-command-events nil)
        (my/noema--app-buffer nil)
        scheduled-fn
        scheduled-args)
    (cl-letf (((symbol-function 'select-frame-set-input-focus)
               (lambda (_frame) nil))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (setq scheduled-fn function)
                 (setq scheduled-args args)
                 'mock-timer)))
      (my/noema--run-emacs-key "C-x C-f"))
    (should (equal unread-command-events
                   (listify-key-sequence (kbd "C-x C-f"))))
    (should (eq scheduled-fn #'my/noema--focus-forwarded-key-target))
    (should (equal scheduled-args (list (selected-window))))))

(ert-deftest my/noema-focus-forwarded-key-target-focuses-window-left-from-aaronnote ()
  (let ((source (generate-new-buffer "*Noema-source*"))
        (target (generate-new-buffer "*Noema-target*"))
        (my/noema--app-buffer nil)
        focused-frame)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (setq-local major-mode 'xwidget-webkit-mode)
          (setq my/noema--app-buffer source)
          (let ((source-window (selected-window))
                (target-window (split-window-right)))
            (set-window-buffer target-window target)
            (select-window target-window)
            (cl-letf (((symbol-function 'select-frame-set-input-focus)
                       (lambda (frame) (setq focused-frame frame))))
              (my/noema--focus-forwarded-key-target source-window))
            (should (eq (selected-window) target-window))
            (should (eq focused-frame (selected-frame)))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest my/noema-buffer-for-client-finds-unregistered-split ()
  (let ((buffer (generate-new-buffer "*Noema-split*"))
        (my/noema--client-buffers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local my/noema--client-id "aaronnote-split:/tmp/note.md:1"))
          (should-not (gethash "aaronnote-split:/tmp/note.md:1"
                               my/noema--client-buffers))
          (should (eq (my/noema--buffer-for-client
                       "aaronnote-split:/tmp/note.md:1")
                      buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/noema-xwidget-buffer-p-accepts-local-aaronnote-client ()
  (let ((buffer (generate-new-buffer "*Noema-split*"))
        (my/noema--app-buffer nil)
        (my/noema--port nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local my/noema--client-id "aaronnote-split:/tmp/note.md:1"))
          (should (my/noema--xwidget-buffer-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/noema-run-emacs-key-uses-client-source-window ()
  (let ((main (generate-new-buffer "*Noema-main*"))
        (split (generate-new-buffer "*Noema-split*"))
        (my/noema--app-buffer nil)
        (my/noema--client-buffers (make-hash-table :test #'equal))
        (unread-command-events nil)
        released-buffer
        scheduled-fn
        scheduled-args)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (with-current-buffer main
            (setq-local major-mode 'xwidget-webkit-mode)
            (setq-local my/noema--client-id "main-client"))
          (setq my/noema--app-buffer main)
          (let ((main-window (selected-window))
                (split-window (split-window-right)))
            (set-window-buffer split-window split)
            (with-current-buffer split
              (setq-local major-mode 'xwidget-webkit-mode)
              (setq-local my/noema--client-id "split-client"))
            (select-window main-window)
            (cl-letf (((symbol-function 'select-frame-set-input-focus)
                       (lambda (_frame) nil))
                      ((symbol-function 'xwidget-webkit-edit-mode)
                       (lambda (arg)
                         (when (equal arg -1)
                           (setq released-buffer (current-buffer)))))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat function &rest args)
                         (setq scheduled-fn function)
                         (setq scheduled-args args)
                         'mock-timer)))
              (my/noema--run-emacs-key "M-x" "split-client"))
            (should (eq (selected-window) split-window))
            (should (eq released-buffer split))
            (should (equal unread-command-events
                           (listify-key-sequence (kbd "M-x"))))
            (should (eq scheduled-fn #'my/noema--focus-forwarded-key-target))
            (should (equal scheduled-args (list split-window)))))
      (when (buffer-live-p main) (kill-buffer main))
      (when (buffer-live-p split) (kill-buffer split)))))

(ert-deftest my/noema-key-event-passes-client-to-run-emacs-key ()
  (let (seen)
    (cl-letf (((symbol-function 'json-serialize)
               (lambda (&rest _args)
                 (ert-fail "structured key events must not be reserialized")))
              ((symbol-function 'my/noema--run-emacs-key)
               (lambda (key &optional client)
                 (setq seen (list key client)))))
      (my/noema--gateway-event
       '((type . "key")
         (payload . ((key . "M-<right>")
                     (client . "split-client"))))
       nil))
    (should (equal seen '("M-<right>" "split-client")))))

(ert-deftest my/noema-key-event-ignores-non-string-key-data ()
  (let (seen)
    (cl-letf (((symbol-function 'json-serialize)
               (lambda (&rest _args)
                 (ert-fail "structured key events must not be reserialized")))
              ((symbol-function 'my/noema--run-emacs-key)
               (lambda (&rest args) (setq seen args))))
      (should
       (equal
        (my/noema--gateway-event
         '((type . "key")
           (payload . ((key . [1 nil 74])
                       (client . "split-client"))))
         nil)
        '((ok . t)))))
    (should-not seen)))

(ert-deftest my/noema-ui-state-gateway-does-not-reserialize-status ()
  (let ((my/noema-echo-severity 'error)
        (binary-status (unibyte-string 1 255 123))
        seen)
    (cl-letf (((symbol-function 'json-serialize)
               (lambda (&rest _args)
                 (ert-fail "structured UI-state must not be reserialized")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq seen (apply #'format format-string args)))))
      (my/noema--gateway-event
       `((type . "ui-state")
         (payload . ((status . ,binary-status)
                     (severity . "error"))))
       nil))
    (should (string-prefix-p "Noema error: " seen))
    (should-not (string-match-p "parse failed" seen))))

(ert-deftest my/noema-ui-state-ignores-malformed-status-fields ()
  (let ((my/noema-echo-severity 'error)
        seen)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args seen))))
      (should
       (equal
        (my/noema--gateway-event
         '((type . "ui-state")
           (payload . ((status . [1 nil 123])
                       (severity . ["error"]))))
         nil)
        '((ok . t)))))
    (should-not seen)))

(ert-deftest my/noema-zotero-events-dispatch-structured-payloads ()
  (let (seen)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (apply function args)))
              ((symbol-function 'my/noema--run-zotero-event)
               (lambda (payload import-p)
                 (push (list payload import-p) seen))))
      (my/noema--gateway-event
       '((type . "zotero")
         (payload . ((key . "Str87")
                     (doi . "10.1515/example"))))
       nil)
      (my/noema--gateway-event
       '((type . "zotero-import")
         (payload . ((currentFile . "/tmp/note.md")
                     (targetFile . "/tmp/bib/test.bib"))))
       nil))
    (should (equal (cadar seen) t))
    (should (equal (alist-get 'targetFile (caar seen))
                   "/tmp/bib/test.bib"))
    (should-not (cadadr seen))
    (should (equal (alist-get 'key (caadr seen)) "Str87"))))

(ert-deftest my/zotero-reference-result-falls-back-from-bib-key-to-doi ()
  (let ((expected '((id . "http://zotero.org/users/1/items/C4ULFN2X")
                    (citekey . "Strassen")
                    (DOI . "10.1515/example")))
        calls)
    (cl-letf (((symbol-function 'my/zotero-better-bibtex-search)
               (lambda (terms)
                 (push terms calls)
                 (if (equal terms [["DOI" "is" "10.1515/example"]])
                     (list expected)
                   nil))))
      (let ((result (my/zotero-reference-result
                     '((key . "Str87") (doi . "https://doi.org/10.1515/example")))))
        (should (equal result expected))
        (should (= (length calls) 1))
        (should (equal (my/zotero-result-select-uri result)
                       "zotero://select/library/items/C4ULFN2X"))))))

(ert-deftest my/zotero-append-bibtex-does-not-duplicate-a-key ()
  (let ((file (make-temp-file "aaronnote-zotero-test-" nil ".bib"))
        (bibtex "@article{UniqueKey,\n  title = {One}\n}\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _args)
                     (error "should not visit BibTeX target")))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (&rest _args)
                     (error "should not display BibTeX target"))))
          (my/zotero-append-bibtex file bibtex)
          (my/zotero-append-bibtex file bibtex)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (should (re-search-forward "^@article{UniqueKey," nil t))
            (should-not (re-search-forward "^@article{UniqueKey," nil t)))
          (should-not (get-file-buffer file)))
      (when-let* ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest my/noema-windmove-focus-advice-focuses-aaronnote-target ()
  (let ((source (generate-new-buffer "*Noema-source*"))
        (target (generate-new-buffer "*Noema-target*"))
        (my/noema--app-buffer nil)
        focused-buffer)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (let ((target-window (split-window-right)))
            (set-window-buffer target-window target)
            (with-current-buffer target
              (setq-local major-mode 'xwidget-webkit-mode))
            (setq my/noema--app-buffer target)
            (cl-letf (((symbol-function 'my/xwidget-focus)
                       (lambda (&optional buffer)
                         (setq focused-buffer buffer))))
              (should (eq (my/noema--windmove-focus-advice
                           (lambda ()
                             (select-window target-window)
                             'moved))
                          'moved)))
            (should (eq (selected-window) target-window))
            (should (eq focused-buffer target))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest my/noema-focus-move-between-aaronnote-windows-releases-source-and-focuses-target ()
  (let ((source (generate-new-buffer "*Noema-source*"))
        (target (generate-new-buffer "*Noema-split*"))
        (released-buffer nil)
        (focused-buffer nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer source)
          (with-current-buffer source
            (setq-local major-mode 'xwidget-webkit-mode))
          (let ((source-window (selected-window))
                (target-window (split-window-right)))
            (set-window-buffer target-window target)
            (with-current-buffer target
              (setq-local major-mode 'xwidget-webkit-mode))
            (select-window target-window)
            (cl-letf (((symbol-function 'my/noema--xwidget-buffer-p)
                       (lambda (&optional buffer)
                         (memq (or buffer (current-buffer)) (list source target))))
                      ((symbol-function 'xwidget-webkit-edit-mode)
                       (lambda (arg)
                         (when (equal arg -1)
                           (setq released-buffer (current-buffer)))))
                      ((symbol-function 'my/xwidget-focus)
                       (lambda (&optional buffer)
                         (setq focused-buffer buffer))))
              (my/noema--focus-selected-window-after-move source-window))
            (should (eq released-buffer source))
            (should (eq focused-buffer target))
            (should (eq my/noema--app-buffer target))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target)))))

(ert-deftest my/noema-gateway-event-handles-ready ()
  (let* ((my/noema--port nil)
         (my/noema--ready nil)
         (flush-count 0))
    (cl-letf (((symbol-function 'my/noema--flush-ready-callbacks)
               (lambda () (cl-incf flush-count))))
      (my/noema--gateway-event
       '((type . "ready") (payload . ((port . 50815)))) nil)
      (should my/noema--ready)
      (should (= my/noema--port 50815))
      (should (= flush-count 1)))))

(ert-deftest my/noema-gateway-event-ignores-invalid-ready-port ()
  (let* ((my/noema--port nil)
         (my/noema--ready nil)
         (flush-count 0))
    (cl-letf (((symbol-function 'my/noema--flush-ready-callbacks)
               (lambda () (cl-incf flush-count))))
      (my/noema--gateway-event
       '((type . "ready") (payload . ((port . "not-a-port")))) nil)
      (should-not my/noema--ready)
      (should-not my/noema--port)
      (should (= flush-count 0)))))

(ert-deftest my/noema-sentinel-resets-dead-current-process ()
  (let* ((buffer (generate-new-buffer " *Noema-sentinel-test*"))
         (proc (make-process
                :name "aaronnote-sentinel-test"
                :buffer buffer
                :command (list "cat")))
         (my/noema--process proc)
         (my/noema--port 50815)
         (my/noema--ready t)
         (my/noema--ready-callbacks (list #'ignore))
         (my/noema--ready-watchdog nil))
    (unwind-protect
        (progn
          (delete-process proc)
          (my/noema--sentinel proc "exited abnormally with code 1\n")
          (should-not my/noema--process)
          (should-not my/noema--port)
          (should-not my/noema--ready)
          (should-not my/noema--ready-callbacks))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my/noema-active-focus-restarts-core-on-the-same-port ()
  "An active retained page restarts core without killing or refreshing it."
  (let* ((buffer (generate-new-buffer "*Noema-active-reconnect*"))
         (my/noema--app-buffer buffer)
         (my/noema--process nil)
         (my/noema--ready nil)
         (my/noema--last-port 50815)
         (my/noema--ready-callbacks nil)
         (my/noema--ready-watchdog nil)
         started-port
         killed-browser)
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema--app-buffer-visible-p)
                   (lambda () t))
                  ((symbol-function 'my/noema--start-server)
                   (lambda (&optional port) (setq started-port port)))
                  ((symbol-function 'my/appine-kill-all)
                   (lambda () (setq killed-browser t)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _args) 'reconnect-watchdog)))
          (my/noema--maybe-reconnect-core-on-activity)
          (should (= started-port 50815))
          (should-not killed-browser)
          (should (memq #'my/noema--notify-xwidgets-core-ready
                        my/noema--ready-callbacks))
          (should (eq my/noema--ready-watchdog 'reconnect-watchdog)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/noema-inactive-page-does-not-restart-core ()
  "A dead core remains dormant until the Aaronnote page is actively focused."
  (let* ((buffer (generate-new-buffer "*Noema-inactive-reconnect*"))
         (my/noema--app-buffer buffer)
         (my/noema--process nil)
         (my/noema--ready nil)
         (my/noema--last-port 50815)
         (my/noema--ready-callbacks nil)
         started)
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema--app-buffer-visible-p)
                   (lambda () nil))
                  ((symbol-function 'my/noema--start-server)
                   (lambda (&optional _port) (setq started t))))
          (my/noema--maybe-reconnect-core-on-activity)
          (should-not started)
          (should-not my/noema--ready-callbacks))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/noema-core-ready-reconnects-retained-xwidget-in-place ()
  (let* ((buffer (generate-new-buffer "*Noema-retained-xwidget*"))
         (my/noema--app-buffer buffer)
         executed-script)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local major-mode 'xwidget-webkit-mode))
          (cl-letf (((symbol-function 'xwidget-webkit-current-session)
                     (lambda () 'mock-session))
                    ((symbol-function 'xwidget-webkit-execute-script)
                     (lambda (_session script &optional _callback)
                       (setq executed-script script))))
            (my/noema--notify-xwidgets-core-ready)
            (should (equal executed-script my/noema--core-ready-script))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest my/noema-runtime-status-renders-debug-payload ()
  (let ((my/noema--ready t)
        channel args displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema--api-call-sync)
                   (lambda (c a)
                     (setq channel c
                           args a)
                     (let ((payload (make-hash-table :test 'equal))
                           (sync (make-hash-table :test 'equal)))
                       (puthash "type" "runtime-debug" payload)
                       (puthash "queued" :false sync)
                       (puthash "roamDbSync" sync payload)
                       payload)))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args)
                     (setq displayed buffer)
                     buffer)))
          (my/noema-runtime-status)
          (should (equal channel "aaronnote:api:runtime:debug"))
          (should (equal args []))
          (should (eq displayed (get-buffer "*Noema runtime status*")))
          (with-current-buffer "*Noema runtime status*"
            (should (derived-mode-p 'special-mode))
            (should (string-match-p "\"runtime-debug\"" (buffer-string)))
            (should (string-match-p "\"emacsActivity\"" (buffer-string)))))
      (when-let* ((buffer (get-buffer "*Noema runtime status*")))
        (kill-buffer buffer)))))

(ert-deftest my/noema-api-call-uses-nonblocking-gateway-request ()
  (let ((my/noema--ready t)
        request
        callback-result)
    (cl-letf (((symbol-function 'remote-gateway-find-client)
               (lambda (_client-id) 'mock-client))
              ((symbol-function 'remote-gateway-request-sync)
               (lambda (&rest _args)
                 (ert-fail "async Noema API must not wait synchronously")))
              ((symbol-function 'remote-gateway-request-async)
               (lambda (client method params callback timeout)
                 (setq request (list client method params timeout))
                 (funcall callback '((ok . t)) nil)
                 '(done))))
      (my/noema--api-call
       "aaronnote:api:test" [1 2]
       (lambda (result) (setq callback-result result)))
      (should
       (equal request
              '(mock-client "aaronnote.api"
                            ((channel . "aaronnote:api:test")
                             (args . [1 2]))
                            10)))
      (should (equal callback-result '((ok . t)))))))

(ert-deftest my/noema-stop-releases-gateway-binding ()
  (let ((my/noema--gateway-binding '(:binding-id "binding-test"))
        (my/noema--process nil)
        released)
    (cl-letf (((symbol-function 'remote-gateway-release-binding)
               (lambda (binding &optional disconnect)
                 (setq released (list binding disconnect))))
              ((symbol-function 'my/noema--remove-activity-hooks)
               #'ignore))
      (my/noema-stop)
      (should
       (equal released
              '((:binding-id "binding-test") t)))
      (should-not my/noema--gateway-binding))))

(ert-deftest my/noema-watchdog-clears-stale-ready-callbacks ()
  (let ((my/noema--ready nil)
        (my/noema--ready-watchdog 'mock-timer)
        (my/noema--ready-callbacks (list #'ignore)))
    (my/noema--watchdog-fire)
    (should-not my/noema--ready-watchdog)
    (should-not my/noema--ready-callbacks)))

(ert-deftest my/noema-activity-hooks-install-idempotently ()
  (let ((after-focus-change-function nil)
        (window-buffer-change-functions nil)
        (window-selection-change-functions nil)
        (my/noema--activity-hooks-installed nil)
        (my/noema--activity-timer nil)
        (my/noema--paused t)
        (my/noema--manual-paused t))
    (my/noema--install-activity-hooks)
    (my/noema--install-activity-hooks)
    (should my/noema--activity-hooks-installed)
    (should (equal window-buffer-change-functions
                   (list #'my/noema--update-activity)))
    (should (equal window-selection-change-functions
                   (list #'my/noema--update-activity)))
    (my/noema--remove-activity-hooks)
    (should-not my/noema--activity-hooks-installed)
    (should-not window-buffer-change-functions)
    (should-not window-selection-change-functions)
    (should-not my/noema--paused)
    (should-not my/noema--manual-paused)))

(ert-deftest my/noema-activity-update-debounces-by-visibility-state ()
  (let ((my/noema--activity-timer nil)
        (my/noema--last-activity-active :unknown)
        (my/noema--app-buffer nil)
        (run-at-count 0)
        (idle-count 0)
        visible)
    (cl-letf (((symbol-function 'my/noema--app-buffer-visible-p)
               (lambda () visible))
              ((symbol-function 'my/noema--xwidget-buffer-p)
               (lambda (_buffer) nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq run-at-count (1+ run-at-count))
                 'normal-timer))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _args)
                 (setq idle-count (1+ idle-count))
                 'idle-timer))
              ((symbol-function 'cancel-timer)
               (lambda (_timer) nil)))
      (setq visible nil)
      (my/noema--update-activity)
      (my/noema--update-activity)
      (should (= run-at-count 1))
      (should (= idle-count 0))
      (should (eq my/noema--activity-timer 'normal-timer))
      (setq visible t)
      (my/noema--update-activity)
      (should (= run-at-count 1))
      (should (= idle-count 1))
      (should (eq my/noema--activity-timer 'idle-timer)))))

(ert-deftest my/noema-manual-pause-blocks-auto-resume ()
  (let ((my/noema--paused nil)
        (my/noema--manual-paused nil)
        commands)
    (cl-letf (((symbol-function 'my/noema--send-command)
               (lambda (command) (push command commands)))
              ((symbol-function 'my/noema--app-buffer-visible-p)
               (lambda () t)))
      (my/noema-pause)
      (should my/noema--paused)
      (should my/noema--manual-paused)
      (should (equal (nreverse commands) '("pause")))
      (setq commands nil)
      (my/noema--apply-activity t)
      (should my/noema--paused)
      (should-not commands)
      (my/noema-resume)
      (should-not my/noema--paused)
      (should-not my/noema--manual-paused)
      (should (equal (nreverse commands) '("resume"))))))

(ert-deftest my/noema-toggle-pause-sends-one-transition ()
  (let ((my/noema--paused nil)
        (my/noema--manual-paused nil)
        commands)
    (cl-letf (((symbol-function 'my/noema--send-command)
               (lambda (command) (push command commands)))
              ((symbol-function 'my/noema--app-buffer-visible-p)
               (lambda () t)))
      (my/noema-toggle-pause)
      (should my/noema--manual-paused)
      (should my/noema--paused)
      (my/noema-toggle-pause)
      (should-not my/noema--manual-paused)
      (should-not my/noema--paused)
      (should (equal (nreverse commands) '("pause" "resume"))))))

(ert-deftest my/noema-process-filter-ignores-stale-proc-ready-line ()
  "A dying old process emitting a ready: line must not clobber the new port."
  (let* ((buffer (generate-new-buffer " *Noema-stale-proc-test*"))
         (stale-proc (make-process
                      :name "aaronnote-stale-test"
                      :buffer buffer
                      :command (list "cat")))
         ;; Simulate a new process by using a different proc object as current.
         (new-proc (make-process
                    :name "aaronnote-new-test"
                    :buffer buffer
                    :command (list "cat")))
         (my/noema--process new-proc)
         (my/noema--port 60000)
         (my/noema--ready t)
         (flush-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'my/noema--flush-ready-callbacks)
                   (lambda () (cl-incf flush-count))))
          ;; stale-proc (not current) emits a ready: line with a different port.
          (my/noema--process-filter stale-proc "aaronote-web-host:ready:99999\n")
          ;; Port and ready state must be unchanged.
          (should (= my/noema--port 60000))
          (should my/noema--ready)
          (should (= flush-count 0)))
      (dolist (p (list stale-proc new-proc))
        (when (process-live-p p) (delete-process p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defmacro my/noema-test--with-xwidget-mocks (&rest body)
  "Run BODY with mocked xwidget opener state."
  (declare (indent 0))
  `(let ((my/noema--file-buffers (make-hash-table :test #'equal))
         (my/noema--client-buffers (make-hash-table :test #'equal))
         (my/noema--app-buffer nil)
         (my/noema--port 50815)
         opened-urls
         buffers)
     (cl-letf (((symbol-function 'my/xwidget-open-url)
                (lambda (url &rest _args)
                  (push url opened-urls)
                  (let ((buffer (generate-new-buffer "*mock-xwidget*")))
                    (push buffer buffers)
                    (with-current-buffer buffer
                      (setq-local major-mode 'xwidget-webkit-mode))
                    (switch-to-buffer buffer)
                    buffer)))
               ((symbol-function 'my/xwidget-session-buffer)
                (lambda (_id) nil))
               ((symbol-function 'my/xwidget-focus)
                (lambda (&optional _buffer) nil))
               ((symbol-function 'run-at-time)
                (lambda (&rest _args) 'mock-timer))
               ((symbol-function 'my/noema--refresh-visible-ibuffers)
                (lambda () nil)))
       (unwind-protect
           (progn ,@body)
         (dolist (buffer buffers)
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))))

(ert-deftest my/noema-open-xwidget-reuses-file-buffer ()
  (my/noema-test--with-xwidget-mocks
    (let* ((file (expand-file-name "note.md" temporary-file-directory))
           (buf1 (my/noema--open-xwidget "ignored" file))
           (buf2 (my/noema--open-xwidget "ignored-again" file)))
      (should (eq buf1 buf2))
      (should (= (length opened-urls) 1))
      (should
       (equal (my/noema-buffer-file buf1)
              (my/noema--canonical-file file)))
      (should (string-match-p "\\*Noema: note\\.md\\*" (buffer-name buf1)))
      (should (string-match-p "client=" (car opened-urls))))))

(ert-deftest my/noema-open-xwidget-keeps-files-distinct ()
  (my/noema-test--with-xwidget-mocks
    (let* ((file1 (expand-file-name "one.md" temporary-file-directory))
           (file2 (expand-file-name "two.md" temporary-file-directory))
           (buf1 (my/noema--open-xwidget "ignored" file1))
           (buf2 (my/noema--open-xwidget "ignored" file2)))
      (should-not (eq buf1 buf2))
      (should (= (length opened-urls) 2))
      (should
       (eq (gethash (my/noema--canonical-file file1)
                    my/noema--file-buffers)
           buf1))
      (should
       (eq (gethash (my/noema--canonical-file file2)
                    my/noema--file-buffers)
           buf2)))))

(ert-deftest my/noema-kill-buffer-cleans-registries ()
  (my/noema-test--with-xwidget-mocks
    (let* ((file (expand-file-name "cleanup.md" temporary-file-directory))
           (client (my/noema--xwidget-session-id file))
           (buffer (my/noema--open-xwidget "ignored" file))
           (canonical-file (my/noema--canonical-file file))
           posted)
      (should
       (eq (gethash canonical-file my/noema--file-buffers) buffer))
      (should (eq (gethash client my/noema--client-buffers) buffer))
      (cl-letf (((symbol-function 'my/noema--post)
                 (lambda (payload) (push payload posted))))
        (kill-buffer buffer))
      (should-not (gethash canonical-file my/noema--file-buffers))
      (should-not (gethash client my/noema--client-buffers))
      (should (equal (alist-get 'type (car posted)) "client-close"))
      (should (equal (alist-get 'client (car posted)) client))
      (should (equal (alist-get 'file (car posted)) file)))))

(ert-deftest my/noema-current-file-client-targets-buffer ()
  (my/noema-test--with-xwidget-mocks
    (let* ((file1 (expand-file-name "first.md" temporary-file-directory))
           (file2 (expand-file-name "second.md" temporary-file-directory))
           (buf1 (my/noema--open-xwidget "ignored" file1))
           (buf2 (my/noema--open-xwidget "ignored" nil)))
      (my/noema--track-app-buffer buf2 nil "client-two")
      (setq my/noema--app-buffer buf1)
      (my/noema--sync-app-buffer-file file2 "client-two")
      (should (eq my/noema--app-buffer buf2))
      (should
       (equal (my/noema-buffer-file buf2)
              (my/noema--canonical-file file2)))
      (should
       (eq (gethash (my/noema--canonical-file file2)
                    my/noema--file-buffers)
           buf2)))))

(ert-deftest my/noema-canonical-buffer-prefers-registered-buffer ()
  (my/noema-test--with-xwidget-mocks
    (let* ((file (expand-file-name "duplicate.md" temporary-file-directory))
           (canonical (my/noema--open-xwidget "ignored" file))
           (duplicate (generate-new-buffer "*Noema: duplicate.md*")))
      (push duplicate buffers)
      (with-current-buffer duplicate
        (setq-local major-mode 'xwidget-webkit-mode)
        (setq-local my/noema-buffer-file-name file))
      (should (eq (my/noema-canonical-buffer duplicate) canonical)))))

(ert-deftest my/noema-jupyter-sidecars-are-logical-and-confined ()
  (should
   (equal
    (my/noema-jupyter--file
     '((file . "/fs:local:/tmp/notes/.cell/note.python.default.py")))
    "/fs:local:/tmp/notes/.cell/note.python.default.py"))
  (should-error
   (my/noema-jupyter--file
    '((file . "/fs:local:/tmp/notes/note.md")))
   :type 'error)
  (should-error
   (my/noema-jupyter--file
    '((file . "/fs:local:/tmp/notes/.cell/nested/note.py")))
   :type 'error))

(ert-deftest my/noema-jupyter-registers-remote-broker-methods ()
  (dolist (method
           '("aaronnote.jupyter.kernels"
             "aaronnote.jupyter.launch"
             "aaronnote.jupyter.status"
             "aaronnote.jupyter.interrupt"
             "aaronnote.jupyter.restart"
             "aaronnote.jupyter.shutdown"
             "aaronnote.jupyter.read-nbextension"
             "aaronnote.jupyter.file.read"
             "aaronnote.jupyter.file.write"
             "aaronnote.jupyter.file.delete"
             "aaronnote.jupyter.file.stat"))
    (should (functionp (gethash method remote-gateway--methods))))
  (should
   (memq #'my/noema-jupyter--doctor
         remote-doctor-check-functions)))

(provide 'init-aaronnote-tests)
;;; init-aaronnote-tests.el ends here

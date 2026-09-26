;;; jupyter-local-remote-lsp-live-smoke.el --- Local file, SSH kernel, local LSP -*- lexical-binding: t; -*-
;; Opt-in live check.  The SSH kernelspec is metadata only; no SSH is needed.
(require 'init-aaronnote-jupyter-lsp)
(require 'init-lsp)
(load (expand-file-name "test/lsp-live-smoke.el" user-emacs-directory) nil t)

(let* ((directory (file-truename (make-temp-file "noema-local-kernel-" t)))
       (file (expand-file-name "analysis.ipynb" directory))
       (target (or (getenv "JUPYTER_PROJECT_TARGET") "aaron-pc"))
       (entry `((name . "ssh-kernel")
                (spec . ((argv . ["python" "-m" "remote_ikernel"
                                  "--interface" "ssh" "--host" ,target
                                  "--workdir" "/tmp" "--kernel_cmd"
                                  "/usr/bin/python3 -m ipykernel -f {host_connection_file}"])))))
       notebook)
  (unwind-protect
      (progn
        (with-temp-file file
          (insert
           "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,"
           "\"id\":\"local-analysis\",\"metadata\":{},\"outputs\":[],"
           "\"source\":[\"import sys\\n\",\"value = sys.version\\n\"]}],"
           "\"metadata\":{\"kernelspec\":{\"display_name\":\"SSH kernel\","
           "\"language\":\"python\",\"name\":\"ssh-kernel\"}},"
           "\"nbformat\":4,\"nbformat_minor\":5}\n"))
        (setq notebook (cl-letf (((symbol-function 'my/noema--ensure-server) #'ignore))
                         (find-file-noselect file)))
        (switch-to-buffer notebook)
        (setq-local my/noema-jupyter-cell-kernel "ssh-kernel"
                    my/noema-jupyter-cell-kernel-spec entry
                    my/language-server--manual-start t
                    lsp-auto-guess-root t
                    lsp-guess-root-without-session t)
        (my/noema-jupyter-project-lsp)
        (when (and (bound-and-true-p lsp--buffer-deferred)
                   (fboundp 'lsp--init-if-visible))
          (lsp--init-if-visible))
        (unless (my/lsp-live-smoke--wait 35)
          (error "Local notebook LSP did not connect"))
        (unless (equal (remote-context-target-id
                        (remote-context
                         (my/language-server--project-root-for-buffer)))
                       "local")
          (error "SSH kernel redirected the file's LSP target"))
        (unless (and (equal buffer-file-name file)
                     (equal lsp-buffer-uri (lsp--path-to-uri file)))
          (error "SSH kernel changed notebook file or LSP document identity: file=%S expected=%S uri=%S"
                 buffer-file-name (lsp--path-to-uri file) lsp-buffer-uri))
        (when (my/language-server-runtime-p my/language-server-runtime-current)
          (error "SSH kernel created a kernel-owned LSP runtime"))
        (princ "PASS local notebook with SSH kernel uses its local Python LSP\n"))
    (when (buffer-live-p notebook)
      (with-current-buffer notebook
        (dolist (workspace (ignore-errors (lsp-workspaces)))
          (ignore-errors
            (my/lsp-mode-shutdown-workspace workspace 'live-smoke)))
        (set-buffer-modified-p nil))
      (kill-buffer notebook))
    (delete-directory directory t)))

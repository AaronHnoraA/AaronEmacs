;;; init-debug.el --- DAP debugging workflow -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Dape is the primary debug adapter client.  This module keeps the debugging
;; UI, breakpoint gutter behavior, common adapter defaults, and diagnostics in
;; one place so LSP setup can stay focused on language servers.

;;; Code:

(require 'cl-lib)
(require 'init-funcs)
(require 'seq)
(require 'subr-x)
(require 'transient)

(eval-when-compile
  (ignore-errors
    (require 'hydra)))

(declare-function dape "dape" (config &optional skip-compile))
(declare-function dape--config-ensure "dape" (config &optional signal))
(declare-function dape--live-connection "dape" (type &optional nowarn require-selected))
(declare-function dape--display-buffer "dape" (buffer))
(declare-function dape--info-get-buffer-create "dape" (mode &optional index))
(declare-function dape-buffer-default "dape" ())
(declare-function dape-breakpoint-expression "dape" (expression))
(declare-function dape-breakpoint-function "dape" (name))
(declare-function dape-breakpoint-hits "dape" (condition))
(declare-function dape-breakpoint-log "dape" (message))
(declare-function dape-breakpoint-load "dape" (&optional filename))
(declare-function dape-breakpoint-remove-all "dape" ())
(declare-function dape-breakpoint-remove-at-point "dape" (&optional skip-notify))
(declare-function dape-breakpoint-save "dape" (&optional filename))
(declare-function dape-breakpoint-toggle "dape" ())
(declare-function dape-continue "dape" (conn))
(declare-function dape-cwd "dape" ())
(declare-function dape-disassemble "dape" (address &optional display-p))
(declare-function dape-disconnect-quit "dape" (conn))
(declare-function dape-evaluate-expression "dape" (conn expression &optional context))
(declare-function dape-info "dape" (&optional maybe-kill))
(declare-function dape-info-update "dape" ())
(declare-function dape-kill "dape" (conn &optional cb with-disconnect))
(declare-function dape-memory "dape" (address &optional reuse-buffer))
(declare-function dape-next "dape" (conn))
(declare-function dape-pause "dape" (conn))
(declare-function dape-quit "dape" ())
(declare-function dape-repl "dape" ())
(declare-function dape-restart "dape" (&optional conn skip-compile))
(declare-function dape-restart-frame "dape" (conn stack-id))
(declare-function dape-select-session "dape" (conn))
(declare-function dape-select-stack "dape" (conn stack-id))
(declare-function dape-select-thread "dape" (conn thread-id))
(declare-function dape-stack-select-down "dape" (conn n))
(declare-function dape-stack-select-up "dape" (conn n))
(declare-function dape-step-in "dape" (conn))
(declare-function dape-step-out "dape" (conn))
(declare-function dape-until "dape" (conn))
(declare-function dape-watch-dwim "dape" (expression &optional remove-only-p add-only-p display-p))
(declare-function dape-breakpoint-global-mode "dape" (&optional arg))
(declare-function my/debug-profile-dispatch "init-debug-profile" ())
(declare-function my/debug-profile-rerun "init-debug-profile" ())

(defvar dape-adapter-dir)
(defvar dape-breakpoint-mode-map)
(defvar dape-buffer-window-arrangement)
(defvar dape-configs)
(defvar dape-default-breakpoints-file)
(defvar dape-info-buffer-window-groups)
(defvar dape--connections)
(defvar my/dape-state-dir)

(defgroup my/debug nil
  "Debug adapter workflow helpers."
  :group 'tools)

(defcustom my/debug-common-adapter-specs
  '((python
     :title "Python"
     :configs (debugpy debugpy-module python-file python-module)
     :commands ("python" "python3")
     :install "python -m pip install debugpy")
	    (javascript
	     :title "JavaScript / TypeScript / Chrome"
	     :configs (js-debug-node js-debug-ts-node js-debug-tsx js-debug-node-attach
	               js-debug-chrome node-file node-attach chrome ts-node tsx-file)
	     :commands ("node")
	     :install "Install vscode-js-debug into dape-adapter-dir/js-debug.")
	    (c-cpp-rust
	     :title "C / C++ / Rust"
	     :configs (lldb-dap lldb-vscode gdb cpptools rust-lldb c-cpp-lldb c-cpp-gdb)
     :commands ("lldb-dap" "lldb-vscode" "gdb")
     :install "Install lldb-dap/lldb-vscode or gdb >= 14.1; cpptools needs the cpptools adapter.")
    (go
     :title "Go"
     :configs (dlv gdb-go gdb-go-test go-dlv go-test)
     :commands ("dlv" "gdb")
     :install "go install github.com/go-delve/delve/cmd/dlv@latest")
    (shell
     :title "Shell"
     :configs (bash-debug bash-script)
     :commands ("bash" "node")
     :install "Install bash-debug into dape-adapter-dir/bash-debug.")
    (dotnet
     :title ".NET / C#"
     :configs (netcoredbg dotnet)
     :commands ("netcoredbg")
     :install "Install netcoredbg.")
    (php
     :title "PHP"
     :configs (xdebug php-xdebug)
     :commands ("node")
     :install "Install php-debug into dape-adapter-dir/php-debug and enable Xdebug.")
    (ruby
     :title "Ruby"
     :configs (rdbg ruby-rdbg)
     :commands ("rdbg")
     :install "gem install debug")
    (ocaml
     :title "OCaml"
     :configs (ocamlearlybird ocaml-earlybird)
     :commands ("ocamlearlybird")
     :install "opam install earlybird"))
  "Common debug adapters shown by `my/debug-adapter-doctor'."
  :type '(repeat sexp)
  :group 'my/debug)

(defvar my/debug-after-register-common-configs-hook nil
  "Hook run after common Dape configs are registered.
Language modules should use this hook to add language-specific debug configs.")

(defun my/debug-register-adapter-spec (name &rest plist)
  "Register adapter doctor metadata NAME with PLIST."
  (setf (alist-get name my/debug-common-adapter-specs nil nil #'eq) plist))

(defun my/debug--plist-merge (base overrides)
  "Return BASE plist with OVERRIDES applied."
  (let ((result (copy-tree base))
        (tail (copy-tree overrides)))
    (while tail
      (setq result (plist-put result (pop tail) (pop tail))))
    result))

(defun my/debug-register-config-alias (alias base &rest overrides)
  "Register ALIAS as BASE Dape config with OVERRIDES."
  (when-let* ((config (copy-tree (alist-get base dape-configs nil nil #'eq))))
    (setf (alist-get alias dape-configs nil nil #'eq)
          (my/debug--plist-merge config overrides))))

(defun my/debug--executable-file-p (file)
  "Return non-nil when FILE is a regular executable file."
  (and (stringp file)
       (file-regular-p file)
       (file-executable-p file)
       (not (string-match-p
             (rx (or "/." ".dSYM" ".so" ".dylib" ".dll" ".a" ".o" ".rlib")
                 string-end)
             file))))

(defun my/debug--project-executable-candidates (&optional root)
  "Return likely executable files below ROOT."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root default-directory))))
         (patterns '("a.out"
                     "main"
                     "bin/*"
                     "build/*"
                     "target/debug/*"
                     "target/debug/examples/*"
                     "target/release/*"
                     "target/release/examples/*"))
         (files (seq-mapcat
                 (lambda (pattern)
                   (file-expand-wildcards (expand-file-name pattern root)))
                 patterns)))
    (delete-dups
     (seq-filter #'my/debug--executable-file-p files))))

(defun my/debug-read-project-program ()
  "Read a compiled program path for native debuggers."
  (let* ((root (file-name-as-directory (expand-file-name (dape-cwd))))
         (candidates (my/debug--project-executable-candidates root))
         (choice (if candidates
                     (completing-read "Program: " candidates nil t nil nil
                                      (car candidates))
                   (read-file-name "Program: " root nil t))))
    (expand-file-name choice root)))

(defun my/debug-register-common-configs ()
  "Register friendly Dape aliases for common language workflows."
  (my/debug-register-config-alias
   'python-file 'debugpy
   :name "Python: current file"
   :program #'dape-buffer-default)
  (my/debug-register-config-alias
   'python-module 'debugpy-module
   :name "Python: module")
  (my/debug-register-config-alias
   'node-file 'js-debug-node
   :name "Node: current file"
   :skipFiles ["<node_internals>/**" "${workspaceFolder}/node_modules/**/*.js"])
  (my/debug-register-config-alias
   'node-attach 'js-debug-node-attach
   :name "Node: attach 9229")
  (my/debug-register-config-alias
   'chrome 'js-debug-chrome
   :name "Chrome: localhost:3000")
  (my/debug-register-config-alias
   'ts-node 'js-debug-ts-node
   :name "TypeScript: ts-node")
  (my/debug-register-config-alias
   'tsx-file 'js-debug-tsx
   :name "TypeScript/TSX: tsx")
  (my/debug-register-config-alias
   'go-dlv 'dlv
   :name "Go: package")
  (my/debug-register-config-alias
   'go-test 'gdb-go-test
   :name "Go: test binary")
  (my/debug-register-config-alias
   'rust-lldb 'lldb-dap
   :name "Rust: lldb-dap"
   :program #'my/debug-read-project-program)
  (my/debug-register-config-alias
   'c-cpp-lldb 'lldb-dap
   :name "C/C++: lldb-dap"
   :program #'my/debug-read-project-program)
  (my/debug-register-config-alias
   'c-cpp-gdb 'gdb
   :name "C/C++: gdb"
   :program #'my/debug-read-project-program)
  (my/debug-register-config-alias
   'bash-script 'bash-debug
   :name "Bash: current script")
  (my/debug-register-config-alias
   'dotnet 'netcoredbg
   :name ".NET: current project")
  (my/debug-register-config-alias
   'php-xdebug 'xdebug
   :name "PHP: Xdebug")
  (my/debug-register-config-alias
   'ruby-rdbg 'rdbg
   :name "Ruby: current file")
  (my/debug-register-config-alias
   'ocaml-earlybird 'ocamlearlybird
   :name "OCaml: Earlybird"))

(defun my/debug--goto-event-line (event)
  "Select EVENT window and move point to the clicked line."
  (let* ((start (event-start event))
         (window (posn-window start)))
    (unless (windowp window)
      (user-error "No window for mouse event"))
    (select-window window)
    (condition-case nil
        (posn-set-point start)
      (error
       (let* ((row (cdr (posn-col-row start)))
              (row (if (integerp row) row 0)))
         (goto-char (window-start window))
         (forward-line row))))
    (beginning-of-line)))

(defun my/debug-breakpoint-toggle-mouse (event)
  "Toggle a Dape breakpoint on the clicked line."
  (interactive "e")
  (save-selected-window
    (save-excursion
      (my/debug--goto-event-line event)
      (call-interactively #'dape-breakpoint-toggle))))

(defun my/debug-breakpoint-expression-mouse (event)
  "Set a conditional Dape breakpoint on the clicked line."
  (interactive "e")
  (save-selected-window
    (save-excursion
      (my/debug--goto-event-line event)
      (call-interactively #'dape-breakpoint-expression))))

(defun my/debug-breakpoint-log-mouse (event)
  "Set a Dape logpoint on the clicked line."
  (interactive "e")
  (save-selected-window
    (save-excursion
      (my/debug--goto-event-line event)
      (call-interactively #'dape-breakpoint-log))))

(defun my/debug-configure-breakpoint-gutter ()
  "Bind Dape breakpoints to the line-number/margin gutter.
The left fringe is left mostly free so fold indicators keep their normal
single-click behavior; use shift-click there for breakpoints."
  (when (boundp 'dape-breakpoint-mode-map)
    (define-key dape-breakpoint-mode-map [left-margin mouse-1]
                #'my/debug-breakpoint-toggle-mouse)
    (define-key dape-breakpoint-mode-map [left-margin mouse-2]
                #'my/debug-breakpoint-expression-mouse)
    (define-key dape-breakpoint-mode-map [left-margin mouse-3]
                #'my/debug-breakpoint-log-mouse)
    (define-key dape-breakpoint-mode-map [left-fringe mouse-1] nil)
    (define-key dape-breakpoint-mode-map [left-fringe S-mouse-1]
                #'my/debug-breakpoint-toggle-mouse)
    (define-key dape-breakpoint-mode-map [left-fringe S-mouse-2]
                #'my/debug-breakpoint-expression-mouse)
    (define-key dape-breakpoint-mode-map [left-fringe S-mouse-3]
                #'my/debug-breakpoint-log-mouse)))

(defun my/debug--live-connection ()
  "Return the current or last Dape connection."
  (or (ignore-errors (dape--live-connection 'stopped t))
      (ignore-errors (dape--live-connection 'running t))
      (ignore-errors (dape--live-connection 'last t))
      (ignore-errors (dape--live-connection 'parent t))))

(defun my/debug-evaluate-region ()
  "Evaluate the active region in the current debug session."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (dape-evaluate-expression
   (or (my/debug--live-connection)
       (user-error "No active Dape session"))
   (buffer-substring-no-properties (region-beginning) (region-end))))

(defun my/debug-evaluate-symbol ()
  "Evaluate the symbol at point in the current debug session."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (dape-evaluate-expression
	     (or (my/debug--live-connection)
	         (user-error "No active Dape session"))
	     symbol)))

(defun my/debug--display-info-buffer (mode &optional index)
  "Display Dape info buffer for MODE and optional INDEX."
  (require 'dape)
  (unless (fboundp 'dape--info-get-buffer-create)
    (user-error "This Dape version does not expose info buffers"))
  (dape-info)
  (dape-info-update)
  (let ((buffer (dape--info-get-buffer-create mode index)))
    (if (fboundp 'dape--display-buffer)
        (select-window (dape--display-buffer buffer))
      (pop-to-buffer buffer))))

(defun my/debug-info-threads ()
  "Show Dape threads info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-threads-mode))

(defun my/debug-info-stack ()
  "Show Dape stack info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-stack-mode))

(defun my/debug-info-scope ()
  "Show Dape scope/locals info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-scope-mode))

(defun my/debug-info-breakpoints ()
  "Show Dape breakpoints info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-breakpoints-mode))

(defun my/debug-info-watch ()
  "Show Dape watch info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-watch-mode))

(defun my/debug-info-modules ()
  "Show Dape modules info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-modules-mode))

(defun my/debug-info-sources ()
  "Show Dape sources info buffer."
  (interactive)
  (my/debug--display-info-buffer 'dape-info-sources-mode))

(defun my/debug-open-adapter-dir ()
  "Open `dape-adapter-dir' in Dired."
  (interactive)
  (require 'dape)
  (make-directory dape-adapter-dir t)
  (dired dape-adapter-dir))

(defun my/debug-open-breakpoints-file ()
  "Open the persistent Dape breakpoints file."
  (interactive)
  (require 'dape)
  (find-file dape-default-breakpoints-file))

(defun my/debug--command-status (commands)
  "Return a compact status string for COMMANDS."
  (let ((available (seq-filter #'executable-find commands)))
    (if available
        (format "found %s" (string-join available ", "))
      (format "missing %s" (string-join commands ", ")))))

(defun my/debug--config-status (config-name)
  "Return a human-readable readiness status for CONFIG-NAME."
  (if-let* ((config (copy-tree (alist-get config-name dape-configs nil nil #'eq))))
      (condition-case err
          (progn
            (dape--config-ensure config t)
            "ready")
        (error (format "needs setup: %s" (error-message-string err))))
    "not registered"))

(defun my/debug-adapter-doctor ()
  "Show a Dape adapter readiness report for common languages."
  (interactive)
  (require 'dape)
  (let ((buffer (get-buffer-create "*Debug Adapter Doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Debug Adapter Doctor\n\n")
        (insert (format "Adapter dir: %s\n" dape-adapter-dir))
        (insert (format "Breakpoints: %s\n" dape-default-breakpoints-file))
        (insert (format "Active sessions: %s\n\n" (length dape--connections)))
        (dolist (spec my/debug-common-adapter-specs)
          (let* ((plist (cdr spec))
                 (title (plist-get plist :title))
                 (configs (plist-get plist :configs))
                 (commands (plist-get plist :commands))
                 (install (plist-get plist :install)))
            (insert (format "%s\n" title))
            (insert (format "  commands: %s\n"
                            (my/debug--command-status commands)))
            (insert "  configs:\n")
            (dolist (config configs)
              (insert (format "    %-22s %s\n"
                              config
                              (my/debug--config-status config))))
            (insert (format "  setup: %s\n\n" install))))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(transient-define-prefix my/debug-dispatch ()
  "Debug workflow."
  [["Session"
    ("d" "start / choose config" dape)
    ("p" "profile menu" my/debug-profile-dispatch)
    ("r" "rerun profile" my/debug-profile-rerun)
    ("R" "restart session" dape-restart)
    ("q" "quit" dape-quit)
    ("D" "disconnect" dape-disconnect-quit)
    ("K" "kill adapter" dape-kill)]
   ["Step"
    ("c" "continue" dape-continue)
    ("P" "pause" dape-pause)
    ("n" "step over" dape-next)
    ("i" "step in" dape-step-in)
    ("o" "step out" dape-step-out)
    ("u" "run to cursor" dape-until)
    ("F" "restart frame" dape-restart-frame)]
   ["Breakpoints"
    ("b" "toggle" dape-breakpoint-toggle)
    ("x" "remove here" dape-breakpoint-remove-at-point)
    ("X" "remove all" dape-breakpoint-remove-all)
    ("e" "condition" dape-breakpoint-expression)
    ("l" "logpoint" dape-breakpoint-log)
    ("h" "hit count" dape-breakpoint-hits)
    ("f" "function" dape-breakpoint-function)]
   ["Inspect"
    ("s" "sessions" dape-select-session)
    ("t" "threads" my/debug-info-threads)
    ("S" "stack" my/debug-info-stack)
    ("L" "locals" my/debug-info-scope)
    ("w" "watch" dape-watch-dwim)
    ("B" "breakpoints" my/debug-info-breakpoints)
    ("m" "modules" my/debug-info-modules)
    ("C" "sources" my/debug-info-sources)
    ("M" "memory" dape-memory)
    ("A" "disassemble" dape-disassemble)]
   ["Eval / Tools"
    ("E" "eval expression" dape-evaluate-expression)
    ("v" "eval region" my/debug-evaluate-region)
    ("V" "eval symbol" my/debug-evaluate-symbol)
    ("W" "watch buffer" my/debug-info-watch)
    ("I" "info buffers" dape-info)
    ("z" "REPL" dape-repl)
    ("?" "adapter doctor" my/debug-adapter-doctor)
    ("a" "adapter dir" my/debug-open-adapter-dir)
    ("g" "breakpoints file" my/debug-open-breakpoints-file)]])

(use-package dape
  :ensure t
  :demand t
  :commands (dape
             dape-breakpoint-toggle
             dape-continue
             dape-next
             dape-step-in
             dape-step-out
             dape-pause
             dape-restart
             dape-quit)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :custom
  (dape-default-breakpoints-file
   (expand-file-name "breakpoints.eld" my/dape-state-dir))
  (dape-buffer-window-arrangement 'right)
  :config
  (repeat-mode 1)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t)))
  (my/debug-register-common-configs)
  (run-hooks 'my/debug-after-register-common-configs-hook)
  (my/debug-configure-breakpoint-gutter)
  (dape-breakpoint-global-mode 1)
  (when (require 'hydra nil t)
    (with-suppressed-warnings ((docstrings) (callargs))
      (defhydra hydra-dape-mode
        (:color pink :hint nil :foreign-keys run)
        "
^Stepping^          ^Switch/View^             ^Breakpoints^         ^Debug^                     ^Eval / Watch^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_n_: Next           _ss_: Sessions(REPL)      _bb_: Toggle          _dd_: Debug menu            _ee_: Eval
_i_: Step in        _st_: Threads             _bd_: Delete here     _dr_: Restart               _er_: Eval region
_o_: Step out       _sf_: Stack               _ba_: Log message     _dq_: Quit                  _es_: Eval symbol
_c_: Continue       _sl_: Locals(scope)       _bc_: Condition       _dR_: REPL                  _ea_: Add watch
_p_: Pause          _sb_: Breakpoints         _bh_: Hit count
"
        ("n" dape-next)
        ("i" dape-step-in)
        ("o" dape-step-out)
        ("c" dape-continue)
        ("p" dape-pause)
        ("ss" dape-repl)
        ("st" my/debug-info-threads)
        ("sf" my/debug-info-stack)
        ("sl" my/debug-info-scope)
        ("sb" my/debug-info-breakpoints)
        ("bb" dape-breakpoint-toggle)
        ("ba" dape-breakpoint-log)
        ("bd" dape-breakpoint-remove-at-point)
        ("bc" dape-breakpoint-expression)
        ("bh" dape-breakpoint-hits)
        ("dd" my/debug-dispatch)
        ("dr" dape-restart)
        ("dR" dape-repl)
        ("dq" dape-quit :color blue)
        ("ee" dape-evaluate-expression)
        ("ea" dape-watch-dwim)
        ("er" my/debug-evaluate-region)
        ("es" my/debug-evaluate-symbol)
        ("q" nil "quit" :color blue)))))

(use-package dape-toolbar
  :load-path "~/.emacs.d/site-lisp/dape-toolbar"
  :after dape
  :config
  (dape-toolbar-mode 1))

(my/leader!
  "c j" '(:def my/debug-dispatch :which-key "debug")
  "c J" '(:def dape :which-key "debug start"))

(provide 'init-debug)
;;; init-debug.el ends here

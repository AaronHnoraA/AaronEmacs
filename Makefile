EMACS ?= emacs
AARONNOTE_DIR = site-lisp/noema
EMACS_BATCH_BASE = $(EMACS) --batch --no-site-file --no-site-lisp --no-splash --init-directory=$(CURDIR) -q
PUBLISH_BATCH = $(EMACS_BATCH_BASE) -L site-lisp/config -L lisp -L lisp/roam -l ./lisp/roam/init-aaronnote-publish.el
# Load early-init first so native-comp never writes into top-level eln-cache.
BATCH = $(EMACS_BATCH_BASE) -l ./early-init.el -l ./init.el
BOOTSTRAP = $(EMACS_BATCH_BASE) -l ./early-init.el -l ./bootstrap.el
BOOTSTRAP_INSTALL = BOOTSTRAP_MODE=install $(BOOTSTRAP)
BOOTSTRAP_EXPORT = BOOTSTRAP_MODE=export $(BOOTSTRAP)
BOOTSTRAP_AUDIT = BOOTSTRAP_MODE=audit $(BOOTSTRAP)
REMOTE_TEST_BATCH = $(EMACS) --batch -Q --eval '(setq user-emacs-directory (file-name-as-directory "$(CURDIR)") load-prefer-newer t)' -L lisp -L lisp/remote -L lisp/remote/backend
UI_TOKEN_FILE = site-lisp/noema/src/styles/aaron-ui-tokens.css
UI_TOKEN_BATCH = $(EMACS) --batch -Q -L site-lisp/aaron-ui -l site-lisp/aaron-ui/aaron-ui.el

.PHONY: default help up setup setup-full bootstrap-health install remote-ikernel-install lock audit-lock doctor build build-force \
        aaronnote-build \
        compile compile-byte compile-byte-force compile-native compile-native-force \
        clean clean-build clean-elc clean-eln clean-state state-backup state-restore \
        health health-startup health-byte health-native ui-test ui-tokens audit-ui-tokens \
        remote-test remote-source-test remote-contract-test remote-conformance-test remote-byte-check remote-check remote-e2e remote-route-benchmark remote-local-visit-benchmark remote-ssh-visit-benchmark remote-ssh-write-benchmark remote-directory-benchmark \
		lsp-test writing-test latex-preview-test lsp-live-smoke lsp-remote-live-smoke lsp-remote-tty-smoke lsp-remote-gui-smoke lsp-gui-company-popup lsp-key-to-screen lsp-existing-file-live-probe remote-task-live-smoke remote-task-disconnect-smoke remote-terminal-live-smoke remote-vterm-live-smoke \
        jupyter-test research-test agenda-test agenda-apple-test \
        publish publish-build publish-deploy publish-clean

default: up

help:
	@printf '%s\n' \
	  'Targets:' \
	  '  make up                   One-click bootstrap; optionally restore SNAPSHOT first' \
	  '  make setup                One-shot restore + startup health check' \
	  '  make setup-full           Restore + full health suite + doctor report' \
	  '  make bootstrap-health     Restore + health + doctor + lock audit' \
	  '  make install              Deterministically restore packages from package-lock.el' \
	  '  make remote-ikernel-install  Install the vendored remote_ikernel into Anaconda' \
	  '  make lock                 Export the current package set back into package-lock.el' \
	  '  make audit-lock           Compare installed packages against package-lock.el' \
	  '  make ui-tokens            Regenerate Noema CSS tokens from aaron-ui' \
	  '  make audit-ui-tokens      Verify committed Noema CSS tokens are current' \
	  '  make ui-test              Run Aaron UI semantic-token and dashboard ERT tests' \
	  '  make doctor               Open/check the config health doctor report in batch' \
	  '  make state-backup         Snapshot migration-worthy local state into var/backup-snapshots' \
	  '  make state-restore SNAPSHOT=/path/to/archive.tar.gz  Restore a saved state snapshot' \
	  '  make build                Full Elisp compile plus Noema static build' \
	  '  make build-force          Same as build, but reset ELN cache first' \
	  '  make aaronnote-build      Build Noema static assets' \
	  '  make compile              btye and native compile'\
	  '  make compile-force        Force btye and native compile'\
	  '  make compile-byte         SByte-compile the local Emacs config' \
	  '  make compile-byte-force   Force byte-compilation for managed files' \
	  '  make compile-native       Queue native compilation for the local config' \
	  '  make compile-native-force Force native compilation after cleaning managed .eln' \
	  '  make clean-build          Remove managed .elc and config-owned .eln' \
	  '  make clean-elc            Remove managed .elc files' \
	  '  make clean-eln            Remove config-owned .eln files and reset ELN cache' \
	  '  make clean-state          Remove ./var runtime state' \
	  '  make health               Run startup + byte + native smoke checks' \
	  '  make health-startup       Run startup smoke check' \
	  '  make health-byte          Run byte-compile smoke check' \
	  '  make health-native        Run native-compile smoke check' \
	  '  make remote-test          Run isolated remote framework ERT suites' \
	  '  make remote-source-test   Run source helper and workspace lifecycle checks' \
	  '  make remote-contract-test Run remote upgrade/provider contract tests' \
	  '  make remote-conformance-test Compare /fs:local semantics with native APIs' \
	  '  make remote-byte-check    Strictly byte-compile remote code in a temp dir' \
	  '  make remote-check         Run all remote tests and compatibility checks' \
	  '  make lsp-test            Run isolated LSP routing, toolchain, runtime, and UI tests' \
	  '  make writing-test        Run LanguageTool/Flymake and LaTeX routing tests' \
	  '  make latex-preview-test   Run the vendored RaTeX math-preview ERT suite' \
	  '  make lsp-live-smoke      Start real clangd, Python LS, and JDTLS projects' \
	  '  make lsp-remote-live-smoke  Start real C/Python/Java LSP through TRAMP + Remote' \
	  '  make lsp-remote-tty-smoke  Measure real terminal redisplay during remote LSP editing' \
	  '  make lsp-remote-gui-smoke  Measure GUI LSP editing; set REMOTE_LSP_E2E_PAIRED=1 for native/SSH pairs' \
	  '  make lsp-gui-company-popup  Compare native/SSH GUI Company popups (set REMOTE_GUI_COMPANY_FILE)' \
	  '  make lsp-key-to-screen   Compare native, /fs:local, and SSH key-to-PTY output (set REMOTE_KEY_SCREEN_FILE)' \
	  '  make lsp-existing-file-live-probe  Check Python Company completion on an existing source' \
	  '  make remote-task-live-smoke  Check target tasks, error links, and cancellation' \
	  '  make remote-task-disconnect-smoke  Check task status after RPC transport loss' \
	  '  make remote-terminal-live-smoke  Check routed PTY input/output on a real SSH target' \
	  '  make remote-vterm-live-smoke  Check the actual VTerm frontend on a real SSH target' \
	  '  make jupyter-test         Run Noema/Jupyter and notebook ERT suites' \
	  '  make research-test        Run Noema research notebook (JuText/Graph Board) ERT suite' \
	  '  make agenda-apple-test    Build and check EventKit without requesting access' \
	  '  make remote-e2e           Run opt-in real SSH E2E (REMOTE_E2E_TARGET optional)' \
	  '  make remote-route-benchmark Compare warm physical and /fs file queries (set REMOTE_BENCHMARK_TARGET)' \
	  '  make remote-local-visit-benchmark Compare warm native and /fs:local: source visits' \
	  '  make remote-ssh-visit-benchmark Compare warm SSH TRAMP and /fs source visits (set REMOTE_BENCHMARK_TARGET)' \
	  '  make remote-ssh-write-benchmark Measure repeated SSH saves and attribute probes (set REMOTE_BENCHMARK_TARGET)' \
	  '  make remote-directory-benchmark Compare Dired open and refresh on local and SSH directories' \
	  '' \
	  '  make publish              Compile CV + deploy site (git push + optional NAS rsync)' \
	  '  make publish-build        Compile CV and verify the site is complete' \
	  '  make publish-deploy       Deploy only (git push, optional NAS rsync)' \
	  '  make publish-clean        Remove CV build intermediates'

up:
	@if [ -n "$(SNAPSHOT)" ]; then \
	  $(MAKE) state-restore SNAPSHOT="$(SNAPSHOT)"; \
	fi
	$(MAKE) bootstrap-health

setup: install health-startup

setup-full: install health doctor

bootstrap-health: install health doctor audit-lock

install:
	$(BOOTSTRAP_INSTALL)

remote-ikernel-install:
	bin/install-remote-ikernel install

lock:
	$(BOOTSTRAP_EXPORT)

audit-lock:
	$(BOOTSTRAP_AUDIT)

ui-tokens:
	$(UI_TOKEN_BATCH) --eval '(aaron-ui-export-css-tokens "$(CURDIR)/$(UI_TOKEN_FILE)" '"'"'wave)'

audit-ui-tokens:
	$(UI_TOKEN_BATCH) --eval '(let ((expected (aaron-ui-css-tokens '"'"'wave)) (file "$(CURDIR)/$(UI_TOKEN_FILE)")) (unless (and (file-readable-p file) (with-temp-buffer (insert-file-contents file) (equal (buffer-string) expected))) (error "Aaron UI CSS tokens are stale; run make ui-tokens")))'

ui-test:
	$(UI_TOKEN_BATCH) --eval '(setq user-emacs-directory (file-name-as-directory "$(CURDIR)"))' -l test/aaron-ui-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/noema-icon-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-ui-dashboard-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-auto-insert-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-snippets-tests.el -f ert-run-tests-batch-and-exit

doctor:
	$(BATCH) --eval '(prin1 (my/health-critical-check))'

state-backup:
	$(BATCH) --eval '(princ (my/maintenance-state-snapshot))'

state-restore:
	@test -n "$(SNAPSHOT)" || (echo "SNAPSHOT=/path/to/archive.tar.gz is required" >&2; exit 2)
	$(BATCH) --eval "(princ (my/maintenance-state-restore \"$(SNAPSHOT)\"))"

build:
	$(BATCH) --eval '(my/build-all)'
	$(MAKE) aaronnote-build

build-force:
	$(BATCH) --eval '(my/build-all t)'
	$(MAKE) aaronnote-build

aaronnote-build:
	npm --prefix $(AARONNOTE_DIR) run build:aaronnote

compile: compile-byte  compare-native

compile-force: compile-byte-force  compare-native-force

compile-byte:
	$(BATCH) --eval '(my/byte-compile-config)'

compile-byte-force:
	$(BATCH) --eval '(my/byte-compile-config t)'

compile-native:
	$(BATCH) --eval '(my/native-compile-config)'

compile-native-force:
	$(BATCH) --eval '(my/native-compile-config t)'

clean: clean-state

clean-build:
	$(BATCH) --eval '(my/compile-clean-all-artifacts)'

clean-elc:
	$(BATCH) --eval '(my/compile-clean-byte-artifacts)'

clean-eln:
	$(BATCH) --eval '(my/compile-clean-native-artifacts)'
	$(BATCH) --eval '(my/native-comp-reset-cache)'

clean-state:
	rm -rf ./var

health: health-startup health-byte health-native ui-test audit-ui-tokens

health-startup:
	$(BATCH) --eval '(prin1 (my/health-startup-check))'

health-byte:
	$(BATCH) --eval '(prin1 (my/health-byte-compile-check))'

health-native:
	$(BATCH) --eval '(prin1 (my/health-native-compile-check))'

remote-contract-test:
	$(REMOTE_TEST_BATCH) -l test/remote-compat-tests.el -f ert-run-tests-batch-and-exit

remote-conformance-test:
	$(REMOTE_TEST_BATCH) -l test/remote-conformance-tests.el -f ert-run-tests-batch-and-exit

remote-byte-check:
	$(REMOTE_TEST_BATCH) -l test/remote-strict-compile.el --eval '(remote-strict-byte-compile)'

remote-check: remote-byte-check remote-test

remote-source-test:
	node --test test/remote-source-agent.test.cjs
	$(REMOTE_TEST_BATCH) -l test/remote-source-tests.el -f ert-run-tests-batch-and-exit

remote-test: remote-contract-test remote-conformance-test remote-source-test
	$(REMOTE_TEST_BATCH) -l test/remote-tests.el -f ert-run-tests-batch-and-exit
	$(REMOTE_TEST_BATCH) -l test/remote-framework-tests.el -f ert-run-tests-batch-and-exit
	$(REMOTE_TEST_BATCH) -l test/remote-task-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/remote-gateway-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-remote-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-toolchain-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-ui-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-copilot-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-project-remote-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-evil-tests.el -f ert-run-tests-batch-and-exit

lsp-test:
	$(BATCH) -l test/init-lsp-remote-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-toolchain-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-runtime-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-ui-tests.el -f ert-run-tests-batch-and-exit

writing-test:
	$(BATCH) -l test/init-writing-tests.el -f ert-run-tests-batch-and-exit

# Math-fragment preview. Runs isolated (-Q): ratex.el is vendored and must keep
# working without the rest of the configuration loaded.
latex-preview-test:
	$(EMACS) --batch -Q -L site-lisp/ratex.el/lisp \
	  -l site-lisp/ratex.el/test/ratex-tests.el -f ert-run-tests-batch-and-exit

lsp-live-smoke:
	$(BATCH) -l test/lsp-live-smoke.el -f my/lsp-live-smoke-batch

lsp-remote-live-smoke:
	REMOTE_LSP_E2E=1 $(BATCH) -l test/lsp-remote-live-smoke.el -f my/lsp-remote-live-smoke-batch

lsp-remote-tty-smoke:
	@result_file="$${REMOTE_LSP_E2E_RESULT_FILE:-}"; cleanup=; \
	  if test -z "$$result_file"; then \
	    result_file=$$(mktemp /tmp/emacs-lsp-tty.XXXXXX) || exit 1; cleanup=1; \
	  fi; \
	  REMOTE_LSP_E2E=1 REMOTE_LSP_E2E_REDISPLAY=1 \
	  REMOTE_LSP_E2E_RESULT_FILE="$$result_file" \
	  $(EMACS) -nw --no-site-file --no-site-lisp --no-splash --init-directory=$(CURDIR) -q -l ./early-init.el -l ./init.el -l test/lsp-remote-live-smoke.el --eval '(run-at-time 0 nil (quote my/lsp-remote-live-smoke-batch))'; \
	  result_status=$$?; \
	  if test -s "$$result_file"; then cat "$$result_file"; fi; \
	  if test -n "$$cleanup"; then rm -f "$$result_file"; fi; \
	  exit "$$result_status"

lsp-remote-gui-smoke:
	@result_file="$${REMOTE_LSP_E2E_RESULT_FILE:-}"; cleanup=; \
	  if test -z "$$result_file"; then \
	    result_file=$$(mktemp /tmp/emacs-lsp-gui.XXXXXX) || exit 1; cleanup=1; \
	  fi; \
	  REMOTE_LSP_E2E=1 REMOTE_LSP_E2E_REDISPLAY=1 \
	  REMOTE_LSP_E2E_FRAME_COLUMNS="$${REMOTE_LSP_E2E_FRAME_COLUMNS:-120}" \
	  REMOTE_LSP_E2E_FRAME_ROWS="$${REMOTE_LSP_E2E_FRAME_ROWS:-50}" \
	  REMOTE_LSP_E2E_RESULT_FILE="$$result_file" \
	  $(EMACS) -Q --eval '(setq user-emacs-directory (file-name-as-directory "$(CURDIR)"))' \
	    --eval '(condition-case error-data (progn (load-file "$(CURDIR)/early-init.el") (load-file "$(CURDIR)/init.el") (load-file "$(CURDIR)/test/lsp-remote-live-smoke.el") (when (getenv "REMOTE_LSP_E2E_UI_VARIANT") (load-file "$(CURDIR)/test/remote-gui-ui-variant.el")) (when (getenv "REMOTE_TYPING_PROFILE_OUTPUT") (load-file "$(CURDIR)/test/remote-typing-profile.el")) (when (getenv "REMOTE_GUI_CPU_PROFILE_OUTPUT") (load-file "$(CURDIR)/test/remote-gui-cpu-profile.el")) (if (equal (getenv "REMOTE_LSP_E2E_PAIRED") "1") (progn (load-file "$(CURDIR)/test/lsp-gui-paired-benchmark.el") (my/lsp-gui-paired-benchmark-run)) (my/lsp-remote-live-smoke-batch))) (error (with-temp-file (getenv "REMOTE_LSP_E2E_RESULT_FILE") (prin1 error-data (current-buffer))) (kill-emacs 1)))'; \
	  result_status=$$?; \
	  if test -s "$$result_file"; then cat "$$result_file"; fi; \
	  if test -n "$$cleanup"; then rm -f "$$result_file"; fi; \
	  exit "$$result_status"

lsp-existing-file-live-probe:
	$(BATCH) -l test/lsp-existing-file-live-probe.el

lsp-key-to-screen:
	python3 test/remote-key-to-screen.py

lsp-gui-company-popup:
	python3 test/remote-gui-company-popup.py

remote-debug-live-smoke:
	$(BATCH) -l test/remote-debug-live-smoke.el

remote-task-live-smoke:
	$(BATCH) -l test/remote-task-live-smoke.el

remote-task-disconnect-smoke:
	$(BATCH) -l test/remote-task-disconnect-smoke.el

remote-terminal-live-smoke:
	$(BATCH) -l test/remote-terminal-live-smoke.el

remote-vterm-live-smoke:
	$(BATCH) -l test/remote-vterm-live-smoke.el

jupyter-test:
	$(BATCH) -l test/init-aaronnote-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-aaronnote-jupyter-notebook-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-lsp-runtime-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-jupyter-board-tests.el -f ert-run-tests-batch-and-exit
	$(REMOTE_TEST_BATCH) -l test/remote-jupyter-tests.el -f ert-run-tests-batch-and-exit

.PHONY: jupyter-live-smoke
jupyter-live-smoke:
	$(BATCH) -l test/jupyter-remote-live-smoke.el

.PHONY: jupyter-contents-live-smoke
jupyter-contents-live-smoke:
	$(BATCH) -l test/jupyter-contents-live-smoke.el

.PHONY: jupyter-debug-live-smoke
jupyter-debug-live-smoke:
	$(BATCH) -l test/jupyter-debug-live-smoke.el

agenda-test:
	$(EMACS) --batch -Q -L site-lisp/noema/lisp -l noema-agenda-tests -l noema-agenda-attention-tests -l noema-agenda-capture-tests -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/init-aaronnote-agenda-source-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/md-roam-tests.el -f ert-run-tests-batch-and-exit

agenda-apple-test:
	$(MAKE) -C site-lisp/noema agenda-apple-test
	$(BATCH) -l test/init-aaronnote-agenda-apple-tests.el -f ert-run-tests-batch-and-exit

research-test:
	$(BATCH) -l test/noema-startup-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/popup-agent-tests.el -L site-lisp/noema/test/elisp -l noema-agent-acp-tests.el -l noema-agent-render-tests.el -l noema-context-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -L site-lisp/noema/test/elisp -l noema-capability-workspace-tests.el -l test/noema-manager-layout-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -L lisp/roam -l test/noema-research-tests.el -f ert-run-tests-batch-and-exit
	$(BATCH) -L site-lisp/noema/lisp -L site-lisp/noema/test/elisp \
	  -l noema-interaction-tests.el -l noema-interaction-magent-tests.el -l noema-api-tests.el -l noema-completion-tests.el \
	  -l noema-project-overview-tests.el -l noema-research-workflow-tests.el -l noema-history-search-tests.el -l noema-findings-tests.el \
	  -f ert-run-tests-batch-and-exit

remote-e2e:
	REMOTE_E2E=1 $(REMOTE_TEST_BATCH) -l test/remote-e2e-tests.el -f ert-run-tests-batch-and-exit
	# The isolated suite can lack package-vc's tramp-rpc load path.  Run its
	# backend-specific checks with the real init and selected RPC route too.
	REMOTE_E2E=1 $(BATCH) -l test/remote-e2e-tests.el \
	  --eval '(ert-run-tests-batch-and-exit "remote-e2e-\\(attribute-cache-preserves-acl-on-repeated-saves\\|rpc-path-batch-preserves-directory-filter\\|executable-lookup-honors-workspace-path\\|project-search-and-magit-worktree-identity\\|missing-java-runtime-does-not-start-jdtls\\|target-home-and-symlinks-keep-logical-identity\\)")'

remote-route-benchmark:
	@test -n "$(REMOTE_BENCHMARK_TARGET)" || { echo 'Set REMOTE_BENCHMARK_TARGET'; exit 2; }
	$(BATCH) -l test/remote-route-benchmark.el

remote-local-visit-benchmark:
	$(BATCH) -l test/remote-local-visit-benchmark.el

remote-ssh-visit-benchmark:
	@test -n "$(REMOTE_BENCHMARK_TARGET)" || { echo 'Set REMOTE_BENCHMARK_TARGET'; exit 2; }
	$(BATCH) -l test/remote-ssh-visit-benchmark.el

remote-ssh-write-benchmark:
	@test -n "$(REMOTE_BENCHMARK_TARGET)" || { echo 'Set REMOTE_BENCHMARK_TARGET'; exit 2; }
	$(BATCH) -l test/remote-ssh-write-benchmark.el

remote-directory-benchmark:
	@test -n "$(REMOTE_BENCHMARK_TARGET)" || { echo 'Set REMOTE_BENCHMARK_TARGET'; exit 2; }
	$(BATCH) -l test/remote-directory-benchmark.el

# ── Publish ────────────────────────────────────────────────────────────────
publish:
	$(PUBLISH_BATCH) --eval '(my/noema-publish-batch)'

publish-build:
	$(PUBLISH_BATCH) --eval '(my/noema-publish-build-batch)'

publish-deploy:
	$(PUBLISH_BATCH) --eval '(my/noema-publish-deploy-batch)'

publish-clean:
	$(PUBLISH_BATCH) --eval '(my/noema-publish-clean-batch)'

EMACS ?= emacs
AARONNOTE_DIR = lisp/roam/aaronnote
EMACS_BATCH_BASE = $(EMACS) --batch --no-site-file --no-site-lisp --no-splash --init-directory=$(CURDIR) -q
PUBLISH_BATCH = $(EMACS_BATCH_BASE) -L site-lisp/config -L lisp -L lisp/roam -l ./lisp/roam/init-aaronnote-publish.el
# Load early-init first so native-comp never writes into top-level eln-cache.
BATCH = $(EMACS_BATCH_BASE) -l ./early-init.el -l ./init.el
BOOTSTRAP = $(EMACS_BATCH_BASE) -l ./early-init.el -l ./bootstrap.el
BOOTSTRAP_INSTALL = BOOTSTRAP_MODE=install $(BOOTSTRAP)
BOOTSTRAP_EXPORT = BOOTSTRAP_MODE=export $(BOOTSTRAP)
BOOTSTRAP_AUDIT = BOOTSTRAP_MODE=audit $(BOOTSTRAP)
TEXPRESSO_DIR ?= $(CURDIR)/var/texpresso
TEXPRESSO_REPOSITORY ?= https://github.com/let-def/texpresso.git

.PHONY: default help up setup setup-full bootstrap-health install remote-ikernel-install lock audit-lock doctor build build-force \
        aaronnote-build texpresso-install texpresso-build texpresso-test \
        compile compile-byte compile-byte-force compile-native compile-native-force \
        clean clean-build clean-elc clean-eln clean-state state-backup state-restore \
        health health-startup health-byte health-native \
        publish publish-force publish-build publish-deploy publish-clean

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
	  '  make texpresso-install    Install/update and build TeXpresso under var/texpresso' \
	  '  make texpresso-build      Rebuild the existing local TeXpresso checkout' \
	  '  make texpresso-test       Run TeXpresso headlessly against its sample document' \
	  '  make lock                 Export the current package set back into package-lock.el' \
	  '  make audit-lock           Compare installed packages against package-lock.el' \
	  '  make doctor               Open/check the config health doctor report in batch' \
	  '  make state-backup         Snapshot migration-worthy local state into var/backup-snapshots' \
	  '  make state-restore SNAPSHOT=/path/to/archive.tar.gz  Restore a saved state snapshot' \
	  '  make build                Full Elisp compile plus Aaronnote static build' \
	  '  make build-force          Same as build, but reset ELN cache first' \
	  '  make aaronnote-build      Build Aaronnote static assets' \
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
	  '' \
	  '  make publish              Build site + deploy (git push + optional NAS rsync)' \
	  '  make publish-force        Force full rebuild + deploy (skip incremental state check)' \
	  '  make publish-build        Build static site only (render notes, compile CV)' \
	  '  make publish-deploy       Deploy only (git push, optional NAS rsync)' \
	  '  make publish-clean        Remove publish state/cache/CV intermediates'

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

texpresso-install:
	@command -v brew >/dev/null || (echo "Homebrew is required" >&2; exit 2)
	@brew list --versions mupdf >/dev/null 2>&1 || brew install mupdf
	@brew list --versions sdl2 >/dev/null 2>&1 || brew install sdl2
	@if [ -d "$(TEXPRESSO_DIR)/.git" ]; then \
	  git -C "$(TEXPRESSO_DIR)" pull --ff-only; \
	else \
	  git clone --recurse-submodules "$(TEXPRESSO_REPOSITORY)" "$(TEXPRESSO_DIR)"; \
	fi
	@git -C "$(TEXPRESSO_DIR)" submodule update --init --recursive
	$(MAKE) -C "$(TEXPRESSO_DIR)" all

texpresso-build:
	@test -d "$(TEXPRESSO_DIR)/.git" || (echo "Run make texpresso-install first" >&2; exit 2)
	$(MAKE) -C "$(TEXPRESSO_DIR)" all

texpresso-test:
	@test -x "$(TEXPRESSO_DIR)/build/texpresso" || (echo "Run make texpresso-install first" >&2; exit 2)
	$(MAKE) -C "$(TEXPRESSO_DIR)" test-texpresso-texlive

lock:
	$(BOOTSTRAP_EXPORT)

audit-lock:
	$(BOOTSTRAP_AUDIT)

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

health: health-startup health-byte health-native

health-startup:
	$(BATCH) --eval '(prin1 (my/health-startup-check))'

health-byte:
	$(BATCH) --eval '(prin1 (my/health-byte-compile-check))'

health-native:
	$(BATCH) --eval '(prin1 (my/health-native-compile-check))'

# ── Publish ────────────────────────────────────────────────────────────────
publish:
	$(PUBLISH_BATCH) --eval '(my/aaronnote-publish-batch)'

publish-force:
	$(PUBLISH_BATCH) --eval '(my/aaronnote-publish-force-batch)'

publish-build:
	$(PUBLISH_BATCH) --eval '(my/aaronnote-publish-build-batch)'

publish-deploy:
	$(PUBLISH_BATCH) --eval '(my/aaronnote-publish-deploy-batch)'

publish-clean:
	$(PUBLISH_BATCH) --eval '(my/aaronnote-publish-clean-batch)'

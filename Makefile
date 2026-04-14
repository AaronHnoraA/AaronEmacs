EMACS ?= emacs
EMACS_BATCH_BASE = $(EMACS) --batch --no-site-file --no-site-lisp --no-splash --init-directory=$(CURDIR) -q
# Load early-init first so native-comp never writes into top-level eln-cache.
BATCH = $(EMACS_BATCH_BASE) -l ./early-init.el -l ./init.el
BOOTSTRAP = $(EMACS_BATCH_BASE) -l ./early-init.el -l ./bootstrap.el
BOOTSTRAP_INSTALL = BOOTSTRAP_MODE=install $(BOOTSTRAP)
BOOTSTRAP_EXPORT = BOOTSTRAP_MODE=export $(BOOTSTRAP)
BOOTSTRAP_AUDIT = BOOTSTRAP_MODE=audit $(BOOTSTRAP)

.PHONY: default help up setup setup-full bootstrap-health install lock audit-lock doctor build build-force \
        compile compile-byte compile-byte-force compile-native compile-native-force \
        clean clean-build clean-elc clean-eln clean-state state-backup state-restore \
        health health-startup health-byte health-native

default: up

help:
	@printf '%s\n' \
	  'Targets:' \
	  '  make up                   One-click bootstrap; optionally restore SNAPSHOT first' \
	  '  make setup                One-shot restore + startup health check' \
	  '  make setup-full           Restore + full health suite + doctor report' \
	  '  make bootstrap-health     Restore + health + doctor + lock audit' \
	  '  make install              Deterministically restore packages from package-lock.el' \
	  '  make lock                 Export the current package set back into package-lock.el' \
	  '  make audit-lock           Compare installed packages against package-lock.el' \
	  '  make doctor               Open/check the config health doctor report in batch' \
	  '  make state-backup         Snapshot migration-worthy local state into var/backup-snapshots' \
	  '  make state-restore SNAPSHOT=/path/to/archive.tar.gz  Restore a saved state snapshot' \
	  '  make build                Full byte + native compile for config and third-party Elisp' \
	  '  make build-force          Same as build, but reset ELN cache first' \
	  '  make compile              Byte-compile the local Emacs config' \
	  '  make compile-byte         Same as compile' \
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
	  '  make health-native        Run native-compile smoke check'

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

build-force:
	$(BATCH) --eval '(my/build-all t)'

compile: compile-byte

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

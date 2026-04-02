EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -l ./init.el
#BOOTSTRAP = $(EMACS) --debug-init -q -l ./bootstrap.el
BOOTSTRAP = $(EMACS) -q -l ./bootstrap.el

.PHONY: default help install \
        compile compile-byte compile-byte-force compile-native compile-native-force \
        clean clean-build clean-elc clean-eln clean-state \
        health health-startup health-byte health-native

default: install

help:
	@printf '%s\n' \
	  'Targets:' \
	  '  make install              Bootstrap packages / refresh lock workflow' \
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

install:
	$(BOOTSTRAP)

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

PORT ?= 5179
HOST ?= 127.0.0.1
ROOT ?= ../roam
APP_NAME := AaronNote
APP_DEST := /Applications/$(APP_NAME).app
APP_BUNDLE := $(firstword $(wildcard release/mac*/$(APP_NAME).app release/mac/$(APP_NAME).app))
ICON_SVG := ../Aaronnote.svg
ICONSET := build/Aaronnote.iconset
ICON_ICNS := build/Aaronnote.icns

.PHONY: install build build-web test icon install-app

install:
	npm install

build: icon
	npm run build:desktop
	$(MAKE) install-app

build-web:
	npm run build:aaronnote

icon:
	cp "$(ICON_SVG)" public/Aaronnote.svg
	rm -rf "$(ICONSET)"
	mkdir -p "$(ICONSET)"
	rsvg-convert -w 16 -h 16 "$(ICON_SVG)" -o "$(ICONSET)/icon_16x16.png"
	rsvg-convert -w 32 -h 32 "$(ICON_SVG)" -o "$(ICONSET)/icon_16x16@2x.png"
	rsvg-convert -w 32 -h 32 "$(ICON_SVG)" -o "$(ICONSET)/icon_32x32.png"
	rsvg-convert -w 64 -h 64 "$(ICON_SVG)" -o "$(ICONSET)/icon_32x32@2x.png"
	rsvg-convert -w 128 -h 128 "$(ICON_SVG)" -o "$(ICONSET)/icon_128x128.png"
	rsvg-convert -w 256 -h 256 "$(ICON_SVG)" -o "$(ICONSET)/icon_128x128@2x.png"
	rsvg-convert -w 256 -h 256 "$(ICON_SVG)" -o "$(ICONSET)/icon_256x256.png"
	rsvg-convert -w 512 -h 512 "$(ICON_SVG)" -o "$(ICONSET)/icon_256x256@2x.png"
	rsvg-convert -w 512 -h 512 "$(ICON_SVG)" -o "$(ICONSET)/icon_512x512.png"
	rsvg-convert -w 1024 -h 1024 "$(ICON_SVG)" -o "$(ICONSET)/icon_512x512@2x.png"
	iconutil -c icns "$(ICONSET)" -o "$(ICON_ICNS)"

install-app:
	@test -n "$(APP_BUNDLE)" || (echo "AaronNote.app was not generated under Aaronnote/release" && exit 1)
	rm -rf "$(APP_DEST)"
	cp -R "$(CURDIR)/$(APP_BUNDLE)" "$(APP_DEST)"
	@echo "Copied $(CURDIR)/$(APP_BUNDLE) -> $(APP_DEST)"

test:
	npm test

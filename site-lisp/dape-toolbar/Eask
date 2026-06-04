;; -*- mode: eask; lexical-binding: t -*-

(package "dape-toolbar"
         "0.0.2"
         "Debug toolbar for dape")

(website-url "https://github.com/zsxh/dape-toolbar")
(keywords "convenience" "tools")

(package-file "dape-toolbar.el")

;; Source archives
(source "gnu")
(source "nongnu")
(source "melpa")

;; Emacs version requirement
(depends-on "emacs" "29.2")

;; Runtime dependencies are specified in the package file,
;; but we can also declare them here for clarity
(depends-on "compat" "30.1.0.0")
(depends-on "dape" "0.26.0")
(depends-on "nerd-icons" "0.1.0")

;; Development dependencies
(development
 (depends-on "ert")                     ; Built-in test framework
 (depends-on "package-lint")            ; Package linting
 )

;; Scripts for common tasks
(script "test" "echo \"Error: no test specified\" && exit 1")
;;; init.el ---   -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(add-to-list 'load-path "~/.emacs.d/ewen/")
(load-file "~/.emacs.d/ewen/init.el")
(setq custom-file "~/.emacs.d/ewen/custom.el")
(load custom-file)

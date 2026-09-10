;;; init.el ---   -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/ewen/")
(load "~/.emacs.d/ewen/init")
(setq custom-file "~/.emacs.d/ewen/custom.el")
(load custom-file)
;; Enable the downcase-region command
(put 'downcase-region 'disabled nil)

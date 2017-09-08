;;; init.el ---   -*- lexical-binding: t; -*-

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/ewen/")
(load-file "~/.emacs.d/ewen/init.el")
(setq custom-file "~/.emacs.d/ewen/custom.el")
(load custom-file)

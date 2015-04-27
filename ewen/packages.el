;;; packages.el --- ewen Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar ewen-packages
  '(
    ;; package ewens go here
    clojure-mode image+ evil-lisp-state
                 )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ewen-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function ewen/init-<package-ewen>
;;
;; (defun ewen/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun ewen/init-clojure-mode ()
  (use-package ewen-clojure-mode
    :commands clojure-mode))

(defun ewen/init-image+ ()
  (use-package ewen-image+
    :commands imagex-sticky-mode
    :init (add-hook 'image-mode-hook #'(lambda ()
                                         (progn
                                           (imagex-sticky-mode 1)
                                           (define-key imagex-sticky-mode-map (kbd "L") 'imagex-sticky-zoom-in)
                                           (define-key imagex-sticky-mode-map (kbd "H") 'imagex-sticky-zoom-out)
                                           (define-key image-mode-map (kbd "j") 'image-next-line)
                                           (define-key image-mode-map (kbd "k") 'image-previous-line)
                                           (define-key image-mode-map (kbd "l") 'image-forward-hscroll)
                                           (define-key image-mode-map (kbd "h") 'image-backward-hscroll)
                                           (define-key image-mode-map (kbd "C-d") 'image-scroll-up)
                                           (define-key image-mode-map (kbd "C-u") 'image-scroll-down)
                                           (define-key image-mode-map (kbd "N") 'image-previous-file)
                                           (evil-make-overriding-map imagex-sticky-mode-map 'normal)
                                           (evil-make-overriding-map image-mode-map 'normal)
                                           (evil-normalize-keymaps))))))

(defun ewen/init-evil-lisp-state ()
  (use-package ewen-evil-lisp-state
    :commands evil-lisp-state
    :config (progn
              
              (setq evil-lisp-state-global t)

              (defconst ewen-lisp-state-commands
                `(("l"   . sp-forward-sexp))
                "alist of keys and commands in lisp state.")
              
              (dolist (x ewen-lisp-state-commands)
                (let ((key (car x))
                      (cmd (cdr x)))
                  (eval
                   `(progn
                      (define-key evil-lisp-state-map ,(kbd key) ',cmd)
                      (if evil-lisp-state-global
                          (evil-leader/set-key
                            ,(kbd (concat evil-lisp-state-leader-prefix " " key))
                            (evil-lisp-state-enter-command ,cmd))
                        (dolist (mm evil-lisp-state-major-modes)
                          (evil-leader/set-key-for-mode mm
                            ,(kbd (concat evil-lisp-state-leader-prefix " " key))
                            (evil-lisp-state-enter-command ,cmd)))))))))))

(require 'package)
(require 'helm-mode)

;;NixOS shell prompt is not recognized by default. This pattern fix the issue.
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(add-to-list 'load-path "~/replique.el/")
(require 'replique)

(add-to-list 'load-path "~/replique2.el/")
(require 'replique2)

;;Install dash
(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash))
(require 'dash)
(unless (package-installed-p 'dash-functional)
  (package-refresh-contents)
  (package-install 'dash-functional))
(require 'dash-functional)
(eval-after-load "dash" '(dash-enable-font-lock))

;;nix-mode
(unless (package-installed-p 'nix-mode)
  (package-refresh-contents)
  (package-install 'nix-mode))
(require 'nix-mode)

;; Scroll margin
(setq scroll-margin 6)

;; Make horizontal split the default
(setq split-width-threshold 160)

;; Prettify symbol
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  (--filter (equal "lambda" it) prettify-symbols-alist))
            (push '("-lambda" . ?λ) prettify-symbols-alist)
            (prettify-symbols-mode 1)))

;; Org indent mode
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (define-key smartparens-mode-map (kbd "M-<up>") nil)
            (define-key smartparens-mode-map (kbd "M-<down>") nil)
            (define-key prelude-mode-map (kbd "M-S-<up>") nil)
            (define-key prelude-mode-map (kbd "M-S-<down>") nil)
            (define-key prelude-mode-map (kbd "C-c TAB") nil)
            (define-key prelude-mode-map (kbd "C-c o") nil)
            ;(setq visual-line-mode 1)
            (require 'epresent)
            (require 'ob-sh)
            (require 'ob-replique)
            (setq org-confirm-babel-evaluate nil)
            (push
             '("sr" "#+RESULTS:\n:" "")
             org-structure-template-alist))
          t)

;; C-up and C-down to slowly scroll the buffer
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)] 'gcm-scroll-up)



;; Isearch
(define-key isearch-mode-map (kbd "C-c s") 'isearch-forward-symbol-at-point)


;;Ediff - split horizontally by default
(custom-set-variables '(ediff-split-window-function 'split-window-horizontally))

;;Install smartparens
(unless (package-installed-p 'smartparens)
  (package-refresh-contents)
  (package-install 'smartparens))
(require 'smartparens)
(smartparens-global-mode 1)

;;Install clojure-mode
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
(require 'clojure-mode)

;;Enable replique-minor-mode
(add-hook 'clojure-mode-hook #'replique/minor-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'js2-mode-hook #'replique/generic-minor-mode)

;;Enable replique-minor-mode
(add-hook 'clojure-mode-hook #'replique2/minor-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;;Install s
(unless (package-installed-p 's)
  (package-refresh-contents)
  (package-install 's))

;;Very large files
(unless (package-installed-p 'vlf)
  (package-refresh-contents)
  (package-install 'vlf))
(require 'vlf-setup)

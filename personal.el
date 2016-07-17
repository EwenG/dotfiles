(require 'package)

;;NixOS shell prompt is not recognized by default. This pattern fix the issue.
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(add-to-list 'load-path "~/replique2.el/")
(require 'replique)

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
            ; (setq visual-line-mode 1)
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
;;(add-hook 'clojure-mode-hook #'replique/minor-mode)
;;(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
;;(add-hook 'js2-mode-hook #'replique/generic-minor-mode)

;;Enable replique-minor-mode
(add-hook 'clojure-mode-hook 'replique/minor-mode)
(add-hook 'css-mode-hook 'replique/minor-mode)
(add-hook 'scss-mode-hook 'replique/minor-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(sp-local-pair 'replique/mode "'" nil :actions nil)


;;Install s
(unless (package-installed-p 's)
  (package-refresh-contents)
  (package-install 's))

;;Very large files
(unless (package-installed-p 'vlf)
  (package-refresh-contents)
  (package-install 'vlf))
(require 'vlf-setup)

;; js2mode indent with 2 spaces
(setq js2-basic-offset 2)

;; Open current directory in file explorer

(defun open-in-file-explorer ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

;; Ivy-mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-f") 'counsel-ag)
(define-key prelude-mode-map (kbd "M-o") nil)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-dispatching-done)
(setq projectile-completion-system 'ivy)

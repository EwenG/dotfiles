;;; init.el ---   -*- lexical-binding: t; -*-

;; No startup screen
(setq inhibit-startup-screen t)

;; Backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
(setq vc-make-backup-files t) ;; Backup versioned files

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)







;; Indent with spaces only
(setq-default indent-tabs-mode nil)

;;NixOS shell prompt is not recognized by default. This pattern fix the issue.
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(add-to-list 'load-path "~/replique2.el/")

(eval-after-load "dash" '(dash-enable-font-lock))

;; Make horizontal split the default
(setq split-width-threshold 160)

;; Org indent mode
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (define-key smartparens-mode-map (kbd "M-<up>") nil)
            (define-key smartparens-mode-map (kbd "M-<down>") nil)
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

;;Ediff - split horizontally by default
(custom-set-variables
 '(ediff-split-window-function 'split-window-horizontally))

;;Enable replique-minor-mode
;;(add-hook 'clojure-mode-hook #'replique/minor-mode)
;;(add-hook 'js2-mode-hook #'replique/generic-minor-mode)

;;Enable replique-minor-mode
(add-hook 'clojure-mode-hook 'replique/minor-mode)
(add-hook 'css-mode-hook 'replique/minor-mode)
(add-hook 'scss-mode-hook 'replique/minor-mode)
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (sp-local-pair 'clojure-mode "'" nil :actions nil)
	    (company-mode 1)))
(add-hook 'replique/mode
	  (lambda ()
	    (sp-local-pair 'replique/mode "'" nil :actions nil)
	    (company-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Handle the case when the packaes are initialized for
            ;; the first time
            (when (package-installed-p 'elisp-slime-nav)
              (elisp-slime-nav-mode 1)
              (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer))
            (when (package-installed-p 'company)
              (company-mode 1))
            (when (package-installed-p 'eldoc)
              (turn-on-eldoc-mode))))


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
(add-hook 'ivy-mode-hook
	  (lambda ()
	    (setq ivy-use-virtual-buffers t)
	    (setq ivy-height 10)
	    (setq ivy-count-format "(%d/%d) ")))
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-f") 'counsel-ag)

(add-hook 'projectile-mode-hook
	  (lambda ()
	    (setq projectile-completion-system 'ivy)))
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p p") 'projectile-switch-project)

;; Smartparens config
(global-set-key (kbd "C-M-f") 'sp-forward-sexp)
(global-set-key (kbd "C-M-b") 'sp-backward-sexp)
(global-set-key (kbd "M-s") 'sp-splice-sexp)
(global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-a") 'beginning-of-line-text)

(require 'ewen)

(add-hook 'after-init-hook 'ewen-after-init-hook)

(defun ewen-after-init-hook ()
  (require 'package)

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  ;; Org-mode's repository
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (unless (package-installed-p 'elisp-slime-nav)
    (package-refresh-contents)
    (package-install 'elisp-slime-nav))
  
  (unless (package-installed-p 'dash)
    (package-refresh-contents)
    (package-install 'dash))
  
  (unless (package-installed-p 'dash-functional)
    (package-refresh-contents)
    (package-install 'dash-functional))

  (unless (package-installed-p 'projectile)
    (package-refresh-contents)
    (package-install 'projectile))
  (projectile-mode 1)

  (unless (package-installed-p 'company)
    (package-refresh-contents)
    (package-install 'company))
  
  (unless (package-installed-p 'nix-mode)
    (package-refresh-contents)
    (package-install 'nix-mode))

  (unless (package-installed-p 'smartparens)
    (package-refresh-contents)
    (package-install 'smartparens))
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (smartparens-global-strict-mode 1)
  (show-smartparens-global-mode 1)

  (unless (package-installed-p 'clojure-mode)
    (package-refresh-contents)
    (package-install 'clojure-mode))

  (unless (package-installed-p 's)
    (package-refresh-contents)
    (package-install 's))

  ;;Very large files
  (unless (package-installed-p 'vlf)
    (package-refresh-contents)
    (package-install 'vlf))

  (unless (package-installed-p 'ivy)
    (package-refresh-contents)
    (package-install 'ivy))
  
  (unless (package-installed-p 'counsel)
    (package-refresh-contents)
    (package-install 'counsel))
  
  (unless (package-installed-p 'swiper)
    (package-refresh-contents)
    (package-install 'swiper))

  ;; Swiper
  (require 'ivy)
  (ivy-mode 1)
  (define-key ivy-mode-map (kbd "C-c s")
    '(lambda ()
       (interactive)
       (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))

  (require 'replique)

  ;; No toolbar
  (tool-bar-mode -1)
  ;; No menubar
  (menu-bar-mode -1)
  
  ;; keyboard scroll one line at a time
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  ;; Scroll before the cursor reaches the edge of the window
  (setq scroll-margin 6)

  (unless (package-installed-p 'zenburn-theme)
    (package-refresh-contents)
    (package-install 'zenburn-theme))
  (load-theme 'zenburn t)
  )

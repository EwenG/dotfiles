;;; init.el ---   -*- lexical-binding: t; -*-

(toggle-debug-on-error)

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

(add-hook 'before-save-hook 'force-backup-of-buffer)





;; No *scratch* message
(setq initial-scratch-message nil)

;; Indent with spaces only
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

;; Disabled the alarm bell
(setq ring-bell-function 'ignore)

;; osx specific
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)
(setq mac-right-option-modifier nil)

;; Start in fullscreen mode (osx)
;; This is buggy on my computer since emacs 25, a workaround is to use run-at-time
;; (run-at-time 1 nil (lambda () (set-frame-parameter nil 'fullscreen 'fullboth)))

;; Start maximized (ubuntu)
(toggle-frame-maximized)

;; other-window in reverse
(define-key global-map (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;;NixOS shell prompt is not recognized by default. This pattern fix the issue.
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; Make horizontal split the default
(setq split-width-threshold 240)
(setq split-height-threshold 160)

;; Org indent mode
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (define-key smartparens-mode-map (kbd "M-<up>") nil)
            (define-key smartparens-mode-map (kbd "M-<down>") nil)
            ;; (setq visual-line-mode 1)
            (require 'epresent)
            (require 'ob-sh)
            (require 'ob-replique)
            (setq org-confirm-babel-evaluate nil)
            (push
             '("sr" "#+RESULTS:\n:" "")
             org-structure-template-alist)))

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
(add-hook 'clojure-mode-hook 'replique/minor-mode)

(add-hook 'css-mode-hook (lambda ()
                           (replique/minor-mode 1)
                           (company-mode 1)))
(add-hook 'js2-mode-hook (lambda ()
                           (replique/minor-mode 1)
                           (company-mode 1)
                           ;; js2mode indent with 2 spaces
                           (setq js2-basic-offset 2)))
(add-hook 'stylus-mode-hook
          (lambda ()
            (replique/minor-mode 1)
            (company-mode 1)))
(add-hook 'less-css-mode-hook
          (lambda ()
            (replique/minor-mode 1)
            (company-mode 1)))
(add-hook 'clojure-mode-hook
	  (lambda ()
      (company-mode 1)))
(add-hook 'replique/mode-hook
	  (lambda ()
      (company-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Handle the case when the packages are initialized for
            ;; the first time
            (when (package-installed-p 'elisp-slime-nav)
              (elisp-slime-nav-mode 1)
              (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer))
            (when (package-installed-p 'company)
              (company-mode 1))))

(add-hook 'comint-mode-hook
          (lambda ()
            (setq comint-input-ignoredups t)))

(defun ewen/in-eval-expression-p ()
  (equal this-command 'eval-expression))

(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda ()
            ;; Hook into `eval-expression`
            (when (ewen/in-eval-expression-p)
              (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))
            ;; Use smartparens even when in the minibuffer
            (smartparens-strict-mode 1)
            (show-smartparens-mode 1)))

(add-hook 'company-mode-hook
          (lambda ()
            (setq company-minimum-prefix-length 2)
            (define-key company-active-map (kbd "TAB") 'company-complete-selection)
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)))

;; open javascript files with js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Open current directory in file explorer

(defun open-in-file-explorer ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

;; Default web browser
(setq browse-url-browser-function 'browse-url-chromium)

(defun ewen/with-universal-prefix-arg (command)
  (lambda ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively command))))

(defun ewen/counsel-ag-in-directory ()
  (interactive)
  (let ((d (read-directory-name (concat
                                 (car counsel-ag-base-command)
                                 " in directory: "))))
    (counsel-ag nil d)))

;; Ivy-mode
(add-hook 'ivy-mode-hook
	  (lambda ()
	    (setq ivy-use-virtual-buffers t)
	    (setq ivy-height 10)
	    (setq ivy-count-format "(%d/%d) ")))
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-f") 'ewen/counsel-ag-in-directory)

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
(add-hook 'smartparens-mode-hook
          (lambda ()
            (define-key smartparens-strict-mode-map [remap kill-region] nil)
            (define-key smartparens-strict-mode-map [remap delete-region] nil)
            (sp-pair "'" nil :actions :rem)
            ;; sp-echo-match-when-invisible may have bad performance for huge forms
            (setq sp-echo-match-when-invisible nil)))

(add-hook 'eshell-mode-hook
          (lambda ()
            ;; Use bash-like completion
            (setq pcomplete-cycle-completions nil)
            (company-mode 1)))

(add-hook 'auto-revert-mode-hook
          (lambda ()
            (setq auto-revert-verbose nil)))

(add-hook 'gif-screencast-mode-hook
          (lambda ()
            ;; (define-key gif-screencast-mode-map (kbd "C-c s") 'gif-screencast-stop)
            ))

(setq-default bidi-display-reordering nil)

;; Lilypond osx

(setq load-path (append (list (expand-file-name"/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp")) load-path))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.lytex$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

;; Replique config
(setq replique/replique-coords (format "{:local/root \"%s\"}" (expand-file-name "~/clojure/replique")))

(require 'ewen)

(add-hook 'after-init-hook 'ewen-after-init-hook)

;; (set-face-attribute 'default nil :height 220)

(defun ewen-after-init-hook ()
  (require 'package)

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  ;; Org-mode's repository
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("replique" . "https://raw.githubusercontent.com/EwenG/EwenG.github.io/master/packages/") t)

  (add-to-list 'load-path "~/clj-data.el")
  (add-to-list 'load-path "~/replique.el")
  (add-to-list 'load-path "~/strette.el")
  
  (when (memq window-system '(mac ns x))
    (if (package-installed-p 'exec-path-from-shell)
	(when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize)
	  (exec-path-from-shell-copy-env "PATH"))
      (package-refresh-contents)
      (package-install 'exec-path-from-shell)))

  (unless (package-installed-p 'elisp-slime-nav)
    (package-refresh-contents)
    (package-install 'elisp-slime-nav))
  
  (unless (package-installed-p 'projectile)
    (package-refresh-contents)
    (package-install 'projectile))
  (projectile-global-mode)

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

  (unless (package-installed-p 'stylus-mode)
    (package-refresh-contents)
    (package-install 'stylus-mode))
  
  (unless (package-installed-p 'sass-mode)
    (package-refresh-contents)
    (package-install 'sass-mode))

  ;; (unless (package-installed-p 'scss-mode)
  ;;   (package-refresh-contents)
  ;;   (package-install 'scss-mode))

  (unless (package-installed-p 'less-css-mode)
    (package-refresh-contents)
    (package-install 'less-css-mode))

  (unless (package-installed-p 'js2-mode)
    (package-refresh-contents)
    (package-install 'js2-mode))

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
  (require 'counsel)

  (unless (package-installed-p 'swiper)
    (package-refresh-contents)
    (package-install 'swiper))

  ;; Swiper
  (require 'ivy)
  ;; Dont use ivy autocompletion when manually triggering autocompletion
  ;; (for example in minibuffer)
  (setq ivy-do-completion-in-region nil)
  (ivy-mode 1)

  (unless (package-installed-p 'replique)
    (package-refresh-contents)
    (package-install 'replique))

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

  (unless (package-installed-p 'multiple-cursors)
    (package-refresh-contents)
    (package-install 'multiple-cursors))

  (unless (package-installed-p 'gif-screencast)
    (package-refresh-contents)
    (package-install 'gif-screencast))

  (unless (package-installed-p 'zenburn-theme)
    (package-refresh-contents)
  (package-install 'zenburn-theme))
  ;;(load-theme 'zenburn t)

  (load-theme 'adwaita t)

  )

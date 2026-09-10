;;; init.el ---   -*- lexical-binding: t; -*-

(setq debug-on-error t)

(setq gc-cons-threshold 3500000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Org-mode's repository
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("replique" . "https://raw.githubusercontent.com/EwenG/EwenG.github.io/master/packages/") t)

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

(setq-default fill-column 80)

;; Disabled the alarm bell
(setq ring-bell-function 'ignore)

;; mac os specific
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)
(setq mac-right-option-modifier nil)
(setq mac-pass-command-to-system nil)

;; Start in fullscreen mode (mac os)
;; This is buggy on my computer since emacs 25, a workaround is to use run-at-time
;; (run-at-time 1 nil (lambda () (set-frame-parameter nil 'fullscreen 'fullboth)))

;; Start maximized (ubuntu)
(toggle-frame-maximized)

;; C-S-<arrow> moves to the window in that direction.
;; NOT the default `shift' modifier: `windmove-default-keybindings' binds into
;; `windmove-mode-map', a MINOR-mode map, so it would override org-mode's own
;; S-<arrow> (todo state, priority, timestamp) rather than losing to it.
(windmove-default-keybindings '(control shift))
;; other-window in reverse
(define-key global-map (kbd "M-o") (lambda () (interactive) (other-window -1)))

;;NixOS shell prompt is not recognized by default. This pattern fix the issue.
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; Make horizontal split the default
(setq split-width-threshold 240)
(setq split-height-threshold 160)

;; Org indent mode.
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (turn-on-auto-fill)
            ;; (setq visual-line-mode 1)
            (require 'ob-replique)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<left>") #'org-metaleft)
  (define-key org-mode-map (kbd "C-<right>") #'org-metaright)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t))))

(defun ewen/org-insert-results ()
  "Insert a `#+RESULTS:' marker followed by an empty result line."
  (interactive)
  (insert "#+RESULTS:\n:"))

;; C-up and C-down to slowly scroll the buffer
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)] 'gcm-scroll-up)

(setq-default treesit-font-lock-level 4)

;; Ediff
(custom-set-variables
 ;; Don't show a separate window for the control panel 
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 ;; Split horizontally by default
 '(ediff-split-window-function 'split-window-horizontally))

;;Enable replique-minor-mode
(add-hook 'replique-clojure-mode-hook 'replique/minor-mode)

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

(add-hook 'replique-clojure-mode-hook
	        (lambda ()
            (company-mode 1)))

(add-hook 'replique/mode-hook
	        (lambda ()
            (company-mode 1)
            (set-face-attribute 'replique-clojure-keyword-face nil :weight 'bold)))

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

;; Use the tree-sitter C mode (better nav/imenu than cc-mode)
(setq treesit-language-source-alist                                                                                                                                                                          
      '((c   . ("https://github.com/tree-sitter/tree-sitter-c"))                                                                                                                                             
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))                                                                                                
(with-eval-after-load 'eglot
  ;; clangd is at /usr/bin/clangd — good defaults, but be explicit:
  (add-to-list 'eglot-server-programs
               '(c-ts-mode . ("clangd" "--header-insertion=never"
                              ;; static analysis inline    
                              "--clang-tidy"))))
(add-hook 'c-ts-mode-hook #'eglot-ensure)

(use-package clang-format
  :ensure t
  :commands (clang-format-buffer clang-format-region))

(use-package dape :ensure t)

(with-eval-after-load 'dape                                                                                                                                           
    ;; Launch a fresh Emacs under the debugger.
    ;; (Never attach to your editing Emacs -- attaching stops the target,
    ;;  which would freeze the very Emacs running dape.)                                                                                                                               
    (add-to-list 'dape-configs                                                                                                                                          
      `(lldb-emacs                                                                                                                                                     
        modes nil                                                                                                                                                       
        ensure dape-ensure-command                                                                                                                                      
        command "/opt/homebrew/opt/llvm/bin/lldb-dap"
        command-cwd ,(expand-file-name "~/clojure/tree-sitter-treejure")                                                                                                
        :type "lldb-dap"                                                                                                                                                
        :request "launch"
        :program "/Applications/Emacs.app/Contents/MacOS/Emacs-arm64-11"
        :args [])))

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
  "Open the current file's directory in the OS file manager."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (pcase system-type
      ('darwin (call-process "open" nil 0 nil dir))
      ('windows-nt (w32-shell-execute "open" dir))
      (_ (call-process "xdg-open" nil 0 nil dir)))))

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

;; Fonts

;; (set-face-attribute 'default nil
;;                     :font "aporetic"
;;                     :height 130)

;; 1. Set the Default Font (for the editor)
;; :height is in 1/10pt. 130 = 13pt.
(set-face-attribute 'default nil 
                    :font "JetBrains Mono" 
                    :height 130
                    :weight 'bold)

;; 2. Set Fixed-Pitch (for code blocks in Org-mode, etc.)
;; It's best to inherit from 'default to keep it crisp.
(set-face-attribute 'fixed-pitch nil 
                    :inherit 'default)

;; 3. Set Variable-Pitch (for prose, comments, or documentation)
;; Using a serif or clean sans-serif font makes comments feel distinct.
(set-face-attribute 'variable-pitch nil 
                    :font "Iosevka Aile" 
                    :height 1.0)

;; On macOS, this makes fonts look sharper/thinner
(setq ns-use-thin-smoothing t)

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
	    (setq projectile-completion-system 'ivy)
      ;; Set to hybrid in order to enable the .projectile file
      ;; (when the value is alien, .projectile is ignored)
      (setq projectile-indexing-method 'hybrid)))

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

;; Long-line redisplay performance.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Lilypond mac os

(setq load-path (append (list (expand-file-name"/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp")) load-path))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.lytex$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

;; Replique config
(setq replique/replique-coords (format "{:local/root \"%s\"}" (expand-file-name "~/clojure/replique")))

;; Fix "inappropriate ioctl for device" when using gpg
(setq epg-pinentry-mode 'loopback)

(require 'ewen)

(add-hook 'after-init-hook 'ewen-after-init-hook)

(defun ewen-after-init-hook ()
  (add-to-list 'load-path "~/clj-data.el")
  (add-to-list 'load-path "~/replique.el")
  ;; (add-to-list 'load-path "~/replique.el-2")
  (add-to-list 'load-path "~/strette.el")

  (when (memq window-system '(mac ns x))
    (unless (package-installed-p 'exec-path-from-shell)
      (package-refresh-contents)
      (package-install 'exec-path-from-shell))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (dolist (var '("PATH" "AWS_ACCESS_KEY" "AWS_SECRET_KEY" "TRANSIFEX_API_TOKEN"))
      (exec-path-from-shell-copy-env var))
    (setenv "QT_QPA_PLATFORM_PLUGIN_PATH" "/opt/homebrew/opt/qtbase/share/qt/plugins/platforms/")
    (setenv "LANG" "en_US.UTF-8")
    (setenv "LC_ALL" "en_US.UTF-8"))

  (unless (package-installed-p 'elisp-slime-nav)
    (package-refresh-contents)
    (package-install 'elisp-slime-nav))
  
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

  (unless (package-installed-p 'magit)
    (package-refresh-contents)
    (package-install 'magit))

  ;; transient: required by magit and nix-mode.
  (use-package transient :ensure t)

  (use-package plz :ensure t)

  (use-package markdown-mode
    :ensure t
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown")
    :bind (:map markdown-mode-map
                ("C-c C-e" . markdown-do)))

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

  (use-package catppuccin-theme
    :ensure t
    :config
    (load-theme 'catppuccin t)
    (setq catppuccin-flavor 'latte)
    (catppuccin-reload))

  (set-face-attribute 'font-lock-comment-face nil :weight 'bold)
  (set-face-attribute 'font-lock-string-face nil :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-call-face nil :weight 'bold)
  (set-face-attribute 'font-lock-type-face nil :weight 'bold)
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
  (set-face-attribute 'font-lock-delimiter-face nil :weight 'bold)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :weight 'bold)
  (set-face-attribute 'comint-highlight-prompt nil :weight 'bold)
  
  (unless (package-installed-p 'editorconfig)
    (package-refresh-contents)
    (package-install 'editorconfig))
  (editorconfig-mode 1)

  )

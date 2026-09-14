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

(defun ewen/backup-enable-predicate (name)
  "Back up NAME unless it is regenerable or large.
Logs and build output under a repo are ignored by git for the same reason
they are not worth backing up."
  (and (normal-backup-enable-predicate name)
       (not (string-match-p "\\.\\(logs?\\)\\'" name))
       (not (string-match-p "/\\(target\\|node_modules\\|\\.shadow-cljs\\)/" name))
       (< (or (file-attribute-size (file-attributes name)) 0)
          (* 1 1024 1024))))
(setq backup-enable-predicate #'ewen/backup-enable-predicate)

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
(defun ewen/other-window-backward ()
  "Select the previous window, like `other-window' in reverse."
  (interactive)
  (other-window -1))

(define-key global-map (kbd "M-o") 'ewen/other-window-backward)

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

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when (package-installed-p 'elisp-slime-nav)
              (elisp-slime-nav-mode 1))
            (when (package-installed-p 'company)
              (company-mode 1))))

(setq comint-input-ignoredups t)

;; Use the tree-sitter C mode (better nav/imenu than cc-mode)
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(c . ("https://github.com/tree-sitter/tree-sitter-c")))
  (add-to-list 'treesit-language-source-alist
               '(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))))
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

(use-package dape
  :ensure t
  ;; Not used day to day: `:defer t' keeps it off the startup path.  The
  ;; config below is applied whenever dape is actually loaded (M-x dape).
  :defer t
  :config
  ;; Debug a *fresh* Emacs under lldb.
  ;;
  ;; This is a launch config, not an attach one, on purpose: attaching to the
  ;; Emacs you are editing in stops the target, which would freeze the very
  ;; Emacs running dape.
  ;;
  (add-to-list 'dape-configs
               '(lldb-emacs
                 modes nil
                 ensure dape-ensure-command
                 ;; lldb-dap ships inside Homebrew's llvm and is not on PATH,
                 ;; so fall back to the keg path when the lookup fails.
                 command (or (executable-find "lldb-dap")
                             "/opt/homebrew/opt/llvm/bin/lldb-dap")
                 ;; Project root of whatever is being visited
                 command-cwd dape-command-cwd
                 :type "lldb-dap"
                 :request "launch"
                 ;; The Emacs binary actually running this session.
                 :program (expand-file-name invocation-name invocation-directory)
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

(setq company-minimum-prefix-length 2)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

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
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-do-completion-in-region nil)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c f") 'counsel-ag)

(defun ewen/project-magit-status ()
  "Show the Magit status of the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'magit-status)))

(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'ghostel-project)
  (setq project-switch-commands
        '((ewen/project-magit-status "Magit" ?m)
          (project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (project-switch-to-buffer "Buffer" ?b)
          (project-dired "Dired" ?d)
          (ghostel-project "Ghostel" ?t))))

;; Smartparens config
(global-set-key (kbd "C-M-f") 'sp-forward-sexp)
(global-set-key (kbd "C-M-b") 'sp-backward-sexp)
(define-key search-map (kbd "M-s") 'sp-splice-sexp)
(global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-a") 'beginning-of-line-text)
(with-eval-after-load 'smartparens-config
  (define-key smartparens-strict-mode-map [remap kill-region] nil)
  (define-key smartparens-strict-mode-map [remap delete-region] nil)
  (sp-pair "'" nil :actions :rem)
  ;; sp-echo-match-when-invisible may have bad performance for huge forms
  (setq sp-echo-match-when-invisible nil))

(add-hook 'eshell-mode-hook
          (lambda ()
            ;; Use bash-like completion
            (setq pcomplete-cycle-completions nil)
            (company-mode 1)))

(setq auto-revert-verbose nil)

;; Long-line redisplay performance.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; LilyPond

(add-to-list 'load-path (expand-file-name "~/lilypond"))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.lytex$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
;; Removed from Emacs in 26, but ~/lilypond/lilypond-what-beat.el still calls
;; it five times -- that is C-c b (LilyPond-what-beat) in a .ly buffer.  The
;; ~/lilypond tree is an unversioned 2020 snapshot, so the shim lives here
;; where git keeps it rather than as a patch that a re-copy would silently
;; undo.
(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

;; Replique config
(setq replique/replique-coords (format "{:local/root \"%s\"}" (expand-file-name "~/clojure/replique")))

(defun ewen/git-worktrees (dir)
  "Worktree directories of DIR's repository, or nil when DIR is not in one."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (ignore-errors
      (seq-keep (lambda (l)
                  (and (string-prefix-p "worktree " l) (substring l 9)))
                (process-lines "git" "worktree" "list" "--porcelain")))))

;; Visit a symlink as itself, not as its target.
(setq vc-follow-symlinks nil)

;; Merge conflicts.  ~/.gitconfig sets merge.conflictStyle to zdiff3, so a
;; conflict carries the merge base between its two sides.  Everything below
;; needs that base: without it `ediff-show-clashes-only' has nothing to compare
;; against and `smerge-resolve' loses its diff3 shortcut.

;; Buffer C starts as our side, not as both sides wrapped in combination
;; markers, which is what `combined' -- the default -- produces.
(setq ediff-default-variant 'default-A)

;; n/p then stop only where both sides changed; regions where one side matches
;; the base are already merged correctly in C.
(setq-default ediff-show-clashes-only t)

(defun ewen/smerge-maybe ()
  "Turn on `smerge-mode' and go to the first conflict, if the file has any."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
    (smerge-start-session)))

(add-hook 'find-file-hook #'ewen/smerge-maybe)

;; Fix "inappropriate ioctl for device" when using gpg
(setq epg-pinentry-mode 'loopback)

;; Ghostel (terminal emulator) helpers

(defun ewen/claude (&optional dir new)
  "Run Claude Code in a ghostel buffer, in DIR.
Interactively, prompt for the directory, completing over the worktrees of the
current repo and defaulting to the project you are in.
`ghostel-buffer-name-function' is disabled buffer-locally so the buffer keeps
its own name rather than being renamed to match a shell terminal in the same
directory."
  (interactive
   (let* ((here (directory-file-name
                 (expand-file-name
                  (or (ignore-errors (project-root (project-current)))
                      default-directory))))
          (cands (or (ewen/git-worktrees here) (list here))))
     (list (completing-read (format "Start Claude in (default %s): "
                                    (file-name-nondirectory here))
                            cands nil nil nil nil here)
           current-prefix-arg)))
  (let* ((dir (directory-file-name
               (expand-file-name
                (or dir (ignore-errors (project-root (project-current)))
                    default-directory))))
         (default-directory (file-name-as-directory dir))
         (label (abbreviate-file-name dir))
         (name (format "*claude: %s*" label))
         (existing (get-buffer name))
         ;; A second session in the same directory gets its own buffer rather
         ;; than being exec'd on top of a running one.  `generate-new-buffer'
         ;; gives "*claude: DIR*<2>".  A buffer whose process has exited is
         ;; reused, since `ghostel-kill-buffer-on-exit' is nil and those linger.
         (buf (if (or new (and existing (get-buffer-process existing)))
                  (generate-new-buffer name)
                (or existing (get-buffer-create name)))))
    ;; Display before exec: `ghostel-exec' sizes the terminal to the buffer's
    ;; window, falling back to 80x24 when the buffer is not on screen yet.
    (pop-to-buffer buf)
    (ghostel-exec buf "claude")
    (with-current-buffer buf
      (setq-local ghostel-buffer-name-function nil))))

(require 'ewen)

(add-hook 'after-init-hook 'ewen-after-init-hook)

(defun ewen-after-init-hook ()
  (add-to-list 'load-path "~/clj-data.el")
  (add-to-list 'load-path "~/replique.el")
  ;; (add-to-list 'load-path "~/replique.el-2")
  (add-to-list 'load-path "~/strette.el")

  (when (memq window-system '(mac ns x))
    (use-package exec-path-from-shell
      :ensure t
      :config
      (exec-path-from-shell-initialize)
      (dolist (var '("PATH" "AWS_ACCESS_KEY" "AWS_SECRET_KEY" "TRANSIFEX_API_TOKEN"))
        (exec-path-from-shell-copy-env var)))
    (setenv "QT_QPA_PLATFORM_PLUGIN_PATH" "/opt/homebrew/opt/qtbase/share/qt/plugins/platforms/")
    (setenv "LANG" "en_US.UTF-8")
    (setenv "LC_ALL" "en_US.UTF-8"))

  (use-package elisp-slime-nav  :ensure t :defer t)
  (use-package company          :ensure t :defer t)
  (use-package nix-mode         :ensure t :defer t)
  (use-package stylus-mode      :ensure t :defer t)
  (use-package sass-mode        :ensure t :defer t)
  (use-package less-css-mode    :ensure t :defer t)
  (use-package js2-mode         :ensure t :defer t)
  (use-package vlf              :ensure t :defer t) ; very large files
  (use-package swiper           :ensure t :defer t)
  (use-package magit
    :ensure t :defer t
    ;; Reuse the selected window, so `C-x g' from a file does not split the
    ;; frame .`magit-revision-mode 'derives from `magit-diff-mode', so commits
    ;; still open beside the log, which is what keeps n/p browsing usable.
    :custom
    (magit-display-buffer-function
     #'magit-display-buffer-same-window-except-diff-v1)
    (magit-ediff-dwim-show-on-hunks t))
  ;; Git changes in the fringe.
  (use-package diff-hl
    :ensure t
    :hook ((prog-mode . diff-hl-mode)
           (magit-post-refresh . diff-hl-magit-post-refresh))
    :config
    (diff-hl-flydiff-mode 1))

  (use-package multiple-cursors :ensure t :defer t)
  (use-package gif-screencast   :ensure t :defer t)

  ;; transient: required by magit and nix-mode.
  (use-package transient :ensure t)

  (use-package plz :ensure t)

  ;; Terminal emulator, for running Claude Code inside Emacs.
  (use-package ghostel
    :ensure t
    :defer t
    :commands (ghostel ghostel-project ghostel-other ghostel-exec)
    :bind ("C-x m" . ghostel)
    :custom
    (ghostel-buffer-name-function #'ghostel-buffer-name-by-directory)
    (ghostel-max-scrollback (* 10 1024 1024))
    ;; Keep the buffer after the process exits so the last output stays readable.
    (ghostel-kill-buffer-on-exit nil)
    :config
    ;; The upstream default plus the window-navigation keys: semi-char mode
    ;; sends everything else straight to the terminal.  `setopt' rather than
    ;; `setq' because this defcustom's :set rebuilds the keymap -- and it lives
    ;; here rather than in :custom because that :set loads ghostel, which would
    ;; defeat :defer and warn about the module on every startup.
    (setopt ghostel-keymap-exceptions
            '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
              "C-S-<left>" "C-S-<right>" "C-S-<up>" "C-S-<down>" "M-o"))
    (define-key ghostel-semi-char-mode-map (kbd "C-s") #'swiper))

  (use-package markdown-mode
    :ensure t
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown")
    :bind (:map markdown-mode-map
                ("C-c C-e" . markdown-do)))

  (use-package smartparens
    :ensure t
    :config
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (smartparens-global-strict-mode 1)
    (show-smartparens-global-mode 1))

  (use-package counsel :ensure t)

  (use-package ivy
    :ensure t
    :config (ivy-mode 1))

  (use-package replique :ensure t)

  (use-package editorconfig
    :ensure t
    :config (editorconfig-mode 1))

  ;; No toolbar
  (tool-bar-mode -1)
  ;; No menubar
  (menu-bar-mode -1)

  ;; keyboard scroll one line at a time
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  ;; Scroll before the cursor reaches the edge of the window
  (setq scroll-margin 6)

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

  )

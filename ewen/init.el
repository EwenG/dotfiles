;;; init.el ---   -*- lexical-binding: t; -*-

(setq debug-on-error t)

;; A GC pause is proportional to the live heap, so scale the interval with the
;; heap too -- a fixed `gc-cons-threshold' makes GC cost more as the session
;; grows.  1.0 measured ~43MiB between GCs on a 21MiB heap.
(setq gc-cons-percentage 1.0)

;; libgccjit's driver derives -mmacosx-version-min from the Darwin version
;; (darwin27 -> 18.0), which clang rejects on macOS 27.  Naming the target is
;; what the driver reads before it derives anything, so the wrong number is
;; never computed rather than overridden by a later flag -- and it is the same
;; knob replique.el's Makefile sets, for a batch run that reads no init.
;; Bump it after a macOS upgrade.
(when (and (eq system-type 'darwin)
           (null (getenv "MACOSX_DEPLOYMENT_TARGET")))
  (setenv "MACOSX_DEPLOYMENT_TARGET" "27.0"))

;; Early, so `comment' and the other utils are available below.
(require 'ewen)

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
            ;;(require 'ob-replique)
            ))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<left>") #'org-metaleft)
  (define-key org-mode-map (kbd "C-<right>") #'org-metaright)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (python . t))))

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
;;(add-hook 'replique-clojure-mode-hook 'replique/minor-mode)

;; (add-hook 'css-mode-hook (lambda ()
;;                            (replique/minor-mode 1)
;;                            (company-mode 1)))
(add-hook 'css-mode-hook #'replique-css-mode)
(add-hook 'js2-mode-hook (lambda ()
                           ;;(replique/minor-mode 1)
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

(add-hook 'replique-repl-mode-hook
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

(defun ewen/magit-kill-buffers (&optional all-repos)
  "Kill this repository's Magit buffers, except its status buffer.
With prefix ALL-REPOS, do it for every repository.  Process
buffers with a live process are spared."
  (interactive "P")
  ;; `magit--default-directory' is how Magit itself tags a buffer with its
  ;; repository; it is permanent-local, so a let-bound `default-directory'
  ;; cannot make a buffer look like it belongs elsewhere.
  (let ((topdir (unless all-repos
                  (or (magit-toplevel)
                      (user-error "Not inside a Git repository"))))
        (killed 0))
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf
              (and (derived-mode-p 'magit-mode)
                   (not (derived-mode-p 'magit-status-mode))
                   (not (get-buffer-process buf))
                   (or all-repos (equal magit--default-directory topdir))))
        (kill-buffer buf)
        (setq killed (1+ killed))))
    (message "Killed %d Magit buffer%s" killed (if (= killed 1) "" "s"))))

(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'ghostel-project)
  (setq project-vc-ignores '("**/.*/**"))
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

;; Replique 2 spells the same setting `replique-coordinates', and it defaults to
;; nil rather than to a path: no coordinate works on another machine until
;; replique 2 is published, so the checkout is named here rather than shipped as
;; a default.  Set before the package loads, which is where the value survives:
;; `defcustom' leaves a variable that already has one alone.
(setq replique-coordinates
      (format "{:local/root \"%s\"}" (expand-file-name "~/clojure/replique-2")))

(setq eldoc-echo-area-use-multiline-p 2
      eldoc-echo-area-display-truncation-message nil
      eldoc-echo-area-prefer-doc-buffer 'maybe)

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

;; `diff-hl-ediff-current-hunk' fabricates the reference buffer and hands it to
;; `ediff-buffers', so ediff cannot tell it from a buffer we opened ourselves
;; and `ediff-keep-variants' leaves it behind on quit.  Nobody owns it, hence
;; the marker.  `ediff-cleanup-hook' runs in the control buffer while
;; ediff-buffer-A/B are still readable, and `ediff-janitor' skips dead buffers.
(defvar-local ewen/diff-hl-ediff-reference nil
  "Non-nil in the throwaway buffer diff-hl builds for Ediff.")

(define-advice diff-hl--ediff-reference-buffer
    (:filter-return (buf) ewen/mark-reference)
  (when (buffer-live-p buf)
    (with-current-buffer buf (setq ewen/diff-hl-ediff-reference t)))
  buf)

(defun ewen/diff-hl-ediff-kill-reference ()
  "Kill the buffer diff-hl built for the Ediff session being quit."
  (dolist (buf (list ediff-buffer-A ediff-buffer-B))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'ewen/diff-hl-ediff-reference buf)
               (not (buffer-modified-p buf)))
      (kill-buffer buf))))

(add-hook 'ediff-cleanup-hook #'ewen/diff-hl-ediff-kill-reference)

;; Ediff never restores the pre-session layout: `ediff-cleanup-mess' re-displays
;; A and B side by side and balances the windows.  Killing the reference buffer
;; above makes that show the file twice, but the layout was wrong either way.
;; Magit's sessions bind `ediff-quit-hook' buffer-locally and restore their own
;; winconf, so this only ever fires for diff-hl and plain ediff.
(defvar ewen/ediff-window-config nil)

(defun ewen/ediff-save-window-config ()
  (setq ewen/ediff-window-config (current-window-configuration)))

(defun ewen/ediff-restore-window-config ()
  (when (window-configuration-p ewen/ediff-window-config)
    (set-window-configuration ewen/ediff-window-config)
    (setq ewen/ediff-window-config nil)))

(add-hook 'ediff-before-setup-hook #'ewen/ediff-save-window-config)
;; Appended: `ediff-cleanup-mess' is on this hook too, and must run first.
(add-hook 'ediff-quit-hook #'ewen/ediff-restore-window-config t)

(defun ewen/smerge-maybe ()
  "Turn on `smerge-mode' and go to the first conflict, if the file has any."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
    (smerge-start-session)))

(add-hook 'find-file-hook #'ewen/smerge-maybe)

(defvar ewen/nosco-local (expand-file-name "~/clojure/nosco-local")
  "Where nosco-gamma's personal dev files and its REPL stage are described.")

(require 'wtlocal)
(setq wtlocal-dirs (list ewen/nosco-local))
(wtlocal-mode 1)

;; WHETHER roots.txt IS STILL THE TRUTH.  It is the one thing about the stage
;; written down rather than read off the disk, and a deps.edn root it does not
;; name is linked whole: the JVM freezes it where it pointed at start, and
;; nothing else says so before the process reads the wrong branch.  The
;; aliases decide the answer and are a directory local variable, hence asked
;; of replique rather than of point.sh.

(defun ewen/stage-unlisted-roots (worktree)
  "The classpath roots WORKTREE declares that roots.txt does not name.
One direction only: a root roots.txt names and deps.edn does not costs
nothing to hold for real."
  (let ((listed (with-temp-buffer
                  (insert-file-contents (expand-file-name "roots.txt" ewen/nosco-local))
                  (seq-remove (lambda (line) (string-prefix-p "#" line))
                              (split-string (buffer-string) "\n" t "[ \t]+"))))
        ;; Only values already known to be safe: a hook must not stop to ask
        (enable-local-variables :safe))
    (and (require 'replique-classpath nil t)
         (seq-difference (replique-classpath-directories worktree) listed))))

(add-hook 'wtlocal-after-point-hook
          (lambda (worktree stage)
            (when-let* ((unlisted (ewen/stage-unlisted-roots worktree)))
              (display-warning
               'wtlocal
               (format (concat "%s declares classpath roots that roots.txt does "
                               "not name:\n\n  %s\n\n"
                               "Those are linked whole, so the JVM freezes them "
                               "where they pointed when it started.  Add them to "
                               "%s, point again, and restart the process.")
                       (file-name-nondirectory (directory-file-name worktree))
                       (string-join unlisted "\n  ")
                       (expand-file-name "roots.txt" ewen/nosco-local))))
            (when (require 'replique nil t)
              (if-let* ((process (replique-process-in stage)))
                  (replique-relinked process worktree wtlocal-pointed-from
                                     (and wtlocal-pointed-from
                                          (wtlocal-unchanged-files
                                           wtlocal-pointed-from worktree)))
                ;; A process started in a worktree reads that worktree whatever
                ;; the stage points at
                (when-let* ((elsewhere (replique-process-current)))
                  (display-warning
                   'wtlocal
                   (format (concat "Nothing is running in %s, so nothing was "
                                   "reloaded.  %s is running in %s instead - "
                                   "kill it and M-x replique-connect in %s.")
                           (abbreviate-file-name stage)
                           (replique-process--id elsewhere)
                           (abbreviate-file-name
                            (directory-file-name
                             (replique-process--directory elsewhere)))
                           (abbreviate-file-name stage))))))))

;; Fix "inappropriate ioctl for device" when using gpg
(setq epg-pinentry-mode 'loopback)

(defun ewen/claude--read-dir (prompt default)
  "Read a directory named by PROMPT, completing over DEFAULT's worktrees."
  (let* ((default (directory-file-name (expand-file-name default)))
         (cands (or (ewen/git-worktrees default) (list default))))
    (completing-read (format "%s (default %s): "
                             prompt (file-name-nondirectory default))
                     cands nil nil nil nil default)))

(defun ewen/claude (&optional dir new args)
  "Run Claude Code in a ghostel buffer, in DIR, passing ARGS to the CLI.
Interactively, prompt for the directory, completing over the worktrees of the
current repo and defaulting to the project you are in.
`ghostel-buffer-name-function' is disabled buffer-locally so the buffer keeps
its own name rather than being renamed to match a shell terminal in the same
directory."
  (interactive
   (list (ewen/claude--read-dir
          "Start Claude in"
          (or (ignore-errors (project-root (project-current))) default-directory))
         current-prefix-arg))
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
    (ghostel-exec buf "claude" args)
    (with-current-buffer buf
      (setq-local ghostel-buffer-name-function nil))))

(defconst ewen/claude-projects-dir (expand-file-name "~/.claude/projects/")
  "Where Claude Code keeps its transcripts, one directory per working directory.")

(defun ewen/claude--transcript (id)
  "The transcript file of session ID, wherever it was recorded, or nil.
Session ids are unique across projects, so a glob over every project
directory finds the one that holds it."
  (car (file-expand-wildcards
        (expand-file-name (concat "*/" id ".jsonl") ewen/claude-projects-dir))))

(defun ewen/claude--transcript-cwd (file)
  "The directory the session recorded in FILE ran in, or nil.
Read out of the transcript rather than taken from the name of the project
directory holding it: that name mangles every slash to a dash, so
~/clojure/nosco-gamma-1 and the worktree nosco-gamma/1 share one spelling and
neither can be turned back into a path.  Only the head of the file is read --
the first rows are bookkeeping, the working directory arrives with the first
message -- and the truncated last line simply fails to parse."
  (with-temp-buffer
    (insert-file-contents file nil 0 200000)
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (when-let* ((row (ignore-errors
                           (json-parse-string
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))
                            :object-type 'alist)))
                    (cwd (alist-get 'cwd row)))
          (throw 'found cwd))
        (forward-line 1))
      nil)))

(defun ewen/claude--read-session-id ()
  "Read a session id, offering one sitting on the kill ring as the default."
  (let* ((killed (ignore-errors (current-kill 0 t)))
         (default (and killed
                       (string-match
                        "[0-9a-f]\\{8\\}\\(-[0-9a-f]\\{4\\}\\)\\{3\\}-[0-9a-f]\\{12\\}"
                        killed)
                       (match-string 0 killed))))
    (string-trim (read-string (if default
                                  (format "Session id (default %s): " default)
                                "Session id: ")
                              nil nil default))))

(defun ewen/claude-resume (id &optional dir)
  "Resume the Claude Code session ID in DIR, which defaults to its own one.
Interactively, read the id -- a session id on the kill ring is offered as the
default -- then the directory, completing over the worktrees of the repo the
session ran in.

In its own directory the session simply continues.  Anywhere else it is
resumed from the transcript\'s path and forked.  Both are needed: \"claude
--resume ID\" only searches the transcripts recorded for the directory it is
started in, and resuming the path alone would keep the original id, which
Claude Code refuses to resume while that session is live and whose transcript
stays filed under the old directory.  The fork replays the conversation into
a new session, recorded under DIR, leaving the original untouched."
  (interactive
   (let* ((id (ewen/claude--read-session-id))
          (file (or (ewen/claude--transcript id)
                    (user-error "No transcript for session %s" id))))
     (list id (ewen/claude--read-dir "Resume in"
                                     (or (ewen/claude--transcript-cwd file)
                                         default-directory)))))
  (let* ((id (string-trim id))
         (file (or (ewen/claude--transcript id)
                   (user-error "No transcript for session %s" id)))
         (origin (ewen/claude--transcript-cwd file))
         (dir (directory-file-name
               (expand-file-name (or dir origin default-directory)))))
    (unless (file-directory-p dir)
      (user-error "No such directory: %s" dir))
    (ewen/claude dir nil (if (and origin (file-equal-p dir origin))
                             (list "--resume" id)
                           (list "--resume" file "--fork-session")))))

(defun ewen/claude-session-p (buffer)
  "Non-nil when BUFFER is a Claude ghostel session with a running process.
`ghostel-kill-buffer-on-exit' is nil, so finished sessions linger; requiring
a live process keeps those out."
  (and (string-prefix-p "*claude: " (buffer-name buffer))
       (process-live-p (get-buffer-process buffer))))

(defun ewen/claude-sessions ()
  "Every live Claude session."
  (seq-filter #'ewen/claude-session-p (buffer-list)))

(defun ewen/claude-buffers-for (dir)
  "Live Claude sessions running in DIR.
Matches by prefix so the \"<2>\" of a second session in the same directory
is included."
  (let ((name (format "*claude: %s*"
                      (abbreviate-file-name (directory-file-name dir)))))
    (seq-filter (lambda (b) (string-prefix-p name (buffer-name b)))
                (ewen/claude-sessions))))

(defun ewen/claude--visible-sessions ()
  "Live Claude sessions on screen, the most recently used window first."
  (let (bufs)
    (dolist (w (sort (window-list nil 'never)
                     (lambda (a b) (> (window-use-time a) (window-use-time b)))))
      (let ((b (window-buffer w)))
        (when (and (ewen/claude-session-p b) (not (memq b bufs)))
          (push b bufs))))
    (nreverse bufs)))

(defun ewen/claude--pick (bufs)
  "The single session in BUFS, or one chosen from several, or nil for none."
  (cond ((null bufs) nil)
        ((null (cdr bufs)) (car bufs))
        (t (get-buffer (completing-read "Claude session: "
                                        (mapcar #'buffer-name bufs) nil t)))))

(defun ewen/claude--start (dir)
  "Offer to start Claude in DIR, then abort: it is not ready to be pasted into."
  (if (y-or-n-p (format "No Claude session.  Start one in %s? "
                        (abbreviate-file-name dir)))
      (progn (ewen/claude dir)
             (user-error "Claude is starting -- send again once it is ready"))
    (user-error "No Claude session")))

(defun ewen/claude--session (dir)
  "The Claude session to send to, preferring DIR's.
A session on screen wins, since that is the one being watched: among several
the one for DIR, else the most recently used window.  With none on screen,
fall back to DIR's sessions, then to any session at all."
  (let* ((visible (ewen/claude--visible-sessions))
         (here (ewen/claude-buffers-for dir)))
    (or (and visible (or (car (seq-intersection visible here)) (car visible)))
        (ewen/claude--pick here)
        (ewen/claude--pick (ewen/claude-sessions))
        (ewen/claude--start dir))))

(defun ewen/claude--file ()
  "The file this buffer visits, following an indirect buffer to its base."
  (or buffer-file-name
      (and-let* ((base (buffer-base-buffer)))
        (buffer-file-name base))))

(defun ewen/claude--targets ()
  "Paths to reference: the Dired selection, or the file this buffer visits.
In Dired with nothing marked this is the entry at point, and on a line that
names no entry it is the directory being listed."
  (or (and (derived-mode-p 'dired-mode)
           (or (ignore-errors (dired-get-marked-files))
               (list default-directory)))
      (and-let* ((f (ewen/claude--file))) (list f))
      (user-error "Nothing here to reference")))

(defun ewen/claude--project-dir (fallback)
  "This buffer's project root, or the directory holding FALLBACK."
  (or (ignore-errors (project-root (project-current)))
      (file-name-directory (directory-file-name fallback))))

(defun ewen/claude--relative (path root)
  "PATH relative to ROOT, or absolute when it lies outside ROOT.
The chosen session may belong to another project, and a \"..\" path out of
its working directory is not something Claude can resolve."
  (let* ((abs (directory-file-name (expand-file-name path)))
         (rel (if (file-equal-p abs root) "." (file-relative-name abs root))))
    (if (string-prefix-p ".." rel) abs rel)))

(defun ewen/claude--ref (path root)
  "PATH as an @-reference relative to ROOT, directories keeping a trailing /."
  (concat "@" (ewen/claude--relative path root)
          (and (file-directory-p path) "/")))

(defun ewen/claude--fence-language ()
  "Markdown info string for the current major mode, or the empty string."
  (let ((name (replace-regexp-in-string "\\(-ts\\)?-mode\\'" ""
                                        (symbol-name major-mode))))
    (pcase name
      ((or "emacs-lisp" "lisp-interaction") "elisp")
      ("sh" "bash")
      ("fundamental" "")
      (_ name))))

(defun ewen/claude--fence (text)
  "A backtick fence one longer than the longest run of backticks in TEXT."
  (let ((longest 0) (pos 0))
    (while (string-match "`+" text pos)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            pos (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(defun ewen/claude-reference-file (&optional submit)
  "Insert @-references to this buffer's file, or the Dired selection, into a
Claude session.  Directories are referenced as such.
With a prefix argument SUBMIT, also send RET.  Paths are relative to the
session's own working directory, not to this buffer's project."
  (interactive "P")
  (let* ((targets (ewen/claude--targets))
         (buf (ewen/claude--session (ewen/claude--project-dir (car targets))))
         (root (buffer-local-value 'default-directory buf))
         (refs (mapconcat (lambda (f) (ewen/claude--ref f root)) targets " ")))
    ;; Bracketed paste, not `ghostel-send-string': typing "@" character by
    ;; character opens Claude's file picker, which then eats the path.
    (with-current-buffer buf
      (ghostel-paste-string (concat refs " "))
      (when submit (ghostel-send-key "return")))
    (message "%s -> %s" refs (buffer-name buf))))

(defun ewen/claude-send-region (beg end &optional submit)
  "Send the region BEG..END to a Claude session as a fenced quote.
The block is headed with \"path:first-last\" so Claude can locate the lines.
With a prefix argument SUBMIT, also send RET."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (user-error "No active region")))
  (let* ((file (ewen/claude--file))
         (buf (ewen/claude--session
               (ewen/claude--project-dir (or file default-directory))))
         (root (buffer-local-value 'default-directory buf))
         (text (buffer-substring-no-properties beg end))
         ;; A line-wise selection ends at the start of the line after the last
         ;; one it shows; naming that line would be off by one.
         (last (line-number-at-pos (if (and (> end beg)
                                            (= end (save-excursion
                                                     (goto-char end)
                                                     (line-beginning-position))))
                                       (1- end)
                                     end)))
         (header (format "%s:%d-%d"
                         (if file (ewen/claude--relative file root) (buffer-name))
                         (line-number-at-pos beg) last))
         (fence (ewen/claude--fence text))
         (block (format "%s\n%s%s\n%s\n%s\n"
                        header fence (ewen/claude--fence-language)
                        (replace-regexp-in-string "\n\\'" "" text)
                        fence)))
    ;; Bracketed paste is required, not merely safer: a typed newline would
    ;; submit the prompt instead of continuing the block.
    (with-current-buffer buf
      (ghostel-paste-string block)
      (when submit (ghostel-send-key "return")))
    (message "%s -> %s" header (buffer-name buf))))

(global-set-key (kbd "C-c @") #'ewen/claude-reference-file)
;; Both spellings: a terminal delivers C-SPC as C-@, a GUI frame as its own
;; event, and `kbd' does not fold the two together.
(global-set-key (kbd "C-c C-@") #'ewen/claude-send-region)
(global-set-key (kbd "C-c C-SPC") #'ewen/claude-send-region)

(add-hook 'after-init-hook 'ewen-after-init-hook)

(defun ewen-after-init-hook ()
  (add-to-list 'load-path "~/clj-data.el")
  ;;(add-to-list 'load-path "~/replique.el")
  (add-to-list 'load-path "~/replique.el-2")
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
    (magit-ediff-dwim-show-on-hunks t)
    :bind (:map magit-mode-map
                ("C-c C-k" . ewen/magit-kill-buffers)))
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
    ;; The libghostty probe runs on every redraw; it was 5.7% of Emacs CPU.
    (ghostel-detect-password-prompts nil)
    ;; 20fps rather than 30: every redraw is a redisplay, and that allocation
    ;; churn fragments the malloc zone over a long session (emacs bug#79023).
    (ghostel-timer-delay 0.05)
    :config
    ;; The upstream default plus the window-navigation keys: semi-char mode
    ;; sends everything else straight to the terminal.  `setopt' rather than
    ;; `setq' because this defcustom's :set rebuilds the keymap -- and it lives
    ;; here rather than in :custom because that :set loads ghostel, which would
    ;; defeat :defer and warn about the module on every startup.
    (setopt ghostel-keymap-exceptions
            '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
              "C-S-<left>" "C-S-<right>" "C-S-<up>" "C-S-<down>"))
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
    :config
    ;; Status buffers are named "magit: repo", the rest "magit-<mode>: repo".
    (add-to-list 'ivy-ignore-buffers "\\`magit-")
    (ivy-mode 1))

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

  ;; Catppuccin latte paints both mode lines in near-identical greys, so the
  ;; boundary between stacked windows disappears. Darken the active one and
  ;; rule both top and bottom.
  (let ((rule (catppuccin-color 'overlay0)))
    (set-face-attribute 'mode-line nil
                        :background (catppuccin-color 'surface1)
                        :foreground (catppuccin-color 'text)
                        :overline rule :underline rule :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :background (catppuccin-color 'mantle)
                        :foreground (catppuccin-color 'subtext0)
                        :overline rule :underline rule :box nil))
  ;; Keep the underline on the mode line's last pixel row instead of eating
  ;; into the text.
  (setq x-underline-at-descent-line t)

  ;; Side-by-side splits are separated by a 1px vertical border, not by a mode
  ;; line. Replace it with a wider, darker window divider.
  (setq window-divider-default-places 'right-only)
  (setq window-divider-default-right-width 3)
  (window-divider-mode 1)
  (let ((rule (catppuccin-color 'overlay0)))
    (set-face-attribute 'window-divider nil :foreground rule)
    (set-face-attribute 'window-divider-first-pixel nil :foreground rule)
    (set-face-attribute 'window-divider-last-pixel nil :foreground rule))

  )

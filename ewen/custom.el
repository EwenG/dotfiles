;;; custom.el ---   -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(danish/level-description "en dansklærende på niveau cirka C1")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(package-selected-packages
   '(catppuccin-theme clang-format clj-data clojure-mode clojure-ts-mode code-cells
                      company counsel dape diff-hl editorconfig elisp-slime-nav
                      exec-path-from-shell ghostel gif-screencast gnuplot
                      graphql-mode ivy js2-mode jupyter less-css-mode magit
                      markdown-mode multiple-cursors nix-mode plz replique
                      sass-mode smartparens stylus-mode vdiff vlf))
 '(safe-local-variable-values
   '((replique-css-outputs "dev-resources/public/main/css/main.css")
     (replique-css-entry . "scss/main.scss")
     (replique-aliases "replique-clj" "jvm-opts" "dev" "cljs" "i18n" "kourou"
                       "test" "cljs-fork")
     (replique-aliases "replique-clj" "jvm-opts" "dev" "cljs" "i18n" "kourou"
                       "test")
     (replique-clojure-source-roots "src")
     (eval add-hook 'org-property-allowed-value-functions
           (lambda (p)
             (when (equal p "BRANCH")
               (append
                (ignore-errors
                  (let
                      ((default-directory
                        (or (locate-dominating-file default-directory ".git")
                            default-directory)))
                    (process-lines "git" "branch" "--format=%(refname:short)")))
                '(:ETC))))
           nil t)
     (eval add-hook 'org-after-todo-state-change-hook
           (lambda nil
             (when (member org-state org-done-keywords)
               (org-entry-delete (point) "WORKTREE")))
           nil t)
     (eval put 'defnc 'clojure-doc-string-elt 2)
     (cider-shadow-default-options . "main")
     (cider-default-cljs-repl . shadow-select)
     (cider-clojure-cli-global-options . "-A:dev"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-bracket-face ((t (:foreground "#555555"))))
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face))))
 '(font-lock-operator-face ((t (:foreground "#2a7bde" :weight bold))))
 '(font-lock-property-use-face ((t (:foreground "#657b83" :slant italic))))
 '(font-lock-punctuation-face ((t (:foreground "#555555"))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face)))))

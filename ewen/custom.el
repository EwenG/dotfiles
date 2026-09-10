(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(danish/level-description "en dansklærende på niveau cirka C1")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(package-selected-packages
   '(catppuccin-theme clang-format clj-data clojure-mode clojure-ts-mode
                  company counsel dape doom-themes editorconfig
                  ef-light-theme elisp-slime-nav exec-path-from-shell
                  gif-screencast gnuplot graphql-mode ivy js2-mode
                  less-css-mode magit markdown-mode
                  multiple-cursors nix-mode plz projectile replique
                  sass-mode smartparens standard-themes stylus-mode vdiff
                  vlf))
 '(safe-local-variable-values
   '((replique-clojure-source-roots "src")
     (eval defun org-dblock-write:nosco-status (params)
           (let*
               ((default-directory
                 (or (locate-dominating-file default-directory ".git")
                     default-directory))
                (wt (make-hash-table :test 'equal)) (path nil) (rows nil))
             (dolist
                 (l
                  (ignore-errors
                    (process-lines "git" "worktree" "list" "--porcelain")))
               (cond
                ((string-prefix-p "worktree " l)
                 (setq path (file-name-nondirectory (substring l 9))))
                ((string-prefix-p "branch refs/heads/" l)
                 (puthash (substring l 18) path wt))))
             (save-excursion
               (org-map-entries
                (lambda nil
                  (when (member (org-get-todo-state) '("DOING" "REVIEW"))
                    (let*
                        ((branch (org-entry-get (point) "BRANCH"))
                         (wtname (and branch (gethash branch wt)))
                         (pr (org-entry-get (point) "PR"))
                         (card (org-entry-get (point) "CARD" t)))
                      (push
                       (list (org-get-todo-state)
                             (org-link-display-format (org-get-heading t t t t))
                             (or branch "") (or wtname "") (or pr "")
                             (or card ""))
                       rows))))))
             (insert "| State | Task | Branch | Worktree | PR | Card |\12|-\12")
             (dolist (r (nreverse rows))
               (insert (apply #'format "| %s | %s | %s | %s | %s | %s |\12" r)))
             (org-table-align)))
     (eval defun org-dblock-write:nosco-status (params)
           (let*
               ((default-directory
                 (or (locate-dominating-file default-directory ".git")
                     default-directory))
                (wt (make-hash-table :test 'equal)) (path nil) (rows nil))
             (dolist
                 (l
                  (ignore-errors
                    (process-lines "git" "worktree" "list" "--porcelain")))
               (cond
                ((string-prefix-p "worktree " l)
                 (setq path (file-name-nondirectory (substring l 9))))
                ((string-prefix-p "branch refs/heads/" l)
                 (puthash (substring l 18) path wt))))
             (save-excursion
               (org-map-entries
                (lambda nil
                  (when (org-get-todo-state)
                    (let*
                        ((branch (org-entry-get (point) "BRANCH"))
                         (wtname (and branch (gethash branch wt)))
                         (pr (org-entry-get (point) "PR"))
                         (card (org-entry-get (point) "CARD" t)))
                      (push
                       (list (org-get-todo-state)
                             (org-link-display-format (org-get-heading t t t t))
                             (or branch "") (or wtname "") (or pr "")
                             (or card ""))
                       rows))))))
             (insert "| State | Task | Branch | Worktree | PR | Card |\12|-\12")
             (dolist (r (nreverse rows))
               (insert (apply #'format "| %s | %s | %s | %s | %s | %s |\12" r)))
             (org-table-align)))
     (eval defun org-dblock-write:nosco-status (params)
           (let*
               ((default-directory
                 (or (locate-dominating-file default-directory ".git")
                     default-directory))
                (wt (make-hash-table :test 'equal))
                (rev (make-hash-table :test 'equal))
                (prs (make-hash-table :test 'equal)) (path nil) (rows nil))
             (dolist
                 (l
                  (ignore-errors
                    (process-lines "git" "worktree" "list" "--porcelain")))
               (cond
                ((string-prefix-p "worktree " l)
                 (setq path (file-name-nondirectory (substring l 9))))
                ((string-prefix-p "branch refs/heads/" l)
                 (let ((b (substring l 18)))
                   (puthash b path wt) (puthash path b rev)))))
             (dolist
                 (l
                  (ignore-errors
                    (process-lines "gh" "pr" "list" "--state" "all" "--limit"
                                   "200" "--json" "headRefName,number,state"
                                   "--jq"
                                   ".[] | \"\\(.headRefName)\\t#\\(.number) \\(.state)\"")))
               (let ((p (split-string l "\11")))
                 (when (cadr p) (puthash (car p) (cadr p) prs))))
             (save-excursion
               (org-map-entries
                (lambda nil
                  (when (org-get-todo-state)
                    (let*
                        ((bp (org-entry-get (point) "BRANCH"))
                         (wp (org-entry-get (point) "WORKTREE"))
                         (branch (or bp (and wp (gethash wp rev))))
                         (wtname (or (and branch (gethash branch wt)) wp))
                         (pr (and branch (gethash branch prs))))
                      (push
                       (list (org-get-todo-state)
                             (org-link-display-format (org-get-heading t t t t))
                             (or branch "") (or wtname "") (or pr ""))
                       rows))))))
             (insert "| State | Task | Branch | Worktree | PR |\12|-\12")
             (dolist (r (nreverse rows))
               (insert (apply #'format "| %s | %s | %s | %s | %s |\12" r)))
             (org-table-align)))
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

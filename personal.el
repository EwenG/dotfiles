(require 'package)
(require 'helm-mode)

(add-to-list 'load-path "~/replique.el/")
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

; Scroll margin
(setq scroll-margin 6)

; Prettify symbol
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  (--filter (equal "lambda" it) prettify-symbols-alist))
            (push '("-lambda" . ?λ) prettify-symbols-alist)
            (prettify-symbols-mode 1)))

; Org indent mode
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (define-key smartparens-mode-map (kbd "M-<up>") nil)
            (define-key smartparens-mode-map (kbd "M-<down>") nil)
            (define-key prelude-mode-map (kbd "M-S-<up>") nil)
            (define-key prelude-mode-map (kbd "M-S-<down>") nil)
            (define-key prelude-mode-map (kbd "C-c TAB") nil)
            (define-key prelude-mode-map (kbd "C-c o") nil))
          t)

;; Org trello
(unless (package-installed-p 'org-trello)
  (package-refresh-contents)
  (package-install 'org-trello))
(custom-set-variables '(org-trello-files '("/home/egr/Documents/test-trello.org")))

; Ace jump
(key-chord-define-global "jj" nil)
(key-chord-define-global "jk" nil)
(key-chord-define-global "JJ" nil)

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


;;Ediff - split vertically by default
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

;;Install s
(unless (package-installed-p 's)
  (package-refresh-contents)
  (package-install 's))

;;Install edn
(unless (package-installed-p 'edn)
  (package-refresh-contents)
  (package-install 'edn))

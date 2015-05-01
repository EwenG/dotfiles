(require 'package)

;;Install dash
(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash))
(require 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))


(setq scroll-margin 6)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  (--filter (equal "lambda" it) prettify-symbols-alist))
            (push '("-lambda" . ?λ) prettify-symbols-alist)
            (prettify-symbols-mode 1)))

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









;; Install inf-clojure
;; (unless (package-installed-p 'inf-clojure)
;;   (package-refresh-contents)
;;   (package-install 'inf-clojure))

;;Enable inf clojure for clojure source buffers
;;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)



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

;;Install s
(unless (package-installed-p 's)
  (package-refresh-contents)
  (package-install 's))

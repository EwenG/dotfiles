(setq scroll-margin 6)


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
(define-key prelude-mode-map (kbd "C-c s") nil)
(define-key search-map (kbd "C-c s") 'isearch-forward-symbol-at-point)


;;Ediff - split vertically by default
(custom-set-variables '(ediff-split-window-function 'split-window-horizontally))









;; Install inf-clojure
;; (require 'package)
;; (unless (package-installed-p 'inf-clojure)
;;   (package-refresh-contents)
;;   (package-install 'inf-clojure))

;;Enable inf clojure for clojure source buffers
;;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)



;;Install smartparens
(require 'package)
(unless (package-installed-p 'smartparens)
  (package-refresh-contents)
  (package-install 'smartparens))
(require 'smartparens)
(smartparens-global-mode 1)

;;Install clojure-mode
(require 'package)
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
(require 'clojure-mode)

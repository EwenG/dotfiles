;; C-up and C-down to slowly scroll the buffer
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)] 'gcm-scroll-up)



;; C-c s Swap buffer
(defun swap-buffer ()
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(global-set-key [remap prelude-swap-windows] 'swap-buffer)





;;Ediff - split vertically by default
(custom-set-variables '(ediff-split-window-function 'split-window-horizontally))




;; Install and use sbt-mode
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'sbt-mode)
  (package-refresh-contents) (package-install 'sbt-mode))



;; Install and use ENSIME
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless (package-installed-p 'ensime)
  (package-refresh-contents) (package-install 'ensime))


(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

                                        ;
(define-key ensime-search-mode-map (kbd "RET") 'ensime-search-choose-current-result)


;; Install inf-clojure
;; (require 'package)
;; (unless (package-installed-p 'inf-clojure)
;;   (package-refresh-contents)
;;   (package-install 'inf-clojure))

;;Enable inf clojure for clojure source buffers
;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; Yascroll bar : Scroll bar mode for emacs
(require 'package)
(unless (package-installed-p 'yascroll)
  (package-refresh-contents)
  (package-install 'yascroll))
(require 'yascroll)

(global-yascroll-bar-mode 1)
(custom-set-variables '(yascroll:delay-to-hide nil))

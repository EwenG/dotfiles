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









;; Install inf-clojure
;; (require 'package)
;; (unless (package-installed-p 'inf-clojure)
;;   (package-refresh-contents)
;;   (package-install 'inf-clojure))

;;Enable inf clojure for clojure source buffers
;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

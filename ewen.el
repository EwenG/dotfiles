;;; ewen.el --- elisp utils  -*- lexical-binding: t; -*-

(require 'dash)

(defun ewen/find-elisp-thing-candidates ()
  (all-completions "" obarray))

(defun ewen/find-elisp-thing-init ()
  (-let ((data (ewen/find-elisp-thing-candidates)))
    (helm-init-candidates-in-buffer 'global data)))

(defun ewen/go-to-definition (candidate)
  (-let ((candidate-sym (intern-soft candidate obarray)))
    (cond ((fboundp candidate-sym)
           (find-function candidate-sym))
          ((boundp candidate-sym)
           (find-variable candidate-sym))
          ((facep candidate-sym)
           (find-face-definition candidate-sym))
          (t (find-library candidate)))))

(defvar ewen/elisp-thing-actions
  `(("go-to-definition" ewen/go-to-definition)))

(defvar ewen/source-elisp-thing
  `((name . "elisp-thing")
    (init . ewen/find-elisp-thing-init)
    (candidates . ewen/find-elisp-thing-candidates)
    (filter-one-by-one
     . (lambda (candidate)
         (-let ((candidate-str (substring candidate 0))
                (candidate-sym (intern-soft candidate obarray)))
           (cond ((fboundp candidate-sym)
                  (add-face-text-property 0 (length candidate-str)
                                          '(:foreground "#93E0E3")
                                          nil
                                          candidate-str))
                 ((boundp candidate-sym)
                  (add-face-text-property 0 (length candidate-str)
                                          '(:foreground "#DFAF8F")
                                          nil
                                          candidate-str))
                 ((facep candidate-sym)
                  (add-face-text-property 0 (length candidate-str)
                                          '(:foreground "#DFAF8F")
                                          nil
                                          candidate-str)))
           candidate-str)))
    (action . ,ewen/elisp-thing-actions)
    (volatile . t)))

;;;###autoload
(defun ewen/find-elisp-thing ()
  (interactive)
  (helm :sources 'ewen/source-elisp-thing
        :buffer "*Find elisp thing*"))

(provide 'ewen)

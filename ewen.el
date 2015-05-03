;;; ewen.el --- elisp utils  -*- lexical-binding: t; -*-

(require 'dash)

(defun ewen/find-elisp-thing-candidates ()
  (append obarray nil))

(defun ewen/find-elisp-thing-init ()
  (-let ((data (ewen/find-elisp-thing-candidates)))
    (helm-init-candidates-in-buffer 'global data)))

(defvar ewen/source-elisp-thing
  `((name . "elisp-thing")
    (init . ewen/find-elisp-thing-init)
    (candidates . ewen/find-elisp-thing-candidates)
    (volatile . t)))

;;;###autoload
(defun ewen/find-elisp-thing ()
  (interactive)
  (helm :sources 'ewen/source-elisp-thing
        :buffer "*Find elisp thing*"))

(provide 'ewen)

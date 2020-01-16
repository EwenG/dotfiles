;;; ewen.el --- elisp utils  -*- lexical-binding: t; -*-

;; Function to call ag programmatically
(defun ewen/ag (&optional ag-args)
  (let ((initial-directory (read-directory-name
                            (concat
                             (car (split-string counsel-ag-base-command))
                             " in directory: "))))
    (counsel-ag nil initial-directory ag-args nil)))

(defun ewen/file-jump (&optional extra-ag-args)
  "Use ag to find files (using the ag \"-g\" flag)"
  (interactive)
  (interactive)
  (let ((d (read-directory-name (concat
                                 (car (split-string counsel-ag-base-command))
                                 " in directory: "))))
    (counsel-file-jump nil d)))

(defun ewen/revert-all ()
  "Revert all file buffers, without confirmation.
Buffers visiting files that no longer exist are ignored.
Files that are not readable (including do not exist) are ignored.
Other errors while reverting a buffer are reported only as messages."
  (interactive)
  (let (file)
    (dolist (buf  (buffer-list))
      (setq file  (buffer-file-name buf))
      (when (and file  (file-readable-p file))
        (with-current-buffer buf
          (with-demoted-errors "Error: %S" (revert-buffer t t)))))))

(provide 'ewen)

;;; ewen.el --- elisp utils  -*- lexical-binding: t; -*-

;; ag custom functions

;; Function to call ag programmatically
(defun ewen/ag (&optional ag-args)
  (let ((initial-directory (read-directory-name
                            (concat
                             (car (split-string counsel-ag-base-command))
                             " in directory: "))))
    (counsel-ag nil initial-directory ag-args nil)))

;; Use ag to search files
(defun ewen/ag-file-function (string extra-ag-args)
  "Grep in the current directory for STRING.
If non-nil, EXTRA-AG-ARGS string is appended to `counsel-ag-base-command'."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 2)
      (counsel-more-chars 2)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let ((ag-cmd (format counsel-ag-base-command
                            (concat extra-ag-args
                                    " -g "
                                    (shell-quote-argument regex)))))
        (counsel--async-command ag-cmd))
      nil)))

(defun ewen/ag-file (&optional extra-ag-args)
  "Use ag to find files (using the ag \"-g\" flag)"
  (let ((initial-directory (read-directory-name
                            (concat
                             (car (split-string counsel-ag-base-command))
                             " in directory: "))))
    (ivy-set-prompt 'counsel-ag counsel-prompt-function)
    (setq counsel--git-grep-dir (or initial-directory default-directory))
    (ivy-read (car (split-string counsel-ag-base-command))
              (lambda (string)
                (ewen/ag-file-function string extra-ag-args))
              :initial-input nil
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-ag)))

(provide 'ewen)

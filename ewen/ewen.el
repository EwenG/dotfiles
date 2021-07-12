;;; ewen.el --- elisp utils  -*- lexical-binding: t; -*-

;; Function to call ag programmatically
(defun ewen/ag (&optional ag-args)
  (let ((initial-directory (read-directory-name
                            (concat
                             (car counsel-ag-base-command)
                             " in directory: "))))
    (counsel-ag nil initial-directory ag-args nil)))

(defun ewen/file-jump (&optional extra-ag-args)
  "Use ag to find files (using the ag \"-g\" flag)"
  (interactive)
  (interactive)
  (let ((d (read-directory-name (concat
                                 (car counsel-ag-base-command)
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

(defun ewen/pid->process-buffer-name (pid)
  (let ((process-name (thread-last (string-to-number pid)
                        process-attributes
                        (assoc 'comm)
                        cdr)))
    (format "*%s*" process-name)))

(defun ewen/rename-buffer-with-process ()
  "Try to find a good name for this buffer, based on the names of the processes associated with this buffer."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-name (process-name proc))
         (pid (process-id proc))
         (pids (with-temp-buffer
                 (insert (shell-command-to-string (format "pstree -p %d -g 3" pid)))
                 (goto-char (point-min))
                 (let ((pids '())
                       (indent-max 0)
                       (stop nil))
                   (save-match-data
                     (while (null stop)
                       (goto-char (line-beginning-position))
                       (when (re-search-forward "[0-9]+" (line-end-position) t)
                         (let ((match-b (match-beginning 0))
                               (match-e (match-end 0)))
                           (cond ((> match-b indent-max)
                                  (setq indent-max match-b)
                                  (setq pids (list (buffer-substring-no-properties match-b match-e))))
                                 ((= match-b indent-max)
                                  (push (buffer-substring-no-properties match-b match-e) pids))
                                 (t nil))))
                       (when (not (= (forward-line 1) 0))
                         (setq stop t)))
                     pids)))))
    (when (equal pids '())
      (user-error "No process found"))
    (let* ((buffer-names-candidates (mapcar 'ewen/pid->process-buffer-name pids))
           (buffer-name (completing-read "Rename buffer to: "
                                         buffer-names-candidates nil t nil nil
                                         (car buffer-names-candidates))))
      (rename-buffer buffer-name))))

(setq load-path (append (list (expand-file-name "~/lilypond")) load-path))

(provide 'ewen)

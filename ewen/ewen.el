;;; ewen.el --- elisp utils  -*- lexical-binding: t; -*-

(require 'subr-x)

(defmacro comment (&rest _body)
  "Ignore BODY, like Clojure\\='s `comment'.
For forms kept in a file to be run by hand: unlike a commented-out block,
`C-x C-e' and `C-M-x' still reach the forms inside."
  (declare (indent 0) (debug (&rest sexp)))
  nil)

(defun ewen/pid->process-buffer-name (pid)
  (let ((process-name (thread-last (string-to-number pid)
                        process-attributes
                        (assoc 'comm)
                        cdr)))
    (format "*%s*" process-name)))

(defun ewen/rename-buffer-with-process ()
  "Rename this buffer after one of the processes running in it.
Candidates are the most deeply nested processes in the process tree
of this buffer's process, as reported by pstree(1)."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
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

(provide 'ewen)

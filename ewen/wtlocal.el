;;; wtlocal.el --- personal dev files shared by a repo's worktrees  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal dev files kept outside a repository and symlinked into every one of
;; its worktrees.  A wtlocal directory holds `files/' -- which is the list, so
;; adding a file means putting it there -- a `repo' naming the repository it
;; manages, and the `sync.sh' that reconciles that repository's worktrees.
;;
;; Put those directories in `wtlocal-dirs' and turn on `wtlocal-mode'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup wtlocal nil
  "Personal dev files shared by a repository's worktrees."
  :group 'tools)

(defcustom wtlocal-dirs nil
  "Directories holding personal dev files and their `sync.sh'.
Each manages the worktrees of the repository named in its `repo' file."
  :type '(repeat directory))

(defun wtlocal--git-dir (dir)
  "Return the git directory for DIR, or nil when DIR is not in a repo.
For a linked worktree this is .git/worktrees/<name>, not the shared .git.
Reads the filesystem directly instead of shelling out.  In a linked worktree
.git is a file holding \"gitdir: <path>\" rather than a directory."
  (when-let* ((root (locate-dominating-file (or dir default-directory) ".git"))
              (dotgit (expand-file-name ".git" root)))
    (if (file-directory-p dotgit)
        dotgit
      (with-temp-buffer
        (insert-file-contents dotgit)
        (and (looking-at "gitdir: \\(.*\\)")
             (match-string 1))))))

(defun wtlocal--git-common-dir (dir)
  "The git directory DIR's repo shares with its worktrees, or nil.
`wtlocal--git-dir' gives the per-worktree one, which holds only HEAD, index
and refs; info/exclude and hooks live in the shared directory it names in
commondir."
  (when-let* ((gitdir (wtlocal--git-dir dir)))
    (let ((commondir (expand-file-name "commondir" gitdir)))
      (if (file-readable-p commondir)
          (with-temp-buffer
            (insert-file-contents commondir)
            (expand-file-name (string-trim (buffer-string)) gitdir))
        gitdir))))

(defun wtlocal--repo (dir)
  "The repository DIR manages, per its `repo' file."
  (let ((f (expand-file-name "repo" dir)))
    (when (file-readable-p f)
      (string-trim (with-temp-buffer (insert-file-contents f) (buffer-string))))))

(defun wtlocal--marker (dir)
  "Name sync.sh gives its block in info/exclude: DIR\='s own name."
  (file-name-nondirectory (directory-file-name dir)))

(defun wtlocal-for (dir)
  "The wtlocal directory managing DIR\='s repository, or nil.
Matched on the common git directory, so every worktree of that repository
finds it -- not only the one its `repo' file happens to name."
  (and-let* ((common (wtlocal--git-common-dir dir)))
    (seq-find (lambda (d)
                (and-let* ((repo (wtlocal--repo d))
                           (repo-common (wtlocal--git-common-dir repo)))
                  (file-equal-p repo-common common)))
              wtlocal-dirs)))

(defun wtlocal--current ()
  "The wtlocal directory managing the current buffer\='s repository."
  (or (wtlocal-for default-directory)
      (user-error "No wtlocal directory manages %s"
                  (abbreviate-file-name default-directory))))

(defun wtlocal--run (dir &rest args)
  "Run DIR\='s sync.sh with ARGS into a buffer, and return that buffer."
  (let ((buf (get-buffer-create (format "*%s*" (wtlocal--marker dir)))))
    (with-current-buffer buf
      ;; `special-mode' from a previous run leaves the buffer read-only, which
      ;; blocks `call-process' writing into it, not just `erase-buffer'.
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process (expand-file-name "sync.sh" dir) nil buf nil args))
      (special-mode))
    buf))

;;;###autoload
(defun wtlocal-status ()
  "Report what syncing would change, without changing anything."
  (interactive)
  (let* ((dir (wtlocal--current))
         (buf (wtlocal--run dir "--status")))
    (if (zerop (buffer-size buf))
        (message "%s: in sync" (wtlocal--marker dir))
      (display-buffer buf))))

;;;###autoload
(defun wtlocal-sync (&optional deep)
  "Reconcile every worktree with the files in this repository\='s wtlocal dir.
With a prefix argument use the deep scan, which also finds a dangling link left
by deleting the last managed file in a directory."
  (interactive "P")
  (let* ((dir (wtlocal--current))
         (buf (if deep (wtlocal--run dir "--deep") (wtlocal--run dir))))
    (if (zerop (buffer-size buf))
        (message "%s: already in sync" (wtlocal--marker dir))
      (display-buffer buf))))

(defun wtlocal--sync-on-save ()
  "Link a file saved under a wtlocal directory that no worktree has yet.
Runs only when the path is genuinely new -- saving an existing managed file
costs nothing -- and asynchronously, since a sync takes a second or two."
  (when-let* ((f (buffer-file-name))
              (dir (seq-find (lambda (d)
                               (string-prefix-p
                                (file-name-as-directory
                                 (expand-file-name "files" d))
                                f))
                             wtlocal-dirs))
              (root (file-name-as-directory (expand-file-name "files" dir)))
              (repo (wtlocal--repo dir))
              (rel (substring f (length root)))
              ((not (file-exists-p (expand-file-name rel repo)))))
    (start-process "wtlocal-sync" nil (expand-file-name "sync.sh" dir))
    (message "%s: linking %s into the worktrees..."
             (wtlocal--marker dir) rel)))

;;;###autoload
(define-minor-mode wtlocal-mode
  "Keep the worktrees of `wtlocal-dirs' repositories stocked.
Saving a new file under a wtlocal directory links it into every worktree, and
project.el lists the links that git has been told to ignore."
  :global t
  (if wtlocal-mode
      (add-hook 'after-save-hook #'wtlocal--sync-on-save)
    (remove-hook 'after-save-hook #'wtlocal--sync-on-save)))

(defun wtlocal-files (root)
  "Paths in ROOT's info/exclude wtlocal block.
These are symlinks into the wtlocal directory; git ignores them, so the vc
backend of project.el never lists them."
  (let* ((dir (wtlocal-for root))
         (marker (and dir (wtlocal--marker dir)))
         (exclude (and dir
                       (and-let* ((gitdir (wtlocal--git-common-dir root)))
                         (expand-file-name "info/exclude" gitdir))))
         files)
    (when (and exclude (file-readable-p exclude))
      (with-temp-buffer
        (insert-file-contents exclude)
        (goto-char (point-min))
        (when (re-search-forward (concat "^# >>> " (regexp-quote marker) "$") nil t)
          (forward-line 1)
          (while (not (or (eobp)
                          (looking-at (concat "^# <<< " (regexp-quote marker) "$"))))
            (let ((path (string-trim (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
              (when (and (not (string-empty-p path))
                         (not (string-prefix-p "#" path))
                         (file-exists-p (expand-file-name path root)))
                (push path files)))
            (forward-line 1)))))
    (nreverse files)))

(declare-function project-root "project" (project))
(defvar project-files-relative-names)

(with-eval-after-load 'project
  (cl-defmethod project-files :around ((project (head vc)) &optional dirs)
    (let ((root (project-root project))
          (files (cl-call-next-method)))
      ;; Only for the root listing; `dirs' is a subdirectory when called from
      ;; e.g. `project-find-regexp', where root-relative paths make no sense.
      (if (and wtlocal-mode
               (or (null dirs) (seq-some (lambda (d) (file-equal-p d root)) dirs)))
          (append files
                  (mapcar (lambda (f)
                            (if project-files-relative-names
                                f
                              (expand-file-name f root)))
                          (wtlocal-files root)))
        files))))

(provide 'wtlocal)
;;; wtlocal.el ends here

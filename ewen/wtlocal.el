;;; wtlocal.el --- personal dev files shared by a repo's worktrees  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal dev files kept outside a repository and symlinked into every one of
;; its worktrees.  A wtlocal directory holds `files/' -- which is the list, so
;; adding a file means putting it there -- a `repo' naming the repository it
;; manages, and the `sync.sh' that reconciles that repository's worktrees.
;;
;; A wtlocal directory may also name a `stage': a directory that is not a
;; worktree, holding the classpath roots of the project for real and everything
;; inside them as links into ONE of the worktrees.  `wtlocal-point' moves those
;; links.  It is there because a JVM resolves each classpath directory once,
;; when it starts, so a repl cannot be made to follow a link over a worktree -
;; but a link BELOW a classpath root it follows on every lookup.  So one repl
;; reads whichever worktree the stage points at, and reads another one without
;; being restarted.  `point.sh' in the wtlocal directory is what moves them.
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

(defun wtlocal--setting (dir name)
  "The one line the file NAME in DIR holds, or nil where it holds none.
An absent file and an empty one are the same answer: a wtlocal directory
says what it manages by naming it, and a setting it does not name is one
the commands that need it decline rather than guess at."
  (let ((f (expand-file-name name dir)))
    (when (file-readable-p f)
      (let ((said (string-trim (with-temp-buffer (insert-file-contents f)
                                                 (buffer-string)))))
        (unless (string-empty-p said) said)))))

(defun wtlocal--repo (dir)
  "The repository DIR manages, per its `repo' file."
  (wtlocal--setting dir "repo"))

(defun wtlocal-stage (dir)
  "The repl stage DIR moves, per its `stage' file, or nil where it has none.
Not every project has one - see the commentary - so this answering nil is
an ordinary thing rather than something missing."
  (wtlocal--setting dir "stage"))

(defun wtlocal-stage-target (stage)
  "The worktree STAGE points at, per the marker point.sh writes, or nil.
Read rather than asked of point.sh, which prints the same line when run
with no argument: this is on the way to every answer about what the
current project is, and a subprocess per call buys nothing."
  (and stage (wtlocal--setting stage ".pointed-at")))

(defun wtlocal-stage-holding (file)
  "The stage of `wtlocal-dirs' that FILE is under, or nil."
  (let ((file (expand-file-name file)))
    (seq-some (lambda (dir)
                (and-let* ((stage (wtlocal-stage dir))
                           ((string-prefix-p
                             (file-name-as-directory (expand-file-name stage))
                             file)))
                  stage))
              wtlocal-dirs)))

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

(defvar-local wtlocal--status nil
  "What the script `wtlocal--run' last ran in this buffer exited with.
Kept beside its output rather than returned with it, because what a caller
wants is nearly always the output - only a command with something to do
afterwards has to know whether the script got that far.")

(defun wtlocal--run (dir script &rest args)
  "Run SCRIPT of DIR with ARGS into a buffer, and return that buffer.
The buffer holds what it printed and `wtlocal--status' what it exited with."
  (let ((buf (get-buffer-create (format "*%s*" (wtlocal--marker dir)))))
    (with-current-buffer buf
      ;; `special-mode' from a previous run leaves the buffer read-only, which
      ;; blocks `call-process' writing into it, not just `erase-buffer'.
      ;; The buffer keeps the `default-directory' of whatever buffer first
      ;; created it, which can be a worktree deleted since; `call-process' runs
      ;; there and fails before the script starts.
      (setq default-directory (file-name-as-directory dir))
      (let ((inhibit-read-only t)
            (status nil))
        (erase-buffer)
        (setq status (apply #'call-process (expand-file-name script dir)
                            nil buf nil args))
        ;; After the mode and not before: a major mode starts by killing the
        ;; buffer's local variables, and this one with them
        (special-mode)
        (setq wtlocal--status status)))
    buf))

;;;###autoload
(defun wtlocal-status ()
  "Report what syncing would change, without changing anything."
  (interactive)
  (let* ((dir (wtlocal--current))
         (buf (wtlocal--run dir "sync.sh" "--status")))
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
         (buf (if deep
                  (wtlocal--run dir "sync.sh" "--deep")
                (wtlocal--run dir "sync.sh"))))
    (if (zerop (buffer-size buf))
        (message "%s: already in sync" (wtlocal--marker dir))
      (display-buffer buf))))

(defun wtlocal-staged ()
  "The wtlocal directory whose repl stage a command is about.
This buffer\='s, where its project has one, and otherwise the only one of
`wtlocal-dirs' that names a stage.  The fallback is the point of this: a
stage is not a worktree and holds no .git, so a command run from the stage
itself - or from a repl buffer, which is in no directory at all - has no
buffer to ask and would otherwise have to be told."
  (let ((here (wtlocal-for default-directory)))
    (if (and here (wtlocal-stage here))
        here
      (let ((staged (seq-filter #'wtlocal-stage wtlocal-dirs)))
        (cond ((null staged)
               (user-error "No wtlocal directory names a stage"))
              ((cdr staged)
               (user-error "More than one wtlocal directory names a stage"))
              (t (car staged)))))))

(defun wtlocal--worktrees (dir)
  "The worktrees of DIR\='s repository, as (LABEL . PATH).
LABEL is the directory\='s own name with the branch after it, which is the
pair that tells two worktrees apart: the directory is what a path is spelt
with and outlives the branch in it, and the branch is what anybody actually
means.  A detached worktree has only the first half."
  (let ((repo (wtlocal--repo dir))
        (found nil) (path nil) (branch nil))
    (dolist (line (and repo (ignore-errors
                              (process-lines "git" "-C" repo
                                             "worktree" "list" "--porcelain"))))
      (cond
       ((string-prefix-p "worktree " line)
        (setq path (substring line 9) branch nil))
       ((string-prefix-p "branch refs/heads/" line)
        (setq branch (substring line 18)))
       ((string-empty-p line)
        (when path
          (push (cons (if branch
                          (format "%s (%s)" (file-name-nondirectory path) branch)
                        (file-name-nondirectory path))
                      path)
                found))
        (setq path nil branch nil))))
    ;; git ends every record with a blank line, so this is belt and braces
    (when path (push (cons (file-name-nondirectory path) path) found))
    (nreverse found)))

(defun wtlocal--clear (stage)
  "Deal with the buffers under STAGE before its links move.
A buffer under a stage still holds the text of the worktree that was staged
when it was visited, and the path it would be saved to is about to mean
another file.  `magit-auto-revert-mode' would silently reload an unmodified
one with the other worktree\='s content, and saving a modified one would write
the worktree that is leaving over the file of the one arriving.  So they are
dealt with first, and not left to be noticed.

There are usually none: the stage is what the process reads, and a worktree
is what anybody edits."
  (let* ((prefix (file-name-as-directory stage))
         (buffers (seq-filter (lambda (b)
                                (when-let* ((f (buffer-file-name b)))
                                  (string-prefix-p prefix f)))
                              (buffer-list)))
         (modified (seq-filter #'buffer-modified-p buffers)))
    (when modified
      (if (y-or-n-p (format "Save %d modified buffer(s) under the stage first? "
                            (length modified)))
          (dolist (b modified) (with-current-buffer b (save-buffer)))
        (user-error "Aborted: %d modified buffer(s) would be stranded"
                    (length modified))))
    (when (and buffers
               (y-or-n-p (format "Kill %d buffer(s) under %s? "
                                 (length buffers)
                                 (abbreviate-file-name stage))))
      (mapc #'kill-buffer buffers))))

;; WHAT STAYED THE SAME, which git knows and the files do not say.  Pointing the
;; stage gives every file under it the modification time of the copy in the
;; worktree arrived at, whatever it holds, and a process deciding what changed
;; by those times then decides that everything did.

(defvar wtlocal-pointed-from nil
  "The worktree the stage pointed at before, while `wtlocal-after-point-hook' runs.
Nil where it pointed at nothing, and where it pointed where it points now -
so a hook reads it as whether anything moved, as well as from where.")

(defun wtlocal--git-paths (dir &rest args)
  "The paths git ARGS names in DIR, a NUL-separated listing read as a list."
  (with-temp-buffer
    (when (zerop (apply #'call-process "git" nil t nil "-C" dir args))
      (split-string (buffer-string) "\0" t))))

(defun wtlocal--dirty (dir)
  "The paths of DIR\='s worktree that are not what its HEAD holds.
Modified, added, deleted and untracked alike: whichever it is, git has
nothing to say about what the file holds."
  (mapcar (lambda (entry) (substring entry 3))
          (wtlocal--git-paths dir "status" "--porcelain" "-z" "--no-renames")))

(defun wtlocal-unchanged-files (from to)
  "The files of worktree TO that hold what the same files of worktree FROM do.
Paths relative to the worktree, as git names them.

What git vouches for and nothing else: tracked in TO, the same in the two
commits, and clean in both checkouts.  A file nobody committed is one whose
content git has never seen, so it is left out rather than guessed at - and
so is everything git ignores, which differs between worktrees as often as
not."
  (let* ((head (car (ignore-errors (process-lines "git" "-C" from "rev-parse" "HEAD"))))
         (differ (make-hash-table :test #'equal)))
    (when head
      (dolist (path (append (wtlocal--git-paths to "diff" "--name-only" "-z"
                                                "--no-renames" head "HEAD")
                            (wtlocal--dirty from)
                            (wtlocal--dirty to)))
        (puthash path t differ))
      (seq-remove (lambda (path) (gethash path differ))
                  (wtlocal--git-paths to "ls-files" "-z")))))

(defvar wtlocal-after-point-hook nil
  "Functions run once the stage has been pointed somewhere.
Called with the worktree it now points at and the stage itself, and with
`wtlocal-pointed-from' bound to the one it pointed at before.
WHAT BELONGS HERE IS WHATEVER HAS TO BE TOLD.  Moving the links is the whole
of what a shell can do, and a process already reading the stage is holding a
reading of its classpath from before they moved - so for replique this is
`replique-relinked'.  Nothing runs
where the script did not get that far.

BOTH ENDS ARE HANDED OVER because a hook needs both and neither can be
worked out from the other here: what was pointed at is the worktree, and
what a process has to be running in for any of this to mean anything is
the stage.")

;;;###autoload
(defun wtlocal-pointed-at ()
  "Say which worktree this repository\='s repl stage points at.
Read from the marker point.sh writes, so it answers without running it."
  (interactive)
  (let* ((dir (wtlocal-staged))
         (stage (wtlocal-stage dir))
         (worktree (wtlocal-stage-target stage))
         (label (and worktree
                     (car (seq-find (lambda (wt) (file-equal-p (cdr wt) worktree))
                                    (wtlocal--worktrees dir))))))
    (cond ((null worktree)
           (message "%s: not pointed anywhere yet" (abbreviate-file-name stage)))
          ((not (file-directory-p worktree))
           (message "%s -> %s, which no longer exists"
                    (abbreviate-file-name stage) (abbreviate-file-name worktree)))
          (t (message "%s -> %s" (abbreviate-file-name stage)
                      (or label (abbreviate-file-name worktree)))))))

;;;###autoload
(defun wtlocal-point (worktree)
  "Point this repository\='s repl stage at WORKTREE.
The stage is a directory that is not a worktree, holding this project\='s
classpath roots for real and everything inside them as links - so a process
running in it reads WORKTREE, and reads another one when this is asked again.
See the commentary for why it has to be shaped that way.

WHAT THIS DOES NOT DO IS DEPENDENCIES.  A classpath is computed once, when a
process starts, so a worktree whose deps or paths differ from the one running
needs that process restarted: point first, then restart.

Offered as the worktrees of the repository, named by directory and branch,
and the one this buffer is in is the default - which is the one case where
pointing somewhere is not what was wanted, and so the one worth having to
move off.

Buffers open under the stage are dealt with before anything moves - see
`wtlocal--clear' - and there are usually none."
  (interactive
   (let* ((dir (wtlocal-staged))
          (worktrees (wtlocal--worktrees dir))
          (here (when-let* ((root (locate-dominating-file default-directory ".git")))
                  (expand-file-name root)))
          (mine (car (seq-find (lambda (wt)
                                 (and here (file-equal-p (cdr wt) here)))
                               worktrees))))
     (unless worktrees (user-error "No worktrees to point at"))
     (list (cdr (assoc (completing-read
                        (format "Point %s at: " (wtlocal-stage dir))
                        (mapcar #'car worktrees) nil t nil nil mine)
                       worktrees)))))
  (let* ((dir (wtlocal-staged))
         (stage (wtlocal-stage dir))
         (_ (wtlocal--clear stage))
         ;; Read before the script writes it again
         (from (wtlocal-stage-target stage))
         (buf (wtlocal--run dir "point.sh" worktree))
         (said (string-trim (with-current-buffer buf (buffer-string)))))
    (message "%s: %s" (wtlocal--marker dir) said)
    (if (zerop (or (buffer-local-value 'wtlocal--status buf) 1))
        (let ((wtlocal-pointed-from
               (and from (file-directory-p from)
                    (not (file-equal-p from worktree))
                    from)))
          (run-hook-with-args 'wtlocal-after-point-hook worktree stage))
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
Saving a new file under a wtlocal directory links it into every worktree,
project.el lists the links that git has been told to ignore, and a file under
a stage belongs to the project of the worktree the stage points at."
  :global t
  ;; Required rather than waited for: `project-find-functions' is a `defvar',
  ;; which leaves a variable that already has a value alone - so adding to it
  ;; before project.el loads would set it to this and lose `project-try-vc'.
  (require 'project)
  (if wtlocal-mode
      (progn
        (add-hook 'after-save-hook #'wtlocal--sync-on-save)
        (add-hook 'project-find-functions #'wtlocal-stage-project))
    (remove-hook 'after-save-hook #'wtlocal--sync-on-save)
    (remove-hook 'project-find-functions #'wtlocal-stage-project)))

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
(declare-function project-current "project" (&optional maybe-prompt directory))

;; WHAT `project-current' ANSWERS IN A STAGE.
;;
;; Nothing at all, left alone.  A stage carries no .git - point.sh leaves it
;; out on purpose, since a git command run there would compare the worktree's
;; index against a tree whose src is a symlink and call the whole repository
;; deleted - so `project-try-vc' walks up out of it, finds no repository, and
;; C-x p f asks which project you meant instead of which file.
;;
;; THE ANSWER IS THE WORKTREE AND NOT THE STAGE, which is the better answer
;; anyway.  `project-files' on the stage would run find, which does not follow
;; links, so every package linked under a classpath root would come back as a
;; single file.  And a buffer opened through the stage is one `wtlocal-point'
;; kills the next time the stage moves, on a path git cannot act on.

(defun wtlocal-stage-project (dir)
  "The project of the worktree the stage holding DIR points at, or nil.
For `project-find-functions', and it cannot recurse through the rest of
them: what it hands `project-current' is the worktree, which is not under
the stage."
  (and-let* ((stage (wtlocal-stage-holding dir))
             (worktree (wtlocal-stage-target stage))
             ((file-directory-p worktree)))
    (project-current nil worktree)))
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

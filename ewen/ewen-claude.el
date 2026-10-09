;;; ewen-claude.el --- Claude Code sessions in ghostel buffers  -*- lexical-binding: t; -*-

(require 'ewen)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function dired-get-marked-files "dired")
(declare-function ghostel-exec "ghostel")
(declare-function ghostel-paste-string "ghostel")
(declare-function ghostel-send-key "ghostel")
(defvar ghostel-buffer-name-function)

(defun ewen/claude--read-dir (prompt default)
  "Read a directory named by PROMPT, starting from DEFAULT, with file completion.
The minibuffer starts in DEFAULT's parent with DEFAULT's name typed in: the
worktrees are checked out beside the main one (nosco-gamma_<branch>), so they
are listed straight away, already narrowed, and RET takes DEFAULT itself.
Worktrees kept elsewhere and the directories past sessions ran in are on M-p."
  (let* ((default (directory-file-name (expand-file-name default)))
         (file-name-history
          (delete-dups
           (append (mapcar (lambda (d) (abbreviate-file-name
                                        (file-name-as-directory d)))
                           (append (ewen/git-worktrees default)
                                   (ewen/claude--recent-dirs)))
                   file-name-history))))
    (read-directory-name (concat prompt ": ")
                         (file-name-directory default) nil t
                         (file-name-nondirectory default))))

(defun ewen/claude (&optional dir new args)
  "Run Claude Code in a ghostel buffer, in DIR, passing ARGS to the CLI.
Interactively, prompt for the directory, completing over the worktrees of the
current repo and defaulting to the project you are in.
`ghostel-buffer-name-function' is disabled buffer-locally so the buffer keeps
its own name rather than being renamed to match a shell terminal in the same
directory."
  (interactive
   (list (ewen/claude--read-dir
          "Start Claude in"
          (or (ignore-errors (project-root (project-current))) default-directory))
         current-prefix-arg))
  (let* ((dir (directory-file-name
               (expand-file-name
                (or dir (ignore-errors (project-root (project-current)))
                    default-directory))))
         (default-directory (file-name-as-directory dir))
         (label (abbreviate-file-name dir))
         (name (format "*claude: %s*" label))
         (existing (get-buffer name))
         ;; A second session in the same directory gets its own buffer rather
         ;; than being exec'd on top of a running one.  `generate-new-buffer'
         ;; gives "*claude: DIR*<2>".  A buffer whose process has exited is
         ;; reused, since `ghostel-kill-buffer-on-exit' is nil and those linger.
         (buf (if (or new (and existing (get-buffer-process existing)))
                  (generate-new-buffer name)
                (or existing (get-buffer-create name)))))
    ;; Display before exec: `ghostel-exec' sizes the terminal to the buffer's
    ;; window, falling back to 80x24 when the buffer is not on screen yet.
    (pop-to-buffer buf)
    (ghostel-exec buf "claude" args)
    (with-current-buffer buf
      (setq-local ghostel-buffer-name-function nil))))

(defconst ewen/claude-projects-dir (expand-file-name "~/.claude/projects/")
  "Where Claude Code keeps its transcripts, one directory per working directory.")

(defun ewen/claude--transcript (id)
  "The transcript file of session ID, wherever it was recorded, or nil.
Session ids are unique across projects, so a glob over every project
directory finds the one that holds it."
  (car (file-expand-wildcards
        (expand-file-name (concat "*/" id ".jsonl") ewen/claude-projects-dir))))

(defconst ewen/claude--head-bytes 200000
  "How much of a transcript's head is read for its directory and first prompt.
The first rows are bookkeeping; the working directory arrives with the first
message.")

(defconst ewen/claude--tail-bytes 100000
  "How much of a transcript's tail is read for its latest title and prompt.
Claude Code appends them again as the session goes on, so the newest ones
are near the end.")

(defvar ewen/claude--summaries (make-hash-table :test #'equal)
  "Transcript summaries by file, each with the size and mtime it was read at.
Transcripts run to hundreds of megabytes; only those that grew are re-read.")

(defun ewen/claude--prompt-text (row)
  "The text the user typed in the transcript ROW, or nil.
Tool results, slash commands and the meta rows Claude Code inserts are not
typed prompts."
  (and (equal (alist-get 'type row) "user")
       (not (alist-get 'isMeta row))
       (let* ((content (alist-get 'content (alist-get 'message row)))
              (text (if (stringp content)
                        content
                      (seq-some (lambda (part)
                                  (and (equal (alist-get 'type part) "text")
                                       (alist-get 'text part)))
                                content))))
         (and text (not (string-prefix-p "<" text)) text))))

(defun ewen/claude--scan (file beg end summary)
  "Fold the rows of FILE between bytes BEG and END into the plist SUMMARY.
The first directory and prompt seen are kept, later titles replace earlier
ones.  A row cut in half by BEG or END fails to parse and is skipped."
  (with-temp-buffer
    (insert-file-contents file nil beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((row (ignore-errors
                         (json-parse-string
                          (buffer-substring-no-properties (pos-bol) (pos-eol))
                          :object-type 'alist :null-object nil
                          :false-object nil))))
        (pcase-dolist (`(,key ,field ,first)
                       '((:cwd cwd t)
                         (:custom-title customTitle)
                         (:ai-title aiTitle)
                         (:last-prompt lastPrompt)))
          (when-let* ((value (alist-get field row)))
            (unless (and first (plist-get summary key))
              (setq summary (plist-put summary key value)))))
        (unless (plist-get summary :prompt)
          (setq summary (plist-put summary :prompt
                                   (ewen/claude--prompt-text row)))))
      (forward-line 1)))
  summary)

(defun ewen/claude--summary (file)
  "A plist describing the session recorded in FILE.
It holds :id, :file, :mtime, :cwd, :prompt (the first one), :last-prompt,
:ai-title and :custom-title, the last four when the transcript has them."
  (let* ((attrs (file-attributes file))
         (size (file-attribute-size attrs))
         (stamp (list size (file-attribute-modification-time attrs)))
         (cached (gethash file ewen/claude--summaries)))
    (if (equal (car cached) stamp)
        (cdr cached)
      (let ((summary (ewen/claude--scan
                      file 0 (min size ewen/claude--head-bytes)
                      (list :id (file-name-base file) :file file
                            :mtime (cadr stamp)))))
        (when (> size ewen/claude--head-bytes)
          (setq summary (ewen/claude--scan
                         file (max ewen/claude--head-bytes
                                   (- size ewen/claude--tail-bytes))
                         size summary)))
        (puthash file (cons stamp summary) ewen/claude--summaries)
        summary))))

(defun ewen/claude--transcript-cwd (file)
  "The directory the session recorded in FILE ran in, or nil.
Read out of the transcript rather than taken from the name of the project
directory holding it: that name mangles every slash to a dash, so
~/clojure/nosco-gamma-1 and the worktree nosco-gamma/1 share one spelling and
neither can be turned back into a path."
  (plist-get (ewen/claude--summary file) :cwd))

(defun ewen/claude--title (session)
  "SESSION's title: the one it was given, else Claude's, else its first prompt."
  (or (plist-get session :custom-title)
      (plist-get session :ai-title)
      (plist-get session :prompt)))

(defun ewen/claude--transcripts (&optional root)
  "Every recorded session, as `ewen/claude--summary' plists, newest first.
With ROOT, only the sessions that ran inside it.  Sessions that never got a
prompt are left out: there is nothing to resume."
  (sort (seq-filter
         (lambda (s)
           (and-let* ((cwd (plist-get s :cwd)))
             (and (ewen/claude--title s)
                  (or (null root) (file-in-directory-p cwd root)))))
         (mapcar #'ewen/claude--summary
                 (file-expand-wildcards
                  (expand-file-name "*/*.jsonl" ewen/claude-projects-dir))))
        :key (lambda (s) (plist-get s :mtime)) :lessp #'time-less-p
        :reverse t))

(defun ewen/claude--recent-dirs ()
  "The directories recorded sessions ran in that still exist, newest first."
  (seq-filter #'file-directory-p
              (delete-dups (mapcar (lambda (s) (plist-get s :cwd))
                                   (ewen/claude--transcripts)))))

(defun ewen/claude--age (time)
  "How long ago TIME was, in the largest whole unit."
  (let ((s (float-time (time-since time))))
    (cond ((< s 3600) (format "%dm" (/ s 60)))
          ((< s 86400) (format "%dh" (/ s 3600)))
          ((< s (* 14 86400)) (format "%dd" (/ s 86400)))
          (t (format "%dw" (/ s 604800))))))

(defun ewen/claude--oneline (text)
  "TEXT with its whitespace runs, newlines included, folded to single spaces."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))

(defun ewen/claude--candidate (session)
  "The line SESSION is offered as: age, directory, title and short id.
The directory and title are both in it so either can be typed to narrow."
  (format "%4s  %s  %s  %s"
          (ewen/claude--age (plist-get session :mtime))
          (truncate-string-to-width
           (file-name-nondirectory
            (directory-file-name (plist-get session :cwd)))
           32 nil ?\s "…")
          (truncate-string-to-width
           (ewen/claude--oneline (ewen/claude--title session))
           64 nil ?\s "…")
          (propertize (substring (plist-get session :id) 0 8) 'face 'shadow)))

(defun ewen/claude--killed-id ()
  "The session id sitting on top of the kill ring, or nil."
  (and-let* ((killed (ignore-errors (current-kill 0 t)))
             ((string-match
               "[0-9a-f]\\{8\\}\\(-[0-9a-f]\\{4\\}\\)\\{3\\}-[0-9a-f]\\{12\\}"
               killed)))
    (match-string 0 killed)))

(defun ewen/claude--read-session (&optional root)
  "Read a session id, choosing among the recorded sessions, newest first.
With ROOT, only the sessions that ran inside it are offered.  A session id on
the kill ring is preselected; an id that matches no line is taken as typed."
  (let* ((cands (mapcar (lambda (s)
                          (cons (ewen/claude--candidate s) (plist-get s :id)))
                        (ewen/claude--transcripts root)))
         (killed (ewen/claude--killed-id))
         (choice (completing-read
                  "Session: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action cands string pred)))
                  nil nil nil nil
                  (and killed (or (car (rassoc killed cands)) killed)))))
    (or (cdr (assoc choice cands)) (string-trim choice))))

(defun ewen/claude--prefix-root ()
  "The current project's root under a prefix argument, else nil."
  (and current-prefix-arg
       (if-let* ((project (project-current)))
           (project-root project)
         default-directory)))

(defun ewen/claude-resume (id &optional dir)
  "Resume the Claude Code session ID in DIR, which defaults to its own one.
Interactively, pick the session among every recorded one, newest first --
only the current project's with a prefix argument, and a session id on the
kill ring preselected -- then the directory, starting from the one the
session ran in.

In its own directory the session simply continues.  Anywhere else it is
resumed from the transcript\'s path and forked.  Both are needed: \"claude
--resume ID\" only searches the transcripts recorded for the directory it is
started in, and resuming the path alone would keep the original id, which
Claude Code refuses to resume while that session is live and whose transcript
stays filed under the old directory.  The fork replays the conversation into
a new session, recorded under DIR, leaving the original untouched."
  (interactive
   (let* ((id (ewen/claude--read-session (ewen/claude--prefix-root)))
          (file (or (ewen/claude--transcript id)
                    (user-error "No transcript for session %s" id))))
     (list id (ewen/claude--read-dir "Resume in"
                                     (or (ewen/claude--transcript-cwd file)
                                         default-directory)))))
  (let* ((id (string-trim id))
         (file (or (ewen/claude--transcript id)
                   (user-error "No transcript for session %s" id)))
         (origin (ewen/claude--transcript-cwd file))
         (dir (directory-file-name
               (expand-file-name (or dir origin default-directory)))))
    (unless (file-directory-p dir)
      (user-error "No such directory: %s" dir))
    (ewen/claude dir nil (if (and origin (file-equal-p dir origin))
                             (list "--resume" id)
                           (list "--resume" file "--fork-session")))))

(defvar-local ewen/claude-transcripts--root nil
  "When non-nil, the directory the listed sessions are limited to.")

(defun ewen/claude-transcripts--refresh ()
  "Re-read the sessions listed in this buffer."
  (setq tabulated-list-entries
        (mapcar (lambda (s)
                  (list s (vector (ewen/claude--age (plist-get s :mtime))
                                  (abbreviate-file-name (plist-get s :cwd))
                                  (ewen/claude--oneline (ewen/claude--title s))
                                  (ewen/claude--oneline
                                   (plist-get s :last-prompt)))))
                (ewen/claude--transcripts ewen/claude-transcripts--root))))

(defun ewen/claude-transcripts--at-point ()
  "The session on this line."
  (or (tabulated-list-get-id) (user-error "No session on this line")))

(defun ewen/claude-transcripts-resume ()
  "Resume the session on this line in the directory it ran in."
  (interactive)
  (ewen/claude-resume (plist-get (ewen/claude-transcripts--at-point) :id)))

(defun ewen/claude-transcripts-resume-in (dir)
  "Resume the session on this line in DIR, forked unless DIR is its own."
  (interactive
   (list (ewen/claude--read-dir
          "Resume in" (plist-get (ewen/claude-transcripts--at-point) :cwd))))
  (ewen/claude-resume (plist-get (ewen/claude-transcripts--at-point) :id) dir))

(defun ewen/claude-transcripts-copy-id ()
  "Put the id of the session on this line on the kill ring."
  (interactive)
  (let ((id (plist-get (ewen/claude-transcripts--at-point) :id)))
    (kill-new id)
    (message "Copied %s" id)))

(defun ewen/claude-transcripts-view ()
  "Visit the transcript of the session on this line."
  (interactive)
  (find-file-read-only (plist-get (ewen/claude-transcripts--at-point) :file)))

(defvar-keymap ewen/claude-transcripts-mode-map
  "RET" #'ewen/claude-transcripts-resume
  "f" #'ewen/claude-transcripts-resume-in
  "w" #'ewen/claude-transcripts-copy-id
  "v" #'ewen/claude-transcripts-view)

(define-derived-mode ewen/claude-transcripts-mode tabulated-list-mode
  "Claude transcripts"
  "Major mode listing recorded Claude Code sessions.
\\{ewen/claude-transcripts-mode-map}"
  (setq tabulated-list-format
        `[("Age" 4 ,(lambda (a b) (time-less-p (plist-get (car b) :mtime)
                                               (plist-get (car a) :mtime)))
           :right-align t)
          ("Directory" 40 t)
          ("Title" 50 t)
          ("Last prompt" 0 t)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'ewen/claude-transcripts--refresh
            nil t)
  (tabulated-list-init-header))

(defun ewen/claude-transcripts (&optional root)
  "List the recorded Claude Code sessions, newest first.
With a prefix argument, only those that ran inside the current project.
RET resumes the session on the line, f resumes it elsewhere, w copies its id
and v visits its transcript."
  (interactive (list (ewen/claude--prefix-root)))
  (let ((buf (get-buffer-create "*claude transcripts*")))
    (with-current-buffer buf
      (ewen/claude-transcripts-mode)
      (setq ewen/claude-transcripts--root root)
      (ewen/claude-transcripts--refresh)
      (tabulated-list-print))
    (pop-to-buffer-same-window buf)))

(defun ewen/claude-session-p (buffer)
  "Non-nil when BUFFER is a Claude ghostel session with a running process.
`ghostel-kill-buffer-on-exit' is nil, so finished sessions linger; requiring
a live process keeps those out."
  (and (string-prefix-p "*claude: " (buffer-name buffer))
       (process-live-p (get-buffer-process buffer))))

(defun ewen/claude-sessions ()
  "Every live Claude session."
  (seq-filter #'ewen/claude-session-p (buffer-list)))

(defun ewen/claude-buffers-for (dir)
  "Live Claude sessions running in DIR.
Matches by prefix so the \"<2>\" of a second session in the same directory
is included."
  (let ((name (format "*claude: %s*"
                      (abbreviate-file-name (directory-file-name dir)))))
    (seq-filter (lambda (b) (string-prefix-p name (buffer-name b)))
                (ewen/claude-sessions))))

(defun ewen/claude--visible-sessions ()
  "Live Claude sessions on screen, the most recently used window first."
  (let (bufs)
    (dolist (w (sort (window-list nil 'never)
                     (lambda (a b) (> (window-use-time a) (window-use-time b)))))
      (let ((b (window-buffer w)))
        (when (and (ewen/claude-session-p b) (not (memq b bufs)))
          (push b bufs))))
    (nreverse bufs)))

(defun ewen/claude--pick (bufs)
  "The single session in BUFS, or one chosen from several, or nil for none."
  (cond ((null bufs) nil)
        ((null (cdr bufs)) (car bufs))
        (t (get-buffer (completing-read "Claude session: "
                                        (mapcar #'buffer-name bufs) nil t)))))

(defun ewen/claude--start (dir)
  "Offer to start Claude in DIR, then abort: it is not ready to be pasted into."
  (if (y-or-n-p (format "No Claude session.  Start one in %s? "
                        (abbreviate-file-name dir)))
      (progn (ewen/claude dir)
             (user-error "Claude is starting -- send again once it is ready"))
    (user-error "No Claude session")))

(defun ewen/claude--session (dir)
  "The Claude session to send to, preferring DIR's.
A session on screen wins, since that is the one being watched: among several
the one for DIR, else the most recently used window.  With none on screen,
fall back to DIR's sessions, then to any session at all."
  (let* ((visible (ewen/claude--visible-sessions))
         (here (ewen/claude-buffers-for dir)))
    (or (and visible (or (car (seq-intersection visible here)) (car visible)))
        (ewen/claude--pick here)
        (ewen/claude--pick (ewen/claude-sessions))
        (ewen/claude--start dir))))

(defun ewen/claude--file ()
  "The file this buffer visits, following an indirect buffer to its base."
  (or buffer-file-name
      (and-let* ((base (buffer-base-buffer)))
        (buffer-file-name base))))

(defun ewen/claude--targets ()
  "Paths to reference: the Dired selection, or the file this buffer visits.
In Dired with nothing marked this is the entry at point, and on a line that
names no entry it is the directory being listed."
  (or (and (derived-mode-p 'dired-mode)
           (or (ignore-errors (dired-get-marked-files))
               (list default-directory)))
      (and-let* ((f (ewen/claude--file))) (list f))
      (user-error "Nothing here to reference")))

(defun ewen/claude--project-dir (fallback)
  "This buffer's project root, or the directory holding FALLBACK."
  (or (ignore-errors (project-root (project-current)))
      (file-name-directory (directory-file-name fallback))))

(defun ewen/claude--relative (path root)
  "PATH relative to ROOT, or absolute when it lies outside ROOT.
The chosen session may belong to another project, and a \"..\" path out of
its working directory is not something Claude can resolve."
  (let* ((abs (directory-file-name (expand-file-name path)))
         (rel (if (file-equal-p abs root) "." (file-relative-name abs root))))
    (if (string-prefix-p ".." rel) abs rel)))

(defun ewen/claude--ref (path root)
  "PATH as an @-reference relative to ROOT, directories keeping a trailing /."
  (concat "@" (ewen/claude--relative path root)
          (and (file-directory-p path) "/")))

(defun ewen/claude--fence-language ()
  "Markdown info string for the current major mode, or the empty string."
  (let ((name (replace-regexp-in-string "\\(-ts\\)?-mode\\'" ""
                                        (symbol-name major-mode))))
    (pcase name
      ((or "emacs-lisp" "lisp-interaction") "elisp")
      ("sh" "bash")
      ("fundamental" "")
      (_ name))))

(defun ewen/claude--fence (text)
  "A backtick fence one longer than the longest run of backticks in TEXT."
  (let ((longest 0) (pos 0))
    (while (string-match "`+" text pos)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            pos (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(defun ewen/claude-reference-file (&optional submit)
  "Insert @-references to this buffer's file, or the Dired selection, into a
Claude session.  Directories are referenced as such.
With a prefix argument SUBMIT, also send RET.  Paths are relative to the
session's own working directory, not to this buffer's project."
  (interactive "P")
  (let* ((targets (ewen/claude--targets))
         (buf (ewen/claude--session (ewen/claude--project-dir (car targets))))
         (root (buffer-local-value 'default-directory buf))
         (refs (mapconcat (lambda (f) (ewen/claude--ref f root)) targets " ")))
    ;; Bracketed paste, not `ghostel-send-string': typing "@" character by
    ;; character opens Claude's file picker, which then eats the path.
    (with-current-buffer buf
      (ghostel-paste-string (concat refs " "))
      (when submit (ghostel-send-key "return")))
    (message "%s -> %s" refs (buffer-name buf))))

(defun ewen/claude-send-region (beg end &optional submit)
  "Send the region BEG..END to a Claude session as a fenced quote.
The block is headed with \"path:first-last\" so Claude can locate the lines.
With a prefix argument SUBMIT, also send RET."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (user-error "No active region")))
  (let* ((file (ewen/claude--file))
         (buf (ewen/claude--session
               (ewen/claude--project-dir (or file default-directory))))
         (root (buffer-local-value 'default-directory buf))
         (text (buffer-substring-no-properties beg end))
         ;; A line-wise selection ends at the start of the line after the last
         ;; one it shows; naming that line would be off by one.
         (last (line-number-at-pos (if (and (> end beg)
                                            (= end (save-excursion
                                                     (goto-char end)
                                                     (line-beginning-position))))
                                       (1- end)
                                     end)))
         (header (format "%s:%d-%d"
                         (if file (ewen/claude--relative file root) (buffer-name))
                         (line-number-at-pos beg) last))
         (fence (ewen/claude--fence text))
         (block (format "%s\n%s%s\n%s\n%s\n"
                        header fence (ewen/claude--fence-language)
                        (replace-regexp-in-string "\n\\'" "" text)
                        fence)))
    ;; Bracketed paste is required, not merely safer: a typed newline would
    ;; submit the prompt instead of continuing the block.
    (with-current-buffer buf
      (ghostel-paste-string block)
      (when submit (ghostel-send-key "return")))
    (message "%s -> %s" header (buffer-name buf))))

(provide 'ewen-claude)
;;; ewen-claude.el ends here

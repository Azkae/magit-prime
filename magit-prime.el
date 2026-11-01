;;; magit-prime.el --- Prime cache before Magit refresh -*- lexical-binding: t; -*-

;; Author: Romain Ouabdelkader <romain.ouabdelkader@gmail.com>
;; URL: https://github.com/Azkae/magit-prime
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (magit "3.0.0"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A Magit extension that primes caches in parallel before refresh operations,
;; reducing Magit buffer refresh times.

;; Usage:
;; (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache)

;;; Code:

(require 'magit)
(require 'tramp-sh)

(defconst magit-prime--kinds '(str lines items wash exit-code config)
  "Kinds of refresh-cache entries primed by magit-prime.

Each command entry has the form (CACHEP KIND . ARGS).  CACHEP is the
symbol `t' when the entry is cached even if git fails.  KIND is one
of the symbols in this list and determines both the cache key and the
cached value:

  str        first line of output;      key (TOPLEVEL . ARGS)
             (as used by `magit-git-str', `magit-git-string', ...)
  lines      list of output lines;     key (TOPLEVEL magit-git-lines . ARGS)
  items      list of NUL-separated items; key (TOPLEVEL magit-git-items . ARGS)
  wash       (EXIT . raw output);      key (TOPLEVEL magit--git-wash . ARGS)
  exit-code  exit code;                 key (TOPLEVEL magit-git-exit-code . ARGS)
  config     hash table of all config; key (TOPLEVEL . config)

If KIND is omitted, it defaults to `str'.")

(defcustom magit-prime-timeout 1
  "Maximum number of seconds to wait for priming git processes.
Processes that finish later still prime the cache while the refresh
is running."
  :type 'number
  :group 'magit-prime)

(defconst magit-prime--commands-phase-one
  `(("symbolic-ref" "--short" "HEAD")
    ("rev-parse" "--show-toplevel")
    ("rev-parse" "--show-cdup")
    (config "config" "--list" "-z")
    (t "describe" "--long" "--tags")
    ("describe" "--contains" "HEAD")
    ("rev-parse" "--git-dir")
    ("rev-parse" "--is-bare-repository")
    ("rev-parse" "--short" "HEAD")
    ("rev-parse" "--short" "HEAD~")
    ("rev-parse" "--verify" "--abbrev-ref" "master@{upstream}")
    ("rev-parse" "--verify" "--abbrev-ref" "main@{upstream}")
    (t "describe" "--contains" "HEAD")
    (t "rev-parse" "--verify" "HEAD")
    (t "rev-parse" "--verify" "refs/stash")
    (t "rev-parse" "--verify" "HEAD~10"))
  "Git commands primed unconditionally before a refresh.

Entries prefixed with t are cached even on failure.")

(defun magit-prime--for-each-ref-commands ()
  "Return `lines' entries mirroring `magit-list-refs' git calls."
  (let ((sortby (mapcar (lambda (s) (concat "--sort=" s))
                        (ensure-list magit-list-refs-sortby))))
    (mapcan
     (lambda (ns)
       (mapcar
        (lambda (format)
          `(lines "for-each-ref" ,(concat "--format=%(symref)\f" format)
                  ,@(if (and (not sortby) (equal ns "refs/tags"))
                        (list "--sort=-version:refname")
                      sortby)
                  ,ns))
        '("%(refname:short)" "%(refname)")))
     magit-list-refs-namespaces)))

(defun magit-prime--log-command (revs largs files)
  "Build a `wash' entry mirroring `magit--insert-log' for REVS with LARGS."
  (let* ((args largs)
         (fmt (format magit-log-heading-format
                      (if (and (member "--left-right" args)
                               (not (member "--graph" args)))
                          "%m " "")
                      (if (member "--decorate" args) "%D" "")
                      ""
                      (if magit-log-margin-show-committer-date "%ct" "%at")
                      ""
                      ""))
         (args (if (member "--decorate" args)
                   (cons "--decorate=full" (remove "--decorate" args))
                 args)))
    `(wash "log" ,fmt ,@args "--use-mailmap" "--no-prefix" ,revs "--" ,@files)))

(defun magit-prime--log-commands (largs lfiles push-branch upstream-branch)
  "Return `wash' entries for the log sections of the status buffer."
  (when (and largs
             (not magit-log-trailer-labels)
             (not (member "--show-signature" largs)))
    (let ((recent-args (cons (format "-n%d" magit-log-section-commit-count)
                             (seq-remove (##string-prefix-p "-n" %) largs))))
      (delq nil
            (list (magit-prime--log-command nil recent-args lfiles)
                  (and push-branch
                       (magit-prime--log-command (concat push-branch "..") largs lfiles))
                  (and push-branch
                       (magit-prime--log-command (concat ".." push-branch) largs lfiles))
                  (magit-prime--log-command "@{upstream}.." largs lfiles)
                  (magit-prime--log-command "..@{upstream}" largs lfiles)
                  (and upstream-branch
                       (magit-prime--log-command (concat upstream-branch "..") largs lfiles))
                  (and upstream-branch
                       (magit-prime--log-command (concat ".." upstream-branch) largs lfiles)))))))

(defun magit-prime--diff-commands (dargs dfiles)
  "Return `wash' entries for the staged/unstaged diff sections."
  (unless (seq-some (##string-prefix-p "-C" %) (append dargs dfiles))
    (list `(wash "diff" "--ita-visible-in-index" ,@dargs "--no-prefix" "--" ,@dfiles)
          `(wash "diff" "--ita-visible-in-index" "--cached" ,@dargs
                 "--no-prefix" "--" ,@dfiles))))

(defun magit-prime--commands-phase-two ()
  "Return git commands that depend on the current branch and remotes.

Commands prefixed with t are cached even on failure."
  (let* ((branch (magit-get-current-branch))
         (main (magit-main-branch))
         (push-main (magit-get-push-branch main))
         (push-branch (magit-get-push-branch branch))
         (upstream-main (magit-get-upstream-branch main))
         (upstream-branch (magit-get-upstream-branch branch))
         (push-remote (magit-get-push-remote branch))
         (primary-remote (magit-primary-remote))
         (status-buffer (magit-get-mode-buffer 'magit-status-mode))
         (dargs (or (and status-buffer
                         (buffer-local-value 'magit-buffer-diff-args status-buffer))
                    (get 'magit-status-mode 'magit-diff-default-arguments)))
         (dfiles (and status-buffer
                      (buffer-local-value 'magit-buffer-diff-files status-buffer)))
         (largs (or (and status-buffer
                         (buffer-local-value 'magit-buffer-log-args status-buffer))
                    (get 'magit-status-mode 'magit-log-default-arguments)))
         (lfiles (and status-buffer
                      (buffer-local-value 'magit-buffer-log-files status-buffer))))
    (cl-remove-duplicates
     `(("rev-parse" "--verify" "--abbrev-ref" ,(concat main "@{upstream}"))
       ("rev-parse" "--verify" "--abbrev-ref" ,(concat branch "@{upstream}"))
       ("rev-parse" "--verify" "--abbrev-ref" ,(concat push-branch "^{commit}"))
       ("log" "--no-walk" "--format=%s" ,(concat upstream-main"^{commit}") "--")
       ("log" "--no-walk" "--format=%s" ,(concat push-main "^{commit}") "--")
       ("log" "--no-walk" "--format=%s" ,(concat push-branch "^{commit}") "--")
       ("log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--")
       (t "symbolic-ref" ,(format "refs/remotes/%s/HEAD" primary-remote))
       (t "symbolic-ref" ,(format "refs/remotes/%s/HEAD" push-remote))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" branch))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" main))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" push-branch))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" push-main))
       (t "rev-parse" "--verify" ,(format "refs/tags/%s/HEAD" primary-remote))
       (t "rev-parse" "--verify" ,push-branch)
       ;; Remotes and branches, cached by `magit-git-lines'.
       (lines "remote")
       ,@(magit-prime--for-each-ref-commands)
       ;; Untracked files, cached by `magit-git-items'.
       (t items "config" "--local" "-z" "--get-all" "--include"
          "status.showUntrackedFiles")
       (items "status" "-z" "--porcelain" "--untracked-files=normal" "--")
       (items "status" "-z" "--porcelain" "--untracked-files=all" "--")
       ;; Index refresh, cached by `magit-git-exit-code'.
       (exit-code "update-index" "--refresh")
       (t exit-code "rev-parse" "@{upstream}")
       ,@(and upstream-branch
              (list `(t exit-code "merge-base" "--is-ancestor" "HEAD"
                             ,upstream-branch)))
       ;; Washed sections (diffs, logs, stashes), cached by `magit--git-wash'.
       (wash "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" "refs/stash")
       ,@(magit-prime--diff-commands dargs dfiles)
       ,@(magit-prime--log-commands largs lfiles push-branch upstream-branch))
     :test #'equal)))

(defun magit-prime--commands-phase-three ()
  "Return git commands derived from already primed log output.

Log decorations (\"tag: NAME\") cause `magit-format-ref-labels' to run
\"git rev-parse --verify refs/tags/NAME\"; prime those too."
  (let (tags)
    (dolist (entry (cdr magit--refresh-cache))
      (pcase-let ((`(,key . (,exit . ,output)) entry))
        (when (and (consp key)
                   (consp (cdr key))
                   (eq (cadr key) 'magit--git-wash)
                   (equal (caddr key) "log")
                   (eq exit 0)
                   (stringp output))
          (with-temp-buffer
            (insert output)
            (goto-char (point-min))
            (while (re-search-forward "\\(?:^\\|, \\)tag: \\([^,]+\\)" nil t)
              (push (match-string-no-properties 1) tags))))))
    (mapcar (lambda (tag)
              (list t "rev-parse" "--verify" (concat "refs/tags/" tag)))
            (delete-dups tags))))

(defun magit-prime-refresh-cache ()
  "Prime the refresh cache if possible."
  (when (and (or magit-refresh-status-buffer
                 (derived-mode-p 'magit-status-mode))
             magit--refresh-cache
             (magit-toplevel))
    ;; Bind `default-directory' to the toplevel so that the cache keys
    ;; computed while gathering phase-two/three commands match the keys
    ;; used when the status buffer is refreshed.
    (let ((default-directory (magit-toplevel))
          (elapsed
           (benchmark-elapse
             (magit-prime--refresh-cache magit-prime--commands-phase-one)
             (magit-prime--refresh-cache (magit-prime--commands-phase-two))
             (magit-prime--refresh-cache (magit-prime--commands-phase-three)))))
      (when magit-refresh-verbose
        (message "Refresh cached primed in %.3fs" elapsed)))))

(defun magit-prime--parse-command (command)
  "Parse a COMMAND entry into (CACHEP KIND ARGS).
ARGS is flattened (and stripped of nils) so that cache keys match the
keys computed by magit, which flattens arguments the same way."
  (let ((cachep (eq (car command) t)))
    (when cachep
      (pop command))
    (if (memq (car command) magit-prime--kinds)
        (list cachep (pop command) (flatten-tree command))
      (list cachep 'str (flatten-tree command)))))

(defun magit-prime--entry-key (repo kind args)
  "Return the refresh-cache key for a primed command of KIND with ARGS."
  (pcase kind
    ('config (cons repo 'config))
    ('str (cons repo args))
    ('lines (cons repo (cons 'magit-git-lines args)))
    ('items (cons repo (cons 'magit-git-items args)))
    ('exit-code (cons repo (cons 'magit-git-exit-code args)))
    ('wash (cons repo (cons 'magit--git-wash args)))))

(defun magit-prime--config-hash-table (output)
  "Build the hash table cached under the `config' key from OUTPUT.
Mirrors `magit-config-get-from-cached-list'."
  (let ((configs (make-hash-table :test #'equal)))
    (dolist (conf (split-string output "\0" t))
      (let* ((nl-pos (cl-position ?\n conf))
             (key (substring conf 0 nl-pos))
             (val (if nl-pos (substring conf (1+ nl-pos)) "")))
        (puthash key (nconc (gethash key configs) (list val)) configs)))
    configs))

(defconst magit-prime--skip (gensym "magit-prime-skip")
  "Marker returned by `magit-prime--entry-value' when nothing should
be cached.")

(defun magit-prime--entry-value (kind cachep exit-status output)
  "Compute the cache value for a finished priming process.
KIND and CACHEP come from the command entry, EXIT-STATUS and OUTPUT
from the git process.  Return `magit-prime--skip' when the entry must
not be cached."
  (setq output (or output ""))
  (if (zerop exit-status)
      (pcase kind
        ('wash (cons exit-status output))
        ('exit-code exit-status)
        ('config (magit-prime--config-hash-table output))
        ('lines (split-string output "\n" t))
        ('items (split-string output "\0" t))
        ('str (and (not (zerop (length output)))
                   (substring output 0 (or (cl-position ?\n output)
                                           (length output))))))
    ;; On failure, only cache (as nil, or as the exit code) when the
    ;; entry is marked with t; never cache failed wash/config output.
    (when (and cachep (memq kind '(str lines items exit-code)))
      (if (eq kind 'exit-code) exit-status nil))))

(defun magit-prime--refresh-cache (commands)
  "Execute git COMMANDS to prime Magit's refresh cache."
  (if (file-remote-p default-directory)
      (magit-prime--refresh-cache-remote commands)
    (magit-prime--refresh-cache-local commands)))

(defun magit-prime--refresh-cache-local (commands)
  "Prime the refresh cache with the provided COMMANDS."
  (let* ((repo-path (magit-toplevel))
         (running 0)
         (buffers
          (mapcar
           (lambda (command)
             (pcase-let ((`(,cachep ,kind ,args)
                          (magit-prime--parse-command command)))
               (let* ((buffer (generate-new-buffer " *magit-prime-refresh-cache*"))
                      (stderr (generate-new-buffer " *magit-prime-refresh-cache-stderr*"))
                      (process-environment (magit-process-environment))
                      (default-process-coding-system (magit--process-coding-system)))
                 (make-process
                  :name (buffer-name buffer)
                  :buffer buffer
                  :stderr stderr
                  :noquery t
                  :connection-type 'pipe
                  :command (cons magit-git-executable
                                 (magit-process-git-arguments args))
                  :sentinel
                  (lambda (proc _event)
                    (when (eq (process-status proc) 'exit)
                      (let ((buf (process-buffer proc)))
                        (when (buffer-live-p buf)
                          (let ((value
                                 (with-current-buffer buf
                                   (magit-prime--entry-value
                                    kind cachep (process-exit-status proc)
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))))
                            (unless (eq value magit-prime--skip)
                              (push (cons (magit-prime--entry-key repo-path kind args)
                                          value)
                                    (cdr magit--refresh-cache))))
                          (kill-buffer buf)))
                      (when (buffer-live-p stderr)
                        (kill-buffer stderr))
                      (cl-decf running))))
                 (cl-incf running)
                 buffer)))
           commands)))

    (with-timeout (magit-prime-timeout)
      (while (> running 0)
        (sit-for 0.01)
        (accept-process-output)))

    ;; Buffers whose processes are still running are left alone; their
    ;; sentinels may still prime the cache while the refresh is running.
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer)
                 (not (get-buffer-process buffer)))
        (kill-buffer buffer)))))

(defconst magit-prime--batch-commands-script "
COMMANDS=\"$1\"
REPO_DIR=\"$2\"

run_command() {
    local id=\"$1\"
    local command=\"$2\"
    local tmp output exit_code

    tmp=$(mktemp)
    (cd \"$REPO_DIR\" && eval \"$command\" >\"$tmp\" 2>/dev/null)
    exit_code=$?
    output=$(base64 <\"$tmp\" | tr -d '\\n')
    rm -f \"$tmp\"

    echo \"($id $exit_code \\\"$output\\\")\"
}

echo \\(

echo \"$COMMANDS\" | {
    while IFS=':' read -r id command; do
        run_command \"$id\" \"$command\" &
    done

    wait
}

echo \\)
")

(defun magit-prime--refresh-cache-remote (commands)
  "Prime the refresh cache with the provided COMMANDS using tramp."
  (let* ((vec (tramp-dissect-file-name default-directory))
         (str-commands (magit-prime--format-commands-for-bash commands))
         (repo-dir (file-remote-p default-directory 'localname))
         (repo-path (magit-toplevel))
         (coding (car (magit--process-coding-system))))
    (tramp-maybe-send-script vec magit-prime--batch-commands-script
                             "magit_prime__batch_commands")
    (let ((results
           (tramp-send-command-and-read
            vec (format "magit_prime__batch_commands '%s' '%s'" str-commands repo-dir))))
      (dolist (item results)
        (pcase-let ((`(,command ,status-code ,b64) item)
                    (`(,cachep ,kind ,args) (magit-prime--parse-command command)))
          (let* ((output (and (stringp b64)
                              (not (zerop (length b64)))
                              (decode-coding-string
                               (base64-decode-string b64) coding)))
                 (value (magit-prime--entry-value kind cachep status-code output)))
            (unless (eq value magit-prime--skip)
              (push (cons (magit-prime--entry-key repo-path kind args) value)
                    (cdr magit--refresh-cache)))))))))

(defun magit-prime--format-commands-for-bash (commands)
  "Convert COMMANDS list to bash script input format.
Each command becomes `LISP-FORM:git ARGS'
where LISP-FORM is the original command."
  (mapconcat
   (lambda (command)
     (pcase-let ((`(,_cachep ,_kind ,args) (magit-prime--parse-command command)))
       (let* ((clean-command (mapcar #'substring-no-properties args))
              (git-command (mapconcat #'shell-quote-argument clean-command " "))
              (line (format "%S:%s %s" command magit-remote-git-executable git-command)))
         line)))
   commands
   "\n"))

;;;###autoload
(define-minor-mode magit-prime-mode
  "Global minor mode to enable magit-prime cache priming.
When enabled, automatically primes caches before Magit refresh operations."
  :global t
  :group 'magit-prime
  (if magit-prime-mode
      (add-hook 'magit-pre-refresh-hook #'magit-prime-refresh-cache)
    (remove-hook 'magit-pre-refresh-hook #'magit-prime-refresh-cache)))

(provide 'magit-prime)

;;; magit-prime.el ends here

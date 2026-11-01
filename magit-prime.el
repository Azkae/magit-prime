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
;; (magit-prime-mode 1)
;;
;; Enabling `magit-prime-mode' adds `magit-prime-refresh-cache' to
;; `magit-pre-refresh-hook' and advises `magit-git-exit-code',
;; `magit-git-lines' and `magit-git-items' to consult the refresh
;; cache, so no changes to Magit itself are needed.  It also advises
;; `magit-rev-parse-safe' to cache "--show-toplevel" and
;; "--show-cdup" results for a few seconds per directory, which
;; avoids repeated git processes from `magit-toplevel' and friends
;; outside of refreshes.

;;; Code:

(require 'magit)
(require 'tramp-sh)

(defconst magit-prime--kinds '(str lines items exit-code config)
  "Kinds of refresh-cache entries primed by magit-prime.

Each command entry has the form (CACHEP KIND . ARGS).  CACHEP is the
symbol `t' when the entry is cached even if git fails.  KIND is one
of the symbols in this list and determines both the cache key and the
cached value:

  str        first line of output;      key (TOPLEVEL . ARGS)
             (as used by `magit-git-str', `magit-git-string', ...)
  lines      list of output lines;     key (TOPLEVEL magit-git-lines . ARGS)
  items      list of NUL-separated items; key (TOPLEVEL magit-git-items . ARGS)
  exit-code  exit code;                 key (TOPLEVEL magit-git-exit-code . ARGS)
  config     hash table of all config; key (TOPLEVEL . config)

If KIND is omitted, it defaults to `str'.")

(defcustom magit-prime-timeout 1
  "Maximum number of seconds to wait for priming git processes.
Processes that finish later still prime the cache while the refresh
is running."
  :type 'number
  :group 'magit-prime)

(defcustom magit-prime-rev-parse-cache-duration 5
  "Seconds to cache `magit-rev-parse-safe' directory lookups.
Calls to `magit-rev-parse-safe' with \"--show-toplevel\" or
\"--show-cdup\" are cached for this many seconds, keyed by
`default-directory', so that repeated calls outside of a refresh
(for example from `magit-toplevel') do not spawn a git process
each time.  nil disables the cache."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'magit-prime)

(defconst magit-prime--rev-parse-cached-args
  '(("--show-toplevel") ("--show-cdup"))
  "Argument lists of `magit-rev-parse-safe' that are cached.")

(defvar magit-prime--rev-parse-cache (make-hash-table :test #'equal)
  "Time-limited cache for `magit-rev-parse-safe' directory lookups.
Keys are (DEFAULT-DIRECTORY . ARGS) and values are (TIMESTAMP . VALUE),
so a cached nil result is still a cache hit.")

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

;; (defun magit-prime--for-each-ref-commands ()
;;   "Return `lines' entries mirroring `magit-list-refs' git calls."
;;   (let ((sortby (mapcar (lambda (s) (concat "--sort=" s))
;;                         (ensure-list magit-list-refs-sortby))))
;;     (mapcan
;;      (lambda (ns)
;;        (mapcar
;;         (lambda (format)
;;           `(lines "for-each-ref" ,(concat "--format=%(symref)\f" format)
;;                   ,@(if (and (not sortby) (equal ns "refs/tags"))
;;                         (list "--sort=-version:refname")
;;                       sortby)
;;                   ,ns))
;;         '("%(refname:short)" "%(refname)")))
;;      magit-list-refs-namespaces)))

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
         (primary-remote (magit-primary-remote)))
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
       ;; (lines "remote")
       ;; ,@(magit-prime--for-each-ref-commands)
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
                             ,upstream-branch))))
     :test #'equal)))

(defun magit-prime-refresh-cache ()
  "Prime the refresh cache if possible."
  (when magit-process-record-invocations
    (let ((messages-buffer-name magit-process-record-buffer-name)
          (inhibit-message t))
      (message "magit-prime: starting refresh")))
  (when (and (or magit-refresh-status-buffer
                 (derived-mode-p 'magit-status-mode))
             magit--refresh-cache
             (magit-toplevel))
    ;; Bind `default-directory' to the toplevel so that the cache keys
    ;; computed while gathering phase-two commands match the keys
    ;; used when the status buffer is refreshed.
    (let ((default-directory (magit-toplevel))
          (elapsed
           (benchmark-elapse
             (magit-prime--refresh-cache magit-prime--commands-phase-one)
             (magit-prime--refresh-cache (magit-prime--commands-phase-two)))))
      (when magit-refresh-verbose
        (message "Refresh cached primed in %.3fs" elapsed))
      (when magit-process-record-invocations
        (let ((messages-buffer-name magit-process-record-buffer-name)
              (inhibit-message t))
          (message "magit-prime: finished in %.3fs" elapsed))))))

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
    ('exit-code (cons repo (cons 'magit-git-exit-code args)))))

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
        ('exit-code exit-status)
        ('config (magit-prime--config-hash-table output))
        ('lines (split-string output "\n" t))
        ('items (split-string output "\0" t))
        ('str (and (not (zerop (length output)))
                   (substring output 0 (or (cl-position ?\n output)
                                           (length output))))))
    ;; On failure, only cache (as nil, or as the exit code) when the
    ;; entry is marked with t; never cache failed config output.
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

# COMMANDS holds alternating lines: a Lisp form identifying the command,
# then the shell command to run.  Reading line pairs avoids any field
# separator clashing with characters inside the Lisp form or the command.
echo \"$COMMANDS\" | {
    while IFS= read -r id; do
        IFS= read -r command
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
            vec (format "magit_prime__batch_commands %s %s"
                         (magit-prime--sh-single-quote str-commands)
                         (magit-prime--sh-single-quote repo-dir)))))
      (dolist (item results)
        (pcase-let ((`(,command ,status-code ,b64) item))
          (pcase-let ((`(,cachep ,kind ,args) (magit-prime--parse-command command)))
            (let* ((output (and (stringp b64)
                                (not (zerop (length b64)))
                                (decode-coding-string
                                 (base64-decode-string b64) coding)))
                   (value (magit-prime--entry-value kind cachep status-code output)))
              (unless (eq value magit-prime--skip)
                (push (cons (magit-prime--entry-key repo-path kind args) value)
                      (cdr magit--refresh-cache))))))))))

(defun magit-prime--sh-single-quote (string)
  "Single-quote STRING for a POSIX shell command line."
  (concat "'" (replace-regexp-in-string "'" "'\\\\''" string) "'"))

(defun magit-prime--format-commands-for-bash (commands)
  "Convert COMMANDS list to bash script input format.
Each command becomes two lines: the original command as a Lisp
form, then the shell command to run."
  (let ((print-escape-newlines t))
    (mapconcat
     (lambda (command)
       (pcase-let ((`(,_cachep ,_kind ,args) (magit-prime--parse-command command)))
         (let* ((clean-command (mapcar #'substring-no-properties args))
                (git-command (mapconcat #'shell-quote-argument clean-command " ")))
           (concat (format "%S" command) "\n"
                   magit-remote-git-executable " " git-command))))
     commands
     "\n")))

(defun magit-prime--advice-git-exit-code (orig-fun &rest args)
  "Around advice for `magit-git-exit-code' consulting the refresh cache.
ORIG-FUN is the original function.  Flatten ARGS and look the exit
code up under (default-directory magit-git-exit-code . ARGS)."
  (setq args (flatten-tree args))
  (magit--with-refresh-cache (cons default-directory (cons 'magit-git-exit-code args))
    (apply orig-fun args)))

(defun magit-prime--advice-git-lines (orig-fun &rest args)
  "Around advice for `magit-git-lines' consulting the refresh cache.
ORIG-FUN is the original function.  Flatten ARGS and look the output
lines up under (default-directory magit-git-lines . ARGS)."
  (setq args (flatten-tree args))
  (magit--with-refresh-cache (cons default-directory (cons 'magit-git-lines args))
    (apply orig-fun args)))

(defun magit-prime--advice-git-items (orig-fun &rest args)
  "Around advice for `magit-git-items' consulting the refresh cache.
ORIG-FUN is the original function.  Flatten ARGS and look the output
items up under (default-directory magit-git-items . ARGS)."
  (setq args (flatten-tree args))
  (magit--with-refresh-cache (cons default-directory (cons 'magit-git-items args))
    (apply orig-fun args)))

(defun magit-prime--rev-parse-cache-prune (now)
  "Remove expired entries from `magit-prime--rev-parse-cache'.
NOW is the current time, as returned by `float-time'."
  (when magit-prime-rev-parse-cache-duration
    (maphash (lambda (key entry)
               (when (> (- now (car entry)) magit-prime-rev-parse-cache-duration)
                 (remhash key magit-prime--rev-parse-cache)))
             magit-prime--rev-parse-cache)))

(defun magit-prime--advice-rev-parse-safe (orig-fun &rest args)
  "Around advice for `magit-rev-parse-safe' caching directory lookups.
ORIG-FUN is the original function.  Calls with \"--show-toplevel\" or
\"--show-cdup\" are served from `magit-prime--rev-parse-cache' when a
fresh entry exists for the current `default-directory'; otherwise
ORIG-FUN is called and its result (even nil) is cached for
`magit-prime-rev-parse-cache-duration' seconds."
  (let ((args (flatten-tree args)))
    (if (or (null magit-prime-rev-parse-cache-duration)
            (not (member args magit-prime--rev-parse-cached-args)))
        (apply orig-fun args)
      (let* ((key (cons (or default-directory "") args))
             (entry (gethash key magit-prime--rev-parse-cache))
             (now (float-time)))
        (if (and entry
                 (<= (- now (car entry)) magit-prime-rev-parse-cache-duration))
            (cdr entry)
          ;; Prune expired entries once the cache grows large, so it
          ;; does not grow unboundedly over a long session.
          (when (> (hash-table-size magit-prime--rev-parse-cache) 256)
            (magit-prime--rev-parse-cache-prune now))
          (let ((value (apply orig-fun args)))
            (puthash key (cons now value) magit-prime--rev-parse-cache)
            value))))))

(defconst magit-prime--advised-functions
  '((magit-git-exit-code . magit-prime--advice-git-exit-code)
    (magit-git-lines . magit-prime--advice-git-lines)
    (magit-git-items . magit-prime--advice-git-items)
    (magit-rev-parse-safe . magit-prime--advice-rev-parse-safe))
  "Alist of Magit functions advised while `magit-prime-mode' is on.
The first three are the functions whose output magit-prime primes but
that upstream Magit does not wrap in `magit--with-refresh-cache'.
`magit-rev-parse-safe' is advised to serve \"--show-toplevel\" and
\"--show-cdup\" lookups from the time-limited
`magit-prime--rev-parse-cache', which matters outside of refreshes
where the refresh cache is inactive.")

(defun magit-prime--add-advices ()
  "Add refresh-cache advices to `magit-prime--advised-functions'."
  (dolist (advice magit-prime--advised-functions)
    (advice-add (car advice) :around (cdr advice))))

(defun magit-prime--remove-advices ()
  "Remove the advices added by `magit-prime--add-advices'."
  (dolist (advice magit-prime--advised-functions)
    (advice-remove (car advice) (cdr advice)))
  (clrhash magit-prime--rev-parse-cache))

;;;###autoload
(define-minor-mode magit-prime-mode
  "Global minor mode to enable magit-prime cache priming.
When enabled, automatically primes caches before Magit refresh
operations.  Also advises `magit-git-exit-code', `magit-git-lines'
and `magit-git-items' so their results are served from the refresh
cache, and advises `magit-rev-parse-safe' so \"--show-toplevel\" and
\"--show-cdup\" lookups are served from a short-lived cache keyed by
`default-directory'; the advices are removed when the mode is
disabled."
  :global t
  :group 'magit-prime
  (if magit-prime-mode
      (progn
        (add-hook 'magit-pre-refresh-hook #'magit-prime-refresh-cache)
        (magit-prime--add-advices))
    (remove-hook 'magit-pre-refresh-hook #'magit-prime-refresh-cache)
    (magit-prime--remove-advices)))

(provide 'magit-prime)

;;; magit-prime.el ends here

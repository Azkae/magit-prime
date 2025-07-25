;;; magit-prime.el --- Prime cache before Magit refresh -*- lexical-binding: t; -*-

;; Author: Romain Ouabdelkader <romain.ouabdelkader@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (magit "3.0.0"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A Magit extension that primes caches in parallel before refresh operations,
;; reducing Magit buffer refresh times.

;; Usage:
;; (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache)

;;; Code:

(require 'magit)

(defvar magit-prime--commands-phase-one
  '(("symbolic-ref" "--short" "HEAD")
    ("describe" "--long" "--tags")
    ("describe" "--contains" "HEAD")
    ("rev-parse" "--git-dir")
    ("rev-parse" "--is-bare-repository")
    ("rev-parse" "--short" "HEAD")
    ("rev-parse" "--short" "HEAD~")
    ("describe" "--contains" "HEAD")
    (t "rev-parse" "--verify" "HEAD")
    (t "rev-parse" "--verify" "refs/stash")
    (t "rev-parse" "--verify" "HEAD~10")))

(defun magit-prime--commands-phase-two ()
  (let* ((branch (magit-get-current-branch))
         (main (magit-main-branch))
         (push-main (magit-get-push-branch main))
         (push-branch (magit-get-push-branch branch))
         (upstream-main (magit-get-upstream-branch main))
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
       ("symbolic-ref" ,(format "refs/remotes/%s/HEAD" primary-remote))
       ("symbolic-ref" ,(format "refs/remotes/%s/HEAD" push-remote))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" branch))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" main))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" push-branch))
       (t "rev-parse" "--verify" ,(concat "refs/tags/" push-main))
       (t "rev-parse" "--verify" ,(format "refs/tags/%s/HEAD" primary-remote))
       (t "rev-parse" "--verify" ,push-branch))
     :test #'equal)))

(defun magit-prime-refresh-cache ()
  "Prime the refresh cache if possible."
  (when (and (or magit-refresh-status-buffer
                 (derived-mode-p 'magit-status-mode))
             magit--refresh-cache
             (not (file-remote-p default-directory)))
    (let ((elapsed
           (benchmark-elapse
             (magit-prime--refresh-cache magit-prime--commands-phase-one)
             (magit-prime--refresh-cache (magit-prime--commands-phase-two)))))
      (when magit-refresh-verbose
        (message "Refresh cached primed in %.3fs" elapsed)))))

(defun magit-prime--refresh-cache (commands)
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
             (let* ((buffer (generate-new-buffer " *magit-prime-refresh-cache*"))
                    (cachep (and (eq (car command) t) (pop command)))
                    (process-environment (magit-process-environment))
                    (default-process-coding-system (magit--process-coding-system)))
               (make-process
                :name (buffer-name buffer)
                :buffer buffer
                :noquery t
                :connection-type 'pipe
                :command (cons magit-git-executable
                               (magit-process-git-arguments command))
                :sentinel
                (lambda (proc _event)
                  (when (eq (process-status proc) 'exit)
                    (when-let* ((buf (process-buffer proc))
                                ((buffer-live-p buf))
                                ((or cachep
                                     (zerop (process-exit-status proc)))))
                      (push (cons (cons repo-path command)
                                  (with-current-buffer buf
                                    (and (zerop (process-exit-status proc))
                                         (not (bobp))
                                         (progn
                                           (goto-char (point-min))
                                           (buffer-substring-no-properties
                                            (point) (line-end-position))))))
                            (cdr magit--refresh-cache)))
                    (cl-decf running))))
               (cl-incf running)
               buffer))
           commands)))

    (with-timeout (1)
      (while (> running 0)
        (sit-for 0.01)
        (accept-process-output)))

    (mapc #'kill-buffer buffers)))

(defconst magit-prime--batch-commands-script "
COMMANDS=\"$1\"

run_command() {
    local id=\"$1\"
    local command=\"$2\"

    local output=$(eval \"$command\" 2>/dev/null | head -n1)
    local exit_code=$?
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
  (let ((vec (tramp-dissect-file-name default-directory))
        (str-commands (magit-prime--format-commands-for-bash commands)))
    (tramp-maybe-send-script vec magit-prime--batch-commands-script
                             "magit_prime__batch_commands")
    (let ((results
           (tramp-send-command-and-read
            vec (format "magit_prime__batch_commands '%s'" str-commands))))
      (dolist (item results)
        (let ((command (nth 0 item))
              (status-code (nth 1 item))
              (output (nth 2 item)))
          ;; Use command, status-code, and output here
          (message "Command: %S, Status: %d, Output: %S"
                   command status-code output))))))

(defun magit-prime--format-commands-for-bash (commands)
  "Convert COMMANDS list to bash script input format.
Each command becomes 'LISP-FORM:git ARGS' where LISP-FORM is the original command."
  (mapconcat
   (lambda (command)
     (let* ((original-command command)
            (cachep (and (eq (car command) t) (pop command)))
            (clean-command (mapcar #'substring-no-properties command))
            (git-command (mapconcat #'shell-quote-argument clean-command " "))
            (lisp-form (format "%S" original-command))
            (line (format "%s:%s %s" lisp-form magit-remote-git-executable git-command)))
       line))
   commands
   "\n"))

(provide 'magit-prime)

;;; magit-prime.el ends here

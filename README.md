# magit-prime

A Magit extension that primes the magit cache in parallel before refresh, reducing refresh times.
Speeds up magit-refresh by ~100ms on my system.

Tramp support is experimental.

## Usage

Magit prime is on MELPA:
```elisp
(use-package magit-prime
  :config
  (magit-prime-mode))
```

## What gets primed

Besides the plain `git` invocations cached by `magit-git-str`/`magit-git-string`
(`rev-parse`, `symbolic-ref`, `describe`, ...), magit-prime also primes:

- `git config --list -z` (the hash table cached under the `config` key)
- `git for-each-ref ...`, `git remote` (cached by `magit-git-lines`)
- `git status --porcelain ...`, `git config --local ...` (cached by `magit-git-items`)
- `git update-index --refresh` and other exit-code checks (cached by `magit-git-exit-code`)
- washed sections: `git diff` (staged/unstaged), `git log` (recent/unpushed/unpulled),
  `git reflog refs/stash` (cached by `magit--git-wash`)
- `git rev-parse --verify refs/tags/<tag>` for tags found in primed log output

The last four require small patches to magit's `lisp/magit-git.el`
(`magit-git-lines`, `magit-git-items`, `magit-git-exit-code` and
`magit--git-wash` are wrapped in `magit--with-refresh-cache`).

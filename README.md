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

The last three are served from the refresh cache through advices that
`magit-prime-mode` adds to `magit-git-lines`, `magit-git-items` and
`magit-git-exit-code` (and removes again when the mode is turned off),
so no changes to Magit itself are needed.

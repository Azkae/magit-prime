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

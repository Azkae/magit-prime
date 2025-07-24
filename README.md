# magit-prime

A Magit extension that primes the magit cache in parallel before refresh, reducing refresh times.
Speeds up magit-refresh by ~100ms on my system.

Tramp support is experimental.

## Usage

```elisp
;; Using straight.el
(use-package magit-prime
  :straight (:type git :host github :repo "Azkae/magit-prime")
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))
```

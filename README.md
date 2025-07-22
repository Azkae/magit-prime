# magit-prime

A Magit extension that primes caches in parallel before refresh operations, reducing Magit buffer refresh times.

## Usage

```elisp
;; Using straight.el
(use-package magit-prime
  :straight (:type git :host github :repo "Azkae/magit-prime")
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))
```

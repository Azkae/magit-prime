# magit-prime

A Magit extension that primes the magit cache in parallel before refresh, reducing Magit buffer refresh times. 
Currently doesn't work on tramp.

## Usage

```elisp
;; Using straight.el
(use-package magit-prime
  :straight (:type git :host github :repo "Azkae/magit-prime")
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))
```

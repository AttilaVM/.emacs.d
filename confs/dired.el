(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

(use-package dired+)

;; TODO Should I use it, anyway? bad performance and svg scaling.
(use-package image+)

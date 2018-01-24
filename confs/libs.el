;; A sane = NOT arcane string manipulation libary
;; see: https://github.com/magnars/s.el
(use-package s)

;; Neat path name handler libary
(use-package f)
;; (unless (package-installed-p 'f)
;;	(package-install 'f))

(use-package dash)

;; An etremly useful package, which makes helm capable to execute asynchronous file operations
;; Even better serve low level for aync functions for hacking Emacs
;; Source: https://github.com/jwiegley/emacs-async
;; (use-package dired-async-mode)

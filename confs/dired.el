(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

(use-package dired+
	:config
	(unbind-key "C-o" dired-mode-map)
	:bind
	(:map dired-mode-map
				("s-0" . diredp-find-file-other-frame))
	)

;; TODO Should I use it, anyway? bad performance and svg scaling.
(use-package image+)

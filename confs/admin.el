(use-package nginx-mode)

(use-package helm-nixos-options)

(use-package daemons
	:config
	(setq daemons-always-sudo t)
	:bind
	(("<insert> 2 d d" . daemons)))

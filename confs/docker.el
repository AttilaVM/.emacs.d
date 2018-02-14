(use-package docker)

(use-package dockerfile-mode
	:bind
	(:map dockerfile-mode-map
				("<insert> e b" . dockerfile-build-buffer)))

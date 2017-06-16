(use-package prolog
	:bind
	(:map prolog-mode-map
				("s-e c" . prolog-compile-file)
				("s-e r" . prolog-compile-region)
				("s-e b" . prolog-compile-buffer)
				("s-e p" . prolog-compile-predicate)
				))

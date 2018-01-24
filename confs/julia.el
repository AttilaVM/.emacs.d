(use-package julia-mode
	:bind
	(:map julia-mode-map
				(("<insert> e r" . julia-repl-send-region-or-line))))

(use-package julia-repl
	:config
	(define-key julia-repl-mode-map (kbd "insert") nil)
	:bind
	(:map julia-repl-mode-map))
(use-package julia-shell)

(use-package julia-mode
	:config
	;;(setq julia-program "remote-julia")
	(julia-repl-set-executable "remote-julia")
	(julia-repl-set-executable "julia-repl")
	:bind
	(:map julia-mode-map
				(("<insert> e r" . julia-repl-send-region-or-line))))

(use-package julia-repl
	:config
	(define-key julia-repl-mode-map (kbd "insert") nil)
	:bind
	(:map julia-repl-mode-map))
(use-package julia-shell)

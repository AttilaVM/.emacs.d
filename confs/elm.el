(use-package elm-mode
	:config
	(setq elm-indent-offset 4)
	;; Add auto completition with elm and dabbrev grouped backends
	(add-hook 'elm-mode-hook
						(lambda ()
							(setq-local company-backends
													(append `((company-elm :separate company-dabbrev)) company-backends))))

	;; Use elm-format after save if available
	(when (executable-find "elm-format")
		(add-hook
		 'before-save-hook
		 (lambda ()
			 (when (string-equal major-mode "elm-mode")
				 (call-interactively 'elm-mode-format-buffer)
				 (condition-case nil
							 (elm-compile-add-annotations)
						 (error nil))
				 ))))

	(define-key elm-compilation-mode-map (kbd "C-o") nil)
	:bind
	(:map elm-mode-map
				("<insert> e b" . elm-compile-buffer)
				("<insert> e f" . elm-compile-file)
				("<insert> j a" . elm-compile-add-annotations)
				("<insert> e l" . elm-repl-load)
				("<insert> j r" . run-elm-iteractive)
				("<insert> e r" . elm-repl-push) ;; Push region
				("<insert> e f" . elm-repl-push-decl) ;; Push declaration
				:map elm-compilation-mode-map
				("o" . compilation-display-error)
				;; ("<insert> e j" . elm-repl-return-to-origin)
				))

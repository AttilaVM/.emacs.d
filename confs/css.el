(use-package css-comb
	:config
	)

(use-package css-eldoc
	:config
	(add-hook 'web-mode-hook
						(turn-on-css-eldoc)
						(lambda ()
							('css-eldoc-enable))))

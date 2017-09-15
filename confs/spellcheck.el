(use-package flyspell
	:config
	;; Disable messaging for misspelled words to avoid slowdown
	(setq flyspell-issue-message-flag nil)

	;; find aspell and hunspell automatically
	(cond
	 ;; try hunspell at first
	 ((executable-find "hunspell")
		(setq ispell-program-name "hunspell")
		(setq ispell-local-dictionary "en_US")
		(setq ispell-local-dictionary-alist
		;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
		;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
		'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
			)))
	 ;; try aspell
	 ((executable-find "aspell")
		(setq ispell-program-name "aspell")
		;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
		(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

	;; Enable spell checking in text mode but disable in change-log and log-edit modes
	(dolist (hook '(text-mode-hook))
		(add-hook hook (lambda () (flyspell-mode 1))))
	(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
		(add-hook hook (lambda () (flyspell-mode -1)))
		)

	;; flyspell-prog-mode only works in comments
	(my/add-hooks '(emacs-lisp-mode-hook
			js2-mode-hook
			python-mode-hook
			c++-mode-hook
			yaml-mode-hook
			json-mode-hook) (lambda ()
						(flyspell-prog-mode)))

	;; FIX hu_HU dictionary does not work
	(defun my/flyspell-toggle-dictionary ()
		"Toggle dictionary between English and Hungarian"
		(interactive)
		(setq ispell-local-dictionary (if (string= ispell-local-dictionary "en_US")
					 "hu_HU"
					 "en_US")))

	(define-key flyspell-mode-map (kbd "C-;") nil)

	:bind (("<insert> c c" . flyspell-check-previous-highlighted-word)
				 ("<insert> c a" . flyspell-check-previous-highlighted-word)
				 ("<insert> c d" . flyspell-check-previous-highlighted-word)
				 ))

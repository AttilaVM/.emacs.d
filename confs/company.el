;; (use-package company-ngram
;;	:config

;; (with-eval-after-load 'company-ngram
;;	; ~/data/ngram/*.txt are used as data
;;	(setq company-ngram-data-dir "~/data/ngram")
;;	(company-ngram-init)
;;	(cons 'company-ngram-backend company-backends)
;;	; or use `M-x turn-on-company-ngram' and
;;	; `M-x turn-off-company-ngram' on individual buffers
;;	;
;;	; save the cache of candidates
;;	(run-with-idle-timer 7200 t
;;											 (lambda ()
;;												 (company-ngram-command "save_cache")
;;												 ))))

;; (require 'company-ngram nil t)

(use-package company-math
	:config
	(defun my/latex-mode-setup ()
		(interactive)
		(setq-local company-backends
								`(company-math-symbols-latex
									:separate
									company-latex-commands
									:separate
									company-math-symbols-unicode
									:separate
									company-dabbrev))))

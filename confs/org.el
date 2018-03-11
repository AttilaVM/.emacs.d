(use-package org
	:bind
	(:map org-mode-map
				("<insert> n i" . org-up-element)
				("<insert> f g" . org-global-cycle)
				))

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)

;;

(setq org-latex-pdf-process (list "texi2dvi -p -b -V %f"))

;; Set up source code fontification
(setq org-src-fontify-natively t)


(define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
(define-key org-mode-map (kbd "<insert> j t s") 'org-set-tags)
(define-key org-mode-map (kbd "<insert> j t t") 'org-tags-view)
(define-key org-mode-map (kbd "<insert> j v p") 'org-toggle-pretty-entities)
(define-key org-mode-map (kbd "<insert> j v l") 'org-toggle-link-display)
(define-key org-mode-map (kbd "<insert> j i l") 'org-insert-link)
(define-key org-mode-map (kbd "<insert> j e p") 'org-latex-export-to-pdf)


(add-hook 'org-mode-hook

					(lambda ()
						(my/latex-mode-setup)
						(linum-mode nil)
						;; make org mode wrap lines
						(visual-line-mode t)))

(use-package helm-org-rifle
	:bind
	(:map org-mode-map
				("<insert> s m m" . helm-org-rifle-current-buffer)))

(use-package org-ref)


;; Babel

(org-babel-do-load-languages
		 'org-babel-load-languages
		 '((ditaa . t)
			 (plantuml . t)
			 (sh . t)
			 (python . t)))
(use-package org-babel-eval-in-repl)
(use-package ob-ipython
	:config
	(setq org-babel-python-command "python3"))

;; graphics

(setq org-ditaa-jar-path (my/find-command-exec-path "ditaa"))
(setq org-plantuml-jar-path (my/find-command-exec-path "plantuml"))
(use-package plantuml-mode)

(require 'org)

;;

(setq org-latex-pdf-process (list "texi2dvi -p -b -V %f"))

;; Set up source code fontification
(setq org-src-fontify-natively t)


(define-key org-mode-map (kbd "<insert> j t s") 'org-set-tags)
(define-key org-mode-map (kbd "<insert> j t t") 'org-tags-view)
(define-key org-mode-map (kbd "<insert> j v p") 'org-toggle-pretty-entities)
(define-key org-mode-map (kbd "<insert> j v l") 'org-toggle-link-display)
(define-key org-mode-map (kbd "<insert> j i l") 'org-insert-link)
(define-key org-mode-map (kbd "<insert> j e p") 'org-latex-export-to-pdf)

(add-hook 'org-mode-hook

					(lambda ()
						(my/latex-mode-setup)
						;; make org mode wrap lines
						(visual-line-mode)))

(use-package org-ref)

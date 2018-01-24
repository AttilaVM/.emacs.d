(require 'org)

;; Set up source code fontification
(setq org-src-fontify-natively t)

(define-key org-mode-map (kbd "<insert> j t s") 'org-set-tags)
(define-key org-mode-map (kbd "<insert> j t t") 'org-tags-view)
(define-key org-mode-map (kbd "<insert> j v p") 'org-toggle-pretty-entities)

(add-hook 'org-mode-hook 'my/latex-mode-setup)

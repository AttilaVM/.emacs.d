(use-package helm-mt
	:config
	(global-set-key (kbd "C-x t") 'helm-mt))

;; TODO do I evev need the term-send-raw function?
(define-key term-raw-map (kbd "C-o") 'nil)
(define-key term-raw-map (kbd "<M-RET>") 'term-send-raw)
(define-key term-raw-map (kbd "<insert>") nil)

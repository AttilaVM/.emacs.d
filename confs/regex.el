(use-package visual-regexp
	:config
	(use-package visual-regexp-steroids
		:bind
		("<insert> r r" . vr/replace)))

;; switch re builder syntax `string` instead of `read`, since it is more convinient
;; source: https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(setq reb-re-syntax 'string)

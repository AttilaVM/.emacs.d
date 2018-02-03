;; SQLite is needed
(use-package helm-dash
	:config
	(setq helm-dash-docsets-path "~/data/dash-docsets/")
	:bind(
				(("<insert> h d d" . helm-dash)
				 ("<insert> h d i" . helm-dash-install-docset)
				 ("<insert> h d u" . helm-dash-update-docset)
				 ("<insert> h d a" . helm-dash-activate-docset))))

(use-package howdoi
	:bind
	(("<insert> h h" . howdoi-query)))

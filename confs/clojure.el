;;; clojure.el --- Clojure specific code

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

;; Associations eldoc config in confs/appearance.el
(use-package cider
	:init
	:config
	(add-hook 'clojure-mode-hook' cider-mode)
	(add-hook 'cider-repl-mode-hook #'company-mode)
	(add-hook 'cider-mode-hook #'company-mode)
	(add-hook 'cider-mode-hook #'eldoc-mode)
	(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
	;; Activate repl pretty print
	(cider-repl-toggle-pretty-printing)
	;; Make cider use figwheel for ClojureScript
	(setq cider-cljs-lein-repl
			"(do (require 'figwheel-sidecar.repl-api)
					 (figwheel-sidecar.repl-api/start-figwheel!)
					 (figwheel-sidecar.repl-api/cljs-repl))")

	(defun my/save-then-eval ()
		""
		(interactive)
		(save-buffer)
		(call-interactively 'cider-eval-buffer))
	:bind
	(:map clojure-mode-map
				;; Save and eval whole buffer without asking questions
				("<insert> j j" . my/save-then-eval)
				;; Map eval to s-e prefix)
				("<insert> e e" . cider-eval-last-sexp) ;; print
				("<insert> j e p" . cider-pprint-eval-last-sexp-to-repl) ;; print
				("<insert> j e b" . cider-eval-buffer)
				("<insert> j e r" . cider-eval-region)
				("<insert> j e i" . cider-eval-print-last-sexp)       ;; insert
				("<insert> j e s" . cider-eval-last-sexp-and-replace) ;; swap
				("<insert> j e m" . cider-macroexpand-1) ;; swap
				("<insert> j e n" . cider-eval-ns-form)  ;; namespace
				("<insert> e p" . cider-pprint-eval-last-sexp-to-repl) ;; print
				("<insert> e b" . cider-eval-buffer)
				("<insert> e r" . cider-eval-region)
				("<insert> e i" . cider-eval-print-last-sexp)       ;; insert
				("<insert> e s" . cider-eval-last-sexp-and-replace) ;; swap
				("<insert> e m" . cider-macroexpand-1) ;; swap
				("<insert> e n" . cider-eval-ns-form)  ;; namespace
				;; Map tests to s-t prefix
				("<insert> j t p" . cider-test-rerun-test) ;; Rerun test at POINT
				;; Map debug features
				("<insert> j d p" . cider-debug-defun-at-point) ;; point
				;; Connect ints
				("<insert> j c c" . cider-jack-in)
				("<insert> j c s" . cider-jack-in-clojurescript)
	))

(use-package helm-cider
	:config
	(add-hook 'cider-mode-hook #'helm-cider-mode))

(use-package elein)
;; clojure.el ends here

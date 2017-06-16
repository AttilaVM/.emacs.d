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
	:bind
	(:map clojure-mode-map
				;; Map eval to s-e prefix)
				("s-e p" . cider-pprint-eval-last-sexp-to-repl) ;; print
				("s-e b" . cider-eval-buffer)
				("s-e r" . cider-eval-region)
				("s-e i" . cider-eval-print-last-sexp)       ;; insert
				("s-e s" . cider-eval-last-sexp-and-replace) ;; swap
				("s-e m" . cider-macroexpand-1) ;; swap
				("s-e n" . cider-eval-ns-form)  ;; namespace
				;; Map tests to s-t prefix
				("s-t p" . cider-test-rerun-test) ;; Rerun test at POINT
				;; Map debug features
				("s-d p" . cider-debug-defun-at-point) ;; point
				;; IDE main
				("s-2 j c" . cider-jack-in)
				("s-2 j s" . cider-jack-in-clojurescript)
	))

(use-package helm-cider
	:config
	(add-hook 'cider-mode-hook #'helm-cider-mode))

(use-package elein)
;; clojure.el ends here

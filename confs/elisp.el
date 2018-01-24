(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(add-to-list 'company-backends
					'(company-elisp company-dabbrev))))


(defun my/eval-region-or-buffer ()
	"Eval region if selected, otherwise eval the buffer"
	(interactive)
	(if (use-region-p)
			(call-interactively 'eval-region)
		(call-interactively 'eval-buffer)))

;; (define-key emacs-lisp-mode-map (kbd "s-e r") 'my/eval-region-or-buffer)
(define-key lisp-mode-map (kbd "s-e r") 'my/eval-region-or-buffer)

(define-key emacs-lisp-mode-map (kbd "<insert> d f") 'edebug-defun)
(define-key lisp-mode-map (kbd "<insert> d f") 'edebug-defun)

(define-key lisp-mode-map (kbd "<insert> d i") 'edebug-mode)
(define-key emacs-lisp-mode-map (kbd "<insert> d i") 'edebug-mode)

(define-key edebug-mode-map (kbd "s") 'edebug-stop)

;; Using ElDoc to automatically show function parameters at point
(add-hook 'emacs-lisp-mode #'eldoc-mode)
(add-hook 'lisp-mode #'eldoc-mode)

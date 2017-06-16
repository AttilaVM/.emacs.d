(defun my/eval-region-or-buffer ()
	"Eval region if selected, otherwise eval the buffer"
	(interactive)
	(if (use-region-p)
			(call-interactively 'eval-region)
		(call-interactively 'eval-buffer)))

(define-key emacs-lisp-mode-map (kbd "s-e r") 'my/eval-region-or-buffer)
(define-key lisp-mode-map (kbd "s-e r") 'my/eval-region-or-buffer)

(define-key emacs-lisp-mode-map (kbd "s-d p") 'edebug-defun)
(define-key lisp-mode-map (kbd "s-d p") 'edebug-defun)

;; Using ElDoc to automatically show function parameters at point
(add-hook 'emacs-lisp-mode #'eldoc-mode)
(add-hook 'lisp-mode #'eldoc-mode)

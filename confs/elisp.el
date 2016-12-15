(defun my/eval-region-or-buffer ()
  "Eval region if selected, otherwise eval the buffer"
  (interactive)
  (if (use-region-p)
      (call-interactively 'eval-region)
    (call-interactively 'eval-buffer)))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'my/eval-region-or-buffer)
(define-key lisp-mode-map (kbd "C-c C-c") 'my/eval-region-or-buffer)

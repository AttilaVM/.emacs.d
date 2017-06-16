;; eshell

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook #'(lambda ()
																(eshell/alias "h" "helm-eshell-history")
																(eshell/alias "c" "clear")
																(eshell/alias "top" "helm-top")
																(eshell/alias "htop" "helm-top")
																(eshell/alias "tree" "my/eshell-tree")))

(use-package eshell-autojump
	:config

	(defun my/clear-eshell-prompt ()
		(interactive)
		(with-current-buffer "*eshell*"
			(call-interactively 'move-end-of-line)
			(call-interactively  'set-mark-command)
			(call-interactively 'eshell-bol)
			(delete-backward-char 1)
			))

	(defun my/eshell-tree ()
		(interactive)
		(call-interactively 'neotree-dir))

	(defun helm-eshell-autojump ()
		(interactive)
		(let ((target-dir (helm :sources (helm-build-sync-source "test"
																			 :candidates (eshell-autojump-candidates))
														:buffer "*helm eshell jump")))
			(when target-dir (with-current-buffer "*eshell*"
				(eshell-return-to-prompt)
				(my/clear-eshell-prompt)
				(insert (concat "cd " target-dir))
				(eshell-send-input))))))



;; Bash completion
(use-package bash-completion
	:config
	(bash-completion-setup))

;; Bash unit testing
(use-package bats-mode
	:bind
	(:map bats-mode-map
	("C-c C-t f" . bats-run-current-file)
	("C-c C-t d" . bats-run-all) ;; Run all in cwd
	("C-c C-t ." . bats-run-current-test)))


(use-package fish-mode
	:config
	(add-hook 'fish-mode-hook (lambda ()
														 (add-hook 'before-save-hook 'fish_indent-before-save))))

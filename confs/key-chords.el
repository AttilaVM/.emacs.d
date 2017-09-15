(use-package key-chord
	:config
	(setq key-chord-two-keys-delay 0.04)

	(require 'key-chord)

	;; Navigate
		(key-chord-define-global "nd" 'move-end-of-line)
		(key-chord-define-global "na" 'move-beginning-of-line)
		(key-chord-define-global "nw" 'beginning-of-buffer)
		(key-chord-define-global "ns" 'end-of-buffer)


	(key-chord-define-global "jf" 'avy-goto-char)
	(key-chord-define-global "jd" 'avy-goto-char-2)
	(key-chord-define-global "jg" 'avy-goto-line)
	(key-chord-define-global "jq" 'goto-last-change)
	(key-chord-define-global "je" 'goto-last-change-reverse)
	;; kill
	(key-chord-define-global "kd" 'kill-word)
	(key-chord-define-global "kf" 'backward-kill-word)
	(key-chord-define-global "ks" 'kill-line)
	(key-chord-define-global "ka" 'kill-whole-line)
	;; insert
	(key-chord-define-global "if" 'yank)
	(key-chord-define-global "id" 'helm-show-kill-ring)

	;; line
	(key-chord-define-global "ls" 'my/line-duplicate-below)
	(key-chord-define-global "lw" 'my/line-duplicate-above)
	;; search
	(key-chord-define-global ";f" 'helm-occur)
	(key-chord-define-global ";d" 'helm-projectile-ag)
	(key-chord-define-global ";e" 'isearch-forward)
	(key-chord-define-global ";q" 'isearch-backward)

	;; common IDE
	;; TODO: I should write a lambda to hook this
	;; (key-chord-define-local  "'e" 'eval-last-sexp)
	(key-chord-define-global  "'f" 'undo)
	(key-chord-define-global  "'d" 'undo-tree-redo)
	(key-chord-define-global  "'s" 'save-buffer)
	(key-chord-define-global  "'q" 'other-window)

	;; open
	(key-chord-define-global  "of" 'helm-find-files)
	(key-chord-define-global  "od" 'helm-projectile-switch-project)
	(key-chord-define-global  "og" 'helm-tramp)
	)

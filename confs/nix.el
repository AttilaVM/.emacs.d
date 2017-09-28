;; nix-shell for given Emacs buffer
(use-package nix-mode)

;; (use-package nix-buffer)

;; nix-buffer does not work, and in my oponioin it has a flowed concept because it relies on nix-build command to mimic the nix-shell command inside a buffer. Why not use the nix-shell command in the first place?
;; (load "/home/attila/.emacs.d/extensions/nix-buffer/nix-buffer.el")

;; my experiment to implement nix-shell inside Emacs.
;; (load "~/.emacs.d/extensions/nix-shell/nix-shell.el")

(use-package nix-sandbox
	:bind
	(:map nix-mode-map
				("<insert> j d" . helm-nixos-options)))

(use-package company-nixos-options)

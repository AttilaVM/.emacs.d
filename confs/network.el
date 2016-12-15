(use-package tramp
  :config
  (progn (setq enable-recursive-minibuffers nil)
	 (my/load-when-readable "~/.emacs.d/proxies.el")
	 (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
	 ;; Prevent tramp from using /dev/null and recreating it as a regular file when history size is reached.
	 (defvar tramp-histfile-override "~/.tramp_history")))

;; Start emacs server for emacs clients
(use-package server
  :config
  (unless (server-running-p)
  (server-start)))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)



(defvar user-emacs-config-file (getenv "EMACS_CONFIG") "path to user alternative config file")

(if user-emacs-config-file
		(load user-emacs-config-file)
	(load "~/.emacs.d/init-old.el"))

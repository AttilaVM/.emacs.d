(toggle-debug-on-error)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")
			 ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))

(package-initialize)
;; Make sure that use-package is installed
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)
;; Make use package automaticly install missing packages
(setq use-package-always-ensure t)

;; User specific information
(load "~/.emacs.d/user.el")

;; functions to make Emacs configuration easier
(load "~/.emacs.d/confs/mylib.el")

;; Define most fundemental Emacs behaviors, appearance and user specific settings
(load "~/.emacs.d/confs/behavior.el")
(load "~/.emacs.d/confs/appearance.el")
(load "~/.emacs.d/confs/helpers.el")
(load "~/.emacs.d/confs/nav.el")
(load "~/.emacs.d/confs/edit.el")
(load "~/.emacs.d/confs/spellcheck.el")
(load "~/.emacs.d/confs/workgroups.el")
(load "~/.emacs.d/confs/ai.el")
(load "~/.emacs.d/confs/term.el")
(load "~/.emacs.d/confs/dired.el")
(load "~/.emacs.d/confs/vc.el")
(load "~/.emacs.d/confs/guide.el")
(load "~/.emacs.d/confs/network.el")

;; Project management and higher capabilities
(load "~/.emacs.d/confs/helm.el")
(load "~/.emacs.d/confs/sunrise-commander.el")

;; Language specific
(load "~/.emacs.d/confs/js.el")
(load "~/.emacs.d/confs/latex.el")
(load "~/.emacs.d/confs/python.el")
(load "~/.emacs.d/confs/typescript.el")
(load "~/.emacs.d/confs/clojure.el")
(load "~/.emacs.d/confs/shell.el")
(load "~/.emacs.d/confs/haskell.el")
(load "~/.emacs.d/confs/web.el")
(load "~/.emacs.d/confs/yaml.el")
(load "~/.emacs.d/confs/data_analysis.el")
(load "~/.emacs.d/confs/ibuffer.el")

(load "~/.emacs.d/confs/kite-mini.el")

;; Multimedia
(when (executable-find "rtorrent")
	(load "~/.emacs.d/confs/mentor.el"))
(when (or (executable-find "vlc") (executable-find "mplayer"))
	(load "~/.emacs.d/confs/emms.el"))

;; Browsing
(when (executable-find "conkeror")
	(load "~/.emacs.d/confs/conkeror.el"))
(when (executable-find "w3m")
(load "~/.emacs.d/confs/w3m.el"))



(load "~/.emacs.d/extensions/liquidsoap-mode.el")

(load "~/.emacs.d/confs/yas.el")
(load "~/.emacs.d/confs/after-save.el")

(load "~/.emacs.d/proxies.el")

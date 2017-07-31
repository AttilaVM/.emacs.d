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

;; Low level deps for elisp programing
(load "~/.emacs.d/confs/libs.el")

;; functions to make Emacs configuration easier
(load "~/.emacs.d/confs/mylib.el")
(load "~/.emacs.d/confs/mapfilters.el")
;; (load "~/.emacs.d/confs/validate.el")

;; System constants
(load "~/.emacs.d/confs/constants.el")

;; Define most fundemental Emacs behaviors, appearance and user specific settings
(load "~/.emacs.d/confs/behavior.el")
(load "~/.emacs.d/confs/appearance.el")
(load "~/.emacs.d/confs/flycheck.el")
(load "~/.emacs.d/confs/tree.el")
(load "~/.emacs.d/confs/helpers.el")
(load "~/.emacs.d/confs/nav.el")
(load "~/.emacs.d/confs/edit.el")
(load "~/.emacs.d/confs/spellcheck.el")
;; (load "~/.emacs.d/confs/workgroups.el")
(load "~/.emacs.d/confs/ai.el")
(load "~/.emacs.d/confs/term.el")
(load "~/.emacs.d/confs/dired.el")
(load "~/.emacs.d/confs/vc.el")
(load "~/.emacs.d/confs/guide.el")
(load "~/.emacs.d/confs/network.el")

;; Project management and higher capabilities
(load "~/.emacs.d/confs/helm.el")
(load "~/.emacs.d/confs/sunrise-commander.el")
(load "~/.emacs.d/confs/realgud.el") ; debugger

;; Language specific
(load "~/.emacs.d/confs/elisp.el")
(load "~/.emacs.d/confs/js.el")
(load "~/.emacs.d/confs/latex.el")
(load "~/.emacs.d/confs/python.el")
(load "~/.emacs.d/confs/typescript.el")
(load "~/.emacs.d/confs/clojure.el")
(load "~/.emacs.d/confs/shell.el")
(load "~/.emacs.d/confs/haskell.el")
(load "~/.emacs.d/confs/web.el")
(load "~/.emacs.d/confs/yaml.el")
(load "~/.emacs.d/confs/maxima.el")
(load "~/.emacs.d/confs/prolog.el")
(load "~/.emacs.d/confs/data_analysis.el")
(load "~/.emacs.d/confs/ibuffer.el")
(load "~/.emacs.d/confs/admin.el") ;; Editing config files and other admin stuffs

;; Devops
(load "~/.emacs.d/devops/core.el")

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

;; System interop
(when (string-equal "Gentoo" my/system-os)
	(load "~/.emacs.d/confs/gentoo.el"))


(load "~/.emacs.d/extensions/liquidsoap-mode.el")

(load "~/.emacs.d/confs/yas.el")
(load "~/.emacs.d/confs/after-save.el")

(load "~/.emacs.d/proxies.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl
	 "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(package-selected-packages
	 (quote
		(pretty-mode realgud relgud howdoi helm-nixos-options nix-mode command-log-mode dired-async-mode dired-async emacs-async s s\.el fish-mode nginx-mode winum npm-mode neotree neo-tree eshell-autojump esehell-autojump bash-completion px helm-cider rainbow-mode indium yaml-mode workgroups web-mode w3m use-package undo-tree tide sunrise-x-mirror sunrise-x-loop sunrise-x-checkpoints stylus-mode strace-mode smartparens show-css scss-mode rainbow-delimiters python-django pug-mode persp-mode pager nodejs-repl mentor markdown-mode magit kite-mini json-mode js2-refactor jade-mode info-buffer import-js image+ ibuffer-tramp ibuffer-projectile hindent highlight-symbol highlight-context-line highlight-blocks hide-region helm-unicode helm-tramp helm-rage helm-projectile helm-mt helm-gtags helm-emms helm-dash helm-cscope helm-c-yasnippet helm-ag haskell-mode gulp-task-runner guide-key grunt goto-chg eslint-fix emmet-mode elpy elein editorconfig discover-my-major dired+ conkeror-minor-mode company-web company-tern company-auctex cider choice-program buffer-move bats-mode avy)))
 '(persp-keymap-prefix "p"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

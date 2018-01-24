;;; helm.el --- Configure helm awesomeness

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(use-package helm
	:config
	;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
	;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
	;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
	(global-unset-key (kbd "C-x c"))
	(global-set-key (kbd "C-c h") 'helm-command-prefix)
	(require 'helm-config)
	(setq enable-recursive-minibuffers t)
	;; make helm adapt to my choices
	(helm-adaptive-mode)
	;; Make helm use the active window for interaction
	(setq
	 ;; Open helm buffer in current winsow
	 helm-split-window-in-side-p           t
	 ;; cylcle throught helm results
	 helm-move-to-line-cycle-in-source     t
	 ;; search for library in `require' and `declare-function' sexp.
	 helm-ff-search-library-in-sexp        t
					; scroll 8 lines other window using M-<next>/M-<prior>
	 helm-scroll-amount                    8
	 ;; Use the recent file, when finding files
	 helm-ff-file-name-history-use-recentf t)
	(helm-mode 1)


	;; Use helm for file finding
	(global-unset-key (kbd "C-x C-f"))
	;; make sure C-h is no longer a prefix key inside a helm buffer
	(define-key helm-map (kbd "C-h") nil)
	:bind
	(
	 ("<insert> x f" . helm-find-files)
	 ("<insert> x d" . dired)
	 ("<insert> b b" . switch-to-buffer)
	 ;; Use helm for command prompt
	 ("M-x" . helm-M-x)
	 ("<insert> <insert>" . helm-M-x)
	 ;; Use helm-buffers-list instead of default helm buffer lister
	 ("s-x b" . helm-buffers-list)
	 ;; get the list of the bookmarks (C-x r m for saving bookmarks)
	 ("C-c p j" . helm-bookmarks)
	 ;; More easier way to acces Emacs's internal "clipboard"
	 ("<insert> i h" . helm-show-kill-ring) ;; as clipboard history
	 ;; Use helm with isearch
	 ("<insert> s s" . helm-occur)
	 ;; resume to previous search
	 ("<insert> s r" . helm-resume)
	 ;; Show kill-ring
	 ("<insert> i h" . helm-show-kill-ring)
	 ;; helm-ls-git
	 ("C-<f6>" . helm-browse-project)
	 ;; helm imenu
	 ("<insert> s i" . helm-imenu)
	 ;; helm-c-source-yasnippet
	 ("<insert> i y" . helm-yas-complete)
	 ;; helm top
	 ("C-x c C-t" . helm-tramp )
	 ;; helm locate
	 ("<insert> s l" . helm-locate)
	 ;; helm woman
	 ("<insert> 1 w" . helm-man-woman)
	 ("<insert> 1 i e" . helm-info-elisp)
	 ("<insert> 1 i m" . helm-info-magit)
	 ("<insert> 1 i z" . helm-info-zsh)
	 ;; ???
	 ("<C-kp-4>" . sm/toggle-showcss)
	 ("<insert> i c" . helm-colors))
	:bind
	(:map helm-map
	("<insert> j j" . helm-select-action)
	("<insert> r" . helm-ff-run-find-file-as-root)
	("<insert> d d" . dired-find-file)
	("<insert> d o" . dired-find-file)
	))

(use-package helm-company
	:config
	(eval-after-load 'company
	'(progn
		 (define-key company-mode-map (kbd "<insert> TAB") 'helm-company)
		 (define-key company-active-map (kbd "<insert> TAB") 'helm-company))))

(use-package helm-c-yasnippet
	:config
	(require `helm-c-yasnippet)
	(setq helm-yas-space-match-any-greedy t))


(use-package projectile
	:config
	(use-package helm-projectile
		:config
		(helm-projectile-on))
	(projectile-global-mode)
	(setq projectile-globally-ignored-directories
				(append '(
				"out"
				"target"
				"venv"
				"node_modules"
				) ))
	(setq projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld")
	(setq projectile-enable-caching t)
	(use-package helm-ag)
	:bind
	("<insert> p p" . helm-projectile-switch-project)
	("<insert> p +" . projectile-add-known-project)
	("<insert> p f" . helm-projectile-find-file)
	("<insert> p b" . helm-projectile-switch-to-buffer)
	("<insert> p i" . projectile-invalidate-cache)
	;; Extreamly fast mehotd search in all recent project files.
	("<insert> p a" . helm-projectile-ag)
	("<insert> s p" . helm-projectile-ag)
	("<insert> x p" . projectile-save-project-buffers)
	;; Very slow, when many project is present.
	("<insert> p g" . helm-projectile-find-file-in-known-projects)
	;; Project level replace, what can go wrong?
	("<insert> p r t" . projectile-replace)
	("<insert> p r r" . projectile-replace-regexp))

(use-package helm-gtags
	:config
	(my/add-hooks '(dired-mode-hook
			eshell-mode-hook
			c-mode-hook
			c++-mode-hook
			asm-mode-hook) 'helm-gtags-mode)
	;; Set up helm-gtags for GNU GLOBAL source tagging system
	;; FIXME \c-cg can only be defined here, throw error if assigned in control.el
	(setq
	 helm-gtags-ignore-case t
	 helm-gtags-auto-update t
	 helm-gtags-use-input-at-cursor t
	 helm-gtags-pulse-at-cursor t
	 helm-gtags-prefix-key "\C-cg"
	 helm-gtags-suggested-key-mapping t
	 )
	:bind
	(:map helm-gtags-mode-map
	("C-c g a" . helm-gtags-tags-in-this-function)
	("M-." . helm-gtags-dwim)
	("M-," . helm-gtags-pop-stack)))

;; Use helm to inser ASCII arts
(use-package helm-rage
	:bind
	(("<insert> i r" . helm-rage)))

;; use helm to inser unicode characters
(use-package helm-unicode
	:bind
	(("<insert> i u" . helm-unicode)))

(use-package helm-tramp
	:config
	(setq tramp-default-method "ssh")
	(defalias 'exit-tramp 'tramp-cleanup-all-buffers)
	:bind
	(("C-x c t" . helm-tramp )))


;;; helm.el ends here

;;; helm.el --- Configure helm awesomeness

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(use-package helm
  :config
  (progn (require 'helm-config)
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
	 (helm-mode 1)))


(use-package helm-c-yasnippet
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package projectile
  :config
  (progn (setq projectile-completion-system 'helm)
	 (helm-projectile-on))
  :bind
  ("C-c p p" . helm-projectile-switch-project)
  ("C-c p f" . helm-projectile-find-file)
  ("C-c p b" . helm-projectile-switch-to-buffer)
  ;; Extreamly fast mehotd search in all recent project files.
  ("C-c p a" . helm-projectile-ag)
  ("C-c p s" . projectile-save-project-buffers)
  ;; Very slow, when many project is present.
  ("C-c p g" . helm-projectile-find-file-in-known-projects)
  ;; Project level replace, what can go wrong?
  ("C-c p r n" . projectile-replace)
  ("C-c p r r" . projectile-replace-regexp))


(use-package helm-cscope
  :config
  (my/add-hooks '(c-mode-hook c++-mode-hook) 'helm-cscope))

(use-package helm-gtags
  :config
  (my/add-hooks '(dired-mode-hook
		  eshell-mode-hook
		  c-mode-hook
		  c++-mode-hook
		  asm-mode-hook) 'helm-gtags-mode )
  ;; Set up helm-gtags for GNU GLOBAL source tagging system
  ;; FIXME \c-cg can only be defined here, throw error if assigned in control.el
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   ))

;; Use helm to inser ASCII arts
(use-package helm-rage
  :bind
  (("s-i m" . helm-rage)))

;; use helm to inser unicode characters
(use-package helm-unicode
  :bind
  (("s-i u" . helm-unicode)))




;;; helm.el ends here

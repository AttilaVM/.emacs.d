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
   ("C-x C-f" . helm-find-files)
   ;; Use helm for command prompt
   ("M-x" . helm-M-x)
   ;; Use helm-buffers-list instead of default helm buffer lister
   ("s-x b" . helm-buffers-list)
   ;; get the list of the bookmarks (C-x r m for saving bookmarks)
   ("C-c p j" . helm-bookmarks)
   ;; More easier way to acces Emacs's internal "clipboard"
   ("M-y" . helm-show-kill-ring)
   ;; Use helm with isearch
   ("s-s" . helm-occur-from-isearch)
   ;; helm-ls-git
   ("C-<f6>" . helm-browse-project)
   ;; helm-c-source-yasnippet
   ("C-c y" . helm-yas-complete)
   ;; ???
   ("<C-kp-4>" . sm/toggle-showcss)
   ("<C-kp-2>" . helm-colors))

  :bind
  (:map helm-map
	("C-z" . helm-select-action)
	("C-r" . helm-ff-run-find-file-as-root)))


(use-package helm-c-yasnippet
  :config
  (setq helm-yas-space-match-any-greedy t))


(use-package projectile
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld")
  (setq projectile-enable-caching t)
  (use-package helm-projectile
    :config
    (helm-projectile-on))
  (use-package helm-ag)
  :bind
  ("C-c p p" . helm-projectile-switch-project)
  ("C-c p +" . projectile-add-known-project)
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
  (("s-i m" . helm-rage)))

;; use helm to inser unicode characters
(use-package helm-unicode
  :bind
  (("s-i u" . helm-unicode)))




;;; helm.el ends here

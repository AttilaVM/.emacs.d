;;; behahavior.el --- Configure Emacs basic behavior

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: basic behavior
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(global-unset-key (kbd "<insert>"))

;; Set variable binding limit
(setq max-specpdl-size 10000)
;; Set recursion limit
(setq max-lisp-eval-depth 10000)

;; y or n for verification instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; Make Emacs delete selected region on insertion event, similary to other editors.
(delete-selection-mode -1)
;; Do not load outdated byte code
(setq load-prefer-newer t)
;; Save opened session (opened files, window configuration...)
;; (desktop-save-mode 1)

;; bidi display reordering makes Emacs significantly slower.
;; see: http://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq bidi-display-reordering nil)

;; Toggle-in server mode for CLI Emacs clients
'(server-mode t)

;; Set defalult-directory to .emacs.d to init helm-projectile at startup.
(setq default-directory "~/.emacs.d")

;; BUG: Doesn't seem to work
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; Use zsh for term
(when (file-executable-p "/bin/zsh")
	(setq explicit-shell-file-name "/bin/zsh"))

;; Set startup buffer
(when (file-exists-p user/home-buffer)
	(progn
		(setq initial-buffer-choice user/home-buffer)
		(global-set-key (kbd "<XF86HomePage>") 'my/change-to-home-buffer)))

;; Remove trailing white spaces on save
(defvar whitespace-cleanup-on-save t)
(add-hook 'before-save-hook
		(lambda ()
			(if whitespace-cleanup-on-save (whitespace-cleanup))))

;; Automaticly make executable script fils
(add-hook 'after-save-hook
	'executable-make-buffer-file-executable-if-script-p)

;; I do not want to put Emacs into the background, when I am not in the terminal
(when (display-graphic-p)
	(progn
		(global-unset-key (kbd "C-z"))
		(global-unset-key (kbd "C-x C-z"))))

;; Set up emacs as a pager .bashrc or zshrc should be modified!
(use-package pager)

;; Set Emacs default browser
(setq gnus-button-url 'browse-url-generic
			browse-url-browser-function gnus-button-url
			browse-url-generic-program (my/get-first-match
					'("chromium" "conkeror" "vivaldi" "google-chrome-stable" "firefox")
					(function (lambda (command)
								(if (executable-find command)
							command
						nil)))))

;; Do not reuse info buffer, but open a new one
(use-package info-buffer
	:bind (("C-h i" . info-buffer)))

(use-package highlight-context-line
	:config
	(highlight-context-line-mode))

(use-package winum)

;; (use-package persp-mode
;;	:config
;;	(setq wg-morph-on nil) ;; switch off animation
;;			(setq persp-autokill-buffer-on-remove 'kill-weak)
;;			(add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

(use-package command-log-mode)

(use-package lispy)

;; do I need it?
(use-package parinfer
	:ensure t
	:bind
	(("C-," . parinfer-toggle-mode))
	:init
	(progn
		(setq parinfer-extensions
					'(defaults       ; should be included.
						pretty-parens  ; different paren styles for different modes.
						;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
						smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
						smart-yank))   ; Yank behavior depend on mode.
		;; (add-hook 'clojure-mode-hook #'parinfer-mode)
		;; (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
		;; (add-hook 'common-lisp-mode-hook #'parinfer-mode)
		;; (add-hook 'scheme-mode-hook #'parinfer-mode)
		;; (add-hook 'lisp-mode-hook #'parinfer-mode)
		))



;; Open disk image files in the hex editor
(add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))

;; Always use bash for tramp ssh sessions
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;;; behavior.el ends here

;; edit

(global-set-key (kbd "<insert> k l l") 'kill-whole-line)
(global-set-key (kbd "<insert> k l k") 'kill-line)

;; navigate
(global-set-key (kbd "<insert> n w") 'beginning-of-buffer)
(global-set-key (kbd "<insert> n s") 'end-of-buffer)

(global-set-key (kbd "<insert> b K") 'kill-matching-buffers)
(global-set-key (kbd "<insert> b r") 'rename-file-and-buffer)
(global-unset-key (kbd "M-%"))
(global-set-key (kbd "<insert> r t") 'query-replace)
(global-unset-key (kbd "C-M-%"))
(global-set-key (kbd "<insert> r r") 'query-replace-regexp)
(global-unset-key (kbd "C-x k"))
(global-set-key (kbd "<insert> b k") 'kill-buffer)
(global-unset-key (kbd "C-x s"))
(global-set-key (kbd "<insert> x a") 'save-some-buffers)
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "<insert> x x") 'helm-find-files)
(global-set-key (kbd "<insert> i i") 'yank)
(global-unset-key (kbd "C-c C-c"))
(global-set-key (kbd "<insert> <escape>") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-s"))
(global-set-key (kbd "<insert> x x") 'save-buffer)
(global-set-key (kbd "<insert> SPC SPC") 'set-mark-command)

;; Selection
(global-unset-key (kbd "C-x h"))
(global-set-key (kbd "<insert> SPC h") 'mark-whole-buffer)

;; Window and frame mamagement
(global-unset-key (kbd "C-x 2"))
(global-set-key (kbd "<insert> 3 v") 'split-window-below)
(global-unset-key (kbd "C-x 3"))
(global-set-key (kbd "<insert> 3 h") 'split-window-horizontally)
(global-unset-key (kbd "C-x 1"))
(global-set-key (kbd "<insert> 3 e") 'delete-other-windows) ;; expand active window
(global-unset-key (kbd "C-x 0"))
(global-set-key (kbd "<insert> 3 k") 'delete-window) ;; expand active window

;; Emacs lisp
(define-key emacs-lisp-mode-map (kbd "<insert> e e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<insert> e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "<insert> e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "<insert> e t") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "<insert> j e e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<insert> j e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "<insert> j e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "<insert> j e t") 'eval-defun) ;; Eval top level sexpr

(global-set-key (kbd "<insert> i p r") 'my-insert-file-name-relative)
(global-set-key (kbd "<insert> i p a") 'my-insert-file-name-absolute)

(global-set-key (kbd "<insert> 1 p") 'package-list-packages)

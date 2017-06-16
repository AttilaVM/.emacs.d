;;; behahavior.el --- Configure Emacs basic behavior

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: basic behavior
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

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
	(setq explicit-shell-file-name "bin/zsh"))

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
					'("conkeror" "vivaldi" "google-chrome-stable" "chromium" "firefox")
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

(use-package persp-mode
	:config
	(setq wg-morph-on nil) ;; switch off animation
			(setq persp-autokill-buffer-on-remove 'kill-weak)
			(add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;; Open disk image files in the hex editor
(add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))

;; Always use bash for tramp ssh sessions
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;;; behavior.el ends here

(global-set-key (kbd "s-g b k") 'kill-matching-buffers)
(global-set-key (kbd "s-g b n") 'rename-file-and-buffer)

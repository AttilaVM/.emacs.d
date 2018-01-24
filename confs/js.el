;; JavaScript IDE capabilities

(defun my/npm-install-required (save-flag)
	(interactive
	 (list
		(completing-read "Choose one: " '("--save" "--save-dev" ""))))
			(let* ((beg (region-beginning))
						 (end (region-end))
						 (selection (buffer-substring-no-properties beg end)))
				(->> selection
						 (s-match-strings-all "['\"].*['\"]")
						 (mapcar 'car)
						 (s-join " ")
						 (s-concat "npm i " save-flag " ")
						 (async-shell-command))))

(defun my/npm-install-required-dev ()
	(interactive)
			(let* ((beg (region-beginning))
						 (end (region-end))
						 (selection (buffer-substring-no-properties beg end)))
				(->> selection
						 (s-match-strings-all "['\"].*['\"]")
						 (mapcar 'car)
						 (s-join " ")
						 (s-concat "npm i --save-dev ")
						 (async-shell-command))))

(use-package js2-mode
	:config
	;; Allow custom indentation, like comma first style
	;; (setq js2-bounce-indent-p t)
	;; (setq js2-auto-indent-p nil)
	;; Basic indentation to 2 spaces
	(setq js2-basic-offset 2)
	(setq js2-skip-preprocessor-directives t)

	(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
	(setq js2-highlight-level 3)
	;; Configure imenu for js2-mode
	(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

	(use-package eslint-fix
		:config
	;; automaticly fix styling with eslint on save
	(eval-after-load 'js2-mode
		'(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))))

(use-package jsfmt
	:config
	(when (executable-find "jsfmt")
		(add-hook
		 'before-save-hook
		 (lambda ()
			 (when (string-equal major-mode "js2-mode")
				 (jsfmt-before-save))))))

;; Configure refactoring
(use-package js2-refactor
	:config
	(add-hook 'js2-mode-hook 'js2-refactor-mode))

;; tern-mode for IDE features like code completition, jump to definition etc... it requires a tern server
(use-package tern
	:config

	(defun my/tern-delete-process ()
	(interactive)
	(delete-process "Tern"))

	(use-package tern-auto-complete)
	(when (executable-find "tern")
		(add-hook 'js-mode-hook (lambda () (tern-mode t))))
	(eval-after-load 'tern
		'(progn
			 (require 'tern-auto-complete)
			 (tern-ac-setup)))
	;; tern auto completion
	(autoload 'tern-mode "tern.el" nil t))

(use-package json-mode
	:config
	;; Hook json-mode to .jscsrc
	(add-to-list 'auto-mode-alist '("\\.jscsrc\\'" . json-mode)))

;; Set up company mode for tern
(use-package company-tern
	:config
	(eval-after-load 'company
		'(add-to-list 'company-backends 'company-tern)))

;; Abality to run nodejs REPL inside emacs
(use-package nodejs-repl)

(use-package import-js
	:config
	(defun my/import-js-choose-directory (directory)
		"sample that uses interactive to get a directory"
		(interactive (list (read-directory-name "js project root? "
							(file-name-directory buffer-file-name))))
		(setq import-js-project-root directory)
		(message "You chose %s." directory)))


(use-package gulp-task-runner)
(use-package grunt)

(use-package indium
	:config
	(add-hook 'js-mode-hook #'indium-interaction-mode)

	:bind
	(:map indium-interaction-mode-map
				("<insert> j c c" . indium-run-chrome)
				("<insert> j c n" . indium-run-chrome)
				("s-j" . company-indium-repl)))

;; After transcendent emacs-helm error indium mode does not load these files automaticly
(require 'indium)

	;; (load "/home/attila/.emacs.d/elpa/indium-20170429.1206/indium-chrome.el")
	;; (load "/home/attila/.emacs.d/elpa/indium-20170429.1206/indium-nodejs.el")

(use-package npm-mode)

(define-key js2-mode-map (kbd "<insert> j i s") 'my/npm-install-required)
(define-key js2-mode-map (kbd "<insert> j i d") 'my/npm-install-required-dev)

(define-key indium-repl-mode-menu (kbd "<insert> j i s") 'my/npm-install-required)
(define-key indium-repl-mode-map (kbd "<insert> j i d") 'my/npm-install-required-dev)

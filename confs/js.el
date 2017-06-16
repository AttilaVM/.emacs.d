;; JavaScript IDE capabilities
(use-package js2-mode
	:config
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

;; Configure refactoring
(use-package js2-refactor
	:config
	(add-hook 'js2-mode-hook 'js2-refactor-mode))

;; tern-mode for IDE features like code completition, jump to definition etc... it requires a tern server
(use-package tern
	:config
	(add-hook 'js-mode-hook (lambda () (tern-mode t)))
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
	("s-j" . company-indium-repl)))

;; After transcendent emacs-helm error indium mode does not load these files automaticly
(require 'indium)

	;; (load "/home/attila/.emacs.d/elpa/indium-20170429.1206/indium-chrome.el")
	;; (load "/home/attila/.emacs.d/elpa/indium-20170429.1206/indium-nodejs.el")

(use-package npm-mode)

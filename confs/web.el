;;; web.el --- Configure varios non-js front end development functionalities

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: web frontend css stylus html
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

;; There is a better stylus mode which is not available on MELPA
;; Try to load it first.

;; Set up web mode fore file extensions

(use-package company-web
	:config)

;; (require company-web-html)
(use-package emmet-mode
	:bind
	(:map emmet-mode-keymap
	("s-<tab>" . emmet-expand-line)))

(use-package web-mode
	:config
	(add-hook 'web-mode-hook
						(lambda ()
							(setq-local company-backends
													(append '((company-css
																		 :separate
																		 company-web-html
																		 :separate
																		 company-dabbrev))))))
	;; turn on pair tag highlight for html
	(web-mode-toggle-current-element-highlight)
	(add-hook 'web-mode-hook (lambda ()
					 (set (make-local-variable 'company-backends) '(company-web-html))
					 (company-mode t)
					 (emmet-mode t)))
	;;web-mode snippets
	(setq web-mode-extra-snippets
	'(("djhtml" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
		("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
				("debug" . ("<?php error_log(__LINE__); ?>"))))))
	;; Associate web mode with the following extensions
	(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))
	(add-to-list 'auto-mode-alist '("\\.liq\\'" . liquidsoap-mode)))


(use-package stylus-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))
	(add-hook 'stylus-mode-hook
						(lambda ()
							(setq-local company-backends
													(append '((company-css
																		 :separate
																		 company-dabbrev)))))))


(use-package scss-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))


(use-package jade-mode
	:config
	(add-hook 'jade-mode-hook
						(lambda ()
							(setq-local company-backends
													(append '((company-web-jade
																		 :separate
																		 company-dabbrev)))))))


(use-package show-css
	:config
	;;Show css properties of an html element
	(autoload 'showcss-mode "show_css"
		"Display the css of the class or id the cursor is at" t)
	;; toggle css show
	(defun sm/toggle-showcss()
		"Toggle showcss-mode"
		(interactive)
		(if (derived-mode-p
	 'html-mode
	 'nxml-mode
	 'nxhtml-mode
	 'web-mode
	 'handlebars-mode)
	(showcss-mode 'toggle)
			(message "Not in an html mode"))))

;; Markdown mode-line
(use-package markdown-mode
	:bind
	(:map markdown-mode-map
				("<insert> j t 1" . markdown-insert-header-atx-1)
				("<insert> j t 2" . markdown-insert-header-atx-2)
				("<insert> j t 3" . markdown-insert-header-atx-3)
				("<insert> j t 4" . markdown-insert-header-atx-4)
				("<insert> j t 5" . markdown-insert-header-atx-5)
				("<insert> j t 6" . markdown-insert-header-atx-6)
				("<insert> j t !" . markdown-insert-header-setext-1)
				("<insert> j t @" . markdown-insert-header-setext-1)
				("<insert> j a l" . markdown-insert-inline-link-dwim)
				("<insert> j a L" . markdown-insert-reference-link-dwim)
				("<insert> j a f" . markdown-insert-footnote)
				("<insert> j c c" . markdown-insert-code)
				("<insert> j c b" . markdown-insert-gfm-code-block)
				("<insert> j j b" . markdown-insert-blackquote)
				("<insert> j j p" . markdown-insert-gfm-code-block)
				("<insert> j j i" . markdown-insert-italic)
				("<insert> j j k" . markdown-insert-kbd)
				))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(use-package markdown-preview-eww)

(use-package simple-httpd
	:bind
	("<insert> 2 h" . httpd-serve-directory))

(use-package skewer-mode)

;;; web.el ends here

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)
;; Make sure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; Make use package automaticly install missing packages
(setq use-package-always-ensure t)






;; Enable spell checking in text mode but disable in change-log and log-edit modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))





(defvar loaded-modes '("~/emacs.d/init.el") "Store loaded hooked modes to prevent reloding them evrery time, when the mode activates" )





;; functions to make Emacs configuration easier
(load "~/.emacs.d/confs/mylib.el")

;; Define most fundemental Emacs behaviors, appearance and user specific settings
(load "~/.emacs.d/confs/behavior.el")
(load "~/.emacs.d/user.el")
(load "~/.emacs.d/confs/appearance.el")

(load "~/.emacs.d/confs/helpers.el")
(load "~/.emacs.d/confs/nav.el")
(load "~/.emacs.d/confs/edit.el")
(load "~/.emacs.d/confs/vc.el")
(load "~/.emacs.d/confs/guide.el")
(load "~/.emacs.d/confs/network.el")

;; Language specific
(load "~/.emacs.d/confs/clojure.el")
(load "~/.emacs.d/confs/data_analysis.el")
(load "~/.emacs.d/confs/ibuffer.el")

(load "~/.emacs.d/confs/kite-mini.el")

;; Project management and higher capabilities
(load "~/.emacs.d/confs/helm.el")

(ace-popup-menu-mode 1)





;; Set up emacs as a pager .bashrc or zshrc should be modified!
(require 'pager)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   (quote
    (latex-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode)))
 '(cider-cljs-lein-repl
   "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(helm-ack-auto-set-filetype nil)
 '(helm-ack-thing-at-point (quote symbol))
 '(js2-imenu-enabled-frameworks nil)
 '(org-agenda-files (quote ("~/Documents/mytime.org"))))

;; Search in css/scss selectors with helm

(require 'auto-complete)

;; Rainbow parenthese
(add-hook 'emacs-lisp-mode-hook' rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook' rainbow-delimiters-mode)
(add-hook 'js2-mode-hook' rainbow-delimiters-mode)


(package-install 'flycheck)

(global-flycheck-mode)

(require 'flyspell)

;; Set up source code fintification
(setq org-src-fontify-natively t)

(require 'undo-tree)


;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)



;; Use company with Clang
(require 'cc-mode)
(setq company-backends (delete 'company-semantic company-backends))

;; Set up header completition
(add-to-list 'company-backends 'company-c-headers)

;; Open .h files as c++ files FIXME: Automatic C and C++ distinction
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Haskell confing
(add-hook 'haskell-mode-hook #'hindent-mode)

;; Markdown mode-line
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; Set up web mode fore file extensions
(require 'web-mode)
;; turn on pair tag highlight for html
(web-mode-toggle-current-element-highlight)

(require 'company-web-html)
(require 'emmet-mode)
(add-hook 'web-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-web-html))
			  (company-mode t)
			  (emmet-mode t)))

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
    (message "Not in an html mode")))

;; Use Syntactically Awesome Stylesheets
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))

(load "~/.emacs.d/confs/js.el")
(load "~/.emacs.d/confs/latex.el")
(load "~/.emacs.d/confs/python.el")

(load "~/.emacs.d/extensions/liquidsoap-mode.el")


(require 'impatient-mode)


;; Toggle-in server mode for CLI Emacs clients
'(server-mode t)

(require 'yaml-mode)
;;  Unlike python-mode, this mode follows the Emacs convention of not binding the ENTER key to `newline-and-indent'.  To get this behavior, add the key definition to `yaml-mode-hook':
(add-hook 'yaml-mode-hook
	  '(lambda ()
	(define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;C customization

; start flymake-google-cpplint-load
; let's define a function for flymake initialization
;; (defun my:flymake-google-init ()
;;   (require 'flymake-google-cpplint)
;;   (custom-set-variables
;;    '(flymake-google-cpplint-command "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/cpplint"))
;;   (flymake-google-cpplint-load)
;; )
;; (add-hook 'c-mode-hook 'my:flymake-google-init)
;; (add-hook 'c++-mode-hook 'my:flymake-google-init)

;; (executable-find "cpplint.py") ; => "/usr/local/bin/cpplint.py"

;; (custom-set-variables
;;  '(flycheck-googlelint-verbose "3")
;;  '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
;;  '(flycheck-googlelint-root "project/src")
;;  '(flycheck-googlelint-linelength "120"))
;;-----------------Custom functions-----------------

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )

;; Basic math operations on number under point
(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
	  (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
	  (error "No number at point"))
      (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun my-insert-file-name (filename &optional args)
    "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
    ;; Based on insert-file in Emacs -- ashawley 20080926
    (interactive "*fInsert file name: \nP")
    (cond ((eq '- args)
	   (insert (file-relative-name filename)))
	  ((not (null args))
	   (insert (expand-file-name filename)))
	  (t
	   (insert filename))))

(defun my-yas-insert-file-name (dir)
  "Insert absolute path inside yassnipets from given directory"
  (let ((default-directory dir))
    (unless yas-modified-p
      (expand-file-name (completing-read "File: " 'read-file-name-internal)))))

;;-----------------Tweaks---------------------------
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

;;-----------------Custom variables------------------




;;-------------web-mode snippets------------------------
(setq web-mode-extra-snippets
      '(("djhtml" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
	("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
		  ("debug" . ("<?php error_log(__LINE__); ?>"))))
       ))



;;----------My functions--------------------

(defun show-file-path ()
  (interactive)
  (message (buffer-file-name)))

(setq grab-screen-color-mode "html")

(defun my-newline-below()
  "Jum to the end of the line and inser a linebreak"
  (interactive)
  (move-end-of-line nil)
  (newline))

(defun my-newline-above()
  "Insert a new line above the point"
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (forward-line -1))

(defun grab-screen-color-mode-set (mode)
  "Set up the insertion format of grab-screen-color"
  (interactive
   (list (completing-read "color insertion mode: " '("html" "rgb"))))
  (setq grab-screen-color-mode mode)
  )

(defun grab-screen-color ()
  "Call grabc to pick a color from the screen in html code to the buffer"
  (interactive)
  (let ((grabc-output (shell-command-to-string "grabc")))
    (if (equal grab-screen-color-mode "html")
	(insert (substring grabc-output 0 (string-match "\n" grabc-output))))
    (if (equal grab-screen-color-mode "rgb")
	(insert
	 (concat "rgba(" (replace-regexp-in-string "\n" " " (substring grabc-output (string-match "\n" grabc-output)) -1) ", 1)")))

    )
  )



;; Taken from https://www.emacswiki.org/emacs/RevertBuffer
(defun my/revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	  (revert-buffer t t t) )))
    (message "Refreshed open files.") )


;; Copied from https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org



(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
	(setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))





;; load keybindings
(load "~/.emacs.d/confs/yas.el")
(load "~/.emacs.d/confs/after-save.el")
(load "~/.emacs.d/control.el")

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

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
(add-to-list 'auto-mode-alist '("\\.liq\\'" . liquidsoap-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

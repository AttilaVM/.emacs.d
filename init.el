(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)

(load "~/.emacs.d/user.el")
;;----------------Appearance--------------------

;; Disable menu- and toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Set global font type
(add-to-list 'default-frame-alist '(font . "inconsolata-12" ))
(set-face-attribute 'default t :font "inconsolata-12" )

;; Enable line numbers globally
(global-linum-mode t)

;; Show parentheses
(show-paren-mode 1)

;; Set marker color to green
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#009900")

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg: 

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property 
                            "WM_HINTS" frame "WM_HINTS" 
                            source nil t) nil))
         (flags (car wm-hints)))
    ; (message flags)
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention. 

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))

;;-----------------Controls-----------------------
;; y or n for verification instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)



;;-----------------Repositories------------------

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))


;;-----------------Global Function------------------

;; Set defalult-directory to .emacs.d to init helm-projectile at startup.
(setq default-directory "~/.emacs.d")

;; Set startup buffer
(setq initial-buffer-choice "~/Documents/reminder.org")

;; Enable spell checking in text mode but disable in change-log and log-edit modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Remove trailing white spaces on save
(defvar whitespace-cleanup-on-save t)
(setq whitespace-cleanup-on-save nil)
(add-hook 'before-save-hook
	  (lambda ()
	    (if whitespace-cleanup-on-save (whitespace-cleanup))))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;;----------------Packages-------------------------

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

(defvar loaded-modes '("~/emacs.d/init.el") "Store loaded hooked modes to preven reloding them evrery time, when the mode activates" )

(defun layer-loader (mode-file &optional control-init)
  "Load hooked mode config"
  (unless (memq mode-file loaded-modes)
      (message "load")
  	(load mode-file)
  	(funcall control-init)
  	(add-to-list 'loaded-modes mode-file)
  	))


;;Auto-complete is a dependency of yasnipper
(package-initialize)

(load "~/.emacs.d/confs/ibuffer.el")
(load "~/.emacs.d/confs/guide-key.el")
(load "~/.emacs.d/confs/kite-mini.el")

(require 'autopair)
(autopair-global-mode)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'hexl)


;; Configure tramp hosts
(require 'tramp)
(setq enable-recursive-minibuffers nil)
(load "~/.emacs.d/proxies.el")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;; Prevent tramp from using /dev/null and recreating it as a regular file when history size is reached.
(defvar tramp-histfile-override "~/.tramp_history" "Prevent tramp from using /dev/null and recreating it as a regular file when history size is reached.")

;; Start emacs server for emacs clients
(require 'server)
(unless (server-running-p)
  (server-start))

;; Set up emacs as a pager .bashrc or zshrc should be modified!
(require 'pager)

; go to the last change
(require 'goto-chg)

;; Highlight TODO, FIXME and BUG in comments 
(require 'fic-ext-mode)
(defun add-something-to-mode-hooks (mode-list something)
  "helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

(add-something-to-mode-hooks '(c++ tcl emacs-lisp python text markdown latex js latex) 'fic-ext-mode)

;; configure magit
(require 'magit)

;; Avy
(require 'avy)
(avy-setup-default)

;; Helm
(require 'helm)
(require 'helm-config)

(setq enable-recursive-minibuffers t)

;; Make helm adapt for frequently selected results
(helm-adaptive-mode)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)

(yas-global-mode 1)

;; Use helm to explore a git project-am
(require 'helm-ls-git)

;; Basic projectile set up
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; helm-ack
(require 'helm-config)
(require 'helm-ack)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(ac-modes
   (quote
    (latex-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode)))
 '(helm-ack-auto-set-filetype nil)
 '(helm-ack-thing-at-point (quote symbol))
 '(js2-imenu-enabled-frameworks nil)
 '(org-agenda-files (quote ("~/Documents/mytime.org"))))

;; Search in css/scss selectors with helm
(add-to-list 'load-path "~/.emacs.d/extensions/helm-css-scss")
(require 'helm-css-scss)
;; Allow comment inserting depth at each end of a brace
(setq helm-css-scss-insert-close-comment-depth 2)
;; If this value is t, split window appears inside the current window
(setq helm-css-scss-split-with-multiple-windows nil)
;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-css-scss-split-direction 'split-window-vertically)

(require 'auto-complete)

;; Rainbow parenthese
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook' rainbow-delimiters-mode)
(add-hook 'js2-mode-hook' rainbow-delimiters-mode)


(package-install 'flycheck)

(global-flycheck-mode)

(require 'flyspell)

;; Set up source code fintification
(setq org-src-fontify-natively t)

(require 'undo-tree)
(global-undo-tree-mode)

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'helm-cscope)

;; Enable helm-cscope-mode
(add-hook 'c-mode-hook 'helm-cscope-mode)
(add-hook 'c++-mode-hook 'helm-cscope-mode)

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

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

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

(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

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


(add-hook 'js2-mode-hook (lambda ()
			  (layer-loader "~/.emacs.d/confs/js.el" 'controll-js)))
(add-hook 'python-mode-hook (lambda()
			      (layer-loader "~/.emacs.d/confs/python.el" 'controll-python)))
(add-hook 'latex-mode-hook (lambda()
			     (layer-loader "~/.emacs.d/confs/latex.el" 'controll-latex)))

(add-hook 'latex-mode-hook (lambda()
			     (layer-loader "~/.emacs.d/confs/latex.el" 'controll-latex)))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "light slate blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "lime green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "orange red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "red2")))))


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

(defun yas-selected-text-replace (beg)
  "Replace selected text in snippets"
  (let ((insertion (concat beg yas-selected-text)))
    (delete-region (mark) (point))
    insertion))

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

(defun magit-display-noselect-toggle ()
  "Display magit buffer but do not select window"
  (interactive)(if (equal magit-display-buffer-noselect nil)
                   (setq magit-display-buffer-noselect t) (setq magit-display-buffer-noselect nil)))


;; Copied from https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;; Spell checking in comments for different modes
(dolist (hook '(lisp-mode-hook
		ruby-mode-hook
		yaml-mode
		;; python-mode-hook
		elpy-mode
		shell-mode-hook
		conf-mode-hook
		php-mode-hook
		css-mode-hook
		nxml-mode-hook
		crontab-mode-hook
		perl-mode-hook
		javascript-mode-hook
		LaTeX-mode-hook))
  (add-hook hook 'flyspell-prog-mode))


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

(defun my/copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*"))
	(original-buffer (buffer-name)))
    (with-current-buffer result-buffer 
      (erase-buffer))
    (save-match-data 
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position) 
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)
    (kill-region (point-min) (point-max))
    (kill-buffer)
    (pop-to-buffer original-buffer)))


;; load keybindings
(load "~/.emacs.d/confs/after-save.el")
(load "~/.emacs.d/control.el")

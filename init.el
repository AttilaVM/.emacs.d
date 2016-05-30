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

;;----------------Packages-------------------------



;;Auto-complete is a dependency of yasnipper
(package-initialize)

(require 'autopair)
(autopair-global-mode)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'hexl)
(add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))

(load "~/.emacs.d/extensions/liquidsoap-mode.el")
(add-to-list 'auto-mode-alist '("\\.liq\\'" . liquidsoap-mode))

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

(add-something-to-mode-hooks '(c++ tcl emacs-lisp python text markdown latex) 'fic-ext-mode)

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
 '(LaTeX-command "latex -synctex=1")
 '(ac-modes
   (quote
    (latex-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode)))
 '(helm-ack-auto-set-filetype nil)
 '(helm-ack-thing-at-point (quote symbol))
 '(js2-imenu-enabled-frameworks nil)
 '(org-agenda-files (quote ("~/Documents/mytime.org"))))


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

;; elpy python IDE
;; Elpy works on the top of python mode
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq-default indent-tabs-mode nil)
;; Set $PYTHONPATH to elpy module
(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" user-home-dir "/.emacs.d/elpa/elpy-20160131.118"))

;; Make defintition jumping more robust
;; see https://github.com/jorgenschaefer/elpy/wiki/Customizations
(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                   (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

;; Django mode
(require 'python-django)

;; Haskell confing
(add-hook 'haskell-mode-hook #'hindent-mode)

;; Markdown mode-line
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; Set up web mode fore file extensions
(require 'web-mode)
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
;; turn on pair tag highlight for html
(web-mode-toggle-current-element-highlight)

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


(require 'impatient-mode)

;; JavaScript IDE capabilities
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
;; Configure imenu for js2-mode
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;; Configure refactoring
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; tern-mode for IDE features like code completition, jump to definition etc... it requires a tern server
(require 'tern)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; tern auto completion
;; (eval-after-load 'tern
;;    '(progn
;;       (tern-ac-setup)))

;; Hook json-mode to .jscsrc
(add-to-list 'auto-mode-alist '("\\.jscsrc\\'" . json-mode))
;; Hook JSCS to js and JSON modes
(add-hook 'js-mode-hook #'jscs-indent-apply)
(add-hook 'js2-mode-hook #'jscs-indent-apply)
(add-hook 'json-mode-hook #'jscs-indent-apply)

;; Set up company mode for tern
(require 'company-tern)
(eval-after-load 'company
    '(add-to-list 'company-backends 'company-tern))

;; Abality to run nodejs REPL inside emacs
(require 'nodejs-repl)

(require 'grunt)

;; helm
(require 'helm)
(require 'helm-config)

;; Search in css/scss selectors with helm
(add-to-list 'load-path "~/.emacs.d/extensions/helm-css-scss")
(require 'helm-css-scss)
;; Allow comment inserting depth at each end of a brace
(setq helm-css-scss-insert-close-comment-depth 2)
;; If this value is t, split window appears inside the current window
(setq helm-css-scss-split-with-multiple-windows nil)
;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-css-scss-split-direction 'split-window-vertically)
;; Toggle-in server mode for CLI Emacs clients
'(server-mode t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;;  Unlike python-mode, this mode follows the Emacs convention of not binding the ENTER key to `newline-and-indent'.  To get this behavior, add the key definition to `yaml-mode-hook':
(add-hook 'yaml-mode-hook
          '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(load "auctex.el" nil t t)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; compile to PDF automaticly
(setq TeX-PDF-mode t)
(require 'tex)
;; (TeX-global-PDF-mode t)

;; Autocomplete latexx
(require 'auto-complete-auctex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;        LaTeX         ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ##### Run emacs in server mode in order to be able to use
;; ##### emacsclient in Okular. Don't forget to configure
;; ##### Okular to use emacs in
;; ##### "Configuration/Configure Okular/Editor"
;; ##### => Editor => Emacsclient. (you should see
;; ##### emacsclient -a emacs --no-wait +%l %f
;; ##### in the field "Command".

(setq-default TeX-master nil)
;; ##### Enable synctex correlation. From Okular just press
;; ##### Shift + Left click to go to the good line.
(setq TeX-source-correlate-method 'synctex)
;; ##### Enable synctex generation. Even though the command shows
;; ##### as "latex" pdflatex is actually called


;; ##### Use Okular to open your document at the good
;; ##### point. It can detect the master file.
(add-hook 'LaTeX-mode-hook '(lambda ()
		  (add-to-list 'TeX-expand-list
		       '("%u" Okular-make-url))))

(defun Okular-make-url () (concat
	       "file://"
	       (expand-file-name (funcall file (TeX-output-extension) t)
			 (file-name-directory (TeX-master-file)))
	       "#src:"
	       (TeX-current-line)
	       (expand-file-name (TeX-master-directory))
	       "./"
	       (TeX-current-file-name-master-relative)))

;; ## Use these lines if you want a confirmation of the
;; ## command line to run...
;; (setq TeX-view-program-selection '((output-pdf "Okular")))
;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
;; ## And theses if you don't want any confirmation.
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("View" "okular --unique %u" TeX-run-discard-or-function nil t :help "View file"))
 )


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

;; clear Ipython console
(defun clearConsole ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

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


(setq elpy-rpc-backend "jedi")
(defun elpy-toggle-backend ()
  "Toggle between jedi and rope backends for Elpy"
  (interactive)
  (message (concat "RPC backend changed to " elpy-rpc-backend))
  )

;; source: http://stackoverflow.com/questions/14664829/emacs-auctex-prefix-arguments
(defun my-run-latex ()
  "Compile .tex into pdf without ask for save confirmation"
  (interactive)
  (if (buffer-modified-p)
      (progn  
        (setq TeX-save-query nil) 
        (TeX-save-document (TeX-master-file))
        (TeX-command "LaTeX" 'TeX-master-file -1))
    (TeX-view)))

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

(defun my-restart-python-console ()
  "Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed"
  (interactive)
  (let ((running-process (get-buffer-process "*Python*")))
    (if (equal (get-buffer-process "*Python*") nil)
        (elpy-shell-send-region-or-buffer)
      (message (concat "killing: " (prin1-to-string (get-process running-process))))
         (kill-process running-process)
    (while (not (equal (get-buffer-process "*Python*") nil))
      (sleep-for 0.01))
    (kill-buffer "*Python*"))
    (elpy-shell-send-region-or-buffer)))

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



(load "~/.emacs.d/control.el")

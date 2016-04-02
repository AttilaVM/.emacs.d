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

(global-set-key (kbd "C-s-a") 'org-agenda)
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
;; Use C-h and M-h , instead of backspace, s-h for help
(global-unset-key (kbd "<backspace>"))
(global-unset-key (kbd "C-h"))
(setq help-char nil)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") help-map)
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "s-h") 'mark-paragraph)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "<f2>") 'quick-calc)
(global-set-key (kbd "C-<f2>") 'calc)
(global-set-key (kbd "<C-kp-add>") 'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'decrement-number-at-point)
(global-set-key (kbd "<C-kp-1>") 'my-insert-file-name)
(global-set-key (kbd "<C-kp-2>") 'helm-colors)
(global-set-key (kbd "s-l") 'clearConsole)

;; Redefine undo/redo
;; (define-key minor-mode-map (kbd "C-s-/") nil)
;; (define-key minor-mode-map (kbd "M-s-/") nil)

(global-set-key (kbd "C-s-/") 'undo-tree-redo)
(global-set-key (kbd "M-s-/") 'undo-tree-visualize)

(global-set-key (kbd "<C-kp-3>") 'scss-compile)
(global-set-key (kbd "<C-kp-5>") 'show-file-name)
(global-set-key (kbd "<C-kp-6>") 'grab-screen-color)

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "s-o") 'other-window)


;;-----------------Repositories------------------

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))


;;-----------------Global Function------------------

;; Interactive mode for easier file and buffer navigation
;; (require 'ido)
;; (ido-mode t)

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

;; Configure tramp hosts
(require 'tramp)

;; Prevent tramp from using /dev/null and recreating it as a regular file when history size is reached.
(defvar tramp-histfile-override "~/.tramp_history" "Prevent tramp from using /dev/null and recreating it as a regular file when history size is reached.")

(defvar tramp-default-proxies-alist)
(add-to-list 'tramp-default-proxies-alist
             '("gyuri" "root" "/ssh:attila@gyuri:"))

;; Start emacs server for emacs clients
(require 'server)
(unless (server-running-p)
  (server-start))

;; Set up emacs as a pager .bashrc or zshrc should be modified!
(require 'pager)

; go to the last change
(require 'goto-chg)
(global-set-key (kbd "s-;") 'goto-last-change)

;; Highlight TODO, FIXME and BUG in comments 
(require 'fic-ext-mode)
(defun add-something-to-mode-hooks (mode-list something)
  "helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

(add-something-to-mode-hooks '(c++ tcl emacs-lisp python text markdown latex) 'fic-ext-mode)

(require 'magit)
(global-set-key (kbd "C-<f3>") 'magit-status)

;; Avy
(require 'avy)

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-unset-key (kbd "M-g g"))
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(avy-setup-default)

;; Helm
(require 'helm)
(require 'helm-config)

;; Make helm adapt for frequently selected results
(helm-adaptive-mode)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; Use helm for file finding
(global-unset-key (kbd "C-x C-f"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Use helm for command prompt
(global-set-key (kbd "M-x") 'helm-M-x)

;; Use helm-buffers-list instead of default helm buffer lister
(global-set-key (kbd "s-x b") 'helm-buffers-list)

;; get the list of the bookmarks (C-x r m for saving bookmarks) 
(global-set-key (kbd "C-x r r") 'helm-bookmarks)

;; More easier way to acces Emacs's internal "clipboard" 
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Use helm with isearch 
(global-set-key (kbd "s-s") 'helm-occur-from-isearch)

(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-map (kbd "C-r")  'helm-ff-run-find-file-as-root)

;; make sure C-h is no longer a prefix key
(define-key helm-map (kbd "C-h") nil)

(helm-mode 1)

;; (require 'helm-fuzzier)
;; (helm-fuzzier-mode 1)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))
(define-key yas-minor-mode-map (kbd "C-j") 'yas-next-field)
(define-key yas-minor-mode-map (kbd "C-l") 'yas-prev-field)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)

;; Use helm to explore a git project-am
(require 'helm-ls-git)
(global-set-key (kbd "C-<f6>") 'helm-browse-project)

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


(package-install 'flycheck)

(global-flycheck-mode)


;;icicles
;; (require 'icicles)
;; (icy-mode nil)
;; (add-hook 'icicle-ido-like-mode-hook
;;	    (lambda () (setq icicle-default-value
;;			(if icicle-ido-like-mode t 'insert-end))))

;; If i want to use auto-complete instead of company I will uncomment these
;; (require 'auto-complete-config)
;; (ac-config-default)

(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil)

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

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Use company with Clang
(require 'cc-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; Set up header completition
(add-to-list 'company-backends 'company-c-headers)

;; Open .h files as c++ files FIXME: Automatic C and C++ distinction
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Set key bindings
(eval-after-load "helm-cscope"
  '(progn
     (define-key helm-cscope-mode-map (kbd "M-s-9") 'helm-cscope-find-symbol)
     (define-key helm-cscope-mode-map (kbd "<C-kp-7>") 'helm-cscope-find-global-definition)
     (define-key helm-cscope-mode-map (kbd "<C-kp-0>") 'helm-cscope-find-called-function)
     (define-key helm-cscope-mode-map (kbd "<C-kp-9>") 'helm-cscope-find-calling-this-funtcion)
     (define-key helm-cscope-mode-map (kbd "<C-kp-8>") 'helm-cscope-select)))


;; elpy python IDE
;; Elpy works on the top of python mode
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq-default indent-tabs-mode nil)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . elpy-mode))

;; Django mode
(require 'python-django)

;; Haskell confing
(add-hook 'haskell-mode-hook #'hindent-mode)

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
(global-set-key (kbd "<C-kp-4>") 'sm/toggle-showcss)

;; Use Syntactically Awesome Stylesheets
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

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
(js2r-add-keybindings-with-prefix "C-c C-r")

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

;; Add Jquery doc for ac and js2
(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

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
;; Set local keybind map for css-mode / scss-mode / less-css-mode
(dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
	   (local-set-key (kbd "s-i") 'helm-css-scss)
	   (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

(define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
(define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)

;; Toggle-in server mode for CLI Emacs clients
'(server-mode t)

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
 )


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

(define-key magit-log-mode-map (kbd "s-<f3>") 'magit-display-noselect-toggle)

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
;; f
(define-key LaTeX-mode-map (kbd "C-c C-c") 'my-run-latex)
(define-key LaTeX-mode-map (kbd "s-c s-c") 'TeX-command-master)

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

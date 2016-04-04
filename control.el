;;; control.el --- all key-binding should be defined here

;;; Commentary:
;; Global package agnostic, global package dependent, local package reletad and sef defined interactive function bindings

;;; Code:

;;---------------GLOBAL PACKAGE AGNOSTIC----------------------
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
;; Calc
(global-set-key (kbd "<f2>") 'quick-calc)
(global-set-key (kbd "C-<f2>") 'calc)
(global-set-key (kbd "<C-kp-add>") 'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'decrement-number-at-point)
(global-set-key (kbd "<C-kp-1>") 'my-insert-file-name)
(global-set-key (kbd "<C-kp-2>") 'helm-colors)
(global-set-key (kbd "s-l") 'clearConsole)

(global-set-key (kbd "<C-kp-3>") 'scss-compile)
(global-set-key (kbd "<C-kp-5>") 'show-file-name)
(global-set-key (kbd "<C-kp-6>") 'grab-screen-color)

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "s-o") 'other-window)

;;---------------------GLOBAL PACKAGE DEPENDENT-------------
;; undo tree
(global-set-key (kbd "C-s-/") 'undo-tree-redo)
(global-set-key (kbd "M-s-/") 'undo-tree-visualize)
;; goto-chg
(global-set-key (kbd "s-;") 'goto-last-change)
;; magit
(global-set-key (kbd "C-<f3>") 'magit-status)
;; avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-unset-key (kbd "M-g g"))
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
;; helm--------------------------------------------
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-unset-key (kbd "C-x c"))
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
;; helm-ls-git
(global-set-key (kbd "C-<f6>") 'helm-browse-project)
;; helm-c-source-yasnippet
(global-set-key (kbd "C-c y") 'helm-yas-complete)
;; ???
(global-set-key (kbd "<C-kp-4>") 'sm/toggle-showcss)

;;---------------LOCAL BINDINGS---------------
;; helm--------------------------------------
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-map (kbd "C-r")  'helm-ff-run-find-file-as-root)

;; make sure C-h is no longer a prefix key
(define-key helm-map (kbd "C-h") nil)
;; helm-gtags
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;; helm csope
;; Set key bindings
(eval-after-load "helm-cscope"
  '(progn
     (define-key helm-cscope-mode-map (kbd "M-s-9") 'helm-cscope-find-symbol)
     (define-key helm-cscope-mode-map (kbd "<C-kp-7>") 'helm-cscope-find-global-definition)
     (define-key helm-cscope-mode-map (kbd "<C-kp-0>") 'helm-cscope-find-called-function)
     (define-key helm-cscope-mode-map (kbd "<C-kp-9>") 'helm-cscope-find-calling-this-funtcion)
     (define-key helm-cscope-mode-map (kbd "<C-kp-8>") 'helm-cscope-select)))
;; yasnippet--------------------------------
(define-key yas-minor-mode-map (kbd "C-j") 'yas-next-field)
(define-key yas-minor-mode-map (kbd "C-l") 'yas-prev-field)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; flyspell
(define-key flyspell-mode-map (kbd "C-;") nil)
;; cc-mode
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; js2-refactor-mode
(js2r-add-keybindings-with-prefix "C-c C-r")
;; css-mode / scss-mode / less-css-mode
(dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
	   (local-set-key (kbd "s-i") 'helm-css-scss)
	   (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

(define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
(define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)

;;------------------HELPER FUNCTIONS------------------------
;; Display magit buffer but do not select window for easy log reading
(define-key magit-log-mode-map (kbd "s-<f3>") 'magit-display-noselect-toggle)
;; Complie latex without asking too many questions baby!
(define-key LaTeX-mode-map (kbd "C-c C-c") 'my-run-latex)
(define-key LaTeX-mode-map (kbd "s-c s-c") 'TeX-command-master)
;; ELPY: Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed
(define-key elpy-mode-map (kbd "s-c s-c") 'my-restart-python-console)

;; Jump to a new line below or above
(global-set-key (kbd "<C-return>") 'my-newline-below)
(global-set-key (kbd "M-RET") 'my-newline-above)

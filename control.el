;;; control.el --- all key-binding should be defined here

;;; Commentary:
;; Global package agnostic, global package dependent, local package reletad and sef defined interactive function bindings

;;;; Conventions:
;; s-.      To toggle folding of one block
;; C-s-.    To toggle folding of all block in current buffer
;; M-.      Jump to definition
;; C-c C-d  Get tag documentation
;; s-r      Refactor prefix


;;; Code:

;;---------------GLOBAL PACKAGE AGNOSTIC----------------------

;; I do not want to put Emacs into the background, when I am not in the terminal
(when (display-graphic-p)
  (progn
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z"))))

;; I do recusive edit rarely, but often look around my point
(global-unset-key (kbd "C-]"))
(global-unset-key (kbd "C-x X a"))
(global-set-key (kbd "<C-escape> r") 'recursive-edit)
(global-set-key (kbd "C-]") 'recenter-top-bottom)

;; Sometimes when using a complex major mode is easier to explore menus.
(global-set-key (kbd "<C-f11> m") 'menu-bar-toggle)

(global-set-key (kbd "C-s-a") 'org-agenda)
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

(global-unset-key (kbd "M-o"))
(global-set-key (kbd "s-o") 'rotate-windows)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)


;; Calc
(global-set-key (kbd "<f2>") 'quick-calc)
(global-set-key (kbd "C-<f2>") 'calc)
(global-set-key (kbd "<C-kp-add>") 'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'decrement-number-at-point)
(global-set-key (kbd "<C-kp-1>") 'my-insert-file-name)
(global-set-key (kbd "<C-kp-2>") 'helm-colors)

(global-set-key (kbd "<C-kp-3>") 'scss-compile)
(global-set-key (kbd "<C-kp-5>") 'show-file-name)
(global-set-key (kbd "<C-kp-6>") 'grab-screen-color)

(global-unset-key (kbd "C-x o"))

;; Running shell command with different ways
(global-unset-key (kbd "M-!")) ;; shell-command
(global-unset-key (kbd "M-&")) ;; async-shell-command
(global-unset-key (kbd "M-|")) ;; shell command or region
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "M-!") 'async-shell-command)
(global-set-key (kbd "C-M-!") 'shell-command-on-region)
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

;; Seilver Searcher mode: ag
(global-set-key (kbd "C-s-s") 'helm-ag-project-root)

;; overide paired expressin jump
;; forward
(global-unset-key (kbd "<C-M-right>"))
(global-unset-key (kbd "C-M-f"))
(global-set-key (kbd "s-f") 'forward-sexp)
;; backward
(global-unset-key (kbd "<C-M-left>"))
(global-unset-key (kbd "C-M-b"))
(global-set-key (kbd "s-b") 'backward-sexp)

;; Use ibuffer for buffer managemnet
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; helm--------------------------------------------
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; Multiple cursor
(global-set-key (kbd "s-n") 'mc/edit-lines)
(global-set-key (kbd "M-s-s") 'mc/mark-next-like-this)

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

;; Rebind scrolling other window
(global-unset-key (kbd "C-M-v"))
(global-unset-key (kbd "C-M-S-v"))
(global-set-key (kbd "s-<down>") 'scroll-other-window)
(global-set-key (kbd "s-<up>") 'scroll-other-window-down)

;; Get all bindings for major mode
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)
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
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas/expand)
;; flyspell
(define-key flyspell-mode-map (kbd "C-;") nil)
;; Emacs-lisp TODO make an eval-buffer-or-region function for it
(define-key emacs-lisp-mode-map (kbd "C-c C-C") 'eval-buffer)
;; cc-mode
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; js2-refactor-mode

(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "s-c f") 'jscs-fix)
(define-key js2-mode-map (kbd "C-c C-o") nil)
(define-key js2-mode-map (kbd "s-.") 'js2-mode-toggle-element)
(define-key js2-mode-map (kbd "C-c C-f") nil)
(define-key js2-mode-map (kbd "C-s-.") 'js2-mode-toggle-hide-functions)
(define-key tern-mode-keymap (kbd "C-c C-r") nil)
(define-key tern-mode-keymap (kbd "s-r v") 'tern-rename-variable)

;; css-mode / scss-mode / less-css-mode

(define-key emmet-mode-keymap (kbd "s-<tab>") 'emmet-expand-line)

(global-set-key (kbd "C-M-g") 'grunt-exec)
;; nxml-mode
;; (define-key nxml-mode-map (kbd "s-n") 'nxml-forward-element)
;; (define-key nxml-mode-map (kbd "s-p") 'nxml-backward-element)

;;------------------HELPER FUNCTIONS------------------------
;; Display magit buffer but do not select window for easy log reading

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'my/smarter-move-beginning-of-line)

(global-set-key (kbd "s-w") 'my/copy-lines-matching-re)

;; Complie latex without asking too many questions baby!

(define-key LaTeX-mode-map (kbd "C-c C-c") 'my-run-latex)
(define-key LaTeX-mode-map (kbd "s-c s-c") 'TeX-command-master)




;; Jump to a new line below or above
(global-set-key (kbd "<M-RET>") 'my-newline-below)
(global-set-key (kbd "<S-M-RET>") 'my-newline-above)

;;----------------CROSS MODE-------------------------

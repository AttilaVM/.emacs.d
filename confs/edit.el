;;; edit.el --- Packages and functions for easier editing

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: editing
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.1
;;; Code:

;; Why the hell is this key even present on a modern keyboard?

(global-unset-key (kbd "C-x o"))

;; overide paired expressin jump
;; forward
(global-unset-key (kbd "<C-M-right>"))
(global-unset-key (kbd "C-M-f"))
(global-set-key (kbd "s-f") 'forward-sexp)
;; backward
(global-unset-key (kbd "<C-M-left>"))
(global-unset-key (kbd "C-M-b"))
(global-set-key (kbd "s-b") 'backward-sexp)

;; I do recusive edit rarely, but often look around my point
(global-unset-key (kbd "C-]"))
(global-unset-key (kbd "C-x X a"))
(global-set-key (kbd "<C-escape> r") 'recursive-edit)
(global-set-key (kbd "C-]") 'recenter-top-bottom)

;; Rebind scrolling other window
(global-unset-key (kbd "C-M-v"))
(global-unset-key (kbd "C-M-S-v"))
(global-set-key (kbd "s-<down>") 'scroll-other-window)
(global-set-key (kbd "s-<up>") 'scroll-other-window-down)

;; Sometimes when using a complex major mode is easier to explore menus.
(global-set-key (kbd "<C-f11> m") 'menu-bar-toggle)

;; Check misspelled word forward
(global-set-key (kbd "s-'") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "s-\\") 'flyspell-check-next-highlighted-word)

;; Calc
(global-set-key (kbd "<insert> 2 c") 'quick-calc)
(global-set-key (kbd "<insert> 2 C") 'calc)
(global-set-key (kbd "<insert> 2 r") 're-builder)

;; window navigation
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'rotate-windows)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)

;; Running shell command with different ways
(global-unset-key (kbd "M-!")) ;; shell-command
(global-unset-key (kbd "M-&")) ;; async-shell-command
(global-unset-key (kbd "M-|")) ;; shell command or region
(global-set-key (kbd "C-!") 'shell-command)
(global-set-key (kbd "M-!") 'async-shell-command)
(global-set-key (kbd "C-M-!") 'shell-command-on-region)

;; Using tabs for indentation can make elpy cranky
(setq-default indent-tabs-mode-mode nil)
(setq tab-width 2)
(add-hook 'find-file-hook (lambda ()
														(setq tab-width 2)))
;; tab-stop-list is a fallback when indent-relative does not find a tab stop
;; see: https://www.emacswiki.org/emacs/TabStopList
(setq tab-stop-list (number-sequence 2 120 2))

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(use-package undo-tree
	:config
	(global-undo-tree-mode)

	;; Unbind register interactions
	(define-key undo-tree-map (kbd "C-x r u") nil)
	(define-key undo-tree-map (kbd "C-x r U") nil)
	;; Undefine C-x r as local prefix key
	(define-key undo-tree-map (kbd "C-x r") nil)

	:bind (("C-s-/" . undo-tree-redo)
	 ("<insert> / /" . undo-tree-visualize)
	 ("s-r u" . undo-tree-save-state-to-register)
	 ("s-r U" . undo-tree-restore-state-from-register)))

(use-package expand-region
	:bind
	("M-j" . er/expand-region)
	("M-l" . er/contract-region)
	)

(require 'dabbrev)
;; make dabbrev case sensitive
(setq dabbrev-case-fold-search 'case-fold-search)
(setq company-dabbrev-ignore-case nil)

;; Use Company mode instead of auto-complete mode
(use-package company
	:config
	(add-hook 'after-init-hook 'global-company-mode)
	;; dabbrev should not downcase it completions
	(setq company-dabbrev-downcase nil)
	;; dabbrev by default only looks for
	(setq company-dabbrev-char-regexp "[a-zA-Z0-9.]")
	)

(use-package vlf
	:config
	(require 'vlf-setup))

(use-package smartparens

	:init
	(smartparens-global-mode)
	:config
	;; sp-clone-sexp works inconsitently: it does not effect the sexp selected by the pointer on its parenthesis
	;; when the closing parenthesis is selected, but the outter one
	(defun my/sp-clone-sexp ()
		(interactive)
		"Clone sexp similar to sp-clone-sexp, however can select it by the closing parenthesis"
		(when (char-equal ?\) (preceding-char))
			(backward-char)
			(call-interactively 'sp-clone-sexp)))

(defun my/avy-select-2 ()
	""
	(interactive)
	(call-interactively 'avy-goto-char-2)
	(call-interactively 'set-mark-command)
	(call-interactively 'avy-goto-char-2)
	)

(defun my/avy-select-1 ()
	""
	(interactive)
	(call-interactively 'avy-goto-char)
	(call-interactively 'set-mark-command)
	(call-interactively 'avy-goto-char)
	)

	;; TODO use mylib.el L104
	;; (defun my/kill-surrounded-content ()
	;;	(interactive)
	;;	(cond
	;;	 (char-equal ?\" ) )
	;;	(call-interactively 'set-mark-command)
	;;	(call-interactively 'forward-sexp)
	;;	(call-interactively 'backward-delete-char-untabify))
	:bind
	(("<insert> k )" . sp-splice-sexp)
	 ("<insert> k u" . sp-unwrap-sexp)
	 ("s-k c" . my/kill-surrounded-content)

	 ("C-M-j" . sp-up-sexp)
	 ("C-M-l" . sp-down-sexp)
	 ("s-p f" . sp-forward-sexp)
	 ("s-p b" . sp-backward-sexp)
	 ("<insert> k (" . sp-kill-sexp)
	 ("s-p <backspace>" . sp-backward-kill-sexp)
	 ("s-p c" . my/sp-clone-sexp)
	 ("s-p u" . sp-unwrap-sexp)
	 ;; select
	 ("<insert> SPC (" . sp-select-previous-thing)
	 ("<insert> SPC )" . sp-select-next-thing)
	 ("<insert> SPC r" . rectangle-mark-mode)
	 ))

(global-unset-key (kbd "M-g g"))
(use-package avy
	:config
	(avy-setup-default)
	;; Unset kbd from default line jump function
	:bind
	(( "<insert> ;" . avy-goto-char)
	( "<insert>  '" . avy-goto-char-2)
	( "<insert> l l" . avy-goto-line)
	( "M-g w" . avy-goto-word-1)))

;; Jump back to previous edits
(use-package goto-chg
	:bind
	(("s-q" . goto-last-change)
	 ("s-e" . goto-last-change-reverse)))

(use-package multiple-cursors
	:config
	(global-unset-key (kbs "M-m"))
	:bind
	(( "s-n" . mc/edit-lines)
	 ( "M-m" . mc/mark-next-like-this)))

(use-package buffer-move)

(use-package editorconfig
	:config
	(editorconfig-mode 1))

;; Enable camelCase word jumps for given languages
(my/add-hooks '(js-mode-hook
								clojure-mode-hook
								python-mode-hook
								java-mode-hook
								c-mode-hook
								haskell-mode-hook
								jade-mode-hook
								elm-mode-hook
								julia-mode-hook
								stylus-mode-hook) 'subword-mode)

(defun my/insert-hun-long-i ()
	(interactive)
	(insert "í"))

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

(defun my-newline-below()
	"Jum to the end of the line and inser a linebreak"
	(interactive)
	(move-end-of-line nil)
	(insert "\n")
	(indent-for-tab-command))

(defun my-newline-above()
	"Insert a new line above the point"
	(interactive)
	(move-beginning-of-line nil)
	(insert "\n")
	(forward-line -1)
	(indent-for-tab-command))

;; TODO duplicate selected lines
(defun my/line-duplicate-below ()
	"Duplicate active line"
	(interactive)
	;; insertion = newline + active line
	(let ((insertion (concat "\n" (buffer-substring (line-beginning-position) (line-end-position)))))
		(end-of-line)
		(insert insertion)
		;; Indent (tab) command in certain modes with automatic indention will lead to right identation
		;; Doing it at the end of the line will not cause any harm in others modes, expect in the ones
		;; where multiple identation levels carry differen meanings (python, yaml etc...)
		;; which is still a TODO task;
		(end-of-line)
		(indent-for-tab-command)))

(defun my/line-duplicate-above ()
	"Duplicate active line above"
	(interactive)
	;; insertion = newline + active line
	(let ((insertion (concat (buffer-substring (line-beginning-position) (line-end-position)))))
		(beginning-of-line)
		(insert "\n")
		(forward-line -1)
		(insert insertion)
		;; Indent (tab) command in certain modes with automatic indention will lead to right identation
		;; Doing it at the end of the line will not cause any harm in others modes, expect in the ones
		;; where multiple identation levels carry differen meanings (python, yaml etc...)
		;; which is still a TODO task;
		(end-of-line)
		(indent-for-tab-command)))

(defun my/line-select ()
	"Select active line"
	(interactive)
	(end-of-line)
	(set-mark (line-beginning-position)))

(defun my/line-cut ()
	"Cut active line"
	(interactive)
		(kill-region (line-beginning-position) (line-end-position)))

(defun my/line-copy ()
	"Copy active line"
	(interactive)
	(kill-ring-save (line-beginning-position) (line-end-position)))

(defun my/line-dump ()
	"Copy active line"
	(interactive)
	(append-to-buffer "*dump*" (line-beginning-position) (line-end-position))
	(with-current-buffer "*dump*"
		(insert "
")))

(defun my/line-query-replace ()
	"Query replace in active line"
	(interactive)
	(my/line-select)
	(call-interactively 'query-replace))

(defun my/line-query-replace-reqexp ()
	"Query replace regexp in active line"
	(interactive)
	(my/line-select)
	(call-interactively 'query-replace-regexp))

(defun my/line-move (n)
	"Move the current line up or down by N lines."
	(interactive "p")
	(setq col (current-column))
	(beginning-of-line) (setq start (point))
	(end-of-line) (forward-char) (setq end (point))
	(let ((line-text (delete-and-extract-region start end)))
		(forward-line n)
		(insert line-text)
		;; restore point to original column in moved line
		(forward-line -1)
		(forward-char col)))

(defun my/line-move-up (n)
	"Move the current line up by N lines."
	(interactive "p")
	(my/line-move (if (null n) -1 (- n))))

(defun my/line-move-down (n)
	"Move the current line down by N lines."
	(interactive "p")
	(my/line-move (if (null n) 1 n)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
	"Renames both current buffer and file it's visiting to NEW-NAME."
	(interactive (list (read-string "New Name: " (buffer-name))))
	(let ((name (buffer-name))
	(filename (buffer-file-name)))
		(if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
			(if (get-buffer new-name)
		(message "A buffer named '%s' already exists!" new-name)
	(progn
		(rename-file filename new-name 1)
		(rename-buffer new-name)
		(set-visited-file-name new-name)
		(set-buffer-modified-p nil))))))

;; Taken from https://www.emacswiki.org/emacs/RevertBuffer
(defun my/revert-all-buffers ()
		"Refreshes all open buffers from their respective files."
		(interactive)
		(dolist (buf (buffer-list))
			(with-current-buffer buf
	(when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
		(revert-buffer t t t) )))
		(message "Refreshed open files."))

(defun my-insert-file-name-relative (filename &optional args)
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
		(insert (file-relative-name filename))
		)



(defun my-insert-file-name-absolute (filename &optional args)
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
		 (insert (expand-file-name filename)))


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
	 (concat "rgba(" (replace-regexp-in-string "\n" " " (substring grabc-output (string-match "\n" grabc-output)) -1) ", 1)")))))

(defun my/avy-zap-to-char-1 ()
	"Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows' (ARG negates it)."
	(interactive)
	(call-interactively 'set-mark-command)
	(call-interactively 'avy-goto-char)
	(call-interactively 'kill-region))

(defun my/avy-zap-to-char-2 ()
	"Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows' (ARG negates it)."
	(interactive)
	(call-interactively 'set-mark-command)
	(call-interactively 'avy-goto-char-2)
	(call-interactively 'kill-region))

(defun my/selecet-sentence ()
	(interactive)
	(call-interactively 'backward-sentence)
	(call-interactively 'set-mark-command)
	(call-interactively 'forward-sentence))

;; (defun my/do-ag-on-filtered-files ()
;;	(interactive)
;;	(let ((targets (read-file-name "Search in: ")))))

;; Originaly used as the register prefix key,
;; which is completly substituted by helm inteaction with the kill ring
;; It will be used to revert active buffer
(global-unset-key (kbd "C-x r"))
(global-set-key (kbd "<insert> x r") 'revert-buffer)
(global-set-key (kbd "<insert> x w") 'write-file)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'my/smarter-move-beginning-of-line)

;; My handy line-interaction functions
(global-set-key (kbd "<insert> l n") 'my/line-duplicate-below)
(global-set-key (kbd "<insert> l p") 'my/line-duplicate-above)
(global-set-key (kbd "<insert> l SPC") 'my/line-select)
(global-set-key (kbd "<insert> l w") 'my/line-copy)
(global-set-key (kbd "<insert> l k") 'my/line-cut)
(global-set-key (kbd "<insert> l r t") 'my/line-query-replace)
(global-set-key (kbd "<insert> l r r") 'my/line-query-replace-reqexp)
(global-set-key (kbd "s-l d") 'my/line-dump)


;; Move lines up&down
(global-set-key (kbd "M-s-<down>") 'my/line-move-down)
(global-set-key (kbd "M-s-<up>") 'my/line-move-up)

(global-set-key (kbd "<C-kp-6>") 'grab-screen-color)

;; Jump to a new line below or above
(global-set-key (kbd "<insert> <return> n") 'my-newline-below)
(global-set-key (kbd "<insert> <return> p") 'my-newline-above)

(global-set-key (kbd "C-x H") 'my/selecet-sentence)

(global-set-key (kbd "<insert> k \;") 'my/avy-zap-to-char-1)
(global-set-key (kbd "<insert> k \'") 'my/avy-zap-to-char-2)

(global-set-key (kbd "M-s-i") 'my/insert-hun-long-i)

(global-set-key (kbd "<insert> SPC ;") 'my/avy-select-1)
(global-set-key (kbd "<insert> SPC '") 'my/avy-select-2)
;;; edit.el ends here

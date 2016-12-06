;;; edit.el --- Packages and functions for easier editing

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: editing
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.1
;;; Code:

;; Why the hell is this key even present on a modern keyboard?
(global-unset-key (kbd "<insert>"))

;; Using tabs for indentation can make elpy cranky
(setq-default indent-tabs-mode-mode nil)
(setq tab-width 2)

(use-package undo-tree
  :config
  (global-undo-tree-mode)

  ;; Unbind register interactions
  (define-key undo-tree-map (kbd "C-x r u") nil)
  (define-key undo-tree-map (kbd "C-x r U") nil)
  ;; Undefine C-x r as local prefix key
  (define-key undo-tree-map (kbd "C-x r") nil)

  :bind (("C-s-/" . undo-tree-redo)
	 ("M-s-/" . undo-tree-visualize)
	 ("s-r u" . undo-tree-save-state-to-register)
	 ("s-r U" . undo-tree-restore-state-from-register)))

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
  :bind
  (("s-p f" . sp-forward-sexp)
   ("s-p b" . sp-backward-sexp)
   ("s-p d" . sp-kill-sexp)
   ("s-p <backspace>" . sp-backward-kill-sexp)
   ("s-p c" . my/sp-clone-sexp)
   ("s-p u" . sp-unwrap-sexp)))

(use-package avy
  :config
  (avy-setup-default))

(use-package goto-chg)

(use-package multiple-cursors)

(use-package buffer-move)

(use-package hide-region)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Enable camelCase word jumps for given languages
(my/add-hooks '(js-mode-hook
		python-mode-hook
		java-mode-hook
		c-mode-hook
		stylus-mode-hook) 'subword-mode)

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

;; Originaly used as the register prefix key,
;; which is completly substituted by helm inteaction with the kill ring
;; It will be used to revert active buffer
(global-unset-key (kbd "C-x r"))
(global-set-key (kbd "C-x r") 'revert-buffer)

;; My handy line-interaction functions
(global-set-key (kbd "s-l C-n") 'my/line-duplicate-below)
(global-set-key (kbd "s-l C-p") 'my/line-duplicate-above)
(global-set-key (kbd "s-l C-SPC") 'my/line-select)
(global-set-key (kbd "s-l M-w") 'my/line-copy)
(global-set-key (kbd "s-l C-w") 'my/line-cut)
(global-set-key (kbd "s-l M-%") 'my/line-query-replace)
(global-set-key (kbd "s-l C-M-%") 'my/line-query-replace-reqexp)
;; Move lines up&down
(global-set-key (kbd "M-s-<down>") 'my/line-move-down)
(global-set-key (kbd "M-s-<up>") 'my/line-move-up)

;; Jump to a new line below or above
(global-set-key (kbd "<M-return>") 'my-newline-below)
(global-set-key (kbd "<M-s-return>") 'my-newline-above)
;;; edit.el ends here

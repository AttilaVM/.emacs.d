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

(use-package undo-tree
  :config
  (global-undo-tree-mode))

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

(use-package editorconfig
  :config
  (editorconfig-mode 1))

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

;;; edit.el ends here

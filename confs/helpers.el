;;; helpers.el --- little functions to make life easier

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords:
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(defun menu-bar-toggle ()
	(interactive)
	(if menu-bar-mode (menu-bar-mode -1) (menu-bar-mode t)))

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

(global-set-key (kbd "<insert> l m") 'my/copy-lines-matching-re)
;;; helpers.el ends here

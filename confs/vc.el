;;; vc.el --- Version controll

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(use-package magit
	:config
	(defun my/magit-display-noselect-toggle ()
	"Display magit buffer but do not select window"
	(interactive)(if (equal magit-display-buffer-noselect nil)
			 (setq magit-display-buffer-noselect t) (setq magit-display-buffer-noselect nil)))
	:bind
	(("<insert> v v" . magit-status)
	 ("<insert> v l" . magit-log-buffer-file)
	 ("<insert> v i" . magit-init)
	 ("<insert> v c" . magit-clone)
	 ("<insert> v f" . magit-find-file)
	 ("<insert> v b" . magit-branch-popup)
	 ("<insert> v p" . magit-push-popup)
	 )
	:bind
	(:map magit-log-mode-map
	 ("s-<f3>" . magit-display-noselect-toggle)))

(require 'dash)

;; Pretty magit
;; source: http://www.modernemacs.com/post/pretty-magit/
(defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
	"Replace sanitized WORD with ICON, PROPS and by default add to prompts."
	`(prog1
		 (add-to-list 'pretty-magit-alist
									(list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
												,ICON ',PROPS))
		 (unless ,NO-PROMPT?
			 (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

(setq pretty-magit-alist nil)
(setq pretty-magit-prompt nil)
(pretty-magit "Feature" ?⚑ (:foreground "slate gray" :height 1.2))
(pretty-magit "Add"     ?✓ (:foreground "#375E97" :height 1.2))
(pretty-magit "Fix"     ?⚒ (:foreground "#FB6542" :height 1.2))
(pretty-magit "Clean"   ?✂ (:foreground "#FFBB00" :height 1.2))
(pretty-magit "Docs"    ?ℹ (:foreground "#3F681C" :height 1.2))
(pretty-magit "master"  ?ᛏ (:box t :height 1.2) t)
(pretty-magit "origin"  ?ᛟ (:box t :height 1.2) t)


(defun add-magit-faces ()
	"Add face properties and compose symbols for buffer from pretty-magit."
	(interactive)
	(with-silent-modifications
		(--each pretty-magit-alist
			(-let (((rgx icon props) it))
				(save-excursion
					(goto-char (point-min))
					(while (search-forward-regexp rgx nil t)
						(compose-region
						 (match-beginning 1) (match-end 1) icon)
						(when props
							(add-face-text-property
							 (match-beginning 1) (match-end 1) props))))))))

(advice-add 'magit-status :after 'add-magit-faces)
(advice-add 'magit-refresh-buffer :after 'add-magit-faces)

;; Show comit message for the current line
(use-package git-messenger
	:bind
	("<insert> v m" . git-messenger:popup-message))

;; Awesome go through git hunks with Helm
(use-package helm-hunks
	:bind
	(("<insert> v h h" . 'helm-hunks)
	 ("<insert> v h s" . 'helm-hunks-staged)
	 ))

;; See the diff of the git index and the head line by line
;; Jump between hunks
(use-package git-gutter
	:config

	(defun my/git-gutter:batch-revert-hunk (beg end)
		(interactive
		 (if (use-region-p)
		 (list (region-beginning) (region-end))
		 (list (point) (point-max))))
		(loop t
			(call-interactively 'git-gutter:next-hunk)
			(when (> (point) end)
				(return))
			(call-interactively 'git-gutter:revert-hunk)))

	:bind
	(("<insert> v r" . git-gutter:revert-hunk)
	 ("<insert> v a a" . global-git-gutter-mode)
	 ("<insert> v a p" . git-gutter:popup-hunk)
	 ("<insert> v a s" . git-gutter:statistic)
	 ("<insert> v h p" . git-gutter:previous-hunk)
	 ("<insert> v h n" . git-gutter:next-hunk)
	 ("s-[" . git-gutter:previous-hunk)
	 ("s-]" . git-gutter:next-hunk)
	 )
	)

(use-package helm-git-grep)

;; Go through the history of a file
;; p previous commit
;; n next commit
;; g nth commit
;; q quit
(use-package git-timemachine
	:bind
	("<insert> v t" . git-timemachine))

;; Show added, deleted and modified files
;; compared to another branch
(use-package git-lens
	:bind
	("<insert> v a f" . git-lens))

;; Syntax hightlighing for git configiration files
(use-package gitconfig-mode
	:config
	(-concat auto-mode-alist
					 '(("\\.gitmodules\\'" . gitconfig-mode)
						 ("\\.gitignore\\'" . gitconfig-mode)))
	)

;; List, pull modify push gists to GitHub
(use-package gist
	:bind
	("<insert> = g g" . gist-list))

(use-package github-search)

;;; vc.el ends here

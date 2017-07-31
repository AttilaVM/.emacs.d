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
	 ("<insert> v f" . magit-find-file))
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

;;; vc.el ends here

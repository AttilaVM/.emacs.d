;;; guide.el --- Make emacs more beginner friendly

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(use-package guide-key
	:config
	(setq guide-key/guide-key-sequence t)
	(setq guide-key/recursive-key-sequence-flag t)
	(setq guide-key/idle-delay 0.5)
	(guide-key-mode 1))

(use-package discover-my-major
	:bind
	(("<insert> h m M" . discover-my-major)
	 ("<insert> h m m" . discover-my-mode)))



;;; guide.el ends here

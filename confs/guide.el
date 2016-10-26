;;; guide.el --- Make emacs more beginner friendly

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c C-e" "C-c C-t" "C-c" "C-c p" "C-x"))
(setq guide-key/guide-key-sequence t)
(setq guide-key/idle-delay 0.5)
(guide-key-mode 1)  ;

;;; guide.el ends here

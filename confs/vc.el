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
  ("C-<f3>" . magit-status)
  :bind
  (:map magit-log-mode-map
   ("s-<f3>" . magit-display-noselect-toggle)))

;;; vc.el ends here

;;; ibuffer.el --- Give emacs a pretty look.

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(defvar ibuffer-filtering-mode "none")


(use-package ibuffer-projectile)


(use-package ibuffer-tramp)

(add-hook 'ibuffer-hook
		(lambda ()
			;; fileter buffer per tramp connection
			(ibuffer-tramp-set-filter-groups-by-tramp-connection)
			;; Filter buffers per project
			(ibuffer-projectile-set-filter-groups)
			(setq ibuffer-filtering-mode "projectile")
			(unless (eq ibuffer-sorting-mode 'alphabetic)
				(ibuffer-do-sort-by-alphabetic))))

(defun ibuffer-switch-filtering ()
	"Swtich between per projectile and per tramp connection filtering"
	(interactive)
	(if (string= ibuffer-filtering-mode "tramp")
			(progn
	(message "filter per project")
	(ibuffer-projectile-set-filter-groups)
	(setq ibuffer-filtering-mode "projectile"))
		(progn
			(ibuffer-tramp-set-filter-groups-by-tramp-connection)
			(setq ibuffer-filtering-mode "tramp")
			(message "filter per tramp connection")))

	(unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))
	)

;; Use ibuffer for buffer managemnet
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "<insert> b i") 'ibuffer)
;;; ibuffer.el ends here

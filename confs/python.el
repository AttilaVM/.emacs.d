;;; python.el --- Python specific code

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: Programming
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.1
;;; Code:

;; Elpy works on the top of python mode
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(setq python-indent 2)

(use-package elpy
	:config
	(elpy-use-ipython)
	(setq elpy-rpc-backend "jedi")
	(setq python-indent-offset 4)

	;; clear Ipython console. use it only in ipython buffer
	(defun my/clear-console ()
		(interactive)
		(let ((comint-buffer-maximum-size 0))
			(comint-truncate-buffer)))

	:bind
	;; ELPY: Restart python console before evaluate buffer or region to avoid various uncanny conflicts
	;; like not reloding modules even when they are changed
	(:map python-mode-map
	("s-c s-c" . my-restart-python-console)
	("M-." . elpy-goto-definition-or-rgrep)
	("s-c f" . elpy-autopep8-fix-code)
	("s-l" . my/clear-console)))

;; Make defintition jumping more robust
;; see https://github.com/jorgenschaefer/elpy/wiki/Customizations
(defun elpy-goto-definition-or-rgrep ()
	"Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
		(interactive)
		(ring-insert find-tag-marker-ring (point-marker))
		(condition-case nil (elpy-goto-definition)
	(error (elpy-rgrep-symbol
			 (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))


(defun elpy-toggle-backend ()
	"Toggle between jedi and rope backends for Elpy."
	(interactive)
	(message (concat "RPC backend changed to " elpy-rpc-backend))
	)


(defun my-restart-python-console ()
	"Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed."
	(interactive)
	(let ((running-process (get-buffer-process "*Python*")))
		(if (equal (get-buffer-process "*Python*") nil)
	(elpy-shell-send-region-or-buffer)
			(message (concat "killing: " (prin1-to-string (get-process running-process))))
	 (kill-process running-process)
		(while (not (equal (get-buffer-process "*Python*") nil))
			(sleep-for 0.01))
		(kill-buffer "*Python*"))
		(elpy-shell-send-region-or-buffer)))

;; Django mode
(use-package python-django)

;;; python.el ends here

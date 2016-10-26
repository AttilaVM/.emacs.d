;;; behahavior.el --- Configure Emacs basic behavior

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: basic behavior
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

;; Set variable binding limit
(setq max-specpdl-size 10000)
;; Set recursion limit
(setq max-lisp-eval-depth 10000)

;; y or n for verification instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; Make Emacs delete selected region on insertion event, similary to other editors.
(delete-selection-mode -1)
;; Do not load outdated byte code
(setq load-prefer-newer t)

;; Set defalult-directory to .emacs.d to init helm-projectile at startup.
(setq default-directory "~/.emacs.d")

;; BUG Doesn't seem to work
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; Set startup buffer
(setq initial-buffer-choice "~/Documents/reminder.org")

;; Remove trailing white spaces on save
(defvar whitespace-cleanup-on-save t)
(add-hook 'before-save-hook
          (lambda ()
            (if whitespace-cleanup-on-save (whitespace-cleanup))))


;;; behavior.el ends here

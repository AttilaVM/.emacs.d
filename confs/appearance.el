;;; appearance.el --- Give emacs a pretty look.

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

;; Disable menu-  tool and scroll
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Set global font type
;; (add-to-list 'default-frame-alist '(font . "inconsolata-12" ))
;; (set-face-attribute 'default t :font "inconsolata-12" )

(add-to-list 'default-frame-alist '(font . "Inconsolata-dz for Powerline-11" ))
(set-face-attribute 'default t :font "Inconsolata-dz for Powerline-11")


;; Set termina font type-break


;; Enable line numbers globally
(global-linum-mode t)
;; Keep point position on scrolling
(setq scroll-preserve-screen-position t)

;; Show parentheses
(show-paren-mode 1)

(require 'paren)
;; Set marker color to green
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#009900")

;; Set different colors for different parenthesis depth
(use-package rainbow-delimiters
  :config
  (my/add-hooks '(emacs-lisp-mode clojure-mode-hook js2-mode-hook) 'rainbow-delimiters-mode)
  '(rainbow-delimiters-depth-1-face ((t (:foreground "light slate blue"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "lime green"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow green"))))
  '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
  '(rainbow-delimiters-depth-6-face ((t (:foreground "goldenrod"))))
  '(rainbow-delimiters-depth-7-face ((t (:foreground "dark orange"))))
  '(rainbow-delimiters-depth-8-face ((t (:foreground "orange red"))))
  '(rainbow-delimiters-depth-9-face ((t (:foreground "red2")))))

;; Highlight TODO, FIXME and BUG in comments
(use-package highlight-symbol
  :config
  (highlight-symbol-add-symbol "TODO")
  (highlight-symbol-add-symbol "@todo")
  (highlight-symbol-add-symbol "FIXME")
  (highlight-symbol-add-symbol "BUG")
  (highlight-symbol-mode 1))

(use-package highlight-blocks)

;; (use-package fic-ext-mode
;;   :config
;;   (fic-ext-mode 1))

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS"
			    source nil t) nil))
	 (flags (car wm-hints)))
    ; (message flags)
    (setcar wm-hints
	    (if arg
		(logior flags #x00000100)
	      (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))

(global-unset-key (kbd "C-x ^"))
(global-unset-key (kbd "C-x }"))
(global-unset-key (kbd "C-x {"))
(global-unset-key (kbd "C-x ^"))

(global-set-key (kbd "s-9") ' enlarge-window-horizontally)
(global-set-key (kbd "s-8") ' shrink-window-horizontally)
(global-set-key (kbd "s-7") ' enlarge-window)
(global-set-key (kbd "s-6") ' shrink-window)
;;; appearance.el ends here

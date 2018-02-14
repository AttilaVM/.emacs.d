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

;; (add-to-list 'default-frame-alist '(font . "Inconsolata-dz for Powerline-11" ))
;; (set-face-attribute 'default t :font "Inconsolata-dz for Powerline-11")

;;(set-default-font "Inconsolata-14")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; (set-default-font "DejaVu Sans Mono-13")

(when (window-system)
		(set-default-font "Fira Code"))

;; (use-package pretty-mode)
;; (require 'pretty-mode)
;; (global-pretty-mode t)

;; Emacs built in prettify
(global-prettify-symbols-mode 1)
(add-hook
 'python-mode-hook
 (lambda ()
	 (mapc (lambda (pair) (push pair prettify-symbols-alist))
				 '(;; Syntax
					 ("def" .      #x03d5) ; ϕ
					 ("not" .      #xffe2) ; ￢
					 ("in" .       #x2208) ; ∈
					 ("not in" .   #x2209) ; ∉
					 ("return" .   #x2b05) ; ⬅
					 ("yield" .    #x21da) ; ⇚
					 ("for" .      #x2200) ; ∀
					 ("while" .    #x27f3) ; ⟳
					 ("==" .       #x2263) ; ≣
					 ("!=" .       #x2260) ; ≠
					 ("<=" .       #x2264) ; ≤
					 (">=" .       #x2265) ; ≥
					 ;; functional
					 ("map" .      #x279b) ; ➛
					 ("compose" .  #x2295) ; ⊕
					 ;; Base Types
					 ("bool" .     #x1d539) ; 𝔹
					 ("int" .      #x2124)
					 ("float" .    #x211d)
					 ("str" .      #x1d54a)
					 ("True" .     #x1d54b)
					 ("False" .    #x1d53d)
					 ("None" .     #x29b0) ; ⦰
					 ;; Mypy
					 ("Dict" .     #x1d507)
					 ("List" .     #x2112)
					 ("Tuple" .    #x2a02)
					 ("Set" .      #x2126) ;
					 ("Iterable" . #x1d50a)
					 ("Any" .      #x2754)
					 ("Union" .    #x22c3)
					 ("lambda" .   #x03bb) ; λ
					 ;; constants
					 ("math.pi" .  #x03c0) ; π
					 ("pi" .       #x03c0) ; π
					 ;; operations
					 ("sum" .      #x2211) ; ∑
					 ;; Cartesian product
					 ("cproduct" . #x2715) ; ✕
					 ("itertools.product" . #x2715)
					 ))))

(add-hook
 'clojure-mode-hook
 (lambda ()
	 (mapc (lambda (pair) (push pair prettify-symbols-alist))
				 '(;; Syntax
					 ("def" . #x1d54d) ; 𝕍
					 ("defn" .      #x03d5) ; ϕ
					 ("fn" .   #x03bb) ; λ
					 ("loop" .    #x27f3) ; ⟳
					 ("recur" .  #x21bb) ; ↻
					 ("defrecord" . #x03b4) ; δ

					 ("not" .      #xffe2) ; ￢
					 ("conatins?" .       #x2208) ; ∈
					 ("not=" .       #x2260) ; ≠
					 ("<=" .       #x2264) ; ≤
					 (">=" .       #x2265) ; ≥
					 ;; functional
					 ("map" .      #x279b) ; ➛
					 ("comp" .  #x2295) ; ⊕
					 ;; logic
					 ("or" . #x22c1) ; ⋁
					 ("and" . #x22c0 ) ;  ⋀
					 ("some" .  #x2203) ; ∃
					 ("every?" .      #x2200) ; ∀
					 ;; meta
					 ("->" . #x2799) ; ➙
					 ("->>" . #x279c) ; ➜
					 ("as->" . #x27a4) ; ➤
					 ;; algebric
					 ("Math/floor" . "⌞x⌟")
					 ("Math/ceil" . "⌜ x ⌝")
					 ("Math/sqrt" . "√")
					 ("Math/abs" . "|x|")

					 ("clojure.set/union" .    #x22c3) ; ⋃
					 ("set/union" .    #x22c3) ; ⋃

					 ("clojure.set/intersection" .    #x22c2) ; ⋂
					 ("set/intersection" .    #x22c2) ; ⋂

					 ("clojure.set/subset" .    #x2282) ; ⊂
					 ("set/subset" .    #x2282) ; ⊂

					 ("clojure.set/superset" .    #x2283) ; ⊃
					 ("set/superset" .    #x2283) ; ⊃

					 ;; Base Types
					 ("^Long" .      #x2124)
					 ("^double" .    #x211d)
					 ("^String" .      #x1d54a)
					 ("true" .     #x1d54b)
					 ("false" .    #x1d53d)
					 ("nil" .     #x29b0) ; ⦰


					 ;; constants
					 ("Math/PI" .  #x03c0) ; π
					 ("PI" .       #x03c0) ; π
					 ;; operations
					 ))))

;; Set terminal font type-break


;; Enable line numbers globally
(global-linum-mode t)
(add-hook `shell-mode-hook
					(lambda () (linum-mode)))

;; Keep point position on scrolling
(setq scroll-preserve-screen-position t)

;; Show parentheses
(show-paren-mode 1)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
	(toggle-read-only)
	(ansi-color-apply-on-region compilation-filter-start (point))
	(toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'paren)
;; Set marker color to green
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#009900")

(use-package rainbow-delimiters
	:config
	;; Enable it in all programing modes
	(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
	;; Set colors to travel through the VIS spectrum from red to blue
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

;; Colorize strings which represent a color, like red, #ec883a, rgb(29, 60, 76)
;; https://julien.danjou.info/projects/emacs-packages#rainbow-mode
(use-package rainbow-mode
	:config
	(add-hook 'stylus-mode-hook (lambda () (rainbow-mode))))

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

(require 'eldoc)
(setq eldoc-echo-area-use-multiline-p t)

(global-unset-key (kbd "C-x ^"))
(global-unset-key (kbd "C-x }"))
(global-unset-key (kbd "C-x {"))
(global-unset-key (kbd "C-x ^"))

(global-set-key (kbd "s-9") ' enlarge-window-horizontally)
(global-set-key (kbd "s-8") ' shrink-window-horizontally)
(global-set-key (kbd "s-7") ' enlarge-window)
(global-set-key (kbd "s-6") ' shrink-window)
;;; appearance.el ends here

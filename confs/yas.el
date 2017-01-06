(use-package yasnippet
	:config
	(load "~/.emacs.d/confs/yas-data.el")
	(yas-global-mode 1)
	(setq yas-snippet-dirs
	'("~/.emacs.d/snippets/"))

	;; keybinding for navigating between yas fields are only used inside of a snippet,
	;; outside they would be useless so I made functions, which navigate inside a snippet
	;; However outside navigate between symbolic expression
	(defun my/yas-next-field-or-forward-sexp ()
		"Try to jump to next yas field if not in a snippet jump forward a symbolic expression"
		(interactive)
		(condition-case err
	(yas-next-field)
			(error
			 (sp-forward-sexp))))

	(defun my/yas-previous-field-or-backward-sexp ()
		"Try to jump to next yas field if not in a snippet jump forward a symbolic expression"
		(interactive)
		(condition-case err
	(yas-prev-field)
			(error
			 (sp-backward-sexp))))

	(define-key yas-minor-mode-map (kbd "C-j") 'my/yas-next-field-or-forward-sexp)
	(define-key yas-minor-mode-map (kbd "C-l") 'my/yas-previous-field-or-backward-sexp)
	(define-key yas-minor-mode-map (kbd "<tab>") nil)
	(define-key yas-minor-mode-map (kbd "TAB") nil)
	(define-key yas-minor-mode-map (kbd "<backtab>") 'yas/expand))

(defun yas/hacks-replace-previous-char (target replacement)
	"Replace the first oocurence of target-chat with replacement-char while searching backward"

	(let ((char-at-point nil)
	(continue-flag t)
	(target-char (string-to-char target))
	(replacement-char (string-to-char replacement)))
		(while continue-flag

			(backward-char)
			(if (char-equal (char-after) target-char)
		(progn
			(delete-char 1)
			(insert replacement-char)
			(setq continue-flag nil)))

			(if (char-equal (char-after) replacement-char)
		(progn
			(setq continue-flag nil))))))

(defun yas/hacks-selected-text-replace ()
	"Replace selected via yasnippet"
	;; (delete-region (mark) (point))
	;; Insert selected text
	(if (char-or-string-p yas/selected-text)
			(progn
	(if (< (point) (mark))
			(progn
				(search-forward yas/selected-text)
				(replace-match "")))
	(if (> (point) (mark))
			(progn
				(search-backward yas/selected-text)
				(replace-match ""))))))

(defun my/camelize (s)
	"Convert under_score string S to CamelCase string."
	(store-substring (mapconcat 'identity (mapcar
					 (lambda (word) (capitalize (downcase word)))
					 (split-string s "[-_ ]+")) "") 0 (if (> (length s) 0)
												(downcase (substring s 0 1)))))

(defun my/char-cleaner (s)
	"Remove characters from string which collide with variable naming scheme"
	(replace-regexp-in-string "[^a-zA-Z]*" "" s))

(defun my/constructor-generator (s prefix postfix)
	"Helps snippets"
	(mapconcat 'identity (mapcar
			(lambda (word) (concat prefix word " = " word postfix))
			(split-string s ", ")) "\n" ))

(defun my/angular-di-generator (s)
	"Helps snippets"
	(let ((di-string (replace-regexp-in-string "'" "" s)))
		(replace-regexp-in-string "[^a-zA-Z\$]*$" ""
						(mapconcat 'identity (mapcar
								(lambda (word) (concat word ", "))
								(split-string di-string ", ")) "" ))))

(defun my-yas-insert-file-name (dir)
	"Insert absolute path inside yassnipets from given directory"
	(let ((default-directory dir))
		(unless yas-modified-p
			(expand-file-name (completing-read "File: " 'read-file-name-internal)))))

;;

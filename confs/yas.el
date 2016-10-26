(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets/")))

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


(defun my/constructor-generator (s prefix postfix)
  "Helps snippets"
  (mapconcat 'identity (mapcar
                        (lambda (word) (concat prefix word " = " word postfix))
                        (split-string s ", ")) "\n" ))

  
;; 



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

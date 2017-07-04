(defun my/add-hooks (hooks mode)
	"Add one mode to multiple hooks"
	(dolist (hook hooks)
		(add-hook hook mode)))

(defun my/load-when-readable (path)
	"Load file is readable, otherwise complain in a message"
	(if (file-readable-p path)
	(load path)
	(message (concat path " is not found or not readable"))))

(defun my/change-to-home-buffer ()
	(interactive)
	(find-file user/home-buffer))

;; Use variable width font faces in current buffer TODO: misbehaving
 (defun my/buffer-face-to-inconsolata-powerline ()
	 "Set font to a variable width (proportional) fonts in current buffer"
	 (interactive)
	 ;; (setq buffer-face-mode-face '(:family "Powerline-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" :height 130))
	 (setq buffer-face-mode-face '(:family "unknown-Inconsolata-dz" :height 130))
	 (buffer-face-mode))

(defun my/get-first-match (list filter)
	(when list
		(if (funcall filter (car list))
		(car list)
		(my/get-first-match (cdr list) filter))))


(defun append-to-list (list-var elements)
	"Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
	(unless (consp elements)
		(error "ELEMENTS must be a list"))
	(let ((list (symbol-value list-var)))
		(if list
	(setcdr (last list) elements)
			(set list-var elements)))
	(symbol-value list-var))

(defun my/basedir (&optional path)
	"Return the base directory of a given path, if no path argument is given, then the path to he current file will be used"
	(let ((path (if path
							path
							buffer-file-name)))
		(file-name-nondirectory (directory-file-name (file-name-directory path)))))

(global-set-key (kbd "<C-kp-add>") 'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'decrement-number-at-point)
(global-set-key (kbd "<C-kp-1>") 'my-insert-file-name)

(defun my/read-file (file-path)
	"Return file-path's file content."
	(with-temp-buffer
		(insert-file-contents file-path)
		(buffer-string)))

(defun my/get-os ()
	(cond
	 ((string-equal system-type "gnu/linux")
		;; Remove parentheses
		(if (file-readable-p "/etc/lsb-release")
				(replace-regexp-in-string "[\"\n]" ""
															;; Remove DISTRIB_ID= prefix
																	(replace-regexp-in-string "^.*=" "" (my/read-file "/etc/lsb-release")))
			"unknown-linux"))))

(defun my/keys (alist)
	(mapcar 'car alist))

(defun my/get-by-string-key (query-key alist)
	(let ((value nil)
				(alist-length (length alist))
				(counter 0))
		(while (and (not value) (< counter alist-length))
			(let ((key (car
									(nth counter alist))))

				(when (and (stringp key) (string-equal key query-key))
						(setq value (cdr (nth counter alist))))

			(setq counter (my/inc counter)))) value ))

(defun my/inc (num)
	(+ num 1))

(defun my/dec (num)
	(- num 1))

(defun my/commint-clear ()
	"Clear comint buffer"
	(interactive)
	(let ((comint-buffer-maximum-size 0))
		(comint-truncate-buffer)))

(defun my/new-buffer (buff-name)
	"Renames both current buffer and file it's visiting to NEW-NAME."
	(interactive (list (read-string "Buffer name: ")))
	(let ((-buf (generate-new-buffer buff-name)))
		(switch-to-buffer -buf)
		(funcall initial-major-mode)
		(setq buffer-offer-save t)))

(defun my/enclosure-p (char)
	(mapcar
	 (lambda (enclosure-char)
		 (char-equal char enclosure-char)) '(?a ?\")))

(defun my/boolean-to-string (bool)
	(if bool
	"True"
	"False"))

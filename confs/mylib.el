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


(global-set-key (kbd "<C-kp-add>") 'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'decrement-number-at-point)
(global-set-key (kbd "<C-kp-1>") 'my-insert-file-name)

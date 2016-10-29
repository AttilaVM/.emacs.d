(defun my/add-hooks (hooks mode)
  "Add one mode to multiple hooks"
  (dolist (hook hooks)
    (add-hook hook mode)))

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

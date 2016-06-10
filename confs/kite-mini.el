(require 'kite-mini)

(defvar kite-mini-watch-list '())
(defun kite-mini-watch-append-to-list ()
  "Saving watched files will make kite-mini to reload connected page"
  (interactive)
  (add-to-list 'kite-mini-watch-list (buffer-name)))    

;; See `after-save-hook' for deatils in `after-save.el'
(defun kite-mini-live-reload ()
  "Reload connected browser page with kite mini if a file in kite-mini-watch-list is changed"
  (if (memq (buffer-name) kite-mini-watch-list)
      (kite-mini-call-rpc
       "Page.reload"
       (list :ignoreCache t))))
 

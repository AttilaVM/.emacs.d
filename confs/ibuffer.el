(defvar ibuffer-filtering-mode "none")

(add-hook 'ibuffer-hook
	  (lambda ()
	    ;; fileter buffer per tramp connection
	    (ibuffer-tramp-set-filter-groups-by-tramp-connection)
	    ;; Filter buffers per project
	    (ibuffer-projectile-set-filter-groups)
	    (setq ibuffer-filtering-mode "projectile")
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

(defun ibuffer-switch-filtering ()
  "Swtich between per projectile and per tramp connection filtering"
  (interactive)
  (if (string= ibuffer-filtering-mode "tramp")
      (progn
	(message "filter per project")
	(ibuffer-projectile-set-filter-groups)
	(setq ibuffer-filtering-mode "projectile"))
    (progn
      (ibuffer-tramp-set-filter-groups-by-tramp-connection)
      (setq ibuffer-filtering-mode "tramp")
      (message "filter per tramp connection")))

  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic))
  )

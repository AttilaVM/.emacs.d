(defun my/deploy-static (src target-dest target-port pretend)
	(if (not (and src target-dest target-port))
			(error "Insufficient src or target definition!")
		;; Pretend rsync
		(async-shell-command
		 (mapconcat
			'my/get-sym-val '(
												;; Echo data
												"echo"
												"'Source: " src
												"\nDestination: "	target-dest
												"\nPort: " target-port
												"\npretend: " (lambda () (my/boolean-to-string pretend))
												"\n';"
												;; rsync command
												"rsync" "--verbose" "--progress" "--stats"
												"--exclude='*.git'"
												"--compress" "--times" "--delete" "--rsh=/usr/bin/ssh"
												(lambda ()
													(if  pretend
															"--dry-run" "-itemize-changes"
															""))
												"-e" (lambda () (concat "\"ssh -p" target-port "\""))
												"--recursive"
												;; src dst
												src target-dest)
			" "))))


(defun my/job-deploy-static-files (file target-type pretend)
	(interactive (list (read-file-name "config file:")
										 (completing-read "Select target:" '("testing" "production"))
										 (y-or-n-p "Shall I pretend the sync operation?")))
	(let ((config (json-read-file file)))
		(if (string-equal "testing" target-type)
				(my/deploy-static (alist-get 'src
																		 config)
													(alist-get 'testingTargetDest
																		 config)
													(alist-get 'testingTargetPort
																		 config)
													pretend)
			(my/deploy-static (alist-get 'src
																	 config)
												(alist-get 'productionTargetDest
																	 config)
												(alist-get 'productionTargetPort
																	 config)
												pretend)
			)))

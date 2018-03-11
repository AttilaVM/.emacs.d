;;; ob-sed.el --- Babel Functions for Sed Scripts

;; Author: Attila Molnar
;; Keywords: literate programming, reproducible research, Nix, DevOps
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(require 'ob)

(defvar org-babel-nix-command "nix-repl"
	"The nix-repl command, which should be executable inside actual environment.")


(add-to-list 'org-babel-tangle-lang-exts '("nix" . "nix"))

(defconst org-babel-header-args:nix
	'()
	"Sed specific header arguments.")

(defvar org-babel-nix-eval-verbose t
	"A non-nil value makes `org-babel-eval' display")

;; src: https://emacs.stackexchange.com/questions/2952/display-errors-and-warnings-in-an-org-mode-code-block
(defun org-babel-nix-eval (cmd body)
	"Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
	(let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
		(with-current-buffer err-buff (erase-buffer))
		(with-temp-buffer
			(insert body)
			(setq exit-code
						(org-babel--shell-command-on-region
						 (point-min) (point-max) cmd err-buff))
			(if (or (not (numberp exit-code)) (> exit-code 0)
							(and org-babel-nix-eval-verbose (> (buffer-size err-buff) 0))) ; new condition
					(progn
						(with-current-buffer err-buff
							(org-babel-eval-error-notify exit-code (buffer-string)))
						nil)
				(buffer-string)))))

(defun org-babel-nix-extract-output (raw-output)
	(->> raw-output
			 (replace-regexp-in-string "Welcome to Nix version [1-9.]* Type :[?] for help." "")
			 (replace-regexp-in-string "nix-repl>.*" "")
			 (replace-regexp-in-string "
[ ]*
" "")
(replace-regexp-in-string "^[ ]*
" "")
	 )
	)

(defun org-babel-execute:nix (body params)
	(-as-> body X
				 (org-babel-nix-eval org-babel-nix-command  X)
				 (org-babel-reassemble-table X "A" "B")
				 (org-babel-nix-extract-output X)
				 )
	)



(call-process-shell-command "touch /tmp/test/new")
(call-process-shell-command
 (remq nil
			 (list org-babel-nix-command
						 (format "<< EOF %s EOF" "1 + 3"))))

(-as-> "1 + 3" X
			 (format "%s << EOF
 %s EOF" "nix-repl" X)
			 (org-babel-eval X)
			 ;;(call-process-shell-command X nil (current-buffer))
			 )
(org-babel-nix-eval "nix-repl" "se")
(org-babel-eval "python -i" "se")

(org-babel-reassemble-table '((1 2) (3 5)) "A" "1")

(cond
 (stdin (with-temp-buffer
					(call-process-shell-command cmd stdin (current-buffer))
					(buffer-string)))
 (t (org-babel-eval cmd "")))

(defun org-babel-execute:sed (body params)
	"Execute a block of sed code with Org Babel.
BODY is the source inside a sed source block and PARAMS is an
association list over the source block configurations.  This
function is called by `org-babel-execute-src-block'."
	(message "executing sed source code block")
	(let* ((result-params (cdr (assq :result-params params)))
				 (cmd-line (cdr (assq :cmd-line params)))
				 (in-file (cdr (assq :in-file params)))
				 (code-file (let ((file (org-babel-temp-file "sed-")))
											(with-temp-file file
												(insert body)) file))
				 (stdin (let ((stdin (cdr (assq :stdin params))))
									(when stdin
										(let ((tmp (org-babel-temp-file "sed-stdin-"))
													(res (org-babel-ref-resolve stdin)))
											(with-temp-file tmp
												(insert res))
											tmp))))
				 (cmd (mapconcat #'identity
												 (remq nil
															 (list org-babel-sed-command
																		 (format "--file=\"%s\"" code-file)
																		 cmd-line
																		 in-file))
												 " ")))
		(org-babel-reassemble-table
		 (let ((results
						(cond
						 (stdin (with-temp-buffer
											(call-process-shell-command cmd stdin (current-buffer))
											(buffer-string)))
						 (t (org-babel-eval cmd "")))))
			 (when results
				 (org-babel-result-cond result-params
					 results
					 (let ((tmp (org-babel-temp-file "sed-results-")))
						 (with-temp-file tmp (insert results))
						 (org-babel-import-elisp-from-file tmp)))))
		 (org-babel-pick-name
			(cdr (assq :colname-names params)) (cdr (assq :colnames params)))
		 (org-babel-pick-name
			(cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

;;; ob-nix.el ends here

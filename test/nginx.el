;; The cursed kwallet jinx my ssh login maybe I should use tramp instead.
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html
(defun my/get-nginx-info (remote-key)
	"Get a plethora of inormation about your Nginx server at a given uri. You can predefine your remotes at "
	(interactive (list (completing-read "Select Remote: " (my/keys user/remotes))))
	(let* (
				 (remote-data (my/get-by-string-key remote-key user/remotes))
				 (remote-uri (car remote-data))
				 (remote-port  (nth 1 remote-data)))
		(message (concat "uri: " remote-uri " port: " remote-port))
		(shell-command (mapconcat
										'my/get-sym-val
										'("ssh -p" remote-port remote-uri
											"'ps -p $(cat /var/run/nginx.pid) -o etime'") " " ))))

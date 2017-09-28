(defun a-get (key alist)
	"Get the valur for given key"
	(cdr (assoc key alist)))

(defun a-get-in (alist &rest keys)
	"Recursively find KEYs in ALIST."
	(while keys
		(setq alist (cdr (assoc (pop keys) alist))))
	alist)

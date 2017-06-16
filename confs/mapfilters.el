(defun my/get-sym-val (elem)
	"If elem is a fun returns call it, if not but a symbol defefer it, if not return the elem itself

Example usage with mapcar
(setq dummy-var \"barfoo\")
(mapconcat 'my/get-sym-val '(\"foo\" \"bar\" (lambda () (concat \"foo\" \"bar\"))) \", \")
(mapconcat 'my/get-sym-val '(\"foo\" \"bar\" (lambda () (concat \"foo\" \"bar\")) dummy-var) \", \")"
	(cond
	 ((functionp elem) (funcall elem))
	 ((symbolp elem) (symbol-value elem))
	 (t elem)))

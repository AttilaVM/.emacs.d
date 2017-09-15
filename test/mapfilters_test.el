(ert-deftest get-sym-val-test-with-fun ()
	"Shuld return the first element from a list, which applies to the criteria definde by the filter function"
	(should (equal (mapconcat 'my/get-sym-val '("foo" "bar" (lambda () (concat "foo" "bar"))) ", ")
								 "foo, bar, foobar"))
	)

(ert-deftest get-sym-val-test-with-var ()
	"Shuld return the first element from a list, which applies to the criteria definde by the filter function"
	(let ((dummy-var "foobar"))
			(should (equal (mapconcat 'my/get-sym-val '("foo" "bar" dummy-var) ", ")
								 "foo, bar, foobar"))))

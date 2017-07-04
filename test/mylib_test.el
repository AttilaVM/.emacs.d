(ert-deftest get-first-match ()
	"Shuld return the first element from a list, which applies to the criteria definde by the filter function"
	(should (equal (my/get-first-match '(1 2 3 4 5 6) (function (lambda (elem)
						 (if (> elem 2)
					 elem
							 nil)))) 3)))

(ert-deftest keys-test ()
	(should (equal (my/keys '((key1 . "foo")
														("key2" . "bar")
														(1 . "bazz")))
								 '(key1 "key2" 1)
									)))

(ert-deftest my/get ()
	(should (my/get "key" '((key1 . "foo")
										 ("key2" . "bar")
										 (1 . "bazz")))))

(ert-deftest my/get ()
	(should (my/get "key2" '((key1 . "foo")
										 ("key2" . "bar")
										 (1 . "bazz"))) "bar"))

(ert-deftest enclosure-test ()
	(should (my/enclosure-p ?\")))

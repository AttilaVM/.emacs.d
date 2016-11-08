(ert-deftest get-first-match ()
  "Shuld return the first element from a list, which applies to the criteria definde by the filter function"
  (should (equal (my/get-first-match '(1 2 3 4 5 6) (function (lambda (elem)
				     (if (> elem 2)
					 elem
				       nil)))) 3)))

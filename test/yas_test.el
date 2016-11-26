(ert-deftest angular-di-generator ()
  "Should return the correnct function arguments from di string"
  (should (equal (my/angular-di-generator "'$scope', '$timeout'")
		 "$scope, $timeout")))

(ert-deftest char-cleaner ()
  "Should return a string clean from non alphanumeric and _ chars"
  (should (equal (my/char-cleaner "This is $ Test :_-$")
		 "ThisisTest")))

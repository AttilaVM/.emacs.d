(ert-deftest angular-di-generator ()
  "Should return the correnct function arguments from di string"
  (should (equal (my/angular-di-generator "'$scope', '$timeout'")
		 "$scope, $timeout")))

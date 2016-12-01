(use-package bats-mode
  :bind
  (:map bats-mode-map
	("C-c C-t f" . bats-run-current-file)
	("C-c C-t d" . bats-run-all) ;; Run all in cwd
	("C-c C-t ." . bats-run-current-test)))

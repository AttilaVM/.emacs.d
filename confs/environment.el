(unless (s-blank-p (getenv "NIXPKGS_CONFIG"))
	(setenv "SHELL" "/run/current-system/sw/bin/bash"))

(setq user-full-name "Attila V. Molnar")
(defvar user-home-dir (getenv "HOME") "Define home directory of the user")
(defvar user/home-buffer "~/Documents/reminder.org")

(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))
		(setq exec-path (append exec-path '("~/.local/bin")))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)
(toggle-frame-fullscreen)

(setq user/remotes '(("localhost" . ("localhost" "22"))
										 ("a2hosting" . '("attila@a2hosting" "7822"))))

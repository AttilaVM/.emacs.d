(require 'dired)
(setq dired-dwim-target t)

;; auto refresh after file operation
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

(define-key dired-mode-map (kbd "i") nil)
(define-key dired-mode-map (kbd "q") nil)
(define-key dired-mode-map (kbd "q") 'dired-view-file)
(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "c") nil)
(define-key dired-mode-map (kbd "C") nil)
(define-key dired-mode-map (kbd "c") 'dired-do-copy)
(define-key dired-mode-map (kbd "C") 'dired-do-compress-to)
(define-key dired-mode-map (kbd ".") nil)
(define-key dired-mode-map (kbd "f") nil)
(define-key dired-mode-map (kbd "f o") 'dired-find-file-other-window)
(define-key dired-mode-map (kbd "f f") 'dired-find-file)
(define-key dired-mode-map (kbd "o") 'dired-display-file)
(define-key dired-mode-map (kbd "<insert> o o") 'dired-find-file-other-window)
(define-key dired-mode-map (kbd "<insert> o d") 'dired-display-file)

(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
(define-key dired-mode-map (kbd "<insert> j r") 'dired-toggle-read-only)
(define-key dired-mode-map (kbd "<insert> m q") 'dired-unmark-all-files)

(define-key dired-mode-map (kbd "e") nil)
(define-key dired-mode-map (kbd "e") nil)

(add-hook 'dired-load-hook '(lambda () (require 'dired-x))) ; Load Dired X when Dired is loaded.
		(setq dired-omit-mode t) ; Turn on Omit mode.

;; (use-package openwith
;;	:config
;;	Scrwews up srewer mode!!!
;;	(setq openwith-associations '(("\\.pdf\\'" "okular" (file))
;;																("\\.png\\'" "gwenview" (file))
;;																("\\.png\\'" "gwenview" (file))
;;																("\\.tif\\'" "gwenview" (file))))
;;	(openwith-mode t))

(use-package dired-dups
	:bind
	(:map dired-mode-map
				("<insert> j d" . dired-find-duplicates)))

(use-package dired-ranger)

(use-package dired-subtree
	:config
	(define-key dired-mode-map (kbd "j") nil)
	(define-key dired-mode-map (kbd "l") nil)
	:bind (:map dired-mode-map
							(";" . helm-occur)
							("j" . dired-subtree-insert)
							("l" . dired-subtree-remove)
							))

(require 'dired-subtree)
(define-key dired-mode-map (kbd "j") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "l") 'dired-subtree-remove)

(use-package dired-quick-sort
	:config
	(dired-quick-sort-setup)
	:bind
	(:map dired-mode-map
				("<insert> j s" . hydra-dired-quick-sort/body)))

(use-package dired-hide-dotfiles
	:config

	:bind (:map dired-mode-map
							("." . dired-hide-dotfiles-mode)))

(use-package dired-filter
	:config


	(add-hook 'dired-mode-hook (lambda ()
															 (dired-filter-mode)
															 (dired-filter-by-dot-files)))
	:bind
	(:map dired-mode-map
				("i S" . dired-filter-save-filters)
				("i o" . dired-filter-load-saved-filters)
				("<insert> j g" . dired-filter-group-mode)
				;; filter BY
				("i i" . dired-filter-mode)
				("i n" . dired-filter-by-name)
				("i x" . dired-filter-by-executable)
				("i e" . dired-filter-by-extension)
				("i ." . dired-filter-by-dot-files)
				("i m" . dired-filter-by-mode)
				("i d" . dired-filter-by-directory)
				("i s" . dired-filter-by-symlink)
				;; TODO does not work with subtrees
				("i g" . dired-filter-by-git-ignored)
				("i r" . dired-filter-by-regexp)
				("i p" . dired-filter-by-predicate)
				;; logical expression
				("i l o" . dired-filter-or)
				("i l n" . dired-filter-negate)
				;; disable all filter
				("i q" . dired-filter-pop-all)
				;; mark
				("<insert> m n" . dired-filter-mark-by-name)
				("<insert> m x" . dired-filter-mark-by-executable)
				("<insert> m e" . dired-filter-mark-by-extension)
				("<insert> m ." . dired-filter-mark-by-dot-files)
				("<insert> m m" . dired-filter-mark-by-mode)
				("<insert> m d" . dired-filter-mark-by-directory)
				("<insert> m s" . dired-filter-mark-by-symlink)
				;; TODO does not work with subtrees
				("<insert> m g" . dired-filter-mark-by-git-ignored)
				("<insert> m r" . dired-filter-mark-by-regexp)
				("<insert> m p" . dired-filter-mark-by-predicate)
				))

(require 'dired-filter)
(dired-filter-save-filters "backup"
															 '((or
																(regexp . ".bck$")
																(regexp . "~$")
																(regexp . "-bck$"))))
	(dired-filter-save-filters "images"
															 '((or
																(extension . "png")
																(extension . "jpg")
																(extension . "jpeg")
																(extension . "tif")
																(extension . "tiff")
																(extension . "gif")
																(extension . "exr")
																(extension . "svg")
																(extension . "cr2"))))

	(dired-filter-save-filters "documents"
															 '((or
																	(extension . "txt")
																	(extension . "rtd")
																	(extension . "org")
																	(extension . "md")
																	(extension . "markdown")
																	(extension . "eldoc")
																	(extension . "tex")
																	(extension . "bib")
																	(extension . "odt")
																	(extension . "odc")
																	(extension . "odp")
																	(extension . "doc")
																	(extension . "docx")
																	(extension . "xls")
																	(extension . "xlsx")
																	(extension . "ppt")
																	(extension . "pptx")
																	(extension . "epub")
																	(extension . "djv")
																	(extension . "djvu")
																	(extension . "pdf")
																	)))
(dired-filter-save-filters "code"
															 '((or
																	(extension . "clj")
																	(extension . "cljs")
																	(extension . "cljc")
																	(extension . "py")
																	(extension . "rb")
																	(extension . "r")
																	(extension . "js")
																	(extension . "elm")
																	(extension . "hs")
																	(extension . "nix")
																	(extension . "h")
																	(extension . "c")
																	(extension . "cpp")
																	(extension . "java")
																	(extension . "go")
																	(extension . "sh")
																	(extension . "php")
																	)))

	(dired-filter-save-filters "video"
															 '((or
																	(extension . "mkv")
																	(extension . "ogv")
																	(extension . "mp4")
																	(extension . "avi"))))

	(dired-filter-save-filters "sound"
															 '((or
																	(extension . "flac")
																	(extension . "ogg")
																	(extension . "wav")
																	(extension . "mp3"))))

(dired-filter-save-filters "archives"
															 '((or
																	(extension . "zip")
																	(extension . "7z")
																	(extension . "tar.gz")
																	(extension . "tar.xz")
																	(extension . "tar.bz")
																	(extension . "tar.bz")
																	(extension . "rar")
																	)))

(dired-filter-save-filters "disk-images"
															 '((or
																	(extension . "iso")
																	(extension . "img")
																	(extension . "raw")
																	)))

(setq dired-filter-group-saved-groups
				'(("default"
					 ("Directories" (directory))
					 ("Documents" "documents")
					 ("Code" "code")
					 ("Images" "images")
					 ("Video" "video")
					 ("Sound" "sound")
					 ("Archives" "archives")
					 ("Disk Images" "disk-images")
					 )
					))

;; TODO Should I use it, anyway? bad performance and svg scaling.
(use-package image+)

(use-package dired-rainbow
	:config
	(progn
		(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
		(dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))

		(dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub"))
		(dired-rainbow-define excel "#3465a4" ("xlsx"))
		(dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))

		(dired-rainbow-define log "#c17d11" ("log"))
		(dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "h" "java" "pl" "rb" "R" "php"))

		(dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
		(dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
		(dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
		(dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

		(dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
		))

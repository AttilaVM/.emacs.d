;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)



(defvar user-emacs-config-file (getenv "EMACS_CONFIG") "path to user alternative config file")

(load "~/.emacs.d/config.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-filter-saved-filters
	 (quote
		(("disk-images"
			(or
			 (extension . "iso")
			 (extension . "img")
			 (extension . "raw")))
		 ("archives"
			(or
			 (extension . "zip")
			 (extension . "7z")
			 (extension . "tar.gz")
			 (extension . "tar.xz")
			 (extension . "tar.bz")
			 (extension . "tar.bz")
			 (extension . "rar")))
		 ("sound"
			(or
			 (extension . "flac")
			 (extension . "ogg")
			 (extension . "wav")
			 (extension . "mp3")))
		 ("video"
			(or
			 (extension . "mkv")
			 (extension . "ogv")
			 (extension . "mp4")
			 (extension . "avi")))
		 ("code"
			(or
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
			 (extension . "php")))
		 ("documents"
			(or
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
			 (extension . "pdf")))
		 ("images"
			(or
			 (extension . "png")
			 (extension . "jpg")
			 (extension . "jpeg")
			 (extension . "tif")
			 (extension . "tiff")
			 (extension . "gif")
			 (extension . "exr")
			 (extension . "svg")
			 (extension . "cr2")))
		 ("backup"
			(or
			 (regexp . ".bck$")
			 (regexp . "~$")
			 (regexp . "-bck$")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
(setq custom-file "~/.emacs.d/custom-options-old.el")
			 ;; ("marmalade" . "https://marmalade-repo.org/packages")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")
			 ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))

(package-initialize)
;; Make sure that use-package is installed
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)
;; Make use package automaticly install missing packages
(setq use-package-always-ensure t)

;; Low level deps for elisp programing
(load "~/.emacs.d/confs/libs.el")
(load "~/.emacs.d/confs/environment.el")

;; User specific information
(load "~/.emacs.d/user.el")

;; functions to make Emacs configuration easier
(load "~/.emacs.d/confs/mylib.el")
(load "~/.emacs.d/confs/mapfilters.el")
;; (load "~/.emacs.d/confs/validate.el")

;; System constants
(load "~/.emacs.d/confs/constants.el")

;; Define most fundemental Emacs behaviors, appearance and user specific settings
(load "~/.emacs.d/confs/behavior.el")
(load "~/.emacs.d/confs/company.el")
(load "~/.emacs.d/confs/appearance.el")
(load "~/.emacs.d/confs/regex.el")
(load "~/.emacs.d/confs/flycheck.el")
(load "~/.emacs.d/confs/tree.el")
(load "~/.emacs.d/confs/helpers.el")
(load "~/.emacs.d/confs/nav.el")
(load "~/.emacs.d/confs/edit.el")
(load "~/.emacs.d/confs/spellcheck.el")
(load "~/.emacs.d/confs/collaboration.el")
;; (load "~/.emacs.d/confs/workgroups.el")
(load "~/.emacs.d/confs/ai.el")
(load "~/.emacs.d/confs/term.el")
(load "~/.emacs.d/confs/dired.el")
(load "~/.emacs.d/confs/vc.el")
(load "~/.emacs.d/confs/docker.el")
(load "~/.emacs.d/confs/guide.el")
(load "~/.emacs.d/confs/network.el")

;; Project management and higher capabilities
(load "~/.emacs.d/confs/helm.el")
(load "~/.emacs.d/confs/gnus.el")
(load "~/.emacs.d/confs/sunrise-commander.el")
(load "~/.emacs.d/confs/realgud.el") ; debugger
(load "~/.emacs.d/confs/docview.el")

;; Research
(load "~/.emacs.d/confs/latex.el")
(load "~/.emacs.d/confs/org.el")

;; Language specific
(load "~/.emacs.d/confs/elisp.el")
(load "~/.emacs.d/confs/js.el")
(load "~/.emacs.d/confs/elm.el")
(load "~/.emacs.d/confs/python.el")
(load "~/.emacs.d/confs/julia.el")
(load "~/.emacs.d/confs/typescript.el")
(load "~/.emacs.d/confs/clojure.el")
(load "~/.emacs.d/confs/shell.el")
(load "~/.emacs.d/confs/haskell.el")
(load "~/.emacs.d/confs/web.el")
(load "~/.emacs.d/confs/css.el")
(load "~/.emacs.d/confs/nix.el")
(load "~/.emacs.d/confs/yaml.el")
(load "~/.emacs.d/confs/graphql.el")
;; (load "~/.emacs.d/confs/sql.el")
(load "~/.emacs.d/confs/maxima.el")
(load "~/.emacs.d/confs/prolog.el")
(load "~/.emacs.d/confs/glsl.el")
(load "~/.emacs.d/confs/data_analysis.el")
(load "~/.emacs.d/confs/ibuffer.el")
(load "~/.emacs.d/confs/admin.el") ;; Editing config files and other admin stuffs
;; Data specific
(load "~/.emacs.d/confs/csv.el")

;; Devops
(load "~/.emacs.d/devops/core.el")

(load "~/.emacs.d/confs/kite-mini.el")

;; Multimedia
(when (executable-find "rtorrent")
	(load "~/.emacs.d/confs/mentor.el"))
(when (or (executable-find "vlc") (executable-find "mplayer"))
	(load "~/.emacs.d/confs/emms.el"))

;; Browsing
(when (executable-find "conkeror")
	(load "~/.emacs.d/confs/conkeror.el"))
(when (executable-find "w3m")
(load "~/.emacs.d/confs/w3m.el"))

;; System interop
(when (string-equal "Gentoo" my/system-os)
	(load "~/.emacs.d/confs/gentoo.el"))

(load "~/.emacs.d/extensions/liquidsoap-mode.el")

(load "~/.emacs.d/confs/yas.el")
(load "~/.emacs.d/confs/after-save.el")
(load "~/.emacs.d/confs/key-chords.el")


(load "~/.emacs.d/proxies.el")
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
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(unless (package-installed-p 'auctex)
  (package-install 'auctex))


(use-package tex
  ;; Ensure is needed because package manage calls AUCTEX as auctex why Emacs refers to it as tex...
  :ensure auctex
  :pin gnu
  :config
  (eval-after-load "tex"
    ;; Add `View' latex command which will make okular to open rendered document
  '(add-to-list 'TeX-command-list
		'("View"  "okular --presentation --unique %o#src:%n%b" TeX-run-discard-or-function nil t :help "View file")))
  ;; source: http://stackoverflow.com/questions/14664829/emacs-auctex-prefix-arguments
  (defun my-run-latex ()
    "Compile .tex into pdf without ask for save confirmation"
    (interactive)
    (if (buffer-modified-p)
	(progn
	  (setq TeX-save-query nil)
	  (TeX-save-document (TeX-master-file))
	  (TeX-command "LaTeX" 'TeX-master-file -1))
      (TeX-view)))
  :bind
  (:map TeX-mode-map
	;; Compile Latex into PDF without asking too many questions
	("C-c C-c" . my-run-latex)
	("s-c s-c" . TeX-command-master)))

;; compile to PDF automaticly
(setq TeX-PDF-mode t)
(require 'tex)
;; (TeX-global-PDF-mode t)

(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


'(LaTeX-command "latex -synctex=1")

;; Autocomplete latex via company
(use-package company-auctex
  :config
  (company-auctex-init))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;        LaTeX         ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ##### Run emacs in server mode in order to be able to use
;; ##### emacsclient in Okular. Don't forget to configure
;; ##### Okular to use emacs in
;; ##### "Configuration/Configure Okular/Editor"
;; ##### => Editor => Emacsclient. (you should see
;; ##### emacsclient -a emacs --no-wait +%l %f
;; ##### in the field "Command".

(setq-default TeX-master nil)
;; ##### Enable synctex correlation. From Okular just press
;; ##### Shift + Left click to go to the good line.
(setq TeX-source-correlate-method 'synctex)
;; ##### Enable synctex generation. Even though the command shows
;; ##### as "latex" pdflatex is actually called


;; ##### Use Okular to open your document at the good
;; ##### point. It can detect the master file.
(add-to-list 'TeX-expand-list
	     '("%u" Okular-make-url))

(defun Okular-make-url () (concat
			   "file://"
			   (expand-file-name (funcall file (TeX-output-extension) t)
					     (file-name-directory (TeX-master-file)))
			   "#src:"
			   (TeX-current-line)
			   (expand-file-name (TeX-master-directory))
			   "./"
			   (TeX-current-file-name-master-relative)))

;; ## Use these lines if you want a confirmation of the
;; ## command line to run...
;; (setq TeX-view-program-selection '((output-pdf "Okular")))
;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
;; ## And theses if you don't want any confirmation.
;;  (setq TeX-view-program-selection '((output-pdf "Okular")))

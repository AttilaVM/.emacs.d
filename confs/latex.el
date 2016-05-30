
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

;; Autocomplete latexx
(require 'auto-complete-auctex)

 '(LaTeX-command "latex -synctex=1")


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
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("View" "okular --unique %u" TeX-run-discard-or-function nil t :help "View file"))
 )

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

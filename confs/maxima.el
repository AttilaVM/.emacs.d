 (autoload 'maxima-mode "maxima" "Maxima mode" t)
 (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
 (autoload 'maxima "maxima" "Maxima interaction" t)
 (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
 (setq imaxima-use-maxima-mode-flag t)
 (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

;; imaxima provides beautiful latex renders, instead of native ASCII rendering
(setq imaxima-use-maxima-mode-flag t)
;; Possible values are "normalsize", "large", and "Large".
(setq imaxima-fnt-size "Large")
(setq imaxima-pt-size 9)
(setq imaxima-print-tex-command "latex %s; dvipdf %s.dvi imax.pdf; open imax.pdf")

(define-key comint-mode-map (kbd "C-c k l") 'comint-kill-whole-line)
(define-key comint-mode-map (kbd "C-c k r") 'comint-kill-region)

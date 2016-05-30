;; JavaScript IDE capabilities
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
;; Configure imenu for js2-mode
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

(add-something-to-mode-hooks '(js) 'fic-ext-mode)

;; Configure refactoring
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; tern-mode for IDE features like code completition, jump to definition etc... it requires a tern server
(require 'tern)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; tern auto completion
;; (eval-after-load 'tern
;;    '(progn
;;       (tern-ac-setup)))

;; Hook json-mode to .jscsrc
(add-to-list 'auto-mode-alist '("\\.jscsrc\\'" . json-mode))
;; Hook JSCS to js and JSON modes
(add-hook 'js-mode-hook #'jscs-indent-apply)
(add-hook 'js2-mode-hook #'jscs-indent-apply)
(add-hook 'json-mode-hook #'jscs-indent-apply)

;; Set up company mode for tern
(require 'company-tern)
(eval-after-load 'company
    '(add-to-list 'company-backends 'company-tern))

;; Abality to run nodejs REPL inside emacs
(require 'nodejs-repl)

(require 'grunt)

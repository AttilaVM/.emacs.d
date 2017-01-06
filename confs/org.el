(require 'org)

;; Set up source code fontification
(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook 'toggle-truncate-lines)

(global-set-key (kbd "C-c 0") 'org-agenda)

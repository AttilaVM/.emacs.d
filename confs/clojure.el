;;; clojure.el --- Clojure specific code

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: theme appearance
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

(use-package cider
  :init
  :config
  (add-hook 'clojure-mode-hook' cider-mode))


(use-package elein)
;; clojure.el ends here

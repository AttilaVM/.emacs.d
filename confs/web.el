;;; web.el --- Configure varios non-js front end development functionalities

;;; Commentary:
;; Author: Attila V. Molnar
;; Keywords: web frontend css stylus html
;; Emacs: GNU Emacs 24.0 or later
;; Version: 0.3
;;; Code:

;; There is a better stylus mode which is not available on MELPA
;; Try to load it first.
(if (file-readable-p "/home/attila/.emacs.d/extensions/stylus-mode/stylus-mode.el")
    (load "/home/attila/.emacs.d/extensions/stylus-mode/stylus-mode.el")
  (progn
    (message "There is a better stylus mode avaliable at: https://github.com/vladh/stylus-mode")
    (use-package stylus-mode)))

;;; helm.el ends here

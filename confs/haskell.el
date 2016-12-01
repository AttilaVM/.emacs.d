(use-package haskell-mode
  :config
  ;; Haskell confing
  (use-package hindent
    :config
    (add-hook 'haskell-mode-hook #'hindent-mode))

  :bind
  (:map haskell-mode-map
	("C-c C-c" . haskell-process-load-file)
	("C-c C-s" . haskell-session-chage)))

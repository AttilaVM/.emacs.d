(defun my/add-hooks (hooks mode)
  "Add one mode to multiple hooks"
  (dolist (hook hooks)
    (add-hook hook mode)))



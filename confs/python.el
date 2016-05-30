;; elpy python IDE
;; Elpy works on the top of python mode
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-something-to-mode-hooks '(python) 'fic-ext-mode)
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq-default indent-tabs-mode nil)
;; Set $PYTHONPATH to elpy module
(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" user-home-dir "/.emacs.d/elpa/elpy-20160131.118"))

(setq elpy-rpc-backend "jedi")

;; clear Ipython console
(defun clearConsole ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


;; Make defintition jumping more robust
;; see https://github.com/jorgenschaefer/elpy/wiki/Customizations
(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                   (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))


(defun elpy-toggle-backend ()
  "Toggle between jedi and rope backends for Elpy"
  (interactive)
  (message (concat "RPC backend changed to " elpy-rpc-backend))
  )


(defun my-restart-python-console ()
  "Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed"
  (interactive)
  (let ((running-process (get-buffer-process "*Python*")))
    (if (equal (get-buffer-process "*Python*") nil)
        (elpy-shell-send-region-or-buffer)
      (message (concat "killing: " (prin1-to-string (get-process running-process))))
         (kill-process running-process)
    (while (not (equal (get-buffer-process "*Python*") nil))
      (sleep-for 0.01))
    (kill-buffer "*Python*"))
    (elpy-shell-send-region-or-buffer)))

;; Django mode
(require 'python-django)

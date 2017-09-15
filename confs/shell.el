;; eshell

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(global-set-key (kbd "<insert> 2 e") 'eshell)

(require 'term)
(global-set-key (kbd "<insert> 2 t") 'term)


(add-hook 'eshell-mode-hook #'(lambda ()
																(eshell/alias "h" "helm-eshell-history")
																(eshell/alias "c" "clear")
																(eshell/alias "top" "helm-top")
																(eshell/alias "htop" "helm-top")
																(eshell/alias "tree" "my/eshell-tree")))

(use-package eshell-autojump
	:config

	(defun my/clear-eshell-prompt ()
		(interactive)
		(with-current-buffer "*eshell*"
			(call-interactively 'move-end-of-line)
			(call-interactively  'set-mark-command)
			(call-interactively 'eshell-bol)
			(delete-backward-char 1)
			))

	(defun my/eshell-tree ()
		(interactive)
		(call-interactively 'neotree-dir))

	(defun helm-eshell-autojump ()
		(interactive)
		(let ((target-dir (helm :sources (helm-build-sync-source "test"
																			 :candidates (eshell-autojump-candidates))
														:buffer "*helm eshell jump")))
			(when target-dir (with-current-buffer "*eshell*"
				(eshell-return-to-prompt)
				(my/clear-eshell-prompt)
				(insert (concat "cd " target-dir))
				(eshell-send-input))))))



;; Bash completion
(use-package bash-completion
	:config
	(bash-completion-setup))

;; Bash unit testing
(use-package bats-mode
	:bind
	(:map bats-mode-map
	("C-c C-t f" . bats-run-current-file)
	("C-c C-t d" . bats-run-all) ;; Run all in cwd
	("C-c C-t ." . bats-run-current-test)))


(use-package fish-mode
	:config
	(add-hook 'fish-mode-hook (lambda ()
														 (add-hook 'before-save-hook 'fish_indent-before-save))))

;; Shell line or region to execute

(defun sh-send-line-or-region (&optional step)
	(interactive ())
	(let ((proc (get-process "shell"))
				pbuf min max command)
		(unless proc
			(let ((currbuff (current-buffer)))
				(shell)
				(switch-to-buffer currbuff)
				(setq proc (get-process "shell"))
				))
		(setq pbuff (process-buffer proc))
		(if (use-region-p)
				(setq min (region-beginning)
							max (region-end))
			(setq min (point-at-bol)
						max (point-at-eol)))
		(setq command (concat (buffer-substring min max) "\n"))
		(with-current-buffer pbuff
			(goto-char (process-mark proc))
			(insert command)
			(move-marker (process-mark proc) (point))
			) ;;pop-to-buffer does not work with save-current-buffer -- bug?
		(process-send-string  proc command)
		(display-buffer (process-buffer proc) t)
		(when step
			(goto-char max)
			(next-line))
		))

(defun sh-send-line-or-region-and-step ()
	(interactive)
	;; Clear *shell* buffer
	(when-let ((shell-buffer (get-buffer "*shell*")))
		(with-current-buffer shell-buffer
			(my/commint-clear)))
	(sh-send-line-or-region t))

(defun sh-switch-to-process-buffer ()
	(interactive)
	(pop-to-buffer (process-buffer (get-process "shell")) t))

(defun sh-send-buffer-and-step ()
	(interactive)
	(mark-whole-buffer)
	(sh-send-line-or-region-and-step))

(require 'sh-script)
(define-key sh-mode-map (kbd "<insert> e e") 'sh-send-line-or-region-and-step)
(define-key sh-mode-map (kbd "<insert> e b") 'sh-send-buffer-and-step)

;; customized eshell prompt

;; (require 'dash)
;; (require 's)

;; (defmacro with-face (STR &rest PROPS)
;;   "Return STR propertized with PROPS."
;;   `(propertize ,STR 'face (list ,@PROPS)))

;; (defmacro esh-section (NAME ICON FORM &rest PROPS)
;;   "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
;;   `(setq ,NAME
;;          (lambda () (when ,FORM
;;                  (-> ,ICON
;;                     (concat esh-section-delim ,FORM)
;;                     (with-face ,@PROPS))))))

;; (defun esh-acc (acc x)
;;   "Accumulator for evaluating and concatenating esh-sections."
;;   (--if-let (funcall x)
;;       (if (s-blank? acc)
;;           it
;;         (concat acc esh-sep it))
;;     acc))

;; (defun esh-prompt-func ()
;;   "Build `eshell-prompt-function'"
;;   (concat esh-header
;;           (-reduce-from 'esh-acc "" eshell-funcs)
;;           "\n"
;;           eshell-prompt-string))

;; (esh-section esh-dir
;;              "\xf07c"  ;  (faicon folder)
;;              (abbreviate-file-name (eshell/pwd))
;;              '(:foreground "gold" :bold ultra-bold :underline t))

;; (esh-section esh-git
;;              "\xe907"  ;  (git icon)
;;              (magit-get-current-branch)
;;              '(:foreground "pink"))

;; (esh-section esh-python
;;              "\xe928"  ;  (python icon)
;;              pyvenv-virtual-env-name)

;; (esh-section esh-clock
;;              "\xf017"  ;  (clock icon)
;;              (format-time-string "%H:%M" (current-time))
;;              '(:foreground "forest green"))

;; ;; Below I implement a "prompt number" section
;; (setq esh-prompt-num 0)
;; (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
;; (advice-add 'eshell-send-input :before
;;             (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

;; (esh-section esh-num
;;              "\xf0c9"  ;  (list icon)
;;              (number-to-string esh-prompt-num)
;;              '(:foreground "brown"))

;; ;; Separator between esh-sections
;; (setq esh-sep "  ")  ; or " | "

;; ;; Separator between an esh-section icon and form
;; (setq esh-section-delim " ")

;; ;; Eshell prompt header
;; (setq esh-header "\n ")  ; or "\n┌─"

;; ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; ;; your login, these can be the same.
;; (setq eshell-prompt-regexp " ")   ; or "└─> "
;; (setq eshell-prompt-string " ")   ; or "└─> "

;; ;; Choose which eshell-funcs to enable
;; (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))

;; ;; Enable the new eshell prompt
;; (setq eshell-prompt-function 'esh-prompt-func)

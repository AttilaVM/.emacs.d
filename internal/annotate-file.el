;;; org-annotate-file.el --- Annotate a file with org syntax

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.2

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is yet another implementation to allow the annotation of a
;; file without modification of the file itself. The annotation is in
;; org syntax so you can use all of the org features you are used to.

;; To use you might put the following in your .emacs:
;;
;; (require 'org-annotate-file)
;; (global-set-key (kbd "C-c C-l") 'org-annotate-file)
;;
;; To change the location of the annotation file:
;;
;; (setq org-annotate-file-storage-file "~/annotated.org")
;;
;; Then when you visit any file and hit C-c C-l you will find yourself
;; in an org buffer on a headline which links to the file you were
;; visiting, e.g:

;; * ~/org-annotate-file.el

;; Under here you can put anything you like, save the file
;; and next time you hit C-c C-l you will hit those notes again.
;;
;; To put a subheading with a text search for the current line set
;; `org-annotate-file-add-search` to non-nil value. Then when you hit
;; C-c C-l (on the above line for example) you will get:

;; * ~/org-annotate-file.el
;; ** `org-annotate-file-add-search` to non-nil value. Then whe...

;; Note that both of the above will be links.

(require 'org)

(defvar org-annotate-file-storage-file "~/.org-annotate-file.org"
  "File in which to keep annotations.")

(defvar org-annotate-file-add-search nil
  "If non-nil then add a link as a second level to the actual
location in the file")

(defvar org-annotate-file-always-open t
  "non-nil means always expand the full tree when you visit
`org-annotate-file-storage-file'.")

(defun org-annotate-file-prettyfy-desc (string)
  "Strip starting and ending whitespace and replace any chars
after the 60th with '...'"
  (let ((replace-map '(("^[ \t]*" . "")
                       ("[ \t]*$" . "")
                       ("^\\(.\\{60\\}\\).*" . "\\1..."))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(defun org-annotate-file ()
  "Put a section for the current file into your annotation file"
  (interactive)
  (unless (buffer-file-name)
    (error "This buffer has no associated file."))
  (org-annotate-file-show-section))

(defun org-annotate-file-show-section (&optional buffer)
  "Visit the buffer named `org-annotate-file-storage-file' and
show the relevant section"
  (let* ((filename (abbreviate-file-name (or buffer (buffer-file-name))))
				 (linum (format-mode-line "%l"))
         (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (link (org-make-link-string (concat "file:" filename)))
         (search-link (org-make-link-string
                       (concat "file:" filename "::" line)
                               (org-annotate-file-prettyfy-desc line))))
    (with-current-buffer (find-file org-annotate-file-storage-file)
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (goto-char (point-min))
      (widen)
      (when org-annotate-file-always-open
        (outline-show-all))
      (unless (search-forward-regexp
               (concat "^* " (regexp-quote link)) nil t)
        (org-annotate-file-add-upper-level link))
      (beginning-of-line)
      (org-narrow-to-subtree)
      ;; deal with a '::' search if need be
      (when org-annotate-file-add-search
        (unless (search-forward-regexp
                 (concat "^** " (regexp-quote search-link)) nil t)
          (org-annotate-file-add-second-level search-link linum))))))

(defun org-annotate-file-add-upper-level (link)
  (goto-char (point-min))
  (call-interactively 'org-insert-heading)
  (insert link))

(defun org-annotate-file-add-second-level (link linum)
  (goto-char (point-at-eol))
  (call-interactively 'org-insert-subheading)
  (insert (concat link " " linum)))

;;;###autoload
(defun org-annotate-file-find-storage-file ()
	(interactive)
	(find-file org-annotate-file-storage-file)) 

(provide 'org-annotate-file)
;;; org-annotate-file.el ends here
;; TODO: Automaticly add playlists, playing order
(use-package emms
  :config
  (require 'emms-source-file)
  (require 'emms-setup)
  (require 'emms-source-playlist)
  (require 'emms-player-simple)
  (require 'emms-player-mplayer)
  (require 'emms-info-libtag)
  (emms-all)

  (emms-default-players)

  (add-to-list 'auto-mode-alist '("\\*music*\\'" . emms-playlist-mode))

  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
		  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
		  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

  (setq emms-player-list '(emms-player-mpg321
			   emms-player-ogg123
			   emms-player-mplayer))

  ;; Enable scoring
  (emms-score-enable)

  (setq emms-info-asynchronously nil)
  (setq emms-playlist-buffer-name "*Music*")

  (defun my/emms-seek-beg ()
    "Seek to the beggining of the current track"
    (interactive)
    (emms-seek -10000))

  (defun my/emms-jump-forward ()
    "Seek to the beggining of the current track"
    (interactive)
    (emms-seek 15))

  ;; Actions in the playlist bufferx
  ;; (define-key emms-playlist-mode-map (kbd "m") 'emms-mark-track)
  ;; (define-key emms-playlist-mode-map (kbd "u") 'emms-mark-unmark-track)
  ;; (define-key emms-playlist-mode-map (kbd "a") 'emms-mark-all)
  ;; (define-key emms-playlist-mode-map (kbd "t") 'emms-mark-toggle)
  ;; (define-key emms-playlist-mode-map (kbd "t") 'emms-mark-toggle)
  ;; (define-key emms-playlist-mode-map (kbd "s") 'emms-mark-regexp)
  ;; (define-key emms-playlist-mode-map (kbd "w") 'emms-playlist-save)

  ;; (define-key emms-playlist-mode-map (kbd "d") 'emms-mark-delete-marked-tracks)
  ;; (define-key emms-playlist-mode-map (kbd "k") 'emms-mark-copy-marked-tracks)
  ;; (define-key emms-playlist-mode-map (kbd "t") 'emms-tag-editor-edit-marked-tracks)
  :bind
  (
   ;; Navigation
   ("<f6>" . emms-seek-backward)
   ("<M-f6>" . my/emms-seek-beg)
   ("<f7>" . emms-seek-forward)
   ("<f5>" . my/emms-jump-forward)
   ("<XF86Forward>" . emms-next)
   ("<XF86Back>" . emms-previous)
   ;; ("<C-f8> s" . emms-start)
   ;; play-pause
   ("<C-f8> SPC" . emms-pause)
   ("<XF86AudioPlay>" . emms-pause)
   ("<C-f8> f" . emms-add-file)
   ;; Actions with playlist
   ("<C-f8> p n" . emms-playlist-new)
   ("<C-f8> p f" . emms-add-playlist-file)
   ("<C-f8> p n" . emms-playlist-new)
   ("<C-f8> p r" . emms-repeat-playlist)
   ;; Scoring
   ("<C-f8> s +" . emms-score-up-playing)
   ("<C-f8> s -" . emms-score-down-playing)
   ("<f10>" . emms-score-up-playing)
   ("<f9>" . emms-score-down-playing)
   ("<C-f8> s n" . emms-score-set-playing)
   ("<C-f8> s t" . emms-score-set-tolerance)
   ("<C-f8> s m" . emms-score-change-mood)
   )
  :bind
  ;; Actions with-in playlist
  (:map emms-playlist-mode-map
	;; Move tracks around TODO: ???

	;; Marking
	("m" . emms-mark-track)
	("u" . emms-mark-unmark-track)
	("a" . emms-mark-all)
	("t" . emms-mark-toggle)
	("s" . emms-mark-regexp)
	;; Read-write actions
	("w" . emms-playlist-save)
	("d" . emms-mark-delete-marked-tracks)
	("k" . emms-mark-copy-marked-tracks)
	("e" . emms-tag-editor-edit-marked-tracks)
	;; Scoring
	("+" . emms-score-up-file-on-line)
	("-" . emms-score-down-file-on-line)))

;; TODO give notebook hostname
  (when (string= system-name "attilaWorkStation")
    (progn (defvar emms-source-file-default-directory "~/music")
	   (emms-add-playlist-directory "~/music/playlists")))

(use-package helm-emms)

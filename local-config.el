(setq calx--server-api-url "http://host.com")
(setq calx--server-api-username "username")
(setq calx--server-api-password "password")
(setq calx--server-backup t) ;; do perform backup?
(setq calx--server-backup-path "/home/user/tmp/TODO-backup") ;; path to backup directory

;(require 'simple-httpd)
;(setq httpd-root "d:/repo/minigame2")

;; remove cleanup before save - helpful if repository I'm working on is total shit ignoring
;; basic coding conventions
;(remove-hook 'before-save-hook 'cleanup-before-save)

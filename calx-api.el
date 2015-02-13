;;; calx-api --- calx web api
;;; Commentary:
;;; Code:

(require 'url)

(defun calx--request-callback-new-buffer (status)
  (switch-to-buffer (current-buffer))
  (rename-buffer "*todo*")
  (org-mode)
  (goto-char (point-min))
  (search-forward "\n\n")
  (delete-region (point-min) (point))
  (buffer-enable-undo (current-buffer)))

(defun calx--request-callback-status (status)
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (search-forward "\n\n")
  (message (buffer-substring-no-properties (point) (point-max)))
  (kill-buffer (current-buffer)))

(defun calx--http-post (url callback &optional data)
  (let ((url-request-method "POST")
    (url-request-extra-headers
     '(("Content-Type" . "application/x-www-form-urlencoded")))
    (url-request-data data))
    (url-retrieve url callback)))

;; shold define (defvar calx--server-api-url "http://xyz.com/")
(defvar calx--server-api-url "unknown")
(defvar calx--server-api-username "unknown")
(defvar calx--server-api-password "unknown")

(defun calx--login (username password)
  (calx--http-post (concat calx--server-api-url "/api_login")
		   (lambda (status)
		     (calx--request-callback-status status)
		     (calx-get))
		   (format "username=%s&password=%s" username password)))

(defun calx ()
  (interactive)
  (calx--login calx--server-api-username calx--server-api-password))

(defun calx-login (username password)
  (interactive "sLogin: \nsPassword:")
  (calx--login username password))

(defun calx-get ()
  (interactive)
  (calx--http-post (concat calx--server-api-url "/api_get")
		   'calx--request-callback-new-buffer))

(defun calx-set ()
  (interactive)
  (calx--http-post (concat calx--server-api-url "/api_set")
		   'calx--request-callback-status
		   (concat "text=" (url-hexify-string (buffer-string)))))

(defun calx-logout ()
  (interactive)
  (calx--http-post (concat calx--server-api-url "/api_logout")
		   (lambda (status)
		     (calx--request-callback-status status)
		     (kill-buffer (current-buffer)))))

(add-hook 'org-mode-hook (lambda ()
			   (local-set-key (kbd "C-x C-s") 'calx-set)
			   (local-set-key (kbd "C-x k") 'calx-logout)))

(provide 'calx-api)
;;; calx-api.el ends here

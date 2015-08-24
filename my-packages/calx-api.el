;; -*- lexical-binding: t -*-
;;; calx-api --- calx web api
;;; Commentary:
;;; Code:

(require 'url)

;; shold define (defvar calx--server-api-url "http://xyz.com/")
(defvar calx--server-api-url "unknown")
(defvar calx--server-api-username "unknown")
(defvar calx--server-api-password "unknown")
(defvar calx--keymap (make-sparse-keymap))

(when calx--keymap
  (define-key calx--keymap (kbd "C-x C-s") 'calx-set)
  (define-key calx--keymap (kbd "C-x k") 'calx-logout))

(defun calx--request (url &optional data callback-ok callback-fail)
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data data))
    (url-retrieve url (lambda (status)
			(switch-to-buffer (current-buffer))
			(goto-char (point-min))
			(search-forward "\n\n")
			(let ((errm (buffer-substring-no-properties
				     (point)
				     (point-max))))
			  (cond ((or (string= "forbidden" errm))
				 (message errm)
				 (kill-buffer (current-buffer))
				 (and callback-fail (funcall callback-fail errm)))
				((or (string= "success" errm)
				     (string= "logged out" errm)
				     (string= "ok" errm))
				 (message errm)
				 (kill-buffer (current-buffer))
				 (and callback-ok (funcall callback-ok errm)))
				(t
				 (delete-region (point-min) (point))
				 (rename-buffer "* TODO *")
				 (org-mode)
				 (goto-char (point-min))
				 (buffer-enable-undo)
				 (use-local-map (make-composed-keymap calx--keymap org-mode-map))
				 (add-hook 'moded-save-hook 'calx-set)
				 (add-hook 'moded-kill-hook 'calx-logout)
				 (and callback-ok (funcall callback-ok errm)))))))))

(defun calx--request-login (callback)
  (calx--request (concat calx--server-api-url "/api_login")
		 (format "username=%s&password=%s"
			 calx--server-api-username
			 calx--server-api-password)
		 callback))

(defun calx-get ()
  (interactive)
  (switch-to-buffer (get-buffer-create "* TODO *"))
  (kill-buffer)
  (calx--request (concat calx--server-api-url "/api_get")
		 nil
		 nil
		 (lambda (res) (calx--request-login (lambda (res) (calx-get))))))

(defun calx-set ()
  (interactive)
  (when (string= (buffer-name) "* TODO *")
    (calx--request (concat calx--server-api-url "/api_set")
		   (concat "text=" (url-hexify-string (buffer-string)))
		   nil
		   (lambda (res) (calx--request-login (lambda (res) (calx-set)))))))

(defun calx ()
  (interactive)
  (calx--request-login
   (lambda (res)
     (calx--request (concat calx--server-api-url "/api_get")))))

(defun calx-logout ()
  (interactive)
  (calx--request (concat calx--server-api-url "/api_logout")
		 nil
		 (lambda (status)
		   (kill-buffer (current-buffer)))))

(provide 'calx-api)
;;; calx-api.el ends here

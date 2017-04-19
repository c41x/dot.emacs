(require 'url)
(require 'json)

;; you must define title id and secret key (Emacs Directory Variables is great for this)
(defvar playfab-title-id "")
(defvar playfab-secret-key "")

(defun playfab-get-cloudscript-revision-url ()
  (concat "https://" playfab-title-id ".playfabapi.com/Admin/GetCloudScriptRevision"))

(defun playfab-update-cloudscript-url ()
  (concat "https://" playfab-title-id ".playfabapi.com/Admin/UpdateCloudScript"))

(defun playfab--get-revision (response)
  (cdr (assoc 'Revision (cdr (assoc 'data (json-read-from-string response))))))

(defun playfab-get-revision ()
  (interactive)
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("X-SecretKey" . ,playfab-secret-key)))
        (url-request-data "{}"))
    (url-retrieve (playfab-get-cloudscript-revision-url)
                  ;; response callback
                  (lambda (status)
                    (switch-to-buffer (current-buffer))
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (message (concat "Current CloudScript Revision: " (number-to-string (playfab--get-revision
                                                                                         (buffer-substring-no-properties (point) (point-max))))))
                    (kill-buffer)))))

(defun playfab--get-update-state (response)
  (cdr (assoc 'status (json-read-from-string response))))

(defun playfab-update-cloudscript ()
  (interactive)
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("X-SecretKey" . ,playfab-secret-key)))
        (url-request-data (json-encode `(:Files [(:FileName "main.js" :FileContents ,(buffer-substring-no-properties (point-min) (point-max)))] :Publish ,t))))
    (url-retrieve (playfab-update-cloudscript-url)
                  (lambda (status)
                    (switch-to-buffer (current-buffer))
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (message (concat "Update Status: "
                                     (playfab--get-update-state
                                      (buffer-substring-no-properties (point) (point-max)))
                                     " ver: " (number-to-string (playfab--get-revision (buffer-substring-no-properties (point) (point-max))))))
                    (kill-buffer)))))

(provide 'playfab)
;;; playfab.el ends here

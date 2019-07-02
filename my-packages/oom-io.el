;; TCP client
;; connects to TCP server, returns t if succeeded, nil if not
(defun client-start (host port process-name buffer-name)
  (condition-case nil
      (progn (make-network-process
              :name process-name
              :buffer buffer-name
              :family 'ipv4
              :host host
              :service port
              :sentinel 'listen-sentinel
              :filter 'listen-filter) t)
    (quit nil)
    (file-error nil)
    (error nil)))

;; disconnects from TCP server
(defun client-stop (process-name)
  (condition-case nil
      (progn (delete-process process-name) t)
    (error nil)))

;; send string to TCP server
(defun client-send (process-name msg)
  (condition-case nil
      (progn (process-send-string
              (get-process process-name) msg) t)
    (error nil)))


;; test
;; (client-start "127.0.0.1" 5555 oom-io-process oom-io-buffer)
;; (client-send oom-io-process "emacs")
;; (client-stop oom-io-process)


;; oom io
(defvar oom-io-process "oom io")
(defvar oom-io-buffer "*oom io*")
(defvar oom-io-server "127.0.0.1")
(defvar oom-io-port 5555)

(defun oom-io-send-command (cmd)
  (unless (client-send oom-io-process cmd)
    (client-start oom-io-server oom-io-port oom-io-process oom-io-buffer)
    (client-send oom-io-process cmd)))

(defun oom-io-shutdown ()
  (client-stop oom-io-process))

(defun oom-io-reload () (oom-io-send-command "reload"))

(provide 'oom-io)
;;; oom-io.el ends here

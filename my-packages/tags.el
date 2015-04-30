;;; page-breaks.el --- tagging / bookmarking code util
;;; Commentary:
;;; Code:

;; highlight "page breaks" / TODOs
(defconst re-page-break ".?//-.")
(defconst re-todo ".?TODO\\:.")
(defconst re-page-break-or-todo "\\(.?//-.\\)\\|\\(.?TODO\\:.\\)")

(defun highlight-page-breaks ()
  (interactive)
  (highlight-lines-matching-regexp re-page-break 'font-lock-page-break-face))

(defun highlight-todos ()
  (interactive)
  (highlight-lines-matching-regexp re-todo 'font-lock-todo-face))

(defvar page-break-wrap-search nil)
(defun page-break-navigate (dir)
  (if (eq dir 1)
      (forward-line)
    (forward-line -1))
  (unless (re-search-forward re-page-break-or-todo nil t dir)
    (message "label not found!")
    (if page-break-wrap-search
	(progn
	  (setq page-break-wrap-search nil)
	  (if (eq dir 1)
	      (goto-char (point-min))
	    (goto-char (point-max)))
	  (page-break-navigate dir))
      (progn
	(setq page-break-wrap-search t)
	(message "Search reached end of document, press button to wrap search")))))

(defun next-page-break ()
  (interactive)
  (page-break-navigate 1))

(defun prev-page-break ()
  (interactive)
  (page-break-navigate -1))

;; list page-breaks in popup.el
(defconst re-page-break-popup ".?//- ?\\(.*\\)")
(defun get-buffer-tags ()
  (let ((ret nil)
	(num 1))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re-page-break-popup nil t 1)
	(add-to-list 'ret (cons (concat " " (number-to-string num) ") " (match-string-no-properties 1))
				(match-beginning 1)))
	(setq num (+ num 1))))
    (nreverse ret)))

(defun get-buffer-tags-cursor (tags-list)
  ;; search tags list untill point position is smaller than element
  (let ((i 0)
	(el tags-list))
    (while (and (< (+ i 1) (length tags-list))
		(< (cdr (car el)) (point)))
      (setq i (+ 1 i))
      (setq el (cdr el)))
    i))

(defun popupize-item (element)
  (popup-make-item (car element) :value (cdr element)))

(defun page-breaks-popup ()
  (interactive)
  (let ((buffer-tags (get-buffer-tags)))
    (if buffer-tags
	(goto-char (popup-menu* (mapcar 'popupize-item buffer-tags)
				:cursor (get-buffer-tags-cursor buffer-tags)
				:scroll-bar t
				:isearch t))
      (message "no page breaks found"))))

(provide 'tags)
;;; taga.el ends here

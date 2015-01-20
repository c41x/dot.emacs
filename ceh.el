;;; ceh-demo --- CEH - C Edit Helper
;;; Commentary:
;;; Code:

;; detail
(defconst ceh--operators "- */\+|&^%!,<>=\n\t")

(defun ceh--in-array (element array)
  (let ((i 0))
    (while (and
	    (< i (length array))
	    (not (= element (elt array i))))
      (setq i (+ i 1)))
    (not (= i (length array)))))

(defun ceh--f-search (re)
  (re-search-forward re nil t))
(defun ceh--b-search (re)
  (re-search-backward re nil t))

(defun ceh--f-peek (search)
  (string= search
	   (buffer-substring-no-properties (point) (+ (point) (length search)))))
(defun ceh--b-peek (search)
  (string= search
	   (buffer-substring-no-properties (- (point) (length search)) (point))))

(defun ceh--f-sexp ()
  (ignore-errors (forward-sexp) t))
(defun ceh--b-sexp ()
  (ignore-errors (backward-sexp) t))

(defun ceh--f-atom ()
  (if (ceh--f-sexp)
      (progn
	(while (or (ceh--f-peek "(")
		   (ceh--f-peek ".")
		   (ceh--f-peek "->")
		   (ceh--f-peek "::"))
	  (ceh--f-sexp))
	t)
    nil))

(defun ceh--b-atom ()
  (if (ceh--b-sexp)
      (progn
	(while (or (ceh--f-peek "(")
		   (ceh--b-peek ".")
		   (ceh--b-peek "->")
		   (ceh--b-peek "::"))
	  (ceh--b-sexp))
	t)
    nil))

;; interactives
(defun ceh-parametrize ()
  (interactive)
  (if (ceh--in-array (char-before) ceh--operators) ;; lr slurp
      (progn (ceh--b-atom)
	     (insert "(")
	     (ceh--f-atom)
	     (ceh--f-atom)
	     (insert ")"))
    (let* ((begin-point (point))
	   (valid (ceh--f-atom))
	   (end-point (point)))
      (if valid
	  (progn
	    (goto-char begin-point)
	    (cond ((ceh--b-peek ")") ;; continue parametrization
		   (delete-char -1)
		   (setq end-point (- end-point 1)))
		  (t ;; init parametrization
		   (insert "(")
		   (setq end-point (+ end-point 1))))
	    (goto-char end-point)
	    (insert ")"))
	(goto-char begin-point)))))

(defun ceh-unparametrize ()
  (interactive)
  (when (ceh--b-peek ")")
    (let* ((begin-point (point))
	   (valid (progn (backward-char)
			 (ceh--b-atom)))
	   (end-point (progn
			(ceh--b-atom)
			(ceh--f-atom)
			(point))))
      (when valid
	(goto-char begin-point)
	(delete-char -1)
	(goto-char end-point)
	(insert ")")))))

(defun ceh-stringize-line ()
  (interactive)
  (let ((lim (progn (end-of-line)
		    (point))))
    (beginning-of-line-text)
    (while (search-forward "\"" lim t) (replace-match "\\\\\"")) ;; escape strings
    (beginning-of-line-text)
    (insert "\"")
    (end-of-line)
    (insert "\"")))

(defun ceh-finish-expression ()
  (interactive)
  (end-of-line)
  (if (not (ceh--in-array (char-before) ";:}{+-|&<\\//.,!*="))
      (progn
	(insert ";")
	(indent-for-tab-command))
    (newline))
  (indent-for-tab-command))

(defun ceh-new-brace ()
  (interactive)
  (end-of-line)
  (insert " {")
  (newline)
  (newline)
  (insert "}")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command))

;; TODO: add string skipping
;; TODO: intelligent killing
;; TODO: killing brackets

;; helpers
(defun ceh--search-forward-skip-nested (opening-char closing-char &optional start-nest-level)
  (let ((nest-level (if start-nest-level start-nest-level 0)))
    (while
	(progn
	  (if (re-search-forward (format "[%c%c]" opening-char closing-char) nil t 1)
	      (cond ((eq (char-before) opening-char)
		     (setq nest-level (+ nest-level 1))) ;; down
		    ((eq (char-before) closing-char)
		     (setq nest-level (- nest-level 1)))) ;; up
	    (setq nest-level 0))
	  (> nest-level 0)))))

(defun ceh--search-for-forward-skip-nested (opening-char closing-char chars &optional start-nest-level)
  (let ((nest-level (if start-nest-level start-nest-level 0)))
    (while
	(progn
	  (if (re-search-forward (format "[%c%c%s]" opening-char closing-char chars) nil t 1)
	      (cond ((eq (char-before) opening-char)
		     (setq nest-level (+ nest-level 1)))
		    ((eq (char-before) closing-char)
		     (setq nest-level (- nest-level 1)))
		    ((and
		      (ceh--in-array (char-before) chars)
		      (<= nest-level 0))
		     (setq nest-level -1)))
	    (setq nest-level 0))
	  (>= nest-level 0)))))

(defun ceh--search-backward-skip-nested (opening-char closing-char &optional start-nest-level)
  (let ((nest-level (if start-nest-level start-nest-level 0)))
    (while
	(progn
	  (if (re-search-backward (format "[%c%c]" opening-char closing-char) nil t 1)
	      (cond ((eq (char-after) closing-char)
		     (setq nest-level (+ nest-level 1)))
		    ((eq (char-after) opening-char)
		     (setq nest-level (- nest-level 1))))
		(setq nest-level 0))
	  (> nest-level 0)))))

(defun ceh--search-for-backward-skip-nested (opening-char closing-char chars &optional start-nest-level)
  (let ((nest-level (if start-nest-level start-nest-level 0)))
    (while
	(progn
	  (if (re-search-backward (format "[%c%c%s]" opening-char closing-char chars) nil t 1)
	      (progn
		(cond ((eq (char-after) closing-char)
		       (setq nest-level (+ nest-level 1)))
		      ((eq (char-after) opening-char)
		       (setq nest-level (- nest-level 1)))
		      ((and
			(ceh--in-array (char-after) chars)
			(<= nest-level 0))
		       (setq nest-level -1))))
	    (setq nest-level 0))
	  (>= nest-level 0)))))

(defconst ceh--id "A-Za-z0-9_\\-\\.\\>\\<")
(defconst ceh--whitespace " \t\n")
(defun ceh--fwd-operators ()
  (skip-chars-forward ceh--operators))
(defun ceh--bck-operators ()
  (skip-chars-backward ceh--operators))
(defun ceh--fwd-id ()
  (skip-chars-forward ceh--id))
(defun ceh--bck-id ()
  (skip-chars-backward ceh--id))

(defun ceh--fwd-expression ()
  (ceh--fwd-id)
  (cond ((eq (char-after) ?\() ;; function
	 (ceh--search-forward-skip-nested ?\( ?\))
	 (forward-char))
	(t ;; expression
	 (re-search-forward "[),; \n\t]" nil t 1))))

(defun ceh--bck-expression ()
  (cond ((eq (char-before) ?\))
	 (ceh--search-backward-skip-nested ?\( ?\))
	 (ceh--bck-id))
	(t
	 (re-search-backward "[(,; \n\t]" nil t 1)
	 (forward-char))))

(defun ceh--fwd-end-of-expr ()
  (re-search-forward "[;]" nil t 1))

(defun ceh--fwd-skip-empty-lines ()
  (skip-chars-forward ceh--whitespace))

(defun ceh--peek? (search)
  (string= search
	   (buffer-substring-no-properties (point) (+ (point) (length search)))))

(defun ceh--peekb? (search)
  (string= search
   (buffer-substring-no-properties (- (point) (length search)) (point))))

(defun ceh--fwd-skip-comment ()
  (cond ((ceh--peek? "//")
	 (end-of-line))
	((ceh--peek? "/*")
	 (re-search-forward "\\*\\/" nil t 1))))

(defun ceh--bck-skip-empty-lines ()
  (skip-chars-backward ceh--whitespace))

(defun ceh--fwd-skip-comments-and-empty-lines ()
  (while (progn
	   (let ((pt (point)))
	     (ceh--fwd-skip-comment)
	     (ceh--fwd-skip-empty-lines)
	     (not (eq pt (point)))))))

;; interactives
(defun ceh-include-expr ()
  (interactive)
  (ceh--search-forward-skip-nested ?\{ ?\} 1) ;; find closing bracket
  (let* ((whitespace-begin (point))
	 (insert-here (- (point) 1))
	 (str-begin (progn (ceh--fwd-skip-empty-lines) (point)))
	 (str-end (progn
		    (ceh--fwd-skip-comments-and-empty-lines)
	  	    (end-of-line)
		    (cond ((eq (char-before) ?\;) ;; single expression
			   (point))
			  ((eq (char-before) ?\{) ;; block of expressions
			   (backward-char)
			   (forward-sexp)
			   (point))
			  ((eq (char-before) ?\)) ;; block of expressions #2
			   (next-line)
			   (end-of-line)
			   (cond ((eq (char-before) ?\{)
				  (backward-char)
				  (forward-sexp)
				  (point))
				 (t
				  (message "could not include expr")
				  (return))))
			  (t
			   (message "could not include expr")
			   (return)))))
	 (redundant-ws-begin (progn (goto-char insert-here) (ceh--bck-skip-empty-lines) (point)))
	 (block-to-insert (buffer-substring-no-properties str-begin str-end))
	 (block-length (- str-end str-begin)))
    (delete-region whitespace-begin str-end)
    (goto-char insert-here)
    (insert block-to-insert)
    (newline)
    (indent-region insert-here (+ insert-here block-length))
    (delete-region redundant-ws-begin insert-here)
    (goto-char redundant-ws-begin)
    (newline-and-indent)))

(defun ceh-exclude-expr ()
  (interactive)
  (beginning-of-line)
  (let* ((whitespace-str-begin (progn (ceh--bck-skip-empty-lines) (point)))
	 (str-begin (progn (ceh--fwd-skip-empty-lines) (point)))
	 (str-end (progn (ceh--fwd-end-of-expr) (point)))
	 (str-to-insert (buffer-substring-no-properties str-begin str-end))
	 (insert-here (progn (ceh--search-forward-skip-nested ?\{ ?\} -1) (point))))
    (newline-and-indent)
    (insert str-to-insert)
    (delete-region whitespace-str-begin str-end)))

(defun ceh-create-block-include-expr ()
  (interactive)
  (end-of-line)
  (cond ((eq (char-before) ?\;)
	 (delete-char -1)))
  (insert " {}")
  (backward-char)
  (ceh-include-expr))

(defun ceh-step-out-of-args ()
  (interactive)
  (ceh--search-forward-skip-nested ?\( ?\) 1))

(defun ceh-step-in-args ()
  (interactive)
  (ceh--search-backward-skip-nested ?\( ?\))
  (if (eq (char-after) ?\()
      (forward-char)))

(defun ceh-transpose-args ()
  (interactive)
  (ceh--fwd-operators)
  (let* ((rstart (point))
	 (rend (progn (ceh--fwd-expression) (- (point) 1)))
	 (lend (progn (goto-char rstart) (ceh--bck-operators) (point)))
	 (lstart (progn (ceh--bck-expression) (point)))
	 (lstr (buffer-substring-no-properties lstart lend))
	 (rstr (buffer-substring-no-properties rstart rend)))
    (goto-char rstart)
    (delete-region rstart rend)
    (insert lstr)
    (goto-char lstart)
    (delete-region lstart lend)
    (insert rstr)))

(defun ceh-args-begin ()
  (interactive)
  (ceh--search-backward-skip-nested ?\( ?\) 1))

(defun ceh-args-end ()
  (interactive)
  (ceh--search-forward-skip-nested ?\( ?\) 1))

(defun ceh-next-argument ()
  (interactive)
  (let* ((pt (point))
	 (args-r (progn (ceh-args-end) (point))))
    (goto-char pt)
    (ceh--search-for-forward-skip-nested ?\( ?\) ",")
    (if (= (point) args-r)
	(goto-char pt)
      (ceh--fwd-skip-comments-and-empty-lines))))

(defun ceh-previous-argument ()
  (interactive)
  (let ((pt (point))
	(args-l (progn (ceh-args-begin) (point))))
    (goto-char pt)
    (ceh--search-for-backward-skip-nested ?\( ?\) ",")
    (if (not (= (point) args-l))
	(ceh--search-for-backward-skip-nested ?\( ?\) ","))
    (if (not (= (point) args-l))
	(ceh-next-argument)
      (forward-char))))

;; killing
(defun ceh-leave-me ()
  (interactive)
  (let* ((expr-begin (progn (ceh--search-for-backward-skip-nested ?\( ?\) ",")
			    (forward-char)
			    (ceh--fwd-skip-empty-lines)
			    (point)))
	 (expr-end (progn (ceh--search-for-forward-skip-nested ?\( ?\) ",")
			  (backward-char)
			  (ceh--bck-skip-empty-lines)
			  (point)))
	 (del-begin (progn (ceh-args-begin) (ceh--bck-expression) (point)))
	 (del-end (progn (ceh--fwd-expression) (backward-char) (point)))
	 (expr (buffer-substring-no-properties expr-begin expr-end)))
    (delete-region del-begin del-end)
    (insert expr)))

;; key chords
(defun ceh--chord-kill-line ()
  (interactive)
  (kill-whole-line)
  (previous-line)
  (end-of-line))

(defun ceh--chord-skip-chars ()
  (interactive)
  (if (ceh--in-array (char-after) "),\"]")
      (forward-char)))

;; expand macro utility
(defun ceh--expand-fallback ()
  (yas-expand))

(defun ceh--not-end-of-line-p ()
  (not (= (point) (line-end-position))))

(defun ceh--end-of-line-p ()
  (= (point) (line-end-position)))

(defun ceh-expand ()
  (interactive)
  (let ((c1 (buffer-substring-no-properties (- (point) 1) (point)))
	(c2 (buffer-substring-no-properties (- (point) 2) (- (point) 1))))
    (cond ((ceh--peekb? " <= ") ;; recursives first
	   (delete-char -4)
	   (insert "<=")
	   (ceh-expand))
	  ((ceh--peekb? " >= ")
	   (delete-char -4)
	   (insert ">=")
	   (ceh-expand))
	  ((ceh--peekb? "->")
	   (delete-char -1)
	   (ceh-expand))
	  ;; construct
	  ((string= c1 "-")
	   (if (string= c2 "-")
	       (progn
		 (delete-char -2)
		 (ceh-step-out-of-args)
		 (insert " - "))
	     (delete-char -1)
	     (if (ceh--not-end-of-line-p)
		 (ceh-step-out-of-args))
	     (insert "->")))
	  ((string= c1 ";")
	   (if (ceh--end-of-line-p)
	       (ceh--expand-fallback)
	     (delete-char -1)
	     (ceh-step-out-of-args)
	     (if (not (ceh--peek? ";"))
		 (insert "; "))))
	  ((string= c1 "=")
	   (if (ceh--end-of-line-p)
	       (ceh--expand-fallback)
	     (cond ((string= c2 "<")
		    (delete-char -2)
		    (ceh-step-out-of-args)
		    (insert " <= "))
		   ((string= c2 ">")
		    (delete-char -2)
		    (ceh-step-out-of-args)
		    (insert " >= "))
		   ((string= c2 "=")
		    (delete-char -2)
		    (ceh-step-out-of-args)
		    (insert " == "))
		   (t
		    (delete-char -1)
		    (end-of-line)
		    (insert " = ")))))
	  ((string= c1 ".")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert "."))
	  ((string= c1 ",")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert ", "))
	  ((string= c1 "+")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert " + "))
	  ((string= c1 "&")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert " && "))
	  ((string= c1 "|")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert " || "))
	  ((string= c1 ">")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert " > "))
	  ((string= c1 "<")
	   (delete-char -1)
	   (ceh-step-out-of-args)
	   (insert " < "))
	  ;; fallback
	  (t (ceh--expand-fallback)))))

;; TODO: .h -> .cpp helper
(defun ceh-decl-to-impl-namespace (namespace)
  (interactive)
  (beginning-of-line)
  (end-of-sexp)
  (forward-char)
  (insert namespace)
  (if (not (string= namespace ""))
      (insert "::"))
  (end-of-line)
  (if (ceh--peekb? ";")
      (progn (delete-char -1)
	     (insert " {")
	     (newline 2)
	     (insert "}")
	     (indent-for-tab-command)
	     (newline)
	     (previous-line 2)
	     (indent-for-tab-command))))

(defun ceh-decl-to-impl ()
  (interactive)
  (end-of-line)
  (if (ceh--peekb? ";")
      (progn (delete-char -1)
	     (insert " {")
	     (newline 2)
	     (insert "}")
	     (indent-for-tab-command)
	     (newline)
	     (previous-line 2)
	     (indent-for-tab-command))))

(defun ceh-decl-to-impl-n (namespace)
  (interactive "sNamespace: ")
  (while (search-forward ";" nil t 1)
    (ceh-decl-to-impl-namespace namespace)))

;; specify mode
(define-minor-mode ceh-mode
  "C Edit Helper - mode for enhancing C - like languages editing"
  :lighter " CEH"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<M-return>") 'ceh-new-brace)
	    (define-key map (kbd "<S-return>") 'ceh-finish-expression)
	    (define-key map (kbd "C-(") 'ceh-parametrize)
	    (define-key map (kbd "C-)") 'ceh-unparametrize) ;; TODO: check this keybind
	    (define-key map (kbd "C-\"") 'ceh-stringize-line)
	    (define-key map (kbd "M-,") 'ceh-step-in-args) ;; tags!
	    (define-key map (kbd "M-.") 'ceh-step-out-of-args) ;; tags!
	    (define-key map (kbd "C-' s") 'ceh-transpose-args)
	    (define-key map (kbd "C-' d") 'ceh-leave-me)
	    (define-key map (kbd "TAB") 'ceh-expand)
	    (define-key map (kbd "<tab>") 'ceh-expand)
	    (define-key map (kbd "C-.") 'ceh-next-argument)
	    (define-key map (kbd "C-,") 'ceh-previous-argument)
	    map)
  ;; chords
  (when (require 'key-chord nil 'noerror)
    (key-chord-define-global "qq" 'ceh--chord-kill-line)
    ;;(key-chord-define-global ",," 'ceh--chord-skip-chars)
    (key-chord-define-global "[[" 'ceh-include-expr)
    (key-chord-define-global "]]" 'ceh-exclude-expr)
    (key-chord-define-global "[]" 'ceh-create-block-include-expr)
    (key-chord-mode +1)))

(provide 'ceh)
;;; ceh.el ends here

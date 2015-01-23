;;; ceh-demo --- CEH - C Edit Helper
;;; Commentary:
;;; Code:

;; TODO: intelligent killing
;; TODO: killing brackets

;;//- general functiions / detail -
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

(defun ceh--inside-string ()
  (if (nth 3 (syntax-ppss))
      t nil))

(defun ceh--f-step-out-of-string ()
  (ceh--f-search "[^\\]\""))
(defun ceh--b-step-out-of-string ()
  (ceh--b-search "[^\\]\"")
  (forward-char))

(defun ceh--f-peek (search)
  (string= search
	   (buffer-substring-no-properties (point) (+ (point) (length search)))))
(defun ceh--b-peek (search)
  (string= search
	   (buffer-substring-no-properties (- (point) (length search)) (point))))

(defun ceh--whitespace-insert-re (rex)
  (replace-regexp-in-string " " "[\t\n ]*" rex))

(defun ceh--f-peekr (rex)
  (looking-at-p (ceh--whitespace-insert-re rex)))
(defun ceh--b-peekr (rex)
  (looking-back (ceh--whitespace-insert-re rex)))

(defun ceh--f-peekrs (rex)
  (if (ceh--f-peekr rex)
      (progn (ceh--f-search (ceh--whitespace-insert-re rex)) t)
    nil))
(defun ceh--b-peekrs (rex)
  (if (ceh--b-peekr rex)
      (progn (ceh--b-search (ceh--whitespace-insert-re rex)) t)
    nil))

(defun ceh--f-sexp ()
  (ignore-errors (forward-sexp) t))
(defun ceh--b-sexp ()
  (ignore-errors (backward-sexp) t))

;; TODO: templates?
;; TODO: array indexing []
(defun ceh--f-atom ()
  (cond ((ceh--inside-string)
	 (ceh--f-step-out-of-string))
	((ceh--f-sexp)
	 (progn
	   (while (or (ceh--f-peekr " ( ")
		      (ceh--f-peekr " \\. ")
		      (ceh--f-peekr " -> ")
		      (ceh--f-peekr " :: "))
	     (ceh--f-sexp))
	   (ceh--f-peekrs "\\+\\+\\|\\-\\-") ;; ++ and -- are part of atom
	   t))
	(t nil)))

(defun ceh--b-atom ()
  (cond ((ceh--inside-string)
	 (ceh--b-step-out-of-string))
	((ceh--b-sexp)
	 (progn
	   (while (or (ceh--f-peekr " ( ")
		      (ceh--b-peekr " \\. ")
		      (ceh--b-peekr " -> ")
		      (ceh--b-peekr " :: "))
	     (ceh--b-sexp))
	   (ceh--b-peekrs "\\+\\+\\|\\-\\-") ;; skip ++ and --
	   t))
	(t nil)))

(defun ceh--f-args ()
  "search for arguments end, returns nil if not in arguments"
  (while (ceh--f-atom))
  (ceh--f-peek ")"))

(defun ceh--b-args ()
  "search for arguments beginning, returns nil if not in arguments"
  (while (ceh--b-atom))
  (ceh--b-peek "("))

(defun ceh--inside-args ()
  (save-excursion (ceh--f-args)))

(defun ceh--step-out-of-args ()
  (when (save-excursion (ceh--f-args))
    (ceh--f-args)
    (forward-char)))

(defun ceh--b-step-out-of-args ()
  (when (save-excursion (ceh--b-args))
    (ceh--b-args)
    (forward-char -1)))

(defun ceh--f-search-ignoring-args-string (rex)
  (while (progn (ceh--f-search rex)
		(or (ceh--inside-string)
		    (ceh--inside-args)))))

(defun ceh--b-search-ignoring-args-string (rex)
  (while (progn (ceh--b-search rex)
		(or (ceh--inside-string)
		    (ceh--inside-args)))))

(defun ceh--flat-expression ()
  (save-excursion
    (ceh--b-step-out-of-args)
    (while (and
	    (ceh--b-sexp)
	    (save-excursion
	      (ceh--b-sexp)
	      (ceh--f-sexp)
	      (not (and (ceh--f-peekr " ( ")
			(or (ceh--b-peekr " if ")
			    (ceh--b-peekr " for ")
			    (ceh--b-peekr " while ")))))))
    (and (ceh--f-peek "(")
	 (or (ceh--b-peekr " if ")
	     (ceh--b-peekr " for ")
	     (ceh--b-peekr " while ")))))

(defun ceh--f-block ()
  (ceh--step-out-of-args)
  (when (save-excursion (ceh--f-sexp)) ;; last block
    (ceh--f-search-ignoring-args-string "{\\|;")
    (when (ceh--b-peek "{")
      (forward-char -1)
      (ceh--f-sexp))
    (when (ceh--f-peekr " else ") ;; else (if) -> continue
      (ceh--f-block))))

(defun ceh--b-block ()
  (ceh--step-out-of-args)
  (when (ceh--b-sexp)
    (ceh--b-search-ignoring-args-string "{\\|}\\|;")
    (ceh--f-peekrs " } ")
    (ceh--f-peekrs " { ")
    (ceh--f-sexp)
    (ceh--b-sexp)
    (when (ceh--f-peekr " else ")
      (ceh--b-block))))

(defun ceh--not-eol ()
  (not (= (point) (line-end-position))))

(defun ceh--eol ()
  (= (point) (line-end-position)))

;;//- user space API (interactives) -
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

(defun ceh-transpose-atoms ()
  (interactive)
  (ceh--b-atom)
  (let* ((lstart (point))
	 (lend (progn (ceh--f-atom) (point)))
	 (rend (progn (ceh--f-atom) (point)))
	 (rstart (progn (ceh--b-atom) (point)))
	 (lstr (buffer-substring-no-properties lstart lend))
	 (rstr (buffer-substring-no-properties rstart rend)))
    (goto-char rstart)
    (delete-region rstart rend)
    (insert lstr)
    (goto-char lstart)
    (delete-region lstart lend)
    (insert rstr)))

(defun ceh-next-argument ()
  (interactive)
  (when (save-excursion (ceh--f-args))
    (while (and
	    (ceh--f-atom)
	    (not (or (ceh--f-peekrs " , ")
		     (ceh--f-peekrs " ; ")
		     (ceh--f-peekrs " : ")))))))

(defun ceh-previous-argument ()
  (interactive)
  (when (save-excursion (ceh--b-args))
    (while (and
	    (ceh--b-atom)
	    (not (or (ceh--b-peekr " , ")
		     (ceh--b-peekr " ; ")
		     (ceh--b-peekr " : ")))))))

(defun ceh-leave-atom ()
  (interactive)
  (when (save-excursion (ceh--b-args))
    (let* ((atom-begin (progn (ceh--b-atom) (point)))
	   (atom-end (progn (ceh--f-atom) (point)))
	   (atom-str (buffer-substring-no-properties atom-begin atom-end))
	   (expr-begin (progn
			 (ceh--b-args)
			 (forward-char -1)
			 (ceh--b-atom)
			 (point)))
	   (expr-end (progn (ceh--f-atom) (point))))
      (delete-region expr-begin expr-end)
      (insert atom-str))))

(defun ceh-step-out-of-args ()
  (interactive)
  (ceh--step-out-of-args))

(defun ceh-step-in-args ()
  (interactive)
  (when (ceh--b-peekr " ) ")
    (forward-char -1)
    (ceh--b-args)))

(defun ceh-kill-line ()
  (interactive)
  (kill-whole-line)
  (forward-line -1)
  (end-of-line))

(defun ceh-include-block ()
  (interactive)
  (while (progn (ceh--f-block) ;; move to {} block end
		(not (ceh--f-peekr " }"))))
  (skip-chars-backward " \t\n")
  (let* ((pt-insert (point))
	 (pt-begin (progn
		     (ceh--f-peekrs " }")
		     (point)))
	 (pt-end (progn
		   (ceh--f-block)
		   (point)))
	 (pt-str (buffer-substring-no-properties pt-begin pt-end)))
    (goto-char pt-insert)
    (delete-region pt-begin pt-end)
    (insert pt-str)
    (indent-region pt-insert (point))))

(defun ceh-exclude-block ()
  (interactive)
  (if (and (ceh--f-peekr " } ")
	   (ceh--b-peekr " { "))
      (progn
	(let* ((pt-begin (save-excursion
			   (skip-chars-backward "{ \t\n")
			   (point)))
	       (pt-end (save-excursion
			 (skip-chars-forward "} \t\n")
			 (point))))
	  (delete-region pt-begin pt-end)
	  (insert ";")
	  (newline-and-indent)
	  (forward-line -1)
	  (end-of-line)))
    (let* ((pt-begin (progn
		       (ceh--b-block)
		       (if (ceh--b-sexp)
			   (ceh--f-sexp)
			 (skip-chars-backward " \t\n"))
		       (ceh--f-peekrs ";")
		       (point)))
	   (pt-end (progn
		     (ceh--f-block)
		     (point)))
	   (pt-str (buffer-substring-no-properties pt-begin pt-end)))
      (delete-region pt-begin pt-end)
      (while (ceh--f-sexp))
      (ceh--f-search "}")
      (insert pt-str)
      (indent-region pt-begin (point))
      (goto-char pt-begin))))

(defun ceh-create-block ()
  (interactive)
  (if (and (save-excursion ;; when expr is in the same line
	     (beginning-of-line-text)
	     (or (ceh--f-peekr " if ")
		 (ceh--f-peekr " else ")
		 (ceh--f-peekr " while ")
		 (ceh--f-peekr " for ")))
	   (save-excursion
	     (end-of-line)
	     (ceh--b-peekr ";")))
      (progn ;; put point after (if|else|while|...)
	(beginning-of-line-text)
	(ceh--f-sexp)
	(newline)
	(forward-line -1)
	(end-of-line))
    (end-of-line)
    (when (ceh--b-peek ";")
      (delete-char -1))) ;; EOL, delete semicolon
  (let* ((pt-begin (point))
	 (pt-end (progn
		   (ceh--f-search-ignoring-args-string ";")
		   (point))))
    (goto-char pt-begin)
    (insert " {")
    (goto-char (+ 2 pt-end))
    (newline)
    (insert "}")
    (indent-region pt-begin (point))
    (forward-char -1)
    (skip-chars-backward " \n\t")))

;;//- expand macro utility -
(defun ceh--expand-fallback ()
  (yas-expand))

(defun ceh--f-expand ()
  (interactive)
  (cond ((not (ceh--f-sexp))
	 (ceh--f-peekrs " )")
	 (ceh--f-peekrs " ]"))
	((ceh--inside-string)
	 (ceh--f-step-out-of-string))
	(t
	 (ceh--f-peekrs " \""))))

(defun ceh--transform (a b)
  (if (ceh--b-peek a)
    (progn (delete-char (length a))
	   (insert b)
	   t)
    nil))

(defun ceh-expand ()
  (interactive)
  (let ((c1 (buffer-substring-no-properties (- (point) 1) (point)))
	(c2 (buffer-substring-no-properties (- (point) 2) (- (point) 1))))
    ;; recursives first
    (cond ((ceh--b-peek " <= ")
	   (delete-char -4)
	   (insert "<=")
	   (ceh-expand))
	  ((ceh--b-peek " >= ")
	   (delete-char -4)
	   (insert ">=")
	   (ceh-expand))
	  ((ceh--b-peek "->")
	   (delete-char -1)
	   (ceh-expand))

	  ;; construct
	  ((string= c1 "-")
	   (if (string= c2 "-")
	       (progn
		 (delete-char -2)
		 (ceh--f-expand)
		 (insert " - "))
	     (delete-char -1)
	     (if (ceh--not-eol)
		 (ceh--f-expand))
	     (insert "->")))
	  ((string= c1 ";")
	   (if (ceh--eol)
	       (ceh--expand-fallback)
	     (delete-char -1)
	     (ceh--f-expand)
	     (if (not (ceh--f-peek ";"))
		 (insert "; "))))
	  ((string= c1 "=")
	   (if (ceh--eol)
	       (ceh--expand-fallback)
	     (cond ((string= c2 "<")
		    (delete-char -2)
		    (ceh--f-expand)
		    (insert " <= "))
		   ((string= c2 ">")
		    (delete-char -2)
		    (ceh--f-expand)
		    (insert " >= "))
		   ((string= c2 "=")
		    (delete-char -2)
		    (ceh--f-expand)
		    (insert " == "))
		   (t
		    (delete-char -1)
		    (end-of-line)
		    (insert " = ")))))
	  ((string= c1 ".")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert "."))
	  ((string= c1 ",")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert ", "))
	  ((string= c1 "+")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert " + "))
	  ((string= c1 "&")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert " && "))
	  ((string= c1 "|")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert " || "))
	  ((string= c1 ">")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert " > "))
	  ((string= c1 "<")
	   (delete-char -1)
	   (ceh--f-expand)
	   (insert " < "))

	  ;; fallback
	  (t (ceh--expand-fallback)))))


;; TODO: .h -> .cpp helper
;(defun ceh-decl-to-impl-namespace (namespace)
;  (interactive)
;  (beginning-of-line)
;  (end-of-sexp)
;  (forward-char)
;  (insert namespace)
;  (if (not (string= namespace ""))
;      (insert "::"))
;  (end-of-line)
;  (if (ceh--peekb? ";")
;      (progn (delete-char -1)
; 	     (insert " {")
; 	     (newline 2)
; 	     (insert "}")
; 	     (indent-for-tab-command)
; 	     (newline)
; 	     (previous-line 2)
; 	     (indent-for-tab-command))))
;
;(defun ceh-decl-to-impl ()
;  (interactive)
;  (end-of-line)
;  (if (ceh--peekb? ";")
;      (progn (delete-char -1)
; 	     (insert " {")
; 	     (newline 2)
; 	     (insert "}")
; 	     (indent-for-tab-command)
; 	     (newline)
; 	     (previous-line 2)
; 	     (indent-for-tab-command))))
;
;(defun ceh-decl-to-impl-n (namespace)
;  (interactive "sNamespace: ")
;  (while (search-forward ";" nil t 1)
;    (ceh-decl-to-impl-namespace namespace)))

;;//- mode definition -
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
	    (define-key map (kbd "M-,") 'ceh-step-in-args)
	    (define-key map (kbd "M-.") 'ceh-step-out-of-args)
	    (define-key map (kbd "C-' s") 'ceh-transpose-atoms)
	    (define-key map (kbd "C-' d") 'ceh-leave-atom)
	    (define-key map (kbd "TAB") 'ceh-expand)
	    (define-key map (kbd "<tab>") 'ceh-expand)
	    (define-key map (kbd "C-.") 'ceh-next-argument)
	    (define-key map (kbd "C-,") 'ceh-previous-argument)
	    map)
  ;; chords
  (when (require 'key-chord nil 'noerror)
    (key-chord-define-global "qq" 'ceh-kill-line)
    ;;(key-chord-define-global ",," 'ceh--chord-skip-chars)
    (key-chord-define-global "[[" 'ceh-include-block)
    (key-chord-define-global "]]" 'ceh-exclude-block)
    (key-chord-define-global "[]" 'ceh-create-block)
    (key-chord-mode +1)))

(provide 'ceh)
;;; ceh.el ends here

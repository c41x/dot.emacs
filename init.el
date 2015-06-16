;;//- package management
(require 'cl-lib)
(require 'cl)
(require 'package)

(package-initialize)
(setq package-archives
      '(("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("elpa" . "http://elpa.gnu.org/packages/")))

(defvar required-packages
  '(yasnippet
    auto-complete
    expand-region
    multiple-cursors
    ace-jump-mode
    glsl-mode
    cmake-mode
    pretty-lambdada
    autopair
    csharp-mode
    js2-mode
    lua-mode
    helm
    smex
    popup
    highlight-symbol
    jedi
    flycheck
    key-chord
    skewer-mode
    fuzzy
    geiser
    ac-geiser
    php-mode
    s))

(defun has-package-to-install ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return t)
	finally (return nil)))

(when (has-package-to-install)
  (message "get latest versions of packages...")
  (package-refresh-contents)
  (message "done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;//- mode-line
(defconst buffer-pos-indicator-length 25)

(defun buffer-all-visible ()
  (>= (+ 1 (- (line-number-at-pos (window-end))
	      (line-number-at-pos (window-start))))
      (total-lines)))

(defun buffer-ind-l ()
  (floor (max 0.0 (- (* (/ (float (line-number-at-pos (window-start)))
			   (float (total-lines)))
			buffer-pos-indicator-length) 1.0))))

(defun buffer-ind-b ()
  (ceiling (+ 1.0 (* (/ (float (- (line-number-at-pos (window-end))
				  (line-number-at-pos (window-start))))
			(float (total-lines)))
		     buffer-pos-indicator-length))))

(defun buffer-ind-r ()
  (- buffer-pos-indicator-length (+ (buffer-ind-l) (buffer-ind-b))))

(setq-default mode-line-format '("%e"
				 mode-line-front-space
				 mode-line-mule-info
				 mode-line-client
				 mode-line-modified
				 mode-line-remote
				 mode-line-frame-identification
				 (:eval (propertize ": " 'face 'mode-line-bg-face))
				 (:propertize (:eval mode-line-buffer-identification) face mode-line-separator-face)
				 (:eval (if (not (buffer-all-visible))
					    (list
					     (propertize " : " 'face 'mode-line-bg-face)
					     (propertize (make-string (buffer-ind-l) 9632) 'face 'mode-line-bg-face)
					     (propertize (make-string (buffer-ind-b) 9632) 'face 'mode-line-progress-face)
					     (propertize (make-string (buffer-ind-r) 9632) 'face 'mode-line-bg-face)
					     (propertize " : " 'face 'mode-line-bg-face))))
				 (vc-mode vc-mode)
				 " "
				 (:eval (if (boundp 'mode-line-project) (propertize (concat " " mode-line-project " ") 'face 'mode-line-2)))
				 " "
				 (:eval (propertize " : " 'face 'mode-line-bg-face))
				 mode-line-modes
				 mode-line-misc-info
				 mode-line-end-spaces
				 "%-"))

;;//- visual/UI
;; no start screen
(setq inhibit-startup-screen 1)

;; theme
(load-theme 'calx t)

;; show line numbers
(global-linum-mode t)

(unless (string= system-type "windows-nt")
  (server-start))
(tool-bar-mode -1) ;; hide toolbar (icons)
(tooltip-mode -1) ;; hide tooltips
(scroll-bar-mode 0) ;; disable system scrollbars
(menu-bar-mode -1) ;; hide menu bar

;; cursor type - horizontal bar '_'
(setq-default cursor-type 'hbar)

;; font lock decoration level: 1-3 | 2 is enough, 3 is too slow
(setq-default font-lock-maximum-decoration 2)

;; highlighting operators
;; "\\([][|!.+=&/%*,<>(){};:^~-?]+\\)"
(defvar operator-rex '(("\\([][|!.+=&/%*,<>(){};:^~?-]\\)" . font-lock-operator-face)))
(defvar operator-rex-xml '(("\\(<>/=\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'c-mode operator-rex)
(font-lock-add-keywords 'c++-mode operator-rex)
(font-lock-add-keywords 'js-mode operator-rex)
(font-lock-add-keywords 'js2-mode operator-rex)
(font-lock-add-keywords 'csharp-mode operator-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'xml-mode operator-rex-xml) ;; TODO: make this work
(font-lock-add-keywords 'python-mode operator-rex)
(setq highlight-symbol-colors (quote ("yellow green" "firebrick" "cornflower blue" "MediumPurple1")))
(setq highlight-symbol-idle-delay 0.5)
;;(setq highlight-symbol-ignore-list (quote ("\\`[0-9]+f?F?[ulUL]*\\'")))

;; highlighting numbers
;;"\\<\\(\\([+-]?[0-9.]+[lufLU]*\\)\\|0[xX][0-9a-fA-F]+\\)\\>"
(defvar number-rex '(("\\<\\(\\([0-9.]+[lufLUe]?\\)\\|0[xX][0-9a-fA-F]+\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'c-mode number-rex)
(font-lock-add-keywords 'c++-mode number-rex)
(font-lock-add-keywords 'js-mode number-rex)
(font-lock-add-keywords 'js2-mode number-rex)
(font-lock-add-keywords 'csharp-mode number-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\(-?[0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'python-mode number-rex)

;;//- general settings/utilities
;; load paths
(add-to-list 'load-path "~/.emacs.d/my-packages")

;; add hooks helper
(defun add-hooks (function hooks)
  "runs [function] for given [hooks]"
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;; F2 - toggle menu bar
(global-set-key [(f2)] 'menu-bar-mode)

;; F4 - isearch => F3 backward, F4 forawrd
(global-set-key [(f4)] 'isearch-forward)
(define-key isearch-mode-map [(f4)] 'isearch-repeat-forward)
(define-key isearch-mode-map [(f3)] 'isearch-repeat-backward)

;; highlight matching braces
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
;; (setq show-paren-style 'expression)
;; (add-hook 'after-init-hook '(lambda ()(set-face-foreground 'show-paren-match nil))) ; keeps syntax color as foreground

;; switches off word wrap
(setq-default truncate-lines 0)

;; no auto-save-backups, thanks
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; keystrokes
(global-unset-key (kbd "<C-z>")) ; disable Ctrl+z hide
(global-set-key (kbd "C-c C-<up>") (lambda () (interactive) (enlarge-window 2)))
(global-set-key (kbd "C-c C-<down>") (lambda () (interactive) (enlarge-window -2)))
(global-set-key (kbd "C-c C-<right>") (lambda () (interactive) (enlarge-window-horizontally 2)))
(global-set-key (kbd "C-c C-<left>") (lambda () (interactive) (enlarge-window-horizontally -2)))

;; switching frames
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)

;; moving frames around
(defun move-frame-with-buffer (dir)
  (let* ((c-buff (current-buffer))
	 (c-win (selected-window))
	 (o-win (windmove-find-other-window dir)))
    (when o-win
      (delete-window c-win)
      (select-window o-win)
      (unless (windmove-find-other-window 'up)
	(split-window-vertically)
	(windmove-down))
      (set-window-buffer (selected-window) c-buff))))

(global-set-key (kbd "C-; .") (lambda () (interactive) (move-frame-with-buffer 'right)))
(global-set-key (kbd "C-; ,") (lambda () (interactive) (move-frame-with-buffer 'left)))

;; comment/uncomment
(global-set-key (kbd "C-; c") 'comment-or-uncomment-region)

(cua-mode 1) ; cua-mode (Ctrl+C,V,X,Z)
(setq x-select-enable-clipboard t) ; allows to copy/paste text between emacs and other apps

;; yank-pop - more CUA - friendly
(global-unset-key (kbd "M-y"))
(global-set-key (kbd "C-M-v") 'yank-pop)

;; C-x f opens file (by default this (annoying) shortcut calls set-fill-column)
(global-set-key (kbd "C-x f") 'ido-find-file)

;; Ctrl + Backspace kill ring
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(defun forward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "<C-delete>") 'forward-delete-word)

;; open config file
(defun cfg ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; fix to conflict between cua rectangle mode and autopair (autopair overrides enter key (cua-rotate-rectangle))
;; just bind cua-rotate-rectangle to other keybind
(global-set-key (kbd "C-M-r") 'cua-rotate-rectangle)

;; file name in title bar
(setq frame-title-format "emacs | %b")
(setq icon-title-format frame-title-format)

;; macro start/end bound to F11/F12
(global-set-key [(f11)] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(f12)] 'kmacro-end-or-call-macro)

;; no shitty spaces
(setq indent-tabs-mode t)

;; auto indenting current line when pressing <enter>
(electric-indent-mode t)

;; shows current function name in modeline
;; (which-function-mode)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; automatically reload files when changed
(global-auto-revert-mode t)

;; org mode annoying shortcuts
(defun nuke-org-keybinds (binds)
  (dolist (bind binds)
    (define-key org-mode-map (kbd bind) nil)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (nuke-org-keybinds '("M-<right>"
				 "M-<left>"
				 "S-<left>"
				 "S-<right>"
				 "M-<up>"
				 "M-<down>"
				 "S-<down>"
				 "S-<up>"
				 "C-S-<down>"
				 "C-S-<up>"))))

(setq query-replace-show-replacement t)

;; returns total lines count in current buffer
(defun total-lines ()
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

;;//- plugins
;; automatic brackets {}()[]""'' pairing
(require 'autopair)
(autopair-global-mode)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
(setq-default yas/trigger-key (kbd "C-S-M-;")) ;; impossibru shortcut
(yas-global-mode t)
(define-key yas-minor-mode-map (kbd "<tab>") nil) ;; disable tab (trigger YAS only with AC)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(setq yas-fallback-behavior '(apply indent-for-tab-command)) ;; indent as fallback

;; ido mode / smex for M-x
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete-dict")
(ac-config-default)
(setq-default auto-complete-mode t)
(global-auto-complete-mode t)
(delq 'ac-source-yasnippet ac-sources)
(setq ac-ignore-case t)
(setq ac-use-fuzzy t)
(setq popup-isearch-cursor-color "orange")
(ac-flyspell-workaround) ; lag hack
(flyspell-mode 0) ;; speedup AC

;; helm
(require 'helm-config)
(set 'helm-idle-delay 0.0)
(set 'helm-input-idle-delay 0.0)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

;; ace-jump mode for quick jumping around
(require 'ace-jump-mode)
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand region
(require 'expand-region)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'er/expand-region)

;; pretty lambda
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; neotree
(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; highlight-symbol mode
(require 'highlight-symbol)
(add-hooks 'highlight-symbol-mode
	   '(emacs-lisp-mode-hook
	     c-mode-hook
	     c++-mode-hook
	     csharp-mode-hook
	     js-mode-hook
	     js2-mode-hook
	     python-mode-hook))
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; flycheck
(require 'flycheck)

;;//- general tweaks
;; documentation tip
(defun popup-doc ()
  (interactive)
  (let* ((position (point))
	 (string-under-cursor (buffer-substring-no-properties
			       (progn (skip-syntax-backward "w_") (point))
			       (progn (skip-syntax-forward "w_") (point)))))
    (if (ac-symbol-documentation (intern string-under-cursor))
	(progn
	  (goto-char position)
	  (popup-tip (ac-symbol-documentation (intern string-under-cursor))))
      (message (format "symbol documentation not found: \"%s\"" (intern string-under-cursor))))))

(global-set-key (kbd "C-?") 'popup-doc)

;; control + space = autocomplete (and enable AC mode if not enabled)
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC")
		'(lambda ()
		   (interactive)
		   (unless auto-complete-mode
		     (message "enabling auto-complete-mode")
		     (auto-complete-mode t))
		   (auto-complete)))

;; simple smooth scrolling (sit-for is some kind of Sleep)
;; time is not accurate because lag may occur while scrolling... so tweak it experimenally
(defun smooth-scroll (lines number-of-iterations time)
  (let ((sit-for-time (/ (float time) (float number-of-iterations))))
    (loop for i from 1 to number-of-iterations do (progn
						    (scroll-up lines)
						    (sit-for sit-for-time)))))

(global-set-key (kbd "C-M-<up>") '(lambda () (interactive) (smooth-scroll -3 7 0.1)))
(global-set-key (kbd "C-M-<down>") '(lambda () (interactive) (smooth-scroll 3 7 0.1)))
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (smooth-scroll -1 8 0.1)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (smooth-scroll 1 8 0.1)))

;; increment closest number
(defun change-closest-number (increase-by)
  (interactive)
  (if (re-search-backward "[0-9]+" -200 t 1) ; no forward searching
      (progn
	(message "changing: %s->%d" (match-string 0) (+ increase-by (string-to-number (match-string 0))))
	(replace-match (number-to-string (+ increase-by (string-to-number (match-string 0))))))
    (error "No number found at this point")))

(global-set-key (kbd "C-c +") (lambda () (interactive) (change-closest-number 1)))
(global-set-key (kbd "C-c -") (lambda () (interactive) (change-closest-number -1)))

;; swapping windows / buffers
(defun swap-windows ()
  "Swaps buffers in selected-window with next-window"
  (interactive)
  (unless (one-window-p)
    (let* ((this (selected-window))
	   (that (next-window))
	   (this-buffer (window-buffer this))
	   (that-buffer (window-buffer that)))
      (set-window-buffer this that-buffer)
      (set-window-buffer that this-buffer)
      (other-window 1))))

(defun move-window-away ()
  "Moves buffer to \"other\""
  (interactive)
  (unless (one-window-p)
    (let* ((that (next-window))
	   (this (selected-window))
	   (this-buffer (window-buffer this))
	   (prv-buffer (other-buffer)))
      (set-window-buffer this prv-buffer)
      (set-window-buffer that this-buffer)
      (other-window 1))))

(global-set-key (kbd "C-; s") 'swap-windows)
(global-set-key (kbd "C-; a") 'move-window-away)

;; kill-close
(defun kill-close-window ()
  "Kills buffer and then closes window"
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "C-x C-k") 'kill-close-window)

;; highlight 80+ lines
(defun highlight-80+ ()
  "Highlights lines that have 80+ characters"
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'font-lock-over-80-face))

;; highlight-symbol advice for ignoring numbers and symbols inside comments
(defadvice highlight-symbol-get-symbol (after highlight-ignore-symbols activate)
  (when (or (save-excursion
	      (skip-chars-backward "0-9.A-Za-z")
	      (looking-at "[0-9]+\\.?[ulULfF]*"))
	    (nth 4 (syntax-ppss)))
    (setq ad-return-value nil)))

;; recompiling stuff
(defun recompile-scripts ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

;; recent buffer toggling
(defvar toggle-buffer-i -1)
(defvar toggle-buffer-list '())

(defun toggle-buffer-n (next)
  (when (< toggle-buffer-i 0)
    (setq toggle-buffer-list (buffer-list))
    (setq toggle-buffer-i 0))
  (while (progn
	   (if next
	       (setq toggle-buffer-i (min (+ 1 toggle-buffer-i) (length toggle-buffer-list)))
	     (setq toggle-buffer-i (max 0 (- toggle-buffer-i 1))))
	   (message (buffer-name (nth toggle-buffer-i toggle-buffer-list)))
	   (= (aref (buffer-name (nth toggle-buffer-i toggle-buffer-list)) 0)
	      ?\s))) ;; buffers with space as first char are special buffers
  (switch-to-buffer (nth toggle-buffer-i toggle-buffer-list)))

(defun toggle-buffer-next ()
  (interactive)
  (toggle-buffer-n t))

(defun toggle-buffer-previous ()
  (interactive)
  (toggle-buffer-n nil))

(defun toggle-buffer-finish ()
  (interactive)
  (setq toggle-buffer-i -1))

;;//- C++
;; tell emacs to open .h files in C++ mode (c-mode by default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C/C++ autocomplete hooks
(defun ac-ccc-mode-setup ()
  (setq ac-sources '(;ac-source-dictionary ; standard dictionary words
		     ac-source-filename ; just press / and directory completion appears
		     ac-source-files-in-current-dir ; from files in current directory
		     ;;ac-source-semantic ; symantic autocomplete for C/C++
		     ac-source-words-in-all-buffer ; all stuff from buffers
		     ;;ac-source-yasnippet
		     )))

(add-hook 'c-mode 'ac-ccc-mode-setup)
(add-hook 'c++-mode 'ac-ccc-mode-setup)

;; C++ coding style (indenting)
(defconst my-c-style
  '((c-tab-always-indent . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist . ((substatement-open after)
			       (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after)))
    (c-cleanup-list . (scope-operator
		       empty-defun-braces
		       defun-close-semi))
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
			(substatement-open . 0)
			(member-init-intro . ++)
			(case-label . 4)
			(block-open . 0)
			(inclass . 4)
			(innamespace . 0)
			(comment-intro . 0)
			(cpp-macro . 0)
			(knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "calx programming style")

(c-add-style "CALX" my-c-style)

(defun my-c-mode-common-hook ()
  (c-set-style "CALX")
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;//- CMake
;; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode)) auto-mode-alist))

;;//- JavaScript
;; js2 mode for .js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;//- Racket
;; Racket (run-racket)
(require 'ac-geiser)
(add-hooks 'ac-geiser-setup '(geiser-mode-hook
			      geiser-repl-mode-hook))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;;//- C#
;; csharp-mode inserts {} braces automatically (this totally breaks autopair)
(add-hook 'csharp-mode-hook (lambda () (local-set-key (kbd "{") 'c-electric-brace)))

;;//- Cg/HLSL/GLSL/ShaderLab
;; CG/HLSL mode
(require 'cg-mode)
(add-to-list 'auto-mode-alist '("\\.fx\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.cgfx\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.shader\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.compute\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.cginc\\'" . cg-mode))
(font-lock-add-keywords 'cg-mode operator-rex) ; highlight operators
(font-lock-add-keywords 'cg-mode number-rex) ; highlight numbers
(defvar preprocessor-rex '(("\\#[A-Za-z]+" . font-lock-preprocessor-face)))
(font-lock-add-keywords 'cg-mode preprocessor-rex) ; highlight operators

;; GLSL (regex)
(font-lock-add-keywords 'glsl-mode operator-rex)
(font-lock-add-keywords 'glsl-mode number-rex)
(font-lock-add-keywords 'glsl-mode preprocessor-rex)

;; GLSL file extensions
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))

;; define flycheck checker
(flycheck-define-checker glsl-checker
  "A GLSL syntax checker using glslangValidator."
  :command ("glslangValidator" source)
  :error-patterns
  ((error line-start
	  "ERROR: "
          column ":"
          line ":"
	  (message)
          line-end)
   (warning line-start
	    "wARNING: "
	    column ":"
	    line ":"
	    (message)
	    line-end)
   (info line-start
	 "NOTE: "
	 column ":"
	 line ":"
	 (message)
	 line-end))
  :modes (glsl-mode))

;; add checker hook
(add-hook 'glsl-mode-hook (lambda ()
			    (flycheck-mode)
			    (flycheck-select-checker 'glsl-checker)))

;; shaderlab indenting
(defconst my-shaderlab-style
  '((c-tab-always-indent . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist . ((substatement-open after)
			       (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after)))
    (c-cleanup-list . (scope-operator
		       empty-defun-braces
		       defun-close-semi))
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
			(substatement-open . 0)
			(case-label . 4)
			(block-open . 0)
			(inclass . 4)
			(innamespace . 0)
			(comment-intro . 0)
			(cpp-macro . 0)
			(statement-cont . 0)
			(knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "calx Unity ShaderLab style")

(c-add-style "CALX-SL" my-shaderlab-style)
(add-hook 'cg-mode-hook (lambda () (c-set-style "CALX-SL")))

;;//- Python
;; python
;;
;; on windows:
;; install python pip
;;
;; in command line:
;; python -m pip install virtualenv
;; python -m pip install epc
;; python -m pip install jedi
;; M-x jedi:start-dedicated-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;//- Emacs Lisp
;; flycheck in LISP
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;;//- Google integration
;; google services integration (uses google command line tools - googlecl)
(defun googlecl (command)
  "calls google command - line tool with given arguments"
  (interactive "sGoogle Command Line | Arguments: ")
  (message (format "google %s" command))
  (shell-command (format "google %s" command)))

(defun google-calendar-add (descr)
  "adds event to calendar"
  (interactive "sDescription: ")
  (googlecl (format "calendar add \"%s\" --reminder 1m" descr)))

(defun google-calendar-today ()
  "lists today's tasks"
  (interactive)
  (googlecl "calendar today"))

(global-set-key (kbd "C-' a") 'google-calendar-add)
(global-set-key (kbd "C-' t") 'google-calendar-today)

;;//- My plugins
;; page breaks / tags utility
(require 'tags)
(add-hooks (lambda ()
	     (highlight-page-breaks)
	     (highlight-todos))
	   '(cg-mode-hook c-mode-hook c++-mode-hook js-mode-hook js2-mode-hook csharp-mode-hook
			  emacs-lisp-mode-hook))
(global-set-key (kbd "M-]") 'next-page-break)
(global-set-key (kbd "M-[") 'prev-page-break)
(global-set-key (kbd "C-; x") 'page-breaks-popup)

;; CMake project utils
(require 'cmake-cpp-proj)

;; CEH
(add-to-list 'load-path "~/.emacs.d/ceh")
(require 'ceh)
(add-hooks 'ceh-mode
	   '(cg-mode-hook
	     c-mode-hook
	     c++-mode-hook
	     js-mode-hook
	     js2-mode-hook
	     csharp-mode-hook))

;; API
(require 'calx-api)

;; Hacker News Reader
(add-to-list 'load-path "~/.emacs.d/hnr")
(require 'hnr)

;;//- GUID generator and utilities
(require 's)

(defconst guid-command
  (if (string= system-type "windows-nt")
      "\"c:/Program Files/Microsoft SDKs/Windows/v6.0A/Bin/x64/uuidgen.exe\""
    "uuidgen"))

(defun generate-guid ()
  (s-trim (shell-command-to-string guid-command)))

(defun insert-guid ()
  (interactive)
  (insert (generate-guid)))

(defun replace-guids ()
  (interactive)
  (while
      (let* ((new-guid (generate-guid))
	     (searching (re-search-forward "{[A-Za-z0-9]\\{8\\}-[A-Za-z0-9]\\{4\\}-[A-Za-z0-9]\\{4\\}-[A-Za-z0-9]\\{4\\}-[A-Za-z0-9]\\{12\\}}" nil t)))
	(when (and searching (y-or-n-p "Replace with new GUID?"))
	  (replace-match (concat "{" new-guid "}")))
	searching)))

;;//- modal edit prototype
(defmacro moded--rk (desc &rest keylists)
  `(pcase (key-description (vector (read-key ,desc)))
     .,keylists))

(defmacro moded--rkl (desc delim &rest keylists)
  `(while (not (string= ,delim
			(let ((rkey (key-description (vector (read-key ,desc)))))
			  (pcase rkey .,keylists)
			  rkey)))))

(defmacro moded--rklk (desc key &rest expr)
  `(while (string= ,key (key-description (vector (read-key ,desc))))
    .,expr))

(defun moded-do ()
  (interactive)
  (set-face-attribute 'mode-line nil :background "firebrick")
  (moded--rk ">"
	     ("f" (ido-find-file))
	     ("g" (ido-switch-buffer))
	     ("j" (smex))
	     ("z" (undo))
	     ("s" (save-buffer))
	     ("x" (page-breaks-popup))
	     ("o" (switch-to-buffer (other-buffer)))
	     ("m" (moded--rkl "> Move"
			      "g"
			      ("l" (smooth-scroll -1 8 0.1))
			      ("m" (smooth-scroll 1 8 0.1))
			      ("j" (backward-word))
			      ("k" (forward-word))
			      ("d" (previous-line))
			      ("f" (next-line))
			      ("e" (backward-paragraph))
			      ("i" (forward-paragraph))))
	     ("c" (moded--rk "> Comment"
			     ("c" (comment-or-uncomment-region (region-beginning) (region-end)))
			     ("n" (moded--rk "> Comment > Next >"
					     ("l" (comment-or-uncomment-region (region-beginning) (save-excursion (forward-line) (point))))
					     ("b" (comment-or-uncomment-region (region-beginning) (save-excursion (forward-paragraph) (point))))))
			     ("b" (comment-or-uncomment-region (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))))
			     ("e" (ceh-comment-to-eol))
			     ("a" (ceh-comment-next-atom))))
	     ("v" (moded--rk "> Version Control"
			     ("d" (call-interactively 'vc-dir))
			     ("=" (vc-diff))
			     ("c" (vc-diff))
			     ("v" (call-interactively 'vc-next-action))
			     ("u" (call-interactively 'vc-revert))
			     ("l" (vc-print-log))
			     ("r" (moded--rk "> Version Control > Root"
					     ("l" (vc-print-root-log))))))
	     ("k" (moded--rk "> Kill"
			     ("k" (kill-buffer))
			     ("w" (kill-buffer-and-window))))
	     ("d" (moded--rk "> Compile"
			     ("d" (cm-compile-debug))
			     ("r" (cm-compile-release))
			     ("s" (cm-run-debug))))
	     ("w" (moded--rk "> Windows"
			     ("w" (delete-window))
			     ("j" (split-window-horizontally))
			     ("f" (split-window-vertically))))
	     ("i" (moded--rk "> Insert"
			     ("g" (insert-guid))))
	     ("b" (moded--rkl "> Buffer" "b"
			      ("j" (windmove-left))
			      ("k" (windmove-down))
			      ("l" (windmove-right))
			      ("i" (windmove-up))))
	     ("l" (toggle-buffer-finish) (toggle-buffer-next)
	      (moded--rklk "> Cycle Buffers" "l" (toggle-buffer-next)))
	     ("p" (moded--rk "> Project"
			     ("i" (or (switch-target) (vs-init)))
			     ("t" (switch-target))
			     ("c" (if (vs-active) (vs-switch-configuration) (switch-configuration))))))
  (set-face-attribute 'mode-line nil :background "#225599"))

(key-chord-define-global "jf" 'moded-do)
(key-chord-define-global "fj" 'moded-do)
(key-chord-mode t)

;;//- local custom settings
(load "~/.emacs.d/local-config" t)

;; TODO: clean up moded (do not call fx)
;; TODO: moded in arg mode / calx api
;; TODO: moded in VC
;; TODO: moded abort bug
;; TODO: toggling buffers
;; TODO: moded escape any key
;; TODO: CMake project - fix searching for executable
;; TODO: remove middle mouse button paste
;; TODO: fj correction on fail

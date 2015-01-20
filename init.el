;; --------------------------------------------------------------------------------------------------
;; package management

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
    fuzzy))

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

;; --------------------------------------------------------------------------------------------------

;; load paths
(add-to-list 'load-path "~/.emacs.d")

;; add hooks helper
(defun add-hooks (function hooks)
  "runs [function] for given [hooks]"
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;; no start screen
(setq inhibit-startup-screen 1)

(server-start)
(tool-bar-mode -1) ;; hide toolbar (icons)
(tooltip-mode -1) ;; hide tooltips
(scroll-bar-mode 0) ;; disable system scrollbars

;; theme
(load-theme 'calx t)

;; highlight matching braces
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
;; (setq show-paren-style 'expression)
;; (add-hook 'after-init-hook '(lambda ()(set-face-foreground 'show-paren-match nil))) ; keeps syntax color as foreground

;; show line numbers
(global-linum-mode t)

;; switches off word wrap
(setq-default truncate-lines 0)

;; tell emacs to open .h files in C++ mode (c-mode by default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; no auto-save-backups, thanks
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; default make command: | or just change filename from mingw-make32 to make on windows 'cause these are 2 different exe-s...
(setq-default compile-command "mingw32-make")

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

(cua-mode 1) ; cua-mode (Ctrl+C,V,X,Z)
(setq x-select-enable-clipboard t) ; allows to copy/paste text between emacs and other apps

;; yank-pop - more CUA - friendly
(global-unset-key (kbd "M-y"))
(global-set-key (kbd "C-M-v") 'yank-pop)

;; Ctrl + Backspace kill ring
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(defun forward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "<C-delete>") 'forward-delete-word)

;; automatic brackets {}()[]""'' pairing
(require 'autopair)
(autopair-global-mode)

;; open config file
(defun cfg ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; fix to conflict between cua rectangle mode and autopair (autopair overrides enter key (cua-rotate-rectangle))
;; just bind cua-rotate-rectangle to other keybind | TODO: check if is in rectangle mode
(global-set-key (kbd "C-M-r") 'cua-rotate-rectangle)

;; file name in title bar
(setq frame-title-format "emacs | %b")

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

;; C/C++ autocomplete hooks
(defun ac-ccc-mode-setup ()
  (setq ac-sources '(;ac-source-dictionary ; standard dictionary words
		     ac-source-filename ; just press / and directory completion appears
		     ac-source-files-in-current-dir ; from files in current directory
		     ;;ac-source-semantic ; symantic autocomplete for C/C++
		     ac-source-words-in-all-buffer ; all stuff from buffers
		     ;;ac-source-yasnippet
		     )))

(ac-flyspell-workaround) ; lag hack
(add-hook 'c-mode 'ac-ccc-mode-setup)
(add-hook 'c++-mode 'ac-ccc-mode-setup)

;; control + space = autocomplete (and enable AC mode if not enabled)
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC")
		'(lambda ()
		   (interactive)
		   (unless auto-complete-mode
		     (message "enabling auto-complete-mode")
		     (auto-complete-mode t))
		   (auto-complete)))

;; cursor type - horizontal bar '_'
(setq-default cursor-type 'hbar)

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

;; helm
(require 'helm-config)
(set 'helm-idle-delay 0.0)
(set 'helm-input-idle-delay 0.0)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

;; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode)) auto-mode-alist))

;; font lock decoration level: 1-3 | 2 is enough, 3 is too slow
(setq-default font-lock-maximum-decoration 2)

;; highlighting operators
;; "\\([][|!.+=&/%*,<>(){};:^~-?]+\\)"
(defvar operator-rex '(("\\([][|!.+=&/%*,<>(){};:^~\\-?]\\)" . font-lock-operator-face)))
(defvar operator-rex-xml '(("\\(<>/=\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'c-mode operator-rex)
(font-lock-add-keywords 'c++-mode operator-rex)
(font-lock-add-keywords 'js-mode operator-rex)
(font-lock-add-keywords 'js2-mode operator-rex)
(font-lock-add-keywords 'csharp-mode operator-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'xml-mode operator-rex-xml) ;; TODO: make this work
(font-lock-add-keywords 'python-mode operator-rex)

;; highlighting numbers
;;"\\<\\(\\([+-]?[0-9.]+[lufLU]*\\)\\|0[xX][0-9a-fA-F]+\\)\\>"
(defvar number-rex '(("\\<\\(\\([0-9.]+[lufLU]?\\)\\|0[xX][0-9a-fA-F]+\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'c-mode number-rex)
(font-lock-add-keywords 'c++-mode number-rex)
(font-lock-add-keywords 'js-mode number-rex)
(font-lock-add-keywords 'js2-mode number-rex)
(font-lock-add-keywords 'csharp-mode number-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\([0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'python-mode number-rex)

;; js2 mode for .js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(load "~/.emacs.d/skewer-config")
;(require 'simple-httpd)
;(setq httpd-root "d:/repo/minigame2")

;; C++ coding style (indenting)
(setq indent-tabs-mode t) ; no shitty spaces

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

;; csharp-mode inserts {} braces automatically (this totally breaks autopair)
(add-hook 'csharp-mode-hook (lambda () (local-set-key (kbd "{") 'c-electric-brace)))

;; auto indenting current line when pressing <enter>
(electric-indent-mode t)

;; ace-jump mode for quick jumping around
(require 'ace-jump-mode)
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)

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

;; speedup AC
(flyspell-mode 0)

;; shows current function name in modeline
;; (which-function-mode)

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

;; CG/HLSL mode
(add-to-list 'load-path "~/.emacs.d/my-packages")
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

;; pretty lambda
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; automatically reload files when changed
(global-auto-revert-mode t)

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

;; neotree
(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

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

;; gnus
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

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

;; recompiling stuff
(defun recompile-scripts ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

;; flycheck in LISP
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; GLSL
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))

;; flycheck
(require 'flycheck)
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

(add-hook 'glsl-mode-hook (lambda ()
			    (flycheck-mode)
			    (flycheck-select-checker 'glsl-checker)))

;; --------------------------------------------------------------------------------------------------
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

;; --------------------------------------------------------------------------------------------------
;; CMake project utils
(require 'cmake-cpp-proj)

;; CEH
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

;; mode-line
(defconst buffer-pos-indicator-length 25)

(defun buffer-pos ()
  (/ (* buffer-pos-indicator-length (point)) (point-max)))
(defun buffer-left ()
  (- buffer-pos-indicator-length (/ (* buffer-pos-indicator-length (point)) (point-max))))

(setq-default mode-line-format '("%e"
				 mode-line-front-space
				 mode-line-mule-info
				 mode-line-client
				 mode-line-modified
				 mode-line-remote
				 mode-line-frame-identification
				 (:eval (propertize ": " 'face 'font-lock-comment-face))
				 (:propertize (:eval mode-line-buffer-identification)  face mode-line-separator-face)
				 (:eval (propertize " : " 'face 'font-lock-comment-face))
				 (:eval (list
					 (propertize (make-string (buffer-pos) 9632) 'face 'mode-line-progress-face)
					 (propertize (make-string (buffer-left) 9632) 'face 'font-lock-comment-face)))
				 (:eval (propertize " : " 'face 'font-lock-comment-face))
				 (vc-mode vc-mode)
				 (:eval (propertize " : " 'face 'font-lock-comment-face))
				 mode-line-modes
				 mode-line-misc-info
				 mode-line-end-spaces
				 "%-"))

;; --------------------------------------------------------------------------------------------------
;; settings made by customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("12d9cd1e2bdcaf8f18c6e9bb56336f7a62c39ed034f8eb5d17ef66ec4fef03de" default)))
 '(highlight-symbol-colors (quote ("yellow green" "firebrick" "cornflower blue" "MediumPurple1")))
 '(highlight-symbol-idle-delay 0.5)
 '(highlight-symbol-ignore-list (quote ("\\`[0-9]+f?F?[ulUL]*\\'")))
 '(query-replace-show-replacement t)
 '(sml/active-background-color "green4"))

(put 'downcase-region 'disabled nil)

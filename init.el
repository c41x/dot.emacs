;; --------------------------------------------------------------------------------------------------
;; package management

(require 'cl-lib)
(require 'cl)
(require 'package)

(package-initialize)
(setq package-archives
      '(("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "http://stable.melpa.org/packages/")))

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
    helm-swoop
    smex
    popup
    highlight-symbol
    jedi
    flycheck
    key-chord
    indent-guide
    skewer-mode))

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
(global-set-key (kbd "C-,") 'helm-for-files)
(global-set-key (kbd "C-.") 'helm-swoop)
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

;; highlighting numbers
;;"\\<\\(\\([+-]?[0-9.]+[lufLU]*\\)\\|0[xX][0-9a-fA-F]+\\)\\>"
(defvar number-rex '(("\\<\\(\\([0-9.]+[lufLU]?\\)\\|0[xX][0-9a-fA-F]+\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'c-mode number-rex)
(font-lock-add-keywords 'c++-mode number-rex)
(font-lock-add-keywords 'js-mode number-rex)
(font-lock-add-keywords 'js2-mode number-rex)
(font-lock-add-keywords 'csharp-mode number-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\([0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))

;; js2 mode for .js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(load "~/.emacs.d/skewer-config")
;(require 'simple-httpd)
;(setq httpd-root "d:/repo/minigame2")


;; C++ compiling keybindings (CMake)
;; keybinds:
;; F5 - run & debug
;; SHIFT + F5 - restore window layout after debug
;; F6 - run
;; F7 - compile
;; Modifiers:
;; Shift - Release version
;; Control - Recompile project
(defvar last-inproject-directory-debug nil)
(defvar last-inproject-directory-release nil)
(defvar last-project-directory-debug nil)
(defvar last-project-directory-release nil)
(defvar last-inproject-executable-debug nil)
(defvar last-inproject-executable-release nil)

(defun actualize-path-cache ()
  (setq last-inproject-directory-debug (find-inproject-directory-debug))
  (setq last-inproject-directory-release (find-inproject-directory-release))
  (setq last-project-directory-debug (find-project-directory-debug))
  (setq last-project-directory-release (find-project-directory-release))
  (setq last-inproject-executable-debug (find-inproject-executable-debug))
  (setq last-inproject-executable-release (find-inproject-executable-release)))

(defmacro run-compile (dir fallback-dir)
  `(let ((dir (,dir)))
     (unless ,fallback-dir
       (setq ,fallback-dir dir)
       (actualize-path-cache))
     (compile (format
	       "mingw32-make -C %s --no-print-directory all"
	       ,fallback-dir))))

(defmacro run-exec (dir fallback-dir)
  `(let ((dir (,dir)))
     (unless ,fallback-dir
       (setq ,fallback-dir dir)
       (actualize-path-cache))
     (compile (format
	       "%s"
	       ,fallback-dir) t)))

(defmacro run-debug (dir fallback-dir)
  `(let ((dir (,dir)))
     (unless ,fallback-dir
       (setq ,fallback-dir dir)
       (actualize-path-cache))
     (gdb (format "gdb -i=mi %s" ,fallback-dir))))


(global-set-key
 (kbd "<f7>")
 '(lambda ()
    (interactive)
    (run-compile find-inproject-directory-debug
		 last-inproject-directory-debug)))

(global-set-key
 (kbd "S-<f7>")
 '(lambda ()
    (interactive)
    (run-compile find-inproject-directory-release
		 last-inproject-directory-release)))

(global-set-key
 (kbd "C-<f7>")
 '(lambda ()
    (interactive)
    (run-compile find-project-directory-debug
		 last-project-directory-debug))) ; compile full project

(global-set-key
 (kbd "C-S-<f7>")
 '(lambda ()
    (interactive)
    (run-compile find-project-directory-release
		 last-project-directory-release))) ; compile full project

(global-set-key
 (kbd "<f6>")
 '(lambda ()
    (interactive)
    (run-exec find-inproject-executable-debug
	      last-inproject-executable-debug)
    (select-window (get-buffer-window "*compilation*"))
    (end-of-buffer)))

(global-set-key
 (kbd "S-<f6>")
 '(lambda ()
    (interactive)
    (run-exec find-inproject-executable-release
	      last-inproject-executable-release)
    (select-window (get-buffer-window "*compilation*"))
    (end-of-buffer)))

(global-set-key
 (kbd "<f5>")
 '(lambda ()
    (interactive)
    (frame-configuration-to-register 1)
    (run-debug find-inproject-executable-debug
	       last-inproject-executable-debug)
    (setq gdb-many-windows t)))

(global-set-key
 (kbd "S-<f5>")
 '(lambda ()
    (interactive)
    (setq gdb-many-windows nil)
    (jump-to-register 1)))

(global-set-key (kbd "<f9>") 'gud-break) ; toggle breakpoint
(global-set-key (kbd "<left-margin> <mouse-1>") 'gud-break)
(global-set-key (kbd "<f10>") 'gud-next) ; next statement

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
(add-to-list 'load-path "~/.emacs.d/my-packages/")
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

;; indent guide for LISP
(require 'indent-guide)
(add-hook 'emacs-lisp-mode-hook 'indent-guide-mode)

;; flycheck in LISP
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

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

;; highlight "page breaks" / TODOs and apply in modes specified below
(defconst re-page-break ".?//-.")
(defconst re-todo ".?TODO\\:.")
(defconst re-page-break-or-todo "\\(.?//-.\\)\\|\\(.?TODO\\:.\\)")

(defun highlight-page-breaks ()
  (interactive)
  (highlight-lines-matching-regexp re-page-break 'font-lock-page-break-face))

(defun highlight-todos ()
  (interactive)
  (highlight-lines-matching-regexp re-todo 'font-lock-todo-face))

(add-hooks (lambda ()
	     (highlight-page-breaks)
	     (highlight-todos))
	   '(cg-mode-hook c-mode-hook c++-mode-hook js-mode-hook js2-mode-hook csharp-mode-hook))

(defvar page-break-wrap-search nil)
(defun page-break-navigate (dir)
  (if (eq dir 1)
      (next-line)
    (previous-line))
  (unless (re-search-forward re-page-break-or-todo nil t dir)
    (message "label not found!")
    (if page-break-wrap-search
	(progn
	  (setq page-break-wrap-search nil)
	  (if (eq dir 1)
	      (beginning-of-buffer)
	    (end-of-buffer))
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

(global-set-key (kbd "M-]") 'next-page-break)
(global-set-key (kbd "M-[") 'prev-page-break)

;; list page-breaks in popup.el
(defconst re-page-break-popup ".?//- ?\\(.*\\)")
(defun get-buffer-tags ()
  (let ((ret nil)
	(num 1))
    (save-excursion
      (beginning-of-buffer)
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
				:cursor (get-buffer-tags-cursor buffer-tags)))
      (message "no page breaks found"))))

(global-set-key (kbd "C-; x") 'page-breaks-popup)

;; --------------------------------------------------------------------------------------------------
;; CMake project utils

(defun upward-check-file (filename startdir)
  "Moves up in directory structure and checks if desired file is there"
  (let ((dirname (expand-file-name startdir))
	(not-found nil)
	(top nil)
	(max-level 5)
	(prv-dirname nil))

    (while (not (or not-found top (= max-level 0)))
      (setq max-level (- max-level 1))
      (if (string= (expand-file-name dirname) "/")
	  (setq top t))
      (if (file-exists-p (expand-file-name filename dirname))
	  (progn
	    (setq prv-dirname dirname)
	    (setq dirname (expand-file-name ".." dirname)))
	(setq not-found t)))

    prv-dirname))

(defun find-project-directory-base (project-dir)
  "Returns CMake project root directory or nil"
  (interactive)
  (let ((file (upward-check-file "CMakeLists.txt" ".")))
    (if file (concat (file-name-as-directory file) project-dir) nil)))

(defun is-cmake-project ()
  (if (upward-check-file "CMakeLists.txt" ".") t nil))

(defun find-inproject-directory-base (project-dir tail)
  "returns corresponding directory in CMake project directory structure"
  (let ((project-root (upward-check-file "CMakeLists.txt" "."))
	(full-path (expand-file-name ".")))
    (if project-root
	(concat project-root project-dir (substring full-path (length project-root)) tail)
      nil)))

(defun find-inproject-executable-base (project-dir)
  "returns path to executable in CMake directory structure"
  (let ((inproject-dir (find-inproject-directory-base project-dir "")))
    (concat inproject-dir "/" (file-name-nondirectory inproject-dir))))

(defun find-executable-name ()
  "returns executable name"
  (file-name-nondirectory (expand-file-name ".")))

(defun find-project-directory-debug ()
  (find-project-directory-base "project/debug"))
(defun find-project-directory-release ()
  (find-project-directory-base "project/release"))

(defun find-project-directory ()
  (find-project-directory-base ""))
(defun find-inproject-directory-release ()
  (find-inproject-directory-base "/project/release" "/"))
(defun find-inproject-directory-debug ()
  (find-inproject-directory-base "/project/debug" "/"))

(defun find-inproject-executable-debug ()
  (find-inproject-executable-base "/project/debug"))
(defun find-inproject-executable-release ()
  (find-inproject-executable-base "/project/release"))

;; flycheck for CMake project
(defun get-string-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun extract-includes-from-file (file-name project-source-dir)
  (let ((i 0) (matches '()) (file-buffer (get-string-from-file file-name)))
    (save-match-data
      (while (string-match "include_directories(\"\\([^\"]*\\)\")" file-buffer i)
	(setq i (match-end 1))
	(add-to-list 'matches
		     (replace-regexp-in-string
		      "//" "/"
		      (replace-regexp-in-string
		       "${project_source_dir}"
		       project-source-dir
		       (match-string-no-properties 1 file-buffer)
		       1)
		      1))))
    matches))

(defun get-current-project-include-list ()
  (let ((proj-dir (find-project-directory)))
    (if proj-dir
	(extract-includes-from-file (concat proj-dir "CMakeLists.txt") proj-dir)
      nil)))

;;(add-hook 'c++-mode-hook
;;          (lambda ()
;; 	    (when (is-cmake-project)
;; 	      (setq flycheck-c/c++-gcc-executable "mingw32-gcc")
;; 	      (setq flycheck-gcc-include-path (get-current-project-include-list))
;; 	      (setq flycheck-idle-change-delay 10.0)
;; 	      (flycheck-mode)
;; 	      (flycheck-select-checker 'c/c++-gcc))))

;; --------------------------------------------------------------------------------------------------
;; CEH - C Edit Helper
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

(defconst ceh--operators "- */\+|&^%!,<>=\n\t")
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

(defun ceh--in-array (element array)
  (let ((i 0))
    (while (and
	    (< i (length array))
	    (not (= element (elt array i))))
      (setq i (+ i 1)))
    (not (= i (length array)))))

;; interactives
(defun ceh-parametrize ()
  (interactive)
  (let ((c-prev (char-before))
	(c-next (char-after)))
    (cond ((ceh--in-array c-prev ceh--operators) ;; when in middle of operators lr slurp
	   (let ((pt (point)))
	     (ceh--bck-operators)
	     (ceh--bck-expression)
	     (insert "(")
	     (goto-char (+ pt 1))
	     (ceh--fwd-operators)))
	  ((or (eq c-prev ?\)) (eq c-next ?\))) ;; when parens here -> slurp argument from r
	   (if (not (eq c-next ?\))) ;; make sure that we are before closing paren
	       (backward-char))
	   (delete-char 1)
	   (ceh--fwd-operators))
	  (t ;; otherwise insert new parenthesis
	   (insert "(")))
    (progn
      (ceh--fwd-expression) ;; (re)insert closing parenthesis
      (backward-char)
      (insert ")"))))

(defun ceh-unparametrize ()
  (interactive)
  (if (eq (char-before) ?\)) ;; barf?
      (progn
	(backward-char)
	(delete-char 1)
	(ceh--bck-expression)
	(ceh--bck-operators)
	(insert ")"))))

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

(defun ceh-next-line ()
  (interactive)
  (end-of-line)
  (next-line)
  (end-of-line))

(defun ceh-new-brace ()
  (interactive)
  (end-of-line)
  (insert " {")
  (newline)
  (newline)
  (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

(defun ceh-finish-expression ()
  (interactive)
  (end-of-line)
  (if (not (ceh--in-array (char-before) ";:}{+-|&<\\//.,!*="))
      (insert ";"))
  (indent-for-tab-command))

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

;; TODO: S-C-"  -> stringize token

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
	    (define-key map (kbd "C-)") 'ceh-unparametrize)
	    (define-key map (kbd "M-,") 'ceh-step-in-args) ;; tags!
	    (define-key map (kbd "M-.") 'ceh-step-out-of-args) ;; tags!
	    (define-key map (kbd "C-' s") 'ceh-transpose-args)
	    (define-key map (kbd "C-' d") 'ceh-leave-me)
	    (define-key map (kbd "TAB") 'ceh-expand)
	    (define-key map (kbd "<tab>") 'ceh-expand)
	    map)
  ;; chords
  (require 'key-chord)
  (key-chord-define-global "qq" 'ceh--chord-kill-line)
  (key-chord-define-global ",," 'ceh--chord-skip-chars)
  (key-chord-define-global "[[" 'ceh-include-expr)
  (key-chord-define-global "]]" 'ceh-exclude-expr)
  (key-chord-define-global "[]" 'ceh-create-block-include-expr)
  (key-chord-mode +1))

;; add hooks
(add-hooks 'ceh-mode
	   '(cg-mode-hook
	     c-mode-hook
	     c++-mode-hook
	     js-mode-hook
	     js2-mode-hook
	     csharp-mode-hook))


;; --------------------------------------------------------------------------------------------------
;; API
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
(load "~/.emacs.d/api-server-config")

(defun calx-login (username password)
  (interactive "sLogin: \nsPassword:")
  (calx--http-post (concat calx--server-api-url "/api_login")
		   (lambda (status)
		     (calx--request-callback-status status)
		     (calx-get))
		   (format "username=%s&password=%s" username password)))

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
 '(highlight-symbol-ignore-list '("\\`[0-9]+f?F?[ulUL]*\\'"))
 '(mode-line-format (quote ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces "%-")))
 '(query-replace-show-replacement t)
 '(sml/active-background-color "green4"))

(put 'downcase-region 'disabled nil)

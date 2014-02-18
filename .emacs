(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(cua-mode t nil (cua-base))
 '(global-linum-mode t)
 '(query-replace-show-replacement t)
 '(show-paren-mode t)
 '(sml/active-background-color "green4")
 '(tool-bar-mode nil))

;-------------------------------------------------------------------------------
; package management

(require 'cl-lib)
(require 'cl)
(require 'package)

(package-initialize)
(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

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
    popup))

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
		    
;--------------------------------------------------------------------------------------------------
; hide unused GUI's

; no start screen
(setq inhibit-startup-screen 1)

(server-start)
(tool-bar-mode -1) ;; hide toolbar (icons)
(tooltip-mode -1) ;; hide tooltips

; theme
(load-theme 'calx t)

; disable system scrollbars
(scroll-bar-mode 0)

; highlight matching braces
(show-paren-mode t)

; show line numbers
(global-linum-mode t)

; switches off word wrap
(setq-default truncate-lines 0)

; tell emacs to open .h files in C++ mode (c-mode by default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; no auto-save-backups, thanks
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

; default make command: | or just change filename from mingw-make32 to make on windows 'cause these are 2 different exe-s...
(setq-default compile-command "mingw32-make")

; keystrokes
(global-unset-key (kbd "<C-z>")) ; disable Ctrl+z hide
(global-set-key (kbd "<C-tab>") 'other-window) ; ctrl+tab to switch panels
(global-set-key (kbd "C-c C-<up>") (lambda () (interactive) (enlarge-window 2)))
(global-set-key (kbd "C-c C-<down>") (lambda () (interactive) (enlarge-window -2)))
(global-set-key (kbd "C-c C-<right>") (lambda () (interactive) (enlarge-window-horizontally 2)))
(global-set-key (kbd "C-c C-<left>") (lambda () (interactive) (enlarge-window-horizontally -2)))
(cua-mode 1) ; cua-mode (Ctrl+C,V,X,Z)
(setq x-select-enable-clipboard t) ; allows to copy/paste text between emacs and other apps

; yank-pop - more CUA - friendly
(global-unset-key (kbd "M-y"))
(global-set-key (kbd "C-M-v") 'yank-pop)

; automatic brackets {}()[]""'' pairing
(require 'autopair)
(autopair-global-mode)

; open config file
(defun cfg ()
  (interactive)
  (find-file "~/.emacs"))

; fix to conflict between cua rectangle mode and autopair (autopair overrides enter key (cua-rotate-rectangle))
; just bind cua-rotate-rectangle to other keybind | TODO: check if is in rectangle mode
(global-set-key (kbd "C-M-r") 'cua-rotate-rectangle)

; iswitchb mode for tabs
(iswitchb-mode t)

; yasnippet
(require 'yasnippet)
(yas--initialize)
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
(setq-default yas/trigger-key (kbd "C-;"))

; ido mode
(require 'ido)
(ido-mode t)

; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete-dict")
(ac-config-default)

; auto-complete
(setq-default auto-complete-mode t)
(global-auto-complete-mode t)

; documentation tip
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

; C/C++ autocomplete hooks
(defun ac-ccc-mode-setup ()
  (setq ac-sources '(;ac-source-dictionary ; standard dictionary words
 					 ac-source-filename ; just press / and directory completion appears
 					 ac-source-files-in-current-dir ; from files in current directory
 					 ;ac-source-semantic ; symantic autocomplete for C/C++
 					 ac-source-words-in-all-buffer ; all stuff from buffers
 					 ac-source-yasnippet)))
 
(ac-flyspell-workaround) ; lag hack
(add-hook 'c-mode 'ac-ccc-mode-setup)
(add-hook 'c++-mode 'ac-ccc-mode-setup)

; control + space = autocomplete
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC") '(lambda () (interactive) (auto-complete)))

; cursor type - horizontal bar '_'
(setq-default cursor-type 'hbar)

; simple smooth scrolling (sit-for is some kind of Sleep)
; time is not accurate because lag may occur while scrolling... so tweak it experimenally
(defun smooth-scroll (lines number-of-iterations time)
    (let ((sit-for-time (/ (float time) (float number-of-iterations))))
	(loop for i from 1 to number-of-iterations do (progn
        (scroll-up lines)
		(sit-for sit-for-time)))))

(global-set-key (kbd "C-M-<up>") '(lambda () (interactive) (smooth-scroll -3 7 0.1)))
(global-set-key (kbd "C-M-<down>") '(lambda () (interactive) (smooth-scroll 3 7 0.1)))
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (smooth-scroll -1 8 0.1)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (smooth-scroll 1 8 0.1)))

; helm
(require 'helm-config)
(helm-mode t)
(set 'helm-idle-delay 0.0)
(set 'helm-input-idle-delay 0.0)
(global-set-key (kbd "C-,") '(lambda ()(interactive) (helm-for-files)))
(global-set-key (kbd "C-.") '(lambda ()(interactive) (helm-swoop)))

; cmake-mode
;(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode)) auto-mode-alist))

; font lock decoration level: 1-3 | 2 is enough, 3 is too slow on WinXP
(setq-default font-lock-maximum-decoration 2)

; highlighting operators
; "\\([][|!.+=&/%*,<>(){};:^~-?]+\\)"
(defvar operator-rex '(("\\([][|!.+=&/%*,<>(){};:^~\\-?]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'c-mode operator-rex)
(font-lock-add-keywords 'c++-mode operator-rex)
(font-lock-add-keywords 'js-mode operator-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
 
; highlighting numbers
;"\\<\\(\\([+-]?[0-9.]+[lufLU]*\\)\\|0[xX][0-9a-fA-F]+\\)\\>"
(defvar number-rex '(("\\<\\(\\([0-9.]+[lufLU]?\\)\\|0[xX][0-9a-fA-F]+\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'c-mode number-rex)
(font-lock-add-keywords 'c++-mode number-rex)
(font-lock-add-keywords 'js-mode number-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\([0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))

; C++ compiling keybindings (CMake)
(global-set-key (kbd "<f7>") '(lambda () (interactive) (compile (format "mingw32-make -C %s --no-print-directory all" (find-inproject-directory-debug)))))
(global-set-key (kbd "S-<f7>") '(lambda () (interactive) (compile (format "mingw32-make -C %s --no-print-directory all" (find-inproject-directory-release)))))
(global-set-key (kbd "C-<f7>") '(lambda () (interactive) (compile (format "mingw32-make -C %s --no-print-directory all" (find-project-directory-debug))))) ; compile full project
(global-set-key (kbd "C-S-<f7>") '(lambda () (interactive) (compile (format "mingw32-make -C %s --no-print-directory all" (find-project-directory-release))))) ; compile full project
(global-set-key (kbd "<f6>") '(lambda () (interactive) (compile (format "%s" (find-inproject-executable-debug)))))
(global-set-key (kbd "S-<f6>") '(lambda () (interactive) (compile (format "%s" (find-inproject-executable-release)))))
(global-set-key (kbd "<f5>") '(lambda () (interactive) (gdb (format "gdb -i=mi %s" (find-inproject-executable-debug)))))
(global-set-key (kbd "<f9>") 'gdb-toggle-breakpoint) ; toggle breakpoint
(global-set-key (kbd "<f10>") 'gud-next) ; next statement

; C++ coding style (indenting)
(setq c-offsets-alist '((member-init-intro . ++)))

; Create my personal style.
(setq indent-tabs-mode t) ; no shitty spaces

(defconst my-c-style
 '((c-tab-always-indent	 . t)
   (c-comment-only-line-offset . 4)
   (c-hanging-braces-alist	 . ((substatement-open after)
				    (brace-list-open)))
   (c-hanging-colons-alist	 . ((member-init-intro before)
				    (inher-intro)
				    (case-label after)
				    (label after)
				    (access-label after)))
   (c-cleanup-list		 . (scope-operator
				    empty-defun-braces
				    defun-close-semi))
   (c-offsets-alist		 . ((arglist-close . c-lineup-arglist)
				    (substatement-open . 0)
				    (case-label . 4)
				    (block-open . 0)
				    (inclass . 4)
				    (innamespace . 0)
				    (comment-intro . 0)
				    (knr-argdecl-intro . -)))
   (c-echo-syntactic-information-p . t))
 "calx programming style")

(c-add-style "CALX" my-c-style)

(defun my-c-mode-common-hook ()
 (c-set-style "CALX")
 (setq indent-tabs-mode t) ; no shitty spaces
 (setq tab-width 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

; auto indenting current line when pressing <enter>
(electric-indent-mode t)

; ace-jump mode for quick jumping around
(require 'ace-jump-mode)
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)

; increment closest number
(defun change-closest-number (increase-by)
  (interactive)
  (if (re-search-backward "[0-9]+" -200 t 1) ; no forward searching
	  (progn
		(message "changing: %s->%d" (match-string 0) (+ increase-by (string-to-int (match-string 0))))
		(replace-match (number-to-string (+ increase-by (string-to-number (match-string 0))))))
	(error "No number found at this point")))

(global-set-key (kbd "C-c +") (lambda () (interactive) (change-closest-number 1)))
(global-set-key (kbd "C-c -") (lambda () (interactive) (change-closest-number -1)))

(flyspell-mode 0)

; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; expand region
(require 'expand-region)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'er/expand-region)

; CG mode
(add-to-list 'load-path "~/.emacs.d/my-packages/")
(require 'cg-mode)
(add-to-list 'auto-mode-alist '("\\.fx\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.cgfx\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.shader\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.cginc\\'" . cg-mode))
(font-lock-add-keywords 'cg-mode operator-rex) ; highlight operators
(font-lock-add-keywords 'cg-mode number-rex) ; highlight numbers
(defvar preprocessor-rex '(("\\#[A-Za-z]+" . font-lock-preprocessor-face)))
(font-lock-add-keywords 'cg-mode preprocessor-rex) ; highlight operators

; pretty lambda
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

; swapping windows
(defun swap-windows ()
  "Swaps buffers in selected-window with next-window"
  (interactive)
  (unless (one-window-p)
	(let* ((this (selected-window))
		   (that (next-window))
		   (this-buffer (window-buffer this))
		   (that-buffer (window-buffer that)))
	  (set-window-buffer this that-buffer)
	  (set-window-buffer that this-buffer))))

; kill-close
(defun kill-close-window ()
  "Kills buffer and then closes window"
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "C-x C-k") 'kill-close-window)

; highlight 80+ lines
(defun highlight-80+ ()
  "Highlights lines that have 80+ characters"
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'font-lock-over-80-face))

; file name in title bar
(setq frame-title-format "emacs | %b")

;--------------------------------------------------------------------------------------------------
; CMake project utils
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

;--------------------------------------------------------------------------------------------------
; additional help ;
; to refresh settings just run M-x (eval-buffer)

(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((t (:foreground "orange red" :underline nil))))
 '(compilation-info ((t (:foreground "light slate blue"))))
 '(compilation-warning ((t (:foreground "green yellow" :underline nil))))
 '(diff-added ((t (:background "dark green"))))
 '(diff-removed ((t (:background "firebrick4"))))
 '(dired-directory ((t (:foreground "#eeaa11"))))
 '(mode-line ((t (:background "#097e00" :foreground "#dddddd" :box nil))))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit mode-line :background "#444444" :foreground "#857b6f" :box nil))))
 '(sml/filename ((t (:inherit sml/global :foreground "lemon chiffon"))))
 '(warning ((t (:foreground "DarkOrange")))))

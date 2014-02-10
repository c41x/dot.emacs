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
 '(tool-bar-mode nil))

;-------------------------------------------------------------------------------
(require 'cl-lib)
(require 'cl)
(require 'package)

; package management
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

;-------------------------------------------------------------------------------
(server-start)

; disable startup screen
(setq inhibit-startup-message t)

; hide unused GUI's
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

; ido mode
(setq-default ido-mode t)
(ido-mode t)
;(ido-enable-flex-matching t)

; tell emacs to open .h files in C++ mode (c-mode by default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; no auto-save-backups, thanks
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

; keystrokes
(global-unset-key (kbd "<C-z>")) ; disable Ctrl+z hide
(global-set-key (kbd "<C-tab>") 'other-window) ; ctrl+tab to switch panels
(global-set-key (kbd "C-c C-<up>") (lambda () (interactive) (enlarge-window 2)))
(global-set-key (kbd "C-c C-<down>") (lambda () (interactive) (enlarge-window -2)))
(global-set-key (kbd "C-c C-<right>") (lambda () (interactive) (enlarge-window-horizontally 2)))
(global-set-key (kbd "C-c C-<left>") (lambda () (interactive) (enlarge-window-horizontally -2)))
(cua-mode 1) ; cua-mode (Ctrl+C,V,X,Z)
(setq x-select-enable-clipboard t) ; allows to copy/paste text between emacs and other apps
;(global-set-key (kbd "C-u C-c") 'uncomment-region)
;(global-set-key (kbd "C-c C-m") 'comment-region)

; yank-pop - more CUA - friendly
(global-unset-key (kbd "M-y"))
(global-set-key (kbd "C-M-v") 'yank-pop)

; remember saved buffers and screens
;(desktop-save-mode 1)

; open config file
(defun cfg ()
  (interactive)
  (find-file "~/.emacs"))

; automatic brackets {}()[]""'' pairing
(require 'autopair)
(autopair-global-mode)

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

; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete-dict")
(ac-config-default)

; auto-complete
(setq-default auto-complete-mode t)
(global-auto-complete-mode t)

; C/C++ autocomplete hooks
(defun ac-ccc-mode-setup ()
  (setq ac-sources '(;ac-source-dictionary ; standard dictionary words
 					 ac-source-filename ; just press / and directory completion appears
 					 ac-source-files-in-current-dir ; from files in current directory
 					 ac-source-semantic ; symantic autocomplete for C/C++
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

; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode)) auto-mode-alist))

; font lock decoration level: 1-3 | 2 is enough, 3 is too slow on WinXP
(setq-default font-lock-maximum-decoration 2)

; highlighting operators
(defvar operator-rex '(("\\([][|!.+=&/%*,<>(){};:^~\\-?]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'c-mode operator-rex)
(font-lock-add-keywords 'c++-mode operator-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
 
; highlighting numbers
(defvar number-rex '(("\\<\\(\\([0-9.]+[lufLU]?\\)\\|0[xX][0-9a-fA-F]+\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'c-mode number-rex)
(font-lock-add-keywords 'c++-mode number-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\([0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))

; C++ coding style (indenting)
(setq c-offsets-alist '((member-init-intro . ++)))

; Create my personal style.
(setq-default indent-tabs-mode t) ; no shitty spaces
(setq-default tab-width 4)

(defconst my-c-style
  '((c-tab-always-indent	 . t)
	(c-comment-only-line-offset . 4)
	(c-hanging-braces-alist	 . ((substatement-open after) (brace-list-open)))
	(c-hanging-colons-alist	 . ((member-init-intro before) (inher-intro) (case-label after) (label after) (access-label after)))
	(c-cleanup-list		 . (scope-operator
							empty-defun-braces
							defun-close-semi))
	(c-offsets-alist		 . ((arglist-close . c-lineup-arglist)
								(substatement-open . 0)
								(case-label . 4)
								(block-open . 0)
								(inclass . 4)
								(innamespace . 0)
								(knr-argdecl-intro . -)))
	(c-echo-syntactic-information-p . t))
 "calx programming style")

(c-add-style "CALX" my-c-style)

(defun my-c-mode-common-hook ()
 (c-set-style "CALX")
 (setq indent-tabs-mode t) ; no shitty spaces
 (setq tab-width 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(electric-indent-mode t)

; tree-mode
;(add-to-list 'load-path "~/.emacs.d/tree")
;(require 'imenu-tree)
;(require 'tags-tree)
;(require 'tree-mode)
;(eval-after-load "tree-widget"
;  '(if (boundp 'tree-widget-themes-load-path)
;       (add-to-list 'tree-widget-themes-load-path "~/.emacs.d/tree/tree-widget/imenu/")))
;(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
;(autoload 'tags-tree "tags-tree" "TAGS tree" t)
; 
;; dirtree
;(require 'dirtree)

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

; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)

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
(font-lock-add-keywords 'cg-mode operator-rex) ; highlight operators
(font-lock-add-keywords 'cg-mode number-rex) ; highlight numbers
(defvar preprocessor-rex '(("\\#[A-Za-z]+" . font-lock-preprocessor-face)))
(font-lock-add-keywords 'cg-mode preprocessor-rex) ; highlight operators

; recompile . files
(defun recompile-dots ()
  (interactive)
  (byte-recompile-directory "~/" 0))

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

;-------------------------------------------------------------------------------
; additional help ;
; to refresh settings just run M-x (eval-buffer)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "forest green"))))
 '(diff-removed ((t (:foreground "orange red"))))
 '(dired-directory ((t (:foreground "#eeaa11"))))
 '(mode-line ((t (:background "#097e00" :foreground "#dddddd" :box nil))))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit mode-line :background "#444444" :foreground "#857b6f" :box nil :weight light)))))


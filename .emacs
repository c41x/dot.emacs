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
		    
;--------------------------------------------------------------------------------------------------
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

; default make command: | or just change filename from mingw-make32 to make on windows 'cause these are 2 different exe-s...
;(setq-default compile-command "mingw32-make")

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
(desktop-save-mode 1)
;(require 'workgroups)
;(workgroups-mode 1)

; popup (needed for autocomplete)
(add-to-list 'load-path "~/.emacs.d")

; automatic brackets {}()[]""'' pairing
(add-to-list 'load-path "~/autopair")
(require 'autopair)
(autopair-global-mode)

; fix to conflict between cua rectangle mode and autopair (autopair overrides enter key (cua-rotate-rectangle))
; just bind cua-rotate-rectangle to other keybind | TODO: check if is in rectangle mode
(global-set-key (kbd "C-M-r") 'cua-rotate-rectangle)

; set window size & position (1920x1200)
;(if (window-system)
;    (progn
;      (set-frame-size (selected-frame) 250 90)
;      (set-frame-position (selected-frame) 60 10)))

; iswitchb mode for tabs
(iswitchb-mode t)

; gtalk
;(add-to-list 'load-path "~/.emacs.d/emacs-jabber-0.8.0")
;(require 'jabber)
;(setq jabber-account-list '(
;                            ("jakubduracz@gmail.com"
;                            ;;   (:password . nil) or (:password . "your-pass")
;                              (:network-server . "talk.google.com")
;                              (:port . 443)
;                              (:connection-type . ssl))
;                            ))

; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas--initialize)
(yas-load-directory "~/.emacs.d/yasnippet/snippets")
(yas-global-mode 1)
(setq-default yas/trigger-key (kbd "C-;"))

; autocomplete
;(add-to-list 'load-path "~/.emacs.d/autocomplete")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete/dict")
;(ac-config-default)

; auto-complete
;(setq-default auto-complete-mode t)
;(global-auto-complete-mode t)

; C/C++ autocomplete hooks
;(defun ac-ccc-mode-setup ()
;  (setq ac-sources '(;ac-source-dictionary ; standard dictionary words
; 					 ;ac-source-filename ; just press / and directory completion appears
; 					 ac-source-files-in-current-dir ; from files in current directory
; 					 ;ac-source-semantic ; symantic autocomplete for C/C++
; 					 ac-source-words-in-all-buffer ; all stuff from buffers
; 					 ac-source-yasnippet)))
; 
;(ac-flyspell-workaround) ; lag hack

(add-hook 'c-mode 'ac-ccc-mode-setup)
(add-hook 'c++-mode 'ac-ccc-mode-setup)

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
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
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

; C++ compiling keybindings (TODO: write cmake extension)
;(global-set-key (kbd "<f7>") '(lambda () (interactive) (compile "make all"))) ; just build default target (all:)
;(global-set-key (kbd "<f6>") '(lambda () (interactive) (compile "make run"))) ; make "run:" target
;(global-set-key (kbd "<f5>") 'gdb) ; debug me
;(global-set-key (kbd "<f9>") 'gdb-toggle-breakpoint) ; toggle breakpoint
;(global-set-key (kbd "<f10>") 'gud-next) ; next statement

;(global-set-key (kbd "<f3>") '(lambda () (interactive) (shell-command "cd d:/repo/multimania/data/scripts & d:/repo/multimania/data/scripts/sync_shared_vita.py")))
;(global-set-key (kbd "<f4>") '(lambda () (interactive) (shell-command "cd d:/repo/shakespears/data/scripts & d:/repo/shakespears/data/scripts/prepare_pc_vita.py")))
(global-set-key (kbd "<f5>") '(lambda () (interactive) (shell-command "cd d:/repo/shakespears/data/scripts & d:/repo/shakespears/data/scripts/prepare_pc_vita.py")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (shell-command "cd d:/repo/shakespears/data/scripts & d:/repo/shakespears/data/scripts/prepare_pc_ps3.py")))
(global-set-key (kbd "<f7>") '(lambda () (interactive) (shell-command "cd d:/repo/shakespears/data/scripts & d:/repo/shakespears/data/scripts/prepare_ps3.py")))

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

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

; tree-mode
(add-to-list 'load-path "~/.emacs.d/tree")
(require 'imenu-tree)
(require 'tags-tree)
(require 'tree-mode)
(eval-after-load "tree-widget"
  '(if (boundp 'tree-widget-themes-load-path)
       (add-to-list 'tree-widget-themes-load-path "~/.emacs.d/tree/tree-widget/imenu/")))
(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
(autoload 'tags-tree "tags-tree" "TAGS tree" t)

; dirtree
(require 'dirtree)

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

;
(defun process-shit ()
  (interactive)
  (replace-string "4096" "2048")
  (replace-string "2048" "1024")
  (replace-string "1024" "512")
  (replace-string "512" "256")
  (replace-string "256" "128")
)

; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
						 ("gnu"  . "http://elpa.gnu.org/packages/")
						 ("melpa". "http://melpa.milkbox.net/packages/")
						 ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))

(flyspell-mode 0)

; multiple cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; expand region
(add-to-list 'load-path "~/.emacs.d/expand-region")
(require 'expand-region)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'er/expand-region)

; CG mode
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

; powerline
;(add-to-list 'load-path "~/.emacs.d/powerline")
;(require 'powerline)
;(powerline-default-theme)

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

;--------------------------------------------------------------------------------------------------
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
 '(mode-line-highlight ((t (:box (:line-width 2 :color "#ffffff"))))))


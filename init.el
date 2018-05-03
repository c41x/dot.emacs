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
    helm-git-grep
    helm-ls-git
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
    web-mode
    s
    company
    company-irony
    irony
    flycheck-irony
    neotree
    omnisharp
    ggtags
    zeal-at-point
    ac-php))

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

(defun flycheck-error-in-line (line range)
  (catch 'ret
    (dolist (e flycheck-current-errors)
      (let ((l (- line range))
            (r (+ line range))
            (error-line (elt e 4))
            (error-type (elt e 7)))
        (when (and (equal error-type 'error)
                   (> error-line l)
                   (< error-line r))
          (throw 'ret t))))
    nil))

(defun modeline-bar (index)
  (let ((line (* (/ (float index)
                    (float buffer-pos-indicator-length))
                 (float (total-lines))))
        (start (line-number-at-pos (window-start)))
        (end (line-number-at-pos (window-end)))
        (lines-in-cell (/ (float (total-lines))
                          (float buffer-pos-indicator-length))))
    (let ((err (flycheck-error-in-line line (/ lines-in-cell 2.0))))
      (if (or (< line start)
              (> line end))
          (if err 'mode-line-error-face-bg 'mode-line-bg-face)
        (if err 'mode-line-error-face 'mode-line-progress-face)))))

(setq-default mode-line-format '("%e"
                                 mode-line-front-space
                                 mode-line-mule-info
                                 mode-line-client
                                 mode-line-modified
                                 mode-line-remote
                                 mode-line-frame-identification
                                 (:eval (if (buffer-modified-p)
                                            (propertize (make-string 1 9632) 'face 'font-lock-number-face)
                                          (propertize (make-string 1 9632) 'face 'mode-line-bg-face)))
                                 " "
                                 (:propertize (:eval mode-line-buffer-identification) face mode-line-separator-face)
                                 " "
                                 (:eval (if (not (buffer-all-visible))
                                            (cl-loop for i from 1 to buffer-pos-indicator-length collect
                                                     (propertize (make-string 1 9632)
                                                                 'face
                                                                 (modeline-bar i)))))
                                 " "
                                 (vc-mode vc-mode)
                                 " "
                                 (:eval (if (boundp 'mode-line-project) (propertize (concat " " mode-line-project " ") 'face 'mode-line-2)))
                                 " "
                                 " "
                                 mode-line-modes
                                 mode-line-misc-info
                                 mode-line-end-spaces
                                 "%-"))

;;//- visual/UI
;; no start screen
(setq inhibit-startup-screen 1)

;; theme
(load-theme 'calx t)

;; set frame size
(add-hook 'after-init-hook (lambda () (set-frame-size (selected-frame) 100 60)))

;; show line numbers
(global-linum-mode t)

;; (unless (string= system-type "windows-nt")
;;   (server-start))
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
(font-lock-add-keywords 'maxscript-mode '(("\\([=()#.,+]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'emacs-lisp-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'lisp-interaction-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'lisp-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'scheme-mode '(("\\([()'.]\\)" . font-lock-operator-face)))
(font-lock-add-keywords 'xml-mode operator-rex-xml) ;; TODO: make this work
(font-lock-add-keywords 'python-mode operator-rex)

;; highlighting numbers
;;"\\<\\(\\([+-]?[0-9.]+[lufLU]*\\)\\|0[xX][0-9a-fA-F]+\\)\\>"
(defvar number-rex '(("\\<\\(\\([0-9.]+[lufLUe]?\\)\\|0[xX][0-9a-fA-F]+\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'c-mode number-rex)
(font-lock-add-keywords 'c++-mode number-rex)
(font-lock-add-keywords 'js-mode number-rex)
(font-lock-add-keywords 'js2-mode number-rex)
(font-lock-add-keywords 'csharp-mode number-rex)
(font-lock-add-keywords 'maxscript-mode number-rex)
(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\(-?[0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'lisp-interaction-mode '(("\\<\\(-?[0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'lisp-mode '(("\\<\\(-?[0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))
(font-lock-add-keywords 'scheme-mode '(("\\<\\(-?[0-9]+\\.?[0-9]*\\)\\>" . font-lock-number-face)))
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

;; smart comment/uncomment
(defmacro smart-comment (key-bind)
  `(lambda ()
     (local-set-key (kbd ,key-bind)
                    (lambda ()
                      (interactive)
                      (if (use-region-p)
                          (comment-or-uncomment-region
                           (region-beginning)
                           (region-end))
                        (insert ,key-bind))))))

(add-hooks (smart-comment ";")
           '(emacs-lisp-mode-hook
            lisp-mode-hook))

(add-hooks (smart-comment "/")
           '(c-mode-hook
             c++-mode-hook
             csharp-mode-hook
             js-mode-hook
             js2-mode-hook))

(add-hooks (smart-comment "#")
           '(python-mode-hook))

;; default comment key binding
(global-set-key (kbd "C-; c") 'comment-or-uncomment-region)

(cua-mode 1) ; cua-mode (Ctrl+C,V,X,Z)
(setq select-enable-clipboard t) ; allows to copy/paste text between emacs and other apps
(setq x-select-enable-clipboard t)

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

(defun cfg-theme ()
  (interactive)
  (find-file "~/.emacs.d/calx-theme.el"))

;; fix to conflict between cua rectangle mode and autopair (autopair overrides enter key (cua-rotate-rectangle))
;; just bind cua-rotate-rectangle to other keybind
(global-set-key (kbd "C-M-r") 'cua-rotate-rectangle)

;; file name in title bar
(defvar default-frame-title-format "emacs | %b")
(setq frame-title-format default-frame-title-format)
(setq icon-title-format frame-title-format)

;; macro start/end bound to F11/F12
(global-set-key [(f11)] 'kmacro-start-macro-or-insert-counter)
(global-set-key [(f12)] 'kmacro-end-or-call-macro)

;; no shitty spaces ... 13-10-2015 - hell has frozen over
(setq-default indent-tabs-mode nil)

;; tabs should be always interpreted as 4 spaces
(setq-default tab-width 4)

;; auto indenting current line when pressing <enter>
(electric-indent-mode t)

;; shows current function name in modeline
;; (which-function-mode)

;; delete trailing whitespace on save, also tabify buffer
(defun cleanup-before-save ()
  (whitespace-cleanup)
  (untabify (point-min) (point-max))
  (when (or (equal major-mode 'c++-mode)
            (equal major-mode 'c-mode)
            (equal major-mode 'csharp-mode)
            (equal major-mode 'js2-mode))
      (indent-for-tab-command)))

(add-hook 'before-save-hook 'cleanup-before-save)

;; automatically reload files when changed
(global-auto-revert-mode t)

;; org mode annoying shortcuts, disable line numbers (linum-mode)
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
                                 "C-S-<up>"))
            (linum-mode -1)))

(setq query-replace-show-replacement t)

;; returns total lines count in current buffer
(defun total-lines ()
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

;; no pasting for MMB
(global-unset-key (kbd "<mouse-2>"))

;; Shift + Space -> move cursor right skipping all whitespace
(defun move ()
  (interactive)
  (if (looking-at-p "[ \t\r\n]")
      (skip-chars-forward " \t\r\n")
    (forward-char 1)))
(global-set-key (kbd "S-SPC") 'move)

;; Shift + Backspace -> hungry delete
(defun hungry-delete ()
  (interactive)
  (save-excursion
    (let* ((begin (point))
           (end (progn
                  (when (= (skip-chars-backward " \r\n\t") 0)
                    (forward-char -1))
                  (point))))
      (delete-region begin end))))
(global-set-key (kbd "S-<backspace>") 'hungry-delete)

;; Shift + Ctrol + Space -> move back
(defun move-back ()
  (interactive)
  (when (= (skip-chars-backward " \r\n\t") 0)
    (forward-char -1)))
(global-set-key (kbd "M-SPC") 'move-back)

;; scratch buffer text
(setq initial-scratch-message (concat ";; Emacs .c41x" (make-string 20 ?\n)))

;; buffer switcher via popup.el
(defun list-visible-buffers ()
  (remove-if (lambda (e)
               (string= " " (substring (buffer-name e) 0 1)))
             (buffer-list)))

(defun popupize-buffer (element)
  (popup-make-item (buffer-name element) :value element))

(defvar last-file-isearch "")

(defun custom-isearch-filter (pattern list)
  (setq last-file-isearch pattern)
  (let ((filter (popup-isearch-filter-list pattern list)))
    (append (if (equal 'git-search (cdr (nth 4 (last filter))))
                (butlast filter)
              filter)
            (last list))))

(defun helm-inject-filter (args)
  (if (plist-member args :sources)
      (plist-put args :input last-file-isearch)
    args))

(defun switch-buffer-popup ()
  (interactive)
  (let ((popup (popup-menu*
                (append (mapcar 'popupize-buffer (list-visible-buffers))
                        (list (popup-make-item "Search in repository..." :value 'git-search)))
                :scroll-bar t
                :isearch t
                :isearch-filter 'custom-isearch-filter)))
    (if (equal popup 'git-search)
        (progn (advice-add 'helm :filter-args 'helm-inject-filter)
               (helm-ls-git-ls)
               (advice-remove 'helm 'helm-inject-filter))
      (switch-to-buffer popup))))

;; TODO: property list ?

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
(add-to-list 'ido-ignore-files "\\.meta")  ;; ignore unity .meta files
(setq ido-ignore-extensions t)

;; autocomplete
(setq-default auto-complete-mode nil)
(setq ac-ignore-case t)
(setq ac-use-fuzzy t)
(setq popup-isearch-cursor-color "orange")
(flyspell-mode 0) ;; speedup AC

;; helm
(require 'helm-config)
(set 'helm-idle-delay 0.0)
(set 'helm-input-idle-delay 0.0)
(require 'helm-git-grep)
(when (eq system-type 'windows-nt)
  (defun helm-git-submodule-grep-process ()))
(setq helm-git-grep-sources '(helm-source-git-grep)) ;; only search in git, not in submodules

;; force helm to use bottom of the screen and to not break window layout
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . nil)
               (window-height . 0.4)))
(setq helm-split-window-in-side-p t)
(setq helm-split-window-default-side 'below)

;; ace-jump mode for quick jumping around
(require 'ace-jump-mode)
(define-key global-map (kbd "C-\\") 'ace-jump-mode)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand region
(require 'expand-region)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-' x") 'kill-region)
(global-set-key (kbd "C-w") 'er/expand-region)

;; pretty lambda
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'nerd)
(setq neo-dont-be-alone t)
(setq neo-vc-integration (quote (face char)))
(setq neo-window-width 35)

;; highlight-symbol mode
(require 'highlight-symbol)
(add-hooks 'highlight-symbol-mode
           '(emacs-lisp-mode-hook
             lisp-mode-hook
             c-mode-hook
             c++-mode-hook
             csharp-mode-hook
             js-mode-hook
             js2-mode-hook
             python-mode-hook))
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(setq highlight-symbol-colors (quote ("yellow green" "firebrick" "cornflower blue" "MediumPurple1" "MistyRose" "turquoise" "IndianRed")))
(setq highlight-symbol-idle-delay 0.5)
(setq highlight-symbol-ignore-list (quote ("\\`[0-9]+f?F?[ulUL]*\\'")))

;; flycheck
(require 'flycheck)

;; company
(require 'company)
(setq company-idle-delay 0.0)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case (quote keep-prefix))
(setq company-irony-ignore-case t)

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

;; control + space = company (and enable company mode if not enabled)
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC")
                '(lambda ()
                   (interactive)
                   (unless company-mode
                     (message "enabling company-mode")
                     (setq-local company-backends '((company-dabbrev)))
                     (company-mode t))
                   (company-complete)))

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

;; this fixes hang-ups in windows 8 when opening/searching for files
(setq w32-get-true-file-attributes nil)

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

;; ;; highlight-symbol advice for ignoring numbers and symbols inside comments
;; (defadvice highlight-symbol-get-symbol (after highlight-ignore-symbols activate)
;;   (when (or (save-excursion
;;            (skip-chars-backward "0-9.A-Za-z")
;;            (looking-at "[0-9]+\\.?[ulULfF]*"))
;;          (nth 4 (syntax-ppss)))
;;     (setq ad-return-value nil)))

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

;; my hooks
(defvar-local moded-save-hook nil)
(defvar-local moded-kill-hook nil)

;; do not ask for kill compile process
(defadvice yes-or-no-p (around compilation-ignore-message activate)
  "Advice for `compile' to not ask for kill existing compilation."
  (if (string-match "A compilation process is running; kill it\\?" prompt)
      (setq ad-return-value nil)
    ad-do-it))

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

(add-hook 'c-mode-hook 'ac-ccc-mode-setup)
(add-hook 'c++-mode-hook 'ac-ccc-mode-setup)

;; GNU global tags
;; 1) install global (sudo apt-get install global)
;; 2) run gtags in project root
(require 'ggtags)
(defun ggtags-enable ()
  (ggtags-mode 1))
(add-hook 'c-mode-hook 'ggtags-enable)
(add-hook 'c++-mode-hook 'ggtags-enable)
(define-key ggtags-mode-map (kbd "M-]") nil) ;; unbind keybing conflict

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
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; irony
(add-hooks (lambda ()
             (setq-local company-backends '((company-irony :separate company-dabbrev)))
             (irony-mode t)
             (company-mode t)
             (flycheck-mode t)
             (flycheck-irony-setup))
           '(c++-mode-hook))

(setq-default company-irony-ignore-case t)
(setq-default irony-supported-major-modes '(c++-mode))
(setq-default w32-pipe-read-delay 0)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;//- CMake
;; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode)) auto-mode-alist))

(defun my-cmake-mode-hook ()
  (setq-local company-backends '((company-cmake :separate company-dabbrev)))
  (company-mode t))

(add-hook 'cmake-mode-hook 'my-cmake-mode-hook)

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
(add-hook 'csharp-mode-hook (lambda ()
                              (setq-local company-backends '((company-omnisharp :separate company-dabbrev)))
                              (company-mode t)
                              (omnisharp-mode)))
(add-hook 'csharp-mode-hook 'flycheck-mode)
(setq omnisharp-server-executable-path "e:/repo/omnisharp-roslyn/artifacts/publish/OmniSharp/default/net46/OmniSharp.exe")
(global-set-key (kbd "C-<f12>") 'omnisharp-go-to-definition)

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
(defun my-elisp-mode-hook ()
  ;; flycheck in LISP
  (flycheck-mode)

  ;; autocomplate
  (setq-local ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))
  (auto-complete-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;;//- MaxScript
(require 'maxscript-mode)
(add-to-list 'auto-mode-alist '("\\.ms$" . maxscript-mode))

;;//- PHP
;;(require 'php-mode)
(require 'web-mode)
(require 'ac-php)
(defun setup-php ()
  (web-mode)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-words-in-buffer ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))
          ("php" . (ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ac-source-dictionary
                    ac-source-php))))
  (flycheck-mode t)
  (flycheck-add-mode 'php 'web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . setup-php))
(add-to-list 'auto-mode-alist '("\\.html$" . setup-php))
(add-to-list 'auto-mode-alist '("\\.css$" . setup-php))

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
                          emacs-lisp-mode-hook lisp-mode-hook))
(global-set-key (kbd "M-]") 'next-page-break)
(global-set-key (kbd "M-[") 'prev-page-break)
(global-set-key (kbd "C-; x") 'page-breaks-popup)

;; C++ project utils
(require 'cpp-proj)

;; CEH
(add-to-list 'load-path "~/.emacs.d/ceh")
(require 'ceh)
(add-hooks 'ceh-mode
           '(cg-mode-hook
             c-mode-hook
             c++-mode-hook
             js-mode-hook
             js2-mode-hook
             csharp-mode-hook
             web-mode-hook))

;; API
(require 'calx-api)

;; Hacker News Reader
(add-to-list 'load-path "~/.emacs.d/hnr")
(require 'hnr)

;; boxy
(add-to-list 'load-path "~/.emacs.d/boxy")
(require 'boxy)

;; recall
(add-to-list 'load-path "~/.emacs.d/recall")
(require 'recall)
(add-hook 'change-major-mode-hook 'recall-mode)

;; PlayFab
(require 'playfab)

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

(defun moded--error-correct ()
  (save-excursion
    (backward-word)
    (when (or (looking-at "jf")
              (looking-at "fj"))
      (undo))))

(defvar moded-boxy t)

(if moded-boxy
    (defun moded-do ()
      (interactive)
      (moded--error-correct)
      (boxy-centered 40 '(" p - [project ...]"
                          " v - [version control ...]"
                          " k - [kill ...]"
                          " d - [compile ...]"
                          " w - [windows ...]"
                          " i - [insert ...]"
                          " b - [buffer ...]"
                          " h - [helm ...]"
                          " j - [ceh / code manipulation]"
                          " = - [online]"
                          " m - > move ..."
                          " c - > comment ..."
                          " f - find file"
                          " g - switch buffer"
                          " z - undo"
                          " s - save"
                          " r - recall"
                          " u - paste"
                          " o - switch to other buffer"
                          " x - page breaks navigation"
                          " ? - Zeal at point")
                     '(("p" . (lambda () (boxy-close) (boxy-centered 40 '(" i - initialize"
                                                                     " t - switch target"
                                                                     " c - configuration (Debug/Release)"
                                                                     " u - unload project"
                                                                     " l - install project (CMake only)"
                                                                     " g - generate CMake project"
                                                                     " r - regenerate CMake project"
                                                                     " q - quick C++ test program")
                                                                '(("i" . (lambda () (boxy-close) (or (switch-target) (vs-init) (vs-search))))
                                                                  ("t" . (lambda () (boxy-close) (switch-target)))
                                                                  ("c" . (lambda () (boxy-close) (if (vs-active) (vs-switch-configuration) (switch-configuration))))
                                                                  ("u" . (lambda () (boxy-close) (unload-project)))
                                                                  ("l" . (lambda () (boxy-close) (cmake-install)))
                                                                  ("g" . (lambda () (boxy-close) (generate-project)))
                                                                  ("r" . (lambda () (boxy-close) (cm-regenerate)))
                                                                  ("q" . (lambda () (boxy-close) (cpp-testfield-init)))))))
                       ("v" . (lambda () (boxy-close) (boxy-centered 40 '(" r - [root ...]"
                                                                     " d - directory status"
                                                                     " c - diff current file (=)"
                                                                     " v - commit current file"
                                                                     " u - revert current file"
                                                                     " l - print log")
                                                                '(("r" . (lambda () (boxy-close) (boxy-centered 40 '(" l - log")
                                                                                                           '(("l" . (lambda () (boxy-close) (vc-print-root-log)))))))
                                                                  ("d" . (lambda () (boxy-close) (call-interactively 'vc-dir)))
                                                                  ("=" . (lambda () (boxy-close) (vc-diff)))
                                                                  ("c" . (lambda () (boxy-close) (vc-diff)))
                                                                  ("v" . (lambda () (boxy-close) (call-interactively 'vc-next-action)))
                                                                  ("u" . (lambda () (boxy-close) (call-interactively 'vc-revert)))
                                                                  ("l" . (lambda () (boxy-close) (call-interactively 'vc-print-log)))))))
                       ("k" . (lambda () (boxy-close) (boxy-centered 40 '(" k - kill current buffer"
                                                                     " w - kill buffer and window"
                                                                     " c - kill compilation process")
                                                                '(("k" . (lambda () (boxy-close) (if moded-kill-hook (run-hooks 'moded-kill-hook) (kill-buffer))))
                                                                  ("w" . (lambda () (boxy-close) (kill-buffer-and-window)))
                                                                  ("c" . (lambda () (boxy-close) (kill-compilation)))))))
                       ("=" . (lambda () (boxy-close) (boxy-centered 40 '(" v - PlayFab: get current version"
                                                                     " u - PlayFab: upload file to playfab"
                                                                     " d - PlayFab: download latest version")
                                                                '(("v" . (lambda () (boxy-close) (playfab-get-revision)))
                                                                  ("u" . (lambda () (boxy-close) (playfab-update-cloudscript)))
                                                                  ("d" . (lambda () (boxy-close) (playfab-download-latest)))))))
                       ("d" . (lambda () (boxy-close) (boxy-centered 40 '(" d - compile (debug)"
                                                                     " r - compile (release)"
                                                                     " s - run (debug)")
                                                                '(("d" . (lambda () (boxy-close) (cm-compile-debug)))
                                                                  ("r" . (lambda () (boxy-close) (cm-compile-release)))
                                                                  ("s" . (lambda () (boxy-close) (cm-run-debug)))))))
                       ("w" . (lambda () (boxy-close) (boxy-centered 40 '(" w - delete window"
                                                                     " j - split horizontally"
                                                                     " f - split vertically")
                                                                '(("w" . (lambda () (boxy-close) (delete-window)))
                                                                  ("j" . (lambda () (boxy-close) (split-window-horizontally)))
                                                                  ("f" . (lambda () (boxy-close) (split-window-vertically)))))))
                       ("i" . (lambda () (boxy-close) (boxy-centered 40 '(" g - insert GUID")
                                                                '(("g" . (lambda () (boxy-close) (insert-guid)))))))
                       ("b" . (lambda () (boxy-close) (boxy-centered 40 '(" j - windmove left"
                                                                     " k - windmove down"
                                                                     " l - windmove right"
                                                                     " i - windmove up")
                                                                '(("j" . (lambda () (boxy-close) (windmove-left)))
                                                                  ("k" . (lambda () (boxy-close) (windmove-down)))
                                                                  ("l" . (lambda () (boxy-close) (windmove-right)))
                                                                  ("i" . (lambda () (boxy-close) (windmove-up)))))))
                       ("j" . (lambda () (boxy-close) (boxy-centered 40 '(" n - name selection as variable")
                                                                '(("n" . (lambda () (boxy-close) (name-selection)))))))
                       ("m" . (lambda () (boxy-close) (moded--rkl "> Move"
                                                             "g"
                                                             ("l" (smooth-scroll -1 8 0.1))
                                                             ("m" (smooth-scroll 1 8 0.1))
                                                             ("j" (backward-word))
                                                             ("k" (forward-word))
                                                             ("d" (forward-line -1))
                                                             ("f" (forward-line 1))
                                                             ("e" (backward-paragraph))
                                                             ("i" (forward-paragraph)))))
                       ("c" . (lambda () (boxy-close) (moded--rk "> Comment"
                                                            ("c" (comment-or-uncomment-region (region-beginning) (region-end)))
                                                            ("n" (moded--rk "> Comment > Next >"
                                                                            ("l" (comment-or-uncomment-region (region-beginning) (save-excursion (forward-line) (point))))
                                                                            ("b" (comment-or-uncomment-region (region-beginning) (save-excursion (forward-paragraph) (point))))))
                                                            ("b" (comment-or-uncomment-region (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))))
                                                            ("e" (ceh-comment-to-eol))
                                                            ("a" (ceh-comment-next-atom)))))
                       ("f" . (lambda () (boxy-close) (ido-find-file)))
                       ("g" . (lambda () (boxy-close) (switch-buffer-popup)))
                       ("z" . (lambda () (boxy-close) (undo)))
                       ("s" . (lambda () (boxy-close) (if moded-save-hook (run-hooks 'moded-save-hook) (save-buffer))))
                       ("r" . (lambda () (boxy-close) (recall)))
                       ("o" . (lambda () (boxy-close) (switch-to-buffer (other-buffer))))
                       ("x" . (lambda () (boxy-close) (page-breaks-popup)))
                       ("u" . (lambda () (boxy-close) (call-interactively 'cua-paste)))
                       ("h" . (lambda () (boxy-close) (boxy-centered 40 '(" g - Git"
                                                                     " b - bookmarks"
                                                                     " f - Grep find in Git")
                                                                '(("g" . (lambda () (boxy-close) (helm-ls-git-ls)))
                                                                  ("b" . (lambda () (boxy-close) (helm-bookmarks)))
                                                                  ("f" . (lambda () (boxy-close) (helm-git-grep)))))))
                       ("?" . (lambda () (boxy-close) (zeal-at-point))))))

  ;; old moded
  (defun moded-do ()
    (interactive)
    (moded--error-correct)
    (set-face-attribute 'mode-line nil :background "firebrick")
    (moded--rk ">"
               ("f" (ido-find-file))
               ("g" (ido-switch-buffer))
               ("j" (smex))
               ("z" (undo))
               ("s" (if moded-save-hook (run-hooks 'moded-save-hook) (save-buffer)))
               ("x" (page-breaks-popup))
               ("o" (switch-to-buffer (other-buffer)))
               ("m" (moded--rkl "> Move"
                                "g"
                                ("l" (smooth-scroll -1 8 0.1))
                                ("m" (smooth-scroll 1 8 0.1))
                                ("j" (backward-word))
                                ("k" (forward-word))
                                ("d" (forward-line -1))
                                ("f" (forward-line 1))
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
                               ("k" (if moded-kill-hook (run-hooks 'moded-kill-hook) (kill-buffer)))
                               ("w" (kill-buffer-and-window))
                               ("c" (kill-compilation))))
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
                               ("i" (or (switch-target) (vs-init) (vs-search)))
                               ("t" (switch-target))
                               ("c" (if (vs-active) (vs-switch-configuration) (switch-configuration)))
                               ("u" (unload-project))
                               ("l" (cmake-install))
                               ("g" (generate-project)))))
    (set-face-attribute 'mode-line nil :background "#225599")))

(key-chord-define-global "jf" 'moded-do)
(key-chord-define-global "fj" 'moded-do)
(key-chord-mode t)

;;//- local custom settings
(load "~/.emacs.d/local-config" t)

;; TODO: moded abort bug
;; TODO: toggling buffers
;; TODO: CMake project - fix searching for executable
;; TODO: uniform c++ project
;; TODO: recall: make recall.el package
;; TODO: recall: open closed buffers?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-php zeal-at-point ggtags omnisharp neotree flycheck-irony company-irony company s web-mode php-mode ac-geiser geiser fuzzy skewer-mode key-chord flycheck jedi highlight-symbol smex helm-ls-git helm-git-grep helm lua-mode js2-mode csharp-mode autopair pretty-lambdada cmake-mode glsl-mode ace-jump-mode multiple-cursors expand-region auto-complete yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

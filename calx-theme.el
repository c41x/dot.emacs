(deftheme calx
  "Created 2012-10-13. Last update 2014-08-07.")

; hints:
; to find out what face (and other info) is under cursor, jest press C-u C-x =

; custom faces
(defvar font-lock-operator-face 'font-lock-operator-face)
(defvar font-lock-number-face 'font-lock-number-face)
(defvar font-lock-over-80-face 'font-lock-over-80-face)
(defvar font-lock-page-break-face 'font-lock-page-break-face)
(defvar font-lock-todo-face 'font-lock-todo-face)

(defface font-lock-operator-face
  '((((class color)) :foreground "#99bde6")
    (t :inverse-video t))
  "Face to match operators (custom)." :group 'basic-faces)

(defface font-lock-over-80-face
  '((((class color)) :foreground "LightSkyBlue")
    (t :inverse-video t))
  "Face to match 80+ lines custom)." :group 'basic-faces)

(defface font-lock-number-face
  '((((class color)) :foreground "red")
    (t :inverse-video t))
  "Face to match numbers (custom)." :group 'basic-faces)

(defface font-lock-page-break-face
  '((((class color)) :background "MediumPurple3" :foreground "black")
    (t :inverse-video t))
  "Face to match page breaks (custom)." :group 'basic-faces)

(defface font-lock-todo-face
  '((((class color)) :background "goldenrod" :foreground "black")
    (t :inverse-video t))
  "Face to match page breaks (custom)." :group 'basic-faces)

(defface mode-line-2
  '((((class color)) :background "#222222" :foreground "#cc5522" :box nil))
  "Alternate mode line face." :group 'basic-faces)

(make-face 'mode-line-separator-face)
(set-face-attribute 'mode-line-separator-face nil
                    :foreground "antique white"
                    :weight 'bold)

(make-face 'mode-line-progress-face)
(set-face-attribute 'mode-line-progress-face nil
                    :foreground "antique white")

(make-face 'mode-line-error-face-bg)
(set-face-attribute 'mode-line-error-face-bg nil
                    :foreground "#881100")

(make-face 'mode-line-error-face)
(set-face-attribute 'mode-line-error-face nil
                    :foreground "#ff2200")

(make-face 'mode-line-bg-face)
(set-face-attribute 'mode-line-bg-face nil
                    :foreground "black")

;;(set-face-attribute 'log-view-message-face nil
;;                  :foreground "#aa7733"
;;                  :weight 'bold)

(defvar font-default '(default ((t (:inherit nil :background "#000000" :foreground "#b2af99" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Liberation Mono")))))
(when (string= system-type "windows-nt")
  (setq font-default '(default ((t (:inherit nil :background "#000000" :foreground "#b2af99" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Liberation Mono"))))))

(custom-theme-set-faces
 'calx
 font-default
 '(cursor ((t (:background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#dc7e28"))))
 '(minibuffer-prompt ((t (:foreground "#dc7e28"))))
 '(highlight ((t (:inherit: nil :foreground "gold" :background "black" :underline t))))
 '(region ((t (:background "#333333"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:foreground "#f6f3e8" :background "#333366"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#649cd8"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#515850"))))
 '(font-lock-constant-face ((t (:foreground "#649cd8"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#b2af99"))))
 '(font-lock-keyword-face ((t (:foreground "#649cd8"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "#ebe05c"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#66aa33"))));dc7e28
 '(font-lock-type-face ((t (:foreground "#649cd8"))))
 '(font-lock-variable-name-face ((t (:foreground "#b2af99"))))
 '(font-lock-warning-face ((t (:foreground "#ccaa8f" :inherit (error)))))
 '(show-paren-match-face ((t (:foreground "yellow" :background "black"))))
 '(button ((t (:foreground "#f6f3e8" :background "#333333" :inherit (link)))))
 '(link ((t (:underline t :foreground "#8ac6f2"))))
 '(link-visited ((t (:underline t :foreground "#e5786d" :inherit (link)))))
 '(header-line ((t (:box nil :foreground "#e7f6da" :background "#303030" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow" :inherit (variable-pitch)))))
 '(isearch ((t (:foreground "#857b6f" :background "#343434"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:foreground "#a0a8b0" :background "#384048"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(compilation-error ((t (:foreground "orange red" :underline nil))))
 '(compilation-info ((t (:foreground "light slate blue"))))
 '(compilation-warning ((t (:foreground "green yellow" :underline nil))))
 '(diff-added ((t (:background "dark green"))))
 '(diff-removed ((t (:background "firebrick4"))))
 '(dired-directory ((t (:foreground "#eeaa11"))))
 '(fringe ((t (:background "black" :foreground "#39ae1c"))))
 '(linum ((t (:background "black" :foreground "#343434"))))
 '(mode-line ((t (:background "#225599" :foreground "#aaaaaa" :box nil))))
 '(mode-line-2 ((t (:background "#222222" :foreground "#cc5522" :box nil))))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit mode-line :background "#222222" :foreground "#857b6f" :box nil :weight light))))
 '(popup-face ((t (:background "AntiqueWhite3" :foreground "black"))))
 '(popup-menu-selection-face ((t (:background "chartreuse4" :foreground "white"))))
 '(popup-isearch-match ((t (:background "orange red" :foreground "black"))))
 '(popup-menu-mouse-face ((t (:background "firebrick" :foreground "black"))))
 '(show-paren-match ((t (:background "#44aaee" :foreground "#000000"))))
 '(show-paren-mismatch ((t (:background "#aa2211"))))
 '(sml/filename ((t (:inherit sml/global :foreground "lemon chiffon"))))
 '(warning ((t (:foreground "DarkOrange"))))
 '(which-func ((t (:foreground "moccasin"))))
 '(flymake-errline ((((class color)) (:background "#772200"))))
 '(flymake-warnline ((((class color)) (:underline "#aaff33"))))
 '(font-lock-number-face ((t (:foreground "#dd5522")))) ;66aa33
 '(font-lock-operator-face ((t (:foreground "#99bde6"))))
 '(font-lock-over-80-face ((t (:foreground "#ff2211"))))
 '(highlight-symbol-face ((t (:background "gray25"))))
 '(neo-file-link-face ((t (:foreground "seashell4"))))
 '(neo-vc-default-face ((t (:foreground "#b2af99"))))
 '(neo-vc-up-to-date-face ((t (:foreground "#b2af99"))))
 '(company-preview ((t (:background "gray20" :foreground "wheat"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "OliveDrab3"))))
 '(company-preview-search ((t (:inherit company-preview :background "dodger blue"))))
 '(company-scrollbar-bg ((t (:background "gray15"))))
 '(company-scrollbar-fg ((t (:background "orange3"))))
 '(company-tooltip ((t (:background "gray15" :foreground "orange1"))))
 '(company-tooltip-annotation ((t (:foreground "DarkOliveGreen3"))))
 '(company-tooltip-common ((t (:foreground "orange2"))))
 '(company-tooltip-selection ((t (:background "gray30"))))
 '(vertical-border ((t (:foreground "#222222")))))

(provide-theme 'calx)

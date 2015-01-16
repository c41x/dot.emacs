;;; cmake-cpp-proj --- CMake C++ project support
;;; Commentary:
;;; Code:

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

(provide 'cmake-cpp-proj)
;;; cmake-cpp-proj.el ends here

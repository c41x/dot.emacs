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

(defun dirs-contains-file (name dir)
  "Searches recursively for given file"
  (let ((res nil))
    (dolist (file (directory-files dir))
      (when (string= name file)
	(setq res (cons (file-name-as-directory dir) res)))
      (when (and
	     (file-directory-p (concat (file-name-as-directory dir) file))
	     (not (or
		   (string= file ".")
		   (string= file ".."))))
	(let ((subdir-result (dirs-contains-file name (concat (file-name-as-directory dir) file))))
	  (when subdir-result
	    (setq res (append subdir-result res))))))
    res))

(defun upward-check-file (filename startdir)
  "Moves up in directory structure and checks if desired file is there, returns last found"
  (let ((dirname (expand-file-name startdir))
	(top nil)
	(max-level 10)
	(prv-dirname nil)
	(last-found nil))
    (while (not (or top (= max-level 0)))
      (setq max-level (- max-level 1))
      (if (string= (expand-file-name dirname) "/")
	  (setq top t))
      (if (file-exists-p (expand-file-name filename dirname))
	  (setq last-found dirname))
      (setq prv-dirname dirname)
      (setq dirname (expand-file-name ".." dirname)))
    last-found))

(defun find-project-directory-base (project-dir)
  "Returns CMake project root directory or nil"
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

(defmacro find-in-file-regex (file-name regex)
  "in-file search builder"
  `(let ((i 0)
	 (matches '())
	 (file-buffer (get-string-from-file ,file-name)))
     (save-match-data
       (while (string-match ,regex file-buffer i)
	 (setq i (match-end 1))
	 (add-to-list 'matches (match-string-no-properties 1 file-buffer))))
     matches))

(defun extract-targets-from-file (file-name)
  "searches for CMake targets in specified file"
  (append (find-in-file-regex file-name "add_executable(\\(\\w+\\)")
	  (find-in-file-regex file-name "add_library(\\(\\w+\\)")))

(defun find-all-targets ()
  "returns lists with all targets available in current CMake project"
  (let ((res '()))
    (dolist (f (dirs-contains-file "CMakeLists.txt" (find-project-directory)))
      (setq res (append res (extract-targets-from-file (concat f "CMakeLists.txt")))))
    res))

(defvar selected-target nil)
(defun popup-get-target ()
  "displays popup and returns selected target name"
  (interactive)
  (let ((all-targets (find-all-targets)))
    ;; for some reason popup-menu* does not work within let - hence global var
    (setq selected-target (popup-menu* all-targets))
    (if (< (length all-targets) 2)
	"ALL_BUILD"
      selected-target)))

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

(defvar current-target-name nil)
(defvar current-dir-release nil)
(defvar current-dir-debug nil)
(defvar current-executable-debug nil)
(defvar current-executable-release nil)

(defun run-compile (release clean &optional target-name)
  (unless current-target-name
    (setq current-target-name (popup-get-target)))
  (unless current-dir-debug
    (setq current-dir-debug (find-project-directory-debug)))
  (unless current-dir-release
    (setq current-dir-release (find-project-directory-release)))
  (compile (format "cmake --build \"%s\" --config %s --target %s %s"
		   (if release current-dir-release current-dir-debug)
		   (if release "Release" "Debug")
		   (if target-name target-name current-target-name)
		   (if clean "--clean-first" ""))))

(defun switch-target ()
  (interactive)
  (setq current-target-name (popup-get-target))
  (setq current-dir-debug (find-project-directory-debug))
  (setq current-dir-release (find-project-directory-release)))

;;;
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

;;(defmacro run-compile (dir fallback-dir release)
;;  `(let ((dir (,dir)))
;;     (unless ,fallback-dir
;;       (setq ,fallback-dir dir)
;;       (actualize-path-cache))
;;     (compile (format
;; 	       "cmake --build \"%s\" --config %s"
;; 	       ,fallback-dir (if ,release "Release" "Debug")))))

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
    (run-compile nil nil)))

(global-set-key
 (kbd "S-<f7>")
 '(lambda ()
    (interactive)
    (run-compile t nil)))

(global-set-key
 (kbd "C-<f7>")
 '(lambda ()
    (interactive)
    (run-compile nil nil "ALL_BUILD"))) ; compile full project

(global-set-key
 (kbd "C-S-<f7>")
 '(lambda ()
    (interactive)
    (run-compile t nil "ALL_BUILD"))) ; compile full project

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

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
  (when (file-exists-p dir)
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
      res)))

(defun search-file (name dir)
  "find file in directory recursively, returns first found"
  (car (dirs-contains-file name dir)))

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
  (append (find-in-file-regex file-name "add_executable(\\([A-Za-z_]*\\)")
	  (find-in-file-regex file-name "add_library(\\([A-Za-z_]*\\)")))

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
    (setq selected-target (popup-menu* all-targets :scroll-bar t :isearch t))
    (if (< (length all-targets) 2)
	(cons selected-target t)
      (cons selected-target nil))))

(defun find-project-directory-debug ()
  (find-project-directory-base "project/debug"))
(defun find-project-directory-release ()
  (find-project-directory-base "project/release"))
(defun find-project-directory ()
  (find-project-directory-base ""))

(defvar current-target-name nil)
(defvar current-target-all nil)
(defvar current-dir-release nil)
(defvar current-dir-debug nil)
(defvar current-executable-debug nil)
(defvar current-executable-release nil)
(defvar current-target-release nil)

(defun exec-name (file-name)
  (if (string= system-type "windows-nt")
      (format "%s.exe" file-name)
    file-name))

(defun refresh-mode-line ()
  (setq mode-line-project (concat "CMake:" current-target-name " / "
				  (if current-target-release "Release" "Debug")))
  (setq frame-title-format mode-line-project)
  (setq icon-title-format mode-line-project))

(defun refresh-target-name ()
  (let ((popup-result (popup-get-target)))
    (setq current-target-name (car popup-result))
    (setq current-target-all (cdr popup-result))
    (refresh-mode-line)))

(defun refresh-dir-release ()
  (setq current-dir-release (find-project-directory-release)))
(defun refresh-dir-debug ()
  (setq current-dir-debug (find-project-directory-debug)))
(defun refresh-executable-debug ()
  (setq current-executable-debug
	(search-file
	 (exec-name current-target-name)
	 current-dir-debug)))
(defun refresh-executable-release ()
  (setq current-executable-release
	(search-file
	 (exec-name current-target-name)
	 current-dir-release)))

(defmacro refresh-unset (flag refresh-function)
  `(unless ,flag
     (,refresh-function)))

(defun run-compile (release clean)
  (refresh-unset current-target-name refresh-target-name)
  (refresh-unset current-dir-debug refresh-dir-debug)
  (refresh-unset current-dir-release refresh-dir-release)
  (compile (format "cmake --build \"%s\" --config %s %s %s"
		   (if release current-dir-release current-dir-debug)
		   (if release "Release" "Debug")
		   (if current-target-all
		       ""
		     (concat "--target " current-target-name))
		   (if clean "--clean-first" ""))))

(defun run-exec (release)
  (refresh-unset current-executable-debug refresh-executable-debug)
  (refresh-unset current-executable-release refresh-executable-release)
  (compile (concat (if release
		       current-executable-release
		     current-executable-debug)
		   (exec-name current-target-name)) t))

(defun run-debug ()
  (refresh-unset current-executable-debug refresh-executable-debug)
  (gdb (format "gdb -i=mi %s" (concat current-executable-debug
				      (exec-name current-target-name)))))

(defun enable-visual-line-mode ()
  (select-window (get-buffer-window "*compilation*"))
  (end-of-buffer)
  (visual-line-mode t))

(defun switch-target ()
  (interactive)
  (refresh-target-name)
  (refresh-dir-debug)
  (refresh-dir-release)
  (setq current-executable-debug nil)
  (setq current-executable-release nil)
  current-target-name)

(defun switch-configuration ()
  (interactive)
  (if (string= "debug" (popup-menu* '("debug" "release") :scroll-bar t :isearch t))
      (setq current-target-release nil)
    (setq current-target-release t))
  (refresh-mode-line))

;;//- Visual Studio project support
(defun find-file-upwards (match)
  (let ((i 0)
	(ret nil)
	(dir "."))
    (while (and (< i 5) (not ret))
      (setq ret (directory-files dir nil match))
      (unless ret (setq dir (expand-file-name ".." dir)))
      (setq i (+ i 1)))
    (if ret (cons dir (car ret)) ret)))

(defvar vs-solution "")
(defvar vs-solution-name "")
(defvar vs-binary-debug "")
(defvar vs-binary-release "")
(defvar vs-release nil) ;; current active target type

(defun vs-active ()
  (interactive)
  (not (string= "" vs-solution)))

(defun vs--refresh-mode-line ()
  (setq mode-line-project (concat "VS:" vs-solution-name (if vs-release " / Release" " / Debug")))
  (setq frame-title-format mode-line-project)
  (setq icon-title-format mode-line-project))

(defun vs-init ()
  (interactive)
  (let ((f (find-file-upwards ".sln")))
    (when f
      (setq vs-solution (concat (car f) "/" (cdr f)))
      (setq vs-binary-release (concat (car f) "/bin/" (file-name-base (cdr f)) ".exe"))
      (setq vs-binary-debug (concat (car f) "/bin/" (file-name-base (cdr f)) "_debug.exe"))
      (setq vs-solution-name (cdr f))
      (global-set-key (kbd "<f7>") 'vs-compile)
      ;;(global-set-key (kbd "S-<f7>") 'vs-compile-release)
      (global-set-key (kbd "<f6>") 'vs-run)
      ;;(global-set-key (kbd "S-<f6>") 'vs-run-release)
      (vs--refresh-mode-line))))

(defun vs--compile (configuration)
  (if (string= "" vs-solution)
      (vs-init))
  (compile (concat "MSBuild.exe \"" vs-solution "\" /nologo /verbosity:m /property:Configuration=" configuration)))

(defun vs-compile-debug ()
  (interactive)
  (vs--compile "Debug")
  (enable-visual-line-mode))

(defun vs-compile-release ()
  (interactive)
  (vs--compile "Release")
  (enable-visual-line-mode))

(defun vs-compile ()
  (interactive)
  (vs--compile (if vs-release "Release" "Debug"))
  (enable-visual-line-mode))

(defun vs-run-debug ()
  (interactive)
  (compile vs-binary-debug t)
  (enable-visual-line-mode))

(defun vs-run-release ()
  (interactive)
  (compile vs-binary-release t)
  (enable-visual-line-mode))

(defun vs-run ()
  (interactive)
  (compile (if vs-release vs-binary-release vs-binary-debug) t)
  (enable-visual-line-mode))

(defun vs-switch-configuration ()
  (interactive)
  (if (string= "debug" (popup-menu* '("debug" "release") :scroll-bar t :isearch t))
      (setq vs-release nil)
    (setq vs-release t))
  (vs--refresh-mode-line))

;;//- key bindings
(defun cm-compile ()
  (interactive)
  (run-compile current-target-release nil)
  (enable-visual-line-mode))

(defun cm-run ()
  (interactive)
  (run-exec current-target-release)
  (select-window (get-buffer-window "*compilation*"))
  (goto-char (point-max)))

(defun cm-compile-clean ()
  (interactive)
  (run-compile current-target-release t)
  (enable-visual-line-mode))


(defun cm-compile-debug ()
  (interactive)
  (run-compile nil nil)
  (enable-visual-line-mode))

(defun cm-compile-release ()
  (interactive)
  (run-compile t nil)
  (enable-visual-line-mode))

(defun cm-compile-debug-clean ()
  (interactive)
  (run-compile nil t)
  (enable-visual-line-mode))

(defun cm-compile-release-clean ()
  (interactive)
  (run-compile t t)
  (enable-visual-line-mode))

(defun cm-run-debug ()
  (interactive)
  (run-exec nil)
  (select-window (get-buffer-window "*compilation*"))
  (goto-char (point-max)))

(defun cm-run-release ()
  (interactive)
  (run-exec t)
  (select-window (get-buffer-window "*compilation*"))
  (goto-char (point-max)))

(defun cm-debug ()
  (interactive)
  (frame-configuration-to-register 1)
  (run-debug)
  (setq gdb-many-windows t))

(defun cm-restore-debug ()
  (interactive)
  (setq gdb-many-windows nil)
  (jump-to-register 1))

(global-set-key (kbd "<f7>") 'cm-compile)
;;(global-set-key (kbd "S-<f7>") 'cm-compile-release)
(global-set-key (kbd "C-<f7>") 'cm-compile-clean)
;;(global-set-key (kbd "C-S-<f7>") 'cm-compile-release-clean)
(global-set-key (kbd "<f6>") 'cm-run)
;;(global-set-key (kbd "S-<f6>") 'cm-run-release)
(global-set-key (kbd "<f5>") 'cm-debug)
(global-set-key (kbd "S-<f5>") 'cm-restore-debug)
(global-set-key (kbd "<f9>") 'gud-break) ; toggle breakpoint
(global-set-key (kbd "<left-margin> <mouse-1>") 'gud-break)
(global-set-key (kbd "<f10>") 'gud-next) ; next statement
(global-set-key (kbd "C-' t") 'switch-target)

;;//- flycheck for CMake project
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

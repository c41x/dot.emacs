;;; cpp-proj --- CMake / Visual Studio C++ project support
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
  (when (and dir (file-exists-p dir))
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
    (when (> (length all-targets) 0)
      (setq selected-target (popup-menu* all-targets :scroll-bar t :isearch t))
      (if (< (length all-targets) 2)
          (cons selected-target t)
        (cons selected-target nil)))))

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
  (save-some-buffers t)
  (compile (format "cmake --build \"%s\" --config %s %s %s"
                   (if release current-dir-release current-dir-debug)
                   (if release "Release" "Debug")
                   (if current-target-all
                       ""
                     (concat "--target " current-target-name))
                   (if clean "--clean-first" ""))))

(defun cmake-regenerate (release)
  (save-some-buffers t)
  (compile (format "cmake %s" (if release current-dir-release current-dir-debug))))

(defun cmake-install ()
  (interactive)
  (let ((current-target-name "install"))
    (run-compile t nil)))

(defun run-exec (release)
  (refresh-unset current-executable-debug refresh-executable-debug)
  (refresh-unset current-executable-release refresh-executable-release)
  (save-some-buffers t)
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

(defun setup-irony-flycheck-for-cmake ()
  (when (is-cmake-project)
    (irony-mode nil)
    (company-mode nil)
    (flycheck-mode nil)
    (irony-cdb-json-add-compile-commands-path
     (find-project-directory)
     (concat (if current-target-release current-dir-release current-dir-debug)
             "/compile_commands.json"))
    (setq-local company-backends '((company-irony :separate company-dabbrev)))
    (irony-mode t)
    (company-mode t)
    (flycheck-mode t)))

(defun switch-target ()
  (interactive)
  (refresh-target-name)
  (refresh-dir-debug)
  (refresh-dir-release)
  (setq current-executable-debug nil)
  (setq current-executable-release nil)
  (setup-irony-flycheck-for-cmake)
  current-target-name)

(defun switch-configuration ()
  (interactive)
  (if (string= "debug" (popup-menu* '("debug" "release") :scroll-bar t :isearch t))
      (setq current-target-release nil)
    (setq current-target-release t))
  (setup-irony-flycheck-for-cmake)
  (refresh-mode-line))

(defun unload-project ()
  (setq current-target-name nil)
  (setq current-target-all nil)
  (setq current-dir-release nil)
  (setq current-dir-debug nil)
  (setq current-executable-debug nil)
  (setq current-executable-release nil)
  (setq current-target-release nil)
  (setq vs-solution "")
  (setq vs-solution-name "")
  (setq vs-binary-debug "")
  (setq vs-binary-release "")
  (setq vs-release nil)
  (makunbound 'mode-line-project)
  (setq frame-title-format default-frame-title-format)
  (setq icon-title-format frame-title-format))

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

(defun vs-init (&rest path-to-sln)
  (interactive)
  (let ((f (or path-to-sln (find-file-upwards ".sln"))))
    (if f
        (progn
          (setq vs-solution (concat (car f) "/" (cdr f)))
          (setq vs-binary-release (concat (car f) "/bin/" (file-name-base (cdr f)) ".exe"))
          (setq vs-binary-debug (concat (car f) "/bin/" (file-name-base (cdr f)) "_debug.exe"))
          (setq vs-solution-name (cdr f))
          (global-set-key (kbd "<f7>") 'vs-compile)
          ;;(global-set-key (kbd "S-<f7>") 'vs-compile-release)
          (global-set-key (kbd "<f6>") 'vs-run)
          ;;(global-set-key (kbd "S-<f6>") 'vs-run-release)
          (vs--refresh-mode-line))
      ;; load project file
      (let ((prj (find-file-upwards ".ep")))
        (when prj
          (load (concat (car prj) "/" (cdr prj)))
          (vs--refresh-mode-line)
          (global-set-key (kbd "<f7>") 'vs-compile)
          (global-set-key (kbd "<f6>") 'vs-run))))))

(defun vs--compile (configuration)
  (if (string= "" vs-solution)
      (vs-init))
  (save-some-buffers t)
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
  (save-some-buffers t)
  (compile vs-binary-debug t)
  (enable-visual-line-mode))

(defun vs-run-release ()
  (interactive)
  (save-some-buffers t)
  (compile vs-binary-release t)
  (enable-visual-line-mode))

(defun vs-run ()
  (interactive)
  (save-some-buffers t)
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

(defun cm-regenerate-debug ()
  (interactive)
  (cmake-regenerate nil))

(defun cm-regenerate-release ()
  (interactive)
  (cmake-regenerate t))

(defun cm-regenerate ()
  (interactive)
  (cmake-regenerate current-target-release))

(defun cm-debug ()
  (interactive)
  (frameset-to-register 1)
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

(defun extract-options-from-file (file-name)
  "Extracts CMake Options from given file"
  (let ((i 0) (matches '()) (file-buffer (get-string-from-file file-name)))
    (save-match-data
      (while (string-match "option(\\([^ ]*\\) " file-buffer i)
        (setq i (match-end 1))
        (add-to-list 'matches (match-string-no-properties 1 file-buffer))))
    matches))

(defun get-current-project-include-list ()
  (let ((proj-dir (find-project-directory)))
    (if proj-dir
        (extract-includes-from-file (concat proj-dir "CMakeLists.txt") proj-dir)
      nil)))

;; (when (not (string= system-type "windows-nt"))
;;   (add-hook 'c++-mode-hook
;;          (lambda ()
;;            (when (is-cmake-project)
;;              (setq flycheck-c/c++-gcc-executable (if (string= system-type "windows-nt") "mingw32-gcc" "gcc"))
;;              (setq flycheck-gcc-include-path (get-current-project-include-list))
;;              (setq flycheck-idle-change-delay 5.0)
;;              (setq flycheck-gcc-args '("-std=c++1y" "-msse" "-msse2" "-msse3"))
;;              (flycheck-mode)
;;              (flycheck-select-checker 'c/c++-gcc)))))

;;//- CMake project generator
(defun generate-project ()
  (interactive)
  (if (is-cmake-project)
      (let* ((root (find-project-directory))
             (configuration (if root
                                (popup-menu* '("Debug"
                                               "Release"
                                               "RelWithDebInfo"
                                               "MinSizeRel") :scroll-bar t :isearch t) nil))
             (generator (if configuration
                            (popup-menu* '("MinGW Makefiles"
                                           "Unix Makefiles"
                                           "MSYS Makefiles"
                                           "NMake Makefiles"
                                           "Ninja"
                                           "Visual Studio 10 2010"
                                           "Visual Studio 11 2012"
                                           "Visual Studio 12 2013"
                                           "Visual Studio 14 2015 Win64"
                                           "CodeBlocks - MinGW Makefiles"
                                           "Eclipse CDT4 - MinGW Makefiles"
                                           "KDevelop3") :scroll-bar t :isearch t) nil))
             (project-dir (if generator
                              (cond
                               ((string= configuration "Debug")
                                (concat root "project/debug"))
                               ((string= configuration "Release")
                                (concat root "project/release"))
                               ((string= configuration "RelWithDebInfo")
                                (concat root "project/relWithDebInfo"))
                               ((string= configuration "MinSizeRel")
                                (concat root "project/minSizeRel"))
                               (t nil)) nil))
             (already-generated (and project-dir (file-exists-p project-dir))))
        ;;(if already-generated
        ;; (message "CMake project with choosen configuration already exists")
        (message (format "Generating project for: %s | %s | %s" project-dir generator configuration))
        (mkdir project-dir t)
        (compile (format "cd \"%s\" && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=%s -G\"%s\" ../.."
                         project-dir configuration generator)))
    (message "Not a CMake project")))

;; example Visual Studio project file:
;; (setq vs-solution "c:/repo/pro/vc2013/pro.sln")
;; (setq vs-solution-name "pro.sln")
;; (setq vs-binary-debug "c:/repo/pro/vc2013/x64/Debug/pro.exe")
;; (setq vs-binary-release "c:/repo/pro/vc2013/x64/Release/pro.exe")
;; (setq vs-release nil)

;; quick testfield for C++
(defun cpp-testfield-init ()
  (interactive)
  (switch-to-buffer (get-buffer-create "C++Tesftield.cpp"))
  (write-file "/tmp/C++Testfield.cpp")
  (local-set-key (kbd "<f6>") 'cpp-testfield-compile-run))

(defun cpp-testfield-compile-run ()
  (interactive)
  (save-some-buffers t)
  (compile "g++ -std=c++1z -o /tmp/C++Testfield /tmp/C++Testfield.cpp && /tmp/C++Testfield" t))

;;// TODO: CMake OPTION support
;;// TODO: Project generation:
;; - scan for project type, if not found -> manual config (like above), finish
;; - display project type to confirm init
;; - if VS project, scan for solution and executable paths, finish, or if manual config is needed - display dialog
;; - create build directory for cmake oos build
;; - ask for options
;; - run cmake generator (asking which one (Unix Makefiles, VSxx, etc.))

(provide 'cpp-proj)
;;; cpp-proj.el ends here

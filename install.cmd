@echo off
git clone https://github.com/c41x/dot.emacs.git
move dot.emacs .emacs.d
git clone https://github.com/c41x/ceh.el.git
move ceh.el .emacs.d/ceh
git clone https://github.com/c41x/hnr.el.git
move hnr.el .emacs.d/hnr
set currentpath=%~dp0
setx HOME %currentpath%
pause
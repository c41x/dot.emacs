#!/bin/bash

read -n1 -p "Set remote to SSH? [y,n]" doit
case $doit in
    y|Y) cd ~
	 git clone git@github.com:c41x/dot.emacs.git
	 mv dot.emacs .emacs.d
	 git clone git@github.com:c41x/ceh.el.git
	 mv ceh.el .emacs.d/ceh
	 git clone git@github.com:c41x/hnr.el.git
	 mv hnr.el .emacs.d/hnr
	 git clone git@github.com:c41x/boxy.el.git
	 mv boxy.el .emacs.d/boxy
	 ;;
    n|N) cd ~
	git clone https://github.com/c41x/dot.emacs.git
	mv dot.emacs .emacs.d
	git clone https://github.com/c41x/ceh.el.git
	mv ceh.el .emacs.d/ceh
	git clone https://github.com/c41x/hnr.el.git
	mv hnr.el .emacs.d/hnr
	git clone https://github.com/c41x/boxy.el.git
	mv boxy.el .emacs.d/boxy
	;;
    *) echo aborting ;;
esac

rm -f install.sh

#!/bin/bash

read -n1 -p "Set remote to SSH? [y,n]" doit
case $doit in
    y|Y) cd ~
         git clone git@github.com:c41x/dot.emacs.git
         git clone git@github.com:c41x/ceh.el.git
         git clone git@github.com:c41x/hnr.el.git
         git clone git@github.com:c41x/boxy.el.git
         git clone git@github.com:c41x/recall.el.git
         ;;
    n|N) cd ~
        git clone https://github.com/c41x/dot.emacs.git
        git clone https://github.com/c41x/ceh.el.git
        git clone https://github.com/c41x/hnr.el.git
        git clone https://github.com/c41x/boxy.el.git
        git clone https://github.com/c41x/recall.el.git
        ;;
    *) echo aborting ;;
esac

mv dot.emacs/* .emacs.d
mv ceh.el .emacs.d/ceh
mv hnr.el .emacs.d/hnr
mv boxy.el .emacs.d/boxy
mv recall.el .emacs.d/recall

rm -f install.sh

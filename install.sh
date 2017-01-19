#!/bin/bash

read -n1 -p "Set remote to SSH? [y,n]" doit
case $doit in
    y|Y) cd ~
         git clone git@github.com:c41x/dot.emacs.git .emacs.d
         cd .emacs.d
         git clone git@github.com:c41x/ceh.el.git ceh
         git clone git@github.com:c41x/hnr.el.git hnr
         git clone git@github.com:c41x/boxy.el.git boxy
         git clone git@github.com:c41x/recall.el.git recall
         ;;
    n|N) cd ~
        git clone https://github.com/c41x/dot.emacs.git .emacs.d
        cd .emacs.d
        git clone https://github.com/c41x/ceh.el.git ceh
        git clone https://github.com/c41x/hnr.el.git hnr
        git clone https://github.com/c41x/boxy.el.git boxy
        git clone https://github.com/c41x/recall.el.git recall
        ;;
    *) echo aborting ;;
esac


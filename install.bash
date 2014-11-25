#!/bin/bash

mkdir -p ~/dev/dotemacs
cd ~/dev/dotemacs

if [ -d .git ]; then
    echo "dotemacs already cloned, run 'cd ~/dev/dotemacs && git pull' to update"
else
    git clone git@github.com:jplindstrom/dotemacs.git .
fi

mkdir ~/.emacs.d                     2>> /dev/null
ln -s ~/dev/dotemacs ~/elisp         2>> /dev/null
ln -s ~/dev/dotemacs/.emacs ~/.emacs 2>> /dev/null
ln -s ~/elisp/elpa ~/.emacs.d/elpa   2>> /dev/null

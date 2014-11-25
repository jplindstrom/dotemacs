#!/bin/bash

REPO=git@github.com:jplindstrom/dotemacs.git
if [ ! -z "$1" ]; then
    REPO=https://github.com/jplindstrom/dotemacs.git
fi

mkdir -p ~/dev/dotemacs
cd ~/dev/dotemacs

if [ -d .git ]; then
    echo "dotemacs already cloned, run 'cd ~/dev/dotemacs && git pull' to update"
else
    echo "Cloning $REPO"
    git clone $REPO .
fi

mkdir ~/.emacs.d                     2>> /dev/null
ln -s ~/dev/dotemacs ~/elisp         2>> /dev/null

if [ -e ~/.emacs ]; then
    echo "~/.emacs exists, run 'ln -s ~/dev/dotemacs/.emacs ~/.emacs' to overwrite it"
else
    ln -s ~/dev/dotemacs/.emacs ~/.emacs 2>> /dev/null
fi

ln -s ~/elisp/elpa ~/.emacs.d/elpa   2>> /dev/null

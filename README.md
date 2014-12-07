dotemacs
========

.emacs and libs

## Install git

    sudo aptitude update

    sudo aptitude -y install git
    git config --global user.email my@email.com
    git config --global user.name Johan Lindstrom


## Install emacs config

Assuming this is checked out in ~/dev/dotemacs

    mkdir ~/.emacs.d
    ln -s ~/dev/dotemacs ~/elisp
    ln -s ~/dev/dotemacs/.emacs ~/.emacs
    ln -s ~/elisp/elpa ~/.emacs.d/elpa

or

    wget -q -O- https://raw.githubusercontent.com/jplindstrom/dotemacs/master/install.bash | bash


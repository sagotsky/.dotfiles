#!/bin/sh

# Creates links to dotfiles in this git repo

find $HOME/.dotfiles/ -maxdepth 1 -not -name .git* -not -name dotlinks.sh -not -name README |\
    xargs -n1 ln -vs -t $HOME/

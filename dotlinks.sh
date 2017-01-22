#!/bin/sh

# Creates links to dotfiles in this git repo

#home
find $HOME/.dotfiles/home -mindepth 1 -maxdepth 1  |
    xargs -n1 ln -vsf -t $HOME/

# nested dirs
for dir in config ssh ; do
  mkdir -p "~/.$dir"
  find "$HOME/.dotfiles/$dir" -mindepth 1  -maxdepth 1  | while read subdir ; do
    ln -vsf $subdir "${HOME}/.${dir}/${subdir##*/}"
  done
done



#!/bin/bash

# Provides a status icon for xmobar so I know when I've changed my kernel and should reoobt

if pacman -Si linux-lts linux | grep "$(uname -r | sed -e 's/-lts//')" &>/dev/null ; then
  echo ''
else
  echo " KERNEL CHANGED " #&& exit 1
fi
